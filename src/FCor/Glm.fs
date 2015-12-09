namespace FCor
#nowarn "9"

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic
open System.IO
open System.Text
open FCor.ExplicitConversion

type GlmLink =
    | Id
    | Log
    | Inverse
    | Logit
    | Probit
    | CLogLog

type GlmDistribution =
    | Gaussian
    | Gamma
    | Poisson
    | Binomial

type GlmDispersion =
    | MaxLikelihood
    | Pearson
    | Deviance

type FactorMask =
    | EnableAllLevels
    | DisableOneLevel

type GlmParameterEstimate =
    {
     Predictor :  Predictor
     Levels : string list
     Value : float
     Std : float
     IsDisabled : bool
     WaldChiSq : float
     PValue : float
    }

[<StructuredFormatDisplay("{AsString}")>]
type GlmGoodnessOfFit =
    {
     ValidObsCount : int64
     DoF : int     
     LogLikehood : float
     Deviance : float
     PearsonChi : float
     //AIC : float
     MLScale : float
     MLPhi : float    
    }
    
    member this.AsString =
            let sb = new StringBuilder()
            let N_dof = float (this.ValidObsCount - int64 this.DoF)
            sb.AppendLine <| "Goodness of Fit:" |> ignore
            sb.AppendLine <| sprintf "%-20s%-12s%s%-12s" "Criterion" "Value" "  " "Value/DoF" |> ignore
            sb.AppendLine <| sprintf "%-20s%12G%s%12G" "Deviance" (float this.Deviance) "  " (float (this.Deviance / N_dof)) |> ignore
            sb.AppendLine <| sprintf "%-20s%12G%s%12G" "Pearson Chi-Square" (float this.PearsonChi) "  " (float (this.PearsonChi / N_dof)) |> ignore
            sb.AppendLine <| sprintf "%-20s%12G" "Log Likelihood" (float this.LogLikehood) |> ignore
            //sb.AppendLine <| sprintf "%-20s%12G" "AIC" (float this.AIC) |> ignore
            sb.AppendLine <| sprintf "%-20s%12G" "ML Scale" (float this.MLScale) |> ignore
            sb.AppendLine <| sprintf "%-20s%12G" "ML Phi" (float this.MLPhi) |> ignore
            sb.ToString()

[<StructuredFormatDisplay("{AsString}")>]
type GlmModel =
    {
     Response : CovariateExpr
     Predictors : Predictor list
     Distribution : GlmDistribution
     Link : GlmLink
     HasIntercept : bool
     GoodnessOfFit : GlmGoodnessOfFit option
     Beta : Vector
     InvHDiag : Vector
     Iter : int
     Parameters : GlmParameterEstimate list
     EstimateMaps : int[] list
     CumEstimateCount : int[]
    }
    member this.AsString =
        let sb = new StringBuilder()
        sb.AppendLine() |> ignore
        sb.AppendLine <| "Generalized Linear Model" |> ignore
        sb.AppendLine <| sprintf "%-15s %A" "Distribution" this.Distribution |> ignore
        sb.AppendLine <| sprintf "%-15s %A" "Link" this.Link |> ignore
        sb.AppendLine <| sprintf "%-15s %s" "Response" this.Response.Name |> ignore
        sb.AppendLine <| sprintf "%-15s %s" "Predictors" (String.Join("+", this.Predictors |> List.map (fun p -> p.Name))) |> ignore
        sb.AppendLine <| sprintf "%-15s %b" "Intercept" this.HasIntercept |> ignore
        match this.GoodnessOfFit with
            | Some(goodnessOfFit) ->
                sb.AppendLine <| sprintf "%-15s %i" "Valid Obs Count" goodnessOfFit.ValidObsCount |> ignore
                sb.AppendLine <| sprintf "Algorithm converged in %d iterations" this.Iter |> ignore
                sb.AppendLine("") |> ignore
                sb.Append(goodnessOfFit.AsString) |> ignore
                sb.AppendLine("") |> ignore
                sb.AppendLine <| "Parameter Estimates" |> ignore
                sb.AppendLine <| sprintf "%-20s  %-20s  %-3s  %-12s %-12s %-12s %-12s" "Predictor" "Level" "DoF" "Estimate" "StdError" "Wald ChiSq" "Pr > ChiSq" |> ignore
                this.Parameters |> List.iter (fun prm -> 
                                                  sb.AppendLine <| sprintf "%-20s  %-20s  %3d  %12G %12G %12G %12G" prm.Predictor.Name (String.Join("*", prm.Levels))
                                                                           (if prm.IsDisabled then 0 else 1) prm.Value prm.Std prm.WaldChiSq prm.PValue |> ignore
                                             )
            | None -> 
                if this.Beta = Vector.Empty then
                    sb.AppendLine <| sprintf "Algorithm did not converge: all parameters disabled" |> ignore
                else
                    sb.AppendLine <| sprintf "Algorithm did not converge after %d iterations" this.Iter |> ignore
        sb.ToString()


module Glm =

    let rec trigamma (x : float) =
        if x > 100.0 then 1.0/x + 1.0 / (2.0*x*x) + 1.0 / (6.0*x*x*x) - 1.0/(30.0*x*x*x*x*x) + 1.0/(42.0*x*x*x*x*x*x*x) - 1.0/(30.0*x*x*x*x*x*x*x*x*x)
        else
            trigamma (x+1.0)+1.0/(x*x)

    let psi (x : float) =
        let mutable res = x
        MklFunctions.D_Digam_Array(1L, &&res, &&res)
        res

    let chicdf (df : float, x : float) =
        let mutable res = x
        MklFunctions.D_Cdfchi_Array(1L, df, &&res, &&res)
        res

    let gammaLogL' (alpha : float) (dev : float) (N : float) =
        psi(alpha) - Math.Log(alpha) + dev / (2.0 * N)

    let rec zipN (xss : seq<'T> list) : seq<'T list> =
        match xss with
            | [] -> Seq.singleton [] 
            | head::[] ->
                head|> Seq.map (fun x -> [x])
            | head::tail ->
                (zipN tail) |> Seq.zip head |> Seq.map (fun (x,y) -> x::y)

    let toCsv (vars : StatVariable seq) (path : string) =
        let join delimiter (data : #obj seq) =
            data |> Seq.map (fun x -> x.ToString()) |> String.concat delimiter
        use writer = new StreamWriter(path)
        let header = vars |> Seq.map (fun v -> v.Name) |> join ","
        writer.WriteLine header
        let dataSeq = vars |> Seq.map (fun v -> match v with
                                                    | StatVariable.Factor(f) -> f.AsSeq |> Seq.map (snd>>box)
                                                    | Covariate(c) -> c.AsSeq |> Seq.map (float32>>box)
                                      )
                           |> Seq.toList |> zipN |> Seq.map (join ",")
        dataSeq |> Seq.iter writer.WriteLine

    let importCsv (path : string) (delimiter : char[]) (dropVars : string[]) (hasHeaders : bool) (sampleOnly : bool) =
        let dropVars = new Set<_>(dropVars)
        use sr = new StreamReader(path)
        let firstN = 1000
        let N = 1000
        let nas = new Set<_>(Factor.NAs)

        let isNotNumerics (s : string) =
            not <| nas.Contains(s) && (match Single.TryParse(s) with | (true, _) -> false | _ -> true)

        let take (sr : StreamReader) (nLines : int) =
            let rec take' (sr : StreamReader) (nLines : int) (lines : string list) =
                if nLines = 0 then lines
                else
                    let line = sr.ReadLine()
                    if not <| String.IsNullOrEmpty(line) then take' sr (nLines - 1) (lines @ [line])
                    else lines
            take' sr nLines [] |> List.toArray

        let take = take sr

        let getColArray (splitLines : string[][]) col =
            Array.init splitLines.Length (fun row -> if col < splitLines.[row].Length then splitLines.[row].[col] else String.Empty)

        let split (s : string) = s.Split delimiter 
        let firstLine = (take 1)
        let headers =
            if hasHeaders then firstLine.[0] |> split
            else firstLine.[0].Split(delimiter) |> Array.mapi (fun i _ -> sprintf "Col%d" i)
        let isDropped = headers |> Array.map (fun h -> dropVars.Contains h)
        let firstChunk =
            if hasHeaders then 
                take firstN |> Array.map split
            else 
                take firstN |> Array.append firstLine |> Array.map split
        let isFactor = Array.init headers.Length (fun col -> if isDropped.[col] then false else col |> getColArray firstChunk |> Array.exists isNotNumerics)
        let factorStorage = Array.init headers.Length (fun col -> new FactorStorage())
        let covStorage = Array.init headers.Length (fun col -> new CovariateStorageFloat32())
        let covSlices = Array.init headers.Length (fun col -> if isFactor.[col] then Array.create 0 0.0f else Array.create N 0.0f)
        let slices = Array.init headers.Length (fun col -> if isDropped.[col] then Array.create 0 String.Empty else Array.create N String.Empty)

        isFactor |> Array.zip isDropped
            |> Array.iteri (fun col (isDropped, isFactor) -> 
                                     if not isDropped then
                                         let colArray = getColArray firstChunk col
                                         if isFactor then
                                             factorStorage.[col].SetSlice(0L, colArray)
                                         else
                                             covStorage.[col].SetSlice(0L, colArray |> Array.map (fun s -> match Single.TryParse(s) with | (true, v) -> v | _ -> Single.NaN))
                                )
        if not sampleOnly then
            let rec processLine (fromObs : int64) (N : int) (row : int) (isFactor : bool[]) (isDropped : bool[]) (delimiter : char[]) =
                let line = sr.ReadLine()
                if not <| String.IsNullOrEmpty(line) then
                    line.Split(delimiter) |> Array.iteri (fun col s -> if not isDropped.[col] then slices.[col].[row] <- s)
                    if row = N - 1 then
                        isFactor |> Array.iteri (fun col isFactor -> 
                                                    if not isFactor && not isDropped.[col] then
                                                        covSlices.[col] <- slices.[col] |> Array.Parallel.map (fun s -> let f = ref 0.0f
                                                                                                                        if Single.TryParse(s, f) then !f else Single.NaN) 
                                                )
                        isFactor |> Array.Parallel.iteri (fun col isFactor ->
                                                                if not isDropped.[col] then
                                                                    if isFactor then factorStorage.[col].SetSlice(fromObs, slices.[col])
                                                                    else
                                                                        covStorage.[col].SetSlice(fromObs, covSlices.[col]))
                        processLine (fromObs + int64(N)) N 0 isFactor isDropped delimiter 
                    else
                        processLine fromObs N (row + 1) isFactor isDropped delimiter 
                else
                    if row > 0 then
                        isFactor |> Array.iteri (fun col isFactor -> 
                                                    if not isFactor && not isDropped.[col] then
                                                        covSlices.[col] <- slices.[col] |> Array.Parallel.map (fun s -> let f = ref 0.0f
                                                                                                                        if Single.TryParse(s, f) then !f else Single.NaN) 
                                                )
                        isFactor |> Array.Parallel.iteri (fun col isFactor ->
                                                                if not isDropped.[col] then
                                                                    if isFactor then factorStorage.[col].SetSlice(fromObs, Array.sub slices.[col] 0 row)
                                                                    else covStorage.[col].SetSlice(fromObs, Array.sub (covSlices.[col]) 0 row))
                    else ()

            processLine (int64(firstN)) N 0 isFactor isDropped delimiter 

        isFactor |> Array.zip headers |> Array.mapi (fun col (name, isFactor) ->
                                                         if not isDropped.[col] then
                                                             if isFactor then 
                                                                 StatVariable.Factor(new Factor(name, factorStorage.[col])) |> Some
                                                             else
                                                                 Covariate(new Covariate(name, covStorage.[col])) |> Some
                                                         else None)
                                      |> Array.choose (id) |> Array.toList

    let lnGamma (x : float) =
        let mutable res = x
        MklFunctions.D_Lngam_Array(1L, &&res, &&res)
        res

    let  epsEqual x y (eps : float) =
        (x = y) || (abs(x) <= eps && abs(y) <= eps) || (abs(x-y) / (max (abs(x)) (abs(y))) <= eps)

    let epsEqualVector (a : Vector) (b : Vector) eps =
        if a.Length = 0 && b.Length = 0 then true
        else (a.Length = b.Length) && (b.ToArray() |> Array.zip (a.ToArray()) |> Array.map (fun (x,y) -> epsEqual x y eps) |> Array.fold (&&) true)

    let getSlices (length : int64) (sliceLen : int64) =
        let m = length / sliceLen |> int
        let k = length % sliceLen 
        if k = 0L then
            Array.init m (fun i -> int64(i) * sliceLen, int64(i + 1) * sliceLen - 1L)
        else
           Array.init (m+1) (fun i -> if i = m then int64(m) * sliceLen, length - 1L else int64(i) * sliceLen, int64(i + 1) * sliceLen - 1L)

    let rec cartesian xss =
        match xss with
        | [] -> []
        | head::[] -> head |> List.map (fun x -> [x])
        | head::tail -> head |> List.map (fun x -> tail |> cartesian |> List.map (fun y -> x::y)) |> List.concat

    let rec zipDesign (design : ((seq<'T list> * int[]) option * seq<'S> option * int[]) list) : seq<(('T list * int[]) option * 'S option * int[]) list> =
        match design with
            | [] -> Seq.singleton [] 
            | head::[] ->
                match head with
                    | Some(factors, dimProd), None, estMap ->
                        factors |> Seq.map (fun f -> [Some(f, dimProd), None, estMap])
                    | None, Some(cov), estMap ->
                        cov |> Seq.map (fun c -> [None, Some c, estMap])
                    | Some(factors, dimProd), Some(cov), estMap -> 
                        cov |> Seq.zip factors |> Seq.map (fun (f, c) -> [Some(f, dimProd), Some c, estMap])
                    | None, None, _ -> Seq.singleton []
            | head::tail -> 
                (zipDesign tail) |> Seq.zip (zipDesign [head]) |> Seq.map (fun (x,y) -> x @ y)

    let colHIsZero (H : Matrix) (col : int) =
        use notZero = (H.ColView(col) .<> 0.0)
        not <| BoolVector.Any(notZero)
     
    let updateEstimateMaps (estimateMaps : int[] list) (cumEstCounts : int[]) (isNumDisabled : bool[]) =

        let rec updateEstMap' (estMap : int list) (isNumDisabled' : bool list) (n : int) =
            match estMap, isNumDisabled' with
                | [], [] -> []
                | (h::t), [] ->
                    if h >= 0 && (h - n) >= 0 then (h - n)::(updateEstMap' t isNumDisabled' n)
                    else h::(updateEstMap' t isNumDisabled' n)
                | [], (h::t) -> raise (new InvalidOperationException())
                | (h1::t1), (h2::t2) ->
                    if h2 then
                        if h1 >= 0 then (-1)::(updateEstMap' t1 t2 (n + 1))   
                        else h1::(updateEstMap' t1 isNumDisabled' n)
                    else
                        if h1 >= 0 then h1::(updateEstMap' t1 t2 n)
                        else h1::(updateEstMap' t1 isNumDisabled' n)

        let startCounts = [|0..cumEstCounts.Length - 1|] |> Array.map (fun i -> if i = 0 then 0, cumEstCounts.[0] else cumEstCounts.[i - 1], (cumEstCounts.[i] - cumEstCounts.[i - 1]))      
                                                         |> Array.toList

        let newEstMaps = estimateMaps |> List.zip startCounts |> List.map (fun ((start, count), estMap) ->
                                                                               let isNumDisabled = Array.sub isNumDisabled start count |> Array.toList
                                                                               updateEstMap' (estMap |> Array.toList) isNumDisabled 0
                                                                          ) |> List.map (fun x -> x |> List.toArray)
        let estCounts = newEstMaps |> List.map (fun x -> x |> Array.filter (fun index -> index >= 0)) |> List.map (fun x -> x.Length) |> List.toArray
        let newCumEstCounts = Array.sub (estCounts |> Array.scan (+) 0) 1 estCounts.Length
        newEstMaps, newCumEstCounts

    let getDimProd (size : int list) =
        (size |> List.scan (*) 1 |> List.rev).Tail |> List.rev

    let sub2ind (dimProd : int[]) (subscripts : int[]) =
        subscripts |> Array.mapi (fun i x -> x * dimProd.[i]) |> Array.fold (+) 0

    let  ind2sub (index : int) (dimProd : int list) =
        let rec ind2sub' (index : int) (dimProd : int list) =
            match dimProd with
                | [] -> []
                | head::tail -> 
                    (index / head)::(ind2sub' (index % head) tail)
        ind2sub' index (dimProd |> List.rev) |> List.rev

    let getNACount (factor : Factor) =
        let nas = new Set<string>(Factor.NAs)
        [0..factor.Cardinality - 1] |> List.filter (fun index -> nas.Contains(factor.Level(index))) |> List.length

    let getIndexOfMaxLevel (factor : Factor) =
        let nas = new Set<string>(Factor.NAs)
        let validlevels = Array.init (factor.Cardinality) (fun i -> i, factor.Level(i)) |> Array.sortBy snd
                          |> Array.filter (fun (_, level) -> not <| nas.Contains(level))
        if validlevels.Length = 0 then -1
        else
            validlevels.[validlevels.Length - 1] |> fst


    let getDisabledSubscripts (currFactor : Factor) (maskedFactors : (Factor * FactorMask) list)  =
        let nas = new Set<string>(Factor.NAs)
        maskedFactors |> List.map (fun (factor, mask) -> 
                                        if factor = currFactor then 
                                            match mask with
                                                | EnableAllLevels -> []
                                                | DisableOneLevel ->
                                                    let indexOfMaxLevel = getIndexOfMaxLevel factor
                                                    if indexOfMaxLevel >= 0 then [indexOfMaxLevel]
                                                    else []
                                        else [0..factor.Cardinality - 1] |> List.filter (fun index -> not <| nas.Contains(factor.Level(index)))
                                    )
                      |> cartesian

    let getNASubscripts (factors : Factor list) =
        let nas = new Set<string>(Factor.NAs)
        factors |> List.map (fun factor -> 
                                 [0..factor.Cardinality - 1] |> List.filter (fun index -> nas.Contains(factor.Level(index)))
                            )
                |> cartesian

    let getDisabled (maskedFactors : ((Factor * FactorMask) list) * FactorMask) =
        let factors = maskedFactors |> fst |> List.map fst |> List.toArray
        let allLevelCount = factors |> Array.fold (fun count f -> count * f.Cardinality) 1
        let isDisabled = Array.create allLevelCount false
        let dimProd = maskedFactors |> fst |> List.map fst |> List.map (fun f -> f.Cardinality) |> getDimProd |> List.toArray
        factors |> Array.iter (fun f ->
                                 maskedFactors |> fst |> getDisabledSubscripts f
                                               |> List.iter (fun subscripts -> 
                                                                let disabledIndex = subscripts |> List.toArray |> sub2ind dimProd
                                                                isDisabled.[disabledIndex] <- true
                                                           )       
                               )
        match maskedFactors |> snd with
            | DisableOneLevel -> 
                let subscripts = maskedFactors |> fst |> List.map (fun (f, _) -> getIndexOfMaxLevel f)
                if subscripts |> List.map (fun x -> x >= 0) |> List.reduce (&&) then
                    let disabledIndex = subscripts |> List.toArray |> sub2ind dimProd
                    isDisabled.[disabledIndex] <- true
            | _ -> ()
        isDisabled

    let getIsNA (factors : Factor list) =
        let allLevelCount = factors |> List.fold (fun count f -> count * f.Cardinality) 1
        let isNA = Array.create allLevelCount false
        let dimProd = factors |> List.map (fun f -> f.Cardinality) |> getDimProd |> List.toArray
        factors |> getNASubscripts
                |> List.iter (fun subscripts -> 
                                let naIndex = subscripts |> List.toArray |> sub2ind dimProd
                                isNA.[naIndex] <- true
                             )
        isNA

    let getPredictorEstimateMap (maskedPredictor : (((Factor * FactorMask) list) * FactorMask) * CovariateExpr option) =
        match maskedPredictor with
            | ((h::t), factorMask), _ ->
                let maskedFactors = (h::t), factorMask
                let factors = maskedFactors |> fst |> List.map fst
                let isDisabled = getDisabled maskedFactors
                let isNA = getIsNA factors
                let cumDisabledCount = Array.sub (isDisabled |> Array.scan (fun cum x -> if x then cum + 1 else cum) 0) 1 isDisabled.Length
                let cumNACount = Array.sub (isNA |> Array.scan (fun cum x -> if x then cum + 1 else cum) 0) 1 isNA.Length
                Array.init isDisabled.Length (fun i -> if isDisabled.[i] then -1
                                                            elif isNA.[i] then -2
                                                            else i - cumDisabledCount.[i] - cumNACount.[i])
            | ([], _), Some(_) -> [|0|]
            | _ -> [||]

    let getCategoricalSlicer (maskedFactors : (Factor * FactorMask) list) =
        match maskedFactors with
            | h::t ->
                let factors = maskedFactors |> List.map fst 
                let dimProd = maskedFactors |> List.map fst |> List.map (fun f -> f.Cardinality) |> getDimProd |> List.toArray
                let slicerFun =
                    fun (fromObs : int64, toObs : int64, sliceLen : int) ->
                        let slices = factors |> List.map (fun f -> f.GetSlices(fromObs, toObs, sliceLen)) |> zipN
                        slices, dimProd
                slicerFun |> Some
            | [] -> None

    let getEstimateCount (maskedPred : (((Factor * FactorMask) list) * FactorMask) * CovariateExpr option) =
        let (maskedFactors, factorMask), covariate = maskedPred
        let factors = maskedFactors |> List.map fst
        let isNA = getIsNA factors
        let isDisabled = getDisabled (maskedFactors, factorMask)
        let numCount = 
            match covariate with
                | Some(_) -> 1
                | None -> 0
        let catCount = isDisabled |> Array.zip isNA |> Array.fold (fun count (isna, isdis) -> if not isna && not isdis then count + 1 else count) 0
        match maskedFactors with
            | [] -> numCount
            | _ -> catCount

    let updateFactorMask (currMaskedFactors : ((Factor * FactorMask) list) * FactorMask) (earlierMaskedFactor : (Factor * FactorMask) * FactorMask) =
        let earlierFactor = earlierMaskedFactor |> fst |> fst
        let earlierPredictorMask = earlierMaskedFactor|> snd
        let currMaskedFactors' = currMaskedFactors |> fst |> List.map (fun (f, mask) -> if f = earlierFactor then f, DisableOneLevel else f, mask)
        let currPredMask' =
            match earlierPredictorMask with
                | EnableAllLevels -> DisableOneLevel
                | DisableOneLevel -> currMaskedFactors |> snd
        currMaskedFactors', currPredMask'

    let updatePredictorMask (currMaskedPred : ((Factor * FactorMask) list * FactorMask) * CovariateExpr option) (earlierMaskedPred : ((Factor * FactorMask) list * FactorMask) * CovariateExpr option) =
        match currMaskedPred, earlierMaskedPred with
            | ((currMaskedFactors, currMask), Some(currMaskedCovExpr)), ((earlierMaskedFactors, earlierMask), Some(earlierCovExpr)) when currMaskedCovExpr.AsCovariate = earlierCovExpr.AsCovariate  ->
                earlierMaskedFactors |> List.map (fun x -> x, earlierMask) |> List.fold updateFactorMask (currMaskedFactors, currMask), Some(currMaskedCovExpr)
            | ((currMaskedFactors, currMask), None), ((earlierMaskedFactors, earlierMask), None) ->
                earlierMaskedFactors |> List.map (fun x -> x, earlierMask) |> List.fold updateFactorMask (currMaskedFactors, currMask), None
            | _ -> currMaskedPred

    let rec getMaskedPredictors (predictors : (Factor list * CovariateExpr option) list) =
        match predictors with
            | [] -> []
            | (factors, covariate) :: tail -> 
                let t = getMaskedPredictors tail
                let h = t |> List.fold updatePredictorMask ((factors |> List.map (fun f -> f, EnableAllLevels), EnableAllLevels) , covariate)
                h :: t

    let getEstimableDesign (predictors : Predictor list) (includeIntercept : bool) =
        let predictors =
            if includeIntercept then
                ((!!Factor.Intercept:Predictor) + predictors) |> List.map (fun p -> p.AsList)
            else
                predictors |> List.map (fun p -> p.AsList)
        let maskedPredictors = getMaskedPredictors (predictors |> List.rev)
        maskedPredictors |> List.rev

    let getPred (xbeta : Vector) (glmLink : GlmLink) =
        match glmLink with
            | Id -> xbeta
            | Log -> Vector.Exp(xbeta)
            | Inverse -> 1.0 ./ xbeta
            | Logit -> Vector.Exp(xbeta) ./ (1.0 + Vector.Exp(xbeta))
            | CLogLog -> 1.0 - Vector.Exp(-Vector.Exp(xbeta))
            | Probit -> Vector.Normcdf(xbeta)

    let get_u (resp : Vector) (pred : Vector) (xbeta : Vector) (obsW : Vector) (glmLink : GlmLink) (glmDistribution : GlmDistribution) =
        match glmDistribution, glmLink with
            | Gaussian, Id -> 
                (resp - pred)
            | Gaussian, Log ->
                ((resp - pred) .* pred)
            | Gaussian, Inverse ->
                ((pred - resp)) .* (pred .* pred)
            | Gamma, Id -> 
                (resp - pred) ./ (pred .* pred)
            | Gamma, Log ->
                VectorExpr.EvalIn((resp.AsExpr - pred) ./ pred, None)
            | Gamma, Inverse ->
                pred - resp
            | Poisson, Id -> 
                (resp - pred) ./ pred
            | Poisson, Log ->
                resp - pred
            | Poisson, Inverse ->
                (pred - resp) .* pred
            | Binomial, Logit -> if obsW = Vector.Empty then (resp - pred) else obsW .* (resp - pred)
            | Binomial, CLogLog ->
                if obsW = Vector.Empty then 
                    -(resp - pred) ./ pred .* Vector.Log(1.0 - pred)
                else
                    -(resp - pred) ./ pred .* Vector.Log(1.0 - pred) .* obsW
            | Binomial, Probit -> 
                if obsW = Vector.Empty then 
                    (resp - pred) ./ (pred .* (1.0 - pred)) .* (Vector.Exp(-0.5 * xbeta .* xbeta) / (Math.Sqrt(2.0 * Math.PI)))
                else
                    (resp - pred) ./ (pred .* (1.0 - pred)) .* obsW .* (Vector.Exp(-0.5 * xbeta .* xbeta) / (Math.Sqrt(2.0 * Math.PI)))
            | _ -> raise (new NotImplementedException("Glm distribution and link not implemented"))

    let getWeight (pred : Vector) (xbeta : Vector) (obsW : Vector) (glmLink : GlmLink) (glmDistribution : GlmDistribution) =
        match glmDistribution, glmLink with
            | Gaussian, Id -> 
                let xbeta = xbeta.AsExpr
                VectorExpr.EvalIn(VectorExpr.IfFunction((xbeta .= xbeta), VectorExpr.Scalar(1.0), VectorExpr.Scalar(Double.NaN)), None) 
            | Gaussian, Log ->
                pred .* pred
            | Gaussian, Inverse ->
                (pred .^ 4.0)
            | Gamma, Id -> 
                1.0 ./ (pred .*pred)
            | Gamma, Log ->
                let xbeta = xbeta.AsExpr
                VectorExpr.EvalIn(VectorExpr.IfFunction((xbeta .= xbeta), VectorExpr.Scalar(1.0), VectorExpr.Scalar(Double.NaN)), None) 
            | Gamma, Inverse ->
                pred .* pred
            | Poisson, Id -> 
                1.0 ./ pred
            | Poisson, Log ->
                pred
            | Poisson, Inverse ->
                (pred .^ 3.0)
            | Binomial, Logit -> if obsW = Vector.Empty then pred .* (1.0 - pred) else obsW .* (pred .* (1.0 - pred))
            | Binomial, CLogLog ->
                if obsW = Vector.Empty then
                    (1.0 - pred) .* (Vector.Log(1.0 - pred) ./ pred) .* Vector.Log(1.0 - pred)
                else
                    (1.0 - pred) .* (Vector.Log(1.0 - pred) ./ pred) .* Vector.Log(1.0 - pred) .* obsW
            | Binomial, Probit ->
                if obsW = Vector.Empty then
                    (Vector.Exp(-0.5 * xbeta .* xbeta) ./ (Math.Sqrt(2.0 * Math.PI)) .^ 2.0) ./ (pred*(1.0 - pred))
                else
                    (Vector.Exp(-0.5 * xbeta .* xbeta) ./ (Math.Sqrt(2.0 * Math.PI)) .^ 2.0) ./ (pred*(1.0 - pred)) .* obsW
            | _ -> raise (new InvalidOperationException())

    let getXBeta (design : ((UInt16Vector list * int[]) option * Vector option * int[]) list) (beta : Vector) (sliceLen : int)
                 (cumEstimateCounts: int[]) =
        let xbeta = new Vector(sliceLen, 0.0)
        design |> List.iteri (fun pInd p ->
                                let estCount = if pInd = 0 then cumEstimateCounts.[0] else cumEstimateCounts.[pInd] - cumEstimateCounts.[pInd - 1]
                                if estCount > 0 then
                                    match p with
                                        | Some(factors), None, estimateMap ->  
                                            let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1]  
                                            let slice, dimProd = factors
                                            if estimateMap.Length = 1 && estimateMap.[0] >= 0 then
                                                VectorExpr.EvalIn(xbeta.AsExpr + beta.[offset], Some xbeta) |> ignore
                                            else
                                                let slice = slice |> List.map (fun v -> v.NativeArray) |> List.toArray
                                                MklFunctions.Glm_Update_XBeta(sliceLen, xbeta.NativeArray, slice.Length, slice, estimateMap, dimProd, beta.NativeArray, Vector.Empty.NativeArray, offset)
                                        | None, Some(covariate), _ -> 
                                            let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1] 
                                            VectorExpr.EvalIn(xbeta.AsExpr + beta.[offset] * covariate.AsExpr, Some xbeta) |> ignore
                                        | Some(factors), Some(covariate), estimateMap -> 
                                            let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1] 
                                            let slice, dimProd = factors
                                            if estimateMap.Length = 1 && estimateMap.[0] >= 0 then
                                                VectorExpr.EvalIn(xbeta.AsExpr + beta.[offset] * covariate.AsExpr, Some xbeta) |> ignore
                                            else
                                                let slice = slice |> List.map (fun v -> v.NativeArray) |> List.toArray
                                                MklFunctions.Glm_Update_XBeta(sliceLen, xbeta.NativeArray, slice.Length, slice, estimateMap, dimProd, beta.NativeArray, covariate.NativeArray, offset)
                                        | _ -> ()
                                )
        xbeta

    let getU (design : ((UInt16Vector list * int[]) option * Vector option * int[]) list) (u : Vector) (sliceLen : int)
             (cumEstimateCounts: int[]) =
        let U = new Vector(cumEstimateCounts.[cumEstimateCounts.Length - 1], 0.0)
        design |> List.iteri (fun pInd p ->
                                let estCount = if pInd = 0 then cumEstimateCounts.[0] else cumEstimateCounts.[pInd] - cumEstimateCounts.[pInd - 1]
                                if estCount > 0 then
                                    match p with
                                        | Some(factors), None, estimateMap ->   
                                            let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1] 
                                            let slice, dimProd = factors
                                            if estimateMap.Length = 1 && estimateMap.[0] >= 0 then
                                                let sum = MklFunctions.Sum_Array_NotNan(sliceLen, u.NativeArray)
                                                U.[offset] <- U.[offset] + (if Double.IsNaN(sum) then 0.0 else sum)
                                            else
                                                let slice = slice |> List.map (fun v -> v.NativeArray) |> List.toArray
                                                MklFunctions.Glm_Update_U(sliceLen, U.NativeArray, u.NativeArray, slice.Length, slice, estimateMap, dimProd, Vector.Empty.NativeArray, offset)
                                        | None, Some(covariate), estimateMap -> 
                                            let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1] 
                                            let innerProd = MklFunctions.Innerprod_Arrays_NotNan(sliceLen, u.NativeArray, covariate.NativeArray)
                                            U.[offset] <- U.[offset] + (if Double.IsNaN(innerProd) then 0.0 else innerProd)
                                        | Some(factors), Some(covariate), estimateMap ->
                                            let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1]  
                                            let slice, dimProd = factors
                                            if estimateMap.Length = 1 && estimateMap.[0] >= 0 then
                                                let innerProd = MklFunctions.Innerprod_Arrays_NotNan(sliceLen, u.NativeArray, covariate.NativeArray)
                                                U.[offset] <- U.[offset] + (if Double.IsNaN(innerProd) then 0.0 else innerProd)
                                            else
                                                let slice = slice |> List.map (fun v -> v.NativeArray) |> List.toArray
                                                MklFunctions.Glm_Update_U(sliceLen, U.NativeArray, u.NativeArray, slice.Length, slice, estimateMap, dimProd, covariate.NativeArray, offset)
                                        | _ -> ()
                                )
        U

    let zeroIntArr = Array.create 0 0
    let zeroIntPtrArr = Array.create 0 (UInt16Vector.Empty.NativeArray)

    let getH (design : ((UInt16Vector list * int[]) option * Vector option * int[]) list) (weight : Vector) (sliceLen : int)
             (cumEstimateCounts: int[]) =
        let p = cumEstimateCounts.[cumEstimateCounts.Length - 1]
        let H = new Matrix(p, p, 0.0)
        let weightIsOne = weight == Vector.Empty
        for pCol in 0..design.Length-1 do
            let colEstCount = if pCol = 0 then cumEstimateCounts.[0] else cumEstimateCounts.[pCol] - cumEstimateCounts.[pCol - 1]
            for pRow in 0..pCol do
                let rowEstCount = if pRow = 0 then cumEstimateCounts.[0] else cumEstimateCounts.[pRow] - cumEstimateCounts.[pRow - 1]
                if rowEstCount > 0 && colEstCount > 0 then
                    let rowOffset = if pRow = 0 then 0 else cumEstimateCounts.[pRow-1]
                    let colOffset = if pCol = 0 then 0 else cumEstimateCounts.[pCol-1]
                    match design.[pRow], design.[pCol] with
                        | (Some(rowFactors), None, rowEstimateMap), (Some(colFactors), None, colEstimateMap) ->
                            let rowSlice, rowDimProd = rowFactors
                            let colSlice, colDimProd = colFactors
                            if rowEstimateMap.Length = 1 && rowEstimateMap.[0] >= 0 && colEstimateMap.Length = 1 && colEstimateMap.[0] >= 0 then
                                if weightIsOne then
                                    H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + float(sliceLen)
                                else
                                    let sum = MklFunctions.Sum_Array_NotNan(sliceLen, weight.NativeArray)
                                    H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + (if Double.IsNaN(sum) then 0.0 else sum)
                            else   
                                let rowSlice = rowSlice |> List.map (fun v -> v.NativeArray) |> List.toArray
                                let colSlice = colSlice |> List.map (fun v -> v.NativeArray) |> List.toArray
                                MklFunctions.Glm_Update_H(sliceLen, p, H.ColMajorDataVector.NativeArray, weight.NativeArray,
                                                          Vector.Empty.NativeArray,
                                                          Vector.Empty.NativeArray,
                                                          rowSlice.Length, rowSlice, rowEstimateMap, rowDimProd,
                                                          colSlice.Length, colSlice, colEstimateMap, colDimProd, rowOffset, colOffset)

                        | (Some(rowFactors), None, rowEstimateMap), (None, Some(colCovariate), _) ->
                            let rowSlice, rowDimProd = rowFactors
                            if rowEstimateMap.Length = 1 && rowEstimateMap.[0] >= 0 then
                                if weightIsOne then
                                    let sum = MklFunctions.Sum_Array_NotNan(sliceLen, colCovariate.NativeArray)
                                    H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + (if Double.IsNaN(sum) then 0.0 else sum)
                                else
                                    let innerProd = MklFunctions.Innerprod_Arrays_NotNan(sliceLen, weight.NativeArray, colCovariate.NativeArray)
                                    H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + (if Double.IsNaN(innerProd) then 0.0 else innerProd)
                            else
                                let rowSlice = rowSlice |> List.map (fun v -> v.NativeArray) |> List.toArray
                                MklFunctions.Glm_Update_H(sliceLen, p, H.ColMajorDataVector.NativeArray, weight.NativeArray,
                                                          Vector.Empty.NativeArray,
                                                          colCovariate.NativeArray,
                                                          rowSlice.Length, rowSlice, rowEstimateMap, rowDimProd,
                                                          0, zeroIntPtrArr, zeroIntArr, zeroIntArr, rowOffset, colOffset)

                        | (Some(rowFactors), None, rowEstimateMap), (Some(colFactors), Some(colCovariate), colEstimateMap) ->
                            let rowSlice, rowDimProd = rowFactors
                            let colSlice, colDimProd = colFactors
                            if rowEstimateMap.Length = 1 && rowEstimateMap.[0] >= 0 && colEstimateMap.Length = 1 && colEstimateMap.[0] >= 0 then
                                if weightIsOne then
                                    let sum = MklFunctions.Sum_Array_NotNan(sliceLen, colCovariate.NativeArray)
                                    H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + (if Double.IsNaN(sum) then 0.0 else sum)
                                else
                                    let innerProd = MklFunctions.Innerprod_Arrays_NotNan(sliceLen, weight.NativeArray, colCovariate.NativeArray)
                                    H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + (if Double.IsNaN(innerProd) then 0.0 else innerProd)
                            else
                                let rowSlice = rowSlice |> List.map (fun v -> v.NativeArray) |> List.toArray
                                let colSlice = colSlice |> List.map (fun v -> v.NativeArray) |> List.toArray
                                MklFunctions.Glm_Update_H(sliceLen, p, H.ColMajorDataVector.NativeArray, weight.NativeArray,
                                                          Vector.Empty.NativeArray,
                                                          colCovariate.NativeArray,
                                                          rowSlice.Length, rowSlice, rowEstimateMap, rowDimProd,
                                                          colSlice.Length, colSlice, colEstimateMap, colDimProd, rowOffset, colOffset)

                        | (None, Some(rowCovariate), _), (None, Some(colCovariate), _) -> 
                            if weightIsOne then
                                let innerProd = MklFunctions.Innerprod_Arrays_NotNan(sliceLen, rowCovariate.NativeArray, colCovariate.NativeArray)
                                H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + (if Double.IsNaN(innerProd) then 0.0 else innerProd) 
                            else
                                let innerProd = MklFunctions.Innerprod_3Arrays_NotNan(sliceLen, weight.NativeArray, rowCovariate.NativeArray, colCovariate.NativeArray)
                                H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + (if Double.IsNaN(innerProd) then 0.0 else innerProd)

                        | (None, Some(rowCovariate), _), (Some(colFactors), None, colEstimateMap) -> 
                            let colSlice, colDimProd = colFactors
                            if colEstimateMap.Length = 1 && colEstimateMap.[0] >= 0 then
                                if weightIsOne then
                                    let sum = MklFunctions.Sum_Array_NotNan(sliceLen, rowCovariate.NativeArray)
                                    H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + (if Double.IsNaN(sum) then 0.0 else sum)
                                else
                                    let innerProd = MklFunctions.Innerprod_Arrays_NotNan(sliceLen, weight.NativeArray, rowCovariate.NativeArray)
                                    H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + (if Double.IsNaN(innerProd) then 0.0 else innerProd)
                            else
                                let colSlice = colSlice |> List.map (fun v -> v.NativeArray) |> List.toArray
                                MklFunctions.Glm_Update_H(sliceLen, p, H.ColMajorDataVector.NativeArray, weight.NativeArray,
                                                          rowCovariate.NativeArray,
                                                          Vector.Empty.NativeArray,
                                                          0, zeroIntPtrArr, zeroIntArr, zeroIntArr,
                                                          colSlice.Length, colSlice, colEstimateMap, colDimProd, rowOffset, colOffset)

                        | (None, Some(rowCovariate), _), (Some(colFactors), Some(colCovariate), colEstimateMap) -> 
                            let colSlice, colDimProd = colFactors
                            if colEstimateMap.Length = 1 && colEstimateMap.[0] >= 0 then
                                if weightIsOne then
                                    let innerProd = MklFunctions.Innerprod_Arrays_NotNan(sliceLen, rowCovariate.NativeArray, colCovariate.NativeArray)
                                    H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + (if Double.IsNaN(innerProd) then 0.0 else innerProd)
                                else
                                    let innerProd = MklFunctions.Innerprod_3Arrays_NotNan(sliceLen, weight.NativeArray, rowCovariate.NativeArray, colCovariate.NativeArray)
                                    H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + (if Double.IsNaN(innerProd) then 0.0 else innerProd)
                            else
                                let colSlice = colSlice |> List.map (fun v -> v.NativeArray) |> List.toArray
                                MklFunctions.Glm_Update_H(sliceLen, p, H.ColMajorDataVector.NativeArray, weight.NativeArray,
                                                          rowCovariate.NativeArray,
                                                          colCovariate.NativeArray,
                                                          0, zeroIntPtrArr, zeroIntArr, zeroIntArr,
                                                          colSlice.Length, colSlice, colEstimateMap, colDimProd, rowOffset, colOffset)

                        | (Some(rowFactors), Some(rowCovariate), rowEstimateMap), (Some(colFactors), Some(colCovariate), colEstimateMap) -> 
                            let rowSlice, rowDimProd = rowFactors
                            let colSlice, colDimProd = colFactors
                            if rowEstimateMap.Length = 1 && rowEstimateMap.[0] >= 0 && colEstimateMap.Length = 1 && colEstimateMap.[0] >= 0 then
                                if weightIsOne then
                                    let innerProd = MklFunctions.Innerprod_Arrays_NotNan(sliceLen, rowCovariate.NativeArray, colCovariate.NativeArray)
                                    H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + (if Double.IsNaN(innerProd) then 0.0 else innerProd)
                                else
                                    let innerProd = MklFunctions.Innerprod_3Arrays_NotNan(sliceLen, weight.NativeArray, rowCovariate.NativeArray, colCovariate.NativeArray)
                                    H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + (if Double.IsNaN(innerProd) then 0.0 else innerProd)
                            else
                                let rowSlice = rowSlice |> List.map (fun v -> v.NativeArray) |> List.toArray
                                let colSlice = colSlice |> List.map (fun v -> v.NativeArray) |> List.toArray
                                MklFunctions.Glm_Update_H(sliceLen, p, H.ColMajorDataVector.NativeArray, weight.NativeArray,
                                                          rowCovariate.NativeArray,
                                                          colCovariate.NativeArray,
                                                          rowSlice.Length, rowSlice, rowEstimateMap, rowDimProd,
                                                          colSlice.Length, colSlice, colEstimateMap, colDimProd, rowOffset, colOffset)

                        | (Some(rowFactors), Some(rowCovariate), rowEstimateMap), (None, Some(colCovariate), _) -> 
                            let rowSlice, rowDimProd = rowFactors
                            if rowEstimateMap.Length = 1 && rowEstimateMap.[0] >= 0 then
                                if weightIsOne then
                                    let innerProd = MklFunctions.Innerprod_Arrays_NotNan(sliceLen, rowCovariate.NativeArray, colCovariate.NativeArray)
                                    H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + (if Double.IsNaN(innerProd) then 0.0 else innerProd)
                                else
                                    let innetProd = MklFunctions.Innerprod_3Arrays_NotNan(sliceLen, weight.NativeArray, rowCovariate.NativeArray, colCovariate.NativeArray)
                                    H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + (if Double.IsNaN(innetProd) then 0.0 else innetProd)
                            else
                                let rowSlice = rowSlice |> List.map (fun v -> v.NativeArray) |> List.toArray
                                MklFunctions.Glm_Update_H(sliceLen, p, H.ColMajorDataVector.NativeArray, weight.NativeArray,
                                                          rowCovariate.NativeArray,
                                                          colCovariate.NativeArray,
                                                          rowSlice.Length, rowSlice, rowEstimateMap, rowDimProd,
                                                          0, zeroIntPtrArr, zeroIntArr, zeroIntArr, rowOffset, colOffset)

                        | (Some(rowFactors), Some(rowCovariate), rowEstimateMap), (Some(colFactors), None, colEstimateMap) -> 
                            let rowSlice, rowDimProd = rowFactors
                            let colSlice, colDimProd = colFactors
                            if rowEstimateMap.Length = 1 && rowEstimateMap.[0] >= 0 then
                                if weightIsOne then
                                    let sum = MklFunctions.Sum_Array_NotNan(sliceLen, rowCovariate.NativeArray)
                                    H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + (if Double.IsNaN(sum) then 0.0 else sum)
                                else
                                    let innerProd = MklFunctions.Innerprod_Arrays_NotNan(sliceLen, weight.NativeArray, rowCovariate.NativeArray)
                                    H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + (if Double.IsNaN(innerProd) then 0.0 else innerProd)
                            else
                                let rowSlice = rowSlice |> List.map (fun v -> v.NativeArray) |> List.toArray
                                let colSlice = colSlice |> List.map (fun v -> v.NativeArray) |> List.toArray
                                MklFunctions.Glm_Update_H(sliceLen, p, H.ColMajorDataVector.NativeArray, weight.NativeArray,
                                                          rowCovariate.NativeArray,
                                                          Vector.Empty.NativeArray,
                                                          rowSlice.Length, rowSlice, rowEstimateMap, rowDimProd,
                                                          colSlice.Length, colSlice, colEstimateMap, colDimProd, rowOffset, colOffset)

                        | _ -> ()
        H

    let rec iwls (response : Covariate) (obsW : Covariate option) (design : ((int64 * int64 * int -> seq<UInt16Vector list> * int[]) option * Covariate option) list)
                 (estimateMaps : int[] list)
                 (glmDistribution : GlmDistribution) (glmLink : GlmLink) (beta : Vector) (H : Matrix option)
                 (maxIter : int) (iter : int) (obsCount : int64) (sliceLength : int) (cumEstimateCounts : int[]) eps (delta : Vector) (halfStep : bool) =
        if iter > maxIter then 
            beta, Vector.Empty, iter, false, estimateMaps, cumEstimateCounts
        else
            let processorCount = Environment.ProcessorCount
            let processorChunk = obsCount / int64(processorCount)
            let (U, isPredInvalid), H =
                match H with
                    | Some(H) ->
                        seq{0..processorCount - 1} 
                           |> Seq.map (fun i -> (int64(i) * processorChunk), if i = processorCount - 1 then obsCount - 1L else (int64(i + 1) * processorChunk - 1L))
                           |> Seq.toArray 
                           |> Array.Parallel.map (fun (fromObs, toObs) -> 
                                                    let totalEstimateCount = cumEstimateCounts.[cumEstimateCounts.Length - 1]
                                                    let U = new Vector(totalEstimateCount, 0.0)
                                                    let respSlices = response.GetSlices(fromObs, toObs, sliceLength) 
                                                    let obsWSlices =
                                                        match obsW with
                                                            | Some(obsW) -> obsW.GetSlices(fromObs, toObs, sliceLength) 
                                                            | None -> respSlices |> Seq.map (fun _ -> Vector.Empty)
                                                    let design' =
                                                        estimateMaps |> List.zip
                                                                        (design |> List.map (fun (factorsOpt, covOpt) ->
                                                                                                factorsOpt |> Option.map (fun f -> f(fromObs, toObs, sliceLength)), covOpt
                                                                                                           |> Option.map (fun c -> c.GetSlices(fromObs, toObs, sliceLength))))
                                                                     |> List.map (fun ((x,y),z) -> x,y,z)
                                                                     |> zipDesign |> Seq.zip3 respSlices obsWSlices
                                                    let mutable isInvalid = false
                                                    for resp, obsW, slice in design' do
                                                       let sliceLength = resp.Length
                                                       use xbeta = getXBeta slice beta sliceLength cumEstimateCounts
                                                       use pred = getPred xbeta glmLink
                                                       match glmDistribution with
                                                           | Gamma | Poisson -> isInvalid <- isInvalid || (Vector.Min(pred) < 0.0)
                                                           | Binomial -> isInvalid <- isInvalid || (Vector.Min(pred) < 0.0) || (Vector.Max(pred) > 1.0)
                                                           | _ -> ()
                                                       use u = get_u resp pred xbeta obsW glmLink glmDistribution
                                                       use updateU = getU slice u sliceLength cumEstimateCounts
                                                       VectorExpr.EvalIn(U.AsExpr + updateU, Some U) |> ignore
                                                    U, isInvalid   
                                                  )
                        |> Array.reduce (fun (u1, isInv1) (u2, isInv2) -> (u1 + u2), (isInv1 || isInv2)), H
                    | None -> 
                        seq{0..processorCount - 1} 
                           |> Seq.map (fun i -> (int64(i) * processorChunk), if i = processorCount - 1 then obsCount - 1L else (int64(i + 1) * processorChunk - 1L))
                           |> Seq.toArray 
                           |> Array.Parallel.map (fun (fromObs, toObs) -> 
                                                    let totalEstimateCount = cumEstimateCounts.[cumEstimateCounts.Length - 1]
                                                    let U = new Vector(totalEstimateCount, 0.0)
                                                    let H = new Matrix(totalEstimateCount, totalEstimateCount, 0.0)
                                                    let respSlices = response.GetSlices(fromObs, toObs, sliceLength) 
                                                    let obsWSlices =
                                                        match obsW with
                                                            | Some(obsW) -> obsW.GetSlices(fromObs, toObs, sliceLength) 
                                                            | None -> respSlices |> Seq.map (fun _ -> Vector.Empty)
                                                    let design' =
                                                        estimateMaps |> List.zip
                                                            (design |> List.map (fun (factorsOpt, covOpt) ->
                                                                                    factorsOpt |> Option.map (fun f -> f(fromObs, toObs, sliceLength)), covOpt
                                                                                               |> Option.map (fun c -> c.GetSlices(fromObs, toObs, sliceLength))))
                                                               |> List.map (fun ((x,y),z) -> x,y,z)
                                                               |> zipDesign |> Seq.zip3 respSlices obsWSlices
                                                    let mutable isInvalid = false
                                                    for resp, obsW, slice in design' do
                                                       let sliceLength = resp.Length
                                                       use xbeta = getXBeta slice beta sliceLength cumEstimateCounts
                                                       use pred = getPred xbeta glmLink
                                                       match glmDistribution with
                                                           | Gamma | Poisson -> isInvalid <- isInvalid || (Vector.Min(pred) < 0.0)
                                                           | Binomial -> isInvalid <- isInvalid || (Vector.Min(pred) < 0.0) || (Vector.Max(pred) > 1.0)
                                                           | _ -> ()
                                                       use u = get_u resp pred xbeta obsW glmLink glmDistribution
                                                       use weight = getWeight pred xbeta obsW glmLink glmDistribution
                                                       use updateU = getU slice u sliceLength cumEstimateCounts
                                                       use updateH = getH slice weight sliceLength cumEstimateCounts
                                                       VectorExpr.EvalIn(U.AsExpr + updateU, Some U) |> ignore
                                                       MatrixExpr.EvalIn(H.AsExpr + updateH, Some H) |> ignore  
                                                    (U, isInvalid), H    
                                                  )
                        |> Array.reduce (fun ((u1, isInv1), h1) ((u2, isInv2), h2) -> ((u1 + u2), (isInv1 || isInv2)), (h1 + h2))
            
            if isPredInvalid then
                iwls response obsW design estimateMaps glmDistribution glmLink (beta - delta) None maxIter (iter + 1) obsCount sliceLength cumEstimateCounts eps delta true
            else
                if iter = 0 then
                    MatrixExpr.EvalIn(H.AsExpr + (Matrix.Transpose(Matrix.UpperTri(H, 1))), Some H) |> ignore
                    let q, r = Matrix.Qr(H)
                    use rdiag = Vector.Abs(r.Diag(0))
                    q.Dispose()
                    r.Dispose()
                    use isNumDisabled : BoolVector = rdiag .<= eps
                    if not <| BoolVector.Any(isNumDisabled) then
                        let delta = Matrix.CholSolve(H, U)
                        let nextBeta = beta + delta
                        U.Dispose()
                        if (glmDistribution = Gaussian && glmLink = Id) || epsEqualVector beta nextBeta 1e-6 then
                            let invHDiag = Matrix.CholInv(H).Diag(0)
                            nextBeta, invHDiag,  iter, true, estimateMaps, cumEstimateCounts
                        elif glmDistribution = Gamma && glmLink = Log then  // weight is 1
                            iwls response obsW design estimateMaps glmDistribution glmLink nextBeta (Some H) maxIter (iter + 1) obsCount sliceLength cumEstimateCounts eps delta false
                        else
                            H.Dispose()
                            iwls response obsW design estimateMaps glmDistribution glmLink nextBeta None maxIter (iter + 1) obsCount sliceLength cumEstimateCounts eps delta false
                    else
                        let newEstMaps, newCumEstCount = updateEstimateMaps estimateMaps cumEstimateCounts (isNumDisabled.ToArray())
                        let newBeta = beta.[BoolVector.Not isNumDisabled]
                        let newDelta = delta.[BoolVector.Not isNumDisabled]
                        if newBeta = Vector.Empty then
                            Vector.Empty, Vector.Empty, iter, false, newEstMaps, newCumEstCount
                        else
                            iwls response obsW design newEstMaps glmDistribution glmLink newBeta None maxIter (iter + 1) obsCount sliceLength newCumEstCount eps newDelta false
                else
                    let delta = if halfStep && delta <> Vector.Empty then 0.5 * delta else Matrix.CholSolve(H, U)
                    let nextBeta = beta + delta
                    U.Dispose()
                    if (glmDistribution = Gaussian && glmLink = Id) || epsEqualVector beta nextBeta 1e-6 then
                        let invHDiag = Matrix.CholInv(H).Diag(0)
                        nextBeta, invHDiag,  iter, true, estimateMaps, cumEstimateCounts
                    elif glmDistribution = Gamma && glmLink = Log then  // weight is 1
                        iwls response obsW design estimateMaps glmDistribution glmLink nextBeta (Some H) maxIter (iter + 1) obsCount sliceLength cumEstimateCounts eps delta false
                    else
                        H.Dispose()
                        iwls response obsW design estimateMaps glmDistribution glmLink nextBeta None maxIter (iter + 1) obsCount sliceLength cumEstimateCounts eps delta false


    let getParameterEstimates (design : (((Factor * FactorMask) list * FactorMask) * CovariateExpr option) list) (beta : Vector)
                              (estimateMaps :  int[] list)
                              (invHDiag : Vector) (cumEstimateCounts : int[]) =

        estimateMaps |> List.zip design
                     |> List.mapi 
                         (fun i ((maskedFactors, cov), estimateMap) ->
                                 let estimateMap = estimateMap |> Array.toList
                                 let predictorOffset = if i = 0 then 0 else cumEstimateCounts.[i - 1]
                                 let estCount = if i = 0 then cumEstimateCounts.[0] else cumEstimateCounts.[i] - cumEstimateCounts.[i - 1]
                                 match maskedFactors, cov with
                                     | ((h::t), _), None ->
                                         let factors = maskedFactors |> fst |> List.map fst
                                         let dimProd = factors |> List.map (fun f -> f.Cardinality) |> getDimProd
                                         estimateMap |> List.mapi (fun index estimateIndex -> 
                                                                          let subscripts = ind2sub index dimProd
                                                                          let levels = subscripts |> List.zip factors |> List.map (fun (f, s) -> f.Level(s))
                                                                          if estimateIndex < 0 then
                                                                              {
                                                                               Predictor = CategoricalPredictor(!!factors)
                                                                               Levels = levels
                                                                               Value = 0.0
                                                                               Std = 0.0
                                                                               IsDisabled = true 
                                                                               WaldChiSq = Double.NaN  
                                                                               PValue = Double.NaN                                                                  
                                                                              }
                                                                          else
                                                                              let estimateIndex = predictorOffset + estimateIndex
                                                                              {
                                                                               Predictor = CategoricalPredictor(!!factors)
                                                                               Levels = levels
                                                                               Value = beta.[estimateIndex]
                                                                               Std = Math.Sqrt(invHDiag.[estimateIndex])
                                                                               IsDisabled = false   
                                                                               WaldChiSq = Math.Pow(beta.[estimateIndex] / Math.Sqrt(invHDiag.[estimateIndex]), 2.0)  
                                                                               PValue = 1.0 - chicdf(1.0, Math.Pow(beta.[estimateIndex] / Math.Sqrt(invHDiag.[estimateIndex]), 2.0) )                                                                     
                                                                              }
                                                                      )
                                     | ([], _), Some cov ->
                                         if estCount = 1 then
                                             [{
                                              Predictor = NumericalPredictor cov
                                              Levels = []
                                              Value = beta.[predictorOffset]
                                              Std = Math.Sqrt(invHDiag.[predictorOffset])
                                              IsDisabled = false  
                                              WaldChiSq = Math.Pow(beta.[predictorOffset] / Math.Sqrt(invHDiag.[predictorOffset]), 2.0)  
                                              PValue = 1.0 - chicdf(1.0, Math.Pow(beta.[predictorOffset] / Math.Sqrt(invHDiag.[predictorOffset]), 2.0) )                                                                         
                                             }]  
                                         else
                                             [{
                                               Predictor = NumericalPredictor cov
                                               Levels = []
                                               Value = 0.0
                                               Std = 0.0
                                               IsDisabled = true 
                                               WaldChiSq = Double.NaN  
                                               PValue = Double.NaN                                                                  
                                             }]                                           
                                     | ((h::t), _), Some cov ->
                                         let factors = maskedFactors |> fst |> List.map fst
                                         let dimProd = factors |> List.map (fun f -> f.Cardinality) |> getDimProd
                                         estimateMap |> List.mapi (fun index estimateIndex -> 
                                                                      let subscripts = ind2sub index dimProd
                                                                      let levels = subscripts |> List.zip factors |> List.map (fun (f, s) -> f.Level(s))
                                                                      if estimateIndex < 0 then
                                                                          {
                                                                           Predictor = MixedInteraction(!!factors, cov)
                                                                           Levels = levels
                                                                           Value = 0.0
                                                                           Std = 0.0
                                                                           IsDisabled = true    
                                                                           WaldChiSq = Double.NaN
                                                                           PValue = Double.NaN                                                                
                                                                          }
                                                                      else
                                                                          let estimateIndex = predictorOffset + estimateIndex
                                                                          {
                                                                           Predictor = MixedInteraction(!!factors, cov)
                                                                           Levels = levels
                                                                           Value = beta.[estimateIndex]
                                                                           Std = Math.Sqrt(invHDiag.[estimateIndex])
                                                                           IsDisabled = false    
                                                                           WaldChiSq = Math.Pow(beta.[estimateIndex] / Math.Sqrt(invHDiag.[estimateIndex]), 2.0)   
                                                                           PValue = 1.0 - chicdf(1.0, Math.Pow(beta.[estimateIndex] / Math.Sqrt(invHDiag.[estimateIndex]), 2.0) )                                                                    
                                                                          }
                                                                  )                                      
                                     | ([], _), None -> []                                 
                           ) |> List.concat

    let getDeviance (resp : Vector) (pred : Vector) (obsW : Vector) (glmDistribution : GlmDistribution) =
        let resp = resp.AsExpr
        match glmDistribution with
            | Gaussian ->
                VectorExpr.EvalIn((resp - pred) .^ 2, None)
            | Poisson ->
                VectorExpr.EvalIn(VectorExpr.IfFunction((resp .= 0.0), (2.0 * pred.AsExpr), (2.0 * (resp .* VectorExpr.Log(resp ./ pred) - (resp - pred))) ), None)
            | Gamma ->
                VectorExpr.EvalIn(-2.0 * (VectorExpr.Log(resp ./ pred) - (resp ./ pred) + 1.0), None) 
            | Binomial -> 
                if obsW = Vector.Empty then
                    VectorExpr.EvalIn(VectorExpr.IfFunction((resp .= 0.0), (-2.0 * VectorExpr.Log(1.0 - pred.AsExpr)), (-2.0 * VectorExpr.Log(pred.AsExpr))), None)
                else
                    let pred = obsW .* pred.AsExpr
                    VectorExpr.EvalIn(2.0 * VectorExpr.IfFunction(resp .= 0.0,
                                                                  obsW .* VectorExpr.Log(obsW ./ (obsW - pred)),
                                                                  VectorExpr.IfFunction(resp .= obsW,
                                                                                        resp .* VectorExpr.Log(resp ./ pred), 
                                                                                        resp .* VectorExpr.Log(resp ./ pred) + (obsW - resp) .* VectorExpr.Log((obsW - resp) ./ (obsW - pred)))),
                                      None)


    let getPearsonChi (resp : Vector) (pred : Vector)(obsW : Vector) (glmDistribution : GlmDistribution) =
        let resp = resp.AsExpr
        match glmDistribution with
            | Gaussian ->
                VectorExpr.EvalIn((resp - pred) .^ 2, None)
            | Poisson ->
                VectorExpr.EvalIn(((resp - pred) ./ VectorExpr.Sqrt(pred.AsExpr)) .^ 2, None)
            | Gamma ->
                VectorExpr.EvalIn(((resp ./ pred) - 1.0) .^ 2, None) 
            | Binomial ->
                if obsW = Vector.Empty then
                    VectorExpr.EvalIn(((resp - pred) .^ 2) ./ (pred.AsExpr .* (1.0 - pred.AsExpr)), None)
                else
                    let resp = resp ./ obsW
                    VectorExpr.EvalIn(((resp - pred) .^ 2) ./ (pred.AsExpr .* (1.0 - pred.AsExpr)) .* obsW, None)

    let getLogLikelihoodPart (resp : Vector) (pred : Vector) (obsW : Vector) (glmDistribution : GlmDistribution) =
        let resp = resp.AsExpr
        match glmDistribution with
            | Gaussian ->
                VectorExpr.EvalIn((resp - pred) .^ 2, None), Vector.Empty
            | Poisson ->
                VectorExpr.EvalIn((resp .* VectorExpr.Log(pred.AsExpr) - pred.AsExpr), None), Vector.Empty
            | Gamma ->
                VectorExpr.EvalIn((VectorExpr.Log(resp ./ pred) - (resp ./ pred)), None), VectorExpr.EvalIn(VectorExpr.Log(resp), None)
            | Binomial ->
                if obsW = Vector.Empty then
                    VectorExpr.EvalIn(VectorExpr.IfFunction((resp .= 0.0), 
                                                            (VectorExpr.Log(1.0 - pred.AsExpr)),
                                                            (VectorExpr.Log(pred.AsExpr))), None), Vector.Empty
                else
                    VectorExpr.EvalIn(resp .* VectorExpr.Log(pred.AsExpr) + (obsW - resp) .* VectorExpr.Log(1.0 - pred.AsExpr), None), Vector.Empty

    let getLogLikelihood (logLikelihoodPart : float * float) (N : int64) (phi : float) (glmDistribution : GlmDistribution) =
        let L1, L2 = logLikelihoodPart
        match glmDistribution with
            | Gaussian ->
                -0.5 * (L1 / phi + float(N) * Math.Log(2.0 * Math.PI * phi))
            | Poisson ->
                L1 / phi
            | Gamma ->
                1.0 / phi * L1 - L2 - (1.0 / phi * Math.Log(phi) + lnGamma(1.0 / phi)) * float(N)
            | Binomial -> 
                L1 / phi

    let getPhi (dispersion : GlmDispersion) (glmDistribution : GlmDistribution) (N : int64) (dof : int) (pearsonChi : float) (deviance : float) =
        let N_dof = float(N - int64(dof))
        let N = float N
        match dispersion with
            | MaxLikelihood ->
                match glmDistribution with
                    | Gaussian -> deviance / N
                    | Poisson -> 1.0
                    | Binomial -> 1.0
                    | Gamma -> 
                        let dev= deviance / N_dof
                        let scale = (6.0*N_dof+2.0*N*dev)/(dev*(6.0*N_dof+N*dev))
                        let rec getScale (currScale : float) (d : float) (N' : float) (iter : int) (maxIter : int) =
                            let gamLogL' = gammaLogL' currScale d N'
                            if Double.IsNaN(currScale) || iter >= maxIter then Double.NaN
                            elif Math.Abs(gamLogL') <= 1e-10 then
                                currScale
                            else 
                                getScale (currScale - (gammaLogL' currScale d N') / ((trigamma currScale) - 1.0 / currScale)) d N' (iter + 1) maxIter
                        1.0 / getScale scale deviance N 0 100
            | Deviance -> deviance / N_dof
            | Pearson -> pearsonChi / N_dof

    let getScale (phi : float) (glmDistribution : GlmDistribution) =
        match glmDistribution with
            | Gamma -> 1.0 / phi
            | Gaussian -> Math.Sqrt(phi)
            | _ -> phi

    let getGoodnessOfFit (response : CovariateExpr) (design : ((int64 * int64 * int -> seq<UInt16Vector list> * int[]) option * Covariate option) list)
                         (estimateMaps : int[] list)
                         (glmDistribution : GlmDistribution) (glmLink : GlmLink) (beta : Vector)
                         (obsCount : int64) (sliceLength : int) (cumEstimateCounts : int[]) =

        let response, obsWCov =
           match glmDistribution, response with
               | Binomial, BinaryFunction(x, y, f, _) ->
                   match f(Vector.Empty.AsExpr, Vector.Empty.AsExpr) with
                       | VectorExpr.BinaryFunction(_, _, _, "./") -> x.AsCovariate, Some y.AsCovariate
                       | _ -> response.AsCovariate, None 
               | _ -> response.AsCovariate, None 
 
        let processorCount = Environment.ProcessorCount
        let processorChunk = obsCount / int64(processorCount)
        let deviance, pearsonChi, likelihoodPart1, likelihoodPart2, validObs =
            seq{0..processorCount - 1} 
                |> Seq.map (fun i -> (int64(i) * processorChunk), if i = processorCount - 1 then obsCount - 1L else (int64(i + 1) * processorChunk - 1L))
                |> Seq.toArray 
                |> Array.Parallel.map (fun (fromObs, toObs) -> 
                                            let respSlices = response.GetSlices(fromObs, toObs, sliceLength) 
                                            let obsWSlices = 
                                                match obsWCov with
                                                    | Some(obsW) -> obsW.GetSlices(fromObs, toObs, sliceLength) 
                                                    | None -> respSlices |> Seq.map (fun _ -> Vector.Empty)

                                            estimateMaps |> List.zip
                                                (design |> List.map (fun (factorsOpt, covOpt) ->
                                                                             factorsOpt |> Option.map (fun f -> f(fromObs, toObs, sliceLength)), covOpt
                                                                                        |> Option.map (fun c -> c.GetSlices(fromObs, toObs, sliceLength))))
                                                   |> List.map (fun ((x,y),z) -> x,y,z)
                                                   |> zipDesign |> Seq.zip3 respSlices obsWSlices
                                                   |> Seq.fold (fun (devSum, pearsonSum, logLSum1, logLSum2, validObs) (resp, obsW, slice) ->
                                                                    let sliceLength = resp.Length
                                                                    use xbeta = getXBeta slice beta sliceLength cumEstimateCounts
                                                                    use pred = getPred xbeta glmLink
                                                                    use isValid = pred .= pred
                                                                    use isValidPred = pred.[isValid]
                                                                    use deviance = getDeviance resp pred obsW glmDistribution
                                                                    use pearsonChi = getPearsonChi resp pred obsW glmDistribution
                                                                    let likelihoodPart1, likelihoodPart2  = getLogLikelihoodPart resp pred obsW glmDistribution 
                                                                    let dsum = MklFunctions.Sum_Array_NotNan(sliceLength, deviance.NativeArray)
                                                                    let psum = MklFunctions.Sum_Array_NotNan(sliceLength, pearsonChi.NativeArray) 
                                                                    let lsum1 = MklFunctions.Sum_Array_NotNan(sliceLength, likelihoodPart1.NativeArray) 
                                                                    let lsum2 = if likelihoodPart2 = Vector.Empty then 0.0 else MklFunctions.Sum_Array_NotNan(sliceLength, likelihoodPart2.NativeArray)                                            
                                                                    (devSum + (if Double.IsNaN(dsum) then 0.0 else dsum)),
                                                                     (pearsonSum + (if Double.IsNaN(psum) then 0.0 else psum)),
                                                                      (logLSum1 + (if Double.IsNaN(lsum1) then 0.0 else lsum1)),
                                                                       (logLSum2 + (if Double.IsNaN(lsum2) then 0.0 else lsum2)),
                                                                       (validObs + isValidPred.LongLength)
                                                               ) (0.0, 0.0, 0.0, 0.0, 0L)
                                        )
                |> Array.reduce (fun (d, p, l1, l2, N) (d', p', l1', l2', N') -> d + d', p + p', l1 + l1', l2 + l2', N + N')

        let dof = beta.Length 
        let N = validObs
        let phi = getPhi MaxLikelihood glmDistribution N dof pearsonChi deviance
        let logLikelihood = getLogLikelihood (likelihoodPart1, likelihoodPart2) N phi glmDistribution
        {
         ValidObsCount = N
         DoF = dof
         LogLikehood = logLikelihood
         Deviance = deviance
         PearsonChi = pearsonChi
         //AIC = -2.0 * logLikelihood + 2.0 * float(dof)
         MLScale = getScale phi glmDistribution
         MLPhi = phi
        }

    let getInitBeta (cumEstCount : int[]) (link : GlmLink) (includeIntercept : bool) (predictors : Predictor list) (meanResponseEstimate : float) =
        let estimateCount = cumEstCount.[cumEstCount.Length - 1]
        let beta = new Vector(estimateCount, 0.0)
        let init = 
            match link with
                | Id ->
                    meanResponseEstimate
                | Log ->
                    Math.Log(meanResponseEstimate)
                | Inverse ->
                    1.0 / meanResponseEstimate
                | Logit ->
                    Math.Log(meanResponseEstimate / (1.0 - meanResponseEstimate))
                | CLogLog ->
                    Math.Log(-Math.Log(1.0 - meanResponseEstimate))
                | Probit ->
                    let mutable res = meanResponseEstimate
                    MklFunctions.D_CdfNormInv_Array(1L, &&res, &&res)
                    res

        if includeIntercept then
            beta.[0] <- init
        else
            match predictors |> List.tryFindIndex (fun p -> match p with | CategoricalPredictor(_) -> true | _ -> false) with
                | Some(i) ->
                    let pEstCount = if i = 0 then cumEstCount.[0] else cumEstCount.[i] - cumEstCount.[i - 1]
                    let offset = if i = 0 then 0 else cumEstCount.[i - 1]
                    for j in 0..pEstCount - 1 do
                        beta.[offset + j] <- init
                | _ -> 
                    for j in 0..estimateCount - 1 do
                        beta.[j] <- init
        beta

    let checkResponse (resp : Covariate) (glmDistribution : GlmDistribution) (sliceLen : int) =
        match glmDistribution with
            | Gaussian -> ()
            | Gamma ->
                let invalidCount =
                    resp.GetSlices(0L, resp.Length - 1L, sliceLen)
                        |> Seq.map (fun v -> 
                                         use isInvalid = BoolVectorExpr.EvalIn(v.AsExpr .<= 0.0, None)
                                         v.[isInvalid].LongLength
                                   )
                        |> Seq.reduce (+)
                if invalidCount > 0L then raise (new ArgumentException("Gamma response must be > 0 or NaN"))
            | Poisson ->
                let invalidCount =
                    resp.GetSlices(0L, resp.Length - 1L, sliceLen)
                        |> Seq.map (fun v -> 
                                         use isInvalid = BoolVectorExpr.EvalIn((v.AsExpr .< 0.0) .|| ( VectorExpr.Floor(v.AsExpr) .<> v.AsExpr) , None)
                                         v.[isInvalid].LongLength
                                   )
                        |> Seq.reduce (+)
                if invalidCount > 0L then raise (new ArgumentException("Poisson response must be 0 1 2... or NaN"))
            | Binomial ->
                let invalidCount =
                    resp.GetSlices(0L, resp.Length - 1L, sliceLen)
                        |> Seq.map (fun v -> 
                                         use isInvalid = BoolVectorExpr.EvalIn((v.AsExpr .< 0.0) .|| (v.AsExpr .> 1.0) , None)
                                         v.[isInvalid].LongLength
                                   )
                        |> Seq.reduce (+)
                if invalidCount > 0L then raise (new ArgumentException("Binomial response must be in [0, 1] or NaN"))


    let fitModel (response : CovariateExpr) (predictors : Predictor list) (includeIntercept : bool)
                 (glmDistribution : GlmDistribution) (glmLink : GlmLink) (sliceLength : int) (maxIter : int) (eps : float) =
        let minLen = min (predictors |> List.map (fun p -> p.MinLength) |> List.min) response.MinLength
        let maxLen = max (predictors |> List.map (fun p -> p.MaxLength) |> List.max) response.MaxLength
        if minLen <> maxLen then raise (new ArgumentException("Glm predictors data length mismatch"))
        let length = minLen
        let obsWCov =
           match glmDistribution, response with
               | Binomial, BinaryFunction(_, y, f, _) ->
                   match f(Vector.Empty.AsExpr, Vector.Empty.AsExpr) with
                       | VectorExpr.BinaryFunction(_, _, _, "./") -> Some y
                       | _ -> None
               | _ -> None 
           |> Option.map (fun covExpr -> covExpr.AsCovariate)
        let respCov = response.AsCovariate
        checkResponse respCov glmDistribution sliceLength
        let meanReponseSlice = (respCov.GetSlices(0L, (min (int64(sliceLength)) length) - 1L, sliceLength) |> Seq.map (fun v -> Vector.Mean (v.[v .= v])))
                                                                                                           |> Seq.take 1 |> Seq.nth 0

        let estimableDesign = getEstimableDesign predictors includeIntercept
        let estimateCounts = estimableDesign |> List.map getEstimateCount |> List.toArray
        let cumEstimateCounts = Array.sub (estimateCounts |> Array.scan (+) 0) 1 estimateCounts.Length
        let initBeta = getInitBeta cumEstimateCounts glmLink includeIntercept predictors meanReponseSlice
        let design = estimableDesign |> List.map (fun ((factors, _), cov) -> (getCategoricalSlicer factors), cov |> Option.map (fun c -> c.AsCovariate)) 
        let estimateMaps = estimableDesign |> List.map getPredictorEstimateMap
        let beta, invHDiag, iter, converged, estimateMapsFinal, cumEstimateCountsFinal = iwls respCov obsWCov design estimateMaps glmDistribution glmLink initBeta None maxIter 0 length sliceLength cumEstimateCounts eps Vector.Empty false
        if converged then
            let parameters = getParameterEstimates estimableDesign beta estimateMapsFinal invHDiag cumEstimateCountsFinal
            let goodnessOfFit = getGoodnessOfFit response design estimateMapsFinal glmDistribution glmLink beta length sliceLength cumEstimateCountsFinal
            {
             Response = response
             Predictors = predictors
             Distribution = glmDistribution
             Link = glmLink
             HasIntercept = includeIntercept
             GoodnessOfFit = goodnessOfFit |> Some
             Beta = beta
             InvHDiag = invHDiag
             Iter = iter
             Parameters = parameters
             EstimateMaps = estimateMapsFinal
             CumEstimateCount = cumEstimateCountsFinal
            }
        else 
            {
             Response = response
             Predictors = predictors
             Distribution = glmDistribution
             Link = glmLink
             HasIntercept = includeIntercept
             GoodnessOfFit = None
             Beta = beta
             InvHDiag = invHDiag
             Iter = iter
             Parameters = []
             EstimateMaps = estimateMapsFinal
             CumEstimateCount = cumEstimateCountsFinal
            }

    let getPermutedFactor (baseFactor : Factor) (factorToPermute : Factor) =
        let baseLevelMap = new Dictionary<string, int>()
        let permuteFactorMap = new Dictionary<int, string>()
        let permuteFactorMapRev = new Dictionary<string, int>()
        [|0..baseFactor.Cardinality - 1|] |> Array.iter (fun index -> baseLevelMap.Add(baseFactor.Level(index), index))
        [|0..factorToPermute.Cardinality - 1|] |> Array.iter (fun index -> permuteFactorMap.Add(index, factorToPermute.Level(index))
                                                                           permuteFactorMapRev.Add(factorToPermute.Level(index), index))
        let naSet = Factor.NAs |> Set.ofArray
        let allLevelsInBase = 
            [|0..factorToPermute.Cardinality - 1|] |> Array.map (fun index ->
                                                                   let level = factorToPermute.Level(index) in baseLevelMap.ContainsKey(level) || naSet.Contains(level))
                                                  |> Array.reduce (&&)
        if not allLevelsInBase then raise (new ArgumentException("Permuted factor: all not NA levels must be in base factor"))

        //add NAs if not there
        [|0..factorToPermute.Cardinality - 1|] |> Array.iter (fun index -> if not <| baseLevelMap.ContainsKey(factorToPermute.Level(index)) then
                                                                              let count = baseLevelMap.Count
                                                                              baseLevelMap.Add(factorToPermute.Level(index), count))

        [|0..baseFactor.Cardinality - 1|] |> Array.iter (fun index -> 
                                                            let level = baseFactor.Level(index)
                                                            if not <| permuteFactorMapRev.ContainsKey(level) then
                                                                let count = permuteFactorMapRev.Count 
                                                                permuteFactorMapRev.Add(level, count)
                                                                permuteFactorMap.Add(count, level)
                                                       )
        let permutation = 
            [|0..permuteFactorMap.Count - 1|] |> Array.map (fun index -> let level = permuteFactorMap.[index]
                                                                         baseLevelMap.[level], level)
        FactorExpr.Permute(factorToPermute.AsExpr, permutation)

    let substituteVar (data : DataFrame) (statVar : StatVariable)  =
        match statVar with
            | StatVariable.Factor(f) -> 
                let factorToPermute = data.[f.Name].AsFactor
                (getPermutedFactor f factorToPermute).AsFactor |> (!!)
            | StatVariable.Covariate(c) -> data.[c.Name]

type GlmModel with

    member this.Predict() : Covariate = this.Predict(DataFrame.Empty)

    member this.Predict(data : DataFrame) : Covariate =
        let predictors =
            if data = DataFrame.Empty then this.Predictors
            else this.Predictors |> List.map (Predictor.Substitute (Glm.substituteVar data))
        let minLen = predictors |> List.map (fun p -> p.MinLength) |> List.min
        let maxLen = predictors |> List.map (fun p -> p.MaxLength) |> List.max
        if minLen <> maxLen then raise (new ArgumentException("Glm predictors data length mismatch"))
        let length = minLen
        let beta = this.Beta
        let glmLink = this.Link
        let estimableDesign = Glm.getEstimableDesign predictors this.HasIntercept
        let cumEstimateCounts = this.CumEstimateCount
        let design = estimableDesign |> List.map (fun ((factors, _), cov) -> (Glm.getCategoricalSlicer factors), cov |> Option.map (fun c -> c.AsCovariate)) 
        let estimateMaps = this.EstimateMaps

        let covariateStorage = 
             {
              new ICovariateStorage with
                  member __.Length = length
                  member __.GetSlices(fromObs : int64, toObs : int64, sliceLength : int) =
                    estimateMaps |> List.zip
                        (design |> List.map (fun (factorsOpt, covOpt) ->
                                                        factorsOpt |> Option.map (fun f -> f(fromObs, toObs, sliceLength)), covOpt
                                                                   |> Option.map (fun c -> c.GetSlices(fromObs, toObs, sliceLength))))
                            |> List.map (fun ((x,y),z) -> x,y,z)
                            |> Glm.zipDesign
                            |> Seq.map (fun slice ->
                                            let sliceLen = 
                                                match slice with
                                                    | (Some(v::_, _), _, _)::_ -> v.Length
                                                    | (_, Some(v), _)::_ -> v.Length
                                                    | _ -> 0
                                            let xbeta = Glm.getXBeta slice beta sliceLen cumEstimateCounts
                                            Glm.getPred xbeta glmLink                               
                                        )
             }
        new Covariate("Predicted", covariateStorage)     
        





       
        
        
        
        




