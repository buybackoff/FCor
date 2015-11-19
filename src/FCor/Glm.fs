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
    | Ln
    | Log of float
    | Power of float
    | Logit
    | Probit
    | LogLog
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
    | DisableAllLevels

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
     AIC : float
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
            sb.AppendLine <| sprintf "%-20s%12G" "AIC" (float this.AIC) |> ignore
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
     GoodnessOfFit : GlmGoodnessOfFit
     Beta : Vector
     InvHDiag : Vector
     Iter : int
     Parameters : GlmParameterEstimate list
    }
    member this.AsString =
        let sb = new StringBuilder()
        sb.AppendLine <| "Generalized Linear Model" |> ignore
        sb.AppendLine <| sprintf "%-15s %A" "Distribution" this.Distribution |> ignore
        sb.AppendLine <| sprintf "%-15s %A" "Link" this.Link |> ignore
        sb.AppendLine <| sprintf "%-15s %s" "Response" this.Response.Name |> ignore
        sb.AppendLine <| sprintf "%-15s %s" "Predictors" (String.Join("+", this.Predictors |> List.map (fun p -> p.Name))) |> ignore
        sb.AppendLine <| sprintf "%-15s %b" "Intercept" this.HasIntercept |> ignore
        sb.AppendLine <| sprintf "%-15s %i" "Valid Obs Count" this.GoodnessOfFit.ValidObsCount |> ignore
        sb.AppendLine <| sprintf "Algorithm converged in %d iterations" this.Iter |> ignore
        sb.AppendLine("") |> ignore
        sb.Append(this.GoodnessOfFit.AsString) |> ignore
        sb.AppendLine("") |> ignore
        sb.AppendLine <| "Parameter Estimates" |> ignore
        sb.AppendLine <| sprintf "%-15s  %-15s  %-3s  %-12s %-12s %-12s %-12s" "Predictor" "Level" "DoF" "Estimate" "StdError" "Wald ChiSq" "Pr > ChiSq" |> ignore
        this.Parameters |> List.iter (fun prm -> 
                                          sb.AppendLine <| sprintf "%-15s  %-15s  %3d  %12G %12G %12G %12G" prm.Predictor.Name (String.Join("*", prm.Levels))
                                                                   (if prm.IsDisabled then 0 else 1) prm.Value prm.Std prm.WaldChiSq prm.PValue |> ignore
                                     )
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

    let importCsv (path : string) (sampleOnly : bool) =
        let delimiter = ','
        use sr = new StreamReader(path)
        let firstN = 1000
        let N = 1000

        let isNotNumerics (s : string) =
            s <> "#N/A" && s <> String.Empty && s <> "N/A" && s <> "." && (match Single.TryParse(s) with | (true, _) -> false | _ -> true)

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

        let headers = (take 1).[0].Split(",".ToCharArray())
        let firstChunk = take firstN |> Array.map (fun line -> line.Split(",".ToCharArray()))
        let isFactor = Array.init headers.Length (fun col -> col |> getColArray firstChunk |> Array.exists isNotNumerics)
        let factorStorage = Array.init headers.Length (fun col -> new FactorStorage())
        let covStorage = Array.init headers.Length (fun col -> new CovariateStorageFloat32())
        let covSlices = Array.init headers.Length (fun col -> if isFactor.[col] then Array.create 0 0.0f else Array.create N 0.0f)
        let slices = Array.init headers.Length (fun col -> Array.create N String.Empty)

        isFactor |> Array.iteri (fun col isFactor -> 
                                     let colArray = getColArray firstChunk col
                                     if isFactor then
                                         factorStorage.[col].SetSlice(0L, colArray)
                                     else
                                         covStorage.[col].SetSlice(0L, colArray |> Array.map (fun s -> match Single.TryParse(s) with | (true, v) -> v | _ -> Single.NaN))
                                )
        if not sampleOnly then
            let rec processLine (fromObs : int64) (N : int) (row : int) (isFactor : bool[]) (delimiter : char) =
                let line = sr.ReadLine()
                if not <| String.IsNullOrEmpty(line) then
                    line.Split(",".ToCharArray()) |> Array.iteri (fun col s -> slices.[col].[row] <- s)
                    if row = N - 1 then
                        isFactor |> Array.iteri (fun col isFactor -> 
                                                    if not isFactor then
                                                        covSlices.[col] <- slices.[col] |> Array.Parallel.map (fun s -> let f = ref 0.0f
                                                                                                                        if Single.TryParse(s, f) then !f else Single.NaN) 
                                                )
                        isFactor |> Array.Parallel.iteri (fun col isFactor ->
                                                                if isFactor then factorStorage.[col].SetSlice(fromObs, slices.[col])
                                                                else
                                                                    covStorage.[col].SetSlice(fromObs, covSlices.[col]))
                        processLine (fromObs + int64(N)) N 0 isFactor delimiter 
                    else
                        processLine fromObs N (row + 1) isFactor delimiter 
                else
                    if row > 0 then
                        isFactor |> Array.iteri (fun col isFactor -> 
                                                    if not isFactor then
                                                        covSlices.[col] <- slices.[col] |> Array.Parallel.map (fun s -> let f = ref 0.0f
                                                                                                                        if Single.TryParse(s, f) then !f else Single.NaN) 
                                                )
                        isFactor |> Array.Parallel.iteri (fun col isFactor ->
                                                                if isFactor then factorStorage.[col].SetSlice(fromObs, Array.sub slices.[col] 0 row)
                                                                else covStorage.[col].SetSlice(fromObs, Array.sub (covSlices.[col]) 0 row))
                    else ()

            processLine (int64(firstN)) N 0 isFactor delimiter 

        isFactor |> Array.zip headers |> Array.mapi (fun col (name, isFactor) ->
                                                         if isFactor then 
                                                             StatVariable.Factor(new Factor(name, factorStorage.[col]))
                                                         else
                                                             Covariate(new Covariate(name, covStorage.[col]))
                                                    ) |> Array.toList

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

    let rec zipDesign (design : ((seq<'T list> * int[] * int[]) option * seq<'S> option) list) : seq<(('T list * int[] * int[]) option * 'S option) list> =
        match design with
            | [] -> Seq.singleton [] 
            | head::[] ->
                match head with
                    | Some(factors, estMap, dimProd), None ->
                        factors |> Seq.map (fun f -> [Some(f, estMap, dimProd), None])
                    | None, Some(cov) ->
                        cov |> Seq.map (fun c -> [None, Some c])
                    | Some(factors, estMap, dimProd), Some(cov) -> 
                        cov |> Seq.zip factors |> Seq.map (fun (f, c) -> [Some(f, estMap, dimProd), Some c])
                    | None, None -> Seq.singleton []
            | head::tail -> 
                (zipDesign tail) |> Seq.zip (zipDesign [head]) |> Seq.map (fun (x,y) -> x @ y)
            
    let getDimProd (size : int list) =
        (size |> List.scan (*) 1 |> List.rev).Tail |> List.rev

    let sub2ind (dimProd : int[]) (subscripts : int[]) =
        subscripts |> Array.mapi (fun i x -> x * dimProd.[i]) |> Array.fold (+) 0

    let rec ind2sub (index : int) (dimProd : int list) =
        match dimProd with
            | [] -> []
            | head::tail -> 
                (index / head)::(ind2sub (index % head) tail)

    let getNACount (factor : Factor) =
        let nas = new Set<string>(Factor.NAs)
        [0..factor.LevelCount - 1] |> List.filter (fun index -> nas.Contains(factor.GetLevel(index))) |> List.length

    let getIndexOfMaxLevel (factor : Factor) =
        let nas = new Set<string>(Factor.NAs)
        let validlevels = Array.init (factor.LevelCount) (fun i -> i, factor.GetLevel(i)) |> Array.sortBy snd
                          |> Array.filter (fun (_, level) -> not <| nas.Contains(level))
        if validlevels.Length = 0 then -1
        else
            validlevels.[validlevels.Length - 1] |> fst


    let getDisabledSubscripts (maskedFactors : (Factor * FactorMask) list) =
        let nas = new Set<string>(Factor.NAs)
        maskedFactors |> List.map (fun (factor, mask) -> 
                                        match mask with
                                            | EnableAllLevels -> []
                                            | DisableOneLevel ->
                                                let indexOfMaxLevel = getIndexOfMaxLevel factor
                                                if indexOfMaxLevel >= 0 then [indexOfMaxLevel]
                                                else []
                                            | DisableAllLevels ->
                                                [0..factor.LevelCount - 1] |> List.filter (fun index -> not <| nas.Contains(factor.GetLevel(index)))
                                    )
                      |> cartesian

    let getNASubscripts (maskedFactors : (Factor * FactorMask) list) =
        let nas = new Set<string>(Factor.NAs)
        maskedFactors |> List.map (fun (factor, _) -> 
                                       [0..factor.LevelCount - 1] |> List.filter (fun index -> nas.Contains(factor.GetLevel(index)))
                                    )
                      |> cartesian

    let getDisabled (maskedFactors : (Factor * FactorMask) list) =
        let factors = maskedFactors |> List.map fst |> List.toArray
        let allLevelCount = factors |> Array.fold (fun count f -> count * f.LevelCount) 1
        let isDisabled = Array.create allLevelCount false
        let dimProd = maskedFactors |> List.map fst |> List.map (fun f -> f.LevelCount) |> getDimProd |> List.toArray
        maskedFactors |> getDisabledSubscripts
                      |> List.iter (fun subscripts -> 
                                        let disabledIndex = subscripts |> List.toArray |> sub2ind dimProd
                                        isDisabled.[disabledIndex] <- true
                                   )
        isDisabled

    let getIsNA (maskedFactors : (Factor * FactorMask) list) =
        let factors = maskedFactors |> List.map fst |> List.toArray
        let allLevelCount = factors |> Array.fold (fun count f -> count * f.LevelCount) 1
        let isNA = Array.create allLevelCount false
        let dimProd = maskedFactors |> List.map fst |> List.map (fun f -> f.LevelCount) |> getDimProd |> List.toArray
        maskedFactors |> getNASubscripts
                      |> List.iter (fun subscripts -> 
                                        let naIndex = subscripts |> List.toArray |> sub2ind dimProd
                                        isNA.[naIndex] <- true
                                   )
        isNA

    let getCategoricalSlicer (maskedFactors : (Factor * FactorMask) list) =
        match maskedFactors with
            | h::t ->
                let factors = maskedFactors |> List.map fst 
                let dimProd = maskedFactors |> List.map fst |> List.map (fun f -> f.LevelCount) |> getDimProd |> List.toArray
                let isDisabled = getDisabled maskedFactors
                let isNA = getIsNA maskedFactors
                let cumDisabledCount = Array.sub (isDisabled |> Array.scan (fun cum x -> if x then cum + 1 else cum) 0) 1 isDisabled.Length
                let cumNACount = Array.sub (isNA |> Array.scan (fun cum x -> if x then cum + 1 else cum) 0) 1 isNA.Length
                let estimateMap = Array.init isDisabled.Length (fun i -> if isDisabled.[i] then -1
                                                                         elif isNA.[i] then -2
                                                                         else i - cumDisabledCount.[i] - cumNACount.[i])
                let slicerFun =
                    fun (fromObs : int64, toObs : int64, sliceLen : int) ->
                        let slices = factors |> List.map (fun f -> f.GetSlices(fromObs, toObs, sliceLen)) |> zipN
                        slices, estimateMap, dimProd
                slicerFun |> Some
            | [] -> None

    let getEstimateCount (maskedPred : (Factor * FactorMask) list * CovariateExpr option) =
        let maskedFactors, covariate = maskedPred
        let numCount = 
            match covariate with
                | Some(_) -> 1
                | None -> 0
        let catCount = 
            maskedFactors |> List.fold (fun count (factor, mask) -> 
                                            let naCount = getNACount factor
                                            count * match mask with
                                                        | EnableAllLevels -> factor.LevelCount - naCount
                                                        | DisableOneLevel ->
                                                            let maxLevelIndex = getIndexOfMaxLevel factor
                                                            if maxLevelIndex >= 0 then 
                                                                factor.LevelCount - 1 - naCount
                                                            else naCount
                                                        | DisableAllLevels -> 0) 1
        match maskedFactors with
            | [] -> numCount
            | _ -> catCount

    let updateFactorMask (maskedFactors : (Factor * FactorMask) list) (factor : Factor) =
        if maskedFactors |> List.exists (fun (f, _) -> f = factor || factor.LevelCount = 1) then
            match maskedFactors with
                | [] -> []
                | (f, m) :: [] ->
                    if f = factor then
                        [f, DisableAllLevels]
                    elif factor.LevelCount = 1 then
                        match m with
                            | EnableAllLevels -> [f, DisableOneLevel]
                            | DisableOneLevel -> [f, DisableOneLevel]
                            | DisableAllLevels -> [f, DisableAllLevels]
                    else
                        maskedFactors
                | _ ->
                    maskedFactors |> List.map (fun (f, mask) -> 
                                                   if f = factor && factor.LevelCount > 1 then f, mask 
                                                   else
                                                       match mask with
                                                           | EnableAllLevels -> f, DisableOneLevel
                                                           | DisableOneLevel -> f, DisableOneLevel
                                                           | DisableAllLevels -> f, DisableAllLevels
                                              )
        else maskedFactors

    let updatePredictorMask (maskedPred : (Factor * FactorMask) list * CovariateExpr option) (pred : Factor list * CovariateExpr option) =
        match maskedPred, pred with
            | (maskedFactors, Some(maskedCovExpr)), (factors, Some(covExpr)) when maskedCovExpr.Vars = covExpr.Vars && maskedCovExpr.Name = covExpr.Name ->
                factors |> List.fold updateFactorMask maskedFactors, Some(maskedCovExpr)
            | (maskedFactors, None), (factors, None) ->
                factors |> List.fold updateFactorMask maskedFactors, None
            | _ -> maskedPred

    let rec getMaskedPredictors (predictors : (Factor list * CovariateExpr option) list) =
        match predictors with
            | [] -> []
            | (factors, covariate) :: tail -> 
                let h = tail |> List.fold updatePredictorMask (factors |> List.map (fun f -> f, EnableAllLevels), covariate)
                let t = getMaskedPredictors tail
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
            | Ln -> Vector.Exp(xbeta)
            | Log(d) -> Vector.Exp(xbeta * Math.Log(d))
            | Power(d) -> xbeta .^ (1.0 / d)
            | Logit -> Vector.Exp(xbeta) ./ (1.0 + Vector.Exp(xbeta))
            | LogLog -> Vector.Exp(-Vector.Exp(-xbeta))
            | CLogLog -> 1.0 - Vector.Exp(-Vector.Exp(xbeta))
            | Probit -> Vector.Normcdf(xbeta)

    let get_u (resp : Vector) (pred : Vector) (xbeta : Vector) (glmLink : GlmLink) (glmDistribution : GlmDistribution) =
        match glmDistribution, glmLink with
            | Gaussian, Id -> 
                (resp - pred)
            | Gaussian, Ln ->
                ((resp - pred) .* pred)
            | Gaussian, Log(d) -> 
                (resp - pred) * Math.Log(d) .* pred
            | Gaussian, Power(d) ->
                ((resp - pred) * d) .* (pred .^ (1.0 - 1.0 / d))
            | Gamma, Id -> 
                (resp - pred) ./ (pred .* pred)
            | Gamma, Ln ->
                VectorExpr.EvalIn((resp.AsExpr - pred) ./ pred, None)
            | Gamma, Log(d) -> 
                ((resp - pred) ./ pred) * Math.Log(d)
            | Gamma, Power(d) ->
                ((resp - pred) * d) .* (pred .^ (-1.0 - 1.0 / d))
            | Poisson, Id -> 
                (resp - pred) ./ pred
            | Poisson, Ln ->
                resp - pred
            | Poisson, Log(d) -> 
                (resp - pred) * Math.Log(d)
            | Poisson, Power(d) ->
                ((resp - pred) * d) .* (pred .^ (-1.0 / d))
            | Binomial, Logit -> resp - pred
            | Binomial, LogLog -> -(resp - pred) ./ (1.0 - pred) .* Vector.Log(pred)
            | Binomial, CLogLog -> -(resp - pred) ./ pred .* Vector.Log(1.0 - pred)
            | Binomial, Probit -> 
                (resp - pred) ./ (pred .* (1.0 - pred)) .* (Vector.Exp(-0.5 * xbeta .* xbeta) / (Math.Sqrt(2.0 * Math.PI)))
            | _ -> raise (new InvalidOperationException())

    let getWeight (pred : Vector) (xbeta : Vector) (glmLink : GlmLink) (glmDistribution : GlmDistribution) =
        match glmDistribution, glmLink with
            | Gaussian, Id -> 
                let xbeta = xbeta.AsExpr
                VectorExpr.EvalIn(VectorExpr.IfFunction((xbeta .= xbeta), VectorExpr.Scalar(1.0), VectorExpr.Scalar(Double.NaN)), None) 
            | Gaussian, Ln ->
                pred .* pred
            | Gaussian, Log(d) -> 
                Math.Log(d) * pred * Math.Log(d) .* pred
            | Gaussian, Power(d) ->
                d * d * (pred .^ (2.0 - 2.0 / d))
            | Gamma, Id -> 
                1.0 ./ (pred .*pred)
            | Gamma, Ln ->
                let xbeta = xbeta.AsExpr
                VectorExpr.EvalIn(VectorExpr.IfFunction((xbeta .= xbeta), VectorExpr.Scalar(1.0), VectorExpr.Scalar(Double.NaN)), None) 
            | Gamma, Log(d) -> 
                new Vector(pred.Length, Math.Log(d) * Math.Log(d))
            | Gamma, Power(d) ->
                d * d / (pred .^ (2.0 / d))
            | Poisson, Id -> 
                1.0 ./ pred
            | Poisson, Ln ->
                pred
            | Poisson, Log(d) -> 
                Math.Log(d) * Math.Log(d) * pred
            | Poisson, Power(d) ->
                d * d * (pred .^ (1.0 - 2.0 / d))
            | Binomial, Logit -> pred .* (1.0 - pred)
            | Binomial, LogLog -> pred .* Vector.Log(pred) .* Vector.Log(pred) ./ (1.0 - pred)
            | Binomial, CLogLog -> (1.0 - pred) .* (Vector.Log(1.0 - pred) ./ pred) .* Vector.Log(1.0 - pred)
            | Binomial, Probit -> (Vector.Exp(-0.5 * xbeta .* xbeta) ./ (Math.Sqrt(2.0 * Math.PI)) .^ 2.0) ./ (pred*(1.0 - pred))
            | _ -> raise (new InvalidOperationException())

    let getXBeta (design : ((UInt16Vector list * int[] * int[]) option * Vector option) list) (beta : Vector) (sliceLen : int)
                 (cumEstimateCounts: int[]) =
        let xbeta = new Vector(sliceLen, 0.0)
        design |> List.iteri (fun pInd p ->
                                match p with
                                    | Some(factors), None ->  
                                        let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1]  
                                        let slice, estimateMap, dimProd = factors
                                        if estimateMap.Length = 1 && estimateMap.[0] >= 0 then
                                            VectorExpr.EvalIn(xbeta.AsExpr + beta.[offset], Some xbeta) |> ignore
                                        else
                                            let slice = slice |> List.map (fun v -> v.NativeArray) |> List.toArray
                                            MklFunctions.Glm_Update_XBeta(sliceLen, xbeta.NativeArray, slice.Length, slice, estimateMap, dimProd, beta.NativeArray, Vector.Empty.NativeArray, offset)
                                    | None, Some(covariate) -> 
                                        let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1] 
                                        VectorExpr.EvalIn(xbeta.AsExpr + beta.[offset] * covariate.AsExpr, Some xbeta) |> ignore
                                    | Some(factors), Some(covariate) -> 
                                        let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1] 
                                        let slice, estimateMap, dimProd = factors
                                        if estimateMap.Length = 1 && estimateMap.[0] >= 0 then
                                            VectorExpr.EvalIn(xbeta.AsExpr + beta.[offset] * covariate.AsExpr, Some xbeta) |> ignore
                                        else
                                            let slice = slice |> List.map (fun v -> v.NativeArray) |> List.toArray
                                            MklFunctions.Glm_Update_XBeta(sliceLen, xbeta.NativeArray, slice.Length, slice, estimateMap, dimProd, beta.NativeArray, covariate.NativeArray, offset)
                                    | _ -> ()
                                )
        xbeta

    let getU (design : ((UInt16Vector list * int[] * int[]) option * Vector option) list) (u : Vector) (sliceLen : int)
             (cumEstimateCounts: int[]) =
        let U = new Vector(cumEstimateCounts.[cumEstimateCounts.Length - 1], 0.0)
        design |> List.iteri (fun pInd p ->
                                match p with
                                    | Some(factors), None ->   
                                        let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1] 
                                        let slice, estimateMap, dimProd = factors
                                        if estimateMap.Length = 1 && estimateMap.[0] >= 0 then
                                            let sum = MklFunctions.Sum_Array_NotNan(sliceLen, u.NativeArray)
                                            U.[offset] <- U.[offset] + (if Double.IsNaN(sum) then 0.0 else sum)
                                        else
                                            let slice = slice |> List.map (fun v -> v.NativeArray) |> List.toArray
                                            MklFunctions.Glm_Update_U(sliceLen, U.NativeArray, u.NativeArray, slice.Length, slice, estimateMap, dimProd, Vector.Empty.NativeArray, offset)
                                    | None, Some(covariate) -> 
                                        let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1] 
                                        let innerProd = MklFunctions.Innerprod_Arrays_NotNan(sliceLen, u.NativeArray, covariate.NativeArray)
                                        U.[offset] <- U.[offset] + (if Double.IsNaN(innerProd) then 0.0 else innerProd)
                                    | Some(factors), Some(covariate) ->
                                        let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1]  
                                        let slice, estimateMap, dimProd = factors
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

    let getH (design : ((UInt16Vector list * int[] * int[]) option * Vector option) list) (weight : Vector) (sliceLen : int)
             (cumEstimateCounts: int[]) =
        let p = cumEstimateCounts.[cumEstimateCounts.Length - 1]
        let H = new Matrix(p, p, 0.0)
        let weightIsOne = weight == Vector.Empty
        for pCol in 0..design.Length-1 do
            for pRow in 0..pCol do
                let rowOffset = if pRow = 0 then 0 else cumEstimateCounts.[pRow-1]
                let colOffset = if pCol = 0 then 0 else cumEstimateCounts.[pCol-1]
                match design.[pRow], design.[pCol] with
                    | (Some(rowFactors), None), (Some(colFactors), None) ->
                        let rowSlice, rowEstimateMap, rowDimProd = rowFactors
                        let colSlice, colEstimateMap, colDimProd = colFactors
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

                    | (Some(rowFactors), None), (None, Some(colCovariate)) ->
                        let rowSlice, rowEstimateMap, rowDimProd = rowFactors
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

                    | (Some(rowFactors), None), (Some(colFactors), Some(colCovariate)) ->
                        let rowSlice, rowEstimateMap, rowDimProd = rowFactors
                        let colSlice, colEstimateMap, colDimProd = colFactors
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

                    | (None, Some(rowCovariate)), (None, Some(colCovariate)) -> 
                        if weightIsOne then
                            let innerProd = MklFunctions.Innerprod_Arrays_NotNan(sliceLen, rowCovariate.NativeArray, colCovariate.NativeArray)
                            H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + (if Double.IsNaN(innerProd) then 0.0 else innerProd) 
                        else
                            let innerProd = MklFunctions.Innerprod_3Arrays_NotNan(sliceLen, weight.NativeArray, rowCovariate.NativeArray, colCovariate.NativeArray)
                            H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + (if Double.IsNaN(innerProd) then 0.0 else innerProd)

                    | (None, Some(rowCovariate)), (Some(colFactors), None) -> 
                        let colSlice, colEstimateMap, colDimProd = colFactors
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

                    | (None, Some(rowCovariate)), (Some(colFactors), Some(colCovariate)) -> 
                        let colSlice, colEstimateMap, colDimProd = colFactors
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

                    | (Some(rowFactors), Some(rowCovariate)), (Some(colFactors), Some(colCovariate)) -> 
                        let rowSlice, rowEstimateMap, rowDimProd = rowFactors
                        let colSlice, colEstimateMap, colDimProd = colFactors
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

                    | (Some(rowFactors), Some(rowCovariate)), (None, Some(colCovariate)) -> 
                        let rowSlice, rowEstimateMap, rowDimProd = rowFactors
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

                    | (Some(rowFactors), Some(rowCovariate)), (Some(colFactors), None) -> 
                        let rowSlice, rowEstimateMap, rowDimProd = rowFactors
                        let colSlice, colEstimateMap, colDimProd = colFactors
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

    let rec iwls (response : Covariate) (design : ((int64 * int64 * int -> seq<UInt16Vector list> * int[] * int[]) option * Covariate option) list)
                 (glmDistribution : GlmDistribution) (glmLink : GlmLink) (beta : Vector) (H : Matrix option)
                 (maxIter : int) (iter : int) (obsCount : int64) (sliceLength : int) (cumEstimateCounts : int[]) eps =
        if iter > maxIter then 
            beta, Vector.Empty, iter, false
        else
            let processorCount = Environment.ProcessorCount
            let processorChunk = obsCount / int64(processorCount)
            let U, H =
                match H with
                    | Some(H) ->
                        seq{0..processorCount - 1} 
                           |> Seq.map (fun i -> (int64(i) * processorChunk), if i = processorCount - 1 then obsCount - 1L else (int64(i + 1) * processorChunk - 1L))
                           |> Seq.toArray 
                           |> Array.Parallel.map (fun (fromObs, toObs) -> 
                                                    let totalEstimateCount = cumEstimateCounts.[cumEstimateCounts.Length - 1]
                                                    let U = new Vector(totalEstimateCount, 0.0)
                                                    let respSlices = response.GetSlices(fromObs, toObs, sliceLength) 
                                                    let design' =
                                                        design |> List.map (fun (factorsOpt, covOpt) ->
                                                                                factorsOpt |> Option.map (fun f -> f(fromObs, toObs, sliceLength)), covOpt
                                                                                           |> Option.map (fun c -> c.GetSlices(fromObs, toObs, sliceLength)))
                                                               |> zipDesign |> Seq.zip respSlices
                                                    for resp, slice in design' do
                                                       let sliceLength = resp.Length
                                                       use xbeta = getXBeta slice beta sliceLength cumEstimateCounts
                                                       use pred = getPred xbeta glmLink
                                                       use u = get_u resp pred xbeta glmLink glmDistribution
                                                       use updateU = getU slice u sliceLength cumEstimateCounts
                                                       VectorExpr.EvalIn(U.AsExpr + updateU, Some U) |> ignore
                                                    U   
                                                  )
                        |> Array.reduce (+), H
                    | None -> 
                        seq{0..processorCount - 1} 
                           |> Seq.map (fun i -> (int64(i) * processorChunk), if i = processorCount - 1 then obsCount - 1L else (int64(i + 1) * processorChunk - 1L))
                           |> Seq.toArray 
                           |> Array.Parallel.map (fun (fromObs, toObs) -> 
                                                    let totalEstimateCount = cumEstimateCounts.[cumEstimateCounts.Length - 1]
                                                    let U = new Vector(totalEstimateCount, 0.0)
                                                    let H = new Matrix(totalEstimateCount, totalEstimateCount, 0.0)
                                                    let respSlices = response.GetSlices(fromObs, toObs, sliceLength) 
                                                    let design' =
                                                        design |> List.map (fun (factorsOpt, covOpt) ->
                                                                                factorsOpt |> Option.map (fun f -> f(fromObs, toObs, sliceLength)), covOpt
                                                                                           |> Option.map (fun c -> c.GetSlices(fromObs, toObs, sliceLength)))
                                                               |> zipDesign |> Seq.zip respSlices
                                                    for resp, slice in design' do
                                                       let sliceLength = resp.Length
                                                       use xbeta = getXBeta slice beta sliceLength cumEstimateCounts
                                                       use pred = getPred xbeta glmLink
                                                       use u = get_u resp pred xbeta glmLink glmDistribution
                                                       use weight = getWeight pred xbeta glmLink glmDistribution
                                                       use updateU = getU slice u sliceLength cumEstimateCounts
                                                       use updateH = getH slice weight sliceLength cumEstimateCounts
                                                       VectorExpr.EvalIn(U.AsExpr + updateU, Some U) |> ignore
                                                       MatrixExpr.EvalIn(H.AsExpr + updateH, Some H) |> ignore  
                                                    U, H    
                                                  )
                        |> Array.reduce (fun (u1, h1) (u2, h2) -> (u1 + u2), (h1 + h2))

            //calc next beta from U and H
            //MatrixExpr.EvalIn(H.AsExpr + (Matrix.Transpose(Matrix.UpperTri(H, 1))), Some H) |> ignore
            use delta = Matrix.CholSolve(H, U)
            let nextBeta = beta + delta
            if (glmDistribution = Gaussian && glmLink = Id) || epsEqualVector beta nextBeta eps then
                let invHDiag = Matrix.CholInv(H).Diag(0)
                nextBeta, invHDiag,  iter, true
            elif glmDistribution = Gamma && glmLink = Ln then  // weight is 1
                iwls response design glmDistribution glmLink nextBeta (Some H) maxIter (iter + 1) obsCount sliceLength cumEstimateCounts eps
            else
                iwls response design glmDistribution glmLink nextBeta None maxIter (iter + 1) obsCount sliceLength cumEstimateCounts eps

    let getParameterEstimates (design : ((Factor * FactorMask) list * CovariateExpr option) list) (beta : Vector)
                              (invHDiag : Vector) (cumEstimateCounts : int[]) =

        design |> List.mapi (fun i (maskedFactors, cov) ->
                                 let predictorOffset = if i = 0 then 0 else cumEstimateCounts.[i - 1]
                                 match maskedFactors, cov with
                                     | (h::t), None ->
                                         let isDisabled = getDisabled maskedFactors |> Array.toList
                                         let isNA = getIsNA maskedFactors
                                         let factors = maskedFactors |> List.map fst
                                         let cumDisabledCount = Array.sub (isDisabled |> List.toArray |> Array.scan (fun cum x -> if x then cum + 1 else cum) 0) 1 isDisabled.Length 
                                         let cumNACount = Array.sub (isNA |> Array.scan (fun cum x -> if x then cum + 1 else cum) 0) 1 isNA.Length 
                                         let dimProd = factors |> List.map (fun f -> f.LevelCount) |> getDimProd
                                         isDisabled |> List.mapi (fun index isDisabled -> 
                                                                      let estimateIndex = predictorOffset + index - cumDisabledCount.[index] - cumNACount.[index]
                                                                      let subscripts = ind2sub index dimProd
                                                                      let levels = subscripts |> List.zip factors |> List.map (fun (f, s) -> f.GetLevel(s))
                                                                      if isDisabled || isNA.[index] then
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
                                     | [], Some cov ->
                                         [{
                                          Predictor = NumericalPredictor cov
                                          Levels = []
                                          Value = beta.[predictorOffset]
                                          Std = Math.Sqrt(invHDiag.[predictorOffset])
                                          IsDisabled = false  
                                          WaldChiSq = Math.Pow(beta.[predictorOffset] / Math.Sqrt(invHDiag.[predictorOffset]), 2.0)  
                                          PValue = 1.0 - chicdf(1.0, Math.Pow(beta.[predictorOffset] / Math.Sqrt(invHDiag.[predictorOffset]), 2.0) )                                                                         
                                         }]   
                                     | (h::t), Some cov ->
                                         let isDisabled = getDisabled maskedFactors |> Array.toList
                                         let isNA = getIsNA maskedFactors
                                         let factors = maskedFactors |> List.map fst
                                         let cumDisabledCount = Array.sub (isDisabled |> List.toArray |> Array.scan (fun cum x -> if x then cum + 1 else cum) 0) 1 isDisabled.Length  
                                         let cumNACount = Array.sub (isNA |> Array.scan (fun cum x -> if x then cum + 1 else cum) 0) 1 isNA.Length 
                                         let dimProd = factors |> List.map (fun f -> f.LevelCount) |> getDimProd
                                         isDisabled |> List.mapi (fun index isDisabled -> 
                                                                      let estimateIndex = predictorOffset + index - cumDisabledCount.[index] - cumNACount.[index]
                                                                      let subscripts = ind2sub index dimProd
                                                                      let levels = subscripts |> List.zip factors |> List.map (fun (f, s) -> f.GetLevel(s))
                                                                      if isDisabled || isNA.[index] then
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
                                     | [], None -> []                                 
                           ) |> List.concat

    let getDeviance (resp : Vector) (pred : Vector) (glmDistribution : GlmDistribution) =
        let resp = resp.AsExpr
        match glmDistribution with
            | Gaussian ->
                VectorExpr.EvalIn((resp - pred) .^ 2, None)
            | Poisson ->
                VectorExpr.EvalIn(VectorExpr.IfFunction((resp .= 0.0), (2.0 * pred.AsExpr), (2.0 * (resp .* VectorExpr.Log(resp ./ pred) - (resp - pred))) ), None)
            | Gamma ->
                VectorExpr.EvalIn(-2.0 * (VectorExpr.Log(resp ./ pred) - (resp ./ pred) + 1.0), None) 
            | Binomial -> 
                VectorExpr.EvalIn(VectorExpr.IfFunction((resp .= 0.0), (-2.0 * VectorExpr.Log(1.0 - pred.AsExpr)), (-2.0 * VectorExpr.Log(pred.AsExpr))), None)

    let getPearsonChi (resp : Vector) (pred : Vector) (glmDistribution : GlmDistribution) =
        let resp = resp.AsExpr
        match glmDistribution with
            | Gaussian ->
                VectorExpr.EvalIn((resp - pred) .^ 2, None)
            | Poisson ->
                VectorExpr.EvalIn(((resp - pred) ./ VectorExpr.Sqrt(pred.AsExpr)) .^ 2, None)
            | Gamma ->
                VectorExpr.EvalIn(((resp ./ pred) - 1.0) .^ 2, None) 
            | Binomial ->
                VectorExpr.EvalIn((resp - pred) ./ VectorExpr.Sqrt(pred.AsExpr .* (1.0 - pred.AsExpr)) .^ 2, None)

    let getLogLikelihoodPart (resp : Vector) (pred : Vector) (glmDistribution : GlmDistribution) =
        let resp = resp.AsExpr
        match glmDistribution with
            | Gaussian ->
                VectorExpr.EvalIn((resp - pred) .^ 2, None)
            | Poisson ->
                VectorExpr.EvalIn((resp .* VectorExpr.Log(pred.AsExpr) - pred.AsExpr), None)
            | Gamma ->
                VectorExpr.EvalIn((VectorExpr.Log(pred ./ resp) + (resp ./ pred)), None) 
            | Binomial ->
                VectorExpr.EvalIn(VectorExpr.IfFunction((resp .= 0.0), 
                                                        (VectorExpr.Log(1.0 - pred.AsExpr)),
                                                        (VectorExpr.Log(pred.AsExpr))), None)

    let getLogLikelihood (logLikelihoodPart : float) (N : int64) (phi : float) (glmDistribution : GlmDistribution) =
        match glmDistribution with
            | Gaussian ->
                -0.5 * (logLikelihoodPart / phi + float(N) * Math.Log(2.0 * Math.PI * phi))
            | Poisson ->
                logLikelihoodPart / phi
            | Gamma ->
                phi * logLikelihoodPart + (phi * Math.Log(phi) - lnGamma(phi)) * float(N)
            | Binomial -> 
                logLikelihoodPart / phi

    let getPhi (dispersion : GlmDispersion) (glmDistribution : GlmDistribution) (N : int64) (dof : int) (pearsonChi : float) (deviance : float) =
        let N_dof = float(N - int64(dof))
        let N = float N
        match dispersion with
            | MaxLikelihood ->
                match glmDistribution with
                    | Gaussian -> deviance / N_dof
                    | Poisson -> 1.0
                    | Binomial -> 1.0
                    | Gamma -> 
                        let dev= deviance / N_dof
                        let scale = (6.0*N_dof+2.0*N*dev)/(dev*(6.0*N_dof+N*dev))
                        let rec getScale (currScale : float) (d : float) (N' : float) (iter : int) (maxIter : int) =
                            let gamLogL' = gammaLogL' currScale d N'
                            if Double.IsNaN(currScale) || iter >= maxIter then Double.NaN
                            elif Math.Abs(gamLogL') <= 1e-10 then
                                printfn "Scale converged after %d iter" iter
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

    let getGoodnessOfFit (response : Covariate) (design : ((int64 * int64 * int -> seq<UInt16Vector list> * int[] * int[]) option * Covariate option) list)
                         (glmDistribution : GlmDistribution) (glmLink : GlmLink) (beta : Vector)
                         (obsCount : int64) (sliceLength : int) (cumEstimateCounts : int[]) =
 
        let processorCount = Environment.ProcessorCount
        let processorChunk = obsCount / int64(processorCount)
        let deviance, pearsonChi, likelihoodPart, validObs =
            seq{0..processorCount - 1} 
                |> Seq.map (fun i -> (int64(i) * processorChunk), if i = processorCount - 1 then obsCount - 1L else (int64(i + 1) * processorChunk - 1L))
                |> Seq.toArray 
                |> Array.Parallel.map (fun (fromObs, toObs) -> 
                                            let respSlices = response.GetSlices(fromObs, toObs, sliceLength) 
                                            design |> List.map (fun (factorsOpt, covOpt) ->
                                                                         factorsOpt |> Option.map (fun f -> f(fromObs, toObs, sliceLength)), covOpt
                                                                                    |> Option.map (fun c -> c.GetSlices(fromObs, toObs, sliceLength)))
                                                   |> zipDesign |> Seq.zip respSlices
                                                   |> Seq.fold (fun (devSum, pearsonSum, logLSum, validObs) (resp, slice) ->
                                                                    let sliceLength = resp.Length
                                                                    use xbeta = getXBeta slice beta sliceLength cumEstimateCounts
                                                                    use pred = getPred xbeta glmLink
                                                                    use isValid = pred .= pred
                                                                    use isValidPred = pred.[isValid]
                                                                    use deviance = getDeviance resp pred glmDistribution
                                                                    use pearsonChi = getPearsonChi resp pred glmDistribution
                                                                    use likelihoodPart = getLogLikelihoodPart resp pred glmDistribution 
                                                                    let dsum = MklFunctions.Sum_Array_NotNan(sliceLength, deviance.NativeArray)
                                                                    let psum = MklFunctions.Sum_Array_NotNan(sliceLength, pearsonChi.NativeArray) 
                                                                    let lsum = MklFunctions.Sum_Array_NotNan(sliceLength, likelihoodPart.NativeArray)                                            
                                                                    (devSum + (if Double.IsNaN(dsum) then 0.0 else dsum)),
                                                                     (pearsonSum + (if Double.IsNaN(psum) then 0.0 else psum)),
                                                                      (logLSum + (if Double.IsNaN(lsum) then 0.0 else lsum)), (validObs + isValidPred.LongLength)
                                                               ) (0.0, 0.0, 0.0, 0L)
                                        )
                |> Array.reduce (fun (d1, p1, l1, N1) (d2, p2, l2, N2) -> d1 + d2, p1 + p2, l1 + l2, N1 + N2)

        let dof = beta.Length 
        let N = validObs
        let phi = getPhi MaxLikelihood glmDistribution N dof pearsonChi deviance
        let logLikelihood = getLogLikelihood likelihoodPart N phi glmDistribution
        {
         ValidObsCount = N
         DoF = dof
         LogLikehood = logLikelihood
         Deviance = deviance
         PearsonChi = pearsonChi
         AIC = -2.0 * logLikelihood + 2.0 * float(N - int64 dof)
         MLScale = getScale phi glmDistribution
         MLPhi = phi
        }

    let getInitBeta (estimateCount : int) (link : GlmLink) (meanResponseEstimate : float) =
        let beta = new Vector(estimateCount, 0.0)
        match link with
            | Id ->
                beta.[0] <- meanResponseEstimate
            | Ln ->
                beta.[0] <- Math.Log(meanResponseEstimate)
            | Log(d) ->
                beta.[0] <- Math.Log(meanResponseEstimate) / Math.Log(d)
            | Power(d) ->
                beta.[0] <- Math.Pow(meanResponseEstimate, 1.0 / d)
            | Logit ->
                beta.[0] <- Math.Log(meanResponseEstimate / (1.0 - meanResponseEstimate))
            | LogLog ->
                beta.[0] <- -Math.Log(-Math.Log(meanResponseEstimate))
            | CLogLog ->
                beta.[0] <- Math.Log(-Math.Log(1.0 - meanResponseEstimate))
            | Probit ->
                beta.[0] <- 
                    let mutable res = meanResponseEstimate
                    MklFunctions.D_CdfNormInv_Array(1L, &&res, &&res)
                    res
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
                                         use isInvalid = BoolVectorExpr.EvalIn((v.AsExpr .<> 0.0) .&& (v.AsExpr .<> 1.0) , None)
                                         v.[isInvalid].LongLength
                                   )
                        |> Seq.reduce (+)
                if invalidCount > 0L then raise (new ArgumentException("Binomial response must be 0, 1 or NaN"))


    let fitModel (response : CovariateExpr) (predictors : Predictor list) (includeIntercept : bool)
                 (glmDistribution : GlmDistribution) (glmLink : GlmLink) (sliceLength : int) (maxIter : int) (eps : float) =
        let minLen = min (predictors |> List.map (fun p -> p.MinLength) |> List.min) response.MinLength
        let maxLen = max (predictors |> List.map (fun p -> p.MaxLength) |> List.max) response.MaxLength
        if minLen <> maxLen then raise (new ArgumentException("Glm design data length mismatch"))
        let length = minLen
        let respCov = response.AsCovariate
        checkResponse respCov glmDistribution sliceLength
        let respSlice = (respCov.GetSlices(0L, int64(sliceLength - 1), sliceLength) |> Seq.take 1 |> Seq.toArray).[0]
        let meanReponseSlice = Vector.Mean(respSlice.[respSlice .= respSlice])

        let estimableDesign = getEstimableDesign predictors includeIntercept
        let estimateCounts = estimableDesign |> List.map getEstimateCount |> List.toArray
        let cumEstimateCounts = Array.sub (estimateCounts |> Array.scan (+) 0) 1 estimateCounts.Length
        let initBeta = getInitBeta cumEstimateCounts.[cumEstimateCounts.Length - 1] glmLink meanReponseSlice
        let design = estimableDesign |> List.map (fun (factors, cov) -> (getCategoricalSlicer factors), cov |> Option.map (fun c -> c.AsCovariate)) 
        let beta, invHDiag, iter, converged = iwls respCov design glmDistribution glmLink initBeta None maxIter 0 length sliceLength cumEstimateCounts eps
        if converged then
            let parameters = getParameterEstimates estimableDesign beta invHDiag cumEstimateCounts
            let goodnessOfFit = getGoodnessOfFit respCov design glmDistribution glmLink beta length sliceLength cumEstimateCounts
            {
             Response = response
             Predictors = predictors
             Distribution = glmDistribution
             Link = glmLink
             HasIntercept = includeIntercept
             GoodnessOfFit = goodnessOfFit
             Beta = beta
             InvHDiag = invHDiag
             Iter = iter
             Parameters = parameters
            }
        else 
            raise (new InvalidOperationException(sprintf "Glm algorithm did not converge after %d iterations" iter))

    let getPermutedFactor (baseFactor : Factor) (factorToPermute : Factor) =
        let baseLevelMap = new Dictionary<string, int>()
        let permuteFactorMap = new Dictionary<int, string>()
        let permuteFactorMapRev = new Dictionary<string, int>()
        [|0..baseFactor.LevelCount - 1|] |> Array.iter (fun index -> baseLevelMap.Add(baseFactor.GetLevel(index), index))
        [|0..factorToPermute.LevelCount - 1|] |> Array.iter (fun index -> permuteFactorMap.Add(index, factorToPermute.GetLevel(index))
                                                                          permuteFactorMapRev.Add(factorToPermute.GetLevel(index), index))
        let naSet = Factor.NAs |> Set.ofArray
        let allLevelsInBase = 
            [|0..factorToPermute.LevelCount - 1|] |> Array.map (fun index ->
                                                                   let level = factorToPermute.GetLevel(index) in baseLevelMap.ContainsKey(level) || naSet.Contains(level))
                                                  |> Array.reduce (&&)
        if not allLevelsInBase then raise (new ArgumentException("Permuted factor: all not NA levels must be in base factor"))

        //add NAs if not there
        [|0..factorToPermute.LevelCount - 1|] |> Array.iter (fun index -> if not <| baseLevelMap.ContainsKey(factorToPermute.GetLevel(index)) then
                                                                              let count = baseLevelMap.Count
                                                                              baseLevelMap.Add(factorToPermute.GetLevel(index), count))

        [|0..baseFactor.LevelCount - 1|] |> Array.iter (fun index -> 
                                                            let level = baseFactor.GetLevel(index)
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

    let fitted (model : GlmModel) (data : DataFrame) : Covariate =
        let predictors = model.Predictors |> List.map (Predictor.Substitute (substituteVar data))
        let minLen = predictors |> List.map (fun p -> p.MinLength) |> List.min
        let maxLen = predictors |> List.map (fun p -> p.MaxLength) |> List.max
        if minLen <> maxLen then raise (new ArgumentException("Glm design data length mismatch"))
        let length = minLen
        let beta = model.Beta
        let glmLink = model.Link
        let estimableDesign = getEstimableDesign predictors model.HasIntercept
        let estimateCounts = estimableDesign |> List.map getEstimateCount |> List.toArray
        let cumEstimateCounts = Array.sub (estimateCounts |> Array.scan (+) 0) 1 estimateCounts.Length
        let design = estimableDesign |> List.map (fun (factors, cov) -> (getCategoricalSlicer factors), cov |> Option.map (fun c -> c.AsCovariate)) 
        
        let covariateStorage = 
             {
              new ICovariateStorage with
                  member __.Length = length
                  member __.GetSlices(fromObs : int64, toObs : int64, sliceLength : int) =
                    design |> List.map (fun (factorsOpt, covOpt) ->
                                                    factorsOpt |> Option.map (fun f -> f(fromObs, toObs, sliceLength)), covOpt
                                                               |> Option.map (fun c -> c.GetSlices(fromObs, toObs, sliceLength)))
                            |> zipDesign
                            |> Seq.map (fun slice ->
                                            let sliceLen = 
                                                match slice with
                                                    | (Some(v::_, _, _), _)::_ -> v.Length
                                                    | (_, Some(v))::_ -> v.Length
                                                    | _ -> 0
                                            use xbeta = getXBeta slice beta sliceLen cumEstimateCounts
                                            getPred xbeta glmLink                               
                                        )
             }
        new Covariate("Fitted", covariateStorage)        
        
        
        
        




