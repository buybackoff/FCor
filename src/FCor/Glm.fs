namespace FCor
#nowarn "9"

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic
open System.IO
open FCor.ExplicitConversion

type GlmLink =
    | Id
    | Ln
    | Log of float
    | Power of float

type GlmDistribution =
    | Gaussian
    | Gamma
    | Poisson

type GlmDispersion =
    | MaxLikelihood
    | Pearson
    | Deviance

type FactorMask =
    | EnableAllLevels
    | DisableOneLevel
    | DisableAllLevels

type ParameterEstimate =
    {
     Predictor :  string
     Level : string
     Value : float
     Std : float
     IsDisabled : bool
    }

type GlmGoodnessOfFit =
    {
     LogLikehood : float
     Deviance : float
     PearsonChi : float
     Scale : float
    }

module Glm =

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
                                                    | Covariate(c) -> c.AsSeq |> Seq.map box
                                      )
                           |> Seq.toList |> zipN |> Seq.map (join ",")
        dataSeq |> Seq.iter writer.WriteLine

    let importCsv (path : string) =
        let delimiter = ','
        use sr = new StreamReader(path)
        let firstN = 1000
        let N = 100000

        let isNotNumerics (s : string) =
            s <> "#N/A" && s <> "" && s <> "N/A" && (match Single.TryParse(s) with | (true, _) -> false | _ -> true)

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
        let factorSlices = Array.init headers.Length (fun col -> if isFactor.[col] then Array.create N String.Empty else Array.create 0 String.Empty)
        let covSlices = Array.init headers.Length (fun col -> if isFactor.[col] then Array.create 0 0.0f else Array.create N 0.0f)
        
        isFactor |> Array.iteri (fun col isFactor -> 
                                     let colArray = getColArray firstChunk col
                                     if isFactor then
                                         factorStorage.[col].SetSlice(0L, colArray)
                                     else
                                         covStorage.[col].SetSlice(0L, colArray |> Array.map (fun s -> match Single.TryParse(s) with | (true, v) -> v | _ -> Single.NaN))
                                )

        let rec processChar (fromObs : int64) (N : int) (row : int) (col : int) (colCount : int) (isFactor : bool[]) (delimiter : char) (value : char []) (valueLen : int) =
            if sr.Peek() >= 0 then
                let ch = sr.Read() |> char
                if ch = delimiter then
                    if isFactor.[col] then 
                        factorSlices.[col].[row] <- new String(Array.sub value 0 valueLen)
                    else
                        covSlices.[col].[row] <- match Single.TryParse(new String(Array.sub value 0 valueLen)) with | (true, v) -> v | _ -> Single.NaN
                    processChar fromObs N row (col + 1) colCount isFactor delimiter value 0
                elif ch = '\r' then
                    if isFactor.[col] then 
                        factorSlices.[col].[row] <- new String(Array.sub value 0 valueLen)
                    else
                        covSlices.[col].[row] <- match Single.TryParse(new String(Array.sub value 0 valueLen)) with | (true, v) -> v | _ -> Single.NaN
                    if row = N - 1 then
                        isFactor |> Array.Parallel.iteri (fun col isFactor ->
                                                              if isFactor then factorStorage.[col].SetSlice(fromObs, factorSlices.[col])
                                                              else covStorage.[col].SetSlice(fromObs, covSlices.[col]))
                        if sr.Peek() |> char = '\n' then sr.Read() |> ignore
                        processChar (fromObs + int64(N)) N 0 0 colCount isFactor delimiter value 0
                    else
                        if sr.Peek() |> char = '\n' then sr.Read() |> ignore
                        processChar fromObs N (row + 1) 0 colCount isFactor delimiter value 0
                else
                    value.[valueLen] <- ch
                    processChar fromObs N row col colCount isFactor delimiter value (valueLen + 1) 
            elif row > 0 then
                isFactor |> Array.Parallel.iteri (fun col isFactor ->
                                                        if isFactor then factorStorage.[col].SetSlice(fromObs, Array.sub factorSlices.[col] 0 row)
                                                        else covStorage.[col].SetSlice(fromObs, Array.sub covSlices.[col] 0 row))
            else ()


        processChar (int64(firstN)) N 0 0 headers.Length isFactor delimiter (Array.create 100 ' ') 0
                   
        isFactor |> Array.zip headers |> Array.mapi (fun col (name, isFactor) ->
                                                         if isFactor then 
                                                             StatVariable.Factor(new Factor(name, factorStorage.[col]))
                                                         else
                                                             StatVariable.Covariate(new Covariate(name, covStorage.[col]))
                                                    )

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

    let getIndexOfMaxLevel (factor : Factor) =
        let levels = Array.init (factor.LevelCount) (fun i -> i, factor.GetLevel(i))
        levels |> Array.sortInPlaceBy snd
        levels.[levels.Length - 1] |> fst

    let getDisabledSubscripts (maskedFactors : (Factor * FactorMask) list) =
        maskedFactors |> List.map (fun (factor, mask) -> 
                                        match mask with
                                            | EnableAllLevels -> []
                                            | DisableOneLevel -> [getIndexOfMaxLevel factor]
                                            | DisableAllLevels -> [0..factor.LevelCount - 1]
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

    let getCategoricalSlicer (maskedFactors : (Factor * FactorMask) list) =
        match maskedFactors with
            | h::t ->
                let factors = maskedFactors |> List.map fst 
                let dimProd = maskedFactors |> List.map fst |> List.map (fun f -> f.LevelCount) |> getDimProd |> List.toArray
                let isDisabled = getDisabled maskedFactors
                let cumDisabledCount = Array.sub (isDisabled |> Array.scan (fun cum x -> if x then cum + 1 else cum) 0) 1 isDisabled.Length
                let estimateMap = Array.init isDisabled.Length (fun i -> if isDisabled.[i] then -1 else i - cumDisabledCount.[i])
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
                                            count * match mask with
                                                        | EnableAllLevels -> factor.LevelCount
                                                        | DisableOneLevel -> factor.LevelCount - 1
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

    let get_u (resp : Vector) (pred : Vector) (glmLink : GlmLink) (glmDistribution : GlmDistribution) =
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
                (resp - pred) ./ (pred .*pred)
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

    let getWeight (pred : Vector) (glmLink : GlmLink) (glmDistribution : GlmDistribution) =
        match glmDistribution, glmLink with
            | Gaussian, Id -> 
                Vector.Empty
            | Gaussian, Ln ->
                pred .* pred
            | Gaussian, Log(d) -> 
                Math.Log(d) * pred * Math.Log(d) .* pred
            | Gaussian, Power(d) ->
                d * d * (pred .^ (2.0 - 2.0 / d))
            | Gamma, Id -> 
                1.0 ./ (pred .*pred)
            | Gamma, Ln ->
                Vector.Empty
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

    let getXBeta (design : ((UInt16Vector list * int[] * int[]) option * Vector option) list) (beta : Vector) (sliceLen : int)
                 (cumEstimateCounts: int[]) =
        let xbeta = new Vector(sliceLen, 0.0)
        design |> List.iteri (fun pInd p ->
                                match p with
                                    | Some(factors), None ->  
                                        let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1]  
                                        let slice, estimateMap, dimProd = factors
                                        if estimateMap.Length = 1 && estimateMap.[0] <> -1 then
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
                                        if estimateMap.Length = 1 && estimateMap.[0] <> -1 then
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
                                        if estimateMap.Length = 1 && estimateMap.[0] <> -1 then
                                            U.[offset] <- U.[offset] + Vector.Sum(u)
                                        else
                                            let slice = slice |> List.map (fun v -> v.NativeArray) |> List.toArray
                                            MklFunctions.Glm_Update_U(sliceLen, U.NativeArray, u.NativeArray, slice.Length, slice, estimateMap, dimProd, Vector.Empty.NativeArray, offset)
                                    | None, Some(covariate) -> 
                                        let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1] 
                                        U.[offset] <- U.[offset] + u * covariate
                                    | Some(factors), Some(covariate) ->
                                        let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1]  
                                        let slice, estimateMap, dimProd = factors
                                        if estimateMap.Length = 1 && estimateMap.[0] <> -1 then
                                            U.[offset] <- U.[offset] + u * covariate
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
                        if rowEstimateMap.Length = 1 && rowEstimateMap.[0] <> -1 && colEstimateMap.Length = 1 && colEstimateMap.[0] <> -1 then
                            if weightIsOne then
                                H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + float(sliceLen)
                            else
                                H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + Vector.Sum(weight)
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
                        if rowEstimateMap.Length = 1 && rowEstimateMap.[0] <> -1 then
                            if weightIsOne then
                                H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + Vector.Sum(colCovariate)
                            else
                                H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + weight * colCovariate
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
                        if rowEstimateMap.Length = 1 && rowEstimateMap.[0] <> -1 && colEstimateMap.Length = 1 && colEstimateMap.[0] <> -1 then
                            if weightIsOne then
                                H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + Vector.Sum(colCovariate)
                            else
                                H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + weight * colCovariate
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
                            H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + rowCovariate * colCovariate
                        else
                            use prod = VectorExpr.EvalIn((weight.AsExpr .* rowCovariate) .* colCovariate, None)
                            H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + Vector.Sum(prod)

                    | (None, Some(rowCovariate)), (Some(colFactors), None) -> 
                        let colSlice, colEstimateMap, colDimProd = colFactors
                        if colEstimateMap.Length = 1 && colEstimateMap.[0] <> -1 then
                            if weightIsOne then
                                H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + Vector.Sum(rowCovariate)
                            else
                                H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + weight * rowCovariate
                        else
                            let colSlice = colSlice |> List.map (fun v -> v.NativeArray) |> List.toArray
                            MklFunctions.Glm_Update_H(sliceLen, p, H.ColMajorDataVector.NativeArray, weight.NativeArray,
                                                      rowCovariate.NativeArray,
                                                      Vector.Empty.NativeArray,
                                                      0, zeroIntPtrArr, zeroIntArr, zeroIntArr,
                                                      colSlice.Length, colSlice, colEstimateMap, colDimProd, rowOffset, colOffset)

                    | (None, Some(rowCovariate)), (Some(colFactors), Some(colCovariate)) -> 
                        let colSlice, colEstimateMap, colDimProd = colFactors
                        if colEstimateMap.Length = 1 && colEstimateMap.[0] <> -1 then
                            if weightIsOne then
                                H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + rowCovariate * colCovariate
                            else
                                use prod = VectorExpr.EvalIn(weight.AsExpr .* rowCovariate .* colCovariate, None)
                                H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + Vector.Sum(prod)
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
                        if rowEstimateMap.Length = 1 && rowEstimateMap.[0] <> -1 && colEstimateMap.Length = 1 && colEstimateMap.[0] <> -1 then
                            if weightIsOne then
                                H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + rowCovariate * colCovariate
                            else
                                use prod = VectorExpr.EvalIn(weight.AsExpr .* rowCovariate .* colCovariate, None)
                                H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + Vector.Sum(prod)
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
                        if rowEstimateMap.Length = 1 && rowEstimateMap.[0] <> -1 then
                            if weightIsOne then
                                H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + rowCovariate * colCovariate
                            else
                                use prod = VectorExpr.EvalIn(weight.AsExpr .* rowCovariate .* colCovariate , None)
                                H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + Vector.Sum(prod) 
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
                        if rowEstimateMap.Length = 1 && rowEstimateMap.[0] <> -1 then
                            if weightIsOne then
                                H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + Vector.Sum(rowCovariate)
                            else
                                H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + weight * rowCovariate
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
                 (glmDistribution : GlmDistribution) (glmLink : GlmLink) (beta : Vector)
                 (maxIter : int) (iter : int) (obsCount : int64) (sliceLength : int) (cumEstimateCounts : int[]) eps =
        if iter > maxIter then 
            beta, Vector.Empty, iter, false
        else
            let processorCount = Environment.ProcessorCount
            let processorChunk = obsCount / int64(processorCount)
            let U, H =
                seq{0..processorCount - 1} 
                   |> Seq.map (fun i -> (int64(i) * processorChunk), if i = processorCount - 1 then obsCount - 1L else (int64(i + 1) * processorChunk - 1L))
                   |> Seq.toArray 
                   |> Array.Parallel.map (fun (fromObs, toObs) -> 
                                            let totalEstimateCount = cumEstimateCounts.[cumEstimateCounts.Length - 1]
                                            let U = new Vector(totalEstimateCount, 0.0)
                                            let H = new Matrix(totalEstimateCount, totalEstimateCount, 0.0)
                                            let respSlices = response.GetSlices(fromObs, toObs, sliceLength) 
                                            let design' =
                                                design |> List.map (fun (factorsOpt, covOpt) -> factorsOpt |> Option.map (fun f -> f(fromObs, toObs, sliceLength)), covOpt |> Option.map (fun c -> c.GetSlices(fromObs, toObs, sliceLength)))
                                                       |> zipDesign |> Seq.zip respSlices
                                            for resp, slice in design' do
                                               let sliceLength = resp.Length
                                               use xbeta = getXBeta slice beta sliceLength cumEstimateCounts
                                               use pred = getPred xbeta glmLink
                                               use u = get_u resp pred glmLink glmDistribution
                                               use weight = getWeight resp glmLink glmDistribution
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
            else
                iwls response design glmDistribution glmLink nextBeta maxIter (iter + 1) obsCount sliceLength cumEstimateCounts eps

    let getParameterEstimates (design : ((Factor * FactorMask) list * CovariateExpr option) list) (beta : Vector)
                              (invHDiag : Vector) (cumEstimateCounts : int[]) =

        let join (s1 : string) (s2 : string) = s1 + "*" + s2
        design |> List.mapi (fun i (maskedFactors, cov) ->
                                 let predictorOffset = if i = 0 then 0 else cumEstimateCounts.[i - 1]
                                 match maskedFactors, cov with
                                     | (h::t), None ->
                                         let isDisabled = getDisabled maskedFactors |> Array.toList
                                         let factors = maskedFactors |> List.map fst
                                         let cumDisabledCount = Array.sub (isDisabled |> List.toArray |> Array.scan (fun cum x -> if x then cum + 1 else cum) 0) 1 isDisabled.Length 
                                         let dimProd = factors |> List.map (fun f -> f.LevelCount) |> getDimProd
                                         let predName = factors |> List.map (fun f -> f.Name) |> List.reduce join
                                         isDisabled |> List.mapi (fun index x -> 
                                                                      let estimateIndex = predictorOffset + index - cumDisabledCount.[index]
                                                                      let subscripts = ind2sub index dimProd
                                                                      let level = subscripts |> List.zip factors |> List.map (fun (f, s) -> f.GetLevel(s)) |> List.reduce join
                                                                      if x then
                                                                          {
                                                                           Predictor = predName
                                                                           Level = level
                                                                           Value = 0.0
                                                                           Std = 0.0
                                                                           IsDisabled = true                                                                       
                                                                          }
                                                                      else
                                                                          {
                                                                           Predictor = predName
                                                                           Level = level
                                                                           Value = beta.[estimateIndex]
                                                                           Std = Math.Sqrt(invHDiag.[estimateIndex])
                                                                           IsDisabled = false                                                                       
                                                                          }
                                                                  )
                                     | [], Some cov ->
                                         [{
                                          Predictor = cov.Name
                                          Level = ""
                                          Value = beta.[predictorOffset]
                                          Std = Math.Sqrt(invHDiag.[predictorOffset])
                                          IsDisabled = false                                                                       
                                         }]   
                                     | (h::t), Some cov ->
                                         let isDisabled = getDisabled maskedFactors |> Array.toList
                                         let factors = maskedFactors |> List.map fst
                                         let cumDisabledCount = Array.sub (isDisabled |> List.toArray |> Array.scan (fun cum x -> if x then cum + 1 else cum) 0) 1 isDisabled.Length  
                                         let dimProd = factors |> List.map (fun f -> f.LevelCount) |> getDimProd
                                         let predName = join (factors |> List.map (fun f -> f.Name) |> List.reduce join) cov.Name
                                         isDisabled |> List.mapi (fun index x -> 
                                                                      let estimateIndex = predictorOffset + index - cumDisabledCount.[index]
                                                                      let subscripts = ind2sub index dimProd
                                                                      let level = subscripts |> List.zip factors |> List.map (fun (f, s) -> f.GetLevel(s)) |> List.reduce join
                                                                      if x then
                                                                          {
                                                                           Predictor = predName
                                                                           Level = level
                                                                           Value = 0.0
                                                                           Std = 0.0
                                                                           IsDisabled = true                                                                       
                                                                          }
                                                                      else
                                                                          {
                                                                           Predictor = predName
                                                                           Level = level
                                                                           Value = beta.[estimateIndex]
                                                                           Std = Math.Sqrt(invHDiag.[estimateIndex])
                                                                           IsDisabled = false                                                                       
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

    let getPearsonChi (resp : Vector) (pred : Vector) (glmDistribution : GlmDistribution) =
        let resp = resp.AsExpr
        match glmDistribution with
            | Gaussian ->
                VectorExpr.EvalIn((resp - pred) .^ 2, None)
            | Poisson ->
                VectorExpr.EvalIn(((resp - pred) ./ VectorExpr.Sqrt(pred.AsExpr)) .^ 2, None)
            | Gamma ->
                VectorExpr.EvalIn(((resp ./ pred) - 1.0) .^ 2, None) 

    let getLogLikelihoodPart (resp : Vector) (pred : Vector) (glmDistribution : GlmDistribution) =
        let resp = resp.AsExpr
        match glmDistribution with
            | Gaussian ->
                VectorExpr.EvalIn((resp - pred) .^ 2, None)
            | Poisson ->
                VectorExpr.EvalIn((resp .* VectorExpr.Log(pred.AsExpr) - pred.AsExpr), None)
            | Gamma ->
                VectorExpr.EvalIn((VectorExpr.Log(pred ./ resp) + (resp ./ pred)), None) 

    let getLogLikelihood (logLikelihoodPart : float) (N : int64) (phi : float) (glmDistribution : GlmDistribution) =
        match glmDistribution with
            | Gaussian ->
                -0.5 * (logLikelihoodPart / phi + float(N) * Math.Log(2.0 * Math.PI * phi))
            | Poisson ->
                logLikelihoodPart
            | Gamma ->
                phi * logLikelihoodPart + (phi * Math.Log(phi) - lnGamma(phi)) * float(N)

    let getGoodnessOfFit (response : Covariate) (design : ((int64 * int64 * int -> seq<UInt16Vector list> * int[] * int[]) option * Covariate option) list)
                         (glmDistribution : GlmDistribution) (glmLink : GlmLink) (beta : Vector)
                         (obsCount : int64) (sliceLength : int) (cumEstimateCounts : int[]) =
 
        let respSlices = response.GetSlices(0L, obsCount - 1L, sliceLength)
        let tempRes =
            design |> List.map (fun (factorsOpt, covOpt) -> factorsOpt |> Option.map (fun f -> f(0L, obsCount - 1L, sliceLength)), covOpt |> Option.map (fun c -> c.GetSlices(0L, obsCount - 1L, sliceLength)))
                   |> zipDesign 
                   |> Seq.zip respSlices 
                   |> Seq.map (fun (resp, slice) ->
                                    let sliceLength = resp.Length
                                    use xbeta = getXBeta slice beta sliceLength cumEstimateCounts
                                    use pred = getPred xbeta glmLink
                                    use deviance = getDeviance resp pred glmDistribution
                                    use pearsonChi = getPearsonChi resp pred glmDistribution
                                    use likelihoodPart = getLogLikelihoodPart resp pred glmDistribution
                                    new Vector([|Vector.Sum(deviance); Vector.Sum(pearsonChi); Vector.Sum(likelihoodPart)|])
                                )
                   |> Seq.reduce (+)
        let dof = beta.Length |> int64
        let N = response.Length
        let phi = tempRes.[0] / float(N - dof)
        {
         LogLikehood = getLogLikelihood tempRes.[2] N phi glmDistribution
         Deviance = tempRes.[0]
         PearsonChi = tempRes.[1]
         Scale = phi
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
        beta

    let fitModel (response : CovariateExpr) (predictors : Predictor list) (includeIntercept : bool)
                 (glmDistribution : GlmDistribution) (glmLink : GlmLink) (sliceLength : int) (maxIter : int) (eps : float) =
        let minLen = min (predictors |> List.map (fun p -> p.MinLength) |> List.min) response.MinLength
        let maxLen = max (predictors |> List.map (fun p -> p.MaxLength) |> List.max) response.MaxLength
        if minLen <> maxLen then raise (new ArgumentException("Glm design data length mismatch"))
        let length = minLen
        let response = response.AsCovariate
        let meanReponseSlice = Vector.Mean((response.GetSlices(0L, int64(sliceLength - 1), sliceLength) |> Seq.take 1 |> Seq.toArray).[0])

        let estimableDesign = getEstimableDesign predictors includeIntercept
        let estimateCounts = estimableDesign |> List.map getEstimateCount |> List.toArray
        let cumEstimateCounts = Array.sub (estimateCounts |> Array.scan (+) 0) 1 estimateCounts.Length
        let initBeta = getInitBeta cumEstimateCounts.[cumEstimateCounts.Length - 1] glmLink meanReponseSlice
        let design = estimableDesign |> List.map (fun (factors, cov) -> (getCategoricalSlicer factors), cov |> Option.map (fun c -> c.AsCovariate)) 
        let beta, invHDiag, iter, converged = iwls response design glmDistribution glmLink initBeta maxIter 0 length sliceLength cumEstimateCounts eps
        if converged then
            printfn "converged in %d iter" iter
            let parameters = getParameterEstimates estimableDesign beta invHDiag cumEstimateCounts
            let goodnessOfFit = getGoodnessOfFit response design glmDistribution glmLink beta length sliceLength cumEstimateCounts
            (parameters, goodnessOfFit) |> Some
        else 
            None

