namespace FCor
#nowarn "9"

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic
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
                let factors = maskedFactors |> List.map fst |> List.toArray
                let allLevelsCount = maskedFactors |> List.map fst |> List.map (fun f -> f.LevelCount) |> List.reduce (*) 
                let dimProd = maskedFactors |> List.map fst |> List.map (fun f -> f.LevelCount) |> getDimProd |> List.toArray
                let isDisabled = getDisabled maskedFactors
                let cumDisabledCount = Array.sub (isDisabled |> Array.scan (fun cum x -> if x then cum + 1 else cum) 0) 1 isDisabled.Length
                let estimateMap = Array.init isDisabled.Length (fun i -> if isDisabled.[i] then -1 else i - cumDisabledCount.[i])
                let subscripts = Array.create factors.Length -1 // cache
                let cachedFromObs : int64 option ref = ref None
                let cachedToObs : int64 option ref = ref None
                let cachedSlice : IntVector option ref = ref None 
                let slicerFun =
                    fun (fromObs : int64, toObs : int64) ->
                        match !cachedFromObs, !cachedToObs, !cachedSlice with
                            | Some(a), Some(b), Some(v) when a = fromObs && b = toObs -> v
                            | _ ->
                                match !cachedSlice with
                                    | Some(v) -> v.Dispose()
                                    | _ -> ()
                                if factors.Length = 1 then
                                    let sliceLen = toObs - fromObs + 1L
                                    let slice = factors.[0].GetSlice(fromObs, toObs)
                                    let res = new IntVector(sliceLen, 0)
                                    for i in 0L..sliceLen - 1L do
                                        res.[i] <- estimateMap.[slice.[i]]
                                    cachedFromObs := Some fromObs
                                    cachedToObs := Some toObs
                                    cachedSlice := Some res
                                    res
                                else
                                    let slices = factors |> Array.map (fun f -> f.GetSlice(fromObs, toObs))
                                    let sliceLen = toObs - fromObs + 1L
                                    let res = new IntVector(sliceLen, 0)
                                    for i in 0L..sliceLen - 1L do
                                        for j in 0..factors.Length - 1 do
                                            subscripts.[j] <- slices.[j].[i] 
                                        let ind = sub2ind dimProd subscripts
                                        res.[i] <- estimateMap.[ind]
                                    cachedFromObs := Some fromObs
                                    cachedToObs := Some toObs
                                    cachedSlice := Some res
                                    res
                (slicerFun, allLevelsCount) |> Some
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
                (resp - pred) ./ pred 
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
                new Vector(pred.Length, 1.0)
            | Gaussian, Ln ->
                pred .* pred
            | Gaussian, Log(d) -> 
                Math.Log(d) * pred * Math.Log(d) .* pred
            | Gaussian, Power(d) ->
                d * d * (pred .^ (2.0 - 2.0 / d))
            | Gamma, Id -> 
                1.0 ./ (pred .*pred)
            | Gamma, Ln ->
                new Vector(pred.Length, 1.0) 
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

    let getXBeta (design : (((int64 * int64 -> IntVector) * int) option * Covariate option) array) (beta : Vector) (slice : (int64*int64))
                 (cumEstimateCounts: int[]) =
        let fromObs, toObs = slice
        let sliceLen = toObs - fromObs + 1L
        let xbeta = new Vector(sliceLen, 0.0)
        design |> Array.iteri (fun pInd p ->
                                match p with
                                    | Some(slicer, allLevelCount), None ->  
                                        let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1]  
                                        if allLevelCount = 1 then
                                            VectorExpr.EvalIn(xbeta.AsExpr + beta.[offset], Some xbeta) |> ignore
                                        else
                                            let slice = slicer(fromObs, toObs)
                                            MklFunctions.Glm_Update_XBeta(sliceLen, xbeta.NativeArray, slice.NativeArray, beta.NativeArray, Vector.Empty.NativeArray, offset)
                                    | None, Some(covariate) -> 
                                        use slice = covariate.GetSlice(fromObs, toObs)
                                        let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1] 
                                        VectorExpr.EvalIn(xbeta.AsExpr + beta.[offset] * slice.AsExpr, Some xbeta) |> ignore
                                    | Some(slicer, allLevelCount), Some(covariate) -> 
                                        let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1] 
                                        use sliceCov = covariate.GetSlice(fromObs, toObs)
                                        if allLevelCount = 1 then
                                            VectorExpr.EvalIn(xbeta.AsExpr + beta.[offset] * sliceCov.AsExpr, Some xbeta) |> ignore
                                        else
                                            let slice = slicer(fromObs, toObs)
                                            MklFunctions.Glm_Update_XBeta(sliceLen, xbeta.NativeArray, slice.NativeArray, beta.NativeArray, sliceCov.NativeArray, offset)
                                    | _ -> ()
                                )
        xbeta

    let getU (design : (((int64 * int64 -> IntVector) * int) option * Covariate option) array) (u : Vector) (slice : (int64*int64))
             (cumEstimateCounts: int[]) =
        let U = new Vector(cumEstimateCounts.[cumEstimateCounts.Length - 1], 0.0)
        let fromObs, toObs = slice
        let sliceLen = toObs - fromObs + 1L
        design |> Array.iteri (fun pInd p ->
                                match p with
                                    | Some(slicer, allLevelCount), None ->   
                                        let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1] 
                                        if allLevelCount = 1 then
                                            U.[offset] <- U.[offset] + Vector.Sum(u)
                                        else
                                            let slice = slicer(fromObs, toObs)
                                            MklFunctions.Glm_Update_U(sliceLen, U.NativeArray, u.NativeArray, slice.NativeArray, Vector.Empty.NativeArray, offset)
                                    | None, Some(covariate) -> 
                                        use slice = covariate.GetSlice(fromObs, toObs)
                                        let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1] 
                                        U.[offset] <- U.[offset] + u * slice
                                    | Some(slicer, allLevelCount), Some(covariate) ->
                                        let offset = if pInd = 0 then 0 else cumEstimateCounts.[pInd-1]  
                                        use sliceCov = covariate.GetSlice(fromObs, toObs)
                                        if allLevelCount = 1 then
                                            U.[offset] <- U.[offset] + u * sliceCov
                                        else
                                            let slice = slicer(fromObs, toObs)
                                            MklFunctions.Glm_Update_U(sliceLen, U.NativeArray, u.NativeArray, slice.NativeArray, sliceCov.NativeArray, offset)
                                    | _ -> ()
                                )
        U

    let getH (design : (((int64 * int64 -> IntVector) * int) option * Covariate option) array) (weight : Vector) (slice : (int64*int64))
             (cumEstimateCounts: int[]) =
        let p = cumEstimateCounts.[cumEstimateCounts.Length - 1]
        let H = new Matrix(p, p, 0.0)
        let fromObs, toObs = slice
        let sliceLen = toObs - fromObs + 1L
        for pRow in 0..design.Length-1 do
            for pCol in pRow..design.Length-1 do
                match design.[pRow], design.[pCol] with
                    | (Some(rowSlicer, rowAllLevelCount), None), (Some(colSlicer, colAllLevelCount), None) ->
                        let rowOffset = if pRow = 0 then 0 else cumEstimateCounts.[pRow-1] 
                        let colOffset = if pCol = 0 then 0 else cumEstimateCounts.[pCol-1] 
                        if rowAllLevelCount = 1 && colAllLevelCount = 1 then
                            H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + Vector.Sum(weight)
                        else                       
                            let rowSlice = rowSlicer(fromObs, toObs)
                            let colSlice = colSlicer(fromObs, toObs)
                            MklFunctions.Glm_Update_H(sliceLen, p, H.ColMajorDataVector.NativeArray, weight.NativeArray, Vector.Empty.NativeArray,
                                                      Vector.Empty.NativeArray, rowSlice.NativeArray, colSlice.NativeArray, rowOffset, colOffset)

                    | (Some(rowSlicer, rowAllLevelCount), None), (None, Some(colCovariate)) ->
                        let rowOffset = if pRow = 0 then 0 else cumEstimateCounts.[pRow-1] 
                        let colOffset = if pCol = 0 then 0 else cumEstimateCounts.[pCol-1] 
                        use colSlice = colCovariate.GetSlice(fromObs, toObs)
                        if rowAllLevelCount = 1 then
                            H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + weight * colSlice
                        else
                            let rowSlice = rowSlicer(fromObs, toObs)
                            MklFunctions.Glm_Update_H(sliceLen, p, H.ColMajorDataVector.NativeArray, weight.NativeArray, Vector.Empty.NativeArray,
                                                      colSlice.NativeArray, rowSlice.NativeArray, IntVector.Empty.NativeArray, rowOffset, colOffset)

                    | (Some(rowSlicer, rowAllLevelCount), None), (Some(colSlicer, colAllLevelCount), Some(colCovariate)) ->
                        let rowOffset = if pRow = 0 then 0 else cumEstimateCounts.[pRow-1] 
                        let colOffset = if pCol = 0 then 0 else cumEstimateCounts.[pCol-1] 
                        use colCovSlice = colCovariate.GetSlice(fromObs, toObs)
                        if rowAllLevelCount = 1 && colAllLevelCount = 1 then
                            H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + weight * colCovSlice
                        else
                            let rowSlice = rowSlicer(fromObs, toObs)
                            let colSlice = colSlicer(fromObs, toObs)
                            MklFunctions.Glm_Update_H(sliceLen, p, H.ColMajorDataVector.NativeArray, weight.NativeArray, Vector.Empty.NativeArray,
                                                      colCovSlice.NativeArray, rowSlice.NativeArray, colSlice.NativeArray, rowOffset, colOffset)


                    | (None, Some(rowCovariate)), (None, Some(colCovariate)) -> 
                        use rowSlice = rowCovariate.GetSlice(fromObs, toObs)
                        use colSlice = colCovariate.GetSlice(fromObs, toObs)
                        let rowOffset = if pRow = 0 then 0 else cumEstimateCounts.[pRow-1]  
                        let colOffset = if pCol = 0 then 0 else cumEstimateCounts.[pCol-1]
                        use prod = VectorExpr.EvalIn((weight.AsExpr .* rowSlice) .* colSlice, None)
                        H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + Vector.Sum(prod)

                    | (None, Some(rowCovariate)), (Some(colSlicer, colAllLevelCount), None) -> 
                        use rowSlice = rowCovariate.GetSlice(fromObs, toObs)
                        let rowOffset = if pRow = 0 then 0 else cumEstimateCounts.[pRow-1]  
                        let colOffset = if pCol = 0 then 0 else cumEstimateCounts.[pCol-1]
                        if colAllLevelCount = 1 then
                            H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + weight * rowSlice
                        else
                            let colSlice = colSlicer(fromObs, toObs)
                            MklFunctions.Glm_Update_H(sliceLen, p, H.ColMajorDataVector.NativeArray, weight.NativeArray, rowSlice.NativeArray,
                                                      Vector.Empty.NativeArray, IntVector.Empty.NativeArray, colSlice.NativeArray, rowOffset, colOffset)

                    | (None, Some(rowCovariate)), (Some(colSlicer, colAllLevelCount), Some(colCovariate)) -> 
                        use rowSlice = rowCovariate.GetSlice(fromObs, toObs)
                        use colCovSlice = colCovariate.GetSlice(fromObs, toObs)
                        let rowOffset = if pRow = 0 then 0 else cumEstimateCounts.[pRow-1]  
                        let colOffset = if pCol = 0 then 0 else cumEstimateCounts.[pCol-1]
                        use prod = VectorExpr.EvalIn(weight.AsExpr .* rowSlice .* colCovSlice, None)
                        if colAllLevelCount = 1 then
                            H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + Vector.Sum(prod)
                        else
                            let colSlice = colSlicer(fromObs, toObs)
                            MklFunctions.Glm_Update_H(sliceLen, p, H.ColMajorDataVector.NativeArray, weight.NativeArray, rowSlice.NativeArray,
                                                      colCovSlice.NativeArray, IntVector.Empty.NativeArray, colSlice.NativeArray, rowOffset, colOffset)

                    | (Some(rowSlicer, rowAllLevelCount), Some(rowCovariate)), (Some(colSlicer, colAllLevelCount), Some(colCovariate)) -> 
                        let rowOffset = if pRow = 0 then 0 else cumEstimateCounts.[pRow-1]  
                        let colOffset = if pCol = 0 then 0 else cumEstimateCounts.[pCol-1]
                        use rowSliceCov = rowCovariate.GetSlice(fromObs, toObs)
                        use colSliceCov = colCovariate.GetSlice(fromObs, toObs)
                        if rowAllLevelCount = 1 && colAllLevelCount = 1 then
                            use prod = weight .* rowSliceCov .* colSliceCov
                            H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + Vector.Sum(prod)
                        else
                            let rowSlice = rowSlicer(fromObs, toObs)
                            let colSlice = colSlicer(fromObs, toObs)
                            MklFunctions.Glm_Update_H(sliceLen, p, H.ColMajorDataVector.NativeArray, weight.NativeArray, rowSliceCov.NativeArray,
                                                      colSliceCov.NativeArray, rowSlice.NativeArray, colSlice.NativeArray, rowOffset, colOffset)

                    | (Some(rowSlicer, rowAllLevelCount), Some(rowCovariate)), (None, Some(colCovariate)) -> 
                        use rowSliceCov = rowCovariate.GetSlice(fromObs, toObs)
                        use colSliceCov = colCovariate.GetSlice(fromObs, toObs)
                        let rowOffset = if pRow = 0 then 0 else cumEstimateCounts.[pRow-1]  
                        let colOffset = if pCol = 0 then 0 else cumEstimateCounts.[pCol-1]
                        use prod = VectorExpr.EvalIn(weight.AsExpr .* rowSliceCov .* colSliceCov , None)
                        if rowAllLevelCount = 1 then
                            H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + Vector.Sum(prod) 
                        else
                            let rowSlice = rowSlicer(fromObs, toObs)
                            MklFunctions.Glm_Update_H(sliceLen, p, H.ColMajorDataVector.NativeArray, weight.NativeArray, rowSliceCov.NativeArray,
                                                      colSliceCov.NativeArray, rowSlice.NativeArray, IntVector.Empty.NativeArray, rowOffset, colOffset)

                    | (Some(rowSlicer, rowAllLevelCount), Some(rowCovariate)), (Some(colSlicer, colAllLevelCount), None) -> 
                        let rowOffset = if pRow = 0 then 0 else cumEstimateCounts.[pRow-1]  
                        let colOffset = if pCol = 0 then 0 else cumEstimateCounts.[pCol-1]
                        use rowSliceCov = rowCovariate.GetSlice(fromObs, toObs)
                        if rowAllLevelCount = 1 && colAllLevelCount = 1 then
                            H.[rowOffset, colOffset] <- H.[rowOffset, colOffset] + weight * rowSliceCov
                        else
                            let rowSlice = rowSlicer(fromObs, toObs)
                            let colSlice = colSlicer(fromObs, toObs)
                            MklFunctions.Glm_Update_H(sliceLen, p, H.ColMajorDataVector.NativeArray, weight.NativeArray, rowSliceCov.NativeArray,
                                                      Vector.Empty.NativeArray, rowSlice.NativeArray, colSlice.NativeArray, rowOffset, colOffset)

                    | _ -> ()
        H

    let rec iwls (response : Covariate) (design : (((int64 * int64 -> IntVector) * int) option * Covariate option) array)
                 (glmDistribution : GlmDistribution) (glmLink : GlmLink) (beta : Vector)
                 (maxIter : int) (iter : int) (slices : (int64*int64)[]) (cumEstimateCounts : int[]) eps =
        if iter > maxIter then 
            beta, Vector.Empty, iter, false
        else
            let totalEstimateCount = cumEstimateCounts.[cumEstimateCounts.Length - 1]
            use U = new Vector(totalEstimateCount, 0.0)
            use H = new Matrix(totalEstimateCount, totalEstimateCount, 0.0)
            for (fromObs, toObs) in slices do
               use resp = response.GetSlice(fromObs, toObs)
               use xbeta = getXBeta design beta (fromObs, toObs) cumEstimateCounts
               use pred = getPred xbeta glmLink
               use u = get_u resp pred glmLink glmDistribution
               use weight = getWeight resp glmLink glmDistribution
               use updateU = getU design u (fromObs,toObs) cumEstimateCounts
               use updateH = getH design weight (fromObs,toObs) cumEstimateCounts
               VectorExpr.EvalIn(U.AsExpr + updateU, Some U) |> ignore
               MatrixExpr.EvalIn(H.AsExpr + updateH, Some H) |> ignore

            //calc next beta from U and H
            //MatrixExpr.EvalIn(H.AsExpr + (Matrix.Transpose(Matrix.UpperTri(H, 1))), Some H) |> ignore
            use delta = Matrix.CholSolve(H, U)
            let nextBeta = beta + delta
            if (glmDistribution = Gaussian && glmLink = Id) || epsEqualVector beta nextBeta eps then
                let invHDiag = Matrix.CholInv(H).Diag(0)
                nextBeta, invHDiag,  iter, true
            else
                iwls response design glmDistribution glmLink nextBeta maxIter (iter + 1) slices cumEstimateCounts eps

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

    let getGoodnessOfFit (response : Covariate) (design : (((int64 * int64 -> IntVector) * int) option * Covariate option) array)
                         (glmDistribution : GlmDistribution) (glmLink : GlmLink) (beta : Vector)
                         (slices : (int64*int64)[]) (cumEstimateCounts : int[]) =

        let tempRes = 
            slices |> Array.map (fun (fromObs, toObs) ->
                                    use xbeta = getXBeta design beta (fromObs, toObs) cumEstimateCounts
                                    use pred = getPred xbeta glmLink
                                    use resp = response.GetSlice(fromObs, toObs)
                                    use deviance = getDeviance resp pred glmDistribution
                                    use pearsonChi = getPearsonChi resp pred glmDistribution
                                    use likelihoodPart = getLogLikelihoodPart resp pred glmDistribution
                                    new Vector([|Vector.Sum(deviance); Vector.Sum(pearsonChi); Vector.Sum(likelihoodPart)|])
                                )
                   |> Array.reduce (+)
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
        let slices = getSlices length (int64(sliceLength))
        let meanReponseSlice = Vector.Mean(response.GetSlice(slices.[0]))

        let estimableDesign = getEstimableDesign predictors includeIntercept
        let estimateCounts = estimableDesign |> List.map getEstimateCount |> List.toArray
        let cumEstimateCounts = Array.sub (estimateCounts |> Array.scan (+) 0) 1 estimateCounts.Length
        let initBeta = getInitBeta cumEstimateCounts.[cumEstimateCounts.Length - 1] glmLink meanReponseSlice
        let design = estimableDesign |> List.map (fun (factors, cov) -> (getCategoricalSlicer factors), cov |> Option.map (fun c -> c.AsCovariate)) |> List.toArray
        let beta, invHDiag, iter, converged = iwls response design glmDistribution glmLink initBeta maxIter 0 slices cumEstimateCounts eps
        if converged then
            printfn "converged in %d iter" iter
            let parameters = getParameterEstimates estimableDesign beta invHDiag cumEstimateCounts
            let goodnessOfFit = getGoodnessOfFit response design glmDistribution glmLink beta slices cumEstimateCounts
            (parameters, goodnessOfFit) |> Some
        else 
            None

