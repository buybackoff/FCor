namespace FCor
#nowarn "9"

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic 
open FCor.ExplicitConversion
open System.Text

[<StructuredFormatDisplay("{AsString}")>]
type CovariateStats =
    {
     ObsCount : int64
     NaNCount : int64
     ``NaN%`` : float
     Mean : float
     Std : float
     Min : float
     Max : float
    }
    member this.AsString =
        let sb = new StringBuilder()
        sb.AppendLine() |> ignore
        sb.AppendLine <| sprintf "%-15s%12d" "Obs Count" this.ObsCount |> ignore
        sb.AppendLine <| sprintf "%-15s%12d" "NaN Freq" this.NaNCount |> ignore
        sb.AppendLine <| sprintf "%-15s%12G" "NaN (%)" (100. * this.``NaN%``) |> ignore
        sb.AppendLine <| sprintf "%-15s%12G" "Min" this.Min |> ignore
        sb.AppendLine <| sprintf "%-15s%12G" "Max" this.Max |> ignore
        sb.AppendLine <| sprintf "%-15s%12G" "Mean" this.Mean |> ignore
        sb.AppendLine <| sprintf "%-15s%12G" "Std" this.Std |> ignore
        sb.ToString()

type FactorLevelStats =
    {
     Level : string
     Freq : int64
     ``Freq%`` : float
    }

[<StructuredFormatDisplay("{AsString}")>]
type FactorStats =
    {
     ObsCount : int64
     MissingFreq :  int64
     ``Missing%`` : float
     LevelStats : FactorLevelStats[]
    }
    member this.AsString =
        let sb = new StringBuilder()
        sb.AppendLine() |> ignore
        sb.AppendLine <| sprintf "%-15s%12d" "Obs Count" this.ObsCount |> ignore
        sb.AppendLine <| sprintf "%-15s%12d" "Missing Freq" this.MissingFreq |> ignore
        sb.AppendLine <| sprintf "%-15s%12G" "Missing %" (100. * this.``Missing%``) |> ignore
        sb.AppendLine() |> ignore
        sb.AppendLine <| sprintf "%-15s%s%-12s%s%-12s" "Level" "  " "Frequency" "  " "Frequency %" |> ignore
        this.LevelStats |> Array.iter (fun levelStat -> 
                                           sb.AppendLine <| sprintf "%-15s%s%12d%s%12G" levelStat.Level "  " levelStat.Freq "  " (100.0 * levelStat.``Freq%``) |> ignore
                                      )
        sb.ToString()

type Factor(name : string, factorStorage : IFactorStorage) =

    static let factorExprCache = new Dictionary<obj, Factor>()

    static let intercept = 
        let factorStorage = 
             {
              new IFactorStorage with
                  member __.Level with get(index) = "<Intercept>"
                  member __.Length = 1L
                  member __.Cardinality = 1
                  member __.GetSlices(fromObs : int64, toObs : int64, sliceLength : int) = 
                    seq
                      {
                        let length = toObs - fromObs + 1L
                        let sliceLength = int64 sliceLength
                        let m = length / sliceLength |> int
                        let k = length % sliceLength 
                        use buffer = new UInt16Vector((if m > 0 then sliceLength else k), 0us)
                        for i in 0..m-1 do
                            yield buffer
                        if k > 0L then
                            yield buffer.View(0L, k-1L)
                      }
             }
        new Factor("<INTERCEPT>", factorStorage)

    static member internal FactorExprCache = factorExprCache

    static member NAs = [|String.Empty;"#N/A";"N/A";"#n/a";"n/a";"."|]

    member this.Name = name
    member this.Length = factorStorage.Length
    member this.Cardinality = factorStorage.Cardinality

    member this.GetSlices(fromObs : int64, toObs : int64, sliceLen : int) = factorStorage.GetSlices(fromObs, toObs, sliceLen)

    member this.Level with get(levelIndex) = factorStorage.Level(levelIndex)

    member this.GetStats() = this.GetStats(10000)

    member this.GetStats(sliceLen : int) =
        let processorCount = Environment.ProcessorCount
        let processorChunk = factorStorage.Length / int64(processorCount)
        let levelFreq = 
            seq{0..processorCount - 1} 
                |> Seq.map (fun i -> (int64(i) * processorChunk), if i = processorCount - 1 then factorStorage.Length - 1L else (int64(i + 1) * processorChunk - 1L))
                |> Seq.toArray 
                |> Array.Parallel.map (fun (fromObs, toObs) -> 
                                            this.GetSlices(fromObs, toObs, sliceLen)
                                                |> Seq.fold (fun freq slice -> 
                                                                MklFunctions.Update_Factor_Freq(slice.Length, slice.NativeArray, freq)
                                                                freq
                                                            ) (Array.create factorStorage.Cardinality 0L)
                                        )
                |> Array.reduce (fun freq1 freq2 -> freq2 |> Array.zip freq1 |> Array.map (fun (x,y) -> x + y))

        let nans = new Set<_>(Factor.NAs)
        let missingFreq =
            [|0..this.Cardinality - 1|] |> Array.fold (fun freq levelIndex -> 
                                                        if nans.Contains(this.Level(levelIndex)) then
                                                            freq + levelFreq.[levelIndex]
                                                        else freq) 0L
        let levelStats = 
            Array.init this.Cardinality (fun i -> 
                                            {
                                             Level = this.Level(i)
                                             Freq = levelFreq.[i]
                                             ``Freq%`` = float(levelFreq.[i]) / float(factorStorage.Length)
                                            }            
                                       )
        {
         ObsCount = factorStorage.Length
         MissingFreq = missingFreq
         ``Missing%`` = float(missingFreq) / float(factorStorage.Length)
         LevelStats = levelStats
        }

    member this.AsSeq =
        let slices = this.GetSlices(0L, factorStorage.Length - 1L, 10000)
        slices |> Seq.map (fun slice -> 
                               seq{0L..slice.LongLength - 1L} 
                               |> Seq.map (fun index -> let levelIndex = slice.[index] in levelIndex, this.Level(int levelIndex)))
                               |> Seq.concat


    interface IFormattable with
        member this.ToString(format, provider) = 
            let n, _ = DisplayControl.MaxDisplaySize
            let slice = this.AsSeq |> Seq.take (min n factorStorage.Length |> int) |> Seq.map snd |> Seq.toArray
            let more = if int64(slice.Length) < factorStorage.Length then "..." else ""
            let data = slice |> String.concat " "
            sprintf "Factor '%s' with %d obs and %d levels: %s %s" name factorStorage.Length factorStorage.Cardinality data more

    override this.ToString() =
        (this:>IFormattable).ToString("", null)

    member this.AsExpr = FactorExpr.Var(this)

    static member Intercept = intercept

    static member op_Explicit(factor : Factor) : FactorExpr = FactorExpr.Var(factor)

    static member op_Explicit(factor : Factor) : Predictor = CategoricalPredictor(!!factor)

    static member op_Explicit(factor : Factor) : CategoricalPredictor = CategoricalPredictor.Factor(!!factor)

    static member op_Explicit(x : string * string[]) =
        let name, data = x
        let factorStorage = new FactorStorage()
        factorStorage.SetSlice(0L, data)
        new Factor(name, factorStorage)

    static member (*) (x : Factor, y : Factor) : CategoricalPredictor = !!x * !!y

    static member (*) (x : Factor, y : FactorExpr) : CategoricalPredictor = !!x * !!y

    static member (*) (x : Factor, y : CategoricalPredictor) : CategoricalPredictor = !!x * y

    static member (*) (x : Factor, y : Covariate) : Predictor = !!x * !!y

    static member (*) (x : Factor, y : CovariateExpr) : Predictor = !!x * !!y

    static member (*) (x : Factor, y : StatVariable) : Predictor = !!x * !!y

    static member (*) (x : Factor, y : Predictor) : Predictor = !!x * y


    static member (+) (x : Factor, y : Factor) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : Factor, y : FactorExpr) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : Factor, y : CategoricalPredictor) : Predictor list = !!x + !!y

    static member (+) (x : Factor, y : Covariate) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : Factor, y : CovariateExpr) : Predictor list = !!x + !!y

    static member (+) (x : Factor, y : StatVariable) : Predictor list = !!x + (!!y : Predictor)

    static member (+) (x : Factor, y : Predictor) : Predictor list = !!x + y

    static member (+) (x : Predictor list, y : Factor) : Predictor list = x + (!!y:Predictor)

    static member (+) (x : Factor, y : Predictor list) : Predictor list = (!!x:Predictor) + y

    static member (.*) (x : Factor, y : Factor) = Cross(x.AsExpr, y.AsExpr)


    interface IDisposable with
        member this.Dispose() = this.DoDispose(true)

    member internal this.DoDispose(isDisposing) = 
        if isDisposing then GC.SuppressFinalize(this)
        match factorStorage with
            | :? IDisposable as d -> d.Dispose()
            | _ -> ()

    override this.Finalize() = try this.DoDispose(false) with _ -> ()

and [<StructuredFormatDisplay("{AsString}")>] FactorExpr =

    | Var of Factor
    | Rename of FactorExpr * (string -> string)
    | MergeLevels of FactorExpr * (string seq)
    | Cross of FactorExpr * FactorExpr
    | Cut of CovariateExpr * float[]
    | Int of CovariateExpr * int[]
    | Permute of FactorExpr * ((int * string)[])


    member this.Vars =
        match this with
            | Var(f) -> [StatVariable.Factor f]
            | Rename(f, _) -> f.Vars
            | MergeLevels(f, _) -> f.Vars
            | Cross(f1, f2) -> f1.Vars @ f2.Vars
            | Cut(c, _) -> c.Vars
            | Int(c, _) -> c.Vars
            | Permute(f, _) -> f.Vars

    member this.MinLength =
        match this with
            | Var(f) ->  f.Length
            | Rename(f, _) -> f.MinLength 
            | MergeLevels(f, _) -> f.MinLength
            | Cross(f1, f2) -> min f1.MinLength f2.MinLength
            | Cut(c, _) -> c.MinLength
            | Int(c, _) -> c.MinLength
            | Permute(f, _) -> f.MinLength

    member this.MaxLength =
        match this with
            | Var(f) ->  f.Length
            | Rename(f, _) -> f.MaxLength 
            | MergeLevels(f, _) -> f.MaxLength
            | Cross(f1, f2) -> max f1.MaxLength f2.MaxLength
            | Cut(c, _) -> c.MaxLength
            | Int(c, _) -> c.MaxLength
            | Permute(f, _) -> f.MaxLength

    member this.Name =
        match this with
            | Var(f) ->  f.Name
            | Rename(f, _) -> f.Name 
            | MergeLevels(f, _) -> f.Name
            | Cross(f1, f2) -> sprintf "%s.*%s" f1.Name f2.Name
            | Cut(c, _) -> sprintf "%s.AsFactor" c.Name 
            | Int(c, _) -> sprintf "%s.AsFactor" c.Name 
            | Permute(f, _) -> f.Name

    static member Substitute (mapF :  StatVariable -> StatVariable) (factorExpr : FactorExpr) =
        match factorExpr with
            | Var(f) -> Var(mapF !!f |> (!!))
            | Rename(f, renameF) -> Rename(FactorExpr.Substitute mapF f, renameF)
            | MergeLevels(f, s) -> MergeLevels(FactorExpr.Substitute mapF f, s)
            | Cross(f1, f2) -> Cross(FactorExpr.Substitute mapF f1, FactorExpr.Substitute mapF f2) 
            | Cut(cov, breaks) -> Cut(CovariateExpr.Substitute mapF cov, breaks) 
            | Int(cov, knots) -> Int(CovariateExpr.Substitute mapF cov, knots) 
            | Permute(f, permutation) -> Permute(FactorExpr.Substitute mapF f, permutation)

    member this.AsFactor =
        if Factor.FactorExprCache.ContainsKey(this:>obj) then Factor.FactorExprCache.[this:>obj]
        else
        let factor = 
            match this with
                | Var(factor) -> factor
                | Rename(factor, renameFun) ->
                    let factor = factor.AsFactor
                    let dict1 = new Dictionary<string, int>()
                    let dict2 = new Dictionary<int, string>()
                    [|0..factor.Cardinality - 1|] |> Array.iter (fun index -> let newLevel = index |> factor.get_Level |> renameFun
                                                                              if not <| dict1.ContainsKey(newLevel) then
                                                                                    let levelIndex = dict1.Count
                                                                                    dict1.Add(newLevel, levelIndex)
                                                                                    dict2.Add(levelIndex, newLevel)
                                                               )
                    let levelMap = Array.init factor.Cardinality (fun index -> dict1.[factor.Level(index) |> renameFun] |> uint16)
                    let factorStorage = 
                         {
                          new IFactorStorage with
                              member __.Level with get(index) = dict2.[index]
                              member __.Length = factor.Length
                              member __.Cardinality = dict1.Count
                              member __.GetSlices(fromObs : int64, toObs : int64, sliceLength : int) = 
                                  factor.GetSlices(fromObs, toObs, sliceLength) |> Seq.map (fun v -> MklFunctions.Update_Level_Index(v.Length, v.NativeArray, levelMap)
                                                                                                     v)
                         }
                    new Factor(factor.Name, factorStorage)

                | MergeLevels(factor, mergedLevels) ->
                    let factor = factor.AsFactor
                    let mergedLevel = String.Join("|", mergedLevels)
                    let mergedLevels = new Set<_>(mergedLevels)
                    let isMerged = Array.init factor.Cardinality (fun i -> mergedLevels.Contains(factor.Level i))
                    let cumMergedCount = Array.sub (isMerged |> Array.scan (fun cum isMerged -> if isMerged then cum + 1 else cum) 0) 1 isMerged.Length
                                         |> Array.map (fun cum -> if cum = 0 then cum else cum - 1)
                    let levelMap = Array.init factor.Cardinality (fun i -> (i - cumMergedCount.[i]) |> uint16)
                    let factorStorage = 
                         {
                          new IFactorStorage with
                              member __.Level
                                  with get(index) =
                                      if isMerged.[index] && cumMergedCount.[index] = 0 then
                                          mergedLevel
                                      else 
                                          factor.Level(index + cumMergedCount.[index])
                              member __.Length = factor.Length
                              member __.Cardinality = factor.Cardinality - cumMergedCount.[factor.Cardinality - 1]
                              member __.GetSlices(fromObs : int64, toObs : int64, sliceLength : int) = 
                                  factor.GetSlices(fromObs, toObs, sliceLength) |> Seq.map (fun v -> MklFunctions.Update_Level_Index(v.Length, v.NativeArray, levelMap)
                                                                                                     v)
                         }
                    new Factor(factor.Name, factorStorage)
                | Cross(f1, f2) -> 
                    let factor1 = f1.AsFactor
                    let factor2 = f2.AsFactor
                    let levelCount = factor1.Cardinality * factor2.Cardinality
                    if levelCount > int UInt16.MaxValue then raise (new ArgumentException("Too many levels in cross factor"))
                    let factorStorage = 
                         {
                          new IFactorStorage with
                              member __.Level
                                  with get(index) =
                                      let index1 = index % factor1.Cardinality
                                      let index2 = index / factor1.Cardinality
                                      sprintf "%s*%s" (factor1.Level(index1)) (factor2.Level(index2))
                              member __.Length = if factor1.Length <> factor2.Length then raise (new ArgumentException("Factor length mismatch")) else factor1.Length
                              member __.Cardinality = levelCount
                              member __.GetSlices(fromObs : int64, toObs : int64, sliceLength : int) = 
                                  factor2.GetSlices(fromObs, toObs, sliceLength) |> Seq.zip (factor1.GetSlices(fromObs, toObs, sliceLength))
                                      |> Seq.map (fun (slice1, slice2) -> let v = new UInt16Vector(slice1.Length, 0us)
                                                                          MklFunctions.Get_Cross_Level_Index(slice1.Length, uint16 factor1.Cardinality, slice1.NativeArray, slice2.NativeArray, v.NativeArray)
                                                                          v)
                          
                         }
                    new Factor(Cross(f1, f2).Name, factorStorage)
                | Cut(c, breaks) ->
                    let covariate : Covariate = c.AsCovariate
                    let breaks = breaks |> Array.filter (fun x -> not <| Double.IsNaN(x)) |> Array.sort
                    if breaks.Length > int UInt16.MaxValue then raise (new ArgumentException("Too many breaks"))
                    if breaks.Length < 3 then raise (new ArgumentException("There must be at least 3 break points"))
                    let factorStorage = 
                         {
                          new IFactorStorage with
                              member __.Level
                                  with get(index) =
                                      if index = breaks.Length - 1 then
                                          String.Empty
                                      elif index = breaks.Length - 2 then
                                          sprintf "[%G,%G]" breaks.[index] breaks.[index + 1]
                                      else
                                          sprintf "[%G,%G)" breaks.[index] breaks.[index + 1]
                              member __.Length = covariate.Length
                              member __.Cardinality = breaks.Length
                              member __.GetSlices(fromObs : int64, toObs : int64, sliceLength : int) =
                                  covariate.GetSlices(fromObs, toObs, sliceLength) |> Seq.map (fun (slice : Vector) -> let v = new UInt16Vector(slice.Length, 0us)
                                                                                                                       MklFunctions.Get_Cut_Level_Index(slice.Length, breaks, slice.NativeArray, v.NativeArray)
                                                                                                                       v
                                                                                              )
                          
                         }
                    new Factor(Cut(c, breaks).Name, factorStorage)
                | Int(c, knots) ->
                    let covariate : Covariate = c.AsCovariate
                    let knots = knots |> Array.sort
                    if knots.Length - 1 > int UInt16.MaxValue then raise (new ArgumentException("Too many knots"))
                    if knots.Length < 2 then raise (new ArgumentException("There must be at least 2 knots"))
                    let factorStorage = 
                         {
                          new IFactorStorage with
                              member __.Level
                                  with get(index) =
                                      if index = knots.Length then
                                          String.Empty
                                      else
                                          sprintf "%d" knots.[index]
                              member __.Length = covariate.Length
                              member __.Cardinality = knots.Length + 1 // if outside then ""
                              member __.GetSlices(fromObs : int64, toObs : int64, sliceLength : int) =
                                  covariate.GetSlices(fromObs, toObs, sliceLength) |> Seq.map (fun (slice : Vector) -> let v = new UInt16Vector(slice.Length, 0us)
                                                                                                                       MklFunctions.Get_Knot_Level_Index(slice.Length, knots, slice.NativeArray, v.NativeArray)
                                                                                                                       v
                                                                                              )
                          
                         }
                    new Factor(Int(c, knots).Name, factorStorage)

                | Permute(factor, permutation) ->
                    let factor = factor.AsFactor
                    if (permutation |> Array.map fst |> Array.sort <> [|0..permutation.Length - 1|]) then
                        raise (new ArgumentException("Invalid factor level permutation"))
                    let levelSet = permutation |> Array.map snd |> Set.ofArray
                    if levelSet.Count <> permutation.Length then 
                       raise (new ArgumentException("Duplicate level in factor level permutation"))
                    if permutation.Length < factor.Cardinality then
                        raise (new ArgumentException("Incomplete permutation of factor levels"))
                    let levelMap = permutation |> Array.map fst |> Array.map Checked.uint16
                    let permutationMap = permutation |> Map.ofArray
                    let factorStorage = 
                         {
                          new IFactorStorage with
                              member __.Level with get(index) = permutationMap.[index]
                              member __.Length = factor.Length
                              member __.Cardinality = permutation.Length
                              member __.GetSlices(fromObs : int64, toObs : int64, sliceLength : int) = 
                                  factor.GetSlices(fromObs, toObs, sliceLength) |> Seq.map (fun v -> MklFunctions.Update_Level_Index(v.Length, v.NativeArray, levelMap)
                                                                                                     v)
                         }
                    new Factor(factor.Name, factorStorage)
        Factor.FactorExprCache.Add(this:>obj, factor)
        factor

    static member op_Explicit(x : FactorExpr) : Predictor = Predictor.CategoricalPredictor(!!x)

    static member op_Explicit(x : FactorExpr) : CategoricalPredictor = CategoricalPredictor.Factor(x)

    static member (*) (x : FactorExpr, y : Factor) : CategoricalPredictor = !!x * !!y

    static member (*) (x : FactorExpr, y : FactorExpr) : CategoricalPredictor = !!x * !!y

    static member (*) (x : FactorExpr, y : CategoricalPredictor) : CategoricalPredictor = !!x * y

    static member (*) (x : FactorExpr, y : Covariate) : Predictor = !!x * !!y

    static member (*) (x : FactorExpr, y : CovariateExpr) : Predictor = !!x * !!y

    static member (*) (x : FactorExpr, y : StatVariable) : Predictor = !!x * !!y

    static member (*) (x : FactorExpr, y : Predictor) : Predictor = !!x * y

    static member (.*) (x : FactorExpr, y : FactorExpr) = Cross(x, y)

    static member (.*) (x : FactorExpr, y : Factor) = Cross(x, y.AsExpr)

    static member (.*) (x : Factor, y : FactorExpr) = Cross(x.AsExpr, y)


    static member (+) (x : FactorExpr, y : Factor) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : FactorExpr, y : FactorExpr) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : FactorExpr, y : CategoricalPredictor) : Predictor list = !!x + !!y

    static member (+) (x : FactorExpr, y : Covariate) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : FactorExpr, y : CovariateExpr) : Predictor list = !!x + !!y

    static member (+) (x : FactorExpr, y : StatVariable) : Predictor list = !!x + (!!y:Predictor)

    static member (+) (x : FactorExpr, y : Predictor) : Predictor list = !!x + y

    static member (+) (x : Predictor list, y : FactorExpr) : Predictor list = x + (!!y:Predictor)

    static member (+) (x : FactorExpr, y : Predictor list) : Predictor list = (!!x:Predictor) + y

    member this.AsString = 
        this.AsFactor.ToString()


and Covariate(name : string, covariateStorage : ICovariateStorage) =

    static let covariateExprCache = new Dictionary<obj, Covariate>()

    static member internal CovariateExprCache = covariateExprCache

    member this.Name = name
    member this.Length = covariateStorage.Length

    member this.AsExpr = CovariateExpr.Var(this)

    member this.GetSlices(fromObs : int64, toObs : int64, sliceLen) = covariateStorage.GetSlices(fromObs, toObs, sliceLen)

    member this.AsSeq =
        let slices = this.GetSlices(0L, covariateStorage.Length - 1L, 10000)
        slices |> Seq.map (fun slice -> 
                               seq{0L..slice.LongLength - 1L} 
                               |> Seq.map (fun index -> slice.[index]))
                               |> Seq.concat

    member this.GetStats() = this.GetStats(10000) 

    member this.GetStats(sliceLen) =
        let processorCount = Environment.ProcessorCount
        let processorChunk = covariateStorage.Length / int64(processorCount)
        let nanCount, sum, sumSq, currMin, currMax =
            seq{0..processorCount - 1} 
                |> Seq.map (fun i -> (int64(i) * processorChunk), if i = processorCount - 1 then covariateStorage.Length - 1L else (int64(i + 1) * processorChunk - 1L))
                |> Seq.toArray 
                |> Array.Parallel.map (fun (fromObs, toObs) -> 
                                            this.GetSlices(fromObs, toObs, sliceLen)
                                                |> Seq.fold (fun (nanCount, sum, sumSq, currMin, currMax) slice -> 
                                                                use isNaN = slice .<> slice
                                                                use isNotNaN = BoolVector.Not isNaN
                                                                use notNan = slice.[isNotNaN]
                                                                let isAllNaN = notNan = Vector.Empty
                                                                let sliceSum = if isAllNaN then 0.0 else Vector.Sum(notNan)
                                                                let sliceSumSq = if isAllNaN then 0.0 else notNan * notNan
                                                                let sliceNaNCount = int64(slice.Length) - notNan.LongLength
                                                                let sliceMin = if isAllNaN then Double.NaN else Vector.Min(notNan)
                                                                let sliceMax = if isAllNaN then Double.NaN else Vector.Max(notNan)
                                                                (nanCount + sliceNaNCount,
                                                                 (if Double.IsNaN(sliceSum) then sum else sliceSum + sum),
                                                                 (if Double.IsNaN(sliceSumSq) then sumSq else sliceSumSq + sumSq),
                                                                 (if Double.IsNaN(sliceMin) then currMin else min currMin sliceMin),
                                                                 (if Double.IsNaN(sliceMax) then currMax else max currMax sliceMax)
                                                                ) 
                                                            ) (0L, 0.0, 0.0, Double.MaxValue, Double.MinValue)
                                        )
                |> Array.reduce (fun (nanCount1, sum1, sumSq1, currMin1, currMax1) (nanCount2, sum2, sumSq2, currMin2, currMax2) ->
                                     (nanCount1 + nanCount2, sum1 + sum2, sumSq1 + sumSq2, min currMin1 currMin2, max currMax1 currMax2)
                                )
        let N = covariateStorage.Length - nanCount
        {
        ObsCount = covariateStorage.Length
        NaNCount = nanCount
        ``NaN%`` = float(nanCount) / float(covariateStorage.Length)
        Mean = if N = 0L then Double.NaN else sum / float(N)
        Std = if N <= 1L then Double.NaN else (sumSq - sum * sum / float(N)) / float(N - 1L) |> Math.Sqrt
        Min = if N = 0L then Double.NaN else currMin
        Max = if N = 0L then Double.NaN else currMax
        }

    interface IFormattable with
        member this.ToString(format, provider) = 
            let n, _ = DisplayControl.MaxDisplaySize
            let slice = this.AsSeq |> Seq.take (min n covariateStorage.Length |> int) |> Seq.map float32 |> Seq.toArray
            let more = if int64(slice.Length) < covariateStorage.Length then "..." else ""
            let data = slice |> Array.map (fun x -> x.ToString()) |> String.concat " "
            sprintf "Covariate '%s' with %d obs: %s %s" name covariateStorage.Length data more

    override this.ToString() =
        (this:>IFormattable).ToString("", null)

    static member op_Explicit(x : Covariate) : Predictor = Predictor.NumericalPredictor(!!x)

    static member op_Explicit(x : Covariate) : CovariateExpr = CovariateExpr.Var x

    static member op_Explicit(x : string * Vector) =
        let name, data = x
        let covariateStorage = 
             {
              new ICovariateStorage with
                  member __.Length = data.LongLength
                  member __.GetSlices(fromObs : int64, toObs : int64, sliceLength : int) =
                    seq
                      {
                        let length = toObs - fromObs + 1L
                        let sliceLength = int64 sliceLength
                        let m = length / sliceLength |> int
                        let k = length % sliceLength 
                        for i in 0..m-1 do
                            yield data.View(fromObs + int64(i) * sliceLength, fromObs + int64(i + 1) * sliceLength - 1L)
                        if k > 0L then
                            yield data.View(fromObs + int64(m) * sliceLength, fromObs + int64(m) * sliceLength + k - 1L)
                      }
             }
        new Covariate(name, covariateStorage)

    static member op_Explicit(x : Covariate) : Vector =
        let v = new Vector(x.Length, 0.0)
        let sliceLen = Int32.MaxValue
        x.GetSlices(0L, x.Length - 1L, Int32.MaxValue)
            |> Seq.iteri (fun i slice -> let fromObs = int64(i) * int64(sliceLen) in v. SetSlice(Some(fromObs), Some(fromObs + slice.LongLength - 1L), slice))
        v


    static member (*) (x : Covariate, y : Factor) : Predictor = !!x * !!y

    static member (*) (x : Covariate, y : FactorExpr) : Predictor = !!x * !!y

    static member (*) (x : Covariate, y : CategoricalPredictor) : Predictor = !!x * !!y

    static member (*) (x : Covariate, y : Covariate) : CovariateExpr = !!x * !!y

    static member (*) (x : Covariate, y : CovariateExpr) : CovariateExpr = !!x * y

    static member (*) (x : Covariate, y : StatVariable) : Predictor = !!x * !!y

    static member (*) (x : Covariate, y : Predictor) : Predictor = !!x * y


    static member (+) (x : Covariate, y : Factor) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : Covariate, y : FactorExpr) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : Covariate, y : CategoricalPredictor) : Predictor list = !!x + !!y

    static member (+) (x : Covariate, y : Covariate) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : Covariate, y : CovariateExpr) : Predictor list = !!x + !!y

    static member (+) (x : Covariate, y : StatVariable) : Predictor list = !!x + (!!y:Predictor)

    static member (+) (x : Covariate, y : Predictor) : Predictor list = !!x + y

    static member (+) (x : Predictor list, y : Covariate) : Predictor list = x + (!!y:Predictor)

    static member (+) (x : Covariate, y : Predictor list) : Predictor list = (!!x:Predictor) + y


    static member (.<) (x : Covariate, y : Covariate) = x.AsExpr .< y.AsExpr

    static member (.<) (x : CovariateExpr, y : Covariate) = x .< y.AsExpr

    static member (.<) (x : Covariate, y : CovariateExpr) = x.AsExpr .< y

    static member (.<) (x : Covariate, y : float) = x.AsExpr .< y

    static member (.<) (x : float, y : Covariate) = x .< y.AsExpr

    static member (.<=) (x : Covariate, y : Covariate) = x.AsExpr .<= y.AsExpr

    static member (.<=) (x : CovariateExpr, y : Covariate) = x .<= y.AsExpr

    static member (.<=) (x : Covariate, y : CovariateExpr) = x.AsExpr .<= y

    static member (.<=) (x : Covariate, y : float) = x.AsExpr .<= y

    static member (.<=) (x : float, y : Covariate) = x .<= y.AsExpr


    static member (.>) (x : Covariate, y : Covariate) = x.AsExpr .> y.AsExpr

    static member (.>) (x : CovariateExpr, y : Covariate) = x .> y.AsExpr

    static member (.>) (x : Covariate, y : CovariateExpr) = x.AsExpr .> y

    static member (.>) (x : Covariate, y : float) = x.AsExpr .> y

    static member (.>) (x : float, y : Covariate) = x .> y.AsExpr

    static member (.>=) (x : Covariate, y : Covariate) = x.AsExpr .>= y.AsExpr

    static member (.>=) (x : CovariateExpr, y : Covariate) = x .>= y.AsExpr

    static member (.>=) (x : Covariate, y : CovariateExpr) = x.AsExpr .>= y

    static member (.>=) (x : Covariate, y : float) = x.AsExpr .>= y

    static member (.>=) (x : float, y : Covariate) = x .>= y.AsExpr


    static member (.<>) (x : Covariate, y : Covariate) = x.AsExpr .<> y.AsExpr

    static member (.<>) (x : CovariateExpr, y : Covariate) = x .<> y.AsExpr

    static member (.<>) (x : Covariate, y : CovariateExpr) = x.AsExpr .<> y

    static member (.<>) (x : Covariate, y : float) = x.AsExpr .<> y

    static member (.<>) (x : float, y : Covariate) = x .<> y.AsExpr

    static member (.=) (x : Covariate, y : Covariate) = x.AsExpr .= y.AsExpr

    static member (.=) (x : CovariateExpr, y : Covariate) = x .= y.AsExpr

    static member (.=) (x : Covariate, y : CovariateExpr) = x.AsExpr .= y

    static member (.=) (x : Covariate, y : float) = x.AsExpr .= y

    static member (.=) (x : float, y : Covariate) = x .= y.AsExpr


    static member (/) (x : Covariate, y : Covariate) = x.AsExpr / y.AsExpr

    static member (/) (x : CovariateExpr, y : Covariate) = x / y.AsExpr

    static member (/) (x : Covariate, y : CovariateExpr) = x.AsExpr / y

    static member (.+) (x : Covariate, y : Covariate) = x.AsExpr .+ y.AsExpr

    static member (.+) (x : CovariateExpr, y : Covariate) = x .+ y.AsExpr

    static member (.+) (x : Covariate, y : CovariateExpr) = x.AsExpr .+ y

    static member (-) (x : Covariate, y : Covariate) = x.AsExpr - y.AsExpr

    static member (-) (x : CovariateExpr, y : Covariate) = x - y.AsExpr

    static member (-) (x : Covariate, y : CovariateExpr) = x.AsExpr - y


    static member Min (x : Covariate, y : Covariate) = CovariateExpr.Min(x.AsExpr, y.AsExpr)

    static member Min (x : Covariate, y : CovariateExpr) = CovariateExpr.Min(x.AsExpr, y)

    static member Min (x : CovariateExpr, y : Covariate) = CovariateExpr.Min(x, y.AsExpr)

    static member Max (x : Covariate, y : Covariate) = CovariateExpr.Max(x.AsExpr, y.AsExpr)

    static member Max (x : Covariate, y : CovariateExpr) = CovariateExpr.Max(x.AsExpr, y)

    static member Max (x : CovariateExpr, y : Covariate) = CovariateExpr.Max(x, y.AsExpr)

    static member (*) (covariate : Covariate, a : float) =
        covariate.AsExpr * a

    static member (*) (a : float, covariate : Covariate) =
        a * covariate.AsExpr

    static member (/) (covariate : Covariate, a : float) =
        covariate.AsExpr / a

    static member (/) (a : float, covariate : Covariate) =
        a / covariate.AsExpr

    static member (+) (covariate : Covariate, a : float) =
        covariate.AsExpr + a

    static member (+) (a : float, covariate : Covariate) =
        a + covariate.AsExpr

    static member (-) (covariate : Covariate, a : float) =
        covariate.AsExpr - a

    static member (-) (a : float, covariate : Covariate) =
        a - covariate.AsExpr

    static member Min (covariate : Covariate, a : float) =
        CovariateExpr.Min(covariate.AsExpr, a)

    static member Min (a : float, covariate : Covariate) =
        CovariateExpr.Min(a, covariate.AsExpr)

    static member Max (covariate : Covariate, a : float) =
        CovariateExpr.Max(covariate.AsExpr, a)

    static member Max (a : float, covariate : Covariate) =
        CovariateExpr.Max(a, covariate.AsExpr)

    static member (^) (covariate : Covariate, n : int) =
        CovariateExpr.op_Concatenate(covariate.AsExpr, n)

    static member (^) (covariate : Covariate, a : float) =
        CovariateExpr.op_Concatenate(covariate.AsExpr, a)

    static member (~-) (covariate : Covariate) =
        -covariate.AsExpr

    static member Abs(covariate : Covariate) =
        CovariateExpr.Abs(covariate.AsExpr)

    static member Log(covariate : Covariate) =
         CovariateExpr.Log(covariate.AsExpr)

    static member Log10(covariate : Covariate) =
         CovariateExpr.Log10(covariate.AsExpr)

    static member Exp(covariate : Covariate) =
         CovariateExpr.Exp(covariate.AsExpr)

    static member Sqrt(covariate : Covariate) =
         CovariateExpr.Sqrt(covariate.AsExpr)

    static member Round(covariate : Covariate) =
         CovariateExpr.Round(covariate.AsExpr)

    static member Ceiling(covariate : Covariate) =
         CovariateExpr.Ceiling(covariate.AsExpr)

    static member Floor(covariate : Covariate) =
         CovariateExpr.Floor(covariate.AsExpr)

    static member Truncate(covariate : Covariate) =
         CovariateExpr.Truncate(covariate.AsExpr)

    interface IDisposable with
        member this.Dispose() = this.DoDispose(true)

    member internal this.DoDispose(isDisposing) = 
        if isDisposing then GC.SuppressFinalize(this)
        match covariateStorage with
            | :? IDisposable as d -> d.Dispose()
            | _ -> ()

    override this.Finalize() = try this.DoDispose(false) with _ -> ()

and [<StructuredFormatDisplay("{AsString}")>] CovariateExpr =
    | Var of Covariate
    | UnaryFunction of CovariateExpr * (VectorExpr -> VectorExpr)  * string
    | BinaryFunction of CovariateExpr * CovariateExpr * (VectorExpr * VectorExpr -> VectorExpr) * string
    | ParseFactor of FactorExpr * (string -> float)
    | BinomialFactor of FactorExpr * (string -> bool)

    member this.Vars =
        match this with
            | Var(v) -> [StatVariable.Covariate v]
            | UnaryFunction(expr, _, _) -> expr.Vars
            | BinaryFunction(expr1, expr2, _, _) -> expr1.Vars @ expr2.Vars
            | ParseFactor(f, _) -> f.Vars
            | BinomialFactor(f, _) -> f.Vars
                
    member this.MinLength =
        match this with
            | Var(c) ->  c.Length
            | UnaryFunction(expr, _, _) -> expr.MinLength 
            | BinaryFunction(expr1, expr2, _, _) ->  
                min expr1.MinLength expr2.MinLength
            | ParseFactor(f, _) -> f.MinLength
            | BinomialFactor(f, _) -> f.MinLength

    member this.MaxLength =
        match this with
            | Var(c) ->  c.Length
            | UnaryFunction(expr, _, _) -> expr.MaxLength 
            | BinaryFunction(expr1, expr2, _, _) ->  
                max expr1.MaxLength expr2.MaxLength
            | ParseFactor(f, _) -> f.MaxLength
            | BinomialFactor(f, _) -> f.MaxLength

    static member Substitute (mapF :  StatVariable -> StatVariable) (covExpr : CovariateExpr) =
        match covExpr with
            | Var(c) -> Var(mapF !!c |> (!!))
            | UnaryFunction(cov, f, label) -> UnaryFunction(CovariateExpr.Substitute mapF cov, f, label)
            | BinaryFunction(cov1, cov2, f, label) -> BinaryFunction(CovariateExpr.Substitute mapF cov1, CovariateExpr.Substitute mapF cov2, f, label)
            | ParseFactor(f, parse) -> ParseFactor(FactorExpr.Substitute mapF f, parse)
            | BinomialFactor(f, parse) -> BinomialFactor(FactorExpr.Substitute mapF f, parse)

    member this.GetSlicesExpr(fromObs : int64, toObs : int64, sliceLen : int) =
        match this with
            | Var(c) ->
                c.GetSlices(fromObs, toObs, sliceLen) |> Seq.map (fun x -> x.AsExpr)
            | UnaryFunction(expr, f, _) -> 
                expr.GetSlicesExpr(fromObs, toObs, sliceLen) |> Seq.map f
            | BinaryFunction(expr1, expr2, f, _) -> 
                expr2.GetSlicesExpr(fromObs, toObs, sliceLen) |> Seq.zip (expr1.GetSlicesExpr(fromObs, toObs, sliceLen))
                                                              |> Seq.map f
            | ParseFactor(factor, parse) ->
                let factor = factor.AsFactor
                let map = Array.init factor.Cardinality (factor.get_Level >> parse)
                factor.GetSlices(fromObs, toObs, sliceLen) |> Seq.map (fun slice -> let v = new Vector(slice.LongLength, 0.0)
                                                                                    MklFunctions.Level_Index_To_Numeric(v.Length, slice.NativeArray, v.NativeArray, map)
                                                                                    v.AsExpr)

            | BinomialFactor(factor, parse) ->
                let factor = factor.AsFactor
                let map = Array.init factor.Cardinality (factor.get_Level >> parse >> (fun b -> if b then 1.0 else 0.0))
                factor.GetSlices(fromObs, toObs, sliceLen) |> Seq.map (fun slice -> let v = new Vector(slice.LongLength, 0.0)
                                                                                    MklFunctions.Level_Index_To_Numeric(v.Length, slice.NativeArray, v.NativeArray, map)
                                                                                    v.AsExpr)
                
    member this.Name =
        match this with
            | Var(c) -> c.Name
            | UnaryFunction(expr, _, format) -> String.Format(format, expr.Name)
            | BinaryFunction(expr1, expr2, _, format) -> String.Format(format, expr1.Name, expr2.Name)
            | ParseFactor(f, _) -> sprintf "%s.AsCovariate" f.Name
            | BinomialFactor(f, _) -> sprintf "%s.AsCovariate" f.Name

    member this.AsCovariate =
        if Covariate.CovariateExprCache.ContainsKey(this:>obj) then Covariate.CovariateExprCache.[this:>obj]
        else
        let covariate = 
            let minLen = this.MinLength
            let maxLen = this.MaxLength
            if minLen <> maxLen then raise (new ArgumentException("Covariate length mismatch"))
            let covariateStorage = 
                 {
                  new ICovariateStorage with
                      member __.Length = this.MinLength
                      member __.GetSlices(fromObs : int64, toObs : int64, sliceLength : int) =
                        seq
                          {
                            let buffer = new Vector((min (int64(sliceLength)) (toObs - fromObs + 1L)), 0.0)
                            for sliceExpr in this.GetSlicesExpr(fromObs, toObs, sliceLength) do
                                yield VectorExpr.EvalIn(sliceExpr, sliceExpr.Length |> Option.map (fun len -> if len = buffer.LongLength then buffer else buffer.View(0L, len - 1L)))
                          }
                 }
            let name = this.Name // remove ()
            new Covariate(name, covariateStorage)
        Covariate.CovariateExprCache.Add(this:>obj, covariate)
        covariate

    static member op_Explicit(x : CovariateExpr) : Predictor = Predictor.NumericalPredictor(x)

    static member (*) (x : CovariateExpr, y : Factor) : Predictor = !!x * !!y

    static member (*) (x : CovariateExpr, y : FactorExpr) : Predictor = !!x * !!y

    static member (*) (x : CovariateExpr, y : CategoricalPredictor) : Predictor = !!x * !!y

    static member (*) (x : CovariateExpr, y : Covariate) : CovariateExpr = x * !!y

    static member (*) (x : CovariateExpr, y : StatVariable) : Predictor = !!x * !!y

    static member (*) (x : CovariateExpr, y : Predictor) : Predictor = !!x * y


    static member (+) (x : CovariateExpr, y : Factor) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : CovariateExpr, y : FactorExpr) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : CovariateExpr, y : CategoricalPredictor) : Predictor list = !!x + !!y

    static member (+) (x : CovariateExpr, y : Covariate) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : CovariateExpr, y : CovariateExpr) : Predictor list = !!x + !!y

    static member (+) (x : CovariateExpr, y : StatVariable) : Predictor list = !!x + (!!y:Predictor)

    static member (+) (x : CovariateExpr, y : Predictor) : Predictor list = !!x + y

    static member (+) (x : Predictor list, y : CovariateExpr) : Predictor list = x + !!y

    static member (+) (x : CovariateExpr, y : Predictor list) : Predictor list = !!x + y


    static member (.<) (x : CovariateExpr, y : CovariateExpr) =
        BinaryFunction(x, y, (fun (x,y) -> VectorExpr.IfFunction(x .< y, VectorExpr.Scalar(1.0), VectorExpr.Scalar(0.0))), "({0:G}<{1:G})")

    static member (.<) (x : CovariateExpr, a : float) =
        UnaryFunction(x, (fun x -> VectorExpr.IfFunction(x .< a, VectorExpr.Scalar(1.0), VectorExpr.Scalar(0.0))), sprintf "({0:G}<%G)" a)

    static member (.<) (a : float, x : CovariateExpr) =
        UnaryFunction(x, (fun x -> VectorExpr.IfFunction(a .< x, VectorExpr.Scalar(1.0), VectorExpr.Scalar(0.0))), sprintf "(%G<{0:G})" a)

    static member (.>) (x : CovariateExpr, y : CovariateExpr) =
        BinaryFunction(x, y, (fun (x,y) -> VectorExpr.IfFunction(x .> y, VectorExpr.Scalar(1.0), VectorExpr.Scalar(0.0))), "({0:G}>{1:G})")

    static member (.>) (x : CovariateExpr, a : float) =
        UnaryFunction(x, (fun x -> VectorExpr.IfFunction(x .> a, VectorExpr.Scalar(1.0), VectorExpr.Scalar(0.0))), sprintf "({0:G}>%G)" a)

    static member (.>) (a : float, x : CovariateExpr) =
        UnaryFunction(x, (fun x -> VectorExpr.IfFunction(a .> x, VectorExpr.Scalar(1.0), VectorExpr.Scalar(0.0))), sprintf "(%G>{0:G})" a)

    static member (.<=) (x : CovariateExpr, y : CovariateExpr) =
        BinaryFunction(x, y, (fun (x,y) -> VectorExpr.IfFunction(x .<= y, VectorExpr.Scalar(1.0), VectorExpr.Scalar(0.0))), "({0:G}<={1:G})")

    static member (.<=) (x : CovariateExpr, a : float) =
        UnaryFunction(x, (fun x -> VectorExpr.IfFunction(x .<= a, VectorExpr.Scalar(1.0), VectorExpr.Scalar(0.0))), sprintf "({0:G}<=%G)" a)

    static member (.<=) (a : float, x : CovariateExpr) =
        UnaryFunction(x, (fun x -> VectorExpr.IfFunction(a .<= x, VectorExpr.Scalar(1.0), VectorExpr.Scalar(0.0))), sprintf "(%G<={0:G})" a)

    static member (.>=) (x : CovariateExpr, y : CovariateExpr) =
        BinaryFunction(x, y, (fun (x,y) -> VectorExpr.IfFunction(x .>= y, VectorExpr.Scalar(1.0), VectorExpr.Scalar(0.0))), "({0:G}>={1:G})")

    static member (.>=) (x : CovariateExpr, a : float) =
        UnaryFunction(x, (fun x -> VectorExpr.IfFunction(x .>= a, VectorExpr.Scalar(1.0), VectorExpr.Scalar(0.0))), sprintf "({0:G}>=%G)" a)

    static member (.>=) (a : float, x : CovariateExpr) =
        UnaryFunction(x, (fun x -> VectorExpr.IfFunction(a .>= x, VectorExpr.Scalar(1.0), VectorExpr.Scalar(0.0))), sprintf "(%G>={0:G})" a)

    static member (.=) (x : CovariateExpr, y : CovariateExpr) =
        BinaryFunction(x, y, (fun (x,y) -> VectorExpr.IfFunction(x .= y, VectorExpr.Scalar(1.0), VectorExpr.Scalar(0.0))), "({0:G}={1:G})")

    static member (.=) (x : CovariateExpr, a : float) =
        UnaryFunction(x, (fun x -> VectorExpr.IfFunction(x .= a, VectorExpr.Scalar(1.0), VectorExpr.Scalar(0.0))), sprintf "({0:G}=%G)" a)

    static member (.=) (a : float, x : CovariateExpr) =
        UnaryFunction(x, (fun x -> VectorExpr.IfFunction(a .= x, VectorExpr.Scalar(1.0), VectorExpr.Scalar(0.0))), sprintf "(%G={0:G})" a)

    static member (.<>) (x : CovariateExpr, y : CovariateExpr) =
        BinaryFunction(x, y, (fun (x,y) -> VectorExpr.IfFunction(x .<> y, VectorExpr.Scalar(1.0), VectorExpr.Scalar(0.0))), "({0:G}<>{1:G})")

    static member (.<>) (x : CovariateExpr, a : float) =
        UnaryFunction(x, (fun x -> VectorExpr.IfFunction(x .<> a, VectorExpr.Scalar(1.0), VectorExpr.Scalar(0.0))), sprintf "({0:G}<>%G)" a)

    static member (.<>) (a : float, x : CovariateExpr) =
        UnaryFunction(x, (fun x -> VectorExpr.IfFunction(a .<> x, VectorExpr.Scalar(1.0), VectorExpr.Scalar(0.0))), sprintf "(%G<>{0:G})" a)


    static member (*) (x : CovariateExpr, y : CovariateExpr) =
        BinaryFunction(x, y, VectorExpr.op_DotMultiply, "({0:G}*{1:G})")

    static member (/) (covariateExpr1 : CovariateExpr, covariateExpr2 : CovariateExpr) =
        BinaryFunction(covariateExpr1, covariateExpr2, VectorExpr.op_DotDivide, "({0:G}/{1:G})")

    static member (.+) (covariateExpr1 : CovariateExpr, covariateExpr2 : CovariateExpr) =
        BinaryFunction(covariateExpr1, covariateExpr2, VectorExpr.op_Addition, "({0:G}+{1:G})")

    static member (-) (covariateExpr1 : CovariateExpr, covariateExpr2 : CovariateExpr) =
        BinaryFunction(covariateExpr1, covariateExpr2, VectorExpr.op_Subtraction, "({0:G}-{1:G})")

    static member Min (covariateExpr1 : CovariateExpr, covariateExpr2 : CovariateExpr) =
        BinaryFunction(covariateExpr1, covariateExpr2, VectorExpr.Min, "min({0:G},{1:G})")

    static member Max (covariateExpr1 : CovariateExpr, covariateExpr2 : CovariateExpr) =
        BinaryFunction(covariateExpr1, covariateExpr2, VectorExpr.Max, "max({0:G},{1:G})")


    static member (*) (covariateExpr : CovariateExpr, a : float) =
        UnaryFunction(covariateExpr, (fun expr -> expr .* a), sprintf "(%G*{0:G})" a)

    static member (*) (a : float, covariateExpr : CovariateExpr) =
        UnaryFunction(covariateExpr, (fun expr -> a .* expr), sprintf "(%G*{0:G})" a)

    static member (/) (covariateExpr : CovariateExpr, a : float) =
        UnaryFunction(covariateExpr, (fun expr -> expr ./ a), sprintf "({0:G}/%G)" a)

    static member (/) (a : float, covariateExpr : CovariateExpr) =
        UnaryFunction(covariateExpr, (fun expr -> a ./ expr), sprintf "(%G/{0:G})" a)

    static member (+) (covariateExpr : CovariateExpr, a : float) =
        UnaryFunction(covariateExpr, (fun expr -> expr + a), sprintf "(%G+{0:G})" a)

    static member (+) (a : float, covariateExpr : CovariateExpr) =
        UnaryFunction(covariateExpr, (fun expr -> a + expr), sprintf "(%G+{0:G})" a)

    static member (-) (covariateExpr : CovariateExpr, a : float) =
        UnaryFunction(covariateExpr, (fun expr -> expr - a), sprintf "({0:G}-%G)" a)

    static member (-) (a : float, covariateExpr : CovariateExpr) =
        UnaryFunction(covariateExpr, (fun expr -> a - expr), sprintf "(%G-{0:G})" a)

    static member Min (covariateExpr : CovariateExpr, a : float) =
        UnaryFunction(covariateExpr, (fun expr -> VectorExpr.Min(expr, a)), sprintf "min({0:G},%G)" a)

    static member Min (a : float, covariateExpr : CovariateExpr) =
        UnaryFunction(covariateExpr, (fun expr -> VectorExpr.Min(a, expr)), sprintf "min(%G,{0:G})" a)

    static member Max (covariateExpr : CovariateExpr, a : float) =
        UnaryFunction(covariateExpr, (fun expr -> VectorExpr.Max(expr, a)), sprintf "max({0:G},%G)" a)

    static member Max (a : float, covariateExpr : CovariateExpr) =
        UnaryFunction(covariateExpr, (fun expr -> VectorExpr.Max(a, expr)), sprintf "max(%G,{0:G})" a)

    static member (^) (covariateExpr : CovariateExpr, n : int) =
        UnaryFunction(covariateExpr, (fun expr -> expr .^ n), sprintf "{0:G}^%d" n)

    static member (^) (covariateExpr : CovariateExpr, a : float) =
        UnaryFunction(covariateExpr, (fun expr -> expr .^ a), sprintf "{0:G}^%G" a)

    static member (~-) (covariateExpr : CovariateExpr) =
        UnaryFunction(covariateExpr, VectorExpr.op_UnaryNegation, "(-{0:G})")

    static member Abs(covariateExpr : CovariateExpr) =
        UnaryFunction(covariateExpr, VectorExpr.Abs, "abs({0:G})")

    static member Log(covariateExpr : CovariateExpr) =
        UnaryFunction(covariateExpr, VectorExpr.Log, "log({0:G})")

    static member Log10(covariateExpr : CovariateExpr) =
        UnaryFunction(covariateExpr, VectorExpr.Log10, "log10({0:G})")

    static member Exp(covariateExpr : CovariateExpr) =
        UnaryFunction(covariateExpr, VectorExpr.Exp, "exp({0:G})")

    static member Sqrt(covariateExpr : CovariateExpr) =
        UnaryFunction(covariateExpr, VectorExpr.Sqrt, "sqrt({0:G})")

    static member Round(covariateExpr : CovariateExpr) =
        UnaryFunction(covariateExpr, VectorExpr.Round, "round({0:G})")

    static member Ceiling(covariateExpr : CovariateExpr) =
        UnaryFunction(covariateExpr, VectorExpr.Ceiling, "ceil({0:G})")

    static member Floor(covariateExpr : CovariateExpr) =
        UnaryFunction(covariateExpr, VectorExpr.Floor, "floor({0:G})")

    static member Truncate(covariateExpr : CovariateExpr) =
        UnaryFunction(covariateExpr, VectorExpr.Truncate, "trunc({0:G})")

    member this.AsString = 
        this.AsCovariate.ToString()
        
and StatVariable =
    | Factor of Factor
    | Covariate of Covariate

    member this.Name = 
       match this with
           | Factor(f) -> f.Name
           | Covariate(c) -> c.Name

    member this.AsFactor =
        match this with
            | Factor(f) -> f
            | _ -> raise (new InvalidCastException())

    member this.AsCovariate =
        match this with
            | Covariate(c) -> c
            | _ -> raise (new InvalidCastException())

    static member op_Explicit(x : StatVariable) : Factor = x.AsFactor

    static member op_Explicit(x : StatVariable) : Covariate = x.AsCovariate

    static member op_Explicit(x : Factor) = StatVariable.Factor(x)

    static member op_Explicit(x : Covariate) = StatVariable.Covariate(x)

    static member op_Explicit(x : StatVariable) : Predictor =
        match x with
            | Factor(f) -> !!f
            | Covariate(c) -> !!c

    static member (*) (x : StatVariable, y : Factor) : Predictor = !!x * !!y

    static member (*) (x : StatVariable, y : FactorExpr) : Predictor = !!x * !!y

    static member (*) (x : StatVariable, y : CategoricalPredictor) : Predictor = !!x * !!y

    static member (*) (x : StatVariable, y : Covariate) : Predictor = !!x * !!y

    static member (*) (x : StatVariable, y : CovariateExpr) : Predictor = !!x * !!y

    static member (*) (x : StatVariable, y : StatVariable) : Predictor = !!x * !!y

    static member (*) (x : StatVariable, y : Predictor) : Predictor = !!x * y


    static member (+) (x : StatVariable, y : Factor) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : StatVariable, y : FactorExpr) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : StatVariable, y : CategoricalPredictor) : Predictor list = !!x + !!y

    static member (+) (x : StatVariable, y : Covariate) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : StatVariable, y : CovariateExpr) : Predictor list = !!x + !!y

    static member (+) (x : StatVariable, y : StatVariable) : Predictor list = !!x + (!!y:Predictor)

    static member (+) (x : StatVariable, y : Predictor) : Predictor list = !!x + y

    static member (+) (x : Predictor list, y : StatVariable) : Predictor list = x + (!!y:Predictor)

    static member (+) (x : StatVariable, y : Predictor list) : Predictor list = (!!x:Predictor) + y


and CategoricalPredictor =
    | Factor of FactorExpr
    | CategoricalInteraction of CategoricalPredictor * FactorExpr

    member this.AsList =
        match this with
            | Factor(f) -> [f.AsFactor]
            | CategoricalInteraction(catPred, factor) -> catPred.AsList @ [factor.AsFactor]

    member this.MinLength =
        match this with
            | Factor(f) -> f.MinLength
            | CategoricalInteraction(c, f) ->
                min c.MinLength f.MinLength

    member this.MaxLength =
        match this with
            | Factor(f) -> f.MaxLength
            | CategoricalInteraction(c, f) ->
                max c.MaxLength f.MaxLength

    member this.Name =
        match this with
            | Factor(f) -> f.Name
            | CategoricalInteraction(c, f) -> sprintf "%s*%s" c.Name f.Name

    static member Substitute (mapF : StatVariable -> StatVariable) (catPred : CategoricalPredictor) =
        match catPred with
            | Factor(f) -> Factor(FactorExpr.Substitute mapF f)
            | CategoricalInteraction(catPred, f) -> CategoricalInteraction(CategoricalPredictor.Substitute mapF catPred, FactorExpr.Substitute mapF f)

    static member (*) (x : CategoricalPredictor, y : CategoricalPredictor) =
        match y with
            | Factor(f) -> CategoricalInteraction(x, f)
            | CategoricalInteraction(p, f) -> x * p * Factor(f)

    static member op_Explicit(x : CategoricalPredictor) : Predictor = Predictor.CategoricalPredictor(x)

    static member (*) (x : CategoricalPredictor, y : Factor) : CategoricalPredictor = x * !!y

    static member (*) (x : CategoricalPredictor, y : FactorExpr) : CategoricalPredictor = x * !!y

    static member (*) (x : CategoricalPredictor, y : Covariate) : Predictor = !!x * !!y

    static member (*) (x : CategoricalPredictor, y : CovariateExpr) : Predictor = !!x * !!y

    static member (*) (x : CategoricalPredictor, y : StatVariable) : Predictor = !!x * !!y

    static member (*) (x : CategoricalPredictor, y : Predictor) : Predictor = !!x * y


    static member (+) (x : CategoricalPredictor, y : Factor) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : CategoricalPredictor, y : FactorExpr) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : CategoricalPredictor, y : CategoricalPredictor) : Predictor list = !!x + !!y

    static member (+) (x : CategoricalPredictor, y : Covariate) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : CategoricalPredictor, y : CovariateExpr) : Predictor list = !!x + !!y

    static member (+) (x : CategoricalPredictor, y : StatVariable) : Predictor list = !!x + (!!y:Predictor)

    static member (+) (x : CategoricalPredictor, y : Predictor) : Predictor list = !!x + y

    static member (+) (x : Predictor list, y : CategoricalPredictor) : Predictor list = x + !!y

    static member (+) (x : CategoricalPredictor, y : Predictor list) : Predictor list = !!x + y

    static member op_Explicit(factors : FactorExpr list) = 
        let rec fromList (factors : FactorExpr list) =
            match factors with
                | [] -> raise (new InvalidOperationException())
                | h::[] -> Factor h
                | h::t -> CategoricalInteraction(fromList t, h)
        factors |> (List.rev >> fromList)

    static member op_Explicit(factors : Factor list) = 
        CategoricalPredictor.op_Explicit(factors |> List.map (fun f -> FactorExpr.Var(f)))

and Predictor =
    | CategoricalPredictor of CategoricalPredictor
    | NumericalPredictor of CovariateExpr
    | MixedInteraction of CategoricalPredictor * CovariateExpr

    member this.MinLength =
        match this with
            | CategoricalPredictor(c) -> c.MinLength
            | NumericalPredictor(n) -> n.MinLength
            | MixedInteraction(c, n) -> min c.MinLength n.MinLength

    member this.MaxLength =
        match this with
            | CategoricalPredictor(c) -> c.MaxLength
            | NumericalPredictor(n) -> n.MaxLength
            | MixedInteraction(c, n) -> max c.MaxLength n.MaxLength

    member this.AsList =
        match this with
            | CategoricalPredictor(catPred) -> catPred.AsList, None
            | NumericalPredictor(x) -> [], Some(x)
            | MixedInteraction(catPred, covExpr) -> catPred.AsList, Some(covExpr)

    member this.Name =
        match this with
            | CategoricalPredictor(catPred) -> catPred.Name
            | NumericalPredictor(x) -> x.Name
            | MixedInteraction(catPred, covExpr) -> sprintf "%s*%s" catPred.Name covExpr.Name

    static member Substitute (mapF : StatVariable -> StatVariable) (predictor : Predictor) =
        match predictor with
            | CategoricalPredictor(cp) -> CategoricalPredictor(CategoricalPredictor.Substitute mapF cp)
            | NumericalPredictor(x) -> NumericalPredictor(CovariateExpr.Substitute mapF x)
            | MixedInteraction(c, n) -> MixedInteraction(CategoricalPredictor.Substitute mapF c, CovariateExpr.Substitute mapF n)

    static member (*) (x : Predictor, y : Predictor) : Predictor =
        match x, y with
            | CategoricalPredictor(x), CategoricalPredictor(y) -> CategoricalPredictor(x * y)
            | CategoricalPredictor(x), NumericalPredictor(y) -> MixedInteraction(x, y)
            | CategoricalPredictor(x), MixedInteraction(p, c) -> MixedInteraction(x * p, c)
            | NumericalPredictor(x), CategoricalPredictor(y) -> MixedInteraction(y, x)
            | NumericalPredictor(x), NumericalPredictor(y) -> NumericalPredictor(x * y)
            | NumericalPredictor(x), MixedInteraction(p, c) -> MixedInteraction(p, x * c)
            | MixedInteraction(p, c), CategoricalPredictor(y) -> MixedInteraction(p * y, c)
            | MixedInteraction(p, c), NumericalPredictor(y) -> MixedInteraction(p, c * y)
            | MixedInteraction(p1, c1), MixedInteraction(p2, c2) -> MixedInteraction(p1 * p2, c1 * c2)


    static member (*) (x : Predictor, y : Factor) : Predictor = x * !!y

    static member (*) (x : Predictor, y : FactorExpr) : Predictor = x * !!y

    static member (*) (x : Predictor, y : Covariate) : Predictor = x * !!y

    static member (*) (x : Predictor, y : CovariateExpr) : Predictor = x * !!y

    static member (*) (x : Predictor, y : StatVariable) : Predictor = x * !!y

    static member (*) (x : Predictor, y : CategoricalPredictor) : Predictor = x * !!y


    static member (+) (x : Predictor, y : Factor) : Predictor list = x + (!!y:Predictor)

    static member (+) (x : Predictor, y : FactorExpr) : Predictor list = x + (!!y:Predictor)

    static member (+) (x : Predictor, y : CategoricalPredictor) : Predictor list = x + !!y

    static member (+) (x : Predictor, y : Covariate) : Predictor list = x + (!!y:Predictor)

    static member (+) (x : Predictor, y : CovariateExpr) : Predictor list = x + !!y

    static member (+) (x : Predictor, y : StatVariable) : Predictor list = x + (!!y:Predictor)

    static member (+) (x : Predictor, y : Predictor) = [x; y]


    static member (+) (x : Predictor list, y : Predictor) = x @ [y]


    static member (+) (x : Predictor, y : Predictor list) = x :: y








     

