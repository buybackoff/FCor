namespace FCor
#nowarn "9"

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic
open FCor.ExplicitConversion
open System.Text

type Factor(name : string, factorStorage : IFactorStorage) =

    static let intercept = 
        let factorStorage = 
             {
              new IFactorStorage with
                  member __.GetLevel(index) = "<Intercept>"
                  member __.Length = 1L
                  member __.LevelCount = 1
                  member __.GetSlices(fromObs : int64, toObs : int64, sliceLength : int) = 
                    seq
                      {
                        let length = toObs - fromObs + 1L
                        let sliceLength = int64 sliceLength
                        let m = length / sliceLength |> int
                        let k = length % sliceLength 
                        use buffer = new UInt16Vector(sliceLength, 0us)
                        for i in 0..m-1 do
                            yield buffer
                        if k > 0L then
                            yield buffer.View(0L, k-1L)
                      }
             }
        new Factor("<INTERCEPT>", factorStorage)

    member this.Name = name
    member this.Length = factorStorage.Length
    member this.LevelCount = factorStorage.LevelCount

    member this.GetSlices(fromObs : int64, toObs : int64, sliceLen : int) = factorStorage.GetSlices(fromObs, toObs, sliceLen)

    member this.GetLevel(levelIndex) = factorStorage.GetLevel(levelIndex)

    member this.AsSeq =
        let slices = this.GetSlices(0L, factorStorage.Length - 1L, 10000)
        slices |> Seq.map (fun slice -> 
                               seq{0L..slice.LongLength - 1L} 
                               |> Seq.map (fun index -> let levelIndex = slice.[index] in levelIndex, this.GetLevel(int levelIndex)))
                               |> Seq.concat


    interface IFormattable with
        member this.ToString(format, provider) = 
            let n, _ = DisplayControl.MaxDisplaySize
            let slice = this.AsSeq |> Seq.take (min n factorStorage.Length |> int) |> Seq.map snd |> Seq.toArray
            let more = if int64(slice.Length) < factorStorage.Length then "..." else ""
            let data = slice |> String.concat " "
            sprintf "Factor '%s' with %d obs and %d levels: %s %s" name factorStorage.Length factorStorage.LevelCount data more

    override this.ToString() =
        (this:>IFormattable).ToString("", null)

    member this.AsExpr = FactorExpr.Var(this)

    static member Intercept = intercept

    static member op_Explicit(factor : Factor) : FactorExpr = FactorExpr.Var(factor)

    static member op_Explicit(factor : Factor) : Predictor = CategoricalPredictor(!!factor)

    static member op_Explicit(factor : Factor) : CategoricalPredictor = CategoricalPredictor.Factor(!!factor)


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

    static member (+) (x : Factor, y : StatVariable) : Predictor list = !!x + !!y

    static member (+) (x : Factor, y : Predictor) : Predictor list = !!x + y

    static member (+) (x : Predictor list, y : Factor) : Predictor list = x + (!!y:Predictor)

    static member (+) (x : Factor, y : Predictor list) : Predictor list = (!!x:Predictor) + y


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

    member this.Vars =
        match this with
            | Var(f) -> [StatVariable.Factor f]
            | Rename(f, _) -> f.Vars
            | MergeLevels(f, _) -> f.Vars
            | Cross(f1, f2) -> f1.Vars @ f2.Vars
            | Cut(c, _) -> c.Vars


    member this.MinLength =
        match this with
            | Var(f) ->  f.Length
            | Rename(f, _) -> f.MinLength 
            | MergeLevels(f, _) -> f.MinLength
            | Cross(f1, f2) -> min f1.MinLength f2.MinLength
            | Cut(c, _) -> c.MinLength

    member this.MaxLength =
        match this with
            | Var(f) ->  f.Length
            | Rename(f, _) -> f.MaxLength 
            | MergeLevels(f, _) -> f.MaxLength
            | Cross(f1, f2) -> max f1.MaxLength f2.MaxLength
            | Cut(c, _) -> c.MaxLength

    member this.Name =
        match this with
            | Var(f) ->  f.Name
            | Rename(f, _) -> f.Name 
            | MergeLevels(f, _) -> f.Name
            | Cross(f1, f2) -> sprintf "%s*%s" f1.Name f2.Name
            | Cut(c, _) -> sprintf "%s.AsFactor" c.Name 

    member this.AsFactor =
        match this with
            | Var(factor) -> factor
            | Rename(factor, renameFun) ->
                let factor = factor.AsFactor
                let factorStorage = 
                     {
                      new IFactorStorage with
                          member __.GetLevel(index) = factor.GetLevel(index) |> renameFun
                          member __.Length = factor.Length
                          member __.LevelCount = factor.LevelCount
                          member __.GetSlices(fromObs : int64, toObs : int64, sliceLength : int) = 
                              factor.GetSlices(fromObs, toObs, sliceLength)
                     }
                new Factor(factor.Name, factorStorage)

            | MergeLevels(factor, mergedLevels) ->
                let factor = factor.AsFactor
                let mergedLevel = String.Join("|", mergedLevels)
                let mergedLevels = new Set<_>(mergedLevels)
                let isMerged = Array.init factor.LevelCount (fun i -> mergedLevels.Contains(factor.GetLevel i))
                let cumMergedCount = Array.sub (isMerged |> Array.scan (fun cum isMerged -> if isMerged then cum + 1 else cum) 0) 1 isMerged.Length
                                     |> Array.map (fun cum -> if cum = 0 then cum else cum - 1)
                let levelMap = Array.init factor.LevelCount (fun i -> (i - cumMergedCount.[i]) |> uint16)
                let factorStorage = 
                     {
                      new IFactorStorage with
                          member __.GetLevel(index) =
                              if isMerged.[index] && cumMergedCount.[index] = 0 then
                                  mergedLevel
                              else 
                                  factor.GetLevel(index + cumMergedCount.[index])
                          member __.Length = factor.Length
                          member __.LevelCount = factor.LevelCount - cumMergedCount.[factor.LevelCount - 1]
                          member __.GetSlices(fromObs : int64, toObs : int64, sliceLength : int) = 
                              factor.GetSlices(fromObs, toObs, sliceLength) |> Seq.map (fun v -> MklFunctions.Update_Level_Index(v.Length, v.NativeArray, levelMap)
                                                                                                 v)
                     }
                new Factor(factor.Name, factorStorage)
            | Cross(f1, f2) -> 
                let factor1 = f1.AsFactor
                let factor2 = f2.AsFactor
                let levelCount = factor1.LevelCount * factor2.LevelCount
                if levelCount > int UInt16.MaxValue then raise (new ArgumentException("Too many levels in cross factor"))
                let factorStorage = 
                     {
                      new IFactorStorage with
                          member __.GetLevel(index) =
                              let index1 = index % factor1.LevelCount
                              let index2 = index / factor1.LevelCount
                              sprintf "%s*%s" (factor1.GetLevel(index1)) (factor2.GetLevel(index2))
                          member __.Length = if factor1.Length <> factor2.Length then raise (new ArgumentException("Factor length mismatch")) else factor1.Length
                          member __.LevelCount = levelCount
                          member __.GetSlices(fromObs : int64, toObs : int64, sliceLength : int) = 
                              factor2.GetSlices(fromObs, toObs, sliceLength) |> Seq.zip (factor1.GetSlices(fromObs, toObs, sliceLength))
                                  |> Seq.map (fun (slice1, slice2) -> let v = new UInt16Vector(slice1.Length, 0us)
                                                                      MklFunctions.Get_Cross_Level_Index(slice1.Length, uint16 factor1.LevelCount, slice1.NativeArray, slice2.NativeArray, v.NativeArray)
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
                          member __.GetLevel(index) =
                              if index = breaks.Length - 1 then
                                  String.Empty
                              elif index = breaks.Length - 2 then
                                  sprintf "[%G,%G]" breaks.[index] breaks.[index + 1]
                              else
                                  sprintf "[%G,%G)" breaks.[index] breaks.[index + 1]
                          member __.Length = covariate.Length
                          member __.LevelCount = breaks.Length
                          member __.GetSlices(fromObs : int64, toObs : int64, sliceLength : int) =
                              covariate.GetSlices(fromObs, toObs, sliceLength) |> Seq.map (fun (slice : Vector) -> let v = new UInt16Vector(slice.Length, 0us)
                                                                                                                   MklFunctions.Get_Cut_Level_Index(slice.Length, breaks, slice.NativeArray, v.NativeArray)
                                                                                                                   v
                                                                                          )
                          
                     }
                new Factor(Cut(c, breaks).Name, factorStorage)


    static member op_Explicit(x : FactorExpr) : Predictor = Predictor.CategoricalPredictor(!!x)

    static member op_Explicit(x : FactorExpr) : CategoricalPredictor = CategoricalPredictor.Factor(x)

    static member (*) (x : FactorExpr, y : Factor) : CategoricalPredictor = !!x * !!y

    static member (*) (x : FactorExpr, y : FactorExpr) : CategoricalPredictor = !!x * !!y

    static member (*) (x : FactorExpr, y : CategoricalPredictor) : CategoricalPredictor = !!x * y

    static member (*) (x : FactorExpr, y : Covariate) : Predictor = !!x * !!y

    static member (*) (x : FactorExpr, y : CovariateExpr) : Predictor = !!x * !!y

    static member (*) (x : FactorExpr, y : StatVariable) : Predictor = !!x * !!y

    static member (*) (x : FactorExpr, y : Predictor) : Predictor = !!x * y


    static member (+) (x : FactorExpr, y : Factor) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : FactorExpr, y : FactorExpr) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : FactorExpr, y : CategoricalPredictor) : Predictor list = !!x + !!y

    static member (+) (x : FactorExpr, y : Covariate) : Predictor list = (!!x:Predictor) + (!!y:Predictor)

    static member (+) (x : FactorExpr, y : CovariateExpr) : Predictor list = !!x + !!y

    static member (+) (x : FactorExpr, y : StatVariable) : Predictor list = !!x + !!y

    static member (+) (x : FactorExpr, y : Predictor) : Predictor list = !!x + y

    static member (+) (x : Predictor list, y : FactorExpr) : Predictor list = x + (!!y:Predictor)

    static member (+) (x : FactorExpr, y : Predictor list) : Predictor list = (!!x:Predictor) + y

    member this.AsString = 
        this.AsFactor.ToString()


and Covariate(name : string, covariateStorage : ICovariateStorage) =

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

    static member (+) (x : Covariate, y : StatVariable) : Predictor list = !!x + !!y

    static member (+) (x : Covariate, y : Predictor) : Predictor list = !!x + y

    static member (+) (x : Predictor list, y : Covariate) : Predictor list = x + (!!y:Predictor)

    static member (+) (x : Covariate, y : Predictor list) : Predictor list = (!!x:Predictor) + y

                
    static member Log(covariate :  Covariate) = CovariateExpr.Log(covariate.AsExpr)

    static member Sqrt(covariate :  Covariate) = CovariateExpr.Sqrt(covariate.AsExpr)

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
    | FromFactor of FactorExpr * (string -> float)

    member this.Vars =
        match this with
            | Var(v) -> [StatVariable.Covariate v]
            | UnaryFunction(expr, _, _) -> expr.Vars
            | BinaryFunction(expr1, expr2, _, _) -> expr1.Vars @ expr2.Vars
            | FromFactor(f, _) -> f.Vars
                
    member this.MinLength =
        match this with
            | Var(c) ->  c.Length
            | UnaryFunction(expr, _, _) -> expr.MinLength 
            | BinaryFunction(expr1, expr2, _, _) ->  
                min expr1.MinLength expr2.MinLength
            | FromFactor(f, _) -> f.MinLength

    member this.MaxLength =
        match this with
            | Var(c) ->  c.Length
            | UnaryFunction(expr, _, _) -> expr.MaxLength 
            | BinaryFunction(expr1, expr2, _, _) ->  
                max expr1.MaxLength expr2.MaxLength
            | FromFactor(f, _) -> f.MaxLength

    member this.GetSlicesExpr(fromObs : int64, toObs : int64, sliceLen : int) =
        match this with
            | Var(c) ->
                c.GetSlices(fromObs, toObs, sliceLen) |> Seq.map (fun x -> x.AsExpr)
            | UnaryFunction(expr, f, _) -> 
                expr.GetSlicesExpr(fromObs, toObs, sliceLen) |> Seq.map f
            | BinaryFunction(expr1, expr2, f, _) -> 
                expr2.GetSlicesExpr(fromObs, toObs, sliceLen) |> Seq.zip (expr1.GetSlicesExpr(fromObs, toObs, sliceLen))
                                                              |> Seq.map f
            | FromFactor(factor, parse) ->
                let factor = factor.AsFactor
                let map = Array.init factor.LevelCount (factor.GetLevel >> parse)
                factor.GetSlices(fromObs, toObs, sliceLen) |> Seq.map (fun slice -> let v = new Vector(slice.LongLength, 0.0)
                                                                                    MklFunctions.Level_Index_To_Numeric(v.Length, slice.NativeArray, v.NativeArray, map)
                                                                                    v.AsExpr)
                
    member this.Name =
        match this with
            | Var(c) -> c.Name
            | UnaryFunction(expr, _, format) -> String.Format(format, expr.Name)
            | BinaryFunction(expr1, expr2, _, format) -> String.Format(format, expr1.Name, expr2.Name)
            | FromFactor(f, _) -> sprintf "%s.AsCovariate" f.Name

    member this.AsCovariate =
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
                        let buffer = new Vector(sliceLength, 0.0)
                        for sliceExpr in this.GetSlicesExpr(fromObs, toObs, sliceLength) do
                            yield VectorExpr.EvalIn(sliceExpr, sliceExpr.Length |> Option.map (fun len -> if len = buffer.LongLength then buffer else buffer.View(0L, len - 1L)))
                      }
             }
        let name = this.Name // remove ()
        new Covariate(name, covariateStorage)

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

    static member (+) (x : CovariateExpr, y : StatVariable) : Predictor list = !!x + !!y

    static member (+) (x : CovariateExpr, y : Predictor) : Predictor list = !!x + y

    static member (+) (x : Predictor list, y : CovariateExpr) : Predictor list = x + !!y

    static member (+) (x : CovariateExpr, y : Predictor list) : Predictor list = !!x + y


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

    static member (+) (x : StatVariable, y : StatVariable) : Predictor list = !!x + !!y

    static member (+) (x : StatVariable, y : Predictor) : Predictor list = !!x + y

    static member (+) (x : Predictor list, y : StatVariable) : Predictor list = x + !!y

    static member (+) (x : StatVariable, y : Predictor list) : Predictor list = !!x + y


and CategoricalPredictor =
    | Factor of FactorExpr
    | CategoricalInteraction of CategoricalPredictor * FactorExpr

    member this.AsList =
        match this with
            | Factor(f) -> [f.AsFactor]
            | CategoricalInteraction(catPred, factor) -> factor.AsFactor :: catPred.AsList

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

    static member (+) (x : CategoricalPredictor, y : StatVariable) : Predictor list = !!x + !!y

    static member (+) (x : CategoricalPredictor, y : Predictor) : Predictor list = !!x + y

    static member (+) (x : Predictor list, y : CategoricalPredictor) : Predictor list = x + !!y

    static member (+) (x : CategoricalPredictor, y : Predictor list) : Predictor list = !!x + y



    static member op_Explicit(factors : FactorExpr list) = 
        let rec fromList (factors : FactorExpr list) =
            match factors with
                | [] -> raise (new InvalidOperationException())
                | h::[] -> Factor(h)
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

    static member (+) (x : Predictor, y : StatVariable) : Predictor list = x + !!y

    static member (+) (x : Predictor, y : Predictor) = [x; y]


    static member (+) (x : Predictor list, y : Predictor) = x @ [y]


    static member (+) (x : Predictor, y : Predictor list) = x :: y








     

