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

    static member Intercept = intercept

    static member (*) (factor1 : Factor, factor2 : Factor) : CategoricalPredictor =
        CategoricalInteraction(!!factor1, factor2)

    static member (*) (catPredictor : CategoricalPredictor, factor : Factor) : CategoricalPredictor =
        CategoricalInteraction(catPredictor, factor)

    static member (+) (factor1 : Factor, factor2 : Factor) : Predictor list =
        CategoricalPredictor(!!factor1) + CategoricalPredictor(!!factor2)
            
    static member (+) (catPredictor : CategoricalPredictor, factor : Factor) : Predictor list = 
        !!catPredictor + CategoricalPredictor(!!factor)

    static member (+) (predictors : Predictor list, factor : Factor) =
        predictors @ [!!factor]   

    static member (+) (factor : Factor, predictors : Predictor list) =
        !!factor :: predictors

    interface IDisposable with
        member this.Dispose() = this.DoDispose(true)

    member internal this.DoDispose(isDisposing) = 
        if isDisposing then GC.SuppressFinalize(this)
        match factorStorage with
            | :? IDisposable as d -> d.Dispose()
            | _ -> ()

    override this.Finalize() = try this.DoDispose(false) with _ -> ()


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

    static member (*) (factor : Factor, covariate : Covariate) : Predictor =
        factor * covariate.AsExpr

    static member (*) (covariate : Covariate, factor : Factor) : Predictor =
        factor * covariate.AsExpr

    static member (*) (catPredictor : CategoricalPredictor, covariate : Covariate) : Predictor =
       catPredictor * covariate.AsExpr

    static member (*) (covariate : Covariate, catPredictor : CategoricalPredictor) : Predictor =
        catPredictor * covariate.AsExpr

    static member (+) (covariate1 : Covariate, covariate2 : Covariate) : Predictor list =
         NumericalPredictor(covariate1.AsExpr) + NumericalPredictor(covariate2.AsExpr)

    static member (+) (factor : Factor, covariate : Covariate) : Predictor list =
        CategoricalPredictor(!!factor) + !!covariate

    static member (+) (covariate : Covariate, factor : Factor) : Predictor list =
        !!covariate + CategoricalPredictor(!!factor) 

    static member (+) (catPredictor : CategoricalPredictor, covariate : Covariate) : Predictor list =
        CategoricalPredictor(catPredictor) + !!covariate

    static member (+) (covariate : Covariate, catPredictor : CategoricalPredictor) : Predictor list =
        !!covariate + CategoricalPredictor(catPredictor) 

    static member (+) (predictors : Predictor list, covariate : Covariate) =
         predictors @ [!!covariate] 

    static member (+) (covariate : Covariate, predictors : Predictor list) =
        !!covariate :: predictors

    static member (*) (covariate1 : Covariate, covariate2 : Covariate) = covariate1.AsExpr * covariate2.AsExpr
                
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

and CovariateExpr =
    | Var of Covariate
    | UnaryFunction of CovariateExpr * (VectorExpr -> VectorExpr)  * string
    | BinaryFunction of CovariateExpr * CovariateExpr * (VectorExpr * VectorExpr -> VectorExpr) * string

    member this.Vars =
        match this with
            | Var(v) -> [v]
            | UnaryFunction(expr, _, _) -> expr.Vars
            | BinaryFunction(expr1, expr2, _, _) -> expr1.Vars @ expr2.Vars
                
    member this.MinLength =
        match this with
            | Var(c) ->  c.Length
            | UnaryFunction(expr, _, _) -> expr.MinLength 
            | BinaryFunction(expr1, expr2, _, _) ->  
                min expr1.MinLength expr2.MinLength

    member this.MaxLength =
        match this with
            | Var(c) ->  c.Length
            | UnaryFunction(expr, _, _) -> expr.MaxLength 
            | BinaryFunction(expr1, expr2, _, _) ->  
                max expr1.MaxLength expr2.MaxLength

    member this.GetSlicesExpr(fromObs : int64, toObs : int64, sliceLen : int) =
        match this with
            | Var(c) ->
                c.GetSlices(fromObs, toObs, sliceLen) |> Seq.map (fun x -> x.AsExpr)
            | UnaryFunction(expr, f, _) -> 
                expr.GetSlicesExpr(fromObs, toObs, sliceLen) |> Seq.map f
            | BinaryFunction(expr1, expr2, f, _) -> 
                expr2.GetSlicesExpr(fromObs, toObs, sliceLen) |> Seq.zip (expr1.GetSlicesExpr(fromObs, toObs, sliceLen))
                                                              |> Seq.map f

    member this.Name =
        match this with
            | Var(c) -> c.Name
            | UnaryFunction(expr, _, format) -> String.Format(format, expr.Name)
            | BinaryFunction(expr1, expr2, _, format) -> String.Format(format, expr1.Name, expr2.Name)

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

    static member (*) (covariateExpr1 : CovariateExpr, covariateExpr2 : CovariateExpr) =
        BinaryFunction(covariateExpr1, covariateExpr2, VectorExpr.op_DotMultiply, "({0:G}*{1:G})")

    static member (/) (covariateExpr1 : CovariateExpr, covariateExpr2 : CovariateExpr) =
        BinaryFunction(covariateExpr1, covariateExpr2, VectorExpr.op_DotDivide, "({0:G}/{1:G})")

    static member (+) (covariateExpr1 : CovariateExpr, covariateExpr2 : CovariateExpr) =
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

    static member (*) (factor : Factor, covariateExpr : CovariateExpr) : Predictor =
        MixedInteraction(!!factor, covariateExpr)

    static member (*) (catPredictor : CategoricalPredictor, covariateExpr : CovariateExpr) : Predictor =
        MixedInteraction(catPredictor, covariateExpr)

    static member (*) (covariateExpr : CovariateExpr, catPredictor : CategoricalPredictor) : Predictor =
        MixedInteraction(catPredictor, covariateExpr)

    static member (+) (factor : Factor, covariateExpr : CovariateExpr) : Predictor list =
        factor + covariateExpr.AsCovariate

    static member (+) (covariate : Covariate, covariateExpr : CovariateExpr) : Predictor list =
        covariate + covariateExpr.AsCovariate

    static member (+) (predictors : Predictor list, covariate : CovariateExpr) : Predictor list =
        predictors + NumericalPredictor(covariate)
        
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

    static member (*) (statVar1 : StatVariable, statVar2 : StatVariable) : Predictor =
        match statVar1, statVar2 with
            | Factor(f1), Factor(f2) -> f1 * f2 |> (!!)
            | Factor(f), Covariate(c) -> f * c 
            | Covariate(c), Factor(f) -> c * f
            | Covariate(c1), Covariate(c2) -> c1 * c2 |> (!!)

    static member (+) (statVar1 : StatVariable, statVar2 : StatVariable) : Predictor list =
        match statVar1, statVar2 with
            | Factor(f1), Factor(f2) -> f1 + f2 
            | Factor(f), Covariate(c) -> f + c 
            | Covariate(c), Factor(f) -> c + f
            | Covariate(c1), Covariate(c2) -> c1 + c2 

    static member (+) (predictors : Predictor list, statVar : StatVariable) : Predictor list =
        match statVar with
            | Factor(f) -> predictors + f
            | Covariate(c) -> predictors + c

and CategoricalPredictor =
    | Factor of Factor
    | CategoricalInteraction of CategoricalPredictor * Factor

    member this.AsList =
        match this with
            | Factor(f) -> [f]
            | CategoricalInteraction(catPred, factor) -> factor :: catPred.AsList

    member this.MinLength =
        match this with
            | Factor(f) -> f.Length
            | CategoricalInteraction(c, f) ->
                min c.MinLength f.Length

    member this.MaxLength =
        match this with
            | Factor(f) -> f.Length
            | CategoricalInteraction(c, f) ->
                max c.MaxLength f.Length

    static member op_Explicit(factor : Factor) = Factor(factor)
    static member op_Explicit(catPred :  CategoricalPredictor, factor : Factor) = CategoricalInteraction(catPred, factor)
    static member op_Explicit(factor1 :  Factor, factor2 : Factor) = CategoricalInteraction(Factor(factor1), factor2)

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

    static member op_Explicit(catPred :  CategoricalPredictor) = CategoricalPredictor(catPred)
    static member op_Explicit(factor :  Factor) = CategoricalPredictor(CategoricalPredictor.Factor(factor))
    static member op_Explicit(covariate :  Covariate) = NumericalPredictor(covariate.AsExpr)
    static member op_Explicit(covariateExpr :  CovariateExpr) = NumericalPredictor(covariateExpr)
    static member op_Explicit(catPred :  CategoricalPredictor, covariate :  Covariate) = MixedInteraction(catPred, covariate.AsExpr)
    static member op_Explicit(catPred :  CategoricalPredictor, covariateExpr :  CovariateExpr) = MixedInteraction(catPred, covariateExpr)
    static member op_Explicit(factor :  Factor, covariate :  Covariate) = MixedInteraction(CategoricalPredictor.Factor(factor), covariate.AsExpr)
    static member op_Explicit(factor :  Factor, covariateExpr :  CovariateExpr) = MixedInteraction(CategoricalPredictor.Factor(factor), covariateExpr)

    static member (*) (predictor : Predictor, factor : Factor) : Predictor =
        match predictor with
            | CategoricalPredictor(catPred) -> !!CategoricalInteraction(catPred, factor)
            | NumericalPredictor(covExpr) -> MixedInteraction(!!factor, covExpr)
            | MixedInteraction(catPred, covExpr) -> MixedInteraction(catPred * factor, covExpr)

    static member (*) (predictor : Predictor, covariateExpr : CovariateExpr) : Predictor =
        match predictor with
            | CategoricalPredictor(catPred) -> MixedInteraction(catPred, covariateExpr)
            | NumericalPredictor(covExpr) -> NumericalPredictor(covExpr * covariateExpr)
            | MixedInteraction(catPred, covExpr) -> MixedInteraction(catPred, covExpr * covariateExpr)

    static member (*) (predictor : Predictor, covariate : Covariate) : Predictor =
        predictor * covariate.AsExpr

    static member (+) (predictor1 : Predictor, predictor2 : Predictor) =
        [predictor1; predictor2]

    static member (+) (predictors : Predictor list, predictor : Predictor) =
         predictors @ [predictor]

    static member (+) (predictor : Predictor, predictors : Predictor list) =
        predictor :: predictors






     

