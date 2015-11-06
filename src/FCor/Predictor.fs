namespace FCor
#nowarn "9"

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic
open FCor.ExplicitConversion

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
                        use buffer = new IntVector(sliceLength, 0)
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


and Covariate(name : string, covariateStorage : ICovariateStorage) =

    member this.Name = name
    member this.Length = covariateStorage.Length

    member this.AsExpr = CovariateExpr.Var(this)

    member this.GetSlices(fromObs : int64, toObs : int64, sliceLen) = covariateStorage.GetSlices(fromObs, toObs, sliceLen)

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
        let name = this.Name
        new Covariate(name, covariateStorage)

    static member Log(covariateExpr : CovariateExpr) =
        UnaryFunction(covariateExpr, VectorExpr.Log, "log({0})")

    static member Sqrt(covariateExpr : CovariateExpr) =
        UnaryFunction(covariateExpr, VectorExpr.Sqrt, "sqrt({0})")

    static member (*) (covariateExpr1 : CovariateExpr, covariateExpr2 : CovariateExpr) =
        BinaryFunction(covariateExpr1, covariateExpr2, VectorExpr.op_DotMultiply, "{0}*{1}")

    static member (*) (factor : Factor, covariateExpr : CovariateExpr) : Predictor =
        MixedInteraction(!!factor, covariateExpr)

    static member (*) (catPredictor : CategoricalPredictor, covariateExpr : CovariateExpr) : Predictor =
        MixedInteraction(catPredictor, covariateExpr)

    static member (*) (covariateExpr : CovariateExpr, catPredictor : CategoricalPredictor) : Predictor =
        MixedInteraction(catPredictor, covariateExpr)
        
and StatVariable =
    | Factor of Factor
    | Covariate of Covariate

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






     

