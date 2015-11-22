namespace FCor
#nowarn "9"

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic
open System.IO
open System.Text
open FCor.ExplicitConversion
open Overloading

module StatModels =
    
    let inline ( <~> ) (x : 'S) (y : 'U) : 'V = ((^T or ^S) : (static member StatFormula: ^T * ^S * ^U -> ^V) DummyType, x, y)

    let inline (|>>) (x : 'S) (y : 'U) : 'V = ((^T or ^S) : (static member ConvertStatVar: ^T * ^S * ^U -> ^V) DummyType, x, y)

    let glm (statFormula : CovariateExpr * (Predictor list)) (includeIntercept : bool)
            (glmDistribution : GlmDistribution) (glmLink : GlmLink) (maxIter : int) (eps : float) =
        let response, predictors = statFormula
        Glm.fitModel response predictors includeIntercept glmDistribution glmLink 10000 maxIter eps
