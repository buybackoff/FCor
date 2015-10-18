namespace FCor
open System
open ExplicitConversion
open Overloading

module BasicStats =

    let inline min (x :'S) : 'U = ((^T or ^S) : (static member Min: ^T * ^S -> ^U) DummyType, x)

    let inline max (x :'S) : 'U = ((^T or ^S) : (static member Max: ^T * ^S -> ^U) DummyType, x)

    let inline sum (x :'S) : 'U = ((^T or ^S) : (static member Sum: ^T * ^S -> ^U) DummyType, x)

    let inline prod (x :'S) : 'U = ((^T or ^S) : (static member Prod: ^T * ^S -> ^U) DummyType, x)

    let inline any (x :'S) : 'U = ((^T or ^S) : (static member Any: ^T * ^S -> ^U) DummyType, x)

    let inline all (x :'S) : 'U = ((^T or ^S) : (static member All: ^T * ^S -> ^U) DummyType, x)

    let inline cumsum (x :'S) : 'U = ((^T or ^S) : (static member CumSum: ^T * ^S -> ^U) DummyType, x)

    let inline cumprod (x :'S) : 'U = ((^T or ^S) : (static member CumProd: ^T * ^S -> ^U) DummyType, x)

    let inline mean (x :'S) : 'U = ((^T or ^S) : (static member Mean: ^T * ^S -> ^U) DummyType, x)

    let inline var (x :'S) : 'U = ((^T or ^S) : (static member Variance: ^T * ^S -> ^U) DummyType, x)

    let inline skewness (x :'S) : 'U = ((^T or ^S) : (static member Skewness: ^T * ^S -> ^U) DummyType, x)

    let inline kurtosis (x :'S) : 'U = ((^T or ^S) : (static member Kurtosis: ^T * ^S -> ^U) DummyType, x)

    let inline quantile (x :'S) : 'U = ((^T or ^S) : (static member Quantile: ^T * ^S -> ^U) DummyType, x)

    let inline corr (x :'S) : 'S = ((^T or ^S) : (static member Corr: ^T * ^S -> ^S) DummyType, x)

    let inline cov (x :'S) : 'S = ((^T or ^S) : (static member Cov: ^T * ^S -> ^S) DummyType, x)



