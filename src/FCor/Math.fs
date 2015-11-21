namespace FCor
open System
open ExplicitConversion
open Overloading

module Math =

    let inline max2 (x : 'S) (y : 'U) : 'V  = ((^T or ^S) : (static member Max: ^T * ^S * ^U -> ^V) DummyType, x, y)

    let inline min2 (x : 'S) (y : 'U) : 'V  = ((^T or ^S) : (static member Min: ^T * ^S * ^U -> ^V) DummyType, x, y)

    let inline abs (x :'S) : 'U = ((^T or ^S) : (static member Abs: ^T * ^S -> ^U) DummyType, x)

    let inline log (x :'S) : 'U = ((^T or ^S) : (static member Log: ^T * ^S -> ^U) DummyType, x)

    let inline log10 (x :'S) : 'U = ((^T or ^S) : (static member Log10: ^T * ^S -> ^U) DummyType, x)

    let inline exp (x :'S) : 'U = ((^T or ^S) : (static member Exp: ^T * ^S -> ^U) DummyType, x)

    let inline sqrt (x :'S) : 'U = ((^T or ^S) : (static member Sqrt: ^T * ^S -> ^U) DummyType, x)

    let inline round (x :'S) : 'U = ((^T or ^S) : (static member Round: ^T * ^S -> ^U) DummyType, x)

    let inline ceil (x :'S) : 'U = ((^T or ^S) : (static member Ceiling: ^T * ^S -> ^U) DummyType, x)

    let inline floor (x :'S) : 'U = ((^T or ^S) : (static member Floor: ^T * ^S -> ^U) DummyType, x)

    let inline trunc (x :'S) : 'U = ((^T or ^S) : (static member Truncate: ^T * ^S -> ^U) DummyType, x)

    let inline asinh (x :'S) : 'S = ((^T or ^S) : (static member ASinh: ^T * ^S -> ^S) DummyType, x)

    let inline acosh (x :'S) : 'S = ((^T or ^S) : (static member ACosh: ^T * ^S -> ^S) DummyType, x)

    let inline atanh (x :'S) : 'S = ((^T or ^S) : (static member ATanh: ^T * ^S -> ^S) DummyType, x)

    let inline expm1 (x :'S) : 'S = ((^T or ^S) : (static member Expm1: ^T * ^S -> ^S) DummyType, x)

    let inline log1p (x :'S) : 'S = ((^T or ^S) : (static member Log1p: ^T * ^S -> ^S) DummyType, x)

    let inline erf (x :'S) : 'S = ((^T or ^S) : (static member Erf: ^T * ^S -> ^S) DummyType, x)

    let inline erfc (x :'S) : 'S = ((^T or ^S) : (static member Erfc: ^T * ^S -> ^S) DummyType, x)

    let inline erfinv (x :'S) : 'S = ((^T or ^S) : (static member Erfinv: ^T * ^S -> ^S) DummyType, x)

    let inline erfcinv (x :'S) : 'S = ((^T or ^S) : (static member Erfcinv: ^T * ^S -> ^S) DummyType, x)

    let inline normcdf (x :'S) : 'S = ((^T or ^S) : (static member Normcdf: ^T * ^S -> ^S) DummyType, x)

    let inline norminv (x :'S) : 'S = ((^T or ^S) : (static member Norminv: ^T * ^S -> ^S) DummyType, x)

    let inline axpby (a : 'U) (x :'S) (b : 'U) (y : 'S) : 'S = ((^T or ^S) : (static member Axpby: ^T * ^U * ^S * ^U * ^S -> ^S) DummyType, a, x, b, y)

    let inline I< ^T, ^S when ^T : (static member Identity : ^S * ^S ->  ^T)> (rows : ^S) (cols : ^S) : ^T =
        (^T : (static member Identity  : ^S * ^S -> ^T ) rows, cols)

    let inline concat (x :'S seq) : 'S = ((^T or ^S) : (static member Concat: ^T * ^S seq -> ^S) DummyType, x)

    let inline diag (x : 'S) (offset : 'U) : 'V = ((^T or ^S) : (static member Diag: ^T * ^S * ^U -> ^V) DummyType, x, offset)

    let inline transpose (x :'S) : 'S = ((^T or ^S) : (static member Transpose: ^T * ^S -> ^S) DummyType, x)

    let inline triU (matrix :'S) (offset : 'U) : 'S = ((^T or ^S) : (static member UpperTri: ^T * ^S * ^U -> ^S) DummyType, matrix, offset)

    let inline triL (matrix :'S) (offset : 'U) : 'S = ((^T or ^S) : (static member LowerTri: ^T * ^S * ^U -> ^S) DummyType, matrix, offset)

    let inline eval (expr :'S) : 'U = ((^T or ^S) : (static member EvalIn: ^T * ^S * ^U option -> ^U) DummyType, expr, None)

    let inline evalIn (expr :'S) (res : 'U) = ((^T or ^S) : (static member EvalIn: ^T * ^S * ^U option -> ^U) DummyType, expr, (Some res)) |> ignore

    let inline iif (boolExpr :'S) (trueExpr : 'U) (falseExpr : 'V) : 'Z = ((^T or ^S) : (static member IIf: ^T * ^S * ^U * ^V -> ^Z) DummyType, boolExpr, trueExpr, falseExpr)