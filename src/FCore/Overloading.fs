namespace FCore
open System
open ExplicitConversion

module Overloading =

    let inline explicitOverload< ^T, ^S, ^U when ^T : (static member op_Explicit : ^U * ^T ->  ^S)> (t : ^T) : ^S =
        (^T : (static member op_Explicit  : ^U * ^T -> ^S ) Unchecked.defaultof<'U>, t) 

    type DummyType = DummyType with

        static member ASinh (DummyType, x : float) = Vector.ASinh(new Vector(x)).[0]
        static member ASinh (DummyType, x) = Vector.ASinh(x)
        static member ASinh (DummyType, x) = Matrix.ASinh(x)
        static member ASinh (DummyType, x) = VectorExpr.ASinh(x)
        static member ASinh (DummyType, x) = MatrixExpr.ASinh(x)

        static member ACosh (DummyType, x : float) = Vector.ACosh(new Vector(x)).[0]
        static member ACosh (DummyType, x) = Vector.ACosh(x)
        static member ACosh (DummyType, x) = Matrix.ACosh(x)
        static member ACosh (DummyType, x) = VectorExpr.ACosh(x)
        static member ACosh (DummyType, x) = MatrixExpr.ACosh(x)

        static member ATanh (DummyType, x : float) = Vector.ATanh(new Vector(x)).[0]
        static member ATanh (DummyType, x) = Vector.ATanh(x)
        static member ATanh (DummyType, x) = Matrix.ATanh(x)
        static member ATanh (DummyType, x) = VectorExpr.ATanh(x)
        static member ATanh (DummyType, x) = MatrixExpr.ATanh(x)

        static member Expm1 (DummyType, x : float) = Vector.Expm1(new Vector(x)).[0]
        static member Expm1 (DummyType, x) = Vector.Expm1(x)
        static member Expm1 (DummyType, x) = Matrix.Expm1(x)
        static member Expm1 (DummyType, x) = VectorExpr.Expm1(x)
        static member Expm1 (DummyType, x) = MatrixExpr.Expm1(x)

        static member Log1p (DummyType, x : float) = Vector.Log1p(new Vector(x)).[0]
        static member Log1p (DummyType, x) = Vector.Log1p(x)
        static member Log1p (DummyType, x) = Matrix.Log1p(x)
        static member Log1p (DummyType, x) = VectorExpr.Log1p(x)
        static member Log1p (DummyType, x) = MatrixExpr.Log1p(x)

        static member Erf (DummyType, x : float) = Vector.Erf(new Vector(x)).[0]
        static member Erf (DummyType, x) = Vector.Erf(x)
        static member Erf (DummyType, x) = Matrix.Erf(x)
        static member Erf (DummyType, x) = VectorExpr.Erf(x)
        static member Erf (DummyType, x) = MatrixExpr.Erf(x)

        static member Erfc (DummyType, x : float) = Vector.Erfc(new Vector(x)).[0]
        static member Erfc (DummyType, x) = Vector.Erfc(x)
        static member Erfc (DummyType, x) = Matrix.Erfc(x)
        static member Erfc (DummyType, x) = VectorExpr.Erfc(x)
        static member Erfc (DummyType, x) = MatrixExpr.Erfc(x)

        static member Erfinv (DummyType, x : float) = Vector.Erfinv(new Vector(x)).[0]
        static member Erfinv (DummyType, x) = Vector.Erfinv(x)
        static member Erfinv (DummyType, x) = Matrix.Erfinv(x)
        static member Erfinv (DummyType, x) = VectorExpr.Erfinv(x)
        static member Erfinv (DummyType, x) = MatrixExpr.Erfinv(x)

        static member Erfcinv (DummyType, x : float) = Vector.Erfcinv(new Vector(x)).[0]
        static member Erfcinv (DummyType, x) = Vector.Erfcinv(x)
        static member Erfcinv (DummyType, x) = Matrix.Erfcinv(x)
        static member Erfcinv (DummyType, x) = VectorExpr.Erfcinv(x)
        static member Erfcinv (DummyType, x) = MatrixExpr.Erfcinv(x)

        static member Normcdf (DummyType, x : float) = Vector.Normcdf(new Vector(x)).[0]
        static member Normcdf (DummyType, x) = Vector.Normcdf(x)
        static member Normcdf (DummyType, x) = Matrix.Normcdf(x)
        static member Normcdf (DummyType, x) = VectorExpr.Normcdf(x)
        static member Normcdf (DummyType, x) = MatrixExpr.Normcdf(x)

        static member Norminv (DummyType, x : float) = Vector.Norminv(new Vector(x)).[0]
        static member Norminv (DummyType, x) = Vector.Norminv(x)
        static member Norminv (DummyType, x) = Matrix.Norminv(x)
        static member Norminv (DummyType, x) = VectorExpr.Norminv(x)
        static member Norminv (DummyType, x) = MatrixExpr.Norminv(x)

        static member Concat (DummyType, x) = BoolVector.Concat(x)
        static member Concat (DummyType, x) = Vector.Concat(x)

        static member Transpose (DummyType, x) = Matrix.Transpose(x)

        static member inline Diag (DummyType, x : Matrix, offset) =
            let offset : T1orT2<int, int64> = !!offset
            match offset with
                | T1of2(offset) ->
                    x.Diag(offset)
                | T2of2(offset) ->
                    x.Diag(offset)

        static member inline Diag (DummyType, x : Vector, offset) =
            let offset : T1orT2<int, int64> = !!offset
            let n = x.LongLength
            match offset with
                | T1of2(offset) ->
                    let k = (if offset < 0 then -offset else offset) |> int64
                    let res = new Matrix(n + k, n + k, 0.0)
                    res.Diag(offset) <- x
                    res
                | T2of2(offset) ->
                    let k = (if offset < 0L then -offset else offset) 
                    let res = new Matrix(n + k, n + k, 0.0)
                    res.Diag(offset) <- x
                    res

        static member UpperTri (DummyType, matrix, offset : int) = Matrix.UpperTri(matrix, offset)
        static member UpperTri (DummyType, matrix, offset : int64) = Matrix.UpperTri(matrix, offset)

        static member LowerTri (DummyType, matrix, offset : int) = Matrix.LowerTri(matrix, offset)
        static member LowerTri (DummyType, matrix, offset : int64) = Matrix.LowerTri(matrix, offset)

        static member EvalIn (DummyType, expr : BoolVectorExpr, res : BoolVector option) = BoolVectorExpr.EvalIn(expr, res)
        static member EvalIn (DummyType, expr : VectorExpr, res : Vector option) = VectorExpr.EvalIn(expr, res)
        static member EvalIn (DummyType, expr : BoolMatrixExpr, res : BoolMatrix option) = BoolMatrixExpr.EvalIn(expr, res)
        static member EvalIn (DummyType, expr : MatrixExpr, res : Matrix option) = MatrixExpr.EvalIn(expr, res)

        static member IIf (DummyType, boolExpr : BoolVectorExpr, trueExpr : BoolVectorExpr, falseExpr : BoolVectorExpr) =
            BoolVectorExpr.IfFunction(boolExpr, trueExpr, falseExpr)

        static member IIf (DummyType, boolExpr : BoolVectorExpr, trueVector : BoolVector, falseVector : BoolVector) =
            BoolVectorExpr.IfFunction(boolExpr, BoolVectorExpr.Var(trueVector), BoolVectorExpr.Var(falseVector))

        static member IIf (DummyType, boolExpr : BoolVectorExpr, trueValue : bool, falseValue : bool) =
            BoolVectorExpr.IfFunction(boolExpr, BoolVectorExpr.Var(new BoolVector(trueValue)), BoolVectorExpr.Var(new BoolVector(falseValue)))


        static member IIf (DummyType, boolExpr : BoolVectorExpr, trueExpr : VectorExpr, falseExpr : VectorExpr) =
            VectorExpr.IfFunction(boolExpr, trueExpr, falseExpr)

        static member IIf (DummyType, boolExpr : BoolVectorExpr, trueVector : Vector, falseVector : Vector) =
            VectorExpr.IfFunction(boolExpr, VectorExpr.Var(trueVector), VectorExpr.Var(falseVector))

        static member IIf (DummyType, boolExpr : BoolVectorExpr, trueValue : float, falseValue : float) =
            VectorExpr.IfFunction(boolExpr, VectorExpr.Var(new Vector(trueValue)), VectorExpr.Var(new Vector(falseValue)))



        static member IIf (DummyType, boolExpr : BoolMatrixExpr, trueExpr : BoolMatrixExpr, falseExpr : BoolMatrixExpr) =
            BoolMatrixExpr.IfFunction(boolExpr, trueExpr, falseExpr)

        static member IIf (DummyType, boolExpr : BoolMatrixExpr, trueMatrix : BoolMatrix, falseMatrix : BoolMatrix) =
            BoolMatrixExpr.IfFunction(boolExpr, BoolMatrixExpr.Var(trueMatrix), BoolMatrixExpr.Var(falseMatrix))

        static member IIf (DummyType, boolExpr : BoolMatrixExpr, trueValue : bool, falseValue : bool) =
            BoolMatrixExpr.IfFunction(boolExpr, BoolMatrixExpr.Var(new BoolMatrix(trueValue)), BoolMatrixExpr.Var(new BoolMatrix(falseValue)))


        static member IIf (DummyType, boolExpr : BoolMatrixExpr, trueExpr : MatrixExpr, falseExpr : MatrixExpr) =
            MatrixExpr.IfFunction(boolExpr, trueExpr, falseExpr)

        static member IIf (DummyType, boolExpr : BoolMatrixExpr, trueMatrix : Matrix, falseMatrix : Matrix) =
            MatrixExpr.IfFunction(boolExpr, MatrixExpr.Var(trueMatrix), MatrixExpr.Var(falseMatrix))

        static member IIf (DummyType, boolExpr : BoolMatrixExpr, trueValue : float, falseValue : float) =
            MatrixExpr.IfFunction(boolExpr, MatrixExpr.Var(new Matrix(trueValue)), MatrixExpr.Var(new Matrix(falseValue)))


        static member Chol (DummyType, x) = Matrix.Chol(x)

        static member CholInv (DummyType, x) = Matrix.CholInv(x)

        static member CholSolve (DummyType, a, b) = Matrix.CholSolve(a, b)

        static member Lu (DummyType, x) = Matrix.Lu(x)

        static member LuInv (DummyType, x) = Matrix.LuInv(x)

        static member LuSolve (DummyType, a, b) = Matrix.LuSolve(a, b)

        static member Qr (DummyType, x) = Matrix.Qr(x)

        static member QrSolveFull (DummyType, a, b) = Matrix.QrSolveFull(a, b)

        static member QrSolve (DummyType, a, b, tol) = Matrix.QrSolve(a, b, tol)

        static member SvdSolve (DummyType, a, b, tol) = Matrix.SvdSolve(a, b, tol)

        static member SvdValues (DummyType, x) = Matrix.SvdValues(x)

        static member Svd (DummyType, x) = Matrix.Svd(x)

        static member Eig (DummyType, x) = Matrix.Eig(x)

        static member EigValues (DummyType, x) = Matrix.EigValues(x)

        static member Min (DummyType, x) = Vector.Min(x)
        static member Min (DummyType, x) = Matrix.Min(x)

        static member Max (DummyType, x) = Vector.Max(x)
        static member Max (DummyType, x) = Matrix.Max(x)

        static member Sum (DummyType, x) = Vector.Sum(x)
        static member Sum (DummyType, x) = Matrix.Sum(x)

        static member Prod (DummyType, x) = Vector.Prod(x)
        static member Prod (DummyType, x) = Matrix.Prod(x)

        static member CumSum (DummyType, x) = Vector.CumSum(x)
        static member CumSum (DummyType, x) = Matrix.CumSum(x)

        static member CumProd (DummyType, x) = Vector.CumProd(x)
        static member CumProd (DummyType, x) = Matrix.CumProd(x)

        static member Mean (DummyType, x) = Vector.Mean(x)
        static member Mean (DummyType, x) = Matrix.Mean(x)

        static member Variance (DummyType, x) = Vector.Variance(x)
        static member Variance (DummyType, x) = Matrix.Variance(x)

        static member Skewness (DummyType, x) = Vector.Skewness(x)
        static member Skewness (DummyType, x) = Matrix.Skewness(x)

        static member Kurtosis (DummyType, x) = Vector.Kurtosis(x)
        static member Kurtosis (DummyType, x) = Matrix.Kurtosis(x)

        static member Quantile (DummyType, x) = Vector.Quantile(x)
        static member Quantile (DummyType, x) = Matrix.Quantile(x)

        static member Corr (DummyType, x) = Matrix.Corr(x)

        static member Cov (DummyType, x) = Matrix.Cov(x)

