namespace FCor
open System
open ExplicitConversion

module Overloading =

    let inline explicitOverload< ^T, ^S, ^U when ^T : (static member op_Explicit : ^U * ^T ->  ^S)> (t : ^T) : ^S =
        (^T : (static member op_Explicit  : ^U * ^T -> ^S ) Unchecked.defaultof<'U>, t) 

    type DummyType = DummyType with

        static member Abs (DummyType, x : float) = Math.Abs(x)
        static member Abs (DummyType, x) = Vector.Abs(x)
        static member Abs (DummyType, x) = Matrix.Abs(x)
        static member Abs (DummyType, x) = VectorExpr.Abs(x)
        static member Abs (DummyType, x) = MatrixExpr.Abs(x)
        static member Abs (DummyType, x) = CovariateExpr.Abs(x)
        static member Abs (DummyType, x) = Covariate.Abs(x)

        static member Log (DummyType, x : float) = Math.Log(x)
        static member Log (DummyType, x) = Vector.Log(x)
        static member Log (DummyType, x) = Matrix.Log(x)
        static member Log (DummyType, x) = VectorExpr.Log(x)
        static member Log (DummyType, x) = MatrixExpr.Log(x)
        static member Log (DummyType, x) = CovariateExpr.Log(x)
        static member Log (DummyType, x) = Covariate.Log(x)


        static member Log10 (DummyType, x : float) = Math.Log10(x)
        static member Log10 (DummyType, x) = Vector.Log10(x)
        static member Log10 (DummyType, x) = Matrix.Log10(x)
        static member Log10 (DummyType, x) = VectorExpr.Log10(x)
        static member Log10 (DummyType, x) = MatrixExpr.Log10(x)
        static member Log10 (DummyType, x) = CovariateExpr.Log10(x)
        static member Log10 (DummyType, x) = Covariate.Log10(x)


        static member Exp (DummyType, x : float) = Math.Exp(x)
        static member Exp (DummyType, x) = Vector.Exp(x)
        static member Exp (DummyType, x) = Matrix.Exp(x)
        static member Exp (DummyType, x) = VectorExpr.Exp(x)
        static member Exp (DummyType, x) = MatrixExpr.Exp(x)
        static member Exp (DummyType, x) = CovariateExpr.Exp(x)
        static member Exp (DummyType, x) = Covariate.Exp(x)

        static member Sqrt (DummyType, x : float) = Math.Sqrt(x)
        static member Sqrt (DummyType, x) = Vector.Sqrt(x)
        static member Sqrt (DummyType, x) = Matrix.Sqrt(x)
        static member Sqrt (DummyType, x) = VectorExpr.Sqrt(x)
        static member Sqrt (DummyType, x) = MatrixExpr.Sqrt(x)
        static member Sqrt (DummyType, x) = CovariateExpr.Sqrt(x)
        static member Sqrt (DummyType, x) = Covariate.Sqrt(x)

        static member Round (DummyType, x : float) = Math.Round(x)
        static member Round (DummyType, x) = Vector.Round(x)
        static member Round (DummyType, x) = Matrix.Round(x)
        static member Round (DummyType, x) = VectorExpr.Round(x)
        static member Round (DummyType, x) = MatrixExpr.Round(x)
        static member Round (DummyType, x) = CovariateExpr.Round(x)
        static member Round (DummyType, x) = Covariate.Round(x)

        static member Ceiling (DummyType, x : float) = Math.Ceiling(x)
        static member Ceiling (DummyType, x) = Vector.Ceiling(x)
        static member Ceiling (DummyType, x) = Matrix.Ceiling(x)
        static member Ceiling (DummyType, x) = VectorExpr.Ceiling(x)
        static member Ceiling (DummyType, x) = MatrixExpr.Ceiling(x)
        static member Ceiling (DummyType, x) = CovariateExpr.Ceiling(x)
        static member Ceiling (DummyType, x) = Covariate.Ceiling(x)

        static member Floor (DummyType, x : float) = Math.Floor(x)
        static member Floor (DummyType, x) = Vector.Floor(x)
        static member Floor (DummyType, x) = Matrix.Floor(x)
        static member Floor (DummyType, x) = VectorExpr.Floor(x)
        static member Floor (DummyType, x) = MatrixExpr.Floor(x)
        static member Floor (DummyType, x) = CovariateExpr.Floor(x)
        static member Floor (DummyType, x) = Covariate.Floor(x)

        static member Truncate (DummyType, x : float) = Math.Truncate(x)
        static member Truncate (DummyType, x) = Vector.Truncate(x)
        static member Truncate (DummyType, x) = Matrix.Truncate(x)
        static member Truncate (DummyType, x) = VectorExpr.Truncate(x)
        static member Truncate (DummyType, x) = MatrixExpr.Truncate(x)
        static member Truncate (DummyType, x) = CovariateExpr.Truncate(x)
        static member Truncate (DummyType, x) = Covariate.Truncate(x)

        static member ASinh (DummyType, x : float) = 
            let mutable res = x
            MklFunctions.D_ASinh_Array(1L, &&res, &&res)
            res

        static member ASinh (DummyType, x) = Vector.ASinh(x)
        static member ASinh (DummyType, x) = Matrix.ASinh(x)
        static member ASinh (DummyType, x) = VectorExpr.ASinh(x)
        static member ASinh (DummyType, x) = MatrixExpr.ASinh(x)

        static member ACosh (DummyType, x : float) = 
            let mutable res = x
            MklFunctions.D_ACosh_Array(1L, &&res, &&res)
            res

        static member ACosh (DummyType, x) = Vector.ACosh(x)
        static member ACosh (DummyType, x) = Matrix.ACosh(x)
        static member ACosh (DummyType, x) = VectorExpr.ACosh(x)
        static member ACosh (DummyType, x) = MatrixExpr.ACosh(x)

        static member ATanh (DummyType, x : float) = 
            let mutable res = x
            MklFunctions.D_ATanh_Array(1L, &&res, &&res)
            res

        static member ATanh (DummyType, x) = Vector.ATanh(x)
        static member ATanh (DummyType, x) = Matrix.ATanh(x)
        static member ATanh (DummyType, x) = VectorExpr.ATanh(x)
        static member ATanh (DummyType, x) = MatrixExpr.ATanh(x)

        static member Expm1 (DummyType, x : float) = 
            let mutable res = x
            MklFunctions.D_Expm1_Array(1L, &&res, &&res)
            res

        static member Expm1 (DummyType, x) = Vector.Expm1(x)
        static member Expm1 (DummyType, x) = Matrix.Expm1(x)
        static member Expm1 (DummyType, x) = VectorExpr.Expm1(x)
        static member Expm1 (DummyType, x) = MatrixExpr.Expm1(x)

        static member Log1p (DummyType, x : float) = 
            let mutable res = x
            MklFunctions.D_Log1p_Array(1L, &&res, &&res)
            res

        static member Log1p (DummyType, x) = Vector.Log1p(x)
        static member Log1p (DummyType, x) = Matrix.Log1p(x)
        static member Log1p (DummyType, x) = VectorExpr.Log1p(x)
        static member Log1p (DummyType, x) = MatrixExpr.Log1p(x)

        static member Erf (DummyType, x : float) = 
            let mutable res = x
            MklFunctions.D_Erf_Array(1L, &&res, &&res)
            res

        static member Erf (DummyType, x) = Vector.Erf(x)
        static member Erf (DummyType, x) = Matrix.Erf(x)
        static member Erf (DummyType, x) = VectorExpr.Erf(x)
        static member Erf (DummyType, x) = MatrixExpr.Erf(x)

        static member Erfc (DummyType, x : float) = 
            let mutable res = x
            MklFunctions.D_Erfc_Array(1L, &&res, &&res)
            res

        static member Erfc (DummyType, x) = Vector.Erfc(x)
        static member Erfc (DummyType, x) = Matrix.Erfc(x)
        static member Erfc (DummyType, x) = VectorExpr.Erfc(x)
        static member Erfc (DummyType, x) = MatrixExpr.Erfc(x)

        static member Erfinv (DummyType, x : float) = 
            let mutable res = x
            MklFunctions.D_Erfinv_Array(1L, &&res, &&res)
            res

        static member Erfinv (DummyType, x) = Vector.Erfinv(x)
        static member Erfinv (DummyType, x) = Matrix.Erfinv(x)
        static member Erfinv (DummyType, x) = VectorExpr.Erfinv(x)
        static member Erfinv (DummyType, x) = MatrixExpr.Erfinv(x)

        static member Erfcinv (DummyType, x : float) = 
            let mutable res = x
            MklFunctions.D_Erfcinv_Array(1L, &&res, &&res)
            res

        static member Erfcinv (DummyType, x) = Vector.Erfcinv(x)
        static member Erfcinv (DummyType, x) = Matrix.Erfcinv(x)
        static member Erfcinv (DummyType, x) = VectorExpr.Erfcinv(x)
        static member Erfcinv (DummyType, x) = MatrixExpr.Erfcinv(x)

        static member Normcdf (DummyType, x : float) = 
            let mutable res = x
            MklFunctions.D_CdfNorm_Array(1L, &&res, &&res)
            res

        static member Normcdf (DummyType, x) = Vector.Normcdf(x)
        static member Normcdf (DummyType, x) = Matrix.Normcdf(x)
        static member Normcdf (DummyType, x) = VectorExpr.Normcdf(x)
        static member Normcdf (DummyType, x) = MatrixExpr.Normcdf(x)

        static member Norminv (DummyType, x : float) = 
            let mutable res = x
            MklFunctions.D_CdfNormInv_Array(1L, &&res, &&res)
            res

        static member Norminv (DummyType, x) = Vector.Norminv(x)
        static member Norminv (DummyType, x) = Matrix.Norminv(x)
        static member Norminv (DummyType, x) = VectorExpr.Norminv(x)
        static member Norminv (DummyType, x) = MatrixExpr.Norminv(x)

        static member Axpby (DummyType, a, x : Vector, b, y : Vector) = Vector.Axpby(a, x, b, y)
        static member Axpby (DummyType, a, x : Matrix, b, y : Matrix) : Matrix = !!Vector.Axpby(a, x.ColMajorDataVector, b, y.ColMajorDataVector)

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
            BoolVectorExpr.IfFunction(boolExpr, trueVector.AsExpr, falseVector.AsExpr)

        static member IIf (DummyType, boolExpr : BoolVectorExpr, trueValue : bool, falseValue : bool) =
            BoolVectorExpr.IfFunction(boolExpr, BoolVectorExpr.Scalar(trueValue), BoolVectorExpr.Scalar(falseValue))


        static member IIf (DummyType, boolExpr : BoolVectorExpr, trueExpr : VectorExpr, falseExpr : VectorExpr) =
            VectorExpr.IfFunction(boolExpr, trueExpr, falseExpr)

        static member IIf (DummyType, boolExpr : BoolVectorExpr, trueVector : Vector, falseVector : Vector) =
            VectorExpr.IfFunction(boolExpr, trueVector.AsExpr, falseVector.AsExpr)

        static member IIf (DummyType, boolExpr : BoolVectorExpr, trueValue : float, falseValue : float) =
            VectorExpr.IfFunction(boolExpr, VectorExpr.Scalar(trueValue), VectorExpr.Scalar(falseValue))



        static member IIf (DummyType, boolExpr : BoolMatrixExpr, trueExpr : BoolMatrixExpr, falseExpr : BoolMatrixExpr) =
            BoolMatrixExpr.IfFunction(boolExpr, trueExpr, falseExpr)

        static member IIf (DummyType, boolExpr : BoolMatrixExpr, trueMatrix : BoolMatrix, falseMatrix : BoolMatrix) =
            BoolMatrixExpr.IfFunction(boolExpr, trueMatrix.AsExpr, falseMatrix.AsExpr)

        static member IIf (DummyType, boolExpr : BoolMatrixExpr, trueValue : bool, falseValue : bool) =
            BoolMatrixExpr.IfFunction(boolExpr, BoolMatrixExpr.Scalar(trueValue), BoolMatrixExpr.Scalar(falseValue))


        static member IIf (DummyType, boolExpr : BoolMatrixExpr, trueExpr : MatrixExpr, falseExpr : MatrixExpr) =
            MatrixExpr.IfFunction(boolExpr, trueExpr, falseExpr)

        static member IIf (DummyType, boolExpr : BoolMatrixExpr, trueMatrix : Matrix, falseMatrix : Matrix) =
            MatrixExpr.IfFunction(boolExpr, trueMatrix.AsExpr, falseMatrix.AsExpr)

        static member IIf (DummyType, boolExpr : BoolMatrixExpr, trueValue : float, falseValue : float) =
            MatrixExpr.IfFunction(boolExpr, MatrixExpr.Scalar(trueValue), MatrixExpr.Scalar(falseValue))


        static member Chol (DummyType, x) = Matrix.Chol(x)

        static member CholInv (DummyType, x) = Matrix.CholInv(x)

        static member CholSolve (DummyType, a, b : Matrix) = Matrix.CholSolve(a, b)
        static member CholSolve (DummyType, a, b : Vector) = Matrix.CholSolve(a, b)

        static member Lu (DummyType, x) = Matrix.Lu(x)

        static member LuInv (DummyType, x) = Matrix.LuInv(x)

        static member LuSolve (DummyType, a, b : Matrix) = Matrix.LuSolve(a, b)
        static member LuSolve (DummyType, a, b : Vector) = Matrix.LuSolve(a, b)

        static member Qr (DummyType, x) = Matrix.Qr(x)

        static member QrSolveFull (DummyType, a, b : Matrix) = Matrix.QrSolveFull(a, b)
        static member QrSolveFull (DummyType, a, b : Vector) = Matrix.QrSolveFull(a, b)

        static member QrSolve (DummyType, a, b : Matrix, tol) = Matrix.QrSolve(a, b, tol)
        static member QrSolve (DummyType, a, b : Vector, tol) = Matrix.QrSolve(a, b, tol)

        static member SvdSolve (DummyType, a, b : Matrix, tol) = Matrix.SvdSolve(a, b, tol)
        static member SvdSolve (DummyType, a, b : Vector, tol) = Matrix.SvdSolve(a, b, tol)

        static member SvdValues (DummyType, x) = Matrix.SvdValues(x)

        static member Svd (DummyType, x) = Matrix.Svd(x)

        static member Eig (DummyType, x) = Matrix.Eig(x)

        static member Min (DummyType, x) = Vector.Min(x)
        static member Min (DummyType, x) = Matrix.Min(x)

        static member Max (DummyType, x) = Vector.Max(x)
        static member Max (DummyType, x) = Matrix.Max(x)

        static member Sum (DummyType, x) = Vector.Sum(x)
        static member Sum (DummyType, x) = Matrix.Sum(x)

        static member Prod (DummyType, x) = Vector.Prod(x)
        static member Prod (DummyType, x) = Matrix.Prod(x)

        static member Any (DummyType, x) = BoolVector.Any(x)
        static member Any (DummyType, x) = BoolMatrix.Any(x)

        static member All (DummyType, x) = BoolVector.All(x)
        static member All (DummyType, x) = BoolMatrix.All(x)

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


        static member Max (DummyType, x : BoolVector, y : BoolVector) = BoolVector.Max(x, y)

        static member Max (DummyType, x : BoolVector, y : bool) = BoolVector.Max(x, y)

        static member Max (DummyType, x : bool, y : BoolVector) = BoolVector.Max(x, y)


        static member Max (DummyType, x : BoolVectorExpr, y : BoolVectorExpr) = BoolVectorExpr.Max(x, y)

        static member Max (DummyType, x : BoolVectorExpr, y : BoolVector) = BoolVectorExpr.Max(x, y)

        static member Max (DummyType, x : BoolVector, y : BoolVectorExpr) = BoolVectorExpr.Max(x, y)

        static member Max (DummyType, x : BoolVectorExpr, y : bool) = BoolVectorExpr.Max(x, y)

        static member Max (DummyType, x : bool, y : BoolVectorExpr) = BoolVectorExpr.Max(x, y)


        static member Max (DummyType, x : Vector, y : Vector) = Vector.Max(x, y)

        static member Max (DummyType, x : Vector, y : float) = Vector.Max(x, y)

        static member Max (DummyType, x : float, y : Vector) = Vector.Max(x, y)



        static member Max (DummyType, x : VectorExpr, y : VectorExpr) = VectorExpr.Max(x, y)

        static member Max (DummyType, x : VectorExpr, y : Vector) = VectorExpr.Max(x, y)

        static member Max (DummyType, x : Vector, y : VectorExpr) = VectorExpr.Max(x, y)

        static member Max (DummyType, x : VectorExpr, y : float) = VectorExpr.Max(x, y)

        static member Max (DummyType, x : float, y : VectorExpr) = VectorExpr.Max(x, y)


        static member Max (DummyType, x : BoolMatrix, y : BoolMatrix) = BoolMatrix.Max(x, y)

        static member Max (DummyType, x : BoolMatrix, y : bool) = BoolMatrix.Max(x, y)

        static member Max (DummyType, x : bool, y : BoolMatrix) = BoolMatrix.Max(x, y)


        static member Max (DummyType, x : BoolMatrixExpr, y : BoolMatrixExpr) = BoolMatrixExpr.Max(x, y)

        static member Max (DummyType, x : BoolMatrixExpr, y : BoolMatrix) = BoolMatrixExpr.Max(x, y)

        static member Max (DummyType, x : BoolMatrix, y : BoolMatrixExpr) = BoolMatrixExpr.Max(x, y)

        static member Max (DummyType, x : BoolMatrixExpr, y : bool) = BoolMatrixExpr.Max(x, y)

        static member Max (DummyType, x : bool, y : BoolMatrixExpr) = BoolMatrixExpr.Max(x, y)


        static member Max (DummyType, x : Matrix, y : Matrix) = Matrix.Max(x, y)

        static member Max (DummyType, x : Matrix, y : float) = Matrix.Max(x, y)

        static member Max (DummyType, x : float, y : Matrix) = Matrix.Max(x, y)



        static member Max (DummyType, x : MatrixExpr, y : MatrixExpr) = MatrixExpr.Max(x, y)

        static member Max (DummyType, x : MatrixExpr, y : Matrix) = MatrixExpr.Max(x, y)

        static member Max (DummyType, x : Matrix, y : MatrixExpr) = MatrixExpr.Max(x, y)

        static member Max (DummyType, x : MatrixExpr, y : float) = MatrixExpr.Max(x, y)

        static member Max (DummyType, x : float, y : MatrixExpr) = MatrixExpr.Max(x, y)


        static member Max (DummyType, x : CovariateExpr, y : CovariateExpr) = CovariateExpr.Max(x, y)

        static member Max (DummyType, x : CovariateExpr, y : float) = CovariateExpr.Max(x, y)

        static member Max (DummyType, x : float, y : CovariateExpr) = CovariateExpr.Max(x, y)


        static member Max (DummyType, a : float, b : float) = max a b

        static member Max (DummyType, a : float32, b : float32) = max a b

        static member Max (DummyType, a : int, b : int) = max a b

        static member Max (DummyType, a : int64, b : int64) = max a b

        static member Max (DummyType, a : bool, b : bool) = max a b







        static member Min (DummyType, x : BoolVector, y : BoolVector) = BoolVector.Min(x, y)

        static member Min (DummyType, x : BoolVector, y : bool) = BoolVector.Min(x, y)

        static member Min (DummyType, x : bool, y : BoolVector) = BoolVector.Min(x, y)


        static member Min (DummyType, x : BoolVectorExpr, y : BoolVectorExpr) = BoolVectorExpr.Min(x, y)

        static member Min (DummyType, x : BoolVectorExpr, y : BoolVector) = BoolVectorExpr.Min(x, y)

        static member Min (DummyType, x : BoolVector, y : BoolVectorExpr) = BoolVectorExpr.Min(x, y)

        static member Min (DummyType, x : BoolVectorExpr, y : bool) = BoolVectorExpr.Min(x, y)

        static member Min (DummyType, x : bool, y : BoolVectorExpr) = BoolVectorExpr.Min(x, y)


        static member Min (DummyType, x : Vector, y : Vector) = Vector.Min(x, y)

        static member Min (DummyType, x : Vector, y : float) = Vector.Min(x, y)

        static member Min (DummyType, x : float, y : Vector) = Vector.Min(x, y)



        static member Min (DummyType, x : VectorExpr, y : VectorExpr) = VectorExpr.Min(x, y)

        static member Min (DummyType, x : VectorExpr, y : Vector) = VectorExpr.Min(x, y)

        static member Min (DummyType, x : Vector, y : VectorExpr) = VectorExpr.Min(x, y)

        static member Min (DummyType, x : VectorExpr, y : float) = VectorExpr.Min(x, y)

        static member Min (DummyType, x : float, y : VectorExpr) = VectorExpr.Min(x, y)


        static member Min (DummyType, x : BoolMatrix, y : BoolMatrix) = BoolMatrix.Min(x, y)

        static member Min (DummyType, x : BoolMatrix, y : bool) = BoolMatrix.Min(x, y)

        static member Min (DummyType, x : bool, y : BoolMatrix) = BoolMatrix.Min(x, y)


        static member Min (DummyType, x : BoolMatrixExpr, y : BoolMatrixExpr) = BoolMatrixExpr.Min(x, y)

        static member Min (DummyType, x : BoolMatrixExpr, y : BoolMatrix) = BoolMatrixExpr.Min(x, y)

        static member Min (DummyType, x : BoolMatrix, y : BoolMatrixExpr) = BoolMatrixExpr.Min(x, y)

        static member Min (DummyType, x : BoolMatrixExpr, y : bool) = BoolMatrixExpr.Min(x, y)

        static member Min (DummyType, x : bool, y : BoolMatrixExpr) = BoolMatrixExpr.Min(x, y)


        static member Min (DummyType, x : Matrix, y : Matrix) = Matrix.Min(x, y)

        static member Min (DummyType, x : Matrix, y : float) = Matrix.Min(x, y)

        static member Min (DummyType, x : float, y : Matrix) = Matrix.Min(x, y)



        static member Min (DummyType, x : MatrixExpr, y : MatrixExpr) = MatrixExpr.Min(x, y)

        static member Min (DummyType, x : MatrixExpr, y : Matrix) = MatrixExpr.Min(x, y)

        static member Min (DummyType, x : Matrix, y : MatrixExpr) = MatrixExpr.Min(x, y)

        static member Min (DummyType, x : MatrixExpr, y : float) = MatrixExpr.Min(x, y)

        static member Min (DummyType, x : float, y : MatrixExpr) = MatrixExpr.Min(x, y)


        static member Min (DummyType, x : CovariateExpr, y : CovariateExpr) = CovariateExpr.Min(x, y)

        static member Min (DummyType, x : CovariateExpr, y : float) = CovariateExpr.Min(x, y)

        static member Min (DummyType, x : float, y : CovariateExpr) = CovariateExpr.Min(x, y)


        static member Min (DummyType, x : Covariate, y : Covariate) = Covariate.Min(x, y)

        static member Min (DummyType, x : Covariate, y : float) = Covariate.Min(x, y)

        static member Min (DummyType, x : float, y : Covariate) = Covariate.Min(x, y)


        static member Min (DummyType, a : float, b : float) = min a b

        static member Min (DummyType, a : float32, b : float32) = min a b

        static member Min (DummyType, a : int, b : int) = min a b

        static member Min (DummyType, a : int64, b : int64) = min a b

        static member Min (DummyType, a : bool, b : bool) = min a b



        static member StatFormula (DummyType, x : CovariateExpr, y : Predictor list) = x, y

        static member StatFormula (DummyType, x : Covariate, y : Predictor list) = x.AsExpr, y

        static member StatFormula (DummyType, x : CovariateExpr, y : Predictor) = x, [y]

        static member StatFormula (DummyType, x : Covariate, y : Predictor) = x.AsExpr, [y]

        static member StatFormula (DummyType, x : CovariateExpr, y : Factor) = x, [(!!y:Predictor)]

        static member StatFormula (DummyType, x : Covariate, y : Factor) = x.AsExpr, [(!!y:Predictor)]

        static member StatFormula (DummyType, x : CovariateExpr, y : FactorExpr) = x, [(!!y:Predictor)]

        static member StatFormula (DummyType, x : Covariate, y : FactorExpr) = x.AsExpr, [(!!y:Predictor)]

        static member StatFormula (DummyType, x : CovariateExpr, y : Covariate) = x, [(!!y:Predictor)]

        static member StatFormula (DummyType, x : Covariate, y : Covariate) = x.AsExpr, [(!!y:Predictor)]

        static member StatFormula (DummyType, x : CovariateExpr, y : CovariateExpr) = x, [(!!y:Predictor)]

        static member StatFormula (DummyType, x : Covariate, y : CovariateExpr) = x.AsExpr, [(!!y:Predictor)]

        static member StatFormula (DummyType, x : CovariateExpr, y : StatVariable) = x, [(!!y:Predictor)]

        static member StatFormula (DummyType, x : Covariate, y : StatVariable) = x.AsExpr, [(!!y:Predictor)]

        static member StatFormula (DummyType, x : CovariateExpr, y : CategoricalPredictor) = x, [(!!y:Predictor)]

        static member StatFormula (DummyType, x : Covariate, y : CategoricalPredictor) = x.AsExpr, [(!!y:Predictor)]



        static member ConvertStatVar (DummyType, x : Factor, f : string -> string) = RenameLevels(x.AsExpr, f)

        static member ConvertStatVar (DummyType, x : FactorExpr, f : string -> string) = RenameLevels(x, f)

        static member ConvertStatVar (DummyType, x : Factor, f : string) = FactorExpr.Rename(x.AsExpr, f)

        static member ConvertStatVar (DummyType, x : FactorExpr, f : string) = FactorExpr.Rename(x, f)

        static member ConvertStatVar (DummyType, x : Covariate, f : string) = CovariateExpr.Rename(x.AsExpr, f)

        static member ConvertStatVar (DummyType, x : CovariateExpr, f : string) = CovariateExpr.Rename(x, f)

        static member ConvertStatVar (DummyType, x : Factor, f : string list) = MergeLevels(x.AsExpr, f)

        static member ConvertStatVar (DummyType, x : FactorExpr, f : string list) = MergeLevels(x, f)

        static member ConvertStatVar (DummyType, x : Factor, f : string[]) = MergeLevels(x.AsExpr, f)

        static member ConvertStatVar (DummyType, x : FactorExpr, f : string[]) = MergeLevels(x, f)

        static member ConvertStatVar (DummyType, x : Factor, f) = Permute(x.AsExpr, f)

        static member ConvertStatVar (DummyType, x : FactorExpr, f) = Permute(x, f)

        static member ConvertStatVar (DummyType, x : Covariate, f : float[]) = Cut(x.AsExpr, f)

        static member ConvertStatVar (DummyType, x : CovariateExpr, f : float[]) = Cut(x, f)

        static member ConvertStatVar (DummyType, x : Covariate, f : int[]) = Int(x.AsExpr, f)

        static member ConvertStatVar (DummyType, x : CovariateExpr, f : int[]) = Int(x, f)

        static member ConvertStatVar (DummyType, x : Factor, f) = ParseFactor(x.AsExpr, f)

        static member ConvertStatVar (DummyType, x : FactorExpr, f) = ParseFactor(x, f)

        static member ConvertStatVar (DummyType, x : Factor, f) = BinomialFactor(x.AsExpr, f)

        static member ConvertStatVar (DummyType, x : FactorExpr, f) = BinomialFactor(x, f)






