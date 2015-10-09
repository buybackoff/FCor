(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FCore"
(**
Linear Algebra
==============
FCore supports 5 matrix factorizations and related solvers:

- `Cholesky`
- `LU`
- `QR`
- `Singular Value`
- `Eigenvalue`

You can also calculate matrix-vector products and transpose a matrix in/out of place.
*)
#r "FCore.dll"

open FCore
open System
open FCore.ExplicitConversion
open FCore.Math
open FCore.LinearAlgebra
(**
Matrix products and transposition
--------------------------------
Use the operators `*` and `^*` for calculating matrix-matrix, matrix-vector and vector-vector products:
*)
let m1 : Matrix = !![[1.0; 2.0]
                     [3.0; 4.0]] 
let v1 : Vector = !![5.0; 6.0]
let m2 = m1 * m1
(** This gives a value for m2 of: *)
(*** include-value: m2 ***)
let m3 = m1 * v1
let m4 = m1 ^* m1 // transpose(m1) * m1 without creating the transposed matrix
let prod = v1 * v1 
(**
m3 produces a value of:
*)
(*** include-value: m3 ***)
(**
m4 produces a value of:
*)
(*** include-value: m4 ***)
(**
prod produces a value of:
*)
(*** include-value: prod ***)
(**
You can transpose a matrix in place or create a new matrix:
*)

let m5 = transpose(m1)
(**
m5 produces a value of:
*)
(*** include-value: m5 ***)
m1.Transpose()
(**
m1 produces a value of:
*)
(*** include-value: m1 ***)
(**
Cholesky factorization
--------------------------------
Cholesky requires a square positive definite matrix. The factorization returns the upper tri matrix.
You can also calculate the inverse matrix and solve `A*x=b`, where `b` can be a vector or a matrix:
*)
let m6 : Matrix = !![[2.0;0.7;0.7]
                     [0.7;2.0;0.7]
                     [0.7;0.7;2.0]]
let b1 : Vector = !![1.0;2.0;3.0]
let m7 = chol m6
(**
m7 produces a value of:
*)
(*** include-value: m7 ***)
let m8 = cholInv m6 // inverse
let v2 = cholSolve m6 b1
(**
v2 produces a value of:
*)
(*** include-value: v2 ***)
(**
LU factorization
--------------------------------
LU factorizes matrix A into lower and upper tri matrix: `A = P*L*U` where `P` is row permutation matrix.
You can also calculate the inverse matrix and solve `A*x=b`, where `b` can be a vector or a matrix:
*)
let l, u, p = lu m6
(**
l produces a value of:
*)
(*** include-value: l ***)
(**
u produces a value of:
*)
(*** include-value: u ***)
(**
p produces a value of:
*)
(*** include-value: p ***)
let m10 = luInv m6
let v3 = luSolve m6 b1
(**
v3 produces a value of:
*)
(*** include-value: v3 ***)
(**
QR factorization
--------------------------------
QR factorizes matrix A [mxn] into orthogonal Q and upper triangular R: `A = Q*R`.
If m > n then economy factorization is performed so that Q has n columns and R has n rows.
You can use it to solve linear least squares problems as well as compute effective rank for given numerical tolerance:
*)
let m11 = new Matrix(4, 3, [2.0..13.0])
(**
m11 produces a value of:
*)
(*** include-value: m11 ***)
let q, r = qr m11
(**
q produces a value of:
*)
(*** include-value: q ***)
(**
r produces a value of:
*)
(*** include-value: r ***)
let m12 = new Matrix(4, 2, [1.0..8.0])
(**
m12 produces a value of:
*)
(*** include-value: m12 ***)
let m13 = qrSolveFull m11 m12 // assumes m11 has full rank
let m14, rank = qrSolve m11 m12 1e-20 // numerical tolerance is used to compute effective rank
(**
m14 produces a value of:
*)
(*** include-value: m14 ***)
(**
Singular value factorization
--------------------------------
Singular value factorization decomposes matrix A [mxn] into diagonal S and unitary U and V: `A = U * S * V.T`.
If m > n then economy factorization is performed so that U has n columns.
You can use it to solve linear least squares problems as well as compute singular values:
*)
let m15 : Matrix = !![[1.0;3.0]
                      [2.0;4.0]
                      [3.0;5.0]]
(**
m15 produces a value of:
*)
(*** include-value: m15 ***)
let U, S, Vt = svd m6 //S is vector of singular values, vt = transpose(V)
(**
U produces a value of:
*)
(*** include-value: U ***)
(**
S produces a value of:
*)
(*** include-value: S ***)
(**
Vt produces a value of:
*)
(*** include-value: Vt ***)
let m16, rnk = svdSolve m6 m15 1e-20
(**
m16 produces a value of:
*)
(*** include-value: m16 ***)
let singVals = svdValues m6 // singular values
(**
singVals produces a value of:
*)
(*** include-value: singVals ***)
(**
Eigenvalue factorization
--------------------------------
Eigenvalue factorization decomposes matrix A [nxn] into eigenvectors V and eigenvalues D so that `A * V = V * D`
*)
let m17 : Matrix = !![[1.0;3.0;5.0]
                      [3.0;2.0;4.0]
                      [5.0;4.0;1.0]]
(**
m17 produces a value of:
*)
(*** include-value: m17 ***)
let m18, v4 = eig m17
(**
m18 produces a value of:
*)
(*** include-value: m18 ***)
(**
v4 produces a value of:
*)
(*** include-value: v4 ***)
