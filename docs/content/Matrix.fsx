(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FCore"
(**
Matrix
============
FCore provides matrices that hold floating point values. The primary use of these is for linear algebra. 
Creating Matrices
--------------------
There are a number of options provided to create these matrices.

They can be created by defining the number of rows and columns using `int` or `int64` and the value to be used for each element, or if you just provide a value you will get a matrix containing one cell: 
*)
#r "FCore.dll"

open FCore
open System
open FCore.Math

let m1 = new Matrix(5, 3, 0.1)
let m2 = new Matrix(3L, 2L, 0.3) // create large matrices with int64 row and column count
let m3 = new Matrix(0.5)
(** This gives a value for m2 of: *)
(*** include-value: m2 ***)
(**
You can also create a matrix by providing a sequence containing all the values and also specifiy the number of rows and columns:
*)
let m4 = new Matrix(2, 2, [0.1; 0.2; 0.3; 0.4])
(** This gives a value for m4 of: *)
(*** include-value: m4 ***)
(**
You can also create a matrix by just providing a sequence of sequences of floats or a 2D array of floats or by specifiying the number of rows and columns and an initializer function:
*)
let m5 = new Matrix([[0.1; 0.2]
                     [0.3; 0.4]])
let a1 = Array2D.create 3 2 0.5
let m6 = new Matrix(a1)
let m7 = new Matrix(3, 3, fun row col -> if row = 1 then 0.1 else 0.2)
(** This gives a value for m7 of: *)
(*** include-value: m7 ***)
(**
You can also create a matrix from a Vector. This creates a matrix with just 1 colum:
*)
let v1 = new Vector([0.1; 0.2])
let m8 = new Matrix(v1) // 2x1, no data is copied so v1 and m8 share memory
(**
You can also create a matrix by converting a single float, a sequence of sequences of floats or a 2D array of floats, using `!!` operator from the ExplicitConversion module.
*)
open FCore.ExplicitConversion
let m9 : Matrix = !!0.1
let m10 : Matrix = !![[0.1; 0.2]
                      [0.3; 0.4]]
let m11 : Matrix = Array2D.create 2 2 0.4 |> (!!)
(**
Identity matrix has `1` on diagonal and `0` otherwise and can be created with `I` function: 
*)
let eye : Matrix = I 2 3 // I is overloaded and we need to help compiler with type annotation
let eyeL : Matrix = I 3L 2L // use int64 for large matrices
(** This gives a value for eye of: *)
(*** include-value: eye ***)
(**
Arithmetic Operators
--------------------
There are a full range of operators provided for working with Matrices. These include:

* `+` - creates a new matrix, which is the result of applying binary `+` elementwise 
* `-` - creates a new matrix, which is the result of applying binary `-` elementwise  
* `.*` - creates a new matrix, which is the result of applying binary `*` elementwise  
* `./` - creates a new matrix, which is the result of applying binary `/` elementwise  
* `.^` - creates a new matrix, which is the result of applying binary `**` (pow) elementwise  
* `-` - creates a new matrix, which is the result of applying unary `~-` elementwise  

`*` and `/` can be used as elementwise operators only if 1 of the arguments is `float`.
Here are some examples:
*)
let m12 = new Matrix([[0.1; 0.2]
                      [0.3; 0.4]])
let m13 = new Matrix([[0.1; 0.2]
                      [0.4; 0.5]])
m12 + m13 //0.2,0.4,0.7,0.9
m12 - m13 //0,0,-0.1,-0.1
m12 .* m13 //0.01,0.04,0.12,0.2
m9 .* m10 // note `*` would try to calculate matrix product here and raise exception because m9 is a scalar
0.1 * m10 // `.` is not required for float values
m10 / 0.1
m12 ./ m13 //1,1,0.75,0.8
m12 .^ m13 //0.794,0.725,0.618,0.632
-m13 //-0.1,-0.2,-0.4,-0.5
(**
Comparison Operators
---------------------
There are a full range of operators provided for working with Matrices. These include:

* `=` - tests whether two matrices are equal by value
* `<>` - tests whether two matrices are not equal by value
* `==` - tests whether two matrices or matrix and scalar are equal by value
* `!=` - tests whether two matrices or matrix and scalar are different by value
* `.<` - creates a new matrix of bools, which is the result of applying `<` elementwise
* `.<=` - creates a new matrix of bools, which is the result of applying `<=` elementwise
* `.>` - creates a new matrix of bools, which is the result of applying `>` elementwise
* `.>=` - creates a new matrix of bools, which is the result of applying `>=` elementwise
* `.=` - creates a new matrix of bools, which is the result of applying `=` elementwise
* `.<>` - creates a new matrix of bools, which is the result of applying `<>` elementwise

Here are some examples:
*)
m12 == m13 //false
m12 != m13 //true
m12 .< m13 //false,false,true,true
m12 .<= m13 //true,true,true,true
m12 .> m13 //false,false,false,false
m12 .>= m13 //true,true,false,false
m12 .<> m13 //false,false,true,true
(**
Accessing elements and slicing/indexing
---------------------------------------
In indexing and slicing you can use indices of type `int` or `int64`. The code examples below only show `int` version.
 
To get or set an individual element within the matrix you can just use the standard F# syntax: `x.[row, col]`. For example:
*)
let m14 = m12.[1,1]
m12.[0,0] <- 0.0
(**
You can also get or set a range within a matrix using slicing syntax: x.[fromRow..toRow, fromCol..toCol]. For example:
*)
let m15 = m12.[1..1,0..1]
m12.[1..1, 0..1] <- 1.1
(**
m15 produces a value of:
*)
(*** include-value: m15 ***)
(**
If `fromRow = toRow` or `fromCol = toCol` then the syntax is simpler and returns a bool vector:
*)
let v16 = m12.[1,0..1]
m12.[1,0..1] <- !![1.0; 0.0]
(**
You can also use a sequence of row and column indices to get or set part of a matrix:
*)
let m17 = m12.[ [0;1], [1] ] 
m12.[ [0;1], [1] ] <- new Matrix(2, 1, [1.0; 0.0])
(**
If row or column sequence has only 1 index then the syntax is simpler and returns a bool vector:
*)
let v18 = m12.[ [0;1], 1] 
m12.[ [0;1], 1] <- !![1.0; 0.0]
(**
It is also possible to index a bool matrix with another bool matrix (logical indexing), e.g.
*)
let v19 = m12.[m13 .<= 0.2] //returns m12 elements where m13 is less than or equal 0.2 in column major order
m12.[m13 .<= 0.2] <- new Vector(0.0) //set elements of m12 where m13 is less than or equal 0.2 to elements of given vector
(**
Data in bool matrix is stored in column major order so we can slice and index a matrix as if it was a vector. This is called linear indexing:
*)
let v20 = m12.[0..1] 
(**
See [Vector](Vector.html) for details about vector slicing and indexing.
*)
(**
Matrix views
------------------------------
For performance reasons you might want to get a slice of a matrix without copying the data. You can create a view on part of an existing matrix:
*)
let view = m12.View(1,2) // creates a linear slice 1..2 which shares memory with the input matrix
let colView = m12.ColView(1) // returns the 2nd column as a vector, no data is copied
(**
Matrix properties
--------------------
You can get the size of a matrix, get its data as a vector in column major order and check if it is disposed:
*)
let rowCount = m12.RowCount
let colCount = m12.ColCount
let rowCountL = m12.LongRowCount // int64
let colCountL = m12.LongColCount
let dataVector = m12.ColMajorDataVector // no data is copied
let isDisposed = m12.IsDisposed
(**
Matrix functions
--------------------
You can convert Matrix to .NET 2D array:
*)
let arr = m12.ToArray2D()
(**
Create a matrix copy:
*)
let m21 = Matrix.Copy(m10)
(**
Use static methods `Min` and `Max` to manipulate matrices elementwise:
*)
let m22 = Matrix.Min(m12, m13)
let m23 = Matrix.Max(m12, m13)
(**
See [Linear Algebra](LinearAlgebra.html) and [Vector and Matrix Functions](VectorAndMatrixFunctions.html) for more Matrix functions
*)
(**
Matrix disposing
------------------------------
Matrix implements `IDisposable` so that you can deallocate unmanaged memory deterministically:
*)
m12.Dispose()
(**
If `Dispose` is not called then the memory is deallocated in Finalizer.
*)



