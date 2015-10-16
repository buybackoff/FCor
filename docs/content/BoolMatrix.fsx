(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FCore"
(**
BoolMatrix
============
FCore provides specialised matrices that hold boolean values. Boolean matrices are usually created as a result of comparing elementwise float matrices but you can also create them directly. 
Creating BoolMatrices
----------------------
There are a number of options provided to create these matrices.

They can be created by defining the number of rows and columns using `int` or `int64` and the value to be used for each element, or if you just provide a value you will get a matrix containing one cell: 
*)
#r "FCore.dll"

open FCore
open System
open FCore.Math

let m1 = new BoolMatrix(5, 3, true)
let m2 = new BoolMatrix(3L, 2L, false) // create large matrices with int64 row and column count
let m3 = new BoolMatrix(false)
(** This gives a value for m2 of: *)
(*** include-value: m2 ***)
(**
You can also create a matrix by providing a sequence containing all the values in column major order and also specifiy the number of rows and columns:
*)
let m4 = new BoolMatrix(2, 2, [false; true; false; false])
(** This gives a value for m4 of: *)
(*** include-value: m4 ***)
(**
You can also create a matrix by just providing a sequence of sequences of bool values or a 2D array of bool values or by specifiying the number of rows and columns and an initializer function:
*)
let m5 = new BoolMatrix([[true; true]
                         [false; true]])
let a1 = Array2D.create 3 2 false
let m6 = new BoolMatrix(a1)
let m7 = new BoolMatrix(3, 2, fun row col -> row = 1 || col = 0)
(** This gives a value for m7 of: *)
(*** include-value: m7 ***)
(**
You can also create a matrix from a BoolVector. This creates a matrix with just 1 column:
*)
let b1 = new BoolVector([false; true])
let m8 = new BoolMatrix(b1) // 2x1, no data is copied so b1 and m8 share memory

(**
You can also create a matrix by converting a single bool, a sequence of sequences of bool values or a 2D array of bool values, using `!!` operator from the ExplicitConversion module.
*)
open FCore.ExplicitConversion
let m9 : BoolMatrix = !!false
let m10 : BoolMatrix = !![[true; true]
                          [false; true]]
let m11 : BoolMatrix = Array2D.create 2 2 true |> (!!)
(**
Identity matrix has `true` on diagonal and `false` otherwise and can be created with `I` function: 
*)
let eye : BoolMatrix = I 2 3 // I is overloaded and we need to help compiler with type annotation
let eyeL : BoolMatrix = I 3L 2L // use int64 for large matrices
(** This gives a value for eye of: *)
(*** include-value: eye ***)
(**
Operators
---------
There are a full range of operators provided for working with the BoolMatrix type. These include:

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
* `.&&` - creates a new matrix, which is the result of applying `&&` elementwise 
* `.||` - creates a new matrix, which is the result of applying `||` elementwise  

Elementwise operators start with a `.` and 1 of the arguments can be a `bool` value.

Here are some examples:
*)
let m12 = new BoolMatrix([[true; true]
                          [false; true]])
let m13 = new BoolMatrix([[false; true ]
                          [false; false]])
m12 == m13 //false
m12 != m13 //true
m12 .&& m13 //false,true,false,false
m12 .|| m13 //true,true,false,true
m12 .|| false
(**
Accessing elements and slicing/indexing
---------------------------------------
In indexing and slicing you can use indices of type `int` or `int64`. The code examples below only show `int` version.
 
To get or set an individual element within the matrix you can just use the standard F# syntax: `x.[row, col]`. For example:
*)
let m14 = m12.[1,1]
m12.[0,0] <- false
(**
You can also get or set a range within a matrix using slicing syntax: x.[fromRow..toRow, fromCol..toCol]. For example:
*)
let m15 = m12.[1..1,0..1]
m12.[1..1, 0..1] <- true
(**
m15 produces a value of:
*)
(*** include-value: m15 ***)
(**
If `fromRow = toRow` or `fromCol = toCol` then the syntax is simpler and returns a bool vector:
*)
let v16 = m12.[1,0..1]
m12.[1,0..1] <- !![true;false]
(**
You can also use a sequence of row and column indices to get or set part of a matrix:
*)
let m17 = m12.[ [0;1], [1] ] 
m12.[ [0;1], [1] ] <- new BoolMatrix(2, 1, [true;false])
(**
If row or column sequence has only 1 index then the syntax is simpler and returns a bool vector:
*)
let v18 = m12.[ [0;1], 1] 
m12.[ [0;1], 1] <- !![true;false]
(**
It is also possible to index a bool matrix with another bool matrix (logical indexing), e.g.
*)
let v19 = m12.[m13] //returns m12 elements where m13 is true in column major order
m12.[m13] <- new BoolVector(false) //set elements of m12 where m13 is true to elements of given vector
(**
Data in bool matrix is stored in column major order so we can slice and index a matrix as if it was a vector. This is called linear indexing:
*)
let v20 = m12.[0..1] 
(**
See [BoolVector](BoolVector.html) for details about vector slicing and indexing.
*)
(**
BoolMatrix views
------------------------------
For performance reasons you might want to get a slice of a matrix without copying the data. You can create a view on part of an existing matrix:
*)
let view = m12.View(1,2) // creates a linear slice 1..2 which shares memory with the input matrix
let colView = m12.ColView(1) // returns the 2nd column as a vector, no data is copied
(**
BoolMatrix properties
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
BoolMatrix functions
--------------------
You can convert BoolMatrix to .NET 2D array:
*)
let arr = m12.ToArray2D()
(**
Create a bool matrix copy:
*)
let m21 = BoolMatrix.Copy(m10)
(**
Use static methods `Min` `Max` and `Not` to manipulate matrices elementwise:
*)
let m22 = BoolMatrix.Min(m12, m13)
let m23 = BoolMatrix.Max(m12, m13)
let m24 = BoolMatrix.Not m12
(**
BoolMatrix disposing
------------------------------
BoolMatrix implements `IDisposable` so that you can deallocate unmanaged memory deterministically:
*)
m12.Dispose()
(**
If `Dispose` is not called then the memory is deallocated in Finalizer.
*)


