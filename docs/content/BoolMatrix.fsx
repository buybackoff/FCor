(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FCore"
(**
BoolMatrix
============
FCore provides specialised matices that hold boolean values. The primary use of these is for linear algebra. 
Creating a BoolMatrix
----------------------
There are a number of options provided to create these matrices.

They can be created by defining the number of rows and columns using integers or longs and the value to be used for each element, or if you just provide a value you will get a matrix containing one cell: 
*)
#r "FCore.dll"

open FCore

let m1 = new BoolMatrix(5, 3, true)
let m2 = new BoolMatrix(3L, 2L, false)
let m3 = new BoolMatrix(false)
(** This gives a value for m2 of: *)
(*** include-value: m2 ***)
(**
You can also create a matrix by providing a sequence containing all the values and also specifiy the number of rows and columns. :
*)
let m4 = new BoolMatrix(2, 2, [ false; true; false; false ])
(** This gives a value for m4 of: *)
(*** include-value: m4 ***)
(**
You can also create a matrix by just providing a sequence of sequences of bool values or a 2D array of bool values or by specifiying the number of rows and columns and an initializer function:
*)
let m5 = new BoolMatrix([[ true; true ];[ false; true ]])
let a1 = Array2D.create 3 2 false
let m6 = new BoolMatrix(a1)
let m7 = new BoolMatrix(3, 1, fun r c -> if r = 1 then false else true)
(** This gives a value for m7 of: *)
(*** include-value: m7 ***)
(**
You can also create a matrix from a BoolVector. This creates a matrix with just 1 colum:
*)
let b1 = new BoolVector([| false; true |])
let m8 = new BoolMatrix(b1)

(**
You can also create a vector by converting a single bool, a sequence of sequences of bool values or a 2D array of bool values, using !! operator from the ExplicitConversion module.
*)
open FCore.ExplicitConversion
let m9 : BoolMatrix = !!false
let m10 : BoolMatrix = !!([[ true; true ];[ false; true ]]|>List.toSeq)
let m11 : BoolMatrix = !!(Array2D.create 2 2 true)
(**
Operators
---------
There are a full range of operators provided for working with the BoolMatrix type. These include:

* **==** - tests whether two matrices are equal 
* **!=** - tests whether two matrices are different 
* **.&&** - creates a new matrix, which is the result of applying && to each element of the two matrices
* **.||** - creates a new matrix, which is the result of applying || to each element of the two matrices  

Here are some examples:
*)
let m12 = new BoolMatrix([[ true; true ];[ false; true ]])
let m13 = new BoolMatrix([[ false; true ];[ false; false ]])
m12 == m13 //false
m12 != m13 //true
m12 .&& m13 //false,true,false,false
m12 .|| m13 //true,true,false,true
(**
Accessing elements and slicing
------------------------------
To access an individual element within the matrix you can just use the standard F# syntax: x.[r,c]. For example:
*)
let m14 = m12.[1,1]
(**
produces a value of:
*)
(*** include-value: m14 ***)
(**
Tou can also access a range within a matrix using slicing syntax: x.[r..s,c..d]. For example:
*)
let m15 = m12.[1..1,0..1]
(**
produces a value of:
*)
(*** include-value: m15 ***)
