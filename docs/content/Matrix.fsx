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

They can be created by defining the number of rows and columns using integers or longs and the value to be used for each element, or if you just provide a value you will get a matrix containing one cell: 
*)
#r "FCore.dll"

open FCore

let m1 = new Matrix(5, 3, 0.1)
let m2 = new Matrix(3L, 2L, 0.3)
let m3 = new Matrix(0.5)
(** This gives a value for m2 of: *)
(*** include-value: m2 ***)
(**
You can also create a matrix by providing a sequence containing all the values and also specifiy the number of rows and columns:
*)
let m4 = new Matrix(2, 2, [ 0.1; 0.2; 0.3; 0.4 ])
(** This gives a value for m4 of: *)
(*** include-value: m4 ***)
(**
You can also create a matrix by just providing a sequence of sequences of floats or a 2D array of floats or by specifiying the number of rows and columns and an initializer function:
*)
let m5 = new Matrix([[ 0.1; 0.2 ];[ 0.3; 0.4 ]])
let a1 = Array2D.create 3 2 0.5
let m6 = new Matrix(a1)
let m7 = new Matrix(3, 1, fun r c -> if r = 1 then 0.1 else 0.2)
(** This gives a value for m7 of: *)
(*** include-value: m7 ***)
(**
You can also create a matrix from a Vector. This creates a matrix with just 1 colum:
*)
let v1 = new Vector([| 0.1; 0.2 |])
let m8 = new Matrix(v1)
(**
You can also create a matrix by converting a single float, a sequence of sequences of floats or a 2D array of floats, using !! operator from the ExplicitConversion module.
*)
open FCore.ExplicitConversion
let m9 : Matrix = !!0.1
let m10 : Matrix = !!([[ 0.1; 0.2 ];[ 0.3; 0.4 ]]|>List.toSeq)
let m11 : Matrix = !!(Array2D.create 2 2 0.4)
(**
Arithmetic Operators
--------------------
There are a full range of operators provided for working with Matrices. These include:

* **+** - creates a new matrix, which is the result of applying + to each element of the two matrices  
* **-** - creates a new matrix, which is the result of applying - to each element of the two matrices  
* ** .* ** - creates a new matrix, which is the result of applying * to each element of the two matrices  
* **./** - creates a new matrix, which is the result of applying / to each element of the two matrices  
* **.^** - creates a new matrix, which is the result of applying ^ to each element of the two matrices 
* **-** - creates a new matrix, which is the result of applying - to each element of a matrix  

Here are some examples:
*)
let m12 = new Matrix([[ 0.1; 0.2 ]; [ 0.3; 0.4 ]])
let m13 = new Matrix([[ 0.1; 0.2 ]; [ 0.4; 0.5 ]])
m12 + m13 //0.2,0.4,0.7,0.9
m12 - m13 //0,0,-0.1,-0.1
m12 .* m13 //0.01,0.04,0.12,0.2
m12 ./ m13 //1,1,0.75,0.8
m12 .^ m13 //0.794,0.725,0.618,0.632
- m13 //-0.1,-0.2,-0.4,-0.5
(**
Conditional Operators
---------------------
There are a full range of operators provided for working with Matrices. These include:

* **==** - tests whether two matrices are equal 
* **!=** - tests whether two matrices are different 
* **.<** - creates a new matrix of bools, which is the result of applying < to each element of the two matrices
* **.<=** - creates a new matrix of bools, which is the result of applying <= to each element of the two matrices
* **.>** - creates a new matrix of bools, which is the result of applying > to each element of the two matrices
* **.>=** - creates a new matrix of bools, which is the result of applying >= to each element of the two matrices
* **.<>** - creates a new matrix of bools, which is the result of applying <> to each element of the two matrices

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
You can also access a range within a matrix using slicing syntax: x.[r..s,c..d]. For example:
*)
let m15 = m12.[1..1,0..1]
(**
produces a value of:
*)
(*** include-value: m15 ***)
