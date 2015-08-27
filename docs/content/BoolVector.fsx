(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FCore"
(**
BoolVector
============
FCore provides specialised vectors that hold boolean values. The primary use of these is for linear algebra. 
Creating BoolVectors
--------------------
There are a number of options provided to create these vectors.

They can be created by defining then length using an integer or long and the value to be used for each element, or if you just provide a value you will get a vector of length 1: 
*)
#r "FCore.dll"

open FCore

let v1 = new BoolVector(5, true)
let v2 = new BoolVector(3L, false)
let v3 = new BoolVector(false)
(** This gives a value for v2 of: *)
(*** include-value: v2 ***)
(**
You can also create a vector by just providing a sequence of bool values or by specifiying a length and an initializer function:
*)
let v4 = new BoolVector([ false; true ])

let v5 = new BoolVector(3, fun i -> 
                       if i = 1 then false
                       else true)
(** This gives a value for v5 of: *)
(*** include-value: v5 ***)
(**
You can also create a vector from an array of bools. For performance reasons you can specify whether to copy the data or just hold a reference to the data.
*)
let v6 = new BoolVector([| false; true |], true)
//v7 just holds a reference to the provided array
let v7 = new BoolVector([| false; true |], false)

(**
You can also create a vector by converting a single bool or an array of bools using !! operator from the ExplicitConversion module.
*)
open FCore.ExplicitConversion
let v8 : BoolVector = !!false
let v9 : BoolVector = !![| false; true |]
(**
Operators
---------
There are a full range of operators provided for working with BoolVectors. These include:

* **==** - tests whether two vectors are equal 
* **!=** - tests whether two vectors are different 
* **.&&** - creates a new vector, which is the result of applying && to each element of the two vectors  
* **.||** - creates a new vector, which is the result of applying || to each element of the two vectors  

Here are some examples:
*)
let v10 = new BoolVector([ false; true; false ])
let v11 = new BoolVector([ false; true; true ])
v10 == v11 //false
v10 != v11 //true
v10 .&& v11 //false,true,false
v10 .|| v11 //false,true,true
(**
Accessing elements and slicing
------------------------------
To access an individual element within the vector you can just use the standard F# syntax: x.[i]. For example:
*)
v10.[2]
(**
produces a value of:
*)
(*** include-value: v10.[2] ***)
(**
Tou can also access a range within a vector using slicing syntax: x.[i..j]. For example:
*)
v10.[1..2]
(**
produces a value of:
*)
(*** include-value: v10.[1..2] ***)
