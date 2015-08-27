(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FCore"
(**
Vector
============
FCore provides vectors that hold floating point values. The primary use of these is for linear algebra. 
Creating Vectors
--------------------
There are a number of options provided to create these vectors.

They can be created by defining then length using an integer or long and the value to be used for each element, or if you just provide a value you will get a vector of length 1: 
*)
#r "FCore.dll"

open FCore

let v1 = new Vector(5, 0.1)
let v2 = new Vector(3L, 0.2)
let v3 = new Vector(0.3)
(** This gives a value for v2 of: *)
(*** include-value: v2 ***)
(**
You can also create a vector by just providing a sequence of floats or by specifiying a length and an initializer function:
*)
let v4 = new Vector([ 0.1; 0.2 ])

let v5 = new Vector(3, fun i -> 
                       if i = 1 then 0.1
                       else 0.2)
(** This gives a value for v5 of: *)
(*** include-value: v5 ***)
(**
You can also create a vector from an array of floats. For performance reasons you can specify whether to copy the data or just hold a reference to the data.
*)
let v6 = new Vector([| 0.1; 0.2 |], true)
//v7 just holds a reference to the provided array
let v7 = new Vector([| 0.1; 0.2 |], false)

(**
You can also create a vector by converting a single float or an array of floats using !! operator from the ExplicitConversion module.
*)
open FCore.ExplicitConversion
let v8 : Vector = !!0.1
let v9 : Vector = !![| 0.1; 0.2 |]
(**
Arithmetic Operators
--------------------
There are a full range of operators provided for working with Vectors. These include:

* **+** - creates a new vector, which is the result of applying + to each element of the two vectors  
* **-** - creates a new vector, which is the result of applying - to each element of the two vectors  
* ** .* ** - creates a new vector, which is the result of applying * to each element of the two vectors  
* **./** - creates a new vector, which is the result of applying / to each element of the two vectors  
* **.^** - creates a new vector, which is the result of applying ^ to each element of the two vectors  
* **-** - creates a new vector, which is the result of applying - to each element of a vector  

Here are some examples:
*)
let v10 = new Vector([ 0.1; 0.2; 0.3 ])
let v11 = new Vector([ 0.1; 0.2; 0.4 ])
v10 + v11 //0.2,0.4,0.7
v10 - v11 //0,0,-0.1
v10 .* v11 //0.01,0.04,0.12
v10 ./ v11 //1,1,0.75
v10 .^ v11 //0.794,0.725,0.618
- v11 //-0.1,-0.2,-0.4
(**
Conditional Operators
---------------------
There are a full range of operators provided for working with Vectors. These include:

* **==** - tests whether two vectors are equal 
* **!=** - tests whether two vectors are different 
* **.<** - creates a new vector of bools, which is the result of applying < to each element of the two vectors
* **.<=** - creates a new vector of bools, which is the result of applying <= to each element of the two vectors
* **.>** - creates a new vector of bools, which is the result of applying > to each element of the two vectors
* **.>=** - creates a new vector of bools, which is the result of applying >= to each element of the two vectors
* **.<>** - creates a new vector of bools, which is the result of applying <> to each element of the two vectors

Here are some examples:
*)
v10 == v11 //false
v10 != v11 //true
v10 .< v11 //false,false,true
v10 .<= v11 //true,true,true
v10 .> v11 //false,false,false
v10 .>= v11 //true,true,false
v10 .<> v11 //false,false,true
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
