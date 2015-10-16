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

They can be created by defining then length using an `int` or `int64` and the value to be used for each element, or if you just provide a value you will get a vector of length 1: 
*)
#r "FCore.dll"

open FCore
open System
open FCore.Math

let v1 = new Vector(5, 0.1)
let v2 = new Vector(3L, 0.2) // create large vectors with int64 length
let v3 = new Vector(0.3)
(** This gives a value for v2 of: *)
(*** include-value: v2 ***)
(**
You can also create a vector by just providing a sequence of floats or by specifiying a length and an initializer function:
*)
let v4 = new Vector([0.1; 0.2])

let v5 = new Vector(3, fun i -> 
                           if i = 1 then 0.1
                           else 0.2
                   )
(** This gives a value for v5 of: *)
(*** include-value: v5 ***)
(**
You can also create a vector from an array of floats. For performance reasons you can specify whether to copy the data or just hold a reference to the data.
*)
let v6 = new Vector([|0.1; 0.2|], copyData = true)
//v7 just holds a reference to the provided array
let v7 = new Vector([| 0.1; 0.2 |], copyData = false)

(**
You can also create a vector by converting a single float or a seq of floats using `!!` operator from the ExplicitConversion module.
*)
open FCore.ExplicitConversion
let v8 : Vector = !!0.1
let v9 : Vector = !![0.1; 0.2]
(**
A sequence of vectors can be concatenated:
*)
let v4_v5 = concat [v4;v5]
(** This gives a value for v4_v5 of: *)
(*** include-value: v4_v5 ***)
(**
Arithmetic Operators
--------------------
There are a full range of operators provided for working with Vectors. These include:

* `+` - creates a new vector, which is the result of applying binary `+` elementwise
* `-` - creates a new vector, which is the result of applying binary `-` elementwise  
* `.*` - creates a new vector, which is the result of applying binary `*` elementwise  
* `./` - creates a new vector, which is the result of applying binary `/` elementwise  
* `.^` - creates a new vector, which is the result of applying binary `**` (pow) elementwise  
* `-` - creates a new vector, which is the result of applying unary `~-` elementwise  

`*` and `/` can be used as elementwise operators only if 1 of the arguments is `float`.
Here are some examples:
*)
let v10 = new Vector([0.1; 0.2; 0.3])
let v11 = new Vector([0.1; 0.2; 0.4])
v10 + v11 //0.2,0.4,0.7
v10 + v8 // v8 is a scalar
v10 - v11 //0,0,-0.1
v10 .* v11 //0.01,0.04,0.12
v8 .* v10 // note `*` would try to calculate inner product here and raise exception because v8 is a scalar
0.1 * v10 // `.` is not required for float values
v10 / 0.1
v10 ./ v11 //1,1,0.75
v10 .^ v11 //0.794,0.725,0.618
v10 .^ 3
-v11 //-0.1,-0.2,-0.4
(**
Comparison Operators
---------------------
There are a full range of operators provided for working with Vectors. These include:

* `=` - tests whether two vectors are equal by value
* `<>` - tests whether two vectors are not equal by value
* `==` - tests whether two vectors or vector and scalar are equal by value
* `!=` - tests whether two vectors or vector and scalar are different by value
* `.<` - creates a new vector of bools, which is the result of applying `<` elementwise
* `.<=` - creates a new vector of bools, which is the result of applying `<=` elementwise
* `.>` - creates a new vector of bools, which is the result of applying `>` elementwise
* `.>=` - creates a new vector of bools, which is the result of applying `>=` elementwise
* `.=` - creates a new vector of bools, which is the result of applying `=` elementwise
* `.<>` - creates a new vector of bools, which is the result of applying `<>` elementwise

Here are some examples:
*)
v10 = v11 //false
v10 <> v11 //true
v10 == 2.2 //false
v10 != 2.2 //true
v10 .< v11 //false,false,true
v10 .<= v11 //true,true,true
v10 .> v11 //false,false,false
v10 .>= v11 //true,true,false
v10 .<> v11 //false,false,true
(**
Accessing elements and slicing/indexing
------------------------------
In indexing and slicing you can use indices of type `int` or `int64`. The code examples below only show `int` version.
 
To get or set an individual element within the vector you can just use the standard F# syntax: `x.[i]`. For example:
*)
let a = v10.[2]
v10.[0] <- 1.1
(**
You can also get and set a range within a vector using slicing syntax: `x.[i..j]`. For example:
*)
let slice = v10.[1..2] 
(**
produces a value of:
*)
(*** include-value: slice ***)
v10.SetSlice(Some(1), Some(2), 3.3) 
v10.SetSlice(Some(1), Some(2), v11.[1..2]) 
(**
You can also use a sequence of indices to get or set part of a vector:
*)
let v12 = v10.[ [0;2] ] 
v10.[ [0;2] ] <- v11.[1..2]
(**
It is also possible to index a vector with a bool vector (logical indexing), e.g.
*)
let v13 = v10.[v11 .<= 0.2] //returns v10 elements where v11 is less than or equal 0.2
v10.[v11 .<= 0.2] <- new Vector(3.3) //set elements of v10 where v11 is less than or equal 0.2 to elements of given vector
(**
Vector views
------------------------------
For performance reasons you might want to get a slice of a vector without copying the data. You can create a view on part of an existing vector:
*)
let view = v10.View(1,2) // creates a slice 1..2 which shares memory with the input vector
(**
Vector properties
--------------------
You can get the length of a vector and check if it is a view and check if it is disposed:
*)
let len = v10.Length
let lenL = v10.LongLength // as int64
let isView = view.IsView
let isDisposed = v10.IsDisposed
(**
Vector functions
--------------------
You can convert Vector to .NET array:
*)
let arr = v10.ToArray()
(**
Create a vector copy:
*)
let v14 = Vector.Copy(v10)
(**
Use static methods `Min` and `Max` to manipulate vectors elementwise:
*)
let v15 = Vector.Min(v10, v11)
let v16 = Vector.Max(v10, v11)
(**
See [Linear Algebra](LinearAlgebra.html) and [Vector and Matrix Functions](VectorAndMatrixFunctions.html) for more Vector functions
*)
(**
Vector disposing
------------------------------
Vector implements `IDisposable` so that you can deallocate unmanaged memory deterministically:
*)
v10.Dispose()
(**
If `Dispose` is not called then the memory is deallocated in Finalizer.
*)
