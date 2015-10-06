(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FCore"
(**
BoolVector
============
FCore provides specialised vectors that hold boolean values. Boolean vectors are usually created as a result of comparing elementwise float vectors but you can also create them directly. 
Creating BoolVectors
--------------------

They can be created by defining the length using an `int` or `int64` and the value to be used for each element, or if you just provide a value you will get a vector of length 1: 
*)
#r "FCore.dll"

open System
open FCore

let v1 = new BoolVector(5, true)
let v2 = new BoolVector(3L, false) // create large vectors with int64 length
let v3 = new BoolVector(false)
(** This gives a value for v2 of: *)
(*** include-value: v2 ***)
(**
You can also create a vector by just providing a sequence of bool values or by specifiying a length and an initializer function:
*)
let v4 = new BoolVector([false; true])

let v5 = new BoolVector(3, fun i -> i <> 1)
(** This gives a value for v5 of: *)
(*** include-value: v5 ***)
(**
You can also create a vector from an array of bools. For performance reasons you can specify whether to copy the data or just hold a reference to the data.
*)
let v6 = new BoolVector([|false; true|], copyData = true)
//v7 just holds a reference to the provided array
let v7 = new BoolVector([|false; true|], copyData = false)

(**
You can also create a vector by converting a single bool or a seq of bools using `!!` operator from the ExplicitConversion module.
*)
open FCore.ExplicitConversion
let v8 : BoolVector = !!false
let v9 : BoolVector = !![false; true]
(**
Operators
---------
There are a full range of operators provided for working with BoolVectors. These include:

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
* `.&&` - creates a new vector, which is the result of applying `&&` elementwise 
* `.||` - creates a new vector, which is the result of applying `||` elementwise  

Here are some examples:
*)
let v10 = new BoolVector([false; true; false])
let v11 = new BoolVector([false; true; true])
v10 = v11 //false
v10 <> v11 //true
v10 == false // false
v10 != v11 //true
v10 .&& v11 //false,true,false
v10 .|| v11 //false,true,true
(**
Accessing elements and slicing/indexing
------------------------------
In indexing and slicing you can use indices of type `int` or `int64`. The code examples below only show `int` version.
 
To get or set an individual element within the vector you can just use the standard F# syntax: `x.[i]`. For example:
*)
let a = v10.[2]
v10.[0] <- false
(**
You can also get and set a range within a vector using slicing syntax: `x.[i..j]`. For example:
*)
let slice = v10.[1..2] 
(**
produces a value of:
*)
(*** include-value: slice ***)
v10.SetSlice(Some(1), Some(2), true) 
v10.SetSlice(Some(1), Some(2), v11.[1..2]) 
(**
You can also use a sequence of indices to get or set part of a vector:
*)
let v12 = v10.[ [0;2] ] 
v10.[ [0;2] ] <- v11.[1..2]
(**
It is also possible to index a bool vector with another bool vector (logical indexing), e.g.
*)
let v13 = v10.[v11] //returns v10 elements where v11 is true
v10.[v11] <- new BoolVector(false) //set elements of v10 where v11 is true to elements of given vector
(**
BoolVector views
------------------------------
For performance reasons you might want to get a slice of a vector without copying the data. You can create a view on part of an existing vector:
*)
let view = v10.View(1,2) // creates a slice 1..2 which shares memory with the input vector
(**
BoolVector properties
--------------------
You can get the length of a vector and check if it is a view and check if it is disposed:
*)
let len = v10.Length
let lenL = v10.LongLength // as int64
let isView = view.IsView
let isDisposed = v10.IsDisposed
(**
BoolVector functions
--------------------
You can convert BoolVector to .NET array:
*)
let arr = v10.ToArray()
(**
Create a bool vector copy:
*)
let v14 = BoolVector.Copy(v10)
(**
Use static methods `Min` `Max` and `Not` to manipulate vectors elementwise:
*)
let v15 = BoolVector.Min(v10, v11)
let v16 = BoolVector.Max(v10, v11)
let v17 = BoolVector.Not v10
(**
Concatenate a seq of bool vectors:
*)
let v18 = BoolVector.Concat([v10;v11;v12])
(**
BoolVector disposing
------------------------------
BoolVector implements `IDisposable` so that you can deallocate unmanaged memory deterministically:
*)
v10.Dispose()
(**
If `Dispose` is not called then the memory is deallocated in Finalizer.
*)

