module Util
open System
open FCore

//let should (f : 'a -> #Constraint) x (y : obj) =
//    let c = f x
//    let y =
//        match y with
//        | :? (unit -> unit) -> box (new TestDelegate(y :?> unit -> unit))
//        | _                 -> y
//    Assert.That(y, c)
//
//let equal x = new EqualConstraint(x)
//
//let not x = new NotConstraint(x)
//
//let contain x = new ContainsConstraint(x)
//
//let haveLength n = Has.Length.EqualTo(n)
//
//let haveCount n = Has.Count.EqualTo(n)
//
//let be = id
//
//let Null = new NullConstraint()
//
//let Empty = new EmptyConstraint()
//
//let EmptyString = new EmptyStringConstraint()
//
//let NullOrEmptyString = new NullOrEmptyStringConstraint()
//
//let True = new TrueConstraint()
//
//let False = new FalseConstraint()
//
//let sameAs x = new SameAsConstraint(x)
//
//let throw = Throws.TypeOf

let inline epsEqual x y eps =
    if x = y then true
    else
        if x = 0.0 then abs(y) <= eps
        elif y = 0.0 then abs(x) <= eps
        else abs(x-y)/(max (abs(x)) (abs(y))) <= eps

let inline nearlyEqual (a : Vector) (b : Vector) (eps : float) =
    let A = a.ToArray()
    let B = b.ToArray()
    B |> Array.zip A |> Array.map (fun (x,y) -> epsEqual x y eps) |> Array.fold (&&) true

