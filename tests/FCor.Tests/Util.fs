namespace FCor.Tests

open System
open FCor

module Util = 
    let inline epsEqualFloat x y eps = 
        if x = y then true
        else if x = 0.0 then abs (y) <= eps
        elif y = 0.0 then abs (x) <= eps
        elif Double.IsNaN(x) && Double.IsNaN(y) then true
        elif Double.IsNegativeInfinity(x) && Double.IsNegativeInfinity(y) then true
        elif Double.IsPositiveInfinity(x) && Double.IsPositiveInfinity(y) then true
        else abs (x - y) / (max (abs (x)) (abs (y))) <= eps
    
    let inline epsEqualArray (a : 'T []) (b : 'T []) (epsEqual : 'T -> 'T -> 'S -> bool) (eps : 'S) = 
        b
        |> Array.zip a
        |> Array.map (fun (x, y) -> epsEqual x y eps)
        |> Array.fold (&&) true
    
    let inline epsEqualArray2D (a : 'T [,]) (b : 'T [,]) (epsEqual : 'T -> 'T -> 'S -> bool) (eps : 'S) = 
        a
        |> Array2D.mapi (fun r c x -> epsEqual x b.[r, c] eps)
        |> Seq.cast<bool>
        |> Seq.toArray
        |> Array.fold (&&) true

    let fixEmpty (a : 'T[,]) =
        if a.Length = 0 then Array2D.zeroCreate<'T> 0 0 
        else a
