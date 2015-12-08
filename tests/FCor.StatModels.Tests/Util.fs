namespace FCor.StatModels.Tests
open System
open System.Runtime.InteropServices
open FCor

module Util =

    let fixEmpty (a : 'T[,]) =
        if a.Length = 0 then Array2D.zeroCreate<'T> 0 0 
        else a

    let inline epsEqualFloat x y eps =
        if x = y then true
        else
            if Double.IsNaN(x) && Double.IsNaN(y) then true
            elif Double.IsNegativeInfinity(x) && Double.IsNegativeInfinity(y) then true
            elif Double.IsPositiveInfinity(x) && Double.IsPositiveInfinity(y) then true
            elif abs(x) <= eps && abs(y) <= eps then true
            else abs(x-y)/(max (abs(x)) (abs(y))) <= eps

    let inline epsEqualArray (a : 'T[]) (b : 'T[]) (epsEqual : 'T -> 'T -> 'S -> bool) (eps : 'S) =
        if a.Length = 0 && b.Length = 0 then true
        else (a.Length = b.Length) && (b |> Array.zip a |> Array.map (fun (x,y) -> epsEqual x y eps) |> Array.fold (&&) true)


    let inline epsEqualArray2D (a : 'T[,]) (b : 'T[,]) (epsEqual : 'T -> 'T -> 'S -> bool) (eps : 'S) =
        if a.Length = 0 && b.Length = 0 then true
        else
            let s1 = seq{for col in 0..a.GetLength(1)-1 do for row in 0..a.GetLength(0)-1 do yield a.[row,col]}
            let s2 = seq{for col in 0..b.GetLength(1)-1 do for row in 0..b.GetLength(0)-1 do yield b.[row,col]}
            (a.GetLength(0) = b.GetLength(0)) && (a.GetLength(1) = b.GetLength(1)) &&
            (s2 |> Seq.zip s1 |> Seq.map (fun (x,y) -> epsEqual x y eps) |> Seq.fold (&&) true)

    let array2DZip (x : 'T[,]) (y : 'T[,]) =
        if x.GetLength(0) <> y.GetLength(0) || x.GetLength(1) <> y.GetLength(1) then
            raise (new ArgumentException())
        else
            Array2D.init (x.GetLength(0)) (x.GetLength(1)) (fun i j -> x.[i, j], y.[i, j])

    let array2DExists (pred : 'T -> bool) (x : 'T[,])=
        seq{for i in 0..x.GetLength(0)-1 do for j in 0..x.GetLength(1)-1 do yield x.[i, j]} |> Seq.exists pred

    let array2DFilter (pred : 'T -> bool) (x : 'T[,])=
        seq{for i in 0..x.GetLength(0)-1 do for j in 0..x.GetLength(1)-1 do yield x.[i, j]} |> Seq.filter pred |> Seq.toArray


        

