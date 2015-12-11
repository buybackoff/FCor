namespace FCor
#nowarn "9"

open System
open System.Text
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic

module ArgumentChecks =
    
    let throwIfLengthNotOKForElementwise (len1 : int64) (len2 : int64) =
        if (len1 = len2) || (len1 = 1L) || (len2 = 1L) then ()
        else raise (new ArgumentException("Elementwise operation: vector length mismatch")) 

    let throwIfSizeNotOKForElementwise (size1 : int64*int64) (size2 : int64*int64) =
        let len1 = fst(size1)*snd(size1)
        let len2 = fst(size2)*snd(size2)
        if (size1 = size2) || (len1 = 1L) || (len2 = 1L) || (len1 = 0L && len2 = 0L) then ()
        else raise (new ArgumentException("Elementwise operation: matrix size mismatch")) 

    let getElementwiseLength (len1 : int64 option) (len2 : int64 option) =
        match len1, len2 with
            | Some(l1), Some(l2) when l1 = l2 -> Some l1
            | Some(l1), Some(l2) when l1 <> 0L && l2 <> 0L && l1 <> l2 && (l1 = 1L || l2 = 1L) -> max l1 l2 |> Some   
            | _ -> None

    let getElementwiseLengthIf (ifLen : int64 option) (trueLen : int64 option) (falseLen : int64 option) =
        match ifLen, trueLen, falseLen with
            | Some(l1), Some(l2), Some(l3) when l1 = l2 && l2 = l3 -> Some l1
            | Some(l1), Some(l2), Some(l3) when ((l1 = l2) || (l2 = 1L)) && ((l1 = l3) || (l3 = 1L)) -> Some l1
            | _ -> None

    let getElementwiseSize (size1 : (int64*int64) option) (size2 : (int64*int64) option) =
        match size1, size2 with
            | Some(s1), Some(s2) when s1 = s2 -> Some s1 
            | Some(r1, c1), Some(r2, c2) when r1*c1 = 0L && r2*c2 = 0L -> Some (0L,0L)
            | Some(r1, c1), Some(s2) when r1*c1 = 1L -> Some s2
            | Some(s1), Some(r2, c2) when r2*c2 = 1L -> Some s1 
            | _ -> None

    let getElementwiseSizeIf (ifSize : (int64*int64) option) (trueSize : (int64*int64) option) (falseSize : (int64*int64) option) =
        match ifSize, trueSize, falseSize with
            | Some(s1), Some(s2), Some(s3) when s1 = s2 && s2 = s3 -> Some s1
            | Some(s1), Some(s2), Some(s3) when ((s1 = s2) || (s2 = (1L,1L))) && ((s1 = s3) || (s3 = (1L,1L))) -> Some s1
            | _ -> None        

