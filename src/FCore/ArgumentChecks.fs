namespace FCore
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
        

