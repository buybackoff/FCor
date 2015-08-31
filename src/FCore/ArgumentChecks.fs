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
        

