namespace FCor
#nowarn "9"

open System
open System.Text
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic

type MklControl =

    static member SetMaxThreads(n : int) = MklFunctions.Set_Max_Threads(n)

    static member FreeBuffers() = MklFunctions.Free_Buffers()

    static member ThreadFreeBuffers() = MklFunctions.Thread_Free_Buffers()

    static member DisableFastMM() = MklFunctions.Disable_Fast_MM()

    static member MemStat() = MklFunctions.Mem_Stat()


