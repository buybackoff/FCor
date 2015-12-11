namespace FCor
#nowarn "9"

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic
open FCor.ExplicitConversion

type UInt16Vector(length : int64, nativeArray : nativeptr<uint16>, gcHandlePtr : IntPtr, isView : bool, parentVector : UInt16Vector option) as this =
    let mutable isDisposed = false
    do
        if length < 0L then
            GC.SuppressFinalize(this)
            raise (new ArgumentException("UInt16Vector length must be >= 0"))

    static let empty = new UInt16Vector(0L, IntPtr.Zero |> NativePtr.ofNativeInt<uint16>, IntPtr.Zero, false, None)

    new(length : int64, init : uint16) =
        let mutable arr = IntPtr.Zero
        if length < 0L then
            new UInt16Vector(length, arr |> NativePtr.ofNativeInt<uint16>, IntPtr.Zero, false, None)
        elif length = 0L then new UInt16Vector(0L, IntPtr.Zero |> NativePtr.ofNativeInt<uint16>, IntPtr.Zero, false, None)
        else
            let nativeArrayPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<UInt16Ptr>
            if init = 0us then
                MklFunctions.UI16_Create_Zero_Array(length, nativeArrayPtr)
                let nativeArray = arr |> NativePtr.ofNativeInt<uint16>
                new UInt16Vector(length, nativeArray, IntPtr.Zero, false, None)
            else
                MklFunctions.UI16_Create_Array(length, nativeArrayPtr)
                let nativeArray = arr |> NativePtr.ofNativeInt<uint16>
                MklFunctions.UI16_Fill_Array(init, length, nativeArray)
                new UInt16Vector(length, nativeArray, IntPtr.Zero, false, None)

    new(length : int, init : uint16) =
        let length = length |> int64
        new UInt16Vector(length, init)

    new(data : uint16 seq) =
        let data = data |> Seq.toArray
        let length = data.GetLongLength(0)
        if length = 0L then
            new UInt16Vector(0L, IntPtr.Zero |> NativePtr.ofNativeInt<uint16>, IntPtr.Zero, false, None)
        else
            let mutable arr = IntPtr.Zero
            let nativeArrayPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<UInt16Ptr>
            MklFunctions.UI16_Create_Array(length, nativeArrayPtr)
            let gcHandle = GCHandle.Alloc(data, GCHandleType.Pinned)
            MklFunctions.UI16_Copy_Array(length, gcHandle.AddrOfPinnedObject() |> NativePtr.ofNativeInt<uint16>, arr |> NativePtr.ofNativeInt<uint16>) 
            gcHandle.Free()
            new UInt16Vector(length, arr |> NativePtr.ofNativeInt<uint16>, IntPtr.Zero, false, None)

    new(data : uint16[], copyData : bool) =
        if copyData then
            new UInt16Vector(data)
        else
            let length = data.GetLongLength(0)
            if length = 0L then
                new UInt16Vector(0L, IntPtr.Zero |> NativePtr.ofNativeInt<uint16>, IntPtr.Zero, false, None)
            else
                let gcHandle = GCHandle.Alloc(data, GCHandleType.Pinned)
                new UInt16Vector(length, gcHandle.AddrOfPinnedObject() |> NativePtr.ofNativeInt<uint16>, GCHandle.ToIntPtr(gcHandle), true, None)

    new(data : uint16) = new UInt16Vector(1L, data)

    new(length : int, initializer : int -> uint16) =
        let data = Array.init length initializer
        new UInt16Vector(data, false)


    member this.Length = length |> int

    member this.LongLength = length

    member this.NativeArray = nativeArray

    member this.IsDisposed =
        if length = 0L then false
        else
            match parentVector with
                | Some(v) -> isDisposed || v.IsDisposed
                | None -> isDisposed

    member this.IsView = isView

    static member Empty = empty

    static member op_Explicit(v : uint16) = new UInt16Vector(v)

    static member op_Explicit(v : uint16 seq) = new UInt16Vector(v)

    static member op_Explicit(v : uint16 list) = new UInt16Vector(v)

    static member op_Explicit(v : uint16  array) = new UInt16Vector(v)

    static member op_Explicit(v : UInt16Vector) = v

    member this.View
        with get(fromIndex, toIndex) =
            if isDisposed then raise (new ObjectDisposedException(""))
            if fromIndex < 0L || fromIndex >= length then raise (new IndexOutOfRangeException())
            if toIndex < 0L || toIndex >= length then raise (new IndexOutOfRangeException())
            if fromIndex > toIndex then UInt16Vector.Empty
            else
                let length = toIndex - fromIndex + 1L
                let sizeof = sizeof<uint16> |> int64
                let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + fromIndex*sizeof) |> NativePtr.ofNativeInt<uint16>
                new UInt16Vector(length, offsetAddr, IntPtr.Zero, true, Some this)

    member this.View
        with get(fromIndex : int, toIndex : int) = this.View(int64(fromIndex), int64(toIndex))

    member this.GetSlice(fromIndex, toIndex) =
        if this.IsDisposed then raise (new ObjectDisposedException(""))
        let fromIndex = defaultArg fromIndex 0L
        let toIndex = defaultArg toIndex (length - 1L)
        if fromIndex < 0L || fromIndex >= length then raise (new IndexOutOfRangeException())
        if toIndex < 0L || toIndex >= length then raise (new IndexOutOfRangeException())
        if fromIndex > toIndex then UInt16Vector.Empty
        else
            let length = toIndex - fromIndex + 1L
            let view = this.View(fromIndex, toIndex)
            let mutable arr = IntPtr.Zero
            let nativeArrayPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<UInt16Ptr>
            MklFunctions.UI16_Create_Array(length, nativeArrayPtr)
            MklFunctions.UI16_Copy_Array(length, view.NativeArray, arr |> NativePtr.ofNativeInt<uint16>) 
            new UInt16Vector(length, arr |> NativePtr.ofNativeInt<uint16>, IntPtr.Zero, false, None)

    member this.GetSlice(fromIndex : int option, toIndex : int option) =
        this.GetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64)

    member this.SetSlice(fromIndex : int64 option, toIndex : int64 option, value: uint16) =
        if this.IsDisposed then raise (new ObjectDisposedException(""))
        let fromIndex = defaultArg fromIndex 0L
        let toIndex = defaultArg toIndex (length - 1L)
        if fromIndex < 0L || fromIndex >= length then raise (new IndexOutOfRangeException())
        if toIndex < 0L || toIndex >= length then raise (new IndexOutOfRangeException())
        if fromIndex > toIndex then ()
        else
            let length = toIndex - fromIndex + 1L
            let view = this.View(fromIndex, toIndex)
            MklFunctions.UI16_Fill_Array(value, length, view.NativeArray)

    member this.SetSlice(fromIndex : int option, toIndex : int option, value: uint16) =
        this.SetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64, value)

    member this.SetSlice(fromIndex : Option<int64>, toIndex : Option<int64>, value: UInt16Vector) =
        if this.IsDisposed then raise (new ObjectDisposedException(""))
        if value.IsDisposed then raise (new ObjectDisposedException(""))
        if value.LongLength = 1L then
            this.SetSlice(fromIndex, toIndex, (value.[0L]:uint16))
        else
            let fromIndex = defaultArg fromIndex 0L
            let toIndex = defaultArg toIndex (length - 1L)
            if fromIndex < 0L || fromIndex >= length then raise (new IndexOutOfRangeException())
            if toIndex < 0L || toIndex >= length then raise (new IndexOutOfRangeException())
            if fromIndex > toIndex && value.LongLength = 0L then ()
            else
                let length = toIndex - fromIndex + 1L
                if value.LongLength <> length then raise (new ArgumentException())
                let view = this.View(fromIndex, toIndex)
                MklFunctions.UI16_Copy_Array(length, value.NativeArray, view.NativeArray)

    member this.SetSlice(fromIndex : int option, toIndex : int option, value: UInt16Vector) =
        this.SetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64, value)

    member this.Item
        with get(i : int64) =
            if isDisposed then raise (new ObjectDisposedException(""))
            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + i*2L) |> NativePtr.ofNativeInt<uint16>
            NativePtr.read offsetAddr  
        and set (i : int64) value =
            if isDisposed then raise (new ObjectDisposedException(""))
            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + i*2L) |> NativePtr.ofNativeInt<uint16>
            NativePtr.write offsetAddr value

    member this.Item
        with get(i : int) =
            if isDisposed then raise (new ObjectDisposedException(""))
            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + int64(i)*2L) |> NativePtr.ofNativeInt<uint16>
            NativePtr.read offsetAddr 
        and set (i : int) value =
            if isDisposed then raise (new ObjectDisposedException(""))
            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + int64(i)*2L) |> NativePtr.ofNativeInt<uint16>
            NativePtr.write offsetAddr value

    interface IFormattable with
        member this.ToString(format, provider) = 
            if this.IsDisposed then raise (new ObjectDisposedException(""))
            let maxRows, _ = DisplayControl.MaxDisplaySize
            let showRows = max 0L (min (maxRows |> int64) length) |> int
            let moreRows = length > (showRows |> int64)
            let arr = Array2D.init showRows 1 (fun row col -> this.[row])
            let formattedArray = DisplayControl.FormatArray2D(arr, format, moreRows, false)
            sprintf "Length = %d\r\n%s" length formattedArray

    member this.Dispose() = (this:>IDisposable).Dispose()

    interface IDisposable with
        member this.Dispose() = this.DoDispose(true)

    member internal this.DoDispose(isDisposing) = if not isDisposed && length > 0L then
                                                     isDisposed <- true
                                                     if isDisposing then GC.SuppressFinalize(this)
                                                     let nativeArray = nativeArray |> NativePtr.toNativeInt
                                                     if not isView && gcHandlePtr = IntPtr.Zero && nativeArray <> IntPtr.Zero then MklFunctions.Free_Array(nativeArray)
                                                     if gcHandlePtr <> IntPtr.Zero then
                                                         try
                                                             let gcHandle = GCHandle.FromIntPtr(gcHandlePtr)
                                                             if gcHandle.IsAllocated then gcHandle.Free()
                                                         with _ -> ()

    override this.Finalize() = try this.DoDispose(false) with _ -> ()

