namespace FCor
#nowarn "9"

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic
open FCor.ExplicitConversion

[<Struct>]
type DataBuffer<'T when 'T : unmanaged>(length : int64, nativeArray : nativeptr<'T>, isView: bool) =

     member this.LongLength = length
     member this.NativeArray = nativeArray
     member this.IsView = isView

     member this.Dispose() = if not isView then MklFunctions.Free_Array(nativeArray |> NativePtr.toNativeInt)

     static member CreateBoolBuffer(length : int64) =
          let mutable arr = IntPtr.Zero
          let nativeArrayPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<BoolPtr>
          MklFunctions.B_Create_Array(length, nativeArrayPtr)
          let nativeArray = arr |> NativePtr.ofNativeInt<bool>
          new DataBuffer<bool>(length, nativeArray, false)

     static member CreateBoolBuffer(length : int64, fill : bool) =
          let mutable arr = IntPtr.Zero
          let nativeArrayPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<BoolPtr>
          MklFunctions.B_Create_Array(length, nativeArrayPtr)
          let nativeArray = arr |> NativePtr.ofNativeInt<bool>
          MklFunctions.B_Fill_Array(fill, length, nativeArray)
          new DataBuffer<bool>(length, nativeArray, false)

     static member CreateFloatBuffer(length : int64) =
          let mutable arr = IntPtr.Zero
          let nativeArrayPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<FloatPtr>
          MklFunctions.D_Create_Array(length, nativeArrayPtr)
          let nativeArray = arr |> NativePtr.ofNativeInt<float>
          new DataBuffer<float>(length, nativeArray, false)

     static member CreateFloatBuffer(length : int64, fill : float) =
          let mutable arr = IntPtr.Zero
          let nativeArrayPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<FloatPtr>
          MklFunctions.D_Create_Array(length, nativeArrayPtr)
          let nativeArray = arr |> NativePtr.ofNativeInt<float>
          MklFunctions.D_Fill_Array(fill, length, nativeArray)
          new DataBuffer<float>(length, nativeArray, false)

type BoolVector(length : int64, nativeArray : nativeptr<bool>, gcHandlePtr : IntPtr, isView : bool, parentVector : BoolVector option) as this =
    let mutable isDisposed = false
    do
        if length < 0L then
            GC.SuppressFinalize(this)
            raise (new ArgumentException("BoolVector length must be >= 0"))

    static let empty = new BoolVector(0L, IntPtr.Zero |> NativePtr.ofNativeInt<bool>, IntPtr.Zero, false, None)

    static let mutable evalSliceLength = 100000

    new(length : int64, init : bool) =
        let mutable arr = IntPtr.Zero
        if length < 0L then
            new BoolVector(length, arr |> NativePtr.ofNativeInt<bool>, IntPtr.Zero, false, None)
        elif length = 0L then new BoolVector(0L, IntPtr.Zero |> NativePtr.ofNativeInt<bool>, IntPtr.Zero, false, None)
        else
            let nativeArrayPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<BoolPtr>
            if init = false then
                MklFunctions.B_Create_Zero_Array(length, nativeArrayPtr)
                let nativeArray = arr |> NativePtr.ofNativeInt<bool>
                new BoolVector(length, nativeArray, IntPtr.Zero, false, None)
            else
                MklFunctions.B_Create_Array(length, nativeArrayPtr)
                let nativeArray = arr |> NativePtr.ofNativeInt<bool>
                MklFunctions.B_Fill_Array(init, length, nativeArray)
                new BoolVector(length, nativeArray, IntPtr.Zero, false, None)

    new(length : int, init : bool) =
        let length = length |> int64
        new BoolVector(length, init)

    new(data : bool seq) =
        let data = data |> Seq.toArray
        let length = data.GetLongLength(0)
        if length = 0L then
            new BoolVector(0L, IntPtr.Zero |> NativePtr.ofNativeInt<bool>, IntPtr.Zero, false, None)
        else
            let mutable arr = IntPtr.Zero
            let nativeArrayPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<BoolPtr>
            MklFunctions.B_Create_Array(length, nativeArrayPtr)
            let gcHandle = GCHandle.Alloc(data, GCHandleType.Pinned)
            MklFunctions.B_Copy_Array(length, gcHandle.AddrOfPinnedObject() |> NativePtr.ofNativeInt<bool>, arr |> NativePtr.ofNativeInt<bool>) 
            gcHandle.Free()
            new BoolVector(length, arr |> NativePtr.ofNativeInt<bool>, IntPtr.Zero, false, None)

    new(data : bool[], copyData : bool) =
        if copyData then
            new BoolVector(data)
        else
            let length = data.GetLongLength(0)
            if length = 0L then
                new BoolVector(0L, IntPtr.Zero |> NativePtr.ofNativeInt<bool>, IntPtr.Zero, false, None)
            else
                let gcHandle = GCHandle.Alloc(data, GCHandleType.Pinned)
                new BoolVector(length, gcHandle.AddrOfPinnedObject() |> NativePtr.ofNativeInt<bool>, GCHandle.ToIntPtr(gcHandle), true, None)

    new(data : bool) = new BoolVector(1L, data)

    new(length : int, initializer : int -> bool) =
        let data = Array.init length initializer
        new BoolVector(data, false)


    member this.Length = length |> int

    member this.LongLength = length

    member this.NativeArray = nativeArray

    member internal this.DataBuffer = new DataBuffer<bool>(length, nativeArray, isView)

    member this.IsDisposed =
        if length = 0L then false
        else
            match parentVector with
                | Some(v) -> isDisposed || v.IsDisposed
                | None -> isDisposed

    member this.IsView = isView

    static member Empty = empty

    static member op_Explicit(v : bool) = new BoolVector(v)

    static member op_Explicit(v : bool seq) = new BoolVector(v)

    static member op_Explicit(v : bool list) = new BoolVector(v)

    static member op_Explicit(v : bool array) = new BoolVector(v)

    static member op_Explicit(v : BoolVector) = v

    static member op_Explicit(v : BoolVector) = v.AsExpr

    static member EvalSliceLength
        with get() = evalSliceLength
        and set(value) = evalSliceLength <- value

    member this.View
        with get(fromIndex, toIndex) =
            if isDisposed then raise (new ObjectDisposedException(""))
            if fromIndex < 0L || fromIndex >= length then raise (new IndexOutOfRangeException())
            if toIndex < 0L || toIndex >= length then raise (new IndexOutOfRangeException())
            if fromIndex > toIndex then BoolVector.Empty
            else
                let length = toIndex - fromIndex + 1L
                let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + fromIndex) |> NativePtr.ofNativeInt<bool>
                new BoolVector(length, offsetAddr, IntPtr.Zero, true, Some this)

    member this.View
        with get(fromIndex : int, toIndex : int) = this.View(int64(fromIndex), int64(toIndex))

    member this.GetSlice(fromIndex, toIndex) =
        if this.IsDisposed then raise (new ObjectDisposedException(""))
        let fromIndex = defaultArg fromIndex 0L
        let toIndex = defaultArg toIndex (length - 1L)
        if fromIndex < 0L || fromIndex >= length then raise (new IndexOutOfRangeException())
        if toIndex < 0L || toIndex >= length then raise (new IndexOutOfRangeException())
        if fromIndex > toIndex then BoolVector.Empty
        else
            let length = toIndex - fromIndex + 1L
            let view = this.View(fromIndex, toIndex)
            let mutable arr = IntPtr.Zero
            let nativeArrayPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<BoolPtr>
            MklFunctions.B_Create_Array(length, nativeArrayPtr)
            MklFunctions.B_Copy_Array(length, view.NativeArray, arr |> NativePtr.ofNativeInt<bool>) 
            new BoolVector(length, arr |> NativePtr.ofNativeInt<bool>, IntPtr.Zero, false, None)

    member this.GetSlice(fromIndex : int option, toIndex : int option) =
        this.GetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64)

    member this.SetSlice(fromIndex : int64 option, toIndex : int64 option, value: bool) =
        if this.IsDisposed then raise (new ObjectDisposedException(""))
        let fromIndex = defaultArg fromIndex 0L
        let toIndex = defaultArg toIndex (length - 1L)
        if fromIndex < 0L || fromIndex >= length then raise (new IndexOutOfRangeException())
        if toIndex < 0L || toIndex >= length then raise (new IndexOutOfRangeException())
        if fromIndex > toIndex then ()
        else
            let length = toIndex - fromIndex + 1L
            let view = this.View(fromIndex, toIndex)
            MklFunctions.B_Fill_Array(value, length, view.NativeArray)

    member this.SetSlice(fromIndex : int option, toIndex : int option, value: bool) =
        this.SetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64, value)

    member this.SetSlice(fromIndex : Option<int64>, toIndex : Option<int64>, value: BoolVector) =
        if this.IsDisposed then raise (new ObjectDisposedException(""))
        if value.IsDisposed then raise (new ObjectDisposedException(""))
        if value.LongLength = 1L then
            this.SetSlice(fromIndex, toIndex, (value.[0L]:bool))
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
                MklFunctions.B_Copy_Array(length, value.NativeArray, view.NativeArray)

    member this.SetSlice(fromIndex : int option, toIndex : int option, value: BoolVector) =
        this.SetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64, value)

    member this.Item
        with get(i : int64) =
            if isDisposed then raise (new ObjectDisposedException(""))
            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + i) |> NativePtr.ofNativeInt<bool>
            NativePtr.read offsetAddr  
        and set (i : int64) value =
            if isDisposed then raise (new ObjectDisposedException(""))
            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + i) |> NativePtr.ofNativeInt<bool>
            NativePtr.write offsetAddr value

    member this.Item
        with get(i : int) =
            if isDisposed then raise (new ObjectDisposedException(""))
            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + int64(i)) |> NativePtr.ofNativeInt<bool>
            NativePtr.read offsetAddr 
        and set (i : int) value =
            if isDisposed then raise (new ObjectDisposedException(""))
            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + int64(i)) |> NativePtr.ofNativeInt<bool>
            NativePtr.write offsetAddr value

    member this.Item
        with get(indices : int64 seq) = 
            if this.IsDisposed then raise (new ObjectDisposedException(""))
            let indices = indices |> Seq.toArray
            let length = indices.GetLongLength(0)
            let res = new BoolVector(length, false)
            indices |> Array.iteri (fun i index -> res.[i] <- this.[index])
            res
        and set (indices : int64 seq) (value : BoolVector) =
            if this.IsDisposed then raise (new ObjectDisposedException(""))
            if value.IsDisposed then raise (new ObjectDisposedException(""))
            let indices = indices |> Seq.toArray
            if value.LongLength = 1L then
                let value = value.[0L]
                indices |> Array.iteri (fun i index -> this.[index] <- value)
            else
                indices |> Array.iteri (fun i index -> this.[index] <- value.[i])

    member this.Item
        with get(indices : int seq) = 
            if this.IsDisposed then raise (new ObjectDisposedException(""))
            let indices = indices |> Seq.toArray
            let length = indices.GetLongLength(0)
            let res = new BoolVector(length, false)
            indices |> Array.iteri (fun i index -> res.[i] <- this.[index])
            res
        and set (indices : int seq) (value : BoolVector) =
            if this.IsDisposed then raise (new ObjectDisposedException(""))
            if value.IsDisposed then raise (new ObjectDisposedException(""))
            if value.LongLength = 1L then
                indices |> Seq.iteri (fun i index -> this.[index] <- value.[0])
            else
                indices |> Seq.iteri (fun i index -> this.[index] <- value.[i])

    member this.Item
        with get(boolVector : BoolVector) = 
            if this.IsDisposed then raise (new ObjectDisposedException(""))
            if boolVector.IsDisposed then raise (new ObjectDisposedException(""))
            if length <> boolVector.LongLength then raise (new ArgumentException("Vector length mismatch"))
            let mutable arr = IntPtr.Zero
            let mutable resLen = 0L
            let nativeArrPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<BoolPtr>
            MklFunctions.B_Get_Bool_Slice(length, nativeArray, boolVector.NativeArray, nativeArrPtr, &&resLen)
            new BoolVector(resLen, arr |> NativePtr.ofNativeInt<bool>, IntPtr.Zero, false, None)

        and set (boolVector : BoolVector) (value : BoolVector) =
            if this.IsDisposed then raise (new ObjectDisposedException(""))
            if boolVector.IsDisposed then raise (new ObjectDisposedException(""))
            if value.IsDisposed then raise (new ObjectDisposedException(""))
            if length <> boolVector.LongLength then raise (new ArgumentException("Vector length mismatch"))
            MklFunctions.B_Set_Bool_Slice(length, nativeArray, boolVector.NativeArray, value.NativeArray, value.LongLength)

    member this.ToArray() =
        if this.IsDisposed then raise (new ObjectDisposedException(""))
        Array.init this.Length (fun i -> this.[i])

    member this.AsExpr
        with get() = 
            if this.IsDisposed then raise (new ObjectDisposedException(""))
            if length = 1L then BoolVectorExpr.Scalar(this.[0L])
            else BoolVectorExpr.Var(this)

    static member Concat(vectors : BoolVector seq) =
        vectors |> Seq.iter (fun v -> if v.IsDisposed then raise (new ObjectDisposedException("")))
        let vectors = vectors |> Seq.filter (fun v -> v.LongLength <> 0L) |> Seq.toArray
        if vectors.Length = 0 then BoolVector.Empty
        else
            let length = vectors |> Array.map (fun v -> v.LongLength) |> Array.reduce (+)
            let res = new BoolVector(length, false)
            vectors |> Array.fold (fun offset v ->
                                     res.View(offset, offset + v.LongLength - 1L).SetSlice(Some(0L), None, v)
                                     offset + v.LongLength
                                ) 0L |> ignore
            res

    static member Copy(vector : BoolVector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        if vector.LongLength = 0L then BoolVector.Empty
        else
            let res = new BoolVector(vector.LongLength, false)
            MklFunctions.B_Copy_Array(vector.LongLength, vector.NativeArray, res.NativeArray)
            res


    static member (==) (vector1: BoolVector, vector2: BoolVector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        vector1 = vector2

    static member (!=) (vector1: BoolVector, vector2: BoolVector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        vector1 <> vector2

    static member (==) (vector: BoolVector, a : bool) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        vector.LongLength = 1L && vector.[0L] = a

    static member (!=) (vector: BoolVector, a : bool) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        not (vector == a)

    static member (==) (a : bool, vector: BoolVector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        vector.LongLength = 1L && vector.[0L] = a

    static member (!=) (a : bool, vector: BoolVector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        not (vector == a)


    static member (.<) (vector1: BoolVector, vector2: BoolVector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.B_Arrays_LessThan(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.<=) (vector1: BoolVector, vector2: BoolVector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.B_Arrays_LessEqual(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.>) (vector1: BoolVector, vector2: BoolVector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.B_Arrays_GreaterThan(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.>=) (vector1: BoolVector, vector2: BoolVector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.B_Arrays_GreaterEqual(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.=) (vector1: BoolVector, vector2: BoolVector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.B_Arrays_EqualElementwise(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.<>) (vector1: BoolVector, vector2: BoolVector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.B_Arrays_NotEqualElementwise(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res



    static member (.<) (vector: BoolVector, a : bool) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new BoolVector(a)
        MklFunctions.B_Arrays_LessThan(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res

    static member (.<=) (vector: BoolVector, a : bool) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new BoolVector(a)
        MklFunctions.B_Arrays_LessEqual(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res

    static member (.>) (vector: BoolVector, a : bool) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new BoolVector(a)
        MklFunctions.B_Arrays_GreaterThan(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res

    static member (.>=) (vector: BoolVector, a : bool) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new BoolVector(a)
        MklFunctions.B_Arrays_GreaterEqual(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res

    static member (.=) (vector: BoolVector, a : bool) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new BoolVector(a)
        MklFunctions.B_Arrays_EqualElementwise(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res

    static member (.<>) (vector: BoolVector, a : bool) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new BoolVector(a)
        MklFunctions.B_Arrays_NotEqualElementwise(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res



    static member (.<) (a : bool, vector: BoolVector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        vector .> a

    static member (.<=) (a : bool, vector: BoolVector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        vector .>= a

    static member (.>) (a : bool, vector: BoolVector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        vector .< a

    static member (.>=) (a : bool, vector: BoolVector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        vector .<= a

    static member (.=) (a : bool, vector: BoolVector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        vector .= a

    static member (.<>) (a : bool, vector: BoolVector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        vector .<> a



    static member Max(vector1 : BoolVector, vector2 : BoolVector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.B_Max_Arrays(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member Min(vector1 : BoolVector, vector2 : BoolVector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.B_Min_Arrays(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member Max(vector : BoolVector, a : bool) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let a = new BoolVector(a)
        BoolVector.Max(vector, a)

    static member Min(vector : BoolVector, a : bool) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let a = new BoolVector(a)
        BoolVector.Min(vector, a)

    static member Max(a : bool, vector : BoolVector) = 
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        BoolVector.Max(vector, a)

    static member Min(a : bool, vector : BoolVector) = 
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        BoolVector.Min(vector, a)

    static member (.&&) (vector1 : BoolVector, vector2 : BoolVector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.B_And_Arrays(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.||) (vector1 : BoolVector, vector2 : BoolVector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.B_Or_Arrays(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.&&) (vector : BoolVector, a : bool) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let a = new BoolVector(a)
        vector .&& a

    static member (.||) (vector : BoolVector, a : bool) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let a = new BoolVector(a)
        vector .|| a

    static member (.&&) (a : bool, vector : BoolVector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let a = new BoolVector(a)
        vector .&& a

    static member (.||) (a : bool, vector : BoolVector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let a = new BoolVector(a)
        vector .|| a

    static member Not (vector : BoolVector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let res = new BoolVector(vector.LongLength, false)
        MklFunctions.B_Not_Array(vector.LongLength, vector.NativeArray, res.NativeArray)
        res

    static member Any(vector : BoolVector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        if vector.LongLength = 0L then raise (new ArgumentException("Vector must have length > 0"))
        let mutable res = false
        MklFunctions.B_Any_Matrix(false, 1L, vector.LongLength, vector.NativeArray, &&res)
        res

    static member All(vector : BoolVector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        if vector.LongLength = 0L then raise (new ArgumentException("Vector must have length > 0"))
        let mutable res = false
        MklFunctions.B_All_Matrix(false, 1L, vector.LongLength, vector.NativeArray, &&res)
        res

    override this.ToString() = 
        if this.IsDisposed then raise (new ObjectDisposedException(""))
        (this:>IFormattable).ToString(GenericFormatting.GenericFormat.Instance.GetFormat<bool>() true, null)

    override this.Equals(yobj) =
        match yobj with
        | :? BoolVector as y ->
            if this.IsDisposed then raise (new ObjectDisposedException(""))
            if y.IsDisposed then raise (new ObjectDisposedException(""))
            if this.LongLength = 0L && y.LongLength = 0L then true
            elif this.LongLength <> y.LongLength then false
            else 
                MklFunctions.B_Arrays_Are_Equal(this.LongLength, this.NativeArray, y.NativeArray)
        | _ -> false
 
    override this.GetHashCode() = 
        if this.IsDisposed then raise (new ObjectDisposedException(""))
        hash (this.LongLength, this.NativeArray)

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

//**************************************BoolVectorExpr**************************************************************************************

and BoolVectorExpr = 
    | Scalar of bool
    | Var of BoolVector
    | UnaryFunction of BoolVectorExpr * (DataBuffer<bool> -> DataBuffer<bool> -> unit) * string
    | BinaryFunction of BoolVectorExpr * BoolVectorExpr * (DataBuffer<bool> -> DataBuffer<bool> -> DataBuffer<bool> -> unit) * string
    | BinaryVectorFunction of VectorExpr * VectorExpr * (DataBuffer<float> -> DataBuffer<float> -> DataBuffer<bool> -> unit) * string
    | IfFunction of BoolVectorExpr * BoolVectorExpr * BoolVectorExpr

    member this.Length =
        match this with
            | Scalar(_)-> Some 1L
            | Var(v) -> Some v.LongLength
            | UnaryFunction(v, _, _) -> v.Length    
            | BinaryFunction(v1, v2, _, _) -> ArgumentChecks.getElementwiseLength v1.Length v2.Length
            | BinaryVectorFunction(v1, v2, _, _) -> ArgumentChecks.getElementwiseLength v1.Length v2.Length               
            | IfFunction(v1, v2, v3) -> ArgumentChecks.getElementwiseLengthIf v1.Length v2.Length v3.Length

    static member op_Explicit(a : bool) = (new BoolVector(a)).AsExpr

    static member op_Explicit(v : BoolVector) = v.AsExpr

    static member EvalSlice (boolVectorExpr : BoolVectorExpr) (sliceStart : int64) (sliceLen : int64) (memPool : MemoryPool) = 
        match boolVectorExpr with
            | Scalar(a) ->
                let v : DataBuffer<bool> = memPool.GetBoolVector(1L)
                NativePtr.write v.NativeArray a
                v, memPool
            | Var(v) ->
                let offsetAddr = IntPtr((v.NativeArray |> NativePtr.toNativeInt).ToInt64() + sliceStart) |> NativePtr.ofNativeInt<bool>
                new DataBuffer<bool>(sliceLen, offsetAddr, true), memPool
            | UnaryFunction(v, f, _) -> 
                let v, memPool = BoolVectorExpr.EvalSlice v sliceStart sliceLen memPool
                if v.IsView then
                    let res = memPool.GetBoolVector(v.LongLength)
                    f v res
                    res, memPool
                else
                    f v v
                    v, memPool
            | BinaryFunction(v1, v2, f, _) -> 
                let v1, memPool = BoolVectorExpr.EvalSlice v1 sliceStart sliceLen memPool
                let v2, memPool = BoolVectorExpr.EvalSlice v2 sliceStart sliceLen memPool
                if not v1.IsView && v1.LongLength >= v2.LongLength then
                    f v1 v2 v1
                    if not v2.IsView then memPool.UnUseBool v2.NativeArray
                    v1, memPool
                elif not v2.IsView && v2.LongLength >= v1.LongLength then
                    f v1 v2 v2
                    if not v1.IsView then memPool.UnUseBool v1.NativeArray
                    v2, memPool
                else
                    let res = memPool.GetBoolVector(max v1.LongLength v2.LongLength)
                    f v1 v2 res
                    if not v1.IsView then memPool.UnUseBool v1.NativeArray
                    if not v2.IsView then memPool.UnUseBool v2.NativeArray
                    res, memPool
            | BinaryVectorFunction(v1, v2, f, _) -> 
                let (v1 : DataBuffer<float>), memPool = VectorExpr.EvalSlice v1 sliceStart sliceLen memPool
                let (v2 : DataBuffer<float>), memPool = VectorExpr.EvalSlice v2 sliceStart sliceLen memPool
                let res = memPool.GetBoolVector(max v1.LongLength v2.LongLength)
                f v1 v2 res
                if not v1.IsView then memPool.UnUseFloat v1.NativeArray
                if not v2.IsView then memPool.UnUseFloat v2.NativeArray
                res, memPool
            | IfFunction(b, v1, v2) -> 
                let boolVector, memPool = BoolVectorExpr.EvalSlice b sliceStart sliceLen memPool
                let v1, memPool = BoolVectorExpr.EvalSlice v1 sliceStart sliceLen memPool
                let v2, memPool = BoolVectorExpr.EvalSlice v2 sliceStart sliceLen memPool
                if not boolVector.IsView then
                    MklFunctions.B_IIf_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, boolVector.LongLength, boolVector.NativeArray, boolVector.NativeArray)
                    if not v1.IsView then memPool.UnUseBool v1.NativeArray
                    if not v2.IsView then memPool.UnUseBool v2.NativeArray
                    boolVector, memPool
                elif not v1.IsView && v1.LongLength >= boolVector.LongLength then
                    MklFunctions.B_IIf_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, boolVector.LongLength, boolVector.NativeArray, v1.NativeArray)
                    if not v2.IsView then memPool.UnUseBool v2.NativeArray
                    v1, memPool
                elif not v2.IsView && v2.LongLength >= boolVector.LongLength then
                    MklFunctions.B_IIf_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, boolVector.LongLength, boolVector.NativeArray, v2.NativeArray)
                    if not v1.IsView then memPool.UnUseBool v1.NativeArray
                    v2, memPool
                else
                    let res = memPool.GetBoolVector(boolVector.LongLength)
                    MklFunctions.B_IIf_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, boolVector.LongLength, boolVector.NativeArray, res.NativeArray)
                    if not v1.IsView then memPool.UnUseBool v1.NativeArray
                    if not v2.IsView then memPool.UnUseBool v2.NativeArray
                    res, memPool

    static member EvalIn(boolVectorExpr : BoolVectorExpr, res : BoolVector option) =
        let length = boolVectorExpr.Length
        let res = 
            match length with
                | None -> raise (new ArgumentException("Elementwise length mismatch"))
                | Some(len) ->
                    match res with
                        | Some(v) when len <> v.LongLength -> raise (new ArgumentException("Elementwise length mismatch")) 
                        | Some(v) -> v
                        | None -> new BoolVector(len, false)
        if res.IsDisposed then raise (new ObjectDisposedException(""))
        if res.LongLength <> 0L then
            let n = BoolVector.EvalSliceLength |> int64
            let len = res.LongLength
            let m = len / n
            let k = len % n
            use memPool = new MemoryPool()

            match boolVectorExpr with
                | Scalar(a) -> 
                    res.[0L] <- a
                | Var(v) ->
                    if res.NativeArray <> v.NativeArray then
                        MklFunctions.B_Copy_Array(v.LongLength, v.NativeArray, res.NativeArray)
                | UnaryFunction(v, f, _) -> 
                    for i in 0L..(m-1L) do
                        let sliceStart = i * n
                        let v, memPool = BoolVectorExpr.EvalSlice v sliceStart n memPool
                        let offsetAddr = IntPtr((res.NativeArray |> NativePtr.toNativeInt).ToInt64() + sliceStart) |> NativePtr.ofNativeInt<bool>
                        f v (new DataBuffer<bool>(n, offsetAddr, true))
                        memPool.UnUseAll()

                    if k > 0L then
                        let sliceStart = m * n
                        let v, _ = BoolVectorExpr.EvalSlice v sliceStart k memPool
                        let offsetAddr = IntPtr((res.NativeArray |> NativePtr.toNativeInt).ToInt64() + sliceStart) |> NativePtr.ofNativeInt<bool>
                        f v (new DataBuffer<bool>(k, offsetAddr, true))

                | BinaryFunction(v1, v2, f, _) -> 
                    for i in 0L..(m-1L) do
                        let sliceStart = i * n
                        let v1, memPool = BoolVectorExpr.EvalSlice v1 sliceStart n memPool
                        let v2, memPool = BoolVectorExpr.EvalSlice v2 sliceStart n memPool
                        let offsetAddr = IntPtr((res.NativeArray |> NativePtr.toNativeInt).ToInt64() + sliceStart) |> NativePtr.ofNativeInt<bool>
                        f v1 v2 (new DataBuffer<bool>(n, offsetAddr, true))
                        memPool.UnUseAll()

                    if k > 0L then
                        let sliceStart = m * n
                        let v1, memPool = BoolVectorExpr.EvalSlice v1 sliceStart k memPool
                        let v2, _ = BoolVectorExpr.EvalSlice v2 sliceStart k memPool
                        let offsetAddr = IntPtr((res.NativeArray |> NativePtr.toNativeInt).ToInt64() + sliceStart) |> NativePtr.ofNativeInt<bool>
                        f v1 v2 (new DataBuffer<bool>(k, offsetAddr, true))

                | BinaryVectorFunction(v1, v2, f, _) -> 
                    for i in 0L..(m-1L) do
                        let sliceStart = i * n
                        let v1, memPool = VectorExpr.EvalSlice v1 sliceStart n memPool
                        let v2, memPool = VectorExpr.EvalSlice v2 sliceStart n memPool
                        let offsetAddr = IntPtr((res.NativeArray |> NativePtr.toNativeInt).ToInt64() + sliceStart) |> NativePtr.ofNativeInt<bool>
                        f v1 v2 (new DataBuffer<bool>(n, offsetAddr, true))
                        memPool.UnUseAll()

                    if k > 0L then
                        let sliceStart = m * n
                        let v1, memPool = VectorExpr.EvalSlice v1 sliceStart k memPool
                        let v2, _ = VectorExpr.EvalSlice v2 sliceStart k memPool
                        let offsetAddr = IntPtr((res.NativeArray |> NativePtr.toNativeInt).ToInt64() + sliceStart) |> NativePtr.ofNativeInt<bool>
                        f v1 v2 (new DataBuffer<bool>(k, offsetAddr, true))

                | IfFunction(b, v1, v2) -> 
                    for i in 0L..(m-1L) do
                        let sliceStart = i * n
                        let boolVector, memPool = BoolVectorExpr.EvalSlice b sliceStart n memPool
                        let v1, memPool = BoolVectorExpr.EvalSlice v1 sliceStart n memPool
                        let v2, memPool = BoolVectorExpr.EvalSlice v2 sliceStart n memPool
                        let offsetAddr = IntPtr((res.NativeArray |> NativePtr.toNativeInt).ToInt64() + sliceStart) |> NativePtr.ofNativeInt<bool>
                        MklFunctions.B_IIf_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, boolVector.LongLength, boolVector.NativeArray, offsetAddr)
                        memPool.UnUseAll()

                    if k > 0L then
                        let sliceStart = m * n
                        let boolVector, memPool = BoolVectorExpr.EvalSlice b sliceStart k memPool
                        let v1, memPool = BoolVectorExpr.EvalSlice v1 sliceStart k memPool
                        let v2, _ = BoolVectorExpr.EvalSlice v2 sliceStart k memPool
                        let offsetAddr = IntPtr((res.NativeArray |> NativePtr.toNativeInt).ToInt64() + sliceStart) |> NativePtr.ofNativeInt<bool>
                        MklFunctions.B_IIf_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, boolVector.LongLength, boolVector.NativeArray, offsetAddr)

        res

    static member (.<) (vector1 : BoolVectorExpr, vector2 : BoolVectorExpr) =
        BinaryFunction(vector1, vector2, 
                       (fun v1 v2 res -> MklFunctions.B_Arrays_LessThan(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray)),
                       ".<")
    
    static member (.<) (vector1 : BoolVectorExpr, vector2 : BoolVector) =
        vector1 .< vector2.AsExpr

    static member (.<) (vector1 : BoolVector, vector2 : BoolVectorExpr) =
        vector1.AsExpr .< vector2

    static member (.<) (vector : BoolVectorExpr, a : bool) =
        vector .< Scalar(a)

    static member (.<) (a : bool, vector : BoolVectorExpr) =
        Scalar(a) .< vector


    static member (.<=) (vector1 : BoolVectorExpr, vector2 : BoolVectorExpr) =
        BinaryFunction(vector1, vector2, 
                       (fun v1 v2 res -> MklFunctions.B_Arrays_LessEqual(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray)),
                       ".<=")
    
    static member (.<=) (vector1 : BoolVectorExpr, vector2 : BoolVector) =
        vector1 .<= vector2.AsExpr

    static member (.<=) (vector1 : BoolVector, vector2 : BoolVectorExpr) =
        vector1.AsExpr .<= vector2

    static member (.<=) (vector : BoolVectorExpr, a : bool) =
        vector .<= Scalar(a)

    static member (.<=) (a : bool, vector : BoolVectorExpr) =
        Scalar(a) .<= vector


    static member (.>) (vector1 : BoolVectorExpr, vector2 : BoolVectorExpr) =
        BinaryFunction(vector1, vector2, 
                       (fun v1 v2 res -> MklFunctions.B_Arrays_GreaterThan(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray)),
                       ".>")
    
    static member (.>) (vector1 : BoolVectorExpr, vector2 : BoolVector) =
        vector1 .> vector2.AsExpr

    static member (.>) (vector1 : BoolVector, vector2 : BoolVectorExpr) =
        vector1.AsExpr .> vector2

    static member (.>) (vector : BoolVectorExpr, a : bool) =
        vector .> Scalar(a)

    static member (.>) (a : bool, vector : BoolVectorExpr) =
        Scalar(a) .> vector


    static member (.>=) (vector1 : BoolVectorExpr, vector2 : BoolVectorExpr) =
        BinaryFunction(vector1, vector2, 
                       (fun v1 v2 res -> MklFunctions.B_Arrays_GreaterEqual(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray)),
                       ".>=")
    
    static member (.>=) (vector1 : BoolVectorExpr, vector2 : BoolVector) =
        vector1 .>= vector2.AsExpr

    static member (.>=) (vector1 : BoolVector, vector2 : BoolVectorExpr) =
        vector1.AsExpr .>= vector2

    static member (.>=) (vector : BoolVectorExpr, a : bool) =
        vector .>= Scalar(a)

    static member (.>=) (a : bool, vector : BoolVectorExpr) =
        Scalar(a) .>= vector


    static member (.=) (vector1 : BoolVectorExpr, vector2 : BoolVectorExpr) =
        BinaryFunction(vector1, vector2, 
                       (fun v1 v2 res -> MklFunctions.B_Arrays_EqualElementwise(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray)),
                       ".=")
    
    static member (.=) (vector1 : BoolVectorExpr, vector2 : BoolVector) =
        vector1 .= vector2.AsExpr

    static member (.=) (vector1 : BoolVector, vector2 : BoolVectorExpr) =
        vector1.AsExpr .= vector2

    static member (.=) (vector : BoolVectorExpr, a : bool) =
        vector .= Scalar(a)

    static member (.=) (a : bool, vector : BoolVectorExpr) =
        Scalar(a) .= vector

    static member (.<>) (vector1 : BoolVectorExpr, vector2 : BoolVectorExpr) =
        BinaryFunction(vector1, vector2, 
                       (fun v1 v2 res -> MklFunctions.B_Arrays_NotEqualElementwise(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray)),
                       ".<>")
    
    static member (.<>) (vector1 : BoolVectorExpr, vector2 : BoolVector) =
        vector1 .<> vector2.AsExpr

    static member (.<>) (vector1 : BoolVector, vector2 : BoolVectorExpr) =
        vector1.AsExpr .<> vector2

    static member (.<>) (vector : BoolVectorExpr, a : bool) =
        vector .<> Scalar(a)

    static member (.<>) (a : bool, vector : BoolVectorExpr) =
        Scalar(a) .<> vector

    static member Min (vector1 : BoolVectorExpr, vector2 : BoolVectorExpr) =
        BinaryFunction(vector1, vector2, 
                       (fun v1 v2 res -> MklFunctions.B_Min_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray)),
                       "Min")

    static member Min (vector1 : BoolVectorExpr, vector2 : BoolVector) =
       BoolVectorExpr.Min(vector1, vector2.AsExpr)

    static member Min (vector1 : BoolVector, vector2 : BoolVectorExpr) =
        BoolVectorExpr.Min(vector1.AsExpr, vector2)

    static member Min (vector : BoolVectorExpr, a : bool) =
        BoolVectorExpr.Min(vector, Scalar(a))

    static member Min (a : bool, vector : BoolVectorExpr) =
        BoolVectorExpr.Min(Scalar(a), vector)

    static member Max (vector1 : BoolVectorExpr, vector2 : BoolVectorExpr) =
        BinaryFunction(vector1, vector2, 
                       (fun v1 v2 res -> MklFunctions.B_Max_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray)),
                       "Max")

    static member Max (vector1 : BoolVectorExpr, vector2 : BoolVector) =
       BoolVectorExpr.Max(vector1, vector2.AsExpr)

    static member Max (vector1 : BoolVector, vector2 : BoolVectorExpr) =
        BoolVectorExpr.Max(vector1.AsExpr, vector2)

    static member Max (vector : BoolVectorExpr, a : bool) =
        BoolVectorExpr.Max(vector, Scalar(a))

    static member Max (a : bool, vector : BoolVectorExpr) =
        BoolVectorExpr.Max(Scalar(a), vector)

    static member (.&&) (vector1 : BoolVectorExpr, vector2 : BoolVectorExpr) =
        BinaryFunction(vector1, vector2, 
                       (fun v1 v2 res -> MklFunctions.B_And_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray)),
                       ".&&")

    static member (.&&) (vector1 : BoolVectorExpr, vector2 : BoolVector) =
        vector1 .&& vector2.AsExpr

    static member (.&&) (vector1 : BoolVector, vector2 : BoolVectorExpr) =
        vector1.AsExpr .&& vector2

    static member (.&&) (vector : BoolVectorExpr, a : bool) =
        vector .&& Scalar(a)

    static member (.&&) (a : bool, vector : BoolVectorExpr) =
        Scalar(a) .&& vector

    static member (.||) (vector1 : BoolVectorExpr, vector2 : BoolVectorExpr) =
        BinaryFunction(vector1, vector2, 
                       (fun v1 v2 res -> MklFunctions.B_Or_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray)),
                       ".||")

    static member (.||) (vector1 : BoolVectorExpr, vector2 : BoolVector) =
        vector1 .|| vector2.AsExpr

    static member (.||) (vector1 : BoolVector, vector2 : BoolVectorExpr) =
        vector1.AsExpr .|| vector2

    static member (.||) (vector : BoolVectorExpr, a : bool) =
        vector .|| Scalar(a)

    static member (.||) (a : bool, vector : BoolVectorExpr) =
        Scalar(a) .|| vector

    static member Not (vector : BoolVectorExpr) =
        UnaryFunction(vector, (fun v res -> MklFunctions.B_Not_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Not")


//*******************************************Vector***********************************************************************************

and Vector (length : int64, nativeArray : nativeptr<float>, gcHandlePtr : IntPtr, isView : bool, parentVector : Vector option) as this =
    let mutable isDisposed = false
    do 
        if length < 0L then 
            GC.SuppressFinalize(this)
            raise (new ArgumentException("Vector length must be >= 0"))

    static let empty = new Vector(0L, IntPtr.Zero |> NativePtr.ofNativeInt<float>, IntPtr.Zero, false, None)

    static let mutable evalSliceLength = 100000

    new(length : int64, init : float) =
        let mutable arr = IntPtr.Zero
        if length < 0L then 
             new Vector(length, arr |> NativePtr.ofNativeInt<float>, IntPtr.Zero, false, None)
        elif length = 0L then
            new Vector(0L, IntPtr.Zero |> NativePtr.ofNativeInt<float>, IntPtr.Zero, false, None)
        else
            let nativeArrayPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<FloatPtr>
            if init = 0.0 then
                MklFunctions.D_Create_Zero_Array(length, nativeArrayPtr)
                let nativeArray = arr |> NativePtr.ofNativeInt<float>
                new Vector(length, nativeArray, IntPtr.Zero, false, None)
            else
                MklFunctions.D_Create_Array(length, nativeArrayPtr)
                let nativeArray = arr |> NativePtr.ofNativeInt<float>
                MklFunctions.D_Fill_Array(init, length, nativeArray)
                new Vector(length, nativeArray, IntPtr.Zero, false, None)

    new(length : int, init : float) =
        let length = length |> int64
        new Vector(length, init)

    new(data : float seq) =
        let data = data |> Seq.toArray
        let length = data.GetLongLength(0)
        if length = 0L then
            new Vector(0L, IntPtr.Zero |> NativePtr.ofNativeInt<float>, IntPtr.Zero, false, None)
        else
            let mutable arr = IntPtr.Zero
            let nativeArrayPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<FloatPtr>
            MklFunctions.D_Create_Array(length, nativeArrayPtr)
            let gcHandle = GCHandle.Alloc(data, GCHandleType.Pinned)
            MklFunctions.D_Copy_Array(length, gcHandle.AddrOfPinnedObject() |> NativePtr.ofNativeInt<float>, arr |> NativePtr.ofNativeInt<float>) 
            gcHandle.Free()
            new Vector(length, arr |> NativePtr.ofNativeInt<float>, IntPtr.Zero, false, None)

    new(data : float[], copyData : bool) =
        if copyData then
            new Vector(data)
        else
            let length = data.GetLongLength(0)
            if length = 0L then
                new Vector(0L, IntPtr.Zero |> NativePtr.ofNativeInt<float>, IntPtr.Zero, false, None)
            else
                let gcHandle = GCHandle.Alloc(data, GCHandleType.Pinned)
                new Vector(length, gcHandle.AddrOfPinnedObject() |> NativePtr.ofNativeInt<float>, GCHandle.ToIntPtr(gcHandle), true, None)

    new(data : float) = new Vector(1L, data)

    new(length : int, initializer : int -> float) =
        let data = Array.init length initializer
        new Vector(data, false)


    member this.Length = length |> int

    member this.LongLength = length

    member this.NativeArray = nativeArray

    member internal this.DataBuffer = new DataBuffer<float>(length, nativeArray, isView)

    member this.IsDisposed =
        if length = 0L then false
        else
            match parentVector with
                | Some(p) -> isDisposed || p.IsDisposed
                | None -> isDisposed

    member this.IsView = isView

    static member Empty = empty

    static member op_Explicit(v : float) = new Vector(v)

    static member op_Explicit(v : float seq) = new Vector(v)

    static member op_Explicit(v : float list) = new Vector(v)

    static member op_Explicit(v : float array) = new Vector(v)

    static member op_Explicit(v : Vector) = v

    static member op_Explicit(v : Vector) =
        v.[0]

    static member op_Explicit(v : Vector) = v.AsExpr

    static member EvalSliceLength
        with get() = evalSliceLength
        and set(value) = evalSliceLength <- value

    member this.View
        with get(fromIndex, toIndex) =
            if isDisposed then raise (new ObjectDisposedException(""))
            if fromIndex < 0L || fromIndex >= length then raise (new IndexOutOfRangeException())
            if toIndex < 0L || toIndex >= length then raise (new IndexOutOfRangeException())
            if fromIndex > toIndex then Vector.Empty
            else
                let length = toIndex - fromIndex + 1L
                let sizeof = sizeof<float> |> int64
                let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + fromIndex*sizeof) |> NativePtr.ofNativeInt<float>
                new Vector(length, offsetAddr, IntPtr.Zero, true, Some this)

    member this.View
        with get(fromIndex : int, toIndex : int) = this.View(int64(fromIndex), int64(toIndex))

    member this.GetSlice(fromIndex, toIndex) =
        if this.IsDisposed then raise (new ObjectDisposedException(""))
        let fromIndex = defaultArg fromIndex 0L
        let toIndex = defaultArg toIndex (length - 1L)
        if fromIndex < 0L || fromIndex >= length then raise (new IndexOutOfRangeException())
        if toIndex < 0L || toIndex >= length then raise (new IndexOutOfRangeException())
        if fromIndex > toIndex then Vector.Empty
        else
            let length = toIndex - fromIndex + 1L
            let view = this.View(fromIndex, toIndex)
            let mutable arr = IntPtr.Zero
            let nativeArrayPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<FloatPtr>
            MklFunctions.D_Create_Array(length, nativeArrayPtr)
            MklFunctions.D_Copy_Array(length, view.NativeArray, arr |> NativePtr.ofNativeInt<float>) 
            new Vector(length, arr |> NativePtr.ofNativeInt<float>, IntPtr.Zero, false, None)

    member this.GetSlice(fromIndex : int option, toIndex : int option) =
        this.GetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64)

    member this.SetSlice(fromIndex, toIndex, value : float) =
        if this.IsDisposed then raise (new ObjectDisposedException(""))
        let fromIndex = defaultArg fromIndex 0L
        let toIndex = defaultArg toIndex (length - 1L)
        if fromIndex < 0L || fromIndex >= length then raise (new IndexOutOfRangeException())
        if toIndex < 0L || toIndex >= length then raise (new IndexOutOfRangeException())
        if fromIndex > toIndex then ()
        else
            let length = toIndex - fromIndex + 1L
            let view = this.View(fromIndex, toIndex)
            MklFunctions.D_Fill_Array(value, length, view.NativeArray)

    member this.SetSlice(fromIndex : int option, toIndex : int option, value : float) =
        this.SetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64, value)

    member this.SetSlice(fromIndex : int64 option, toIndex : int64 option, value: Vector) =
        if this.IsDisposed then raise (new ObjectDisposedException(""))
        if value.IsDisposed then raise (new ObjectDisposedException(""))
        if value.LongLength = 1L then
            this.SetSlice(fromIndex, toIndex, (value.[0L]:float))
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
                MklFunctions.D_Copy_Array(length, value.NativeArray, view.NativeArray)

    member this.SetSlice(fromIndex : int option, toIndex : int option, value: Vector) =
        this.SetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64, value)


    member this.Item
        with get(i : int64) =
            if isDisposed then raise (new ObjectDisposedException(""))
            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + i*8L) |> NativePtr.ofNativeInt<float>
            NativePtr.read offsetAddr  
        and set (i : int64) value =
            if isDisposed then raise (new ObjectDisposedException(""))
            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + i*8L) |> NativePtr.ofNativeInt<float>
            NativePtr.write offsetAddr value

    member this.Item
        with get(i : int) =
            if isDisposed then raise (new ObjectDisposedException(""))
            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + int64(i)*8L) |> NativePtr.ofNativeInt<float>
            NativePtr.read offsetAddr 
        and set (i : int) value =
            if isDisposed then raise (new ObjectDisposedException(""))
            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + int64(i)*8L) |> NativePtr.ofNativeInt<float>
            NativePtr.write offsetAddr value

    member this.Item
        with get(indices : int64 seq) = 
            if this.IsDisposed then raise (new ObjectDisposedException(""))
            let indices = indices |> Seq.toArray
            let length = indices.GetLongLength(0)
            let res = new Vector(length, 0.0)
            indices |> Array.iteri (fun i index -> res.[i] <- this.[index])
            res
        and set (indices : int64 seq) (value : Vector) =
            if this.IsDisposed then raise (new ObjectDisposedException(""))
            if value.IsDisposed then raise (new ObjectDisposedException(""))
            let indices = indices |> Seq.toArray
            if value.LongLength = 1L then
                let value = value.[0L]
                indices |> Array.iteri (fun i index -> this.[index] <- value)
            else
                indices |> Array.iteri (fun i index -> this.[index] <- value.[i])

    member this.Item
        with get(indices : int seq) = 
            if this.IsDisposed then raise (new ObjectDisposedException(""))
            let indices = indices |> Seq.toArray
            let length = indices.GetLongLength(0)
            let res = new Vector(length, 0.0)
            indices |> Array.iteri (fun i index -> res.[i] <- this.[index])
            res
        and set (indices : int seq) (value : Vector) =
            if this.IsDisposed then raise (new ObjectDisposedException(""))
            if value.IsDisposed then raise (new ObjectDisposedException(""))
            if value.LongLength = 1L then
                indices |> Seq.iter (fun index -> this.[index] <- value.[0])
            else
                indices |> Seq.iteri (fun i index -> this.[index] <- value.[i])

    member this.Item
        with get(boolVector : BoolVector) = 
            if this.IsDisposed then raise (new ObjectDisposedException(""))
            if boolVector.IsDisposed then raise (new ObjectDisposedException(""))
            if length <> boolVector.LongLength then raise (new ArgumentException("Vector length mismatch"))
            let mutable arr = IntPtr.Zero
            let mutable resLen = 0L
            let nativeArrPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<FloatPtr>
            MklFunctions.D_Get_Bool_Slice(length, nativeArray, boolVector.NativeArray, nativeArrPtr, &&resLen)
            new Vector(resLen, arr |> NativePtr.ofNativeInt<float>, IntPtr.Zero, false, None)

        and set (boolVector : BoolVector) (value : Vector) =
            if this.IsDisposed then raise (new ObjectDisposedException(""))
            if value.IsDisposed then raise (new ObjectDisposedException(""))
            if boolVector.IsDisposed then raise (new ObjectDisposedException(""))
            if length <> boolVector.LongLength then raise (new ArgumentException("Vector length mismatch"))
            MklFunctions.D_Set_Bool_Slice(length, nativeArray, boolVector.NativeArray, value.NativeArray, value.LongLength)


    member this.ToArray() =
        if this.IsDisposed then raise (new ObjectDisposedException(""))
        Array.init this.Length (fun i -> this.[i])

    member this.AsExpr
        with get() = 
            if this.IsDisposed then raise (new ObjectDisposedException(""))
            if length = 1L then VectorExpr.Scalar(this.[0L])
            else VectorExpr.Var(this)

    static member Concat(vectors : Vector seq) =
        vectors |> Seq.iter (fun v -> if v.IsDisposed then raise (new ObjectDisposedException("")))
        let vectors = vectors |> Seq.filter (fun v -> v.LongLength <> 0L) |> Seq.toArray
        if vectors.Length = 0 then Vector.Empty
        else
            let length = vectors |> Array.map (fun v -> v.LongLength) |> Array.reduce (+)
            let res = new Vector(length, 0.0)
            vectors |> Array.fold (fun offset v ->
                                     res.View(offset, offset + v.LongLength - 1L).SetSlice(Some(0L), None, v)
                                     offset + v.LongLength
                                ) 0L |> ignore
            res

    static member Copy(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        if vector.LongLength = 0L then Vector.Empty
        else
            let res = new Vector(vector.LongLength, 0.0)
            MklFunctions.D_Copy_Array(vector.LongLength, vector.NativeArray, res.NativeArray)
            res


    static member (==) (vector1: Vector, vector2: Vector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        vector1 = vector2

    static member (!=) (vector1: Vector, vector2: Vector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        vector1 <> vector2

    static member (==) (vector: Vector, a : float) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        vector.LongLength = 1L && vector.[0L] = a

    static member (!=) (vector: Vector, a : float) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        not (vector == a)

    static member (==) (a : float, vector: Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        vector.LongLength = 1L && vector.[0L] = a

    static member (!=) (a : float, vector: Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        not (vector == a)



    static member (.<) (vector1: Vector, vector2: Vector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.D_Arrays_LessThan(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.<=) (vector1: Vector, vector2: Vector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.D_Arrays_LessEqual(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.>) (vector1: Vector, vector2: Vector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.D_Arrays_GreaterThan(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.>=) (vector1: Vector, vector2: Vector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.D_Arrays_GreaterEqual(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.=) (vector1: Vector, vector2: Vector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.D_Arrays_EqualElementwise(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.<>) (vector1: Vector, vector2: Vector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.D_Arrays_NotEqualElementwise(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res



    static member (.<) (vector: Vector, a : float) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new Vector(a)
        MklFunctions.D_Arrays_LessThan(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res

    static member (.<=) (vector: Vector, a : float) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new Vector(a)
        MklFunctions.D_Arrays_LessEqual(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res

    static member (.>) (vector: Vector, a : float) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new Vector(a)
        MklFunctions.D_Arrays_GreaterThan(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res

    static member (.>=) (vector: Vector, a : float) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new Vector(a)
        MklFunctions.D_Arrays_GreaterEqual(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res

    static member (.=) (vector: Vector, a : float) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new Vector(a)
        MklFunctions.D_Arrays_EqualElementwise(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res

    static member (.<>) (vector: Vector, a : float) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new Vector(a)
        MklFunctions.D_Arrays_NotEqualElementwise(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res



    static member (.<) (a : float, vector: Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        vector .> a

    static member (.<=) (a : float, vector: Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        vector .>= a

    static member (.>) (a : float, vector: Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        vector .< a

    static member (.>=) (a : float, vector: Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        vector .<= a

    static member (.=) (a : float, vector: Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        vector .= a

    static member (.<>) (a : float, vector: Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        vector .<> a


    static member Max(vector1 : Vector, vector2 : Vector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new Vector(length, 0.0)
        MklFunctions.D_Max_Arrays(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member Min(vector1 : Vector, vector2 : Vector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new Vector(length, 0.0)
        MklFunctions.D_Min_Arrays(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member Max(vector : Vector, a : float) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let a = new Vector(a)
        Vector.Max(vector, a)

    static member Min(vector : Vector, a : float) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let a = new Vector(a)
        Vector.Min(vector, a)

    static member Max(a : float, vector : Vector) = 
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        Vector.Max(vector, a)

    static member Min(a : float, vector : Vector) = 
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        Vector.Min(vector, a)


    static member (*) (vector1 : Vector, vector2 : Vector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        if vector1.LongLength <> vector2.LongLength then raise (new ArgumentException("Vector lengths must be equal"))
        if vector1.LongLength = 0L || vector2.LongLength = 0L then raise (new ArgumentException("Vector lengths must be > 0"))
        MklFunctions.D_Inner_Product(vector1.LongLength, vector1.NativeArray, vector2.NativeArray)

    static member (.*) (a: float, vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Scalar_Mul_Array(a, len, vector.NativeArray, res.NativeArray)
        res 

    static member (*) (a: float, vector : Vector) =
        a .* vector

    static member (.*) (vector : Vector, a :  float) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        a .* vector

    static member (*) (vector : Vector, a :  float) =
        vector .* a

    static member (.*) (vector1 : Vector, vector2 : Vector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        if vector1.LongLength = 1L then
            vector1.[0] .* vector2
        elif vector2.LongLength = 1L then
            vector2.[0] .* vector1
        else
           let len = vector1.LongLength
           let res = new Vector(len, 0.0)
           MklFunctions.D_Array_Mul_Array(len, vector1.NativeArray, vector2.NativeArray, res.NativeArray)
           res

    static member (+) (a: float, vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Scalar_Add_Array(a, len, vector.NativeArray, res.NativeArray)
        res 

    static member (+) (vector : Vector, a :  float) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        a + vector

    static member (+) (vector1 : Vector, vector2 : Vector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        if vector1.LongLength = 1L then
            vector1.[0] + vector2
        elif vector2.LongLength = 1L then
            vector2.[0] + vector1
        else
           let len = vector1.LongLength
           let res = new Vector(len, 0.0)
           MklFunctions.D_Array_Add_Array(len, vector1.NativeArray, vector2.NativeArray, res.NativeArray)
           res

    static member (./) (a: float, vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Scalar_Div_Array(a, len, vector.NativeArray, res.NativeArray)
        res 

    static member (/) (a: float, vector : Vector) =
        a ./ vector

    static member (./) (vector : Vector, a :  float) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Array_Div_Scalar(a, len, vector.NativeArray, res.NativeArray)
        res 

    static member (/) (vector : Vector, a :  float) =
        vector ./ a

    static member (./) (vector1 : Vector, vector2 : Vector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        if vector1.LongLength = 1L then
            vector1.[0] ./ vector2
        elif vector2.LongLength = 1L then
            vector1 ./ vector2.[0] 
        else
           let len = vector1.LongLength
           let res = new Vector(len, 0.0)
           MklFunctions.D_Array_Div_Array(len, vector1.NativeArray, vector2.NativeArray, res.NativeArray)
           res

    static member (-) (a: float, vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Scalar_Sub_Array(a, len, vector.NativeArray, res.NativeArray)
        res 

    static member (-) (vector : Vector, a :  float) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Array_Sub_Scalar(a, len, vector.NativeArray, res.NativeArray)
        res 

    static member (-) (vector1 : Vector, vector2 : Vector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        if vector1.LongLength = 1L then
            vector1.[0] - vector2
        elif vector2.LongLength = 1L then
            vector1 - vector2.[0] 
        else
           let len = vector1.LongLength
           let res = new Vector(len, 0.0)
           MklFunctions.D_Array_Sub_Array(len, vector1.NativeArray, vector2.NativeArray, res.NativeArray)
           res

    static member (~-) (vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Minus_Array(len, vector.NativeArray, res.NativeArray)
        res   
        
    static member (.^) (a: float, vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Scalar_Pow_Array(a, len, vector.NativeArray, res.NativeArray)
        res 

    static member (.^) (vector : Vector, a :  float) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Array_Pow_scalar(a, len, vector.NativeArray, res.NativeArray)
        res  
        
    static member (.^) (vector1 : Vector, vector2 : Vector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        if vector1.LongLength = 1L then
            vector1.[0] .^ vector2
        elif vector2.LongLength = 1L then
            vector1 .^ vector2.[0] 
        else
           let len = vector1.LongLength
           let res = new Vector(len, 0.0)
           MklFunctions.D_Array_Pow_Array(len, vector1.NativeArray, vector2.NativeArray, res.NativeArray)
           res

    static member (.^) (vector : Vector, n :  int) =
        vector .^ float(n)


    static member Abs(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Abs_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Sqrt(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Sqrt_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Sin(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Sin_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Cos(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Cos_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Tan(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Tan_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Asin(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_ASin_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Acos(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_ACos_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Atan(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_ATan_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Sinh(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Sinh_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Cosh(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Cosh_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Tanh(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Tanh_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member ASinh(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_ASinh_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member ACosh(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_ACosh_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member ATanh(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_ATanh_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Exp(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Exp_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Expm1(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Expm1_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Log(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Ln_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Log10(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Log10_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Log1p(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Log1p_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Erf(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Erf_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Erfc(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Erfc_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Erfinv(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Erfinv_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Erfcinv(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Erfcinv_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Normcdf(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_CdfNorm_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Norminv(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_CdfNormInv_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Round(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Round_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Ceiling(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Ceil_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Floor(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Floor_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Truncate(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Trunc_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Axpby(a : float, vector1 : Vector, b : float, vector2 : Vector) =
        if vector1.IsDisposed then raise (new ObjectDisposedException(""))
        if vector2.IsDisposed then raise (new ObjectDisposedException(""))
        if vector1.LongLength = 0L && vector2.LongLength = 0L then Vector.Empty
        else
            if vector1.LongLength <> vector2.LongLength then raise (new ArgumentException("Vector length mismatch"))

            let len = vector1.LongLength
            let res = new Vector(len, 0.0)
            MklFunctions.D_Array_Axpby_Array(len, vector1.NativeArray, a, vector2.NativeArray, b, res.NativeArray)
            res



    static member Sum(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        if vector.LongLength = 0L then raise (new ArgumentException("Vector must have length > 0"))
        let mutable res = 0.0
        MklFunctions.D_Sum_Matrix(false, 1L, vector.LongLength, vector.NativeArray, &&res)
        res

    static member Prod(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        if vector.LongLength = 0L then raise (new ArgumentException("Vector must have length > 0"))
        let mutable res = 0.0
        MklFunctions.D_Prod_Matrix(false, 1L, vector.LongLength, vector.NativeArray, &&res)
        res

    static member CumSum(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        if vector.LongLength = 0L then Vector.Empty
        else
            let res = new Vector(vector.LongLength, 0.0)
            MklFunctions.D_CumSum_Matrix(false, 1L, vector.LongLength, vector.NativeArray, res.NativeArray)
            res

    static member CumProd(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        if vector.LongLength = 0L then Vector.Empty
        else
            let res = new Vector(vector.LongLength, 0.0)
            MklFunctions.D_CumProd_Matrix(false, 1L, vector.LongLength, vector.NativeArray, res.NativeArray)
            res

    static member Min(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        if vector.LongLength = 0L then raise (new ArgumentException("Vector must have length > 0"))
        if vector.LongLength = 1L then
            vector.[0]
        else 
            let mutable res = 0.0
            MklFunctions.D_Min_Matrix(false, 1L, vector.LongLength, vector.NativeArray, &&res)
            res

    static member Max(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        if vector.LongLength = 0L then raise (new ArgumentException("Vector must have length > 0"))
        if vector.LongLength = 1L then
            vector.[0]
        else
            let mutable res = 0.0
            MklFunctions.D_Max_Matrix(false, 1L, vector.LongLength, vector.NativeArray, &&res)
            res

    static member Mean(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        if vector.LongLength = 0L then raise (new ArgumentException("Vector must have length > 0"))
        let mutable res = 0.0
        MklFunctions.D_Mean_Matrix(false, 1L, vector.LongLength, vector.NativeArray, &&res)
        res

    static member Variance(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        if vector.LongLength = 0L then raise (new ArgumentException("Vector must have length > 0"))
        elif vector.LongLength = 1L then
            if Double.IsInfinity(vector.[0]) || Double.IsNaN(vector.[0]) then Double.NaN
            else 0.0
        else
            let mutable res = 0.0
            MklFunctions.D_Variance_Matrix(false, 1L, vector.LongLength, vector.NativeArray, &&res)
            res

    static member Skewness(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        if vector.LongLength <= 1L then raise (new ArgumentException("Vector must have length > 1"))
        let mutable res = 0.0
        MklFunctions.D_Skewness_Matrix(false, 1L, vector.LongLength, vector.NativeArray, &&res)
        res

    static member Kurtosis(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        if vector.LongLength <= 1L then raise (new ArgumentException("Vector must have length > 1"))
        let mutable res = 0.0
        MklFunctions.D_Kurtosis_Matrix(false, 1L, vector.LongLength, vector.NativeArray, &&res)
        res

    static member Quantile(vector : Vector) =
        if vector.IsDisposed then raise (new ObjectDisposedException(""))
        fun (quantileOrders : Vector) ->
            if quantileOrders.LongLength = 0L then raise (new ArgumentException("Quantile orders vector must not be empty"))
            if vector.LongLength = 0L then Vector.Empty
            else
                let res = new Vector(quantileOrders.LongLength, 0.0)
                MklFunctions.D_Quantiles_Matrix(false, 1L, vector.LongLength, quantileOrders.LongLength, vector.NativeArray, quantileOrders.NativeArray, res.NativeArray)
                res

    override this.ToString() = 
        if this.IsDisposed then raise (new ObjectDisposedException(""))
        (this:>IFormattable).ToString(GenericFormatting.GenericFormat.Instance.GetFormat<float>() 0.0, null)

    override this.Equals(yobj) =
        match yobj with
        | :? Vector as y ->
            if this.IsDisposed then raise (new ObjectDisposedException(""))
            if y.IsDisposed then raise (new ObjectDisposedException(""))
            if this.LongLength = 0L && y.LongLength = 0L then true
            elif this.LongLength <> y.LongLength then false
            else 
                MklFunctions.D_Arrays_Are_Equal(this.LongLength, this.NativeArray, y.NativeArray)
        | _ -> false
 
    override this.GetHashCode() = 
        if this.IsDisposed then raise (new ObjectDisposedException(""))
        hash (this.LongLength, this.NativeArray)

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

//************************************************VectorExpr*******************************************************************************

and VectorExpr = 
    | Scalar of float
    | Var of Vector
    | UnaryFunction of VectorExpr * (DataBuffer<float> -> DataBuffer<float> -> unit) * string
    | BinaryFunction of VectorExpr * VectorExpr * (DataBuffer<float> -> DataBuffer<float> -> DataBuffer<float> -> unit) * string
    | IfFunction of BoolVectorExpr * VectorExpr * VectorExpr

    member this.Length =
        match this with
            | Scalar(_) -> Some 1L
            | Var(v) -> Some v.LongLength
            | UnaryFunction(v, _, _) -> v.Length
            | BinaryFunction(v1, v2, _, _) -> ArgumentChecks.getElementwiseLength v1.Length v2.Length
            | IfFunction(v1, v2, v3) -> ArgumentChecks.getElementwiseLengthIf v1.Length v2.Length v3.Length

    static member FuseAxpby(vectorExpr : VectorExpr) =
        match vectorExpr with
            | BinaryFunction(BinaryFunction(Scalar(a), X, _ , ".*"), BinaryFunction(Scalar(b), Y, _, ".*"), _, "+") ->
                BinaryFunction(X, Y, (fun v1 v2 v3 -> MklFunctions.D_Array_Axpby_Array(v1.LongLength, v1.NativeArray, a, v2.NativeArray, b, v3.NativeArray)), "axpby")
            | BinaryFunction(BinaryFunction(X, Scalar(a), _ , ".*"), BinaryFunction(Scalar(b), Y, _, ".*"), _, "+") ->
                BinaryFunction(X, Y, (fun v1 v2 v3 -> MklFunctions.D_Array_Axpby_Array(v1.LongLength, v1.NativeArray, a, v2.NativeArray, b, v3.NativeArray)), "axpby")
            | BinaryFunction(BinaryFunction(Scalar(a), X, _ , ".*"), BinaryFunction(Y, Scalar(b), _, ".*"), _, "+") ->
                BinaryFunction(X, Y, (fun v1 v2 v3 -> MklFunctions.D_Array_Axpby_Array(v1.LongLength, v1.NativeArray, a, v2.NativeArray, b, v3.NativeArray)), "axpby")
            | BinaryFunction(BinaryFunction(X, Scalar(a), _ , ".*"), BinaryFunction(Y, Scalar(b), _, ".*"), _, "+") ->
                BinaryFunction(X, Y, (fun v1 v2 v3 -> MklFunctions.D_Array_Axpby_Array(v1.LongLength, v1.NativeArray, a, v2.NativeArray, b, v3.NativeArray)), "axpby")
            | _ -> vectorExpr

    static member EvalSlice (vectorExpr : VectorExpr) (sliceStart : int64) (sliceLen : int64) (memPool : MemoryPool) =
        match vectorExpr with
            | Scalar(a) -> 
                let v : DataBuffer<float> = memPool.GetVector(1L)
                NativePtr.write v.NativeArray a
                v, memPool
            | Var(v) ->
                let offsetAddr = IntPtr((v.NativeArray |> NativePtr.toNativeInt).ToInt64() + sliceStart*8L) |> NativePtr.ofNativeInt<float>
                new DataBuffer<float>(sliceLen, offsetAddr, true), memPool
            | UnaryFunction(v, f, _) -> 
                let v, memPool = VectorExpr.EvalSlice v sliceStart sliceLen memPool
                if not v.IsView then
                    f v v
                    v, memPool
                else
                    let res = memPool.GetVector(v.LongLength)
                    f v res
                    res, memPool
            | BinaryFunction(v1, v2, f, _) -> 
                let v1, memPool = VectorExpr.EvalSlice v1 sliceStart sliceLen memPool
                let v2, memPool = VectorExpr.EvalSlice v2 sliceStart sliceLen memPool
                if not v1.IsView && v1.LongLength >= v2.LongLength then
                    f v1 v2 v1
                    if not v2.IsView then memPool.UnUseFloat v2.NativeArray
                    v1, memPool
                elif not v2.IsView && v2.LongLength >= v1.LongLength then
                    f v1 v2 v2
                    if not v1.IsView then memPool.UnUseFloat v1.NativeArray
                    v2, memPool
                else
                    let res = memPool.GetVector(max v1.LongLength v2.LongLength)
                    f v1 v2 res
                    if not v1.IsView then memPool.UnUseFloat v1.NativeArray
                    if not v2.IsView then memPool.UnUseFloat v2.NativeArray
                    res, memPool
            | IfFunction(b, v1, v2) -> 
                let boolVector, memPool = BoolVectorExpr.EvalSlice b sliceStart sliceLen memPool
                let v1, memPool = VectorExpr.EvalSlice v1 sliceStart sliceLen memPool
                let v2, memPool = VectorExpr.EvalSlice v2 sliceStart sliceLen memPool
                if not v1.IsView && v1.LongLength >= boolVector.LongLength then
                    MklFunctions.D_IIf_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, boolVector.LongLength, boolVector.NativeArray, v1.NativeArray)
                    if not v2.IsView then memPool.UnUseFloat v2.NativeArray
                    if not boolVector.IsView then memPool.UnUseBool boolVector.NativeArray
                    v1, memPool
                elif not v2.IsView && v2.LongLength >= boolVector.LongLength then
                    MklFunctions.D_IIf_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, boolVector.LongLength, boolVector.NativeArray, v2.NativeArray)
                    if not boolVector.IsView then memPool.UnUseBool boolVector.NativeArray
                    if not v1.IsView then memPool.UnUseFloat v1.NativeArray
                    v2, memPool
                else
                    let res = memPool.GetVector(boolVector.LongLength)
                    MklFunctions.D_IIf_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, boolVector.LongLength, boolVector.NativeArray, res.NativeArray)
                    if not boolVector.IsView then memPool.UnUseBool boolVector.NativeArray
                    if not v1.IsView then memPool.UnUseFloat v1.NativeArray
                    if not v2.IsView then memPool.UnUseFloat v2.NativeArray
                    res, memPool

    static member EvalIn(vectorExpr : VectorExpr, res : Vector option) =
        let vectorExpr = vectorExpr |> VectorExpr.FuseAxpby
        let length = vectorExpr.Length
        let res = 
            match length with
                | None -> raise (new ArgumentException("Elementwise length mismatch"))
                | Some(len) ->
                    match res with
                        | Some(v) when len <> v.LongLength -> raise (new ArgumentException("Elementwise length mismatch")) 
                        | Some(v) -> v
                        | None -> new Vector(len, 0.0)
        if res.IsDisposed then raise (new ObjectDisposedException(""))
        if res.LongLength <> 0L then
            let n = Vector.EvalSliceLength |> int64
            let len = res.LongLength
            let m = len / n
            let k = len % n
            use memPool = new MemoryPool()

            match vectorExpr with
                | Scalar(a) -> 
                    res.[0L] <- a
                | Var(v) ->
                    if res.NativeArray <> v.NativeArray then
                        MklFunctions.D_Copy_Array(v.LongLength, v.NativeArray, res.NativeArray)
                | UnaryFunction(v, f, _) -> 
                    for i in 0L..(m-1L) do
                        let sliceStart = i * n
                        let v, memPool = VectorExpr.EvalSlice v sliceStart n memPool
                        let offsetAddr = IntPtr((res.NativeArray |> NativePtr.toNativeInt).ToInt64() + sliceStart*8L) |> NativePtr.ofNativeInt<float>
                        f v (new DataBuffer<float>(n, offsetAddr, true))
                        memPool.UnUseAll()

                    if k > 0L then
                        let sliceStart = m * n
                        let v, _ = VectorExpr.EvalSlice v sliceStart k memPool
                        let offsetAddr = IntPtr((res.NativeArray |> NativePtr.toNativeInt).ToInt64() + sliceStart*8L) |> NativePtr.ofNativeInt<float>
                        f v (new DataBuffer<float>(k, offsetAddr, true))

                | BinaryFunction(v1, v2, f, _) -> 
                    for i in 0L..(m-1L) do
                        let sliceStart = i * n
                        let v1, memPool = VectorExpr.EvalSlice v1 sliceStart n memPool
                        let v2, memPool = VectorExpr.EvalSlice v2 sliceStart n memPool
                        let offsetAddr = IntPtr((res.NativeArray |> NativePtr.toNativeInt).ToInt64() + sliceStart*8L) |> NativePtr.ofNativeInt<float>
                        f v1 v2 (new DataBuffer<float>(n, offsetAddr, true))
                        memPool.UnUseAll()

                    if k > 0L then
                        let sliceStart = m * n
                        let v1, memPool = VectorExpr.EvalSlice v1 sliceStart k memPool
                        let v2, _ = VectorExpr.EvalSlice v2 sliceStart k memPool
                        let offsetAddr = IntPtr((res.NativeArray |> NativePtr.toNativeInt).ToInt64() + sliceStart*8L) |> NativePtr.ofNativeInt<float>
                        f v1 v2 (new DataBuffer<float>(k, offsetAddr, true))

                | IfFunction(b, v1, v2) -> 
                    for i in 0L..(m-1L) do
                        let sliceStart = i * n
                        let boolVector, memPool = BoolVectorExpr.EvalSlice b sliceStart n memPool
                        let v1, memPool = VectorExpr.EvalSlice v1 sliceStart n memPool
                        let v2, memPool = VectorExpr.EvalSlice v2 sliceStart n memPool
                        let offsetAddr = IntPtr((res.NativeArray |> NativePtr.toNativeInt).ToInt64() + sliceStart*8L) |> NativePtr.ofNativeInt<float>
                        MklFunctions.D_IIf_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, boolVector.LongLength, boolVector.NativeArray, offsetAddr)
                        memPool.UnUseAll()

                    if k > 0L then
                        let sliceStart = m * n
                        let boolVector, memPool = BoolVectorExpr.EvalSlice b sliceStart k memPool
                        let v1, memPool = VectorExpr.EvalSlice v1 sliceStart k memPool
                        let v2, _ = VectorExpr.EvalSlice v2 sliceStart k memPool
                        let offsetAddr = IntPtr((res.NativeArray |> NativePtr.toNativeInt).ToInt64() + sliceStart*8L) |> NativePtr.ofNativeInt<float>
                        MklFunctions.D_IIf_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, boolVector.LongLength, boolVector.NativeArray, offsetAddr)

        res

    static member (.<) (vector1 : VectorExpr, vector2 : VectorExpr) =
        BinaryVectorFunction(vector1, vector2, 
                             (fun v1 v2 res -> MklFunctions.D_Arrays_LessThan(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray)),
                             ".<")
    
    static member (.<) (vector1 : VectorExpr, vector2 : Vector) =
        vector1 .< vector2.AsExpr

    static member (.<) (vector1 : Vector, vector2 : VectorExpr) =
        vector1.AsExpr .< vector2

    static member (.<) (vector : VectorExpr, a : float) =
        vector .< Scalar(a)

    static member (.<) (a : float, vector : VectorExpr) =
        Scalar(a) .< vector

    static member (.<=) (vector1 : VectorExpr, vector2 : VectorExpr) =
        BinaryVectorFunction(vector1, vector2, 
                             (fun v1 v2 res -> MklFunctions.D_Arrays_LessEqual(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray)),
                             ".<=")
    
    static member (.<=) (vector1 : VectorExpr, vector2 : Vector) =
        vector1 .<= vector2.AsExpr

    static member (.<=) (vector1 : Vector, vector2 : VectorExpr) =
        vector1.AsExpr .<= vector2

    static member (.<=) (vector : VectorExpr, a : float) =
        vector .<= Scalar(a)

    static member (.<=) (a : float, vector : VectorExpr) =
        Scalar(a) .<= vector

    static member (.>) (vector1 : VectorExpr, vector2 : VectorExpr) =
        BinaryVectorFunction(vector1, vector2, 
                             (fun v1 v2 res -> MklFunctions.D_Arrays_GreaterThan(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray)),
                             ".>")
    
    static member (.>) (vector1 : VectorExpr, vector2 : Vector) =
        vector1 .> vector2.AsExpr

    static member (.>) (vector1 : Vector, vector2 : VectorExpr) =
        vector1.AsExpr .> vector2

    static member (.>) (vector : VectorExpr, a : float) =
        vector .> Scalar(a)

    static member (.>) (a : float, vector : VectorExpr) =
        Scalar(a) .> vector

    static member (.>=) (vector1 : VectorExpr, vector2 : VectorExpr) =
        BinaryVectorFunction(vector1, vector2, 
                             (fun v1 v2 res -> MklFunctions.D_Arrays_GreaterEqual(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray)),
                             ".>=")
    
    static member (.>=) (vector1 : VectorExpr, vector2 : Vector) =
        vector1 .>= vector2.AsExpr

    static member (.>=) (vector1 : Vector, vector2 : VectorExpr) =
        vector1.AsExpr .>= vector2

    static member (.>=) (vector : VectorExpr, a : float) =
        vector .>= Scalar(a)

    static member (.>=) (a : float, vector : VectorExpr) =
        Scalar(a) .>= vector

    static member (.=) (vector1 : VectorExpr, vector2 : VectorExpr) =
        BinaryVectorFunction(vector1, vector2, 
                             (fun v1 v2 res -> MklFunctions.D_Arrays_EqualElementwise(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray)),
                             ".=")
    
    static member (.=) (vector1 : VectorExpr, vector2 : Vector) =
        vector1 .= vector2.AsExpr

    static member (.=) (vector1 : Vector, vector2 : VectorExpr) =
        vector1.AsExpr .= vector2

    static member (.=) (vector : VectorExpr, a : float) =
        vector .= Scalar(a)

    static member (.=) (a : float, vector : VectorExpr) =
        Scalar(a) .= vector

    static member (.<>) (vector1 : VectorExpr, vector2 : VectorExpr) =
        BinaryVectorFunction(vector1, vector2, 
                             (fun v1 v2 res -> MklFunctions.D_Arrays_NotEqualElementwise(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray)),
                             ".<>")
    
    static member (.<>) (vector1 : VectorExpr, vector2 : Vector) =
        vector1 .<> vector2.AsExpr

    static member (.<>) (vector1 : Vector, vector2 : VectorExpr) =
        vector1.AsExpr .<> vector2

    static member (.<>) (vector : VectorExpr, a : float) =
        vector .<> Scalar(a)

    static member (.<>) (a : float, vector : VectorExpr) =
        Scalar(a) .<> vector


    static member Min (vector1 : VectorExpr, vector2 : VectorExpr) =
        BinaryFunction(vector1, vector2, 
                       (fun v1 v2 res -> MklFunctions.D_Min_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray)),
                       "Min")

    static member Min (vector1 : VectorExpr, vector2 : Vector) =
       VectorExpr.Min(vector1, vector2.AsExpr)

    static member Min (vector1 : Vector, vector2 : VectorExpr) =
        VectorExpr.Min(vector1.AsExpr, vector2)

    static member Min (vector : VectorExpr, a : float) =
        VectorExpr.Min(vector, Scalar(a))

    static member Min (a : float, vector : VectorExpr) =
        VectorExpr.Min(Scalar(a), vector)

    static member Max (vector1 : VectorExpr, vector2 : VectorExpr) =
        BinaryFunction(vector1, vector2, 
                       (fun v1 v2 res -> MklFunctions.D_Max_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray)),
                       "Max")

    static member Max (vector1 : VectorExpr, vector2 : Vector) =
       VectorExpr.Max(vector1, vector2.AsExpr)

    static member Max (vector1 : Vector, vector2 : VectorExpr) =
        VectorExpr.Max(vector1.AsExpr, vector2)

    static member Max (vector : VectorExpr, a : float) =
        VectorExpr.Max(vector, Scalar(a))

    static member Max (a : float, vector : VectorExpr) =
        VectorExpr.Max(Scalar(a), vector)



    static member (.*) (vectorExpr1 : VectorExpr, vectorExpr2 : VectorExpr) =
        BinaryFunction(vectorExpr1, vectorExpr2, (fun v1 v2 res ->
                                                    if v1.LongLength = 1L then
                                                        let a = NativePtr.read v1.NativeArray
                                                        MklFunctions.D_Scalar_Mul_Array(a, v2.LongLength, v2.NativeArray, res.NativeArray)
                                                    elif v2.LongLength = 1L then
                                                        let a = NativePtr.read v2.NativeArray
                                                        MklFunctions.D_Scalar_Mul_Array(a, v1.LongLength, v1.NativeArray, res.NativeArray)
                                                    else
                                                        let len = v1.LongLength
                                                        MklFunctions.D_Array_Mul_Array(len, v1.NativeArray, v2.NativeArray, res.NativeArray)),
                        ".*"
                        )

    static member (.*) (vectorExpr1 : VectorExpr, vector2 : Vector) =
        vectorExpr1 .* vector2.AsExpr

    static member (.*) (vector1 : Vector, vectorExpr2 : VectorExpr) =
        vector1.AsExpr .* vectorExpr2

    static member (.*) (vectorExpr : VectorExpr, a :  float) =
        vectorExpr .* Scalar(a)

    static member (*) (vectorExpr : VectorExpr, a :  float) =
        vectorExpr .* a

    static member (.*) (a :  float, vectorExpr : VectorExpr) =
        Scalar(a) .* vectorExpr

    static member (*) (a :  float, vectorExpr : VectorExpr) =
        a .* vectorExpr

    static member (+) (vectorExpr1 : VectorExpr, vectorExpr2 : VectorExpr) =
        BinaryFunction(vectorExpr1, vectorExpr2, (fun v1 v2 res ->
                                                    if v1.LongLength = 1L then
                                                        let a = NativePtr.read v1.NativeArray
                                                        MklFunctions.D_Scalar_Add_Array(a, v2.LongLength, v2.NativeArray, res.NativeArray)
                                                    elif v2.LongLength = 1L then
                                                        let a = NativePtr.read v2.NativeArray
                                                        MklFunctions.D_Scalar_Add_Array(a, v1.LongLength, v1.NativeArray, res.NativeArray)
                                                    else
                                                        let len = v1.LongLength
                                                        MklFunctions.D_Array_Add_Array(len, v1.NativeArray, v2.NativeArray, res.NativeArray)),
                        "+"
                        )

    static member (+) (vectorExpr1 : VectorExpr, vector2 : Vector) =
        vectorExpr1 + vector2.AsExpr

    static member (+) (vector1 : Vector, vectorExpr2 : VectorExpr) =
        vector1.AsExpr + vectorExpr2

    static member (+) (vectorExpr : VectorExpr, a :  float) =
        vectorExpr + Scalar(a)

    static member (+) (a :  float, vectorExpr : VectorExpr) =
        Scalar(a) + vectorExpr

    static member (./) (vectorExpr1 : VectorExpr, vectorExpr2 : VectorExpr) =
        BinaryFunction(vectorExpr1, vectorExpr2, (fun v1 v2 res ->
                                                    if v1.LongLength = 1L then
                                                        let a = NativePtr.read v1.NativeArray
                                                        MklFunctions.D_Scalar_Div_Array(a, v2.LongLength, v2.NativeArray, res.NativeArray)
                                                    elif v2.LongLength = 1L then
                                                        let a = NativePtr.read v2.NativeArray
                                                        MklFunctions.D_Array_Div_Scalar(a, v1.LongLength, v1.NativeArray, res.NativeArray)
                                                    else
                                                        let len = v1.LongLength
                                                        MklFunctions.D_Array_Div_Array(len, v1.NativeArray, v2.NativeArray, res.NativeArray)),
                        "./"
                        )

    static member (./) (vectorExpr1 : VectorExpr, vector2 : Vector) =
        vectorExpr1 ./ vector2.AsExpr

    static member (./) (vector1 : Vector, vectorExpr2 : VectorExpr) =
        vector1.AsExpr ./ vectorExpr2

    static member (./) (vectorExpr : VectorExpr, a :  float) =
        vectorExpr ./ Scalar(a)

    static member (/) (vectorExpr : VectorExpr, a :  float) =
        vectorExpr ./ a

    static member (./) (a :  float, vectorExpr : VectorExpr) =
        Scalar(a) ./ vectorExpr

    static member (/) (a :  float, vectorExpr : VectorExpr) =
        a ./ vectorExpr


    static member (-) (vectorExpr1 : VectorExpr, vectorExpr2 : VectorExpr) =
        BinaryFunction(vectorExpr1, vectorExpr2, (fun v1 v2 res ->
                                                    if v1.LongLength = 1L then
                                                        let a = NativePtr.read v1.NativeArray
                                                        MklFunctions.D_Scalar_Sub_Array(a, v2.LongLength, v2.NativeArray, res.NativeArray)
                                                    elif v2.LongLength = 1L then
                                                        let a = NativePtr.read v2.NativeArray
                                                        MklFunctions.D_Array_Sub_Scalar(a, v1.LongLength, v1.NativeArray, res.NativeArray)
                                                    else
                                                        let len = v1.LongLength
                                                        MklFunctions.D_Array_Sub_Array(len, v1.NativeArray, v2.NativeArray, res.NativeArray)),
                        "-"
                        )

    static member (-) (vectorExpr1 : VectorExpr, vector2 : Vector) =
        vectorExpr1 - vector2.AsExpr

    static member (-) (vector1 : Vector, vectorExpr2 : VectorExpr) =
        vector1.AsExpr - vectorExpr2

    static member (-) (vectorExpr : VectorExpr, a :  float) =
        vectorExpr - Scalar(a)

    static member (-) (a :  float, vectorExpr : VectorExpr) =
        Scalar(a) - vectorExpr

    static member (~-) (vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Minus_Array(v.LongLength, v.NativeArray, res.NativeArray)), "~-")

    static member (.^) (vectorExpr1 : VectorExpr, vectorExpr2 : VectorExpr) =
        BinaryFunction(vectorExpr1, vectorExpr2, (fun v1 v2 res ->
                                                    if v1.LongLength = 1L then
                                                        let a = NativePtr.read v1.NativeArray
                                                        MklFunctions.D_Scalar_Pow_Array(a, v2.LongLength, v2.NativeArray, res.NativeArray)
                                                    elif v2.LongLength = 1L then
                                                        let a = NativePtr.read v2.NativeArray
                                                        MklFunctions.D_Array_Pow_scalar(a, v1.LongLength, v1.NativeArray, res.NativeArray)
                                                    else
                                                        let len = v1.LongLength
                                                        MklFunctions.D_Array_Pow_Array(len, v1.NativeArray, v2.NativeArray, res.NativeArray)),
                        ".^"
                        )

    static member (.^) (vectorExpr1 : VectorExpr, vector2 : Vector) =
        vectorExpr1 .^ vector2.AsExpr

    static member (.^) (vector1 : Vector, vectorExpr2 : VectorExpr) =
        vector1.AsExpr .^ vectorExpr2

    static member (.^) (vectorExpr : VectorExpr, a :  float) =
        vectorExpr .^ Scalar(a)

    static member (.^) (a :  float, vectorExpr : VectorExpr) =
        Scalar(a) .^ vectorExpr

    static member (.^) (vectorExpr : VectorExpr, n : int) =
        if n = 1 then
            vectorExpr
        elif n > 1 then
            (vectorExpr .^ (n - 1)) .* vectorExpr
        elif n = 0 then
            vectorExpr .^ 0.0
        else 
            (vectorExpr .^ (-n)) .^ (-1.0)

    static member Abs(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Abs_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Abs")

    static member Sqrt(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Sqrt_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Sqrt")

    static member Sin(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Sin_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Sin")

    static member Cos(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Cos_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Cos")

    static member Tan(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Tan_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Tan")

    static member Asin(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_ASin_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Asin")

    static member Acos(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_ACos_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Acos")

    static member Atan(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_ATan_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Atan")

    static member Sinh(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Sinh_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Sinh")

    static member Cosh(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Cosh_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Cosh")

    static member Tanh(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Tanh_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Tanh")

    static member ASinh(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_ASinh_Array(v.LongLength, v.NativeArray, res.NativeArray)), "ASinh")

    static member ACosh(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_ACosh_Array(v.LongLength, v.NativeArray, res.NativeArray)), "ACosh")

    static member ATanh(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_ATanh_Array(v.LongLength, v.NativeArray, res.NativeArray)), "ATanh")

    static member Exp(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Exp_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Exp")

    static member Expm1(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Expm1_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Expm1")

    static member Log(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Ln_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Log")

    static member Log10(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Log10_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Log10")

    static member Log1p(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Log1p_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Log1p")

    static member Erf(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Erf_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Erf")

    static member Erfc(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Erfc_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Erfc")

    static member Erfinv(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Erfinv_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Erfinv")

    static member Erfcinv(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Erfcinv_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Erfcinv")

    static member Normcdf(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_CdfNorm_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Normcdf")

    static member Norminv(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_CdfNormInv_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Norminv")

    static member Round(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Round_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Round")

    static member Ceiling(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Ceil_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Ceiling")

    static member Floor(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Floor_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Floor")

    static member Truncate(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, (fun v res -> MklFunctions.D_Trunc_Array(v.LongLength, v.NativeArray, res.NativeArray)), "Truncate")

and PoolVector<'T>(vector : 'T) =
    let mutable isUsed = true

    member this.IsUsed 
        with get() = isUsed
        and set(value) = isUsed <- value

    member this.Vector = vector

and MemoryPool() =

    let boolVectorPool = new Dictionary<nativeptr<bool>, PoolVector<DataBuffer<bool>>>() 
    let vectorPool = new Dictionary<nativeptr<float>, PoolVector<DataBuffer<float>>>()
    let boolScalarPool = new Dictionary<nativeptr<bool>, PoolVector<DataBuffer<bool>>>() 
    let scalarPool = new Dictionary<nativeptr<float>, PoolVector<DataBuffer<float>>>()

    member this.GetBoolVector(length : int64) =
        if length <> 1L then
            match boolVectorPool.Values |> Seq.tryFind (fun poolVector -> not poolVector.IsUsed && poolVector.Vector.LongLength >= length) with
                | Some(v) -> 
                     v.IsUsed <- true
                     if v.Vector.LongLength = length then
                         v.Vector
                     else
                         new DataBuffer<bool>(length, v.Vector.NativeArray, false)// isView is a proxy here for 'not created in memory pool'
                | None ->
                    let v = DataBuffer<bool>.CreateBoolBuffer(length)
                    boolVectorPool.Add(v.NativeArray, new PoolVector<_>(v))
                    v
        else
            match boolScalarPool.Values |> Seq.tryFind (fun poolVector -> not poolVector.IsUsed) with
                | Some(v) -> 
                     v.IsUsed <- true
                     v.Vector
                | None ->
                    let v = DataBuffer<bool>.CreateBoolBuffer(1L)
                    boolScalarPool.Add(v.NativeArray, new PoolVector<_>(v))
                    v

    member this.GetVector(length : int64) =
        if length <> 1L then
            match vectorPool.Values |> Seq.tryFind (fun poolVector -> not poolVector.IsUsed && poolVector.Vector.LongLength >= length) with
                | Some(v) -> 
                     v.IsUsed <- true
                     if v.Vector.LongLength = length then
                         v.Vector
                     else
                         new DataBuffer<float>(length, v.Vector.NativeArray, false)// isView is a proxy here for 'not created in memory pool'
                | None ->
                    let v = DataBuffer<float>.CreateFloatBuffer(length)
                    vectorPool.Add(v.NativeArray, new PoolVector<_>(v))
                    v
        else
            match scalarPool.Values |> Seq.tryFind (fun poolVector -> not poolVector.IsUsed) with
                | Some(v) -> 
                     v.IsUsed <- true
                     v.Vector
                | None ->
                    let v = DataBuffer<float>.CreateFloatBuffer(1L)
                    scalarPool.Add(v.NativeArray, new PoolVector<_>(v))
                    v

    member this.UnUseBool(nativeArray : nativeptr<bool>) =
        if boolVectorPool.ContainsKey(nativeArray) then
            boolVectorPool.[nativeArray].IsUsed <- false
        if boolScalarPool.ContainsKey(nativeArray) then
            boolScalarPool.[nativeArray].IsUsed <- false

    member this.UnUseFloat(nativeArray : nativeptr<float>) =
        if vectorPool.ContainsKey(nativeArray) then
            vectorPool.[nativeArray].IsUsed <- false
        if scalarPool.ContainsKey(nativeArray) then
            scalarPool.[nativeArray].IsUsed <- false

    member this.UnUseAll() =
        boolVectorPool.Values |> Seq.iter (fun v -> v.IsUsed <- false)
        vectorPool.Values |> Seq.iter (fun v -> v.IsUsed <- false)
        boolScalarPool.Values |> Seq.iter (fun v -> v.IsUsed <- false)
        scalarPool.Values |> Seq.iter (fun v -> v.IsUsed <- false)
           
    interface IDisposable with
        member this.Dispose() = 
             boolVectorPool |> Seq.iter (fun kv -> let vector = kv.Value.Vector in vector.Dispose())  
             vectorPool |> Seq.iter (fun kv -> let vector = kv.Value.Vector in vector.Dispose())  
             boolScalarPool |> Seq.iter (fun kv -> let vector = kv.Value.Vector in vector.Dispose())  
             scalarPool |> Seq.iter (fun kv -> let vector = kv.Value.Vector in vector.Dispose()) 
              

        
