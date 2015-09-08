namespace FCore
#nowarn "9"

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic

type BoolVector(length : int64, nativeArray : nativeptr<bool>, gcHandlePtr : IntPtr, isView : bool, parentVector : BoolVector option) =
    let mutable isDisposed = false

    static let empty = new BoolVector(0L, IntPtr.Zero |> NativePtr.ofNativeInt<bool>, IntPtr.Zero, false, None)

    new(length : int64, init : bool) =
        if length < 0L then raise (new ArgumentException("Vector length must be >= 0"))
        let mutable arr = IntPtr.Zero
        let nativeArrayPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<BoolPtr>
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
            let gcHandle = GCHandle.Alloc(data, GCHandleType.Pinned)
            new BoolVector(length, gcHandle.AddrOfPinnedObject() |> NativePtr.ofNativeInt<bool>, GCHandle.ToIntPtr(gcHandle), true, None)

    new(data : bool) = new BoolVector([|data|])

    new(length : int, initializer : int -> bool) =
        let data = Array.init length initializer
        new BoolVector(data, false)


    member this.Length = length |> int

    member this.LongLength = length

    member this.NativeArray = nativeArray

    member this.IsDisposed =
        match parentVector with
            | Some(v) -> isDisposed || v.IsDisposed
            | None -> isDisposed

    member this.IsView = isView

    static member Empty = empty

    static member op_Explicit(v : bool) = new BoolVector(v)

    static member op_Explicit(v : bool[]) = new BoolVector(v)

    static member op_Explicit(v : BoolVector) = v

    member this.View
        with get(fromIndex, length) =
            if length = 0L then
                BoolVector.Empty
            else
                if fromIndex < 0L || length < 0L || (fromIndex + length > this.LongLength) then raise (new IndexOutOfRangeException())
                let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + fromIndex) |> NativePtr.ofNativeInt<bool>
                new BoolVector(length, offsetAddr, IntPtr.Zero, true, Some this)

    member this.View
        with get(fromIndex : int, length : int) = this.View(int64(fromIndex), int64(length))


    member this.GetSlice(fromIndex, toIndex) =
        let fromIndex = defaultArg fromIndex 0L
        let toIndex = defaultArg toIndex (length - 1L)
        if fromIndex < 0L || fromIndex >= length then raise (new IndexOutOfRangeException())
        if toIndex < 0L || toIndex >= length then raise (new IndexOutOfRangeException())
        if fromIndex > toIndex then raise (new IndexOutOfRangeException())
        let length = toIndex - fromIndex + 1L
        let view = this.View(fromIndex, length)
        let mutable arr = IntPtr.Zero
        let nativeArrayPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<BoolPtr>
        MklFunctions.B_Create_Array(length, nativeArrayPtr)
        MklFunctions.B_Copy_Array(length, view.NativeArray, arr |> NativePtr.ofNativeInt<bool>) 
        new BoolVector(length, arr |> NativePtr.ofNativeInt<bool>, IntPtr.Zero, false, None)

    member this.GetSlice(fromIndex : int option, toIndex : int option) =
        this.GetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64)

    member this.SetSlice(fromIndex, toIndex, value: bool) =
        let fromIndex = defaultArg fromIndex 0L
        let toIndex = defaultArg toIndex (length - 1L)
        if fromIndex < 0L || fromIndex >= length then raise (new IndexOutOfRangeException())
        if toIndex < 0L || toIndex >= length then raise (new IndexOutOfRangeException())
        if fromIndex > toIndex then raise (new IndexOutOfRangeException())
        let length = toIndex - fromIndex + 1L
        let view = this.View(fromIndex, length)
        MklFunctions.B_Fill_Array(value, length, view.NativeArray)

    member this.SetSlice(fromIndex : int option, toIndex : int option, value: bool) =
        this.SetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64, value)

    member this.SetSlice(fromIndex, toIndex, value: BoolVector) =
        let fromIndex = defaultArg fromIndex 0L
        let toIndex = defaultArg toIndex (length - 1L)
        if fromIndex < 0L || fromIndex >= length then raise (new IndexOutOfRangeException())
        if toIndex < 0L || toIndex >= length then raise (new IndexOutOfRangeException())
        if fromIndex > toIndex then raise (new IndexOutOfRangeException())
        let length = toIndex - fromIndex + 1L
        if value.LongLength <> length then raise (new ArgumentException())
        let view = this.View(fromIndex, length)
        MklFunctions.B_Copy_Array(length, value.NativeArray, view.NativeArray)

    member this.SetSlice(fromIndex : int option, toIndex : int option, value: BoolVector) =
        this.SetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64, value)

    member this.Item
        with get(i : int64) =
            let offsetArray = this.View(i, 1L).NativeArray
            NativePtr.read offsetArray  
        and set (i : int64) value =
            let offsetArray = this.View(i, 1L).NativeArray
            NativePtr.write offsetArray value

    member this.Item
        with get(i : int) = this.[i |> int64]
        and set (i : int) value =
            this.[i |> int64] <- value

    member this.Item
        with get(indices : int64 seq) = 
            let indices = indices |> Seq.toArray
            let length = indices.GetLongLength(0)
            let res = new BoolVector(length, false)
            indices |> Array.iteri (fun i index -> res.[i] <- this.[index])
            res
        and set (indices : int64 seq) (value : BoolVector) =
            let indices = indices |> Seq.toArray
            if value.LongLength = 1L then
                let value = value.[0L]
                indices |> Array.iteri (fun i index -> this.[index] <- value)
            else
                indices |> Array.iteri (fun i index -> this.[index] <- value.[i])

    member this.Item
        with get(indices : int seq) = 
            let indices = indices |> Seq.toArray
            let length = indices.GetLongLength(0)
            let res = new BoolVector(length, false)
            indices |> Array.iteri (fun i index -> res.[i] <- this.[index])
            res
        and set (indices : int seq) (value : BoolVector) =
            if value.LongLength = 1L then
                indices |> Seq.iteri (fun i index -> this.[index] <- value.[0])
            else
                indices |> Seq.iteri (fun i index -> this.[index] <- value.[i])

    member this.Item
        with get(boolVector : BoolVector) = 
            let mutable arr = IntPtr.Zero
            let mutable resLen = 0L
            let nativeArrPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<BoolPtr>
            MklFunctions.B_Get_Bool_Slice(length, nativeArray, boolVector.NativeArray, nativeArrPtr, &&resLen)
            new BoolVector(resLen, arr |> NativePtr.ofNativeInt<bool>, IntPtr.Zero, false, None)

        and set (boolVector : BoolVector) (value : BoolVector) =
            MklFunctions.B_Set_Bool_Slice(length, nativeArray, boolVector.NativeArray, value.NativeArray, value.LongLength)

    member this.ToArray() =
        Array.init this.Length (fun i -> this.[i])

    member this.AsExpr
        with get() = BoolVectorExpr.Var(this)

    static member Concat(vectors : BoolVector seq) =
        let length = vectors |> Seq.map (fun v -> v.LongLength) |> Seq.reduce (+)
        let res = new BoolVector(length, false)
        vectors |> Seq.fold (fun offset v ->
                                 res.View(offset, v.LongLength).SetSlice(Some(0L), None, v)
                                 offset + v.LongLength
                            ) 0L |> ignore
        res


    static member (==) (vector1: BoolVector, vector2: BoolVector) =
        if vector1.LongLength = 0L && vector2.LongLength = 0L then true
        elif vector1.LongLength <> vector2.LongLength then false
        else 
            MklFunctions.B_Arrays_Are_Equal(vector1.LongLength, vector1.NativeArray, vector2.NativeArray)

    static member (!=) (vector1: BoolVector, vector2: BoolVector) =
        not (vector1 == vector2)

    static member (==) (vector: BoolVector, a : bool) =
        vector.LongLength = 1L && vector.[0L] = a

    static member (!=) (vector: BoolVector, a : bool) =
        not (vector == a)

    static member (==) (a : bool, vector: BoolVector) =
        vector.LongLength = 1L && vector.[0L] = a

    static member (!=) (a : bool, vector: BoolVector) =
        not (vector == a)


    static member (.<) (vector1: BoolVector, vector2: BoolVector) =
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.B_Arrays_LessThan(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.<=) (vector1: BoolVector, vector2: BoolVector) =
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.B_Arrays_LessEqual(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.>) (vector1: BoolVector, vector2: BoolVector) =
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.B_Arrays_GreaterThan(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.>=) (vector1: BoolVector, vector2: BoolVector) =
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.B_Arrays_GreaterEqual(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.=) (vector1: BoolVector, vector2: BoolVector) =
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.B_Arrays_EqualElementwise(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.<>) (vector1: BoolVector, vector2: BoolVector) =
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.B_Arrays_NotEqualElementwise(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res



    static member (.<) (vector: BoolVector, a : bool) =
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new BoolVector(a)
        MklFunctions.B_Arrays_LessThan(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res

    static member (.<=) (vector: BoolVector, a : bool) =
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new BoolVector(a)
        MklFunctions.B_Arrays_LessEqual(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res

    static member (.>) (vector: BoolVector, a : bool) =
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new BoolVector(a)
        MklFunctions.B_Arrays_GreaterThan(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res

    static member (.>=) (vector: BoolVector, a : bool) =
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new BoolVector(a)
        MklFunctions.B_Arrays_GreaterEqual(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res

    static member (.=) (vector: BoolVector, a : bool) =
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new BoolVector(a)
        MklFunctions.B_Arrays_EqualElementwise(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res

    static member (.<>) (vector: BoolVector, a : bool) =
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new BoolVector(a)
        MklFunctions.B_Arrays_NotEqualElementwise(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res



    static member (.<) (a : bool, vector: BoolVector) =
        vector .> a

    static member (.<=) (a : bool, vector: BoolVector) =
        vector .>= a

    static member (.>) (a : bool, vector: BoolVector) =
        vector .< a

    static member (.>=) (a : bool, vector: BoolVector) =
        vector .<= a

    static member (.=) (a : bool, vector: BoolVector) =
        vector .= a

    static member (.<>) (a : bool, vector: BoolVector) =
        vector .<> a



    static member Max(vector1 : BoolVector, vector2 : BoolVector) =
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.B_Max_Arrays(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member Min(vector1 : BoolVector, vector2 : BoolVector) =
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.B_Min_Arrays(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member Max(vector : BoolVector, a : bool) =
        let a = new BoolVector(a)
        BoolVector.Max(vector, a)

    static member Min(vector : BoolVector, a : bool) =
        let a = new BoolVector(a)
        BoolVector.Min(vector, a)

    static member Max(a : bool, vector : BoolVector) = 
        BoolVector.Max(vector, a)

    static member Min(a : bool, vector : BoolVector) = 
        BoolVector.Min(vector, a)

    static member (.&&) (vector1 : BoolVector, vector2 : BoolVector) =
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.B_And_Arrays(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.||) (vector1 : BoolVector, vector2 : BoolVector) =
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.B_Or_Arrays(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.&&) (vector : BoolVector, a : bool) =
        let a = new BoolVector(a)
        vector .&& a

    static member (.||) (vector : BoolVector, a : bool) =
        let a = new BoolVector(a)
        vector .|| a

    static member (.&&) (a : bool, vector : BoolVector) =
        let a = new BoolVector(a)
        vector .&& a

    static member (.||) (a : bool, vector : BoolVector) =
        let a = new BoolVector(a)
        vector .|| a

    static member Not (vector : BoolVector) =
        let res = new BoolVector(vector.LongLength, false)
        MklFunctions.B_Not_Array(vector.LongLength, vector.NativeArray, res.NativeArray)
        res

    override this.ToString() = 
        (this:>IFormattable).ToString(GenericFormatting.GenericFormat.Instance.GetFormat<bool>() true, null)

    interface IFormattable with
        member this.ToString(format, provider) = 
            let maxRows, _ = DisplayControl.MaxDisplaySize
            let showRows = max 0L (min (maxRows |> int64) length) |> int
            let moreRows = length > (showRows |> int64)
            let arr = Array2D.init showRows 1 (fun row col -> this.[row])
            let formattedArray = DisplayControl.FormatArray2D(arr, format, moreRows, false)
            sprintf "Length = %d\r\n%s" length formattedArray

    interface IDisposable with
        member this.Dispose() = this.DoDispose(true)

    member internal this.DoDispose(isDisposing) = if not isDisposed then
                                                     isDisposed <- true
                                                     if isDisposing then GC.SuppressFinalize(this)
                                                     let nativeArray = nativeArray |> NativePtr.toNativeInt
                                                     if not isView && gcHandlePtr = IntPtr.Zero && nativeArray <> IntPtr.Zero then MklFunctions.Free_Array(nativeArray)
                                                     if gcHandlePtr <> IntPtr.Zero then
                                                         try
                                                             let gcHandle = GCHandle.FromIntPtr(gcHandlePtr)
                                                             if gcHandle.IsAllocated then gcHandle.Free()
                                                         with _ -> ()

    override this.Finalize() = this.DoDispose(false)

//**************************************BoolVectorExpr**************************************************************************************

and BoolVectorExpr = 
    | Var of BoolVector
    | UnaryFunction of BoolVectorExpr * (BoolVector -> BoolVector -> unit)
    | BinaryFunction of BoolVectorExpr * BoolVectorExpr * (BoolVector -> BoolVector -> BoolVector -> unit)
    | BinaryVectorFunction of VectorExpr * VectorExpr * (Vector -> Vector -> BoolVector -> unit)
    | IfFunction of BoolVectorExpr * BoolVectorExpr * BoolVectorExpr

    member this.MaxLength
        with get() =
            let rec getMaxLength = function
                | Var(v) -> v.LongLength
                | UnaryFunction(v, _) -> getMaxLength v
                | BinaryFunction(v1, v2, _) -> 
                    let len1 = getMaxLength v1
                    let len2 = getMaxLength v2
                    max len1 len2 
                | BinaryVectorFunction(v1, v2, _) ->
                    let len1 = v1.MaxLength
                    let len2 = v2.MaxLength
                    max len1 len2                  
                | IfFunction(v1, v2, v3) -> 
                    let len1 = getMaxLength v1
                    let len2 = getMaxLength v2
                    let len3 = getMaxLength v3
                    max len1 (max len2 len3)
            getMaxLength this

    static member internal DeScalar(boolVectorExpr : BoolVectorExpr) = 
        match boolVectorExpr with
            | Var(v) -> Var(v)
            | UnaryFunction(Var(v), f) ->
                if v.LongLength = 1L then
                    let res = new BoolVector(false)
                    f v res
                    Var(res)
                else 
                    UnaryFunction(Var(v), f)
            | UnaryFunction(v, f) -> UnaryFunction(BoolVectorExpr.DeScalar(v), f)
            | BinaryFunction(Var(v1), Var(v2), f) -> 
                if v1.LongLength = 1L && v2.LongLength = 1L then
                    let res = new BoolVector(false)
                    f v1 v2 res
                    Var(res)
                else
                  BinaryFunction(Var(v1), Var(v2), f)  
            | BinaryVectorFunction(VectorExpr.Var(v1), VectorExpr.Var(v2), f) -> 
                if v1.LongLength = 1L && v2.LongLength = 1L then
                    let res = new BoolVector(false)
                    f v1 v2 res
                    Var(res)
                else
                  BinaryVectorFunction(VectorExpr.Var(v1), VectorExpr.Var(v2), f)  
            | BinaryFunction(v1, v2, f) -> BinaryFunction(BoolVectorExpr.DeScalar(v1), BoolVectorExpr.DeScalar(v2), f)
            | BinaryVectorFunction(v1, v2, f) -> BinaryVectorFunction(VectorExpr.DeScalar(v1), VectorExpr.DeScalar(v2), f)
            | IfFunction(BoolVectorExpr.Var(v1), Var(v2), Var(v3)) -> 
                if v1.LongLength = 1L && v2.LongLength = 1L && v3.LongLength = 1L then
                    let res = if v1.[0] then v2.[0] else v3.[0]
                    Var(new BoolVector(res))
                else
                    IfFunction(BoolVectorExpr.Var(v1), Var(v2), Var(v3))
            | IfFunction(v1, v2, v3) -> IfFunction(BoolVectorExpr.DeScalar(v1), BoolVectorExpr.DeScalar(v2), BoolVectorExpr.DeScalar(v3))

    static member internal EvalSlice (boolVectorExpr : BoolVectorExpr) (sliceStart : int64) (sliceLen : int64)
                                     (usedPool : List<Vector>) (freePool : List<Vector>) (usedBoolPool : List<BoolVector>) (freeBoolPool : List<BoolVector>) : BoolVector * List<Vector> * List<Vector> * List<BoolVector> * List<BoolVector> = 
        match boolVectorExpr with
            | Var(v) ->
                if v.LongLength = 1L then
                    v, usedPool, freePool, usedBoolPool, freeBoolPool
                else
                    v.View(sliceStart, sliceLen), usedPool, freePool, usedBoolPool, freeBoolPool
            | UnaryFunction(v, f) -> 
                let v, usedPool, freePool, usedBoolPool, freeBoolPool = BoolVectorExpr.EvalSlice v sliceStart sliceLen usedPool freePool usedBoolPool freeBoolPool
                if usedBoolPool.Contains(v) then
                    f v v
                    v, usedPool, freePool, usedBoolPool, freeBoolPool
                else
                    if freeBoolPool.Count = 0 then
                        freeBoolPool.Add(new BoolVector(sliceLen, false))
                    let res = freeBoolPool.[0]
                    usedBoolPool.Add(res)
                    freeBoolPool.RemoveAt(0)
                    f v res
                    res, usedPool, freePool, usedBoolPool, freeBoolPool
            | BinaryFunction(v1, v2, f) -> 
                let v1, usedPool, freePool, usedBoolPool, freeBoolPool = BoolVectorExpr.EvalSlice v1 sliceStart sliceLen usedPool freePool usedBoolPool freeBoolPool
                let v2, usedPool, freePool, usedBoolPool, freeBoolPool = BoolVectorExpr.EvalSlice v2 sliceStart sliceLen usedPool freePool usedBoolPool freeBoolPool
                if usedBoolPool.Contains(v1) then
                    f v1 v2 v1
                    if usedBoolPool.Contains(v2) then
                        freeBoolPool.Add(v2)
                        usedBoolPool.Remove(v2) |> ignore
                    v1, usedPool, freePool, usedBoolPool, freeBoolPool
                elif usedBoolPool.Contains(v2) then
                    f v1 v2 v2
                    v2, usedPool, freePool, usedBoolPool, freeBoolPool
                else
                    if freeBoolPool.Count = 0 then
                        freeBoolPool.Add(new BoolVector(sliceLen, false))
                    let res = freeBoolPool.[0]
                    usedBoolPool.Add(res)
                    freeBoolPool.RemoveAt(0)
                    f v1 v2 res
                    res, usedPool, freePool, usedBoolPool, freeBoolPool
            | BinaryVectorFunction(v1, v2, f) -> 
                let v1, usedPool, freePool, usedBoolPool, freeBoolPool = VectorExpr.EvalSlice v1 sliceStart sliceLen usedPool freePool usedBoolPool freeBoolPool
                let v2, usedPool, freePool, usedBoolPool, freeBoolPool = VectorExpr.EvalSlice v2 sliceStart sliceLen usedPool freePool usedBoolPool freeBoolPool
                if freeBoolPool.Count = 0 then
                    freeBoolPool.Add(new BoolVector(sliceLen, false))
                let res = freeBoolPool.[0]
                usedBoolPool.Add(res)
                freeBoolPool.RemoveAt(0)
                f v1 v2 res
                if usedPool.Contains(v1) then
                    freePool.Add(v1)
                    usedPool.Remove(v1) |> ignore
                if usedPool.Contains(v2) then
                    freePool.Add(v2)
                    usedPool.Remove(v2) |> ignore
                res, usedPool, freePool, usedBoolPool, freeBoolPool
            | IfFunction(b, v1, v2) -> 
                let boolVector, usedPool, freePool, usedBoolPool, freeBoolPool = BoolVectorExpr.EvalSlice b sliceStart sliceLen usedPool freePool usedBoolPool freeBoolPool
                let v1, usedPool, freePool, usedBoolPool, freeBoolPool = BoolVectorExpr.EvalSlice v1 sliceStart sliceLen usedPool freePool usedBoolPool freeBoolPool
                let v2, usedPool, freePool, usedBoolPool, freeBoolPool = BoolVectorExpr.EvalSlice v2 sliceStart sliceLen usedPool freePool usedBoolPool freeBoolPool
                if usedBoolPool.Contains(v1) then
                    MklFunctions.B_IIf_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, boolVector.NativeArray, v1.NativeArray)
                    if usedBoolPool.Contains(v2) then
                        freeBoolPool.Add(v2)
                        usedBoolPool.Remove(v2) |> ignore
                    v1, usedPool, freePool, usedBoolPool, freeBoolPool
                elif usedBoolPool.Contains(v2) then
                    MklFunctions.B_IIf_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, boolVector.NativeArray, v2.NativeArray)
                    v2, usedPool, freePool, usedBoolPool, freeBoolPool
                else
                    if freeBoolPool.Count = 0 then
                        freeBoolPool.Add(new BoolVector(sliceLen, false))
                    let res = freeBoolPool.[0]
                    usedBoolPool.Add(res)
                    freeBoolPool.RemoveAt(0)
                    MklFunctions.B_IIf_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, boolVector.NativeArray, res.NativeArray)
                    res, usedPool, freePool, usedBoolPool, freeBoolPool


    static member EvalIn(vectorExpr : BoolVectorExpr, res : BoolVector option) =
        let vectorExpr = BoolVectorExpr.DeScalar(vectorExpr)
        let n = 1000000L
        let len = vectorExpr.MaxLength
        let res = defaultArg res (new BoolVector(len, false))
        let m = len / n
        let k = len % n
        let freePool = new List<Vector>()
        let usedPool = new List<Vector>()
        let freeBoolPool = new List<BoolVector>()
        let usedBoolPool = new List<BoolVector>()

        for i in 0L..(m-1L) do
            let sliceStart = i * n
            let v, _, _, _, _ = BoolVectorExpr.EvalSlice vectorExpr sliceStart n usedPool freePool usedBoolPool freeBoolPool
            res.View(sliceStart, n).SetSlice(Some(0L), None, v)
            freePool.AddRange(usedPool)
            usedPool.Clear()
            freeBoolPool.AddRange(usedBoolPool)
            usedBoolPool.Clear()

        if k > 0L then
            freeBoolPool.AddRange(usedBoolPool)
            usedBoolPool.Clear()
            freePool.AddRange(usedPool)
            usedPool.Clear()
            let freeBoolPool' = new List<BoolVector>(freeBoolPool |> Seq.map (fun v -> v.View(0L, k)))
            let freePool' = new List<Vector>(freePool |> Seq.map (fun v -> v.View(0L, k)))
            let sliceStart = m * n
            let v, _, _, _, _ = BoolVectorExpr.EvalSlice vectorExpr sliceStart k usedPool freePool' usedBoolPool freeBoolPool'
            res.View(sliceStart, k).SetSlice(Some(0L), None, v)
            freeBoolPool' |> Seq.iter (fun x -> (x:>IDisposable).Dispose())
            freePool' |> Seq.iter (fun x -> (x:>IDisposable).Dispose())

        freeBoolPool |> Seq.iter (fun x -> (x:>IDisposable).Dispose())
        usedBoolPool |> Seq.iter (fun x -> (x:>IDisposable).Dispose())
        freePool |> Seq.iter (fun x -> (x:>IDisposable).Dispose())
        usedPool |> Seq.iter (fun x -> (x:>IDisposable).Dispose())
        res

    static member (.<) (vector1 : BoolVectorExpr, vector2 : BoolVectorExpr) =
        BinaryFunction(vector1, vector2, 
                       fun v1 v2 res -> MklFunctions.B_Arrays_LessThan(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.<) (vector1 : BoolVectorExpr, vector2 : BoolVector) =
        vector1 .< Var(vector2)

    static member (.<) (vector1 : BoolVector, vector2 : BoolVectorExpr) =
        Var(vector1) .< vector2

    static member (.<) (vector : BoolVectorExpr, a : bool) =
        vector .< Var(new BoolVector(a))

    static member (.<) (a : bool, vector : BoolVectorExpr) =
        Var(new BoolVector(a)) .< vector


    static member (.<=) (vector1 : BoolVectorExpr, vector2 : BoolVectorExpr) =
        BinaryFunction(vector1, vector2, 
                       fun v1 v2 res -> MklFunctions.B_Arrays_LessEqual(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.<=) (vector1 : BoolVectorExpr, vector2 : BoolVector) =
        vector1 .<= Var(vector2)

    static member (.<=) (vector1 : BoolVector, vector2 : BoolVectorExpr) =
        Var(vector1) .<= vector2

    static member (.<=) (vector : BoolVectorExpr, a : bool) =
        vector .<= Var(new BoolVector(a))

    static member (.<=) (a : bool, vector : BoolVectorExpr) =
        Var(new BoolVector(a)) .<= vector


    static member (.>) (vector1 : BoolVectorExpr, vector2 : BoolVectorExpr) =
        BinaryFunction(vector1, vector2, 
                       fun v1 v2 res -> MklFunctions.B_Arrays_GreaterThan(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.>) (vector1 : BoolVectorExpr, vector2 : BoolVector) =
        vector1 .> Var(vector2)

    static member (.>) (vector1 : BoolVector, vector2 : BoolVectorExpr) =
        Var(vector1) .> vector2

    static member (.>) (vector : BoolVectorExpr, a : bool) =
        vector .> Var(new BoolVector(a))

    static member (.>) (a : bool, vector : BoolVectorExpr) =
        Var(new BoolVector(a)) .> vector


    static member (.>=) (vector1 : BoolVectorExpr, vector2 : BoolVectorExpr) =
        BinaryFunction(vector1, vector2, 
                       fun v1 v2 res -> MklFunctions.B_Arrays_GreaterEqual(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.>=) (vector1 : BoolVectorExpr, vector2 : BoolVector) =
        vector1 .>= Var(vector2)

    static member (.>=) (vector1 : BoolVector, vector2 : BoolVectorExpr) =
        Var(vector1) .>= vector2

    static member (.>=) (vector : BoolVectorExpr, a : bool) =
        vector .>= Var(new BoolVector(a))

    static member (.>=) (a : bool, vector : BoolVectorExpr) =
        Var(new BoolVector(a)) .>= vector


    static member (.=) (vector1 : BoolVectorExpr, vector2 : BoolVectorExpr) =
        BinaryFunction(vector1, vector2, 
                       fun v1 v2 res -> MklFunctions.B_Arrays_EqualElementwise(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.=) (vector1 : BoolVectorExpr, vector2 : BoolVector) =
        vector1 .= Var(vector2)

    static member (.=) (vector1 : BoolVector, vector2 : BoolVectorExpr) =
        Var(vector1) .= vector2

    static member (.=) (vector : BoolVectorExpr, a : bool) =
        vector .= Var(new BoolVector(a))

    static member (.=) (a : bool, vector : BoolVectorExpr) =
        Var(new BoolVector(a)) .= vector

    static member (.<>) (vector1 : BoolVectorExpr, vector2 : BoolVectorExpr) =
        BinaryFunction(vector1, vector2, 
                       fun v1 v2 res -> MklFunctions.B_Arrays_NotEqualElementwise(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.<>) (vector1 : BoolVectorExpr, vector2 : BoolVector) =
        vector1 .<> Var(vector2)

    static member (.<>) (vector1 : BoolVector, vector2 : BoolVectorExpr) =
        Var(vector1) .<> vector2

    static member (.<>) (vector : BoolVectorExpr, a : bool) =
        vector .<> Var(new BoolVector(a))

    static member (.<>) (a : bool, vector : BoolVectorExpr) =
        Var(new BoolVector(a)) .<> vector

    static member Min (vector1 : BoolVectorExpr, vector2 : BoolVectorExpr) =
        BinaryFunction(vector1, vector2, 
                       fun v1 v2 res -> MklFunctions.B_Min_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))

    static member Min (vector1 : BoolVectorExpr, vector2 : BoolVector) =
       BoolVectorExpr.Min(vector1, Var(vector2))

    static member Min (vector1 : BoolVector, vector2 : BoolVectorExpr) =
        BoolVectorExpr.Min(Var(vector1), vector2)

    static member Min (vector : BoolVectorExpr, a : bool) =
        BoolVectorExpr.Min(vector, Var(new BoolVector(a)))

    static member Min (a : bool, vector : BoolVectorExpr) =
        BoolVectorExpr.Min(Var(new BoolVector(a)), vector)

    static member Max (vector1 : BoolVectorExpr, vector2 : BoolVectorExpr) =
        BinaryFunction(vector1, vector2, 
                       fun v1 v2 res -> MklFunctions.B_Max_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))

    static member Max (vector1 : BoolVectorExpr, vector2 : BoolVector) =
       BoolVectorExpr.Max(vector1, Var(vector2))

    static member Max (vector1 : BoolVector, vector2 : BoolVectorExpr) =
        BoolVectorExpr.Max(Var(vector1), vector2)

    static member Max (vector : BoolVectorExpr, a : bool) =
        BoolVectorExpr.Max(vector, Var(new BoolVector(a)))

    static member Max (a : bool, vector : BoolVectorExpr) =
        BoolVectorExpr.Max(Var(new BoolVector(a)), vector)

    static member (.&&) (vector1 : BoolVectorExpr, vector2 : BoolVectorExpr) =
        BinaryFunction(vector1, vector2, 
                       fun v1 v2 res -> MklFunctions.B_And_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))

    static member (.&&) (vector1 : BoolVectorExpr, vector2 : BoolVector) =
        vector1 .&& Var(vector2)

    static member (.&&) (vector1 : BoolVector, vector2 : BoolVectorExpr) =
        Var(vector1) .&& vector2

    static member (.&&) (vector : BoolVectorExpr, a : bool) =
        vector .&& Var(new BoolVector(a))

    static member (.&&) (a : bool, vector : BoolVectorExpr) =
        Var(new BoolVector(a)) .&& vector

    static member (.||) (vector1 : BoolVectorExpr, vector2 : BoolVectorExpr) =
        BinaryFunction(vector1, vector2, 
                       fun v1 v2 res -> MklFunctions.B_Or_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))

    static member (.||) (vector1 : BoolVectorExpr, vector2 : BoolVector) =
        vector1 .|| Var(vector2)

    static member (.||) (vector1 : BoolVector, vector2 : BoolVectorExpr) =
        Var(vector1) .|| vector2

    static member (.||) (vector : BoolVectorExpr, a : bool) =
        vector .|| Var(new BoolVector(a))

    static member (.||) (a : bool, vector : BoolVectorExpr) =
        Var(new BoolVector(a)) .|| vector

    static member Not (vector : BoolVectorExpr) =
        UnaryFunction(vector, fun v res -> MklFunctions.B_Not_Array(v.LongLength, v.NativeArray, res.NativeArray))


//*******************************************Vector***********************************************************************************

and Vector (length : int64, nativeArray : nativeptr<float>, gcHandlePtr : IntPtr, isView : bool, parentVector : Vector option) =
    let mutable isDisposed = false

    static let empty = new Vector(0L, IntPtr.Zero |> NativePtr.ofNativeInt<float>, IntPtr.Zero, false, None)

    new(length : int64, init : float) =
        if length < 0L then raise (new ArgumentException("Vector length must be >= 0"))
        let mutable arr = IntPtr.Zero
        let nativeArrayPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<FloatPtr>
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
            let gcHandle = GCHandle.Alloc(data, GCHandleType.Pinned)
            new Vector(length, gcHandle.AddrOfPinnedObject() |> NativePtr.ofNativeInt<float>, GCHandle.ToIntPtr(gcHandle), true, None)

    new(data : float) = new Vector([|data|])

    new(length : int, initializer : int -> float) =
        let data = Array.init length initializer
        new Vector(data)


    member this.Length = length |> int

    member this.LongLength = length

    member this.NativeArray = nativeArray

    member this.IsDisposed =
        match parentVector with
            | Some(p) -> isDisposed || p.IsDisposed
            | None -> isDisposed

    member this.IsView = isView

    static member Empty = empty

    static member op_Explicit(v : float) = new Vector(v)

    static member op_Explicit(v : float[]) = new Vector(v)

    static member op_Explicit(v : Vector) = v


    member this.View
        with get(fromIndex, length) =
            if length = 0L then
                Vector.Empty
            else
                if fromIndex < 0L || length < 0L || (fromIndex + length > this.LongLength) then raise (new IndexOutOfRangeException())
                let sizeof = sizeof<float> |> int64
                let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + fromIndex*sizeof) |> NativePtr.ofNativeInt<float>
                new Vector(length, offsetAddr, IntPtr.Zero, true, Some this)

    member this.View
        with get(fromIndex : int, length : int) = this.View(int64(fromIndex), int64(length))

    member this.GetSlice(fromIndex, toIndex) =
        let fromIndex = defaultArg fromIndex 0L
        let toIndex = defaultArg toIndex (length - 1L)
        if fromIndex < 0L || fromIndex >= length then raise (new IndexOutOfRangeException())
        if toIndex < 0L || toIndex >= length then raise (new IndexOutOfRangeException())
        if fromIndex > toIndex then raise (new IndexOutOfRangeException())
        let length = toIndex - fromIndex + 1L
        let view = this.View(fromIndex, length)
        let mutable arr = IntPtr.Zero
        let nativeArrayPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<FloatPtr>
        MklFunctions.D_Create_Array(length, nativeArrayPtr)
        MklFunctions.D_Copy_Array(length, view.NativeArray, arr |> NativePtr.ofNativeInt<float>) 
        new Vector(length, arr |> NativePtr.ofNativeInt<float>, IntPtr.Zero, false, None)

    member this.GetSlice(fromIndex : int option, toIndex : int option) =
        this.GetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64)

    member this.SetSlice(fromIndex, toIndex, value : float) =
        let fromIndex = defaultArg fromIndex 0L
        let toIndex = defaultArg toIndex (length - 1L)
        if fromIndex < 0L || fromIndex >= length then raise (new IndexOutOfRangeException())
        if toIndex < 0L || toIndex >= length then raise (new IndexOutOfRangeException())
        if fromIndex > toIndex then raise (new IndexOutOfRangeException())
        let length = toIndex - fromIndex + 1L
        let view = this.View(fromIndex, length)
        MklFunctions.D_Fill_Array(value, length, view.NativeArray)

    member this.SetSlice(fromIndex : int option, toIndex : int option, value : float) =
        this.SetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64, value)

    member this.SetSlice(fromIndex, toIndex, value: Vector) =
        let fromIndex = defaultArg fromIndex 0L
        let toIndex = defaultArg toIndex (length - 1L)
        if fromIndex < 0L || fromIndex >= length then raise (new IndexOutOfRangeException())
        if toIndex < 0L || toIndex >= length then raise (new IndexOutOfRangeException())
        if fromIndex > toIndex then raise (new IndexOutOfRangeException())
        let length = toIndex - fromIndex + 1L
        if value.LongLength <> length then raise (new ArgumentException())
        let view = this.View(fromIndex, length)
        MklFunctions.D_Copy_Array(length, value.NativeArray, view.NativeArray)

    member this.SetSlice(fromIndex : int option, toIndex : int option, value: Vector) =
        this.SetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64, value)


    member this.Item
        with get(i : int64) =
            let offsetArray = this.View(i, 1L).NativeArray
            NativePtr.read offsetArray  
        and set (i : int64) value =
            let offsetArray = this.View(i, 1L).NativeArray
            NativePtr.write offsetArray value

    member this.Item
        with get(i : int) = this.[i |> int64]
        and set (i : int) value =
            this.[i |> int64] <- value

    member this.Item
        with get(indices : int64 seq) = 
            let indices = indices |> Seq.toArray
            let length = indices.GetLongLength(0)
            let res = new Vector(length, 0.0)
            indices |> Array.iteri (fun i index -> res.[i] <- this.[index])
            res
        and set (indices : int64 seq) (value : Vector) =
            let indices = indices |> Seq.toArray
            if value.LongLength = 1L then
                let value = value.[0L]
                indices |> Array.iteri (fun i index -> this.[index] <- value)
            else
                indices |> Array.iteri (fun i index -> this.[index] <- value.[i])

    member this.Item
        with get(indices : int seq) = 
            let indices = indices |> Seq.toArray
            let length = indices.GetLongLength(0)
            let res = new Vector(length, 0.0)
            indices |> Array.iteri (fun i index -> res.[i] <- this.[index])
            res
        and set (indices : int seq) (value : Vector) =
            if value.LongLength = 1L then
                indices |> Seq.iteri (fun i index -> this.[index] <- value.[0])
            else
                indices |> Seq.iteri (fun i index -> this.[index] <- value.[i])

    member this.Item
        with get(boolVector : BoolVector) = 
            let mutable arr = IntPtr.Zero
            let mutable resLen = 0L
            let nativeArrPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<FloatPtr>
            MklFunctions.D_Get_Bool_Slice(length, nativeArray, boolVector.NativeArray, nativeArrPtr, &&resLen)
            new Vector(resLen, arr |> NativePtr.ofNativeInt<float>, IntPtr.Zero, false, None)

        and set (boolVector : BoolVector) (value : Vector) =
            MklFunctions.D_Set_Bool_Slice(length, nativeArray, boolVector.NativeArray, value.NativeArray, value.LongLength)


    member this.ToArray() =
        Array.init this.Length (fun i -> this.[i])

    member this.AsExpr
        with get() = VectorExpr.Var(this)

    static member Concat(vectors : Vector seq) =
        let length = vectors |> Seq.map (fun v -> v.LongLength) |> Seq.reduce (+)
        let res = new Vector(length, 0.0)
        vectors |> Seq.fold (fun offset v ->
                                 res.View(offset, v.LongLength).SetSlice(Some(0L), None, v)
                                 offset + v.LongLength
                            ) 0L |> ignore
        res


    static member (==) (vector1: Vector, vector2: Vector) =
        if vector1.LongLength = 0L && vector2.LongLength = 0L then true
        elif vector1.LongLength <> vector2.LongLength then false
        else 
        MklFunctions.D_Arrays_Are_Equal(vector1.LongLength, vector1.NativeArray, vector2.NativeArray)

    static member (!=) (vector1: Vector, vector2: Vector) =
        not (vector1 == vector2)

    static member (==) (vector: Vector, a : float) =
        vector.LongLength = 1L && vector.[0L] = a

    static member (!=) (vector: Vector, a : float) =
        not (vector == a)

    static member (==) (a : float, vector: Vector) =
        vector.LongLength = 1L && vector.[0L] = a

    static member (!=) (a : float, vector: Vector) =
        not (vector == a)



    static member (.<) (vector1: Vector, vector2: Vector) =
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.D_Arrays_LessThan(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.<=) (vector1: Vector, vector2: Vector) =
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.D_Arrays_LessEqual(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.>) (vector1: Vector, vector2: Vector) =
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.D_Arrays_GreaterThan(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.>=) (vector1: Vector, vector2: Vector) =
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.D_Arrays_GreaterEqual(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.=) (vector1: Vector, vector2: Vector) =
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.D_Arrays_EqualElementwise(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member (.<>) (vector1: Vector, vector2: Vector) =
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new BoolVector(length, false)
        MklFunctions.D_Arrays_NotEqualElementwise(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res



    static member (.<) (vector: Vector, a : float) =
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new Vector(a)
        MklFunctions.D_Arrays_LessThan(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res

    static member (.<=) (vector: Vector, a : float) =
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new Vector(a)
        MklFunctions.D_Arrays_LessEqual(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res

    static member (.>) (vector: Vector, a : float) =
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new Vector(a)
        MklFunctions.D_Arrays_GreaterThan(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res

    static member (.>=) (vector: Vector, a : float) =
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new Vector(a)
        MklFunctions.D_Arrays_GreaterEqual(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res

    static member (.=) (vector: Vector, a : float) =
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new Vector(a)
        MklFunctions.D_Arrays_EqualElementwise(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res

    static member (.<>) (vector: Vector, a : float) =
        let length = vector.LongLength
        let res = new BoolVector(length, false)
        use a = new Vector(a)
        MklFunctions.D_Arrays_NotEqualElementwise(vector.LongLength, vector.NativeArray, a.LongLength, a.NativeArray, res.NativeArray)
        res



    static member (.<) (a : float, vector: Vector) =
        vector .> a

    static member (.<=) (a : float, vector: Vector) =
        vector .>= a

    static member (.>) (a : float, vector: Vector) =
        vector .< a

    static member (.>=) (a : float, vector: Vector) =
        vector .<= a

    static member (.=) (a : float, vector: Vector) =
        vector .= a

    static member (.<>) (a : float, vector: Vector) =
        vector .<> a


    static member Max(vector1 : Vector, vector2 : Vector) =
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new Vector(length, 0.0)
        MklFunctions.D_Max_Arrays(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member Min(vector1 : Vector, vector2 : Vector) =
        ArgumentChecks.throwIfLengthNotOKForElementwise vector1.LongLength vector2.LongLength
        let length = if vector1.LongLength = 0L || vector2.LongLength = 0L then 0L else max vector1.LongLength vector2.LongLength
        let res = new Vector(length, 0.0)
        MklFunctions.D_Min_Arrays(vector1.LongLength, vector1.NativeArray, vector2.LongLength, vector2.NativeArray, res.NativeArray)
        res

    static member Max(vector : Vector, a : float) =
        let a = new Vector(a)
        Vector.Max(vector, a)

    static member Min(vector : Vector, a : float) =
        let a = new Vector(a)
        Vector.Min(vector, a)

    static member Max(a : float, vector : Vector) = 
        Vector.Max(vector, a)

    static member Min(a : float, vector : Vector) = 
        Vector.Min(vector, a)


    static member (*) (vector1 : Vector, vector2 : Vector) =
        if vector1.LongLength <> vector2.LongLength then raise (new ArgumentException("Vector lengths must be equal"))
        if vector1.LongLength = 0L || vector2.LongLength = 0L then raise (new ArgumentException("Vector lengths must be > 0"))
        MklFunctions.D_Inner_Product(vector1.LongLength, vector1.NativeArray, vector2.NativeArray)

    static member (.*) (a: float, vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Scalar_Mul_Array(a, len, vector.NativeArray, res.NativeArray)
        res 

    static member (.*) (vector : Vector, a :  float) =
        a .* vector

    static member (.*) (vector1 : Vector, vector2 : Vector) =
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
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Scalar_Add_Array(a, len, vector.NativeArray, res.NativeArray)
        res 

    static member (+) (vector : Vector, a :  float) =
        a + vector

    static member (+) (vector1 : Vector, vector2 : Vector) =
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
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Scalar_Div_Array(a, len, vector.NativeArray, res.NativeArray)
        res 

    static member (./) (vector : Vector, a :  float) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Array_Div_Scalar(a, len, vector.NativeArray, res.NativeArray)
        res 

    static member (./) (vector1 : Vector, vector2 : Vector) =
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
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Scalar_Sub_Array(a, len, vector.NativeArray, res.NativeArray)
        res 

    static member (-) (vector : Vector, a :  float) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Array_Sub_Scalar(a, len, vector.NativeArray, res.NativeArray)
        res 

    static member (-) (vector1 : Vector, vector2 : Vector) =
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
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Minus_Array(len, vector.NativeArray, res.NativeArray)
        res   
        
    static member (.^) (a: float, vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Scalar_Pow_Array(a, len, vector.NativeArray, res.NativeArray)
        res 

    static member (.^) (vector : Vector, a :  float) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Array_Pow_scalar(a, len, vector.NativeArray, res.NativeArray)
        res  
        
    static member (.^) (vector1 : Vector, vector2 : Vector) =
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


    static member Abs(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Abs_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Sqrt(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Sqrt_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Sin(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Sin_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Cos(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Cos_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Tan(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Tan_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Asin(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_ASin_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Acos(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_ACos_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Atan(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_ATan_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Sinh(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Sinh_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Cosh(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Cosh_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Tanh(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Tanh_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member ASinh(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_ASinh_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member ACosh(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_ACosh_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member ATanh(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_ATanh_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Exp(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Exp_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Expm1(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Expm1_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Log(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Ln_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Log10(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Log10_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Log1p(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Log1p_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Erf(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Erf_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Erfc(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Erfc_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Erfinv(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Erfinv_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Erfcinv(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Erfcinv_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Normcdf(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_CdfNorm_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Norminv(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_CdfNormInv_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Round(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Round_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Ceiling(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Ceil_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Floor(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Floor_Array(len, vector.NativeArray, res.NativeArray)
        res

    static member Truncate(vector : Vector) =
        let len = vector.LongLength
        let res = new Vector(len, 0.0)
        MklFunctions.D_Trunc_Array(len, vector.NativeArray, res.NativeArray)
        res



    static member Sum(vector : Vector) =
        if vector.LongLength = 0L then raise (new ArgumentException("Vector must have length > 0"))
        let mutable res = 0.0
        MklFunctions.D_Sum_Matrix(false, 1L, vector.LongLength, vector.NativeArray, &&res)
        res

    static member Prod(vector : Vector) =
        if vector.LongLength = 0L then raise (new ArgumentException("Vector must have length > 0"))
        let mutable res = 0.0
        MklFunctions.D_Prod_Matrix(false, 1L, vector.LongLength, vector.NativeArray, &&res)
        res

    static member CumSum(vector : Vector) =
        let res = new Vector(vector.LongLength, 0.0)
        MklFunctions.D_CumSum_Matrix(false, 1L, vector.LongLength, vector.NativeArray, res.NativeArray)
        res

    static member CumProd(vector : Vector) =
        let res = new Vector(vector.LongLength, 0.0)
        MklFunctions.D_CumProd_Matrix(false, 1L, vector.LongLength, vector.NativeArray, res.NativeArray)
        res

    static member Min(vector : Vector) =
        if vector.LongLength = 0L then raise (new ArgumentException("Vector must have length > 0"))
        if vector.LongLength = 1L && Double.IsNaN(vector.[0]) then
            Double.NaN
        else 
            let mutable res = 0.0
            MklFunctions.D_Min_Matrix(false, 1L, vector.LongLength, vector.NativeArray, &&res)
            res

    static member Max(vector : Vector) =
        if vector.LongLength = 0L then raise (new ArgumentException("Vector must have length > 0"))
        if vector.LongLength = 1L && Double.IsNaN(vector.[0]) then
            Double.NaN
        else
            let mutable res = 0.0
            MklFunctions.D_Max_Matrix(false, 1L, vector.LongLength, vector.NativeArray, &&res)
            res

    static member Mean(vector : Vector) =
        if vector.LongLength = 0L then raise (new ArgumentException("Vector must have length > 0"))
        let mutable res = 0.0
        MklFunctions.D_Mean_Matrix(false, 1L, vector.LongLength, vector.NativeArray, &&res)
        res

    static member Variance(vector : Vector) =
        if vector.LongLength = 0L then raise (new ArgumentException("Vector must have length > 0"))
        elif vector.LongLength = 1L then
            if Double.IsInfinity(vector.[0]) || Double.IsNaN(vector.[0]) then Double.NaN
            else 0.0
        else
            let mutable res = 0.0
            MklFunctions.D_Variance_Matrix(false, 1L, vector.LongLength, vector.NativeArray, &&res)
            res

    static member Skewness(vector : Vector) =
        if vector.LongLength <= 1L then raise (new ArgumentException("Vector must have length > 1"))
        let mutable res = 0.0
        MklFunctions.D_Skewness_Matrix(false, 1L, vector.LongLength, vector.NativeArray, &&res)
        res

    static member Kurtosis(vector : Vector) =
        if vector.LongLength <= 1L then raise (new ArgumentException("Vector must have length > 1"))
        let mutable res = 0.0
        MklFunctions.D_Kurtosis_Matrix(false, 1L, vector.LongLength, vector.NativeArray, &&res)
        res

    static member Quantile(vector : Vector) =
        fun (quantileOrders : Vector) ->
            if quantileOrders.LongLength = 0L then raise (new ArgumentException("Quantile orders vector must not be empty"))
            if vector.LongLength = 0L then Vector.Empty
            else
                let res = new Vector(quantileOrders.LongLength, 0.0)
                MklFunctions.D_Quantiles_Matrix(false, 1L, vector.LongLength, quantileOrders.LongLength, vector.NativeArray, quantileOrders.NativeArray, res.NativeArray)
                res

    override this.ToString() = 
        (this:>IFormattable).ToString(GenericFormatting.GenericFormat.Instance.GetFormat<float>() 0.0, null)

    interface IFormattable with
        member this.ToString(format, provider) = 
            let maxRows, _ = DisplayControl.MaxDisplaySize
            let showRows = max 0L (min (maxRows |> int64) length) |> int
            let moreRows = length > (showRows |> int64)
            let arr = Array2D.init showRows 1 (fun row col -> this.[row])
            let formattedArray = DisplayControl.FormatArray2D(arr, format, moreRows, false)
            sprintf "Length = %d\r\n%s" length formattedArray

    interface IDisposable with
        member this.Dispose() = this.DoDispose(true)

    member internal this.DoDispose(isDisposing) = if not isDisposed then
                                                     isDisposed <- true
                                                     if isDisposing then GC.SuppressFinalize(this)
                                                     let nativeArray = nativeArray |> NativePtr.toNativeInt
                                                     if not isView && gcHandlePtr = IntPtr.Zero && nativeArray <> IntPtr.Zero then MklFunctions.Free_Array(nativeArray)
                                                     if gcHandlePtr <> IntPtr.Zero then
                                                         try
                                                             let gcHandle = GCHandle.FromIntPtr(gcHandlePtr)
                                                             if gcHandle.IsAllocated then gcHandle.Free()
                                                         with _ -> ()

    override this.Finalize() = this.DoDispose(false)

//************************************************VectorExpr*******************************************************************************

and VectorExpr = 
    | Var of Vector
    | UnaryFunction of VectorExpr * (Vector -> Vector -> unit)
    | BinaryFunction of VectorExpr * VectorExpr * (Vector -> Vector -> Vector -> unit)
    | IfFunction of BoolVectorExpr * VectorExpr * VectorExpr

    member this.MaxLength
        with get() =
            let rec getMaxLength = function
                | Var(v) -> v.LongLength
                | UnaryFunction(v, _) -> getMaxLength v
                | BinaryFunction(v1, v2, _) -> 
                    let len1 = getMaxLength v1
                    let len2 = getMaxLength v2
                    max len1 len2 
                | IfFunction(v1, v2, v3) -> 
                    let len1 = v1.MaxLength 
                    let len2 = getMaxLength v2
                    let len3 = getMaxLength v3
                    max len1 (max len2 len3)
            getMaxLength this

    static member internal DeScalar(vectorExpr : VectorExpr) =
        match vectorExpr with
            | Var(v) -> Var(v)
            | UnaryFunction(Var(v), f) ->
                if v.LongLength = 1L then
                    let res = new Vector(0.0)
                    f v res
                    Var(res)
                else 
                    UnaryFunction(Var(v), f)
            | UnaryFunction(v, f) -> UnaryFunction(VectorExpr.DeScalar(v), f)
            | BinaryFunction(Var(v1), Var(v2), f) -> 
                if v1.LongLength = 1L && v2.LongLength = 1L then
                    let res = new Vector(0.0)
                    f v1 v2 res
                    Var(res)
                else
                  BinaryFunction(Var(v1), Var(v2), f)  
            | BinaryFunction(v1, v2, f) -> BinaryFunction(VectorExpr.DeScalar(v1), VectorExpr.DeScalar(v2), f)
            | IfFunction(BoolVectorExpr.Var(v1), Var(v2), Var(v3)) -> 
                if v1.LongLength = 1L && v2.LongLength = 1L && v3.LongLength = 1L then
                    let res = if v1.[0] then v2.[0] else v3.[0]
                    Var(new Vector(res))
                else
                    IfFunction(BoolVectorExpr.Var(v1), Var(v2), Var(v3))
            | IfFunction(v1, v2, v3) -> IfFunction(BoolVectorExpr.DeScalar(v1), VectorExpr.DeScalar(v2), VectorExpr.DeScalar(v3))


    static member internal EvalSlice (vectorExpr : VectorExpr) (sliceStart : int64) (sliceLen : int64) (usedPool : List<Vector>) (freePool : List<Vector>) 
                                     (usedBoolPool : List<BoolVector>) (freeBoolPool : List<BoolVector>) =
        match vectorExpr with
            | Var(v) ->
                if v.LongLength = 1L then
                    v, usedPool, freePool, usedBoolPool, freeBoolPool
                else
                    v.View(sliceStart, sliceLen), usedPool, freePool, usedBoolPool, freeBoolPool
            | UnaryFunction(v, f) -> 
                let v, usedPool, freePool, usedBoolPool, freeBoolPool = VectorExpr.EvalSlice v sliceStart sliceLen usedPool freePool usedBoolPool freeBoolPool
                if usedPool.Contains(v) then
                    f v v
                    v, usedPool, freePool, usedBoolPool, freeBoolPool
                else
                    if freePool.Count = 0 then
                        freePool.Add(new Vector(sliceLen, 0.0))
                    let res = freePool.[0]
                    usedPool.Add(res)
                    freePool.RemoveAt(0)
                    f v res
                    res, usedPool, freePool, usedBoolPool, freeBoolPool
            | BinaryFunction(v1, v2, f) -> 
                let v1, usedPool, freePool, usedBoolPool, freeBoolPool = VectorExpr.EvalSlice v1 sliceStart sliceLen usedPool freePool usedBoolPool freeBoolPool
                let v2, usedPool, freePool, usedBoolPool, freeBoolPool = VectorExpr.EvalSlice v2 sliceStart sliceLen usedPool freePool usedBoolPool freeBoolPool
                if usedPool.Contains(v1) then
                    f v1 v2 v1
                    if usedPool.Contains(v2) then
                        freePool.Add(v2)
                        usedPool.Remove(v2) |> ignore
                    v1, usedPool, freePool, usedBoolPool, freeBoolPool
                elif usedPool.Contains(v2) then
                    f v1 v2 v2
                    v2, usedPool, freePool, usedBoolPool, freeBoolPool
                else
                    if freePool.Count = 0 then
                        freePool.Add(new Vector(sliceLen, 0.0))
                    let res = freePool.[0]
                    usedPool.Add(res)
                    freePool.RemoveAt(0)
                    f v1 v2 res
                    res, usedPool, freePool, usedBoolPool, freeBoolPool
            | IfFunction(b, v1, v2) -> 
                let boolVector, usedPool, freePool, usedBoolPool, freeBoolPool = BoolVectorExpr.EvalSlice b sliceStart sliceLen usedPool freePool usedBoolPool freeBoolPool
                let v1, usedPool, freePool, usedBoolPool, freeBoolPool = VectorExpr.EvalSlice v1 sliceStart sliceLen usedPool freePool usedBoolPool freeBoolPool
                let v2, usedPool, freePool, usedBoolPool, freeBoolPool = VectorExpr.EvalSlice v2 sliceStart sliceLen usedPool freePool usedBoolPool freeBoolPool
                if usedPool.Contains(v1) then
                    MklFunctions.D_IIf_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, boolVector.NativeArray, v1.NativeArray)
                    if usedPool.Contains(v2) then
                        freePool.Add(v2)
                        usedPool.Remove(v2) |> ignore
                    if usedBoolPool.Contains(boolVector) then
                        freeBoolPool.Add(boolVector)
                        usedBoolPool.Remove(boolVector) |> ignore
                    v1, usedPool, freePool, usedBoolPool, freeBoolPool
                elif usedPool.Contains(v2) then
                    MklFunctions.D_IIf_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, boolVector.NativeArray, v2.NativeArray)
                    if usedBoolPool.Contains(boolVector) then
                        freeBoolPool.Add(boolVector)
                        usedBoolPool.Remove(boolVector) |> ignore
                    v2, usedPool, freePool, usedBoolPool, freeBoolPool
                else
                    if freePool.Count = 0 then
                        freePool.Add(new Vector(sliceLen, 0.0))
                    let res = freePool.[0]
                    usedPool.Add(res)
                    freePool.RemoveAt(0)
                    MklFunctions.D_IIf_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, boolVector.NativeArray, res.NativeArray)
                    if usedBoolPool.Contains(boolVector) then
                        freeBoolPool.Add(boolVector)
                        usedBoolPool.Remove(boolVector) |> ignore
                    res, usedPool, freePool, usedBoolPool, freeBoolPool


    static member EvalIn(vectorExpr : VectorExpr, res : Vector option) =
        let vectorExpr = VectorExpr.DeScalar(vectorExpr)
        let n = 1000000L
        let len = vectorExpr.MaxLength
        let res = defaultArg res (new Vector(len, 0.0))
        let m = len / n
        let k = len % n
        let freePool = new List<Vector>()
        let usedPool = new List<Vector>()
        let freeBoolPool = new List<BoolVector>()
        let usedBoolPool = new List<BoolVector>()

        for i in 0L..(m-1L) do
            let sliceStart = i * n
            let v, _, _, _, _ = VectorExpr.EvalSlice vectorExpr sliceStart n usedPool freePool usedBoolPool freeBoolPool
            res.View(sliceStart, n).SetSlice(Some(0L), None, v)
            freePool.AddRange(usedPool)
            usedPool.Clear()
            freeBoolPool.AddRange(usedBoolPool)
            usedBoolPool.Clear()

        if k > 0L then
            freePool.AddRange(usedPool)
            usedPool.Clear()
            freeBoolPool.AddRange(usedBoolPool)
            usedBoolPool.Clear()
            let freePool' = new List<Vector>(freePool |> Seq.map (fun v -> v.View(0L, k)))
            let freeBoolPool' = new List<BoolVector>(freeBoolPool |> Seq.map (fun v -> v.View(0L, k)))
            let sliceStart = m * n
            let v, _, _, _, _ = VectorExpr.EvalSlice vectorExpr sliceStart k usedPool freePool' usedBoolPool freeBoolPool'
            res.View(sliceStart, k).SetSlice(Some(0L), None, v)
            freePool' |> Seq.iter (fun x -> (x:>IDisposable).Dispose())
            freeBoolPool' |> Seq.iter (fun x -> (x:>IDisposable).Dispose())

        freePool |> Seq.iter (fun x -> (x:>IDisposable).Dispose())
        usedPool |> Seq.iter (fun x -> (x:>IDisposable).Dispose())
        freeBoolPool |> Seq.iter (fun x -> (x:>IDisposable).Dispose())
        usedBoolPool |> Seq.iter (fun x -> (x:>IDisposable).Dispose())
        res

    static member (.<) (vector1 : VectorExpr, vector2 : VectorExpr) =
        BinaryVectorFunction(vector1, vector2, 
                             fun v1 v2 res -> MklFunctions.D_Arrays_LessThan(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.<) (vector1 : VectorExpr, vector2 : Vector) =
        vector1 .< Var(vector2)

    static member (.<) (vector1 : Vector, vector2 : VectorExpr) =
        Var(vector1) .< vector2

    static member (.<) (vector : VectorExpr, a : float) =
        vector .< Var(new Vector(a))

    static member (.<) (a : float, vector : VectorExpr) =
        Var(new Vector(a)) .< vector

    static member (.<=) (vector1 : VectorExpr, vector2 : VectorExpr) =
        BinaryVectorFunction(vector1, vector2, 
                             fun v1 v2 res -> MklFunctions.D_Arrays_LessEqual(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.<=) (vector1 : VectorExpr, vector2 : Vector) =
        vector1 .<= Var(vector2)

    static member (.<=) (vector1 : Vector, vector2 : VectorExpr) =
        Var(vector1) .<= vector2

    static member (.<=) (vector : VectorExpr, a : float) =
        vector .<= Var(new Vector(a))

    static member (.<=) (a : float, vector : VectorExpr) =
        Var(new Vector(a)) .<= vector

    static member (.>) (vector1 : VectorExpr, vector2 : VectorExpr) =
        BinaryVectorFunction(vector1, vector2, 
                             fun v1 v2 res -> MklFunctions.D_Arrays_GreaterThan(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.>) (vector1 : VectorExpr, vector2 : Vector) =
        vector1 .> Var(vector2)

    static member (.>) (vector1 : Vector, vector2 : VectorExpr) =
        Var(vector1) .> vector2

    static member (.>) (vector : VectorExpr, a : float) =
        vector .> Var(new Vector(a))

    static member (.>) (a : float, vector : VectorExpr) =
        Var(new Vector(a)) .> vector

    static member (.>=) (vector1 : VectorExpr, vector2 : VectorExpr) =
        BinaryVectorFunction(vector1, vector2, 
                             fun v1 v2 res -> MklFunctions.D_Arrays_GreaterEqual(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.>=) (vector1 : VectorExpr, vector2 : Vector) =
        vector1 .>= Var(vector2)

    static member (.>=) (vector1 : Vector, vector2 : VectorExpr) =
        Var(vector1) .>= vector2

    static member (.>=) (vector : VectorExpr, a : float) =
        vector .>= Var(new Vector(a))

    static member (.>=) (a : float, vector : VectorExpr) =
        Var(new Vector(a)) .>= vector

    static member (.=) (vector1 : VectorExpr, vector2 : VectorExpr) =
        BinaryVectorFunction(vector1, vector2, 
                             fun v1 v2 res -> MklFunctions.D_Arrays_EqualElementwise(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.=) (vector1 : VectorExpr, vector2 : Vector) =
        vector1 .= Var(vector2)

    static member (.=) (vector1 : Vector, vector2 : VectorExpr) =
        Var(vector1) .= vector2

    static member (.=) (vector : VectorExpr, a : float) =
        vector .= Var(new Vector(a))

    static member (.=) (a : float, vector : VectorExpr) =
        Var(new Vector(a)) .= vector

    static member (.<>) (vector1 : VectorExpr, vector2 : VectorExpr) =
        BinaryVectorFunction(vector1, vector2, 
                             fun v1 v2 res -> MklFunctions.D_Arrays_NotEqualElementwise(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.<>) (vector1 : VectorExpr, vector2 : Vector) =
        vector1 .<> Var(vector2)

    static member (.<>) (vector1 : Vector, vector2 : VectorExpr) =
        Var(vector1) .<> vector2

    static member (.<>) (vector : VectorExpr, a : float) =
        vector .<> Var(new Vector(a))

    static member (.<>) (a : float, vector : VectorExpr) =
        Var(new Vector(a)) .<> vector



    static member (.*) (vectorExpr1 : VectorExpr, vectorExpr2 : VectorExpr) =
        BinaryFunction(vectorExpr1, vectorExpr2, fun v1 v2 res ->
                                                    if v1.LongLength = 1L then
                                                        MklFunctions.D_Scalar_Mul_Array(v1.[0], v2.LongLength, v2.NativeArray, res.NativeArray)
                                                    elif v2.LongLength = 1L then
                                                        MklFunctions.D_Scalar_Mul_Array(v2.[0], v1.LongLength, v1.NativeArray, res.NativeArray)
                                                    else
                                                       let len = v1.LongLength
                                                       MklFunctions.D_Array_Mul_Array(len, v1.NativeArray, v2.NativeArray, res.NativeArray)
                      )

    static member (.*) (vectorExpr1 : VectorExpr, vector2 : Vector) =
        vectorExpr1 .* Var(vector2)

    static member (.*) (vector1 : Vector, vectorExpr2 : VectorExpr) =
        Var(vector1) .* vectorExpr2

    static member (.*) (vectorExpr : VectorExpr, a :  float) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Scalar_Mul_Array(a, v.LongLength, v.NativeArray, res.NativeArray))

    static member (.*) (a :  float, vectorExpr : VectorExpr) =
        vectorExpr .* a

    static member (+) (vectorExpr1 : VectorExpr, vectorExpr2 : VectorExpr) =
        BinaryFunction(vectorExpr1, vectorExpr2, fun v1 v2 res ->
                                                    if v1.LongLength = 1L then
                                                        MklFunctions.D_Scalar_Add_Array(v1.[0], v2.LongLength, v2.NativeArray, res.NativeArray)
                                                    elif v2.LongLength = 1L then
                                                        MklFunctions.D_Scalar_Add_Array(v2.[0], v1.LongLength, v1.NativeArray, res.NativeArray)
                                                    else
                                                       let len = v1.LongLength
                                                       MklFunctions.D_Array_Add_Array(len, v1.NativeArray, v2.NativeArray, res.NativeArray)
                      )

    static member (+) (vectorExpr1 : VectorExpr, vector2 : Vector) =
        vectorExpr1 + Var(vector2)

    static member (+) (vector1 : Vector, vectorExpr2 : VectorExpr) =
        Var(vector1) + vectorExpr2

    static member (+) (vectorExpr : VectorExpr, a :  float) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Scalar_Add_Array(a, v.LongLength, v.NativeArray, res.NativeArray))

    static member (+) (a :  float, vectorExpr : VectorExpr) =
        vectorExpr + a

    static member (./) (vectorExpr1 : VectorExpr, vectorExpr2 : VectorExpr) =
        BinaryFunction(vectorExpr1, vectorExpr2, fun v1 v2 res ->
                                                    if v1.LongLength = 1L then
                                                        MklFunctions.D_Scalar_Div_Array(v1.[0], v2.LongLength, v2.NativeArray, res.NativeArray)
                                                    elif v2.LongLength = 1L then
                                                        MklFunctions.D_Array_Div_Scalar(v2.[0], v1.LongLength, v1.NativeArray, res.NativeArray)
                                                    else
                                                       let len = v1.LongLength
                                                       MklFunctions.D_Array_Div_Array(len, v1.NativeArray, v2.NativeArray, res.NativeArray)
                      )

    static member (./) (vectorExpr1 : VectorExpr, vector2 : Vector) =
        vectorExpr1 ./ Var(vector2)

    static member (./) (vector1 : Vector, vectorExpr2 : VectorExpr) =
        Var(vector1) ./ vectorExpr2

    static member (./) (vectorExpr : VectorExpr, a :  float) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Array_Div_Scalar(a, v.LongLength, v.NativeArray, res.NativeArray))

    static member (./) (a :  float, vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Scalar_Div_Array(a, v.LongLength, v.NativeArray, res.NativeArray))


    static member (-) (vectorExpr1 : VectorExpr, vectorExpr2 : VectorExpr) =
        BinaryFunction(vectorExpr1, vectorExpr2, fun v1 v2 res ->
                                                    if v1.LongLength = 1L then
                                                        MklFunctions.D_Scalar_Sub_Array(v1.[0], v2.LongLength, v2.NativeArray, res.NativeArray)
                                                    elif v2.LongLength = 1L then
                                                        MklFunctions.D_Array_Sub_Scalar(v2.[0], v1.LongLength, v1.NativeArray, res.NativeArray)
                                                    else
                                                       let len = v1.LongLength
                                                       MklFunctions.D_Array_Sub_Array(len, v1.NativeArray, v2.NativeArray, res.NativeArray)
                      )

    static member (-) (vectorExpr1 : VectorExpr, vector2 : Vector) =
        vectorExpr1 - Var(vector2)

    static member (-) (vector1 : Vector, vectorExpr2 : VectorExpr) =
        Var(vector1) - vectorExpr2

    static member (-) (vectorExpr : VectorExpr, a :  float) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Array_Sub_Scalar(a, v.LongLength, v.NativeArray, res.NativeArray))

    static member (-) (a :  float, vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Scalar_Sub_Array(a, v.LongLength, v.NativeArray, res.NativeArray))

    static member (~-) (vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Minus_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member (.^) (vectorExpr1 : VectorExpr, vectorExpr2 : VectorExpr) =
        BinaryFunction(vectorExpr1, vectorExpr2, fun v1 v2 res ->
                                                    if v1.LongLength = 1L then
                                                        MklFunctions.D_Scalar_Pow_Array(v1.[0], v2.LongLength, v2.NativeArray, res.NativeArray)
                                                    elif v2.LongLength = 1L then
                                                        MklFunctions.D_Array_Pow_scalar(v2.[0], v1.LongLength, v1.NativeArray, res.NativeArray)
                                                    else
                                                       let len = v1.LongLength
                                                       MklFunctions.D_Array_Pow_Array(len, v1.NativeArray, v2.NativeArray, res.NativeArray)
                      )

    static member (.^) (vectorExpr1 : VectorExpr, vector2 : Vector) =
        vectorExpr1 .^ Var(vector2)

    static member (.^) (vector1 : Vector, vectorExpr2 : VectorExpr) =
        Var(vector1) .^ vectorExpr2

    static member (.^) (vectorExpr : VectorExpr, a :  float) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Array_Pow_scalar(a, v.LongLength, v.NativeArray, res.NativeArray))

    static member (.^) (a :  float, vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Scalar_Pow_Array(a, v.LongLength, v.NativeArray, res.NativeArray))

    static member Abs(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Abs_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Sqrt(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Sqrt_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Sin(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Sin_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Cos(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Cos_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Tan(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Tan_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Asin(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_ASin_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Acos(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_ACos_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Atan(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_ATan_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Sinh(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Sinh_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Cosh(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Cosh_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Tanh(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Tanh_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member ASinh(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_ASinh_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member ACosh(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_ACosh_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member ATanh(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_ATanh_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Exp(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Exp_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Expm1(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Expm1_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Log(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Ln_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Log10(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Log10_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Log1p(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Log1p_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Erf(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Erf_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Erfc(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Erfc_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Erfinv(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Erfinv_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Erfcinv(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Erfcinv_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Normcdf(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_CdfNorm_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Norminv(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_CdfNormInv_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Round(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Round_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Ceiling(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Ceil_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Floor(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Floor_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Truncate(vectorExpr : VectorExpr) =
        UnaryFunction(vectorExpr, fun v res -> MklFunctions.D_Trunc_Array(v.LongLength, v.NativeArray, res.NativeArray))
