namespace FCore
#nowarn "9"

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic
open ExplicitConversion

type MatrixAxis = | RowAxis | ColumnAxis

type BoolMatrix(rowCount : int64, colCount : int64, colMajorDataVector : BoolVector) =

    let mutable rowCount = rowCount

    let mutable colCount = colCount

    static let empty = new BoolMatrix(0L, 0L, BoolVector.Empty)

    new(colMajorDataVector : BoolVector) =
        new BoolMatrix(colMajorDataVector.LongLength, 1L, colMajorDataVector)

    new(rowCount : int64, colCount : int64, init : bool) =
        if rowCount < 0L || colCount < 0L then raise (new ArgumentException("Matrix row and column count must be >= 0"))
        let length = rowCount * colCount
        let v = new BoolVector(length, init)
        new BoolMatrix(rowCount, colCount, v)

    new(rowCount : int, colCount : int, init : bool) =
        let rowCount = rowCount |> int64
        let colCount = colCount |> int64
        new BoolMatrix(rowCount, colCount, init)

    new(rowCount : int64, colCount : int64, colMajorDataSeq : seq<bool>) =
        if rowCount < 0L || colCount < 0L then raise (new ArgumentException("Matrix row and column count must be >= 0"))
        let length = rowCount * colCount
        let v = new BoolVector(colMajorDataSeq)
        if v.LongLength <> length then raise (new ArgumentException("Matrix row and column count not compatible with data length"))
        new BoolMatrix(rowCount, colCount, v)

    new(rowCount : int, colCount : int, colMajorDataSeq : seq<bool>) =
        new BoolMatrix(rowCount |> int64, colCount |> int64, colMajorDataSeq)

    new(data : bool[,]) =
        let rowCount = data.GetLength(0)
        let colCount = data.GetLength(1)
        let colMajorData = Array.init (rowCount * colCount) (fun i -> data.[i%rowCount, i/rowCount])
        new BoolMatrix(rowCount, colCount, colMajorData)

    new(dataRows : seq<seq<bool>>) =
        let rows = dataRows |> Seq.length
        let cols = dataRows |> Seq.map Seq.length |> Seq.max
        let data = Array2D.create rows cols false
        dataRows |> Seq.iteri (fun row rowSeq -> rowSeq |> Seq.iteri (fun col x -> data.[row, col] <- x))
        new BoolMatrix(data)

    new(dataRows : seq<bool list>) =
        new BoolMatrix(dataRows |> Seq.map Seq.ofList)

    new(dataRows : seq<bool array>) =
        new BoolMatrix(dataRows |> Seq.map Seq.ofArray)

    new(data : bool) = new BoolMatrix(1L, 1L, data)

    new(rowCount : int, colCount : int, initializer : int -> int -> bool) =
        let data = Array2D.init rowCount colCount initializer
        new BoolMatrix(data)

    member this.RowCount = rowCount |> int

    member this.LongRowCount = rowCount

    member this.ColCount = colCount |> int

    member this.LongColCount = colCount

    member this.ColMajorDataVector = colMajorDataVector

    member this.LongLength = colMajorDataVector.LongLength

    member this.Length = colMajorDataVector.Length

    member this.LongSize = rowCount, colCount

    member this.Size = (int rowCount), (int colCount)

    member this.IsDisposed = colMajorDataVector.IsDisposed

    member this.IsScalar = rowCount = 1L && colCount = 1L

    static member Empty = empty

    static member op_Explicit(v : bool) = new BoolMatrix(v)

    static member op_Explicit(v : bool[,]) = new BoolMatrix(v)

    static member op_Explicit(v : seq<seq<bool>>) = new BoolMatrix(v)

    static member op_Explicit(v : seq<bool list>) = new BoolMatrix(v)

    static member op_Explicit(v : seq<bool array>) = new BoolMatrix(v)

    static member op_Explicit(v : BoolMatrix) = v

    member this.View
        with get(fromIndex : int64, toIndex : int64) =
            colMajorDataVector.View(fromIndex, toIndex)

    member this.View
        with get(fromIndex : int, toIndex : int) = this.View(int64(fromIndex), int64(toIndex))

    member this.ColView
        with get(colIndex : int64) =
            this.View(colIndex * rowCount, (colIndex + 1L) * rowCount - 1L)

    member this.ColView
        with get(colIndex : int) =
            this.ColView(colIndex |> int64)

    member this.GetSlice(fromIndex : int64 option, toIndex : int64 option) =
        colMajorDataVector.GetSlice(fromIndex, toIndex)

    member this.GetSlice(fromIndex : int option, toIndex : int option) =
        this.GetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64)

    member this.SetSlice(fromIndex : int64 option, toIndex : int64 option, value: bool) =
        colMajorDataVector.SetSlice(fromIndex, toIndex, new BoolVector(value))

    member this.SetSlice(fromIndex : int option, toIndex : int option, value: bool) =
        this.SetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64, value)

    member this.SetSlice(fromIndex : int64 option, toIndex : int64 option, value: BoolVector) =
        colMajorDataVector.SetSlice(fromIndex, toIndex, value)

    member this.SetSlice(fromIndex : int option, toIndex : int option, value: BoolVector) =
        this.SetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64, value)

    member this.GetSlice(fromRowIndex : int64 option, toRowIndex : int64 option, fromColIndex : int64 option, toColIndex : int64 option) =
        ArgumentChecks.throwIfContainsDisposed [this]
        let fromRowIndex = defaultArg fromRowIndex 0L
        let toRowIndex = defaultArg toRowIndex (rowCount - 1L)
        if fromRowIndex < 0L || fromRowIndex >= rowCount then raise (new IndexOutOfRangeException())
        if toRowIndex < 0L || toRowIndex >= rowCount then raise (new IndexOutOfRangeException())
        let fromColIndex = defaultArg fromColIndex 0L
        let toColIndex = defaultArg toColIndex (colCount - 1L)
        if fromColIndex < 0L || fromColIndex >= colCount then raise (new IndexOutOfRangeException())
        if toColIndex < 0L || toColIndex >= colCount then raise (new IndexOutOfRangeException())

        if fromRowIndex > toRowIndex || fromColIndex > toColIndex then BoolMatrix.Empty
        else
            let sliceRowCount = toRowIndex - fromRowIndex + 1L
            let sliceColCount = toColIndex - fromColIndex + 1L

            let slice = new BoolMatrix(sliceRowCount, sliceColCount, false)
            for i in 0L..sliceColCount - 1L do 
                let sliceFromIndex = (fromColIndex + i) * rowCount + fromRowIndex
                slice.ColView(i).SetSlice(Some(0L), None, this.View(sliceFromIndex, sliceFromIndex + sliceRowCount - 1L))
            slice 

    member this.GetSlice(fromRowIndex : int64 option, toRowIndex : int64 option, colIndex) =
        this.GetSlice(fromRowIndex, toRowIndex, Some colIndex, Some colIndex).ColMajorDataVector

    member this.GetSlice(rowIndex : int64, fromColIndex : int64 option, toColIndex : int64 option) =
        this.GetSlice(Some rowIndex, Some rowIndex, fromColIndex, toColIndex).ColMajorDataVector

    member this.GetSlice(fromRowIndex : int option, toRowIndex : int option, fromColIndex : int option, toColIndex : int option) =
        this.GetSlice(fromRowIndex |> Option.map int64, toRowIndex |> Option.map int64, fromColIndex |> Option.map int64, toColIndex |> Option.map int64)

    member this.GetSlice(fromRowIndex : int option, toRowIndex : int option, colIndex) =
        this.GetSlice(fromRowIndex, toRowIndex, Some colIndex, Some colIndex).ColMajorDataVector

    member this.GetSlice(rowIndex : int, fromColIndex : int option, toColIndex : int option) =
        this.GetSlice(Some rowIndex, Some rowIndex, fromColIndex, toColIndex).ColMajorDataVector

    member this.SetSlice(fromRowIndex : int64 option, toRowIndex : int64 option, fromColIndex : int64 option, toColIndex : int64 option, value : bool) =
        ArgumentChecks.throwIfContainsDisposed [this]
        let fromRowIndex = defaultArg fromRowIndex 0L
        let toRowIndex = defaultArg toRowIndex (rowCount - 1L)
        if fromRowIndex < 0L || fromRowIndex >= rowCount then raise (new IndexOutOfRangeException())
        if toRowIndex < 0L || toRowIndex >= rowCount then raise (new IndexOutOfRangeException())

        let fromColIndex = defaultArg fromColIndex 0L
        let toColIndex = defaultArg toColIndex (colCount - 1L)
        if fromColIndex < 0L || fromColIndex >= colCount then raise (new IndexOutOfRangeException())
        if toColIndex < 0L || toColIndex >= colCount then raise (new IndexOutOfRangeException())

        for i in fromColIndex..toColIndex do
            this.ColView(i).SetSlice(Some(fromRowIndex), Some(toRowIndex), new BoolVector(value))

    member this.SetSlice(fromRowIndex : int64 option, toRowIndex : int64 option, colIndex : int64, value : bool) =
        this.SetSlice(fromRowIndex, toRowIndex, Some colIndex, Some colIndex, value)

    member this.SetSlice(rowIndex : int64, fromColIndex : int64 option, toColIndex : int64 option, value : bool) =
        this.SetSlice(Some rowIndex, Some rowIndex, fromColIndex, toColIndex, value)

    member this.SetSlice(fromRowIndex : int option, toRowIndex : int option, fromColIndex : int option, toColIndex : int option, value : bool) =
        this.SetSlice(fromRowIndex |> Option.map int64, toRowIndex |> Option.map int64, fromColIndex |> Option.map int64, toColIndex |> Option.map int64, value)

    member this.SetSlice(fromRowIndex : int option, toRowIndex : int option, colIndex : int, value : bool) =
        this.SetSlice(fromRowIndex, toRowIndex, Some colIndex, Some colIndex, value)

    member this.SetSlice(rowIndex : int, fromColIndex : int option, toColIndex : int option, value : bool) =
        this.SetSlice(Some rowIndex, Some rowIndex, fromColIndex, toColIndex, value)

    member this.SetSlice(fromRowIndex : int64 option, toRowIndex : int64 option, fromColIndex : int64 option, toColIndex : int64 option, value : BoolMatrix) =
        ArgumentChecks.throwIfContainsDisposed [this;value]
        if value.LongLength = 1L then
            this.SetSlice(fromRowIndex, toRowIndex, fromColIndex, toColIndex, (value.[0L]:bool))
        else
            let fromRowIndex = defaultArg fromRowIndex 0L
            let toRowIndex = defaultArg toRowIndex (rowCount - 1L)
            if fromRowIndex < 0L || fromRowIndex >= rowCount then raise (new IndexOutOfRangeException())
            if toRowIndex < 0L || toRowIndex >= rowCount then raise (new IndexOutOfRangeException())

            let fromColIndex = defaultArg fromColIndex 0L
            let toColIndex = defaultArg toColIndex (colCount - 1L)
            if fromColIndex < 0L || fromColIndex >= colCount then raise (new IndexOutOfRangeException())
            if toColIndex < 0L || toColIndex >= colCount then raise (new IndexOutOfRangeException())

            if (fromRowIndex > toRowIndex || fromColIndex > toColIndex) && value.LongLength = 0L then ()
            else
                let sliceRowCount = toRowIndex - fromRowIndex + 1L
                let sliceColCount = toColIndex - fromColIndex + 1L
                if sliceRowCount <> value.LongRowCount || sliceColCount <> value.LongColCount then
                    raise (new ArgumentException())
                for i in 0L..sliceColCount - 1L do
                    this.ColView(fromColIndex + i).SetSlice(Some(fromRowIndex), Some(toRowIndex), value.ColView(i))

    member this.SetSlice(fromRowIndex : int64 option, toRowIndex : int64 option, colIndex : int64, value : BoolVector) =
        ArgumentChecks.throwIfContainsDisposed [this]
        ArgumentChecks.throwIfContainsDisposed [value]
        let value = new BoolMatrix(value)
        this.SetSlice(fromRowIndex, toRowIndex, Some colIndex, Some colIndex, value)

    member this.SetSlice(rowIndex : int64, fromColIndex : int64 option, toColIndex : int64 option, value : BoolVector) =
        ArgumentChecks.throwIfContainsDisposed [this]
        ArgumentChecks.throwIfContainsDisposed [value]
        let value = new BoolMatrix(1L, value.LongLength, value)
        this.SetSlice(Some rowIndex, Some rowIndex, fromColIndex, toColIndex, value)

    member this.SetSlice(fromRowIndex : int option, toRowIndex : int option, fromColIndex : int option, toColIndex : int option, value : BoolMatrix) =
       this.SetSlice(fromRowIndex |> Option.map int64, toRowIndex |> Option.map int64, fromColIndex |> Option.map int64, toColIndex |> Option.map int64, value)

    member this.SetSlice(fromRowIndex : int option, toRowIndex : int option, colIndex : int, value : BoolVector) =
        ArgumentChecks.throwIfContainsDisposed [this]
        ArgumentChecks.throwIfContainsDisposed [value]
        let value = new BoolMatrix(value)
        this.SetSlice(fromRowIndex, toRowIndex, Some colIndex, Some colIndex, value)

    member this.SetSlice(rowIndex : int, fromColIndex : int option, toColIndex : int option, value : BoolVector) =
        ArgumentChecks.throwIfContainsDisposed [this]
        ArgumentChecks.throwIfContainsDisposed [value]
        let value = new BoolMatrix(1L, value.LongLength, value)
        this.SetSlice(Some rowIndex, Some rowIndex, fromColIndex, toColIndex, value)

    member this.Item
        with get(i : int64) =
            colMajorDataVector.[i]
        and set (i : int64) value =
            colMajorDataVector.[i] <- value

    member this.Item
        with get(i : int) = this.[i |> int64]
        and set (i : int) value =
            this.[i |> int64] <- value

    member this.Item
        with get(indices : int64 seq) = 
            colMajorDataVector.[indices]
        and set (indices : int64 seq) (value : BoolVector) =
            colMajorDataVector.[indices] <- value

    member this.Item
        with get(indices : int seq) = 
            colMajorDataVector.[indices]
        and set (indices : int seq) (value : BoolVector) =
            colMajorDataVector.[indices] <- value

    member this.Item
        with get(boolVector : BoolVector) = 
            colMajorDataVector.[boolVector]

        and set (boolVector : BoolVector) (value : BoolVector) =
            colMajorDataVector.[boolVector] <- value

    member this.Item
        with get(rowIndex : int64, colIndex : int64) =
            colMajorDataVector.[colIndex * rowCount + rowIndex]
        and set (rowIndex : int64, colIndex : int64) value =
            colMajorDataVector.[colIndex * rowCount + rowIndex] <- value

    member this.Item
        with get(rowIndex : int, colIndex : int) =
            this.[rowIndex |> int64, colIndex |> int64]
        and set (rowIndex : int, colIndex : int) value =
            this.[rowIndex |> int64, colIndex |> int64] <- value

    member this.Item
        with get(rowIndices : int64 seq, colIndices : int64 seq) = 
            ArgumentChecks.throwIfContainsDisposed [this]
            let rowIndices = rowIndices |> Seq.toArray
            let rowCount = rowIndices.GetLongLength(0)
            let colIndices = colIndices |> Seq.toArray
            let colCount = colIndices.GetLongLength(0)
            let res = new BoolMatrix(rowCount, colCount, false)
            rowIndices |> Array.iteri (fun i rowIndex ->
                                           colIndices |> Array.iteri (fun j colIndex -> res.[i, j] <- this.[rowIndex, colIndex])
                                      )
            res

        and set (rowIndices : int64 seq, colIndices : int64 seq) (value : BoolMatrix) =
            ArgumentChecks.throwIfContainsDisposed [this;value]
            let rowIndices = rowIndices |> Seq.toArray
            let colIndices = colIndices |> Seq.toArray
            if value.LongLength = 1L then
                let value = value.[0L]
                rowIndices |> Array.iteri (fun i rowIndex -> colIndices |> Array.iteri (fun j colIndex -> this.[rowIndex, colIndex] <- value))
            else
                rowIndices |> Array.iteri (fun i rowIndex -> colIndices |> Array.iteri (fun j colIndex -> this.[rowIndex, colIndex] <- value.[i, j]))

    member this.Item
        with get(rowIndex : int64, colIndices : int64 seq) = 
            this.[[|rowIndex|], colIndices].ColMajorDataVector

        and set (rowIndex : int64, colIndices : int64 seq) (value : BoolVector) =
            let value = new BoolMatrix(1L, value.LongLength, value)
            this.[[|rowIndex|], colIndices] <- value

    member this.Item
        with get(rowIndices : int64 seq, colIndex : int64) = 
            this.[rowIndices, [|colIndex|]].ColMajorDataVector

        and set (rowIndices : int64 seq, colIndex : int64) (value : BoolVector) =
            let value = new BoolMatrix(value)
            this.[rowIndices, [|colIndex|]] <- value

    member this.Item
        with get(rowIndices : int seq, colIndices : int seq) = 
            let rowIndices = rowIndices |> Seq.map int64
            let colIndices = colIndices |> Seq.map int64
            this.[rowIndices, colIndices]

        and set (rowIndices : int seq, colIndices : int seq) (value : BoolMatrix) =
            let rowIndices = rowIndices |> Seq.map int64
            let colIndices = colIndices |> Seq.map int64
            this.[rowIndices, colIndices] <- value

    member this.Item
        with get(rowIndex : int, colIndices : int seq) = 
            this.[[|rowIndex|], colIndices].ColMajorDataVector

        and set (rowIndex : int, colIndices : int seq) (value : BoolVector) =
            let value = new BoolMatrix(1L, value.LongLength, value)
            this.[[|rowIndex|], colIndices] <- value

    member this.Item
        with get(rowIndices : int seq, colIndex : int) = 
            this.[rowIndices, [|colIndex|]].ColMajorDataVector

        and set (rowIndices : int seq, colIndex : int) (value : BoolVector) =
            let value = new BoolMatrix(value)
            this.[rowIndices, [|colIndex|]] <- value

    member this.Item
        with get(boolMatrix : BoolMatrix) = 
            colMajorDataVector.[boolMatrix.ColMajorDataVector]

        and set (boolMatrix : BoolMatrix) (value : BoolVector) =
            colMajorDataVector.[boolMatrix.ColMajorDataVector] <- value

    member this.ToArray2D() =
        ArgumentChecks.throwIfContainsDisposed [this]
        Array2D.init this.RowCount this.ColCount (fun i j -> this.[i, j])

    static member Identity(rows : int64, cols : int64) =
        let res = new BoolMatrix(rows, cols, false)
        MklFunctions.B_Identity(rows, cols, res.ColMajorDataVector.NativeArray)
        res

    static member Identity(rows : int, cols : int) =
        BoolMatrix.Identity(rows |> int64, cols |> int64)

    static member Copy(matrix : BoolMatrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let rows = matrix.LongRowCount
        let cols = matrix.LongColCount
        let res = new BoolMatrix(rows, cols, false)
        MklFunctions.B_Copy_Array(matrix.ColMajorDataVector.LongLength, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    member this.AsExpr
        with get() = 
            ArgumentChecks.throwIfContainsDisposed [this]
            if this.LongLength = 1L then BoolMatrixExpr.Scalar(this.[0L, 0L])
            else BoolMatrixExpr.Var(this)


    static member (==) (matrix1: BoolMatrix, matrix2: BoolMatrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        matrix1 = matrix2

    static member (!=) (matrix1: BoolMatrix, matrix2: BoolMatrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        matrix1 <> matrix2

    static member (==) (matrix: BoolMatrix, a : bool) =
        matrix.ColMajorDataVector == a

    static member (!=) (matrix: BoolMatrix, a : bool) =
        not (matrix == a)

    static member (==) (a : bool, matrix: BoolMatrix) =
        matrix.ColMajorDataVector == a

    static member (!=) (a : bool, matrix: BoolMatrix) =
        not (matrix == a)


    static member (.<) (matrix1: BoolMatrix, matrix2: BoolMatrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        let rowCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongRowCount matrix2.LongRowCount
        let colCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongColCount matrix2.LongColCount
        new BoolMatrix(rowCount, colCount, matrix1.ColMajorDataVector .< matrix2.ColMajorDataVector)

    static member (.<=) (matrix1: BoolMatrix, matrix2: BoolMatrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        let rowCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongRowCount matrix2.LongRowCount
        let colCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongColCount matrix2.LongColCount
        new BoolMatrix(rowCount, colCount, matrix1.ColMajorDataVector .<= matrix2.ColMajorDataVector)

    static member (.>) (matrix1: BoolMatrix, matrix2: BoolMatrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        let rowCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongRowCount matrix2.LongRowCount
        let colCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongColCount matrix2.LongColCount
        new BoolMatrix(rowCount, colCount, matrix1.ColMajorDataVector .> matrix2.ColMajorDataVector)

    static member (.>=) (matrix1: BoolMatrix, matrix2: BoolMatrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        let rowCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongRowCount matrix2.LongRowCount
        let colCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongColCount matrix2.LongColCount
        new BoolMatrix(rowCount, colCount, matrix1.ColMajorDataVector .>= matrix2.ColMajorDataVector)

    static member (.=) (matrix1: BoolMatrix, matrix2: BoolMatrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        let rowCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongRowCount matrix2.LongRowCount
        let colCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongColCount matrix2.LongColCount
        new BoolMatrix(rowCount, colCount, matrix1.ColMajorDataVector .= matrix2.ColMajorDataVector)

    static member (.<>) (matrix1: BoolMatrix, matrix2: BoolMatrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        let rowCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongRowCount matrix2.LongRowCount
        let colCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongColCount matrix2.LongColCount
        new BoolMatrix(rowCount, colCount, matrix1.ColMajorDataVector .<> matrix2.ColMajorDataVector)




    static member (.<) (matrix: BoolMatrix, a : bool) =
        new BoolMatrix(matrix.LongRowCount, matrix.LongColCount, matrix.ColMajorDataVector .< a)

    static member (.<=) (matrix: BoolMatrix, a : bool) =
        new BoolMatrix(matrix.LongRowCount, matrix.LongColCount, matrix.ColMajorDataVector .<= a)

    static member (.>) (matrix: BoolMatrix, a : bool) =
        new BoolMatrix(matrix.LongRowCount, matrix.LongColCount, matrix.ColMajorDataVector .> a)

    static member (.>=) (matrix: BoolMatrix, a : bool) =
        new BoolMatrix(matrix.LongRowCount, matrix.LongColCount, matrix.ColMajorDataVector .>= a)

    static member (.=) (matrix: BoolMatrix, a : bool) =
        new BoolMatrix(matrix.LongRowCount, matrix.LongColCount, matrix.ColMajorDataVector .= a)

    static member (.<>) (matrix: BoolMatrix, a : bool) =
        new BoolMatrix(matrix.LongRowCount, matrix.LongColCount, matrix.ColMajorDataVector .<> a)



    static member (.<) (a : bool, matrix: BoolMatrix) =
        matrix .> a

    static member (.<=) (a : bool, matrix: BoolMatrix) =
        matrix .>= a

    static member (.>) (a : bool, matrix: BoolMatrix) =
        matrix .< a

    static member (.>=) (a : bool, matrix: BoolMatrix) =
        matrix .<= a

    static member (.=) (a : bool, matrix: BoolMatrix) =
        matrix .= a

    static member (.<>) (a : bool, matrix: BoolMatrix) =
        matrix .<> a


    static member Max(matrix1: BoolMatrix, matrix2: BoolMatrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        let rowCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongRowCount matrix2.LongRowCount
        let colCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongColCount matrix2.LongColCount
        new BoolMatrix(rowCount, colCount, BoolVector.Max(matrix1.ColMajorDataVector, matrix2.ColMajorDataVector))

    static member Min(matrix1: BoolMatrix, matrix2: BoolMatrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        let rowCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongRowCount matrix2.LongRowCount
        let colCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongColCount matrix2.LongColCount
        new BoolMatrix(rowCount, colCount, BoolVector.Min(matrix1.ColMajorDataVector, matrix2.ColMajorDataVector))

    static member Max(matrix : BoolMatrix, a : bool) =
        let a = new BoolMatrix(a)
        BoolMatrix.Max(matrix, a)

    static member Min(matrix : BoolMatrix, a : bool) =
        let a = new BoolMatrix(a)
        BoolMatrix.Min(matrix, a)

    static member Max(a : bool, matrix : BoolMatrix) = 
        BoolMatrix.Max(matrix, a)

    static member Min(a : bool, matrix : BoolMatrix) = 
        BoolMatrix.Min(matrix, a)

    static member (.&&) (matrix1: BoolMatrix, matrix2: BoolMatrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        let rowCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongRowCount matrix2.LongRowCount
        let colCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongColCount matrix2.LongColCount
        new BoolMatrix(rowCount, colCount, matrix1.ColMajorDataVector .&& matrix2.ColMajorDataVector)

    static member (.||) (matrix1: BoolMatrix, matrix2: BoolMatrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        let rowCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongRowCount matrix2.LongRowCount
        let colCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongColCount matrix2.LongColCount
        new BoolMatrix(rowCount, colCount, matrix1.ColMajorDataVector .|| matrix2.ColMajorDataVector)

    static member (.&&) (matrix : BoolMatrix, a : bool) =
        let a = new BoolMatrix(a)
        matrix .&& a

    static member (.||) (matrix : BoolMatrix, a : bool) =
        let a = new BoolMatrix(a)
        matrix .|| a

    static member (.&&) (a : bool, matrix : BoolMatrix) =
        let a = new BoolMatrix(a)
        matrix .&& a

    static member (.||) (a : bool, matrix : BoolMatrix) =
        let a = new BoolMatrix(a)
        matrix .|| a

    static member Not (matrix : BoolMatrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        new BoolMatrix(matrix.LongRowCount, matrix.LongColCount, BoolVector.Not matrix.ColMajorDataVector)

    interface IDisposable with
        member this.Dispose() = this.ColMajorDataVector.DoDispose(true)

    override this.Finalize() =
            try this.ColMajorDataVector.DoDispose(false) with _ -> ()

    override this.ToString() = 
        ArgumentChecks.throwIfContainsDisposed [this]
        (this:>IFormattable).ToString(GenericFormatting.GenericFormat.Instance.GetFormat<bool>() true, null)

    override this.Equals(yobj) =
        match yobj with
        | :? BoolMatrix as y ->
            ArgumentChecks.throwIfContainsDisposed [this;y]
            if this.LongLength = 0L && y.LongLength = 0L then true
            elif this.LongRowCount <> y.LongRowCount || this.LongColCount <> y.LongColCount then false
            else 
                this.ColMajorDataVector = y.ColMajorDataVector
        | _ -> false
 
    override this.GetHashCode() = 
        ArgumentChecks.throwIfContainsDisposed [this]
        hash (this.LongRowCount, this.LongColCount, this.ColMajorDataVector.NativeArray)

    interface IFormattable with
        member this.ToString(format, provider) = 
            ArgumentChecks.throwIfContainsDisposed [this]
            let maxRows, maxCols = DisplayControl.MaxDisplaySize
            let showRows = max 0L (min (maxRows |> int64) rowCount) |> int
            let showCols = max 0L (min (maxCols |> int64) colCount) |> int
            let moreRows = rowCount > (showRows |> int64)
            let moreCols = colCount > (showCols |> int64)
            let arr = Array2D.init showRows showCols (fun row col -> this.[row, col])
            let formattedArray = DisplayControl.FormatArray2D(arr, format, moreRows, moreCols)
            sprintf "Size = [%d,%d]\r\n%s" rowCount colCount formattedArray

//**************************************BoolMatrixExpr**************************************************************************************

and BoolMatrixExpr = 
    | Scalar of bool
    | Var of BoolMatrix
    | UnaryFunction of BoolMatrixExpr * (BoolVector -> BoolVector -> unit)
    | BinaryFunction of BoolMatrixExpr * BoolMatrixExpr * (BoolVector -> BoolVector -> BoolVector -> unit)
    | BinaryMatrixFunction of MatrixExpr * MatrixExpr * (Vector -> Vector -> BoolVector -> unit)
    | IfFunction of BoolMatrixExpr * BoolMatrixExpr * BoolMatrixExpr

    member this.AsBoolVectorExpr =
        match this with
            | Scalar(v) -> BoolVectorExpr.Scalar(v)
            | Var(v) -> v.ColMajorDataVector.AsExpr
            | UnaryFunction(expr, f) -> BoolVectorExpr.UnaryFunction(expr.AsBoolVectorExpr, f)
            | BinaryFunction(expr1, expr2, f) -> BoolVectorExpr.BinaryFunction(expr1.AsBoolVectorExpr, expr2.AsBoolVectorExpr, f)
            | BinaryMatrixFunction(expr1, expr2, f) -> BoolVectorExpr.BinaryVectorFunction(expr1.AsVectorExpr, expr2.AsVectorExpr, f) 
            | IfFunction(ifExpr, trueExpr, falseExpr) -> BoolVectorExpr.IfFunction(ifExpr.AsBoolVectorExpr, trueExpr.AsBoolVectorExpr, falseExpr.AsBoolVectorExpr)

    member this.Size =
        match this with
            | Scalar(_) -> Some(1L, 1L)
            | Var(v) -> Some(v.LongRowCount, v.LongColCount)
            | UnaryFunction(v, _) -> v.Size
            | BinaryFunction(v1, v2, _) -> ArgumentChecks.getElementwiseSize v1.Size v2.Size 
            | BinaryMatrixFunction(v1, v2, _) -> ArgumentChecks.getElementwiseSize v1.Size v2.Size                  
            | IfFunction(v1, v2, v3) -> ArgumentChecks.getElementwiseSizeIf v1.Size v2.Size v3.Size  

    static member EvalIn(matrixExpr : BoolMatrixExpr, res : BoolMatrix option) =
        let size = matrixExpr.Size
        let res = 
            match size with
                | None -> raise (new ArgumentException("Elementwise size mismatch"))
                | Some(r,c) ->
                    match res with
                        | Some(v) when (r,c) <> v.LongSize -> raise (new ArgumentException("Elementwise size mismatch")) 
                        | Some(v) -> v
                        | None -> new BoolMatrix(r, c, false)
        ArgumentChecks.throwIfContainsDisposed [res]
        if res.LongSize <> (0L, 0L) then
            BoolVectorExpr.EvalIn(matrixExpr.AsBoolVectorExpr, Some res.ColMajorDataVector) |> ignore
        res

    static member (.<) (matrix1 : BoolMatrixExpr, matrix2 : BoolMatrixExpr) =
        BinaryFunction(matrix1, matrix2, 
                        fun v1 v2 res -> MklFunctions.B_Arrays_LessThan(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.<) (matrix1 : BoolMatrixExpr, matrix2 : BoolMatrix) =
        matrix1 .< matrix2.AsExpr

    static member (.<) (matrix1 : BoolMatrix, matrix2 : BoolMatrixExpr) =
        matrix1.AsExpr .< matrix2

    static member (.<) (matrix : BoolMatrixExpr, a : bool) =
        matrix .< Scalar(a)

    static member (.<) (a : bool, matrix : BoolMatrixExpr) =
        Scalar(a) .< matrix


    static member (.<=) (matrix1 : BoolMatrixExpr, matrix2 : BoolMatrixExpr) =
        BinaryFunction(matrix1, matrix2, 
                       fun v1 v2 res -> MklFunctions.B_Arrays_LessEqual(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.<=) (matrix1 : BoolMatrixExpr, matrix2 : BoolMatrix) =
        matrix1 .<= matrix2.AsExpr

    static member (.<=) (matrix1 : BoolMatrix, matrix2 : BoolMatrixExpr) =
        matrix1.AsExpr .<= matrix2

    static member (.<=) (matrix : BoolMatrixExpr, a : bool) =
        matrix .<= Scalar(a)

    static member (.<=) (a : bool, matrix : BoolMatrixExpr) =
        Scalar(a) .<= matrix


    static member (.>) (matrix1 : BoolMatrixExpr, matrix2 : BoolMatrixExpr) =
        BinaryFunction(matrix1, matrix2, 
                       fun v1 v2 res -> MklFunctions.B_Arrays_GreaterThan(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.>) (matrix1 : BoolMatrixExpr, matrix2 : BoolMatrix) =
        matrix1 .> matrix2.AsExpr

    static member (.>) (matrix1 : BoolMatrix, matrix2 : BoolMatrixExpr) =
        matrix1.AsExpr .> matrix2

    static member (.>) (matrix : BoolMatrixExpr, a : bool) =
        matrix .> Scalar(a)

    static member (.>) (a : bool, matrix : BoolMatrixExpr) =
        Scalar(a) .> matrix


    static member (.>=) (matrix1 : BoolMatrixExpr, matrix2 : BoolMatrixExpr) =
        BinaryFunction(matrix1, matrix2, 
                       fun v1 v2 res -> MklFunctions.B_Arrays_GreaterEqual(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.>=) (matrix1 : BoolMatrixExpr, matrix2 : BoolMatrix) =
        matrix1 .>= matrix2.AsExpr

    static member (.>=) (matrix1 : BoolMatrix, matrix2 : BoolMatrixExpr) =
        matrix1.AsExpr .>= matrix2

    static member (.>=) (matrix : BoolMatrixExpr, a : bool) =
        matrix .>= Scalar(a)

    static member (.>=) (a : bool, matrix : BoolMatrixExpr) =
        Scalar(a) .>= matrix


    static member (.=) (matrix1 : BoolMatrixExpr, matrix2 : BoolMatrixExpr) =
        BinaryFunction(matrix1, matrix2, 
                       fun v1 v2 res -> MklFunctions.B_Arrays_EqualElementwise(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.=) (matrix1 : BoolMatrixExpr, matrix2 : BoolMatrix) =
        matrix1 .= matrix2.AsExpr

    static member (.=) (matrix1 : BoolMatrix, matrix2 : BoolMatrixExpr) =
        matrix1.AsExpr .= matrix2

    static member (.=) (matrix : BoolMatrixExpr, a : bool) =
        matrix .= Scalar(a)

    static member (.=) (a : bool, matrix : BoolMatrixExpr) =
        Scalar(a) .= matrix

    static member (.<>) (matrix1 : BoolMatrixExpr, matrix2 : BoolMatrixExpr) =
        BinaryFunction(matrix1, matrix2, 
                       fun v1 v2 res -> MklFunctions.B_Arrays_NotEqualElementwise(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.<>) (matrix1 : BoolMatrixExpr, matrix2 : BoolMatrix) =
        matrix1 .<> matrix2.AsExpr

    static member (.<>) (matrix1 : BoolMatrix, matrix2 : BoolMatrixExpr) =
        matrix1.AsExpr .<> matrix2

    static member (.<>) (matrix : BoolMatrixExpr, a : bool) =
        matrix .<> Scalar(a)

    static member (.<>) (a : bool, matrix : BoolMatrixExpr) =
        Scalar(a) .<> matrix

    static member Min (matrix1 : BoolMatrixExpr, matrix2 : BoolMatrixExpr) =
        BinaryFunction(matrix1, matrix2, 
                       fun v1 v2 res -> MklFunctions.B_Min_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))

    static member Min (matrix1 : BoolMatrixExpr, matrix2 : BoolMatrix) =
       BoolMatrixExpr.Min(matrix1, matrix2.AsExpr)

    static member Min (matrix1 : BoolMatrix, matrix2 : BoolMatrixExpr) =
        BoolMatrixExpr.Min(matrix1.AsExpr, matrix2)

    static member Min (matrix : BoolMatrixExpr, a : bool) =
        BoolMatrixExpr.Min(matrix, Scalar(a))

    static member Min (a : bool, matrix : BoolMatrixExpr) =
        BoolMatrixExpr.Min(Scalar(a), matrix)

    static member Max (matrix1 : BoolMatrixExpr, matrix2 : BoolMatrixExpr) =
        BinaryFunction(matrix1, matrix2, 
                       fun v1 v2 res -> MklFunctions.B_Max_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))

    static member Max (matrix1 : BoolMatrixExpr, matrix2 : BoolMatrix) =
       BoolMatrixExpr.Max(matrix1, matrix2.AsExpr)

    static member Max (matrix1 : BoolMatrix, matrix2 : BoolMatrixExpr) =
        BoolMatrixExpr.Max(matrix1.AsExpr, matrix2)

    static member Max (matrix : BoolMatrixExpr, a : bool) =
        BoolMatrixExpr.Max(matrix, Scalar(a))

    static member Max (a : bool, matrix : BoolMatrixExpr) =
        BoolMatrixExpr.Max(Scalar(a), matrix)

    static member (.&&) (matrix1 : BoolMatrixExpr, matrix2 : BoolMatrixExpr) =
        BinaryFunction(matrix1, matrix2, 
                       fun v1 v2 res -> MklFunctions.B_And_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))

    static member (.&&) (matrix1 : BoolMatrixExpr, matrix2 : BoolMatrix) =
        matrix1 .&& matrix2.AsExpr

    static member (.&&) (matrix1 : BoolMatrix, matrix2 : BoolMatrixExpr) =
        matrix1.AsExpr .&& matrix2

    static member (.&&) (matrix : BoolMatrixExpr, a : bool) =
        matrix .&& Scalar(a)

    static member (.&&) (a : bool, matrix : BoolMatrixExpr) =
        Scalar(a) .&& matrix

    static member (.||) (matrix1 : BoolMatrixExpr, matrix2 : BoolMatrixExpr) =
        BinaryFunction(matrix1, matrix2, 
                       fun v1 v2 res -> MklFunctions.B_Or_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))

    static member (.||) (matrix1 : BoolMatrixExpr, matrix2 : BoolMatrix) =
        matrix1 .|| matrix2.AsExpr

    static member (.||) (matrix1 : BoolMatrix, matrix2 : BoolMatrixExpr) =
        matrix1.AsExpr .|| matrix2

    static member (.||) (matrix : BoolMatrixExpr, a : bool) =
        matrix .|| Scalar(a)

    static member (.||) (a : bool, matrix : BoolMatrixExpr) =
        Scalar(a) .|| matrix

    static member Not (matrix : BoolMatrixExpr) =
        UnaryFunction(matrix, fun v res -> MklFunctions.B_Not_Array(v.LongLength, v.NativeArray, res.NativeArray))

//***************************************************Matrix**************************************************************************************

and Matrix(rowCount : int64, colCount : int64, colMajorDataVector : Vector) =

    let mutable rowCount = rowCount
    let mutable colCount = colCount

    static let empty = new Matrix(0L, 0L, Vector.Empty)

    new(colMajorDataVector : Vector) =
        new Matrix(colMajorDataVector.LongLength, 1L, colMajorDataVector)

    new(rowCount : int64, colCount : int64, init : float) =
        if rowCount < 0L || colCount < 0L then raise (new ArgumentException("Matrix row and column count must be >= 0"))
        let length = rowCount * colCount
        let v = new Vector(length, init)
        new Matrix(rowCount, colCount, v)

    new(rowCount : int, colCount : int, init : float) =
        let rowCount = rowCount |> int64
        let colCount = colCount |> int64
        new Matrix(rowCount, colCount, init)

    new(rowCount : int64, colCount : int64, colMajorDataSeq : seq<float>) =
        if rowCount < 0L || colCount < 0L then raise (new ArgumentException("Matrix row and column count must be >= 0"))
        let length = rowCount * colCount
        let v = new Vector(colMajorDataSeq)
        if v.LongLength <> length then raise (new ArgumentException("Matrix row and column count not compatible with data length"))
        new Matrix(rowCount, colCount, v)

    new(rowCount : int, colCount : int, colMajorDataSeq : seq<float>) =
        new Matrix(rowCount |> int64, colCount |> int64, colMajorDataSeq)

    new(data : float[,]) =
        let rowCount = data.GetLength(0)
        let colCount = data.GetLength(1)
        let colMajorData = Array.init (rowCount * colCount) (fun i -> data.[i%rowCount, i/rowCount])
        new Matrix(rowCount, colCount, colMajorData)

    new(dataRows : seq<seq<float>>) =
        let rows = dataRows |> Seq.length
        let cols = dataRows |> Seq.map Seq.length |> Seq.max
        let data = Array2D.create rows cols 0.0
        dataRows |> Seq.iteri (fun row rowSeq -> rowSeq |> Seq.iteri (fun col x -> data.[row, col] <- x))
        new Matrix(data)

    new(dataRows : seq<float list>) =
        new Matrix(dataRows |> Seq.map Seq.ofList)

    new(dataRows : seq<float array>) =
        new Matrix(dataRows |> Seq.map Seq.ofArray)

    new(data : float) = new Matrix(1L, 1L, data)

    new(rowCount : int, colCount : int, initializer : int -> int -> float) =
        let data = Array2D.init rowCount colCount initializer
        new Matrix(data)

    member this.RowCount = rowCount |> int

    member this.LongRowCount = rowCount

    member this.ColCount = colCount |> int

    member this.LongColCount = colCount

    member this.ColMajorDataVector = colMajorDataVector

    member this.LongLength = colMajorDataVector.LongLength

    member this.Length = colMajorDataVector.Length

    member this.LongSize = rowCount, colCount

    member this.Size = (int rowCount), (int colCount)

    member this.IsDisposed = colMajorDataVector.IsDisposed

    member this.IsScalar = rowCount = 1L && colCount = 1L

    static member Empty = empty

    static member op_Explicit(v : float) = new Matrix(v)

    static member op_Explicit(v : float[,]) = new Matrix(v)

    static member op_Explicit(v : seq<seq<float>>) = new Matrix(v)

    static member op_Explicit(v : seq<float list>) = new Matrix(v)

    static member op_Explicit(v : seq<float array>) = new Matrix(v)

    static member op_Explicit(v : Matrix) = v

    member this.View
        with get(fromIndex : int64, toIndex : int64) =
            colMajorDataVector.View(fromIndex, toIndex)

    member this.View
        with get(fromIndex : int, toIndex : int) = this.View(int64(fromIndex), int64(toIndex))

    member this.ColView
        with get(colIndex : int64) =
            this.View(colIndex * rowCount, (colIndex + 1L) * rowCount - 1L)

    member this.ColView
        with get(colIndex : int) =
            this.ColView(colIndex |> int64)

    member this.Diag
        with get(offset : int64) =
            ArgumentChecks.throwIfContainsDisposed [this]
            let len = if offset < 0L then 
                                if rowCount + offset < colCount then rowCount + offset else colCount
                            else
                                if colCount - offset < rowCount then colCount - offset else rowCount
            if len <= 0L then Vector.Empty
            else
                let res = new Vector(len, 0.0)
                MklFunctions.D_Get_Diag(rowCount, offset, len, colMajorDataVector.NativeArray, res.NativeArray)
                res
        and set (offset : int64) (diag : Vector) =
            ArgumentChecks.throwIfContainsDisposed [this]
            ArgumentChecks.throwIfContainsDisposed [diag]
            let n = diag.LongLength
            if this.LongLength = 0L || n = 0L then ()
            else
                let k = (if offset < 0L then -offset else offset)
                if (this.LongRowCount, this.LongColCount) <> (n + k, n + k) then
                    raise (new ArgumentException("Matrix size mismatch"))
                MklFunctions.D_Set_Diag(diag.LongLength, offset, diag.NativeArray, colMajorDataVector.NativeArray) 

    member this.Diag
        with get(offset : int) =
            this.Diag (offset |> int64)
        and set (offset : int) (diag : Vector) =
            this.Diag(offset |> int64) <- diag

    member this.GetSlice(fromIndex : int64 option, toIndex : int64 option) =
        colMajorDataVector.GetSlice(fromIndex, toIndex)

    member this.GetSlice(fromIndex : int option, toIndex : int option) =
        this.GetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64)

    member this.SetSlice(fromIndex : int64 option, toIndex : int64 option, value: float) =
        colMajorDataVector.SetSlice(fromIndex, toIndex, value)

    member this.SetSlice(fromIndex : int option, toIndex : int option, value: float) =
        this.SetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64, value)

    member this.SetSlice(fromIndex : int64 option, toIndex : int64 option, value: Vector) =
        colMajorDataVector.SetSlice(fromIndex, toIndex, value)

    member this.SetSlice(fromIndex : int option, toIndex : int option, value: Vector) =
        this.SetSlice(fromIndex |> Option.map int64, toIndex |> Option.map int64, value)

    member this.GetSlice(fromRowIndex : int64 option, toRowIndex : int64 option, fromColIndex : int64 option, toColIndex : int64 option) =
        ArgumentChecks.throwIfContainsDisposed [this]
        let fromRowIndex = defaultArg fromRowIndex 0L
        let toRowIndex = defaultArg toRowIndex (rowCount - 1L)
        if fromRowIndex < 0L || fromRowIndex >= rowCount then raise (new IndexOutOfRangeException())
        if toRowIndex < 0L || toRowIndex >= rowCount then raise (new IndexOutOfRangeException())

        let fromColIndex = defaultArg fromColIndex 0L
        let toColIndex = defaultArg toColIndex (colCount - 1L)
        if fromColIndex < 0L || fromColIndex >= colCount then raise (new IndexOutOfRangeException())
        if toColIndex < 0L || toColIndex >= colCount then raise (new IndexOutOfRangeException())

        if fromRowIndex > toRowIndex || fromColIndex > toColIndex then Matrix.Empty
        else
            let sliceRowCount = toRowIndex - fromRowIndex + 1L
            let sliceColCount = toColIndex - fromColIndex + 1L

            let slice = new Matrix(sliceRowCount, sliceColCount, 0.0)
            for i in 0L..sliceColCount - 1L do
                let sliceFromIndex = (fromColIndex + i) * rowCount + fromRowIndex
                slice.ColView(i).SetSlice(Some(0L), None, this.View(sliceFromIndex, sliceFromIndex + sliceRowCount - 1L))
            slice 

    member this.GetSlice(fromRowIndex : int64 option, toRowIndex : int64 option, colIndex) =
        this.GetSlice(fromRowIndex, toRowIndex, Some colIndex, Some colIndex).ColMajorDataVector

    member this.GetSlice(rowIndex : int64, fromColIndex : int64 option, toColIndex : int64 option) =
        this.GetSlice(Some rowIndex, Some rowIndex, fromColIndex, toColIndex).ColMajorDataVector

    member this.GetSlice(fromRowIndex : int option, toRowIndex : int option, fromColIndex : int option, toColIndex : int option) =
        this.GetSlice(fromRowIndex |> Option.map int64, toRowIndex |> Option.map int64, fromColIndex |> Option.map int64, toColIndex |> Option.map int64)

    member this.GetSlice(fromRowIndex : int option, toRowIndex : int option, colIndex) =
        this.GetSlice(fromRowIndex, toRowIndex, Some colIndex, Some colIndex).ColMajorDataVector

    member this.GetSlice(rowIndex : int, fromColIndex : int option, toColIndex : int option) =
        this.GetSlice(Some rowIndex, Some rowIndex, fromColIndex, toColIndex).ColMajorDataVector

    member this.SetSlice(fromRowIndex : int64 option, toRowIndex : int64 option, fromColIndex : int64 option, toColIndex : int64 option, value : float) =
        ArgumentChecks.throwIfContainsDisposed [this]
        let fromRowIndex = defaultArg fromRowIndex 0L
        let toRowIndex = defaultArg toRowIndex (rowCount - 1L)
        if fromRowIndex < 0L || fromRowIndex >= rowCount then raise (new IndexOutOfRangeException())
        if toRowIndex < 0L || toRowIndex >= rowCount then raise (new IndexOutOfRangeException())

        let fromColIndex = defaultArg fromColIndex 0L
        let toColIndex = defaultArg toColIndex (colCount - 1L)
        if fromColIndex < 0L || fromColIndex >= colCount then raise (new IndexOutOfRangeException())
        if toColIndex < 0L || toColIndex >= colCount then raise (new IndexOutOfRangeException())

        for i in fromColIndex..toColIndex do
            this.ColView(i).SetSlice(Some(fromRowIndex), Some(toRowIndex), value)

    member this.SetSlice(fromRowIndex : int64 option, toRowIndex : int64 option, colIndex : int64, value : float) =
        this.SetSlice(fromRowIndex, toRowIndex, Some colIndex, Some colIndex, value)

    member this.SetSlice(rowIndex : int64, fromColIndex : int64 option, toColIndex : int64 option, value : float) =
        this.SetSlice(Some rowIndex, Some rowIndex, fromColIndex, toColIndex, value)

    member this.SetSlice(fromRowIndex : int option, toRowIndex : int option, fromColIndex : int option, toColIndex : int option, value : float) =
        this.SetSlice(fromRowIndex |> Option.map int64, toRowIndex |> Option.map int64, fromColIndex |> Option.map int64, toColIndex |> Option.map int64, value)

    member this.SetSlice(fromRowIndex : int option, toRowIndex : int option, colIndex : int, value : float) =
        this.SetSlice(fromRowIndex, toRowIndex, Some colIndex, Some colIndex, value)

    member this.SetSlice(rowIndex : int, fromColIndex : int option, toColIndex : int option, value : float) =
        this.SetSlice(Some rowIndex, Some rowIndex, fromColIndex, toColIndex, value)

    member this.SetSlice(fromRowIndex : int64 option, toRowIndex : int64 option, fromColIndex : int64 option, toColIndex : int64 option, value : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [this;value]
        if value.LongLength = 1L then
            this.SetSlice(fromRowIndex, toRowIndex, fromColIndex, toColIndex, (value.[0L]:float))
        else
            let fromRowIndex = defaultArg fromRowIndex 0L
            let toRowIndex = defaultArg toRowIndex (rowCount - 1L)
            if fromRowIndex < 0L || fromRowIndex >= rowCount then raise (new IndexOutOfRangeException())
            if toRowIndex < 0L || toRowIndex >= rowCount then raise (new IndexOutOfRangeException())

            let fromColIndex = defaultArg fromColIndex 0L
            let toColIndex = defaultArg toColIndex (colCount - 1L)
            if fromColIndex < 0L || fromColIndex >= colCount then raise (new IndexOutOfRangeException())
            if toColIndex < 0L || toColIndex >= colCount then raise (new IndexOutOfRangeException())

            if (fromRowIndex > toRowIndex || fromColIndex > toColIndex) && value.LongLength = 0L then ()
            else
                let sliceRowCount = toRowIndex - fromRowIndex + 1L
                let sliceColCount = toColIndex - fromColIndex + 1L
                if sliceRowCount <> value.LongRowCount || sliceColCount <> value.LongColCount then
                    raise (new ArgumentException())
                for i in 0L..sliceColCount - 1L do
                    this.ColView(fromColIndex + i).SetSlice(Some(fromRowIndex), Some(toRowIndex), value.ColView(i))

    member this.SetSlice(fromRowIndex : int64 option, toRowIndex : int64 option, colIndex : int64, value : Vector) =
        let value = new Matrix(value)
        this.SetSlice(fromRowIndex, toRowIndex, Some colIndex, Some colIndex, value)

    member this.SetSlice(rowIndex : int64, fromColIndex : int64 option, toColIndex : int64 option, value : Vector) =
        let value = new Matrix(1L, value.LongLength, value)
        this.SetSlice(Some rowIndex, Some rowIndex, fromColIndex, toColIndex, value)

    member this.SetSlice(fromRowIndex : int option, toRowIndex : int option, fromColIndex : int option, toColIndex : int option, value : Matrix) =
       this.SetSlice(fromRowIndex |> Option.map int64, toRowIndex |> Option.map int64, fromColIndex |> Option.map int64, toColIndex |> Option.map int64, value)

    member this.SetSlice(fromRowIndex : int option, toRowIndex : int option, colIndex : int, value : Vector) =
        let value = new Matrix(value)
        this.SetSlice(fromRowIndex, toRowIndex, Some colIndex, Some colIndex, value)

    member this.SetSlice(rowIndex : int, fromColIndex : int option, toColIndex : int option, value : Vector) =
        let value = new Matrix(1L, value.LongLength, value)
        this.SetSlice(Some rowIndex, Some rowIndex, fromColIndex, toColIndex, value)

    member this.Item
        with get(i : int64) =
            colMajorDataVector.[i]
        and set (i : int64) value =
            colMajorDataVector.[i] <- value

    member this.Item
        with get(i : int) = this.[i |> int64]
        and set (i : int) value =
            this.[i |> int64] <- value

    member this.Item
        with get(indices : int64 seq) = 
            colMajorDataVector.[indices]
        and set (indices : int64 seq) (value : Vector) =
            colMajorDataVector.[indices] <- value

    member this.Item
        with get(indices : int seq) = 
            colMajorDataVector.[indices]
        and set (indices : int seq) (value : Vector) =
            colMajorDataVector.[indices] <- value

    member this.Item
        with get(boolVector : BoolVector) = 
            colMajorDataVector.[boolVector]

        and set (boolVector : BoolVector) (value : Vector) =
            colMajorDataVector.[boolVector] <- value

    member this.Item
        with get(rowIndex : int64, colIndex : int64) =
            colMajorDataVector.[colIndex * rowCount + rowIndex]
        and set (rowIndex : int64, colIndex : int64) value =
            colMajorDataVector.[colIndex * rowCount + rowIndex] <- value

    member this.Item
        with get(rowIndex : int, colIndex : int) =
            this.[rowIndex |> int64, colIndex |> int64]
        and set (rowIndex : int, colIndex : int) value =
            this.[rowIndex |> int64, colIndex |> int64] <- value

    member this.Item
        with get(rowIndices : int64 seq, colIndices : int64 seq) = 
            ArgumentChecks.throwIfContainsDisposed [this]
            let rowIndices = rowIndices |> Seq.toArray
            let rowCount = rowIndices.GetLongLength(0)
            let colIndices = colIndices |> Seq.toArray
            let colCount = colIndices.GetLongLength(0)
            let res = new Matrix(rowCount, colCount, 0.0)
            rowIndices |> Array.iteri (fun i rowIndex ->
                                           colIndices |> Array.iteri (fun j colIndex -> res.[i, j] <- this.[rowIndex, colIndex])
                                      )
            res

        and set (rowIndices : int64 seq, colIndices : int64 seq) (value : Matrix) =
            ArgumentChecks.throwIfContainsDisposed [this;value]
            let rowIndices = rowIndices |> Seq.toArray
            let colIndices = colIndices |> Seq.toArray
            if value.LongRowCount * value.LongColCount = 1L then
                let value = value.[0L]
                rowIndices |> Array.iteri (fun i rowIndex -> colIndices |> Array.iteri (fun j colIndex -> this.[rowIndex, colIndex] <- value))
            else
                rowIndices |> Array.iteri (fun i rowIndex -> colIndices |> Array.iteri (fun j colIndex -> this.[rowIndex, colIndex] <- value.[i, j]))

    member this.Item
        with get(rowIndex : int64, colIndices : int64 seq) = 
            this.[[|rowIndex|], colIndices].ColMajorDataVector

        and set (rowIndex : int64, colIndices : int64 seq) (value : Vector) =
            let value = new Matrix(1L, value.LongLength, value)
            this.[[|rowIndex|], colIndices] <- value

    member this.Item
        with get(rowIndices : int64 seq, colIndex : int64) = 
            this.[rowIndices, [|colIndex|]].ColMajorDataVector

        and set (rowIndices : int64 seq, colIndex : int64) (value : Vector) =
            let value = new Matrix(value)
            this.[rowIndices, [|colIndex|]] <- value

    member this.Item
        with get(rowIndices : int seq, colIndices : int seq) = 
            let rowIndices = rowIndices |> Seq.map int64
            let colIndices = colIndices |> Seq.map int64
            this.[rowIndices, colIndices]

        and set (rowIndices : int seq, colIndices : int seq) (value : Matrix) =
            let rowIndices = rowIndices |> Seq.map int64
            let colIndices = colIndices |> Seq.map int64
            this.[rowIndices, colIndices] <- value

    member this.Item
        with get(rowIndex : int, colIndices : int seq) = 
            this.[[|rowIndex|], colIndices].ColMajorDataVector

        and set (rowIndex : int, colIndices : int seq) (value : Vector) =
            let value = new Matrix(1L, value.LongLength, value)
            this.[[|rowIndex|], colIndices] <- value

    member this.Item
        with get(rowIndices : int seq, colIndex : int) = 
            this.[rowIndices, [|colIndex|]].ColMajorDataVector

        and set (rowIndices : int seq, colIndex : int) (value : Vector) =
            let value = new Matrix(value)
            this.[rowIndices, [|colIndex|]] <- value

    member this.Item
        with get(boolMatrix : BoolMatrix) = 
            colMajorDataVector.[boolMatrix.ColMajorDataVector]

        and set (boolMatrix : BoolMatrix) (value : Vector) =
            colMajorDataVector.[boolMatrix.ColMajorDataVector] <- value

    member this.ToArray2D() =
        ArgumentChecks.throwIfContainsDisposed [this]
        Array2D.init this.RowCount this.ColCount (fun i j -> this.[i, j])

    member this.AsExpr
        with get() = 
            ArgumentChecks.throwIfContainsDisposed [this]
            if this.LongLength = 1L then MatrixExpr.Scalar(this.[0L, 0L])
            else MatrixExpr.Var(this)

    member this.Transpose() =
        ArgumentChecks.throwIfContainsDisposed [this]
        let r = rowCount
        let c = colCount
        MklFunctions.D_Transpose_In_Place(rowCount, colCount, colMajorDataVector.NativeArray)
        rowCount <- c
        colCount <- r


    static member Identity(rows : int64, cols : int64) =
        let res = new Matrix(rows, cols, 0.0)
        MklFunctions.D_Identity(rows, cols, res.ColMajorDataVector.NativeArray)
        res

    static member Identity(rows : int, cols : int) =
        Matrix.Identity(rows |> int64, cols |> int64)

    static member Transpose(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let res : Matrix = Matrix.Copy(matrix)
        res.Transpose()
        res

    static member UpperTri(matrix : Matrix, offset : int64) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let res = new Matrix(matrix.LongRowCount, matrix.LongColCount, 0.0)
        MklFunctions.D_Get_Upper_Tri(offset, matrix.LongRowCount, matrix.LongColCount, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member UpperTri(matrix : Matrix, offset : int) =
        Matrix.UpperTri(matrix, offset |> int64)

    static member LowerTri(matrix : Matrix, offset : int64) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let res = new Matrix(matrix.LongRowCount, matrix.LongColCount, 0.0)
        MklFunctions.D_Get_Lower_Tri(offset, matrix.LongRowCount, matrix.LongColCount, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member LowerTri(matrix : Matrix, offset : int) =
        Matrix.LowerTri(matrix, offset |> int64)

    static member (==) (matrix1: Matrix, matrix2: Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        matrix1 = matrix2

    static member (!=) (matrix1: Matrix, matrix2: Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        matrix1 <> matrix2

    static member (==) (matrix: Matrix, a : float) =
        matrix.ColMajorDataVector == a

    static member (!=) (matrix: Matrix, a : float) =
        not (matrix == a)

    static member (==) (a : float, matrix: Matrix) =
        matrix.ColMajorDataVector == a

    static member (!=) (a : float, matrix: Matrix) =
        not (matrix == a)


    static member (.<) (matrix1: Matrix, matrix2: Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        let rowCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongRowCount matrix2.LongRowCount
        let colCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongColCount matrix2.LongColCount
        new BoolMatrix(rowCount, colCount, matrix1.ColMajorDataVector .< matrix2.ColMajorDataVector)

    static member (.<=) (matrix1: Matrix, matrix2: Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        let rowCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongRowCount matrix2.LongRowCount
        let colCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongColCount matrix2.LongColCount
        new BoolMatrix(rowCount, colCount, matrix1.ColMajorDataVector .<= matrix2.ColMajorDataVector)

    static member (.>) (matrix1: Matrix, matrix2: Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        let rowCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongRowCount matrix2.LongRowCount
        let colCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongColCount matrix2.LongColCount
        new BoolMatrix(rowCount, colCount, matrix1.ColMajorDataVector .> matrix2.ColMajorDataVector)

    static member (.>=) (matrix1: Matrix, matrix2: Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        let rowCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongRowCount matrix2.LongRowCount
        let colCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongColCount matrix2.LongColCount
        new BoolMatrix(rowCount, colCount, matrix1.ColMajorDataVector .>= matrix2.ColMajorDataVector)

    static member (.=) (matrix1: Matrix, matrix2: Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        let rowCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongRowCount matrix2.LongRowCount
        let colCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongColCount matrix2.LongColCount
        new BoolMatrix(rowCount, colCount, matrix1.ColMajorDataVector .= matrix2.ColMajorDataVector)

    static member (.<>) (matrix1: Matrix, matrix2: Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        let rowCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongRowCount matrix2.LongRowCount
        let colCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongColCount matrix2.LongColCount
        new BoolMatrix(rowCount, colCount, matrix1.ColMajorDataVector .<> matrix2.ColMajorDataVector)



    static member (.<) (matrix: Matrix, a : float) =
        new BoolMatrix(matrix.LongRowCount, matrix.LongColCount, matrix.ColMajorDataVector .< a)

    static member (.<=) (matrix: Matrix, a : float) =
        new BoolMatrix(matrix.LongRowCount, matrix.LongColCount, matrix.ColMajorDataVector .<= a)

    static member (.>) (matrix: Matrix, a : float) =
        new BoolMatrix(matrix.LongRowCount, matrix.LongColCount, matrix.ColMajorDataVector .> a)

    static member (.>=) (matrix: Matrix, a : float) =
        new BoolMatrix(matrix.LongRowCount, matrix.LongColCount, matrix.ColMajorDataVector .>= a)

    static member (.=) (matrix: Matrix, a : float) =
        new BoolMatrix(matrix.LongRowCount, matrix.LongColCount, matrix.ColMajorDataVector .= a)

    static member (.<>) (matrix: Matrix, a : float) =
        new BoolMatrix(matrix.LongRowCount, matrix.LongColCount, matrix.ColMajorDataVector .<> a)



    static member (.<) (a : float, matrix: Matrix) =
        matrix .> a

    static member (.<=) (a : float, matrix: Matrix) =
        matrix .>= a

    static member (.>) (a : float, matrix: Matrix) =
        matrix .< a

    static member (.>=) (a : float, matrix: Matrix) =
        matrix .<= a

    static member (.=) (a : float, matrix: Matrix) =
        matrix .= a

    static member (.<>) (a : float, matrix: Matrix) =
        matrix .<> a


    static member Max(matrix1: Matrix, matrix2: Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        let rowCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongRowCount matrix2.LongRowCount
        let colCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongColCount matrix2.LongColCount
        new Matrix(rowCount, colCount, Vector.Max(matrix1.ColMajorDataVector, matrix2.ColMajorDataVector))

    static member Min(matrix1: Matrix, matrix2: Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        let rowCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongRowCount matrix2.LongRowCount
        let colCount = if matrix1.LongLength = 0L || matrix2.LongLength = 0L then 0L else max matrix1.LongColCount matrix2.LongColCount
        new Matrix(rowCount, colCount, Vector.Min(matrix1.ColMajorDataVector, matrix2.ColMajorDataVector))

    static member Max(matrix : Matrix, a : float) =
        let a = new Matrix(a)
        Matrix.Max(matrix, a)

    static member Min(matrix : Matrix, a : float) =
        let a = new Matrix(a)
        Matrix.Min(matrix, a)

    static member Max(a : float, matrix : Matrix) = 
        Matrix.Max(matrix, a)

    static member Min(a : float, matrix : Matrix) = 
        Matrix.Min(matrix, a)




    static member (*) (matrix1 : Matrix, matrix2 : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        let n = matrix2.LongColCount
        let m = matrix1.LongRowCount
        let k = matrix1.LongColCount
        if k <> matrix2.LongRowCount then raise (new ArgumentException("Matrix size mismatch"))
        if matrix1.LongLength = 0L || matrix2.LongLength = 0L then Matrix.Empty
        else
            let res = new Matrix(m, n, 0.0)
            MklFunctions.D_Multiply_Matrices(matrix1.ColMajorDataVector.NativeArray, matrix2.ColMajorDataVector.NativeArray, 
                                             res.ColMajorDataVector.NativeArray, n, m, k, false)
            res  

    static member (*) (matrix : Matrix, vector : Vector) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        ArgumentChecks.throwIfContainsDisposed [vector]
        let m = new Matrix(vector)
        matrix * m

    static member (^*) (matrix1 : Matrix, matrix2 : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        let n = matrix2.LongColCount
        let k = matrix1.LongRowCount
        let m = matrix1.LongColCount
        if k <> matrix2.LongRowCount then raise (new ArgumentException("Matrix size mismatch"))
        if matrix1.LongLength = 0L || matrix2.LongLength = 0L then Matrix.Empty
        else
            let res = new Matrix(m, n, 0.0)
            MklFunctions.D_Multiply_Matrices(matrix1.ColMajorDataVector.NativeArray, matrix2.ColMajorDataVector.NativeArray, 
                                             res.ColMajorDataVector.NativeArray, n, m, k, true)
            res  


    static member (.*) (a: float, matrix : Matrix) =
        new Matrix(matrix.LongRowCount, matrix.LongColCount, a .* matrix.ColMajorDataVector)

    static member (.*) (matrix : Matrix, a :  float) =
        a .* matrix

    static member (.*) (matrix1 : Matrix, matrix2 : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        if matrix1.LongLength = 0L || matrix2.LongLength = 0L then Matrix.Empty
        else
            if matrix1.ColMajorDataVector.LongLength = 1L then
                matrix1.[0] .* matrix2
            elif matrix2.ColMajorDataVector.LongLength = 1L then
                matrix2.[0] .* matrix1
            else
               new Matrix(matrix1.LongRowCount, matrix1.LongColCount, matrix1.ColMajorDataVector .* matrix2.ColMajorDataVector)

    static member (+) (a: float, matrix : Matrix) =
        new Matrix(matrix.LongRowCount, matrix.LongColCount, a + matrix.ColMajorDataVector)

    static member (+) (matrix : Matrix, a :  float) =
        a + matrix

    static member (+) (matrix1 : Matrix, matrix2 : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        if matrix1.LongLength = 0L || matrix2.LongLength = 0L then Matrix.Empty
        else
            if matrix1.ColMajorDataVector.LongLength = 1L then
                matrix1.[0] + matrix2
            elif matrix2.ColMajorDataVector.LongLength = 1L then
                matrix2.[0] + matrix1
            else
               new Matrix(matrix1.LongRowCount, matrix1.LongColCount, matrix1.ColMajorDataVector + matrix2.ColMajorDataVector)

    static member (./) (a: float, matrix : Matrix) =
        new Matrix(matrix.LongRowCount, matrix.LongColCount, a ./ matrix.ColMajorDataVector)

    static member (./) (matrix : Matrix, a :  float) =
        new Matrix(matrix.LongRowCount, matrix.LongColCount, matrix.ColMajorDataVector ./ a)

    static member (./) (matrix1 : Matrix, matrix2 : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        if matrix1.LongLength = 0L || matrix2.LongLength = 0L then Matrix.Empty
        else
            if matrix1.ColMajorDataVector.LongLength = 1L then
                matrix1.[0] ./ matrix2
            elif matrix2.ColMajorDataVector.LongLength = 1L then
                matrix1 ./ matrix2.[0]
            else
               new Matrix(matrix1.LongRowCount, matrix1.LongColCount, matrix1.ColMajorDataVector ./ matrix2.ColMajorDataVector)

    static member (-) (a: float, matrix : Matrix) =
        new Matrix(matrix.LongRowCount, matrix.LongColCount, a - matrix.ColMajorDataVector)

    static member (-) (matrix : Matrix, a :  float) =
        new Matrix(matrix.LongRowCount, matrix.LongColCount, matrix.ColMajorDataVector - a)

    static member (-) (matrix1 : Matrix, matrix2 : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        if matrix1.LongLength = 0L || matrix2.LongLength = 0L then Matrix.Empty
        else
            if matrix1.ColMajorDataVector.LongLength = 1L then
                matrix1.[0] - matrix2
            elif matrix2.ColMajorDataVector.LongLength = 1L then
                matrix1 - matrix2.[0]
            else
               new Matrix(matrix1.LongRowCount, matrix1.LongColCount, matrix1.ColMajorDataVector - matrix2.ColMajorDataVector)

    static member (~-) (matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        new Matrix(matrix.LongRowCount, matrix.LongColCount, -matrix.ColMajorDataVector)
        
    static member (.^) (a: float, matrix : Matrix) =
        new Matrix(matrix.LongRowCount, matrix.LongColCount, a .^ matrix.ColMajorDataVector) 

    static member (.^) (matrix : Matrix, a :  float) =
        new Matrix(matrix.LongRowCount, matrix.LongColCount, matrix.ColMajorDataVector .^ a) 

    static member (.^) (matrix1 : Matrix, matrix2 : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix1;matrix2]
        ArgumentChecks.throwIfSizeNotOKForElementwise matrix1.LongSize matrix2.LongSize
        if matrix1.LongLength = 0L || matrix2.LongLength = 0L then Matrix.Empty
        else
            if matrix1.ColMajorDataVector.LongLength = 1L then
                matrix1.[0] .^ matrix2
            elif matrix2.ColMajorDataVector.LongLength = 1L then
                matrix1 .^ matrix2.[0]
            else
               new Matrix(matrix1.LongRowCount, matrix1.LongColCount, matrix1.ColMajorDataVector .^ matrix2.ColMajorDataVector)

    static member (.^) (matrix : Matrix, n :  int) =
        matrix .^ float(n)

    static member Abs(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Abs_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Sqrt(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Sqrt_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Sin(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Sin_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Cos(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Cos_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Tan(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Tan_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Asin(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_ASin_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Acos(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_ACos_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Atan(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_ATan_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Sinh(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Sinh_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Cosh(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Cosh_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Tanh(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Tanh_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member ASinh(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_ASinh_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member ACosh(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_ACosh_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member ATanh(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_ATanh_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Exp(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Exp_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Expm1(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Expm1_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Log(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Ln_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Log10(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Log10_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Log1p(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Log1p_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Erf(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Erf_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Erfc(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Erfc_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Erfinv(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Erfinv_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Erfcinv(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Erfcinv_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Normcdf(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_CdfNorm_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Norminv(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_CdfNormInv_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Round(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Round_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Ceiling(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Ceil_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Floor(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Floor_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res

    static member Truncate(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let res = new Matrix(m, n, 0.0)
        MklFunctions.D_Trunc_Array(m * n, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res



    static member Sum(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        fun (matrixAxis : MatrixAxis) ->
            match matrixAxis with
                | RowAxis ->
                    let varCount = matrix.LongRowCount
                    let obsCount = matrix.LongColCount
                    if obsCount = 0L then Vector.Empty
                    elif obsCount = 1L then Matrix.Copy(matrix).ColMajorDataVector
                    else
                        let res = new Vector(varCount, 0.0)
                        MklFunctions.D_Sum_Matrix(true, varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.NativeArray)
                        res
                | ColumnAxis ->
                    let varCount = matrix.LongColCount
                    let obsCount = matrix.LongRowCount 
                    if obsCount = 0L then Vector.Empty
                    elif obsCount = 1L then Matrix.Copy(matrix).ColMajorDataVector
                    else
                        let res = new Vector(varCount, 0.0)
                        MklFunctions.D_Sum_Matrix(false, varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.NativeArray)
                        res

    static member Prod(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        fun (matrixAxis : MatrixAxis) ->
            match matrixAxis with
                | RowAxis ->
                    let varCount = matrix.LongRowCount
                    let obsCount = matrix.LongColCount
                    if obsCount = 0L then Vector.Empty
                    elif obsCount = 1L then Matrix.Copy(matrix).ColMajorDataVector
                    else
                        let res = new Vector(varCount, 0.0)
                        MklFunctions.D_Prod_Matrix(true, varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.NativeArray)
                        res
                | ColumnAxis ->
                    let varCount = matrix.LongColCount
                    let obsCount = matrix.LongRowCount 
                    if obsCount = 0L then Vector.Empty
                    elif obsCount = 1L then Matrix.Copy(matrix).ColMajorDataVector
                    else
                        let res = new Vector(varCount, 0.0)
                        MklFunctions.D_Prod_Matrix(false, varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.NativeArray)
                        res

    static member CumSum(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        fun (matrixAxis : MatrixAxis) ->
            match matrixAxis with
                | RowAxis ->
                    let varCount = matrix.LongRowCount
                    let obsCount = matrix.LongColCount
                    if obsCount = 0L then Matrix.Empty
                    elif obsCount = 1L then Matrix.Copy(matrix)
                    else
                        let res = new Matrix(matrix.LongRowCount, matrix.LongColCount, 0.0)
                        MklFunctions.D_CumSum_Matrix(true, varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
                        res
                | ColumnAxis ->
                    let varCount = matrix.LongColCount
                    let obsCount = matrix.LongRowCount 
                    if obsCount = 0L then Matrix.Empty
                    elif obsCount = 1L then Matrix.Copy(matrix)
                    else
                        let res = new Matrix(matrix.LongRowCount, matrix.LongColCount, 0.0)
                        MklFunctions.D_CumSum_Matrix(false, varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
                        res

    static member CumProd(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        fun (matrixAxis : MatrixAxis) ->
            match matrixAxis with
                | RowAxis ->
                    let varCount = matrix.LongRowCount
                    let obsCount = matrix.LongColCount
                    if obsCount = 0L then Matrix.Empty
                    elif obsCount = 1L then Matrix.Copy(matrix)
                    else
                        let res = new Matrix(matrix.LongRowCount, matrix.LongColCount, 0.0)
                        MklFunctions.D_CumProd_Matrix(true, varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
                        res
                | ColumnAxis ->
                    let varCount = matrix.LongColCount
                    let obsCount = matrix.LongRowCount 
                    if obsCount = 0L then Matrix.Empty
                    elif obsCount = 1L then Matrix.Copy(matrix)
                    else
                        let res = new Matrix(matrix.LongRowCount, matrix.LongColCount, 0.0)
                        MklFunctions.D_CumProd_Matrix(false, varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
                        res

    static member Min(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        fun (matrixAxis : MatrixAxis) ->
            match matrixAxis with
                | RowAxis ->
                    let varCount = matrix.LongRowCount
                    let obsCount = matrix.LongColCount
                    if obsCount = 0L then Vector.Empty
                    elif obsCount = 1L then Matrix.Copy(matrix).ColMajorDataVector
                    else
                        let res = new Vector(varCount, 0.0)
                        MklFunctions.D_Min_Matrix(true, varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.NativeArray)
                        res
                | ColumnAxis ->
                    let varCount = matrix.LongColCount
                    let obsCount = matrix.LongRowCount 
                    if obsCount = 0L then Vector.Empty
                    elif obsCount = 1L then Matrix.Copy(matrix).ColMajorDataVector
                    else
                        let res = new Vector(varCount, 0.0)
                        MklFunctions.D_Min_Matrix(false, varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.NativeArray)
                        res

    static member Max(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        fun (matrixAxis : MatrixAxis) ->
            match matrixAxis with
                | RowAxis ->
                    let varCount = matrix.LongRowCount
                    let obsCount = matrix.LongColCount
                    if obsCount = 0L then Vector.Empty
                    elif obsCount = 1L then Matrix.Copy(matrix).ColMajorDataVector
                    else
                        let res = new Vector(varCount, 0.0)
                        MklFunctions.D_Max_Matrix(true, varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.NativeArray)
                        res
                | ColumnAxis ->
                    let varCount = matrix.LongColCount
                    let obsCount = matrix.LongRowCount
                    if obsCount = 0L then Vector.Empty
                    elif obsCount = 1L then Matrix.Copy(matrix).ColMajorDataVector
                    else 
                        let res = new Vector(varCount, 0.0)
                        MklFunctions.D_Max_Matrix(false, varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.NativeArray)
                        res

    static member Mean(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        fun (matrixAxis : MatrixAxis) ->
            match matrixAxis with
                | RowAxis ->
                    let varCount = matrix.LongRowCount
                    let obsCount = matrix.LongColCount
                    if obsCount = 0L then Vector.Empty
                    elif obsCount = 1L then Matrix.Copy(matrix).ColMajorDataVector
                    else
                        let res = new Vector(varCount, 0.0)
                        MklFunctions.D_Mean_Matrix(true, varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.NativeArray)
                        res
                | ColumnAxis ->
                    let varCount = matrix.LongColCount
                    let obsCount = matrix.LongRowCount 
                    if obsCount = 0L then Vector.Empty
                    elif obsCount = 1L then Matrix.Copy(matrix).ColMajorDataVector
                    else
                        let res = new Vector(varCount, 0.0)
                        MklFunctions.D_Mean_Matrix(false, varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.NativeArray)
                        res

    static member Variance(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        fun (matrixAxis : MatrixAxis) ->
            match matrixAxis with
                | RowAxis ->
                    let varCount = matrix.LongRowCount
                    let obsCount = matrix.LongColCount
                    if obsCount = 0L then Vector.Empty
                    elif obsCount = 1L then new Vector(varCount, 0.0)
                    else
                        let res = new Vector(varCount, 0.0)
                        MklFunctions.D_Variance_Matrix(true, varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.NativeArray)
                        res
                | ColumnAxis ->
                    let varCount = matrix.LongColCount
                    let obsCount = matrix.LongRowCount 
                    if obsCount = 0L then Vector.Empty
                    elif obsCount = 1L then new Vector(varCount, 0.0)
                    else
                        let res = new Vector(varCount, 0.0)
                        MklFunctions.D_Variance_Matrix(false, varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.NativeArray)
                        res

    static member Skewness(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        fun (matrixAxis : MatrixAxis) ->
            match matrixAxis with
                | RowAxis ->
                    let varCount = matrix.LongRowCount
                    let obsCount = matrix.LongColCount
                    if obsCount = 0L then Vector.Empty
                    elif obsCount = 1L then new Vector(varCount, Double.NaN)
                    else
                        let res = new Vector(varCount, 0.0)
                        MklFunctions.D_Skewness_Matrix(true, varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.NativeArray)
                        res
                | ColumnAxis ->
                    let varCount = matrix.LongColCount
                    let obsCount = matrix.LongRowCount 
                    if obsCount = 0L then Vector.Empty
                    elif obsCount = 1L then new Vector(varCount, Double.NaN)
                    else
                        let res = new Vector(varCount, 0.0)
                        MklFunctions.D_Skewness_Matrix(false, varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.NativeArray)
                        res

    static member Kurtosis(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        fun (matrixAxis : MatrixAxis) ->
            match matrixAxis with
                | RowAxis ->
                    let varCount = matrix.LongRowCount
                    let obsCount = matrix.LongColCount
                    if obsCount = 0L then Vector.Empty
                    elif obsCount = 1L then new Vector(varCount, Double.NaN)
                    else
                        let res = new Vector(varCount, 0.0)
                        MklFunctions.D_Kurtosis_Matrix(true, varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.NativeArray)
                        res
                | ColumnAxis ->
                    let varCount = matrix.LongColCount
                    let obsCount = matrix.LongRowCount 
                    if obsCount = 0L then Vector.Empty
                    elif obsCount = 1L then new Vector(varCount, Double.NaN)
                    else
                        let res = new Vector(varCount, 0.0)
                        MklFunctions.D_Kurtosis_Matrix(false, varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.NativeArray)
                        res

    static member Quantile(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        fun (quantileOrders : Vector) (matrixAxis : MatrixAxis) ->
            ArgumentChecks.throwIfContainsDisposed [quantileOrders]
            match matrixAxis with
                | RowAxis ->
                    let varCount = matrix.LongRowCount
                    let obsCount = matrix.LongColCount
                    if quantileOrders.LongLength = 0L then raise (new ArgumentException("Quantile orders vector must not be empty"))
                    if obsCount = 0L then Matrix.Empty
                    else
                        let res = new Matrix(varCount, quantileOrders.LongLength, 0.0)
                        MklFunctions.D_Quantiles_Matrix(true, varCount, obsCount, quantileOrders.LongLength, matrix.ColMajorDataVector.NativeArray, quantileOrders.NativeArray, res.ColMajorDataVector.NativeArray)
                        res
                | ColumnAxis ->
                    let varCount = matrix.LongColCount
                    let obsCount = matrix.LongRowCount 
                    if quantileOrders.LongLength = 0L then raise (new ArgumentException("Quantile orders vector must not be empty"))
                    if obsCount = 0L then Matrix.Empty
                    else
                        let res = new Matrix(quantileOrders.LongLength, varCount, 0.0)
                        MklFunctions.D_Quantiles_Matrix(false, varCount, obsCount, quantileOrders.LongLength, matrix.ColMajorDataVector.NativeArray, quantileOrders.NativeArray, res.ColMajorDataVector.NativeArray)
                        res


    static member Corr(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        if matrix.LongLength = 0L then Matrix.Empty
        else
            let varCount = matrix.LongColCount
            let obsCount = matrix.LongRowCount 
            if obsCount = 1L then new Matrix(varCount, varCount, Double.NaN)
            else
                let res = new Matrix(varCount, varCount, 0.0)
                MklFunctions.D_Corr_Matrix(varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
                res

    static member Cov(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        if matrix.LongLength = 0L then Matrix.Empty
        else
            let varCount = matrix.LongColCount
            let obsCount = matrix.LongRowCount 
            if obsCount = 1L then new Matrix(varCount, varCount, 0.0)
            else
                let res = new Matrix(varCount, varCount, 0.0)
                MklFunctions.D_Cov_Matrix(varCount, obsCount, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
                res

    static member Copy(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let rows = matrix.LongRowCount
        let cols = matrix.LongColCount
        let res = new Matrix(rows, cols, 0.0)
        MklFunctions.D_Copy_Array(matrix.ColMajorDataVector.LongLength, matrix.ColMajorDataVector.NativeArray, res.ColMajorDataVector.NativeArray)
        res


    static member Chol(matrix: Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        if matrix == Matrix.Empty then Matrix.Empty
        else
            if matrix.LongRowCount <> matrix.LongColCount then raise (new ArgumentException("Matrix is not square"))
            let res = Matrix.Copy(matrix)
            MklFunctions.D_Cholesky_Factor(res.LongRowCount, res.ColMajorDataVector.NativeArray)
            res

    static member CholInv(matrix: Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        if matrix == Matrix.Empty then Matrix.Empty
        else
            if matrix.LongRowCount <> matrix.LongColCount then raise (new ArgumentException("Matrix is not square"))
            let res = Matrix.Copy(matrix)
            MklFunctions.D_Cholesky_Inverse(res.LongRowCount, res.ColMajorDataVector.NativeArray)
            res

    static member CholSolve(a : Matrix, b : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [a;b]
        if a.LongRowCount <> b.LongRowCount then raise (new ArgumentException("Matrix dimensions must agree"))
        if a.LongRowCount <> a.LongColCount then raise (new ArgumentException("Matrix is not square"))
        if a == Matrix.Empty then new Matrix(a.LongRowCount, b.LongColCount, 0.0)
        else
            use a = Matrix.Copy(a)
            let b = Matrix.Copy(b)
            MklFunctions.D_Cholesky_Solve(a.LongRowCount, b.LongColCount, a.ColMajorDataVector.NativeArray, b.ColMajorDataVector.NativeArray)
            b

    static member Lu(matrix: Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        let m = matrix.LongRowCount
        let n = matrix.LongColCount
        let L =
            if m > n then
                Matrix.Copy(matrix)
            else
                new Matrix(m, m, 0.0)
        let U = 
            if m > n then
                new Matrix(n, n, 0.0)
            else
                Matrix.Copy(matrix)
        let pivot = Array.zeroCreate<int> (int(m))
        MklFunctions.D_Lu_Factor(m, n, L.ColMajorDataVector.NativeArray, U.ColMajorDataVector.NativeArray, pivot)
        (L, U, pivot)

    static member LuInv(matrix: Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        if matrix == Matrix.Empty then Matrix.Empty
        else
            if matrix.LongRowCount <> matrix.LongColCount then raise (new ArgumentException("Matrix is not square"))
            let res = Matrix.Copy(matrix)
            MklFunctions.D_Lu_Inverse(res.LongRowCount, res.ColMajorDataVector.NativeArray)
            res

    static member LuSolve(a : Matrix, b : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [a;b]
        if a.LongRowCount <> b.LongRowCount then raise (new ArgumentException("Matrix dimensions must agree"))
        if a.LongRowCount <> a.LongColCount then raise (new ArgumentException("Matrix is not square"))
        if a == Matrix.Empty then new Matrix(a.LongRowCount, b.LongColCount, 0.0)
        else
            use a = Matrix.Copy(a)
            let b = Matrix.Copy(b)
            MklFunctions.D_Lu_Solve(a.LongRowCount, b.LongColCount, a.ColMajorDataVector.NativeArray, b.ColMajorDataVector.NativeArray)
            b

    static member Qr(matrix: Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        if matrix == Matrix.Empty then Matrix.Empty, Matrix.Empty
        else
            let m = matrix.LongRowCount
            let n = matrix.LongColCount
            let Q =
                if m > n then
                    Matrix.Copy(matrix)
                else
                    new Matrix(m, m, 0.0)
            let R = 
                if m > n then
                    new Matrix(n, n, 0.0)
                else
                    Matrix.Copy(matrix)
            MklFunctions.D_Qr_Factor(m, n, matrix.ColMajorDataVector.NativeArray, Q.ColMajorDataVector.NativeArray, R.ColMajorDataVector.NativeArray)
            (Q, R)

    static member QrSolveFull(a : Matrix, b : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [a;b]
        if a.LongRowCount <> b.LongRowCount then raise (new ArgumentException("Matrix dimensions must agree"))
        if a == Matrix.Empty then new Matrix(a.LongRowCount, b.LongColCount, 0.0)
        else
            let m = a.LongRowCount
            let n = a.LongColCount
            let nrhs = b.LongColCount
            use a = Matrix.Copy(a)
            let x = new Matrix(n, nrhs, 0.0)
            MklFunctions.D_Qr_Solve_Full(m, n, nrhs, a.ColMajorDataVector.NativeArray, b.ColMajorDataVector.NativeArray, x.ColMajorDataVector.NativeArray)
            x

    static member QrSolve(a : Matrix, b : Matrix, tol : float) =
        ArgumentChecks.throwIfContainsDisposed [a;b]
        if a.LongRowCount <> b.LongRowCount then raise (new ArgumentException("Matrix dimensions must agree"))
        if a == Matrix.Empty then new Matrix(a.LongRowCount, b.LongColCount, 0.0), 0
        else
            let m = a.LongRowCount
            let n = a.LongColCount
            let nrhs = b.LongColCount
            use a = Matrix.Copy(a)
            let x = new Matrix(n, nrhs, 0.0)
            let mutable rank = 0
            MklFunctions.D_Qr_Solve(m, n, nrhs, a.ColMajorDataVector.NativeArray, b.ColMajorDataVector.NativeArray, x.ColMajorDataVector.NativeArray, &&rank, tol)
            (x, rank)

    static member SvdSolve(a : Matrix, b : Matrix, tol : float) =
        ArgumentChecks.throwIfContainsDisposed [a;b]
        if a.LongRowCount <> b.LongRowCount then raise (new ArgumentException("Matrix dimensions must agree"))
        if a == Matrix.Empty then new Matrix(a.LongRowCount, b.LongColCount, 0.0), 0
        else
            let m = a.LongRowCount
            let n = a.LongColCount
            let nrhs = b.LongColCount
            use a = Matrix.Copy(a)
            let x = new Matrix(n, nrhs, 0.0)
            let mutable rank = 0
            MklFunctions.D_Svd_Solve(m, n, nrhs, a.ColMajorDataVector.NativeArray, b.ColMajorDataVector.NativeArray, x.ColMajorDataVector.NativeArray, &&rank, tol)
            (x, rank)

    static member SvdValues(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        if matrix == Matrix.Empty then Vector.Empty
        else
            let m = matrix.LongRowCount
            let n = matrix.LongColCount
            let res = new Vector(min m n, 0.0)
            use matrix = Matrix.Copy(matrix)
            MklFunctions.D_Svd_Values(m, n, matrix.ColMajorDataVector.NativeArray, res.NativeArray)
            res

    static member Svd(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        if matrix == Matrix.Empty then Matrix.Empty, Vector.Empty, Matrix.Empty
        else
            let m = matrix.LongRowCount
            let n = matrix.LongColCount
            let U = new Matrix(m, min m n, 0.0)
            let S = new Vector(min m n, 0.0)
            let Vt = new Matrix(min m n, n, 0.0)
            use matrix = Matrix.Copy(matrix)
            MklFunctions.D_Svd_Factor(m, n, matrix.ColMajorDataVector.NativeArray, U.ColMajorDataVector.NativeArray, S.NativeArray, Vt.ColMajorDataVector.NativeArray)
            (U, S, Vt)

    static member Eig(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        if matrix.LongRowCount <> matrix.LongColCount then raise (new ArgumentException("Matrix must be square"))
        if matrix == Matrix.Empty then Matrix.Empty, Vector.Empty
        else
            let n = matrix.LongRowCount
            let Z = Matrix.Copy(matrix)
            let D = new Vector(n, 0.0)
            MklFunctions.D_Eigen_Factor(n, Z.ColMajorDataVector.NativeArray, D.NativeArray)
            (Z, D)

    static member EigValues(matrix : Matrix) =
        ArgumentChecks.throwIfContainsDisposed [matrix]
        if matrix.LongRowCount <> matrix.LongColCount then raise (new ArgumentException("Matrix must be square"))
        if matrix == Matrix.Empty then Vector.Empty
        else
            let n = matrix.LongRowCount
            let D = new Vector(n, 0.0)
            MklFunctions.D_Eigen_Values(n, matrix.ColMajorDataVector.NativeArray, D.NativeArray)
            D

    override this.ToString() = 
        ArgumentChecks.throwIfContainsDisposed [this]
        (this:>IFormattable).ToString(GenericFormatting.GenericFormat.Instance.GetFormat<float>() 0.0, null)

    override this.Equals(yobj) =
        match yobj with
        | :? Matrix as y ->
            ArgumentChecks.throwIfContainsDisposed [this;y]
            if this.LongRowCount * this.LongColCount = 0L && y.LongRowCount * y.LongColCount = 0L then true
            elif this.LongRowCount <> y.LongRowCount || this.LongColCount <> y.LongColCount then false
            else 
                this.ColMajorDataVector = y.ColMajorDataVector
        | _ -> false
 
    override this.GetHashCode() = 
        ArgumentChecks.throwIfContainsDisposed [this]
        hash (this.LongRowCount, this.LongColCount, this.ColMajorDataVector.NativeArray)

    interface IFormattable with
        member this.ToString(format, provider) = 
            ArgumentChecks.throwIfContainsDisposed [this]
            let maxRows, maxCols = DisplayControl.MaxDisplaySize
            let showRows = max 0L (min (maxRows |> int64) rowCount) |> int
            let showCols = max 0L (min (maxCols |> int64) colCount) |> int
            let moreRows = rowCount > (showRows |> int64)
            let moreCols = colCount > (showCols |> int64)
            let arr = Array2D.init showRows showCols (fun row col -> this.[row, col])
            let formattedArray = DisplayControl.FormatArray2D(arr, format, moreRows, moreCols)
            sprintf "Size = [%d,%d]\r\n%s" rowCount colCount formattedArray

    interface IDisposable with
        member this.Dispose() = this.ColMajorDataVector.DoDispose(true)

    override this.Finalize() = try this.ColMajorDataVector.DoDispose(false) with _ -> ()

//************************************************MatrixExpr*******************************************************************************

and MatrixExpr = 
    | Scalar of float
    | Var of Matrix
    | UnaryFunction of MatrixExpr * (Vector -> Vector -> unit)
    | BinaryFunction of MatrixExpr * MatrixExpr * (Vector -> Vector -> Vector -> unit)
    | IfFunction of BoolMatrixExpr * MatrixExpr * MatrixExpr

    member this.AsVectorExpr =
        match this with
            | Scalar(v) -> VectorExpr.Scalar(v)
            | Var(v) -> v.ColMajorDataVector.AsExpr
            | UnaryFunction(expr, f) -> VectorExpr.UnaryFunction(expr.AsVectorExpr, f)
            | BinaryFunction(expr1, expr2, f) -> VectorExpr.BinaryFunction(expr1.AsVectorExpr, expr2.AsVectorExpr, f) 
            | IfFunction(ifExpr, trueExpr, falseExpr) -> VectorExpr.IfFunction(ifExpr.AsBoolVectorExpr, trueExpr.AsVectorExpr, falseExpr.AsVectorExpr) 

    member this.Size = 
        match this with
            | Scalar(_) -> Some(1L, 1L)
            | Var(v) -> Some(v.LongRowCount, v.LongColCount)
            | UnaryFunction(v, _) -> v.Size
            | BinaryFunction(v1, v2, _) -> ArgumentChecks.getElementwiseSize v1.Size v2.Size
            | IfFunction(v1, v2, v3) -> ArgumentChecks.getElementwiseSizeIf v1.Size v2.Size v3.Size

    static member EvalIn(matrixExpr : MatrixExpr, res : Matrix option) =
        let size = matrixExpr.Size
        let res = 
            match size with
                | None -> raise (new ArgumentException("Elementwise size mismatch"))
                | Some(r,c) ->
                    match res with
                        | Some(v) when (r,c) <> v.LongSize -> raise (new ArgumentException("Elementwise size mismatch")) 
                        | Some(v) -> v
                        | None -> new Matrix(r, c, 0.0)
        ArgumentChecks.throwIfContainsDisposed [res]
        if res.LongSize <> (0L, 0L) then
            VectorExpr.EvalIn(matrixExpr.AsVectorExpr, Some res.ColMajorDataVector) |> ignore
        res

    static member (.<) (matrix1 : MatrixExpr, matrix2 : MatrixExpr) =
        BinaryMatrixFunction(matrix1, matrix2, 
                             fun v1 v2 res -> MklFunctions.D_Arrays_LessThan(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.<) (matrix1 : MatrixExpr, matrix2 : Matrix) =
        matrix1 .< matrix2.AsExpr

    static member (.<) (matrix1 : Matrix, matrix2 : MatrixExpr) =
        matrix1.AsExpr .< matrix2

    static member (.<) (matrix : MatrixExpr, a : float) =
        matrix .< Scalar(a)

    static member (.<) (a : float, matrix : MatrixExpr) =
        Scalar(a) .< matrix

    static member (.<=) (matrix1 : MatrixExpr, matrix2 : MatrixExpr) =
        BinaryMatrixFunction(matrix1, matrix2, 
                             fun v1 v2 res -> MklFunctions.D_Arrays_LessEqual(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.<=) (matrix1 : MatrixExpr, matrix2 : Matrix) =
        matrix1 .<= matrix2.AsExpr

    static member (.<=) (matrix1 : Matrix, matrix2 : MatrixExpr) =
        matrix1.AsExpr .<= matrix2

    static member (.<=) (matrix : MatrixExpr, a : float) =
        matrix .<= Scalar(a)

    static member (.<=) (a : float, matrix : MatrixExpr) =
        Scalar(a) .<= matrix

    static member (.>) (matrix1 : MatrixExpr, matrix2 : MatrixExpr) =
        BinaryMatrixFunction(matrix1, matrix2, 
                             fun v1 v2 res -> MklFunctions.D_Arrays_GreaterThan(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.>) (matrix1 : MatrixExpr, matrix2 : Matrix) =
        matrix1 .> matrix2.AsExpr

    static member (.>) (matrix1 : Matrix, matrix2 : MatrixExpr) =
        matrix1.AsExpr .> matrix2

    static member (.>) (matrix : MatrixExpr, a : float) =
        matrix .> Scalar(a)

    static member (.>) (a : float, matrix : MatrixExpr) =
        Scalar(a) .> matrix

    static member (.>=) (matrix1 : MatrixExpr, matrix2 : MatrixExpr) =
        BinaryMatrixFunction(matrix1, matrix2, 
                             fun v1 v2 res -> MklFunctions.D_Arrays_GreaterEqual(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.>=) (matrix1 : MatrixExpr, matrix2 : Matrix) =
        matrix1 .>= matrix2.AsExpr

    static member (.>=) (matrix1 : Matrix, matrix2 : MatrixExpr) =
        matrix1.AsExpr .>= matrix2

    static member (.>=) (matrix : MatrixExpr, a : float) =
        matrix .>= Scalar(a)

    static member (.>=) (a : float, matrix : MatrixExpr) =
        Scalar(a) .>= matrix

    static member (.=) (matrix1 : MatrixExpr, matrix2 : MatrixExpr) =
        BinaryMatrixFunction(matrix1, matrix2, 
                             fun v1 v2 res -> MklFunctions.D_Arrays_EqualElementwise(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.=) (matrix1 : MatrixExpr, matrix2 : Matrix) =
        matrix1 .= matrix2.AsExpr

    static member (.=) (matrix1 : Matrix, matrix2 : MatrixExpr) =
        matrix1.AsExpr .= matrix2

    static member (.=) (matrix : MatrixExpr, a : float) =
        matrix .= Scalar(a)

    static member (.=) (a : float, matrix : MatrixExpr) =
        Scalar(a) .= matrix

    static member (.<>) (matrix1 : MatrixExpr, matrix2 : MatrixExpr) =
        BinaryMatrixFunction(matrix1, matrix2, 
                             fun v1 v2 res -> MklFunctions.D_Arrays_NotEqualElementwise(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))
    
    static member (.<>) (matrix1 : MatrixExpr, matrix2 : Matrix) =
        matrix1 .<> matrix2.AsExpr

    static member (.<>) (matrix1 : Matrix, matrix2 : MatrixExpr) =
        matrix1.AsExpr .<> matrix2

    static member (.<>) (matrix : MatrixExpr, a : float) =
        matrix .<> Scalar(a)

    static member (.<>) (a : float, matrix : MatrixExpr) =
        Scalar(a) .<> matrix


    static member Min (matrix1 : MatrixExpr, matrix2 : MatrixExpr) =
        BinaryFunction(matrix1, matrix2, 
                       fun v1 v2 res -> MklFunctions.D_Min_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))

    static member Min (matrix1 : MatrixExpr, matrix2 : Matrix) =
       MatrixExpr.Min(matrix1, matrix2.AsExpr)

    static member Min (matrix1 : Matrix, matrix2 : MatrixExpr) =
        MatrixExpr.Min(matrix1.AsExpr, matrix2)

    static member Min (matrix : MatrixExpr, a : float) =
        MatrixExpr.Min(matrix, Scalar(a))

    static member Min (a : float, matrix : MatrixExpr) =
        MatrixExpr.Min(Scalar(a), matrix)

    static member Max (matrix1 : MatrixExpr, matrix2 : MatrixExpr) =
        BinaryFunction(matrix1, matrix2, 
                       fun v1 v2 res -> MklFunctions.D_Max_Arrays(v1.LongLength, v1.NativeArray, v2.LongLength, v2.NativeArray, res.NativeArray))

    static member Max (matrix1 : MatrixExpr, matrix2 : Matrix) =
       MatrixExpr.Max(matrix1, matrix2.AsExpr)

    static member Max (matrix1 : Matrix, matrix2 : MatrixExpr) =
        MatrixExpr.Max(matrix1.AsExpr, matrix2)

    static member Max (matrix : MatrixExpr, a : float) =
        MatrixExpr.Max(matrix, Scalar(a))

    static member Max (a : float, matrix : MatrixExpr) =
        MatrixExpr.Max(Scalar(a), matrix)



    static member (.*) (matrixExpr1 : MatrixExpr, matrixExpr2 : MatrixExpr) =
        BinaryFunction(matrixExpr1, matrixExpr2, fun v1 v2 res ->
                                                    if v1.LongLength = 1L then
                                                        MklFunctions.D_Scalar_Mul_Array(v1.[0], v2.LongLength, v2.NativeArray, res.NativeArray)
                                                    elif v2.LongLength = 1L then
                                                        MklFunctions.D_Scalar_Mul_Array(v2.[0], v1.LongLength, v1.NativeArray, res.NativeArray)
                                                    else
                                                       let len = v1.LongLength
                                                       MklFunctions.D_Array_Mul_Array(len, v1.NativeArray, v2.NativeArray, res.NativeArray)
                      )

    static member (.*) (matrixExpr1 : MatrixExpr, matrix2 : Matrix) =
        matrixExpr1 .* matrix2.AsExpr

    static member (.*) (matrix1 : Matrix, matrixExpr2 : MatrixExpr) =
        matrix1.AsExpr .* matrixExpr2

    static member (.*) (matrixExpr : MatrixExpr, a :  float) =
        matrixExpr .* Scalar(a)

    static member (.*) (a :  float, matrixExpr : MatrixExpr) =
        Scalar(a) .* matrixExpr 

    static member (+) (matrixExpr1 : MatrixExpr, matrixExpr2 : MatrixExpr) =
        BinaryFunction(matrixExpr1, matrixExpr2, fun v1 v2 res ->
                                                    if v1.LongLength = 1L then
                                                        MklFunctions.D_Scalar_Add_Array(v1.[0], v2.LongLength, v2.NativeArray, res.NativeArray)
                                                    elif v2.LongLength = 1L then
                                                        MklFunctions.D_Scalar_Add_Array(v2.[0], v1.LongLength, v1.NativeArray, res.NativeArray)
                                                    else
                                                       let len = v1.LongLength
                                                       MklFunctions.D_Array_Add_Array(len, v1.NativeArray, v2.NativeArray, res.NativeArray)
                      )

    static member (+) (matrixExpr1 : MatrixExpr, matrix2 : Matrix) =
        matrixExpr1 + matrix2.AsExpr

    static member (+) (matrix1 : Matrix, matrixExpr2 : MatrixExpr) =
        matrix1.AsExpr + matrixExpr2

    static member (+) (matrixExpr : MatrixExpr, a :  float) =
        matrixExpr + Scalar(a)

    static member (+) (a :  float, matrixExpr : MatrixExpr) =
        Scalar(a) + matrixExpr 

    static member (./) (matrixExpr1 : MatrixExpr, matrixExpr2 : MatrixExpr) =
        BinaryFunction(matrixExpr1, matrixExpr2, fun v1 v2 res ->
                                                    if v1.LongLength = 1L then
                                                        MklFunctions.D_Scalar_Div_Array(v1.[0], v2.LongLength, v2.NativeArray, res.NativeArray)
                                                    elif v2.LongLength = 1L then
                                                        MklFunctions.D_Array_Div_Scalar(v2.[0], v1.LongLength, v1.NativeArray, res.NativeArray)
                                                    else
                                                       let len = v1.LongLength
                                                       MklFunctions.D_Array_Div_Array(len, v1.NativeArray, v2.NativeArray, res.NativeArray)
                      )

    static member (./) (matrixExpr1 : MatrixExpr, matrix2 : Matrix) =
        matrixExpr1 ./ matrix2.AsExpr

    static member (./) (matrix1 : Matrix, matrixExpr2 : MatrixExpr) =
        matrix1.AsExpr ./ matrixExpr2

    static member (./) (matrixExpr : MatrixExpr, a :  float) =
        matrixExpr ./ Scalar(a)

    static member (./) (a :  float, matrixExpr : MatrixExpr) =
        Scalar(a) ./ matrixExpr


    static member (-) (matrixExpr1 : MatrixExpr, matrixExpr2 : MatrixExpr) =
        BinaryFunction(matrixExpr1, matrixExpr2, fun v1 v2 res ->
                                                    if v1.LongLength = 1L then
                                                        MklFunctions.D_Scalar_Sub_Array(v1.[0], v2.LongLength, v2.NativeArray, res.NativeArray)
                                                    elif v2.LongLength = 1L then
                                                        MklFunctions.D_Array_Sub_Scalar(v2.[0], v1.LongLength, v1.NativeArray, res.NativeArray)
                                                    else
                                                       let len = v1.LongLength
                                                       MklFunctions.D_Array_Sub_Array(len, v1.NativeArray, v2.NativeArray, res.NativeArray)
                      )

    static member (-) (matrixExpr1 : MatrixExpr, matrix2 : Matrix) =
        matrixExpr1 - matrix2.AsExpr

    static member (-) (matrix1 : Matrix, matrixExpr2 : MatrixExpr) =
        matrix1.AsExpr - matrixExpr2

    static member (-) (matrixExpr : MatrixExpr, a :  float) =
        matrixExpr - Scalar(a)

    static member (-) (a :  float, matrixExpr : MatrixExpr) =
        Scalar(a) - matrixExpr

    static member (~-) (matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Minus_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member (.^) (matrixExpr1 : MatrixExpr, matrixExpr2 : MatrixExpr) =
        BinaryFunction(matrixExpr1, matrixExpr2, fun v1 v2 res ->
                                                    if v1.LongLength = 1L then
                                                        MklFunctions.D_Scalar_Pow_Array(v1.[0], v2.LongLength, v2.NativeArray, res.NativeArray)
                                                    elif v2.LongLength = 1L then
                                                        MklFunctions.D_Array_Pow_scalar(v2.[0], v1.LongLength, v1.NativeArray, res.NativeArray)
                                                    else
                                                       let len = v1.LongLength
                                                       MklFunctions.D_Array_Pow_Array(len, v1.NativeArray, v2.NativeArray, res.NativeArray)
                      )

    static member (.^) (matrixExpr1 : MatrixExpr, matrix2 : Matrix) =
        matrixExpr1 .^ matrix2.AsExpr

    static member (.^) (matrix1 : Matrix, matrixExpr2 : MatrixExpr) =
        matrix1.AsExpr .^ matrixExpr2

    static member (.^) (matrixExpr : MatrixExpr, a :  float) =
        matrixExpr .^ Scalar(a)

    static member (.^) (a :  float, matrixExpr : MatrixExpr) =
        Scalar(a) .^ matrixExpr

    static member (.^) (matrixExpr : MatrixExpr, n : int) =
        if n = 1 then
            matrixExpr
        elif n > 1 then
            (matrixExpr .^ (n - 1)) .* matrixExpr
        elif n = 0 then
            matrixExpr .^ 0.0
        else 
            (matrixExpr .^ (-n)) .^ (-1.0)

    static member Abs(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Abs_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Sqrt(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Sqrt_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Sin(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Sin_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Cos(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Cos_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Tan(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Tan_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Asin(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_ASin_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Acos(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_ACos_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Atan(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_ATan_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Sinh(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Sinh_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Cosh(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Cosh_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Tanh(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Tanh_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member ASinh(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_ASinh_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member ACosh(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_ACosh_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member ATanh(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_ATanh_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Exp(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Exp_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Expm1(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Expm1_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Log(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Ln_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Log10(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Log10_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Log1p(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Log1p_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Erf(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Erf_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Erfc(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Erfc_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Erfinv(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Erfinv_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Erfcinv(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Erfcinv_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Normcdf(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_CdfNorm_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Norminv(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_CdfNormInv_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Round(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Round_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Ceiling(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Ceil_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Floor(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Floor_Array(v.LongLength, v.NativeArray, res.NativeArray))

    static member Truncate(matrixExpr : MatrixExpr) =
        UnaryFunction(matrixExpr, fun v res -> MklFunctions.D_Trunc_Array(v.LongLength, v.NativeArray, res.NativeArray))