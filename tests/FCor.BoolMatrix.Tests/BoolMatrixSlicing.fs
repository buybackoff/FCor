namespace FCor.BoolMatrix.Tests

open FCor
open FCor.Math
open FCor.BasicStats
open FCor.Random
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System
open MLApp
open Util

module BoolMatrixSlicing =

    let rnd = new Random()

    [<Property>]
    let ``GetItem int64``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let index = rnd.Next(v.Length) |> int64
                                    setScalar app "index" (float(index + 1L))
                                    app.Execute("res = v(index);") |> ignore
                                    let res = getBoolMatrix app "res"
                                    let v = new BoolMatrix(v)
                                    Array2D.create 1 1 v.[index] = res       
                                    )

    [<Property>]
    let ``GetItem int64 int64``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let rowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let colIndex = rnd.Next(v.GetLength(1)) |> int64
                                    setScalar app "rowindex" (float(rowIndex + 1L))
                                    setScalar app "colindex" (float(colIndex + 1L))
                                    app.Execute("res = v(rowindex,colindex);") |> ignore
                                    let res = getBoolMatrix app "res"
                                    let v = new BoolMatrix(v)
                                    Array2D.create 1 1 v.[rowIndex, colIndex] = res       
                                    )

    [<Property>]
    let ``GetItem int``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let index = rnd.Next(v.Length) 
                                    setScalar app "index" (float(index + 1))
                                    app.Execute("res = v(index);") |> ignore
                                    let res = getBoolMatrix app "res"
                                    let v = new BoolMatrix(v)
                                    Array2D.create 1 1 v.[index] = res       
                                    )

    [<Property>]
    let ``GetItem int int``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let rowIndex = rnd.Next(v.GetLength(0)) 
                                    let colIndex = rnd.Next(v.GetLength(1)) 
                                    setScalar app "rowindex" (float(rowIndex + 1))
                                    setScalar app "colindex" (float(colIndex + 1))
                                    app.Execute("res = v(rowindex,colindex);") |> ignore
                                    let res = getBoolMatrix app "res"
                                    let v = new BoolMatrix(v)
                                    Array2D.create 1 1 v.[rowIndex, colIndex] = res       
                                    )

    [<Property>]
    let ``SetItem int64``(v : bool[,]) (a : bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let index = rnd.Next(v.Length) |> int64
                                    setScalar app "index" (float(index + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(index) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.[index] <- a
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetItem int64 int64``(v : bool[,]) (a : bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let rowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let colIndex = rnd.Next(v.GetLength(1)) |> int64
                                    setScalar app "rowindex" (float(rowIndex + 1L))
                                    setScalar app "colindex" (float(colIndex + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(rowindex, colindex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.[rowIndex, colIndex] <- a
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetItem int``(v : bool[,]) (a : bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let index = rnd.Next(v.Length) 
                                    setScalar app "index" (float(index + 1))
                                    setBoolScalar app "a" a
                                    app.Execute("v(index) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.[index] <- a
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetItem int int``(v : bool[,]) (a : bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let rowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let colIndex = rnd.Next(v.GetLength(1)) |> int64
                                    setScalar app "rowindex" (float(rowIndex + 1L))
                                    setScalar app "colindex" (float(colIndex + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(rowindex, colindex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.[rowIndex, colIndex] <- a
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``GetItem int64 seq``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let indices = [|0..v.Length-1|] |> Array.map (fun x -> rnd.Next(v.Length) |> int64)
                                    setVector app "indices" (indices |> Array.map (fun index -> float(index + 1L)))
                                    app.Execute("res = v(indices);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolMatrix(v)
                                    v.[indices |> Array.toSeq].ToArray() = res       
                                    )

    [<Property>]
    let ``GetItem int64 seq, int64 seq``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let rowIndices = [|0..v.GetLength(0)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(0)) |> int64)
                                    let colIndices = [|0..v.GetLength(1)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(1)) |> int64)
                                    setVector app "rowindices" (rowIndices |> Array.map (fun index -> float(index + 1L)))
                                    setVector app "colindices" (colIndices |> Array.map (fun index -> float(index + 1L)))
                                    app.Execute("res = v(rowindices, colindices);") |> ignore
                                    let res = getBoolMatrix app "res"
                                    let v = new BoolMatrix(v)
                                    v.[rowIndices |> Array.toSeq, colIndices |> Array.toSeq].ToArray2D() = res       
                                    )

    [<Property>]
    let ``GetItem int64, int64 seq``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let rowIndices = [|rnd.Next(v.GetLength(0)) |> int64|] 
                                    let colIndices = [|0..v.GetLength(1)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(1)) |> int64)
                                    setVector app "rowindices" (rowIndices |> Array.map (fun index -> float(index + 1L)))
                                    setVector app "colindices" (colIndices |> Array.map (fun index -> float(index + 1L)))
                                    app.Execute("res = v(rowindices, colindices);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolMatrix(v)
                                    v.[rowIndices.[0], colIndices |> Array.toSeq].ToArray() = res       
                                    )

    [<Property>]
    let ``GetItem int64 seq, int64``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let rowIndices = [|0..v.GetLength(0)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(0)) |> int64)
                                    let colIndices = [|rnd.Next(v.GetLength(1)) |> int64|] 
                                    setVector app "rowindices" (rowIndices |> Array.map (fun index -> float(index + 1L)))
                                    setVector app "colindices" (colIndices |> Array.map (fun index -> float(index + 1L)))
                                    app.Execute("res = v(rowindices, colindices);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolMatrix(v)
                                    v.[rowIndices |> Array.toSeq, colIndices.[0]].ToArray() = res       
                                    )

    [<Property>]
    let ``GetItem int seq``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let indices = [|0..v.Length-1|] |> Array.map (fun x -> rnd.Next(v.Length))
                                    setVector app "indices" (indices |> Array.map (fun index -> float(index + 1)))
                                    app.Execute("res = v(indices);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolMatrix(v)
                                    v.[indices |> Array.toSeq].ToArray() = res       
                                    )

    [<Property>]
    let ``GetItem int seq, int seq``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let rowIndices = [|0..v.GetLength(0)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(0)))
                                    let colIndices = [|0..v.GetLength(1)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(1)))
                                    setVector app "rowindices" (rowIndices |> Array.map (fun index -> float(index + 1)))
                                    setVector app "colindices" (colIndices |> Array.map (fun index -> float(index + 1)))
                                    app.Execute("res = v(rowindices, colindices);") |> ignore
                                    let res = getBoolMatrix app "res"
                                    let v = new BoolMatrix(v)
                                    v.[rowIndices |> Array.toSeq, colIndices |> Array.toSeq].ToArray2D() = res        
                                    )

    [<Property>]
    let ``GetItem int, int seq``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let rowIndices = [|rnd.Next(v.GetLength(0))|]
                                    let colIndices = [|0..v.GetLength(1)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(1)))
                                    setVector app "rowindices" (rowIndices |> Array.map (fun index -> float(index + 1)))
                                    setVector app "colindices" (colIndices |> Array.map (fun index -> float(index + 1)))
                                    app.Execute("res = v(rowindices, colindices);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolMatrix(v)
                                    v.[rowIndices.[0], colIndices |> Array.toSeq].ToArray() = res        
                                    )

    [<Property>]
    let ``GetItem int seq, int``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let rowIndices = [|0..v.GetLength(0)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(0)))
                                    let colIndices = [|rnd.Next(v.GetLength(1))|] 
                                    setVector app "rowindices" (rowIndices |> Array.map (fun index -> float(index + 1)))
                                    setVector app "colindices" (colIndices |> Array.map (fun index -> float(index + 1)))
                                    app.Execute("res = v(rowindices, colindices);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolMatrix(v)
                                    v.[rowIndices |> Array.toSeq, colIndices.[0]].ToArray() = res        
                                    )

    [<Property>]
    let ``SetItem int64 seq scalar``(v : bool[,]) (a : bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolVector app "a" [|a|]
                                    let a = new BoolVector(a)
                                    setBoolMatrix app "v" v
                                    let indices = [|0..v.Length-1|] |> Array.map (fun x -> rnd.Next(v.Length) |> int64)
                                    setVector app "indices" (indices |> Array.map (fun index -> float(index + 1L)))
                                    app.Execute("v(indices) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.[indices |> Array.toSeq] <- a
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetItem int64 seq, int64 seq scalar``(v : bool[,]) (a : bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolVector app "a" [|a|]
                                    let a = new BoolMatrix(a)
                                    setBoolMatrix app "v" v
                                    let rowIndices = [|0..v.GetLength(0)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(0)) |> int64)
                                    let colIndices = [|0..v.GetLength(1)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(1)) |> int64)
                                    setVector app "rowindices" (rowIndices |> Array.map (fun index -> float(index + 1L)))
                                    setVector app "colindices" (colIndices |> Array.map (fun index -> float(index + 1L)))
                                    app.Execute("v(rowindices, colindices) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.[rowIndices |> Array.toSeq, colIndices |> Array.toSeq] <- a
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetItem int64, int64 seq scalar``(v : bool[,]) (a : bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolVector app "a" [|a|]
                                    let a = new BoolVector(a)
                                    setBoolMatrix app "v" v
                                    let rowIndices = [|rnd.Next(v.GetLength(0)) |> int64|] 
                                    let colIndices = [|0..v.GetLength(1)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(1)) |> int64)
                                    setVector app "rowindices" (rowIndices |> Array.map (fun index -> float(index + 1L)))
                                    setVector app "colindices" (colIndices |> Array.map (fun index -> float(index + 1L)))
                                    app.Execute("v(rowindices, colindices) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.[rowIndices.[0], colIndices |> Array.toSeq] <- a
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetItem int64 seq, int64 scalar``(v : bool[,]) (a : bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolVector app "a" [|a|]
                                    let a = new BoolVector(a)
                                    setBoolMatrix app "v" v
                                    let rowIndices = [|0..v.GetLength(0)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(0)) |> int64)
                                    let colIndices = [|rnd.Next(v.GetLength(1)) |> int64|] 
                                    setVector app "rowindices" (rowIndices |> Array.map (fun index -> float(index + 1L)))
                                    setVector app "colindices" (colIndices |> Array.map (fun index -> float(index + 1L)))
                                    app.Execute("v(rowindices, colindices) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.[rowIndices |> Array.toSeq, colIndices.[0]] <- a
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetItem int seq scalar``(v : bool[,]) (a : bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolVector app "a" [|a|]
                                    let a = new BoolVector(a)
                                    setBoolMatrix app "v" v
                                    let indices = [|0..v.Length-1|] |> Array.map (fun x -> rnd.Next(v.Length))
                                    setVector app "indices" (indices |> Array.map (fun index -> float(index + 1)))
                                    app.Execute("v(indices) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.[indices |> Array.toSeq] <- a
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetItem int seq, int seq scalar``(v : bool[,]) (a : bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolVector app "a" [|a|]
                                    let a = new BoolMatrix(a)
                                    setBoolMatrix app "v" v
                                    let rowIndices = [|0..v.GetLength(0)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(0)))
                                    let colIndices = [|0..v.GetLength(1)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(1)))
                                    setVector app "rowindices" (rowIndices |> Array.map (fun index -> float(index + 1)))
                                    setVector app "colindices" (colIndices |> Array.map (fun index -> float(index + 1)))
                                    app.Execute("v(rowindices, colindices) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.[rowIndices |> Array.toSeq, colIndices |> Array.toSeq] <- a
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetItem int, int seq scalar``(v : bool[,]) (a : bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolVector app "a" [|a|]
                                    let a = new BoolVector(a)
                                    setBoolMatrix app "v" v
                                    let rowIndices = [|rnd.Next(v.GetLength(0))|] 
                                    let colIndices = [|0..v.GetLength(1)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(1)))
                                    setVector app "rowindices" (rowIndices |> Array.map (fun index -> float(index + 1)))
                                    setVector app "colindices" (colIndices |> Array.map (fun index -> float(index + 1)))
                                    app.Execute("v(rowindices, colindices) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.[rowIndices.[0], colIndices |> Array.toSeq] <- a
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetItem int seq, int scalar``(v : bool[,]) (a : bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolVector app "a" [|a|]
                                    let a = new BoolVector(a)
                                    setBoolMatrix app "v" v
                                    let rowIndices = [|0..v.GetLength(0)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(0)))
                                    let colIndices = [|rnd.Next(v.GetLength(1))|] 
                                    setVector app "rowindices" (rowIndices |> Array.map (fun index -> float(index + 1)))
                                    setVector app "colindices" (colIndices |> Array.map (fun index -> float(index + 1)))
                                    app.Execute("v(rowindices, colindices) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.[rowIndices |> Array.toSeq, colIndices.[0]] <- a
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetItem int64 seq vector``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let indices = [|0..v.Length-1|] |> Array.map (fun x -> rnd.Next(v.Length) |> int64)
                                    let a = [|0..v.Length-1|] |> Array.map (fun x -> rnd.NextDouble() < 0.5)
                                    setBoolVector app "a" a
                                    setVector app "indices" (indices |> Array.map (fun index -> float(index + 1L)))
                                    app.Execute("v(indices) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.[indices |> Array.toSeq] <- new BoolVector(a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetItem int64 seq, int64 seq Matrix``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let rowIndices = [|0..v.GetLength(0)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(0)) |> int64)
                                    let colIndices = [|0..v.GetLength(1)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(1)) |> int64)
                                    setVector app "rowindices" (rowIndices |> Array.map (fun index -> float(index + 1L)))
                                    setVector app "colindices" (colIndices |> Array.map (fun index -> float(index + 1L)))
                                    let a = Array2D.init rowIndices.Length colIndices.Length (fun i j -> rnd.NextDouble() < 0.5)
                                    setBoolMatrix app "a" a
                                    app.Execute("v(rowindices, colindices) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.[rowIndices |> Array.toSeq, colIndices |> Array.toSeq] <- new BoolMatrix(a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetItem int64, int64 seq vector``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let rowIndices = [|rnd.Next(v.GetLength(0)) |> int64|] 
                                    let colIndices = [|0..v.GetLength(1)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(1)) |> int64)
                                    setVector app "rowindices" (rowIndices |> Array.map (fun index -> float(index + 1L)))
                                    setVector app "colindices" (colIndices |> Array.map (fun index -> float(index + 1L)))
                                    let a = Array.init colIndices.Length (fun i -> rnd.NextDouble() < 0.5)
                                    setBoolVector app "a" a
                                    app.Execute("v(rowindices, colindices) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.[rowIndices.[0], colIndices |> Array.toSeq] <- new BoolVector(a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetItem int64 seq, int64 vector``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let rowIndices = [|0..v.GetLength(0)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(0)) |> int64)
                                    let colIndices = [|rnd.Next(v.GetLength(1)) |> int64|] 
                                    setVector app "rowindices" (rowIndices |> Array.map (fun index -> float(index + 1L)))
                                    setVector app "colindices" (colIndices |> Array.map (fun index -> float(index + 1L)))
                                    let a = Array.init rowIndices.Length (fun i -> rnd.NextDouble() < 0.5)
                                    setBoolVector app "a" a
                                    app.Execute("v(rowindices, colindices) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.[rowIndices |> Array.toSeq, colIndices.[0]] <- new BoolVector(a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetItem int seq vector``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let indices = [|0..v.Length-1|] |> Array.map (fun x -> rnd.Next(v.Length))
                                    let a = [|0..v.Length-1|] |> Array.map (fun x -> rnd.NextDouble() < 0.5)
                                    setBoolVector app "a" a
                                    setVector app "indices" (indices |> Array.map (fun index -> float(index + 1)))
                                    app.Execute("v(indices) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.[indices |> Array.toSeq] <- new BoolVector(a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetItem int seq, int seq Matrix``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let rowIndices = [|0..v.GetLength(0)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(0)))
                                    let colIndices = [|0..v.GetLength(1)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(1)))
                                    setVector app "rowindices" (rowIndices |> Array.map (fun index -> float(index + 1)))
                                    setVector app "colindices" (colIndices |> Array.map (fun index -> float(index + 1)))
                                    let a = Array2D.init rowIndices.Length colIndices.Length (fun i j -> rnd.NextDouble() < 0.5)
                                    setBoolMatrix app "a" a
                                    app.Execute("v(rowindices, colindices) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.[rowIndices |> Array.toSeq, colIndices |> Array.toSeq] <- new BoolMatrix(a)
                                    v.ToArray2D() = res        
                                    )

    [<Property>]
    let ``SetItem int, int seq vector``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let rowIndices = [|rnd.Next(v.GetLength(0))|] 
                                    let colIndices = [|0..v.GetLength(1)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(1)))
                                    setVector app "rowindices" (rowIndices |> Array.map (fun index -> float(index + 1)))
                                    setVector app "colindices" (colIndices |> Array.map (fun index -> float(index + 1)))
                                    let a = Array.init colIndices.Length (fun i -> rnd.NextDouble() < 0.5)
                                    setBoolVector app "a" a
                                    app.Execute("v(rowindices, colindices) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.[rowIndices.[0], colIndices |> Array.toSeq] <- new BoolVector(a)
                                    v.ToArray2D() = res        
                                    )

    [<Property>]
    let ``SetItem int seq, int vector``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let rowIndices = [|0..v.GetLength(0)-1|] |> Array.map (fun x -> rnd.Next(v.GetLength(0)))
                                    let colIndices = [|rnd.Next(v.GetLength(1))|] 
                                    setVector app "rowindices" (rowIndices |> Array.map (fun index -> float(index + 1)))
                                    setVector app "colindices" (colIndices |> Array.map (fun index -> float(index + 1)))
                                    let a = Array.init rowIndices.Length (fun i -> rnd.NextDouble() < 0.5)
                                    setBoolVector app "a" a
                                    app.Execute("v(rowindices, colindices) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.[rowIndices |> Array.toSeq, colIndices.[0]] <- new BoolVector(a)
                                    v.ToArray2D() = res        
                                    )

    [<Property>]
    let ``GetSlice Some int64, Some int64``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    app.Execute("res = v(fromIndex:toIndex);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolMatrix(v)
                                    v.[fromIndex..toIndex].ToArray() = res       
                                    )

    [<Property>]
    let ``GetSlice Some int64, Some int64, Some int64, Some int64``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let toRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let fromColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    let toColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1L))
                                    setScalar app "toRowIndex" (float(toRowIndex + 1L))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1L))
                                    setScalar app "toColIndex" (float(toColIndex + 1L))
                                    app.Execute("res = v(fromRowIndex:toRowIndex, fromColIndex:toColIndex);") |> ignore
                                    let res = getBoolMatrix app "res"
                                    let v = new BoolMatrix(v)
                                    v.[fromRowIndex..toRowIndex, fromColIndex..toColIndex].ToArray2D() = res       
                                    )

    [<Property>]
    let ``GetSlice Some int64, None``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    app.Execute("res = v(fromIndex:end);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolMatrix(v)
                                    v.[fromIndex..].ToArray() = res       
                                    )

    [<Property>]
    let ``GetSlice Some int64, None, Some int64, None``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let fromColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1L))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1L))
                                    app.Execute("res = v(fromRowIndex:end, fromColIndex:end);") |> ignore
                                    let res = getBoolMatrix app "res"
                                    let v = new BoolMatrix(v)
                                    v.[fromRowIndex.., fromColIndex..].ToArray2D() = res        
                                    )

    [<Property>]
    let ``GetSlice None, Some int64``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    app.Execute("res = v(1:toIndex);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolMatrix(v)
                                    v.[..toIndex].ToArray() = res       
                                    )

    [<Property>]
    let ``GetSlice None, Some int64, None, Some int64``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let toRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let toColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    setScalar app "toRowIndex" (float(toRowIndex + 1L))
                                    setScalar app "toColIndex" (float(toColIndex + 1L))
                                    app.Execute("res = v(1:toRowIndex, 1:toColIndex);") |> ignore
                                    let res = getBoolMatrix app "res"
                                    let v = new BoolMatrix(v)
                                    v.[..toRowIndex, ..toColIndex].ToArray2D() = res      
                                    )

    [<Property>]
    let ``GetSlice Some int, Some int``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromIndex = rnd.Next(v.Length)
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    app.Execute("res = v(fromIndex:toIndex);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolMatrix(v)
                                    v.[fromIndex..toIndex].ToArray() = res       
                                    )

    [<Property>]
    let ``GetSlice Some int, Some int, Some int, Some int``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) 
                                    let toRowIndex = rnd.Next(v.GetLength(0)) 
                                    let fromColIndex = rnd.Next(v.GetLength(1)) 
                                    let toColIndex = rnd.Next(v.GetLength(1)) 
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1))
                                    setScalar app "toRowIndex" (float(toRowIndex + 1))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1))
                                    setScalar app "toColIndex" (float(toColIndex + 1))
                                    app.Execute("res = v(fromRowIndex:toRowIndex, fromColIndex:toColIndex);") |> ignore
                                    let res = getBoolMatrix app "res"
                                    let v = new BoolMatrix(v)
                                    v.[fromRowIndex..toRowIndex, fromColIndex..toColIndex].ToArray2D() = res       
                                    )

    [<Property>]
    let ``GetSlice Some int, None``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromIndex = rnd.Next(v.Length)
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    app.Execute("res = v(fromIndex:end);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolMatrix(v)
                                    v.[fromIndex..].ToArray() = res       
                                    )

    [<Property>]
    let ``GetSlice Some int, None, Some int, None``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) 
                                    let fromColIndex = rnd.Next(v.GetLength(1)) 
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1))
                                    app.Execute("res = v(fromRowIndex:end, fromColIndex:end);") |> ignore
                                    let res = getBoolMatrix app "res"
                                    let v = new BoolMatrix(v)
                                    v.[fromRowIndex.., fromColIndex..].ToArray2D() = res        
                                    )

    [<Property>]
    let ``GetSlice None, Some int``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    app.Execute("res = v(1:toIndex);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolMatrix(v)
                                    v.[..toIndex].ToArray() = res       
                                    )

    [<Property>]
    let ``GetSlice None, Some int, None, Some int``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let toRowIndex = rnd.Next(v.GetLength(0)) 
                                    let toColIndex = rnd.Next(v.GetLength(1)) 
                                    setScalar app "toRowIndex" (float(toRowIndex + 1))
                                    setScalar app "toColIndex" (float(toColIndex + 1))
                                    app.Execute("res = v(1:toRowIndex, 1:toColIndex);") |> ignore
                                    let res = getBoolMatrix app "res"
                                    let v = new BoolMatrix(v)
                                    v.[..toRowIndex, ..toColIndex].ToArray2D() = res      
                                    )

    [<Property>]
    let ``SetSlice Some int64, Some int64 scalar``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromIndex:toIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromIndex, Some toIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, Some int64 scalar vector``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromIndex:toIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromIndex, Some toIndex, new BoolVector(a))
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, Some int64, Some int64, Some int64 scalar``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let toRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let fromColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    let toColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1L))
                                    setScalar app "toRowIndex" (float(toRowIndex + 1L))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1L))
                                    setScalar app "toColIndex" (float(toColIndex + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromRowIndex:toRowIndex, fromColIndex:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromRowIndex, Some toRowIndex, Some fromColIndex, Some toColIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice int64, Some int64, Some int64 scalar``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let toRowIndex = fromRowIndex
                                    let fromColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    let toColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1L))
                                    setScalar app "toRowIndex" (float(toRowIndex + 1L))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1L))
                                    setScalar app "toColIndex" (float(toColIndex + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromRowIndex:toRowIndex, fromColIndex:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(fromRowIndex, Some fromColIndex, Some toColIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, Some int64, int64 scalar``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let toRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let fromColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    let toColIndex = fromColIndex
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1L))
                                    setScalar app "toRowIndex" (float(toRowIndex + 1L))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1L))
                                    setScalar app "toColIndex" (float(toColIndex + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromRowIndex:toRowIndex, fromColIndex:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromRowIndex, Some toRowIndex, toColIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, Some int64, Some int64, Some int64 scalar matrix``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let toRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let fromColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    let toColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1L))
                                    setScalar app "toRowIndex" (float(toRowIndex + 1L))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1L))
                                    setScalar app "toColIndex" (float(toColIndex + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromRowIndex:toRowIndex, fromColIndex:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromRowIndex, Some toRowIndex, Some fromColIndex, Some toColIndex, new BoolMatrix(a))
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice int64, Some int64, Some int64 scalar vector``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let toRowIndex = fromRowIndex
                                    let fromColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    let toColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1L))
                                    setScalar app "toRowIndex" (float(toRowIndex + 1L))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1L))
                                    setScalar app "toColIndex" (float(toColIndex + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromRowIndex:toRowIndex, fromColIndex:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(fromRowIndex, Some fromColIndex, Some toColIndex, new BoolVector(a))
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, Some int64, int64 scalar vector``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let toRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let fromColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    let toColIndex = fromColIndex
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1L))
                                    setScalar app "toRowIndex" (float(toRowIndex + 1L))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1L))
                                    setScalar app "toColIndex" (float(toColIndex + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromRowIndex:toRowIndex, fromColIndex:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromRowIndex, Some toRowIndex, toColIndex, new BoolVector(a))
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, None scalar``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromIndex:end) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromIndex, None, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, None, Some int64, None scalar``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let fromColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1L))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromRowIndex:end, fromColIndex:end) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromRowIndex, None, Some fromColIndex, None, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice None, Some int64 scalar``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(1:toIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(None, Some toIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice None, Some int64, None, Some int64 scalar``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let toRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let toColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    setScalar app "toRowIndex" (float(toRowIndex + 1L))
                                    setScalar app "toColIndex" (float(toColIndex + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(1:toRowIndex, 1:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(None, Some toRowIndex, None, Some toColIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int, Some int scalar``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromIndex = rnd.Next(v.Length) 
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromIndex:toIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromIndex, Some toIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int, Some int scalar vector``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromIndex = rnd.Next(v.Length) 
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromIndex:toIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromIndex, Some toIndex, new BoolVector(a))
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int, Some int, Some int, Some int scalar``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) 
                                    let toRowIndex = rnd.Next(v.GetLength(0)) 
                                    let fromColIndex = rnd.Next(v.GetLength(1)) 
                                    let toColIndex = rnd.Next(v.GetLength(1)) 
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1))
                                    setScalar app "toRowIndex" (float(toRowIndex + 1))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1))
                                    setScalar app "toColIndex" (float(toColIndex + 1))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromRowIndex:toRowIndex, fromColIndex:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromRowIndex, Some toRowIndex, Some fromColIndex, Some toColIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice int, Some int, Some int scalar``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) 
                                    let toRowIndex = fromRowIndex 
                                    let fromColIndex = rnd.Next(v.GetLength(1)) 
                                    let toColIndex = rnd.Next(v.GetLength(1)) 
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1))
                                    setScalar app "toRowIndex" (float(toRowIndex + 1))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1))
                                    setScalar app "toColIndex" (float(toColIndex + 1))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromRowIndex:toRowIndex, fromColIndex:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(fromRowIndex, Some fromColIndex, Some toColIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int, Some int, int scalar``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) 
                                    let toRowIndex = rnd.Next(v.GetLength(0)) 
                                    let fromColIndex = rnd.Next(v.GetLength(1)) 
                                    let toColIndex = fromColIndex
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1))
                                    setScalar app "toRowIndex" (float(toRowIndex + 1))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1))
                                    setScalar app "toColIndex" (float(toColIndex + 1))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromRowIndex:toRowIndex, fromColIndex:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromRowIndex, Some toRowIndex, fromColIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int, Some int, Some int, Some int scalar matrix``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) 
                                    let toRowIndex = rnd.Next(v.GetLength(0)) 
                                    let fromColIndex = rnd.Next(v.GetLength(1)) 
                                    let toColIndex = rnd.Next(v.GetLength(1)) 
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1))
                                    setScalar app "toRowIndex" (float(toRowIndex + 1))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1))
                                    setScalar app "toColIndex" (float(toColIndex + 1))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromRowIndex:toRowIndex, fromColIndex:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromRowIndex, Some toRowIndex, Some fromColIndex, Some toColIndex, new BoolMatrix(a))
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice int, Some int, Some int scalar vector``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) 
                                    let toRowIndex = fromRowIndex 
                                    let fromColIndex = rnd.Next(v.GetLength(1)) 
                                    let toColIndex = rnd.Next(v.GetLength(1)) 
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1))
                                    setScalar app "toRowIndex" (float(toRowIndex + 1))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1))
                                    setScalar app "toColIndex" (float(toColIndex + 1))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromRowIndex:toRowIndex, fromColIndex:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(fromRowIndex, Some fromColIndex, Some toColIndex, new BoolVector(a))
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int, Some int, int scalar vector``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) 
                                    let toRowIndex = rnd.Next(v.GetLength(0)) 
                                    let fromColIndex = rnd.Next(v.GetLength(1)) 
                                    let toColIndex = fromColIndex
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1))
                                    setScalar app "toRowIndex" (float(toRowIndex + 1))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1))
                                    setScalar app "toColIndex" (float(toColIndex + 1))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromRowIndex:toRowIndex, fromColIndex:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromRowIndex, Some toRowIndex, fromColIndex, new BoolVector(a))
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int, None scalar``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromIndex = rnd.Next(v.Length) 
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromIndex:end) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromIndex, None, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int, None, Some int, None scalar``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) 
                                    let fromColIndex = rnd.Next(v.GetLength(1)) 
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromRowIndex:end, fromColIndex:end) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromRowIndex, None, Some fromColIndex, None, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice None, Some int scalar``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    setBoolScalar app "a" a
                                    app.Execute("v(1:toIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(None, Some toIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice None, Some int, None, Some int scalar``(v : bool[,]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let toRowIndex = rnd.Next(v.GetLength(0)) 
                                    let toColIndex = rnd.Next(v.GetLength(1)) 
                                    setScalar app "toRowIndex" (float(toRowIndex + 1))
                                    setScalar app "toColIndex" (float(toColIndex + 1))
                                    setBoolScalar app "a" a
                                    app.Execute("v(1:toRowIndex, 1:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(None, Some toRowIndex, None, Some toColIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, Some int64 vector``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    let a =
                                        if toIndex >= fromIndex then
                                            new BoolVector(Array.init (int(toIndex - fromIndex + 1L)) (fun i -> rnd.NextDouble() < 0.5))
                                        else BoolVector.Empty
                                    setBoolVector app "a" (a.ToArray())
                                    app.Execute("v(fromIndex:toIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromIndex, Some toIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, Some int64, Some int64, Some int64 Matrix``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let toRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let fromColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    let toColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1L))
                                    setScalar app "toRowIndex" (float(toRowIndex + 1L))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1L))
                                    setScalar app "toColIndex" (float(toColIndex + 1L))
                                    let a =
                                        if toRowIndex >= fromRowIndex && toColIndex >= fromColIndex then
                                            new BoolMatrix(Array2D.init (int(toRowIndex - fromRowIndex + 1L)) (int(toColIndex - fromColIndex + 1L)) (fun i j -> rnd.NextDouble() < 0.5))
                                        else BoolMatrix.Empty
                                    setBoolMatrix app "a" (a.ToArray2D())
                                    app.Execute("v(fromRowIndex:toRowIndex, fromColIndex:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromRowIndex, Some toRowIndex, Some fromColIndex, Some toColIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice int64, Some int64, Some int64 vector``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let toRowIndex = fromRowIndex
                                    let fromColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    let toColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1L))
                                    setScalar app "toRowIndex" (float(toRowIndex + 1L))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1L))
                                    setScalar app "toColIndex" (float(toColIndex + 1L))
                                    let a =
                                        if toRowIndex >= fromRowIndex && toColIndex >= fromColIndex then
                                            new BoolVector(Array.init (int(toColIndex - fromColIndex + 1L)) (fun i -> rnd.NextDouble() < 0.5))
                                        else BoolVector.Empty
                                    setBoolVector app "a" (a.ToArray())
                                    app.Execute("v(fromRowIndex:toRowIndex, fromColIndex:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(fromRowIndex, Some fromColIndex, Some toColIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, Some int64, int64 vector``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let toRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let fromColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    let toColIndex = fromColIndex
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1L))
                                    setScalar app "toRowIndex" (float(toRowIndex + 1L))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1L))
                                    setScalar app "toColIndex" (float(toColIndex + 1L))
                                    let a =
                                        if toRowIndex >= fromRowIndex && toColIndex >= fromColIndex then
                                            new BoolVector(Array.init (int(toRowIndex - fromRowIndex + 1L)) (fun i -> rnd.NextDouble() < 0.5))
                                        else BoolVector.Empty
                                    setBoolVector app "a" (a.ToArray())
                                    app.Execute("v(fromRowIndex:toRowIndex, fromColIndex:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromRowIndex, Some toRowIndex, fromColIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, None Matrix``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    let v = new BoolMatrix(v)
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    let a =  new BoolVector(Array.init (int(v.LongLength - fromIndex)) (fun i -> rnd.NextDouble() < 0.5))
                                    setBoolVector app "a" (a.ToArray())
                                    app.Execute("v(fromIndex:end) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    v.SetSlice(Some fromIndex, None, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, None, Some int64, None Matrix``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let fromColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1L))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1L))
                                    let a =
                                        new BoolMatrix(Array2D.init (int(v.GetLongLength(0) - fromRowIndex)) (int(v.GetLongLength(1) - fromColIndex)) (fun i j -> rnd.NextDouble() < 0.5))
                                    setBoolMatrix app "a" (a.ToArray2D())
                                    app.Execute("v(fromRowIndex:end, fromColIndex:end) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromRowIndex, None, Some fromColIndex, None, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice None, Some int64 Matrix``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    let a =
                                        if toIndex >= 0L then
                                            new BoolVector(Array.init (int(toIndex + 1L)) (fun i -> rnd.NextDouble() < 0.5))
                                        else BoolVector.Empty
                                    setBoolVector app "a" (a.ToArray())
                                    app.Execute("v(1:toIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(None, Some toIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice None, Some int64, None, Some int64 Matrix``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let toRowIndex = rnd.Next(v.GetLength(0)) |> int64
                                    let toColIndex = rnd.Next(v.GetLength(1)) |> int64
                                    setScalar app "toRowIndex" (float(toRowIndex + 1L))
                                    setScalar app "toColIndex" (float(toColIndex + 1L))
                                    let a =
                                        if toRowIndex >= 0L && toColIndex >= 0L then
                                            new BoolMatrix(Array2D.init (int(toRowIndex + 1L)) (int(toColIndex + 1L)) (fun i j -> rnd.NextDouble() < 0.5))
                                        else BoolMatrix.Empty
                                    setBoolMatrix app "a" (a.ToArray2D())
                                    app.Execute("v(1:toRowIndex, 1:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(None, Some toRowIndex, None, Some toColIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int, Some int vector``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromIndex = rnd.Next(v.Length) 
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    let a =
                                        if toIndex >= fromIndex then
                                            new BoolVector(Array.init (toIndex - fromIndex + 1) (fun i -> rnd.NextDouble() < 0.5))
                                        else BoolVector.Empty
                                    setBoolVector app "a" (a.ToArray())
                                    app.Execute("v(fromIndex:toIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromIndex, Some toIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int, Some int, Some int, Some int Matrix``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) 
                                    let toRowIndex = rnd.Next(v.GetLength(0)) 
                                    let fromColIndex = rnd.Next(v.GetLength(1)) 
                                    let toColIndex = rnd.Next(v.GetLength(1)) 
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1))
                                    setScalar app "toRowIndex" (float(toRowIndex + 1))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1))
                                    setScalar app "toColIndex" (float(toColIndex + 1))
                                    let a =
                                        if toRowIndex >= fromRowIndex && toColIndex >= fromColIndex then
                                            new BoolMatrix(Array2D.init (int(toRowIndex - fromRowIndex + 1)) (int(toColIndex - fromColIndex + 1)) (fun i j -> rnd.NextDouble() < 0.5))
                                        else BoolMatrix.Empty
                                    setBoolMatrix app "a" (a.ToArray2D())
                                    app.Execute("v(fromRowIndex:toRowIndex, fromColIndex:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromRowIndex, Some toRowIndex, Some fromColIndex, Some toColIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice int, Some int, Some int vector``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) 
                                    let toRowIndex = fromRowIndex
                                    let fromColIndex = rnd.Next(v.GetLength(1)) 
                                    let toColIndex = rnd.Next(v.GetLength(1)) 
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1))
                                    setScalar app "toRowIndex" (float(toRowIndex + 1))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1))
                                    setScalar app "toColIndex" (float(toColIndex + 1))
                                    let a =
                                        if toRowIndex >= fromRowIndex && toColIndex >= fromColIndex then
                                            new BoolVector(Array.init (int(toColIndex - fromColIndex + 1)) (fun i -> rnd.NextDouble() < 0.5))
                                        else BoolVector.Empty
                                    setBoolVector app "a" (a.ToArray())
                                    app.Execute("v(fromRowIndex:toRowIndex, fromColIndex:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(fromRowIndex, Some fromColIndex, Some toColIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int, Some int, int vector``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) 
                                    let toRowIndex = rnd.Next(v.GetLength(0)) 
                                    let fromColIndex = rnd.Next(v.GetLength(1)) 
                                    let toColIndex = fromColIndex
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1))
                                    setScalar app "toRowIndex" (float(toRowIndex + 1))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1))
                                    setScalar app "toColIndex" (float(toColIndex + 1))
                                    let a =
                                        if toRowIndex >= fromRowIndex && toColIndex >= fromColIndex then
                                            new BoolVector(Array.init (int(toRowIndex - fromRowIndex + 1)) (fun i -> rnd.NextDouble() < 0.5))
                                        else BoolVector.Empty
                                    setBoolVector app "a" (a.ToArray())
                                    app.Execute("v(fromRowIndex:toRowIndex, fromColIndex:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromRowIndex, Some toRowIndex, fromColIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int, None Matrix``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromIndex = rnd.Next(v.Length) 
                                    let v = new BoolMatrix(v)
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    let a = new BoolVector(Array.init (v.Length - fromIndex) (fun i -> rnd.NextDouble() < 0.5))
                                    setBoolVector app "a" (a.ToArray())
                                    app.Execute("v(fromIndex:end) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    v.SetSlice(Some fromIndex, None, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int, None, Some int, None Matrix``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let fromRowIndex = rnd.Next(v.GetLength(0)) 
                                    let fromColIndex = rnd.Next(v.GetLength(1)) 
                                    setScalar app "fromRowIndex" (float(fromRowIndex + 1))
                                    setScalar app "fromColIndex" (float(fromColIndex + 1))
                                    let a =
                                        new BoolMatrix(Array2D.init (int(v.GetLength(0) - fromRowIndex)) (int(v.GetLength(1) - fromColIndex)) (fun i j -> rnd.NextDouble() < 0.5))
                                    setBoolMatrix app "a" (a.ToArray2D())
                                    app.Execute("v(fromRowIndex:end, fromColIndex:end) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(Some fromRowIndex, None, Some fromColIndex, None, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice None, Some int Matrix``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    let a =
                                        if toIndex >= 0 then
                                            new BoolVector(Array.init (toIndex + 1) (fun i -> rnd.NextDouble() < 0.5))
                                        else BoolVector.Empty
                                    setBoolVector app "a" (a.ToArray())
                                    app.Execute("v(1:toIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(None, Some toIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetSlice None, Some int, None, Some int Matrix``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let toRowIndex = rnd.Next(v.GetLength(0)) 
                                    let toColIndex = rnd.Next(v.GetLength(1)) 
                                    setScalar app "toRowIndex" (float(toRowIndex + 1))
                                    setScalar app "toColIndex" (float(toColIndex + 1))
                                    let a =
                                        if toRowIndex >= 0 && toColIndex >= 0 then
                                            new BoolMatrix(Array2D.init (int(toRowIndex + 1)) (int(toColIndex + 1)) (fun i j -> rnd.NextDouble() < 0.5))
                                        else BoolMatrix.Empty
                                    setBoolMatrix app "a" (a.ToArray2D())
                                    app.Execute("v(1:toRowIndex, 1:toColIndex) = a;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    v.SetSlice(None, Some toRowIndex, None, Some toColIndex, a)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``GetItem bool vector``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let b = Array.init v.Length (fun i -> rnd.NextDouble() < 0.5)
                                    setBoolVector app "b" b
                                    app.Execute("res = v(b);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolMatrix(v)
                                    let b = new BoolVector(b)
                                    v.[b].ToArray() = res       
                                    )

    [<Property>]
    let ``GetItem bool matrix``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let b = Array2D.init (v.GetLength(0))  (v.GetLength(1)) (fun i j -> rnd.NextDouble() < 0.5)
                                    setBoolMatrix app "b" b
                                    app.Execute("res = v(b);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolMatrix(v)
                                    let b = new BoolMatrix(b)
                                    v.[b].ToArray() = res       
                                    )

    [<Property>]
    let ``SetItem bool vector``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let b = Array.init v.Length (fun i -> rnd.NextDouble() < 0.5)
                                    let trueB = b |> Array.filter id
                                    let y = trueB |> Array.map (fun x -> rnd.NextDouble() < 0.5)
                                    setBoolVector app "b" b
                                    setBoolVector app "y" y
                                    app.Execute("v(b) = y;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    let b = new BoolVector(b)
                                    v.[b] <- new BoolVector(y)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetItem bool scalar vector``(v : bool[,]) (a : bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let b = Array.init v.Length (fun i -> rnd.NextDouble() < 0.5)
                                    let trueB = b |> Array.filter id
                                    let y = [|a|]
                                    setBoolVector app "b" b
                                    setBoolVector app "y" y
                                    app.Execute("v(b) = y;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    let b = new BoolVector(b)
                                    v.[b] <- new BoolVector(y)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetItem bool matrix``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let b = Array2D.init (v.GetLength(0))  (v.GetLength(1)) (fun i j -> rnd.NextDouble() < 0.5)
                                    let trueB = b |> array2DFilter id
                                    let y = trueB |> Array.map (fun x -> rnd.NextDouble() < 0.5)
                                    setBoolMatrix app "b" b
                                    setBoolVector app "y" y
                                    app.Execute("v(b) = y;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    let b = new BoolMatrix(b)
                                    v.[b] <- new BoolVector(y)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``SetItem bool scalar matrix``(v : bool[,]) (a : bool) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    setBoolMatrix app "v" v
                                    let b = Array2D.init (v.GetLength(0))  (v.GetLength(1)) (fun i j -> rnd.NextDouble() < 0.5)
                                    let trueB = b |> array2DFilter id
                                    let y = [|a|]
                                    setBoolMatrix app "b" b
                                    setBoolVector app "y" y
                                    app.Execute("v(b) = y;") |> ignore
                                    let res = getBoolMatrix app "v"
                                    let v = new BoolMatrix(v)
                                    let b = new BoolMatrix(b)
                                    v.[b] <- new BoolVector(y)
                                    v.ToArray2D() = res       
                                    )

    [<Property>]
    let ``View int64``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    let v = new BoolMatrix(v)
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    let length = rnd.Next(1, v.Length - int(fromIndex) + 1) |> int64
                                    let view = v.View(fromIndex, fromIndex + length - 1L)                    
                                    let y = new BoolVector(Array.init (int(length)) (fun i -> rnd.NextDouble() < 0.5))
                                    v.SetSlice(Some fromIndex, Some (fromIndex + length - 1L), y)
                                    view.ToArray() = v.[fromIndex..(fromIndex + length - 1L)].ToArray() && view.IsView
                                    )

    [<Property>]
    let ``ColView int64``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    let v = new BoolMatrix(v)
                                    let colIndex = rnd.Next(v.ColCount) |> int64
                                    let view = v.ColView(colIndex)                    
                                    let y = new BoolMatrix(v.RowCount, 1, Array.init v.RowCount (fun i -> rnd.NextDouble() < 0.5))
                                    v.SetSlice(None, None, Some colIndex, Some colIndex, y)
                                    view.ToArray() = v.[*, colIndex..colIndex].ColMajorDataVector.ToArray() && view.IsView
                                    )

    [<Property>]
    let ``View int``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    let v = new BoolMatrix(v)
                                    let fromIndex = rnd.Next(v.Length)
                                    let length = rnd.Next(1, v.Length - fromIndex + 1)
                                    let view = v.View(fromIndex, fromIndex + length - 1)                    
                                    let y = new BoolVector(Array.init length (fun i -> rnd.NextDouble() < 0.5))
                                    v.SetSlice(Some fromIndex, Some (fromIndex + length - 1), y)
                                    view.ToArray() = v.[fromIndex..(fromIndex + length - 1)].ToArray() && view.IsView
                                    )

    [<Property>]
    let ``ColView int``(v : bool[,]) =
        (v.LongLength > 0L) ==> lazy(
                                    let app = getApp()
                                    let v = new BoolMatrix(v)
                                    let colIndex = rnd.Next(v.ColCount) 
                                    let view = v.ColView(colIndex)                    
                                    let y = new BoolMatrix(v.RowCount, 1, Array.init v.RowCount (fun i -> rnd.NextDouble() < 0.5))
                                    v.SetSlice(None, None, Some colIndex, Some colIndex, y)
                                    view.ToArray() = v.[*, colIndex..colIndex].ColMajorDataVector.ToArray() && view.IsView
                                    )

