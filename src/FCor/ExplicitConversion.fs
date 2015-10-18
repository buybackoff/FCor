namespace FCor

module ExplicitConversion =
    open System
    open System.Collections.Generic

    let getMethod (t : Type) methName argType retType =
        let m = t.GetMethods() |> Array.filter (fun x -> x.Name = methName && x.ReturnType = retType && x.GetParameters().Length = 1 &&
                                                         x.GetParameters().[0].ParameterType = argType)
        if m.Length = 1 then Some m.[0]
        else None

    type ExplicitConverter<'T,'S>() =
        static let value : Option<'T -> 'S> = 
            let t = typeof<'T>
            let s = typeof<'S>
            seq{yield getMethod s  "op_Explicit" t s
                yield getMethod t  "op_Explicit" t s
                yield getMethod s  "op_Implicit" t s
                yield getMethod t  "op_Implicit" t s
               }
            |> Seq.tryFind (fun x -> x.IsSome)
            |> Option.map (fun mi ->
                    System.Delegate.CreateDelegate(typeof<System.Converter<'T,'S>>, mi.Value):?> System.Converter<'T,'S> |> FuncConvert.ToFSharpFunc)

        static member Value
            with get() = value

    type GenericConverter<'T,'S>() =
        static let mutable value : Option<'T -> 'S> = None

        static member Value
            with get() = match value with
                          | Some conv -> value
                          | None -> ExplicitConverter<'T,'S>.Value
            and set(v) = value <- v

    type GenericConverter private() =
        static do GenericConverter<byte, float>.Value <-  Some float
        static do GenericConverter<int, float>.Value <-  Some float
        static do GenericConverter<int16, float>.Value <-  Some float
        static do GenericConverter<int64, float>.Value <-  Some float
        static do GenericConverter<decimal, float>.Value <-  Some float
        static do GenericConverter<float32, float>.Value <-  Some float
        static do GenericConverter<float, float>.Value <-  Some float

        static do GenericConverter<byte, float32>.Value <-  Some float32
        static do GenericConverter<int, float32>.Value <-  Some float32
        static do GenericConverter<int16, float32>.Value <-  Some float32
        static do GenericConverter<int64, float32>.Value <-  Some float32
        static do GenericConverter<decimal, float32>.Value <-  Some float32
        static do GenericConverter<float32, float32>.Value <-  Some float32
        static do GenericConverter<float, float32>.Value <-  Some float32

        static do GenericConverter<byte, decimal>.Value <-  Some decimal
        static do GenericConverter<int, decimal>.Value <-  Some decimal
        static do GenericConverter<int16, decimal>.Value <-  Some decimal
        static do GenericConverter<int64, decimal>.Value <-  Some decimal
        static do GenericConverter<decimal, decimal>.Value <-  Some decimal
        static do GenericConverter<float32, decimal>.Value <-  Some decimal
        static do GenericConverter<float, decimal>.Value <-  Some decimal

        static do GenericConverter<byte, int64>.Value <-  Some int64
        static do GenericConverter<int, int64>.Value <-  Some int64
        static do GenericConverter<int16, int64>.Value <-  Some int64
        static do GenericConverter<int64, int64>.Value <-  Some int64
        static do GenericConverter<decimal, int64>.Value <-  Some int64
        static do GenericConverter<float32, int64>.Value <-  Some int64
        static do GenericConverter<float, int64>.Value <-  Some int64

        static do GenericConverter<byte, int>.Value <-  Some int
        static do GenericConverter<int, int>.Value <-  Some int
        static do GenericConverter<int16, int>.Value <-  Some int
        static do GenericConverter<int64, int>.Value <-  Some int
        static do GenericConverter<decimal, int>.Value <-  Some int
        static do GenericConverter<float32, int>.Value <-  Some int
        static do GenericConverter<float, int>.Value <-  Some int

        static do GenericConverter<byte, int16>.Value <-  Some int16
        static do GenericConverter<int, int16>.Value <-  Some int16
        static do GenericConverter<int16, int16>.Value <-  Some int16
        static do GenericConverter<int64, int16>.Value <-  Some int16
        static do GenericConverter<decimal, int16>.Value <-  Some int16
        static do GenericConverter<float32, int16>.Value <-  Some int16
        static do GenericConverter<float, int16>.Value <-  Some int16

        static do GenericConverter<byte, byte>.Value <-  Some byte
        static do GenericConverter<int, byte>.Value <-  Some byte
        static do GenericConverter<int16, byte>.Value <-  Some byte
        static do GenericConverter<int64, byte>.Value <-  Some byte
        static do GenericConverter<decimal, byte>.Value <-  Some byte
        static do GenericConverter<float32, byte>.Value <-  Some byte
        static do GenericConverter<float, byte>.Value <-  Some byte

        static let instance = new GenericConverter()

        static member Instance = instance

        member this.Convert<'T,'S>(x:'T) =
            match GenericConverter<'T,'S>.Value with
              | Some conv -> conv x
              | None -> raise (new InvalidOperationException("No explicit conversion found"))
 
    let inline (!!) (x : 'T when (^T or ^S) : (static member op_Explicit : ^T -> ^S)) : 'S = GenericConverter.Instance.Convert<'T,'S>(x)

    type T1orT2<'T1,'T2> =
        | T1of2 of 'T1
        | T2of2 of 'T2
        member __.T1
            with get() = match __ with
                            | T1of2(t1) -> t1
                            | _ -> raise (new InvalidOperationException())
        member __.T2
            with get() = match __ with
                            | T2of2(t2) -> t2
                            | _ -> raise (new InvalidOperationException())  
        static member op_Explicit(t1 : 'T1) : T1orT2<'T1,'T2> = T1of2 t1
        static member op_Explicit(t2 : 'T2) : T1orT2<'T1,'T2> = T2of2 t2                           
                               

    type T1orT2orT3orT4orT5<'T1,'T2,'T3,'T4,'T5> =
        | T1of5 of 'T1
        | T2of5 of 'T2
        | T3of5 of 'T3
        | T4of5 of 'T4
        | T5of5 of 'T5
        member __.T1
            with get() = match __ with
                            | T1of5(t1) -> t1
                            | _ -> raise (new InvalidOperationException())
        member __.T2
            with get() = match __ with
                            | T2of5(t2) -> t2
                            | _ -> raise (new InvalidOperationException())   
        member __.T3
            with get() = match __ with
                            | T3of5(t3) -> t3
                            | _ -> raise (new InvalidOperationException())     
        member __.T4
            with get() = match __ with
                            | T4of5(t4) -> t4
                            | _ -> raise (new InvalidOperationException())  
        member __.T5
            with get() = match __ with
                            | T5of5(t5) -> t5
                            | _ -> raise (new InvalidOperationException()) 

//        static member op_Explicit(t : T1orT2orT3orT4orT5<'T1,'T2,'T3,'T4,'T5>) =
//            match t with
//                | T1of5(t1) -> t1
//                | _ -> raise (new InvalidOperationException())
//        static member op_Explicit(t : T1orT2orT3orT4orT5<'T1,'T2,'T3,'T4,'T5>) =
//            match t with
//                | T2of5(t2) -> t2
//                | _ -> raise (new InvalidOperationException())   
//        static member op_Explicit(t : T1orT2orT3orT4orT5<'T1,'T2,'T3,'T4,'T5>) =
//            match t with
//                | T3of5(t3) -> t3
//                | _ -> raise (new InvalidOperationException())   
//        static member op_Explicit(t : T1orT2orT3orT4orT5<'T1,'T2,'T3,'T4,'T5>) =
//            match t with
//                | T4of5(t4) -> t4
//                | _ -> raise (new InvalidOperationException())
//        static member op_Explicit(t : T1orT2orT3orT4orT5<'T1,'T2,'T3,'T4,'T5>) =
//            match t with
//                | T5of5(t5) -> t5
//                | _ -> raise (new InvalidOperationException())
                                                                                 
        static member op_Explicit(t1 : 'T1) : T1orT2orT3orT4orT5<'T1,'T2,'T3,'T4,'T5> = T1of5 t1
        static member op_Explicit(t2 : 'T2) : T1orT2orT3orT4orT5<'T1,'T2,'T3,'T4,'T5> = T2of5 t2
        static member op_Explicit(t3 : 'T3) : T1orT2orT3orT4orT5<'T1,'T2,'T3,'T4,'T5> = T3of5 t3
        static member op_Explicit(t4 : 'T4) : T1orT2orT3orT4orT5<'T1,'T2,'T3,'T4,'T5> = T4of5 t4
        static member op_Explicit(t5 : 'T5) : T1orT2orT3orT4orT5<'T1,'T2,'T3,'T4,'T5> = T5of5 t5


