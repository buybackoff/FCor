namespace FCor
#nowarn "9"

open System
open System.Reflection
open System.IO
open System.IO.Compression
open System.Runtime.InteropServices
open System.ComponentModel
open Microsoft.FSharp.NativeInterop
open System.Security
open System.Collections.Generic

type internal CompCode =
    | Less = 0
    | LessEq = 1
    | Greater = 2
    | GreaterEq = 3
    | Eq = 4
    | NotEq = 5

type FloatPtr = nativeptr<float>

type BoolPtr = nativeptr<bool>

type Int32Ptr = nativeptr<int>

type internal MklFunctions() =

    [<Literal>]
    static let dllName = "FCor.MKL.dll"

    [<Literal>]
    static let x86Zip = "FCor.MKL.x86.zip"

    [<Literal>]
    static let x64Zip = "FCor.MKL.x64.zip"
    
    static do
        try
            let BUFFERSIZE = 1000000
            let decompress (data : byte[]) =
                let buffer = Array.zeroCreate<byte> BUFFERSIZE
                let returnVal = new List<byte>()
                use memoryStream = new MemoryStream(data)
                use deflateStream = new DeflateStream(memoryStream, CompressionMode.Decompress)
                let mutable count = deflateStream.Read(buffer, 0, BUFFERSIZE)
                while count > 0 do
                    if count <> BUFFERSIZE then
                        let tmpBuffer = Array.zeroCreate<byte> count
                        Array.Copy(buffer, tmpBuffer, count)
                        returnVal.AddRange(tmpBuffer)
                    else
                        returnVal.AddRange(buffer)
                    count <- deflateStream.Read(buffer, 0, BUFFERSIZE)
                returnVal.ToArray()

            let tempFolder = Path.GetTempPath()
            let asm = Assembly.GetExecutingAssembly()
            let version = asm.GetName().Version
            let mklFolder =
                if Environment.Is64BitProcess then
                    Path.Combine(tempFolder, "FCor", "x64", sprintf "v%d_%d_%d" version.Major version.Minor version.Build)
                else
                    Path.Combine(tempFolder, "FCor", "x86", sprintf "v%d_%d_%d" version.Major version.Minor version.Build)
            let mklPath = Path.Combine(mklFolder, dllName)
            let zipFile =
                if Environment.Is64BitProcess then
                    x64Zip
                else
                    x86Zip
            if not <| Directory.Exists(mklFolder) then Directory.CreateDirectory(mklFolder) |> ignore
            if not <| File.Exists(mklPath) then
                if asm.GetManifestResourceNames() |> Array.exists (fun x -> x = zipFile) then
                    use stream = asm.GetManifestResourceStream(zipFile)
                    let n = stream.Length |> int
                    let buffer = Array.zeroCreate<byte> n
                    stream.Read(buffer, 0, n) |> ignore
                    let bytes = decompress buffer
                    File.WriteAllBytes(mklPath, bytes) 

            if File.Exists(mklPath) then
                MklFunctions.LoadLibrary_(mklPath) |> ignore

        with ex -> System.Windows.Forms.MessageBox.Show(ex.Message) |> ignore

    static let validateRetCode(code : int) =
        match code with
            | 0 -> ()
            | -1 -> raise (new OutOfMemoryException())
            | -2 -> raise (new ArgumentException("Matrix is not positive definite"))
            | -3 -> raise (new ArgumentException("MKL arg error"))
            | -4 -> raise (new ArgumentException("Matrix is singular"))
            | -5 -> raise (new ArgumentException("Matrix is not full rank"))
            | -6 -> raise (new ArgumentException("Algorithm did not converge"))
            | -7 -> raise (new ArgumentException("VSL arg error"))
            | -9 -> raise (new ArgumentException("Vector length mismatch"))
            | _ -> raise (new ArgumentException("Error in native FCor")) 


    [<DllImport("kernel32")>]
    static extern IntPtr LoadLibrary(string lpFileName)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_create_array(IntPtr length, FloatPtr* array)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_create_zero_array(IntPtr length, FloatPtr* array)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int b_create_array(IntPtr length, BoolPtr* array)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int b_create_zero_array(IntPtr length, BoolPtr* array)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int i32_create_array(IntPtr length, Int32Ptr* array)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int i32_create_zero_array(IntPtr length, Int32Ptr* array)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_fill_array(float a, IntPtr length, float* array)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void b_fill_array(bool a, IntPtr length, bool* array)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void i32_fill_array(int a, IntPtr length, int* array)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_copy_array(IntPtr length, float* fromArray, float* toArray)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void b_copy_array(IntPtr length, bool* fromArray, bool* toArray)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void i32_copy_array(IntPtr length, int* fromArray, int* toArray)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void free_array(IntPtr array)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern float d_get_item(IntPtr i, float* array)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_set_item(IntPtr i, float a, float* array)


    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void set_max_threads(IntPtr num_threads)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void free_buffers()

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void thread_free_buffers()

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void disable_fast_mm()

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int64 mem_stat()


    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int b_get_bool_slice(IntPtr length, bool* x, bool* b, BoolPtr* y, int64* ny)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_get_bool_slice(IntPtr length, float* x, bool* b, FloatPtr* y, int64* ny)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int b_set_bool_slice(IntPtr length, bool* x, bool* b, bool* y, IntPtr ny)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_set_bool_slice(IntPtr length, float* x, bool* b, float* y, IntPtr ny)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int b_arrays_are_equal(IntPtr length, bool* x, bool* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_arrays_are_equal(IntPtr length, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_arrays_are_equal(IntPtr length, float32* x, float32* y)




    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void b_compare_arrays(IntPtr nx, bool* x, IntPtr ny, bool* y, int compCode, bool* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_compare_arrays(IntPtr nx, float* x, IntPtr ny, float* y, int compCode, bool* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_compare_arrays(IntPtr nx, float32* x, IntPtr ny, float32* y, int compCode, bool* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void b_min_arrays(IntPtr nx, bool* x, IntPtr ny, bool* y, bool* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_min_arrays(IntPtr nx, float* x, IntPtr ny, float* y, float* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_min_arrays(IntPtr nx, float32* x, IntPtr ny, float32* y, float32* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void b_max_arrays(IntPtr nx, bool* x, IntPtr ny, bool* y, bool* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_max_arrays(IntPtr nx, float* x, IntPtr ny, float* y, float* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_max_arrays(IntPtr nx, float32* x, IntPtr ny, float32* y, float32* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void b_iif_arrays(IntPtr nx, bool* x, IntPtr ny, bool* y, IntPtr nb, bool* b, bool* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_iif_arrays(IntPtr nx, float* x, IntPtr ny, float* y, IntPtr nb, bool* b, float* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_iif_arrays(IntPtr nx, float32* x, IntPtr ny, float32* y, IntPtr nb, bool* b, float32* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void b_and_arrays(IntPtr nx, bool* x, IntPtr ny, bool* y, bool* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void b_or_arrays(IntPtr nx, bool* x, IntPtr ny, bool* y, bool* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void b_not_array(IntPtr n, bool* x, bool* result)



    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_scalar_mul_array(float a, IntPtr n, float* x, float* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_scalar_mul_array(float32 a, IntPtr n, float32* x, float32* result)


    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_scalar_add_array(float a, IntPtr n, float* x, float* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_scalar_add_array(float32 a, IntPtr n, float32* x, float32* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_scalar_sub_array(float a, IntPtr n, float* x, float* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_scalar_sub_array(float32 a, IntPtr n, float32* x, float32* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_array_sub_scalar(float a, IntPtr n, float* x, float* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_array_sub_scalar(float32 a, IntPtr n, float32* x, float32* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_array_div_scalar(float a, IntPtr n, float* x, float* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_array_div_scalar(float32 a, IntPtr n, float32* x, float32* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_scalar_div_array(float a, IntPtr n, float* x, float* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_scalar_div_array(float32 a, IntPtr n, float32* x, float32* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_scalar_pow_array(float a, IntPtr n, float* x, float* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_scalar_pow_array(float32 a, IntPtr n, float32* x, float32* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_array_pow_scalar(float a, IntPtr n, float* x, float* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_array_pow_scalar(float32 a, IntPtr n, float32* x, float32* result)


    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern float d_inner_product(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern float32 s_inner_product(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_array_add_array(IntPtr n, float* x, float* y, float* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_array_add_array(IntPtr n, float32* x, float32* y, float32* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_array_sub_array(IntPtr n, float* x, float* y, float* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_array_sub_array(IntPtr n, float32* x, float32* y, float32* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_array_mul_array(IntPtr n, float* x, float* y, float* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_array_mul_array(IntPtr n, float32* x, float32* y, float32* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_array_div_array(IntPtr n, float* x, float* y, float* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_array_div_array(IntPtr n, float32* x, float32* y, float32* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_array_pow_array(IntPtr n, float* x, float* y, float* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_array_pow_array(IntPtr n, float32* x, float32* y, float32* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_array_axpby_array(IntPtr n, float* x, float a, float* y, float b, float* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_array_axpby_array(IntPtr n, float32* x, float32 a, float32* y, float32 b, float32* result)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_sqr_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_sqr_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_abs_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_abs_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_inv_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_inv_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_sqrt_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_sqrt_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_invsqrt_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_invsqrt_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_cbrt_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_cbrt_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_invcbrt_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_invcbrt_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_pow2o3_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_pow2o3_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_pow3o2_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_pow3o2_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_minus_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_minus_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_powx_array(float a, IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_powx_array(float32 a, IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_exp_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_exp_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_expm1_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_expm1_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_ln_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_ln_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_log10_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_log10_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_log1p_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_log1p_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_cos_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_cos_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_sin_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_sin_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_tan_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_tan_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_acos_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_acos_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_asin_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_asin_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_atan_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_atan_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_cosh_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_cosh_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_sinh_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_sinh_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_tanh_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_tanh_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_acosh_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_acosh_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_asinh_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_asinh_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_atanh_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_atanh_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_erf_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_erf_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_erfc_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_erfc_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_cdfnorm_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_cdfnorm_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_erfinv_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_erfinv_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_erfcinv_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_erfcinv_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_cdfnorminv_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_cdfnorminv_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_floor_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_floor_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_ceil_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_ceil_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_trunc_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_trunc_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_round_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_round_array(IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_lngam_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_digam_array(IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_cdfchi_array(IntPtr n, float* x, float* y)




    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_min_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, float* x, float* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_min_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, float32* x, float32* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_max_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, float* x, float* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_max_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, float32* x, float32* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_mean_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, float* x, float* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_mean_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, float32* x, float32* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_variance_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, float* x, float* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_variance_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, float32* x, float32* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_skewness_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, float* x, float* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_skewness_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, float32* x, float32* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_kurtosis_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, float* x, float* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_kurtosis_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, float32* x, float32* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_sum_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, float* x, float* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_sum_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, float32* x, float32* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void b_any_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, bool* x, bool* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void b_all_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, bool* x, bool* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_prod_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, float* x, float* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_prod_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, float32* x, float32* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_cumsum_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, float* x, float* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_cumsum_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, float32* x, float32* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_cumprod_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, float* x, float* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_cumprod_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, float32* x, float32* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_cov_matrix(IntPtr varCount, IntPtr obsCount, float* x, float* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_cov_matrix(IntPtr varCount, IntPtr obsCount, float32* x, float32* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_corr_matrix(IntPtr varCount, IntPtr obsCount, float* x, float* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_corr_matrix(IntPtr varCount, IntPtr obsCount, float32* x, float32* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_quantiles_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, IntPtr qCount, float* x, float* q, float* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_quantiles_matrix(bool byRows, IntPtr varCount, IntPtr obsCount, IntPtr qCount, float32* x, float32* q, float32* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern IntPtr create_rng(int brng, uint32[] seed, int n, int subStream, int quasiDims)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern IntPtr copy_rng(IntPtr streamPtr)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int skipahead_rng(IntPtr streamPtr, int64 nskip)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int leapfrog_rng(IntPtr streamPtr, int k, int nstreams)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void delete_rng(IntPtr streamPtr)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_uniform_rnd(IntPtr streamPtr, IntPtr n, float a, float b, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_uniform_rnd(IntPtr streamPtr, IntPtr n, float32 a, float32 b, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_norm_rnd(IntPtr streamPtr, IntPtr n, float mean, float sigma, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_norm_rnd(IntPtr streamPtr, IntPtr n, float32 mean, float32 sigma, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_expon_rnd(IntPtr streamPtr, IntPtr n, float alpha, float beta, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_expon_rnd(IntPtr streamPtr, IntPtr n, float32 alpha, float32 beta, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_laplace_rnd(IntPtr streamPtr, IntPtr n, float alpha, float beta, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_laplace_rnd(IntPtr streamPtr, IntPtr n, float32 alpha, float32 beta, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_weibull_rnd(IntPtr streamPtr, IntPtr n, float a, float alpha, float beta, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_weibull_rnd(IntPtr streamPtr, IntPtr n, float32 a, float32 alpha, float32 beta, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_cauchy_rnd(IntPtr streamPtr, IntPtr n, float alpha, float beta, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_cauchy_rnd(IntPtr streamPtr, IntPtr n, float32 alpha, float32 beta, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_rayleigh_rnd(IntPtr streamPtr, IntPtr n, float alpha, float beta, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_rayleigh_rnd(IntPtr streamPtr, IntPtr n, float32 alpha, float32 beta, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_lognorm_rnd(IntPtr streamPtr, IntPtr n, float a, float sigma, float b, float beta, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_lognorm_rnd(IntPtr streamPtr, IntPtr n, float32 a, float32 sigma, float32 b, float32 beta, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_gumbel_rnd(IntPtr streamPtr, IntPtr n, float alpha, float beta, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_gumbel_rnd(IntPtr streamPtr, IntPtr n, float32 alpha, float32 beta, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_gamma_rnd(IntPtr streamPtr, IntPtr n, float a, float alpha, float beta, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_gamma_rnd(IntPtr streamPtr, IntPtr n, float32 a, float32 alpha, float32 beta, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_beta_rnd(IntPtr streamPtr, IntPtr n, float p, float q, float a, float beta, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_beta_rnd(IntPtr streamPtr, IntPtr n, float32 p, float32 q, float32 a, float32 beta, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_bern_rnd(IntPtr streamPtr, IntPtr n, float p, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_bern_rnd(IntPtr streamPtr, IntPtr n, float32 p, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_geom_rnd(IntPtr streamPtr, IntPtr n, float p, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_geom_rnd(IntPtr streamPtr, IntPtr n, float32 p, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_binom_rnd(IntPtr streamPtr, IntPtr n, int ntrial, float p, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_binom_rnd(IntPtr streamPtr, IntPtr n, int ntrial, float32 p, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_hypergeom_rnd(IntPtr streamPtr, IntPtr n, int l, int s, int m, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_hypergeom_rnd(IntPtr streamPtr, IntPtr n, int l, int s, int m, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_poisson_rnd(IntPtr streamPtr, IntPtr n, float lambda, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_poisson_rnd(IntPtr streamPtr, IntPtr n, float32 lambda, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_negbinom_rnd(IntPtr streamPtr, IntPtr n, float a, float p, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_negbinom_rnd(IntPtr streamPtr, IntPtr n, float32 a, float32 p, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_transpose_in_place(IntPtr rows, IntPtr cols, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_transpose_in_place(IntPtr rows, IntPtr cols, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_transpose(IntPtr rows, IntPtr cols, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_transpose(IntPtr rows, IntPtr cols, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_get_diag(IntPtr rowCount, IntPtr offset, IntPtr n, float* x, float* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_get_diag(IntPtr rowCount, IntPtr offset, IntPtr n, float32* x, float32* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_set_diag(IntPtr n, IntPtr offset, float* x, float* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_set_diag(IntPtr n, IntPtr offset, float32* x, float32* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_get_upper_tri(IntPtr offset, IntPtr rows, IntPtr cols, float* x, float* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_get_upper_tri(IntPtr offset, IntPtr rows, IntPtr cols, float32* x, float32* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_get_lower_tri(IntPtr offset, IntPtr rows, IntPtr cols, float* x, float* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_get_lower_tri(IntPtr offset, IntPtr rows, IntPtr cols, float32* x, float32* res)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_identity(IntPtr rows, IntPtr cols, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_identity(IntPtr rows, IntPtr cols, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void b_identity(IntPtr rows, IntPtr cols, bool* x)



    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_cholesky_factor(IntPtr n, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_cholesky_factor(IntPtr n, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_cholesky_inverse(IntPtr n, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_cholesky_inverse(IntPtr n, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_cholesky_solve(IntPtr n, IntPtr nrhs, float* A, float* B)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_cholesky_solve(IntPtr n, IntPtr nrhs, float32* A, float32* B)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_lu_factor(IntPtr m, IntPtr n, float* L, float* U, int[] pivot)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_lu_factor(IntPtr m, IntPtr n, float32* L, float32* U, int[] pivot)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_lu_inverse(IntPtr n, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_lu_inverse(IntPtr n, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_lu_solve(IntPtr n, IntPtr nrhs, float* A, float* B)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_lu_solve(IntPtr n, IntPtr nrhs, float32* A, float32* B)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_qr_factor(IntPtr m, IntPtr n, float* x, float* Q, float* R)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_qr_factor(IntPtr m, IntPtr n, float32* x, float32* Q, float32* R)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_qr_solve(IntPtr m, IntPtr n, IntPtr nrhs, float* A, float* B, float* x, int* rank, float cond)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_qr_solve(IntPtr m, IntPtr n, IntPtr nrhs, float32* A, float32* B, float32* x, int* rank, float32 cond)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_qr_solve_full(IntPtr m, IntPtr n, IntPtr nrhs, float* A, float* B, float* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_qr_solve_full(IntPtr m, IntPtr n, IntPtr nrhs, float32* A, float32* B, float32* x)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_svd_factor(IntPtr m, IntPtr n, float* x, float* U, float* S, float* Vt)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_svd_factor(IntPtr m, IntPtr n, float32* x, float32* U, float32* S, float32* Vt)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_svd_solve(IntPtr m, IntPtr n, IntPtr nrhs, float* A, float* B, float* x, int* rank, float cond)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_svd_solve(IntPtr m, IntPtr n, IntPtr nrhs, float32* A, float32* B, float32* x, int* rank, float32 cond)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_svd_values(IntPtr m, IntPtr n, float* x, float* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_svd_values(IntPtr m, IntPtr n, float32* x, float32* y)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int d_eigen_factor(IntPtr n, float* Z, float* D)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern int s_eigen_factor(IntPtr n, float32* Z, float32* D)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void d_multiply_matrices(float* x, float* y, float* z, IntPtr n, IntPtr m, IntPtr k, bool trans)

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    static extern void s_multiply_matrices(float32* x, float32* y, float32* z, IntPtr n, IntPtr m, IntPtr k, bool trans)





    static member LoadLibrary_(dllPath) = LoadLibrary(dllPath)

    static member D_Create_Array(length : int64, array) =
        d_create_array(new IntPtr(length), array) |> validateRetCode

    static member D_Create_Zero_Array(length : int64, array) =
        d_create_zero_array(new IntPtr(length), array) |> validateRetCode

    static member B_Create_Array(length : int64, array) =
        b_create_array(new IntPtr(length), array) |> validateRetCode

    static member B_Create_Zero_Array(length : int64, array) =
        b_create_zero_array(new IntPtr(length), array) |> validateRetCode

    static member I32_Create_Array(length : int64, array) =
        i32_create_array(new IntPtr(length), array) |> validateRetCode

    static member I32_Create_Zero_Array(length : int64, array) =
        i32_create_zero_array(new IntPtr(length), array) |> validateRetCode

    static member D_Fill_Array(a, length : int64, array : nativeptr<float>) =
       d_fill_array(a, new IntPtr(length), array) 

    static member B_Fill_Array(a, length : int64, array) =
       b_fill_array(a, new IntPtr(length), array)

    static member I32_Fill_Array(a, length : int64, array) =
       i32_fill_array(a, new IntPtr(length), array)

    static member D_Copy_Array(length : int64, fromArray, toArray) =
       d_copy_array(new IntPtr(length), fromArray, toArray)

    static member B_Copy_Array(length : int64, fromArray, toArray) =
       b_copy_array(new IntPtr(length), fromArray, toArray)

    static member I32_Copy_Array(length : int64, fromArray, toArray) =
       i32_copy_array(new IntPtr(length), fromArray, toArray)

    static member D_Get_Item(index : int64, arr) =
        d_get_item(new IntPtr(index), arr)

    static member D_Set_Item(index : int64, a, arr) =
        d_set_item(new IntPtr(index), a, arr)


    static member Free_Array(x : IntPtr) = free_array(x)

    static member Set_Max_Threads(n : int) = set_max_threads(new IntPtr(n))

    static member Free_Buffers() = free_buffers()

    static member Thread_Free_Buffers() = thread_free_buffers()

    static member Disable_Fast_MM() = disable_fast_mm()

    static member Mem_Stat() = mem_stat()


    static member B_Get_Bool_Slice(length : int64, fromArray, predicate, resArray, resLength) =
        b_get_bool_slice(new IntPtr(length), fromArray, predicate, resArray, resLength)
        |> validateRetCode

    static member D_Get_Bool_Slice(length : int64, fromArray, predicate, resArray, resLength) =
        d_get_bool_slice(new IntPtr(length), fromArray, predicate, resArray, resLength)
        |> validateRetCode

    static member B_Set_Bool_Slice(length : int64, x, predicate, y, yLength : int64) =
        b_set_bool_slice(new IntPtr(length), x, predicate, y, new IntPtr(yLength))
        |> validateRetCode

    static member D_Set_Bool_Slice(length : int64, x, predicate, y, yLength : int64) =
        d_set_bool_slice(new IntPtr(length), x, predicate, y, new IntPtr(yLength))
        |> validateRetCode

    static member B_Arrays_Are_Equal(length : int64, x, y) =
        b_arrays_are_equal(new IntPtr(length), x, y) = 1

    static member D_Arrays_Are_Equal(length : int64, x, y) =
        d_arrays_are_equal(new IntPtr(length), x, y) = 1

    static member S_Arrays_Are_Equal(length : int64, x, y) =
        s_arrays_are_equal(new IntPtr(length), x, y) = 1


    static member B_Arrays_GreaterThan(lengthX : int64, x, lengthY : int64, y, result) =
        b_compare_arrays(new IntPtr(lengthX), x, new IntPtr(lengthY), y, int(CompCode.Greater), result)

    static member D_Arrays_GreaterThan(lengthX : int64, x, lengthY : int64, y, result) =
        d_compare_arrays(new IntPtr(lengthX), x, new IntPtr(lengthY), y, int(CompCode.Greater), result)
                         
    static member B_Arrays_GreaterEqual(lengthX : int64, x, lengthY : int64, y, result) =
        b_compare_arrays(new IntPtr(lengthX), x, new IntPtr(lengthY), y, int(CompCode.GreaterEq), result)

    static member D_Arrays_GreaterEqual(lengthX : int64, x, lengthY : int64, y, result) =
        d_compare_arrays(new IntPtr(lengthX), x, new IntPtr(lengthY), y, int(CompCode.GreaterEq), result)

    static member B_Arrays_LessThan(lengthX : int64, x, lengthY : int64, y, result) =
        b_compare_arrays(new IntPtr(lengthX), x, new IntPtr(lengthY), y, int(CompCode.Less), result)

    static member D_Arrays_LessThan(lengthX : int64, x, lengthY : int64, y, result) =
        d_compare_arrays(new IntPtr(lengthX), x, new IntPtr(lengthY), y, int(CompCode.Less), result)
                         
    static member B_Arrays_LessEqual(lengthX : int64, x, lengthY : int64, y, result) =
        b_compare_arrays(new IntPtr(lengthX), x, new IntPtr(lengthY), y, int(CompCode.LessEq), result)

    static member D_Arrays_LessEqual(lengthX : int64, x, lengthY : int64, y, result) =
        d_compare_arrays(new IntPtr(lengthX), x, new IntPtr(lengthY), y, int(CompCode.LessEq), result)

    static member B_Arrays_EqualElementwise(lengthX : int64, x, lengthY : int64, y, result) =
        b_compare_arrays(new IntPtr(lengthX), x, new IntPtr(lengthY), y, int(CompCode.Eq), result)

    static member D_Arrays_EqualElementwise(lengthX : int64, x, lengthY : int64, y, result) =
        d_compare_arrays(new IntPtr(lengthX), x, new IntPtr(lengthY), y, int(CompCode.Eq), result)
                         
    static member B_Arrays_NotEqualElementwise(lengthX : int64, x, lengthY : int64, y, result) =
        b_compare_arrays(new IntPtr(lengthX), x, new IntPtr(lengthY), y, int(CompCode.NotEq), result)

    static member D_Arrays_NotEqualElementwise(lengthX : int64, x, lengthY : int64, y, result) =
        d_compare_arrays(new IntPtr(lengthX), x, new IntPtr(lengthY), y, int(CompCode.NotEq), result)

    static member B_Min_Arrays(nx : int64, x, ny : int64, y, result) =
        b_min_arrays(new IntPtr(nx), x, new IntPtr(ny), y, result)

    static member D_Min_Arrays(nx : int64, x, ny : int64, y, result) =
        d_min_arrays(new IntPtr(nx), x, new IntPtr(ny), y, result)

    static member B_Max_Arrays(nx : int64, x, ny : int64, y, result) =
        b_max_arrays(new IntPtr(nx), x, new IntPtr(ny), y, result)

    static member D_Max_Arrays(nx : int64, x, ny : int64, y, result) =
        d_max_arrays(new IntPtr(nx), x, new IntPtr(ny), y, result)

    static member B_IIf_Arrays(nx : int64, x, ny : int64, y, nb : int64, b, result) =
        b_iif_arrays(new IntPtr(nx), x, new IntPtr(ny), y, new IntPtr(nb), b, result)

    static member D_IIf_Arrays(nx : int64, x, ny : int64, y, nb : int64, b, result) =
        d_iif_arrays(new IntPtr(nx), x, new IntPtr(ny), y, new IntPtr(nb), b, result)

    static member S_IIf_Arrays(nx : int64, x, ny : int64, y, nb : int64, b, result) =
        s_iif_arrays(new IntPtr(nx), x, new IntPtr(ny), y, new IntPtr(nb), b, result)

    static member B_And_Arrays(nx : int64, x, ny : int64, y, result) =
        b_and_arrays(new IntPtr(nx), x, new IntPtr(ny), y, result)

    static member B_Or_Arrays(nx : int64, x, ny : int64, y, result) =
        b_or_arrays(new IntPtr(nx), x, new IntPtr(ny), y, result)

    static member B_Not_Array(n : int64, x, result) =
        b_not_array(new IntPtr(n), x, result)

    static member D_Scalar_Mul_Array(a, n : int64, x, result) =
        d_scalar_mul_array(a, new IntPtr(n), x, result)

    static member S_Scalar_Mul_Array(a, n : int64, x, result) =
        s_scalar_mul_array(a, new IntPtr(n), x, result)

    static member D_Scalar_Add_Array(a, n : int64, x, result) =
        d_scalar_add_array(a, new IntPtr(n), x, result)

    static member S_Scalar_Add_Array(a, n : int64, x, result) =
        s_scalar_add_array(a, new IntPtr(n), x, result)

    static member D_Scalar_Sub_Array(a, n : int64, x, result) =
        d_scalar_sub_array(a, new IntPtr(n), x, result)

    static member S_Scalar_Sub_Array(a, n : int64, x, result) =
        s_scalar_sub_array(a, new IntPtr(n), x, result)

    static member D_Array_Sub_Scalar(a, n : int64, x, result) =
        d_array_sub_scalar(a, new IntPtr(n), x, result)

    static member S_Array_Sub_Scalar(a, n : int64, x, result) =
        s_array_sub_scalar(a, new IntPtr(n), x, result)

    static member D_Array_Div_Scalar(a, n : int64, x, result) =
        d_array_div_scalar(a, new IntPtr(n), x, result)

    static member S_Array_Div_Scalar(a, n : int64, x, result) =
        s_array_div_scalar(a, new IntPtr(n), x, result)

    static member D_Scalar_Div_Array(a, n : int64, x, result) =
        d_scalar_div_array(a, new IntPtr(n), x, result)

    static member S_Scalar_Div_Array(a, n : int64, x, result) =
        s_scalar_div_array(a, new IntPtr(n), x, result)

    static member D_Scalar_Pow_Array(a, n : int64, x, result) =
        d_scalar_pow_array(a, new IntPtr(n), x, result)

    static member S_Scalar_Pow_Array(a, n : int64, x, result) =
        s_scalar_pow_array(a, new IntPtr(n), x, result)

    static member D_Array_Pow_scalar(a, n : int64, x, result) =
        d_array_pow_scalar(a, new IntPtr(n), x, result)

    static member S_Array_Pow_Scalar(a, n : int64, x, result) =
        s_array_pow_scalar(a, new IntPtr(n), x, result)

    static member D_Inner_Product(n : int64, x, y) =
        d_inner_product(new IntPtr(n), x, y)

    static member S_Inner_Product(n : int64, x, y) =
        s_inner_product(new IntPtr(n), x, y)

    static member D_Array_Add_Array(n : int64, x, y, result) =
        d_array_add_array(new IntPtr(n), x, y, result)

    static member S_Array_Add_Array(n : int64, x, y, result) =
        s_array_add_array(new IntPtr(n), x, y, result)

    static member D_Array_Sub_Array(n : int64, x, y, result) =
        d_array_sub_array(new IntPtr(n), x, y, result)

    static member S_Array_Sub_Array(n : int64, x, y, result) =
        s_array_sub_array(new IntPtr(n), x, y, result)

    static member D_Array_Mul_Array(n : int64, x, y, result) =
        d_array_mul_array(new IntPtr(n), x, y, result)

    static member S_Array_Mul_Array(n : int64, x, y, result) =
        s_array_mul_array(new IntPtr(n), x, y, result)

    static member D_Array_Div_Array(n : int64, x, y, result) =
        d_array_div_array(new IntPtr(n), x, y, result)

    static member S_Array_Div_Array(n : int64, x, y, result) =
        s_array_div_array(new IntPtr(n), x, y, result)

    static member D_Array_Pow_Array(n : int64, x, y, result) =
        d_array_pow_array(new IntPtr(n), x, y, result)

    static member S_Array_Pow_Array(n : int64, x, y, result) =
        s_array_pow_array(new IntPtr(n), x, y, result)

    static member D_Array_Axpby_Array(n : int64, x, a, y, b, result) =
        d_array_axpby_array(new IntPtr(n), x, a, y, b, result)

    static member S_Array_Axpby_Array(n : int64, x, a, y, b, result) =
        s_array_axpby_array(new IntPtr(n), x, a, y, b, result)

    static member D_Sqr_Array(n : int64, x, y) =
        d_sqr_array(new IntPtr(n), x, y)

    static member S_Sqr_Array(n : int64, x, y) =
        s_sqr_array(new IntPtr(n), x, y)

    static member D_Abs_Array(n : int64, x, y) =
        d_abs_array(new IntPtr(n), x, y)

    static member S_Abs_Array(n : int64, x, y) =
        s_abs_array(new IntPtr(n), x, y)

    static member D_Inv_Array(n : int64, x, y) =
        d_inv_array(new IntPtr(n), x, y)

    static member S_Inv_Array(n : int64, x, y) =
        s_inv_array(new IntPtr(n), x, y)

    static member D_Sqrt_Array(n : int64, x, y) =
        d_sqrt_array(new IntPtr(n), x, y)

    static member S_Sqrt_Array(n : int64, x, y) =
        s_sqrt_array(new IntPtr(n), x, y)

    static member D_InvSqrt_Array(n : int64, x, y) =
        d_invsqrt_array(new IntPtr(n), x, y)

    static member S_InvSqrt_Array(n : int64, x, y) =
        s_invsqrt_array(new IntPtr(n), x, y)

    static member D_Cbrt_Array(n : int64, x, y) =
        d_cbrt_array(new IntPtr(n), x, y)

    static member S_Cbrt_Array(n : int64, x, y) =
        s_cbrt_array(new IntPtr(n), x, y)

    static member D_InvCbrt_Array(n : int64, x, y) =
        d_invcbrt_array(new IntPtr(n), x, y)

    static member S_InvCbrt_Array(n : int64, x, y) =
        s_invcbrt_array(new IntPtr(n), x, y)

    static member D_Pow2o3_Array(n : int64, x, y) =
        d_pow2o3_array(new IntPtr(n), x, y)

    static member S_Pow2o3_Array(n : int64, x, y) =
        s_pow2o3_array(new IntPtr(n), x, y)

    static member D_Pow3o2_Array(n : int64, x, y) =
        d_pow3o2_array(new IntPtr(n), x, y)

    static member S_Pow3o2_Array(n : int64, x, y) =
        s_pow3o2_array(new IntPtr(n), x, y)

    static member D_Minus_Array(n : int64, x, y) =
        d_minus_array(new IntPtr(n), x, y)

    static member S_Minus_Array(n : int64, x, y) =
        s_minus_array(new IntPtr(n), x, y)

    static member D_Powx_Array(a, n : int64, x, y) =
        d_powx_array(a, new IntPtr(n), x, y)

    static member S_Powx_Array(a, n : int64, x, y) =
        s_powx_array(a, new IntPtr(n), x, y)

    static member D_Exp_Array(n : int64, x, y) =
        d_exp_array(new IntPtr(n), x, y)

    static member S_Exp_Array(n : int64, x, y) =
        s_exp_array(new IntPtr(n), x, y)

    static member D_Expm1_Array(n : int64, x, y) =
        d_expm1_array(new IntPtr(n), x, y)

    static member S_Expm1_Array(n : int64, x, y) =
        s_expm1_array(new IntPtr(n), x, y)

    static member D_Ln_Array(n : int64, x, y) =
        d_ln_array(new IntPtr(n), x, y)

    static member S_Ln_Array(n : int64, x, y) =
        s_ln_array(new IntPtr(n), x, y)

    static member D_Log10_Array(n : int64, x, y) =
        d_log10_array(new IntPtr(n), x, y)

    static member S_Log10_Array(n : int64, x, y) =
        s_log10_array(new IntPtr(n), x, y)

    static member D_Log1p_Array(n : int64, x, y) =
        d_log1p_array(new IntPtr(n), x, y)

    static member S_Log1p_Array(n : int64, x, y) =
        s_log1p_array(new IntPtr(n), x, y)

    static member D_Cos_Array(n : int64, x, y) =
        d_cos_array(new IntPtr(n), x, y)

    static member S_Cos_Array(n : int64, x, y) =
        s_cos_array(new IntPtr(n), x, y)

    static member D_Sin_Array(n : int64, x, y) =
        d_sin_array(new IntPtr(n), x, y)

    static member S_Sin_Array(n : int64, x, y) =
        s_sin_array(new IntPtr(n), x, y)

    static member D_Tan_Array(n : int64, x, y) =
        d_tan_array(new IntPtr(n), x, y)

    static member S_Tan_Array(n : int64, x, y) =
        s_tan_array(new IntPtr(n), x, y)

    static member D_ACos_Array(n : int64, x, y) =
        d_acos_array(new IntPtr(n), x, y)

    static member S_ACos_Array(n : int64, x, y) =
        s_acos_array(new IntPtr(n), x, y)

    static member D_ASin_Array(n : int64, x, y) =
        d_asin_array(new IntPtr(n), x, y)

    static member S_ASin_Array(n : int64, x, y) =
        s_asin_array(new IntPtr(n), x, y)

    static member D_ATan_Array(n : int64, x, y) =
        d_atan_array(new IntPtr(n), x, y)

    static member S_ATan_Array(n : int64, x, y) =
        s_atan_array(new IntPtr(n), x, y)

    static member D_Cosh_Array(n : int64, x, y) =
        d_cosh_array(new IntPtr(n), x, y)

    static member S_Cosh_Array(n : int64, x, y) =
        s_cosh_array(new IntPtr(n), x, y)

    static member D_Sinh_Array(n : int64, x, y) =
        d_sinh_array(new IntPtr(n), x, y)

    static member S_Sinh_Array(n : int64, x, y) =
        s_sinh_array(new IntPtr(n), x, y)

    static member D_Tanh_Array(n : int64, x, y) =
        d_tanh_array(new IntPtr(n), x, y)

    static member S_Tanh_Array(n : int64, x, y) =
        s_tanh_array(new IntPtr(n), x, y)


    static member D_ACosh_Array(n : int64, x, y) =
        d_acosh_array(new IntPtr(n), x, y)

    static member S_ACosh_Array(n : int64, x, y) =
        s_acosh_array(new IntPtr(n), x, y)

    static member D_ASinh_Array(n : int64, x, y) =
        d_asinh_array(new IntPtr(n), x, y)

    static member S_ASinh_Array(n : int64, x, y) =
        s_asinh_array(new IntPtr(n), x, y)

    static member D_ATanh_Array(n : int64, x, y) =
        d_atanh_array(new IntPtr(n), x, y)

    static member S_ATanh_Array(n : int64, x, y) =
        s_atanh_array(new IntPtr(n), x, y)

    static member D_Erf_Array(n : int64, x, y) =
        d_erf_array(new IntPtr(n), x, y)

    static member S_Erf_Array(n : int64, x, y) =
        s_erf_array(new IntPtr(n), x, y)

    static member D_Erfc_Array(n : int64, x, y) =
        d_erfc_array(new IntPtr(n), x, y)

    static member S_Erfc_Array(n : int64, x, y) =
        s_erfc_array(new IntPtr(n), x, y)

    static member D_CdfNorm_Array(n : int64, x, y) =
        d_cdfnorm_array(new IntPtr(n), x, y)

    static member S_CdfNorm_Array(n : int64, x, y) =
        s_cdfnorm_array(new IntPtr(n), x, y)

    static member D_Erfinv_Array(n : int64, x, y) =
        d_erfinv_array(new IntPtr(n), x, y)

    static member S_Erfinv_Array(n : int64, x, y) =
        s_erfinv_array(new IntPtr(n), x, y)

    static member D_Erfcinv_Array(n : int64, x, y) =
        d_erfcinv_array(new IntPtr(n), x, y)

    static member S_Erfcinv_Array(n : int64, x, y) =
        s_erfcinv_array(new IntPtr(n), x, y)

    static member D_CdfNormInv_Array(n : int64, x, y) =
        d_cdfnorminv_array(new IntPtr(n), x, y)

    static member S_CdfNormInv_Array(n : int64, x, y) =
        s_cdfnorminv_array(new IntPtr(n), x, y)

    static member D_Floor_Array(n : int64, x, y) =
        d_floor_array(new IntPtr(n), x, y)

    static member S_Floor_Array(n : int64, x, y) =
        s_floor_array(new IntPtr(n), x, y)

    static member D_Ceil_Array(n : int64, x, y) =
        d_ceil_array(new IntPtr(n), x, y)

    static member S_Ceil_Array(n : int64, x, y) =
        s_ceil_array(new IntPtr(n), x, y)

    static member D_Trunc_Array(n : int64, x, y) =
        d_trunc_array(new IntPtr(n), x, y)

    static member S_Trunc_Array(n : int64, x, y) =
        s_trunc_array(new IntPtr(n), x, y)

    static member D_Round_Array(n : int64, x, y) =
        d_round_array(new IntPtr(n), x, y)

    static member S_Round_Array(n : int64, x, y) =
        s_round_array(new IntPtr(n), x, y)

    static member D_Lngam_Array(n : int64, x, y) =
        d_lngam_array(new IntPtr(n), x, y)

    static member D_Digam_Array(n : int64, x, y) =
        d_digam_array(new IntPtr(n), x, y)

    static member D_Cdfchi_Array(n : int64, x, y) =
        d_cdfchi_array(new IntPtr(n), x, y)



    static member D_Min_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        d_min_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res) |> validateRetCode

    static member S_Min_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        s_min_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res) |> validateRetCode

    static member D_Max_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        d_max_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res) |> validateRetCode

    static member S_Max_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        s_max_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res) |> validateRetCode

    static member D_Mean_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        d_mean_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res) |> validateRetCode

    static member S_Mean_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        s_mean_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res) |> validateRetCode

    static member D_Variance_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        d_variance_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res) |> validateRetCode

    static member S_Variance_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        s_variance_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res) |> validateRetCode

    static member D_Skewness_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        d_skewness_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res) |> validateRetCode

    static member S_Skewness_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        s_skewness_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res) |> validateRetCode

    static member D_Kurtosis_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        d_kurtosis_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res) |> validateRetCode

    static member S_Kurtosis_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        s_kurtosis_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res) |> validateRetCode

    static member D_Sum_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        d_sum_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res) |> validateRetCode

    static member S_Sum_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        s_sum_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res) |> validateRetCode

    static member B_Any_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        b_any_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res)

    static member B_All_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        b_all_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res)

    static member D_Prod_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        d_prod_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res) 

    static member S_Prod_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        s_prod_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res) 

    static member D_CumSum_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        d_cumsum_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res) 

    static member S_CumSum_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        s_cumsum_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res) 

    static member D_CumProd_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        d_cumprod_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res) 

    static member S_CumProd_Matrix(byRows, varCount : int64, obsCount : int64, x, res) =
        s_cumprod_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), x, res) 

    static member D_Cov_Matrix(varCount : int64, obsCount : int64, x, res) =
        d_cov_matrix(new IntPtr(varCount), new IntPtr(obsCount), x, res) |> validateRetCode

    static member S_Cov_Matrix(varCount : int64, obsCount : int64, x, res) =
        s_cov_matrix(new IntPtr(varCount), new IntPtr(obsCount), x, res) |> validateRetCode 

    static member D_Corr_Matrix(varCount : int64, obsCount : int64, x, res) =
        d_corr_matrix(new IntPtr(varCount), new IntPtr(obsCount), x, res) |> validateRetCode

    static member S_Corr_Matrix(varCount : int64, obsCount : int64, x, res) =
        s_corr_matrix(new IntPtr(varCount), new IntPtr(obsCount), x, res) |> validateRetCode

    static member D_Quantiles_Matrix(byRows, varCount : int64, obsCount : int64, qCount : int64, x, q, res) =
        d_quantiles_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), new IntPtr(qCount), x, q, res) |> validateRetCode

    static member S_Quantiles_Matrix(byRows, varCount : int64, obsCount : int64, qCount : int64, x, q, res) =
        s_quantiles_matrix(byRows, new IntPtr(varCount), new IntPtr(obsCount), new IntPtr(qCount), x, q, res) |> validateRetCode

    static member Create_Rng(brng, seed, subStream, quasiDims) =
        let res = create_rng(brng, seed, seed.Length, subStream, quasiDims)
        if res = IntPtr.Zero then
            raise (new ArgumentException())
        else res

    static member Delete_Rng(streamPtr) =
        delete_rng(streamPtr)

    static member Copy_Rng(streamPtr) =
        copy_rng(streamPtr)

    static member Skipahead_Rng(streamPtr, nskip) =
        skipahead_rng(streamPtr, nskip) |> validateRetCode  

    static member Leapfrog_Rng(streamPtr, k, nstreams) =
        leapfrog_rng(streamPtr, k, nstreams) |> validateRetCode  




    static member D_Uniform_Rnd(streamPtr, n : int64, a, b, x) =
        d_uniform_rnd(streamPtr, new IntPtr(n), a, b, x) |> validateRetCode

    static member S_Uniform_Rnd(streamPtr, n : int64, a, b, x) =
        s_uniform_rnd(streamPtr, new IntPtr(n), a, b, x) |> validateRetCode

    static member D_Norm_Rnd(streamPtr, n : int64, mean, sigma, x) =
        d_norm_rnd(streamPtr, new IntPtr(n), mean, sigma, x) |> validateRetCode

    static member S_Norm_Rnd(streamPtr, n : int64, mean, sigma, x) =
        s_norm_rnd(streamPtr, new IntPtr(n), mean, sigma, x) |> validateRetCode

    static member D_Expon_Rnd(streamPtr, n : int64, alpha, beta, x) =
        d_expon_rnd(streamPtr, new IntPtr(n), alpha, beta, x) |> validateRetCode

    static member S_Expon_Rnd(streamPtr, n : int64, alpha, beta, x) =
        s_expon_rnd(streamPtr, new IntPtr(n), alpha, beta, x) |> validateRetCode

    static member D_Laplace_Rnd(streamPtr, n : int64, alpha, beta, x) =
        d_laplace_rnd(streamPtr, new IntPtr(n), alpha, beta, x) |> validateRetCode

    static member S_Laplace_Rnd(streamPtr, n : int64, alpha, beta, x) =
        s_laplace_rnd(streamPtr, new IntPtr(n), alpha, beta, x) |> validateRetCode

    static member D_Weibull_Rnd(streamPtr, n : int64, a, alpha, beta, x) =
        d_weibull_rnd(streamPtr, new IntPtr(n), a, alpha, beta, x) |> validateRetCode

    static member S_Weibull_Rnd(streamPtr, n : int64, a, alpha, beta, x) =
        s_weibull_rnd(streamPtr, new IntPtr(n), a, alpha, beta, x) |> validateRetCode

    static member D_Cauchy_Rnd(streamPtr, n : int64, alpha, beta, x) =
        d_cauchy_rnd(streamPtr, new IntPtr(n), alpha, beta, x) |> validateRetCode

    static member S_Cauchy_Rnd(streamPtr, n : int64, alpha, beta, x) =
        s_cauchy_rnd(streamPtr, new IntPtr(n), alpha, beta, x) |> validateRetCode

    static member D_Rayleigh_Rnd(streamPtr, n : int64, alpha, beta, x) =
        d_rayleigh_rnd(streamPtr, new IntPtr(n), alpha, beta, x) |> validateRetCode

    static member S_Rayleigh_Rnd(streamPtr, n : int64, alpha, beta, x) =
        s_rayleigh_rnd(streamPtr, new IntPtr(n), alpha, beta, x) |> validateRetCode

    static member D_Lognorm_Rnd(streamPtr, n : int64, a, sigma, b, beta, x) =
        d_lognorm_rnd(streamPtr, new IntPtr(n),  a, sigma, b, beta, x) |> validateRetCode

    static member S_Lognorm_Rnd(streamPtr, n : int64, a, sigma, b, beta, x) =
        s_lognorm_rnd(streamPtr, new IntPtr(n),  a, sigma, b, beta, x) |> validateRetCode

    static member D_Gumbel_Rnd(streamPtr, n : int64, alpha, beta, x) =
        d_gumbel_rnd(streamPtr, new IntPtr(n), alpha, beta, x) |> validateRetCode

    static member S_Gumbel_Rnd(streamPtr, n : int64, alpha, beta, x) =
        s_gumbel_rnd(streamPtr, new IntPtr(n), alpha, beta, x) |> validateRetCode

    static member D_Gamma_Rnd(streamPtr, n : int64, a, alpha, beta, x) =
        d_gamma_rnd(streamPtr, new IntPtr(n), a, alpha, beta, x) |> validateRetCode

    static member S_Gamma_Rnd(streamPtr, n : int64, a, alpha, beta, x) =
        s_gamma_rnd(streamPtr, new IntPtr(n), a, alpha, beta, x) |> validateRetCode

    static member D_Beta_Rnd(streamPtr, n : int64, p, q, a, beta, x) =
        d_beta_rnd(streamPtr, new IntPtr(n), p, q, a, beta, x) |> validateRetCode

    static member S_Beta_Rnd(streamPtr, n : int64, p, q, a, beta, x) =
        s_beta_rnd(streamPtr, new IntPtr(n), p, q, a, beta, x) |> validateRetCode

    static member D_Bern_Rnd(streamPtr, n : int64, p, x) =
        d_bern_rnd(streamPtr, new IntPtr(n), p, x) |> validateRetCode

    static member S_Bern_Rnd(streamPtr, n : int64, p, x) =
        s_bern_rnd(streamPtr, new IntPtr(n), p, x) |> validateRetCode

    static member D_Geom_Rnd(streamPtr, n : int64, p, x) =
        d_geom_rnd(streamPtr, new IntPtr(n), p, x) |> validateRetCode

    static member S_Geom_Rnd(streamPtr, n : int64, p, x) =
        s_geom_rnd(streamPtr, new IntPtr(n), p, x) |> validateRetCode

    static member D_Binom_Rnd(streamPtr, n : int64, ntrial, p, x) =
        d_binom_rnd(streamPtr, new IntPtr(n), ntrial, p, x) |> validateRetCode

    static member S_Binom_Rnd(streamPtr, n : int64, ntrial, p, x) =
        s_binom_rnd(streamPtr, new IntPtr(n), ntrial, p, x) |> validateRetCode

    static member D_Hypergeom_Rnd(streamPtr, n : int64, l, s, m, x) =
        d_hypergeom_rnd(streamPtr, new IntPtr(n), l, s, m, x) |> validateRetCode

    static member S_Hypergeom_Rnd(streamPtr, n : int64, l, s, m, x) =
        s_hypergeom_rnd(streamPtr, new IntPtr(n), l, s, m, x) |> validateRetCode

    static member D_Poisson_Rnd(streamPtr, n : int64, lambda, x) =
        d_poisson_rnd(streamPtr, new IntPtr(n), lambda, x) |> validateRetCode

    static member S_Poisson_Rnd(streamPtr, n : int64, lambda, x) =
        s_poisson_rnd(streamPtr, new IntPtr(n), lambda, x) |> validateRetCode

    static member D_Negbinom_Rnd(streamPtr, n : int64, a, p, x) =
        d_negbinom_rnd(streamPtr, new IntPtr(n), a, p, x) |> validateRetCode

    static member S_Negbinom_Rnd(streamPtr, n : int64, a, p, x) =
        s_negbinom_rnd(streamPtr, new IntPtr(n), a, p, x) |> validateRetCode

    static member D_Transpose_In_Place(rows : int64, cols : int64, x) =
        d_transpose_in_place(new IntPtr(rows), new IntPtr(cols), x)

    static member S_Transpose_In_Place(rows : int64, cols : int64, x) =
        s_transpose_in_place(new IntPtr(rows), new IntPtr(cols), x)

    static member D_Transpose(rows : int64, cols : int64, x, y) =
        d_transpose(new IntPtr(rows), new IntPtr(cols), x, y)

    static member S_Transpose(rows : int64, cols : int64, x, y) =
        s_transpose(new IntPtr(rows), new IntPtr(cols), x, y)

    static member D_Get_Diag(rowCount : int64, offset : int64, n : int64, x, res) =
        d_get_diag(new IntPtr(rowCount), new IntPtr(offset), new IntPtr(n), x, res) 

    static member S_Get_Diag(rowCount : int64, offset : int64, n : int64, x, res) =
        s_get_diag(new IntPtr(rowCount), new IntPtr(offset), new IntPtr(n), x, res) 

    static member D_Set_Diag(n : int64, offset : int64, x, res) =
        d_set_diag(new IntPtr(n), new IntPtr(offset), x, res) 

    static member S_Set_Diag(n : int64, offset : int64, x, res) =
        s_set_diag(new IntPtr(n), new IntPtr(offset), x, res) 

    static member D_Get_Upper_Tri(offset : int64, rows : int64, cols : int64, x, res) =
        d_get_upper_tri(new IntPtr(offset), new IntPtr(rows), new IntPtr(cols), x, res)

    static member S_Get_Upper_Tri(offset : int64, rows : int64, cols : int64, x, res) =
        s_get_upper_tri(new IntPtr(offset), new IntPtr(rows), new IntPtr(cols), x, res)

    static member D_Get_Lower_Tri(offset : int64, rows : int64, cols : int64, x, res) =
        d_get_lower_tri(new IntPtr(offset), new IntPtr(rows), new IntPtr(cols), x, res)

    static member S_Get_Lower_Tri(offset : int64, rows : int64, cols : int64, x, res) =
        s_get_lower_tri(new IntPtr(offset), new IntPtr(rows), new IntPtr(cols), x, res)

    static member D_Identity(rows : int64, cols : int64, x) =
        d_identity(new IntPtr(rows), new IntPtr(cols), x)

    static member S_Identity(rows : int64, cols : int64, x) =
        s_identity(new IntPtr(rows), new IntPtr(cols), x)

    static member B_Identity(rows : int64, cols : int64, x) =
        b_identity(new IntPtr(rows), new IntPtr(cols), x)




    static member D_Cholesky_Factor(n : int64, x) =
        d_cholesky_factor(new IntPtr(n), x) |> validateRetCode

    static member S_Cholesky_Factor(n : int64, x) =
        s_cholesky_factor(new IntPtr(n), x) |> validateRetCode

    static member D_Cholesky_Inverse(n : int64, x) =
        d_cholesky_inverse(new IntPtr(n), x) |> validateRetCode

    static member S_Cholesky_Inverse(n : int64, x) =
        s_cholesky_inverse(new IntPtr(n), x) |> validateRetCode

    static member D_Cholesky_Solve(n : int64, nrhs : int64, A, B) =
        d_cholesky_solve(new IntPtr(n), new IntPtr(nrhs), A, B) |> validateRetCode

    static member S_Cholesky_Solve(n : int64, nrhs : int64, A, B) =
        s_cholesky_solve(new IntPtr(n), new IntPtr(nrhs), A, B) |> validateRetCode

    static member D_Lu_Factor(m : int64, n : int64, L, U, pivot : int[]) =
        d_lu_factor(new IntPtr(m), new IntPtr(n), L, U, pivot) |> validateRetCode

    static member S_Lu_Factor(m : int64, n : int64, L, U, pivot : int[]) =
        s_lu_factor(new IntPtr(m), new IntPtr(n), L, U, pivot) |> validateRetCode

    static member D_Lu_Inverse(n : int64, x) =
        d_lu_inverse(new IntPtr(n), x) |> validateRetCode

    static member S_Lu_Inverse(n : int64, x) =
        s_lu_inverse(new IntPtr(n), x) |> validateRetCode

    static member D_Lu_Solve(n : int64, nrhs : int64, A, B) =
        d_lu_solve(new IntPtr(n), new IntPtr(nrhs), A, B) |> validateRetCode

    static member S_Lu_Solve(n : int64, nrhs : int64, A, B) =
        s_lu_solve(new IntPtr(n), new IntPtr(nrhs), A, B) |> validateRetCode

    static member D_Qr_Factor(m : int64, n : int64, x, Q, R) =
        d_qr_factor(new IntPtr(m), new IntPtr(n), x, Q, R) |> validateRetCode

    static member S_Qr_Factor(m : int64, n : int64, x, Q, R) =
        s_qr_factor(new IntPtr(m), new IntPtr(n), x, Q, R) |> validateRetCode

    static member D_Qr_Solve(m : int64, n : int64, nrhs : int64, A, B, x, rank, cond) =
        d_qr_solve(new IntPtr(m), new IntPtr(n), new IntPtr(nrhs), A, B, x, rank, cond) |> validateRetCode

    static member S_Qr_Solve(m : int64, n : int64, nrhs : int64, A, B, x, rank, cond) =
        s_qr_solve(new IntPtr(m), new IntPtr(n), new IntPtr(nrhs), A, B, x, rank, cond) |> validateRetCode

    static member D_Qr_Solve_Full(m : int64, n : int64, nrhs : int64, A, B, x) =
        d_qr_solve_full(new IntPtr(m), new IntPtr(n), new IntPtr(nrhs), A, B, x) |> validateRetCode

    static member S_Qr_Solve_Full(m : int64, n : int64, nrhs : int64, A, B, x) =
        s_qr_solve_full(new IntPtr(m), new IntPtr(n), new IntPtr(nrhs), A, B, x) |> validateRetCode

    static member D_Svd_Factor(m : int64, n : int64, x, U, S, Vt) =
        d_svd_factor(new IntPtr(m), new IntPtr(n), x, U, S, Vt) |> validateRetCode

    static member S_Svd_Factor(m : int64, n : int64, x, U, S, Vt) =
        s_svd_factor(new IntPtr(m), new IntPtr(n), x, U, S, Vt) |> validateRetCode

    static member D_Svd_Solve(m : int64, n : int64, nrhs : int64, A, B, x, rank, cond) =
        d_svd_solve(new IntPtr(m), new IntPtr(n), new IntPtr(nrhs), A, B, x, rank, cond) |> validateRetCode

    static member S_Svd_Solve(m : int64, n : int64, nrhs : int64, A, B, x, rank, cond) =
        s_svd_solve(new IntPtr(m), new IntPtr(n), new IntPtr(nrhs), A, B, x, rank, cond) |> validateRetCode

    static member D_Svd_Values(m : int64, n : int64, x, y) =
        d_svd_values(new IntPtr(m), new IntPtr(n), x, y) |> validateRetCode

    static member S_Svd_Values(m : int64, n : int64, x, y) =
        s_svd_values(new IntPtr(m), new IntPtr(n), x, y) |> validateRetCode

    static member D_Eigen_Factor(n : int64, Z, D) =
        d_eigen_factor(new IntPtr(n), Z, D) |> validateRetCode

    static member S_Eigen_Factor(n : int64, Z, D) =
        s_eigen_factor(new IntPtr(n), Z, D) |> validateRetCode

    static member D_Multiply_Matrices(x, y, z, n : int64, m : int64, k :  int64, trans) =
        d_multiply_matrices(x, y, z, new IntPtr(n), new IntPtr(m), new IntPtr(k), trans)

    static member S_Multiply_Matrices(x, y, z, n : int64, m : int64, k :  int64, trans) =
        s_multiply_matrices(x, y, z, new IntPtr(n), new IntPtr(m), new IntPtr(k), trans)


