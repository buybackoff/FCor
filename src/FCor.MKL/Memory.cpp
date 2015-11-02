#include <stdlib.h>
#include "malloc.h"
#include <string.h>
#include <mkl_types.h>
#include "mkl.h"

#define OUTOFMEMORY -1

template<typename T>
int create_array(const MKL_INT length, T*& x)
{
	if (x == nullptr)
	{
		x = (T*)mkl_malloc(length*sizeof(T),64);
		if (x == nullptr)
		{
			return OUTOFMEMORY;
		}
	}
	return 0;
}

template<typename T>
int create_zero_array(const MKL_INT length, T*& x)
{
	if (x == nullptr)
	{
		x = (T*)mkl_calloc(length, sizeof(T), 64);
		if (x == nullptr)
		{
			return OUTOFMEMORY;
		}
	}
	return 0;
}

template<typename T>
void copy_array(const MKL_INT length, const T* x, T* y)
{
	memcpy(y, x, length * sizeof(T));
}

template<typename T>
void fill_array(const T a, const MKL_INT length, T* x)
{
	for (MKL_INT i = 0; i < length; i++)
	{
		x[i] = a;
	}
}

extern "C" __declspec(dllexport) void free_array(void* x)
{
	mkl_free(x);
}

extern "C" __declspec(dllexport) int d_create_array(const MKL_INT length, double*& x)
{
	return create_array(length, x); 
}

extern "C" __declspec(dllexport) int d_create_zero_array(const MKL_INT length, double*& x)
{
	return create_zero_array(length, x);
}



extern "C" __declspec(dllexport) int b_create_array(const MKL_INT length, bool*& x)
{
	return create_array(length, x); 
}

extern "C" __declspec(dllexport) int b_create_zero_array(const MKL_INT length, bool*& x)
{
	return create_zero_array(length, x);
}



extern "C" __declspec(dllexport) int i32_create_zero_array(const MKL_INT length, int*& x)
{
	return create_zero_array(length, x);
}

extern "C" __declspec(dllexport) int i32_create_array(const MKL_INT length, int*& x)
{
	return create_array(length, x);
}



extern "C" __declspec(dllexport) void d_copy_array(const MKL_INT length, const double* x, double* y)
{
	copy_array(length, x, y); 
}

extern "C" __declspec(dllexport) void b_copy_array(const MKL_INT length, const bool* x, bool* y)
{
	copy_array(length, x, y); 
}

extern "C" __declspec(dllexport) void i32_copy_array(const MKL_INT length, const int* x, int* y)
{
	copy_array(length, x, y);
}




extern "C" __declspec(dllexport) void d_fill_array(const double a, const MKL_INT length, double* x)
{
	fill_array(a, length, x); 
}

extern "C" __declspec(dllexport) void b_fill_array(const bool a, const MKL_INT length, bool* x)
{
	fill_array(a, length, x); 
}

extern "C" __declspec(dllexport) void i32_fill_array(const int a, const MKL_INT length, int* x)
{
	fill_array(a, length, x);
}




extern "C" __declspec(dllexport) void set_max_threads(const MKL_INT num_threads)
{
	mkl_set_num_threads(num_threads);
}

extern "C" __declspec(dllexport) void free_buffers()
{
	mkl_free_buffers();
}

extern "C" __declspec(dllexport) void thread_free_buffers()
{
	mkl_thread_free_buffers();
}

extern "C" __declspec(dllexport) void disable_fast_mm()
{
	mkl_disable_fast_mm();
}

extern "C" __declspec(dllexport) MKL_INT64 mem_stat()
{
	int allocatedBuffers = 0;
	return mkl_mem_stat(&allocatedBuffers);
}



