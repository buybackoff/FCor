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
int resize_array(const MKL_INT length, T*& x)
{
	x = (T*)mkl_realloc(x, length*sizeof(T));
	if (x == nullptr)
	{
		return OUTOFMEMORY;
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

template<typename T1, typename T2>
void convert_array(const MKL_INT length, const T1* x, T2* y)
{
	for (MKL_INT i = 0; i < length; i++)
	{
		y[i] = (T2)x[i];
	}
}

template<typename T>
T get_item(const MKL_INT i, T* x)
{
	return x[i];
}

template<typename T>
void set_item(const MKL_INT i, T* x, const T a)
{
	x[i] = a;
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

extern "C" __declspec(dllexport) int d_resize_array(const MKL_INT length, double*& x)
{
	return resize_array(length, x);
}



extern "C" __declspec(dllexport) int s_create_array(const MKL_INT length, float*& x)
{
	return create_array(length, x);
}

extern "C" __declspec(dllexport) int s_create_zero_array(const MKL_INT length, float*& x)
{
	return create_zero_array(length, x);
}

extern "C" __declspec(dllexport) int s_resize_array(const MKL_INT length, float*& x)
{
	return resize_array(length, x);
}



extern "C" __declspec(dllexport) int b_create_array(const MKL_INT length, bool*& x)
{
	return create_array(length, x); 
}

extern "C" __declspec(dllexport) int b_create_zero_array(const MKL_INT length, bool*& x)
{
	return create_zero_array(length, x);
}

extern "C" __declspec(dllexport) int b_resize_array(const MKL_INT length, bool*& x)
{
	return resize_array(length, x);
}



extern "C" __declspec(dllexport) int ui8_create_array(const MKL_INT length, unsigned char*& x)
{
	return create_array(length, x);
}

extern "C" __declspec(dllexport) int ui8_create_zero_array(const MKL_INT length, unsigned char*& x)
{
	return create_zero_array(length, x);
}

extern "C" __declspec(dllexport) int ui8_resize_array(const MKL_INT length, unsigned char*& x)
{
	return resize_array(length, x);
}



extern "C" __declspec(dllexport) int i32_create_zero_array(const MKL_INT length, int*& x)
{
	return create_zero_array(length, x);
}

extern "C" __declspec(dllexport) int i32_create_array(const MKL_INT length, int*& x)
{
	return create_array(length, x);
}

extern "C" __declspec(dllexport) int i32_resize_array(const MKL_INT length, int*& x)
{
	return resize_array(length, x);
}


extern "C" __declspec(dllexport) int ui16_create_zero_array(const MKL_INT length, unsigned short*& x)
{
	return create_zero_array(length, x);
}

extern "C" __declspec(dllexport) int ui16_create_array(const MKL_INT length, unsigned short*& x)
{
	return create_array(length, x);
}

extern "C" __declspec(dllexport) int ui16_resize_array(const MKL_INT length, unsigned short*& x)
{
	return resize_array(length, x);
}



extern "C" __declspec(dllexport) void d_copy_array(const MKL_INT length, const double* x, double* y)
{
	copy_array(length, x, y); 
}

extern "C" __declspec(dllexport) void s_copy_array(const MKL_INT length, const float* x, float* y)
{
	copy_array(length, x, y);
}

extern "C" __declspec(dllexport) void b_copy_array(const MKL_INT length, const bool* x, bool* y)
{
	copy_array(length, x, y); 
}

extern "C" __declspec(dllexport) void ui8_copy_array(const MKL_INT length, const unsigned char* x, unsigned char* y)
{
	copy_array(length, x, y);
}

extern "C" __declspec(dllexport) void i32_copy_array(const MKL_INT length, const int* x, int* y)
{
	copy_array(length, x, y);
}

extern "C" __declspec(dllexport) void ui16_copy_array(const MKL_INT length, const unsigned short* x, unsigned short* y)
{
	copy_array(length, x, y);
}




extern "C" __declspec(dllexport) void d_fill_array(const double a, const MKL_INT length, double* x)
{
	fill_array(a, length, x); 
}

extern "C" __declspec(dllexport) void s_fill_array(const float a, const MKL_INT length, float* x)
{
	fill_array(a, length, x);
}

extern "C" __declspec(dllexport) void b_fill_array(const bool a, const MKL_INT length, bool* x)
{
	fill_array(a, length, x); 
}

extern "C" __declspec(dllexport) void ui8_fill_array(const unsigned char a, const MKL_INT length, unsigned char* x)
{
	fill_array(a, length, x);
}

extern "C" __declspec(dllexport) void i32_fill_array(const int a, const MKL_INT length, int* x)
{
	fill_array(a, length, x);
}

extern "C" __declspec(dllexport) void ui16_fill_array(const unsigned short a, const MKL_INT length, unsigned short* x)
{
	fill_array(a, length, x);
}


extern "C" __declspec(dllexport) void ui8_ui16_convert_array(const MKL_INT length, const unsigned char* x, unsigned short* y)
{
	convert_array(length, x, y);
}

extern "C" __declspec(dllexport) void s_d_convert_array(const MKL_INT length, const float* x, double* y)
{
	convert_array(length, x, y);
}




extern "C" __declspec(dllexport) double d_get_item(const MKL_INT i, double* x)
{
	return x[i];
}

extern "C" __declspec(dllexport) void d_set_item(const MKL_INT i, double* x, const double a)
{
	x[i] = a;
}


extern "C" __declspec(dllexport) float s_get_item(const MKL_INT i, float* x)
{
	return x[i];
}

extern "C" __declspec(dllexport) void s_set_item(const MKL_INT i, float* x, const float a)
{
	x[i] = a;
}


extern "C" __declspec(dllexport) bool b_get_item(const MKL_INT i, bool* x)
{
	return x[i];
}

extern "C" __declspec(dllexport) void b_set_item(const MKL_INT i, bool* x, const bool a)
{
	x[i] = a;
}


extern "C" __declspec(dllexport) int i32_get_item(const MKL_INT i, int* x)
{
	return x[i];
}

extern "C" __declspec(dllexport) void i32_set_item(const MKL_INT i, int* x, const int a)
{
	x[i] = a;
}

extern "C" __declspec(dllexport) unsigned char ui8_get_item(const MKL_INT i, unsigned char* x)
{
	return x[i];
}

extern "C" __declspec(dllexport) void ui8_set_item(const MKL_INT i, unsigned char* x, const unsigned char a)
{
	x[i] = a;
}

extern "C" __declspec(dllexport) unsigned short ui16_get_item(const MKL_INT i, unsigned short* x)
{
	return x[i];
}

extern "C" __declspec(dllexport) void ui16_set_item(const MKL_INT i, unsigned short* x, const unsigned short a)
{
	x[i] = a;
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



