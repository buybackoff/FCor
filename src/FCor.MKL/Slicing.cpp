#include <stdlib.h>
#include <mkl_blas.h>
#include <mkl_types.h>
#include "mkl.h"
#include "mkl_vml_functions.h"
#include "mkl_lapacke.h"
#include <string.h>
#include "mkl_vsl_functions.h"
#include "mkl_vsl.h"
#include "mkl_trans.h"
#include <math.h>
#include <mkl_service.h>
#include "ErrorCodes.h"

template<typename T>
T get_item(MKL_INT i, T* x)
{
	return x[i];
}

template<typename T>
void set_item(MKL_INT i, T a, T* x)
{
	x[i] = a;
}

template<typename T>
int get_bool_slice(MKL_INT n, T* x, bool* b, T*& y, __int64& ny)
{
	MKL_INT trueCount = 0;
	for (MKL_INT i = 0; i < n; i++)
	{
		if (b[i])
		{
			trueCount++;
		}
	}
	ny = trueCount;
	if (trueCount > 0)
	{
		if (y == nullptr)
		{
			y = (T*)mkl_malloc(trueCount*sizeof(T),64);
			if (y == nullptr)
			{
				return OUTOFMEMORY;
			}
		}

		MKL_INT index = 0;
		for (MKL_INT i = 0; i < n; i++)
		{
			if (b[i])
			{
				y[index++] = x[i];
			}
		}
	}
	return 0;
}

template<typename T>
int set_bool_slice(MKL_INT n, T* x, bool* b, T* y, MKL_INT ny)
{
	if (ny == 1) 
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			if (b[i])
			{
				x[i] = y[0];
			}
		}
	}
	else
	{
		MKL_INT trueCount = 0;
		for (MKL_INT i = 0; i < n; i++)
		{
			if (b[i])
			{
				trueCount++;
			}
		}
		if (trueCount != ny)
		{
			return DATALENGTHMISMATCH;
		}
		MKL_INT index = 0;
		for (MKL_INT i = 0; i < n; i++)
		{
			if (b[i])
			{
				x[i] = y[index++];
			}
		}
	}
	return 0;
}

extern "C" __declspec(dllexport) double d_get_item(MKL_INT i, double* x)
{
	return get_item(i, x);
}

extern "C" __declspec(dllexport) void d_set_item(MKL_INT i, double a, double* x)
{
	set_item(i, a, x);
}

extern "C" __declspec(dllexport) int b_get_bool_slice(MKL_INT n, bool* x, bool* b, bool*& y, __int64& ny)
{
	return get_bool_slice(n, x, b, y, ny); 
}

extern "C" __declspec(dllexport) int d_get_bool_slice(MKL_INT n, double* x, bool* b, double*& y, __int64& ny)
{
	return get_bool_slice(n, x, b, y, ny); 
}

extern "C" __declspec(dllexport) int s_get_bool_slice(MKL_INT n, float* x, bool* b, float*& y, __int64& ny)
{
	return get_bool_slice(n, x, b, y, ny); 
}

extern "C" __declspec(dllexport) int b_set_bool_slice(MKL_INT n, bool* x, bool* b, bool* y, MKL_INT ny)
{
	return set_bool_slice(n, x, b, y, ny); 
}

extern "C" __declspec(dllexport) int d_set_bool_slice(MKL_INT n, double* x, bool* b, double* y, MKL_INT ny)
{
	return set_bool_slice(n, x, b, y, ny); 
}

extern "C" __declspec(dllexport) int s_set_bool_slice(MKL_INT n, float* x, bool* b, float* y, MKL_INT ny)
{
	return set_bool_slice(n, x, b, y, ny); 
}