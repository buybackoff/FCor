#include <stdlib.h>
#include <stdbool.h>
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
#include "ComparisonCodes.h"

template<typename T>
inline bool Comp(T a, T b, int op)
{
	switch (op)
	{
		case Less: 
			return (a < b);
		case LessEq:
			return (a <= b);
		case Greater: 
			return (a > b);
		case GreaterEq:
			return (a >= b);
		case Eq: 
			return (a == b);
		case NotEq:
			return (a != b);
	}
}

template<typename T>
int arrays_are_equal(MKL_INT n, T* x, T* y)
{
	for (MKL_INT i = 0; i < n; i++)
	{
		if (x[i] != y[i])
		{
			return 0;
		}
	}
	return 1;
}

template<typename T>
void compare_arrays(MKL_INT nx, T* x, MKL_INT ny, T* y, int compCode, bool* result)
{
	if (nx == 1)
	{
		for (MKL_INT i = 0; i < ny; i++)
		{
			result[i] = Comp(x[0], y[i], compCode);
		}
	}
	else if (ny == 1)
	{
		for (MKL_INT i = 0; i < nx; i++)
		{
			result[i] = Comp(x[i], y[0], compCode);
		}
	}
	else
	{
		for (MKL_INT i = 0; i < nx; i++)
		{
			result[i] = Comp(x[i], y[i], compCode);
		}
	}
}

extern "C" __declspec(dllexport) int b_arrays_are_equal(MKL_INT n, bool* x, bool* y)
{
	return arrays_are_equal(n, x, y); 
}

extern "C" __declspec(dllexport) int d_arrays_are_equal(MKL_INT n, double* x, double* y)
{
	return arrays_are_equal(n, x, y); 
}

extern "C" __declspec(dllexport) int s_arrays_are_equal(MKL_INT n, float* x, float* y)
{
	return arrays_are_equal(n, x, y); 
}

extern "C" __declspec(dllexport) void b_compare_arrays(MKL_INT nx, bool* x, MKL_INT ny, bool* y, int compCode, bool* result)
{
    compare_arrays(nx, x, ny, y, compCode, result); 
	
}

extern "C" __declspec(dllexport) void d_compare_arrays(MKL_INT nx, double* x, MKL_INT ny, double* y, int compCode, bool* result)
{
	compare_arrays(nx, x, ny, y, compCode, result); 
}

extern "C" __declspec(dllexport) void s_compare_arrays(MKL_INT nx, float* x, MKL_INT ny, float* y, int compCode, bool* result)
{
	compare_arrays(nx, x, ny, y, compCode, result); 
}

