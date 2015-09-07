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
//#include <math.h>
#include <cmath>
#include <mkl_service.h>


#define max(a,b)    (((a) > (b)) ? (a) : (b))
#define min(a,b)    (((a) < (b)) ? (a) : (b))


template<typename T>
void minus_array(MKL_INT n, T* x, T* result)
{
	for (MKL_INT i = 0; i < n; i++)
	{
		result[i] = -x[i];
	}
}

template<typename T>
void scalar_mul_array(T a, MKL_INT n, T* x, T* result)
{
	for (MKL_INT i = 0; i < n; i++)
	{
		result[i] = x[i] * a;
	}
}

template<typename T>
void scalar_add_array(T a, MKL_INT n, T* x, T* result)
{
	for (MKL_INT i = 0; i < n; i++)
	{
		result[i] = x[i] + a;
	}
}

template<typename T>
void scalar_sub_array(T a, MKL_INT n, T* x, T* result)
{
	for (MKL_INT i = 0; i < n; i++)
	{
		result[i] = a - x[i];
	}
}

template<typename T>
void array_sub_scalar(T a, MKL_INT n, T* x, T* result)
{
	for (MKL_INT i = 0; i < n; i++)
	{
		result[i] = x[i] - a;
	}
}

template<typename T>
void scalar_div_array(T a, MKL_INT n, T* x, T* result)
{
	for (MKL_INT i = 0; i < n; i++)
	{
		result[i] = a / x[i];
	}
}

template<typename T>
void array_div_scalar(T a, MKL_INT n, T* x, T* result)
{
	for (MKL_INT i = 0; i < n; i++)
	{
		result[i] = x[i] / a;
	}
}

template<typename T>
void scalar_pow_array(T a, MKL_INT n, T* x, T* result)
{
	for (MKL_INT i = 0; i < n; i++)
	{
		result[i] = pow(a, x[i]);
	}
}

template<typename T>
void array_pow_scalar(T a, MKL_INT n, T* x, T* result)
{
	for (MKL_INT i = 0; i < n; i++)
	{
		result[i] = pow(x[i], a);
	}
}

template<typename T>
void min_arrays(MKL_INT nx, T* x, MKL_INT ny, T* y, T* result)
{
	if (nx == 1)
	{
		if (isnan(x[0]))
		{
			for (MKL_INT i = 0; i < ny; i++)
			{
				result[i] = nan("");
			}
		}
		else
		{
			for (MKL_INT i = 0; i < ny; i++)
			{
				if (isnan(y[i]))
				{
					result[i] = nan("");
				}
				else
				{
					result[i] = fmin(x[0], y[i]);
				}
			}
		}

	}
	else if (ny == 1)
	{
		if (isnan(y[0]))
		{
			for (MKL_INT i = 0; i < nx; i++)
			{
				result[i] = nan("");
			}
		}
		else
		{
			for (MKL_INT i = 0; i < nx; i++)
			{
				if (isnan(x[i]))
				{
					result[i] = nan("");
				}
				else
				{
					result[i] = fmin(x[i], y[0]);
				}
			}
		}
	}
	else
	{
		for (MKL_INT i = 0; i < nx; i++)
		{
			if (isnan(x[i]) || isnan(y[i]))
			{
				result[i] = nan("");
			}
			else
			{
				result[i] = fmin(x[i], y[i]);
			}
		}
	}
}

template<typename T>
void max_arrays(MKL_INT nx, T* x, MKL_INT ny, T* y, T* result)
{
	if (nx == 1)
	{
		if (isnan(x[0]))
		{
			for (MKL_INT i = 0; i < ny; i++)
			{
				result[i] = nan("");
			}
		}
		else
		{
			for (MKL_INT i = 0; i < ny; i++)
			{
				if (isnan(y[i]))
				{
					result[i] = nan("");
				}
				else
				{
					result[i] = fmax(x[0], y[i]);
				}
			}
		}

	}
	else if (ny == 1)
	{
		if (isnan(y[0]))
		{
			for (MKL_INT i = 0; i < nx; i++)
			{
				result[i] = nan("");
			}
		}
		else
		{
			for (MKL_INT i = 0; i < nx; i++)
			{
				if (isnan(x[i]))
				{
					result[i] = nan("");
				}
				else
				{
					result[i] = fmax(x[i], y[0]);
				}
			}
		}
	}
	else
	{
		for (MKL_INT i = 0; i < nx; i++)
		{
			if (isnan(x[i]) || isnan(y[i]))
			{
				result[i] = nan("");
			}
			else
			{
				result[i] = fmax(x[i], y[i]);
			}
		}
	}
}

template<typename T>
void iif_arrays(MKL_INT nx, T* x, MKL_INT ny, T* y, bool* b, T* result)
{
	if (nx == 1)
	{
		for (MKL_INT i = 0; i < ny; i++)
		{
			result[i] = b[i] ? x[0] : y[i];
		}
	}
	else if (ny == 1)
	{
		for (MKL_INT i = 0; i < nx; i++)
		{
			result[i] = b[i] ? x[i] : y[0];
		}
	}
	else
	{
		for (MKL_INT i = 0; i < nx; i++)
		{
			result[i] = b[i] ? x[i] : y[i];
		}
	}
}

extern "C" __declspec(dllexport) void b_min_arrays(MKL_INT nx, bool* x, MKL_INT ny, bool* y, bool* result)
{
	if (nx == 1)
	{
		for (MKL_INT i = 0; i < ny; i++)
		{
			result[i] = min(x[0], y[i]);
		}
	}
	else if (ny == 1)
	{
		for (MKL_INT i = 0; i < nx; i++)
		{
			result[i] = min(x[i], y[0]);
		}
	}
	else
	{
		for (MKL_INT i = 0; i < nx; i++)
		{
			result[i] = min(x[i], y[i]);
		}
	}
}

extern "C" __declspec(dllexport) void d_min_arrays(MKL_INT nx, double* x, MKL_INT ny, double* y, double* result)
{
	min_arrays(nx, x, ny, y, result);
}

extern "C" __declspec(dllexport) void s_min_arrays(MKL_INT nx, float* x, MKL_INT ny, float* y, float* result)
{
	min_arrays(nx, x, ny, y, result); 
}

extern "C" __declspec(dllexport) void b_max_arrays(MKL_INT nx, bool* x, MKL_INT ny, bool* y, bool* result)
{
	if (nx == 1)
	{
		for (MKL_INT i = 0; i < ny; i++)
		{
			result[i] = max(x[0], y[i]);
		}
	}
	else if (ny == 1)
	{
		for (MKL_INT i = 0; i < nx; i++)
		{
			result[i] = max(x[i], y[0]);
		}
	}
	else
	{
		for (MKL_INT i = 0; i < nx; i++)
		{
			result[i] = max(x[i], y[i]);
		}
	}
}

extern "C" __declspec(dllexport) void d_max_arrays(MKL_INT nx, double* x, MKL_INT ny, double* y, double* result)
{
	max_arrays(nx, x, ny, y, result);
}

extern "C" __declspec(dllexport) void s_max_arrays(MKL_INT nx, float* x, MKL_INT ny, float* y, float* result)
{
	max_arrays(nx, x, ny, y, result); 
}

extern "C" __declspec(dllexport) void b_iif_arrays(MKL_INT nx, bool* x, MKL_INT ny, bool* y, bool* b, bool* result)
{
	iif_arrays(nx, x, ny, y, b, result); 
}

extern "C" __declspec(dllexport) void d_iif_arrays(MKL_INT nx, double* x, MKL_INT ny, double* y, bool* b, double* result)
{
	iif_arrays(nx, x, ny, y, b, result); 
}

extern "C" __declspec(dllexport) void s_iif_arrays(MKL_INT nx, float* x, MKL_INT ny, float* y, bool* b, float* result)
{
	iif_arrays(nx, x, ny, y, b, result); 
}

extern "C" __declspec(dllexport) void b_and_arrays(MKL_INT nx, bool* x, MKL_INT ny, bool* y, bool* result)
{
	if (nx == 1)
	{
		for (MKL_INT i = 0; i < ny; i++)
		{
			result[i] = x[0] && y[i];
		}
	}
	else if (ny == 1)
	{
		for (MKL_INT i = 0; i < nx; i++)
		{
			result[i] = x[i] && y[0];
		}
	}
	else
	{
		for (MKL_INT i = 0; i < nx; i++)
		{
			result[i] = x[i] && y[i];
		}
	}
}

extern "C" __declspec(dllexport) void b_or_arrays(MKL_INT nx, bool* x, MKL_INT ny, bool* y, bool* result)
{
	if (nx == 1)
	{
		for (MKL_INT i = 0; i < ny; i++)
		{
			result[i] = x[0] || y[i];
		}
	}
	else if (ny == 1)
	{
		for (MKL_INT i = 0; i < nx; i++)
		{
			result[i] = x[i] || y[0];
		}
	}
	else
	{
		for (MKL_INT i = 0; i < nx; i++)
		{
			result[i] = x[i] || y[i];
		}
	}
}

extern "C" __declspec(dllexport) void b_not_array(MKL_INT n, bool* x, bool* result)
{
	for (MKL_INT i = 0; i < n; i++)
	{
		result[i] = !x[i];
	}
}






extern "C" __declspec(dllexport) void d_scalar_mul_array(double a, MKL_INT n, double* x, double* result)
{
	scalar_mul_array(a, n, x, result);
}

extern "C" __declspec(dllexport) void s_scalar_mul_array(float a, MKL_INT n, float* x, float* result)
{
	scalar_mul_array(a, n, x, result);
}

extern "C" __declspec(dllexport) void d_scalar_add_array(double a, MKL_INT n, double* x, double* result)
{
	scalar_add_array(a, n, x, result);
}

extern "C" __declspec(dllexport) void s_scalar_add_array(float a, MKL_INT n, float* x, float* result)
{
	scalar_add_array(a, n, x, result);
}

extern "C" __declspec(dllexport) void d_scalar_sub_array(double a, MKL_INT n, double* x, double* result)
{
	scalar_sub_array(a, n, x, result);
}

extern "C" __declspec(dllexport) void s_scalar_sub_array(float a, MKL_INT n, float* x, float* result)
{
	scalar_sub_array(a, n, x, result);
}

extern "C" __declspec(dllexport) void d_array_sub_scalar(double a, MKL_INT n, double* x, double* result)
{
	array_sub_scalar(a, n, x, result);
}

extern "C" __declspec(dllexport) void s_array_sub_scalar(float a, MKL_INT n, float* x, float* result)
{
	array_sub_scalar(a, n, x, result);
}

extern "C" __declspec(dllexport) void d_array_div_scalar(double a, MKL_INT n, double* x, double* result)
{
	array_div_scalar(a, n, x, result);
}

extern "C" __declspec(dllexport) void s_array_div_scalar(float a, MKL_INT n, float* x, float* result)
{
	array_div_scalar(a, n, x, result);
}

extern "C" __declspec(dllexport) void d_scalar_div_array(double a, MKL_INT n, double* x, double* result)
{
	scalar_div_array(a, n, x, result);
}

extern "C" __declspec(dllexport) void s_scalar_div_array(float a, MKL_INT n, float* x, float* result)
{
	scalar_div_array(a, n, x, result);
}

extern "C" __declspec(dllexport) void d_scalar_pow_array(double a, MKL_INT n, double* x, double* result)
{
	scalar_pow_array(a, n, x, result);
}

extern "C" __declspec(dllexport) void s_scalar_pow_array(float a, MKL_INT n, float* x, float* result)
{
	scalar_pow_array(a, n, x, result);
}

extern "C" __declspec(dllexport) void d_array_pow_scalar(double a, MKL_INT n, double* x, double* result)
{
	array_pow_scalar(a, n, x, result);
}

extern "C" __declspec(dllexport) void s_array_pow_scalar(float a, MKL_INT n, float* x, float* result)
{
	array_pow_scalar(a, n, x, result);
}



extern "C" __declspec(dllexport) double d_inner_product(MKL_INT n, double* x, double* y)
{
	MKL_INT inc = 1;
	return ddot(&n, x, &inc, y, &inc);
}

extern "C" __declspec(dllexport) float s_inner_product(MKL_INT n, float* x, float* y)
{
	MKL_INT inc = 1;
	return sdot(&n, x, &inc, y, &inc);
}



extern "C" __declspec(dllexport) void d_array_add_array(MKL_INT n, double* x, double* y, double* result)
{
	vdAdd(n, x, y, result);
}

extern "C" __declspec(dllexport) void s_array_add_array(MKL_INT n, float* x, float* y, float* result)
{
	vsAdd(n, x, y, result);
}

extern "C" __declspec(dllexport) void d_array_sub_array(MKL_INT n, double* x, double* y, double* result)
{
	vdSub(n, x, y, result);
}

extern "C" __declspec(dllexport) void s_array_sub_array(MKL_INT n, float* x, float* y, float* result)
{
	vsSub(n, x, y, result);
}

extern "C" __declspec(dllexport) void d_array_mul_array(MKL_INT n, double* x, double* y, double* result)
{
	vdMul(n, x, y, result);
}

extern "C" __declspec(dllexport) void s_array_mul_array(MKL_INT n, float* x, float* y, float* result)
{
	vsMul(n, x, y, result);
}

extern "C" __declspec(dllexport) void d_array_div_array(MKL_INT n, double* x, double* y, double* result)
{
	vdDiv(n, x, y, result);
}

extern "C" __declspec(dllexport) void s_array_div_array(MKL_INT n, float* x, float* y, float* result)
{
	vsDiv(n, x, y, result);
}

extern "C" __declspec(dllexport) void d_array_pow_array(MKL_INT n, double* x, double* y, double* result)
{
	vdPow(n, x, y, result);
}

extern "C" __declspec(dllexport) void s_array_pow_array(MKL_INT n, float* x, float* y, float* result)
{
	vsPow(n, x, y, result);
}

extern "C" __declspec(dllexport) void d_array_axpby_array(MKL_INT n, double* x, double a, double* y, double b, double* result)
{
	MKL_INT inc = 1;
	if (x == result)
	{
		daxpby(&n, &b, y, &inc, &a, x, &inc); 
	}
	else if (y == result)
	{
		daxpby(&n, &a, x, &inc, &b, y, &inc); 
	}
	else
	{
		dcopy(&n, y, &inc, result, &inc);
		daxpby(&n, &a, x, &inc, &b, result, &inc);
	}
}

extern "C" __declspec(dllexport) void s_array_axpby_array(MKL_INT n, float* x, float a, float* y, float b, float* result)
{
	MKL_INT inc = 1;
	if (x == result)
	{
		saxpby(&n, &b, y, &inc, &a, x, &inc); 
	}
	else if (y == result)
	{
		saxpby(&n, &a, x, &inc, &b, y, &inc); 
	}
	else
	{
		scopy(&n, y, &inc, result, &inc);
		saxpby(&n, &a, x, &inc, &b, result, &inc);
	}
}

extern "C" __declspec(dllexport) void d_sqr_array(MKL_INT length, double* x, double* y)
{
	vdSqr(length, x, y); 
}

extern "C" __declspec(dllexport) void s_sqr_array(MKL_INT length, float* x, float* y)
{
	vsSqr(length, x, y); 
}

extern "C" __declspec(dllexport) void d_abs_array(MKL_INT length, double* x, double* y)
{
	vdAbs(length, x, y); 
}

extern "C" __declspec(dllexport) void s_abs_array(MKL_INT length, float* x, float* y)
{
	vsAbs(length, x, y); 
}

extern "C" __declspec(dllexport) void d_inv_array(MKL_INT length, double* x, double* y)
{
	vdInv(length, x, y); 
}

extern "C" __declspec(dllexport) void s_inv_array(MKL_INT length, float* x, float* y)
{
	vsInv(length, x, y); 
}

extern "C" __declspec(dllexport) void d_sqrt_array(MKL_INT length, double* x, double* y)
{
	vdSqrt(length, x, y); 
}

extern "C" __declspec(dllexport) void s_sqrt_array(MKL_INT length, float* x, float* y)
{
	vsSqrt(length, x, y); 
}

extern "C" __declspec(dllexport) void d_invsqrt_array(MKL_INT length, double* x, double* y)
{
	vdInvSqrt(length, x, y); 
}

extern "C" __declspec(dllexport) void s_invsqrt_array(MKL_INT length, float* x, float* y)
{
	vsInvSqrt(length, x, y); 
}

extern "C" __declspec(dllexport) void d_cbrt_array(MKL_INT length, double* x, double* y)
{
	vdCbrt(length, x, y); 
}

extern "C" __declspec(dllexport) void s_cbrt_array(MKL_INT length, float* x, float* y)
{
	vsCbrt(length, x, y); 
}

extern "C" __declspec(dllexport) void d_invcbrt_array(MKL_INT length, double* x, double* y)
{
	vdInvCbrt(length, x, y); 
}

extern "C" __declspec(dllexport) void s_invcbrt_array(MKL_INT length, float* x, float* y)
{
	vsInvCbrt(length, x, y); 
}

extern "C" __declspec(dllexport) void d_pow2o3_array(MKL_INT length, double* x, double* y)
{
	vdPow2o3(length, x, y); 
}

extern "C" __declspec(dllexport) void s_pow2o3_array(MKL_INT length, float* x, float* y)
{
	vsPow2o3(length, x, y); 
}

extern "C" __declspec(dllexport) void d_pow3o2_array(MKL_INT length, double* x, double* y)
{
	vdPow3o2(length, x, y); 
}

extern "C" __declspec(dllexport) void s_pow3o2_array(MKL_INT length, float* x, float* y)
{
	vsPow3o2(length, x, y); 
}

extern "C" __declspec(dllexport) void d_minus_array(MKL_INT length, double* x, double* y)
{
	minus_array(length, x, y); 
}

extern "C" __declspec(dllexport) void s_minus_array(MKL_INT length, float* x, float* y)
{
	minus_array(length, x, y); 
}

extern "C" __declspec(dllexport) void d_powx_array(double a, MKL_INT n, double* x, double* result)
{
	vdPowx(n, x, a, result);
}

extern "C" __declspec(dllexport) void s_powx_array(float a, MKL_INT n, float* x, float* result)
{
	vsPowx(n, x, a, result);
}

extern "C" __declspec(dllexport) void d_exp_array(MKL_INT length, double* x, double* y)
{
	vdExp(length, x, y); 
}

extern "C" __declspec(dllexport) void s_exp_array(MKL_INT length, float* x, float* y)
{
	vsExp(length, x, y); 
}

extern "C" __declspec(dllexport) void d_expm1_array(MKL_INT length, double* x, double* y)
{
	vdExpm1(length, x, y); 
}

extern "C" __declspec(dllexport) void s_expm1_array(MKL_INT length, float* x, float* y)
{
	vsExpm1(length, x, y); 
}

extern "C" __declspec(dllexport) void d_ln_array(MKL_INT length, double* x, double* y)
{
	vdLn(length, x, y); 
}

extern "C" __declspec(dllexport) void s_ln_array(MKL_INT length, float* x, float* y)
{
	vsLn(length, x, y); 
}

extern "C" __declspec(dllexport) void d_log10_array(MKL_INT length, double* x, double* y)
{
	vdLog10(length, x, y); 
}

extern "C" __declspec(dllexport) void s_log10_array(MKL_INT length, float* x, float* y)
{
	vsLog10(length, x, y); 
}

extern "C" __declspec(dllexport) void d_log1p_array(MKL_INT length, double* x, double* y)
{
	vdLog1p(length, x, y); 
}

extern "C" __declspec(dllexport) void s_log1p_array(MKL_INT length, float* x, float* y)
{
	vsLog1p(length, x, y); 
}

extern "C" __declspec(dllexport) void d_cos_array(MKL_INT length, double* x, double* y)
{
	vdCos(length, x, y); 
}

extern "C" __declspec(dllexport) void s_cos_array(MKL_INT length, float* x, float* y)
{
	vsCos(length, x, y); 
}

extern "C" __declspec(dllexport) void d_sin_array(MKL_INT length, double* x, double* y)
{
	vdSin(length, x, y); 
}

extern "C" __declspec(dllexport) void s_sin_array(MKL_INT length, float* x, float* y)
{
	vsSin(length, x, y); 
}

extern "C" __declspec(dllexport) void d_tan_array(MKL_INT length, double* x, double* y)
{
	vdTan(length, x, y); 
}

extern "C" __declspec(dllexport) void s_tan_array(MKL_INT length, float* x, float* y)
{
	vsTan(length, x, y); 
}



extern "C" __declspec(dllexport) void d_acos_array(MKL_INT length, double* x, double* y)
{
	vdAcos(length, x, y); 
}

extern "C" __declspec(dllexport) void s_acos_array(MKL_INT length, float* x, float* y)
{
	vsAcos(length, x, y); 
}

extern "C" __declspec(dllexport) void d_asin_array(MKL_INT length, double* x, double* y)
{
	vdAsin(length, x, y); 
}

extern "C" __declspec(dllexport) void s_asin_array(MKL_INT length, float* x, float* y)
{
	vsAsin(length, x, y); 
}

extern "C" __declspec(dllexport) void d_atan_array(MKL_INT length, double* x, double* y)
{
	vdAtan(length, x, y); 
}

extern "C" __declspec(dllexport) void s_atan_array(MKL_INT length, float* x, float* y)
{
	vsAtan(length, x, y); 
}



extern "C" __declspec(dllexport) void d_cosh_array(MKL_INT length, double* x, double* y)
{
	vdCosh(length, x, y); 
}

extern "C" __declspec(dllexport) void s_cosh_array(MKL_INT length, float* x, float* y)
{
	vsCosh(length, x, y); 
}

extern "C" __declspec(dllexport) void d_sinh_array(MKL_INT length, double* x, double* y)
{
	vdSinh(length, x, y); 
}

extern "C" __declspec(dllexport) void s_sinh_array(MKL_INT length, float* x, float* y)
{
	vsSinh(length, x, y); 
}

extern "C" __declspec(dllexport) void d_tanh_array(MKL_INT length, double* x, double* y)
{
	vdTanh(length, x, y); 
}

extern "C" __declspec(dllexport) void s_tanh_array(MKL_INT length, float* x, float* y)
{
	vsTanh(length, x, y); 
}



extern "C" __declspec(dllexport) void d_acosh_array(MKL_INT length, double* x, double* y)
{
	vdAcosh(length, x, y); 
}

extern "C" __declspec(dllexport) void s_acosh_array(MKL_INT length, float* x, float* y)
{
	vsAcosh(length, x, y); 
}

extern "C" __declspec(dllexport) void d_asinh_array(MKL_INT length, double* x, double* y)
{
	vdAsinh(length, x, y); 
}

extern "C" __declspec(dllexport) void s_asinh_array(MKL_INT length, float* x, float* y)
{
	vsAsinh(length, x, y); 
}

extern "C" __declspec(dllexport) void d_atanh_array(MKL_INT length, double* x, double* y)
{
	vdAtanh(length, x, y); 
}

extern "C" __declspec(dllexport) void s_atanh_array(MKL_INT length, float* x, float* y)
{
	vsAtanh(length, x, y); 
}


extern "C" __declspec(dllexport) void d_erf_array(MKL_INT length, double* x, double* y)
{
	vdErf(length, x, y); 
}

extern "C" __declspec(dllexport) void s_erf_array(MKL_INT length, float* x, float* y)
{
	vsErf(length, x, y); 
}

extern "C" __declspec(dllexport) void d_erfc_array(MKL_INT length, double* x, double* y)
{
	vdErfc(length, x, y); 
}

extern "C" __declspec(dllexport) void s_erfc_array(MKL_INT length, float* x, float* y)
{
	vsErfc(length, x, y); 
}

extern "C" __declspec(dllexport) void d_cdfnorm_array(MKL_INT length, double* x, double* y)
{
	vdCdfNorm(length, x, y); 
}

extern "C" __declspec(dllexport) void s_cdfnorm_array(MKL_INT length, float* x, float* y)
{
	vsCdfNorm(length, x, y); 
}

extern "C" __declspec(dllexport) void d_erfinv_array(MKL_INT length, double* x, double* y)
{
	vdErfInv(length, x, y); 
}

extern "C" __declspec(dllexport) void s_erfinv_array(MKL_INT length, float* x, float* y)
{
	vsErfInv(length, x, y); 
}

extern "C" __declspec(dllexport) void d_erfcinv_array(MKL_INT length, double* x, double* y)
{
	vdErfcInv(length, x, y); 
}

extern "C" __declspec(dllexport) void s_erfcinv_array(MKL_INT length, float* x, float* y)
{
	vsErfcInv(length, x, y); 
}

extern "C" __declspec(dllexport) void d_cdfnorminv_array(MKL_INT length, double* x, double* y)
{
	vdCdfNormInv(length, x, y); 
}

extern "C" __declspec(dllexport) void s_cdfnorminv_array(MKL_INT length, float* x, float* y)
{
	vsCdfNormInv(length, x, y); 
}

extern "C" __declspec(dllexport) void d_floor_array(MKL_INT length, double* x, double* y)
{
	vdFloor(length, x, y); 
}

extern "C" __declspec(dllexport) void s_floor_array(MKL_INT length, float* x, float* y)
{
	vsFloor(length, x, y); 
}

extern "C" __declspec(dllexport) void d_ceil_array(MKL_INT length, double* x, double* y)
{
	vdCeil(length, x, y); 
}

extern "C" __declspec(dllexport) void s_ceil_array(MKL_INT length, float* x, float* y)
{
	vsCeil(length, x, y); 
}

extern "C" __declspec(dllexport) void d_trunc_array(MKL_INT length, double* x, double* y)
{
	vdTrunc(length, x, y); 
}

extern "C" __declspec(dllexport) void s_trunc_array(MKL_INT length, float* x, float* y)
{
	vsTrunc(length, x, y); 
}

extern "C" __declspec(dllexport) void d_round_array(MKL_INT length, double* x, double* y)
{
	vdRound(length, x, y); 
}

extern "C" __declspec(dllexport) void s_round_array(MKL_INT length, float* x, float* y)
{
	vsRound(length, x, y); 
}






