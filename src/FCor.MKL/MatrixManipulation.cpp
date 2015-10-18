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
void get_diag(MKL_INT rowCount, MKL_INT offset, MKL_INT n, T* x, T* res)
{
	MKL_INT k = offset < 0 ? -offset : offset;
	MKL_INT indexIn = offset > 0 ? k * rowCount : k;
	MKL_INT rowCountplus1 = rowCount + 1;
	for (MKL_INT i = 0; i < n; i++)
	{
		res[i] = x[indexIn];
		indexIn += rowCountplus1;
	}
}

template<typename T>
void set_diag(MKL_INT n, MKL_INT offset, T* x, T* res)
{
	MKL_INT k = offset < 0 ? -offset : offset;
	MKL_INT nk = n + k;
	MKL_INT step = n + k + 1;
	MKL_INT index = offset > 0 ? k * nk : k;
	for (MKL_INT i = 0; i < n; i++)
	{
		res[index] = x[i];
		index += step;
	}
}

template<typename T>
void get_upper_tri(MKL_INT offset, MKL_INT rows, MKL_INT cols, T* x, T* res)
{
	MKL_INT len;
	if (offset <= 0)
	{
		MKL_INT index = 0;
		int row = -offset;
		for (MKL_INT i = 0; i < cols; i++)
		{
			len = row < rows ? row + 1 : rows;
			if (len > 0)
			{
				memcpy(res + index, x + index, len*sizeof(T));
			}
			row++;
			index += rows;
		}
	}
	else
	{
		MKL_INT index = offset * rows;
		MKL_INT row = 0;
		for (MKL_INT i = offset; i < cols; i++)
		{
			len = row < rows ? row + 1 : rows;
			if (len > 0)
			{
				memcpy(res + index, x + index, len*sizeof(T));
			}
			row++;
			index += rows;
		}
	}
}

template<typename T>
void get_lower_tri(MKL_INT offset, MKL_INT rows, MKL_INT cols, T* x, T* res)
{
	MKL_INT len;
	if (offset <= 0)
	{
		MKL_INT index = -offset;
		MKL_INT row = -offset;
		for (MKL_INT i = 0; i < cols; i++)
		{
			len = rows - row;
			if (len > 0)
			{
				memcpy(res + index, x + index, len*sizeof(T));
			}
			row++;
			if (row >= rows)
			{
				break;
			}
			index += rows + 1;
		}
	}
	else
	{
		len = rows * (offset < cols ? offset + 1 : cols);
		memcpy(res, x, len*sizeof(T));
		MKL_INT index = (offset + 1) * rows + 1;
		MKL_INT row = 1;
		for (MKL_INT i = offset + 1; i < cols; i++)
		{
			len = rows - row;
			if (len > 0)
			{
				memcpy(res + index, x + index, len*sizeof(T));
			}
			row++;
			if (row >= rows)
			{
				break;
			}
			index += rows + 1;
		}
	}
}

template<typename T>
void identity(MKL_INT rows, MKL_INT cols, T* x)
{
	MKL_INT n = rows < cols ? rows : cols;
	MKL_INT index = 0;
	MKL_INT rowsplus1 = rows + 1;
	T one = 1.0f;
	for (MKL_INT i = 0; i < n; i++)
	{
		x[index] = one;
		index += rowsplus1;
	}
}

extern "C" __declspec(dllexport) void d_transpose_in_place(MKL_INT rows, MKL_INT cols, double* x)
{
	double a = 1;
	mkl_dimatcopy('C', 'T', rows, cols, a, x, rows, cols);
}

extern "C" __declspec(dllexport) void s_transpose_in_place(MKL_INT rows, MKL_INT cols, float* x)
{
	float a = 1;
	mkl_simatcopy('C', 'T', rows, cols, a, x, rows, cols);
}

extern "C" __declspec(dllexport) void d_transpose(MKL_INT rows, MKL_INT cols, double* x, double* y)
{
	double a = 1;
	mkl_domatcopy('C', 'T', rows, cols, a, x, rows, y, cols);
}

extern "C" __declspec(dllexport) void s_transpose(MKL_INT rows, MKL_INT cols, float* x, float* y)
{
	float a = 1;
	mkl_somatcopy('C', 'T', rows, cols, a, x, rows, y, cols);
}

extern "C" __declspec(dllexport) void d_get_diag(MKL_INT rowCount, MKL_INT offset, MKL_INT n, double* x, double* res)
{
	get_diag(rowCount, offset, n, x, res);
}

extern "C" __declspec(dllexport) void s_get_diag(MKL_INT rowCount, MKL_INT offset, MKL_INT n, float* x, float* res)
{
	get_diag(rowCount, offset, n, x, res);
}

extern "C" __declspec(dllexport) void d_set_diag(MKL_INT n, MKL_INT offset, double* x, double* res)
{
	set_diag(n, offset, x, res);
}

extern "C" __declspec(dllexport) void s_set_diag(MKL_INT n, MKL_INT offset, float* x, float* res)
{
	set_diag(n, offset, x, res);
}

extern "C" __declspec(dllexport) void d_get_upper_tri(MKL_INT offset, MKL_INT rows, MKL_INT cols, double* x, double* res)
{
	get_upper_tri(offset, rows, cols, x, res);
}

extern "C" __declspec(dllexport) void s_get_upper_tri(MKL_INT offset, MKL_INT rows, MKL_INT cols, float* x, float* res)
{
	get_upper_tri(offset, rows, cols, x, res);
}

extern "C" __declspec(dllexport) void d_get_lower_tri(MKL_INT offset, MKL_INT rows, MKL_INT cols, double* x, double* res)
{
	get_lower_tri(offset, rows, cols, x, res);
}

extern "C" __declspec(dllexport) void s_get_lower_tri(MKL_INT offset, MKL_INT rows, MKL_INT cols, float* x, float* res)
{
	get_lower_tri(offset, rows, cols, x, res);
}

extern "C" __declspec(dllexport) void d_identity(MKL_INT rows, MKL_INT cols, double* x)
{
	identity(rows, cols, x);
}

extern "C" __declspec(dllexport) void s_identity(MKL_INT rows, MKL_INT cols, float* x)
{
	identity(rows, cols, x);
}

extern "C" __declspec(dllexport) void b_identity(MKL_INT rows, MKL_INT cols, bool* x)
{
	identity(rows, cols, x);
}