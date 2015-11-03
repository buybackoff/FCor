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
#include <cmath>
#include <mkl_service.h>

extern "C" __declspec(dllexport) void update_xbeta(MKL_INT n, double* xbeta, int* indices, double* beta, double* covariate, int offset)
{
	double* offsetBeta = beta + offset;
	if (covariate == nullptr)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			int index = indices[i];
			if (index >= 0)
			{
				xbeta[i] += offsetBeta[index];
			}
		}
	}
	else
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			int index = indices[i];
			if (index >= 0)
			{
				xbeta[i] += offsetBeta[index] * covariate[i];
			}
		}
	}
}

extern "C" __declspec(dllexport) void update_U(MKL_INT n, double* U, double* u, int* indices, double* covariate, int offset)
{
	double* offsetU = U + offset;
	if (covariate == nullptr)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			int index = indices[i];
			if (index >= 0)
			{
				offsetU[index] += u[i];
			}
		}
	}
	else
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			int index = indices[i];
			if (index >= 0)
			{
				offsetU[index] += u[i] * covariate[i];
			}
		}
	}
}

extern "C" __declspec(dllexport) void update_H(MKL_INT n, int p, double* H, double* weight, double* rowCovariate, double* colCovariate,
	                                           int* rowIndices, int* colIndices, int rowOffset, int colOffset)
{
	double* offsetH = H + colOffset * p;
	if (rowIndices == nullptr && rowCovariate != nullptr && colIndices != nullptr && colCovariate != nullptr)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			int colIndex = colIndices[i];
			if (colIndex >= 0)
			{
				offsetH[colIndex * p + rowOffset] += weight[i] * rowCovariate[i] * colCovariate[i];
			}
		}
	}
	else if (rowIndices == nullptr && rowCovariate != nullptr && colIndices != nullptr && colCovariate == nullptr)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			int colIndex = colIndices[i];
			if (colIndex >= 0)
			{
				offsetH[colIndex * p + rowOffset] += weight[i] * rowCovariate[i];
			}
		}
	}
	else if (rowIndices != nullptr && rowCovariate != nullptr && colIndices == nullptr && colCovariate != nullptr)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			int rowIndex = rowIndices[i];
			if (rowIndex >= 0)
			{
				offsetH[rowOffset + rowIndex] += weight[i] * rowCovariate[i] * colCovariate[i];
			}
		}
	}
	else if (rowIndices != nullptr && rowCovariate != nullptr && colIndices != nullptr && colCovariate != nullptr)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			int rowIndex = rowIndices[i];
			int colIndex = colIndices[i];
			if (rowIndex >= 0 && colIndex >= 0)
			{
				offsetH[colIndex  * p + rowOffset + rowIndex] += weight[i] * rowCovariate[i] * colCovariate[i];
			}
		}
	}
	else if (rowIndices != nullptr && rowCovariate != nullptr && colIndices != nullptr && colCovariate == nullptr)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			int rowIndex = rowIndices[i];
			int colIndex = colIndices[i];
			if (rowIndex >= 0 && colIndex >= 0)
			{
				offsetH[colIndex  * p + rowOffset + rowIndex] += weight[i] * rowCovariate[i];
			}
		}
	}
	else if (rowIndices != nullptr && rowCovariate == nullptr && colIndices == nullptr && colCovariate != nullptr)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			int rowIndex = rowIndices[i];
			if (rowIndex >= 0)
			{
				offsetH[rowOffset + rowIndex] += weight[i] * colCovariate[i];
			}
		}
	}
	else if (rowIndices != nullptr && rowCovariate == nullptr && colIndices != nullptr && colCovariate != nullptr)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			int rowIndex = rowIndices[i];
			int colIndex = colIndices[i];
			if (rowIndex >= 0 && colIndex >= 0)
			{
				offsetH[colIndex  * p + rowOffset + rowIndex] += weight[i] * colCovariate[i];
			}
		}
	}
	else if (rowIndices != nullptr && rowCovariate == nullptr && colIndices != nullptr && colCovariate == nullptr)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			int rowIndex = rowIndices[i];
			int colIndex = colIndices[i];
			if (rowIndex >= 0 && colIndex >= 0)
			{
				offsetH[colIndex * p + rowOffset + rowIndex] += weight[i];
			}
		}
	}
}
