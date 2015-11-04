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

int sub2ind(int n, int* dimProd, int* subscripts)
{
	int index = subscripts[0];
	for (int i = 1; i < n; i++)
	{
		index += subscripts[i] * dimProd[i];
	}
	return index;
}

extern "C" __declspec(dllexport) void update_xbeta(MKL_INT n, double* xbeta, int k, int** slices, int* estimateMap, int* dimProd,
	                                               double* beta, double* covariate, int offset)
{
	double* offsetBeta = beta + offset;
	int* subscripts = (int*)mkl_malloc(k*sizeof(int), 64);
	if (covariate == nullptr)
	{
		if (k = 1)
		{
			for (MKL_INT i = 0; i < n; i++)
			{
				int index = estimateMap[slices[0][i]];
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
				for (int j = 0; j < k; j++)
				{
					subscripts[j] = slices[j][i];
				}
				int index = estimateMap[sub2ind(k, dimProd, subscripts)];
				if (index >= 0)
				{
					xbeta[i] += offsetBeta[index];
				}
			}
		}
	}
	else
	{
		if (k = 1)
		{
			for (MKL_INT i = 0; i < n; i++)
			{
				int index = estimateMap[slices[0][i]];
				if (index >= 0)
				{
					xbeta[i] += offsetBeta[index] * covariate[i];
				}
			}
		}
		else
		{
			for (MKL_INT i = 0; i < n; i++)
			{
				for (int j = 0; j < k; j++)
				{
					subscripts[j] = slices[j][i];
				}
				int index = estimateMap[sub2ind(k, dimProd, subscripts)];
				if (index >= 0)
				{
					xbeta[i] += offsetBeta[index] * covariate[i];
				}
			}
		}

	}
	mkl_free(subscripts);
}

extern "C" __declspec(dllexport) void update_U(MKL_INT n, double* U, double* u, int k, int** slices, int* estimateMap, int* dimProd,
	                                           double* covariate, int offset)
{
	double* offsetU = U + offset;
	int* subscripts = (int*)mkl_malloc(k*sizeof(int), 64);
	if (covariate == nullptr)
	{
		if (k = 1)
		{
			for (MKL_INT i = 0; i < n; i++)
			{
				int index = estimateMap[slices[0][i]];
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
				for (int j = 0; j < k; j++)
				{
					subscripts[j] = slices[j][i];
				}
				int index = estimateMap[sub2ind(k, dimProd, subscripts)];
				if (index >= 0)
				{
					offsetU[index] += u[i];
				}
			}
		}
	}
	else
	{
		if (k = 1)
		{
			for (MKL_INT i = 0; i < n; i++)
			{
				int index = estimateMap[slices[0][i]];
				if (index >= 0)
				{
					offsetU[index] += u[i] * covariate[i];
				}
			}
		}
		else
		{
			for (MKL_INT i = 0; i < n; i++)
			{
				for (int j = 0; j < k; j++)
				{
					subscripts[j] = slices[j][i];
				}
				int index = estimateMap[sub2ind(k, dimProd, subscripts)];
				if (index >= 0)
				{
					offsetU[index] += u[i] * covariate[i];
				}
			}
		}
	}
	mkl_free(subscripts);
}

extern "C" __declspec(dllexport) void update_H(MKL_INT n, int p, double* H, double* weight, double* rowCovariate, double* colCovariate,
	                                           int rowK, int** rowSlices, int* rowEstimateMap, int* rowDimProd,
											   int colK, int** colSlices, int* colEstimateMap, int* colDimProd, int rowOffset, int colOffset)
{
	double* offsetH = H + colOffset * p + rowOffset;
	int* rowSubscripts = (int*)mkl_malloc(rowK*sizeof(int), 64);
	int* colSubscripts = (int*)mkl_malloc(colK*sizeof(int), 64);
	int rowIndex;
	int colIndex;
	if (rowK == 0 && rowCovariate != nullptr && colK != 0 && colCovariate != nullptr)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			if (colK = 1)
			{
				colIndex = colEstimateMap[colSlices[0][i]];
			}
			else
			{
				for (int j = 0; j < colK; j++)
				{
					colSubscripts[j] = colSlices[j][i];
				}
				colIndex = colEstimateMap[sub2ind(colK, colDimProd, colSubscripts)];
			}
			if (colIndex >= 0)
			{
				offsetH[colIndex * p] += weight[i] * rowCovariate[i] * colCovariate[i];
			}
		}
	}
	else if (rowK == 0 && rowCovariate != nullptr && colK != 0 && colCovariate == nullptr)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			if (colK = 1)
			{
				colIndex = colEstimateMap[colSlices[0][i]];
			}
			else
			{
				for (int j = 0; j < colK; j++)
				{
					colSubscripts[j] = colSlices[j][i];
				}
				colIndex = colEstimateMap[sub2ind(colK, colDimProd, colSubscripts)];
			}
			if (colIndex >= 0)
			{
				offsetH[colIndex * p] += weight[i] * rowCovariate[i];
			}
		}
	}
	else if (rowK != 0 && rowCovariate != nullptr && colK == 0 && colCovariate != nullptr)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			if (rowK = 1)
			{
				rowIndex = rowEstimateMap[rowSlices[0][i]];
			}
			else
			{
				for (int j = 0; j < rowK; j++)
				{
					rowSubscripts[j] = rowSlices[j][i];
				}
				rowIndex = rowEstimateMap[sub2ind(rowK, rowDimProd, rowSubscripts)];
			}
			if (rowIndex >= 0)
			{
				offsetH[rowIndex] += weight[i] * rowCovariate[i] * colCovariate[i];
			}
		}
	}
	else if (rowK != 0 && rowCovariate != nullptr && colK != 0 && colCovariate != nullptr)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			if (rowK = 1)
			{
				rowIndex = rowEstimateMap[rowSlices[0][i]];
			}
			else
			{
				for (int j = 0; j < rowK; j++)
				{
					rowSubscripts[j] = rowSlices[j][i];
				}
				rowIndex = rowEstimateMap[sub2ind(rowK, rowDimProd, rowSubscripts)];
			}
			if (colK = 1)
			{
				colIndex = colEstimateMap[colSlices[0][i]];
			}
			else
			{
				for (int j = 0; j < colK; j++)
				{
					colSubscripts[j] = colSlices[j][i];
				}
				colIndex = colEstimateMap[sub2ind(colK, colDimProd, colSubscripts)];
			}
			if (rowIndex >= 0 && colIndex >= 0)
			{
				offsetH[colIndex  * p + rowIndex] += weight[i] * rowCovariate[i] * colCovariate[i];
			}
		}
	}
	else if (rowK != 0 && rowCovariate != nullptr && colK != 0 && colCovariate == nullptr)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			if (rowK = 1)
			{
				rowIndex = rowEstimateMap[rowSlices[0][i]];
			}
			else
			{
				for (int j = 0; j < rowK; j++)
				{
					rowSubscripts[j] = rowSlices[j][i];
				}
				rowIndex = rowEstimateMap[sub2ind(rowK, rowDimProd, rowSubscripts)];
			}
			if (colK = 1)
			{
				colIndex = colEstimateMap[colSlices[0][i]];
			}
			else
			{
				for (int j = 0; j < colK; j++)
				{
					colSubscripts[j] = colSlices[j][i];
				}
				colIndex = colEstimateMap[sub2ind(colK, colDimProd, colSubscripts)];
			}
			if (rowIndex >= 0 && colIndex >= 0)
			{
				offsetH[colIndex  * p + rowIndex] += weight[i] * rowCovariate[i];
			}
		}
	}
	else if (rowK != 0 && rowCovariate == nullptr && colK == 0 && colCovariate != nullptr)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			if (rowK = 1)
			{
				rowIndex = rowEstimateMap[rowSlices[0][i]];
			}
			else
			{
				for (int j = 0; j < rowK; j++)
				{
					rowSubscripts[j] = rowSlices[j][i];
				}
				rowIndex = rowEstimateMap[sub2ind(rowK, rowDimProd, rowSubscripts)];
			}
			if (rowIndex >= 0)
			{
				offsetH[rowIndex] += weight[i] * colCovariate[i];
			}
		}
	}
	else if (rowK != 0 && rowCovariate == nullptr && colK != 0 && colCovariate != nullptr)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			if (rowK = 1)
			{
				rowIndex = rowEstimateMap[rowSlices[0][i]];
			}
			else
			{
				for (int j = 0; j < rowK; j++)
				{
					rowSubscripts[j] = rowSlices[j][i];
				}
				rowIndex = rowEstimateMap[sub2ind(rowK, rowDimProd, rowSubscripts)];
			}
			if (colK = 1)
			{
				colIndex = colEstimateMap[colSlices[0][i]];
			}
			else
			{
				for (int j = 0; j < colK; j++)
				{
					colSubscripts[j] = colSlices[j][i];
				}
				colIndex = colEstimateMap[sub2ind(colK, colDimProd, colSubscripts)];
			}
			if (rowIndex >= 0 && colIndex >= 0)
			{
				offsetH[colIndex  * p + rowIndex] += weight[i] * colCovariate[i];
			}
		}
	}
	else if (rowK != 0 && rowCovariate == nullptr && colK != 0 && colCovariate == nullptr)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			if (rowK = 1)
			{
				rowIndex = rowEstimateMap[rowSlices[0][i]];
			}
			else
			{
				for (int j = 0; j < rowK; j++)
				{
					rowSubscripts[j] = rowSlices[j][i];
				}
				rowIndex = rowEstimateMap[sub2ind(rowK, rowDimProd, rowSubscripts)];
			}
			if (colK = 1)
			{
				colIndex = colEstimateMap[colSlices[0][i]];
			}
			else
			{
				for (int j = 0; j < colK; j++)
				{
					colSubscripts[j] = colSlices[j][i];
				}
				colIndex = colEstimateMap[sub2ind(colK, colDimProd, colSubscripts)];
			}
			if (rowIndex >= 0 && colIndex >= 0)
			{
				offsetH[colIndex * p + rowIndex] += weight[i];
			}
		}
	}
	mkl_free(rowSubscripts);
	mkl_free(colSubscripts);
}


