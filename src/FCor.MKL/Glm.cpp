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

inline int sub2ind(int n, int* dimProd, unsigned short* subscripts)
{
	int index = subscripts[0];
	for (int i = 1; i < n; i++)
	{
		index += subscripts[i] * dimProd[i];
	}
	return index;
}

inline int findcutindex(int breakCount, double* breaks, double x)
{
	for (int i = 0; i < breakCount - 2; i++)
	{
		if (x >= breaks[i] && x < breaks[i+1])
		{
			return i;
		}
	}
	if (x >= breaks[breakCount - 2] && x <= breaks[breakCount-1])
	{
		return breakCount - 2;
	}
	return breakCount - 1;
}

extern "C" __declspec(dllexport) void get_cut_level_index(int n, int breakCount, double* breaks, double* numSlice, unsigned short* result)
{
	for (int i = 0; i < n; i++)
	{
		result[i] = findcutindex(breakCount, breaks, numSlice[i]);
	}
}

extern "C" __declspec(dllexport) void get_cross_level_index(int n, unsigned short rowCount, unsigned short* slice1, unsigned short* slice2, unsigned short* result)
{
	for (int i = 0; i < n; i++)
	{
		result[i] = slice2[i] * rowCount + slice1[i];
	}
}

extern "C" __declspec(dllexport) void level_index_to_numeric(int n, unsigned short* slice, double* x, double* map)
{
	for (int i = 0; i < n; i++)
	{
		x[i] = map[slice[i]];
	}
}

extern "C" __declspec(dllexport) void update_level_index(int n, unsigned short* slice, unsigned short* map)
{
	for (int i = 0; i < n; i++)
	{
		slice[i] = map[slice[i]];
	}
}

extern "C" __declspec(dllexport) void update_xbeta(int n, double* xbeta, int k, unsigned short** slices, int* estimateMap, int* dimProd,
	                                               double* beta, double* covariate, int offset)
{
	double* offsetBeta = beta + offset;
	unsigned short* subscripts = (unsigned short*)mkl_malloc(k*sizeof(unsigned short), 64);
	if (covariate == nullptr)
	{
		if (k = 1)
		{
			for (int i = 0; i < n; i++)
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
			for (int i = 0; i < n; i++)
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
			for (int i = 0; i < n; i++)
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
			for (int i = 0; i < n; i++)
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

extern "C" __declspec(dllexport) void update_U(int n, double* U, double* u, int k, unsigned short** slices, int* estimateMap, int* dimProd,
	                                           double* covariate, int offset)
{
	double* offsetU = U + offset;
	unsigned short* subscripts = (unsigned short*)mkl_malloc(k*sizeof(unsigned short), 64);
	if (covariate == nullptr)
	{
		if (k = 1)
		{
			for (int i = 0; i < n; i++)
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
			for (int i = 0; i < n; i++)
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
			for (int i = 0; i < n; i++)
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
			for (int i = 0; i < n; i++)
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

extern "C" __declspec(dllexport) void update_H(int n, int p, double* H, double* weight, double* rowCovariate, double* colCovariate,
	                                           int rowK, unsigned short** rowSlices, int* rowEstimateMap, int* rowDimProd,
											   int colK, unsigned short** colSlices, int* colEstimateMap, int* colDimProd, int rowOffset, int colOffset)
{
	double* offsetH = H + colOffset * p + rowOffset;
	unsigned short* rowSubscripts = (unsigned short*)mkl_malloc(rowK*sizeof(unsigned short), 64);
	unsigned short* colSubscripts = (unsigned short*)mkl_malloc(colK*sizeof(unsigned short), 64);
	int rowIndex;
	int colIndex;
	if (rowK == 0 && rowCovariate != nullptr && colK != 0 && colCovariate != nullptr && weight != nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			if (colK == 1)
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
	else if (rowK == 0 && rowCovariate != nullptr && colK != 0 && colCovariate != nullptr && weight == nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			if (colK == 1)
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
				offsetH[colIndex * p] += rowCovariate[i] * colCovariate[i];
			}
		}
	}
	else if (rowK == 0 && rowCovariate != nullptr && colK != 0 && colCovariate == nullptr && weight != nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			if (colK == 1)
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
	else if (rowK == 0 && rowCovariate != nullptr && colK != 0 && colCovariate == nullptr && weight == nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			if (colK == 1)
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
				offsetH[colIndex * p] += rowCovariate[i];
			}
		}
	}
	else if (rowK != 0 && rowCovariate != nullptr && colK == 0 && colCovariate != nullptr && weight != nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			if (rowK == 1)
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
	else if (rowK != 0 && rowCovariate != nullptr && colK == 0 && colCovariate != nullptr && weight == nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			if (rowK == 1)
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
				offsetH[rowIndex] += rowCovariate[i] * colCovariate[i];
			}
		}
	}
	else if (rowK != 0 && rowCovariate != nullptr && colK != 0 && colCovariate != nullptr && weight != nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			if (rowK == 1)
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
			if (colK == 1)
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
	else if (rowK != 0 && rowCovariate != nullptr && colK != 0 && colCovariate != nullptr && weight == nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			if (rowK == 1)
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
			if (colK == 1)
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
				offsetH[colIndex  * p + rowIndex] += rowCovariate[i] * colCovariate[i];
			}
		}
	}
	else if (rowK != 0 && rowCovariate != nullptr && colK != 0 && colCovariate == nullptr && weight != nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			if (rowK == 1)
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
			if (colK == 1)
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
	else if (rowK != 0 && rowCovariate != nullptr && colK != 0 && colCovariate == nullptr && weight == nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			if (rowK == 1)
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
			if (colK == 1)
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
				offsetH[colIndex  * p + rowIndex] += rowCovariate[i];
			}
		}
	}
	else if (rowK != 0 && rowCovariate == nullptr && colK == 0 && colCovariate != nullptr && weight != nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			if (rowK == 1)
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
	else if (rowK != 0 && rowCovariate == nullptr && colK == 0 && colCovariate != nullptr && weight == nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			if (rowK == 1)
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
				offsetH[rowIndex] += colCovariate[i];
			}
		}
	}
	else if (rowK != 0 && rowCovariate == nullptr && colK != 0 && colCovariate != nullptr && weight != nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			if (rowK == 1)
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
			if (colK == 1)
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
	else if (rowK != 0 && rowCovariate == nullptr && colK != 0 && colCovariate != nullptr && weight == nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			if (rowK == 1)
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
			if (colK == 1)
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
				offsetH[colIndex  * p + rowIndex] += colCovariate[i];
			}
		}
	}
	else if (rowK != 0 && rowCovariate == nullptr && colK != 0 && colCovariate == nullptr && weight != nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			if (rowK == 1)
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
			if (colK == 1)
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
	else if (rowK != 0 && rowCovariate == nullptr && colK != 0 && colCovariate == nullptr && weight == nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			if (rowK == 1)
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
			if (colK == 1)
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
				offsetH[colIndex * p + rowIndex] += 1.0;
			}
		}
	}
	mkl_free(rowSubscripts);
	mkl_free(colSubscripts);
}


