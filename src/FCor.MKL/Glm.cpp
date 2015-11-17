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

inline int findknotindex(int knotCount, int* knots, double x)
{
	for (int i = 0; i < knotCount - 1; i++)
	{
		if (x == (double)knots[i])
		{
			return i;
		}
	}
	return knotCount;
}

extern "C" __declspec(dllexport) double sum_array_notnan(int n, double* x)
{
	double res = nan("");
	bool isnan = true;
	for (int i = 0; i < n; i++)
	{
		double y = x[i];
		if (y == y)
		{
			if (isnan)
			{
				res = y;
				isnan = false;
			}
			else
			{
				res += y;
			}
		}
	}
	return res;
}

extern "C" __declspec(dllexport) double innerprod_arrays_notnan(int n, double* x, double* y)
{
	double res = nan("");
	bool isnan = true;
	for (int i = 0; i < n; i++)
	{
		double X = x[i];
		double Y = y[i];
		if (X == X && Y == Y)
		{
			if (isnan)
			{
				res = X * Y;
				isnan = false;
			}
			else
			{
				res += X * Y;
			}
		}
	}
	return res;
}

extern "C" __declspec(dllexport) double innerprod_3arrays_notnan(int n, double* x, double* y, double* z)
{
	double res = nan("");
	bool isnan = true;
	for (int i = 0; i < n; i++)
	{
		double X = x[i];
		double Y = y[i];
		double Z = z[i];
		if (X == X && Y == Y && Z == Z)
		{
			if (isnan)
			{
				res = X * Y * Z;
				isnan = false;
			}
			else
			{
				res += X * Y * Z;
			}
		}
	}
	return res;
}

extern "C" __declspec(dllexport) void get_cut_level_index(int n, int breakCount, double* breaks, double* numSlice, unsigned short* result)
{
	for (int i = 0; i < n; i++)
	{
		result[i] = findcutindex(breakCount, breaks, numSlice[i]);
	}
}

extern "C" __declspec(dllexport) void get_knot_level_index(int n, int knotCount, int* knots, double* numSlice, unsigned short* result)
{
	for (int i = 0; i < n; i++)
	{
		result[i] = findknotindex(knotCount, knots, numSlice[i]);
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
				else if (index == -2) // NA
				{
					xbeta[i] = nan("");
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
				else if (index == -2) // NA
				{
					xbeta[i] = nan("");
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
				double cov = covariate[i];
				if (cov == cov)
				{
					int index = estimateMap[slices[0][i]];
					if (index >= 0)
					{
						xbeta[i] += offsetBeta[index] * cov;
					}
					else if (index == -2) // NA
					{
						xbeta[i] = nan("");
					}
				}
				else
				{
					xbeta[i] = nan("");
				}
			}
		}
		else
		{
			for (int i = 0; i < n; i++)
			{
				double cov = covariate[i];
				if (cov == cov)
				{
					for (int j = 0; j < k; j++)
					{
						subscripts[j] = slices[j][i];
					}
					int index = estimateMap[sub2ind(k, dimProd, subscripts)];
					if (index >= 0)
					{
						xbeta[i] += offsetBeta[index] * cov;
					}
					else if (index == -2) // NA
					{
						xbeta[i] = nan("");
					}
				}
				else
				{
					xbeta[i] = nan("");
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
				double ui = u[i];
				if (ui == ui)
				{
					int index = estimateMap[slices[0][i]];
					if (index >= 0)
					{
						offsetU[index] += ui;
					}
				}
			}
		}
		else
		{
			for (int i = 0; i < n; i++)
			{
				double ui = u[i];
				if (ui == ui)
				{
					for (int j = 0; j < k; j++)
					{
						subscripts[j] = slices[j][i];
					}
					int index = estimateMap[sub2ind(k, dimProd, subscripts)];
					if (index >= 0)
					{
						offsetU[index] += ui;
					}
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
				double cov = covariate[i];
				double ui = u[i];
				if (ui == ui && cov == cov)
				{
					int index = estimateMap[slices[0][i]];
					if (index >= 0)
					{ 
						offsetU[index] += ui * cov;
					}
				}
			}
		}
		else
		{
			for (int i = 0; i < n; i++)
			{
				double cov = covariate[i];
				double ui = u[i];
				if (ui == ui && cov == cov)
				{
					for (int j = 0; j < k; j++)
					{
						subscripts[j] = slices[j][i];
					}
					int index = estimateMap[sub2ind(k, dimProd, subscripts)];
					if (index >= 0)
					{
						offsetU[index] += ui * cov;
					}
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
	if (rowK == 0 && rowCovariate != nullptr && colK != 0 && colCovariate != nullptr && weight != nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			double w = weight[i];
			double rowCov = rowCovariate[i];
			double colCov = colCovariate[i];
			if (w == w && rowCov == rowCov && colCov == colCov)
			{
				int colIndex;
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
					offsetH[colIndex * p] += w * rowCov * colCov;
				}
			}
		}
	}
	else if (rowK == 0 && rowCovariate != nullptr && colK != 0 && colCovariate != nullptr && weight == nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			double rowCov = rowCovariate[i];
			double colCov = colCovariate[i];
			if (rowCov == rowCov && colCov == colCov)
			{
				int colIndex;
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
					offsetH[colIndex * p] += rowCov * colCov;
				}
			}
		}
	}
	else if (rowK == 0 && rowCovariate != nullptr && colK != 0 && colCovariate == nullptr && weight != nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			double w = weight[i];
			double rowCov = rowCovariate[i];
			if (w == w && rowCov == rowCov)
			{
				int colIndex;
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
					offsetH[colIndex * p] += w * rowCov;
				}
			}
		}
	}
	else if (rowK == 0 && rowCovariate != nullptr && colK != 0 && colCovariate == nullptr && weight == nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			double rowCov = rowCovariate[i];
			if (rowCov == rowCov)
			{
				int colIndex;
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
					offsetH[colIndex * p] += rowCov;
				}
			}
		}
	}
	else if (rowK != 0 && rowCovariate != nullptr && colK == 0 && colCovariate != nullptr && weight != nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			double w = weight[i];
			double rowCov = rowCovariate[i];
			double colCov = colCovariate[i];
			if (w == w && rowCov == rowCov && colCov == colCov)
			{
				int rowIndex;
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
					offsetH[rowIndex] += w * rowCov * colCov;
				}
			}
		}
	}
	else if (rowK != 0 && rowCovariate != nullptr && colK == 0 && colCovariate != nullptr && weight == nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			double rowCov = rowCovariate[i];
			double colCov = colCovariate[i];
			if (rowCov == rowCov && colCov == colCov)
			{
				int rowIndex;
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
					offsetH[rowIndex] += rowCov * colCov;
				}
			}
		}
	}
	else if (rowK != 0 && rowCovariate != nullptr && colK != 0 && colCovariate != nullptr && weight != nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			double w = weight[i];
			double rowCov = rowCovariate[i];
			double colCov = colCovariate[i];
			if (w == w && rowCov == rowCov && colCov == colCov)
			{
				int rowIndex;
				int colIndex;
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
					offsetH[colIndex  * p + rowIndex] += w * rowCov * colCov;
				}
			}

		}
	}
	else if (rowK != 0 && rowCovariate != nullptr && colK != 0 && colCovariate != nullptr && weight == nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			double rowCov = rowCovariate[i];
			double colCov = colCovariate[i];
			if (rowCov == rowCov && colCov == colCov)
			{
				int colIndex;
				int rowIndex;
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
					offsetH[colIndex  * p + rowIndex] += rowCov * colCov;
				}
			}
		}
	}
	else if (rowK != 0 && rowCovariate != nullptr && colK != 0 && colCovariate == nullptr && weight != nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			double w = weight[i];
			double rowCov = rowCovariate[i];
			if (w == w && rowCov == rowCov)
			{
				int colIndex;
				int rowIndex;
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
					offsetH[colIndex  * p + rowIndex] += w * rowCov;
				}
			}
		}
	}
	else if (rowK != 0 && rowCovariate != nullptr && colK != 0 && colCovariate == nullptr && weight == nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			double rowCov = rowCovariate[i];
			if (rowCov == rowCov)
			{
				int colIndex;
				int rowIndex;
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
					offsetH[colIndex  * p + rowIndex] += rowCov;
				}
			}
		}
	}
	else if (rowK != 0 && rowCovariate == nullptr && colK == 0 && colCovariate != nullptr && weight != nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			double w = weight[i];
			double colCov = colCovariate[i];
			if (w == w && colCov == colCov)
			{
				int rowIndex;
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
					offsetH[rowIndex] += w * colCov;
				}
			}
		}
	}
	else if (rowK != 0 && rowCovariate == nullptr && colK == 0 && colCovariate != nullptr && weight == nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			double colCov = colCovariate[i];
			if (colCov == colCov)
			{
				int rowIndex;
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
					offsetH[rowIndex] += colCov;
				}
			}
		}
	}
	else if (rowK != 0 && rowCovariate == nullptr && colK != 0 && colCovariate != nullptr && weight != nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			double w = weight[i];
			double colCov = colCovariate[i];
			if (w == w && colCov == colCov)
			{
				int rowIndex;
				int colIndex;
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
					offsetH[colIndex  * p + rowIndex] += w * colCov;
				}
			}
		}
	}
	else if (rowK != 0 && rowCovariate == nullptr && colK != 0 && colCovariate != nullptr && weight == nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			double colCov = colCovariate[i];
			if (colCov == colCov)
			{
				int rowIndex;
				int colIndex;
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
					offsetH[colIndex  * p + rowIndex] += colCov;
				}
			}
		}
	}
	else if (rowK != 0 && rowCovariate == nullptr && colK != 0 && colCovariate == nullptr && weight != nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			double w = weight[i];
			if (w == w)
			{
				int rowIndex;
				int colIndex;
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
					offsetH[colIndex * p + rowIndex] += w;
				}
			}
		}
	}
	else if (rowK != 0 && rowCovariate == nullptr && colK != 0 && colCovariate == nullptr && weight == nullptr)
	{
		for (int i = 0; i < n; i++)
		{
			int rowIndex;
			int colIndex;
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


