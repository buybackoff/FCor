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

extern "C" __declspec(dllexport) int d_min_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, double* x, double* res)
{
	VSLSSTaskPtr task;
	int status;
	MKL_INT xstorage;
	if (byRows)
	{
		xstorage = VSL_SS_MATRIX_STORAGE_COLS;
		for (MKL_INT i = 0; i < varCount; i++)
		{
			if (x[i] != x[i])
			{
				res[i] = INFINITY;
			}
			else
			{
				res[i] = x[i];
			}
		}
	}
	else
	{
		xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
		for (MKL_INT i = 0; i < varCount; i++)
		{
			if (x[i*obsCount] != x[i*obsCount])
			{
				res[i] = INFINITY;
			}
			else
			{
				res[i] = x[i*obsCount];
			}
		}
	}

	status = vsldSSNewTask(&task, &varCount, &obsCount, &xstorage, x, 0, 0);
	status = vsldSSEditTask(task, VSL_SS_ED_MIN, res);
	status = vsldSSCompute(task, VSL_SS_MIN, VSL_SS_METHOD_FAST);
	status = vslSSDeleteTask( &task );

	if (status != 0)
	{
		return VSLERROR;
	}
	if (byRows)
	{
		for (MKL_INT i = 0; i < varCount; i++)
		{
			if (res[i] == INFINITY)
			{
				res[i] = nan("");
				for (MKL_INT j = 0; j < obsCount; j++)
				{
					if (x[j*varCount + i] == INFINITY)
					{
						res[i] = INFINITY;
						break;
					}
				}
			}
		}
	}
	else
	{
		for (MKL_INT i = 0; i < varCount; i++)
		{
			if (res[i] == INFINITY)
			{
				res[i] = nan("");
				for (MKL_INT j = 0; j < obsCount; j++)
				{
					if (x[i*obsCount + j] == INFINITY)
					{
						res[i] = INFINITY;
						break;
					}
				}
			}
		}
	}
	return status;
}

extern "C" __declspec(dllexport) int s_min_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, float* x, float* res)
{
	VSLSSTaskPtr task;
	int status;
	MKL_INT xstorage;
	if (byRows)
	{
		xstorage = VSL_SS_MATRIX_STORAGE_COLS;
	}
	else
	{
		xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
	}

	for (MKL_INT i = 0; i < varCount; i++)
	{
		res[i] = INFINITY;
	}

	status = vslsSSNewTask(&task, &varCount, &obsCount, &xstorage, x, 0, 0);
	status = vslsSSEditTask(task, VSL_SS_ED_MIN, res);
	status = vslsSSCompute(task, VSL_SS_MIN, VSL_SS_METHOD_FAST);
	status = vslSSDeleteTask( &task );

	if (status != 0)
	{
		return VSLERROR;
	}
	for (MKL_INT i = 0; i < varCount; i++)
	{
		if (res[i] == INFINITY)
		{
			res[i] = nan("");
		}
	}
	return status;
}

extern "C" __declspec(dllexport) int d_max_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, double* x, double* res)
{
	VSLSSTaskPtr task;
	int status;
	MKL_INT xstorage;
	if (byRows)
	{
		xstorage = VSL_SS_MATRIX_STORAGE_COLS;
		for (MKL_INT i = 0; i < varCount; i++)
		{
			if (x[i] != x[i])
			{
				res[i] = -INFINITY;
			}
			else
			{
				res[i] = x[i];
			}
		}
	}
	else
	{
		xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
		for (MKL_INT i = 0; i < varCount; i++)
		{
			if (x[i*obsCount] != x[i*obsCount])
			{
				res[i] = -INFINITY;
			}
			else
			{
				res[i] = x[i*obsCount];
			}
		}
	}

	status = vsldSSNewTask(&task, &varCount, &obsCount, &xstorage, x, 0, 0);
	status = vsldSSEditTask(task, VSL_SS_ED_MAX, res);
	status = vsldSSCompute(task, VSL_SS_MAX, VSL_SS_METHOD_FAST);
	status = vslSSDeleteTask( &task );

	if (status != 0)
	{
		return VSLERROR;
	}

	if (byRows)
	{
		for (MKL_INT i = 0; i < varCount; i++)
		{
			if (res[i] == -INFINITY)
			{
				res[i] = nan("");
				for (MKL_INT j = 0; j < obsCount; j++)
				{
					if (x[j*varCount + i] == -INFINITY)
					{
						res[i] = -INFINITY;
						break;
					}
				}
			}
		}
	}
	else
	{
		for (MKL_INT i = 0; i < varCount; i++)
		{
			if (res[i] == -INFINITY)
			{
				res[i] = nan("");
				for (MKL_INT j = 0; j < obsCount; j++)
				{
					if (x[i*obsCount + j] == -INFINITY)
					{
						res[i] = -INFINITY;
						break;
					}
				}
			}
		}
	}
	return status;
}

extern "C" __declspec(dllexport) int s_max_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, float* x, float* res)
{
	VSLSSTaskPtr task;
	int status;
	MKL_INT xstorage;
	if (byRows)
	{
		xstorage = VSL_SS_MATRIX_STORAGE_COLS;
	}
	else
	{
		xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
	}

	for (MKL_INT i = 0; i < varCount; i++)
	{
		res[i] = -INFINITY;
	}

	status = vslsSSNewTask(&task, &varCount, &obsCount, &xstorage, x, 0, 0);
	status = vslsSSEditTask(task, VSL_SS_ED_MAX, res);
	status = vslsSSCompute(task, VSL_SS_MAX, VSL_SS_METHOD_FAST);
	status = vslSSDeleteTask( &task );

	if (status != 0)
	{
		return VSLERROR;
	}
	for (MKL_INT i = 0; i < varCount; i++)
	{
		if (res[i] == -INFINITY)
		{
			res[i] = nan("");
		}
	}
	return status;
}

extern "C" __declspec(dllexport) int d_mean_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, double* x, double* res)
{
	VSLSSTaskPtr task;
	int status;
	MKL_INT xstorage;
	if (byRows)
	{
		xstorage = VSL_SS_MATRIX_STORAGE_COLS;
	}
	else
	{
		xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
	}

	status = vsldSSNewTask(&task, &varCount, &obsCount, &xstorage, x, 0, 0);
	status = vsldSSEditTask(task, VSL_SS_ED_MEAN, res);
	status = vsldSSCompute(task, VSL_SS_MEAN, VSL_SS_METHOD_FAST);
	status = vslSSDeleteTask( &task );

	if (status != 0)
	{
		return VSLERROR;
	}
	return status;
}

extern "C" __declspec(dllexport) int s_mean_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, float* x, float* res)
{
	VSLSSTaskPtr task;
	int status;
	MKL_INT xstorage;
	if (byRows)
	{
		xstorage = VSL_SS_MATRIX_STORAGE_COLS;
	}
	else
	{
		xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
	}

	status = vslsSSNewTask(&task, &varCount, &obsCount, &xstorage, x, 0, 0);
	status = vslsSSEditTask(task, VSL_SS_ED_MEAN, res);
	status = vslsSSCompute(task, VSL_SS_MEAN, VSL_SS_METHOD_FAST);
	status = vslSSDeleteTask( &task );

	if (status != 0)
	{
		return VSLERROR;
	}
	return status;
}

extern "C" __declspec(dllexport) int d_variance_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, double* x, double* res)
{
	VSLSSTaskPtr task;
	int status;
	MKL_INT xstorage;
	double* mean;
	double* raw2;
	mean = (double*)malloc(varCount*sizeof(double));
	raw2 = (double*)malloc(varCount*sizeof(double));
	if (raw2 == nullptr || mean == nullptr)
	{
		free(raw2);
		free(mean);
		return OUTOFMEMORY;
	}
	if (byRows)
	{
		xstorage = VSL_SS_MATRIX_STORAGE_COLS;
	}
	else
	{
		xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
	}

	status = vsldSSNewTask(&task, &varCount, &obsCount, &xstorage, x, 0, 0);
	status = vsldSSEditTask(task, VSL_SS_ED_MEAN, mean);
	status = vsldSSEditTask(task, VSL_SS_ED_2R_MOM, raw2);
	status = vsldSSEditTask(task, VSL_SS_ED_2C_MOM, res);
	status = vsldSSCompute(task, VSL_SS_2C_MOM, VSL_SS_METHOD_FAST);
	status = vslSSDeleteTask( &task );

	free(mean);
	free(raw2);

	if (status != 0)
	{
		return VSLERROR;
	}
	return status;
}

extern "C" __declspec(dllexport) int s_variance_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, float* x, float* res)
{
	VSLSSTaskPtr task;
	int status;
	MKL_INT xstorage;
	float* mean;
	float* raw2;
	mean = (float*)malloc(varCount*sizeof(float));
	raw2 = (float*)malloc(varCount*sizeof(float));
	if (raw2 == nullptr || mean == nullptr)
	{
		free(raw2);
		free(mean);
		return OUTOFMEMORY;
	}
	if (byRows)
	{
		xstorage = VSL_SS_MATRIX_STORAGE_COLS;
	}
	else
	{
		xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
	}

	status = vslsSSNewTask(&task, &varCount, &obsCount, &xstorage, x, 0, 0);
	status = vslsSSEditTask(task, VSL_SS_ED_MEAN, mean);
	status = vslsSSEditTask(task, VSL_SS_ED_2R_MOM, raw2);
	status = vslsSSEditTask(task, VSL_SS_ED_2C_MOM, res);
	status = vslsSSCompute(task, VSL_SS_2C_MOM, VSL_SS_METHOD_FAST);
	status = vslSSDeleteTask(&task);

	free(mean);
	free(raw2);


	if (status != 0)
	{
		return VSLERROR;
	}
	return status;
}

extern "C" __declspec(dllexport) int d_skewness_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, double* x, double* res)
{
	double factor = pow((double)obsCount / (obsCount - 1), 1.5); 
	VSLSSTaskPtr task;
	double* c2;
	double* c3;
	double* r2;
	double* r3;
	double* mean;
	c2 = (double*)malloc(varCount*sizeof(double));
	c3 = (double*)malloc(varCount*sizeof(double));
	r2 = (double*)malloc(varCount*sizeof(double));
	r3 = (double*)malloc(varCount*sizeof(double));
	mean = (double*)malloc(varCount*sizeof(double));
	if (c2 == nullptr || c3 == nullptr || r2 == nullptr || r3 == nullptr || mean == nullptr)
	{
		free(c2);
		free(c3);
		free(r2);
		free(r3);
		free(mean);
		return OUTOFMEMORY;
	}

	int status;
	MKL_INT xstorage;
	if (byRows)
	{
		xstorage = VSL_SS_MATRIX_STORAGE_COLS;
	}
	else
	{
		xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
	}

	status = vsldSSNewTask(&task, &varCount, &obsCount, &xstorage, x, 0, 0);
	status = vsldSSEditTask(task, VSL_SS_ED_MEAN, mean );
	status = vsldSSEditTask(task, VSL_SS_ED_2C_MOM, c2);
	status = vsldSSEditTask(task, VSL_SS_ED_3C_MOM, c3);
	status = vsldSSEditTask(task, VSL_SS_ED_2R_MOM, r2 );
	status = vsldSSEditTask(task, VSL_SS_ED_3R_MOM, r3 );
	status = vsldSSEditTask(task, VSL_SS_ED_SKEWNESS, res);
	status = vsldSSCompute(task, VSL_SS_MEAN | VSL_SS_2R_MOM | VSL_SS_3R_MOM | VSL_SS_2C_MOM | VSL_SS_3C_MOM | VSL_SS_SKEWNESS, VSL_SS_METHOD_FAST);
	status = vslSSDeleteTask(&task);

	for (MKL_INT i = 0; i < varCount; i++)
	{
		res[i] *= factor;
	}

	free(mean);
	free(c2);
	free(c3);
	free(r2);
	free(r3);
	if (status != 0)
	{
		return VSLERROR;
	}
	return status;
}

extern "C" __declspec(dllexport) int s_skewness_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, float* x, float* res)
{
	double factor = pow((double)obsCount / double(obsCount - 1), 1.5);
	VSLSSTaskPtr task;
	float* c2;
	float* c3;
	float* r2;
	float* r3;
	float* mean;
	c2 = (float*)malloc(varCount*sizeof(float));
	c3 = (float*)malloc(varCount*sizeof(float));
	r2 = (float*)malloc(varCount*sizeof(float));
	r3 = (float*)malloc(varCount*sizeof(float));
	mean = (float*)malloc(varCount*sizeof(float));
	if (c2 == nullptr || c3 == nullptr || r2 == nullptr || r3 == nullptr || mean == nullptr)
	{
		free(c2);
		free(c3);
		free(r2);
		free(r3);
		free(mean);
		return OUTOFMEMORY;
	}

	int status;
	MKL_INT xstorage;
	if (byRows)
	{
		xstorage = VSL_SS_MATRIX_STORAGE_COLS;
	}
	else
	{
		xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
	}

	status = vslsSSNewTask(&task, &varCount, &obsCount, &xstorage, x, 0, 0);
	status = vslsSSEditTask(task, VSL_SS_ED_MEAN, mean);
	status = vslsSSEditTask(task, VSL_SS_ED_2C_MOM, c2);
	status = vslsSSEditTask(task, VSL_SS_ED_3C_MOM, c3);
	status = vslsSSEditTask(task, VSL_SS_ED_2R_MOM, r2);
	status = vslsSSEditTask(task, VSL_SS_ED_3R_MOM, r3);
	status = vslsSSEditTask(task, VSL_SS_ED_SKEWNESS, res);
	status = vslsSSCompute(task, VSL_SS_MEAN | VSL_SS_2R_MOM | VSL_SS_3R_MOM | VSL_SS_2C_MOM | VSL_SS_3C_MOM | VSL_SS_SKEWNESS, VSL_SS_METHOD_FAST);
	status = vslSSDeleteTask(&task);

	for (MKL_INT i = 0; i < varCount; i++)
	{
		res[i] *= factor;
	}

	free(mean);
	free(c2);
	free(c3);
	free(r2);
	free(r3);
	if (status != 0)
	{
		return VSLERROR;
	}
	return status;
}

extern "C" __declspec(dllexport) int d_kurtosis_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, double* x, double* res)
{
	VSLSSTaskPtr task;
	double* c2;
	double* c3;
	double* c4;
	double* r2;
	double* r3;
	double* r4;
	double* mean;

	c2 = (double*)malloc(varCount*sizeof(double));
	c3 = (double*)malloc(varCount*sizeof(double));
	c4 = (double*)malloc(varCount*sizeof(double));
	r2 = (double*)malloc(varCount*sizeof(double));
	r3 = (double*)malloc(varCount*sizeof(double));
	r4 = (double*)malloc(varCount*sizeof(double));
	mean = (double*)malloc(varCount*sizeof(double));
	if (c2 == nullptr || c3 == nullptr || c4 == nullptr || r2 == nullptr || r3 == nullptr || r4 == nullptr || mean == nullptr)
	{
		free(c2);
		free(c3);
		free(c4);
		free(r2);
		free(r3);
		free(r4);
		free(mean);
		return OUTOFMEMORY;
	}

	int status;
	MKL_INT xstorage;
	if (byRows)
	{
		xstorage = VSL_SS_MATRIX_STORAGE_COLS;
	}
	else
	{
		xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
	}

	status = vsldSSNewTask(&task, &varCount, &obsCount, &xstorage, x, 0, 0);
	status = vsldSSEditTask(task, VSL_SS_ED_MEAN, mean );
	status = vsldSSEditTask(task, VSL_SS_ED_2C_MOM, c2);
	status = vsldSSEditTask(task, VSL_SS_ED_3C_MOM, c3);
	status = vsldSSEditTask(task, VSL_SS_ED_4C_MOM, c4);
	status = vsldSSEditTask(task, VSL_SS_ED_2R_MOM, r2 );
	status = vsldSSEditTask(task, VSL_SS_ED_3R_MOM, r3 );
	status = vsldSSEditTask(task, VSL_SS_ED_4R_MOM, r4 );
	status = vsldSSEditTask(task, VSL_SS_ED_KURTOSIS, res);
	status = vsldSSCompute(task, VSL_SS_MEAN | VSL_SS_2R_MOM | VSL_SS_3R_MOM | VSL_SS_4R_MOM | VSL_SS_2C_MOM | VSL_SS_3C_MOM | VSL_SS_4C_MOM | VSL_SS_KURTOSIS, VSL_SS_METHOD_FAST);
	status = vslSSDeleteTask( &task );

	if (obsCount == 2)
	{
		for (MKL_INT i = 0; i < varCount; i++)
		{
			res[i] = 12.0 + res[i] * 4.0;
		}
	}
	else
	{
		double N = (double)obsCount;
		double tmp = N/(N-1.0);
		double factor = tmp*tmp;
		for (MKL_INT i = 0; i < varCount; i++)
		{
			res[i] = (3.0+res[i]) * factor;
		}
	}


	free(mean);
	free(c2);
	free(c3);
	free(c4);
	free(r2);
	free(r3);
	free(r4);
	if (status != 0)
	{
		return VSLERROR;
	}
	return status;
}

extern "C" __declspec(dllexport) int s_kurtosis_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, float* x, float* res)
{
	VSLSSTaskPtr task;
	float* c2;
	float* c3;
	float* c4;
	float* r2;
	float* r3;
	float* r4;
	float* mean;

	c2 = (float*)malloc(varCount*sizeof(float));
	c3 = (float*)malloc(varCount*sizeof(float));
	c4 = (float*)malloc(varCount*sizeof(float));
	r2 = (float*)malloc(varCount*sizeof(float));
	r3 = (float*)malloc(varCount*sizeof(float));
	r4 = (float*)malloc(varCount*sizeof(float));
	mean = (float*)malloc(varCount*sizeof(float));
	if (c2 == nullptr || c3 == nullptr || c4 == nullptr || r2 == nullptr || r3 == nullptr || r4 == nullptr || mean == nullptr)
	{
		free(c2);
		free(c3);
		free(c4);
		free(r2);
		free(r3);
		free(r4);
		free(mean);
		return OUTOFMEMORY;
	}

	int status;
	MKL_INT xstorage;
	if (byRows)
	{
		xstorage = VSL_SS_MATRIX_STORAGE_COLS;
	}
	else
	{
		xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
	}

	status = vslsSSNewTask(&task, &varCount, &obsCount, &xstorage, x, 0, 0);
	status = vslsSSEditTask(task, VSL_SS_ED_MEAN, mean);
	status = vslsSSEditTask(task, VSL_SS_ED_2C_MOM, c2);
	status = vslsSSEditTask(task, VSL_SS_ED_3C_MOM, c3);
	status = vslsSSEditTask(task, VSL_SS_ED_4C_MOM, c4);
	status = vslsSSEditTask(task, VSL_SS_ED_2R_MOM, r2);
	status = vslsSSEditTask(task, VSL_SS_ED_3R_MOM, r3);
	status = vslsSSEditTask(task, VSL_SS_ED_4R_MOM, r4);
	status = vslsSSEditTask(task, VSL_SS_ED_KURTOSIS, res);
	status = vslsSSCompute(task, VSL_SS_MEAN | VSL_SS_2R_MOM | VSL_SS_3R_MOM | VSL_SS_4R_MOM | VSL_SS_2C_MOM | VSL_SS_3C_MOM | VSL_SS_4C_MOM | VSL_SS_KURTOSIS, VSL_SS_METHOD_FAST);
	status = vslSSDeleteTask(&task);

	double tmp = (double)obsCount / (obsCount - 1);
	double factor = tmp*tmp;
	for (MKL_INT i = 0; i < varCount; i++)
	{
		res[i] = (3 + res[i]) * factor;
	}

	free(mean);
	free(c2);
	free(c3);
	free(c4);
	free(r2);
	free(r3);
	free(r4);
	if (status != 0)
	{
		return VSLERROR;
	}
	return status;
}

extern "C" __declspec(dllexport) int d_sum_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, double* x, double* res)
{
	VSLSSTaskPtr task;
	int status;
	MKL_INT xstorage;
	if (byRows)
	{
		xstorage = VSL_SS_MATRIX_STORAGE_COLS;
	}
	else
	{
		xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
	}

	status = vsldSSNewTask(&task, &varCount, &obsCount, &xstorage, x, 0, 0);
	status = vsldSSEditTask(task, VSL_SS_ED_SUM, res);
	status = vsldSSCompute(task, VSL_SS_SUM, VSL_SS_METHOD_FAST);
	status = vslSSDeleteTask( &task );

	if (status != 0)
	{
		return VSLERROR;
	}
	return status;
}

extern "C" __declspec(dllexport) int s_sum_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, float* x, float* res)
{
	VSLSSTaskPtr task;
	int status;
	MKL_INT xstorage;
	if (byRows)
	{
		xstorage = VSL_SS_MATRIX_STORAGE_COLS;
	}
	else
	{
		xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
	}

	status = vslsSSNewTask(&task, &varCount, &obsCount, &xstorage, x, 0, 0);
	status = vslsSSEditTask(task, VSL_SS_ED_SUM, res);
	status = vslsSSCompute(task, VSL_SS_SUM, VSL_SS_METHOD_FAST);
	status = vslSSDeleteTask( &task );

	if (status != 0)
	{
		return VSLERROR;
	}
	return status;
}

extern "C" __declspec(dllexport) void b_any_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, bool* x, bool* res)
{
	if (byRows)
	{
		memcpy(res, x, varCount);
		for (MKL_INT i = 1; i < obsCount; i++)
		{
			for (MKL_INT j = 0; j < varCount; j++)
			{
				res[j] = res[j] || x[i * varCount + j];
			}
		}
	}
	else
	{
		for (MKL_INT i = 0; i < varCount; i++)
		{
			res[i] = x[i*obsCount];
			for (MKL_INT j = 1; j < obsCount; j++)
			{
				res[i] = res[i] || x[i*obsCount + j];
			}
		}
	}
}

extern "C" __declspec(dllexport) void b_all_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, bool* x, bool* res)
{
	if (byRows)
	{
		memcpy(res, x, varCount);
		for (MKL_INT i = 1; i < obsCount; i++)
		{
			for (MKL_INT j = 0; j < varCount; j++)
			{
				res[j] = res[j] && x[i * varCount + j];
			}
		}
	}
	else
	{
		for (MKL_INT i = 0; i < varCount; i++)
		{
			res[i] = x[i*obsCount];
			for (MKL_INT j = 1; j < obsCount; j++)
			{
				res[i] = res[i] && x[i*obsCount + j];
			}
		}
	}
}

extern "C" __declspec(dllexport) void d_prod_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, double* x, double* res)
{
	if (byRows)
	{
		MKL_INT inc = 1;
		dcopy(&varCount, x, &inc, res, &inc);
		for (MKL_INT i = 1; i < obsCount; i++)
		{
			vdMul(varCount, res, x + i * varCount, res);
		}
	}
	else
	{
		for (MKL_INT i = 0; i < varCount; i++)
		{
			res[i] = x[i*obsCount];
			for (MKL_INT j = 1; j < obsCount; j++)
			{
				res[i] *= x[i*obsCount + j];
			}
		}
	}
}

extern "C" __declspec(dllexport) void s_prod_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, float* x, float* res)
{
	if (byRows)
	{
		MKL_INT inc = 1;
		scopy(&varCount, x, &inc, res, &inc);
		for (MKL_INT i = 1; i < obsCount; i++)
		{
			vsMul(varCount, res, x + i * varCount, res);
		}
	}
	else
	{
		for (MKL_INT i = 0; i < varCount; i++)
		{
			res[i] = x[i*obsCount];
			for (MKL_INT j = 1; j < obsCount; j++)
			{
				res[i] *= x[i*obsCount + j];
			}
		}
	}
}

extern "C" __declspec(dllexport) void d_cumsum_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, double* x, double* res)
{
	if (byRows)
	{
		MKL_INT inc = 1;
		dcopy(&varCount, x, &inc, res, &inc);
		for (MKL_INT i = 1; i < obsCount; i++)
		{
			vdAdd(varCount, res + (i-1) * varCount, x + i * varCount, res + i * varCount);
		}
	}
	else
	{
		for (MKL_INT i = 0; i < varCount; i++)
		{
			res[i*obsCount] = x[i*obsCount];
			for (MKL_INT j = 1; j < obsCount; j++)
			{
				res[i*obsCount + j] = res[i*obsCount + j - 1] + x[i*obsCount + j];
			}
		}
	}
}

extern "C" __declspec(dllexport) void s_cumsum_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, float* x, float* res)
{
	if (byRows)
	{
		MKL_INT inc = 1;
		scopy(&varCount, x, &inc, res, &inc);
		for (MKL_INT i = 1; i < obsCount; i++)
		{
			vsAdd(varCount, res + (i-1) * varCount, x + i * varCount, res + i * varCount);
		}
	}
	else
	{
		for (MKL_INT i = 0; i < varCount; i++)
		{
			res[i*obsCount] = x[i*obsCount];
			for (MKL_INT j = 1; j < obsCount; j++)
			{
				res[i*obsCount + j] = res[i*obsCount + j - 1] + x[i*obsCount + j];
			}
		}
	}
}

extern "C" __declspec(dllexport) void d_cumprod_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, double* x, double* res)
{
	if (byRows)
	{
		MKL_INT inc = 1;
		dcopy(&varCount, x, &inc, res, &inc);
		for (MKL_INT i = 1; i < obsCount; i++)
		{
			vdMul(varCount, res + (i-1) * varCount, x + i * varCount, res + i * varCount);
		}
	}
	else
	{
		for (MKL_INT i = 0; i < varCount; i++)
		{
			res[i*obsCount] = x[i*obsCount];
			for (MKL_INT j = 1; j < obsCount; j++)
			{
				res[i*obsCount + j] = res[i*obsCount + j - 1] * x[i*obsCount + j];
			}
		}
	}
}

extern "C" __declspec(dllexport) void s_cumprod_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, float* x, float* res)
{
	if (byRows)
	{
		MKL_INT inc = 1;
		scopy(&varCount, x, &inc, res, &inc);
		for (MKL_INT i = 1; i < obsCount; i++)
		{
			vsMul(varCount, res + (i-1) * varCount, x + i * varCount, res + i * varCount);
		}
	}
	else
	{
		for (MKL_INT i = 0; i < varCount; i++)
		{
			res[i*obsCount] = x[i*obsCount];
			for (MKL_INT j = 1; j < obsCount; j++)
			{
				res[i*obsCount + j] = res[i*obsCount + j - 1] * x[i*obsCount + j];
			}
		}
	}
}

extern "C" __declspec(dllexport) int d_cov_matrix(MKL_INT varCount, MKL_INT obsCount, double* x, double* res)
{
	intptr_t xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
	VSLSSTaskPtr task;
	double* mean = (double*)calloc(varCount, sizeof(double));
	if (mean == nullptr)
	{
		return OUTOFMEMORY;
	}
	int status = vsldSSNewTask( &task, &varCount, &obsCount, &xstorage, x, 0, 0 );
	MKL_INT covStorage = VSL_SS_MATRIX_STORAGE_FULL;
	status = vsldSSEditCovCor(task, mean, res, &covStorage, 0, 0 );
	status = vsldSSCompute(task, VSL_SS_COV, VSL_SS_METHOD_FAST );
	status = vslSSDeleteTask( &task );
	free(mean);
	if (status != 0)
	{
		return VSLERROR;
	}
	return status;
}

extern "C" __declspec(dllexport) int s_cov_matrix(MKL_INT varCount, MKL_INT obsCount, float* x, float* res)
{
	intptr_t xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
	VSLSSTaskPtr task;
	float* mean = (float*)malloc(varCount*sizeof(float));
	if (mean == nullptr)
	{
		return OUTOFMEMORY;
	}
	int status = vslsSSNewTask( &task, &varCount, &obsCount, &xstorage, x, 0, 0 );
	MKL_INT covStorage = VSL_SS_MATRIX_STORAGE_FULL;
	status = vslsSSEditCovCor(task, mean, res, &covStorage, 0, 0 );
	status = vslsSSCompute(task, VSL_SS_COV, VSL_SS_METHOD_FAST );
	status = vslSSDeleteTask( &task );
	free(mean);
	if (status != 0)
	{
		return VSLERROR;
	}
	return status;
}

extern "C" __declspec(dllexport) int d_corr_matrix(MKL_INT varCount, MKL_INT obsCount, double* x, double* res)
{
	intptr_t xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
	VSLSSTaskPtr task;
	double* mean = (double*)calloc(varCount, sizeof(double));
	double* cov = (double*)calloc(varCount*varCount, sizeof(double));
	if (cov == nullptr || mean == nullptr)
	{
		free(cov);
		free(mean);
		return OUTOFMEMORY;
	}
	int status = vsldSSNewTask( &task, &varCount, &obsCount, &xstorage, x, 0, 0 );
	MKL_INT corrStorage = VSL_SS_MATRIX_STORAGE_FULL;
	status = vsldSSEditCovCor(task, mean, cov, &corrStorage, res, &corrStorage);
	status = vsldSSCompute(task, VSL_SS_COV|VSL_SS_COR, VSL_SS_METHOD_FAST);
	status = vslSSDeleteTask( &task );
	free(mean);
	free(cov);
	if (status != 0)
	{
		return VSLERROR;
	}
	MKL_INT step = varCount + 1;
	for (MKL_INT i = 0; i < varCount; i++)
	{
		if (res[i * step] == 0.0)
		{
			res[i * step] = nan("");
		}
		else
		{
		    res[i * step] = 1.0;
		}
	}
	return status;
}

extern "C" __declspec(dllexport) int s_corr_matrix(MKL_INT varCount, MKL_INT obsCount, float* x, float* res)
{
	intptr_t xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
	VSLSSTaskPtr task;
	float* mean = (float*)malloc(varCount*sizeof(float));
	if (mean == nullptr)
	{
		return OUTOFMEMORY;
	}
	float* cov = (float*)malloc(varCount*varCount*sizeof(float));
	if (cov == nullptr)
	{
		return OUTOFMEMORY;
	}
	int status = vslsSSNewTask( &task, &varCount, &obsCount, &xstorage, x, 0, 0 );
	MKL_INT covStorage = VSL_SS_MATRIX_STORAGE_FULL;
	status = vslsSSEditCovCor(task, mean, cov, &covStorage, res, &covStorage);
	status = vslsSSCompute(task, VSL_SS_COR, VSL_SS_METHOD_FAST );
	status = vslSSDeleteTask( &task );
	free(mean);
	free(cov);
	if (status != 0)
	{
		return VSLERROR;
	}
	return status;
}

extern "C" __declspec(dllexport) int d_quantiles_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, MKL_INT qCount, double* x, double* q, double* res)
{
	for (MKL_INT i = 0; i < qCount; i++)
	{
		if (q[i] < 0 || q[i] > 1)
		{
			return VSLERROR;
		}
	}

	VSLSSTaskPtr task;
	int status;
	MKL_INT xstorage;
	if (byRows)
	{
		xstorage = VSL_SS_MATRIX_STORAGE_COLS;
	}
	else
	{
		xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
	}
	status = vsldSSNewTask( &task, &varCount, &obsCount, &xstorage, x, 0, 0 );
	status = vsldSSEditQuantiles( task, &qCount, q, res, 0, &xstorage );
	status = vsldSSCompute(task, VSL_SS_QUANTS, VSL_SS_METHOD_FAST );
	status = vslSSDeleteTask( &task );

	double a = 1;
	if (byRows)
	{
		mkl_dimatcopy('C', 'T', qCount, varCount, a, res, qCount, varCount);
	}

	if (status != 0)
	{
		return VSLERROR;
	}
	return status;
}

extern "C" __declspec(dllexport) int s_quantiles_matrix(bool byRows, MKL_INT varCount, MKL_INT obsCount, MKL_INT qCount, float* x, float* q, float* res)
{
	for (MKL_INT i = 0; i < qCount; i++)
	{
		if (q[i] < 0 || q[i] > 1)
		{
			return VSLERROR;
		}
	}

	VSLSSTaskPtr task;
	int status;
	MKL_INT xstorage;
	if (byRows)
	{
		xstorage = VSL_SS_MATRIX_STORAGE_COLS;
	}
	else
	{
		xstorage = VSL_SS_MATRIX_STORAGE_ROWS;
	}
	status = vslsSSNewTask( &task, &varCount, &obsCount, &xstorage, x, 0, 0 );
	status = vslsSSEditQuantiles( task, &qCount, q, res, 0, &xstorage );
	status = vslsSSCompute(task, VSL_SS_QUANTS, VSL_SS_METHOD_FAST );
	status = vslSSDeleteTask( &task );

	if (status != 0)
	{
		return VSLERROR;
	}
	return status;
}
