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

template<typename T, typename POTRF>
int cholesky_factor(MKL_INT n, T* x, POTRF potrf)
{
	int info =  potrf(LAPACK_COL_MAJOR, 'U', n, x, n);
	if (info > 0)
	{
		return NOTPOSDEF;
	}
	if (info < 0)
	{
		return MKLARGERROR;
	}
	for (MKL_INT i = 1; i < n; i++)
	{
		for (MKL_INT j = 0; j < i; j++)
		{
			x[j * n + i] = 0;
		}
	}
	return 0;
}

template<typename T, typename POTRF, typename POTRI>
int cholesky_inverse(MKL_INT n, T* x, POTRF potrf, POTRI potri)
{
	int info =  potrf(LAPACK_COL_MAJOR, 'U', n, x, n);
	if (info > 0)
	{
		return NOTPOSDEF;
	}
	if (info < 0)
	{
		return MKLARGERROR;
	}
	info =  potri(LAPACK_COL_MAJOR, 'U', n, x, n);
	if (info > 0)
	{
		return SINGULAR;
	}
	if (info < 0)
	{
		return MKLARGERROR;
	}
	for (MKL_INT i = 1; i < n; i++)
	{
		for (MKL_INT j = 0; j < i; j++)
		{
			x[j * n + i] = x[i * n + j];
		}
	}
	return info;
}

template<typename T, typename POSV>
int cholesky_solve(MKL_INT n, MKL_INT nrhs, T* A, T* B, POSV posv)
{
	int info =  posv(LAPACK_COL_MAJOR, 'U', n, nrhs, A, n, B, n);
	if (info > 0)
	{
		return NOTPOSDEF;
	}
	if (info < 0)
	{
		return MKLARGERROR;
	}
	return 0;
}

template<typename T, typename GETRF>
int lu_factor(MKL_INT m, MKL_INT n, T* L, T* U, int* pivot, GETRF getrf)
{
	int info;
    MKL_INT* ipiv = (MKL_INT*)mkl_malloc((m < n ? m : n)*sizeof(MKL_INT),64);
	if (ipiv == nullptr)
	{
		return OUTOFMEMORY;
	}
	if (m > n)
	{
		info = getrf(LAPACK_COL_MAJOR, m, n, L, m, ipiv);
		if (info < 0)
		{
			return MKLARGERROR;
		}
		for (MKL_INT i = 0; i < n; i++)
		{
			for (MKL_INT j = i; j < n; j++)
			{
				U[j * n + i] = L[j * m + i];
				if (i == j)
				{
					L[j * m + i] = 1;
				}
				else
				{
					L[j * m + i] = 0;
				}
			}
		}
		for (MKL_INT i = 0; i < m; i++)
		{
			pivot[i] = i;
		}
		for (MKL_INT i = 0; i < n; i++)
		{
			int a = pivot[i];
			MKL_INT k = ipiv[i] - 1;
			pivot[i] = pivot[k];
			pivot[k] = a;
		}
		mkl_free(ipiv);
	}
	else
	{
		info = getrf(LAPACK_COL_MAJOR, m, n, U, m, ipiv);
		if (info < 0)
		{
			return MKLARGERROR;
		}
		for (MKL_INT i = 0; i < m; i++)
		{
			for (MKL_INT j = 0; j < i; j++)
			{
				L[j * m + i] = U[j * m + i];
				U[j * m + i] = 0;
			}
			L[i * (m + 1)] = 1;
			for (MKL_INT j = i + 1; j < m; j++)
			{
				L[j * m + i] = 0;
			}
		}
		for (MKL_INT i = 0; i < m; i++)
		{
			pivot[i] = i;
		}
		for (MKL_INT i = 0; i < m; i++)
		{
			int a = pivot[i];
			MKL_INT k = ipiv[i] - 1;
			pivot[i] = pivot[k];
			pivot[k] = a;
		}
		mkl_free(ipiv);
	}
	return 0;
}

template<typename T, typename GETRF, typename GETRI>
int lu_inverse(MKL_INT n, T* x, GETRF getrf, GETRI getri)
{
	MKL_INT* pivot = (MKL_INT*)mkl_malloc(n*sizeof(MKL_INT),64);
	if (pivot == nullptr)
	{
		return OUTOFMEMORY;
	}
	int info =  getrf(LAPACK_COL_MAJOR, n, n, x, n, pivot);
	if (info < 0)
	{
		mkl_free(pivot);
		return MKLARGERROR;
	}
	if (info > 0)
	{
		mkl_free(pivot);
		return SINGULAR;
	}
	info = getri(LAPACK_COL_MAJOR, n, x, n, pivot);
	if (info < 0)
	{
		mkl_free(pivot);
		return MKLARGERROR;
	}
	if (info > 0)
	{
		mkl_free(pivot);
		return SINGULAR;
	}
	mkl_free(pivot);
	return 0;
}

template<typename T, typename GESV>
int lu_solve(MKL_INT n, MKL_INT nrhs, T* A, T* B, GESV gesv)
{
	MKL_INT* pivot = (MKL_INT*)mkl_malloc(n*sizeof(MKL_INT),64);
	if (pivot == nullptr)
	{
		return OUTOFMEMORY;
	}
	int info =  gesv(LAPACK_COL_MAJOR, n, nrhs, A, n, pivot, B, n);
	if (info < 0)
	{
		mkl_free(pivot);
		return MKLARGERROR;
	}
	if (info > 0)
	{
		mkl_free(pivot);
		return SINGULAR;
	}
	mkl_free(pivot);
	return 0;
}

//template<typename T, typename GEQRF>
//int qtau(MKL_INT m, MKL_INT n, T* Q, T* tau, GEQRF geqrf)
//{
//	int info = geqrf(LAPACK_COL_MAJOR, m, n, Q, m, tau);
//	return info;
//}

template<typename T, typename GEQRF, typename ORGQR>
int qr_factor(MKL_INT m, MKL_INT n, T* x, T* Q, T* R, GEQRF geqrf, ORGQR orgqr)
{
	int info;
	if (m >= n)
	{
		T* tau = (T*)mkl_malloc(n*sizeof(T),64);
		if (tau == nullptr)
		{
			return OUTOFMEMORY;
		}
		info = geqrf(LAPACK_COL_MAJOR, m, n, Q, m, tau);
		if (info < 0)
		{
			mkl_free(tau);
			return MKLARGERROR;
		}
		for (MKL_INT i = 0; i < n; i++)
		{
			for (MKL_INT j = i; j < n; j++)
			{
				R[j * n + i] = Q[j * m + i];
			}
		}
		info = orgqr(LAPACK_COL_MAJOR, m, n, n, Q, m, tau);
		if (info < 0)
		{
			mkl_free(tau);
			return MKLARGERROR;
		}
		mkl_free(tau);
	}
	else
	{
		T* qTemp = (T*)mkl_malloc(m*n*sizeof(T),64);
		if (qTemp == nullptr)
		{
			return OUTOFMEMORY;
		}
		memcpy(qTemp, x, m*n*sizeof(T));
		T* tau = (T*)mkl_malloc(m*sizeof(T),64);
		if (tau == nullptr)
		{
			mkl_free(qTemp);
			return OUTOFMEMORY;
		}
		info = geqrf(LAPACK_COL_MAJOR, m, n, qTemp, m, tau);
		if (info < 0)
		{
			mkl_free(qTemp);
			mkl_free(tau);
			return MKLARGERROR;
		}
		for (MKL_INT i = 0; i < m; i++)
		{
			for (MKL_INT j = i; j < n; j++)
			{
				R[j * m + i] = qTemp[j * m + i];
			}
			for (MKL_INT j = 0; j < i; j++)
			{
				R[j * m + i] = 0;
			}
		}
		info = orgqr(LAPACK_COL_MAJOR, m, m, m, qTemp, m, tau);
		if (info < 0)
		{
			mkl_free(qTemp);
			mkl_free(tau);
			return MKLARGERROR;
		}
		memcpy(Q, qTemp, m * m * sizeof(T));
		mkl_free(tau);
		mkl_free(qTemp);
	}
	return info;
}

template<typename T, typename GELSY>
int qr_solve(MKL_INT m, MKL_INT n, MKL_INT nrhs, T* A, T* B, T* x, int* rank, T cond, GELSY gelsy)
{
    T* xTemp = (T*)mkl_malloc(m >= n ? m*nrhs*sizeof(T) : n*nrhs*sizeof(T),64);
	if (xTemp == nullptr)
	{
		return OUTOFMEMORY;
	}
	if (m >= n)
	{
        memcpy(xTemp, B, m*nrhs*sizeof(T));
	}
	else
	{
		for (MKL_INT i = 0; i < nrhs; i++)
		{
			memcpy(xTemp + i * n, B + i * m, m*sizeof(T));
		}
	}
	MKL_INT* jpvt = (MKL_INT*)mkl_calloc(n,sizeof(MKL_INT),64);
	if (jpvt == nullptr)
	{
		mkl_free(xTemp);
		return OUTOFMEMORY;
	}
	MKL_INT r = 0;
	lapack_int info =  gelsy(LAPACK_COL_MAJOR, m, n, nrhs, A, m, xTemp, m > n ? m : n, jpvt, cond, &r);
	if (info < 0)
	{
		mkl_free(xTemp);
		mkl_free(jpvt);
		return MKLARGERROR;
	}
	*rank = r;
	if (m > n)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			for (MKL_INT j = 0; j < nrhs; j++)
			{
				x[j * n + i] = xTemp[j * m + i];
			}
		}
	}
	else
	{
		memcpy(x, xTemp, m > n ? m*nrhs*sizeof(T) : n*nrhs*sizeof(T));
	}
    mkl_free(xTemp);
	mkl_free(jpvt);
	return 0;
}

template<typename T, typename GELS>
int qr_solve_full(MKL_INT m, MKL_INT n, MKL_INT nrhs, T* A, T* B, T* x, GELS gels)
{
    T* xTemp = (T*)mkl_malloc(m > n ? m*nrhs*sizeof(T) : n*nrhs*sizeof(T),64);
	if (xTemp == nullptr)
	{
		return OUTOFMEMORY;
	}
	if (m > n)
	{
        memcpy(xTemp, B, m*nrhs*sizeof(T));
	}
	else
	{
		for (MKL_INT i = 0; i < nrhs; i++)
		{
			memcpy(xTemp + i * n, B + i * m, m*sizeof(T));
		}
	}

	int info =  gels(LAPACK_COL_MAJOR, 'N', m, n, nrhs, A, m, xTemp, m > n ? m : n);
	if (info < 0)
	{
		mkl_free(xTemp);
		return MKLARGERROR;
	}
	if (info > 0)
	{
		mkl_free(xTemp);
		return NOTFULLRANK;
	}
	if (m > n)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			for (MKL_INT j = 0; j < nrhs; j++)
			{
				x[j * n + i] = xTemp[j * m + i];
			}
		}
	}
	else
	{
		memcpy(x, xTemp, m > n ? m*nrhs*sizeof(T) : n*nrhs*sizeof(T));
	}
    mkl_free(xTemp);
	return 0;
}

template<typename T, typename GESVD>
int svd_factor(MKL_INT m, MKL_INT n, T* x, T* U, T* S, T* Vt, GESVD gesvd)
{
	T* superB = (T*)mkl_malloc((m > n ? n  : m )*sizeof(T),64);
	if (superB == nullptr)
	{
		return OUTOFMEMORY;
	}
	int info =  gesvd(LAPACK_COL_MAJOR, 'S', 'S', m, n, x, m, S, U, m, Vt, m < n ? m : n, superB);
	if (info < 0)
	{
		mkl_free(superB);
		return MKLARGERROR;
	}
	if (info > 0)
	{
		mkl_free(superB);
		return NOCONVERGE;
	}
	mkl_free(superB);
	return 0;
}

template<typename T, typename GELSS>
int svd_solve(MKL_INT m, MKL_INT n, MKL_INT nrhs, T* A, T* B, T* x, int* rank, T cond, GELSS gelss)
{
    T* xTemp = (T*)mkl_malloc(m > n ? m*nrhs*sizeof(T) : n*nrhs*sizeof(T),64);
	if (xTemp == nullptr)
	{
		return OUTOFMEMORY;
	}
	if (m > n)
	{
        memcpy(xTemp, B, m*nrhs*sizeof(T));
	}
	else
	{
		for (MKL_INT i = 0; i < nrhs; i++)
		{
			memcpy(xTemp + i * n, B + i * m, m*sizeof(T));
		}
	}

	T* s = (T*)mkl_malloc((m > n ? n : m)*sizeof(T),64);
	if (s == nullptr)
	{
		mkl_free(xTemp);
		return OUTOFMEMORY;
	}
	MKL_INT r = 0;
	int info =  gelss(LAPACK_COL_MAJOR, m, n, nrhs, A, m, xTemp, m > n ? m : n, s, cond, &r);
	if (info < 0)
	{
		mkl_free(xTemp);
		mkl_free(s);
		return MKLARGERROR;
	}
	if (info > 0)
	{
		mkl_free(xTemp);
		mkl_free(s);
		return NOCONVERGE;
	}
	*rank = r;
	if (m > n)
	{
		for (MKL_INT i = 0; i < n; i++)
		{
			for (MKL_INT j = 0; j < nrhs; j++)
			{
				x[j * n + i] = xTemp[j * m + i];
			}
		}
	}
	else
	{
		memcpy(x, xTemp, m > n ? m*nrhs*sizeof(T) : n*nrhs*sizeof(T));
	}
    mkl_free(xTemp);
	mkl_free(s);
	return 0;
}

template<typename T, typename GESVD>
int svd_values(MKL_INT m, MKL_INT n, T* x, T* y, GESVD gesvd)
{
	T* u = 0;
	T* vt = 0;
	T* superB = (T*)mkl_malloc((m > n ? n  : m )*sizeof(T),64);
	if (superB == nullptr)
	{
		return OUTOFMEMORY;
	}
	int info =  gesvd(LAPACK_COL_MAJOR, 'N', 'N', m, n, x, m, y, u, 1, vt, 1, superB);
	if (info < 0)
	{
		mkl_free(superB);
		return MKLARGERROR;
	}
	if (info > 0)
	{
		mkl_free(superB);
		return NOCONVERGE;
	}
	mkl_free(superB);
	return 0;
}

template<typename T, typename SYEVD>
int eigen_factor(MKL_INT n, T* Z, T* D, SYEVD syevd)
{
	char job = 'V';
	char uplo = 'U';
	intptr_t N = n;
	T work = 0;
	intptr_t lwork = -1;
	intptr_t liwork = -1;
	intptr_t iwork = 0;
	intptr_t info = 0;
	syevd(&job, &uplo, &N, Z, &N, D, &work, &lwork, &iwork, &liwork, &info);
	if (info != 0)
	{
		return NOCONVERGE;
	}
	lwork = (intptr_t)work;
	liwork = iwork;
	T* wrk = (T*)mkl_malloc(lwork*sizeof(T),64);
	intptr_t* iwrk = (intptr_t*)mkl_malloc(liwork*sizeof(intptr_t),64);
	syevd(&job, &uplo, &N, Z, &N, D, wrk, &lwork, iwrk, &liwork, &info);
	if (info != 0)
	{
		mkl_free(wrk);
		mkl_free(iwrk);
		return NOCONVERGE;
	}
	mkl_free(wrk);
	mkl_free(iwrk);
	return 0;
}

template<typename T, typename SYEVD>
int eigen_values(MKL_INT n, T* x, T* D, SYEVD syevd)
{
	char job = 'N';
	char uplo = 'U';
	intptr_t N = n;
	T work = 0;
	intptr_t lwork = -1;
	intptr_t liwork = -1;
	intptr_t iwork = 0;
	intptr_t info = 0;
	syevd(&job, &uplo, &N, x, &N, D, &work, &lwork, &iwork, &liwork, &info);
	if (info != 0)
	{
		return NOCONVERGE;
	}
	lwork = (intptr_t)work;
	liwork = iwork;
	T* wrk = (T*)mkl_malloc(lwork*sizeof(T),64);
	intptr_t* iwrk = (intptr_t*)mkl_malloc(liwork*sizeof(intptr_t),64);
	syevd(&job, &uplo, &N, x, &N, D, wrk, &lwork, iwrk, &liwork, &info);
	if (info != 0)
	{
		mkl_free(wrk);
		mkl_free(iwrk);
		return NOCONVERGE;
	}
	mkl_free(wrk);
	mkl_free(iwrk);
	return 0;
}

template<typename T, typename GEMM>
void multiply_matrices(T* x, T* y, T* z, MKL_INT n, MKL_INT m, MKL_INT k, bool trans, GEMM gemm)
{
	char opX = trans ? 'T': 'N';
	char opY = 'N';
	T alpha = 1;
	T beta = 0;
	if (trans)
	{
		gemm(&opX, &opY, &m, &n, &k, &alpha, x, &k, y, &k, &beta, z, &m);
	}
	else
	{
        gemm(&opX, &opY, &m, &n, &k, &alpha, x, &m, y, &k, &beta, z, &m);
	}
}


extern "C" __declspec(dllexport) int d_cholesky_factor(MKL_INT n, double* x)
{
	return cholesky_factor(n, x, LAPACKE_dpotrf);
}

extern "C" __declspec(dllexport) int s_cholesky_factor(MKL_INT n, float* x)
{
	return cholesky_factor(n, x, LAPACKE_spotrf);
}

extern "C" __declspec(dllexport) int d_cholesky_inverse(MKL_INT n, double* x)
{
	return cholesky_inverse(n, x, LAPACKE_dpotrf, LAPACKE_dpotri);
}

extern "C" __declspec(dllexport) int s_cholesky_inverse(MKL_INT n, float* x)
{
	return cholesky_inverse(n, x, LAPACKE_spotrf, LAPACKE_spotri);
}

extern "C" __declspec(dllexport) int d_cholesky_solve(MKL_INT n, MKL_INT nrhs, double* A, double* B)
{
	return cholesky_solve(n, nrhs, A, B, LAPACKE_dposv);
}

extern "C" __declspec(dllexport) int s_cholesky_solve(MKL_INT n, MKL_INT nrhs, float* A, float* B)
{
	return cholesky_solve(n, nrhs, A, B, LAPACKE_sposv);
}

extern "C" __declspec(dllexport) int d_lu_factor(MKL_INT m, MKL_INT n, double* L, double* U, int* pivot)
{
	return lu_factor(m, n, L, U, pivot, LAPACKE_dgetrf);
}

extern "C" __declspec(dllexport) int s_lu_factor(MKL_INT m, MKL_INT n, float* L, float* U, int* pivot)
{
	return lu_factor(m, n, L, U, pivot, LAPACKE_sgetrf);
}

extern "C" __declspec(dllexport) int d_lu_inverse(MKL_INT n, double* x)
{
	return lu_inverse(n, x, LAPACKE_dgetrf, LAPACKE_dgetri);
}

extern "C" __declspec(dllexport) int s_lu_inverse(MKL_INT n, float* x)
{
	return lu_inverse(n, x, LAPACKE_sgetrf, LAPACKE_sgetri);
}

extern "C" __declspec(dllexport) int d_lu_solve(MKL_INT n, MKL_INT nrhs, double* A, double* B)
{
	return lu_solve(n, nrhs, A, B, LAPACKE_dgesv);
}

extern "C" __declspec(dllexport) int s_lu_solve(MKL_INT n, MKL_INT nrhs, float* A, float* B)
{
	return lu_solve(n, nrhs, A, B, LAPACKE_sgesv);
}

//extern "C" __declspec(dllexport) int d_qtau(MKL_INT m, MKL_INT n, double* Q, double* tau)
//{
//	return qtau(m, n, Q, tau, LAPACKE_dgeqrf);
//}
//
//extern "C" __declspec(dllexport) int s_qtau(MKL_INT m, MKL_INT n, float* Q, float* tau)
//{
//	return qtau(m, n, Q, tau, LAPACKE_sgeqrf);
//}

extern "C" __declspec(dllexport) int d_qr_factor(MKL_INT m, MKL_INT n, double* x, double* Q, double* R)
{
	return qr_factor(m, n, x, Q, R, LAPACKE_dgeqrf, LAPACKE_dorgqr);
}

extern "C" __declspec(dllexport) int s_qr_factor(MKL_INT m, MKL_INT n, float* x, float* Q, float* R)
{
	return qr_factor(m, n, x, Q, R, LAPACKE_sgeqrf, LAPACKE_sorgqr);
}

extern "C" __declspec(dllexport) int d_qr_solve(MKL_INT m, MKL_INT n, MKL_INT nrhs, double* A, double* B, double* x, int* rank, double cond)
{
	return qr_solve(m, n, nrhs, A, B, x, rank, cond, LAPACKE_dgelsy);
}

extern "C" __declspec(dllexport) int s_qr_solve(MKL_INT m, MKL_INT n, MKL_INT nrhs, float* A, float* B, float* x, int* rank, float cond)
{
	return qr_solve(m, n, nrhs, A, B, x, rank, cond, LAPACKE_sgelsy);
}

extern "C" __declspec(dllexport) int d_qr_solve_full(MKL_INT m, MKL_INT n, MKL_INT nrhs, double* A, double* B, double* x)
{
	return qr_solve_full(m, n, nrhs, A, B, x, LAPACKE_dgels);
}

extern "C" __declspec(dllexport) int s_qr_solve_full(MKL_INT m, MKL_INT n, MKL_INT nrhs, float* A, float* B, float* x)
{
	return qr_solve_full(m, n, nrhs, A, B, x, LAPACKE_sgels);
}

extern "C" __declspec(dllexport) int d_svd_factor(MKL_INT m, MKL_INT n, double* x, double* U, double* S, double* Vt)
{
	return svd_factor(m, n, x, U, S, Vt, LAPACKE_dgesvd);
}

extern "C" __declspec(dllexport) int s_svd_factor(MKL_INT m, MKL_INT n, float* x, float* U, float* S, float* Vt)
{
	return svd_factor(m, n, x, U, S, Vt, LAPACKE_sgesvd);
}

extern "C" __declspec(dllexport) int d_svd_solve(MKL_INT m, MKL_INT n, MKL_INT nrhs, double* A, double* B, double* x, int* rank, double cond)
{
	return svd_solve(m, n, nrhs, A, B, x, rank, cond, LAPACKE_dgelss);
}

extern "C" __declspec(dllexport) int s_svd_solve(MKL_INT m, MKL_INT n, MKL_INT nrhs, float* A, float* B, float* x, int* rank, float cond)
{
	return svd_solve(m, n, nrhs, A, B, x, rank, cond, LAPACKE_sgelss);
}

extern "C" __declspec(dllexport) int d_svd_values(MKL_INT m, MKL_INT n, double* x, double* y)
{
	return svd_values(m, n, x, y, LAPACKE_dgesvd);
}

extern "C" __declspec(dllexport) int s_svd_values(MKL_INT m, MKL_INT n, float* x, float* y)
{
	return svd_values(m, n, x, y, LAPACKE_sgesvd);
}

extern "C" __declspec(dllexport) int d_eigen_factor(MKL_INT n, double* Z, double* D)
{
	return eigen_factor(n, Z, D, dsyevd);
}

extern "C" __declspec(dllexport) int s_eigen_factor(MKL_INT n, float* Z, float* D)
{
	return eigen_factor(n, Z, D, ssyevd);
}

extern "C" __declspec(dllexport) int d_eigen_values(MKL_INT n, double* x, double* D)
{
	return eigen_values(n, x, D, dsyevd);
}

extern "C" __declspec(dllexport) int s_eigen_values(MKL_INT n, float* x, float* D)
{
	return eigen_values(n, x, D, ssyevd);
}

extern "C" __declspec(dllexport) void d_multiply_matrices(double* x, double* y, double* z, MKL_INT n, MKL_INT m, MKL_INT k, bool trans)
{
	multiply_matrices(x, y, z, n, m, k, trans, dgemm);
}

extern "C" __declspec(dllexport) void s_multiply_matrices(float* x, float* y, float* z, MKL_INT n, MKL_INT m, MKL_INT k, bool trans)
{
	multiply_matrices(x, y, z, n, m, k, trans, sgemm);
}










