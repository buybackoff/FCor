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

#define MCG31 0
#define R250 1
#define MRG32K3A 2
#define MCG59 3
#define WH 4
#define MT19937 5
#define MT2203 6
#define SFMT19937 7
#define SOBOL 8
#define NIEDERR 9

extern "C" __declspec(dllexport) void* create_rng(int brng, unsigned int* seed, int n, int subStream, int quasiDims)
{
    VSLStreamStatePtr streamPtr;
	unsigned int s[1];
	switch (brng)
	{
		case MCG31:
			vslNewStreamEx(&streamPtr, VSL_BRNG_MCG31 + subStream, n, seed);
			break;
		case R250:
			vslNewStreamEx(&streamPtr, VSL_BRNG_R250 + subStream, n, seed);
			break;
		case MRG32K3A:
			vslNewStreamEx(&streamPtr, VSL_BRNG_MRG32K3A + subStream, n, seed);
			break;
		case MCG59:
			vslNewStreamEx(&streamPtr, VSL_BRNG_MCG59 + subStream, n, seed);
			break;
		case WH:
			vslNewStreamEx(&streamPtr, VSL_BRNG_WH + subStream, n, seed);
			break;
		case MT19937:
			vslNewStreamEx(&streamPtr, VSL_BRNG_MT19937 + subStream, n, seed);
			break;
		case MT2203:
			vslNewStreamEx(&streamPtr, VSL_BRNG_MT2203 + subStream, n, seed);
			break;
		case SFMT19937:
			vslNewStreamEx(&streamPtr, VSL_BRNG_SFMT19937 + subStream, n, seed);
			break;
		case SOBOL:
			s[0] = quasiDims;
			n = 1;
			vslNewStreamEx(&streamPtr, VSL_BRNG_SOBOL, n, &s[0]);
			break;
		case NIEDERR:
			s[0] = quasiDims;
			n = 1;
			vslNewStreamEx(&streamPtr, VSL_BRNG_NIEDERR, n, &s[0]);
			break;
		default:
			return nullptr;
	}
	return streamPtr;
}

extern "C" __declspec(dllexport) void* copy_rng(VSLStreamStatePtr streamPtr)
{
	VSLStreamStatePtr copyStreamPtr;
	vslCopyStream(&copyStreamPtr, streamPtr);
	return copyStreamPtr;
}

extern "C" __declspec(dllexport) int skipahead_rng(VSLStreamStatePtr streamPtr, __int64 nskip)
{
	return vslSkipAheadStream(streamPtr, nskip);
}

extern "C" __declspec(dllexport) int leapfrog_rng(VSLStreamStatePtr streamPtr, int k, int nstreams)
{
	return vslLeapfrogStream(streamPtr, k, nstreams);
}

extern "C" __declspec(dllexport) void delete_rng(VSLStreamStatePtr streamPtr)
{
	vslDeleteStream(&streamPtr);
}

extern "C" __declspec(dllexport) int d_uniform_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, double a, double b, double* x)
{
	int status;
	status = vdRngUniform(VSL_RNG_METHOD_UNIFORM_STD_ACCURATE, streamPtr, n, x, a, b);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int s_uniform_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, float a, float b, float* x)
{
	int status;
	status = vsRngUniform(VSL_RNG_METHOD_UNIFORM_STD_ACCURATE, streamPtr, n, x, a, b);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int d_norm_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, double mean, double sigma, double* x)
{
	int status;
	status = vdRngGaussian(VSL_RNG_METHOD_GAUSSIAN_BOXMULLER, streamPtr, n, x, mean, sigma);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int s_norm_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, float mean, float sigma, float* x)
{
	int status;
	status = vsRngGaussian(VSL_RNG_METHOD_GAUSSIAN_BOXMULLER, streamPtr, n, x, mean, sigma);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int d_expon_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, double alpha, double beta, double* x)
{
	int status;
	status = vdRngExponential(VSL_RNG_METHOD_EXPONENTIAL_ICDF_ACCURATE, streamPtr, n, x, alpha, beta);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int s_expon_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, float alpha, float beta, float* x)
{
	int status;
	status = vsRngExponential(VSL_RNG_METHOD_EXPONENTIAL_ICDF_ACCURATE, streamPtr, n, x, alpha, beta);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int d_laplace_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, double alpha, double beta, double* x)
{
	int status;
	status = vdRngLaplace(VSL_RNG_METHOD_LAPLACE_ICDF, streamPtr, n, x, alpha, beta);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int s_laplace_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, float alpha, float beta, float* x)
{
	int status;
	status = vsRngLaplace(VSL_RNG_METHOD_LAPLACE_ICDF, streamPtr, n, x, alpha, beta);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int d_weibull_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, double a, double alpha, double beta, double* x)
{
	int status;
	status = vdRngWeibull(VSL_RNG_METHOD_WEIBULL_ICDF_ACCURATE, streamPtr, n, x, alpha, a, beta);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int s_weibull_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, float a, float alpha, float beta, float* x)
{
	int status;
	status = vsRngWeibull(VSL_RNG_METHOD_WEIBULL_ICDF_ACCURATE, streamPtr, n, x, alpha, a, beta);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int d_cauchy_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, double alpha, double beta, double* x)
{
	int status;
	status = vdRngCauchy(VSL_RNG_METHOD_LAPLACE_ICDF, streamPtr, n, x, alpha, beta);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int s_cauchy_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, float alpha, float beta, float* x)
{
	int status;
	status = vsRngCauchy(VSL_RNG_METHOD_LAPLACE_ICDF, streamPtr, n, x, alpha, beta);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int d_rayleigh_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, double alpha, double beta, double* x)
{
	int status;
	status = vdRngRayleigh(VSL_RNG_METHOD_RAYLEIGH_ICDF_ACCURATE, streamPtr, n, x, alpha, beta);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int s_rayleigh_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, float alpha, float beta, float* x)
{
	int status;
	status = vsRngRayleigh(VSL_RNG_METHOD_RAYLEIGH_ICDF_ACCURATE, streamPtr, n, x, alpha, beta);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int d_lognorm_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, double a, double sigma, double b, double beta, double* x)
{
	int status;
	status = vdRngLognormal(VSL_RNG_METHOD_LOGNORMAL_BOXMULLER2_ACCURATE, streamPtr, n, x, a, sigma, b, beta);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int s_lognorm_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, float a, float sigma, float b, float beta, float* x)
{
	int status;
	status = vsRngLognormal(VSL_RNG_METHOD_LOGNORMAL_BOXMULLER2_ACCURATE, streamPtr, n, x, a, sigma, b, beta);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int d_gumbel_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, double alpha, double beta, double* x)
{
	int status;
	status = vdRngGumbel(VSL_RNG_METHOD_GUMBEL_ICDF, streamPtr, n, x, alpha, beta);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int s_gumbel_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, float alpha, float beta, float* x)
{
	int status;
	status = vsRngGumbel(VSL_RNG_METHOD_GUMBEL_ICDF, streamPtr, n, x, alpha, beta);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int d_gamma_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, double a, double alpha, double beta, double* x)
{
	int status;
	status = vdRngGamma(VSL_RNG_METHOD_GAMMA_GNORM_ACCURATE, streamPtr, n, x, alpha, a, beta);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int s_gamma_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, float a, float alpha, float beta, float* x)
{
	int status;
	status = vsRngGamma(VSL_RNG_METHOD_GAMMA_GNORM_ACCURATE, streamPtr, n, x, alpha, a, beta);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int d_beta_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, double p, double q, double a, double beta, double* x)
{
	int status;
	status = vdRngBeta(VSL_RNG_METHOD_BETA_CJA_ACCURATE, streamPtr, n, x, p, q, a, beta);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int s_beta_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, float p, float q, float a, float beta, float* x)
{
	int status;
	status = vsRngBeta(VSL_RNG_METHOD_BETA_CJA_ACCURATE, streamPtr, n, x, p, q, a, beta);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int d_bern_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, double p, double* x)
{
	int status;
	int* sample = (int*)mkl_malloc(n*sizeof(int),64);
	if (sample == nullptr)
	{
		return OUTOFMEMORY;
	}
	status = viRngBernoulli(VSL_RNG_METHOD_BERNOULLI_ICDF, streamPtr, n, sample, p);
	for (MKL_INT i = 0; i < n; i++)
	{
		x[i] = sample[i];
	}
	mkl_free(sample);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int s_bern_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, float p, float* x)
{
	int status;
	int* sample = (int*)mkl_malloc(n*sizeof(int),64);
	if (sample == nullptr)
	{
		return OUTOFMEMORY;
	}
	status = viRngBernoulli(VSL_RNG_METHOD_BERNOULLI_ICDF, streamPtr, n, sample, p);
	for (MKL_INT i = 0; i < n; i++)
	{
		x[i] = sample[i];
	}
	mkl_free(sample);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int d_geom_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, double p, double* x)
{
	int status;
	int* sample = (int*)mkl_malloc(n*sizeof(int),64);
	if (sample == nullptr)
	{
		return OUTOFMEMORY;
	}
	status = viRngGeometric(VSL_RNG_METHOD_GEOMETRIC_ICDF, streamPtr, n, sample, p);
	for (MKL_INT i = 0; i < n; i++)
	{
		x[i] = sample[i];
	}
	mkl_free(sample);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int s_geom_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, float p, float* x)
{
	int status;
	int* sample = (int*)mkl_malloc(n*sizeof(int),64);
	if (sample == nullptr)
	{
		return OUTOFMEMORY;
	}
	status = viRngGeometric(VSL_RNG_METHOD_GEOMETRIC_ICDF, streamPtr, n, sample, p);
	for (MKL_INT i = 0; i < n; i++)
	{
		x[i] = sample[i];
	}
	mkl_free(sample);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int d_binom_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, int ntrial, double p, double* x)
{
	int status;
	int* sample = (int*)mkl_malloc(n*sizeof(int),64);
	if (sample == nullptr)
	{
		return OUTOFMEMORY;
	}
	status = viRngBinomial(VSL_RNG_METHOD_BINOMIAL_BTPE, streamPtr, n, sample, ntrial, p);
	for (MKL_INT i = 0; i < n; i++)
	{
		x[i] = sample[i];
	}
	mkl_free(sample);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int s_binom_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, int ntrial, float p, float* x)
{
	int status;
	int* sample = (int*)mkl_malloc(n*sizeof(int),64);
	if (sample == nullptr)
	{
		return OUTOFMEMORY;
	}
	status = viRngBinomial(VSL_RNG_METHOD_BINOMIAL_BTPE, streamPtr, n, sample, ntrial, p);
	for (MKL_INT i = 0; i < n; i++)
	{
		x[i] = sample[i];
	}
	mkl_free(sample);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int d_hypergeom_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, int l, int s, int m, double* x)
{
	int status;
	int* sample = (int*)mkl_malloc(n*sizeof(int),64);
	if (sample == nullptr)
	{
		return OUTOFMEMORY;
	}
	status = viRngHypergeometric(VSL_RNG_METHOD_HYPERGEOMETRIC_H2PE, streamPtr, n, sample, l, s, m);
	for (MKL_INT i = 0; i < n; i++)
	{
		x[i] = sample[i];
	}
	mkl_free(sample);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int s_hypergeom_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, int l, int s, int m, float* x)
{
	int status;
	int* sample = (int*)mkl_malloc(n*sizeof(int),64);
	if (sample == nullptr)
	{
		return OUTOFMEMORY;
	}
	status = viRngHypergeometric(VSL_RNG_METHOD_HYPERGEOMETRIC_H2PE, streamPtr, n, sample, l, s, m);
	for (MKL_INT i = 0; i < n; i++)
	{
		x[i] = sample[i];
	}
	mkl_free(sample);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int d_poisson_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, double lambda, double* x)
{
	int status;
	int* sample = (int*)mkl_malloc(n*sizeof(int),64);
	if (sample == nullptr)
	{
		return OUTOFMEMORY;
	}
	status = viRngPoisson(VSL_RNG_METHOD_POISSON_PTPE, streamPtr, n, sample, lambda);
	for (MKL_INT i = 0; i < n; i++)
	{
		x[i] = sample[i];
	}
	mkl_free(sample);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int s_poisson_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, float lambda, float* x)
{
	int status;
	int* sample = (int*)mkl_malloc(n*sizeof(int),64);
	if (sample == nullptr)
	{
		return OUTOFMEMORY;
	}
	status = viRngPoisson(VSL_RNG_METHOD_POISSON_PTPE, streamPtr, n, sample, lambda);
	for (MKL_INT i = 0; i < n; i++)
	{
		x[i] = sample[i];
	}
	mkl_free(sample);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int d_negbinom_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, double a, double p, double* x)
{
	int status;
	int* sample = (int*)mkl_malloc(n*sizeof(int),64);
	if (sample == nullptr)
	{
		return OUTOFMEMORY;
	}
	status = viRngNegbinomial(VSL_RNG_METHOD_NEGBINOMIAL_NBAR, streamPtr, n, sample, a, p);
	for (MKL_INT i = 0; i < n; i++)
	{
		x[i] = sample[i];
	}
	mkl_free(sample);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}

extern "C" __declspec(dllexport) int s_negbinom_rnd(VSLStreamStatePtr streamPtr, MKL_INT n, float a, float p, float* x)
{
	int status;
	int* sample = (int*)mkl_malloc(n*sizeof(int),64);
	if (sample == nullptr)
	{
		return OUTOFMEMORY;
	}
	status = viRngNegbinomial(VSL_RNG_METHOD_NEGBINOMIAL_NBAR, streamPtr, n, sample, a, p);
	for (MKL_INT i = 0; i < n; i++)
	{
		x[i] = sample[i];
	}
	mkl_free(sample);
	if (status != 0)
	{
		return VSLERROR;
	}
	return 0;
}




