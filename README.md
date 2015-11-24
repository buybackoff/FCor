# FCor Numerical Library

Copyright (c) 2007-2015 StatFactory Ltd

FCor is an open source high performance F# math and stat library.

The math contains functions for construction and manipulation of vectors and matrices, including matrix factorizations and solvers, random number generators, vector functions and basic stats.

The stat part contains strongly typed statistical data frame with factors and covariates, strongly typed statistical model formula, ultra fast implementation of Generalized Linear Model and CSV Data Type Provider.

FCor is a .NET library but for maximum performance on Intel it is accelerated by Intel Math Kernel Library. Also, it uses native C functions to allocate memory so that .NET memory limits are bypassed and Garbage Collection is minimized. 

FCor source code on GitHub is covered under the terms of the MIT/X11 license. Please note that MKL is not open source and therefore FCor binary distribution on Nuget, which contains MKL, is available under a separate license.

Documentation: http://statfactory.github.io/FCor
