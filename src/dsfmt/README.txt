 =================================================================
 dSFMT ver. 2.2.1
 2011.11.10

 double precision SIMD oriented Fast Mersenne Twister(dSFMT)
 based on IEEE 754 floating point format.

 Mutsuo Saito (Hiroshima University) and
 Makoto Matsumoto (Hiroshima University)

 Copyright (C) 2007, 2008, 2009 Mutsuo Saito, Makoto Matsumoto and
 Hiroshima University.
 Copyright (C) 2011, 2013 Mutsuo Saito, Makoto Matsumoto, Hiroshima
 University and The University of Tokyo.
 All rights reserved.

 The (modified) BSD License is applied to this software, see
 LICENSE.txt
 =================================================================
 The documents written in English is the official one.

 dSFMT ver. 2.0 and ver. 2.1 are completely different from dSFMT ver 1.xx.
 The algorithm is changed.

 This program only works on systems which have IEEE754 floating point
 format.

 This version uses `struct' of C language.
 Don't use different DSFMT_MEXP for compiling dSFMT.c and your program.

 To see documents, see html/index.html.

 To make test program, see html/howto-compile.html.
 If your CPU is BIG ENDIAN and your compiler is not gcc,
 define DSFMT_BIG_ENDIAN preprocessor macro, please.

 If you want to redistribute and/or change source files, see LICENSE.txt.

 When you change these files and redistribute them, PLEASE write your
 e-mail address in redistribution and write to contact YOU first if
 users of your changed source encounter troubles.
