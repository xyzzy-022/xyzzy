#ifndef DECLARE_LDATA_BEGIN
# define DECLARE_LDATA_BEGIN /* empty */
# define DECLARE_LDATA_END /* empty */
#endif
#ifndef DECLARE_LARRAY
# define DECLARE_LARRAY DECLARE_LDATA
#endif
/**/DECLARE_LDATA_BEGIN
DECLARE_LDATA (lsimple_vector, Tsimple_vector)
DECLARE_LARRAY (lcomplex_vector, Tcomplex_vector)
DECLARE_LDATA (lsimple_string, Tsimple_string)
DECLARE_LARRAY (lcomplex_string, Tcomplex_string)
DECLARE_LARRAY (lgeneral_array, Tarray)
DECLARE_LARRAY (lstring_array, Tstring_array)
DECLARE_LDATA (lcons, Tcons)     // ã§óLîzóÒÇÃGCÇ≈ïsìsçáÇ™Ç†ÇÈÇΩÇﬂarrayÇÊÇËå„ÇÎ
DECLARE_LDATA (lsymbol, Tsymbol)
DECLARE_LDATA (llong_int, Tlong_int)
DECLARE_LDATA (lbignum, Tbignum)
DECLARE_LDATA (lfraction, Tfraction)
DECLARE_LDATA (lsingle_float, Tsingle_float)
DECLARE_LDATA (ldouble_float, Tdouble_float)
DECLARE_LDATA (lcomplex, Tcomplex)
DECLARE_LDATA (lfunction, Tfunction)
DECLARE_LDATA (lhash_table, Thash_table)
DECLARE_LDATA (lclosure, Tclosure)
DECLARE_LDATA (lerror, Terror)
DECLARE_LDATA (lstream, Tstream)
DECLARE_LDATA (lpackage, Tpackage)
DECLARE_LDATA (lrandom_state, Trandom_state)
DECLARE_LDATA (lstruct_def, Tstruct_def)
DECLARE_LDATA (lstruct_data, Tstruct_data)
DECLARE_LDATA (lreadtable, Treadtable)
DECLARE_LDATA (lwindow, Twindow)
DECLARE_LDATA (lbuffer, Tbuffer)
DECLARE_LDATA (lsyntax_table, Tsyntax_table)
DECLARE_LDATA (lmarker, Tmarker)
DECLARE_LDATA (lregexp, Tregexp)
DECLARE_LDATA (lprocess, Tprocess)
DECLARE_LDATA (lwin32_menu, Twin32_menu)
DECLARE_LDATA (lwin32_dde_handle, Twin32_dde_handle)
DECLARE_LDATA (lchunk, Tchunk)
DECLARE_LDATA (ldll_module, Tdll_module)
DECLARE_LDATA (ldll_function, Tdll_function)  // undumpÇÃÇΩÇﬂdll-moduleÇÊÇËå„ÇÎ
DECLARE_LDATA (lc_callable, Tc_callable)
DECLARE_LDATA (loledata, Toledata)
DECLARE_LDATA (lwait_object, Twait_object)
DECLARE_LDATA (lchar_encoding, Tchar_encoding)
DECLARE_LDATA (lenvironment, Tenvironment)
/**/DECLARE_LDATA_END
#undef DECLARE_LDATA_BEGIN
#undef DECLARE_LDATA_END
#undef DECLARE_LDATA
#undef DECLARE_LARRAY
