// -*-C++-*-
#ifndef _signal_h_
# define _signal_h_

enum message_code;

void init_condition ();

void check_condition (lisp);

void FEstorage_error ();
lisp FEstack_overflow ();
lisp FEwin32_exception (const char* desc, u_int code, PVOID address);
lisp FEtype_error (lisp, lisp);
lisp FErange_error (lisp);
lisp FEsimple_error (message_code);
lisp FEsimple_error (message_code, lisp);
lisp FEplain_error (message_code);
lisp FEtoo_few_arguments ();
lisp FEtoo_many_arguments ();
lisp FEbad_type_specifier (lisp);
lisp FEmodify_constant (lisp);
lisp FEunbound_variable (lisp);
lisp FEno_target_for (lisp, lisp);
lisp FEtarget_missing (lisp, lisp);
lisp FEinvalid_byte_code ();
lisp FEinvalid_function (lisp);
lisp FEundefined_function (lisp);
lisp FEbad_macro_form (lisp);
lisp FEinvalid_variable_list (lisp);
lisp FEinvalid_lambda_list (lisp);
lisp FEinvalid_keyword_list (lisp);
lisp FEdivision_by_zero ();
lisp FEfloating_point_overflow ();
lisp FEfloating_point_underflow ();
lisp FElog_domain_error ();
lisp FEarchiver_error (message_code, lisp);
lisp FEdde_timeout ();
lisp FEdde_busy ();
lisp FEdde_low_memory ();
lisp FEdde_no_conv ();
lisp FEdde_not_processed ();
lisp FEdde_server_died ();
lisp FEdde_error (lisp);
lisp FEdde_terminated_transaction ();
lisp FEquit ();
lisp FEsilent_quit ();
lisp FEprogram_error (message_code);
lisp FEprogram_error (message_code, lisp);
lisp FEformat_error (message_code);
lisp FEread_only_buffer ();
lisp FEnetwork_error (lisp, lisp);
lisp FEsimple_package_error (lisp, message_code, lisp);
lisp FEfile_lost_error (lisp, lisp);
lisp FEfile_error (message_code, lisp);
lisp FEreader_error (lisp, lisp, lisp, lisp);
lisp FEsimple_crtl_error (int);
lisp FEsimple_crtl_error (int, lisp);
lisp FEsimple_win32_error (int);
lisp FEsimple_win32_error (int, lisp);
lisp FEwin32_file_error (lisp, int);
lisp FEwin32_file_error (lisp, int, lisp);
lisp FEend_of_file (lisp);
lisp FEbignum_overflow ();
lisp FEpower_number_too_large ();
lisp FEsocket_error (int, const char *);

#endif
