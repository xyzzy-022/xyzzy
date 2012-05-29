#ifndef _EXCEPT_H_
# define _EXCEPT_H_

class Win32Exception
{
public:
  struct known_exception
    {
      u_int code;
      const char *desc;
    };
  static const known_exception known_excep[];
  static EXCEPTION_RECORD r;
  static CONTEXT c;
  static u_int code;
  Win32Exception (u_int, const EXCEPTION_POINTERS *);
  void throw_lisp_error ();
};

void __cdecl se_handler (u_int, EXCEPTION_POINTERS *);
void cleanup_exception ();

#endif
