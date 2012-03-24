#ifndef _system_h_
# define _system_h_

#include "ed.h"
#include <rpc.h>

static inline void
rpc_error (RPC_STATUS r)
{
  if (r != RPC_S_OK)
    FEsimple_win32_error (MAKE_HRESULT (1, FACILITY_RPC, r));
}

class safe_rpc_str
{
protected:
  RPC_CSTR u;
public:
  safe_rpc_str () : u (0) {}
  ~safe_rpc_str () {if (u) RpcStringFree (&u);}
  RPC_CSTR *operator & () {return &u;}
  lisp make_string () {return ::make_string (u);}
};

#endif
