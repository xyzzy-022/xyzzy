#ifndef _DIALOGS_H_
# define _DIALOGS_H_

class IdleDialog
{
private:
  enum {IE_ALIVE, IE_DEAD, IE_DELETE};
  int id_end;
  int id_result;
  int id_idle;
  int id_modeless;
  const int id_auto_delete;
  int id_wndproc_depth;

  void process ();

public:
  HWND id_hwnd;
protected:

  IdleDialog (int = 0);
  virtual ~IdleDialog ();
  virtual BOOL WndProc (UINT, WPARAM, LPARAM) = 0;
  virtual void IdleProc () = 0;
  static BOOL CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM);
  void EndDialog (int);
  void set_idle (int);
  int modeless_p () const {return id_modeless;}
  int auto_delete_p () const {return id_auto_delete;}
  virtual int IsDialogMessage (MSG *msg)
    {return ::IsDialogMessage (id_hwnd, msg);}
public:
  int DoModal (HWND, UINT);
  int Create (HWND, UINT);
};

inline
IdleDialog::IdleDialog (int auto_delete)
     : id_end (IE_ALIVE), id_result (-1), id_idle (1),
       id_modeless (0), id_auto_delete (auto_delete),
       id_wndproc_depth (0)
{
}

inline void
IdleDialog::set_idle (int f)
{
  id_idle = f;
}

#endif
