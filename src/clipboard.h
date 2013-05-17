#ifndef _clipboard_h_
# define _clipboard_h_

typedef BOOL (*AddClipboardFormatListener)(HWND hwnd);
typedef BOOL (*RemoveClipboardFormatListener)(HWND hwnd);

class clipboard
{
private:
  AddClipboardFormatListener AddClipboardFormatListenerProc;
  RemoveClipboardFormatListener RemoveClipboardFormatListenerProc;

  HWND hwnd_next_clipboard;
  DWORD last_clipboard_seqno;
  bool use_newapi_p;

  void add_clipboard_chain (HWND hwnd);
  void remove_clipboard_chain (HWND hwnd);

public:
  clipboard ();
  void add_listener (HWND hwnd);
  void remove_listener (HWND hwnd);
  void repair_clipboard_chain_if_need (HWND hwnd);
  void draw_clipboard (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam);
  void change_clipboard_chain (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam);
  void clipboard_update (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam);
};

#endif
