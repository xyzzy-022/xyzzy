#ifndef _fnkey_h_
# define _fnkey_h_

LRESULT CALLBACK fnkey_wndproc (HWND, UINT, WPARAM, LPARAM);

extern const char FunctionKeyClassName[];

# define MAX_Fn (CCF_Fn_MAX - CCF_F1 + 1)
# define MAX_FUNCTION_BAR_LABEL (MAX_Fn * 8)

class FKWin
{
protected:
  HWND fk_hwnd;    //
  SIZE fk_sz;      // クライアント領域のサイズ
  SIZE fk_btn;     // ボタンサイズ
  int fk_nbuttons; // ボタンの数
  int fk_height;   // FKWinの高さ
  int fk_offset[MAX_Fn]; // 各ボタンの開始位置

  RECT fk_cur_rect; // 処理対象(fk_cur_btn)の矩形
  int fk_cur_btn;   // 処理対象ボタン(なければ-1)
  int fk_cur_on;    // 沈んでいるボタン(必ずfk_cur_btnと同じか-1)
  int fk_vkey;      // シフトキーの状態
  enum
    {
      FVK_SHIFT = 1,
      FVK_CONTROL = 2,
      FVK_META = 4
    };

  void get_button_rect (int, RECT &) const;
  void paint_off (HDC hdc, int n, const RECT &r) const;
  void paint_on (HDC hdc, int n, const RECT &r) const;
  void paint_text (HDC, int, const RECT &, int) const;
  void paint_buttons (HDC) const;
  void button_on (int);
  int vk2fvk (int) const;

  struct divinfo
    {
      int nbuttons;
      int ndiv;
    };
  static const divinfo fk_divinfo[];
  static int fk_default_nbuttons;

public:
  FKWin ();
  void refresh_button (int) const;
  void set_hwnd (HWND hwnd) {fk_hwnd = hwnd;};
  HWND hwnd () const {return fk_hwnd;}
  int height () const {return fk_height;}
  void OnPaint ();
  void OnSize (int, int);
  void OnLButtonDown (int, int, int);
  void OnLButtonUp (int, int, int);
  void OnMouseMove (int, int, int);
  void OnKillFocus ();
  void OnCancelMode ();
  void set_vkey (int);
  void unset_vkey (int);
  void update_vkey (int);
  int get_nbuttons () const {return fk_nbuttons;}
  int set_nbuttons (int);

  static int &default_nbuttons () {return fk_default_nbuttons;}
};

#endif
