#ifndef _syntaxinfo_h_
#define _syntaxinfo_h_

struct syntax_state
{
  enum
    {
      SS_INVALID,
      SS_NORMAL,
      SS_ESC_NORMAL,
      SS_MCOMM1S_OR_CPPCOMM1,
      SS_MCOMM1S,
      SS_CPPCOMM1,
      SS_MCOMMENT,
      SS_MCOMM1E,
      SS_CPPCOMMENT,
      SS_STRING,
      SS_ESC_STRING,
      SS_SCOMMENT,

      SS_TAG_FIRST,
      SS_TAG,
      SS_MCOMM2E,

      SS_MAX
    };
  enum
    {
      KWD_OK = 0x80000000,
      KWD2_OK = 0x40000000
    };

  Char ss_strch;
  u_char ss_state;

  static const syntax_table *ss_tab;
  static void (syntax_state::*update)(const Char *);
  static u_int (*ss_colors)[SS_MAX];
  static u_int (*ss_prev_colors)[SS_MAX];
  static lisp ss_hashtab;
  static Buffer *ss_bp;
  static Chunk *ss_chunk;

  void update_normal (const Char *);
  void update_column_comment (const Char *);
  void update_html (const Char *);
  void update_parentheses (const Char *);

  syntax_state () : ss_state (SS_NORMAL), ss_strch (0) {}
  void operator = (const syntax_state &src)
    {
      ss_state = src.ss_state;
      ss_strch = src.ss_strch;
    }

  static u_int ss_normal_colors[SS_MAX][SS_MAX];
  static u_int ss_normal_prev_colors[SS_MAX][SS_MAX];
  static u_int ss_html_colors[SS_MAX][SS_MAX];
  static u_int ss_html_prev_colors[SS_MAX][SS_MAX];
  static u_char ss_maybe_comment[SS_MAX];

  int maybe_comment_p () const {return ss_maybe_comment[ss_state];}
  static void init_color_table ();
  static void define_chunk (Chunk *cp) {ss_chunk = cp;}
};

class syntax_info
{
public:
  syntax_state si;
  syntax_state bsi;
  point_t point;

  syntax_info (Buffer *, lisp, int);
  void point_syntax (const Point &);
};

#endif
