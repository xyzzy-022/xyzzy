// -*-C++-*-
#ifndef _stream_h_
# define _stream_h_

enum stream_type_internal
{
  sti_synonym,
  sti_broadcast,
  sti_concatenated,
  sti_two_way,
  sti_echo,

  sti_input_stream     = 0x10,
  sti_output_stream    = 0x20,

  sti_file_stream      = 0x100,
  sti_string_stream    = 0x200,
  sti_composite_stream = 0x400,
  sti_status_stream    = 0x800,
  sti_buffer_stream    = 0x1000,
  sti_keyboard_stream  = 0x2000,
  sti_wstream_stream   = 0x4000,
  sti_socket_stream    = 0x8000,
  sti_general_stream   = 0x10000,
  sti_debug_stream     = 0x20000,
};

enum stream_type
{
  st_file_input = sti_file_stream | sti_input_stream,
  st_file_output = sti_file_stream | sti_output_stream,
  st_file_io = sti_file_stream | sti_input_stream | sti_output_stream,
  st_string_input = sti_string_stream | sti_input_stream,
  st_string_output = sti_string_stream | sti_output_stream,
  st_synonym = sti_composite_stream | sti_input_stream | sti_output_stream | sti_synonym,
  st_broadcast = sti_composite_stream | sti_output_stream | sti_broadcast,
  st_concatenated = sti_composite_stream | sti_input_stream | sti_concatenated,
  st_two_way = sti_composite_stream | sti_input_stream | sti_output_stream | sti_two_way,
  st_echo = sti_composite_stream | sti_input_stream | sti_output_stream | sti_echo,
  st_status = sti_status_stream | sti_output_stream,
  st_buffer = sti_buffer_stream | sti_input_stream | sti_output_stream,
  st_keyboard = sti_keyboard_stream | sti_input_stream,
  st_wstream = sti_wstream_stream | sti_output_stream,
  st_socket = sti_socket_stream | sti_input_stream | sti_output_stream,
  st_general_input = sti_general_stream | sti_input_stream,
  st_general_output = sti_general_stream | sti_output_stream,
  st_debug_output = sti_debug_stream | sti_output_stream,
};

class lstream: public lisp_object
{
public:
  stream_type type;  // ストリームのサブタイプ
  int column;        // カラム位置
  long linenum;      // 行番号
  void *input;
  void *output;
  lChar pending;     // unread-char用バッファ
  lisp pathname;
  char *alt_pathname;
  int start;
  int end;
  char open_p;        // ストリームがオープンされている?
  char encoding;      // エンコードモード
  enum
    {
      ENCODE_CANON,   // text(行末変換する)
      ENCODE_RAW,     // text(行末変換しない)
      ENCODE_BINARY   // binary
    };

  ~lstream ();
};

/*
  LSTREAM USAGE:

  FILE STREAM:
    input        : (FILE *) 入力ファイルのストリーム
    output       : (FILE *) 出力ファイルのストリーム
    pathname     : (lisp)   ファイル名
    alt_pathname : (char *) テンポラリファイルのファイル名
    start        : -
    end          : -

  STRING STREAM:
    input        : (lisp) 入力文字列
    output       : (lisp) 出力文字列
    pathname     : -
    alt_pathname : -
    start        : (int)  開始位置
    end          : (int)  終了位置

  BUFFER STREAM:
    input        : (lisp) EOB位置のマーカ
    output       : (lisp) 入出力位置のマーカ
    pathname     : -
    alt_pathname : -
    start        : -
    end          : -

  COMPOSITE STREAM:
    input        : (lisp) 入力
    output       : (lisp) 出力
    pathname     : -
    alt_pathname : -
    start        : -
    end          : -

  SOCKET STREAM:
    input        : (sockinet *) sockinet object
    output       : -
    pathname     : -
    alt_pathname : -
    start        : -
    end          : -

  GENERAL INPUT STREAM:
    input        : (lisp) i/o callback
    output       : (lisp) close callback
    pathname     : (lisp) listen callback
    alt_pathname : (lisp) 入力文字列
    start        : 入力位置
    end          : -

  GENERAL OUTPUT STREAM:
    input        : (lisp) i/o callback
    output       : (lisp) close callback
    pathname     : (lisp) flush callback
    alt_pathname : -
    start        : -
    end          : -

 */

# define streamp(X) typep ((X), Tstream)

# define file_stream_p(X) (xstream_type (X) & sti_file_stream)
# define string_stream_p(X) (xstream_type (X) & sti_string_stream)
# define composite_stream_p(X) (xstream_type (X) & sti_composite_stream)
# define input_stream_p(X) (xstream_type (X) & sti_input_stream)
# define output_stream_p(X) (xstream_type (X) & sti_output_stream)
# define buffer_stream_p(X) (xstream_type (X) & sti_buffer_stream)
# define status_stream_p(X) (xstream_type (X) & sti_status_stream)
# define wstream_stream_p(X) (xstream_type (X) & sti_wstream_stream)
# define socket_stream_p(X) (xstream_type (X) & sti_socket_stream)
# define general_stream_p(X) (xstream_type (X) & sti_general_stream)

inline void
check_stream (lisp x)
{
  check_type (x, Tstream, Qstream);
}

inline stream_type &
xstream_type (lisp x)
{
  assert (streamp (x));
  return ((lstream *)x)->type;
}

inline char &
xstream_open_p (lisp x)
{
  assert (streamp (x));
  return ((lstream *)x)->open_p;
}

inline int &
xstream_column (lisp x)
{
  assert (streamp (x));
  return ((lstream *)x)->column;
}

inline long &
xstream_linenum (lisp x)
{
  assert (streamp (x));
  return ((lstream *)x)->linenum;
}

inline lChar &
xstream_pending (lisp x)
{
  assert (streamp (x));
  return ((lstream *)x)->pending;
}

inline FILE *&
xfile_stream_input (lisp x)
{
  assert (streamp (x));
  assert (file_stream_p (x));
  return (FILE *&)((lstream *)x)->input;
}

inline FILE *&
xfile_stream_output (lisp x)
{
  assert (streamp (x));
  assert (file_stream_p (x));
  return (FILE *&)((lstream *)x)->output;
}

inline lisp &
xfile_stream_pathname (lisp x)
{
  assert (streamp (x));
  assert (file_stream_p (x));
  return ((lstream *)x)->pathname;
}

inline char *&
xfile_stream_alt_pathname (lisp x)
{
  assert (streamp (x));
  assert (file_stream_p (x));
  return ((lstream *)x)->alt_pathname;
}

inline char &
xfile_stream_encoding (lisp x)
{
  assert (streamp (x));
  assert (file_stream_p (x));
  return ((lstream *)x)->encoding;
}

inline lisp &
xstring_stream_input (lisp x)
{
  assert (streamp (x));
  assert (string_stream_p (x));
  return (lisp &)((lstream *)x)->input;
}

inline lisp &
xstring_stream_output (lisp x)
{
  assert (streamp (x));
  assert (string_stream_p (x));
  return (lisp &)((lstream *)x)->output;
}

inline int &
xstring_stream_start (lisp x)
{
  assert (streamp (x));
  assert (string_stream_p (x));
  return ((lstream *)x)->start;
}

inline int &
xstring_stream_end (lisp x)
{
  assert (streamp (x));
  assert (string_stream_p (x));
  return ((lstream *)x)->end;
}

inline lisp &
xbuffer_stream_marker (lisp x)
{
  assert (streamp (x));
  assert (buffer_stream_p (x));
  return (lisp &)((lstream *)x)->output;
}

inline lisp &
xbuffer_stream_eob (lisp x)
{
  assert (streamp (x));
  assert (buffer_stream_p (x));
  return (lisp &)((lstream *)x)->input;
}

inline lisp &
xcomposite_stream_input (lisp x)
{
  assert (streamp (x));
  assert (composite_stream_p (x));
  return (lisp &)((lstream *)x)->input;
}

inline lisp &
xcomposite_stream_output (lisp x)
{
  assert (streamp (x));
  assert (composite_stream_p (x));
  return (lisp &)((lstream *)x)->output;
}

class sockinet;

inline sockinet *&
xsocket_stream_sock (lisp x)
{
  assert (streamp (x));
  assert (socket_stream_p (x));
  return (sockinet *&)((lstream *)x)->input;
}

inline char &
xsocket_stream_encoding (lisp x)
{
  assert (streamp (x));
  assert (socket_stream_p (x));
  return ((lstream *)x)->encoding;
}

inline lisp &
xgeneral_stream_io_callback (lisp x)
{
  assert (streamp (x));
  assert (general_stream_p (x));
  return (lisp &)((lstream *)x)->input;
}

inline lisp &
xgeneral_stream_close_callback (lisp x)
{
  assert (streamp (x));
  assert (general_stream_p (x));
  return (lisp &)((lstream *)x)->output;
}

inline lisp &
xgeneral_input_stream_listen_callback (lisp x)
{
  assert (streamp (x));
  assert (general_stream_p (x));
  return ((lstream *)x)->pathname;
}

inline lisp &
xgeneral_input_stream_string (lisp x)
{
  assert (streamp (x));
  assert (general_stream_p (x));
  return (lisp &)((lstream *)x)->alt_pathname;
}

inline int &
xgeneral_input_stream_index (lisp x)
{
  assert (streamp (x));
  assert (general_stream_p (x));
  return ((lstream *)x)->start;
}

inline lisp &
xgeneral_output_stream_flush_callback (lisp x)
{
  assert (streamp (x));
  assert (general_stream_p (x));
  return ((lstream *)x)->pathname;
}

class wStream;

inline wStream *&
xwstream_stream_wstream (lisp x)
{
  assert (streamp (x));
  assert (wstream_stream_p (x));
  return (wStream *&)((lstream *)x)->output;
}

void write_stream (lisp, const Char *, size_t);
void writec_stream (lisp, Char);
int unreadc_stream (lChar, lisp);
lChar peekc_stream (lisp);
lChar readc_stream (lisp);
void create_std_streams ();
lisp create_file_stream (lisp, lisp, lisp, lisp, lisp, lisp);
int listen_stream (lisp);
int get_stream_column (lisp);
void flush_stream (lisp);
long stream_linenum (lisp);

lisp input_stream (lisp);
lisp output_stream (lisp);

#endif
