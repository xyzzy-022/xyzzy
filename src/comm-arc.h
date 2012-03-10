#ifndef __COMM_ARC_H__
#define __COMM_ARC_H__

#ifdef __cplusplus
extern "C" {
#endif

#ifndef __wtypes_h__
#include <wtypes.h>
#endif

#ifndef _TIME_T_DEFINED
#define _TIME_T_DEFINED
typedef long time_t;
#endif

#ifndef FNAME_MAX32
#define FNAME_MAX32 512
#endif

#if defined(ARC_DECSTRACT) && !defined(__ARC_STRUCT_DEFINED__)
#define __ARC_STRUCT_DEFINED__
#endif

#ifndef __ARC_STRUCT_DEFINED__
#define __ARC_STRUCT_DEFINED__

typedef	HGLOBAL	HARC;

#ifdef __BORLANDC__
#pragma option -a-
#else
#pragma pack(1)
#endif

typedef struct
{
  DWORD dwOriginalSize;              /* ファイルのサイズ                  */
  DWORD dwCompressedSize;            /* 圧縮後のサイズ                    */
  DWORD dwCRC;                       /* 格納ファイルのチェックサム/CRC    */
  UINT  uFlag;                       /* 解凍やテストの処理結果            */
  UINT  uOSType;                     /* このファイルの作成に使われたＯＳ  */
  WORD  wRatio;                      /* 圧縮率（パーミル)                 */
  WORD  wDate;                       /* 格納ファイルの日付                */
  WORD  wTime;                       /* 格納ファイルの時刻                */
  char  szFileName[FNAME_MAX32 + 1]; /* アーカイブファイル名              */
  char  dummy1[3];
  char  szAttribute[8];              /* 格納ファイルの属性                */
  char  szMode[8];                   /* 格納ファイルの格納モード          */
}
  INDIVIDUALINFO, FAR *LPINDIVIDUALINFO;

typedef struct
{
  DWORD dwFileSize;                        /* 格納ファイルのサイズ */
  DWORD dwWriteSize;                       /* 解凍して書き込んだサイズ */
  char  szSourceFileName[FNAME_MAX32 + 1]; /* 処理を行う格納ファイル名 */
  char  dummy1[3];
  char  szDestFileName[FNAME_MAX32 + 1];   /* 実際に書き込まれるパス名 */
  char  dummy[3];
}
  EXTRACTINGINFO, FAR *LPEXTRACTINGINFO;

typedef struct
{
  EXTRACTINGINFO exinfo;
  DWORD dwCompressedSize;               /* 圧縮後のサイズ  */
  DWORD dwCRC;                          /* 格納ファイルのチェックサム/CRC */
  UINT  uOSType;                        /* このファイルの作成に使われたＯＳ */
  WORD  wRatio;                         /* 圧縮率（パーミル) */
  WORD  wDate;                          /* 格納ファイルの日付  */
  WORD  wTime;                          /* 格納ファイルの時刻  */
  char  szAttribute[8];                 /* 格納ファイルの属性  */
  char  szMode[8];                      /* 格納ファイルの格納モード  */
}
  EXTRACTINGINFOEX, FAR *LPEXTRACTINGINFOEX;

#ifdef __BORLANDC__
#pragma option -a.
#else
#pragma pack()
#endif

#endif /* __ARC_STRUCT_DEFINED__ */

#ifndef CHECKARCHIVE_RAPID
#define CHECKARCHIVE_MODE_MASK  3
#define	CHECKARCHIVE_RAPID	0 	/* 簡易型（高速） */
#define	CHECKARCHIVE_BASIC	1 	/* 標準型（ヘッダーのみ） */
#define	CHECKARCHIVE_FULLCRC	2	/* 完全型（ＣＲＣ等のチェックを含む） */

/* 以下のフラグは上記と組み合わせて使用。*/
#define CHECKARCHIVE_RECOVERY	4	/* 破損ヘッダを読み飛ばして処理 */
#define CHECKARCHIVE_SFX	8	/* SFX かどうかを返す */
#define CHECKARCHIVE_ALL	16	/* ファイルの最後まで検索する */
#endif /* CHECKARCHIVE_RAPID */

#ifndef UNPACK_CONFIG_MODE
#define	UNPACK_CONFIG_MODE	1 /* 解凍（復元）系のコマンド */
#define	PACK_CONFIG_MODE	2 /* 圧縮（作成）系のコマンド */
#endif

#ifndef ISARC_FUNCTION_START
#define ISARC_FUNCTION_START			0
#define ISARC					0
#define ISARC_GET_VERSION			1
#define ISARC_GET_CURSOR_INTERVAL		2
#define ISARC_SET_CURSOR_INTERVAL		3
#define ISARC_GET_BACK_GROUND_MODE		4
#define ISARC_SET_BACK_GROUND_MODE		5
#define ISARC_GET_CURSOR_MODE			6
#define ISARC_SET_CURSOR_MODE			7
#define ISARC_GET_RUNNING			8

#define ISARC_CHECK_ARCHIVE			16
#define ISARC_CONFIG_DIALOG			17
#define ISARC_GET_FILE_COUNT			18
#define ISARC_QUERY_FUNCTION_LIST		19
#define ISARC_HOUT				20
#define ISARC_STRUCTOUT				21
#define ISARC_GET_ARC_FILE_INFO			22

#define ISARC_OPEN_ARCHIVE			23
#define ISARC_CLOSE_ARCHIVE			24
#define ISARC_FIND_FIRST			25
#define ISARC_FIND_NEXT				26
#define ISARC_EXTRACT				27
#define ISARC_ADD				28
#define ISARC_MOVE				29
#define ISARC_DELETE				30

#define ISARC_GET_ARC_FILE_NAME			40
#define ISARC_GET_ARC_FILE_SIZE			41
#define ISARC_GET_ARC_ORIGINAL_SIZE		42
#define ISARC_GET_ARC_COMPRESSED_SIZE		43
#define ISARC_GET_ARC_RATIO			44
#define ISARC_GET_ARC_DATE			45
#define ISARC_GET_ARC_TIME			46
#define ISARC_GET_ARC_OS_TYPE			47
#define ISARC_GET_ARC_IS_SFX_FILE		48
#define ISARC_GET_FILE_NAME			57
#define ISARC_GET_ORIGINAL_SIZE			58
#define ISARC_GET_COMPRESSED_SIZE		59
#define ISARC_GET_RATIO				60
#define ISARC_GET_DATE				61
#define ISARC_GET_TIME				62
#define ISARC_GET_CRC				63
#define ISARC_GET_ATTRIBUTE			64
#define ISARC_GET_OS_TYPE			65
#define ISARC_GET_METHOD			66
#define ISARC_GET_WRITE_TIME			67
#define ISARC_GET_CREATE_TIME			68
#define ISARC_GET_ACCESS_TIME			69

#define ISARC_FUNCTION_END			69

#endif /* ISARC_FUNCTION_START */

/* Attributes */
#ifndef FA_RDONLY
#define FA_RDONLY       0x01            /* Read only attribute */
#define FA_HIDDEN       0x02            /* Hidden file */
#define FA_SYSTEM       0x04            /* System file */
#define FA_LABEL        0x08            /* Volume label */
#define FA_DIREC        0x10            /* Directory */
#define FA_ARCH         0x20            /* Archive */
#endif /* FA_RDONLY */

#ifndef ERROR_START
#define ERROR_START			0x8000

/* Warnings */
#define ERROR_DISK_SPACE		0x8005
#define ERROR_READ_ONLY			0x8006
#define ERROR_USER_SKIP			0x8007
#define ERROR_UNKNOWN_TYPE		0x8008
#define ERROR_METHOD			0x8009
#define ERROR_PASSWORD_FILE		0x800A
#define ERROR_VERSION			0x800B
#define ERROR_FILE_CRC			0x800C
#define ERROR_FILE_OPEN			0x800D
#define ERROR_MORE_FRESH		0x800E
#define ERROR_NOT_EXIST			0x800F
#define ERROR_ALREADY_EXIST		0x8010
#define ERROR_TOO_MANY_FILES		0x8011

/* Errors */
/*#define ERROR_DIRECTORY		0x8012*/
#define ERROR_MAKEDIRECTORY		0x8012
#define ERROR_CANNOT_WRITE		0x8013
#define ERROR_HUFFMAN_CODE		0x8014
#define ERROR_COMMENT_HEADER		0x8015
#define ERROR_HEADER_CRC		0x8016
#define ERROR_HEADER_BROKEN		0x8017
#define ERROR_ARCHIVE_FILE_OPEN		0x8018
#define ERROR_ARC_FILE_OPEN		ERROR_ARCHIVE_FILE_OPEN
#define ERROR_NOT_ARC_FILE		0x8019
#define ERROR_NOT_ARCHIVE_FILE		ERROR_NOT_ARC_FILE
#define ERROR_CANNOT_READ		0x801A
#define ERROR_FILE_STYLE		0x801B
#define ERROR_COMMAND_NAME		0x801C
#define ERROR_MORE_HEAP_MEMORY		0x801D
#define ERROR_ENOUGH_MEMORY		0x801E
#ifndef ERROR_ALREADY_RUNNING
#define ERROR_ALREADY_RUNNING		0x801F
#endif
#define ERROR_USER_CANCEL		0x8020
#define ERROR_HARC_ISNOT_OPENED		0x8021
#define ERROR_NOT_SEARCH_MODE		0x8022
#define ERROR_NOT_SUPPORT		0x8023
#define ERROR_TIME_STAMP		0x8024
/*#define ERROR_NULL_POINTER		0x8025*/
/*#define ERROR_ILLEGAL_PARAMETER	0x8026*/
#define ERROR_TMP_OPEN			0x8025
#define ERROR_LONG_FILE_NAME		0x8026
#define ERROR_ARC_READ_ONLY		0x8027
#define ERROR_SAME_NAME_FILE		0x8028
#define ERROR_NOT_FIND_ARC_FILE 	0x8029
#define ERROR_RESPONSE_READ		0x802A
#define ERROR_NOT_FILENAME		0x802B
#define ERROR_TMP_COPY			0x802C
#define ERROR_EOF			0x802D
#define ERROR_ADD_TO_LARC		0x802E

#ifndef NO_COMPAT_ISH32
#define	ERROR_CANNOT_CREATE		0x8030
#define	ERROR_CANNOT_OPEN		0x8031
#endif /* NO_COMPAT_ISH32 */

#define ERROR_END			ERROR_ADD_TO_LARC

#endif /* ERROR_START */

/* Modes for UnarjOpenArchive */
#if !defined(EXTRACT_FOUND_FILE)
#define M_INIT_FILE_USE		0x00000001L	/* レジストリの設定を使用 */
#define M_REGARDLESS_INIT_FILE	0x00000002L	/* 〃 を使用しない */
#ifndef NO_COMPAT_UNARJ32
#define M_NOT_USE_TIME_STAMP	0x00000008L
#define M_EXTRACT_REPLACE_FILE	0x00000010L
#define M_EXTRACT_NEW_FILE	0x00000020L
#define M_EXTRACT_UPDATE_FILE	0x00000040L
#endif /* NO_COMPAT_UNARJ32 */
#define M_CHECK_ALL_PATH	0x00000100L	/* 厳格なファイル名サーチ */
#define M_CHECK_FILENAME_ONLY	0x00000200L	/* 〃を行わない */
#ifndef NO_COMPAT_UNARJ32
#define M_CHECK_DISK_SIZE	0x00000400L
#define M_REGARDLESS_DISK_SIZE	0x00000800L
#endif /* NO_COMPAT_UNARJ32 */
#define M_USE_DRIVE_LETTER	0x00001000L	/* ドライブ名から格納 */
#define M_NOT_USE_DRIVE_LETTER	0x00002000L	/* 〃 を格納しない */
#ifndef NO_COMPAT_UNARJ32
#define M_INQUIRE_DIRECTORY	0x00004000L
#define M_NOT_INQUIRE_DIRECTORY 0x00008000L
#define M_INQUIRE_WRITE		0x00010000L
#define M_NOT_INQUIRE_WRITE	0x00020000L
#define M_CHECK_READONLY	0x00040000L
#define M_REGARDLESS_READONLY	0x00080000L
#define M_REGARD_E_COMMAND	0x00100000L
#define M_REGARD_X_COMMAND	0x00200000L
#endif /* NO_COMPAT_UNARJ32 */
#define M_ERROR_MESSAGE_ON	0x00400000L	/* エラーメッセージを表示 */
#define M_ERROR_MESSAGE_OFF	0x00800000L	/* 〃を表示しない */
#define M_RECOVERY_ON		0x08000000L	/* 破損ヘッダの読み飛ばし */
#ifndef NO_COMPAT_TAR32
#define M_BAR_WINDOW_ON		0x01000000L
#define M_BAR_WINDOW_OFF	0x02000000L
#endif /* NO_COMPAT_TAR32 */
#ifndef NO_COMPAT_UNARJ32
#define M_MAKE_INDEX_FILE	0x10000000L
#define M_NOT_MAKE_INDEX_FILE	0x20000000L
#endif /* NO_COMPAT_UNARJ32 */
#define EXTRACT_FOUND_FILE	0x40000000L	/* 検索されたファイルを解凍 */
#define EXTRACT_NAMED_FILE	0x80000000L	/* 指定したファイルを解凍 */
#endif /* EXTRACT_FOUND_FILE */

#ifndef SFX_NOT
#define SFX_NOT			0	/* 通常の書庫 */

#ifndef NO_COMPAT_UNLHA32
#define SFX_DOS_S		1	/* LHA's SFX 系 (small) */
#define SFX_DOS_204S		1	/* LHA's SFX 2.04S 以降 */
#define SFX_DOS_213S		1	/* LHA's SFX 2.04S 以降 */
#define SFX_DOS_250S		2	/* LHA's SFX 2.50S 以降 */
#define SFX_DOS_260S		3	/* LHA's SFX 2.60S 以降 */
#define SFX_DOS_265S		3	/* LHA's SFX 2.60S 以降 */

#define SFX_DOS_L		51	/* LHA's SFX 系 (large) */
#define SFX_DOS_204L		51	/* LHA's SFX 2.04L 以降 */
#define SFX_DOS_213L		51	/* LHA's SFX 2.04L 以降 */
#define SFX_DOS_250L		52	/* LHA's SFX 2.50L 以降 */
#define SFX_DOS_260L		53	/* LHA's SFX 2.60L 以降 */
#define SFX_DOS_265L		53	/* LHA's SFX 2.60L 以降 */

#define SFX_DOS_LARC		201	/* SFX by LARC 系 */
#define SFX_DOS_LARC_S		201	/* SFX by LARC (small) */

#define SFX_DOS_LHARC		301	/* LHarc's SFX 系 */
#define SFX_DOS_LHARC_S		301	/* LHarc's SFX (small) */
#define SFX_DOS_LHARC_L		351	/* LHarc's SFX (large) */

#define SFX_WIN16_213		1001	/* LHA's SFX 2.13.w16 系 */
#define SFX_WIN16_213_1		1001	/* WinSFX 2.13.w16.1 */
#define SFX_WIN16_213_2		1002	/* WinSFX 2.13.w16.2 */
#define SFX_WIN16_213_3		1003	/* WinSFX 2.13.w16.3 以降 */

#define SFX_WIN16_250		1011	/* LHA's SFX 2.50.w16 系 */
#define SFX_WIN16_250_1		1011	/* WinSFXM 2.50.w16.0001 以降 */
#define SFX_WIN16_255_1		1021	/* WinSFXM 2.55.w16.0001 以降 */

#define SFX_WIN32_213		2001	/* LHA's SFX 2.13.w32 系 */
#define SFX_WIN32_213_1		2001	/* WinSFX32 2.13.w32.1 以降 */
#define SFX_WIN32_213_3		2002	/* WinSFX32 2.13.w32.3 以降 */

#define SFX_WIN32_250		2011	/* LHA's SFX 2.50.w32 系 */
#define SFX_WIN32_250_1		2011	/* WinSFX32M 2.50.w32.0001 以降 */
#define SFX_WIN32_250_6		2012	/* WinSFX32M 2.50.w32.0006 以降 */

#define SFX_LZHSFX		2051	/* LZHSFX 系 */
#define SFX_LZHSFX_1002		2051	/* LZHSFX 1.0.0.2 以降 */
#define SFX_LZHSFX_1100		2052	/* LZHSFX 1.1.0.0 以降 */

#define SFX_LZHAUTO		2101	/* LZHAUTO 系 */
#define SFX_LZHAUTO_0002	2101	/* LZHAUTO 0.0.0.2 以降 */
#define SFX_LZHAUTO_1000	2102	/* LZHAUTO 1.0.0.0 以降 */
#define SFX_LZHAUTO_1002	2102	/* LZHAUTO 1.0.0.0 以降 */
#define SFX_LZHAUTO_1100	2103	/* LZHAUTO 1.1.0.0 以降 */

#define SFX_WIN32_LHASA		3001	/* Lhasa インストーラ */

#define SFX_DOS_UNKNOWN		9901	/* 認識できない DOS SFX */
#define SFX_WIN16_UNKNOWN	9911	/* 認識できない Win16 SFX */
#define SFX_WIN32_UNKNOWN	9921	/* 認識できない Win32 SFX */
#endif /* NO_COMPAT_UNLHA32 */
#endif /* SFX_NOT */

#if !defined(ARCHIVETYPE_TAR) && !defined(NO_COMPAT_TAR32)
#define ARCHIVETYPE_TAR   1
#define ARCHIVETYPE_TARGZ 2
#define ARCHIVETYPE_TARZ  3
#define ARCHIVETYPE_GZ    4
#define ARCHIVETYPE_Z     5
#define ARCHIVETYPE_TARBZ2 6
#define ARCHIVETYPE_BZ2   7
#define ARCHIVETYPE_TARLZMA 8
#define ARCHIVETYPE_TARXZ 9
#define ARCHIVETYPE_LZMA  10
#define ARCHIVETYPE_XZ    11
#endif /* !ARCHIVETYPE_TAR && !NO_COMPAT_TAR32 */

#ifndef WM_ARCEXTRACT
#define WM_ARCEXTRACT "wm_arcextract"

#define ARCEXTRACT_BEGIN        0       /* 該当ファイルの処理の開始 */
#define ARCEXTRACT_INPROCESS    1       /* 該当ファイルの展開中 */
#define ARCEXTRACT_END          2       /* 処理終了、関連メモリを開放 */
#define ARCEXTRACT_OPEN         3       /* 該当書庫の処理の開始 */
#define ARCEXTRACT_COPY         4       /* ワークファイルの書き戻し */

typedef BOOL (CALLBACK *LPARCHIVERPROC)(HWND, UINT, UINT, LPEXTRACTINGINFOEX);

#endif /* WM_ARCEXTRACT */

#ifdef __cplusplus
}
#endif

#endif /* __COMM_ARC_H__ */
