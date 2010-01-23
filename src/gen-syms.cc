#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <mbctype.h>
#define NOT_COMPILE_TIME
#include "cdecl.h"
#include "symbol.h"
#include "function.h"

struct symbols
{
  const char *name;
  const char *fn;
  const char *sym;
  int req;
  int opt;
  int flags;
  const char *interactive;
  int offset;
  int len;
  int iindex;
};

#define STR(a) #a
#define DEF(a, b, c, d, e, f, g) {STR (a), STR (b), STR (c), d, e, f, g}
#define DEFX(a, b, c, d, e, f, g) {a, STR (b), STR (c), d, e, f, g}

#define CAT CONCAT

#define DEFSF(a, b, c) DEF (a, b, c, 2, 0, FFspecial_form, 0)
#define DEFSF2(lname, cname) DEFSF (lname, CAT (F, cname), CAT (S, cname))
#define DEFSF3(name) DEFSF (name, CAT (F, name), CAT (S, name))
#define DEFSF3Q(name) DEFSF (name, CAT (F, name), CAT (Q, name))
#define SI_DEFSF3(name) DEFSF (name, CAT (Fsi_, name), CAT (Ssi_, name))

#define DEFMACRO(a, b, c) DEF (a, b, c, 2, 0, FFspecial_form | FFmacro, 0)
#define DEFMACRO3(name) DEFMACRO (name, CAT (F, name), CAT (S, name))
#define DEFMACRO3Q(name) DEFMACRO (name, CAT (F, name), CAT (Q, name))

#define DEFPMACRO(a, b, c) DEF (a, b, c, 2, 0, FFspecial_form | FFpseudo_macro, 0)
#define DEFPMACRO3(name) DEFPMACRO (name, CAT (F, name), CAT (S, name))
#define DEFPMACRO3Q(name) DEFPMACRO (name, CAT (F, name), CAT (Q, name))

#define DEFUN(a, b, c, d, e, f) DEF (a, b, c, d, e, f, 0)
#define DEFUN2(lname, cname, req, opt, f) \
  DEFUN (lname, CAT (F, cname), CAT (S, cname), req, opt, f)
#define DEFUN3(name, req, opt, f) \
  DEFUN (name, CAT (F, name), CAT (S, name), req, opt, f)
#define DEFUN3Q(name, req, opt, f) \
  DEFUN (name, CAT (F, name), CAT (Q, name), req, opt, f)
#define SI_DEFUN2X(lname, cname, req, opt, f) \
  DEFX (lname, CAT (Fsi_, cname), CAT (Ssi_, cname), req, opt, f, 0)
#define SI_DEFUN3(name, req, opt, f) \
  DEFUN (name, CAT (Fsi_, name), CAT (Ssi_, name), req, opt, f)

#define DEFCMD(a, b, c, d, e, f, g) DEF (a, b, c, d, e, f, g)
#define DEFCMD2(lname, cname, req, opt, f, g) \
  DEFCMD (lname, CAT (F, cname), CAT (S, cname), req, opt, f, g)
#define DEFCMD3(name, req, opt, f, g) \
  DEFCMD (name, CAT (F, name), CAT (S, name), req, opt, f, g)

#define VDEF(a, b, c) {STR (a), 0, STR (b), 0, 0, c}

#define DEFCONST(a, b) VDEF (a, b, SFconstant | SFspecial)
#define DEFCONST2Q(name) DEFCONST (name, CAT (Q, name))
#define DEFKWD DEFCONST
#define DEFKWD2(name) DEFCONST (name, CAT (K, name))
#define DEFVAR(a, b) VDEF (a, b, SFspecial)
#define DEFVAR2(name) DEFVAR (name, CAT (V, name))
#define SI_DEFVAR2(name) DEFVAR (name, CAT (Vsi_, name))
#define DEFLAMBDAKEY(a, b) VDEF (a, b, SFconstant | SFlambda_key)
#define MAKE_SYMBOL(a, b) VDEF (a, b, 0)
#define MAKE_SYMBOL2(name) MAKE_SYMBOL (name, CAT (V, name))
#define MAKE_SYMBOL2Q(name) MAKE_SYMBOL (name, CAT (Q, name))
#define MAKE_SYMBOL2QC(name) MAKE_SYMBOL (name, CAT (QC, name))
#define MAKE_SYMBOL2F(name, f) VDEF (name, CAT (V, name), f)
#define SI_MAKE_SYMBOL2(name) MAKE_SYMBOL (name, CAT (Vsi_, name))

#define DEFCONDITION(a, b, c, d) {0, 0, "Q" STR (a), 0, 0, 0}

static symbols lsp[] =
{
  /* eval.cc */
  DEFSF3Q (quote),
  DEFSF3Q (function),
  DEFSF3 (progn),
  DEFSF3 (let),
  DEFSF2 (let*, let_star),
  DEFSF3 (if),
  DEFSF3 (setq),
  DEFUN3 (set, 2, 0, 0),
  DEFUN3 (symbol-value, 1, 0, 0),
  DEFUN3 (special-form-p, 1, 0, 0),
  DEFUN3 (macro-function, 1, 0, 0),
  DEFUN3 (symbol-function, 1, 0, 0),
  DEFUN3 (set-default, 2, 0, 0),
  DEFUN3 (default-value, 1, 0, 0),
  DEFUN3 (boundp, 1, 0, 0),
  DEFUN3 (fboundp, 1, 0, 0),
  DEFUN3 (constantp, 1, 0, 0),
  DEFSF3Q (block),
  DEFSF3 (return-from),
  DEFSF3Q (tagbody),
  DEFSF3 (go),
  DEFSF3 (eval-when),
  DEFSF3 (unwind-protect),
  DEFSF3Q (catch),
  DEFSF3 (throw),
  DEFUN3 (eval, 1, 0, 0),
  DEFUN3 (funcall, 1, 0, FFneed_rest),
  DEFUN3 (apply, 1, 0, FFneed_rest),
  DEFUN3 (evalhook, 3, 1, 0),
  DEFUN3 (applyhook, 4, 0, 0),
  DEFUN3 (mapcar, 1, 0, FFneed_rest),
  DEFUN3 (maplist, 1, 0, FFneed_rest),
  DEFUN3 (mapc, 1, 0, FFneed_rest),
  DEFUN3 (mapl, 1, 0, FFneed_rest),
  DEFUN3 (values-list, 1, 0, 0),
  DEFSF3 (multiple-value-call),
  DEFSF3 (multiple-value-prog1),
  DEFPMACRO3 (multiple-value-setq),
  DEFPMACRO3 (multiple-value-bind),
  DEFUN3 (macroexpand, 1, 1, 0),
  DEFUN3 (macroexpand-1, 1, 1, 0),
  DEFUN3 (makunbound, 1, 0, 0),
  DEFUN3 (fmakunbound, 1, 0, 0),
  DEFUN3 (identity, 1, 0, 0),
  DEFSF3 (flet),
  DEFSF3 (labels),
  DEFSF3 (macrolet),

  DEFCONST2Q (t),
  DEFLAMBDAKEY (&optional, Qoptional),
  DEFLAMBDAKEY (&key, Qkey),
  DEFLAMBDAKEY (&rest, Qrest),
  DEFLAMBDAKEY (&body, Qbody),
  DEFLAMBDAKEY (&aux, Qaux),
  DEFLAMBDAKEY (&whole, Qwhole),
  DEFLAMBDAKEY (&environment, Qenvironment),
  MAKE_SYMBOL2Q (lambda),
  MAKE_SYMBOL2Q (macro),
  MAKE_SYMBOL2Q (special),
  MAKE_SYMBOL2Q (declare),
  MAKE_SYMBOL2Q (toplevel),
  MAKE_SYMBOL2Q (exit-this-level),
  DEFCONST2Q (call-arguments-limit),
  DEFCONST2Q (lambda-parameters-limit),
  DEFCONST2Q (multiple-values-limit),
  MAKE_SYMBOL2Q (compile),

  /* pred.cc */
  DEFUN3 (type-of, 1, 0, 0),
  MAKE_SYMBOL2Q (or),
  MAKE_SYMBOL2Q (and),
  MAKE_SYMBOL2Q (not),
  DEFUN3Q (null, 1, 0, 0),
  DEFUN3 (symbolp, 1, 0, 0),
  DEFUN3Q (atom, 1, 0, 0),
  DEFUN3 (consp, 1, 0, 0),
  DEFUN3 (listp, 1, 0, 0),
  DEFUN3 (numberp, 1, 0, 0),
  DEFUN3 (integerp, 1, 0, 0),
  DEFUN3 (rationalp, 1, 0, 0),
  DEFUN3 (floatp, 1, 0, 0),
  DEFUN3 (short-float-p, 1, 0, 0),
  DEFUN3 (single-float-p, 1, 0, 0),
  DEFUN3 (double-float-p, 1, 0, 0),
  DEFUN3 (long-float-p, 1, 0, 0),
  DEFUN3 (realp, 1, 0, 0),
  DEFUN3 (complexp, 1, 0, 0),
  DEFUN3 (characterp, 1, 0, 0),
  DEFUN3 (stringp, 1, 0, 0),
  DEFUN3 (simple-string-p, 1, 0, 0),
  DEFUN3 (vectorp, 1, 0, 0),
  DEFUN3 (simple-vector-p, 1, 0, 0),
  DEFUN3 (arrayp, 1, 0, 0),
  DEFUN3 (functionp, 1, 0, 0),
  DEFUN3 (compiled-function-p, 1, 0, 0),
  DEFUN3 (packagep, 1, 0, 0),
  DEFUN3 (readtablep, 1, 0, 0),
  DEFUN3 (eq, 2, 0, 0),
  DEFUN3 (eql, 2, 0, 0),
  DEFUN3 (equal, 2, 0, 0),
  DEFUN3 (equalp, 2, 0, 0),

  MAKE_SYMBOL2Q (symbol),
  MAKE_SYMBOL2Q (number),
  MAKE_SYMBOL2Q (integer),
  MAKE_SYMBOL2Q (sequence),
  MAKE_SYMBOL2Q (message),
  MAKE_SYMBOL2Q (compiled-function),
  MAKE_SYMBOL2Q (package),
  MAKE_SYMBOL2Q (hash-table),
  MAKE_SYMBOL2Q (simple-string),
  MAKE_SYMBOL2Q (simple-vector),
  MAKE_SYMBOL2Q (pathname),
  MAKE_SYMBOL2Q (random-state),
  MAKE_SYMBOL2Q (fixnum),
  MAKE_SYMBOL2Q (bignum),
  MAKE_SYMBOL2Q (short-float),
  MAKE_SYMBOL2Q (single-float),
  MAKE_SYMBOL2Q (double-float),
  MAKE_SYMBOL2Q (long-float),
  MAKE_SYMBOL2Q (array),
  MAKE_SYMBOL2Q (stream),
  MAKE_SYMBOL2Q (ratio),
  MAKE_SYMBOL2Q (vector),
  MAKE_SYMBOL2Q (real),
  MAKE_SYMBOL2Q (broadcast-stream),
  MAKE_SYMBOL2Q (concatenated-stream),
  MAKE_SYMBOL2Q (echo-stream),
  MAKE_SYMBOL2Q (file-stream),
  MAKE_SYMBOL2Q (string-stream),
  MAKE_SYMBOL2Q (synonym-stream),
  MAKE_SYMBOL2Q (two-way-stream),
  MAKE_SYMBOL2Q (output-stream),
  MAKE_SYMBOL2Q (input-stream),
  MAKE_SYMBOL2Q (string-input-stream),
  MAKE_SYMBOL2Q (string-output-stream),
  MAKE_SYMBOL2Q (open-stream),
  MAKE_SYMBOL2Q (readtable),

  /* symbol.cc */
  DEFUN3 (get, 2, 1, 0),
  DEFUN3 (remprop, 2, 0, 0),
  DEFUN3 (symbol-plist, 1, 0, 0),
  DEFUN3 (getf, 2, 1, 0),
  DEFUN3 (get-properties, 2, 0, 0),
  DEFUN3 (symbol-name, 1, 0, 0),
  DEFUN3 (make-symbol, 1, 0, 0),
  DEFUN3 (copy-symbol, 1, 1, 0),
  DEFUN3 (symbol-package, 1, 0, 0),
  DEFUN3 (keywordp, 1, 0, 0),

  /* list.cc */
  DEFUN3 (car, 1, 0, 0),
  DEFUN3 (cdr, 1, 0, 0),
  DEFUN3Q (cons, 2, 0, 0),
  DEFUN3 (tree-equal, 2, 0, FFneed_rest),
  DEFUN3 (endp, 1, 0, 0),
  DEFUN3 (list-length, 1, 0, 0),
  DEFUN3 (nth, 2, 0, 0),
  DEFUN3 (nthcdr, 2, 0, 0),
  DEFUN3 (last, 1, 1, 0),
  DEFUN3Q (list, 0, 0, FFneed_rest),
  DEFUN2 (list*, list_star, 0, 0, FFneed_rest),
  DEFUN3 (make-list, 1, 0, FFneed_rest),
  DEFUN3 (append, 0, 0, FFneed_rest),
  DEFUN3 (copy-list, 1, 0, 0),
  DEFUN3 (copy-alist, 1, 0, 0),
  DEFUN3 (copy-tree, 1, 0, 0),
  DEFUN3 (revappend, 2, 0, 0),
  DEFUN3 (nconc, 0, 0, FFneed_rest),
  DEFUN3 (nreconc, 2, 0, 0),
  DEFUN3 (rplaca, 2, 0, 0),
  DEFUN3 (rplacd, 2, 0, 0),
  DEFUN3 (subst, 3, 0, FFneed_rest),
  DEFUN3 (subst-if, 3, 0, FFneed_rest),
  DEFUN3 (subst-if-not, 3, 0, FFneed_rest),
  DEFUN3 (nsubst, 3, 0, FFneed_rest),
  DEFUN3 (nsubst-if, 3, 0, FFneed_rest),
  DEFUN3 (nsubst-if-not, 3, 0, FFneed_rest),
  DEFUN3 (sublis, 2, 0, FFneed_rest),
  DEFUN3 (nsublis, 2, 0, FFneed_rest),
  DEFUN3 (member, 2, 0, FFneed_rest),
  DEFUN3 (member-if, 2, 0, FFneed_rest),
  DEFUN3 (member-if-not, 2, 0, FFneed_rest),
  DEFUN3 (adjoin, 2, 0, FFneed_rest),
  DEFUN3 (acons, 3, 0, 0),
  DEFUN3 (pairlis, 2, 1, 0),
  DEFUN3 (assoc, 2, 0, FFneed_rest),
  DEFUN3 (assoc-if, 2, 0, FFneed_rest),
  DEFUN3 (assoc-if-not, 2, 0, FFneed_rest),
  DEFUN3 (rassoc, 2, 0, FFneed_rest),
  DEFUN3 (rassoc-if, 2, 0, FFneed_rest),
  DEFUN3 (rassoc-if-not, 2, 0, FFneed_rest),

  /* hash.cc */
  DEFUN3 (make-hash-table, 0, 0, FFneed_rest),
  DEFUN3 (hash-table-p, 1, 0, 0),
  DEFUN3 (gethash, 2, 1, 0),
  DEFUN3 (remhash, 2, 0, 0),
  DEFUN3 (clrhash, 1, 0, 0),
  DEFUN3 (hash-table-count, 1, 0, 0),
  DEFUN3 (hash-table-rehash-size, 1, 0, 0),
  DEFUN3 (hash-table-size, 1, 0, 0),
  DEFUN3 (hash-table-test, 1, 0, 0),
  DEFUN3 (sxhash, 1, 0, 0),

  /* sequence.cc */
  DEFUN3 (sequencep, 1, 0, 0),
  DEFUN3 (elt, 2, 0, 0),
  DEFUN3 (subseq, 2, 1, 0),
  DEFUN3 (copy-seq, 1, 0, 0),
  DEFUN3 (length, 1, 0, 0),
  DEFUN3 (reverse, 1, 0, 0),
  DEFUN3 (nreverse, 1, 0, 0),
  DEFUN3 (fill, 2, 0, FFneed_rest),
  DEFUN3 (replace, 2, 0, FFneed_rest),
  DEFUN3 (remove, 2, 0, FFneed_rest),
  DEFUN3 (remove-if, 2, 0, FFneed_rest),
  DEFUN3 (remove-if-not, 2, 0, FFneed_rest),
  DEFUN3 (delete, 2, 0, FFneed_rest),
  DEFUN3 (delete-if, 2, 0, FFneed_rest),
  DEFUN3 (delete-if-not, 2, 0, FFneed_rest),
  DEFUN3 (find, 2, 0, FFneed_rest),
  DEFUN3 (find-if, 2, 0, FFneed_rest),
  DEFUN3 (find-if-not, 2, 0, FFneed_rest),
  DEFUN3 (position, 2, 0, FFneed_rest),
  DEFUN3 (position-if, 2, 0, FFneed_rest),
  DEFUN3 (position-if-not, 2, 0, FFneed_rest),
  DEFUN3 (stable-sort, 2, 0, FFneed_rest),

  /* array.cc */
  DEFUN3 (aref, 1, 0, FFneed_rest),
  DEFUN3 (array-has-fill-pointer-p, 1, 0, 0),
  DEFUN3 (array-total-size, 1, 0, 0),
  DEFCONST2Q (array-rank-limit),
  DEFCONST2Q (array-dimension-limit),
  DEFCONST2Q (array-total-size-limit),
  DEFUN3 (array-element-type, 1, 0, 0),
  DEFUN3 (array-rank, 1, 0, 0),
  DEFUN3 (array-dimension, 2, 0, 0),
  DEFUN3 (array-row-major-index, 1, 0, FFneed_rest),
  DEFUN3 (row-major-aref, 2, 0, 0),
  DEFUN3 (adjustable-array-p, 1, 0, 0),

  /* vector.cc */
  DEFUN3 (svref, 2, 0, 0),
  DEFUN3 (vector-push, 2, 0, 0),
  DEFUN3 (vector-push-extend, 2, 1, 0),
  DEFUN3 (vector-pop, 1, 0, 0),
  DEFUN3 (fill-pointer, 1, 0, 0),

  /* string.cc */
  DEFUN3 (copy-string, 1, 0, 0),
  DEFUN3 (char, 2, 0, 0),
  DEFUN3 (schar, 2, 0, 0),
  DEFUN3 (substring, 2, 1, 0),
  DEFUN2 (string=, string_equal, 2, 0, FFneed_rest),
  DEFUN2 (string-equal, string_equalp, 2, 0, FFneed_rest),
  DEFUN2 (string/=, string_not_equal, 2, 0, FFneed_rest),
  DEFUN2 (string-not-equal, string_not_equalp, 2, 0, FFneed_rest),
  DEFUN2 (string<, string_less, 2, 0, FFneed_rest),
  DEFUN3 (string-lessp, 2, 0, FFneed_rest),
  DEFUN2 (string>, string_greater, 2, 0, FFneed_rest),
  DEFUN3 (string-greaterp, 2, 0, FFneed_rest),
  DEFUN2 (string<=, string_not_greater, 2, 0, FFneed_rest),
  DEFUN3 (string-not-greaterp, 2, 0, FFneed_rest),
  DEFUN2 (string>=, string_not_less, 2, 0, FFneed_rest),
  DEFUN3 (string-not-lessp, 2, 0, FFneed_rest),
  DEFUN3 (string-left-trim, 2, 0, 0),
  DEFUN3 (string-right-trim, 2, 0, 0),
  DEFUN3 (string-trim, 2, 0, 0),
  DEFUN3 (string-upcase, 1, 0, FFneed_rest),
  DEFUN3 (string-downcase, 1, 0, FFneed_rest),
  DEFUN3 (string-capitalize, 1, 0, FFneed_rest),
  DEFUN3 (nstring-upcase, 1, 0, FFneed_rest),
  DEFUN3 (nstring-downcase, 1, 0, FFneed_rest),
  DEFUN3 (nstring-capitalize, 1, 0, FFneed_rest),
  DEFUN3Q (string, 1, 0, 0),
  DEFUN3 (parse-integer, 1, 0, FFneed_rest),

  /* number.cc */
  DEFUN3 (zerop, 1, 0, 0),
  DEFUN3 (plusp, 1, 0, 0),
  DEFUN3 (minusp, 1, 0, 0),
  DEFUN3 (oddp, 1, 0, 0),
  DEFUN3 (evenp, 1, 0, 0),
  DEFUN2 (=, number_eql, 1, 0, FFneed_rest),
  DEFUN2 (/=, number_not_eql, 0, 0, FFneed_rest),
  DEFUN2 (<, number_less, 1, 0, FFneed_rest),
  DEFUN2 (>, number_greater, 1, 0, FFneed_rest),
  DEFUN2 (<=, number_not_greater, 1, 0, FFneed_rest),
  DEFUN2 (>=, number_not_less, 1, 0, FFneed_rest),
  DEFUN3 (max, 1, 0, FFneed_rest),
  DEFUN3 (min, 1, 0, FFneed_rest),
  DEFUN2 (+, add, 0, 0, FFneed_rest),
  DEFUN2 (-, subtract, 1, 0, FFneed_rest),
  DEFUN2 (*, multiply, 0, 0, FFneed_rest),
  DEFUN2 (/, divide, 1, 0, FFneed_rest),
  DEFUN3 (conjugate, 1, 0, 0),
  DEFUN3 (gcd, 0, 0, FFneed_rest),
  DEFUN3 (lcm, 0, 0, FFneed_rest),
  DEFUN3 (exp, 1, 0, 0),
  DEFUN3 (expt, 2, 0, 0),
  DEFUN3 (log, 1, 1, 0),
  DEFUN3 (sqrt, 1, 0, 0),
  DEFUN3 (isqrt, 1, 0, 0),
  DEFUN3 (abs, 1, 0, 0),
  DEFUN3 (signum, 1, 0, 0),
  DEFUN3 (sin, 1, 0, 0),
  DEFUN3 (cos, 1, 0, 0),
  DEFUN3 (tan, 1, 0, 0),
  DEFUN3 (asin, 1, 0, 0),
  DEFUN3 (acos, 1, 0, 0),
  DEFUN3 (atan, 1, 1, 0),
  DEFCONST2Q (pi),
  DEFUN3Q (float, 1, 1, 0),
  DEFUN3Q (rational, 1, 0, 0),
  DEFUN3Q (complex, 1, 1, 0),
  DEFUN3 (realpart, 1, 0, 0),
  DEFUN3 (imagpart, 1, 0, 0),
  DEFUN3 (rationalize, 1, 0, 0),
  DEFUN3 (numerator, 1, 0, 0),
  DEFUN3 (denominator, 1, 0, 0),
  DEFUN3 (floor, 1, 1, 0),
  DEFUN3 (ceiling, 1, 1, 0),
  DEFUN3 (truncate, 1, 1, 0),
  DEFUN3 (round, 1, 1, 0),
  DEFUN3 (rem, 2, 0, 0),
  DEFUN3 (mod, 2, 0, 0),
  DEFUN3 (ffloor, 1, 1, 0),
  DEFUN3 (fceiling, 1, 1, 0),
  DEFUN3 (ftruncate, 1, 1, 0),
  DEFUN3 (fround, 1, 1, 0),
  DEFUN3 (decode-float, 1, 0, 0),
  DEFUN3 (scale-float, 2, 0, 0),
  DEFUN3 (float-radix, 1, 0, 0),
  DEFUN3 (float-sign, 1, 1, 0),
  DEFUN3 (float-digits, 1, 0, 0),
  DEFUN3 (float-precision, 1, 0, 0),
  DEFUN3 (boole, 3, 0, 0),
  DEFUN3 (lognot, 1, 0, 0),
  DEFUN3 (logtest, 2, 0, 0),
  DEFUN3 (logbitp, 2, 0, 0),
  DEFUN3 (ash, 2, 0, 0),
  DEFUN3 (logcount, 1, 0, 0),
  DEFUN3 (integer-length, 1, 0, 0),
  DEFCONST2Q (most-positive-fixnum),
  DEFCONST2Q (most-negative-fixnum),
  DEFCONST2Q (most-positive-short-float),
  DEFCONST2Q (least-positive-short-float),
  DEFCONST2Q (least-negative-short-float),
  DEFCONST2Q (most-negative-short-float),
  DEFCONST2Q (most-positive-single-float),
  DEFCONST2Q (least-positive-single-float),
  DEFCONST2Q (least-negative-single-float),
  DEFCONST2Q (most-negative-single-float),
  DEFCONST2Q (most-positive-double-float),
  DEFCONST2Q (least-positive-double-float),
  DEFCONST2Q (least-negative-double-float),
  DEFCONST2Q (most-negative-double-float),
  DEFCONST2Q (most-positive-long-float),
  DEFCONST2Q (least-positive-long-float),
  DEFCONST2Q (least-negative-long-float),
  DEFCONST2Q (most-negative-long-float),
  DEFCONST2Q (least-positive-normalized-short-float),
  DEFCONST2Q (least-negative-normalized-short-float),
  DEFCONST2Q (least-positive-normalized-single-float),
  DEFCONST2Q (least-negative-normalized-single-float),
  DEFCONST2Q (least-positive-normalized-double-float),
  DEFCONST2Q (least-negative-normalized-double-float),
  DEFCONST2Q (least-positive-normalized-long-float),
  DEFCONST2Q (least-negative-normalized-long-float),
  DEFCONST2Q (short-float-epsilon),
  DEFCONST2Q (single-float-epsilon),
  DEFCONST2Q (double-float-epsilon),
  DEFCONST2Q (long-float-epsilon),
  DEFCONST2Q (short-float-negative-epsilon),
  DEFCONST2Q (single-float-negative-epsilon),
  DEFCONST2Q (double-float-negative-epsilon),
  DEFCONST2Q (long-float-negative-epsilon),

  /* char.cc */
  DEFUN3 (standard-char-p, 1, 0, 0),
  DEFUN3 (graphic-char-p, 1, 0, 0),
  DEFUN3 (alpha-char-p, 1, 0, 0),
  DEFUN3 (upper-case-p, 1, 0, 0),
  DEFUN3 (lower-case-p, 1, 0, 0),
  DEFUN3 (both-case-p, 1, 0, 0),
  DEFUN3 (digit-char-p, 1, 1, 0),
  DEFUN3 (alphanumericp, 1, 0, 0),
  DEFUN2 (char=, char_eql, 1, 0, FFneed_rest),
  DEFUN2 (char/=, char_not_eql, 0, 0, FFneed_rest),
  DEFUN2 (char<, char_less, 1, 0, FFneed_rest),
  DEFUN2 (char>, char_greater, 1, 0, FFneed_rest),
  DEFUN2 (char<=, char_not_greater, 1, 0, FFneed_rest),
  DEFUN2 (char>=, char_not_less, 1, 0, FFneed_rest),
  DEFUN3 (char-equal, 1, 0, FFneed_rest),
  DEFUN3 (char-not-equal, 0, 0, FFneed_rest),
  DEFUN3 (char-lessp, 1, 0, FFneed_rest),
  DEFUN3 (char-greaterp, 1, 0, FFneed_rest),
  DEFUN3 (char-not-greaterp, 1, 0, FFneed_rest),
  DEFUN3 (char-not-lessp, 1, 0, FFneed_rest),
  DEFUN3 (char-code, 1, 0, 0),
  DEFUN3 (code-char, 1, 0, 0),
  DEFUN3 (char-upcase, 1, 0, 0),
  DEFUN3 (char-downcase, 1, 0, 0),
  DEFUN3 (digit-char, 1, 1, 0),
  DEFUN3Q (character, 1, 0, 0),
  DEFCONST2Q (char-code-limit),

  /* lprint.cc */
  DEFUN3 (write, 1, 0, FFneed_rest),
  DEFUN3 (write-char, 1, 1, 0),
  DEFUN3 (terpri, 0, 1, 0),
  DEFUN3 (fresh-line, 0, 1, 0),
  DEFUN3 (finish-output, 0, 1, 0),
  DEFUN3 (write-to-string, 1, 0, FFneed_rest),
  DEFUN3 (format, 2, 0, FFneed_rest),
  DEFUN3 (stack-trace, 0, 2, 0),

  DEFVAR2 (*print-readably*),
  DEFVAR2 (*print-escape*),
  DEFVAR2 (*print-pretty*),
  DEFVAR2 (*print-base*),
  DEFVAR2 (*print-radix*),
  DEFVAR2 (*print-circle*),
  DEFVAR2 (*print-length*),
  DEFVAR2 (*print-level*),

  /* pathname.cc */
  DEFUN3 (merge-pathnames, 1, 1, 0),
  DEFUN3 (namestring, 1, 0, 0),
  DEFUN3 (file-namestring, 1, 0, 0),
  DEFUN3 (directory-namestring, 1, 0, 0),
  DEFUN3 (pathname-host, 1, 0, 0),
  DEFUN3 (pathname-device, 1, 0, 0),
  DEFUN3 (pathname-directory, 1, 0, 0),
  DEFUN3 (pathname-name, 1, 0, 0),
  DEFUN3 (pathname-type, 1, 0, 0),
  DEFUN3 (file-exist-p, 1, 0, 0),
  DEFUN3 (file-readable-p, 1, 0, 0),
  DEFUN3 (file-writable-p, 1, 0, 0),
  DEFUN3 (file-executable-p, 1, 0, 0),
  DEFUN3 (file-directory-p, 1, 0, 0),
  DEFUN3 (special-file-p, 1, 0, 0),
  DEFUN3 (path-equal, 2, 0, 0),
  DEFUN3 (sub-directory-p, 2, 0, 0),
  DEFUN3 (valid-path-p, 1, 0, 0),
  DEFUN3 (check-valid-pathname, 1, 0, 0),
  DEFUN3 (truename, 1, 0, 0),
  DEFUN3 (user-homedir-pathname, 0, 0, 0),
  DEFUN3 (compile-file-pathname, 1, 0, 0),
  DEFUN3 (delete-file, 1, 0, FFneed_rest),
  DEFUN3 (rename-file, 2, 0, FFneed_rest),
  DEFUN3 (copy-file, 2, 0, FFneed_rest),
  DEFUN3 (create-directory, 1, 0, FFneed_rest),
  DEFUN3 (delete-directory, 1, 0, FFneed_rest),
  DEFUN3 (file-length, 1, 0, 0),
  DEFUN3 (file-write-time, 1, 0, 0),
  DEFUN3 (set-file-write-time, 2, 0, 0),
  DEFUN3 (set-per-device-directory, 1, 0, 0),
  DEFVAR2 (*auto-update-per-device-directory*),
  DEFUN3 (get-short-path-name, 1, 0, 0),
  DEFUN3 (find-load-path, 1, 0, 0),

  /* glob.cc */
  DEFUN3 (directory, 1, 0, FFneed_rest),
  DEFUN3 (wild-pathname-p, 1, 0, 0),
  DEFUN3 (pathname-match-p, 2, 0, 0),
  DEFVAR2 (*brackets-is-wildcard-character*),

  /* stream.cc */
  DEFUN3 (streamp, 1, 0, 0),
  DEFUN3 (synonym-stream-p, 1, 0, 0),
  DEFUN3 (broadcast-stream-p, 1, 0, 0),
  DEFUN3 (concatenated-stream-p, 1, 0, 0),
  DEFUN3 (echo-stream-p, 1, 0, 0),
  DEFUN3 (two-way-stream-p, 1, 0, 0),
  DEFUN3 (string-stream-p, 1, 0, 0),
  DEFUN3 (string-input-stream-p, 1, 0, 0),
  DEFUN3 (string-output-stream-p, 1, 0, 0),
  DEFUN3 (open-stream-p, 1, 0, 0),
  DEFUN3 (input-stream-p, 1, 0, 0),
  DEFUN3 (output-stream-p, 1, 0, 0),
  DEFUN3 (open, 1, 0, FFneed_rest),
  DEFUN3 (file-position, 1, 1, 0),
  DEFUN3 (make-synonym-stream, 1, 0, 0),
  DEFUN3 (make-broadcast-stream, 0, 0, FFneed_rest),
  DEFUN3 (make-concatenated-stream, 0, 0, FFneed_rest),
  DEFUN3 (make-two-way-stream, 2, 0, 0),
  DEFUN3 (make-echo-stream, 2, 0, 0),
  DEFUN3 (make-string-input-stream, 1, 2, 0),
  DEFUN3 (make-string-output-stream, 0, 0, 0),
  DEFUN3 (get-output-stream-string, 1, 0, 0),
  DEFUN3 (close, 1, 0, FFneed_rest),
  DEFUN3 (broadcast-stream-streams, 1, 0, 0),
  DEFUN3 (concatenated-stream-streams, 1, 0, 0),
  DEFUN3 (echo-stream-input-stream, 1, 0, 0),
  DEFUN3 (echo-stream-output-stream, 1, 0, 0),
  DEFUN3 (two-way-stream-input-stream, 1, 0, 0),
  DEFUN3 (two-way-stream-output-stream, 1, 0, 0),
  DEFUN3 (synonym-stream-symbol, 1, 0, 0),
  DEFUN3 (interactive-stream-p, 1, 0, 0),

  DEFVAR2 (*terminal-io*),
  DEFVAR2 (*standard-input*),
  DEFVAR2 (*standard-output*),
  DEFVAR2 (*error-output*),
  DEFVAR2 (*query-io*),
  DEFVAR2 (*debug-io*),
  DEFVAR2 (*trace-output*),
  MAKE_SYMBOL2 (read-from-string),

  /* lread.cc */
  DEFUN3 (read, 0, 4, 0),
  DEFUN3 (read-line, 0, 4, 0),
  DEFUN3 (read-char, 0, 4, 0),
  DEFUN3 (unread-char, 1, 1, 0),
  DEFUN3 (peek-char, 0, 5, 0),
  DEFUN3 (listen, 0, 1, 0),
  DEFUN3 (read-char-no-hang, 0, 4, 0),
  DEFUN3 (clear-input, 0, 1, 0),
  DEFUN3 (load, 1, 0, FFneed_rest),
  DEFUN3 (copy-readtable, 0, 2, 0),
  DEFUN3 (read-preserving-whitespace, 0, 4, 0),
  DEFUN3 (set-syntax-from-char, 2, 2, 0),
  DEFUN3 (set-macro-character, 2, 2, 0),
  DEFUN3 (get-macro-character, 1, 1, 0),
  DEFUN3 (make-dispatch-macro-character, 1, 2, 0),
  DEFUN3 (set-dispatch-macro-character, 3, 1, 0),
  DEFUN3 (get-dispatch-macro-character, 2, 1, 0),
  DEFUN3 (read-delimited-list, 1, 2, 0),
  DEFUN3 (readtable-case, 1, 0, 0),

  DEFVAR2 (*load-verbose*),
  DEFVAR2 (*load-print*),
  DEFVAR2 (*load-pathname*),
  DEFVAR2 (*load-path*),
  DEFVAR2 (*features*),
  DEFVAR2 (*read-default-float-format*),
  DEFVAR2 (*readtable*),
  DEFVAR2 (*read-suppress*),

  /* package.cc */
  DEFUN3 (make-package, 1, 0, FFneed_rest),
  DEFUN3 (find-package, 1, 0, 0),
  DEFUN3 (package-name, 1, 0, 0),
  DEFUN3 (package-nicknames, 1, 0, 0),
  DEFUN3 (rename-package, 2, 1, 0),
  DEFUN3 (package-use-list, 1, 0, 0),
  DEFUN3 (package-used-by-list, 1, 0, 0),
  DEFUN3 (package-shadowing-symbols, 1, 0, 0),
  DEFUN3 (list-all-packages, 0, 0, 0),
  DEFUN3 (delete-package, 1, 0, 0),
  DEFUN3 (intern, 1, 1, 0),
  DEFUN3 (find-symbol, 1, 1, 0),
  DEFUN3 (unintern, 1, 1, 0),
  DEFUN3 (export, 1, 1, 0),
  DEFUN3 (unexport, 1, 1, 0),
  DEFUN3 (import, 1, 1, 0),
  DEFUN3 (shadowing-import, 1, 1, 0),
  DEFUN3 (shadow, 1, 1, 0),
  DEFUN3 (use-package, 1, 1, 0),
  DEFUN3 (unuse-package, 1, 1, 0),

  DEFVAR2 (*package*),

  /* random.cc */
  DEFUN3 (random, 1, 1, 0),
  DEFUN3 (make-random-state, 0, 1, 0),
  DEFUN3 (random-state-p, 1, 0, 0),
  DEFVAR2 (*random-state*),
  DEFCONST2Q (internal-time-units-per-second),

  /* data.cc */
  DEFUN3 (gc, 0, 1, 0),
  DEFVAR2 (*garbage-collection-messages*),

  /* environ.cc */
  DEFUN3 (get-decoded-time, 0, 0, 0),
  DEFUN3 (get-universal-time, 0, 0, 0),
  DEFUN3 (decode-universal-time, 1, 1, 0),
  DEFUN3 (encode-universal-time, 6, 1, 0),
  DEFUN3 (get-internal-real-time, 0, 0, 0),
  DEFUN3 (software-type, 0, 0, 0),
  DEFUN3 (software-version, 0, 0, 0),
  DEFUN3 (software-version-display-string, 0, 0, 0),

  /* structure.cc */
  MAKE_SYMBOL (structure, Qstructure),

  /* signal.cc */
  MAKE_SYMBOL2Q (condition),
  MAKE_SYMBOL2Q (simple-condition),
  MAKE_SYMBOL2Q (serious-condition),
  MAKE_SYMBOL2Q (error),
  MAKE_SYMBOL2Q (simple-error),
  MAKE_SYMBOL2Q (plain-error),
  MAKE_SYMBOL2Q (arithmetic-error),
  MAKE_SYMBOL2Q (simple-arithmetic-error),
  MAKE_SYMBOL2Q (division-by-zero),
  MAKE_SYMBOL2Q (floating-point-overflow),
  MAKE_SYMBOL2Q (floating-point-underflow),
  MAKE_SYMBOL2Q (domain-error),
  MAKE_SYMBOL2Q (bignum-overflow),
  MAKE_SYMBOL2Q (power-number-too-large),
  MAKE_SYMBOL2Q (cell-error),
  MAKE_SYMBOL2Q (unbound-variable),
  MAKE_SYMBOL2Q (modify-constant),
  MAKE_SYMBOL2Q (undefined-function),
  MAKE_SYMBOL2Q (control-error),
  MAKE_SYMBOL2Q (target-missing),
  MAKE_SYMBOL2Q (file-error),
  MAKE_SYMBOL2Q (file-not-found),
  MAKE_SYMBOL2Q (path-not-found),
  MAKE_SYMBOL2Q (access-denied),
  MAKE_SYMBOL2Q (invalid-drive),
  MAKE_SYMBOL2Q (current-directory),
  MAKE_SYMBOL2Q (not-same-device),
  MAKE_SYMBOL2Q (write-protected),
  MAKE_SYMBOL2Q (bad-unit),
  MAKE_SYMBOL2Q (device-not-ready),
  MAKE_SYMBOL2Q (sharing-violation),
  MAKE_SYMBOL2Q (lock-violation),
  MAKE_SYMBOL2Q (wrong-disk),
  MAKE_SYMBOL2Q (file-exists),
  MAKE_SYMBOL2Q (not-empty),
  MAKE_SYMBOL2Q (archiver-error),
  MAKE_SYMBOL2Q (network-error),
  MAKE_SYMBOL2Q (file-lost-error),
  MAKE_SYMBOL2Q (package-error),
  MAKE_SYMBOL2Q (simple-package-error),
  MAKE_SYMBOL2Q (program-error),
  MAKE_SYMBOL2Q (simple-program-error),
  MAKE_SYMBOL2Q (format-error),
  MAKE_SYMBOL2Q (no-target),
  MAKE_SYMBOL2Q (bad-macro-form),
  MAKE_SYMBOL2Q (invalid-function),
  MAKE_SYMBOL2Q (invalid-variable-list),
  MAKE_SYMBOL2Q (invalid-lambda-list),
  MAKE_SYMBOL2Q (invalid-keyword-list),
  MAKE_SYMBOL2Q (type-error),
  MAKE_SYMBOL2Q (range-error),
  MAKE_SYMBOL2Q (stream-error),
  MAKE_SYMBOL2Q (end-of-file),
  MAKE_SYMBOL2Q (reader-error),
  MAKE_SYMBOL2Q (too-few-arguments),
  MAKE_SYMBOL2Q (too-many-arguments),
  MAKE_SYMBOL2Q (bad-type-specifier),
  MAKE_SYMBOL2Q (read-only-buffer),
  MAKE_SYMBOL2Q (dde-error),
  MAKE_SYMBOL2Q (dde-timeout),
  MAKE_SYMBOL2Q (dde-busy),
  MAKE_SYMBOL2Q (dde-low-memory),
  MAKE_SYMBOL2Q (dde-no-conv),
  MAKE_SYMBOL2Q (dde-not-processed),
  MAKE_SYMBOL2Q (dde-server-died),
  MAKE_SYMBOL2Q (dde-terminated-transaction),
  MAKE_SYMBOL2Q (storage-condition),
  MAKE_SYMBOL2Q (stack-overflow),
  MAKE_SYMBOL2Q (invalid-byte-code),
  MAKE_SYMBOL2Q (quit),
  MAKE_SYMBOL2Q (silent-quit),
  MAKE_SYMBOL2Q (warning),
  MAKE_SYMBOL2Q (simple-warning),
  MAKE_SYMBOL2Q (socket-error),
};

static symbols sys[] =
{
  /* eval.cc */
  SI_DEFUN3 (*specialp, 1, 0, 0),
  SI_DEFUN3 (*make-constant, 1, 0, 0),
  SI_DEFUN3 (*make-special, 1, 0, 0),
  SI_DEFUN3 (*fset, 2, 0, 0),
  MAKE_SYMBOL (*flet-helper, Vsi_flet_helper),
  SI_DEFUN3 (*find-in-environment, 2, 0, 0),
  SI_DEFUN3 (*set-function-name, 2, 0, 0),

  /* bytecode.cc */
  SI_DEFSF3 (*byte-code),

  /* backquote.cc */
  SI_DEFUN3 (*bq-completely-process, 1, 0, 0),
  SI_MAKE_SYMBOL2 (*bq-simplify*),

  /* symbol.cc */
  SI_DEFUN3 (*remf, 2, 0, 0),
  SI_DEFUN3 (*putprop, 3, 0, 0),
  SI_DEFUN3 (*putf, 3, 0, 0),
  SI_DEFUN3 (*set-symbol-plist, 2, 0, 0),

  /* signal.cc */
  SI_DEFUN3 (*throw-error, 1, 0, 0),
  SI_DEFVAR2 (*condition-handlers*),
  SI_DEFUN3 (*report-simple-condition, 2, 0, 0),
  SI_DEFUN3 (*report-target-missing, 2, 0, 0),
  SI_DEFUN3 (*report-file-error, 2, 0, 0),
  SI_DEFUN3 (*report-file-lost-error, 2, 0, 0),
  SI_DEFUN3 (*report-no-target-for, 2, 0, 0),
  SI_DEFUN3 (*report-reader-error, 2, 0, 0),
  SI_DEFUN3 (*report-simple-package-error, 2, 0, 0),
  SI_DEFUN3 (*report-socket-error, 2, 0, 0),
  SI_DEFVAR2 (*trace-on-error*),
  SI_DEFVAR2 (*report-simple-errors-mildly*),

  /* list.cc */
  SI_DEFUN3 (*tree-find, 2, 0, FFneed_rest),
  SI_DEFUN3 (*tree-count, 2, 0, FFneed_rest),

  /* array.cc */
  SI_DEFUN3 (*aset, 2, 0, FFneed_rest),
  SI_DEFUN3 (*make-array, 6, 0, 0),
  SI_DEFUN3 (*row-major-aset, 3, 0, 0),
  SI_DEFUN3 (*replace-array, 2, 0, 0),

  /* vector.cc */
  SI_DEFUN3 (*make-vector, 7, 0, 0),
  SI_DEFUN3 (*svset, 3, 0, 0),
  SI_DEFUN3 (*set-fill-pointer, 2, 0, 0),
  SI_DEFUN3 (*set-vector-length, 2, 0, 0),

  /* string.cc */
  SI_DEFUN3 (*set-char, 3, 0, 0),
  SI_DEFUN3 (*set-schar, 3, 0, 0),

  /* hash.cc */
  SI_DEFUN3 (*puthash, 3, 0, 0),
  SI_DEFUN3 (*enum-hash-table, 2, 0, 0),

  /* sequence.cc */
  SI_DEFUN3 (*set-elt, 3, 0, 0),
  SI_DEFUN3 (*copy-into-seq, 1, 0, FFneed_rest),

  /* stream.cc */
  SI_DEFUN3 (*make-string-output-stream-from-string, 1, 0, 0),
  SI_DEFUN3 (*get-string-input-stream-index, 1, 0, 0),

  /* package.cc */
  SI_DEFUN3 (*package-internal, 1, 0, 0),
  SI_DEFUN3 (*package-external, 1, 0, 0),
  SI_DEFUN3 (*package-summary, 1, 0, 0),

  /* lprint.cc */
  SI_DEFUN3 (*print-condition, 1, 0, 0),
  SI_DEFUN3 (*condition-string, 1, 0, 0),
  SI_DEFUN3 (*stream-column, 0, 1, 0),
  SI_DEFUN3 (*stream-line-number, 0, 1, 0),

  /* init.cc */
  SI_DEFVAR2 (*command-line-args*),
  SI_DEFUN3 (*startup, 0, 0, 0),

  /* toplev.cc */
  SI_DEFUN3 (*minibuffer-message, 1, 1, 0),
  SI_DEFUN3 (*show-window-foreground, 0, 0, 0),
  SI_DEFUN3 (*activate-toplevel, 0, 0, 0),
  SI_DEFVAR2 (*paste-hook*),

  /* buffer.cc */
  SI_DEFUN3 (*create-wait-object, 0, 0, 0),
  SI_DEFUN3 (*add-wait-object, 1, 1, 0),
  SI_DEFUN3 (*remove-wait-object, 1, 1, 0),

  /* disp.cc */
  SI_DEFVAR2 (*find-motion*),

  /* pred.cc */
  SI_DEFUN3 (*ratiop, 1, 0, 0),
  SI_DEFUN3 (*builtin-function-p, 1, 0, 0),
  SI_DEFUN3 (*dll-function-p, 1, 0, 0),
  SI_DEFUN3 (*c-callable-p, 1, 0, 0),
  SI_DEFUN3 (*bignump, 1, 0, 0),
  SI_DEFUN3 (*fixnump, 1, 0, 0),
  SI_DEFUN3 (*simple-array-p, 1, 0, 0),
  SI_DEFUN3 (*environmentp, 1, 0, 0),

  /* lex.cc */
  SI_DEFUN3 (closure-variable, 1, 0, 0),
  SI_DEFUN3 (closure-function, 1, 0, 0),
  SI_DEFUN3 (closure-frame, 1, 0, 0),
  SI_DEFUN3 (closure-body, 1, 0, 0),
  SI_DEFUN3 (*closurep, 1, 0, 0),
  MAKE_SYMBOL2Q (closure),
  MAKE_SYMBOL (environment, Qsi_environment),

  /* structure.cc */
  MAKE_SYMBOL (structure-definition, Qsi_structure_definition),
  SI_DEFUN3 (*structure-definition-p, 1, 0, 0),
  SI_DEFUN3 (*make-structure-definition, 7, 2, 0),
  SI_DEFUN3 (*structure-definition-name, 1, 0, 0),
  SI_DEFUN3 (*structure-definition-type, 1, 0, 0),
  SI_DEFUN3 (*structure-definition-constructors, 1, 0, 0),
  SI_DEFUN3 (*structure-definition-add-constructor, 2, 0, 0),
  SI_DEFUN3 (*structure-definition-print-function, 1, 0, 0),
  SI_DEFUN3 (*structure-definition-report, 1, 0, 0),
  SI_DEFUN3 (*structure-definition-nslots, 1, 0, 0),
  SI_DEFUN3 (*structure-definition-slot-description, 2, 0, 0),
  SI_DEFUN3 (*structure-definition-named-p, 1, 0, 0),
  SI_DEFUN3 (*structure-definition-read-only-p, 1, 0, 0),
  SI_DEFUN3 (*structure-definition-important-p, 1, 0, 0),
  SI_DEFUN3 (*make-structure-data, 1, 0, 0),
  SI_DEFUN3 (*copy-structure-data, 1, 0, 0),
  SI_DEFUN3 (*structurep, 1, 0, 0),
  SI_DEFUN3 (*structure-definition, 1, 0, 0),
  SI_DEFUN3 (*slot-index, 2, 0, 0),
  SI_DEFUN3 (*slot-value, 2, 0, 0),
  SI_DEFUN3 (*set-slot-value, 3, 0, 0),
  SI_DEFUN3 (*index-slot-value, 2, 0, 0),
  SI_DEFUN3 (*set-index-slot-value, 3, 0, 0),
  SI_DEFUN3 (*structure-subtypep, 2, 0, 0),
  SI_DEFUN3 (*structure-reader, 2, 0, 0),

  /* environ.cc */
  SI_DEFUN3 (system-root, 0, 0, 0),
  SI_DEFUN3 (getenv, 1, 0, 0),
  SI_DEFUN3 (delete-registry-tree, 0, 0, 0),
  SI_DEFUN3 (performance-counter, 0, 0, 0),
  SI_DEFUN3 (dump-image-path, 0, 0, 0),
  DEFCONST2Q (*performance-counter-frequency*),

  /* chunk.cc */
  SI_DEFUN3 (make-chunk, 2, 2, 0),
  SI_DEFUN3 (make-string-chunk, 1, 0, 0),
  SI_DEFUN3 (chunk-data, 1, 0, 0),
  SI_DEFUN3 (chunk-size, 1, 0, 0),
  SI_DEFUN3 (chunk-type, 1, 0, 0),
  SI_DEFUN3 (chunk-owner, 1, 0, 0),
  SI_DEFUN3 (address-of, 1, 0, 0),
  SI_DEFUN3 (fill-chunk, 2, 2, 0),
  SI_DEFUN3 (clear-chunk, 1, 2, 0),
  SI_DEFUN3 (copy-chunk, 2, 3, 0),
  SI_DEFUN3 (unpack-int8, 2, 0, 0),
  SI_DEFUN3 (unpack-uint8, 2, 0, 0),
  SI_DEFUN3 (unpack-int16, 2, 0, 0),
  SI_DEFUN3 (unpack-uint16, 2, 0, 0),
  SI_DEFUN3 (unpack-int32, 2, 0, 0),
  SI_DEFUN3 (unpack-uint32, 2, 0, 0),
  SI_DEFUN3 (unpack-float, 2, 0, 0),
  SI_DEFUN3 (unpack-double, 2, 0, 0),
  SI_DEFUN3 (unpack-string, 2, 2, 0),
  SI_DEFUN3 (pack-int8, 3, 0, 0),
  SI_DEFUN3 (pack-uint8, 3, 0, 0),
  SI_DEFUN3 (pack-int16, 3, 0, 0),
  SI_DEFUN3 (pack-uint16, 3, 0, 0),
  SI_DEFUN3 (pack-int32, 3, 0, 0),
  SI_DEFUN3 (pack-uint32, 3, 0, 0),
  SI_DEFUN3 (pack-float, 3, 0, 0),
  SI_DEFUN3 (pack-double, 3, 0, 0),
  SI_DEFUN3 (pack-string, 3, 1, 0),

  /* dll.cc */
  SI_DEFUN3 (load-dll-module, 1, 0, 0),
  SI_DEFUN3 (make-c-function, 4, 0, 0),
  SI_DEFUN3 (make-c-callable, 3, 0, 0),

  MAKE_SYMBOL (dll-module, Qsi_dll_module),
  MAKE_SYMBOL (c-function, Qsi_c_function),
  MAKE_SYMBOL (c-callable, Qsi_c_callable),
  MAKE_SYMBOL (chunk, Qsi_chunk),

  /* encdec.cc */
  SI_DEFUN3 (base64-decode, 1, 1, 0),
  SI_DEFUN3 (base64-encode, 1, 2, 0),
  SI_DEFUN3 (uuencode, 1, 1, 0),
  SI_DEFUN3 (uudecode, 1, 1, 0),
  SI_DEFUN3 (quoted-printable-decode, 1, 2, 0),
  SI_DEFUN3 (quoted-printable-encode, 1, 2, 0),
  SI_DEFUN3 (www-url-decode, 1, 1, 0),
  SI_DEFUN3 (www-url-encode, 1, 2, 0),
  SI_DEFUN3 (binhex-decode, 1, 1, 0),
  SI_DEFUN3 (md5, 1, 0, 0),
  SI_DEFUN3 (sha-1, 1, 0, 0),
  SI_DEFUN3 (hmac-md5, 2, 0, 0),
  SI_DEFUN3 (hmac-sha-1, 2, 0, 0),

  /* lread.cc */
  SI_DEFUN3 (*load-library, 1, 0, FFneed_rest),
  SI_DEFUN3 (*set-readtable-case, 2, 0, 0),

  SI_DEFUN2X ("\"-reader", double_quote_reader, 2, 0, 0),
  SI_DEFUN2X ("'-reader", single_quote_reader, 2, 0, 0),
  SI_DEFUN2X ("(-reader", open_paren_reader, 2, 0, 0),
  SI_DEFUN2X (")-reader", close_paren_reader, 2, 0, 0),
  SI_DEFUN2X (";-reader", semicolon_reader, 2, 0, 0),
  SI_DEFUN2X (",-reader", comma_reader, 2, 0, 0),
  SI_DEFUN2X ("`-reader", backquote_reader, 2, 0, 0),
  SI_DEFUN2X ("#-reader", dispmacro_reader, 2, 0, 0),
  SI_DEFUN2X ("#\\-reader", number_backslash_reader, 3, 0, 0),
  SI_DEFUN2X ("#'-reader", number_single_quote_reader, 3, 0, 0),

  SI_DEFUN2X ("#(-reader", number_open_paren_reader, 3, 0, 0),
  SI_DEFUN2X ("#:-reader", number_colon_reader, 3, 0, 0),
  SI_DEFUN2X ("#.-reader", number_dot_reader, 3, 0, 0),
  SI_DEFUN2X ("#B-reader", number_B_reader, 3, 0, 0),
  SI_DEFUN2X ("#O-reader", number_O_reader, 3, 0, 0),
  SI_DEFUN2X ("#X-reader", number_X_reader, 3, 0, 0),
  SI_DEFUN2X ("#R-reader", number_R_reader, 3, 0, 0),
  SI_DEFUN2X ("#S-reader", number_S_reader, 3, 0, 0),
  SI_DEFUN2X ("#C-reader", number_C_reader, 3, 0, 0),
  SI_DEFUN2X ("#A-reader", number_A_reader, 3, 0, 0),
  SI_DEFUN2X ("#=-reader", number_equal_reader, 3, 0, 0),
  SI_DEFUN2X ("##-reader", number_number_reader, 3, 0, 0),
  SI_DEFUN2X ("#+-reader", number_plus_reader, 3, 0, 0),
  SI_DEFUN2X ("#--reader", number_minus_reader, 3, 0, 0),
  SI_DEFUN2X ("#|-reader", number_bar_reader, 3, 0, 0),
  SI_DEFVAR2 (*character-name-hash-table*),

  /* Window.cc */
  SI_DEFUN3 (*instance-number, 0, 0, 0),

  /* pane.cc */
  SI_DEFUN3 (plugin-arg, 0, 0, 0),

  /* doc.cc */
  SI_DEFUN3 (*snarf-documentation, 2, 0, 0),
  SI_DEFUN3 (get-documentation-string, 4, 0, 0),

  /* listen.cc */
  SI_DEFVAR2 (*accept-kill-xyzzy*),

  SI_DEFUN3 (inflate-stream, 1, 1, 0),
  //SI_DEFUN3 (deflate-stream, 1, 2, 0),
};

static symbols kwd[] =
{
  DEFKWD2 (test),
  DEFKWD2 (test-not),
  DEFKWD2 (from-end),
  DEFKWD2 (start),
  DEFKWD2 (end),
  DEFKWD2 (start1),
  DEFKWD2 (end1),
  DEFKWD2 (start2),
  DEFKWD2 (end2),
  DEFKWD2 (key),
  DEFKWD2 (size),
  DEFKWD2 (rehash-size),
  DEFKWD2 (initial-element),
  DEFKWD2 (element-type),
  DEFKWD2 (initial-contents),
  DEFKWD2 (fill-pointer),
  DEFKWD2 (stream),
  DEFKWD2 (readably),
  DEFKWD2 (escape),
  DEFKWD2 (pretty),
  DEFKWD2 (base),
  DEFKWD2 (radix),
  DEFKWD2 (count),
  DEFKWD2 (circle),
  DEFKWD2 (length),
  DEFKWD2 (level),
  DEFKWD2 (absolute),
  DEFKWD2 (recursive),
  DEFKWD2 (file-only),
  DEFKWD2 (directory-only),
  DEFKWD2 (show-dots),
  DEFKWD2 (error),
  DEFKWD2 (create),
  DEFKWD2 (input),
  DEFKWD2 (output),
  DEFKWD2 (io),
  DEFKWD2 (probe),
  DEFKWD2 (new-version),
  DEFKWD2 (overwrite),
  DEFKWD2 (append),
  DEFKWD2 (direction),
  DEFKWD2 (if-exists),
  DEFKWD2 (if-does-not-exist),
  DEFKWD2 (if-access-denied),
  DEFKWD2 (force),
  DEFKWD2 (rename),
  DEFKWD2 (rename-and-delete),
  DEFKWD2 (skip),
  DEFKWD2 (newer),
  DEFKWD2 (supersede),
  DEFKWD2 (verbose),
  DEFKWD2 (print),
  DEFKWD2 (execute),
  DEFKWD2 (nicknames),
  DEFKWD2 (use),
  DEFKWD2 (internal),
  DEFKWD2 (external),
  DEFKWD2 (inherited),
  DEFKWD2 (no-suffix),
  DEFKWD2 (no-message),
  DEFKWD2 (and),
  DEFKWD2 (or),
  DEFKWD2 (not),
  DEFKWD2 (data),
  DEFKWD2 (xyzzy),
  DEFKWD2 (ieee-floating-point),
  DEFKWD2 (win32s),
  DEFKWD2 (windows-95),
  DEFKWD2 (windows-98),
  DEFKWD2 (windows-me),
  DEFKWD2 (windows-nt),
  DEFKWD2 (windows-2000),
  DEFKWD2 (windows-xp),
  DEFKWD2 (no-dup),
  DEFKWD2 (case-fold),
  DEFKWD2 (reverse),
  DEFKWD2 (left-bound),
  DEFKWD2 (right-bound),
  DEFKWD2 (symbol),
  DEFKWD2 (tail),
  DEFKWD2 (limit),
  DEFKWD2 (ok),
  DEFKWD2 (ok-cancel),
  DEFKWD2 (abort-retry-ignore),
  DEFKWD2 (yes-no-cancel),
  DEFKWD2 (yes-no),
  DEFKWD2 (retry-cancel),
  DEFKWD2 (hand),
  DEFKWD2 (question),
  DEFKWD2 (exclamation),
  DEFKWD2 (asterisk),
  DEFKWD2 (information),
  DEFKWD2 (stop),
  DEFKWD2 (button1),
  DEFKWD2 (button2),
  DEFKWD2 (button3),
  DEFKWD2 (button4),
  DEFKWD2 (button5),
  DEFKWD2 (abort),
  DEFKWD2 (cancel),
  DEFKWD2 (no),
  DEFKWD2 (retry),
  DEFKWD2 (yes),
  DEFKWD2 (ignore),
  DEFKWD2 (regexp),
  DEFKWD2 (symbol-name),
  DEFKWD2 (function-name),
  DEFKWD2 (command-name),
  DEFKWD2 (variable-name),
  DEFKWD2 (non-trivial-symbol-name),
  DEFKWD2 (exist-file-name),
  DEFKWD2 (file-name),
  DEFKWD2 (file-name-list),
  DEFKWD2 (list),
  DEFKWD2 (list-ignore-case),
  DEFKWD2 (directory-name),
  DEFKWD2 (buffer-name),
  DEFKWD2 (exist-buffer-name),
  DEFKWD2 (no-completions),
  DEFKWD2 (no-match),
  DEFKWD2 (solo-match),
  DEFKWD2 (not-unique),
  DEFKWD2 (junk-allowed),
  DEFKWD2 (integer),
  DEFKWD2 (lisp-sexp),
  DEFKWD2 (literal),
  DEFKWD2 (show),
  DEFKWD2 (no-active),
  DEFKWD2 (maximize),
  DEFKWD2 (minimize),
  DEFKWD2 (hide),
  DEFKWD2 (wait),
#undef environ
  DEFKWD2 (environ),
  DEFKWD2 (exec-directory),
  DEFKWD2 (no-std-handles),
  DEFKWD2 (run),
  DEFKWD2 (exit),
  DEFKWD2 (default),
  DEFKWD2 (title),
  DEFKWD2 (history),
  DEFKWD2 (modified),
  DEFKWD2 (undo),
  DEFKWD2 (redo),
  DEFKWD2 (selection),
  DEFKWD2 (modify-selection),
  DEFKWD2 (any-selection),
  DEFKWD2 (modify-any-selection),
  DEFKWD2 (rectangle),
  DEFKWD2 (modify-rectangle),
  DEFKWD2 (clipboard),
  DEFKWD2 (check),
  DEFKWD2 (disable),
  DEFKWD2 (caption),
  DEFKWD2 (font),
  DEFKWD2 (control),
  DEFKWD2 (button),
  DEFKWD2 (edit),
  DEFKWD2 (static),
  DEFKWD2 (listbox),
  DEFKWD2 (scrollbar),
  DEFKWD2 (combobox),
  DEFKWD2 (spin),
  DEFKWD2 (ascii),
  DEFKWD2 (hiragana),
  DEFKWD2 (katakana),
  DEFKWD2 (password-char),
  DEFKWD2 (non-null),
  DEFKWD2 (must-match),
  DEFKWD2 (enable),
  DEFKWD2 (min),
  DEFKWD2 (max),
  DEFKWD2 (range-error),
  DEFKWD2 (type-error),
  DEFKWD2 (type),
  DEFKWD2 (index),
  DEFKWD2 (end-macro),
  DEFKWD2 (column),
  DEFKWD2 (no-result),
  DEFKWD2 (filter),
  DEFKWD2 (filter-index),
  DEFKWD2 (extension),
  DEFKWD2 (save),
  DEFKWD2 (multiple),
  DEFKWD2 (must-exist),
  DEFKWD2 (explorer),
  DEFKWD2 (hide-read-only),
  DEFKWD2 (read-only),
  DEFKWD2 (file-name-dialog),
  DEFKWD2 (directory-name-dialog),
  DEFKWD2 (related),
  DEFKWD2 (internal-size),
  DEFKWD2 (external-size),
  DEFKWD2 (shared),
  DEFKWD2 (arrow),
  DEFKWD2 (ibeam),
  DEFKWD2 (classes-root),
  DEFKWD2 (current-user),
  DEFKWD2 (local-machine),
  DEFKWD2 (users),
  DEFKWD2 (copy-attributes),
  DEFKWD2 (format-string),
  DEFKWD2 (format-arguments),
  DEFKWD2 (operation),
  DEFKWD2 (operands),
  DEFKWD2 (datum),
  DEFKWD2 (datum1),
  DEFKWD2 (datum2),
  DEFKWD2 (target),
  DEFKWD2 (pathname),
  DEFKWD2 (lost-pathname),
  DEFKWD2 (package),
  DEFKWD2 (expected-type),
  DEFKWD2 (linenum),
  DEFKWD2 (arguments),
  DEFKWD2 (name),
  DEFKWD2 (wild),
  DEFKWD2 (description),
  DEFKWD2 (working-directory),
  DEFKWD2 (desktop),
  DEFKWD2 (network),
  DEFKWD2 (personal),
  DEFKWD2 (programs),
  DEFKWD2 (recent),
  DEFKWD2 (send-to),
  DEFKWD2 (start-menu),
  DEFKWD2 (startup),
  DEFKWD2 (templates),
  DEFKWD2 (int8),
  DEFKWD2 (uint8),
  DEFKWD2 (int16),
  DEFKWD2 (uint16),
  DEFKWD2 (int32),
  DEFKWD2 (uint32),
  DEFKWD2 (float),
  DEFKWD2 (double),
  DEFKWD2 (void),
  DEFKWD2 (encoding),
  DEFKWD2 (text),
  DEFKWD2 (canonical),
  DEFKWD2 (raw),
  DEFKWD2 (binary),
  DEFKWD2 (copy),
  DEFKWD2 (move),
  DEFKWD2 (link),
  DEFKWD2 (char-encoding),
  DEFKWD2 (exact-char-encoding),
  DEFKWD2 (newline-code),
  DEFKWD2 (eol-code),
  DEFKWD2 (share),
  DEFKWD2 (read),
  DEFKWD2 (write),
  DEFKWD2 (read-write),
  DEFKWD2 (any-one),
  DEFKWD2 (string),
  DEFKWD2 (comment),
  DEFKWD2 (tag),
  DEFKWD2 (once),
  DEFKWD2 (command-line),
  DEFKWD2 (depth),
  DEFKWD2 (empty),
  DEFKWD2 (upcase),
  DEFKWD2 (downcase),
  DEFKWD2 (preserve),
  DEFKWD2 (invert),
  DEFKWD2 (incode),
  DEFKWD2 (outcode),
  DEFKWD2 (url),
  DEFKWD2 (backlog),
  DEFKWD2 (reuseaddr),
  DEFKWD2 (callback),
  DEFKWD2 (foreground),
  DEFKWD2 (background),
  DEFKWD2 (underline),
  DEFKWD2 (strike-out),
  DEFKWD2 (prefix),
  DEFKWD2 (extend),
  DEFKWD2 (bold),
  DEFKWD2 (us-ascii),
  DEFKWD2 (jisx0201-kana),
  DEFKWD2 (iso8859-1),
  DEFKWD2 (iso8859-2),
  DEFKWD2 (iso8859-3),
  DEFKWD2 (iso8859-4),
  DEFKWD2 (iso8859-5),
  DEFKWD2 (iso8859-7),
  DEFKWD2 (iso8859-9),
  DEFKWD2 (iso8859-10),
  DEFKWD2 (iso8859-13),
  DEFKWD2 (jisx0208),
  DEFKWD2 (jisx0212),
  DEFKWD2 (cp932),
  DEFKWD2 (gb2312),
  DEFKWD2 (ksc5601),
  DEFKWD2 (big5),
  DEFKWD2 (big5-1),
  DEFKWD2 (big5-2),
  DEFKWD2 (cns11643-1),
  DEFKWD2 (cns11643-2),
  DEFKWD2 (g0),
  DEFKWD2 (g1),
  DEFKWD2 (g2),
  DEFKWD2 (g3),
  DEFKWD2 (cjk),
  DEFKWD2 (jp),
  DEFKWD2 (jp2),
  DEFKWD2 (kr),
  DEFKWD2 (cn),
  DEFKWD2 (cn-gb),
  DEFKWD2 (cn-big5),
  DEFKWD2 (no-escape),
  DEFKWD2 (ascii-eol),
  DEFKWD2 (ascii-control),
  DEFKWD2 (7bits),
  DEFKWD2 (locking-shift),
  DEFKWD2 (short-form),
  DEFKWD2 (use-cns11643),
  DEFKWD2 (signature),
  DEFKWD2 (windows),
  DEFKWD2 (byte-order),
  DEFKWD2 (big-endian),
  DEFKWD2 (little-endian),
  DEFKWD2 (smart),
  DEFKWD2 (imap4-mailbox-name),
  DEFKWD2 (direct-encode-white),
  DEFKWD2 (direct-encode-set-o),
  DEFKWD2 (last-match),
  DEFKWD2 (remote),
  DEFKWD2 (sep),
  DEFKWD2 (left),
  DEFKWD2 (top),
  DEFKWD2 (right),
  DEFKWD2 (bottom),
  DEFKWD2 (first),
  DEFKWD2 (last),
  DEFKWD2 (before),
  DEFKWD2 (after),
  DEFKWD2 (initial-directory),
  DEFKWD2 (label),
  DEFKWD2 (greek),
  DEFKWD2 (cyrillic),
  DEFKWD2 (emacs),
  DEFKWD2 (ish32),
  DEFKWD2 (tar32),
  DEFKWD2 (unlha32),
  DEFKWD2 (unarj32),
  DEFKWD2 (unzip32),
  DEFKWD2 (zip32j),
  DEFKWD2 (cab32),
  DEFKWD2 (unrar32),
  DEFKWD2 (bga32),
  DEFKWD2 (yz1),
  DEFKWD2 (ungca32),
  DEFKWD2 (7-zip),
  DEFKWD2 (close-box),
  DEFKWD2 (insert),
  DEFKWD2 (delete),
  DEFKWD2 (modify),
  DEFKWD2 (file-info),
  DEFKWD2 (no-wrap),
  DEFKWD2 (recycle),
  DEFKWD2 (buffer-bar-order),
  DEFKWD2 (ibmext),
  DEFKWD2 (necext),
  DEFKWD2 (osfjvc),
  DEFKWD2 (vender),
  DEFKWD2 (invalidate),
};

static symbols unint[] =
{
  /* backquote.cc */
  MAKE_SYMBOL2Q (backquote),
  MAKE_SYMBOL2Q (comma),
  MAKE_SYMBOL2Q (comma-atsign),
  MAKE_SYMBOL2Q (comma-dot),
  MAKE_SYMBOL2 (reader-in-backquote),
  MAKE_SYMBOL2 (reader-preserve-white),
  MAKE_SYMBOL2 (reader-label-alist),

  MAKE_SYMBOL2Q (deleted),
  MAKE_SYMBOL2Q (close-paren),
  MAKE_SYMBOL2Q (eof),
  MAKE_SYMBOL2Q (dot),
  MAKE_SYMBOL2Q (home-dir),
  MAKE_SYMBOL2Q (default-dir),
  MAKE_SYMBOL2Q (module-dir),
  MAKE_SYMBOL2Q (system-dir),
  MAKE_SYMBOL2Q (windows-dir),
  MAKE_SYMBOL2Q (bq-list),
  MAKE_SYMBOL2Q (bq-append),
  MAKE_SYMBOL2Q (bq-nconc),
  MAKE_SYMBOL (bq-list*, Qbq_list_star),
  MAKE_SYMBOL2Q (bq-quote),
  MAKE_SYMBOL2Q (bq-clobberable),
  MAKE_SYMBOL2 (*package-list),
  MAKE_SYMBOL2 (*user-package),
  MAKE_SYMBOL2 (*lisp-package),
  MAKE_SYMBOL2 (*system-package),
  MAKE_SYMBOL2 (*keyword-package),
  MAKE_SYMBOL2 (*editor-package),
  MAKE_SYMBOL2 (default-random-state),
  MAKE_SYMBOL2 (default-syntax-table),

  MAKE_SYMBOL2Q (user-config-path),
  MAKE_SYMBOL2 (user-name),
  MAKE_SYMBOL2 (machine-name),
  MAKE_SYMBOL2 (os-major-version),
  MAKE_SYMBOL2 (os-minor-version),
  MAKE_SYMBOL2 (os-build-number),
  MAKE_SYMBOL2 (os-platform),
  MAKE_SYMBOL2 (os-csd-version),

  MAKE_SYMBOL2 (next-prefix-value),
  MAKE_SYMBOL2 (next-prefix-args),

  MAKE_SYMBOL2 (process-list),

  MAKE_SYMBOL2Q (or-string-integer),
  MAKE_SYMBOL2Q (or-symbol-integer),
  MAKE_SYMBOL2Q (or-string-character),
  MAKE_SYMBOL2Q (or-integer-marker),
  MAKE_SYMBOL2Q (or-character-cons),
  MAKE_SYMBOL2Q (or-rational-float),
  MAKE_SYMBOL2Q (or-symbol-string),
  MAKE_SYMBOL2Q (or-string-stream),

  MAKE_SYMBOL2Q (temporary-string),
  MAKE_SYMBOL2 (minibuffer-message),
  MAKE_SYMBOL2 (minibuffer-prompt),
  MAKE_SYMBOL2 (default-menu),
  MAKE_SYMBOL2 (last-active-menu),
  MAKE_SYMBOL2 (tracking-menu),

  MAKE_SYMBOL2 (wstream-stream),

  MAKE_SYMBOL2Q (software-type),
  MAKE_SYMBOL2Q (software-version),
  MAKE_SYMBOL2Q (software-version-display-string),

  MAKE_SYMBOL2Q (imag_two),

  /* signal.cc */
  MAKE_SYMBOL2QC (*condition),
  MAKE_SYMBOL2QC (*simple-condition),
  MAKE_SYMBOL2QC (*serious-condition),
  MAKE_SYMBOL2QC (*error),
  MAKE_SYMBOL2QC (*simple-error),
  MAKE_SYMBOL2QC (*plain-error),
  MAKE_SYMBOL2QC (*arithmetic-error),
  MAKE_SYMBOL2QC (*simple-arithmetic-error),
  MAKE_SYMBOL2QC (*division-by-zero),
  MAKE_SYMBOL2QC (*floating-point-overflow),
  MAKE_SYMBOL2QC (*floating-point-underflow),
  MAKE_SYMBOL2QC (*domain-error),
  MAKE_SYMBOL2QC (*bignum-overflow),
  MAKE_SYMBOL2QC (*power-number-too-large),
  MAKE_SYMBOL2QC (*cell-error),
  MAKE_SYMBOL2QC (*unbound-variable),
  MAKE_SYMBOL2QC (*modify-constant),
  MAKE_SYMBOL2QC (*undefined-function),
  MAKE_SYMBOL2QC (*control-error),
  MAKE_SYMBOL2QC (*target-missing),
  MAKE_SYMBOL2QC (*file-error),
  MAKE_SYMBOL2QC (*file-not-found),
  MAKE_SYMBOL2QC (*path-not-found),
  MAKE_SYMBOL2QC (*access-denied),
  MAKE_SYMBOL2QC (*invalid-drive),
  MAKE_SYMBOL2QC (*current-directory),
  MAKE_SYMBOL2QC (*not-same-device),
  MAKE_SYMBOL2QC (*write-protected),
  MAKE_SYMBOL2QC (*bad-unit),
  MAKE_SYMBOL2QC (*device-not-ready),
  MAKE_SYMBOL2QC (*sharing-violation),
  MAKE_SYMBOL2QC (*lock-violation),
  MAKE_SYMBOL2QC (*wrong-disk),
  MAKE_SYMBOL2QC (*file-exists),
  MAKE_SYMBOL2QC (*not-empty),
  MAKE_SYMBOL2QC (*archiver-error),
  MAKE_SYMBOL2QC (*network-error),
  MAKE_SYMBOL2QC (*file-lost-error),
  MAKE_SYMBOL2QC (*package-error),
  MAKE_SYMBOL2QC (*simple-package-error),
  MAKE_SYMBOL2QC (*program-error),
  MAKE_SYMBOL2QC (*simple-program-error),
  MAKE_SYMBOL2QC (*format-error),
  MAKE_SYMBOL2QC (*no-target),
  MAKE_SYMBOL2QC (*bad-macro-form),
  MAKE_SYMBOL2QC (*invalid-function),
  MAKE_SYMBOL2QC (*invalid-variable-list),
  MAKE_SYMBOL2QC (*invalid-lambda-list),
  MAKE_SYMBOL2QC (*invalid-keyword-list),
  MAKE_SYMBOL2QC (*type-error),
  MAKE_SYMBOL2QC (*range-error),
  MAKE_SYMBOL2QC (*stream-error),
  MAKE_SYMBOL2QC (*end-of-file),
  MAKE_SYMBOL2QC (*reader-error),
  MAKE_SYMBOL2QC (*too-few-arguments),
  MAKE_SYMBOL2QC (*too-many-arguments),
  MAKE_SYMBOL2QC (*bad-type-specifier),
  MAKE_SYMBOL2QC (*read-only-buffer),
  MAKE_SYMBOL2QC (*dde-error),
  MAKE_SYMBOL2QC (*dde-timeout),
  MAKE_SYMBOL2QC (*dde-busy),
  MAKE_SYMBOL2QC (*dde-low-memory),
  MAKE_SYMBOL2QC (*dde-no-conv),
  MAKE_SYMBOL2QC (*dde-not-processed),
  MAKE_SYMBOL2QC (*dde-server-died),
  MAKE_SYMBOL2QC (*dde-terminated-transaction),
  MAKE_SYMBOL2QC (*storage-condition),
  MAKE_SYMBOL2QC (*stack-overflow),
  MAKE_SYMBOL2QC (*invalid-byte-code),
  MAKE_SYMBOL2QC (*quit),
  MAKE_SYMBOL2QC (*silent-quit),
  MAKE_SYMBOL2QC (*warning),
  MAKE_SYMBOL2QC (*simple-warning),
  MAKE_SYMBOL2QC (socket-error),
  MAKE_SYMBOL2Q (uninitialized),

  MAKE_SYMBOL2 (ierror-storage-condition),
  MAKE_SYMBOL2 (ierror-quit),
  MAKE_SYMBOL2 (ierror-silent-quit),
  MAKE_SYMBOL2 (ierror-read-only-buffer),

  MAKE_SYMBOL2 (dll_module_list),
  MAKE_SYMBOL2 (function_bar_labels),

  MAKE_SYMBOL2 (default-readtable),
  MAKE_SYMBOL2 (standard-readtable),

  MAKE_SYMBOL2 (default-kinsoku-bol-chars),
  MAKE_SYMBOL2 (default-kinsoku-eol-chars),

  MAKE_SYMBOL2Q (encoding-sjis),
  MAKE_SYMBOL2Q (encoding-auto),

  MAKE_SYMBOL2 (kbd-encoding),
  MAKE_SYMBOL2 (internal-char-encoding-list),

  MAKE_SYMBOL2 (bypass-evalhook),
  MAKE_SYMBOL2 (bypass-applyhook),

  MAKE_SYMBOL2 (last-status-bar-format),

  MAKE_SYMBOL2Q (dump-image-path),
  MAKE_SYMBOL2 (last-match-string),

  MAKE_SYMBOL2 (popup-list-callback),
};

static symbols ed[] =
{
  /* eval.cc */
  DEFSF3Q (interactive),
  DEFVAR2 (*interactive-specifier-alist*),
  DEFVAR2 (*emacs-interactive-specifier-alist*),
  DEFUN3 (make-local-variable, 1, 0, 0),
  DEFUN3 (make-variable-buffer-local, 1, 0, 0),
  DEFUN3 (kill-all-local-variables, 0, 0, 0),
  DEFUN3 (kill-local-variable, 1, 0, 0),
  DEFUN3 (local-variable-p, 1, 1, 0),
  DEFUN3 (command-execute, 1, 1, 0),
  DEFUN3 (interactive-p, 0, 0, 0),
  DEFSF3 (save-excursion),
  DEFSF3 (save-restriction),
  DEFSF3 (save-window-excursion),
  DEFUN3 (commandp, 1, 0, 0),
  DEFUN3 (buffer-local-value, 2, 0, 0),
  DEFVAR2 (*protected-local-variables*),
  DEFUN3 (run-hooks, 0, 0, FFneed_rest),
  DEFUN3 (run-hook-with-args, 1, 0, FFneed_rest),
  DEFUN3 (run-hook-with-args-while-success, 1, 0, FFneed_rest),
  DEFUN3 (run-hook-with-args-until-success, 1, 0, FFneed_rest),
  DEFVAR2 (*evalhook*),
  DEFVAR2 (*applyhook*),

  /* environ.cc */
  DEFUN3 (write-registry, 3, 0, 0),
  DEFUN3 (write-registry-literally, 3, 0, 0),
  DEFUN3 (read-registry, 2, 1, 0),
  DEFUN3 (list-registry-key, 1, 1, 0),
  DEFUN3 (user-name, 0, 0, 0),
  DEFUN3 (machine-name, 0, 0, 0),
  DEFUN3 (os-major-version, 0, 0, 0),
  DEFUN3 (os-minor-version, 0, 0, 0),
  DEFUN3 (os-build-number, 0, 0, 0),
  DEFUN3 (os-platform, 0, 0, 0),
  DEFUN3 (os-csd-version, 0, 0, 0),
  DEFUN3 (get-windows-directory, 0, 0, 0),
  DEFUN3 (get-system-directory, 0, 0, 0),
  MAKE_SYMBOL2 (win32s),
  MAKE_SYMBOL2 (windows-95),
  MAKE_SYMBOL2 (windows-98),
  MAKE_SYMBOL2 (windows-me),
  MAKE_SYMBOL2 (windows-nt),
  MAKE_SYMBOL2 (windows-2000),
  MAKE_SYMBOL2 (windows-xp),
  DEFUN3 (user-config-path, 0, 0, 0),
  DEFVAR2 (*convert-registry-to-file-p*),

  /* Buffer.cc */
  MAKE_SYMBOL2 (*default-buffer-mode*),
  MAKE_SYMBOL2 (*create-buffer-hook*),
  MAKE_SYMBOL2 (*before-delete-buffer-hook*),
  MAKE_SYMBOL2 (*delete-buffer-hook*),
  MAKE_SYMBOL2 (*default-fileio-encoding*),
  MAKE_SYMBOL2 (*expected-fileio-encoding*),
  MAKE_SYMBOL2 (*default-eol-code*),
  MAKE_SYMBOL2 (*expected-eol-code*),
  MAKE_SYMBOL2F (buffer-read-only, SFmake_buffer_local),
  MAKE_SYMBOL2F (need-not-save, SFmake_buffer_local),
  MAKE_SYMBOL2F (auto-save, SFmake_buffer_local),
  MAKE_SYMBOL2 (mode-line-format),
  MAKE_SYMBOL2 (title-bar-format),
  MAKE_SYMBOL2F (mode-name, SFmake_buffer_local),
  MAKE_SYMBOL2F (buffer-mode, SFmake_buffer_local),
  MAKE_SYMBOL2F (auto-fill, SFmake_buffer_local),
  DEFUN3 (erase-buffer, 1, 0, 0),
  DEFUN3 (buffer-size, 0, 1, 0),
  DEFUN3 (buffer-lines, 0, 1, 0),
  DEFUN3 (create-new-buffer, 1, 0, 0),
  DEFUN3 (selected-buffer, 0, 0, 0),
  DEFUN3 (deleted-buffer-p, 1, 0, 0),
  DEFUN3 (get-next-buffer, 0, 4, 0),
  DEFUN3 (set-buffer, 1, 0, 0),
  DEFUN3 (get-file-buffer, 1, 0, 0),
  DEFUN3 (create-file-buffer, 1, 0, 0),
  DEFUN3 (set-buffer-modified-p, 1, 1, 0),
  DEFUN3 (buffer-modified-p, 0, 1, 0),
  DEFUN3 (buffer-modified-count, 0, 1, 0),
  DEFUN3 (set-buffer-truncated-p, 1, 1, 0),
  DEFUN3 (buffer-truncated-p, 0, 1, 0),
  DEFUN3 (file-visited-p, 0, 1, 0),
  DEFUN3 (delete-buffer, 1, 0, 0),
  DEFUN3 (buffer-name, 1, 0, 0),
  DEFUN3 (find-buffer, 1, 0, 0),
  DEFUN3 (other-buffer, 0, 1, 0),
  DEFUN3 (bury-buffer, 0, 1, 0),
  DEFUN3 (set-buffer-file-name, 1, 1, 0),
  DEFUN3 (get-buffer-file-name, 0, 1, 0),
  DEFUN3 (set-buffer-alternate-file-name, 1, 1, 0),
  DEFUN3 (get-buffer-alternate-file-name, 0, 1, 0),
  DEFUN3 (default-directory, 0, 1, 0),
  DEFUN3 (set-default-directory, 1, 1, 0),
  DEFCMD3 (rename-buffer, 1, 1, 0, "sRename buffer: "),
  DEFUN3 (buffer-fileio-encoding, 0, 1, 0),
  DEFUN3 (buffer-eol-code, 0, 1, 0),
  DEFUN3 (set-buffer-fileio-encoding, 1, 1, 0),
  DEFUN3 (set-buffer-eol-code, 1, 1, 0),
  DEFUN3 (buffer-list, 0, 0, FFneed_rest),
  DEFUN3 (enum-buffers, 1, 0, 0),
  DEFUN3 (find-name-buffer, 1, 0, 0),
  DEFUN3 (need-buffer-save-p, 1, 0, 0),
  DEFUN3 (count-modified-buffers, 0, 0, 0),
  DEFUN3 (count-buffers, 0, 1, 0),
  DEFCMD3 (kill-xyzzy, 0, 0, 0, ""),
  DEFUN3 (lock-file, 0, 1, 0),
  DEFUN3 (unlock-file, 0, 1, 0),
  DEFUN3 (file-locked-p, 0, 1, 0),
  MAKE_SYMBOL2 (exclusive-lock-file),
  DEFUN3 (set-buffer-colors, 1, 1, 0),
  DEFUN3 (get-buffer-colors, 0, 1, 0),
  DEFVAR2 (*change-buffer-colors-hook*),
  DEFVAR2 (*sort-buffer-list-by-created-order*),
  DEFVAR2 (*title-bar-text-order*),
  DEFUN3 (set-default-fold-width, 1, 0, 0),
  DEFUN3 (set-buffer-fold-width, 1, 1, 0),
  DEFUN3 (default-fold-width, 0, 0, 0),
  DEFUN3 (buffer-fold-width, 0, 1, 0),
  DEFUN3 (buffer-fold-column, 0, 1, 0),
  DEFUN3 (default-line-number-mode, 0, 0, 0),
  DEFUN3 (buffer-line-number-mode, 0, 1, 0),
  DEFUN3 (set-default-line-number-mode, 1, 0, 0),
  DEFUN3 (set-buffer-line-number-mode, 1, 1, 0),
  DEFVAR2 (*kill-xyzzy-hook*),
  DEFUN3 (last-modified-point, 0, 1, 0),
  DEFUN3 (update-mode-line, 0, 1, 0),
  DEFUN3 (set-kinsoku-chars, 2, 1, 0),
  DEFUN3 (kinsoku-bol-chars, 0, 1, 0),
  DEFUN3 (kinsoku-eol-chars, 0, 1, 0),
  DEFUN3 (kinsoku-mode, 0, 1, 0),
  DEFUN3 (set-kinsoku-mode, 1, 1, 0),
  DEFUN3 (kinsoku-extend-limit, 0, 1, 0),
  DEFUN3 (set-kinsoku-extend-limit, 1, 1, 0),
  DEFUN3 (kinsoku-shorten-limit, 0, 1, 0),
  DEFUN3 (set-kinsoku-shorten-limit, 1, 1, 0),
  DEFVAR2 (*query-kill-xyzzy-hook*),
  DEFUN3 (set-hjump-columns, 1, 1, 0),
  DEFUN3 (hjump-columns, 0, 1, 0),
  DEFUN3 (refresh-title-bar, 0, 0, 0),
  DEFUN3 (create-buffer-bar, 0, 0, 0),
  MAKE_SYMBOL2 (buffer-bar),
  DEFVAR2 (*buffer-bar-selected-buffer-to-first*),
  DEFVAR2 (*buffer-bar-hook*),
  DEFVAR2 (*initial-buffer-mode*),
  MAKE_SYMBOL2 (*buffer-bar-context-menu-handler*),
  MAKE_SYMBOL2 (buffer-in-any-pseudo-frame-p),
  DEFUN3 (buffer-ime-mode, 0, 1, 0),
  DEFUN3 (set-buffer-ime-mode, 1, 1, 0),
  MAKE_SYMBOL2 (post-buffer-modified-hook),
  DEFUN3 (enable-post-buffer-modified-hook, 1, 1, 0),
  DEFUN3 (post-buffer-modified-hook-enabled-p, 0, 1, 0),

  /* insdel.cc */
  DEFUN3 (insert-file-contents, 1, 3, 0),
  DEFUN3 (insert, 0, 0, FFneed_rest),
  DEFCMD3 (delete-region, 2, 0, 0, "*r"),
  DEFUN3 (buffer-substring, 2, 0, 0),
  MAKE_SYMBOL2 (overwrite-mode),
  DEFUN3 (copy-to-clipboard, 1, 0, 0),
  DEFUN3 (get-clipboard-data, 0, 0, 0),
  DEFUN3 (clipboard-empty-p, 0, 0, 0),
  DEFVAR2 (*clipboard-newer-than-kill-ring-p*),
  DEFVAR2 (*kill-ring-newer-than-clipboard-p*),
  DEFUN3 (set-text-attribute, 3, 0, FFneed_rest),
  DEFUN3 (clear-all-text-attributes, 0, 0, 0),
  DEFUN3 (delete-text-attributes, 1, 0, FFneed_rest),
  DEFUN3 (delete-text-attributes-if, 1, 0, FFneed_rest),
  DEFUN3 (delete-text-attributes-if-not, 1, 0, FFneed_rest),
  DEFUN3 (find-text-attribute, 1, 0, FFneed_rest),
  DEFUN3 (find-text-attribute-if, 1, 0, FFneed_rest),
  DEFUN3 (find-text-attribute-if-not, 1, 0, FFneed_rest),
  DEFUN3 (modify-text-attributes, 1, 0, FFneed_rest),
  DEFUN3 (modify-text-attributes-if, 1, 0, FFneed_rest),
  DEFUN3 (modify-text-attributes-if-not, 1, 0, FFneed_rest),
  DEFUN3 (list-text-attributes, 0, 2, 0),
  DEFUN3 (find-text-attribute-point, 1, 0, 0),
  DEFUN3 (delete-text-attribute-point, 1, 0, 0),
  MAKE_SYMBOL2 (*clipboard-char-encoding*),
#if 0
  MAKE_SYMBOL2 (*pre-insert-hook*),
  MAKE_SYMBOL2 (*post-insert-hook*),
  MAKE_SYMBOL2 (*pre-delete-hook*),
  MAKE_SYMBOL2 (*post-delete-hook*),
  MAKE_SYMBOL2 (*pre-modify-hook*),
  MAKE_SYMBOL2 (*post-modify-hook*),
#endif
  /* data.cc */
  DEFUN3 (dump-xyzzy, 0, 1, 0),
  DEFUN3 (xyzzy-dumped-p, 0, 0, 0),

  /* pred.cc */
  DEFUN3 (windowp, 1, 0, 0),
  DEFUN3 (bufferp, 1, 0, 0),
  DEFUN3 (syntax-table-p, 1, 0, 0),
  DEFUN3 (markerp, 1, 0, 0),
  DEFUN3 (regexpp, 1, 0, 0),
  DEFUN3 (processp, 1, 0, 0),
  DEFUN3 (menup, 1, 0, 0),
  DEFUN3 (dde-handle-p, 1, 0, 0),
  DEFUN3 (oledatap, 1, 0, 0),
  DEFUN3 (wait-object-p, 1, 0, 0),
  DEFUN3 (char-encoding-p, 1, 0, 0),
  MAKE_SYMBOL2Q (window),
  MAKE_SYMBOL2Q (buffer),
  MAKE_SYMBOL2Q (marker),
  MAKE_SYMBOL2Q (regexp),
  MAKE_SYMBOL2Q (process),
  MAKE_SYMBOL2Q (menu),
  MAKE_SYMBOL2Q (dde-handle),
  MAKE_SYMBOL2Q (oledata),
  MAKE_SYMBOL2Q (wait-object),
  MAKE_SYMBOL2Q (char-encoding),

  /* stream.cc */
  DEFUN3 (status-window-stream-p, 1, 0, 0),
  DEFUN3 (buffer-stream-p, 1, 0, 0),
  DEFUN3 (make-buffer-stream, 1, 2, 0),
  DEFUN3 (buffer-stream-buffer, 1, 0, 0),
  DEFUN3 (buffer-stream-point, 1, 0, 0),
  DEFUN3 (buffer-stream-set-point, 2, 0, 0),
  DEFVAR2 (*status-window*),
  DEFUN3 (keyboard-stream-p, 1, 0, 0),
  DEFCONST2Q (*keyboard*),
  DEFUN3 (socket-stream-p, 1, 0, 0),
  DEFUN3 (connect, 2, 0, FFneed_rest),
  DEFUN3 (make-listen-socket, 0, 2, FFneed_rest),
  DEFUN3 (accept-connection, 1, 0, FFneed_rest),
  DEFUN3 (socket-stream-local-name, 1, 0, 0),
  DEFUN3 (socket-stream-local-address, 1, 0, 0),
  DEFUN3 (socket-stream-local-port, 1, 0, 0),
  DEFUN3 (socket-stream-peer-name, 1, 0, 0),
  DEFUN3 (socket-stream-peer-address, 1, 0, 0),
  DEFUN3 (socket-stream-peer-port, 1, 0, 0),
  DEFUN3 (socket-stream-set-timeout, 2, 0, 0),
  DEFUN3 (socket-stream-get-timeout, 1, 0, 0),
  DEFUN3 (socket-stream-set-oob-inline, 2, 0, 0),
  DEFUN3 (socket-stream-send-oob-data, 2, 0, 0),
  DEFUN3 (stream-encoding, 1, 0, 0),
  DEFUN3 (set-stream-encoding, 2, 0, 0),
  DEFUN3 (set-end-of-file, 1, 0, 0),
  DEFUN3 (make-general-input-stream, 1, 2, 0),
  DEFUN3 (make-general-output-stream, 1, 2, 0),
  DEFUN3 (general-input-stream-p, 1, 0, 0),
  DEFUN3 (general-output-stream-p, 1, 0, 0),
  MAKE_SYMBOL2Q (buffer-stream),
  MAKE_SYMBOL2Q (socket-stream),

  /* move.cc */
  DEFCMD3 (forward-char, 0, 1, 0, "p"),
  DEFCMD3 (forward-line, 0, 1, 0, "p"),
  DEFCMD3 (forward-word, 0, 1, 0, "p"),
  DEFCMD3 (forward-virtual-line, 0, 1, 0, "p"),
  DEFUN3 (goto-bol, 0, 0, 0),
  DEFUN3 (goto-eol, 0, 0, 0),
  DEFUN3 (goto-virtual-bol, 0, 0, 0),
  DEFUN3 (goto-virtual-eol, 0, 0, 0),
  DEFUN3 (point-min, 0, 0, 0),
  DEFUN3 (point-max, 0, 0, 0),
  DEFUN3 (goto-char, 1, 0, 0),
  DEFUN3 (goto-column, 1, 1, 0),
  DEFUN3 (goto-virtual-column, 1, 1, 0),
  DEFUN3 (forward-column, 2, 1, 0),
  DEFUN3 (point, 0, 0, 0),
  DEFUN3 (set-mark, 0, 1, 0),
  DEFUN3 (mark, 0, 1, 0),
  DEFUN3 (region-beginning, 0, 0, 0),
  DEFUN3 (region-end, 0, 0, 0),
  DEFUN3 (continue-pre-selection, 0, 0, 0),
  DEFUN3 (pre-selection-p, 0, 0, 0),
  DEFUN3 (get-selection-type, 0, 0, 0),
  DEFUN3 (set-selection-type, 1, 1, 0),
  DEFUN3 (start-selection, 1, 2, 0),
  DEFUN3 (stop-selection, 0, 0, 0),
  DEFUN3 (fix-selection-point, 0, 0, 0),
  DEFUN3 (selection-point, 0, 0, 0),
  DEFUN3 (selection-mark, 0, 0, 0),
  DEFUN3 (reverse-region, 2, 1, 0),
  DEFUN3 (clear-reverse-region, 0, 0, 0),
  DEFUN3 (make-marker, 0, 1, 0),
  DEFUN3 (set-marker, 1, 1, 0),
  DEFUN3 (unset-marker, 1, 0, 0),
  DEFUN3 (goto-marker, 1, 0, 0),
  DEFUN3 (delete-marker, 1, 0, 0),
  DEFUN3 (marker-point, 1, 0, 0),
  DEFUN3 (marker-buffer, 1, 0, 0),
  DEFUN3 (scroll-window, 1, 0, 0),
  DEFUN3 (scroll-window-horizontally, 1, 0, 0),
  DEFCMD3 (narrow-to-region, 2, 0, 0, "r"),
  DEFCMD3 (widen, 0, 0, 0, ""),
  DEFVAR2 (*scroll-bar-step*),
  DEFUN3 (bolp, 0, 0, 0),
  DEFUN3 (eolp, 0, 0, 0),
  DEFUN3 (virtual-bolp, 0, 0, 0),
  DEFUN3 (virtual-eolp, 0, 0, 0),
  DEFUN3 (bobp, 0, 0, 0),
  DEFUN3 (eobp, 0, 0, 0),
  DEFUN3 (char-after, 1, 1, 0),
  DEFUN3 (char-before, 1, 1, 0),
  DEFUN3 (following-char, 0, 0, 0),
  DEFUN3 (preceding-char, 0, 0, 0),
  DEFUN3 (current-column, 0, 0, 0),
  DEFUN3 (current-virtual-column, 0, 0, 0),
  DEFUN3 (goal-column, 0, 0, 0),
  DEFUN3 (set-goal-column, 1, 0, 0),
  DEFCMD3 (goto-line, 1, 0, 0, "NLine to goto: "),
  DEFUN3 (current-line-number, 0, 0, 0),
  DEFCMD3 (goto-virtual-line, 1, 0, 0, "NLine to goto: "),
  DEFUN3 (current-virtual-line-number, 0, 0, 0),
  DEFUN3 (count-column, 1, 2, 0),
  DEFUN3 (current-line-columns, 0, 0, 0),
  DEFUN3 (kinsoku-goto-column, 1, 1, 0),
  DEFUN3 (char-columns, 1, 0, 0),
  DEFUN3 (extended-alphabet-char-p, 1, 0, 0),

  /* undo.cc */
  MAKE_SYMBOL2F (kept-undo-information, SFmake_buffer_local),
  DEFUN3 (undo-boundary, 0, 0, 0),
  DEFUN3 (clear-undo-boundary, 0, 1, 0),
  DEFCMD3 (undo, 0, 0, 0, ""),
  DEFCMD3 (redo, 0, 0, 0, ""),
  DEFUN3 (buffer-can-undo-p, 1, 0, 0),
  DEFUN3 (buffer-can-redo-p, 1, 0, 0),
  DEFVAR2 (*move-forward-after-undo-deletion*),

  /* keymap.cc */
  DEFUN3 (keymapp, 1, 0, 0),
  DEFUN3 (make-keymap, 0, 0, 0),
  DEFUN3 (make-sparse-keymap, 0, 0, 0),
  DEFUN3 (use-keymap, 1, 1, 0),
  DEFUN3 (define-key, 3, 0, 0),
  DEFUN3 (lookup-keymap, 2, 2, 0),
  DEFUN3 (local-keymap, 0, 1, 0),
  DEFUN3 (*keymap-index-char, 1, 0, 0),
  DEFUN3 (*keymap-char-index, 1, 0, 0),
  DEFUN3 (command-keys, 3, 1, 0),
  MAKE_SYMBOL2Q (keymap),
  DEFVAR2 (*global-keymap*),
  MAKE_SYMBOL2 (selection-keymap),
  DEFUN3 (set-minor-mode-map, 1, 1, 0),
  DEFUN3 (unset-minor-mode-map, 1, 1, 0),
  DEFUN3 (minor-mode-map, 0, 1, 0),
  DEFUN3 (current-selection-keymap, 0, 0, 0),

  /* lprint.cc */
  DEFUN3 (ding, 0, 0, 0),
  DEFUN3 (message-box, 1, 2, FFneed_rest),
  DEFVAR2 (*visible-bell*),

  /* string.cc */
  DEFUN3 (split-string, 2, 2, 0),
  DEFUN3 (quote-string, 3, 0, 0),
  DEFUN3 (abbreviate-display-string, 2, 1, 0),
  DEFUN3 (decode-escape-sequence, 2, 0, 0),
  DEFUN3 (abbreviate-string-column, 2, 0, 0),

  /* search.cc */
  DEFUN3 (scan-buffer, 1, 0, FFneed_rest),
  DEFUN3 (replace-buffer, 2, 0, FFneed_rest),
  DEFUN3 (string-match, 2, 2, 0),
  DEFUN3 (string-matchp, 2, 2, 0),
  DEFUN3 (match-beginning, 1, 0, 0),
  DEFUN3 (match-end, 1, 0, 0),
  DEFUN3 (match-string, 1, 0, 0),
  DEFUN3 (compile-regexp, 1, 1, 0),
  DEFUN3 (regexp-quote, 1, 0, 0),
  DEFUN3 (looking-at, 1, 1, 0),
  DEFUN3 (looking-for, 1, 1, 0),
  DEFUN3 (looking-back, 1, 1, 0),
  DEFUN3 (skip-chars-forward, 1, 0, 0),
  DEFUN3 (skip-chars-backward, 1, 0, 0),
  DEFUN3 (skip-syntax-spec-forward, 1, 0, 0),
  DEFUN3 (skip-syntax-spec-backward, 1, 0, 0),
  DEFUN3 (replace-match, 1, 0, FFneed_rest),
  DEFUN3 (match-data, 0, 1, 0),
  DEFUN3 (store-match-data, 1, 0, 0),
  DEFUN3 (string-replace-match, 2, 0, 0),
  DEFUN3 (substitute-string, 3, 0, FFneed_rest),
  DEFUN3 (string-looking-at, 2, 0, FFneed_rest),
  DEFUN3 (compare-buffer-substrings, 6, 1, 0),
  DEFUN3 (compiled-regexp-source, 1, 0, 0),
  DEFUN3 (compiled-regexp-case-fold-p, 1, 0, 0),

  /* Window.cc */
  DEFCMD3 (split-window, 0, 2, 0, "p"),
  DEFCMD3 (delete-other-windows, 0, 0, 0, ""),
  DEFCMD3 (delete-window, 0, 0, 0, ""),
  DEFUN3 (selected-window, 0, 0, 0),
  DEFUN3 (minibuffer-window, 0, 0, 0),
  DEFUN3 (minibuffer-window-p, 1, 0, 0),
  DEFUN3 (window-buffer, 1, 0, 0),
  DEFUN3 (next-window, 1, 1, 0),
  DEFUN3 (previous-window, 1, 1, 0),
  DEFUN3 (get-buffer-window, 1, 1, 0),
  DEFUN3 (set-window, 1, 0, 0),
  DEFCMD3 (enlarge-window, 0, 2, 0, "p"),
  MAKE_SYMBOL2 (insert-default-directory),
  DEFUN3 (screen-width, 0, 0, 0),
  DEFUN3 (screen-height, 0, 0, 0),
  DEFUN3 (window-width, 0, 1, 0),
  DEFUN3 (window-height, 0, 1, 0),
  DEFUN3 (window-columns, 0, 1, 0),
  DEFUN3 (window-lines, 0, 1, 0),
  DEFUN3 (pos-not-visible-in-window-p, 1, 1, 0),
  DEFUN3 (get-window-line, 0, 1, 0),
  DEFUN3 (get-window-start-line, 0, 1, 0),
  DEFUN3 (get-window-handle, 0, 1, 0),
  DEFUN3 (get-window-flags, 0, 0, 0),
  DEFUN3 (set-window-flags, 1, 0, 0),
  DEFUN3 (get-local-window-flags, 1, 0, 0),
  DEFUN3 (set-local-window-flags, 3, 0, 0),
  DEFCMD3 (next-xyzzy-window, 0, 0, 0, ""),
  DEFCMD3 (previous-xyzzy-window, 0, 0, 0, ""),
  DEFUN3 (list-xyzzy-windows, 0, 0, 0),
  DEFUN3 (activate-xyzzy-window, 1, 0, 0),
  DEFUN3 (count-xyzzy-instance, 0, 0, 0),
  DEFUN3 (current-window-configuration, 0, 0, 0),
  DEFUN3 (set-window-configuration, 1, 0, 0),
  DEFUN3 (window-coordinate, 0, 1, 0),
  MAKE_SYMBOL2Q (window-configuration),
  MAKE_SYMBOL2 (mouse-wheel-handler),
  DEFCMD3 (begin-auto-scroll, 0, 0, 0, ""),
  MAKE_SYMBOL2 (*page-scroll-half-window*),
  MAKE_SYMBOL2 (*next-screen-context-lines*),

  /* syntax.cc */
  DEFUN3 (make-syntax-table, 0, 0, 0),
  DEFUN3Q (syntax-table, 0, 1, 0),
  DEFUN3 (use-syntax-table, 1, 2, 0),
  DEFUN3 (copy-syntax-table, 2, 0, 0),
  DEFUN3 (set-syntax-whitespace, 2, 0, 0),
  DEFUN3 (set-syntax-punctuation, 2, 0, 0),
  DEFUN3 (set-syntax-match, 3, 0, 0),
  DEFUN3 (set-syntax-math, 2, 0, 0),
  DEFUN3 (set-syntax-string, 2, 0, 0),
  DEFUN3 (set-syntax-start-comment, 2, 1, 0),
  DEFUN3 (set-syntax-end-comment, 2, 2, 0),
  DEFUN2 (set-syntax-end-c++-comment, set-syntax-end-cplusplus-comment, 2, 1, 0),
  DEFUN3 (set-syntax-escape, 2, 0, 0),
  DEFUN3 (set-syntax-quote, 2, 0, 0),
  DEFUN3 (set-syntax-symbol, 2, 0, 0),
  DEFUN3 (set-syntax-symbol-prefix, 2, 0, 0),
  DEFUN3 (set-syntax-word, 2, 0, 0),
  DEFUN3 (set-syntax-junk, 2, 0, 0),
  DEFUN3 (set-syntax-tag, 3, 0, 0),
  DEFUN3 (set-syntax-start-multi-comment, 2, 0, 0),
  DEFUN3 (set-syntax-end-multi-comment, 2, 0, 0),
  DEFUN2 (set-syntax-start-c++-comment, set-syntax-start-cplusplus-comment, 2, 1, 0),
  DEFUN3 (set-syntax-start-column-comment, 2, 1, 0),
  DEFUN3 (syntax-whitespace-p, 1, 1, 0),
  DEFUN3 (syntax-punctuation-p, 1, 1, 0),
  DEFUN3 (syntax-open-p, 1, 1, 0),
  DEFUN3 (syntax-close-p, 1, 1, 0),
  DEFUN3 (syntax-open-tag-p, 1, 1, 0),
  DEFUN3 (syntax-close-tag-p, 1, 1, 0),
  DEFUN3 (syntax-math-p, 1, 1, 0),
  DEFUN3 (syntax-string-p, 1, 1, 0),
  DEFUN3 (syntax-start-comment-p, 1, 1, 0),
  DEFUN3 (syntax-end-comment-p, 1, 1, 0),
  DEFUN2 (syntax-end-c++-comment-p, syntax-end-cplusplus-comment-p, 1, 1, 0),
  DEFUN3 (syntax-escape-p, 1, 1, 0),
  DEFUN3 (syntax-quote-p, 1, 1, 0),
  DEFUN3 (syntax-symbol-p, 1, 1, 0),
  DEFUN3 (syntax-symbol-prefix-p, 1, 1, 0),
  DEFUN3 (syntax-word-p, 1, 1, 0),
  DEFUN3 (syntax-junk-p, 1, 1, 0),
  DEFUN3 (syntax-start-multi-comment-1-p, 1, 1, 0),
  DEFUN3 (syntax-start-multi-comment-2-p, 1, 1, 0),
  DEFUN3 (syntax-end-multi-comment-1-p, 1, 1, 0),
  DEFUN3 (syntax-end-multi-comment-2-p, 1, 1, 0),
  DEFUN2 (syntax-c++-comment-p, syntax-cplusplus-comment-p, 1, 1, 0),
  DEFUN3 (syntax-start-column-comment-p, 1, 1, 0),
  DEFUN3 (set-syntax-option, 2, 0, 0),
  DEFUN3 (get-syntax-option, 1, 0, 0),
  DEFUN3 (set-syntax-comment-column, 2, 0, 0),
  DEFCMD3 (upcase-region, 2, 0, 0, "*r"),
  DEFCMD3 (downcase-region, 2, 0, 0, "*r"),
  DEFCMD3 (capitalize-region, 2, 0, 0, "*r"),
  DEFCMD3 (goto-matched-parenthesis, 0, 1, 0, "p"),
  DEFCMD3 (forward-sexp, 0, 2, 0, "p"),
  DEFCMD3 (forward-list, 0, 2, 0, "p"),
  DEFCMD3 (up-list, 0, 2, 0, "p"),
  DEFCMD3 (down-list, 0, 2, 0, "p"),
  DEFUN3 (skip-white-forward, 0, 0, 0),
  DEFUN3 (skip-white-backward, 0, 0, 0),
  DEFUN3 (skip-token, 0, 0, 0),
  DEFUN3 (indent-to, 1, 0, 0),
  DEFUN3 (forward-identifier, 3, 0, 0),
  DEFUN3 (backward-identifier, 3, 0, 0),
  MAKE_SYMBOL2 (indent-tabs-mode),
  MAKE_SYMBOL2 (c-indent-level),
  MAKE_SYMBOL2 (c-brace-offset),
  MAKE_SYMBOL2 (c-continued-statement-offset),
  MAKE_SYMBOL2 (c-argdecl-indent),
  MAKE_SYMBOL2 (c-brace-imaginary-offset),
  MAKE_SYMBOL2 (c-label-offset),
  MAKE_SYMBOL2 (c-comment-indent),

  MAKE_SYMBOL (c++-indent-level, Vcplusplus-indent-level),
  MAKE_SYMBOL (c++-brace-offset, Vcplusplus-brace-offset),
  MAKE_SYMBOL (c++-continued-statement-offset, Vcplusplus-continued-statement-offset),
  MAKE_SYMBOL (c++-argdecl-indent, Vcplusplus-argdecl-indent),
  MAKE_SYMBOL (c++-brace-imaginary-offset, Vcplusplus-brace-imaginary-offset),
  MAKE_SYMBOL (c++-label-offset, Vcplusplus-label-offset),
  MAKE_SYMBOL (c++-comment-indent, Vcplusplus-comment-indent),

  MAKE_SYMBOL2 (java-indent-level),
  MAKE_SYMBOL2 (java-brace-offset),
  MAKE_SYMBOL2 (java-continued-statement-offset),
  MAKE_SYMBOL2 (java-argdecl-indent),
  MAKE_SYMBOL2 (java-brace-imaginary-offset),
  MAKE_SYMBOL2 (java-label-offset),
  MAKE_SYMBOL2 (java-comment-indent),

  MAKE_SYMBOL2 (csharp-indent-level),
  MAKE_SYMBOL2 (csharp-brace-offset),
  MAKE_SYMBOL2 (csharp-continued-statement-offset),
  MAKE_SYMBOL2 (csharp-argdecl-indent),
  MAKE_SYMBOL2 (csharp-brace-imaginary-offset),
  MAKE_SYMBOL2 (csharp-label-offset),
  MAKE_SYMBOL2 (csharp-comment-indent),

  DEFUN3 (calc-c-indent, 0, 0, 0),
  DEFUN3 (tab-columns, 0, 1, 0),
  DEFUN3 (set-tab-columns, 1, 1, 0),
  DEFUN3 (parse-point-syntax, 0, 1, 0),
  MAKE_SYMBOL2F (parentheses-hash-table, SFmake_buffer_local),
  DEFUN3 (refresh-highlight-info, 0, 1, 0),

  /* minibuf.cc */
  DEFCMD3 (exit-recursive-edit, 0, 1, 0, ""),
  DEFCMD3 (quit-recursive-edit, 0, 1, 0, ""),
  DEFVAR2 (*enable-recursive-minibuffers*),
  DEFVAR2 (*enter-minibuffer-hook*),
  DEFVAR2 (*exit-minibuffer-hook*),
  MAKE_SYMBOL2 (minibuffer-local-must-match-map),
  MAKE_SYMBOL2 (minibuffer-local-completion-map),
  MAKE_SYMBOL2 (minibuffer-local-map),
  MAKE_SYMBOL2 (minibuffer-local-command-line-map),
  DEFUN3 (*do-completion, 2, 2, 0),
  DEFUN3 (minibuffer-completion-type, 0, 1, 0),
  DEFUN3 (minibuffer-completion-list, 0, 1, 0),
  DEFUN3 (minibuffer-buffer, 0, 1, 0),
  DEFUN3 (minibuffer-dialog-title, 0, 1, 0),
  DEFUN3 (minibuffer-default, 0, 1, 0),
  MAKE_SYMBOL2 (show-dots),
  MAKE_SYMBOL2 (ignored-extensions),
  DEFUN3 (read-string, 1, 0, FFneed_rest),
  DEFUN3 (read-function-name, 1, 0, FFneed_rest),
  DEFUN3 (read-command-name, 1, 0, FFneed_rest),
  DEFUN3 (read-symbol-name, 1, 0, FFneed_rest),
  DEFUN3 (read-variable-name, 1, 0, FFneed_rest),
  DEFUN3 (read-file-name, 1, 0, FFneed_rest),
  DEFUN3 (read-file-name-list, 1, 0, FFneed_rest),
  DEFUN3 (read-exist-file-name, 1, 0, FFneed_rest),
  DEFUN3 (read-directory-name, 1, 0, FFneed_rest),
  DEFUN3 (read-buffer-name, 1, 0, FFneed_rest),
  DEFUN3 (read-exist-buffer-name, 1, 0, FFneed_rest),
  DEFUN3 (read-integer, 1, 0, FFneed_rest),
  DEFUN3 (read-sexp, 1, 0, FFneed_rest),
  DEFUN3 (read-char-encoding, 1, 0, FFneed_rest),
  DEFUN3 (read-exact-char-encoding, 1, 0, FFneed_rest),
  DEFUN3 (completing-read, 2, 0, FFneed_rest),
  MAKE_SYMBOL2 (universal-argument),
  MAKE_SYMBOL2 (*buffer-package*),
  DEFVAR2 (*minibuffer-save-ime-status*),

  /* fileio.cc */
  DEFUN3 (clear-visited-file-modtime, 0, 1, 0),
  DEFUN3 (update-visited-file-modtime, 0, 1, 0),
  DEFUN3 (verify-visited-file-modtime, 0, 1, 0),
  MAKE_SYMBOL2 (version-control),
  MAKE_SYMBOL2 (kept-old-versions),
  MAKE_SYMBOL2 (kept-new-versions),
  MAKE_SYMBOL2 (make-backup-files),
  MAKE_SYMBOL2 (make-backup-file-always),
  MAKE_SYMBOL2 (pack-backup-file-name),
  MAKE_SYMBOL2 (backup-by-copying),
  MAKE_SYMBOL2Q (never),
  DEFCMD3 (save-buffer, 0, 2, 0, ""),
  DEFUN3 (delete-auto-save-file, 1, 0, 0),
  DEFUN3 (do-auto-save, 0, 1, 0),
  DEFCMD3 (write-region, 3, 3, 0, "r\nFƒtƒ@ƒCƒ‹–¼: \np"),
  MAKE_SYMBOL2 (to-ascii-fileio),
  MAKE_SYMBOL2 (to-kanji-fileio),
  MAKE_SYMBOL2 (to-kana-fileio),
  DEFVAR2 (*auto-save-interval*),
  DEFVAR2 (*auto-save-interval-timer*),
  MAKE_SYMBOL2 (*make-backup-filename-hook*),
  MAKE_SYMBOL2 (*auto-save-to-backup-directory*),
  MAKE_SYMBOL2 (*before-save-buffer-hook*),
  MAKE_SYMBOL2 (*after-save-buffer-hook*),
  MAKE_SYMBOL2 (*save-buffer-no-filenames-hook*),

  /* package.cc */
  DEFUN3 (lookup-symbol, 2, 1, 0),

  /* toplev.cc */
  DEFVAR2 (*drag-and-drop-hook*),
  DEFVAR2 (*drag-and-drop-auto-activate*),
  DEFUN3 (cancel-mouse-event, 0, 0, 0),
  DEFVAR2 (*eat-mouse-activate*),
  DEFUN3 (begin-wait-cursor, 0, 0, 0),
  DEFUN3 (end-wait-cursor, 0, 0, 0),
  DEFUN3 (main-loop, 0, 0, 0),
  DEFUN3 (set-cursor, 1, 0, 0),
  DEFVAR2 (*cursor-shape*),
  DEFVAR2 (*ime-mode-hook*),
  DEFVAR2 (*this-command*),
  DEFVAR2 (*last-command*),
  MAKE_SYMBOL2 (*pre-command-hook*),
  MAKE_SYMBOL2 (*post-command-hook*),
  DEFVAR2 (*blink-caret*),
  DEFUN3 (call-menu, 1, 0, 0),
  DEFUN3 (set-quit-char, 1, 0, 0),
  DEFUN3 (quit-char, 0, 0, 0),
  DEFVAR2 (*support-mouse-wheel*),

  /* mouse.cc */
  DEFVAR2 (*last-mouse-window*),
  DEFVAR2 (*last-mouse-line*),
  DEFVAR2 (*last-mouse-column*),
  DEFVAR2 (*last-mouse-click-count*),
  DEFVAR2 (*hide-mouse-cursor*),

  /* disp.cc */
  DEFUN3 (refresh-screen, 0, 1, 0),
  DEFVAR2 (*inverse-mode-line*),
  MAKE_SYMBOL2F (hide-restricted-region, SFmake_buffer_local),
  DEFVAR2 (*minor-mode-alist*),
  MAKE_SYMBOL2 (keyword-hash-table),
  MAKE_SYMBOL2 (highlight-keyword),
  MAKE_SYMBOL2F (html-highlight-mode, SFmake_buffer_local),
  DEFVAR2 (*normal-caret-shape*),
  DEFVAR2 (*overwrite-caret-shape*),
  MAKE_SYMBOL2 (*inverse-mark-line*),
  DEFVAR2 (*inhibit-reverse-keywords*),
  MAKE_SYMBOL2 (*scroll-margin*),
  MAKE_SYMBOL2 (*jump-scroll-threshold*),
  DEFVAR2 (*show-cursor-line-always*),
  DEFVAR2 (*old-relocation-method*),
  MAKE_SYMBOL2 (regexp-keyword-list),
  MAKE_SYMBOL2 (display-newline-char),
  MAKE_SYMBOL2 (display-first-tab-char),
  MAKE_SYMBOL2 (display-rest-tab-char),
  MAKE_SYMBOL2 (inverse-cursor-line),

  /* char.cc */
  DEFUN3 (set-meta-bit, 2, 0, 0),
  DEFUN3 (dbc-first-byte-p, 1, 0, 0),
  DEFUN3 (dbc-second-byte-p, 1, 0, 0),
  DEFUN3 (kanji-char-p, 1, 0, 0),
  DEFUN3 (kana-char-p, 1, 0, 0),
  DEFUN3 (char-unicode, 1, 0, 0),
  DEFUN3 (unicode-char, 1, 0, 0),
  DEFUN3 (iso-char-code, 1, 1, 0),
  DEFUN3 (iso-code-char, 2, 1, 0),
  DEFUN3 (iso-char-charset, 1, 1, 0),
  DEFUN3 (word-char-p, 1, 0, 0),

  /* pathname.cc */
  DEFUN3 (make-temp-file-name, 0, 4, 0),
  DEFUN3 (file-newer-than-file-p, 2, 0, 0),
  DEFUN3 (file-system-supports-long-file-name-p, 1, 0, 0),
  DEFUN3 (append-trail-slash, 1, 0, 0),
  DEFUN3 (remove-trail-slash, 1, 0, 0),
  DEFUN3 (cwd, 0, 0, 0),
  DEFUN3 (map-slash-to-backslash, 1, 0, 0),
  DEFUN3 (map-backslash-to-slash, 1, 0, 0),
  DEFUN3 (network-connect-dialog, 0, 0, 0),
  DEFUN3 (network-disconnect-dialog, 0, 0, 0),
  DEFUN3 (get-file-attributes, 1, 0, 0),
  DEFUN3 (set-file-attributes, 2, 0, 0),
  DEFUN3 (modify-file-attributes, 2, 1, 0),
  DEFUN3 (get-disk-usage, 1, 1, 0),
  DEFUN3 (format-drive, 0, 2, 0),
  DEFUN3 (compare-file, 2, 0, 0),
  DEFUN3 (file-property, 1, 0, 0),
  DEFUN3 (eject-media, 1, 0, 0),
  DEFUN3 (list-servers, 0, 1, 0),
  DEFUN3 (list-server-resources, 1, 1, 0),
  DEFVAR2 (*rename-alternate-file-name*),
  DEFUN3 (get-file-info, 1, 0, 0),

  /* process.cc */
  DEFUN3 (call-process, 1, 0, FFneed_rest),
  DEFUN3 (make-process, 1, 0, FFneed_rest),
  DEFUN3 (open-network-stream, 3, 0, FFneed_rest),
  DEFUN3 (buffer-process, 0, 1, 0),
  DEFUN3 (process-buffer, 1, 0, 0),
  DEFUN3 (process-command, 1, 0, 0),
  DEFUN3 (process-status, 1, 0, 0),
  DEFUN3 (process-exit-code, 1, 0, 0),
  DEFUN3 (signal-process, 1, 0, 0),
  DEFUN3 (kill-process, 1, 0, 0),
  DEFUN3 (process-incode, 1, 0, 0),
  DEFUN3 (process-outcode, 1, 0, 0),
  DEFUN3 (set-process-incode, 2, 0, 0),
  DEFUN3 (set-process-outcode, 2, 0, 0),
  DEFUN3 (process-eol-code, 1, 0, 0),
  DEFUN3 (set-process-eol-code, 2, 0, 0),
  DEFUN3 (process-send-string, 2, 0, 0),
  DEFUN3 (set-process-filter, 2, 0, 0),
  DEFUN3 (process-filter, 1, 0, 0),
  DEFUN3 (set-process-sentinel, 2, 0, 0),
  DEFUN3 (process-sentinel, 1, 0, 0),
  DEFUN3 (process-marker, 1, 0, 0),
  DEFUN3 (shell-execute, 1, 2, 0),
  DEFVAR2 (*default-process-encoding*),
  DEFVAR2 (*use-shell-execute-ex*),
  DEFVAR2 (*shell-execute-disregards-shift-key*),

  /* menu.cc */
  DEFUN3 (create-menu, 0, 1, 0),
  DEFUN3 (create-popup-menu, 0, 1, 0),
  DEFUN3 (add-popup-menu, 3, 0, 0),
  DEFUN3 (add-menu-item, 3, 2, 0),
  DEFUN3 (add-menu-separator, 1, 1, 0),
  DEFUN3 (get-menu-position, 2, 0, 0),
  DEFUN3 (get-menu, 2, 1, 0),
  DEFUN3 (insert-popup-menu, 4, 0, 0),
  DEFUN3 (insert-menu-item, 4, 2, 0),
  DEFUN3 (insert-menu-separator, 2, 1, 0),
  DEFUN3 (set-menu, 1, 0, 0),
  DEFUN3 (delete-menu, 2, 1, 0),
  DEFUN3 (track-popup-menu, 1, 1, 0),
  DEFUN3 (use-local-menu, 1, 0, 0),
  DEFUN3 (copy-menu-items, 2, 0, 0),
  DEFUN3 (current-menu, 0, 1, 0),

  /* kbd.cc */
  DEFUN3 (sit-for, 1, 1, 0),
  DEFUN3 (sleep-for, 1, 0, 0),
  DEFUN3 (do-events, 0, 0, 0),
  DEFUN3 (reset-prefix-args, 2, 0, 0),
  DEFUN3 (set-next-prefix-args, 2, 1, 0),
  DEFUN3 (start-save-kbd-macro, 0, 0, 0),
  DEFUN3 (stop-save-kbd-macro, 0, 0, 0),
  DEFUN3 (kbd-macro-saving-p, 0, 0, 0),
  DEFCMD3 (toggle-ime, 0, 1, 0, ""),
  DEFUN3 (get-recent-keys, 0, 0, 0),
  DEFUN3 (get-ime-mode, 0, 0, 0),
  DEFUN3 (get-ime-composition-string, 0, 0, 0),
  DEFUN3 (pop-ime-composition-string, 0, 0, 0),
  DEFUN3 (set-ime-read-string, 0, 1, 0),
  DEFUN3 (*ime-register-word-dialog, 0, 2, 0),
  DEFUN3 (enable-global-ime, 1, 0, 0),
  DEFVAR2 (*ime-control*),
  DEFVAR2 (*extended-key-translate-table*),
  DEFVAR2 (*kbd-translate-table*),
  DEFVAR2 (*activate-hook*),
  DEFVAR2 (*deactivate-hook*),
  DEFVAR2 (*save-buffer-ime-mode*),
  DEFVAR2 (*ime-reconvert-helper*),
  DEFVAR2 (*ime-documentfeed-helper*),
  DEFVAR2 (*keyboard-layout-list*),
  DEFVAR (*ime-does-not-process-C-\\*, Vime_does_not_process_control_backslach),
  DEFVAR2 (*no-input-language-change-notification*),
  DEFUN3 (list-kbd-layout, 0, 0, 0),
  DEFUN3 (select-kbd-layout, 1, 0, 0),
  DEFUN3 (current-kbd-layout, 0, 0, 0),
  DEFVAR2 (*unicode-ime*),

  /* kanji.cc */
  DEFCMD3 (map-to-half-width-region, 2, 0, FFneed_rest, "*r"),
  DEFCMD3 (map-to-full-width-region, 2, 0, FFneed_rest, "*r"),
  DEFUN3 (map-to-half-width-string, 1, 0, FFneed_rest),
  DEFUN3 (map-to-full-width-string, 1, 0, FFneed_rest),
  DEFUN3 (convert-encoding-to-internal, 2, 1, 0),
  DEFUN3 (convert-encoding-from-internal, 2, 1, 0),
  DEFUN3 (detect-char-encoding, 1, 0, 0),
  DEFVAR2 (*accept-mule-ucs-funny-utf8*),

  /* ldialog.cc */
  DEFUN3 (dialog-box, 3, 0, 0),
  MAKE_SYMBOL2Q (dialog),
#undef IDOK
  MAKE_SYMBOL (IDOK, Qidok),
#undef IDCANCEL
  MAKE_SYMBOL (IDCANCEL, Qidcancel),
  DEFUN3 (property-sheet, 1, 2, 0),
  MAKE_SYMBOL2Q (font-page),
  MAKE_SYMBOL2Q (color-page),
  DEFVAR2 (*color-page-enable-dir-p*),
  DEFVAR2 (*color-page-enable-subdir-p*),
  MAKE_SYMBOL2Q (property-page),
  DEFVAR2 (*std-control-up-char*),
  DEFVAR2 (*std-control-down-char*),
  DEFVAR2 (*std-control-default-char*),
  DEFVAR2 (*std-control-prior-char*),
  DEFVAR2 (*std-control-next-char*),

  /* dialogs.cc */
  DEFUN3 (buffer-selector, 0, 0, 0),
  DEFVAR2 (*minibuffer-file-name-history*),
  DEFUN3 (file-name-dialog, 0, 0, FFneed_rest),
  DEFUN3 (directory-name-dialog, 0, 0, FFneed_rest),
  DEFUN3 (drive-dialog, 0, 1, 0),

  /* winhelp.cc */
  DEFUN3 (*run-winhelp, 1, 1, 0),
  DEFUN3 (*kill-winhelp, 1, 0, 0),
  DEFUN3 (find-winhelp-path, 2, 0, 0),
  DEFUN3 (html-help, 2, 0, 0),

  /* printdlg.cc */
  DEFUN3 (print-dialog, 0, 1, 0),
  DEFUN3 (print-buffer, 0, 1, 0),

  /* dde.cc */
  DEFUN3 (dde-initiate, 2, 0, 0),
  DEFUN3 (dde-terminate, 1, 0, 0),
  DEFUN3 (dde-execute, 2, 0, 0),
  DEFUN3 (dde-poke, 3, 0, 0),
  DEFUN3 (dde-request, 2, 1, 0),
  DEFVAR2 (*dde-timeout*),

  /* filer.cc */
  DEFUN3 (filer, 0, 5, 0),
  DEFUN3 (filer-forward-line, 0, 2, 0),
  DEFUN3 (filer-forward-page, 0, 2, 0),
  DEFUN3 (filer-goto-bof, 0, 1, 0),
  DEFUN3 (filer-goto-eof, 0, 1, 0),
  DEFUN3 (filer-scroll-left, 0, 1, 0),
  DEFUN3 (filer-scroll-right, 0, 1, 0),
  DEFUN3 (filer-reload, 0, 2, 0),
  DEFUN3 (filer-demand-reload, 0, 0, 0),
  DEFUN3 (filer-subscribe-to-reload, 1, 1, 0),
  DEFUN3 (filer-set-directory, 1, 1, 0),
  DEFUN3 (filer-get-directory, 0, 1, 0),
  DEFUN3 (filer-get-drive, 0, 1, 0),
  DEFUN3 (filer-mark, 0, 2, 0),
  DEFUN3 (filer-mark-all, 0, 2, 0),
  DEFUN3 (filer-mark-match-files, 1, 1, 0),
  DEFUN3 (filer-toggle-mark, 0, 2, 0),
  DEFUN3 (filer-toggle-all-marks, 0, 2, 0),
  DEFUN3 (filer-close, 1, 0, 0),
  DEFUN3 (filer-get-mark-files, 0, 2, 0),
  DEFUN3 (filer-get-current-file, 0, 1, 0),
  DEFUN3 (filer-current-file-dot-dot-p, 0, 1, 0),
  DEFUN3 (filer-current-file-directory-p, 0, 1, 0),
  DEFUN3 (filer-get-text, 0, 0, 0),
  DEFUN3 (filer-set-text, 1, 0, 0),
  DEFUN3 (filer-isearch, 0, 3, 0),
  DEFUN3 (filer-goto-file, 1, 3, 0),
  DEFUN3 (filer-set-file-mask, 1, 1, 0),
  DEFUN3 (filer-clear-all-marks, 0, 1, 0),
  DEFUN3 (filer-count-marks, 0, 2, 0),
  DEFUN3 (filer-sort, 1, 1, 0),
  DEFUN3 (filer-get-sort-order, 0, 1, 0),
  DEFUN3 (filer-dual-window-p, 0, 0, 0),
  DEFUN3 (filer-left-window, 0, 0, 0),
  DEFUN3 (filer-right-window, 0, 0, 0),
  DEFUN3 (filer-left-window-p, 0, 0, 0),
  DEFUN3 (filer-calc-directory-size, 0, 1, 0),
  DEFUN3 (filer-calc-directory-byte-size, 0, 1, 0),
  MAKE_SYMBOL2 (filer-keymap),
  DEFVAR2 (*filer-last-command-char*),
  DEFVAR2 (*filer-dual-window*),
  DEFVAR2 (*filer-primary-directory*),
  DEFVAR2 (*filer-secondary-directory*),
  DEFVAR2 (*filer-left-window-p*),
  DEFVAR2 (*filer-primary-file-mask*),
  DEFVAR2 (*filer-secondary-file-mask*),
  DEFVAR2 (*filer-last-file-mask*),
  DEFVAR2 (*filer-guide-text*),
  DEFVAR2 (*filer-retrieve-icon*),
  MAKE_SYMBOL2 (*filer-drag-and-drop-helper),
  MAKE_SYMBOL2 (*filer-chdir-hook*),
  DEFVAR2 (*filer-chdir-primary-p*),
  DEFUN3 (filer-viewer, 0, 0, 0),
  DEFUN3 (filer-modal-p, 0, 0, 0),
  DEFUN3 (filer-modify-column-width, 2, 1, 0),
  DEFVAR2 (*filer-format-comma*),
  DEFUN3 (filer-context-menu, 0, 0, 0),
  DEFVAR2 (*modal-filer-save-position*),
  DEFVAR2 (*modal-filer-save-size*),
  DEFVAR2 (*filer-echo-filename*),
  DEFVAR2 (*filer-eat-esc*),
  DEFUN3 (filer-swap-windows, 0, 0, 0),
  DEFVAR2 (*filer-click-toggle-marks-always*),
  DEFUN3 (filer-read-char, 0, 0, 0),
  DEFVAR2 (*filer-mark-file-size-unit*),

  /* edict.cc */
  DEFUN3 (lookup-dictionary, 4, 0, 0),

  /* archiver.cc */
  DEFUN3 (extract-archive, 2, 0, FFneed_rest),
  DEFUN3 (create-archive, 3, 0, 0),
  DEFUN3 (delete-file-in-archive, 1, 0, FFneed_rest),
  DEFUN3 (list-archive, 1, 1, 0),
  DEFUN3 (convert-to-SFX, 1, 1, 0),
  DEFUN3 (archiver-dll-version, 1, 0, 0),
  DEFUN3 (archiver-dll-config-dialog, 1, 1, 0),

  /* com.cc */
  DEFUN3 (get-special-folder-location, 1, 0, 0),
  DEFUN3 (*create-shortcut, 2, 0, FFneed_rest),
  DEFUN3 (resolve-shortcut, 1, 0, 0),
  DEFUN3 (ole-drop-files, 4, 0, 0),

  /* fnkey.cc */
  DEFUN3 (set-function-bar-label, 2, 0, 0),
  DEFUN3 (set-number-of-function-bar-labels, 1, 0, 0),
  DEFUN3 (number-of-function-bar-labels, 0, 0, 0),

  /* DnD.cc */
  DEFUN3 (drag-region, 2, 0, 0),
  DEFVAR (*enable-D&D-edit*, Venable_DnD_edit),

  /* hash.cc */
  DEFUN3 (gethash-region, 3, 1, 0),

  /* popup.cc */
  DEFUN3 (continue-popup, 0, 0, 0),
  DEFUN3 (popup-string, 2, 1, 0),
  DEFUN3 (popup-list, 2, 1, 0),

  /* oledata.cc */
  DEFUN3 (ole-create-object, 1, 0, 0),
  DEFUN3 (ole-get-object, 1, 0, 0),
  DEFUN3 (ole-putprop, 3, 0, FFneed_rest),
  DEFUN3 (ole-method, 2, 0, FFneed_rest),
  DEFUN3 (ole-getprop, 2, 0, FFneed_rest),
  DEFUN3 (ole-create-event-sink, 1, 2, 0),
  DEFUN3 (set-ole-event-handler, 3, 0, 0),
  DEFUN3 (ole-enumerator-next, 1, 0, 0),
  DEFUN3 (ole-enumerator-reset, 1, 0, 0),
  DEFUN3 (ole-enumerator-skip, 1, 1, 0),

  /* lread.cc */
  DEFUN3 (read-as-string, 1, 2, 0),
  DEFUN3 (read-line-into, 1, 3, 0),
  DEFUN3 (read-into, 1, 4, 0),

  /* ces.cc */
  DEFUN3 (make-auto-detect-encoding, 2, 0, 0),
  DEFUN3 (make-sjis-encoding, 2, 0, 0),
  DEFUN3 (make-big5-encoding, 2, 0, 0),
  DEFUN3 (make-binary-encoding, 2, 0, 0),
  DEFUN3 (make-iso2022-encoding, 2, 0, FFneed_rest),
  DEFUN3 (make-iso8859-encoding, 3, 0, 0),
  DEFUN3 (make-windows-codepage-encoding, 3, 0, 0),
  DEFUN3 (make-utf5-encoding, 2, 0, FFneed_rest),
  DEFUN3 (make-utf7-encoding, 2, 0, FFneed_rest),
  DEFUN3 (make-utf8-encoding, 2, 0, FFneed_rest),
  DEFUN3 (make-utf16-encoding, 2, 0, FFneed_rest),
  DEFUN3 (char-encoding-name, 1, 0, 0),
  DEFUN3 (char-encoding-display-name, 1, 0, 0),
  DEFVAR2 (*encoding-sjis*),
  MAKE_SYMBOL2 (*encoding-euc-jp*),
  MAKE_SYMBOL2 (*encoding-jis*),
  MAKE_SYMBOL2 (*encoding-default-utf16le-bom*),
  MAKE_SYMBOL2 (*encoding-default-utf16be-bom*),
  MAKE_SYMBOL2 (*encoding-default-utf8*),
  MAKE_SYMBOL2 (*encoding-default-utf8n*),
  MAKE_SYMBOL2 (*encoding-windows-latin1*),
  MAKE_SYMBOL2 (*encoding-big5*),
  DEFVAR2 (*encoding-auto*),
  MAKE_SYMBOL2 (*encoding-default-euc*),
  MAKE_SYMBOL2 (*encoding-default-iso-2022*),
  MAKE_SYMBOL2 (*default-utf16-byte-order*),
  DEFVAR2 (*char-encoding-list*),
  DEFUN3 (parse-char-encoding-region, 3, 0, 0),
  DEFUN3 (parse-char-encoding-string, 2, 0, FFneed_rest),
  DEFVAR2 (*unicode-to-half-width*),
  DEFVAR2 (*vender-depend-code-mapping*),

  /* listen.cc */
  DEFUN3 (start-xyzzy-server, 0, 0, 0),
  DEFUN3 (stop-xyzzy-server, 0, 0, 0),

  /* statarea.cc */
  MAKE_SYMBOL2 (*status-bar-format*),

  /* usertool.cc */
  DEFUN3 (create-tool-bar, 3, 0, 0),
  DEFUN3 (show-tool-bar, 1, 4, 0),
  DEFUN3 (hide-tool-bar, 1, 0, 0),
  DEFUN3 (delete-tool-bar, 1, 0, 0),
  DEFUN3 (tool-bar-exist-p, 1, 0, 0),
  DEFUN3 (tool-bar-info, 1, 0, 0),
  DEFUN3 (list-tool-bars, 0, 0, 0),
  DEFCMD3 (focus-tool-bar, 0, 0, 0, ""),
  DEFUN3 (refresh-tool-bars, 0, 0, 0),
  DEFVAR2 (*tab-bar-horizontal-text*),

  /* usertab.cc */
  DEFUN3 (create-tab-bar, 2, 0, 0),
  DEFUN3 (tab-bar-add-item, 3, 2, FFneed_rest),
  DEFUN3 (tab-bar-delete-item, 2, 0, 0),
  DEFUN3 (tab-bar-select-item, 2, 0, 0),
  DEFUN3 (tab-bar-current-item, 1, 0, 0),
  DEFUN3 (tab-bar-find-item, 2, 0, 0),
  DEFUN3 (tab-bar-list-items, 1, 0, 0),
  DEFUN3 (tab-bar-modify-item, 2, 3, 0),
  DEFVAR2 (*tab-bar-never-focus*),

  /* utimer.cc */
  DEFUN3 (start-timer, 2, 1, 0),
  DEFUN3 (stop-timer, 1, 0, 0),

  DEFVAR2 (*last-command-char*),
  DEFVAR2 (*enable-meta-key*),
  DEFVAR2 (*beep-on-error*),
  DEFVAR2 (*beep-on-warn*),
  DEFVAR2 (*beep-on-never*),
  DEFVAR2 (*quit-flag*),
  DEFVAR2 (*inhibit-quit*),
  MAKE_SYMBOL2 (default-input-function),
  DEFVAR2 (*prefix-value*),
  DEFVAR2 (*prefix-args*),
  DEFVAR2 (*save-window-size*),
  DEFVAR2 (*save-window-position*),
  DEFVAR2 (*restore-window-size*),
  DEFVAR2 (*restore-window-position*),
  DEFVAR2 (*buffer-list-sort-ignore-case*),
};

static void
print_name (const symbols *p)
{
  printf ("SS + %d, ", p->offset);
}

static void
print_cname (const char *p)
{
  for (; *p; p++)
    {
      if (*p == '-')
        putchar ('_');
      else if (*p != '*')
        putchar (*p);
    }
}

static void
print_arg (int nargs, int f)
{
  if (f & FFspecial_form)
    printf ("lisp, lex_env &");
  else
    {
      if (f & FFneed_rest)
        nargs++;
      for (int i = 1; i < nargs; i++)
        printf ("lisp, ");
      if (nargs)
        printf ("lisp");
    }
}

static void
print_defuns (symbols *p, int n, const char *pkg)
{
  if (p == kwd || p == unint)
    return;

  printf ("lfns %s_fns[] =\n", pkg);
  printf ("{\n");
  for (int i = 0; i < n; i++, p++)
    if (p->fn)
      {
        if (p->req >= 16 || p->opt >= 16)
          {
            fprintf (stderr, "Too many args\n");
            exit (2);
          }
        if (p->len >= 256)
          {
            fprintf (stderr, "Name too long\n");
            exit (2);
          }
        printf ("  {");
        print_name (p);
        printf ("(lisp (__stdcall *)())(lisp (__stdcall *)(");
        print_arg (p->req + p->opt, p->flags);
        printf ("))");
        print_cname (p->fn);
        printf (", &");
        print_cname (p->sym);
        printf (", 0, %d, %d, %d, %d, %d},\n",
                p->len, p->req, p->opt, p->flags, p->iindex);
      }
  printf ("  {0},\n");
  printf ("};\n\n");
}

static void
print_defvars (symbols *p, int n, const char *pkg)
{
  printf ("lvars %s_vars[] =\n", pkg);
  printf ("{\n");
  for (int i = 0; i < n; i++, p++)
    if (!p->fn)
      {
        if (p->len >= 256)
          {
            fprintf (stderr, "Name too long\n");
            exit (2);
          }
        printf ("  {");
        print_name (p);
        printf ("&");
        print_cname (p->sym);
        printf (", %d, %d},\n", p->len, p->flags);
      }
  printf ("  {0},\n");
  printf ("};\n\n");
}

static void
print_proto (symbols *p, int n, const char *)
{
  for (int i = 0; i < n; i++, p++)
    if (p->fn)
      {
        printf ("lisp ");
        print_cname (p->fn);
        printf (" (");
        print_arg (p->req + p->opt, p->flags);
        printf (");\n");
      }
}

static void
print_vars (symbols *p, int n, const char *)
{
  for (int i = 0; i < n; i++, p++)
    {
      printf ("EXTERN lisp ");
      print_cname (p->sym);
      printf (";\n");
    }
}

static int __cdecl
compare (const void *a, const void *b)
{
  return strcmp (*(const char **)a, *(const char **)b);
}

static void
check_dup (symbols *p, int n, const char *pkg)
{
  if (p == unint)
    return;

  const char **buf = (const char **)alloca (sizeof (char *) * n);
  for (int i = 0; i < n; i++, p++)
    buf[i] = p->name;

  qsort (buf, n, sizeof *buf, compare);
  int f = 0;
  for (i = 1; i < n; i++)
    if (!strcmp (buf[i - 1], buf[i]))
      {
        fprintf (stderr, "package %s: duplicate definition: %s \n", pkg, buf[i]);
        f = 1;
      }
  if (f)
    exit (2);
}

static void
make_symbol_name (symbols *p, int n, const char *)
{
  if (p == unint)
    return;

  for (int i = 0; i < n; i++, p++)
    if (!p->name)
      {
        char *s = strdup (p->sym + 1);
        for (char *s2 = s; *s2; *s2++)
          if (*s2 == '_')
            *s2 = '-';
        p->name = s;
      }
}

static void
do_all (void (*fn)(symbols *, int, const char *))
{
  fn (lsp, numberof (lsp), "lsp");
  fn (sys, numberof (sys), "sys");
  fn (kwd, numberof (kwd), "kwd");
  fn (ed, numberof (ed), "ed");
  fn (unint, numberof (unint), "unint");
}

static int soffset;

static void
compose (symbols *p, int n, const char *)
{
  if (p == unint)
    for (int i = 0; i < n; i++, p++)
      {
        p->offset = soffset;
        p->len = 0;
      }
  else
    for (int i = 0; i < n; i++, p++)
      {
        p->offset = soffset;
        p->len = strlen (p->name);
        soffset += p->len;
      }
}

static void
printc (int c)
{
  if (soffset == 72)
    {
      printf ("\"\n\"");
      soffset = 0;
    }
  soffset++;
  if (c == '\\' || c == '"')
    putchar ('\\');
  putchar (c);
}

static void
print_string (symbols *p, int n, const char *)
{
  if (p == unint)
    return;
  for (int i = 0; i < n; i++, p++)
    {
      const char *s = p->name;
      while (*s)
        printc (*s++);
    }
}

static void
print_string ()
{
  soffset = 0;
  printf ("#define EXTERN /* empty */\n");
  printf ("#include \"ed.h\"\n");
  printf ("#include \"symtable.h\"\n\n");
  printf ("static const char SS[] = \n\"");
  do_all (print_string);
  printf ("\";\n\n");
  soffset = 0;
}

static void
putq (const char *p)
{
  putchar ('"');
  while (*p)
    {
      if ((*p & 0xff) < ' ')
        printf ("\\%03o", *p++);
      else
        {
          if (*p == '\\' || *p == '"')
            putchar ('\\');
          putchar (*p);
          if (_ismbblead (*p++ & 0xff))
            putchar (*p++);
        }
    }
  putchar ('"');
}

static void
process_interactive ()
{
  const char **intr = (const char **)alloca (sizeof (char *) * numberof (ed));
  for (int i = 0, j = 0; i < numberof (ed); i++)
    if (ed[i].interactive)
      intr[j++] = ed[i].interactive;
  if (!j)
    {
      fprintf (stderr, "No commands\n");
      exit (2);
    }
  int n = j;
  qsort (intr, n, sizeof *intr, compare);

  for (i = 1, j = 1; i < n; i++)
    if (strcmp (intr[i], intr[j - 1]))
      intr[j++] = intr[i];
  n = j;

  for (i = 0; i < numberof (ed); i++)
    if (ed[i].interactive)
      {
        for (j = 0; j < n; j++)
          if (!strcmp (ed[i].interactive, intr[j]))
            {
              ed[i].iindex = j + 1;
              break;
            }
        if (j == n)
          {
            fprintf (stderr, "Unable to find `%s'\n", ed[i].interactive);
            exit (2);
          }
      }

  printf ("lintr intrs[] =\n{\n");
  for (i = 0; i < n; i++)
    {
      printf ("  {");
      putq (intr[i]);
      printf ("},\n");
    }
  printf ("  {0},\n");
  printf ("};\n\n");
}

static void
print_version ()
{
  printf ("int dump_version = %d;\n", time (0));
}

void
main (int argc, char **argv)
{
  if (argc == 1)
    exit (2);
  do_all (make_symbol_name);
  do_all (check_dup);
  if (!strcmp (argv[1], "-symtable"))
    {
      print_string ();
      process_interactive ();
      do_all (compose);
      do_all (print_defuns);
      do_all (print_defvars);
    }
  else if (!strcmp (argv[1], "-proto"))
    do_all (print_proto);
  else if (!strcmp (argv[1], "-vars"))
    do_all (print_vars);
  else if (!strcmp (argv[1], "-version"))
    print_version ();
  else
    exit (2);
  exit (0);
}
