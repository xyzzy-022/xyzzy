#include "stdafx.h"
#include "ed.h"
#include "sequence.h"
#include "safe_ptr.h"

/*GENERIC_FUNCTION:SEQUENCE*/

seq_testproc::seq_testproc (lisp it, lisp keys)
     : test_proc (keys)
{
  ltest = find_keyword (Ktest, keys, Qnil);
  lisp ltest_not = find_keyword (Ktest_not, keys, Qnil);
  if (ltest != Qnil && ltest_not != Qnil)
    FEprogram_error (Etest_and_test_not_are_both_specified);
  test_not = ltest_not != Qnil;
  if (ltest == Qnil)
    ltest = test_not ? ltest_not : Seql;
  ptest = fast_funcall_p (ltest, 2);
  item = it;
}

int
seq_testproc::test (lisp x)
{
  return call_testfn (item, call_keyfn (x)) != test_not;
}

seq_testproc_if::seq_testproc_if (lisp pred, lisp keys)
     : test_proc (keys)
{
  ltest = pred;
  ptest = fast_funcall_p (ltest, 1);
}

int
seq_testproc_if::test (lisp x)
{
  return call_testfn (call_keyfn (x));
}

seq_testproc_if_not::seq_testproc_if_not (lisp pred, lisp keys)
     : seq_testproc_if (pred, keys)
{
}

int
seq_testproc_if_not::test (lisp x)
{
  return !call_testfn (call_keyfn (x));
}

enum
{
  SEQ_LIST,
  SEQ_VECTOR,
  SEQ_STRING,
  SEQ_INVALID
};

static int
sequence_type (lisp object)
{
  if (immediatep (object))
    return SEQ_INVALID;
  switch (object_typeof (object))
    {
    case Tcons:
      return SEQ_LIST;

    case Tsimple_vector:
    case Tcomplex_vector:
      return SEQ_VECTOR;

    case Tsimple_string:
    case Tcomplex_string:
      return SEQ_STRING;

    default:
      return object == Qnil ? SEQ_LIST : SEQ_INVALID;
    }
}

static inline int
check_sequence (lisp object)
{
  int type = sequence_type (object);
  if (type == SEQ_INVALID)
    FEtype_error (object, Qsequence);
  return type;
}

void
seq_start_end (int len, int &start, int &end, lisp lstart, lisp lend)
{
  start = fixnum_value (lstart);
  end = (lend && lend != Qnil) ? fixnum_value (lend) : len;
  if (start < 0 || start > end)
    FErange_error (lstart);
  if (end > len)
    FErange_error (lend);
}

lisp
seq_to_string (lisp seq)
{
  switch (check_sequence (seq))
    {
    default:
      assert (0);

    case SEQ_LIST:
      return make_string_from_list (seq);

    case SEQ_VECTOR:
      return make_string_from_vector (seq);

    case SEQ_STRING:
      return seq;
    }
}

lisp
Fsequencep (lisp object)
{
  return boole (sequence_type (object) != SEQ_INVALID);
}

lisp
Felt (lisp seq, lisp index)
{
  int i;
  switch (check_sequence (seq))
    {
    default:
      assert (0);

    case SEQ_LIST:
      return Fnth (index, seq);

    case SEQ_VECTOR:
      i = fixnum_value (index);
      if (i < 0 || i >= xvector_length (seq))
        FErange_error (index);
      return xvector_contents (seq) [i];

    case SEQ_STRING:
      i = fixnum_value (index);
      if (i < 0 || i >= xstring_length (seq))
        FErange_error (index);
      return make_char (xstring_contents (seq) [i]);
    }
}

lisp
Fsi_set_elt (lisp seq, lisp index, lisp value)
{
  int i;
  switch (check_sequence (seq))
    {
    default:
      assert (0);

    case SEQ_LIST:
      Frplaca (Fnthcdr (index, seq), value);
      break;

    case SEQ_VECTOR:
      i = fixnum_value (index);
      if (i < 0 || i >= xvector_length (seq))
        FErange_error (index);
      xvector_contents (seq) [i] = value;
      break;

    case SEQ_STRING:
      check_char (value);
      i = fixnum_value (index);
      if (i < 0 || i >= xstring_length (seq))
        FErange_error (index);
      xstring_contents (seq) [i] = xchar_code (value);
      break;
    }
  return value;
}

lisp
Fsubseq (lisp seq, lisp start, lisp end)
{
  switch (check_sequence (seq))
    {
    default:
      assert (0);

    case SEQ_LIST:
      return subseq_list (seq, start, end);

    case SEQ_VECTOR:
      return subseq_vector (seq, start, end);

    case SEQ_STRING:
      return subseq_string (seq, start, end);
    }
}

lisp
Fcopy_seq (lisp seq)
{
  switch (check_sequence (seq))
    {
    default:
      assert (0);

    case SEQ_LIST:
      return Fcopy_list (seq);

    case SEQ_VECTOR:
      return copy_vector (seq);

    case SEQ_STRING:
      return copy_string (seq);
    }
}

lisp
Flength (lisp seq)
{
  switch (check_sequence (seq))
    {
    default:
      assert (0);

    case SEQ_LIST:
      return make_fixnum (xlist_length (seq));

    case SEQ_STRING:
    case SEQ_VECTOR:
      return make_fixnum (xvector_length (seq));
    }
}

template <class T>
void
nreverse (T *l, int size)
{
  T *r = l + size - 1;
  T *m = l + size / 2;
  for (; l < m; l++, r--)
    {
      T t = *l;
      *l = *r;
      *r = t;
    }
}

lisp
Freverse (lisp seq)
{
  switch (check_sequence (seq))
    {
    default:
      assert (0);

    case SEQ_LIST:
      return Frevappend (seq, Qnil);

    case SEQ_VECTOR:
      seq = copy_vector (seq);
      nreverse (xvector_contents (seq), xvector_length (seq));
      return seq;

    case SEQ_STRING:
      seq = copy_string (seq);
      nreverse (xstring_contents (seq), xstring_length (seq));
      return seq;
    }
}

lisp
Fnreverse (lisp seq)
{
  switch (check_sequence (seq))
    {
    default:
      assert (0);

    case SEQ_LIST:
      return Fnreconc (seq, Qnil);

    case SEQ_VECTOR:
      nreverse (xvector_contents (seq), xvector_length (seq));
      return seq;

    case SEQ_STRING:
      nreverse (xstring_contents (seq), xstring_length (seq));
      return seq;
    }
}

static void
copy_into_list (lisp list, lisp sequences)
{
  for (; consp (sequences); sequences = xcdr (sequences))
    {
      lisp seq = xcar (sequences);
      switch (sequence_type (seq))
        {
        default:
          assert (0);

        case SEQ_LIST:
          for (; consp (list) && consp (seq);
               list = xcdr (list), seq = xcdr (seq))
            xcar (list) = xcar (seq);
          break;

        case SEQ_VECTOR:
          {
            for (lisp *v = xvector_contents (seq), *ve = v + xvector_length (seq);
                 v < ve && consp (list); v++, list = xcdr (list))
              xcar (list) = *v;
            break;
          }

        case SEQ_STRING:
          {
            for (const Char *s = xstring_contents (seq), *se = s + xstring_length (seq);
                 s < se && consp (list); s++, list = xcdr (list))
              xcar (list) = make_char (*s);
            break;
          }
        }
    }
}


static void
copy_into_vector (lisp vector, lisp sequences)
{
  int l;
  switch (object_typeof (vector))
    {
    default:
      assert (0);

    case Tsimple_vector:
      l = xvector_length (vector);
      break;

    case Tcomplex_vector:
      l = xvector_dimension (vector);
      break;
    }

  if (!l)
    return;
  lisp *v = xvector_contents (vector), *ve = v + l;
  for (; consp (sequences); sequences = xcdr (sequences))
    {
      lisp seq = xcar (sequences);
      switch (sequence_type (seq))
        {
        default:
          assert (0);

        case SEQ_LIST:
          for (; consp (seq) && v < ve; seq = xcdr (seq))
            *v++ = xcar (seq);
          break;

        case SEQ_VECTOR:
          l = min (ve - v, xvector_length (seq));
          bcopy (xvector_contents (seq), v, l);
          v += l;
          break;

        case SEQ_STRING:
          {
            l = min (ve - v, xstring_length (seq));
            for (const Char *s = xstring_contents (seq), *se = s + l;
                 s < se; s++)
              *v++ = make_char (*s);
            break;
          }
        }
    }
}

static void
copy_into_string (lisp string, lisp sequences)
{
  int l;
  switch (object_typeof (string))
    {
    default:
      assert (0);

    case Tsimple_string:
      l = xvector_length (string);
      break;

    case Tcomplex_string:
      l = xvector_dimension (string);
      break;
    }

  if (!l)
    return;
  Char *s = xstring_contents (string), *se = s + l;
  for (; consp (sequences); sequences = xcdr (sequences))
    {
      lisp seq = xcar (sequences);
      switch (sequence_type (seq))
        {
        default:
          assert (0);

        case SEQ_LIST:
          for (; consp (seq) && s < se; seq = xcdr (seq))
            {
              check_char (xcar (seq));
              *s++ = xchar_code (xcar (seq));
            }
          break;

        case SEQ_VECTOR:
          {
            l = min (se - s, xvector_length (seq));
            for (lisp *v = xvector_contents (seq), *ve = v + l; v < ve; v++)
              {
                check_char (*v);
                *s++ = xchar_code (*v);
              }
            break;
          }

        case SEQ_STRING:
          l = min (se - s, xstring_length (seq));
          bcopy (xstring_contents (seq), s, l);
          s += l;
          break;
        }
    }
}

lisp
Fsi_copy_into_seq (lisp result_seq, lisp sequences)
{
  switch (check_sequence (result_seq))
    {
    default:
      assert (0);
      break;

    case SEQ_LIST:
      copy_into_list (result_seq, sequences);
      break;

    case SEQ_VECTOR:
      copy_into_vector (result_seq, sequences);
      break;

    case SEQ_STRING:
      copy_into_string (result_seq, sequences);
      break;
    }
  return result_seq;
}

static inline void
fill_list (lisp list, int start, int end, lisp item)
{
  list = Fnthcdr (make_fixnum (start), list);
  for (; start < end; start++, list = xcdr (list))
    {
      assert (consp (list));
      xcar (list) = item;
    }
}

lisp
Ffill (lisp seq, lisp item, lisp keys)
{
  lisp lstart = find_keyword (Kstart, keys, make_fixnum (0));
  lisp lend = find_keyword (Kend, keys, Qnil);
  int start, end;

  switch (check_sequence (seq))
    {
    default:
      assert (0);

    case SEQ_LIST:
      seq_start_end (xlist_length (seq), start, end, lstart, lend);
      fill_list (seq, start, end, item);
      break;

    case SEQ_VECTOR:
      seq_start_end (xvector_length (seq), start, end, lstart, lend);
      bfill (xvector_contents (seq), start, end, item);
      break;

    case SEQ_STRING:
      seq_start_end (xstring_length (seq), start, end, lstart, lend);
      check_char (item);
      bfill (xstring_contents (seq), start, end, xchar_code (item));
      break;
    }
  return seq;
}

lisp
Freplace (lisp seq1, lisp seq2, lisp keys)
{
  lisp seq = seq1;
  lisp lstart1 = find_keyword (Kstart1, keys, make_fixnum (0));
  lisp lend1 = find_keyword (Kend1, keys, Qnil);
  lisp lstart2 = find_keyword (Kstart2, keys, make_fixnum (0));
  lisp lend2 = find_keyword (Kend2, keys, Qnil);
  int start1, end1;
  int start2, end2;
  int l;

  switch (check_sequence (seq1))
    {
    default:
      assert (0);

    case SEQ_LIST:
      seq_start_end (xlist_length (seq1), start1, end1, lstart1, lend1);
      switch (check_sequence (seq2))
        {
        default:
          assert (0);

        case SEQ_LIST:
          {
            seq_start_end (xlist_length (seq2), start2, end2, lstart2, lend2);
            l = min (end1 - start1, end2 - start2);
            end1 = start1 + l;
            end2 = start2 + l;
            if (seq1 != seq2 || start1 >= end2 || end1 <= start2)
              {
                seq1 = Fnthcdr (make_fixnum (start1), seq1);
                seq2 = Fnthcdr (make_fixnum (start2), seq2);
                for (int i = 0; i < l; i++, seq1 = xcdr (seq1), seq2 = xcdr (seq2))
                  {
                    assert (consp (seq1));
                    assert (consp (seq2));
                    xcar (seq1) = xcar (seq2);
                  }
              }
            else if (start1 != start2)
              {
                seq1 = Fnthcdr (make_fixnum (start1), seq1);
                seq2 = Fnthcdr (make_fixnum (start2), seq2);
                safe_ptr <char> sp (new char [sizeof (lisp) * l]);
                lisp *x = (lisp *)(char *)sp;
                for (int i = 0; i < l; i++, seq2 = xcdr (seq2))
                  {
                    assert (consp (seq2));
                    x[i] = xcar (seq2);
                  }
                for (int i = 0; i < l; i++, seq1 = xcdr (seq1))
                  {
                    assert (consp (seq1));
                    xcar (seq1) = x[i];
                  }
              }
          }
          break;

        case SEQ_VECTOR:
          {
            seq_start_end (xvector_length (seq2), start2, end2, lstart2, lend2);
            l = min (end1 - start1, end2 - start2);
            seq1 = Fnthcdr (make_fixnum (start1), seq1);
            lisp *v = xvector_contents (seq2) + start2;
            lisp *ve = v + l;
            for (; v < ve; v++, seq1 = xcdr (seq1))
              {
                assert (consp (seq1));
                xcar (seq1) = *v;
              }
          }
          break;

        case SEQ_STRING:
          {
            seq_start_end (xstring_length (seq2), start2, end2, lstart2, lend2);
            l = min (end1 - start1, end2 - start2);
            seq1 = Fnthcdr (make_fixnum (start1), seq1);
            Char *s = xstring_contents (seq2) + start2;
            Char *se = s + l;
            for (; s < se; s++, seq1 = xcdr (seq1))
              {
                assert (consp (seq1));
                xcar (seq1) = make_char (*s);
              }
          }
          break;
        }
      break;

    case SEQ_VECTOR:
      seq_start_end (xvector_length (seq1), start1, end1, lstart1, lend1);
      switch (check_sequence (seq2))
        {
        default:
          assert (0);

        case SEQ_LIST:
          {
            seq_start_end (xlist_length (seq2), start2, end2, lstart2, lend2);
            l = min (end1 - start1, end2 - start2);
            seq2 = Fnthcdr (make_fixnum (start2), seq2);
            lisp *v = xvector_contents (seq1) + start1;
            lisp *ve = v + l;
            for (lisp p = seq2; v < ve; p = xcdr (p), v++)
              {
                assert (consp (p));
                *v = xcar (p);
              }
          }
          break;

        case SEQ_VECTOR:
          {
            seq_start_end (xvector_length (seq2), start2, end2, lstart2, lend2);
            l = min (end1 - start1, end2 - start2);
            lisp *d = xvector_contents (seq1) + start1;
            lisp *de = d + l;
            lisp *s = xvector_contents (seq2) + start2;
            lisp *se = s + l;
            if (s != d)
              {
                if (d < s)
                  while (d < de)
                    *d++ = *s++;
                else
                  while (de > d)
                    *--de = *--se;
              }
          }
          break;

        case SEQ_STRING:
          {
            seq_start_end (xstring_length (seq2), start2, end2, lstart2, lend2);
            l = min (end1 - start1, end2 - start2);
            lisp *v = xvector_contents (seq1) + start1;
            lisp *ve = v + l;
            Char *s = xstring_contents (seq2) + start2;
            while (v < ve)
              *v++ = make_char (*s++);
          }
          break;
        }
      break;

    case SEQ_STRING:
      seq_start_end (xstring_length (seq1), start1, end1, lstart1, lend1);
      switch (check_sequence (seq2))
        {
        default:
          assert (0);

        case SEQ_LIST:
          {
            seq_start_end (xlist_length (seq2), start2, end2, lstart2, lend2);
            l = min (end1 - start1, end2 - start2);
            end2 = start2 + l;
            seq2 = Fnthcdr (make_fixnum (start2), seq2);
            for (lisp p = seq2; start2 < end2; start2++, p = xcdr (p))
              {
                assert (consp (p));
                check_char (xcar (p));
              }
            Char *s = xstring_contents (seq1) + start1;
            Char *se = s + l;
            for (lisp p = seq2; s < se; p = xcdr (p), s++)
              *s = xchar_code (xcar (p));
          }
          break;

        case SEQ_VECTOR:
          {
            seq_start_end (xvector_length (seq2), start2, end2, lstart2, lend2);
            l = min (end1 - start1, end2 - start2);
            lisp *v0 = xvector_contents (seq2) + start2;
            lisp *ve = v0 + l;
            for (lisp *v = v0; v < ve; v++)
              check_char (*v);
            Char *s = xstring_contents (seq1) + start1;
            for (lisp *v = v0; v < ve; v++, s++)
              *s = xchar_code (*v);
          }
          break;

        case SEQ_STRING:
          {
            seq_start_end (xstring_length (seq2), start2, end2, lstart2, lend2);
            l = min (end1 - start1, end2 - start2);
            Char *d = xstring_contents (seq1) + start1;
            Char *de = d + l;
            const Char *s = xstring_contents (seq2) + start2;
            const Char *se = s + l;
            if (s != d)
              {
                if (d < s)
                  while (d < de)
                    *d++ = *s++;
                else
                  while (de > d)
                    *--de = *--se;
              }
          }
          break;
        }
      break;
    }
  return seq;
}

inline lisp
coerce_to_lisp_object (lisp x)
{
  return x;
}

inline lisp
coerce_to_lisp_object (Char c)
{
  return make_char (c);
}

template <class T>
int
xdelete (test_proc &test, T *p, int start, int end, lisp from_end, int count, int l)
{
  if (count <= 0)
    return l;
  if (from_end == Qnil || count >= end - start)
    {
      int i, j;
      for (i = start, j = start; i < end; i++)
        if (!test.test (coerce_to_lisp_object (p[i])))
          p[j++] = p[i];
        else if (!--count)
          {
            i++;
            break;
          }
      if (i == j)
        return l;
      for (; i < l; i++, j++)
        p[j] = p[i];
      return j;
    }
  else
    {
      int i, j;
      for (i = end - 1, j = end; i >= start; i--)
        if (!test.test (coerce_to_lisp_object (p[i])))
          p[--j] = p[i];
        else if (!--count)
          break;
      if (i < start)
        i = start;
      int n = j - i;
      if (!n)
        return l;
      bcopy (&p[j], &p[i], l - j);
      return l - n;
    }
}

static lisp
xdelete (lisp seq, test_proc &test, lisp keys)
{
  lisp lstart = find_keyword (Kstart, keys, make_fixnum (0));
  lisp lend = find_keyword (Kend, keys, Qnil);
  lisp lcount = find_keyword (Kcount, keys, Qnil);
  lisp from_end = find_keyword (Kfrom_end, keys, Qnil);
  int start, end;

  switch (check_sequence (seq))
    {
    default:
      assert (0);

    case SEQ_LIST:
      {
        int l = xlist_length (seq);
        seq_start_end (l, start, end, lstart, lend);
        if (start != end)
          {
            safe_ptr <char> sp (new char [sizeof (lisp) * l]);
            lisp *v = (lisp *)(char *)sp;
            lisp p = seq;
            for (int i = 0; i < l; i++, p = xcdr (p))
              v[i] = xcar (p);
            int n = xdelete (test, v, start, end, from_end,
                             lcount == Qnil ? l : fixnum_value (lcount), l);
            if (!n)
              return Qnil;
            if (n != l)
              {
                p = seq;
                n--;
                int i;
                for (i = 0; i < n; i++, p = xcdr (p))
                  xcar (p) = v[i];
                assert (consp (p));
                xcar (p) = v[i];
                xcdr (p) = Qnil;
              }
          }
      }
      break;

    case SEQ_VECTOR:
      seq_start_end (xvector_length (seq), start, end, lstart, lend);
      xvector_length (seq) = xdelete (test, xvector_contents (seq), start, end, from_end,
                                      lcount == Qnil ? end : fixnum_value (lcount),
                                      xvector_length (seq));
      break;

    case SEQ_STRING:
      seq_start_end (xstring_length (seq), start, end, lstart, lend);
      xstring_length (seq) = xdelete (test, xstring_contents (seq), start, end, from_end,
                                      lcount == Qnil ? end : fixnum_value (lcount),
                                      xstring_length (seq));
      break;
    }
  return seq;
}

lisp
Fremove (lisp item, lisp seq, lisp keys)
{
  seq = Fcopy_seq (seq);
  protect_gc gcpro (seq);
  return Fdelete (item, seq, keys);
}

lisp
Fremove_if (lisp pred, lisp seq, lisp keys)
{
  seq = Fcopy_seq (seq);
  protect_gc gcpro (seq);
  return Fdelete_if (pred, seq, keys);
}

lisp
Fremove_if_not (lisp pred, lisp seq, lisp keys)
{
  seq = Fcopy_seq (seq);
  protect_gc gcpro (seq);
  return Fdelete_if_not (pred, seq, keys);
}

lisp
Fdelete (lisp item, lisp seq, lisp keys)
{
  seq_testproc t (item, keys);
  return xdelete (seq, t, keys);
}

lisp
Fdelete_if (lisp pred, lisp seq, lisp keys)
{
  seq_testproc_if t (pred, keys);
  return xdelete (seq, t, keys);
}

lisp
Fdelete_if_not (lisp pred, lisp seq, lisp keys)
{
  seq_testproc_if_not t (pred, keys);
  return xdelete (seq, t, keys);
}

static void
position_find (lisp seq, test_proc &test, lisp keys, lisp &pos, lisp &found)
{
  lisp lstart = find_keyword (Kstart, keys, make_fixnum (0));
  lisp lend = find_keyword (Kend, keys, Qnil);
  lisp from_end = find_keyword (Kfrom_end, keys, Qnil);
  int start, end, i;

  switch (check_sequence (seq))
    {
    default:
      assert (0);

    case SEQ_LIST:
      seq_start_end (xlist_length (seq), start, end, lstart, lend);
      seq = Fnthcdr (make_fixnum (start), seq);
      if (from_end == Qnil)
        {
          for (i = start; i < end; i++, seq = xcdr (seq))
            {
              assert (consp (seq));
              if (test.test (xcar (seq)))
                {
                  pos = make_fixnum (i);
                  found = xcar (seq);
                  return;
                }
            }
        }
      else if (start != end)
        {
          int l = end - start;
          safe_ptr <char> sp (new char [sizeof (lisp) * l]);
          lisp *p = (lisp *)(char *)sp;
          for (i = 0; i < l; i++, seq = xcdr (seq))
            {
              assert (seq);
              p[i] = xcar (seq);
            }
          for (i = l - 1; i >= 0; i--)
            if (test.test (p[i]))
              {
                pos = make_fixnum (start + i);
                found = p[i];
                return;
              }
        }
      break;

    case SEQ_VECTOR:
      {
        seq_start_end (xvector_length (seq), start, end, lstart, lend);
        lisp *p = xvector_contents (seq);
        if (from_end == Qnil)
          {
            for (i = start; i < end; i++)
              if (test.test (p[i]))
                {
                  pos = make_fixnum (i);
                  found = p[i];
                  return;
                }
          }
        else
          {
            for (i = end - 1; i >= start; i--)
              if (test.test (p[i]))
                {
                  pos = make_fixnum (i);
                  found = p[i];
                  return;
                }
          }
      }
      break;

    case SEQ_STRING:
      {
        seq_start_end (xstring_length (seq), start, end, lstart, lend);
        Char *s = xstring_contents (seq);
        if (from_end == Qnil)
          {
            for (i = start; i < end; i++)
              if (test.test (make_char (s[i])))
                {
                  pos = make_fixnum (i);
                  found = make_char (s[i]);
                  return;
                }
          }
        else
          {
            for (i = end - 1; i >= start; i--)
              if (test.test (make_char (s[i])))
                {
                  pos = make_fixnum (i);
                  found = make_char (s[i]);
                  return;
                }
          }
      }
      break;
    }
  pos = Qnil;
  found = Qnil;
}

static lisp
find (lisp seq, test_proc &test, lisp keys)
{
  lisp pos, item;
  position_find (seq, test, keys, pos, item);
  return item;
}

lisp
Ffind (lisp key, lisp seq, lisp keys)
{
  seq_testproc t (key, keys);
  return find (seq, t, keys);
}

lisp
Ffind_if (lisp pred, lisp seq, lisp keys)
{
  seq_testproc_if t (pred, keys);
  return find (seq, t, keys);
}

lisp
Ffind_if_not (lisp pred, lisp seq, lisp keys)
{
  seq_testproc_if_not t (pred, keys);
  return find (seq, t, keys);
}

static lisp
position (lisp seq, test_proc &test, lisp keys)
{
  lisp pos, item;
  position_find (seq, test, keys, pos, item);
  return pos;
}

lisp
Fposition (lisp key, lisp seq, lisp keys)
{
  seq_testproc t (key, keys);
  return position (seq, t, keys);
}

lisp
Fposition_if (lisp pred, lisp seq, lisp keys)
{
  seq_testproc_if t (pred, keys);
  return position (seq, t, keys);
}

lisp
Fposition_if_not (lisp pred, lisp seq, lisp keys)
{
  seq_testproc_if_not t (pred, keys);
  return position (seq, t, keys);
}

class sort_testproc: public test_proc
{
public:
  sort_testproc (lisp pred, lisp keys) : test_proc (keys)
    {
      ltest = pred;
      ptest = fast_funcall_p (ltest, 2);
    }
  int test (lisp x, lisp y) const
    {return call_testfn (call_keyfn (x), call_keyfn (y));}
  int test (lisp) { return 0; }
};

template <class key_t>
void
sort (const sort_testproc &test, key_t *base, int num, key_t *tem)
{
  if (num == 2)
    {
      if (test.test (coerce_to_lisp_object (base[1]),
                     coerce_to_lisp_object (base[0])))
        swap (base[0], base[1]);
    }
  else /* num > 2 */
    {
      int n1 = num / 2;
      int n2 = num - n1;
      if (n1 != 1)
        sort (test, base, n1, tem);
      if (n2 != 1)
        sort (test, base + n1, n2, tem);
      memcpy (tem, base, sizeof *tem * n1);
      key_t *b = base;
      key_t *p = tem, *pe = tem + n1;
      key_t *q = base + n1, *qe = base + num;
      while (p < pe && q < qe)
        *b++ = (test.test (coerce_to_lisp_object (*q),
                           coerce_to_lisp_object (*p))
                ? *q++ : *p++);
      while (p < pe)
        *b++ = *p++;
      while (q < qe)
        *b++ = *q++;
    }
}

static inline int
sort_protect_gc (Char *, int)
{
  return 0;
}

static inline int
sort_protect_gc (lisp *p, int n)
{
  for (lisp *pe = p + n; p < pe; p++)
    *p = Qnil;
  return n;
}

template <class key_t>
void
sort (const sort_testproc &test, key_t *base, int num)
{
#define WORKBUF_SIZE 1024
  if (num > 2)
    {
      int n = num / 2;
      if (n > WORKBUF_SIZE)
        {
          safe_ptr <key_t> tem (new key_t [n]);
          protect_gc gcpro ((lisp *)(key_t *)tem, sort_protect_gc (tem, n));
          sort (test, base, num, (key_t *)tem);
        }
      else
        {
          key_t buf[WORKBUF_SIZE];
          protect_gc gcpro ((lisp *)buf, sort_protect_gc (buf, n));
          sort (test, base, num, buf);
        }
    }
  else if (num == 2 && test.test (coerce_to_lisp_object (base[1]),
                                  coerce_to_lisp_object (base[0])))
    swap (base[0], base[1]);
}

static lisp
sort_list (const sort_testproc &test, lisp p, int num)
{
  if (num == 2)
    {
      lisp q = xcdr (p);
      if (!test.test (xcar (q), xcar (p)))
        return p;
      xcdr (q) = p;
      xcdr (p) = Qnil;
      return q;
    }
  else /* num > 2 */
    {
      int n1 = num / 2;
      int n2 = num - n1;
      lisp q, r = p;

      for (int n = 1; n < n1; n++)
        r = xcdr (r);
      q = xcdr (r);
      xcdr (r) = Qnil;

      protect_gc gcpro1 (p), gcpro2 (q);
      if (n1 != 1)
        p = sort_list (test, p, n1);
      if (n2 != 1)
        q = sort_list (test, q, n2);

      lisp b = Qnil, bp;
      protect_gc gcpro3 (b);
      while (consp (p) && consp (q))
        {
          if (!test.test (xcar (q), xcar (p)))
            {
              r = p;
              p = xcdr (p);
            }
          else
            {
              r = q;
              q = xcdr (q);
            }
          if (b == Qnil)
            b = bp = r;
          else
            {
              xcdr (bp) = r;
              bp = r;
            }
        }
      xcdr (bp) = consp (p) ? p : q;
      return b;
    }
}

lisp
Fstable_sort (lisp seq, lisp pred, lisp keys)
{
  sort_testproc test (pred, keys);
  switch (check_sequence (seq))
    {
    default:
      assert (0);

    case SEQ_LIST:
      {
        int l = xlist_length (seq);
        if (l >= 2)
          return sort_list (test, seq, l);
        break;
      }

    case SEQ_VECTOR:
      sort (test, xvector_contents (seq), xvector_length (seq));
      break;

    case SEQ_STRING:
      sort (test, xstring_contents (seq), xstring_length (seq));
      break;
    }
  return seq;
}
