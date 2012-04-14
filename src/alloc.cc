#include "stdafx.h"
#include "alloc.h"

struct alloc_page_rep
{
  alloc_page_rep *next;
  u_int commit;          // 各ビットに対応するページがcommitされているかどうか
};

struct fixed_heap_rep
{
  fixed_heap_rep *next;
};

u_int alloc_page::ap_page_size;
u_int alloc_page::ap_block_size;

#ifdef DEBUG
static int
power_of_2_p (u_int size)
{
  int n;
  for (n = 0; size; size >>= 1)
    if (size & 1)
      n++;
  return n == 1;
}
#endif

alloc_page::alloc_page (u_int size)
     : ap_rep (0)
{
  assert (size);
  assert (power_of_2_p (size));

  if (!ap_page_size)
    {
      SYSTEM_INFO si;
      GetSystemInfo (&si);
      ap_page_size = si.dwPageSize;
      ap_block_size = si.dwAllocationGranularity;
    }

  ap_unit_size = max (size, ap_page_size);
  ap_units_per_block = ap_block_size / ap_unit_size;

  assert (ap_units_per_block <= BITS_PER_INT);

  if (ap_units_per_block == 1)
    ap_units_per_block = 0;
}

void *
alloc_page::alloc ()
{
  if (ap_units_per_block)
    {
      if (!ap_rep)
        {
          void *base = VirtualAlloc (0, ap_block_size,
                                     MEM_RESERVE, PAGE_NOACCESS);
          if (!base)
            return 0;

          assert (!(pointer_t (base) % ap_block_size));

          ap_rep = (alloc_page_rep *)VirtualAlloc (base, ap_unit_size,
                                                   MEM_COMMIT, PAGE_READWRITE);
          if (!ap_rep)
            {
              VirtualFree (base, 0, MEM_RELEASE);
              return 0;
            }

          assert (ap_rep == base);

          ap_rep->commit = 1;
        }

      for (u_int i = 0; i < ap_units_per_block; i++)
        if (!(ap_rep->commit & (1 << i)))
          {
            void *base = (void *)((u_int (ap_rep) & ~(ap_block_size - 1))
                                  + i * ap_unit_size);
            void *p = VirtualAlloc (base, ap_unit_size,
                                    MEM_COMMIT, PAGE_READWRITE);
            if (!p)
              return 0;

            assert (p == base);

            ap_rep->commit |= (1 << i);
            return p;
          }

      void *p = (void *)ap_rep;
      ap_rep = ap_rep->next;
      return p;
    }
  else
    {
      void *p = VirtualAlloc (0, ap_unit_size, MEM_RESERVE, PAGE_READWRITE);
      if (!p)
        return 0;
      void *q = VirtualAlloc (p, ap_unit_size, MEM_COMMIT, PAGE_READWRITE);
      if (!q)
        {
          assert (0);
          VirtualFree (p, 0, MEM_RELEASE);
          return 0;
        }
      assert (p == q);
      assert (!(pointer_t (q) % ap_block_size));
      return q;
    }
}

void
alloc_page::free (void *p)
{
  assert (p);

  if (ap_units_per_block)
    {
      pointer_t base = pointer_t (p);
      assert (!(base & (ap_unit_size - 1)));

      u_long mask = ~(ap_block_size - 1);
      base &= mask;

      alloc_page_rep *r, *prev = 0;
      for (r = ap_rep; r; prev = r, r = r->next)
        if ((pointer_t (r) & mask) == base)
          {
            u_long d = (pointer_t (p) - base) / ap_unit_size;
            assert (r->commit & (1 << d));
            r->commit &= ~(1 << d);
            VirtualFree (p, ap_unit_size, MEM_DECOMMIT);

            d = (pointer_t (r) - base) / ap_unit_size;
            assert (r->commit & (1 << d));
            if (r->commit != (1U << d))
              return;

            if (prev)
              prev->next = r->next;
            else
              ap_rep = r->next;
            VirtualFree (r, ap_unit_size, MEM_DECOMMIT);
            VirtualFree ((void *)base, 0, MEM_RELEASE);
            return;
          }

      r = (alloc_page_rep *)p;
      r->commit = (1U << ap_units_per_block) - 1;
      r->next = ap_rep;
      ap_rep = r;
    }
  else
    {
      assert (!(pointer_t (p) & (ap_block_size - 1)));
      VirtualFree (p, 0, MEM_RELEASE);
    }
}

fixed_heap::fixed_heap (u_int size)
     : fh_ap (size), fh_heap (0), fh_heap_size (size)
{
  assert (fh_heap_size);
  assert (power_of_2_p (fh_heap_size));
  assert (fh_heap_size >= fh_ap.ap_page_size
          ? !(fh_heap_size % fh_ap.ap_page_size)
          : !(fh_ap.ap_page_size % fh_heap_size));
  fh_heap_per_page = fh_ap.ap_page_size / fh_heap_size;
  if (fh_heap_per_page == 1)
    fh_heap_per_page = 0;
}

void *
fixed_heap::alloc ()
{
  if (!fh_heap_per_page)
    return fh_ap.alloc ();

  if (!fh_heap)
    {
      fh_heap = (fixed_heap_rep *)fh_ap.alloc ();
      if (!fh_heap)
        return 0;

      fixed_heap_rep *h = fh_heap;
      for (u_int i = 1; i < fh_heap_per_page; i++, h = h->next)
        h->next = (fixed_heap_rep *)((char *)h + fh_heap_size);
      h->next = 0;
    }

  void *p = (void *)fh_heap;
  fh_heap = fh_heap->next;
  return p;
}

void
fixed_heap::free (void *p)
{
  assert (p);

  if (!fh_heap_per_page)
    {
      fh_ap.free (p);
      return;
    }

  pointer_t base = pointer_t (p);
  assert (!(base & (fh_heap_size - 1)));

  u_long mask = ~(fh_ap.ap_page_size - 1);
  base &= mask;

  u_int count = 1;
  for (fixed_heap_rep *h = fh_heap; h; h = h->next)
    if ((pointer_t (h) & mask) == base)
      count++;
  assert (count <= fh_heap_per_page);

  if (count < fh_heap_per_page)
    {
      fixed_heap_rep *h = (fixed_heap_rep *)p;
      h->next = fh_heap;
      fh_heap = h;
    }
  else
    {
      fixed_heap_rep *nheap = 0;
      fixed_heap_rep *next;
      for (fixed_heap_rep *h = fh_heap; h; h = next)
        {
          next = h->next;
          if ((pointer_t (h) & mask) != base)
            {
              h->next = nheap;
              nheap = h;
            }
        }
      fh_heap = nheap;
      fh_ap.free ((void *)base);
    }
}
