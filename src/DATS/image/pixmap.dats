#include
"share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload
_ = "prelude/DATS/integer.dats"
staload
_ = "prelude/DATS/integer_fixed.dats"
staload
_ = "prelude/DATS/array.dats"
staload
_ = "prelude/DATS/matrix.dats"

staload "./../../SATS/image/pixmap.sats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0

(* ****** ****** *)

assume pixmap (t:t@ype, m:int, n:int) = [l:addr] (
  matrix_v (t, l, m, n)
, mfree_gc_v (l)
| size_t m, size_t n, ptr l
) (* end of [pixmap] *)

implement{a}
pixmap_new {m,n} (res, m, n, fill) = let
  val (pf_mat, pf_free | p_mat) = matrix_ptr_alloc<a> (m, n)
  implement
  matrix_initize$init<a> (i, j, x) = (x := fill)
  val () = matrix_initize<a> (!p_mat, m, n)
  val () = res := (pf_mat, pf_free | m, n, p_mat)
in

end // end of [pixmap_new]

implement{a}
pixmap_delete {m,n} (pm) = {
  val (pf_mat, pf_free | m, n, p_mat) = pm
  prval [l:addr] EQADDR () = eqaddr_make_ptr (p_mat)
  prval pf_mn = mul_make {m,n} ()
  val mn = g1uint_mul (m, n)
  prval () = mul_elim (pf_mn)
  prval pf_arr = matrix2array_v {a} (pf_mat)
  implement
  array_uninitize$clear<a> (i, x) = ()
  val () = array_uninitize<a> (!p_mat, mn)
  prval pf_mat = array2matrix_v {a?} {l} {m,n} (pf_arr)
  val () = matrix_ptr_free {a} (pf_mat, pf_free | p_mat)
} (* end of [pixmap_delete] *)

implement
pixmap_delete_getbuf {a} {m,n} (pm, res) = let
  val (pf_mat, pf_free | m, n, p_mat) = pm
  val () = res := p_mat
in
  (pf_mat, pf_free | ())
end // end of [pixmap_delete_getbuf]

implement{a}
pixmap_set_at_int (pm, i, j, x) = let
  val (pf_mat, pf_free | m, n, p_mat) = pm
  val i = (i2sz)i
  val j = (i2sz)j
  val () = matrix_set_at_size<a> (!p_mat, i, n, j, x)
  val () = pm := (pf_mat, pf_free | m, n, p_mat)
in
end // end of [pixmap_set_at_int]

implement{a}
pixmap_set_at_int2 {m,n} (pm, i, j, x) = let
//
  val i = (g1ofg0)i
  val w = pixmap_get_width (pm)
  prval () = lemma_g1uint_param (w)
  val w = g1uint2int_size_int (w)
//
  val j = (g1ofg0)j
  val h = pixmap_get_height (pm)
  prval () = lemma_g1uint_param (h)
  val h = g1uint2int_size_int (h)
//
in
  if i < w then (
    if i >= 0 then (
      if j >= 0 then (
        if j < h then pixmap_set_at_int (pm, i, j, x))))
end

implement{a}
pixmap_get_at_int {m,n} (pm, i, j) = let
  val (pf_mat, pf_free | m, n, p_mat) = pm
  val i = (i2sz)i
  val j = (i2sz)j
  val res = matrix_get_at_size<a> (!p_mat, i, n, j)
  val () = pm := (pf_mat, pf_free | m, n, p_mat)
in
  res
end
  
implement{a}
pixmap_get_width {m,n} (pm) = pm.2
implement{a}
pixmap_get_height {m,n} (pm) = pm.3

implement{a}
pixmap_clear {m,n} (pm, fill) = {
  implement(env)
  matrix_foreach$fwork<a><env> (x, env) = {
    val () = x := fill
  } (* end of [matrix_foreach$fwork] *)
  val (pf_mat, pf_free | m, n, p_mat) = pm
  val () = matrix_foreach<a> (!p_mat, m, n)
  val () = pm := (pf_mat, pf_free | m, n, p_mat)
} (* end of [pixmap_clear] *)
