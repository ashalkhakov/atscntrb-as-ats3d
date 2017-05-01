#include "share/atspre_staload.hats"

staload STDIO = "libats/libc/SATS/stdio.sats"

staload _ = "prelude/DATS/string.dats"
staload _ = "prelude/DATS/strptr.dats"

staload "./../SATS/file_util.sats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0

(* ****** ****** *)

implement
string_replace_extension (base, suffix) = let
  val base = g1ofg0 (base)
  val ix = string_rindex (base, '.')
in
  if ix = ~1 then strptr_null ()
  else let
    val ix = g1int2uint_ssize_size ix
    val subs = string_make_substring (base, (i2sz)0, ix)
    val subs = strnptr2strptr subs
    val ext = string1_copy ((g1ofg0)suffix)
    val ext = strnptr2strptr ext
    val res = strptr_append (subs, ext)
    val () = strptr_free (subs)
    val () = strptr_free (ext)
  in
    res
  end
end // end of [string_replace_extension]

(* ****** ****** *)

implement{a,b}
file_read_tup2 (fp, a, b) = let
  val ar = file_read<a> (fp, a) in
  if ar then let
    val br = file_read<b> (fp, b) in
    if br then true
    else let
      prval () = opt_reset_mac (a) in
      false
    end
  end else let
    prval () = opt_none {b} (b) in
    false
  end
end // end of [file_read_tup2]

implement{a,b,c}
file_read_tup3 (fp, a, b, c) = let
  val abr = file_read_tup2<a,b> (fp, a, b) in
  if abr then let
    val cr = file_read<c> (fp, c) in
    if cr then true
    else let
      prval () = opt_reset_mac (a, b) in
      false
    end
  end else let
    prval () = opt_none {c} (c) in
    false
  end
end // end of [file_read_tup3]

implement{a,b,c,d}
file_read_tup4 (fp, a, b, c, d) = let
  val abcr = file_read_tup3<a,b,c> (fp, a, b, c) in
  if abcr then let
    val dr = file_read<d> (fp, d) in
    if dr then true
    else let
      prval () = opt_reset_mac (a, b, c) in
      false
    end
  end else let
    prval () = opt_none {d} (d) in
    false
  end
end // end of [file_read_tup4]

implement{a,b,c,d,e}
file_read_tup5 (fp, a, b, c, d, e) = let
  val abcdr = file_read_tup4<a,b,c,d> (fp, a, b, c, d) in
  if abcdr then let
    val er = file_read<e> (fp, e) in
    if er then true
    else let
      prval () = opt_reset_mac (a, b, c, d) in
      false
    end
  end else let
    prval () = opt_none {e} (e) in
    false
  end
end // end of [file_read_tup5]

implement{a,b,c,d,e,f}
file_read_tup6 (fp, a, b, c, d, e, f) = let
  val abcder = file_read_tup5<a,b,c,d,e> (fp, a, b, c, d, e) in
  if abcder then let
    val fr = file_read<f> (fp, f) in
    if fr then true
    else let
      prval () = opt_reset_mac (a, b, c, d, e) in
      false
    end
  end else let
    prval () = opt_none {f} (f) in
    false
  end
end // end of [file_read_tup6]

implement{a,b,c,d,e,f,g}
file_read_tup7 (fp, a, b, c, d, e, f, g) = let
  val abcdefr = file_read_tup6<a,b,c,d,e,f> (fp, a, b, c, d, e, f) in
  if abcdefr then let
    val gr = file_read<g> (fp, g) in
    if gr then true
    else let
      prval () = opt_reset_mac (a, b, c, d, e, f) in
      false
    end
  end else let
    prval () = opt_none {g} (g) in
    false
  end
end // end of [file_read_tup7]

implement{a,b,c,d,e,f,g,h}
file_read_tup8 (fp, a, b, c, d, e, f, g, h) = let
  val abcdefgr = file_read_tup7<a,b,c,d,e,f,g> (fp, a, b, c, d, e, f, g) in
  if abcdefgr then let
    val hr = file_read<h> (fp, h) in
    if hr then true
    else let
      prval () = opt_reset_mac (a, b, c, d, e, f, g) in
      false
    end
  end else let
    prval () = opt_none {h} (h) in
    false
  end
end // end of [file_read_tup8]

(* ****** ****** *)

implement
file_read_uint8_le (f, r) = let
  val res = $STDIO.fgetc(f)
in
  if res < 0 then let
    prval () = opt_none {uint8} (r)
  in
    false
  end else let
    val () = r := $UNSAFE.castvwtp0 {uint8} (res)
    prval () = opt_some {uint8} (r)
  in
    true
  end
end // end of [file_read_uint8_le]
//
implement
file_read_uint16_le (f, r) = let
  var r1: uint8
  and r2: uint8
  val b1 = file_read_uint8_le (f, r1)
in
  if :(r1: uint8?, r2: uint8?) => b1 then let
    val b2 = file_read_uint8_le (f, r2)
  in
    if :(r1: uint8?, r2: uint8?) => b2 then let
      val r1_ = $UNSAFE.castvwtp0{uint16} (opt_unsome_get<uint8> (r1))
      val r2_ = $UNSAFE.castvwtp0{uint16} (opt_unsome_get<uint8> (r2))
      val () = r := g0uint_lor_uint16 (g0uint_lsl_uint16 (r2_, 8), r1_)
      prval () = opt_some {uint16} (r)
    in
      true
    end else let
      prval () = opt_none {uint16} (r)
      prval () = opt_clear {uint8} (r1)
      prval () = opt_unnone {uint8} (r2)
    in
      false
    end
  end else let
    prval () = opt_none {uint16} (r)
    prval () = opt_unnone {uint8} (r1)
  in
    false
  end
end
//
implement
file_read_uint32_le (f, r) = let
  var r1: uint16
  and r2: uint16
  val b1 = file_read_uint16_le (f, r1)
in
  if :(r1: uint16?, r2: uint16?) => b1 then let
    val b2 = file_read_uint16_le (f, r2)
  in
    if :(r1: uint16?, r2: uint16?) => b2 then let
      val r1_ = $UNSAFE.castvwtp0{uint32} (opt_unsome_get<uint16> (r1))
      val r2_ = $UNSAFE.castvwtp0{uint32} (opt_unsome_get<uint16> (r2))
      val () = r := g0uint_lor_uint32 (g0uint_lsl_uint32 (r2_, 16), r1_)
      prval () = opt_some {uint32} (r)
    in
      true
    end else let
      prval () = opt_none {uint32} (r)
      prval () = opt_clear {uint16} (r1)
      prval () = opt_unnone {uint16} (r2)
    in
      false
    end
  end else let
    prval () = opt_none {uint32} (r)
    prval () = opt_unnone {uint16} (r1)
  in
    false
  end
end

(* ****** ****** *)

implement{a}
file_read_array$elt (fp, x) = file_read<a> (fp, x)
//
implement{a}
file_read_array_lr {n} (fp, A, asz) = let
//
fun
aux {i,n:nat | i <= n} {l:addr} (
  pf1_arr: array_v (a, l, i), pf2_arr: array_v (a?, l+i*sizeof(a), n-i)
| p_arr: ptr (l+i*sizeof(a)), i: size_t i, n: size_t n
): [r:int] (vor (array_v (a, l, n), array_v (a?, l, n), r) | int r) = (
  if i < n then let
    prval (pf_at, pf2_arr) = array_v_uncons {a?} (pf2_arr)
    prval [l0:addr] EQADDR () = ptr_get_index (p_arr)
    val p_at = p_arr : ptr l0
    prval pf_at = pf_at : a? @ l0
    val res = file_read_array$elt<a> (fp, !p_at)
  in
    if res then let
      prval () = opt_unsome {a} (!p_at)
      prval pf1_arr = array_v_extend {a} (pf1_arr, pf_at)
      val p1_arr = ptr1_succ<a> (p_arr)
    in
      aux (pf1_arr, pf2_arr | p1_arr, succ(i), n)
    end else let
      prval p_bas = praxi_ptr {l} ()
      prval () = opt_unnone {a} (!p_at)
      prval () = topize {@[a][i]} (!p_bas)
      prval pf2_arr = array_v_cons {a?} (pf_at, pf2_arr)
      prval pf_res = array_v_unsplit {a?} (pf1_arr, pf2_arr)
    in
      #[.. | (VOR_r (pf_res) | 1)]
    end
  end else let
    prval () = array_v_unnil {a?} (pf2_arr)
    prval pf_res = pf1_arr
  in
    #[.. | (VOR_l (pf_res) | 0)]
  end // end of [if]
) (* end of [aux] *)
//
prval pf1_arr = array_v_nil {a} ()
prval pf2_arr = view@(A)
val p_arr = addr@(A)
prval [l:addr] EQADDR () = eqaddr_make_ptr (p_arr)
prval () = lemma_g1uint_param (asz)
val (pf_vor | r) = aux {0,n} {l} (pf1_arr, pf2_arr |  p_arr, (i2sz)0, asz)
//
in
//
if r = 0 then let
  prval VOR_l pf_arr = pf_vor
  prval () = view@A := pf_arr
  prval () = opt_some {@[a][n]} (A)
in
  true
end else let
  prval VOR_r pf_arr = pf_vor
  prval () = view@A := pf_arr
  prval () = opt_none {@[a][n]} (A)
in
  false
end // end of [if]
//
end // end of [file_read_array_lr]
//
implement{a}
file_read_array_rl {n} (fp, A, asz) = let
//
fun
aux {i,n:nat | i <= n} {l:addr} (
  pf1_arr: array_v (a?, l, i), pf2_arr: array_v (a, l+i*sizeof(a), n-i)
| p_arr: ptr (l+i*sizeof(a)), i: size_t i, n: size_t n
): [r:int] (vor (array_v (a, l, n), array_v (a?, l, n), r) | int r) = (
  if i > 0 then let
    prval (pf1_arr, pf_at) = array_v_unextend {a?} (pf1_arr)
    val p1_arr = ptr1_pred<a> (p_arr)
    prval [l0:addr] EQADDR () = ptr_get_index (p1_arr)
    val p_at = p1_arr : ptr l0
    prval pf_at = pf_at : a? @ l0
    val res = file_read_array$elt<a> (fp, !p_at)
  in
    if res then let
      prval () = opt_unsome {a} (!p_at)
      prval pf2_arr = array_v_cons {a}{l0} (pf_at, pf2_arr)
    in
      aux (pf1_arr, pf2_arr | p1_arr, pred(i), n)
    end else let
      prval p_bas = praxi_ptr {l} ()
      prval () = opt_unnone {a} (!p_at)
      prval pf1_arr = array_v_extend {a?} (pf1_arr, pf_at)
      prval pf_res = array_v_unsplit {a?} (pf1_arr, pf2_arr)
    in
      #[.. | (VOR_r (pf_res) | 1)]
    end
  end else let
    prval () = array_v_unnil {a?} (pf1_arr)
    prval pf_res = pf2_arr
    prval MULbas () = mul_make {i,sizeof(a)} ()
  in
    #[.. | (VOR_l (pf_res) | 0)]
  end // end of [if]
) (* end of [aux] *)
//
val p_arr = addr@(A)
prval [l0:addr] EQADDR () = eqaddr_make_ptr (p_arr)
val p_arr = ptr1_add_guint<a> (p_arr, asz)
prval [l:addr] EQADDR () = eqaddr_make_ptr (p_arr)
//
prval pf1_arr = view@(A)
prval pf2_arr = array_v_nil {a} ()
prval () = lemma_g1uint_param (asz)
val (pf_vor | r) = aux (pf1_arr, pf2_arr |  p_arr, asz, asz)
//
in
//
if r = 0 then let
  prval VOR_l pf_arr = pf_vor
  prval () = view@A := pf_arr
  prval () = opt_some {@[a][n]} (A)
in
  true
end else let
  prval VOR_r pf_arr = pf_vor
  prval () = view@A := pf_arr
  prval () = opt_none {@[a][n]} (A)
in
  false
end // end of [if]
//
end // end of [file_read_array_rl]
//
(* ****** ****** *)
//
extern
praxi
lemma_array_sizeof {a:vt@ype} {n:nat} (): [sizeof(@[a][n]) == n*sizeof(a)] void
//
extern
praxi
lemma_array2_sizeof {a:vt@ype} {m,n:nat} (): [sizeof(@[a][m*n]) == m*n*sizeof(a)] void
//
implement{a}
file_read_matrix_row {n} (fp, A, asz) = file_read_array_lr<a> (fp, A, asz)
//
implement{a}
file_read_matrix_td {m,n} (fp, mat, m, n) = let
//
typedef T = @[a][n]
prval () = lemma_g1uint_param (n)
//
fun
aux {i,m:nat | i <= m} {l:addr} (
  pf1_arr: array_v (T, l, i), pf2_arr: array_v (T?, l+i*sizeof(T), m-i)
| p_arr: ptr (l+i*sizeof(T)), i: size_t i, m: size_t m
): [r:int] (vor (array_v (T, l, m), array_v (T?, l, m), r) | int r) = (
  if i < m then let
    prval (pf_at, pf2_arr) = array_v_uncons {T?} (pf2_arr)
    prval [l0:addr] EQADDR () = ptr_get_index (p_arr)
    val p_at = p_arr : ptr l0
    prval pf_at = pf_at : T? @ l0
    val res = file_read_matrix_row<a> (fp, !p_at, n)
  in
    if res then let
      prval () = opt_unsome {T} (!p_at)
      prval pf1_arr = array_v_extend {T} (pf1_arr, pf_at)
      val p1_arr = ptr1_add_guint<a> (p_arr, n)
      prval () = lemma_array_sizeof {a}{n} ()
    in
      aux (pf1_arr, pf2_arr | p1_arr, succ(i), m)
    end else let
      prval p_bas = praxi_ptr {l} ()
      prval () = opt_unnone {T} (!p_at)
      prval () = topize {@[T][i]} (!p_bas)
      prval pf2_arr = array_v_cons {T?} (pf_at, pf2_arr)
      prval pf_res = array_v_unsplit {T?} (pf1_arr, pf2_arr)
    in
      #[.. | (VOR_r (pf_res) | 1)]
    end
  end else let
    prval () = array_v_unnil {T?} (pf2_arr)
    prval pf_res = pf1_arr
  in
    #[.. | (VOR_l (pf_res) | 0)]
  end // end of [if]
) (* end of [aux] *)
//
prval pf_arr = view@(mat)
prval pf_mat = array_v_group {a?} (pf_arr)
//
prval pf1_arr = array_v_nil {T} ()
prval pf2_arr = pf_mat
val p_arr = addr@(mat)
prval [l:addr] EQADDR () = eqaddr_make_ptr (p_arr)
prval () = lemma_g1uint_param (m)
val (pf_vor | r) = aux {0,m} {l} (pf1_arr, pf2_arr |  p_arr, (i2sz)0, m)
//
in
//
if r = 0 then let
  prval VOR_l pf_arr = pf_vor
  prval () = view@(mat) := array_v_ungroup {a} (pf_arr)
  prval () = opt_some {@[a][m*n]} (mat)
in
  true
end else let
  prval VOR_r pf_arr = pf_vor
  prval () = view@(mat) := array_v_ungroup {a?} (pf_arr)
  prval () = opt_none {@[a][m*n]} (mat)
in
  false
end // end of [if]
//
end // end of [file_read_matrix_td]
//
implement{a}
file_read_matrix_bu {m,n} (fp, mat, m, n) = let
//
typedef T = @[a][n]
prval () = lemma_g1uint_param (n)
//
fun
aux {i,m:nat | i <= m} {l:addr} (
  pf1_arr: array_v (T?, l, i), pf2_arr: array_v (T, l+i*sizeof(T), m-i)
| p_arr: ptr (l+i*sizeof(T)), i: size_t i, m: size_t m
) : [r:int] (vor (array_v (T, l, m), array_v (T?, l, m), r) | int r) = (
  if i > 0 then let
    prval (pf1_arr, pf_at) = array_v_unextend {T?} (pf1_arr)
    val p1_arr = ptr1_sub_guint<a> (p_arr, n)
    prval () = lemma_array_sizeof {a}{n} ()
    prval [l0:addr] EQADDR () = ptr_get_index (p1_arr)
    val p_at = p1_arr : ptr l0
    prval pf_at = pf_at : T? @ l0
    val res = file_read_matrix_row<a> (fp, !p_at, n)
  in
    if res then let
      prval () = opt_unsome {T} (!p_at)
      prval pf2_arr = array_v_cons {T}{l0} (pf_at, pf2_arr)
    in
      aux (pf1_arr, pf2_arr | p1_arr, pred(i), m)
    end else let
      prval p_bas = praxi_ptr {l} ()
      prval () = opt_unnone {T} (!p_at)
      prval pf1_arr = array_v_extend {T?} (pf1_arr, pf_at)
      prval pf_res = array_v_unsplit {T?} (pf1_arr, pf2_arr)
    in
      #[.. | (VOR_r (pf_res) | 1)]
    end
  end else let
    prval () = array_v_unnil {T?} (pf1_arr)
    prval pf_res = pf2_arr
    prval MULbas () = mul_make {i,sizeof(T)} ()
  in
    (VOR_l (pf_res) | 0)
  end // end of [if]
) (* end of [aux] *)
//
val p_arr = addr@(mat)
prval [l0:addr] EQADDR () = eqaddr_make_ptr (p_arr)
prval () = lemma_g1uint_param (m)
prval () = lemma_g1uint_param (n)
val mn = m*n
prval pf_mul_mn = mul_make {m,n} ()
prval [mn:int] EQINT () = eqint_make_guint (mn)
prval () = mul_nat_nat_nat (pf_mul_mn)
prval () = mul_elim (pf_mul_mn)
val p_arr = ptr1_add_guint<a> (p_arr, mn)
prval () = lemma_array_sizeof {a?} {m} ()
prval () = lemma_array2_sizeof {a?} {m,n} ()
prval [l:addr] EQADDR () = eqaddr_make_ptr (p_arr)
prval () = __assert () where {
  extern
  prfun
  __assert (): [mn*sizeof(a?) == m*sizeof(@[a?][n])] void
} (* end of [prval] *)
//
prval pf_arr = view@(mat)
prval pf_mat = array_v_group {a?}{l0}{m,n} (pf_arr)
prval pf1_arr = pf_mat
prval pf2_arr = array_v_nil {T} ()
val (pf_vor | r) = aux (pf1_arr, pf2_arr |  p_arr, m, m)
//
in
//
if r = 0 then let
  prval VOR_l pf_arr = pf_vor
  prval () = view@(mat) := array_v_ungroup (pf_arr)
  prval () = opt_some {@[a][m*n]} (mat)
in
  true
end else let
  prval VOR_r pf_arr = pf_vor
  prval () = view@(mat) := array_v_ungroup (pf_arr)
  prval () = opt_none {@[a][m*n]} (mat)
in
  false
end // end of [if]
//
end // end of [file_read_matrix_bu]
//
