//
extern
fun{vt:vt@ype}
vec_init$fwork (natLt(NDIM), &(vt)): T
//
extern
fun{vt:vt@ype}
vec_init_env (
  vec: &vec? >> _, env: &(vt)
): void // end of [vec_init_env]
//
extern
fun{a:t@ype}{env:vt@ype}
vec_fold$fwork (&a >> a, natLt(NDIM), &(env) >> _): void
//
extern
fun{a:t@ype}{env:vt@ype}
vec_fold_env  (
  res: &a >> a
, env: &(env) >> _
): void // end of [vec_fold_env]
//
(* ****** ****** *)
//
extern
fun
add_vec_vec (&vec, &vec): vec
//
extern
fun
mul_T_vec (T, &vec): vec
//
extern
fun
neg_vec (&vec): vec
//
extern
fun
sub_vec_vec (&vec, &vec): vec
//
extern
fun
length_sq_vec (&vec): T
//
extern
fun
length_vec (&vec): T
//
extern
fun
normalize_vec (&vec): vec
//
extern
fun
dotprod_vec_vec (&vec, &vec): T
//
extern
fun
lerp_vec_vec_T (&vec, &vec, T): vec
//
(*
extern
fun{}
equal_vec$eps (): T
*)
extern
fun
equal_vec_vec (&vec, &vec): bool
//
extern
fun
fprint_vec (FILEref, &vec): void
//
extern
fun
print_vec (&vec): void
//
(* ****** ****** *)
//
implement{vt}
vec_init_env (v, env) = {
//
fun
aux (res: &(@[T?][NDIM]) >> @[T][NDIM], env: &vt): void = {
//
var i: int = 0
prval [lres:addr] EQADDR () = eqaddr_make_ptr (addr@(res))
var p = addr@(res)
prvar pf0 = array_v_nil {T} ()
prvar pf1 = view@(res)
//
val () =
while* {i:nat | i <= NDIM} .<NDIM-i>. (
  i: int (i)
, p: ptr (lres + i*sizeof(T))
, pf0: array_v (T, lres, i)
, pf1: array_v (T?, lres+i*sizeof(T), NDIM-i)
) : (
  pf0: array_v (T, lres, NDIM)
, pf1: array_v (T?, lres+i*sizeof(T), 0)
) => (
  i < NDIM
) {
//
  prval (pf_at, pf1_res) = array_v_uncons {T?} (pf1)
  prval () = pf1 := pf1_res
  val e = vec_init$fwork<vt> (i, env)
  val () = ptr_set<T> (pf_at | p, e)
  val () = p := ptr1_succ<T> (p)
  prval () = pf0 := array_v_extend {T} (pf0, pf_at)
  val () = i := i + 1
//
} // end of [val]
//
prval () = view@(res) := pf0
prval () = array_v_unnil {T?} (pf1)
//
} (* end of [aux] *)
//
val () = aux (v.V, env)
//
} (* end of [vec_init_env] *)
//
implement{a}{vt}
vec_fold_env (x, env) = {
//
var i: int = 0
//
val () =
while* {i:nat | i <= NDIM} .<NDIM-i>. (
  i: int (i)
) => (
  i < NDIM
) {
  val () = vec_fold$fwork<a><vt> (x, i, env)
  val () = i := i + 1
} // end of [val]
//
} (* end of [vec_fold_env] *)

(* ****** ****** *)

implement
add_vec_vec (x, y) = let
  var res: vec
  prval [lx:addr] EQADDR () = eqaddr_make_ptr (addr@(x))
  prval [ly:addr] EQADDR () = eqaddr_make_ptr (addr@(y))
  //
  absvt@ype VT = void
  viewdef V = (vec @ lx, vec @ ly)
  prval (dec, enc) = make_iso_v_vt {V} {VT} ()
  //
  implement
  vec_init$fwork<VT> (i, env) = let
    prval pf = dec (env)
    val res = gadd_val_val<T> (x.V.[i], y.V.[i])
    prval () = enc (env, pf)
  in
    res
  end
  //
  var env = ()
  prval () = enc (env, (view@x, view@y))
  //
  val () = vec_init_env<VT> (res, env)
  //
  prval (pf0, pf1) = dec (env)
  prval () = view@x := pf0
  and () = view@y := pf1
  //
in
  res
end // end of [add_vec_vec]
//
implement
mul_T_vec (x, y) = let
  var res: vec
  vtypedef VT = vec
  //
  implement
  vec_init$fwork<VT> (i, env) =
    gmul_val_val<T> (x, env.V.[i])
  //
  val () = vec_init_env<VT> (res, y)
in
  res
end // end of [mul_T_vec]
//
implement
neg_vec (x) = let
  var res: vec
  vtypedef VT = vec
  //
  implement
  vec_init$fwork<VT> (i, env) =
    gneg_val<T> (env.V.[i])
  //
  val () = vec_init_env<VT> (res, x)
in
  res
end // end of [neg_vec]
//
implement
sub_vec_vec (x, y) = let
  var res: vec
  prval [lx:addr] EQADDR () = eqaddr_make_ptr (addr@(x))
  prval [ly:addr] EQADDR () = eqaddr_make_ptr (addr@(y))
  //
  absvt@ype VT = void
  viewdef V = (vec @ lx, vec @ ly)
  prval (dec, enc) = make_iso_v_vt {V} {VT} ()
  //
  implement
  vec_init$fwork<VT> (i, env) = let
    prval pf = dec (env)
    val res = gsub_val_val<T> (x.V.[i], y.V.[i])
    prval () = enc (env, pf)
  in
    res
  end
  //
  var env = ()
  prval () = enc (env, (view@x, view@y))
  //
  val () = vec_init_env<VT> (res, env)
  //
  prval (pf0, pf1) = dec (env)
  prval () = view@x := pf0
  and () = view@y := pf1
  //
in
  res
end // end of [sub_vec_vec]
//
implement
length_sq_vec (x) = let
  //
  vtypedef VT = vec
  //
  implement
  vec_fold$fwork<T><VT> (e, i, env) = let
    val v = env.V.[i] in
    e := gadd_val_val<T> (e, gmul_val_val<T> (v, v))
  end // end of [vec_fold$fwork]
  //
  var res: T = gnumber_int<T>(0)
  val () = vec_fold_env<T><VT> (res, x)
  //
in
  res
end // end of [length_sq_vec]
//
implement
length_vec (x) = let
  val len_sq = length_sq_vec (x)
in
  sqrt<T> (len_sq)
end // end of [length_vec]
//
implement
normalize_vec (x) = let
  val len = length_vec (x)
  val inv_len = gdiv_val_val<T> (gnumber_int<T> (1), len)
in
  mul_T_vec (inv_len, x)
end
//
implement
dotprod_vec_vec (x, y) = let
  //
  prval [lx:addr] EQADDR () = eqaddr_make_ptr (addr@(x))
  prval [ly:addr] EQADDR () = eqaddr_make_ptr (addr@(y))
  //
  absvt@ype VT = void
  viewdef V = (vec @ lx, vec @ ly)
  prval (dec, enc) = make_iso_v_vt {V} {VT} ()
  //
  implement
  vec_fold$fwork<T><VT> (e, i, env) = {
    prval pf = dec (env)
    val () = e := gadd_val_val<T> (e, gmul_val_val<T> (x.V.[i], y.V.[i]))
    prval () = enc (env, pf)
  }
  //
  var res: T = gnumber_int<T> (0)
  var env = ()
  prval () = enc (env, (view@x, view@y))
  //
  val () = vec_fold_env<T><VT> (res, env)
  //
  prval (pf0, pf1) = dec (env)
  prval () = view@x := pf0
  and () = view@y := pf1
  //
in
  res
end // end of [dotprod_vec_vec]
//
implement
lerp_vec_vec_T (x, y, s) = let
  var res: vec
  prval [lx:addr] EQADDR () = eqaddr_make_ptr (addr@(x))
  prval [ly:addr] EQADDR () = eqaddr_make_ptr (addr@(y))
  //
  absvt@ype VT = void
  viewdef V = (vec @ lx, vec @ ly)
  prval (dec, enc) = make_iso_v_vt {V} {VT} ()
  //
  val inv_s = gsub_val_val<T> (gnumber_int<T>(1), s)
  //
  implement
  vec_init$fwork<VT> (i, env) = let
    prval pf = dec (env)
    val res = gadd_val_val<T> (gmul_val_val<T> (inv_s, x.V.[i]), gmul_val_val<T> (s, y.V.[i]))
    prval () = enc (env, pf)
  in
    res
  end
  //
  var env = ()
  prval () = enc (env, (view@x, view@y))
  //
  val () = vec_init_env<VT> (res, env)
  //
  prval (pf0, pf1) = dec (env)
  prval () = view@x := pf0
  and () = view@y := pf1
  //
in
  res
end // end of [lerp_vec_vec_T]
//
implement
equal_vec_vec (x, y) = let
//
val eps = equal_vec$eps<> ()
//
implement
array_foreach2$cont<T,T><bool> (x0, y0, res) = let
  val d = gabs_val<T> (gsub_val_val<T> (x0, y0))
  val sgn0 = gcompare_val_val<T> (d, eps)
in
  if sgn0 < 0 then true
  else (res := false; false)
end // end of [array_foreach2$cont]
//
implement
array_foreach2$fwork<T,T><bool> (x0, y0, sgn) = ()
//
var res: bool = true
val _ = array_foreach2_env<T,T><bool> (x.V, y.V, (i2sz)NDIM, res)
//
in
//
res
//
end // end of [equal_vec_vec]

implement
fprint_vec (out, v) = {
//
val () = fprint (out, "(")
val () = fprint_array_int<T> (out, v.V, NDIM)
val () = fprint (out, ")")
//
} (* end of [fprint_vec] *)

implement
print_vec (v) = fprint_vec (stdout_ref, v)

(* ****** ****** *)
