//
extern
fun{vt:vt@ype}
mat_init$fwork (natLt(NROW), natLt(NCOL), &(vt)): T
//
extern
fun{vt:vt@ype}
mat_init_env (
  mat: &mat? >> _, env: &(vt)
): void // end of [mat_init_env]
//
(* ****** ****** *)
//
implement{vt}
mat_init_env (m, env) = {
//
fun aux_col {lres:addr} (
  pf_res: !COL_T? @ lres >> COL_T @ lres
| pres: ptr lres, i: natLt(NCOL), env: &(vt)
): void = loop (pres->V, i, env) where {
//
fun loop (res: &(@[T?][NROW]) >> @[T][NROW], i: natLt(NCOL), env: &(vt)): void = {
//
var j: int = 0
prval [lres:addr] EQADDR () = eqaddr_make_ptr (addr@(res))
var p = addr@(res)
prvar pf0 = array_v_nil {T} ()
prvar pf1 = view@(res)
//
val () =
while* {j:nat | j <= NROW} .<NROW-j>. (
  j: int (j)
, p: ptr (lres + j*sizeof(T))
, pf0: array_v (T, lres, j)
, pf1: array_v (T?, lres+j*sizeof(T), NROW-j)
) : (
  pf0: array_v (T, lres, NROW)
, pf1: array_v (T?, lres+j*sizeof(T), 0)
) => (
  j < NROW
) {
//
  prval (pf_at, pf1_res) = array_v_uncons {T?} (pf1)
  prval () = pf1 := pf1_res
  //
  val () = ptr_set<T> (pf_at | p, mat_init$fwork<vt> (j, i, env))
  val () = p := ptr1_succ<T> (p)
  //
  prval () = pf0 := array_v_extend {T} (pf0, pf_at)
  val () = j := j + 1
//
} // end of [val]
//
prval () = view@(res) := pf0
prval () = array_v_unnil {T?} (pf1)
//
} (* end of [loop] *)
//
} (* end of [aux_col] *)
//
fun
aux (res: &(@[COL_T?][NCOL]) >> @[COL_T][NCOL], env: &vt): void = {
//
var i: int = 0
prval [lres:addr] EQADDR () = eqaddr_make_ptr (addr@(res))
var p = addr@(res)
prvar pf0 = array_v_nil {COL_T} ()
prvar pf1 = view@(res)
//
val () =
while* {i:nat | i <= NCOL} .<NCOL-i>. (
  i: int (i)
, p: ptr (lres + i*sizeof(COL_T))
, pf0: array_v (COL_T, lres, i)
, pf1: array_v (COL_T?, lres+i*sizeof(COL_T), NCOL-i)
) : (
  pf0: array_v (COL_T, lres, NCOL)
, pf1: array_v (COL_T?, lres+i*sizeof(COL_T), 0)
) => (
  i < NCOL
) {
//
  prval (pf_at, pf1_res) = array_v_uncons {COL_T?} (pf1)
  prval () = pf1 := pf1_res
  //
  val () = aux_col (pf_at | p, i, env)
  val () = p := ptr1_succ<COL_T> (p)
  //
  prval () = pf0 := array_v_extend {COL_T} (pf0, pf_at)
  val () = i := i + 1
//
} // end of [val]
//
prval () = view@(res) := pf0
prval () = array_v_unnil {COL_T?} (pf1)
//
} (* end of [aux] *)
//
val () = aux (m.M, env)
//
} (* end of [mat_init_env] *)

(* ****** ****** *)

extern
fun
mat_init1 (m: &mat? >> _, x: T): void
//
extern
fun
mat_init_arrptr {n:nat | n >= NROW*NCOL} (
  &mat? >> _,
  &(@[float][n]) >> _
): void
//
extern
fun
add_mat_mat (&mat, &mat): mat
//
extern
fun
add_T_mat (T, &mat): mat
//
extern
fun
mul_T_mat (T, &mat): mat
//
extern
fun
mul_mat_mat (&mat, &mat): mat
//
extern
fun
sub_mat_mat (&mat, &mat): mat
//
extern
fun
neg_mat (&mat): mat
//
(* ****** ****** *)
//
#if NROW = NCOL #then
//
extern
fun
transpose_mat (&mat): mat
//
extern
fun
identity_mat (&mat? >> _): void
//
#endif // NROW = NCOL
//
(* ****** ****** *)
//
extern
fun
mul_mat_vec (&mat, &COL_T): vec
//
extern
fun
fprint_mat (FILEref, &mat): void
//
extern
fun
print_mat (&mat): void
//
(* ****** ****** *)

implement
mat_init1 (m, x) = {
//
implement
mat_init$fwork<void> (c, r, env) = x
//
var env = ()
val () = mat_init_env<void> (m, env)
//
} (* end of [mat_init1] *)
//
implement
mat_init_arrptr {n} (m, arr) = {
//
prval [larr:addr] EQADDR () = eqaddr_make_ptr (addr@(arr))
//
absvt@ype VT = void
viewdef V = (@[T][n] @ larr)
prval (dec, enc) = make_iso_v_vt {V} {VT} ()
//
implement
mat_init$fwork<VT> (i, j, env) = res where {
  prval pf = dec (env)
  val ix = i*NCOL + j
  val res = arr.[ix]
  prval () = enc (env, pf)
}
//
var env = ()
prval () = enc (env, view@arr)
//
val () = mat_init_env<VT> (m, env)
//
prval () = view@arr := dec (env)
//
} (* end of [mat_init_arrptr] *)
//
implement
add_mat_mat (x, y) = let
//
prval [lx:addr] EQADDR () = eqaddr_make_ptr (addr@(x))
prval [ly:addr] EQADDR () = eqaddr_make_ptr (addr@(y))
//
absvt@ype VT = void
viewdef V = (mat @ lx, mat @ ly)
prval (dec, enc) = make_iso_v_vt {V} {VT} ()
//
implement
mat_init$fwork<VT> (i, j, env) = res where {
  prval pf = dec (env)
  val res = gadd_val_val<T> (x.M.[i].V.[j], y.M.[i].V.[j])
  prval () = enc (env, pf)
}
//
var env = ()
prval () = enc (env, (view@x, view@y))
//
var res: mat
val () = mat_init_env<VT> (res, env)
//
prval (pf0, pf1) = dec (env)
prval () = view@x := pf0
and () = view@y := pf1
//
in
  res
end // end of [add_mat_mat]
//
implement
add_T_mat (s, x) = let
//
prval [lx:addr] EQADDR () = eqaddr_make_ptr (addr@(x))
//
absvt@ype VT = void
viewdef V = (mat @ lx)
prval (dec, enc) = make_iso_v_vt {V} {VT} ()
//
implement
mat_init$fwork<VT> (i, j, env) = res where {
  prval pf = dec (env)
  val res = gadd_val_val<T> (s, x.M.[i].V.[j])
  prval () = enc (env, pf)
}
//
var env = ()
prval () = enc (env, (view@x))
//
var res: mat
val () = mat_init_env<VT> (res, env)
//
prval pf0 = dec (env)
prval () = view@x := pf0
//
in
  res
end // end of [add_T_mat]
//
implement
mul_T_mat (s, x) = let
//
prval [lx:addr] EQADDR () = eqaddr_make_ptr (addr@(x))
//
absvt@ype VT = void
viewdef V = (mat @ lx)
prval (dec, enc) = make_iso_v_vt {V} {VT} ()
//
implement
mat_init$fwork<VT> (i, j, env) = res where {
  prval pf = dec (env)
  val res = gmul_val_val<T> (s, x.M.[i].V.[j])
  prval () = enc (env, pf)
}
//
var env = ()
prval () = enc (env, (view@x))
//
var res: mat
val () = mat_init_env<VT> (res, env)
//
prval pf0 = dec (env)
prval () = view@x := pf0
//
in
  res
end // end of [mul_T_mat]
//
implement
mul_mat_mat (x, y) = let
//
prval [lx:addr] EQADDR () = eqaddr_make_ptr (addr@(x))
prval [ly:addr] EQADDR () = eqaddr_make_ptr (addr@(y))
//
absvt@ype VT = void
viewdef V = (mat @ lx, mat @ ly)
prval (dec, enc) = make_iso_v_vt {V} {VT} ()
//
implement
mat_init$fwork<VT> (i, j, env) = res where {
  var row: COL_T
  //
  implement
  vec_init$fwork<VT>(k, env) = res where {
    prval pf = dec (env)
    val res = x.M.[k].V.[i]
    prval () = enc (env, pf)
  }
  //
  val () = vec_init_env<VT> (row, env)
  //
  prval pf = dec (env)
  val res = dotprod_vec_vec (y.M.[j], row)
  prval () = enc (env, pf)
}
//
var env = ()
prval () = enc (env, (view@x, view@y))
//
var res: mat
val () = mat_init_env<VT> (res, env)
//
prval (pf0, pf1) = dec (env)
prval () = view@x := pf0
and () = view@y := pf1
//
in
  res
end // end of [mul_mat_mat]
//
implement
sub_mat_mat (x, y) = let
//
prval [lx:addr] EQADDR () = eqaddr_make_ptr (addr@(x))
prval [ly:addr] EQADDR () = eqaddr_make_ptr (addr@(y))
//
absvt@ype VT = void
viewdef V = (mat @ lx, mat @ ly)
prval (dec, enc) = make_iso_v_vt {V} {VT} ()
//
implement
mat_init$fwork<VT> (i, j, env) = res where {
  prval pf = dec (env)
  val res = gsub_val_val<T> (x.M.[i].V.[j], y.M.[i].V.[j])
  prval () = enc (env, pf)
}
//
var env = ()
prval () = enc (env, (view@x, view@y))
//
var res: mat
val () = mat_init_env<VT> (res, env)
//
prval (pf0, pf1) = dec (env)
prval () = view@x := pf0
and () = view@y := pf1
//
in
  res
end // end of [sub_mat_mat]
//
implement
neg_mat (x) = let
//
prval [lx:addr] EQADDR () = eqaddr_make_ptr (addr@(x))
//
absvt@ype VT = void
viewdef V = (mat @ lx)
prval (dec, enc) = make_iso_v_vt {V} {VT} ()
//
implement
mat_init$fwork<VT> (i, j, env) = res where {
  prval pf = dec (env)
  val res = gneg_val<T> (x.M.[i].V.[j])
  prval () = enc (env, pf)
}
//
var env = ()
prval () = enc (env, (view@x))
//
var res: mat
val () = mat_init_env<VT> (res, env)
//
prval pf0 = dec (env)
prval () = view@x := pf0
//
in
  res
end // end of [neg_mat]
//
(* ****** ****** *)
//
#if NROW = NCOL #then
//
implement
transpose_mat (m) = let
//
var res: mat
//
implement
mat_init$fwork<mat> (i, j, env) = env.M.[i].V.[j]
//
val () = mat_init_env<mat> (res, m)
//
in
  res
end // end of [transpose_mat]
//
implement
identity_mat (res) = {
//
implement
mat_init$fwork<void> (c, r, env) =
  gnumber_int<T> (bool2int(c = r))
//
var env = ()
val () = mat_init_env<void> (res, env)  
//
} (* end of [identity_mat] *)
//
#endif // NROW = NCOL
//
(* ****** ****** *)
//
implement
mul_mat_vec (m, v) = let
//
prval [lv:addr] EQADDR () = eqaddr_make_ptr (addr@(v))
prval [lm:addr] EQADDR () = eqaddr_make_ptr (addr@(m))
//
absvt@ype VT = void
viewdef V = (COL_T @ lv, mat @ lm)
prval (dec, enc) = make_iso_v_vt {V} {VT} ()
//
implement
vec_init$fwork<VT> (i, env) = res where {
  prval pf = dec (env)
  var j: int = 0
  var res: T = gnumber_int<T>(0)
  val () =
    while* {j:nat | j < NROW} .<NROW-j>. (j: int j) =>
      (j < NROW) {
      val () = res := gadd_val_val<T> (res, gmul_val_val<T> (m.M.[j].V.[i], v.V.[j]))
      val () = j := j + 1
    } // end of [val]
  prval () = enc (env, pf)
}
//
var env = ()
prval () = enc (env, (view@v, view@m))
//
var res: COL_T
val () = vec_init_env<VT> (res, env)
//
prval (pf0, pf1) = dec (env)
prval () = view@v := pf0
and () = view@m := pf1
//
in
  res
end // end of [mul_mat_vec]
//
implement
fprint_mat (out, m) = {
//
val () = fprint (out, "(")
//
implement
fprint_ref<COL_T> (out, c) =
  fprint_vec (out, c)
//
val () = fprint_array_int<COL_T> (out, m.M, NCOL)
//
val () = fprint (out, ")")
//
} (* end of [fprint_mat] *)
//
implement
print_mat (mat) = fprint_mat (stdout_ref, mat)
//
