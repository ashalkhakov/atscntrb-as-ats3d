//
#define ATS_DYNLOADFLAG 0
//
#include
"share/atspre_staload.hats"
//
staload "./../SATS/vector.sats"

staload _ = "prelude/DATS/gnumber.dats"
staload _ = "prelude/DATS/SHARE/gnumber_float.dats"
staload _ = "prelude/DATS/gorder.dats"

staload "libats/libc/SATS/math.sats" // NOTE: requires -lm during compilation (for length_vector)
staload _ = "libats/libc/DATS/math.dats"

(* ****** ****** *)
//
typedef T = float
#define NDIM 2
typedef vec = vec2f

implement{}
equal_vec2f$eps () = 0.001f
fun{}
equal_vec$eps (): T = equal_vec2f$eps<> ()
//
(* ****** ****** *)

#include "./SHARE/vector.hats"

(* ****** ****** *)
//
implement
vec2f_init2 (v, x, y) = {
//
fun
aux (v: &(@[INV(T)][NDIM])? >> _, x: T, y: T): void = {
//
prval pf_arr = view@(v)
var pv = addr@(v)
//
prval (pf1_at, pf1_arr) = array_v_uncons {T?} (pf_arr)
val () = ptr_set<T> (pf1_at | pv, x)
//
val () = pv := ptr1_succ<T> (pv)
prval (pf2_at, pf2_arr) = array_v_uncons {T?} (pf1_arr)
val () = ptr_set<T> (pf2_at | pv, y)
//
#define :: array_v_cons
//
prval pf2_nil = array_v_unnil_nil (pf2_arr)
prval () = view@(v) := pf1_at :: pf2_at :: pf2_nil
//
} (* end of [aux] *)
//
val () = aux (v.V, x, y)
//
} (* end of [vec2f_init2] *)

implement
vec2f_init1 (v, x) = vec2f_init2 (v, x, x)

(* ****** ****** *)

implement{env}
vec_init$fwork (n, env) = vec2f_init$fwork<env> (n, env)
implement{env}
vec2f_init_env (v, env) = vec_init_env<env> (v, env)
//
implement{a}{env}
vec_fold$fwork (x, i, env) = vec2f_fold$fwork<a><env> (x, i, env)
implement{a}{env}
vec2f_fold_env (v, env) = vec_fold_env<a><env> (v, env)

(* ****** ****** *)

implement
add_vec2f_vec2f (x, y) = add_vec_vec (x, y)
implement
mul_float_vec2f (x, y) = mul_T_vec (x, y)
implement
neg_vec2f (x) = neg_vec (x)
implement
sub_vec2f_vec2f (x, y) = sub_vec_vec (x, y)
implement
length_sq_vec2f (x) = length_sq_vec (x)
implement
length_vec2f (x) = length_vec (x)
implement
normalize_vec2f (x) = normalize_vec (x)
implement
dotprod_vec2f_vec2f (x, y) = dotprod_vec_vec (x, y)
implement
lerp_vec2f_vec2f_float (x, y, s) = lerp_vec_vec_T (x, y, s)
implement
equal_vec2f_vec2f (x, y) = equal_vec_vec (x, y)
implement
fprint_vec2f (x, y) = fprint_vec (x, y)
implement
print_vec2f (x) = print_vec (x)

(* ****** ****** *)

implement
vec2f_get_int (v, i) = v.V.[i]
implement
vec2f_set_int (v, i, x) = v.V.[i] := x
//
implement
vec2f_get_x (v) = v.V.[0]
implement
vec2f_get_y (v) = v.V.[1]
//
implement
vec2f_set_x (v, x) = v.V.[0] := x
implement
vec2f_set_y (v, y) = v.V.[1] := y

(* ****** ****** *)
