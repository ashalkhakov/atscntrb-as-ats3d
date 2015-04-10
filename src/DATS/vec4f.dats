//
#define ATS_DYNLOADFLAG 0
//
#include
"share/atspre_staload.hats"
//
staload "../SATS/vector.sats"

staload _ = "prelude/DATS/gnumber.dats"
staload _ = "prelude/DATS/SHARE/gnumber_float.dats"
staload _ = "prelude/DATS/gorder.dats"

staload "libc/SATS/math.sats" // NOTE: requires -lm during compilation (for length_vec)
staload _ = "libc/DATS/math.dats"

(* ****** ****** *)
//
typedef T = float
#define NDIM 4
typedef vec = vec4f

implement{}
equal_vec4f$eps () = 0.001f
fun{}
equal_vec$eps (): T = equal_vec4f$eps<> ()

(* ****** ****** *)

#include "./SHARE/vector.hats"

(* ****** ****** *)
//
implement
vec4f_init4 (v, x, y, z, w) = {
//
fun
aux (v: &(@[INV(T)][NDIM])? >> _, x: T, y: T, z: T, w: T): void = {
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
val () = pv := ptr1_succ<T> (pv)
prval (pf3_at, pf3_arr) = array_v_uncons {T?} (pf2_arr)
val () = ptr_set<T> (pf3_at | pv, z)
//
val () = pv := ptr1_succ<T> (pv)
prval (pf4_at, pf4_arr) = array_v_uncons {T?} (pf3_arr)
val () = ptr_set<T> (pf4_at | pv, w)
//
#define :: array_v_cons
//
prval pf4_nil = array_v_unnil_nil (pf4_arr)
prval () = view@(v) := pf1_at :: pf2_at :: pf3_at :: pf4_at :: pf4_nil
//
} (* end of [aux] *)
//
val () = aux (v.V, x, y, z, w)
//
} (* end of [vec4f_init4] *)

implement
vec4f_init1 (v, x) = vec4f_init4 (v, x, x, x, x)

(* ****** ****** *)

implement{env}
vec_init$fwork (n, env) = vec4f_init$fwork<env> (n, env)
implement{env}
vec4f_init_env (v, env) = vec_init_env<env> (v, env)
//
implement{a}{env}
vec_fold$fwork (x, i, env) = vec4f_fold$fwork<a><env> (x, i, env)
implement{a}{env}
vec4f_fold_env (v, env) = vec_fold_env<a><env> (v, env)

(* ****** ****** *)

implement
add_vec4f_vec4f (x, y) = add_vec_vec (x, y)
implement
mul_float_vec4f (x, y) = mul_T_vec (x, y)
implement
neg_vec4f (x) = neg_vec (x)
implement
sub_vec4f_vec4f (x, y) = sub_vec_vec (x, y)
implement
length_sq_vec4f (x) = length_sq_vec (x)
implement
length_vec4f (x) = length_vec (x)
implement
normalize_vec4f (x) = normalize_vec (x)
implement
dotprod_vec4f_vec4f (x, y) = dotprod_vec_vec (x, y)
implement
lerp_vec4f_vec4f_float (x, y, s) = lerp_vec_vec_T (x, y, s)
implement
equal_vec4f_vec4f (x, y) = equal_vec_vec (x, y)
implement
fprint_vec4f (x, y) = fprint_vec (x, y)
implement
print_vec4f (x) = print_vec (x)

(* ****** ****** *)

implement
vec4f_get_int (v, i) = v.V.[i]
implement
vec4f_set_int (v, i, x) = v.V.[i] := x
//
implement
vec4f_get_x (v) = v.V.[0]
implement
vec4f_get_y (v) = v.V.[1]
implement
vec4f_get_z (v) = v.V.[2]
implement
vec4f_get_w (v) = v.V.[3]
//
implement
vec4f_set_x (v, x) = v.V.[0] := x
implement
vec4f_set_y (v, y) = v.V.[1] := y
implement
vec4f_set_z (v, z) = v.V.[2] := z
implement
vec4f_set_w (v, w) = v.V.[3] := w

(* ****** ****** *)
