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

staload "libats/libc/SATS/math.sats" // NOTE: requires -lm during compilation (for length_vec)
staload _ = "libats/libc/DATS/math.dats"

(* ****** ****** *)
//
typedef T = float
#define NDIM 3
typedef vec = vec3f
//
implement{}
equal_vec3f$eps () = 0.001f
fun{}
equal_vec$eps (): T = equal_vec3f$eps<> ()
//
(* ****** ****** *)

#include "./SHARE/vector.hats"

(* ****** ****** *)
//
implement
vec3f_init3 (v, x, y, z) = {
//
fun
aux (v: &(@[INV(T)][NDIM])? >> _, x: T, y: T, z: T): void = {
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
#define :: array_v_cons
//
prval pf3_nil = array_v_unnil_nil (pf3_arr)
prval () = view@(v) := pf1_at :: pf2_at :: pf3_at :: pf3_nil
//
} (* end of [aux] *)
//
val () = aux (v.V, x, y, z)
//
} (* end of [vec3f_init3] *)

implement
vec3f_init1 (v, x) = vec3f_init3 (v, x, x, x)

implement
crossprod_vec3f_vec3f (v1, v2) = res where {
//
var res: vec
val vx = gsub_val_val<T> (gmul_val_val<T> (v1.V.[1], v2.V.[2]), gmul_val_val<T> (v1.V.[2], v2.V.[1]))
val vy = gsub_val_val<T> (gmul_val_val<T> (v1.V.[2], v2.V.[0]), gmul_val_val<T> (v1.V.[0], v2.V.[2]))
val vz = gsub_val_val<T> (gmul_val_val<T> (v1.V.[0], v2.V.[1]), gmul_val_val<T> (v1.V.[1], v2.V.[0]))
val () = res.init (vx, vy, vz)
//
} // end of [crossprod_vec3f_vec3f]

(* ****** ****** *)

implement{env}
vec_init$fwork (n, env) = vec3f_init$fwork<env> (n, env)
implement{env}
vec3f_init_env (v, env) = vec_init_env<env> (v, env)
//
implement{a}{env}
vec_fold$fwork (x, i, env) = vec3f_fold$fwork<a><env> (x, i, env)
implement{a}{env}
vec3f_fold_env (v, env) = vec_fold_env<a><env> (v, env)

(* ****** ****** *)

implement
add_vec3f_vec3f (x, y) = add_vec_vec (x, y)
implement
mul_float_vec3f (x, y) = mul_T_vec (x, y)
implement
neg_vec3f (x) = neg_vec (x)
implement
sub_vec3f_vec3f (x, y) = sub_vec_vec (x, y)
implement
length_sq_vec3f (x) = length_sq_vec (x)
implement
length_vec3f (x) = length_vec (x)
implement
normalize_vec3f (x) = normalize_vec (x)
implement
dotprod_vec3f_vec3f (x, y) = dotprod_vec_vec (x, y)
implement
lerp_vec3f_vec3f_float (x, y, s) = lerp_vec_vec_T (x, y, s)
implement
equal_vec3f_vec3f (x, y) = equal_vec_vec (x, y)
implement
fprint_vec3f (x, y) = fprint_vec (x, y)
implement
print_vec3f (x) = print_vec (x)

(* ****** ****** *)

implement
vec3f_get_int (v, i) = v.V.[i]
implement
vec3f_set_int (v, i, x) = v.V.[i] := x
//
implement
vec3f_get_x (v) = v.V.[0]
implement
vec3f_get_y (v) = v.V.[1]
implement
vec3f_get_z (v) = v.V.[2]
//
implement
vec3f_set_x (v, x) = v.V.[0] := x
implement
vec3f_set_y (v, y) = v.V.[1] := y
implement
vec3f_set_z (v, z) = v.V.[2] := z

(* ****** ****** *)
