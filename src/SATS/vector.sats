(* ****** ****** *)

%{#

#include "../CATS/vector.cats"

%}

(* ****** ****** *)

//
viewdef
iso_v_vt (V:view, VT:vt@ype) =
  ((&VT >> void) -<prf> V, (&void >> VT, V) -<prf> void)
// end of [iso_v_vt]
prfun
make_iso_v_vt {V:view} {VT:vt@ype} (): iso_v_vt (V, VT)
//

(* ****** ****** *)

typedef
vector_2_float_t0ype =
$extype_struct "vec2f_t" of {
  V= @[float][2]
}

stadef vec2f = vector_2_float_t0ype // local shorthand
//
fun{vt:vt@ype}
vec2f_init$fwork (natLt(2), &(vt)): float
//
fun{a:t@ype}{env:vt@ype}
vec2f_fold$fwork (&a >> a, natLt(2), &(env) >> _): void
//
fun{vt:vt@ype}
vec2f_init_env (
  vec: &vec2f? >> _, env: &(vt)
): void // end of [vec2f_init_env]
//
fun{a:t@ype}{env:vt@ype}
vec2f_fold_env  (
  res: &a >> a
, env: &(env) >> _
): void // end of [vec2f_fold_env]
//
fun add_vec2f_vec2f (&vec2f, &vec2f): vec2f
fun mul_float_vec2f (float, &vec2f): vec2f
fun neg_vec2f (&vec2f): vec2f
fun sub_vec2f_vec2f (&vec2f, &vec2f): vec2f
fun length_sq_vec2f (&vec2f): float
fun length_vec2f (&vec2f): float
fun normalize_vec2f (&vec2f): vec2f
fun dotprod_vec2f_vec2f (&vec2f, &vec2f): float
fun lerp_vec2f_vec2f_float (&vec2f, &vec2f, float): vec2f
fun{}
equal_vec2f$eps (): float
fun
equal_vec2f_vec2f (&vec2f, &vec2f): bool
fun
fprint_vec2f (FILEref, &vec2f): void
fun
print_vec2f (&vec2f): void

//
overload + with add_vec2f_vec2f
overload * with mul_float_vec2f
overload ~ with neg_vec2f
overload - with sub_vec2f_vec2f
overload .length_sq with length_sq_vec2f
overload .length with length_vec2f
overload .normalize with normalize_vec2f
overload dotprod with dotprod_vec2f_vec2f
overload lerp with lerp_vec2f_vec2f_float
overload = with equal_vec2f_vec2f
overload fprint with fprint_vec2f
overload print with print_vec2f
//

fun
vec2f_init1 (&vec2f? >> _, float): void
overload .init with vec2f_init1

fun
vec2f_init2 (&vec2f? >> _, float, float): void
overload .init with vec2f_init2

fun
vec2f_get_int (&vec2f, natLt(2)): float
fun
vec2f_set_int (&vec2f, natLt(2), float): void

overload [] with vec2f_get_int of 10
overload [] with vec2f_set_int of 10

fun
vec2f_get_x (&vec2f): float
fun
vec2f_get_y (&vec2f): float

overload .x with vec2f_get_x
overload .y with vec2f_get_y

fun
vec2f_set_x (&vec2f, float): void
fun
vec2f_set_y (&vec2f, float): void

overload .x with vec2f_set_x
overload .y with vec2f_set_y

(* ****** ****** *)

typedef
vector_3_float_t0ype =
$extype_struct "vec3f_t" of {
  V= @[float][3]
}

stadef vec3f = vector_3_float_t0ype // local shorthand
//
fun{vt:vt@ype}
vec3f_init$fwork (natLt(3), &(vt)): float
//
fun{a:t@ype}{env:vt@ype}
vec3f_fold$fwork (&a >> a, natLt(3), &(env) >> _): void
//
fun{vt:vt@ype}
vec3f_init_env (
  vec: &vec3f? >> _, env: &(vt)
): void // end of [vec3f_init_env]
//
fun{a:t@ype}{env:vt@ype}
vec3f_fold_env  (
  res: &a >> a
, env: &(env) >> _
): void // end of [vec3f_fold_env]
//
fun add_vec3f_vec3f (&vec3f, &vec3f): vec3f
fun mul_float_vec3f (float, &vec3f): vec3f
fun neg_vec3f (&vec3f): vec3f
fun sub_vec3f_vec3f (&vec3f, &vec3f): vec3f
fun length_sq_vec3f (&vec3f): float
fun length_vec3f (&vec3f): float
fun normalize_vec3f (&vec3f): vec3f
fun dotprod_vec3f_vec3f (&vec3f, &vec3f): float
fun lerp_vec3f_vec3f_float (&vec3f, &vec3f, float): vec3f
fun{}
equal_vec3f$eps (): float
fun
equal_vec3f_vec3f (&vec3f, &vec3f): bool
fun
fprint_vec3f (FILEref, &vec3f): void
fun
print_vec3f (&vec3f): void

//
overload + with add_vec3f_vec3f of 10
overload * with mul_float_vec3f of 10
overload ~ with neg_vec3f of 10
overload - with sub_vec3f_vec3f of 10
overload .length_sq with length_sq_vec3f of 10
overload .length with length_vec3f of 10
overload .normalize with normalize_vec3f of 10
overload dotprod with dotprod_vec3f_vec3f of 10
overload lerp with lerp_vec3f_vec3f_float of 10
overload = with equal_vec3f_vec3f of 10
overload fprint with fprint_vec3f of 10
overload print with print_vec3f of 10
//

fun
crossprod_vec3f_vec3f (&vec3f, &vec3f): vec3f

overload crossprod with crossprod_vec3f_vec3f

fun
vec3f_init1 (&vec3f? >> _, float): void
overload .init with vec3f_init1 of 10

fun
vec3f_init3 (&vec3f? >> _, float, float, float): void
overload .init with vec3f_init3 of 10

fun
vec3f_get_int (&vec3f, natLt(3)): float
fun
vec3f_set_int (&vec3f, natLt(3), float): void

overload [] with vec3f_get_int of 10
overload [] with vec3f_set_int of 10

fun
vec3f_get_x (&vec3f): float
fun
vec3f_get_y (&vec3f): float
fun
vec3f_get_z (&vec3f): float

overload .x with vec3f_get_x of 10
overload .y with vec3f_get_y of 10
overload .z with vec3f_get_z of 10

fun
vec3f_set_x (&vec3f, float): void
fun
vec3f_set_y (&vec3f, float): void
fun
vec3f_set_z (&vec3f, float): void

overload .x with vec3f_set_x of 10
overload .y with vec3f_set_y of 10
overload .z with vec3f_set_z of 10

(* ****** ****** *)

typedef
vector_4_float_t0ype =
$extype_struct "vec4f_t" of {
  V= @[float][4]
}

stadef vec4f = vector_4_float_t0ype // local shorthand
//
fun{vt:vt@ype}
vec4f_init$fwork (natLt(4), &(vt)): float
//
fun{a:t@ype}{env:vt@ype}
vec4f_fold$fwork (&a >> a, natLt(4), &(env) >> _): void
//
fun{vt:vt@ype}
vec4f_init_env (
  vec: &vec4f? >> _, env: &(vt)
): void // end of [vec4f_init_env]
//
fun{a:t@ype}{env:vt@ype}
vec4f_fold_env  (
  res: &a >> a
, env: &(env) >> _
): void // end of [vec3f_fold_env]
//

fun add_vec4f_vec4f (&vec4f, &vec4f): vec4f
fun mul_float_vec4f (float, &vec4f): vec4f
fun neg_vec4f (&vec4f): vec4f
fun sub_vec4f_vec4f (&vec4f, &vec4f): vec4f
fun length_sq_vec4f (&vec4f): float
fun length_vec4f (&vec4f): float
fun normalize_vec4f (&vec4f): vec4f
fun dotprod_vec4f_vec4f (&vec4f, &vec4f): float
fun lerp_vec4f_vec4f_float (&vec4f, &vec4f, float): vec4f
fun{}
equal_vec4f$eps (): float
fun
equal_vec4f_vec4f (&vec4f, &vec4f): bool
fun
fprint_vec4f (FILEref, &vec4f): void
fun
print_vec4f (&vec4f): void

//
overload + with add_vec4f_vec4f
overload * with mul_float_vec4f
overload ~ with neg_vec4f
overload - with sub_vec4f_vec4f
overload .length_sq with length_sq_vec4f
overload .length with length_vec4f
overload .normalize with normalize_vec4f
overload dotprod with dotprod_vec4f_vec4f
overload lerp with lerp_vec4f_vec4f_float
overload = with equal_vec4f_vec4f
overload fprint with fprint_vec4f
overload print with print_vec4f
//

fun
vec4f_init1 (&vec4f >> _, float): void
overload .init with vec4f_init1

fun
vec4f_init4 (&vec4f? >> _, float, float, float, float): void
overload .init with vec4f_init4

fun
vec4f_get_int (&vec4f, natLt(4)): float
fun
vec4f_set_int (&vec4f, natLt(4), float): void

overload [] with vec4f_get_int of 10
overload [] with vec4f_set_int of 10

fun
vec4f_get_x (&vec4f): float
fun
vec4f_get_y (&vec4f): float
fun
vec4f_get_z (&vec4f): float
fun
vec4f_get_w (&vec4f): float

overload .x with vec4f_get_x
overload .y with vec4f_get_y
overload .z with vec4f_get_z
overload .w with vec4f_get_w

fun
vec4f_set_x (&vec4f, float): void
fun
vec4f_set_y (&vec4f, float): void
fun
vec4f_set_z (&vec4f, float): void
fun
vec4f_set_w (&vec4f, float): void

overload .x with vec4f_set_x
overload .y with vec4f_set_y
overload .z with vec4f_set_z
overload .w with vec4f_set_w

(* ****** ****** *)

typedef
vector_3_int_t0ype =
$extype_struct "vec3i_t" of {
  V= @[int][3]
}

stadef vec3i = vector_3_int_t0ype // local shorthand
//
fun{vt:vt@ype}
vec3i_init$fwork (natLt(3), &(vt)): int
//
fun{a:t@ype}{env:vt@ype}
vec3i_fold$fwork (&a >> a, natLt(3), &(env) >> _): void
//
fun{vt:vt@ype}
vec3i_init_env (
  vec: &vec3i? >> _, env: &(vt)
): void // end of [vec3i_init_env]
//
fun{a:t@ype}{env:vt@ype}
vec3i_fold_env  (
  res: &a >> a
, env: &(env) >> _
): void // end of [vec3i_fold_env]
//
fun add_vec3i_vec3i (&vec3i, &vec3i): vec3i
fun mul_int_vec3i (int, &vec3i): vec3i
fun neg_vec3i (&vec3i): vec3i
fun sub_vec3i_vec3i (&vec3i, &vec3i): vec3i
fun length_sq_vec3i (&vec3i): int
fun length_vec3i (&vec3i): int
fun normalize_vec3i (&vec3i): vec3i
fun dotprod_vec3i_vec3i (&vec3i, &vec3i): int
fun lerp_vec3i_vec3i_int (&vec3i, &vec3i, int): vec3i
fun{}
equal_vec3i$eps (): int
fun
equal_vec3i_vec3i (&vec3i, &vec3i): bool
fun
fprint_vec3i (FILEref, &vec3i): void
fun
print_vec3i (&vec3i): void

//
overload + with add_vec3i_vec3i of 10
overload * with mul_int_vec3i of 10
overload ~ with neg_vec3i of 10
overload - with sub_vec3i_vec3i of 10
overload .length_sq with length_sq_vec3i of 10
overload .length with length_vec3i of 10
overload .normalize with normalize_vec3i of 10
overload dotprod with dotprod_vec3i_vec3i of 10
overload lerp with lerp_vec3i_vec3i_int of 10
overload = with equal_vec3i_vec3i of 10
overload fprint with fprint_vec3i of 10
overload print with print_vec3i of 10
//

fun
crossprod_vec3i_vec3i (&vec3i, &vec3i): vec3i

overload crossprod with crossprod_vec3i_vec3i

fun
vec3i_init1 (&vec3i? >> _, int): void
overload .init with vec3i_init1 of 10

fun
vec3i_init3 (&vec3i? >> _, int, int, int): void
overload .init with vec3i_init3 of 10

fun
vec3i_get_int (&vec3i, natLt(3)): int
fun
vec3i_set_int (&vec3i, natLt(3), int): void

overload [] with vec3i_get_int of 10
overload [] with vec3i_set_int of 10

fun
vec3i_get_x (&vec3i): int
fun
vec3i_get_y (&vec3i): int
fun
vec3i_get_z (&vec3i): int

overload .x with vec3i_get_x of 10
overload .y with vec3i_get_y of 10
overload .z with vec3i_get_z of 10

fun
vec3i_set_x (&vec3i, int): void
fun
vec3i_set_y (&vec3i, int): void
fun
vec3i_set_z (&vec3i, int): void

overload .x with vec3i_set_x of 10
overload .y with vec3i_set_y of 10
overload .z with vec3i_set_z of 10

(* ****** ****** *)
