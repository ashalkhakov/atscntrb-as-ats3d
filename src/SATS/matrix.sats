staload "./vector.sats"

%{#

#include "../CATS/vector.cats"
#include "../CATS/matrix.cats"

%}

// threshold of determinant when it is considered too small
// for the matrix inverse to be calculated
fun{a:t@ype}
gdet_treshold (): a

(* ****** ****** *)

typedef
matrix_3_3_float_t0ype =
$extype_struct "mat3x3f_t" of {
  // NOTE: row of column-vectors
  M= @[vec3f][3]
}

stadef mat3x3f = matrix_3_3_float_t0ype // local shorthand
//

// construct a matrix from a single scalar T
fun
mat3x3f_init1 (&mat3x3f? >> _, float): void

// construct a matrix from NROW*NCOL scalars T
fun
mat3x3f_init9 (
  &mat3x3f? >> _,
  s00: float, s10: float, s20: float,
  s01: float, s11: float, s21: float,
  s02: float, s12: float, s22: float
): void

// construct a matrix from NCOL vectors of NDIM=NROW
fun
mat3x3f_init3_vec3f (
  &mat3x3f? >> _,
  col0: &vec3f, col1: &vec3f, col2: &vec3f
): void

// construct a matrix from first NROW*NCOL elements of array of T
fun
mat3x3f_init_arrptr {n:nat | n >= 9} (
  &mat3x3f? >> _,
  &(@[float][n]) >> _
): void

overload .init with mat3x3f_init1
overload .init with mat3x3f_init9
overload .init with mat3x3f_init_arrptr

fun
mat3x3f_get_int_int (&mat3x3f, row: natLt(3), col: natLt(3)): float
(*
fun
mat3x3f_get_int (&mat3x3f, i: natLt(3*3)): float
*)
fun
mat3x3f_set_int_int (&mat3x3f, row: natLt(3), col: natLt(3), float): void
(*
fun
mat3x3f_set_int (&mat3x3f, i:natLt(3*3), float): void
*)

overload [] with mat3x3f_get_int_int of 10
(*
overload [] with mat3x3f_get_int of 10
*)
overload [] with mat3x3f_set_int_int of 10
(*
overload [] with mat3x3f_set_int of 10
*)

fun
add_mat3x3f_mat3x3f (&mat3x3f, &mat3x3f): mat3x3f
fun
add_float_mat3x3f (float, &mat3x3f): mat3x3f
fun
mul_float_mat3x3f (float, &mat3x3f): mat3x3f
fun
mul_mat3x3f_mat3x3f (&mat3x3f, &mat3x3f): mat3x3f
fun
sub_mat3x3f_mat3x3f (&mat3x3f, &mat3x3f): mat3x3f
fun
neg_mat3x3f (&mat3x3f): mat3x3f
fun
invert_mat3x3f (&mat3x3f): mat3x3f
fun
invert_mat3x3f_mat3x3f (&mat3x3f, &mat3x3f? >> opt (mat3x3f, b)): #[b:bool] bool b
fun
transpose_mat3x3f (&mat3x3f): mat3x3f
fun
identity_mat3x3f (&mat3x3f? >> _): void
fun
mul_mat3x3f_vec3f (&mat3x3f, &vec3f): vec3f
fun
fprint_mat3x3f (FILEref, &mat3x3f): void
fun
print_mat3x3f (&mat3x3f): void

//
overload + with add_mat3x3f_mat3x3f
overload + with add_float_mat3x3f
overload * with mul_float_mat3x3f
overload * with mul_mat3x3f_mat3x3f
overload - with sub_mat3x3f_mat3x3f
overload ~ with neg_mat3x3f
overload invert with invert_mat3x3f
overload invert with invert_mat3x3f_mat3x3f
overload transpose with transpose_mat3x3f
overload .identity with identity_mat3x3f
overload * with mul_mat3x3f_vec3f
overload fprint with fprint_mat3x3f
overload print with print_mat3x3f
//

fun
mat3x3f_of_translation (&vec2f): mat3x3f
fun
mat3x3f_of_scale (&vec2f): mat3x3f
fun
mat3x3f_rotation_x_vec2f (&vec2f): mat3x3f
fun
mat3x3f_rotation_y_vec2f (&vec2f): mat3x3f
fun
mat3x3f_rotation_z_vec2f (&vec2f): mat3x3f
fun
mat3x3f_rotation_x_rad (float): mat3x3f
fun
mat3x3f_rotation_y_rad (float): mat3x3f
fun
mat3x3f_rotation_z_rad (float): mat3x3f

// TODO: overloads for the functions declared above?

(* ****** ****** *)

typedef
matrix_4_4_float_t0ype =
$extype_struct "mat4x4f_t" of {
  // NOTE: row of column-vectors
  M= @[vec4f][4]
}

stadef mat4x4f = matrix_4_4_float_t0ype // local shorthand
//

fun
mat4x4f_init1 (&mat4x4f? >> _, float): void
//
fun
mat4x4f_init16 (
  &mat4x4f? >> _,
  s00: float, s10: float, s20: float, s30: float,
  s01: float, s11: float, s21: float, s31: float,
  s02: float, s12: float, s22: float, s32: float,
  s03: float, s13: float, s23: float, s33: float
): void
//
fun
mat4x4f_init4_vec4f (
  &mat4x4f? >> _,
  col0: &vec4f, col1: &vec4f, col2: &vec4f, col3: &vec4f
): void
//
fun
mat4x4f_init_arrptr {n:nat | n >= 16} (
  &mat4x4f? >> _,
  &(@[float][n]) >> _
): void

overload .init with mat4x4f_init1
overload .init with mat4x4f_init16
overload .init with mat4x4f_init4_vec4f
overload .init with mat4x4f_init_arrptr

fun
mat4x4f_get_int_int (&mat4x4f, row: natLt(4), col: natLt(4)): float
(*
fun
mat4x4f_get_int (&mat4x4f, i: natLt(4*4)): float
*)
fun
mat4x4f_set_int_int (&mat4x4f, row: natLt(4), col: natLt(4), float): void
(*
fun
mat4x4f_set_int (&mat4x4f, i: natLt(3*3), float): void
*)

overload [] with mat4x4f_get_int_int of 10
(*
overload [] with mat4x4f_get_int of 10
*)
overload [] with mat4x4f_set_int_int of 10
(*
overload [] with mat4x4f_set_int of 10
*)

fun
add_mat4x4f_mat4x4f (&mat4x4f, &mat4x4f): mat4x4f
fun
add_float_mat4x4f (float, &mat4x4f): mat4x4f
fun
mul_float_mat4x4f (float, &mat4x4f): mat4x4f
fun
mul_mat4x4f_mat4x4f (&mat4x4f, &mat4x4f): mat4x4f
fun
sub_mat4x4f_mat4x4f (&mat4x4f, &mat4x4f): mat4x4f
fun
neg_mat4x4f (&mat4x4f): mat4x4f
fun
invert_mat4x4f (&mat4x4f): mat4x4f
fun
invert_mat4x4f_mat4x4f (&mat4x4f, &mat4x4f? >> opt (mat4x4f, b)): #[b:bool] bool b
fun
transpose_mat4x4f (&mat4x4f): mat4x4f
fun
identity_mat4x4f (&mat4x4f? >> _): void
fun
mul_mat4x4f_vec4f (&mat4x4f, &vec4f): vec4f
fun
fprint_mat4x4f (FILEref, &mat4x4f): void
fun
print_mat4x4f (&mat4x4f): void

//
overload + with add_mat4x4f_mat4x4f
overload + with add_float_mat4x4f
overload * with mul_float_mat4x4f
overload * with mul_mat4x4f_mat4x4f
overload - with sub_mat4x4f_mat4x4f
overload ~ with neg_mat4x4f
overload invert with invert_mat4x4f
overload invert with invert_mat4x4f_mat4x4f
overload transpose with transpose_mat4x4f
overload .identity with identity_mat4x4f
overload * with mul_mat4x4f_vec4f
overload fprint with fprint_mat4x4f
overload print with print_mat4x4f
//

fun
mat4x4f_of_translation (&vec3f): mat4x4f
fun
mat4x4f_of_scale (&vec3f): mat4x4f
fun
mat4x4f_of_mat3x3f (&mat3x3f): mat4x4f
fun
mat4x4f_perspective (fovx: float, aspect: float, znear: float, zfar: float): mat4x4f
fun
mat4x4f_ortho (width: float, height: float, znear: float, zfar: float): mat4x4f
fun
mat4x4f_view (origin: &vec3f, forward: &vec3f, right: &vec3f, up: &vec3f): mat4x4f
fun
mat4x4f_look_at (at: &vec3f, eye: &vec3f, up: &vec3f): mat4x4f
