staload "./vector.sats"
staload "./matrix.sats"

(* ****** ****** *)

#define ATS_PACKNAME "ATSCNTRB.as.ats3d"
#define ATS_EXTERN_PREFIX "atscntrb_as_ats3d_" // prefix for external names

(* ****** ****** *)

// quaternion in vector notation
typedef quat_float_vec3f_t0ype = @{s= float, v= vec3f}

stadef quatf = quat_float_vec3f_t0ype // local shorthand

fun
quatf_init4 (&quatf? >> _, float, float, float, float): void
fun
quatf_init_float_vec3f (&quatf? >> _, float, &vec3f): void
fun
quatf_init_mat3x3f (&quatf? >> _, &mat3x3f): void

overload .init with quatf_init4
overload .init with quatf_init_float_vec3f
overload .init with quatf_init_mat3x3f

(*
fun
add_quatf_quatf (&quatf, &quatf): quatf
fun
sub_quatf_quatf (&quatf, &quatf): quatf
*)
fun
mul_quatf_quatf (&quatf, &quatf): quatf
fun
mul_float_quatf (float, &quatf): quatf
(*
fun
mul_vec3f_quatf (&vec3f, &quatf): vec3f
*)

fun
slerp_quatf_quatf_float (&quatf, &quatf, float): quatf

fun
quatf_get_int (&quatf, natLt(4)): float
fun
quatf_set_int (&quatf, natLt(4), float): void

fun
invert_quatf (&quatf): quatf
fun
normalize_quatf (&quatf): float

// TODO: Euler angles
fun
quatf_of_axis_angle (angle: float, axis: &vec3f): quatf

fun
mat3x3f_of_quatf (&quatf): mat3x3f

fun
mat4x4f_of_quatf (&quatf): mat4x4f

fun
axis_angle_of_quatf (&quatf, angle: &float? >> _, axis: &vec3f? >> _): void

fun
fprint_quatf (FILEref, &quatf): void

fun
print_quatf (&quatf): void
// TODO: from axis/angle, from angles

overload invert with invert_quatf
overload * with mul_quatf_quatf
overload * with mul_float_quatf
overload print with print_quatf
overload fprint with fprint_quatf
