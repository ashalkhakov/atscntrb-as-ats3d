//
#define ATS_PACKNAME "PLAYGROUND.mat3x3f"
#define ATS_DYNLOADFLAG 0
//
#include
"share/atspre_staload.hats"
//
staload "../SATS/vector.sats"
staload "../SATS/matrix.sats"

staload _ = "prelude/DATS/gnumber.dats"
staload _ = "prelude/DATS/SHARE/gnumber_float.dats"
staload _ = "prelude/DATS/gorder.dats"

staload "libats/libc/SATS/math.sats" // NOTE: requires -lm during compilation
staload _ = "libats/libc/DATS/math.dats"

staload "libats/SATS/typeval.sats"
staload _ = "libats/DATS/typeval.dats"

staload _ = "./matrix.dats"

staload "./vec3f.dats" // bring the names in

(* ****** ****** *)
//
typedef T = float
#define NROW 3
#define NCOL 3
typedef vec = vec3f
typedef ROW_T = vec3f
typedef COL_T = vec3f
typedef mat = mat3x3f
//
(* ****** ****** *)

#include "./SHARE/matrix.hats"

(* ****** ****** *)

//
implement
mat3x3f_init1 (m, x) = mat_init1 (m, x)
//
implement
mat3x3f_init9 (m, s00, s10, s20, s01, s11, s21, s02, s12, s22) = {
//
var c0: COL_T
var c1: COL_T
var c2: COL_T
//
val () = c0.init (s00, s10, s20)
val () = c1.init (s01, s11, s21)
val () = c2.init (s02, s12, s22)
//
val () = mat3x3f_init3_vec3f (m, c0, c1, c2)
//
} (* end of [mat3x3f_init9] *)
//
implement
mat3x3f_init3_vec3f (m, col0, col1, col2) = {
//
fun
aux (m: &(@[INV(COL_T)][NCOL])? >> _, c0: &COL_T, c1: &COL_T, c2: &COL_T): void = {
//
prval pf_arr = view@(m)
var pm = addr@(m)
//
prval (pf1_at, pf1_arr) = array_v_uncons {COL_T?} (pf_arr)
val () = ptr_set<COL_T> (pf1_at | pm, c0)
//
val () = pm := ptr1_succ<COL_T> (pm)
prval (pf2_at, pf2_arr) = array_v_uncons {COL_T?} (pf1_arr)
val () = ptr_set<COL_T> (pf2_at | pm, c1)
//
val () = pm := ptr1_succ<COL_T> (pm)
prval (pf3_at, pf3_arr) = array_v_uncons {COL_T?} (pf2_arr)
val () = ptr_set<COL_T> (pf3_at | pm, c2)
//
#define :: array_v_cons
//
prval pf3_nil = array_v_unnil_nil (pf3_arr)
prval () = view@(m) := pf1_at :: pf2_at :: pf3_at :: pf3_nil
//
} (* end of [aux] *)
//
val () = aux (m.M, col0, col1, col2)
//
} (* end of [mat3x3f_init3_vec3f] *)

implement
mat3x3f_init_arrptr {n} (m, arr) = mat_init_arrptr (m, arr)

implement
mat3x3f_get_int_int (m, r, c) = m.M.[c].V.[r]
(*
implement
mat3x3f_get_int (m, i) = let val r = i % NROW; val c = i / NROW in m.M[c].V[r] end
*)
implement
mat3x3f_set_int_int (m, r, c, x) = m.M.[c].V.[r] := x
(*
implement
mat3x3f_set_int (m, i, x) = ?
*)
//
implement
add_mat3x3f_mat3x3f (x, y) = add_mat_mat (x, y)
//
implement
add_float_mat3x3f (s, x) = add_T_mat (s, x)
//
implement
mul_float_mat3x3f (s, x) = mul_T_mat (s, x)
//
implement
mul_mat3x3f_mat3x3f (x, y) = mul_mat_mat (x, y)
//
implement
sub_mat3x3f_mat3x3f (x, y) = sub_mat_mat (x, y)
//
implement
neg_mat3x3f (x) = neg_mat (x)
//
extern
fun{N:type}
invert_mat3x3f_mat3x3f_opt {n:nat | n < 2} (
  pf: tieq (N, n)
| m: &mat3x3f, res: &mat3x3f? >> opt (mat3x3f, b)
) : #[b:bool | n == 0 && b == true || n == 1] bool b
//
implement{N}
invert_mat3x3f_mat3x3f_opt {n} (pf | m, res) = let
//
  macdef mul (x, y) = gmul_val_val<T>(,(x), ,(y))
  macdef neg (x) = gneg_val<T>(,(x))
  macdef add (x, y) = gadd_val_val<T>(,(x), ,(y))
  macdef sub (x, y) = gsub_val_val<T>(,(x), ,(y))
//
val det = add (sub (mul (m[0, 0], sub (mul (m[1, 1], m[2, 2]), mul (m[2, 1], m[1, 2]))),
                    mul (m[0, 1], sub (mul (m[1, 0], m[2, 2]), mul (m[1, 2], m[2, 0])))),
               mul (m[0, 2], sub (mul (m[1, 0], m[2, 1]), mul (m[1, 1], m[2, 0]))))
//
val ck = tieq2int<N> (pf | (*void*))
//
macdef calc = let
  val invdet = gdiv_val_val<T> (gnumber_int<T>(1), det);
in
  mat3x3f_init9 (
    res,
    mul (sub (mul (m[1, 1], m[2, 2]), mul (m[2, 1], m[1, 2])), invdet),
    mul (sub (mul (m[1, 2], m[2, 0]), mul (m[1, 0], m[2, 2])), invdet),
    mul (sub (mul (m[1, 0], m[2, 1]), mul (m[2, 0], m[1, 1])), invdet),
    mul (sub (mul (m[0, 2], m[2, 1]), mul (m[0, 1], m[2, 2])), invdet),
    mul (sub (mul (m[0, 0], m[2, 2]), mul (m[0, 2], m[2, 0])), invdet),
    mul (sub (mul (m[2, 0], m[0, 1]), mul (m[0, 0], m[2, 1])), invdet),
    mul (sub (mul (m[0, 1], m[1, 2]), mul (m[0, 2], m[1, 1])), invdet),
    mul (sub (mul (m[1, 0], m[0, 2]), mul (m[0, 0], m[1, 2])), invdet),
    mul (sub (mul (m[0, 0], m[1, 1]), mul (m[1, 0], m[0, 1])), invdet)
  )
end // end of [macdef]
//
in
//
if ck = 0 then let
  val () = calc
  prval () = opt_some {mat} (res)
in
  true
end else (
//
  if glt_val_val<T> (gabs_val<T> (det), gdet_treshold<T> ()) then let
    prval () = opt_none {mat} (res)
  in
    false
  end else let
    val () = calc
    prval () = opt_some {mat} (res)
  in
    true
  end // end of [if]
//
)
//
end // end of [invert_mat3x3f_mat3x3f_opt]
//
implement
invert_mat3x3f (m) = let
  var res: mat
  val+ true = invert_mat3x3f_mat3x3f_opt<fS(0)> (TIEQZ() | m, res)
  prval () = opt_unsome {mat} (res)
in
  res
end // end of [invert_mat3x3f]
//
implement
invert_mat3x3f_mat3x3f (m, res) =
  invert_mat3x3f_mat3x3f_opt<fS(1)> (TIEQS(TIEQZ()) | m, res)
// end of [invert_mat3x3f_mat3x3f]
//
implement
transpose_mat3x3f (m) = transpose_mat (m)
//
implement
identity_mat3x3f (res) = identity_mat (res)
//
implement
mul_mat3x3f_vec3f (m, v) = mul_mat_vec (m, v)
//
implement
fprint_mat3x3f (out, m) = fprint_mat (out, m)
//
implement
print_mat3x3f (m) = print_mat (m)

(* ****** ****** *)

implement
mat3x3f_of_translation (v) = let
//
var res: mat
//
val _1 = gnumber_int<T>(1)
val _0 = gnumber_int<T>(0)
//
val () =
  res.init (
    _1, _0, _0,
    _0, _1, _0,
    v.V.[0], v.V.[1], _1
  )
in
  res
end // end of [mat3x3f_of_translation]

implement
mat3x3f_of_scale (v) = let
//
var res: mat
//
val _1 = gnumber_int<T> (1)
val _0 = gnumber_int<T> (0)
//
val () =
  res.init (
    v.V.[0], _0, _0,
    _0, v.V.[1], _0,
    _0, _0, _1
  )
//
in
  res
end // end of [mat3x3f_of_scale]

implement
mat3x3f_rotation_x_vec2f (v) = let
//
var res: mat
//
val _1 = gnumber_int<T> (1)
val _0 = gnumber_int<T> (0)
//
val () =
  res.init (
    _1, _0, _0,
    _0, v.V.[0], v.V.[1],
    _0, ~v.V.[1], v.V.[0]
  )
//
in
  res
end // end of [mat3x3f_rotation_x_vec2f]

implement
mat3x3f_rotation_y_vec2f (v) = let
//
var res: mat
//
val _1 = gnumber_int<T> (1)
val _0 = gnumber_int<T> (0)
//
val () =
  res.init (
    v.V.[0], _0, ~v.V.[1],
    _0, _1, _0,
    v.V.[1], _0, v.V.[0]
  )
//
in
  res
end // end of [mat3x3f_rotation_y_vec2f]

implement
mat3x3f_rotation_z_vec2f (v) = let
//
var res: mat
//
val _1 = gnumber_int<T> (1)
val _0 = gnumber_int<T> (0)
//
val () =
  res.init (
    v.V.[0], v.V.[1], _0,
    ~v.V.[1], v.V.[0], _0,
    _0, _0, _1
  )
//
in
  res
end // end of [mat3x3f_rotation_z_vec2f]

implement
mat3x3f_rotation_x_rad (x) = let
  var v: vec2f
  val () = v.init (cos<T>(x), sin<T>(x))
in
  mat3x3f_rotation_x_vec2f (v)
end // end of [mat3x3f_rotation_x_rad]

implement
mat3x3f_rotation_y_rad (x) = let
  var v: vec2f
  val () = v.init (cos<T>(x), sin<T>(x))
in
  mat3x3f_rotation_y_vec2f (v)
end // end of [mat3x3f_rotation_y_rad]

implement
mat3x3f_rotation_z_rad (x) = let
  var v: vec2f
  val () = v.init (cos<T>(x), sin<T>(x))
in
  mat3x3f_rotation_z_vec2f (v)
end // end of [mat3x3f_rotation_z_rad]

