(*
On this page we defined quaternion arithmetic in terms of qw, qx, qy and qz but we can also define the arithetic operations in terms of scalar and vector notation:

(sa,va) + (sb,vb) = (sa+sb,va+vb)
(sa,va) - (sb,vb) = (sa-sb,va-vb)
(sa,va) * (sb,vb) = (sa*sb-dotprod (va,vb),crossprod (va, vb) + sa*vb + sb*va)
(sa,va) / (sb,vb) = (sa*sb+dotprod (va,vb),-crossprod (va, vb) - sa*vb + sb*va)

where:

    (sa,va) = quaternion a
    (sb,vb) = quaternion b
    • = vector dot product
    × = vector cross product
    
http://www.euclideanspace.com/maths/algebra/realNormedAlgebra/quaternions/
http://www.euclideanspace.com/maths/algebra/realNormedAlgebra/quaternions/arithmetic/index.htm

https://github.com/google/mathfu/blob/master/include/mathfu/quaternion.h
*)
//
#define ATS_DYNLOADFLAG 0
//
#include
"share/atspre_staload.hats"
//
staload _ = "prelude/DATS/gnumber.dats"
staload _ = "prelude/DATS/SHARE/gnumber_float.dats"
staload _ = "prelude/DATS/gorder.dats"

staload "libats/libc/SATS/math.sats" // NOTE: requires -lm during compilation (for length_vec)
staload _ = "libats/libc/DATS/math.dats"

staload "./../SATS/vector.sats"
staload "./../SATS/matrix.sats"
staload "./../SATS/quaternion.sats"

staload _ = "./vec3f.dats"

(* ****** ****** *)

typedef T = float
typedef vec = vec3f
typedef mat = mat3x3f
typedef quat = quatf

(* ****** ****** *)

macdef mul (x, y) = gmul_val_val<T>(,(x), ,(y))
macdef neg (x) = gneg_val<T>(,(x))
macdef pos (x) = (,(x))
macdef add (x, y) = gadd_val_val<T>(,(x), ,(y))
macdef div (x, y) = gdiv_val_val<T>(,(x), ,(y))
macdef sub (x, y) = gsub_val_val<T>(,(x), ,(y))

(* ****** ****** *)

macdef _0 = gnumber_int<T>(0)
macdef _1 = gnumber_int<T>(1)
macdef _2 = gnumber_int<T>(2)

(* ****** ****** *)

implement
quatf_init4 (res, s, x, y, z) = {
//
val () = res.s := s
val () = res.v.init (x, y, z)
//
}

implement
quatf_init_float_vec3f (res, s, v) = {
//
val () = res.s := s
val () = res.v := v
//
} (* end of [quatf_init_float_vec3f] *)

(* ****** ****** *)

implement
quatf_init_mat3x3f (res, m) = {
//
val trace = add (m[0, 0], add (m[1, 1], m[2, 2]))
//
val () =
case+ :(res: quat) => 0 of
| _ when gisgtz_val<T> (trace) => {
//
val s = mul (sqrt<T> (add (trace, _1)), _2)
val invs = grecip_val<T> (s)
//
val () =
res.init (
  mul (gnumber_double<T>(0.25), s),
  // 
  mul (sub (m[2,1], m[1,2]), invs),
  mul (sub (m[0,2], m[2,0]), invs),
  mul (sub (m[1,0], m[0,1]), invs)
)
//
} (* end of [gisgtz_val<T> ...] *)
//
| _ when (ggt_val_val<T> (m[0,0], m[1,1]) && ggt_val_val<T> (m[0,0], m[2,2])) => {
//
val s = mul (sqrt<T> (add (sub (m[0,0], sub (m[1,1], m[2,2])), gnumber_int<T>(1))), gnumber_int<T>(2))
val invs = grecip_val<T> (s)
//
val () = res.init (
  mul (sub (m[2,1], m[1,2]), invs),
  mul (gnumber_double<T>(0.25), s),
  mul (sub (m[0,1], m[1,0]), invs),
  mul (sub (m[0,2], m[2,0]), invs)
)
//
} (* end of [m[0,0] > m[1,1] ...] *)
| _ when (ggt_val_val<T> (m[1,1], m[2,2])) => {
//
val s = mul (sqrt<T> (add (sub (m[1,1], sub (m[0,0], m[2,2])), gnumber_int<T>(1))), gnumber_int<T>(2))
val invs = grecip_val<T> (s)
//
val () =
res.init (
  mul (sub (m[0,2], m[2,0]), invs),
  mul (add (m[0,1], m[1,0]), invs),
  mul (gnumber_double<T>(0.25), s),
  mul (add (m[2,1], m[1,2]), invs)
)
//
}
//
| _ => {
//
val s = mul (sqrt<T> (add (sub (m[2,2], sub (m[0,0], m[1,1])), gnumber_int<T>(1))), gnumber_int<T>(2))
val invs = grecip_val<T> (s)
//
val () =
res.init (
mul (sub (m[1,0], m[0,1]), invs),
mul (add (m[0,2], m[2,0]), invs),
mul (add (m[2,1], m[1,2]), invs),
mul (gnumber_double<T>(0.25), s)
)
//
} (* end of [_] *)
//
} (* end of [quatf_init_mat3x3f] *)

(* ****** ****** *)

implement
mul_quatf_quatf (x, y) = let
//
var res: quat
var cr = crossprod (x.v, y.v)
//
val () = res.s := sub (mul (x.s, y.s), dotprod (x.v, y.v))
//
prval [lx:addr] EQADDR () = eqaddr_make_ptr (addr@(x))
prval [ly:addr] EQADDR () = eqaddr_make_ptr (addr@(y))
//
absvt@ype VT = void
viewdef V = (quat @ lx, quat @ ly, vec @ cr)
prval (dec, enc) = make_iso_v_vt {V} {VT} ()
//
implement
vec3f_init$fwork<VT> (i, env) = let
  prval pf = dec (env)
  val res = add (cr.V.[i], add (mul (x.s, y.v.V.[i]), mul (y.s, x.v.V.[i])))
  prval () = enc (env, pf)
in
  res
end
//
var env = ()
prval () = enc (env, (view@x, view@y, view@cr))
//
val () = vec3f_init_env<VT> (res.v, env)
//
prval (pf0, pf1, pf2) = dec (env)
prval () = view@x := pf0
and () = view@y := pf1
and () = view@cr := pf2
//
in
  res
end // end of [mul_quatf_quatf]

implement
mul_float_quatf (s, q) = let
//
var angle: float
var axis: vec3f
val () = axis_angle_of_quatf (q, angle, axis)
val () = angle := angle * s
//
in
  quatf_of_axis_angle (angle, axis)
end // end of [mul_float_quatf]

(* ****** ****** *)

implement
slerp_quatf_quatf_float (q1, q2, s) = let
//
var res: quat
macdef eps = gnumber_double<T>(1.0 - 0e-6)
//
val () = (
//
  if :(res: quat) =>
     ggt_val_val<T> (add (mul (q1.s, q2.s), dotprod (q1.v, q2.v)), eps) then let
  //
  var q1v = sub (_1, s) * q1.v
  var q2v = s * q2.v
  var tmp = q1v + q2v
  //
  in
  //
    res.init (
      mul (q1.s, sub (_1, s)) + mul (q2.s, s),
      tmp
    )
  //
  end else let
  //
  var inv_q1 = invert (q1)
  var tmp1 = inv_q1 * q2
  var tmp2 = q1 * tmp1
  //
  in
    res := s * tmp2
  end // end of [if]
//
)
//
in
  res
end // end of [slerp_quatf_quatf_float]

(* ****** ****** *)

implement
quatf_get_int (q, i) = (
  if i = 0 then q.s
  else q.v.V.[i - 1]
) (* end of [quatf_get_int] *)

(* ****** ****** *)

implement
quatf_set_int (q, i, x) =
  if i = 0 then q.s := x
  else q.v.V.[i-1] := x
// end of [quatf_set_int]

(* ****** ****** *)

implement
invert_quatf (q) = let
//
var res: quat
//
var nv = ~q.v
val () = res.init (neg (q.s), nv)
//
in
  res
end // end of [invert_quatf]

implement
normalize_quatf (q) = let
//
val len = sqrt<T> (add (mul (q.s, q.s), dotprod (q.v, q.v)))
val scale = grecip_val<T> (len)
//
val () = q.s := scale * q.s
val () = q.v := scale * q.v
//
in
  len
end // end of [normalize_quatf]

(* ****** ****** *)

implement
quatf_of_axis_angle (angle, axis) = let
//
val half = div (angle, _2)
//
var loc_axis = axis.normalize ()
//
var res: quat
//
var tmp = sin<T>(half) * loc_axis
val () = res.init (cos<T> (half), tmp)
//
in
  res
end // end of [quatf_of_axis_angle]

(* ****** ****** *)

implement
mat3x3f_of_quatf (q) = let
//
val x2 = mul (q.v.V.[0], q.v.V.[0])
val y2 = mul (q.v.V.[1], q.v.V.[1])
val z2 = mul (q.v.V.[2], q.v.V.[2])
val sx = mul (q.s, q.v.V.[0])
val sy = mul (q.s, q.v.V.[1])
val sz = mul (q.s, q.v.V.[2])
val xz = mul (q.v.V.[0], q.v.V.[2])
val yz = mul (q.v.V.[1], q.v.V.[2])
val xy = mul (q.v.V.[0], q.v.V.[1])
//
var res: mat
//
val () =
  res.init (
sub (_1, mul (_2, add (y2, z2))),
mul (_2, add (xy, sz)),
mul (_2, sub (xz, sy)),
mul (_2, sub (xy, sz)),
sub (_1, mul (_2, add (x2, z2))),
mul (_2, add (sx, yz)),
mul (_2, add (sy, xz)),
mul (_2, sub (yz, sx)),
sub (_1, mul (_2, add (x2, y2)))
  )
//
in
  res
end

(* ****** ****** *)

implement
mat4x4f_of_quatf (q) = let
//
var res_3x3: mat3x3f
val() = res_3x3 := mat3x3f_of_quatf(q)
//print res_3x3
val () = println! ("res_3x3 = ", res_3x3)
//
// val () = println! (res_3x3[0][0])
var res: mat4x4f
val () = 
  res.init(
res_3x3[0][0], res_3x3[1][0], res_3x3[2][0], 0.0f,
res_3x3[0][1], res_3x3[1][1], res_3x3[2][1], 0.0f,
res_3x3[0][2], res_3x3[1][2], res_3x3[2][2], 0.0f,
          0.0f,          0.0f,          0.0f, 1.0f)
  // res.init(0.0f)
// val () = mat4x4f_init16(res, 
//   res_3x3[0][0], res_3x3[0][1], res_3x3[0][1], 0.0f,
//   res_3x3[1][0], res_3x3[1][1], res_3x3[1][1], 0.0f,
//   res_3x3[2][0], res_3x3[2][1], res_3x3[2][1], 0.0f,
//            0.0f,          0.0f,          0.0f, 1.0f
//   )
// val () = mat4x4f_init16(res, 
//   1.0f, 0.0f, 0.0f, 0.0f,
//   0.0f, 1.0f, 0.0f, 0.0f,
//   0.0f, 0.0f, 1.0f, 0.0f,
//   0.0f, 0.0f, 0.0f, 1.0f
//   )
//print res
// val () = println! ("res = ")
//
in
  res
end

(* ****** ****** *)

implement
axis_angle_of_quatf (q, angle, axis) = {
//
val () = axis := (if gisgtz_val<T> (q.s) then q.v else ~q.v)
val len = length_vec3f(axis)
val () = axis := grecip_val<T> (len) * axis
val () = angle := _2 * atan2 (len, if gisgtz_val<T> (q.s) then q.s else neg (q.s))
//
} (* end of [axis_angle_of_quatf] *)

(* ****** ****** *)

implement
fprint_quatf(out, q) = {
val () = fprint! (out, "( s = ", q.s, ", xyz = ", q.v, " )")
}

(* ****** ****** *)

implement
print_quatf (q) = fprint_quatf(stdout_ref, q)

(* ****** ****** *)