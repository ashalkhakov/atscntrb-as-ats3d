#include
"share/atspre_staload.hats"

staload PM = "./../SATS/image/pixmap.sats"
staload _ = "./../DATS/image/pixmap.dats"

staload "./../SATS/raster.sats"

staload "./../SATS/vector.sats"
staload _ = "./../DATS/vec2f.dats"
staload _ = "./../DATS/vec3f.dats"
staload _ = "./../DATS/vec4f.dats"

staload _ = "prelude/DATS/float.dats"
staload _ = "prelude/DATS/integer.dats"

#define ATS_DYNLOADFLAG 0

implement
line (canvas, x0, y0, x1, y1, color) = let
//
  var x0 = x0
  var y0 = y0
  var x1 = x1
  var y1 = y1
//
  val steep =
    if :(x0: int, y0: int, x1: int, y1: int) => abs(x0-x1) < abs(y0-y1) then let
      var tmp : int
      val () = tmp := x0
      val () = x0 := y0
      val () = y0 := tmp
      val () = tmp := x1
      val () = x1 := y1
      val () = y1 := tmp
    in
      true
    end else false
  // end of [val]
  val dx = x1 - x0
  val dy = y1 - y0
  val derror2 = abs(dy) * 2
  var error2 : int = 0
  var x : int
  var y : int = y0
  val () =
    for (x := x0; x <= x1; x := x + 1) {
      val () =
        if steep then $PM.pixmap_set_at_int2 (canvas, x, y, color)
	else $PM.pixmap_set_at_int2 (canvas, y, x, color)
      // end of [val]
      val () = error2 := error2 + derror2
      val () =
        if error2 > dx then {
	  val () = y := g0int_add_int (y, if y1 > y0 then 1 else ~1)
          val () = error2 := error2 - dx * 2
        } (* end of [val] *)
    } (* end of [for] *)
  // end of [val]
in
end

macdef
min3 (a, b, c) = min(,(a), min(,(b), ,(c)))
macdef
max3 (a, b, c) = max(,(a), max(,(b), ,(c)))

extern
fun
barycentric (A: &vec2f, B: &vec2f, C: &vec2f, P: &vec2f, res: &vec3f? >> vec3f): void
implement
barycentric (A, B, C, P, res) = {    
  var s: vec3f and t: vec3f
  val () = t.init (C.y()-A.y(), B.y()-A.y(), A.y()-P.y())
  val () = s.init (C.x()-A.x(), B.x()-A.x(), A.x()-P.x())
  var u = crossprod (s, t)
  val () =
    // backface culling
    if :(res: vec3f) => abs(u.z()) > 1e-2f then
      // don't forget that u[2] is integer. If it is zero then triangle ABC is degenerate
      res.init (1.0f - (u.x () + u.y ()) / u.z (), u.y () / u.z (), u.x () / u.z ())
    else (
      // println!("backface culled");
      // in this case generate negative coordinates, it will be thrown away by the rasterizator
      res.init (~1.0f,1.0f,1.0f)
    )
  // end of [val]
} (* end of [barycentric] *)

extern
fun
clamp {v:int;a,b:nat | a <= b}
  (v: int v, a: int a, b: int b)
  : [r:nat | r >= a; r <= b] int r
implement
clamp {v,a,b} (v, a, b) =
  if v < a then a
  else if v > b then b
  else v

implement{env}
triangle {m,n} (
  env
, a, b, c
, image, zbuffer
): void = {
//
//  val () = println!("a = ", a, ", b = ", b, ", c = ", c)
//
  var bboxmin: vec2f
  and bboxmax: vec2f
//
  var p_a: vec2f
  and p_b: vec2f
  and p_c: vec2f
  val () = p_a.init (a.x() / a.w(), a.y() / a.w())
  val () = p_b.init (b.x() / b.w(), b.y() / b.w())
  val () = p_c.init (c.x() / c.w(), c.y() / c.w())
//
  val () = bboxmin.init (
    min3 (p_a.x(), p_b.x(), p_c.x()),
    min3 (p_a.y(), p_b.y(), p_c.y())
  ) (* end of [val] *)
  val () = bboxmax.init (
    max3 (p_a.x(), p_b.x(), p_c.x()),
    max3 (p_a.y(), p_b.y(), p_c.y())
  ) (* end of [val] *)
//
//  val () = println!("bboxmin = ", bboxmin)
//  val () = println!("bboxmax = ", bboxmax)
//
  val xmin = g1ofg0 (g0float2int (bboxmin.x()))
  val xmax = g1ofg0 (g0float2int (bboxmax.x()))
  val ymin = g1ofg0 (g0float2int (bboxmin.y()))
  val ymax = g1ofg0 (g0float2int (bboxmax.y()))
//
  val width = sz2i ($PM.pixmap_get_width (zbuffer))
  val height = sz2i ($PM.pixmap_get_height (zbuffer))
//
  val xmin = clamp (xmin, 0, width-1)
  //val xmin = max (xmin, 0)
  prval [xmin:int] EQINT () = eqint_make_gint (xmin)
  //val xmax = min (xmax, width-1)
  val xmax = clamp (xmax, xmin, width-1)
  prval [xmax:int] EQINT () = eqint_make_gint (xmax)
  //val ymin = max (ymin, 0)
  val ymin = clamp (ymin, 0, height-1)
  prval [ymin:int] EQINT () = eqint_make_gint (ymin)
  //val ymax = min (ymax, height-1)
  val ymax = clamp (ymax, ymin, height-1)
  prval [ymax:int] EQINT () = eqint_make_gint (ymax)
//
(*
  val () = println!("x = (", xmin, ", ", xmax, ")")
  val () = println!("y = (", ymin, ", ", ymax, ")")
*)
//
  val () = assertloc (xmin <= xmax && ymin <= ymax)
  prval () = __trustme () where {
    // we compute xmin via [min], and xmax via [max],
    // so this holds but is unclear to the typechecker
    // (to show that it holds we'd have to use indexed
    // floating-point type and a separate constraint checker)
    extern praxi __trustme (): [xmin <= xmax; ymin <= ymax] void
  }
//
  var x: int?
  and y: int?
  val () =  
  for* {x:int | x >= xmin; x <= xmax+1} (x: int(x)) => (x := xmin ; x <= xmax; x := x + 1) (
    for* {y:int | y >= ymin; y <= ymax+1} (y: int(y)) => (y := ymin ; y <= ymax; y := y + 1) (let
      var P: vec2f
      val () = P.init (g0int2float (g0ofg1_int x), g0int2float (g0ofg1_int y))
      var cr: vec3f
      (*
      val () = println!("taking point ", P)
      val () = println!("a = ", a, ", b =", b, ", c = ", c)
      *)
      val () = barycentric (p_a, p_b, p_c, P, cr) // what about backfacing? seems glitchy!
      (*
      val () = println!("barycentric: ", cr)
      *)
      val z = a.z() * cr.x() + b.z() * cr.y() + c.z() * cr.z()
      val w = a.w() * cr.x() + b.w() * cr.y() + c.w() * cr.z()
      val frag_depth = z / w
      //val () = println!("frag depth bef: ", z / w + 0.5f)
      //val frag_depth = max(0, min(255, g0float2int (255.0f * (z / w + 0.5f))))
      //val () = println!("frag depth aft: ", frag_depth)
      //val frag_depth = $UN.cast{uint8}(frag_depth)
    in
      if cr.x() < 0.0f || cr.y() < 0.0f || cr.z() < 0.0f then ((*println!("discarded: out of screen")*))
      else if $PM.pixmap_get_at_int (zbuffer, x, y) > frag_depth then ((*println!("discarded: depth")*))
      else let
        var color: uint32
        val preserve = triangle$fragment<env> (env, cr, color)
      in
        if :(color: uint32?) => preserve then let
          prval () = opt_unsome {uint32} (color)
        in
          $PM.pixmap_set_at_int (zbuffer, x, y, frag_depth);
          $PM.pixmap_set_at_int (image, x, y, color)
        end else let
          prval () = opt_unnone {uint32} (color) in
        end // end of [if]
      end // end of [val]
    end) (* end of [val] *)
  ) (* end of [val] *)
//
} (* end of [triangle] *)
