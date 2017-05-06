#include
"share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload
_ = "prelude/DATS/integer.dats"
staload
_ = "prelude/DATS/integer_fixed.dats"
staload
_ = "prelude/DATS/array.dats"
staload
_ = "prelude/DATS/matrix.dats"

staload PM = "./../src/SATS/image/pixmap.sats"
staload _ = "./../src/DATS/image/pixmap.dats"

staload PPM = "./../src/SATS/image/PPM.sats"

staload "./../src/SATS/vector.sats"
staload _ = "./../src/DATS/vec3i.dats"

(* ****** ****** *)

extern
fun{env:vt@ype}
line_raster$fwork (&vec3i, &(env) >> _): void
extern
fun{env:vt@ype}
line_raster_foreach_env (&vec3i, &vec3i, &(env) >> _): void

implement{env}
line_raster_foreach_env (from, to, env) = {
//
  typedef raster_state = @{step= vec3i, d= vec3i, major_axis= natLt(3)}
//
  var from = from
  var to = to
  var state : raster_state
  val () = vec3i_init3 (state.step, 0, 0, 0)
  val () = vec3i_init3 (state.d, 0, 0, 0)
  val () = state.major_axis := 0
//
  var max = (g0ofg1)0
  var i: int = 0
  val () =
    while* {i:nat | i <= 3}
      (i: int(i), state: raster_state, from: vec3i, to: vec3i, max: int):
      (state: raster_state, from: vec3i, to: vec3i, max: int) =>
      (i < 3) {
      val d = to[i] - from[i]
      val () = vec3i_set_int (state.step, i, if d > 0 then 1 else ~1)
      val d = abs(d)
      val () =
        if :(state: raster_state, max: int) => d > max then begin
          max := d;
          state.major_axis := i
        end // end of [val]
      val () = i := i + 1
    } (* end of [for] *)
  val () = line_raster$fwork<env> (from, env)
//
  fun
  calc_rs (from: &vec3i, to: &vec3i, axis: natLt(3)): int = abs (to[axis] - from[axis])
//
  val () =
    while*
      (state: raster_state, from: vec3i, to: vec3i, env: env):
      (state: raster_state, from: vec3i, to: vec3i, env: env) =>
      (~(from = to)) {
        val () =
          from[state.major_axis] :=
          from[state.major_axis] + vec3i_get_int (state.step, state.major_axis)
        // end of [val]
        val rs_base = calc_rs (from, to, state.major_axis)

        var i: int = 0
        val () =
          while* {i:nat | i <= 3}
            (i: int(i), state: raster_state, from: vec3i, to: vec3i):
            (state: raster_state, from: vec3i, to: vec3i) =>
            (i < 3)
          {
            val rs = calc_rs (from, to, i)
            val () =
              if :(from: vec3i, state: raster_state) => (rs > 0 && i != state.major_axis) then {
                val () = vec3i_set_int (state.d, i, vec3i_get_int (state.d, i) + rs)
                val () =
                  if :(from: vec3i, state: raster_state) => (vec3i_get_int (state.d, i) >= rs_base) then {
                    val () = vec3i_set_int (state.d, i, vec3i_get_int (state.d, i) - rs_base)
                    val () = from[i] := from[i] + vec3i_get_int (state.step, i)
                  }
              } (* end of [val] *)
            val () = i := i + 1
          } (* end of [while] *)
        val () = line_raster$fwork<env> (from, env)
      } (* end of [while] *)
} (* end of [line_raster_foreach_env] *)

(* ****** ****** *)

vtypedef pixmap (m:int, n:int) = $PM.pixmap (uint32, m, n)

extern
fun
test_code {m,n:int} (&pixmap (m, n)): void

implement
test_code {m,n} (canvas) = {
//
  var a: vec3i
  var b: vec3i
  val () = vec3i_init3 (a, 0, 0, 0)
  val () = vec3i_init3 (b, 50, 55, ~20)
//
  val p_canvas = addr@(canvas)
  prval pf_canvas = view@(canvas)
//
  implement{env}
  line_raster$fwork (point, env) = {
    prval (pf_canvas, fpf) = decode($vcopyenv_v(pf_canvas))
    val color = $UN.castvwtp0{uint32}(0xFFFFFF)
    val i = point.x()
    val i = (g1ofg0)i
    val-true = i >= 0
    val w = $PM.pixmap_get_width (canvas)
    prval () = lemma_g1uint_param (w)
    val w = g1uint2int_size_int (w)
    val-true = i < w
    val j = point.y()
    val j = (g1ofg0)j
    val-true = j >= 0
    val h = $PM.pixmap_get_height (canvas)
    prval () = lemma_g1uint_param (h)
    val h = g1uint2int_size_int (h)
    val-true = j < h
    val () = $PM.pixmap_set_at_int (canvas, i, j, color)
    prval () = fpf (pf_canvas)
  } (* end of [line_raster$fwork] *)
  var env = 0: int
  val () = line_raster_foreach_env<int> (a, b, env)
//
  prval () = view@(canvas) := pf_canvas
//
} (* end of [test_code] *)

(* ****** ****** *)

implement
main0 () = {
//
  var pic: pixmap (0, 0)
  val () = $PM.pixmap_new<uint32> (pic, (i2sz)300, (i2sz)300, $UN.castvwtp0{uint32}(0))
  prval pf_pic = view@(pic)
//  
  var a: vec3i
  var b: vec3i
  val () = vec3i_init3 (a, 0, 0, 0)
  val () = vec3i_init3 (b, 10, 5, ~4)
//
  implement{env}
  line_raster$fwork (point, env) = {
    prval (pf_pic, fpf) = decode($vcopyenv_v(pf_pic))
    val color = $UN.castvwtp0{uint32} (0xffffff)
    //
    val i = point.x()
    val i = (g1ofg0)i
    val-true = i >= 0
    val w = $PM.pixmap_get_width (pic)
    prval () = lemma_g1uint_param (w)
    val w = g1uint2int_size_int (w)
    val-true = i < w
    val j = point.y()
    val j = (g1ofg0)j
    val-true = j >= 0
    val h = $PM.pixmap_get_height (pic)
    prval () = lemma_g1uint_param (h)
    val h = g1uint2int_size_int (h)
    val-true = j < h
    val () = $PM.pixmap_set_at_int (pic, i, j, color)
    val () = println!("ATS: point x:", point.x(), " y:", point.y(), " z:", point.z(), " color: ", color)
    prval () = fpf (pf_pic)
  } (* end of [line_raster$fwork] *)
  var env = 0: int
  val () = line_raster_foreach_env<int> (a, b, env)
//
  implement{env}
  intrange_foreach$fwork (i, env) = let
    prval (pf_pic, fpf) = decode($vcopyenv_v(pf_pic))
    val () = test_code (pic)
    prval () = fpf (pf_pic)
  in
  end
  val _ = intrange_foreach (0, 1000000)
//
  prval () = view@pic := pf_pic
  val () =
  if :(pic: pixmap (0,0)?) => true then {
    var p_pixmapbuf : ptr
    val (pf_pixmapbuf, pf_free_pixmapbuf | ()) = $PM.pixmap_delete_getbuf (pic, p_pixmapbuf)
    val out = fileref_open_exn ("./output.ppm", file_mode_w)
    val () = $PPM.save_PPM (out, !p_pixmapbuf, (i2sz)300, (i2sz)300)
    val () = matrix_ptr_free {uint32} (pf_pixmapbuf, pf_free_pixmapbuf | p_pixmapbuf)
  } else {
    val () = $PM.pixmap_delete (pic)
  }
//
} (* end of [main0] *)
