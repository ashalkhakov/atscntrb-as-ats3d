#include
"share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload _(*anon*) = "libats/DATS/dynarray.dats"

staload PM = "./../SATS/image/pixmap.sats"
staload _ = "./../DATS/image/pixmap.dats"

staload PPM = "./../SATS/image/PPM.sats"

staload OBJ = "./../SATS/mesh/OBJ.sats"
staload _ = "./../DATS/mesh/OBJ.dats"

staload "./../SATS/vector.sats"
staload _ = "./../DATS/vec2f.dats"
staload _ = "./../DATS/vec3f.dats"
staload _ = "./../DATS/vec3i.dats"


(* ****** ****** *)

vtypedef pixmap (m:int, n:int) = $PM.pixmap (uint32, m, n)

extern
fun
line {m,n:int} (
  canvas: &pixmap (m, n), x0: int, y0: int, x1: int, y1: int, color: uint32
): void
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

(* ****** ****** *)

vtypedef mesh = $OBJ.mesh

fun
render {n,m:int} (m: &mesh, pb: &pixmap (n, m)): void = {
//
val halfwidth = g0int2float (sz2i ($PM.pixmap_get_width (pb))) / 2.0f
val halfheight = g0int2float (sz2i ($PM.pixmap_get_height (pb))) / 2.0f
//
val white = $UN.castvwtp0{uint32}(0xFFFFFF)  
//
val p_pixmap = addr@(pb)
prval pf_pixmap = view@(pb)
//
implement(env)
$OBJ.mesh_foreach_face_env$fwork<env> (pb, f, va, vb, vc) = {
//
  val ax = g0float2int ((va.x()+1.0f)*halfwidth)
  val ay = g0float2int ((va.y()+1.0f)*halfheight)
  val bx = g0float2int ((vb.x()+1.0f)*halfwidth)
  val by = g0float2int ((vb.y()+1.0f)*halfheight)
  val cx = g0float2int ((vc.x()+1.0f)*halfwidth)
  val cy = g0float2int ((vc.y()+1.0f)*halfheight)
//
  prval (pf_pixmap, fpf) = decode($vcopyenv_v(pf_pixmap))
//  
  val () = line (!p_pixmap, ax, ay, bx, by, white)
  val () = line (!p_pixmap, bx, by, cx, cy, white)
  val () = line (!p_pixmap, cx, cy, ax, ay, white)
//
  prval () = fpf (pf_pixmap)
} (* end of [mesh_foreach_face_env$fwork] *)
//
var env: int = 1
val () = $OBJ.mesh_foreach_face_env<int> (env, m)
//
prval () = view@(pb) := pf_pixmap
//
} (* end of [render] *)

(* ****** ****** *)

implement
main0 (argc, argv) = let
in
  if argc >= 3 then let
    val inp = fileref_open_exn (argv[1], file_mode_r)
    var mesh: $OBJ.mesh
    val res = $OBJ.load_OBJ (inp, mesh)
    val () = fileref_close (inp)
  in
    if :(mesh: $OBJ.mesh?) => res then let
      prval () = opt_unsome (mesh)
      val () = println!("successfully opened the input file")

      var pic: pixmap (0, 0)
      val () = $PM.pixmap_new<uint32> (pic, (i2sz)800, (i2sz)800, $UN.castvwtp0{uint32}(0))

      val () = render (mesh, pic)

      var p_pixmapbuf : ptr
      val (pf_pixmapbuf, pf_free_pixmapbuf | ()) = $PM.pixmap_delete_getbuf (pic, p_pixmapbuf)
      val out = fileref_open_exn (argv[2], file_mode_w)
      val () = $PPM.save_PPM (out, !p_pixmapbuf, (i2sz)800, (i2sz)800)
      val () = matrix_ptr_free {uint32} (pf_pixmapbuf, pf_free_pixmapbuf | p_pixmapbuf)

      val () = $OBJ.mesh_delete (mesh)
    in
    end else let
      prval () = opt_unnone (mesh)
      val () = println!("failure")
    in
    end
  end else let
    val () = println!(
    "not enough arguments! usage: ", argv[0],
    " <obj file> <ppm file>: write rendering of OBJ file to PPM"
    ) (* end of [val] *)
  in
    // nothing
  end // end of [if]
end // end of [main0]
