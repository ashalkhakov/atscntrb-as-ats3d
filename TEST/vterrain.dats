#include
"share/atspre_staload.hats"

staload STDIO = "libats/libc/SATS/stdio.sats"

staload UN = "prelude/SATS/unsafe.sats"

staload _ = "prelude/DATS/integer.dats"

staload FU = "./../src/SATS/file_util.sats" // opt_unsome

staload TGA = "./../src/SATS/image/TGA.sats"
staload PPM = "./../src/SATS/image/PPM.sats"

staload "libats/libc/SATS/math.sats" // NOTE: requires -lm during compilation
staload _ = "libats/libc/DATS/math.dats"

staload "./../src/SATS/sampler.sats"
staload _ = "./../src/DATS/sampler.dats"

staload PM = "./../src/SATS/image/pixmap.sats"
staload _ = "./../src/DATS/image/pixmap.dats"

staload "./../src/SATS/vector.sats"
staload _ = "./../src/DATS/vec2i.dats"
staload _ = "./../src/DATS/vec3f.dats"

vtypedef pixmap (m:int, n:int) = $PM.pixmap (uint32, m, n)

(* ****** ****** *)

extern
fun{a:t0p}
ybuffer$min (INV(a), a): a
absvt0ype ybuffer (a:t0p, m:int) = (size_t, ptr)
extern
fun{a:t0p}
ybuffer_init {m:int} (m: size_t(m), INV(a)): ybuffer(a, m)
extern
fun{a:t0p}
ybuffer_free {m:int} (ybuffer(a, m)): void
extern
fun{a:t0p}
ybuffer_clear {m:int} (&ybuffer(INV(a), m), a): void
extern
fun{a:t0p}
ybuffer_get {m:int} (&ybuffer(INV(a), m), i: sizeLt(m)): a
extern
fun{a:t0p}
ybuffer_update {m:int} (&ybuffer(INV(a), m), i: sizeLt(m), a): void

local

assume ybuffer (a:t0p, m:int) = [l:addr] (
  array_v (a, l, m), mfree_gc_v (l)
| size_t(m), ptr(l)
)

in // in of [local]

implement{a}
ybuffer_init {m} (m, x) = let
  val (pf_arr, pf_mem | p) = array_ptr_alloc<a> (m)
  implement
  array_initize$init<a> (_, r) = (r := x)
  val () = array_initize<a> (!p, m)
in
  (pf_arr, pf_mem | m, p)
end

implement{a}
ybuffer_free {m} (buf) = {
  val (pf_arr, pf_mem | m, p) = buf
  implement
  array_uninitize<a>(i, v) = ()
  val () = array_uninitize (!p, m)
  val () = array_ptr_free (pf_arr, pf_mem | p)
}

implement{a}
ybuffer_clear {m} (buf, x) = {
  val (pf_arr, pf_mem | m, p) = buf
  implement
  array_initize$init<a> (_, v) = (v := x)
  val () = array_initize<a> (!p, m)
  val () = buf := (pf_arr, pf_mem | m, p)
}

implement{a}
ybuffer_get {m} (buf, i) = let
  val (pf_arr, pf_mem | m, p) = buf
  val res = array_get_at_guint (!p, i)
  val () = buf := (pf_arr, pf_mem | m, p)  
in
  res
end

implement{a}
ybuffer_update {m} (buf, i, x) = {
  val (pf_arr, pf_mem | m, p) = buf
  val res = array_get_at_guint (!p, i)
  val res = ybuffer$min<a> (res, x)
  val () = array_set_at_guint (!p, i, res)
  val () = buf := (pf_arr, pf_mem | m, p)  
}

end // end of [local]

(* ****** ****** *)

extern
fun
pixmap_of_TGA (
  path: string
, res: &pixmap (0, 0)? >> opt (pixmap (m, n), b)
): #[b:bool;m,n:int] bool b

implement
pixmap_of_TGA (path, res) = let
  val inp = fileref_open_opt (path, file_mode_r)
in
  case+ inp of
  | ~None_vt () => let
      val () = println!("pixmap_of_TGA: unable to open file at ", path)
      prval () = opt_none (res)
    in
      false
    end
  | ~Some_vt inp => let
      var w: size_t(0)
      var h: size_t(0)
      var p_mat: ptr(null)
      val (pf | r) = $TGA.load_TGA (inp, w, h, p_mat)
    in
      if r then let
      	prval Some_v @(pf_mat, pf_free) = pf
        prval () = $FU.opt_unsome_mac (w, h, p_mat)
	prval [l:addr] EQADDR () = eqaddr_make_ptr (p_mat)
        prval [w:int] EQINT () = eqint_make_guint (w)
        prval [h:int] EQINT () = eqint_make_guint (h)
        val () = fileref_close (inp)
        prval () = lemma_g1uint_param (w)
        prval () = lemma_g1uint_param (h)

        val () = assert_errmsg (w > (i2sz)0, "width is 0!")
        val () = assert_errmsg (h > (i2sz)0, "height is 0!")

	val () = $PM.pixmap_new_matrix<uint32> (pf_mat, pf_free | res, h, w, p_mat)
	val () = w := (i2sz)0
	val () = h := (i2sz)0
	val () = p_mat := the_null_ptr
	prval () = opt_some (res)
      in
        true
      end else let
        prval None_v () = pf
        prval () = opt_unnone (w)
        and () = opt_unnone (h)
        and () = opt_unnone (p_mat)
	prval () = opt_none (res)
        val () = w := (i2sz)0
        val () = h := (i2sz)0
        val () = p_mat := the_null_ptr
      in
        false
      end
    end
end

(* ****** ****** *)

vtypedef heightmap (m: int, n:int) = @{
  colormap= pixmap (m, n)
, heightmap= pixmap (m, n)
}
vtypedef heightmap = [m,n:int] heightmap (m, n)
vtypedef heightmap0 = heightmap (0, 0)?

extern
fun
heightmap_init (
  color: string
, height: string
, h: &heightmap0? >> opt (heightmap, b)
) : #[b:bool] bool b

extern
fun
heightmap_free (
  h: &heightmap >> heightmap0?
): void

(*
implement
heightmap_init (color, height, hm) = let
  val rcm = pixmap_of_TGA (co
pixmap_of_TGA (
  path: string
, res: &pixmap (0, 0)? >> opt (pixmap (m, n), b)
): #[b:bool;m,n:int] bool b
  val rcm = pixmap_of_TGA (
  val cm = pixmap_of_TGA (
  val inp = fileref_open_opt (path, file_mode_r)
  val inp = fileref_open_opt (color, file_mode_r)
in
  case+ inp of
  | ~None_vt () => let
      val () = println!("heightmap_init: unable to open file at ", inp)
      prval () = opt_none{
end*)

implement
heightmap_free (h) = {
  val () = $PM.pixmap_delete<uint32> (h.colormap)
  val () = $PM.pixmap_delete<uint32> (h.heightmap)
}

(* ****** ****** *)

(*
create two samplers (one for heightmap, one for colormap)
specify camera position (x/y pos, height, horizon, scale for height, zfar distance, screen width/height)
create a pixmap for output
*)

typedef camera = @{
  pos= vec2i
, height= int
, phi= float
, distance= float
, horizon= int
, scale_height= float
}

extern
fun
draw_vert_line {m,n:int} (s: &pixmap (m, n), x: int, y0: int, y1: int, c: uint32): void
implement
draw_vert_line {m,n} (s, x, y0, y1, c) = {
  var y: int
  val () =
    for (y := y0; y <= y1; y := y + 1) begin
      $PM.pixmap_set_at_int2 (s, x, y, c)
    end
} (* end of [draw_vert_line] *)

implement
ybuffer$min<int> (x, y) = min (x, y)

fun
render {n,m:int} (
  hmap: &heightmap >> _,
  ybuf: &ybuffer(int, n) >> _,
  cam: &camera,
  screen: &pixmap (n, m) >> _
): void = let
  #define f2i g0float2int
  #define i2f g0int2float

  val sinphi = sin(cam.phi)
  val cosphi = cos(cam.phi)

  val screen_width = $PM.pixmap_get_width (screen)
  val () = assertloc(isgtz(screen_width))
  val screen_height = $PM.pixmap_get_height (screen)
  val () = assertloc(isgtz(screen_height))

  val () = ybuffer_clear (ybuf, sz2i(screen_height))

  var dz = 1.0f
  var z = 1.0f
  val dist = cam.distance
  val sw = sz2i(screen_width)
  val () =
     while* (dz: float, z: float):
     	    (dz: float, z: float) =>
	    (z < dist) {
       var pleft: vec2i
       and pright: vec2i
       val () = pleft.init (
          f2i(~cosphi * z - sinphi * z) + cam.pos.x(),
	  f2i( sinphi * z - cosphi * z) + cam.pos.y()
       ) (* end of [val] *)
       val () = pright.init (
	 f2i( cosphi * z - sinphi * z) + cam.pos.x(),
	 f2i(~sinphi * z - cosphi * z) + cam.pos.y()
       ) (* end of [val] *)
       var dv: vec2i
       val () = dv.init ((pright.x() - pleft.x()) / sw, (pright.y() - pleft.y()) / sw)

       // for every column on screen
       var i: size_t
       val () =
       for* {i:nat | i <= n}
       	    (i: size_t(i)):
       	    (i: size_t) =>
	    (i := (i2sz)0; i < screen_width; i := succ(i))
       {
         implement
	 $PM.pixmap_get_at_int2$default<uint32> () = $UN.castvwtp0{uint32}(0)
         val ph = $PM.pixmap_get_at_int2 (hmap.heightmap, pleft.x(), pleft.y())
	 val ph = g0uint2int(ph)
         val h = f2i (i2f(cam.height - ph) / z * cam.scale_height) + cam.horizon
         val col = $PM.pixmap_get_at_int2 (hmap.colormap, pleft.x(), pleft.y())
  
         val oh = ybuffer_get (ybuf, i)
	 val i0 = g1uint2int(i)
         val () = draw_vert_line (screen, (g0ofg1)i0, h, oh, col)
         val () = ybuffer_update (ybuf, i, h)
         val () = pleft := pleft + dv
       }
       val () = z := z + dz
       val () = dz := dz + 0.2f
  } (* end of [val] *)
in
end

implement
main0 (argc, argv) = let
in
  if argc >= 2 then let
    val inp = fileref_open_exn (argv[1], file_mode_r)
    var w: size_t(0)
    var h: size_t(0)
    var p_mat: ptr(null)
    val (pf_res | res) = $TGA.load_TGA (inp, w, h, p_mat)
  in
    if res then let
      prval Some_v @(pf_mat, pf_gc) = pf_res
      prval () = $FU.opt_unsome_mac (w, h, p_mat)
      prval [l:addr] EQADDR () = eqaddr_make_ptr (p_mat)
      prval [w:int] EQINT () = eqint_make_guint (w)
      prval [h:int] EQINT () = eqint_make_guint (h)
      val () = println!("open: success! width: ", w, ", height: ", h)
      //
      val () =
        if argc >= 3 then let
          val out = fileref_open_exn (argv[2], file_mode_w)      
          val () = $PPM.save_PPM (out, !p_mat, w, h)
        in
          fileref_close (out)
        end // end of [if]
      //
      prval pf_arr = matrix2array_v {uint32}{l}{w,h} (pf_mat)
      prval () = topize {@[uint32][w*h]} (!p_mat)
      prval pf_mat = array2matrix_v {uint32?}{l}{w,h} (pf_arr)
      val () = matrix_ptr_free (pf_mat, pf_gc | p_mat)
      //
      val () = w := (i2sz)0
      val () = h := (i2sz)0
      val () = p_mat := the_null_ptr
      //
    in
      fileref_close (inp)
    end else let
      prval None_v () = pf_res
      prval () = $FU.opt_clear_mac (w, h, p_mat)
      val () = println!("open: failed")
      val () = w := (i2sz)0
      val () = h := (i2sz)0
      val () = p_mat := the_null_ptr
    in
      fileref_close (inp)
    end
  end else let
    val () = println!(
    "not enough arguments! usage: ", argv[0], " <tga file> [<ppm file>] (optionally specify where to save the converted data"
    ) (* end of [val] *)
  in
    // nothing
  end // end of [if]
end
