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
staload _ = "./../src/DATS/vec2f.dats"
staload _ = "./../src/DATS/vec3f.dats"

vtypedef pixmap (m:int, n:int) = $PM.pixmap (uint32, m, n)

(* ****** ****** *)

absvt0ype ybuffer (a:t0p, m:int) = (size_t, ptr)
typedef ybuffer0 (a:t0p) = ybuffer (a, 0)?

extern
fun{a:t0p}
ybuffer$min (INV(a), a): a

extern
fun{a:t0p}
ybuffer_init {m:int} (
  ybuf: &ybuffer0(a) >> ybuffer(a, m)
, m: size_t(m), INV(a)
): void
extern
fun{a:t0p}
ybuffer_free {m:int} (&ybuffer(a, m) >> ybuffer0(a)): void
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
ybuffer_init {m} (buf, m, x) = {
  val (pf_arr, pf_mem | p) = array_ptr_alloc<a> (m)
  implement
  array_initize$init<a> (_, r) = (r := x)
  val () = array_initize<a> (!p, m)
  val () = buf := (pf_arr, pf_mem | m, p)
}

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

extern
fun{a:t@ype}
pixmap_compare_size {m1,n1,m2,n2:int} (
  & $PM.pixmap (INV(a), m1, n1)
, & $PM.pixmap (a, m2, n2)
) : bool (m1 == m2 && n1 == n2)

implement{a}
pixmap_compare_size (p1, p2) = let
  val m1 = $PM.pixmap_get_width<a> (p1)
  val m2 = $PM.pixmap_get_width<a> (p2)
  val n1 = $PM.pixmap_get_height<a> (p1)
  val n2 = $PM.pixmap_get_height<a> (p2)
in
  (m1 = m2) * (n1 = n2)
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

implement
heightmap_init (color, height, hm) = let
  val b_rcm = pixmap_of_TGA (color, hm.colormap)
in
  if b_rcm then let
    prval () = opt_unsome (hm.colormap)
    val b_rhm = pixmap_of_TGA (height, hm.heightmap)
  in
    if b_rhm then let
      prval () = opt_unsome (hm.heightmap)
      val ok = pixmap_compare_size<uint32> (hm.heightmap, hm.colormap)
    in
      if ok then let
        prval [m:int] EQINT () = eqint_make_guint (
	  $effmask_all ($PM.pixmap_get_width (hm.heightmap))
	)
        prval [n:int] EQINT () = eqint_make_guint (
	  $effmask_all ($PM.pixmap_get_height (hm.heightmap))
	)
        prval () = opt_some {heightmap (m, n)} (hm)
      in
        true
      end else let
        val () = $PM.pixmap_delete<uint32> (hm.colormap)
        val () = $PM.pixmap_delete<uint32> (hm.heightmap)
	val () = println!("heightmap and colormap must have the same dimensions")
	prval () = opt_none {heightmap (0, 0)} (hm)
      in
        false
      end
    end else let
      prval () = opt_unnone (hm.heightmap)
      val () = println!("failed to load heightmap: ", height)
      val () = $PM.pixmap_delete<uint32> (hm.colormap)
      val () = hm.heightmap := $UN.castvwtp0 {pixmap (0, 0)?} (hm.heightmap)
      prval () = opt_none {heightmap (0, 0)} (hm)
    in
      false
    end
  end else let
    // failed to load colormap
    prval () = opt_unnone (hm.colormap)
    val () = println!("failed to load colormap: ", color)
    val () = hm.heightmap := $UN.castvwtp0 {pixmap (0, 0)?} (hm.heightmap)
    val () = hm.colormap := $UN.castvwtp0 {pixmap (0, 0)?} (hm.colormap)
    prval () = opt_none {heightmap (0, 0)} (hm)
  in
    false
  end
end

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
  val sw = i2f (sz2i(screen_width))
  val () =
     while* (dz: float, z: float):
     	    (dz: float, z: float) =>
	    (z < dist) {
       var pleft: vec2f
       and pright: vec2f
       val () = pleft.init (
          (~cosphi * z - sinphi * z) + i2f(cam.pos.x()),
	  ( sinphi * z - cosphi * z) + i2f(cam.pos.y())
       ) (* end of [val] *)
       val () = pright.init (
	 ( cosphi * z - sinphi * z) + i2f(cam.pos.x()),
	 (~sinphi * z - cosphi * z) + i2f(cam.pos.y())
       ) (* end of [val] *)
       var dv: vec2f
       val () = dv.init (
         (pright.x() - pleft.x()) / sw,
	 (pright.y() - pleft.y()) / sw
       )

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
	 val px = f2i(pleft.x())
	 val py = f2i(pleft.y())
         val ph = $PM.pixmap_get_at_int2 (hmap.heightmap, px, py)
	 // NOTE: we need this because the TGA code will output
	 // it with all channels
	 val ph = ph land $UN.castvwtp0{uint32}(0xFFu)
	 val ph = g0uint2int(ph)

	 val h = f2i (
	   i2f(cam.height - ph) / z * cam.scale_height
	 ) + cam.horizon
	 val () = println!("h = ", h)

         val col = $PM.pixmap_get_at_int2 (hmap.colormap, px, py)
  
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

(* ****** ****** *)

vtypedef raster_state (a:t@ype, b:t@ype, m:int, n:int) = @{
  framebuf= $PM.pixmap (a, m, n)
, depthbuf= $PM.pixmap (b, m, n)
}

extern
fun{a,b:t@ype}
raster_state_init {m,n:pos} (
  &raster_state (a, b, 0, 0)? >> raster_state (a, b, m, n)
, int m, int n
, a, b
): void
extern
fun{b:t@ype}
raster_state_flush {m,n:pos} (
  &raster_state (uint32, b, m, n) >> raster_state (uint32, b, 0, 0)?
, string
): void

implement{a,b}
raster_state_init (rst, width, height, a, b) = {
  //
  val () = $PM.pixmap_new<a> (rst.framebuf, (i2sz)width, (i2sz)height, a)
  val () = $PM.pixmap_new<b> (rst.depthbuf, (i2sz)width, (i2sz)height, b)
  //
} (* end of [raster_state_init] *)

implement{b}
raster_state_flush (rst, filename) = {
  (*
  var p_depthbuf: ptr
  val (pf_depthbuf, pf_free_depthbuf | ()) = $PM.pixmap_delete_getbuf (depthbuffer, p_depthbuf)
  val filename_depth = string_append (filename, ".depth.pgm")
  val filename_depth = strptr2string filename_depth
  val out_z = fileref_open_exn (filename_depth, file_mode_w)
  val () = $PPM.save_PGM (out_z, !p_depthbuf, (i2sz)IMAGE_HEIGHT, (i2sz)IMAGE_WIDTH)
  val () = matrix_ptr_free {uint8} (pf_depthbuf, pf_free_depthbuf | p_depthbuf)
  val () = fileref_close (out_z)
  *)
  val () = $PM.pixmap_delete (rst.depthbuf)
  //
  val width = $PM.pixmap_get_width (rst.framebuf)
  val height = $PM.pixmap_get_height (rst.framebuf)
  //
  var p_framebuf : ptr
  val (pf_framebuf, pf_free_framebuf | ()) = $PM.pixmap_delete_getbuf (rst.framebuf, p_framebuf)
  val out = fileref_open_exn (filename, file_mode_w)
  val () = $PPM.save_PPM (out, !p_framebuf, height, width)
  val () = matrix_ptr_free {uint32} (pf_framebuf, pf_free_framebuf | p_framebuf)
  val () = fileref_close (out)
  //
} (* end of [raster_state_flush] *)

(* ****** ****** *)

implement
main0 (argc, argv) = let
in
  if argc >= 4 then let
    var hm : heightmap0
    val filename = argv[3]
    var res = heightmap_init (argv[1], argv[2], hm)
  in
    if res then let
      prval () = opt_unsome (hm)
      //
      val () = println!("successfully loaded the heightmap")
      //
      #define IMAGE_WIDTH 800
      #define IMAGE_HEIGHT 600
      var rastate: raster_state (uint32, float, 0, 0)
      val () = raster_state_init (
        rastate,
	IMAGE_WIDTH, IMAGE_HEIGHT,
	$UN.castvwtp0{uint32}(0),
	$UN.cast{float}(0.0f)
      )
      //
      var ybuf: ybuffer0 (int)
      val () = ybuffer_init<int> (ybuf, (i2sz)IMAGE_WIDTH, IMAGE_HEIGHT)
      var eye: vec2i
      val () = eye.init (128, 128)
      var cam: camera = @{
        pos = eye
      , height= 120
      , phi= 45.0f
      , distance= 300.0f
      , horizon= 120
      , scale_height= 120.0f
      } (* end of [cam] *)
      val () = render (hm, ybuf, cam, rastate.framebuf)
      val () = println!("done")

      val () = ybuffer_free (ybuf)      
      //
      val () = raster_state_flush (rastate, filename)
      //
      val () = heightmap_free (hm)
      val () = hm := $UN.castvwtp0 {heightmap0?} (hm)
    in
      
    end else let
      prval () = opt_unnone (hm)
      val () = hm := $UN.castvwtp0 {heightmap0?} (hm)
    in
    end
  end else let
    val () = println!(
    "not enough arguments! usage: ", argv[0], " <colormap tga file> <heightmap tga file> <output file>"
    ) (* end of [val] *)
  in
    // nothing
  end // end of [if]
end
