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
staload _ = "./../DATS/vec2i.dats"
staload _ = "./../DATS/vec3i.dats"
staload _ = "./../DATS/vec4f.dats"
staload "./../SATS/matrix.sats"
staload _ = "./../DATS/mat4x4f.dats"

staload "libats/libc/SATS/math.sats"
staload _ = "libats/libc/DATS/math.dats"

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

extern
fun{}
transform_point (&vec3f, &vec2i): bool

fun{}
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

#define INCH_TO_MM 25.4f

datatype FitResolutionGate = FRG_FILL | FRG_OVERSCAN
//
(*
Compute screen coordinates based on a physically-based camera model
http://www.scratchapixel.com/lessons/3d-basic-rendering/3d-viewing-pinhole-camera
*)
fun{}
compute_screen_coords (
    film_aperture_width: float,
    film_aperture_height: float,
    image_width: int,
    image_height: int,
    fit_film: FitResolutionGate,
    near_clip_plane: float, 
    focal_length: float,
    top: &float? >> float, bottom: &float? >> float, left: &float? >> float, right: &float? >> float
) : void = { 
  val film_aspect_ratio = film_aperture_width / film_aperture_height
  val device_aspect_ratio = g0int2float image_width / g0int2float image_height
  
  val () = top := ((film_aperture_height * INCH_TO_MM / 2) / focal_length) * near_clip_plane
  val () = right := ((film_aperture_width * INCH_TO_MM / 2) / focal_length) * near_clip_plane
 
  // field of view (horizontal)
  val fov = 2.0f * 180.0f / g0float2float(M_PI) * atan_float((film_aperture_width * INCH_TO_MM / 2.0f) / focal_length)
  val () = println!("horizontal field of view: ", fov)

  var xscale = 1.0f
  var yscale = 1.0f 

  val () =
    case+ :(xscale: float, yscale: float) => fit_film of 
    | FRG_OVERSCAN () => (
        if :(xscale: float, yscale: float) => film_aspect_ratio > device_aspect_ratio then
          yscale := film_aspect_ratio / device_aspect_ratio
        else
          xscale := device_aspect_ratio / film_aspect_ratio
      ) (* end of [FRG_OVERSCAN] *)
    | FRG_FILL () => (
        if :(xscale: float, yscale: float) => film_aspect_ratio > device_aspect_ratio then
          xscale := device_aspect_ratio / film_aspect_ratio
        else
          yscale := film_aspect_ratio / device_aspect_ratio 
      ) (* end of [FRG_FILL] *)
  // end of [val]
 
  val () = right := g0float_mul (right, xscale)
  val () = top := g0float_mul (top, yscale)
 
  val () = bottom := ~top
  val () = left := ~right
} (* end of [compute_screen_coords] *)

(*
Compute vertex raster screen coordinates. Vertices are defined in world space. They are then converted to camera space, then to NDC space (in the range [-1,1]) and then to raster space. The z-coordinates of the vertex in raster space is set with the z-coordinate of the vertex in camera space.
*)
fun
convert2raster( 
  vworld: &vec3f,
  world2camera: &mat4x4f,
  l: float, r: float, t: float, b: float,
  near: float,
  image_width: int, image_height: int,
  vraster: &vec3f? >> vec3f
): void = {
  var vworld4: vec4f
  val () = vworld4.init (vworld.x(), vworld.y(), vworld.z(), 1.0f)
  var vcamera = world2camera * vworld4
  
  var vscreen: vec2f 
  val () = vscreen.init (
    near * vcamera.x() / ~vcamera.z(),
    near * vcamera.y() / ~vcamera.z()
  ) (* end of [val] *)
 
  // now convert point from screen space to NDC space (in range [-1,1])
  var vndc: vec2f
  val () = vndc.init (
    2.0f * vscreen.x() / (r - l) - (r + l) / (r - l),
    2.0f * vscreen.y() / (t - b) - (t + b) / (t - b)
  ) (* end of [val] *)
 
  // convert to raster space
  val () = vraster.init (
    (vndc.x() + 1.0f) / 2.0f * g0int2float image_width,
    // in raster space y is down so invert direction
    (1.0f - vndc.y()) / 2.0f * g0int2float image_height,
    ~vcamera.z()
  ) (* end of [val] *)
} (* end of [convert2raster] *)

macdef
min3 (a, b, c) = min(,(a), min(,(b), ,(c)))
macdef
max3 (a, b, c) = max(,(a), max(,(b), ,(c)))

fun
edge_function (a: &vec3f, b: &vec3f , c: &vec3f): float =
  (c.x() - a.x()) * (b.y() - a.y()) - (c.y() - a.y()) * (b.x() - a.x())
// end of [edge_function]


//
extern
fun{}
rasterize_tri$raster (v: &vec3f, r: &vec3f? >> vec3f): void
extern
fun{}
rasterize_tri$image_width (): int
extern
fun{}
rasterize_tri$image_height (): int
extern
fun{}
rasterize_tri$depth_test (x: int, y: int, z: float): bool
extern
fun{}
rasterize_tri$fb_write (x: int, y: int, c: uint32): void

fun{}  
rasterize_tri (
  v0: &vec3f, v1: &vec3f, v2: &vec3f, st0: &vec2f, st1: &vec2f, st2: &vec2f
): void = let
  var v0r: vec3f
  and v1r: vec3f
  and v2r: vec3f

  // Convert the vertices of the triangle to raster space
  val () = rasterize_tri$raster (v0, v0r)
  val () = rasterize_tri$raster (v1, v1r)
  val () = rasterize_tri$raster (v2, v2r)

  // Precompute reciprocal of vertex z-coordinate
  val () = v0r.z (1.0f / v0r.z())
  val () = v1r.z (1.0f / v1r.z())
  val () = v2r.z (1.0f / v2r.z())

  // Prepare vertex attributes. Divide them by their vertex z-coordinate (though we use a multiplication here because v.z = 1 / v.z) 
  var st0: vec2f = v0r.z() * st0
  var st1: vec2f = v1r.z() * st1
  var st2: vec2f = v2r.z() * st2

  val xmin = min3 (v0r.x(), v1r.x(), v2r.x())
  val ymin = min3 (v0r.y(), v1r.y(), v2r.y())
  val xmax = max3 (v0r.x(), v1r.x(), v2r.x())
  val ymax = max3 (v0r.y(), v1r.y(), v2r.y())
in
//
// the triangle is out of screen
if xmin > (g0int2float)(rasterize_tri$image_width () - 1) ||
   xmax < 0.0f ||
   ymin > (g0int2float)(rasterize_tri$image_height () - 1) ||
   ymax < 0.0f then ()
else let
  // be careful xmin/xmax/ymin/ymax can be negative. Don't cast to uint32_t
  val x0 = max (0, g0float2int(floor(xmin)))
  val x1 = min (rasterize_tri$image_width () - 1, g0float2int(floor(xmax)))
  val y0 = max (0, g0float2int(floor(ymin)))
  val y1 = min (rasterize_tri$image_height () - 1, g0float2int(floor(ymax)))
        
  // what if area < 0? say a degenerate triangle?
  val area = edge_function (v0r, v1r, v2r)
in
  if area <= 0.0f then ()
  else let
    // inner loop
    // y0 <= y1
    // x0 <= x1
    // also:
    // 0 <= y0, y1 <= IMAGE_HEIGHT-1
    // 0 <= x0, x1 <= IMAGE_WIDTH-1
    var y: int = 0
    var x: int = 0
  in
    for (y := y0; y <= y1; y := succ(y)) (
      for (x := x0; x <= x1; x := succ(x)) (let
        var pixel_sample: vec3f
        val () = pixel_sample.init (g0int2float x + 0.5f, g0int2float y + 0.5f, 0.0f)

        var w0 = edge_function (v1r, v2r, pixel_sample)
        var w1 = edge_function (v2r, v0r, pixel_sample) 
        var w2 = edge_function (v0r, v1r, pixel_sample)             
      in
        if ~(w0 >= 0.0f && w1 >= 0.0f && w2 >= 0.0f) then () // pixel is outside the triangle
        else let
          val inv_area = 1.0f / area
          val () = w0 := w0 * inv_area
          val () = w1 := w1 * inv_area
          val () = w2 := w2 * inv_area
          val inv_z = v0r.z() * w0 + v1r.z() * w1 + v2r.z() * w2
          val z = 1.0f / inv_z
          // Depth-buffer test
          val rd = rasterize_tri$depth_test (x, y,z)
        in
          if ~rd then () // farther away, so occluded
          else {
            // divide the point coordinates by the vertex z-coordinate,
            // then interpolate using barycentric coordinates,
            // and finally, multiply by sample depth
            var st : vec2f
            // we use perspective-correct interpolation, so we need
            // to multiply the result of this interpolation by z,
            // the depth of the point of the triangle that the pixel overlaps
            val () = st.init (
              z * (w0 * st0.x() + w1 * st1.x() + w2 * st2.x()),
              z * (w0 * st0.y() + w1 * st1.y() + w2 * st2.y())
            ) (* end of [val] *)
                      
            // checkerboard pattern
            val p = c where {
               #define M 10
               val b0 = $UN.cast{uint} (fmod (M * st.x(), 1.0f) > 0.5f)
               val b1 = $UN.cast{uint} (fmod (M * st.y(), 1.0f) < 0.5f)
               val p = $UN.cast{int} (b0 lxor b1)
               // end of [p]
               val c = 0.3f * g0int2float (1 - p) + 0.7f * g0int2float p
               val c = $UN.cast{uint} (c * 255.0f)
               val c = (0xFFu << 24) lor (c << 16) lor (c << 8) lor c
               val c = $UN.cast{uint32}(c)
            } (* end of [val] *)
            val () = rasterize_tri$fb_write (x, y, p)
          } (* end of [if] *)
        end // end of [let]
      end))
  end // end of [let]
end
//
end // end of [rasterize_tri]
//

#define IMAGE_WIDTH 640
#define IMAGE_HEIGHT 480
 
#define NEAR_CLIP_PLANE 1.0f
#define FAR_CLIP_PLANE 1000.0f
#define FOCAL_LENGTH 20.0f // mm, was 20
// 35mm Full Aperture in inches
#define FILM_APERTURE_WIDTH 0.980f
#define FILM_APERTURE_HEIGHT 0.735f

fun
do_the_job (mesh: &mesh, filename: string): void = let
  var world2camera: mat4x4f
  val () = world2camera.init (
    0.707107f, ~0.331295f, 0.624695f, 0.0f,
    0.0f, 0.883452f, 0.468521f, 0.0f,
    ~0.707107f, ~0.331295f, 0.624695f, 0.0f,
    ~1.63871f, ~5.747777f, ~40.400412f, 1.0f
  ) (* end of [val] *)

  var camera2world : mat4x4f
  val-true = invert_mat4x4f_mat4x4f (world2camera, camera2world)
  prval () = opt_unsome {mat4x4f} (camera2world)
 
  // compute screen coordinates
  var t: float
  and b: float
  and l: float
  and r: float

  val () = 
    compute_screen_coords (
      FILM_APERTURE_WIDTH, FILM_APERTURE_HEIGHT,
      IMAGE_WIDTH, IMAGE_HEIGHT,
      FRG_OVERSCAN,
      NEAR_CLIP_PLANE,
      FOCAL_LENGTH,
      t, b, l, r
    )
  // end of [val]
 
  // define the frame-buffer and the depth-buffer. Initialize depth buffer
  // to far clipping plane.
  var framebuffer: pixmap (0, 0)
  val () = $PM.pixmap_new<uint32> (framebuffer, (i2sz)IMAGE_HEIGHT, (i2sz)IMAGE_WIDTH, $UN.castvwtp0{uint32}(0x0))
  var depthbuffer: $PM.pixmap (float, 0, 0)
  val () = $PM.pixmap_new<float> (depthbuffer, (i2sz)IMAGE_HEIGHT, (i2sz)IMAGE_WIDTH, FAR_CLIP_PLANE)
  //
  val top = t
  val bottom = b
  val left = l
  val right = r
  val p_world2camera = addr@world2camera
  prval pf_world2camera = view@world2camera
  implement
  rasterize_tri$raster<> (v, r) = {
    prval (pf_world2camera, fpf_world2camera) = decode($vcopyenv_v(pf_world2camera))
    val () = convert2raster (v, !p_world2camera, left, right, top, bottom, NEAR_CLIP_PLANE, IMAGE_WIDTH, IMAGE_HEIGHT, r)
    prval () = fpf_world2camera (pf_world2camera)
  } (* end of [rasterize_tri$raster] *)
  prval () = view@world2camera := pf_world2camera
  implement
  rasterize_tri$image_width<> () = IMAGE_WIDTH
  implement
  rasterize_tri$image_height<> () = IMAGE_HEIGHT
  //
  val p_framebuffer = addr@(framebuffer)
  prval pf_framebuffer = view@(framebuffer)
  val p_depthbuffer = addr@(depthbuffer)
  prval pf_depthbuffer = view@(depthbuffer)
  //
  implement
  rasterize_tri$depth_test<> (x: int, y: int, z: float): bool = let
    val x = (g1ofg0)x
    val y = (g1ofg0)y
  in
    if x < 0 then false
    else if x >= IMAGE_WIDTH then false
    else if y < 0 then false
    else if y >= IMAGE_HEIGHT then false
    else let
      prval (pf_depthbuffer, fpf_depthbuffer) = decode($vcopyenv_v(pf_depthbuffer))
      val z1 = $PM.pixmap_get_at_int (!p_depthbuffer, y, x)
      val res = z < z1
      val () = if res then $PM.pixmap_set_at_int (!p_depthbuffer, y, x, z)
      prval () = fpf_depthbuffer (pf_depthbuffer)
    in
      res
    end
  end // end of [rasterize_tri$depth_test]
  implement
  rasterize_tri$fb_write<> (x, y, c) = {
    prval (pf_framebuffer, fpf_framebuffer) = decode($vcopyenv_v(pf_framebuffer))
    val () = $PM.pixmap_set_at_int2 (!p_framebuffer, y, x, c)
    prval () = fpf_framebuffer (pf_framebuffer)
  } (* end of [rasterize_tri$fb_write] *)
  //
  implement(env)
  $OBJ.mesh_foreach_gface_env$fwork<env> (pb, f, va, vb, vc, na, nb, nc, ta, tb, tc) = {
    //
    val () = rasterize_tri (va, vb, vc, ta, tb, tc)
    //
  } (* end of [mesh_foreach_gface_env$fwork] *)
//
  var env: int = 1
  val () = $OBJ.mesh_foreach_gface_env<int> (env, mesh)
//
  prval () = view@(framebuffer) := pf_framebuffer
  prval () = view@(depthbuffer) := pf_depthbuffer
//
        
  val () = $PM.pixmap_delete (depthbuffer) // TODO: print it?
  var p_framebuf : ptr
  val (pf_framebuf, pf_free_framebuf | ()) = $PM.pixmap_delete_getbuf (framebuffer, p_framebuf)
  val out = fileref_open_exn (filename, file_mode_w)
  val () = $PPM.save_PPM (out, !p_framebuf, (i2sz)IMAGE_HEIGHT, (i2sz)IMAGE_WIDTH)
  val () = matrix_ptr_free {uint32} (pf_framebuf, pf_free_framebuf | p_framebuf)

in
end // end of [...]

(* ****** ****** *)

implement
main0 (argc, argv) = let
  // from object to world to camera space
  // then from camera space to image/screen space (camera space with persp divide)
  // from screen space to NDC(normalized device coordinates) with clipping
  // from NDC to raster space (multiply by pixel width/height; take floor)
//
(*
  var camera2world : mat4x4f
  val () = camera2world.init( 0.871214f, 0.0f, ~0.490904f, 0.0f, ~0.192902f, 0.919559f, ~0.342346f, 0.0f, 0.451415f, 0.392953f, 0.801132f, 0.0f, 14.777467f, 29.361945f, 27.993464f, 1.0f)
  var world2camera : mat4x4f
  val-true = invert_mat4x4f_mat4x4f (camera2world, world2camera)
  prval () = opt_unsome {mat4x4f} (world2camera)
    
  val canvas_width = 2.0f
  val canvas_height = 2.0f
  val image_width = 512
  val image_height = 512

  var pWorld: vec4f
  val () = pWorld.init (0.0f, 39.034f, 0.0f, 1.0f)

  // The 2D pixel coordinates of pWorld in the image if the point is visible
  var pRaster: vec2i
  val () = pRaster.init (0, 0)
  val () =
    if computePixelCoordinates (pWorld, world2camera, canvas_width, canvas_height, image_width, image_height, pRaster) then {
       val () = println!("pixel coordinates: ", pRaster)
    }
    else {
       val () = println!(pWorld, " is not visible")
    }
//
// next: clipping triangles and stuff
//
*)
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
      
      val () = do_the_job (mesh, argv[2])
      (*
      var pic: pixmap (0, 0)
      val () = $PM.pixmap_new<uint32> (pic, (i2sz)800, (i2sz)800, $UN.castvwtp0{uint32}(0))

      val () = render (mesh, pic)

      var p_pixmapbuf : ptr
      val (pf_pixmapbuf, pf_free_pixmapbuf | ()) = $PM.pixmap_delete_getbuf (pic, p_pixmapbuf)
      val out = fileref_open_exn (argv[2], file_mode_w)
      val () = $PPM.save_PPM (out, !p_pixmapbuf, (i2sz)800, (i2sz)800)
      val () = matrix_ptr_free {uint32} (pf_pixmapbuf, pf_free_pixmapbuf | p_pixmapbuf)
      *)

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
