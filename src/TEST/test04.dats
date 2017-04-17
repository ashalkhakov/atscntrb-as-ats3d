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
staload _ = "prelude/DATS/float.dats"

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

macdef
min3 (a, b, c) = min(,(a), min(,(b), ,(c)))
macdef
max3 (a, b, c) = max(,(a), max(,(b), ,(c)))

fun
barycentric (A: &vec2f, B: &vec2f, C: &vec2f, P: &vec2f, res: &vec3f? >> vec3f): void = {    
  var s: vec3f and t: vec3f
  val () = t.init (C.y()-A.y(), B.y()-A.y(), A.y()-P.y())
  val () = s.init (C.x()-A.x(), B.x()-A.x(), A.x()-P.x())
  var u = crossprod (s, t)
  val () =
    // backface culling
    if :(res: vec3f) => u.z() > 1e-2f then
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
fun{env:vt@ype}
triangle$fragment (
  env: &(@[env][3]) >> _
, &vec3f
, &uint32? >> opt (uint32, b)
): #[b:bool] bool (b)

fun{env:vt@ype}
triangle {m,n:int} (
  env: &(@[env][3]) >> _
, a: &vec4f, b: &vec4f, c: &vec4f
, image: &$PM.pixmap (uint32, m, n)
, zbuffer: &$PM.pixmap (uint8, m, n)
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
  val xmin = max (xmin, 0)
  prval [xmin:int] EQINT () = eqint_make_gint (xmin)
  val xmax = min (xmax, width-1)
  prval [xmax:int] EQINT () = eqint_make_gint (xmax)
  val ymin = max (ymin, 0)
  prval [ymin:int] EQINT () = eqint_make_gint (ymin)
  val ymax = min (ymax, height-1)
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
      val frag_depth = max(0, min(255, g0float2int (z / w + 0.5f)))
      val frag_depth = $UN.cast{uint8}(frag_depth)
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

(* ****** ****** *)

typedef gl_state = @{
  mvp= mat4x4f (* model-view-projection matrix *)
, viewport= mat4x4f (* viewport matrix *)
, light_dir= vec3f
} (* end of [gl_state] *)

(* ****** ****** *)

extern
fun{env:vt@ype}{v:t@ype}
shader_vert (state: &gl_state, &(env)? >> env, vert: &v): vec4f
extern
fun{env:vt@ype}
shader_frag (
  &gl_state
, &(@[env][3]) >> _
, bar: &vec3f
, color: &uint32? >> opt (uint32, b)
): #[b:bool] bool(b)

(* ****** ****** *)
// Gouraud shader

abst@ype gouraud_shader = float
typedef gouraud_vert = @{pos= vec3f, norm= vec3f}
local

assume gouraud_shader = float

in // in of [local]

implement
shader_vert<gouraud_shader><gouraud_vert> (state, varying, v) = let
  var gl_Vertex: vec4f
  val () = gl_Vertex.init (v.pos.x(), v.pos.y(), v.pos.z(), 1.0f)
  (*
  val () = println!("V = ", gl_Vertex)
  *)
  var gl_Vertex' = state.mvp * gl_Vertex
  (*
  val () = println!("MVP * V = ", gl_Vertex')
  *)
  var gl_Vertex'' = state.viewport * gl_Vertex'
  (*
  val () = println!("VIEWPORT * MVP * V = ", gl_Vertex'')
  *)
  val () = varying := min (1.0f, max (0.0f, dotprod (v.norm, state.light_dir)))
in
  gl_Vertex''
end

implement
shader_frag<gouraud_shader> (
  state, varying, bar, color
) = let
  // NOTE: something with the fragment shader
  val intensity = varying.[0] * bar.x() + varying.[1] * bar.y() + varying.[2] * bar.z()
  val intensity = max(0.0f, min(1.0f, intensity))
  val c = $UN.cast{int} (255.0f * intensity)
  val c = $UN.cast{uint} (c)
  (*
  val () = println!("intens = ", c)
  *)
  val c = (0xFFu << 24) lor (c << 16) lor (c << 8) lor c
  val c = $UN.cast{uint32}(c)
  val () = color := c
  //val () = color := $UN.cast{uint32} (0xFFFFFFFF)
  prval () = opt_some {uint32} (color)
in
  true
end // end of [shader_frag]

end // end of [local]

extern
fun{env:vt@ype}{v:t@ype}
shader_vert_prf {l_env,l_scrn: addr} (
  pf_env: !env? @ l_env >> env @ l_env
, pf_scrn: !vec4f? @ l_scrn >> vec4f @ l_scrn
| state: &gl_state
, env: ptr l_env
, vert: &v
, scrn: ptr l_scrn
): void

implement{env}{v}
shader_vert_prf {l_env,l_scrn} (
  pf_env, pf_scrn
| gl_state, p_env, vert, p_scrn
) = !p_scrn := shader_vert<env><v> (gl_state, !p_env, vert)


(* ****** ****** *)

fun
mesh_rasterize {m,n:int} (
  gl_state: &gl_state
, mesh: &mesh
, framebuffer: &$PM.pixmap (uint32, m, n)
, depthbuffer: &$PM.pixmap (uint8, m, n)
): void = let
  //
  val p_framebuffer = addr@(framebuffer)
  prval pf_framebuffer = view@(framebuffer)
  val p_depthbuffer = addr@(depthbuffer)
  prval pf_depthbuffer = view@(depthbuffer)
  val p_gl_state = addr@(gl_state)
  prval pf_gl_state = view@(gl_state)
  //
  implement
  $OBJ.mesh_foreach_gface_env$fwork<gl_state> (gl_state, f, va, vb, vc, na, nb, nc, ta, tb, tc) = {
    //
    // TODO: move this out to mesh loading!
    var ga: gouraud_vert
    val () = ga.pos := va
    val () = ga.norm := na
    var gb: gouraud_vert
    val () = gb.pos := vb
    val () = gb.norm := nb
    var gc: gouraud_vert
    val () = gc.pos := vc
    val () = gc.norm := nc
    //
    typedef V = gouraud_shader
    typedef V3 = @[V][3]
    typedef S = vec4f
    //
    var varyings: V3
    var scrn0: S
    and scrn1: S
    and scrn2: S
    //
    prval pf_var = view@(varyings)
    prval [lpvar:addr] EQADDR () = eqaddr_make_ptr (addr@(varyings))
    var pvar = addr@(varyings)
    //
    prval (pf1_at_var, pf1_var) = array_v_uncons {V?} (pf_var)
    val () = shader_vert_prf<V><gouraud_vert> (pf1_at_var, view@scrn0 | gl_state, pvar, ga, addr@scrn0)
    //
    val () = pvar := ptr1_succ<V> (pvar)
    //
    prval (pf2_at_var, pf2_var) = array_v_uncons {V?} (pf1_var)
    val () = shader_vert_prf<V><gouraud_vert> (pf2_at_var, view@scrn1 | gl_state, pvar, gb, addr@scrn1)
    //
    val () = pvar := ptr1_succ<V> (pvar)
    //
    prval (pf3_at_var, pf3_var) = array_v_uncons {V?} (pf2_var)
    val () = shader_vert_prf<V><gouraud_vert> (pf3_at_var, view@scrn2 | gl_state, pvar, gc, addr@scrn2)
    //
    #define :: array_v_cons
    //
    prval pf3_nil_var = array_v_unnil_nil (pf3_var)
    prval () = view@(varyings) := pf1_at_var :: pf2_at_var :: pf3_at_var :: pf3_nil_var
    //
    implement
    triangle$fragment<V> (env, pos, color) = res where {
      prval (pf_gls, fpf_gls) = decode($vcopyenv_v(pf_gl_state))
      val res = shader_frag<V> (!p_gl_state, env, pos, color)
      prval () = fpf_gls (pf_gls)
    } (* end of [triangle$fragment] *)
    //
    prval (pf_framebuffer, fpf_framebuffer) = decode($vcopyenv_v(pf_framebuffer))
    prval (pf_depthbuffer, fpf_depthbuffer) = decode($vcopyenv_v(pf_depthbuffer))
    val () = triangle<V> (varyings, scrn0, scrn1, scrn2, !p_framebuffer, !p_depthbuffer)
    prval () = fpf_framebuffer (pf_framebuffer)
    prval () = fpf_depthbuffer (pf_depthbuffer)
    //
  } (* end of [mesh_foreach_gface_env$fwork] *)
//
  val () = $OBJ.mesh_foreach_gface_env<gl_state> (gl_state, mesh)
//
  prval () = view@(framebuffer) := pf_framebuffer
  prval () = view@(depthbuffer) := pf_depthbuffer
  prval () = view@(gl_state) := pf_gl_state
//
in
end // end of [...]

(* ****** ****** *)

// a matrix for converting values in NDC (all axes range -1 to 1)
// to fit into a rendering screen
fun
mat4x4f_viewport (x: int, y: int, w: int, h: int): mat4x4f = let
  var res: mat4x4f
  val () = res.identity ()
  val () = res[0,3] := g0int2float (x+w/2)
  val () = res[1,3] := g0int2float (y+h/2)
  val () = res[2,3] := 255.0f/2.0f
  val () = res[0,0] := g0int2float (w/2)
  val () = res[1,1] := g0int2float (h/2)
  val () = res[2,2] := 255.0f/2.0f
in
  res
end // end of [mat4x4f_viewport]

fun
do_the_job (mesh: &mesh, filename: string): void = let
  //
  #define IMAGE_WIDTH 1024
  #define IMAGE_HEIGHT 768
  //
  var framebuffer: $PM.pixmap (uint32, 0, 0)
  val () = $PM.pixmap_new<uint32> (framebuffer, (i2sz)IMAGE_WIDTH, (i2sz)IMAGE_HEIGHT, $UN.castvwtp0{uint32}(0x0))
  var depthbuffer: $PM.pixmap (uint8, 0, 0)
  val () = $PM.pixmap_new<uint8> (depthbuffer, (i2sz)IMAGE_WIDTH, (i2sz)IMAGE_HEIGHT, $UN.cast{uint8}(255))
  //
  var mins: vec3f
  and maxs: vec3f
  val () = $OBJ.mesh_bounds (mesh, mins, maxs)
  //
  var env: gl_state
  var light_dir: vec3f
  val () = light_dir.init (1.0f, 1.0f, 1.0f)
  val () = light_dir := normalize_vec3f (light_dir)
  val () = println!("light_dir = ", light_dir)
  var eye: vec3f
  val () = eye.init (0.0f, 0.0f, 20.0f)
  var center: vec3f
  val () = center.init (0.0f, 0.0f, 0.0f)
  var up: vec3f
  val () = up.init (0.0f, 1.0f, 0.0f)
  var lookat: mat4x4f
  val () = lookat := mat4x4f_look_at (center, eye, up)
  val () = println!("lookat = ", lookat)
  val () = env.viewport := mat4x4f_viewport (
    0, 0, IMAGE_WIDTH, IMAGE_HEIGHT
  ) (* end of [val] *)
  val () = println!("viewport = ", env.viewport)
(*
  val () = {
    // this works
    var p: vec4f
    val () = p.init (0.0f, 0.0f, 0.0f, 1.0f)
    val () = print!("P = ", p)
    var p' = env.viewport * p
    val () = println!(", viewport*P = ", p')

    var q: vec4f
    val () = q.init (1.0f, 1.0f, 1.0f, 1.0f)
    val () = print!("Q = ", q)
    var q' = env.viewport * q
    val () = println!(", viewport*Q = ", q')
  } (* end of [val] *)
  val () = {
    // fixed (culprit was with normalize function)
    val () = println!("origin along z")
    var at: vec3f
    and eye: vec3f
    and up: vec3f
    val () = at.init (0.0f, 0.0f, 1.0f)
    val () = eye.init (0.0f, 0.0f, 0.0f)
    val () = up.init (0.0f, 1.0f, 0.0f)
    var M1 = mat4x4f_look_at (at, eye, up)
    var M2: mat4x4f
    val () = M2.init(1.0f, 0.0f, 0.0f, 0.0f,
                     0.0f, 1.0f, 0.0f, 0.0f,
                     0.0f, 0.0f, 1.0f, 0.0f,
                     0.0f, 0.0f, 0.0f, 1.0f)
    val () = println!("M1 = ", M1)
    val () = println!("M2 = ", M2)
  } (* end of [val] *)
*)
  var projection = mat4x4f_perspective (
    30.0f * g0float2float_double_float(M_PI) / 180.0f,
    g0int2float IMAGE_WIDTH / g0int2float IMAGE_HEIGHT,
    0.1f, 100.0f
  ) (* end of [val] *)

  val () = env.mvp := projection * lookat
  val () = env.light_dir := light_dir
  val () = mesh_rasterize (env, mesh, framebuffer, depthbuffer)
  //
  val () = $PM.pixmap_delete (depthbuffer) // TODO: print it?
  var p_framebuf : ptr
  val (pf_framebuf, pf_free_framebuf | ()) = $PM.pixmap_delete_getbuf (framebuffer, p_framebuf)
  val out = fileref_open_exn (filename, file_mode_w)
  val () = $PPM.save_PPM (out, !p_framebuf, (i2sz)IMAGE_HEIGHT, (i2sz)IMAGE_WIDTH)
  val () = matrix_ptr_free {uint32} (pf_framebuf, pf_free_framebuf | p_framebuf)
  //
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
