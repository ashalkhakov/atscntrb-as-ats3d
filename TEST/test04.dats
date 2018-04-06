#include
"share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload _(*anon*) = "libats/DATS/dynarray.dats"

staload PM = "./../src/SATS/image/pixmap.sats"
staload _ = "./../src/DATS/image/pixmap.dats"

staload PPM = "./../src/SATS/image/PPM.sats"

staload OBJ = "./../src/SATS/mesh/OBJ.sats"
staload _ = "./../src/DATS/mesh/OBJ.dats"

staload "./../src/SATS/vector.sats"
staload _ = "./../src/DATS/vec2f.dats"
staload _ = "./../src/DATS/vec3f.dats"
staload _ = "./../src/DATS/vec2i.dats"
staload _ = "./../src/DATS/vec3i.dats"
staload _ = "./../src/DATS/vec4f.dats"
staload "./../src/SATS/matrix.sats"
staload _ = "./../src/DATS/mat4x4f.dats"

staload "./../src/SATS/raster.sats"
staload _ = "./../src/DATS/raster.dats"

staload "./../src/SATS/sampler.sats"
staload _ = "./../src/DATS/sampler.dats"

staload "./../src/SATS/shader.sats"
staload "./../src/SATS/shader_sample.sats"
staload _ = "./../src/DATS/shader_sample.dats"

staload "libats/libc/SATS/math.sats"
staload _ = "libats/libc/DATS/math.dats"
staload _ = "prelude/DATS/float.dats"

(* ****** ****** *)

vtypedef pixmap (m:int, n:int) = $PM.pixmap (uint32, m, n)
vtypedef mesh = $OBJ.mesh

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

extern
fun{uniform,env:vt@ype}{v:t@ype}
shader_vert_prf {l_env,l_scrn: addr} (
  pf_env: !env? @ l_env >> env @ l_env
, pf_scrn: !vec4f? @ l_scrn >> vec4f @ l_scrn
| state: &INV(uniform)
, env: ptr l_env
, vert: &INV(v)
, scrn: ptr l_scrn
): void

implement{uniform,env}{v}
shader_vert_prf {l_env,l_scrn} (
  pf_env, pf_scrn
| gl_state, p_env, vert, p_scrn
) = !p_scrn := shader_vert<uniform,env><v> (gl_state, !p_env, vert)

(* ****** ****** *)

extern
fun{v:t@ype}
vert_unpack (&INV(v)? >> v, pos: &vec3f, norm: &vec3f, texcoord: &vec2f): void

fun{uniform:vt@ype;s:t@ype}{v:t@ype}
mesh_rasterize {m,n:pos} (
  gl_state: &INV(uniform)
, mesh: &mesh
, framebuffer: &$PM.pixmap (uint32, m, n)
, depthbuffer: &$PM.pixmap (float, m, n)
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
  $OBJ.mesh_foreach_gface_env$fwork<uniform> (gl_state, f, va, vb, vc, na, nb, nc, ta, tb, tc) = {
    //
    // TODO: move this out to mesh loading!
    var ga: v
    val () = vert_unpack<v> (ga, va, na, ta)
    var gb: v
    val () = vert_unpack<v> (gb, vb, nb, tb)
    var gc: v
    val () = vert_unpack<v> (gc, vc, nc, tc)
    //
    typedef V = s // shader type
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
    val () = shader_vert_prf<uniform,V><v> (pf1_at_var, view@scrn0 | gl_state, pvar, ga, addr@scrn0)
    //
    val () = pvar := ptr1_succ<V> (pvar)
    //
    prval (pf2_at_var, pf2_var) = array_v_uncons {V?} (pf1_var)
    val () = shader_vert_prf<uniform,V><v> (pf2_at_var, view@scrn1 | gl_state, pvar, gb, addr@scrn1)
    //
    val () = pvar := ptr1_succ<V> (pvar)
    //
    prval (pf3_at_var, pf3_var) = array_v_uncons {V?} (pf2_var)
    val () = shader_vert_prf<uniform,V><v> (pf3_at_var, view@scrn2 | gl_state, pvar, gc, addr@scrn2)
    //
    #define :: array_v_cons
    //
    prval pf3_nil_var = array_v_unnil_nil (pf3_var)
    prval () = view@(varyings) := pf1_at_var :: pf2_at_var :: pf3_at_var :: pf3_nil_var
    //
    implement
    triangle$fragment<V> (env, pos, color) = res where {
      prval (pf_gls, fpf_gls) = decode($vcopyenv_v(pf_gl_state))
      val res = shader_frag<uniform,V> (!p_gl_state, env, pos, color)
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
  val () = $OBJ.mesh_foreach_gface_env<uniform> (gl_state, mesh)
//
  prval () = view@(framebuffer) := pf_framebuffer
  prval () = view@(depthbuffer) := pf_depthbuffer
  prval () = view@(gl_state) := pf_gl_state
//
in
end // end of [...]

(* ****** ****** *)

fun
gl_state_init (mesh: &mesh, width: int, height: int, env: &gl_state? >> gl_state): void = {
  //
  var mins: vec3f
  and maxs: vec3f
  val () = $OBJ.mesh_bounds (mesh, mins, maxs)
  //
  var light_dir: vec3f
  val () = light_dir.init (1.0f, 1.0f, 1.0f)
  val () = light_dir := normalize_vec3f (light_dir)
  val () = println!("light_dir = ", light_dir)
  var eye: vec3f
  val () = eye.init (0.0f, 0.0f, (maxs.z()-mins.z()) * 4.0f (*3.0f*))
  var center: vec3f
  val () = center.init (0.0f, 0.0f, 0.0f)
  var up: vec3f
  val () = up.init (0.0f, 1.0f, 0.0f)
  var lookat: mat4x4f
  val () = lookat := mat4x4f_look_at (center, eye, up)
  val () = println!("lookat = ", lookat)
  val () = env.viewport := mat4x4f_viewport (
    0, 0, width, height
  ) (* end of [val] *)
  val () = println!("viewport = ", env.viewport)

  var projection = mat4x4f_perspective (
    30.0f * g0float2float_double_float(M_PI) / 180.0f,
    g0int2float width / g0int2float height,
    1.0f, 100.0f
  ) (* end of [val] *)

  var translation_vec: vec3f
  val () = translation_vec.init (0.0f, 0.0f, 0.0f)
  var model_to_world = mat4x4f_of_translation(translation_vec)
  
  val () = env.mv := lookat * model_to_world
  
  val () = env.mvp := projection * env.mv
  
  val () = env.light_dir := light_dir
} (* end of [gl_state_init] *)

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

fun
do_the_job {b:bool} (
  mesh: &mesh
, diffuse: &opt (sampler2D(uint32), b) >> _
, has_diffuse: bool b
, filename: string
): void = let
  //
  #define IMAGE_WIDTH 1024
  #define IMAGE_HEIGHT 768
  //
  var rastate: raster_state (uint32, float, 0, 0)
  val () = raster_state_init (
    rastate
  , IMAGE_WIDTH
  , IMAGE_HEIGHT
  , $UN.castvwtp0{uint32}(0x0)
  , $UN.cast{float}(0.0f)
  ) (* end of [val] *)
  //
  var env: gl_state
  val () = gl_state_init (mesh, IMAGE_WIDTH, IMAGE_HEIGHT, env)
  //
  implement
  vert_unpack<gouraud_vert> (ga, va, na, ta) = {
    val () = ga.pos := va
    val () = ga.norm := na
  } (* end of [vert_unpack] *)
  //
  implement
  vert_unpack<texture_vert> (ga, va, na, ta) = {
    val () = ga.pos := va
    val () = ga.norm := na
    val () = ga.uv := ta
  } (* end of [vert_unpack] *)
  //
  val () =
    if :(
      rastate: raster_state (uint32, float, IMAGE_WIDTH, IMAGE_HEIGHT)
    , diffuse : opt (sampler2D (uint32), b)
    ) => has_diffuse then let
      prval () = opt_unsome {sampler2D(uint32)} (diffuse)
      var glstate2: gl_state_diffuse
      val () = glstate2.base := env
      val () = glstate2.diffuse := diffuse
      val () = mesh_rasterize<gl_state_diffuse,texture_shader><texture_vert> (
        glstate2, mesh, rastate.framebuf, rastate.depthbuf
      ) (* end of [val] *)
      val () = diffuse := glstate2.diffuse
      prval () = opt_some {sampler2D(uint32)} (diffuse)
    in
    end else mesh_rasterize<gl_state,gouraud_shader><gouraud_vert> (
      env, mesh, rastate.framebuf, rastate.depthbuf
    ) (* end of [val] *)
  //
  val () = raster_state_flush (rastate, filename)
  //
in
end // end of [...]

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
      
      // open the TGA file, obtain the corresponding pixmap...
      var sampler_diffuse: sampler2D(uint32)
      val sampler_diffuse_res = sampler_new (argv[1], "_diffuse.tga", sampler_diffuse)
      val () =
        if :(sampler_diffuse: sampler2D(uint32)?) => sampler_diffuse_res then let
          val () = println!("initialized diffuse sampler")
          val () = do_the_job (mesh, sampler_diffuse, true, argv[2])
          prval () = opt_unsome {sampler2D(uint32)} (sampler_diffuse)
          val () = sampler_delete<uint32> (sampler_diffuse)
        in
        end else let
          val () = println!("unable to initialize the diffuse sampler")
          val () = do_the_job (mesh, sampler_diffuse, false, argv[2])
          prval () = opt_unnone {sampler2D(uint32)} (sampler_diffuse)
        in
        end
      // end of [val]

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
