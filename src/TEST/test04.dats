#include
"share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload _(*anon*) = "libats/DATS/dynarray.dats"

staload PM = "./../SATS/image/pixmap.sats"
staload _ = "./../DATS/image/pixmap.dats"

staload TGA = "./../SATS/image/TGA.sats"
staload _ = "./../DATS/image/TGA.dats"

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

staload _ = "prelude/DATS/string.dats"
staload _ = "prelude/DATS/strptr.dats"

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
fun{env:vt@ype}
triangle$fragment (
  env: &(@[env][3]) >> _
, &vec3f
, &uint32? >> opt (uint32, b)
): #[b:bool] bool (b)

fun
clamp {v:int;a,b:nat | a <= b}
  (v: int v, a: int a, b: int b)
  : [r:nat | r >= a; r <= b] int r =
  if v < a then a
  else if v > b then b
  else v

fun{env:vt@ype}
triangle {m,n:pos} (
  env: &(@[env][3]) >> _
, a: &vec4f, b: &vec4f, c: &vec4f
, image: &$PM.pixmap (uint32, m, n)
, zbuffer: &$PM.pixmap (float, m, n)
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

(* ****** ****** *)

vtypedef gl_state = @{
  mvp= mat4x4f (* model-view-projection matrix *)
, viewport= mat4x4f (* viewport matrix *)
, light_dir= vec3f
} (* end of [gl_state] *)

(* ****** ****** *)

extern
fun{uniform,env:vt@ype}{v:t@ype}
shader_vert (state: &uniform, &(env)? >> env, vert: &v): vec4f
extern
fun{uniform,env:vt@ype}
shader_frag (
  &uniform
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
shader_vert<gl_state,gouraud_shader><gouraud_vert> (state, varying, v) = let
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
shader_frag<gl_state,gouraud_shader> (
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

(* ****** ****** *)

absvtype sampler2D (a:t@ype) = ptr
extern
fun
sampler_new (basename: string, extension: string, res: &ptr? >> opt (sampler2D(uint32), b)): #[b:bool] bool b
extern
fun{a:t@ype}
sampler_delete (sampler2D(INV(a))): void
extern
fun{a:t@ype}
sampler_lookup (!sampler2D(a), INV(a), &vec2f): a

(* ****** ****** *)
// sampler implementation

fun
string_replace_extension (base: string, suffix: string): Strptr0 = let
  val base = g1ofg0 (base)
  val ix = string_rindex (base, '.')
in
  if ix = ~1 then strptr_null ()
  else let
    val ix = g1int2uint_ssize_size ix
    val subs = string_make_substring (base, (i2sz)0, ix)
    val subs = strnptr2strptr subs
    val ext = string1_copy ((g1ofg0)suffix)
    val ext = strnptr2strptr ext
    val res = strptr_append (subs, ext)
    val () = strptr_free (subs)
    val () = strptr_free (ext)
  in
    res
  end
end // end of [string_replace_extension]

local

vtypedef pixmap1 (a:t@ype) = [m,n:int] $PM.pixmap (a, m, n)
assume sampler2D (a:t@ype) = [l:addr] (pixmap1 (a) @ l, mfree_gc_v (l) | ptr l)

in // in of [local]

implement
sampler_new (
  basename
, extension
, res
) = let
  val path = string_replace_extension (basename, extension)
in
  if strptr_is_null (path) then let
    val () = strptr_free (path)
    prval () = opt_none{sampler2D(uint32)} (res)
  in
    false
  end else let
    val p_path = ptrcast(path)
    val inp = fileref_open_opt ($UN.cast{string}(p_path), file_mode_r)
  in
    case+ inp of
    | ~None_vt () => let
        val () = println!("sampler_new: unable to open file at ", path)
        val () = strptr_free (path)
        prval () = opt_none{sampler2D(uint32)} (res)
      in
        false
      end // end of [let]
    | ~Some_vt inp => let
        val () = strptr_free (path)
        var w: size_t(0)
        and h: size_t(0)
        and p: ptr(null)
        val (pf | r) = $TGA.load_TGA (inp, w, h, p)
        val () = fileref_close (inp)
      in
        if r then let
          prval Some_v @(pf_mat, pf_free) = pf
          val (pf_pm, pf_free_pm | p_pm) = ptr_alloc<$PM.pixmap (uint32, 0, 0)> ()
          
          prval () = opt_unsome (w)
          and () = opt_unsome (h)
          and () = opt_unsome (p)
          prval [l_p:addr] EQADDR () = eqaddr_make_ptr (p)
          prval [w:int] EQINT () = eqint_make_guint (w)
          prval [h:int] EQINT () = eqint_make_guint (h)
          prval () = lemma_g1uint_param (w)
          prval () = lemma_g1uint_param (h)

          val () = assert_errmsg (w > (i2sz)0, "width is 0!")
          val () = assert_errmsg (h > (i2sz)0, "height is 0!")

          val () = $PM.pixmap_new_matrix<uint32> (pf_mat, pf_free | !p_pm, h, w, p)
          val () = w := (i2sz)0
          val () = h := (i2sz)0
          val () = p := the_null_ptr
          val () = res := (pf_pm, pf_free_pm | p_pm)
          prval () = opt_some{sampler2D(uint32)} (res)
        in
          true
        end else let
          prval None_v () = pf
          prval () = opt_unnone (w)
          and () = opt_unnone (h)
          and () = opt_unnone (p)
          prval () = opt_none{sampler2D(uint32)} (res)
          val () = w := (i2sz)0
          val () = h := (i2sz)0
          val () = p := the_null_ptr
        in
          false
        end // end of [if]
      end // end of [let]
  end // end of [if]
end // end of [sampler_new]

implement{a}
sampler_delete (s) = {
  val (pf_s, pf_free | p_s) = s
  val () = $PM.pixmap_delete<a> (!p_s)
  val () = ptr_free (pf_free, pf_s | p_s)
} (* end of [sampler_delete] *)

implement{a}
sampler_lookup (s, x, uv) = let
  val p = s.2
  val u = g0float2int (uv.x() * g0int2float (g0ofg1 (g1uint2int_size_int ($PM.pixmap_get_width (!p)))))
  val v = g0float2int (uv.y() * g0int2float (g0ofg1 (g1uint2int_size_int ($PM.pixmap_get_height (!p)))))
  implement
  $PM.pixmap_get_at_int2$default<a> () = x
  val res = $PM.pixmap_get_at_int2<a> (!p, u, v)
in
  res
end // end of [sample]

end // end of [local]

(* ****** ****** *)

vtypedef gl_state_diffuse = @{
  base= gl_state,
  diffuse= sampler2D(uint32)
}

abst@ype texture_shader = @{intensity= float, uv= vec2f}
typedef texture_vert = @{pos= vec3f, norm= vec3f, uv= vec2f}

local

assume texture_shader = @{intensity= float, uv= vec2f}

in // in of [local]

implement
shader_vert<gl_state_diffuse,texture_shader><texture_vert> (state, varying, v) = let
  var gl_Vertex: vec4f
  val () = gl_Vertex.init (v.pos.x(), v.pos.y(), v.pos.z(), 1.0f)
  var gl_Vertex' = state.base.mvp * gl_Vertex
  var gl_Vertex'' = state.base.viewport * gl_Vertex'
  
  val () = varying.intensity := min (1.0f, max (0.0f, dotprod (v.norm, state.base.light_dir)))
  val () = varying.uv := v.uv  
in
  gl_Vertex''
end // end of [shader_vert]

implement
shader_frag<gl_state_diffuse,texture_shader> (
  state, varying, bar, color
) = let
  val intensity = varying.[0].intensity * bar.x() + varying.[1].intensity * bar.y() + varying.[2].intensity * bar.z()
  val intensity = max(0.0f, min(1.0f, intensity))

  var uv0 = bar.x() * varying.[0].uv
  var uv1 = bar.y() * varying.[1].uv
  var uv2 = bar.z() * varying.[2].uv
  
  var uv = uv0 + uv1
  val () = uv := uv + uv2

  // lookup the fragment color & multiply
  val c0 = $UN.cast{uint} (sampler_lookup<uint32> (state.diffuse, $UN.cast{uint32}(0), uv))
  val r = $UN.cast{uint} (mul_int_float ($UN.cast{int} (g0uint_land (c0, 0xFFu)), intensity))
  val g = $UN.cast{uint} (mul_int_float ($UN.cast{int} (g0uint_land (c0 >>  8, 0xFFu)), intensity))
  val b = $UN.cast{uint} (mul_int_float ($UN.cast{int} (g0uint_land (c0 >> 16, 0xFFu)), intensity))

  val c = (0xFFu << 24) lor (b << 16) lor (g << 8) lor r
  val c = $UN.cast{uint32}(c)
  val () = color := c
  prval () = opt_some {uint32} (color)
in
  true
end // end of [shader_frag]

end // end of [local]


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

  val () = env.mvp := projection * lookat
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
