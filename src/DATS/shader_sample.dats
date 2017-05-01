#include
"share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload "./../SATS/vector.sats"
staload _ = "./../DATS/vec2f.dats"
staload _ = "./../DATS/vec3f.dats"
staload _ = "./../DATS/vec4f.dats"
staload "./../SATS/matrix.sats"
staload _ = "./../DATS/mat4x4f.dats"

staload "./../SATS/sampler.sats"

staload "./../SATS/shader_sample.sats"
staload "./../SATS/shader.sats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0

(* ****** ****** *)
// Gouraud shader

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
