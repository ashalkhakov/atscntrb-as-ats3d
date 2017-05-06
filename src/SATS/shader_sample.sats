staload "./vector.sats"
staload "./matrix.sats"
staload "./sampler.sats"

(* ****** ****** *)

#define ATS_PACKNAME "ATSCNTRB.as.ats3d"
#define ATS_EXTERN_PREFIX "atscntrb_as_ats3d_" // prefix for external names

(* ****** ****** *)

vtypedef gl_state = @{
  mvp= mat4x4f (* model-view-projection matrix *)
, viewport= mat4x4f (* viewport matrix *)
, light_dir= vec3f
} (* end of [gl_state] *)

abst@ype gouraud_shader = float
typedef gouraud_vert = @{pos= vec3f, norm= vec3f}

(* ****** ****** *)

vtypedef gl_state_diffuse = @{
  base= gl_state,
  diffuse= sampler2D(uint32)
}

abst@ype texture_shader = @{intensity= float, uv= vec2f}
typedef texture_vert = @{pos= vec3f, norm= vec3f, uv= vec2f}

(* ****** end of [shader_sample.sats" ****** *)
