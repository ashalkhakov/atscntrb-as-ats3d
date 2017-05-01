staload "./vector.sats"

fun{uniform,env:vt@ype}{v:t@ype}
shader_vert (state: &uniform, &(env)? >> env, vert: &v): vec4f

fun{uniform,env:vt@ype}
shader_frag (
  &uniform
, &(@[env][3]) >> _
, bar: &vec3f
, color: &uint32? >> opt (uint32, b)
): #[b:bool] bool(b)
