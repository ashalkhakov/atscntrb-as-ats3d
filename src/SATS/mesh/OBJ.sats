staload "./../vector.sats"

absvt@ype arrayptrsz (a:vt@ype) = @(ptr, size_t)

castfn
arrayptrsz_encode :
  {a:vt@ype}{n:int} @(arrayptr (a, n), size_t n) -<0> arrayptrsz (a)
castfn
arrayptrsz_decode :
  {a:vt@ype} arrayptrsz (a) -<0> [n:int] @(arrayptr (a, n), size_t n)

absvt@ype mesh = @{
  verts= ptr,
  normals= ptr,
  texcoords= ptr,
  faces= @(ptr, size_t),
  mins= vec3f,
  maxs= vec3f
} (* end of [mesh] *)

fun{env:vt@ype}
mesh_foreach_face_env$fwork (env: &env, f: size_t, va: &vec3f, vb: &vec3f, vc: &vec3f): void
fun{env:vt@ype}
mesh_foreach_face_env (env: &env, m: &mesh): void

// TODO: add a generic "vertex type" parameter?
fun{env:vt@ype}
mesh_foreach_gface_env$fwork (
  env: &env, f: size_t,
  va: &vec3f, vb: &vec3f, vc: &vec3f,
  na: &vec3f, nb: &vec3f, nc: &vec3f,
  tc_a: &vec2f, tc_b: &vec2f, tc_c: &vec2f
): void
fun{env:vt@ype}
mesh_foreach_gface_env (env: &env, m: &mesh): void

fun
mesh_bounds (&mesh, mins: &vec3f? >> vec3f, maxs: &vec3f? >> vec3f): void

fun
mesh_delete (&mesh >> mesh?): void

fun
load_OBJ (FILEref, &mesh? >> opt (mesh, b)): #[b:bool] bool b

