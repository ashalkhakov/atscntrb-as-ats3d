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
  faces= @(ptr, size_t)
} (* end of [mesh] *)

fun
mesh_delete (&mesh >> mesh?): void

fun
load_OBJ (FILEref, &mesh? >> opt (mesh, b)): #[b:bool] bool b

