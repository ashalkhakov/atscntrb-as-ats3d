staload "./vector.sats"
staload PM = "./image/pixmap.sats"

fun
line {m,n:int} (
  canvas: &$PM.pixmap (uint32, m, n), x0: int, y0: int, x1: int, y1: int, color: uint32
): void

fun{env:vt@ype}
triangle$fragment (
  env: &(@[env][3]) >> _
, &vec3f
, &uint32? >> opt (uint32, b)
): #[b:bool] bool (b)

fun{env:vt@ype}
triangle {m,n:pos} (
  env: &(@[env][3]) >> _
, a: &vec4f, b: &vec4f, c: &vec4f
, image: &$PM.pixmap (uint32, m, n)
, zbuffer: &$PM.pixmap (float, m, n)
): void
