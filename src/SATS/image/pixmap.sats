(* ****** ****** *)

absvt@ype pixmap (t:t@ype, m:int, n:int) = (size_t, size_t, ptr)

fun{a:t@ype}
pixmap_new {m,n:pos} (&pixmap (a, 0, 0)? >> pixmap (a, m, n), size_t m, size_t n, a): void
fun{a:t@ype}
pixmap_delete {m,n:int} (&pixmap (a, m, n) >> pixmap (a, 0, 0)?): void

fun
pixmap_delete_getbuf {a:t@ype} {m,n:int} (
  &pixmap (a, m, n) >> pixmap (a, 0, 0)?
, &ptr? >> ptr l
): #[l:addr] (matrix_v (a, l, m, n), mfree_gc_v (l) | void)

fun{a:t@ype}
pixmap_set_at_int {m,n:int} (&pixmap (INV(a), m, n), natLt(m), natLt(n), a): void
overload [] with pixmap_set_at_int
fun{a:t@ype}
pixmap_set_at_int2 {m,n:int} (&pixmap (INV(a), m, n), int, int, a): void

fun{a:t@ype}
pixmap_get_width {m,n:int} (&pixmap (INV(a), m, n)): size_t m
fun{a:t@ype}
pixmap_get_height {m,n:int} (&pixmap (INV(a), m, n)): size_t n

fun{a:t@ype}
pixmap_clear {m,n:int} (&pixmap (INV(a), m, n), a): void

(* ****** end of [pixmap.sats] ****** *)
