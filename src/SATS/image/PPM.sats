(* ****** ****** *)

fun
save_PPM {m,n:int} (FILEref, &matrix (uint32, m, n), size_t m, size_t n): void
fun
save_PGM {m,n:int} (FILEref, &matrix (uint8, m, n), size_t m, size_t n): void

(* ****** end of [PPM.sats] ****** *)
