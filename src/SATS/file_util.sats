fun
string_replace_extension (base: string, suffix: string): Strptr0

macdef
implies (x, y) = (if ,(x) then ,(y) else false)

praxi
opt_reset
  {a:t0p}{b:bool}(x: !opt(INV(a), b) >> opt (a, false)):<prf> void

local

macrodef
rec
auxlist
  (f, xs, y) =
(
//
if iscons! (xs) then
  `(let prval () = ,(f) (,(car! xs)) in ,(auxlist (f, cdr! xs, y)) end)
else y // end of [if]
//
) // end of [auxlist]

in (* in of [local] *)

macdef
opt_none_mac (x) =
,(
  if islist! (x) then auxlist(`(opt_none), x, `()) else `(opt_none ,(x))
)

macdef
opt_some_mac (x) =
,(
  if islist! (x) then auxlist (`(opt_some), x, `()) else `(opt_some ,(x))
)

macdef
opt_unsome_mac (x) =
,(
  if islist! (x) then auxlist (`(opt_unsome), x, `()) else `(opt_unsome ,(x))
)

macdef
opt_clear_mac (x) =
,(
  if islist! (x) then auxlist (`(opt_clear), x, `()) else `(opt_clear ,(x))
)

macdef
opt_reset_mac (x) =
,(
  if islist! (x) then auxlist (`(opt_reset), x, `()) else `(opt_reset ,(x))
) // end of [opt_reset_mac]

macdef
opt_clear_mac(x) =
,(
  if islist! (x) then auxlist (`(opt_clear), x, `()) else `(opt_clear ,(x))
) // end of [opt_clear_mac]

end // end of [local]

(* ****** ****** *)
// primitive file reading functions

fun{a:t@ype}
file_read (FILEref, &a? >> opt (a, r)): #[r:bool] bool (r)

typedef
file_read_type (a:t@ype) = (FILEref, &a? >> opt (a, r)) -> #[r:bool] bool (r)

fun{a,b:t@ype}
file_read_tup2 (FILEref, &a? >> opt (a, r), &b? >> opt (b, r)): #[r:bool] bool (r)
fun{a,b,c:t@ype}
file_read_tup3 (FILEref, &a? >> opt (a, r), &b? >> opt (b, r), &c? >> opt (c, r)): #[r:bool] bool (r)
fun{a,b,c,d:t@ype}
file_read_tup4 (FILEref, &a? >> opt (a, r), &b? >> opt (b, r), &c? >> opt (c, r), &d? >> opt (d, r)): #[r:bool] bool (r)
fun{a,b,c,d,e:t@ype}
file_read_tup5 (FILEref, &a? >> opt (a, r), &b? >> opt (b, r), &c? >> opt (c, r), &d? >> opt (d, r), &e? >> opt (e, r)): #[r:bool] bool (r)
fun{a,b,c,d,e,f:t@ype}
file_read_tup6 (FILEref, &a? >> opt (a, r), &b? >> opt (b, r), &c? >> opt (c, r), &d? >> opt (d, r), &e? >> opt (e, r), &f? >> opt (f, r)): #[r:bool] bool (r)
fun{a,b,c,d,e,f,g:t@ype}
file_read_tup7 (
  FILEref
, &a? >> opt (a, r), &b? >> opt (b, r), &c? >> opt (c, r), &d? >> opt (d, r), &e? >> opt (e, r)
, &f? >> opt (f, r), &g? >> opt (g, r)
): #[r:bool] bool (r)
fun{a,b,c,d,e,f,g,h:t@ype}
file_read_tup8 (
  FILEref
, &a? >> opt (a, r), &b? >> opt (b, r), &c? >> opt (c, r), &d? >> opt (d, r), &e? >> opt (e, r)
, &f? >> opt (f, r), &g? >> opt (g, r), &h? >> opt (h, r)
): #[r:bool] bool (r)

(* ****** ****** *)
// "le" for little-endian, "be" for big-endian

fun
file_read_uint8_le : file_read_type (uint8)
fun
file_read_uint16_le : file_read_type (uint16)
fun
file_read_uint32_le : file_read_type (uint32)

(* ****** ****** *)
// reading into arrays

fun{a:t@ype}
file_read_array$elt (
  FILEref
, &a? >> opt (a, r)
): #[r:bool] bool (r)

fun{a:t@ype}
file_read_array_lr {n:int} ( // left to right
  FILEref
, &(@[a?][n]) >> opt (@[a][n], r)
, size_t n
) : #[r:bool] bool (r)

fun{a:t@ype}
file_read_array_rl {n:int} ( // right to left
  FILEref
, &(@[a?][n]) >> opt (@[a][n], r)
, size_t n
) : #[r:bool] bool (r)

(* ****** ****** *)
// reading into matrices

fun{a:t@ype}
file_read_matrix_row {n:int} (
  FILEref
, &(@[a?][n]) >> opt (@[a][n], r)
, size_t n
): #[r:bool] bool (r)

fun{a:t@ype}
file_read_matrix_td(*top-down*) {m,n:int} (
  FILEref
, &(@[a?][m*n]) >> opt (@[a][m*n], r)
, size_t m
, size_t n
) : #[r:bool] bool (r)
//
fun{a:t@ype}
file_read_matrix_bu(*bottom-up*) {m,n:int} (
  FILEref
, &(@[a?][m*n]) >> opt (@[a][m*n], r)
, size_t m
, size_t n
) : #[r:bool] bool (r)
