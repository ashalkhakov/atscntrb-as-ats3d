(* ****** ****** *)

#define ATS_PACKNAME "ATSCNTRB.as.ats3d"
#define ATS_EXTERN_PREFIX "atscntrb_as_ats3d_" // prefix for external names

(* ****** ****** *)

fun
load_TGA {i,j:int;l0:addr} (
  FILEref
, &size_t(i)? >> opt (size_t w, r)
, &size_t(j)? >> opt (size_t h, r)
, &ptr(l0)? >> opt (ptr l, r)
) : #[w,h:int;l:addr;r:bool] (
  option_v ((matrix_v (uint32, l, w, h), mfree_gc_v (l)), r)
| bool (r)
) (* end of [load_TGA] *)

(* ****** end of [TGA.sats] ****** *)
