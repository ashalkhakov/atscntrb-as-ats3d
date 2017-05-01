#include
"share/atspre_staload.hats"

staload "./../SATS/sampler.sats"

staload UN = "prelude/SATS/unsafe.sats"

staload PM = "./../SATS/image/pixmap.sats"
staload _ = "./../DATS/image/pixmap.dats"

staload TGA = "./../SATS/image/TGA.sats"
staload _ = "./../DATS/image/TGA.dats"

staload "./../SATS/vector.sats"
staload _ = "./../DATS/vec2f.dats"

staload FU = "./../SATS/file_util.sats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0

(* ****** ****** *)
// sampler implementation

local

vtypedef pixmap1 (a:t@ype) = [m,n:int] $PM.pixmap (a, m, n)
assume sampler2D (a:t@ype) = [l:addr] (pixmap1 (a) @ l, mfree_gc_v (l) | ptr l)

in // in of [local]

implement
sampler_new (
  basename
, extension
, res
) = let
  val path = $FU.string_replace_extension (basename, extension)
in
  if strptr_is_null (path) then let
    val () = strptr_free (path)
    prval () = opt_none{sampler2D(uint32)} (res)
  in
    false
  end else let
    val p_path = ptrcast(path)
    val inp = fileref_open_opt ($UN.cast{string}(p_path), file_mode_r)
  in
    case+ inp of
    | ~None_vt () => let
        val () = println!("sampler_new: unable to open file at ", path)
        val () = strptr_free (path)
        prval () = opt_none{sampler2D(uint32)} (res)
      in
        false
      end // end of [let]
    | ~Some_vt inp => let
        val () = strptr_free (path)
        var w: size_t(0)
        and h: size_t(0)
        and p: ptr(null)
        val (pf | r) = $TGA.load_TGA (inp, w, h, p)
        val () = fileref_close (inp)
      in
        if r then let
          prval Some_v @(pf_mat, pf_free) = pf
          val (pf_pm, pf_free_pm | p_pm) = ptr_alloc<$PM.pixmap (uint32, 0, 0)> ()
          
          prval () = opt_unsome (w)
          and () = opt_unsome (h)
          and () = opt_unsome (p)
          prval [l_p:addr] EQADDR () = eqaddr_make_ptr (p)
          prval [w:int] EQINT () = eqint_make_guint (w)
          prval [h:int] EQINT () = eqint_make_guint (h)
          prval () = lemma_g1uint_param (w)
          prval () = lemma_g1uint_param (h)

          val () = assert_errmsg (w > (i2sz)0, "width is 0!")
          val () = assert_errmsg (h > (i2sz)0, "height is 0!")

          val () = $PM.pixmap_new_matrix<uint32> (pf_mat, pf_free | !p_pm, h, w, p)
          val () = w := (i2sz)0
          val () = h := (i2sz)0
          val () = p := the_null_ptr
          val () = res := (pf_pm, pf_free_pm | p_pm)
          prval () = opt_some{sampler2D(uint32)} (res)
        in
          true
        end else let
          prval None_v () = pf
          prval () = opt_unnone (w)
          and () = opt_unnone (h)
          and () = opt_unnone (p)
          prval () = opt_none{sampler2D(uint32)} (res)
          val () = w := (i2sz)0
          val () = h := (i2sz)0
          val () = p := the_null_ptr
        in
          false
        end // end of [if]
      end // end of [let]
  end // end of [if]
end // end of [sampler_new]

implement{a}
sampler_delete (s) = {
  val (pf_s, pf_free | p_s) = s
  val () = $PM.pixmap_delete<a> (!p_s)
  val () = ptr_free (pf_free, pf_s | p_s)
} (* end of [sampler_delete] *)

implement{a}
sampler_lookup (s, x, uv) = let
  val p = s.2
  val u = g0float2int (uv.x() * g0int2float (g0ofg1 (g1uint2int_size_int ($PM.pixmap_get_width (!p)))))
  val v = g0float2int (uv.y() * g0int2float (g0ofg1 (g1uint2int_size_int ($PM.pixmap_get_height (!p)))))
  implement
  $PM.pixmap_get_at_int2$default<a> () = x
  val res = $PM.pixmap_get_at_int2<a> (!p, u, v)
in
  res
end // end of [sample]

end // end of [local]
