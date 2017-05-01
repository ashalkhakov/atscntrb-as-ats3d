//
// Author: Artyom Shakhakov (artyom DOT shalkhakov AT gmail DOT com)
// Time: May, 2011
// Time: Dec, 2015 (ported to ATS2)
//

#include
"share/atspre_staload.hats"

staload STDIO = "libats/libc/SATS/stdio.sats"

staload "./../../SATS/file_util.sats"
staload _ = "./../file_util.dats"

staload "./../../SATS/image/TGA.sats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0

(* ****** ****** *)

datatype PIXELTYPE (t@ype) =
  | PTrgb (uint32)
  | PTrgba (uint32)
  | PTbw (uint8)

(* ****** ****** *)
// TGA image loading

local
//
extern
fun{a:t@ype}
row_read {n:int} (
  fp: FILEref
, A: &(@[a?][n]) >> opt (@[a][n], r)
, n: size_t n
) : #[r:bool] bool (r)
//
extern
fun{a:t@ype}
row_read_rle {n:int} (
  fp: FILEref
, A: &(@[a?][n]) >> opt (@[a][n], r)
, n: size_t n
) : #[r:bool] bool (r)
//
implement{a}
row_read {n} (fp, A, asz) = file_read_array_lr<a> (fp, A, asz)
//
implement{a}
row_read_rle {n} (fp, A, asz) = let
//
fun
row_read_cst {n:int} (
  A: &(@[a?][n]) >> @[a][n], asz: size_t n, x: a
): void = array_initize_elt<a> (A, asz, x)
//
fun loop {i,n:nat | i <= n} {l:addr} .<n-i>. (
  pf1_arr: array_v (a, l, i)
, pf2_arr: array_v (a?, l+i*sizeof(a), n-i)
| p1: ptr l
, p2: ptr (l+i*sizeof(a))
, m: size_t (n-i)
) : [r:int] (vor (array_v (a, l, n), array_v (a?, l, n), r) | int r) =
if m = 0 then let
  prval () = array_v_unnil {a?} (pf2_arr)
  prval pf_res = VOR_l (pf1_arr)
in
  (pf_res | 0)
end else let
  prval p_bas = praxi_ptr {l} ()
  var hd: uint8
  val b_hd = file_read<uint8> (fp, hd)
in
  if :(hd: uint8?) => b_hd then let
    val hd = opt_unsome_get<uint8> (hd)
    val hd = g0uint2uint_uint8_uint (hd)
    val sz = (u2sz)((g1ofg0)((hd land (g0ofg1)0x7fu)))
    val sz = (g1ofg0)sz
    val sz = sz + (i2sz)1
    prval [j:int] EQINT () = eqint_make_guint (sz)
    val x = (hd land 0x80u) > 0u
  in
    if sz > m then let
      val () = prerrln!("[row_read_rle]: spans rows, sz=", sz, ", m=", m)
      prval () = topize {@[a][i]} (!p_bas)
      prval pf_arr = array_v_unsplit {a?} (pf1_arr, pf2_arr)
      prval pf_res = VOR_r (pf_arr)
    in
      (pf_res | 1)
    end else let // sz <= m
      prval (pf31_arr, pf32_arr) = array_v_split_at {a?} (pf2_arr | sz)
      prval [l0:addr] EQADDR () = ptr_get_index (p2)
      val p_arr = p2 : ptr l0
      prval pf31_arr = pf31_arr : array_v (a?, l0, j)
    in
      if x then let
        var v: a
        val bv = file_read<a> (fp, v)
      in
        if :(v: a?) => bv then let
          val v = opt_unsome_get<a> (v)
          val () = row_read_cst (!p_arr, sz, v)
          val p2 = ptr1_add_guint<a> (p_arr, sz)
          prval pf_arr = array_v_unsplit {a} (pf1_arr, pf31_arr)
          prval () = lemma_g1uint_param (sz)
          val m1 = m - sz
        in
          loop (pf_arr, pf32_arr | p1, p2, m1)
        end else let
          prval () = opt_unnone {a} (v)
          prval pf2_arr = array_v_unsplit (pf31_arr, pf32_arr)
          prval () = topize (!p_bas)
          prval pf_arr = array_v_unsplit (pf1_arr, pf2_arr)
        in
          (VOR_r (pf_arr) | 1)
        end
      end else let
        val br = row_read (fp, !p_arr, sz)
      in
        if br then let
          prval () = opt_unsome (!p_arr)
          val p2 = ptr1_add_guint<a> (p2, sz)
          prval pf_arr = array_v_unsplit {a} (pf1_arr, pf31_arr)
          val m = m - sz
        in
          loop (pf_arr, pf32_arr | p1, p2, m)
        end else let
          prval () = opt_unnone (!p_arr)
          prval pf3_arr = array_v_unsplit {a?} (pf31_arr, pf32_arr)
          prval () = topize (!p_bas)
          prval pf_arr = array_v_unsplit {a?} (pf1_arr, pf3_arr)
          prval pf_res = VOR_r (pf_arr)
        in
          (pf_res | 1)
        end
      end
    end
  end else let
    prval () = opt_unnone {uint8} (hd)
    prval () = topize {@[a][i]} (!p_bas)
    prval pf_res = array_v_unsplit {a?} (pf1_arr, pf2_arr)
  in
      #[.. | (VOR_r (pf_res) | 1)]
  end
end // end of [loop]
//
prval pf1_arr = array_v_nil {a} ()
val p_arr = addr@(A)
prval pf2_arr = view@(A)
prval () = lemma_g1uint_param (asz)
val (pf_res | res) = loop (pf1_arr, pf2_arr | p_arr, p_arr, asz)
//
in
  if res = 0 then let
    prval VOR_l pf_arr = pf_res
    prval () = view@(A) := pf_arr
    prval () = opt_some {@[a][n]} (A)
  in
    true
  end else let
    prval VOR_r pf_arr = pf_res
    prval () = view@(A) := pf_arr
    prval () = opt_none {@[a][n]} (A)
  in
    false
  end
end // end of [row_read_rle]
//
in // of [local]
//
implement
file_read<uint8> (fp, x) = file_read_uint8_le (fp, x)
implement
file_read<uint16> (fp, x) = file_read_uint16_le (fp, x)
implement
file_read<uint32> (fp, x) = file_read_uint32_le (fp, x)
//
typedef TGA_hdr = @{
  id_len= uint8
, color_map_type= uint8
, image_type= uint8
, color_map_index= uint16
, color_map_length= uint16
, color_map_size= uint8
, origin_x= uint16
, origin_y= uint16
, width= uint16
, height= uint16
, pixel_size= uint8
, attribs= uint8
}
//
implement
file_read<TGA_hdr> (fp, res) = let
  val b1 = file_read_tup8<uint8,uint8,uint8,uint16,uint16,uint8,uint16,uint16> (
    fp
  , res.id_len, res.color_map_type
  , res.image_type
  , res.color_map_index, res.color_map_length, res.color_map_size
  , res.origin_x, res.origin_y
  )
in
  if b1 then let
    val b2 = file_read_tup4<uint16,uint16,uint8,uint8> (fp, res.width, res.height, res.pixel_size, res.attribs)
  in
    if b2 then let
      prval () = opt_unsome (res.id_len)
      // skip comment
      val id_len = g0uint2uint_uint8_uint (res.id_len)
      val id_len = g0uint2int_uint_lint (id_len)
      val b3 = $STDIO.fseek0 (fp, id_len, $STDIO.SEEK_CUR)
    in
      if b3 = 0 then let
        prval () = opt_unsome_mac (
          res.color_map_type, res.image_type, res.color_map_index, res.color_map_length, res.color_map_size
        , res.origin_x, res.origin_y, res.width, res.height, res.pixel_size, res.attribs
        ) (* end of [prval] *)
        prval () = opt_some {TGA_hdr} (res)        
      in
        true
      end else let
        prval () = topize (res.id_len)
        prval () = opt_clear_mac (
          res.color_map_type, res.image_type, res.color_map_index, res.color_map_length, res.color_map_size
        , res.origin_x, res.origin_y, res.width, res.height, res.pixel_size, res.attribs
        ) (* end of [prval] *)
        prval () = opt_none {TGA_hdr} (res)
      in
        false
      end
    end else let
      prval () = opt_clear_mac (
        res.id_len
      , res.color_map_type, res.image_type, res.color_map_index, res.color_map_length, res.color_map_size
      , res.origin_x, res.origin_y, res.width, res.height, res.pixel_size, res.attribs
      ) (* end of [prval] *)
      prval () = opt_none {TGA_hdr} (res)
    in
      false
    end
  end else let
    prval () = opt_clear_mac (
      res.id_len, res.color_map_type, res.image_type, res.color_map_index, res.color_map_length, res.color_map_size
    , res.origin_x, res.origin_y
    ) (* end of [prval] *)
    prval () = opt_none {TGA_hdr} (res)
  in
    false
  end
end // end of [file_read<TGA_hdr>]
//
extern
fun
file_read_grayscale : file_read_type (uint32)
extern
fun
file_read_rgb : file_read_type (uint32)
extern
fun
file_read_rgba : file_read_type (uint32)
//
overload lsl with g0uint_lsl_uint32
//
implement
file_read_grayscale (fp, res) = let
  var b: uint8
  val xb = file_read<uint8> (fp, b)
in
  if :(b: uint8?) => xb then let
    val b = $UNSAFE.castvwtp0{uint32} (opt_unsome_get<uint8> (b))
    val () = res := (($UNSAFE.castvwtp0{uint32}(255u) lsl 24) lor (b lsl 16) lor (b lsl 8) lor b)
    prval () = opt_some (res)
  in
    true
  end else let
    prval () = opt_clear (b)
    prval () = opt_none (res)
  in
    false
  end
end // end of [file_read_grayscale]
//
implement
file_read_rgb (fp, res) = let
  var b: uint8
  var g: uint8
  var r: uint8
  val bgr = file_read_tup3<uint8,uint8,uint8> (fp, b, g, r)
in
  if :(b: uint8?, g: uint8?, r: uint8?) => bgr then let
    val b = $UNSAFE.castvwtp0{uint32} (opt_unsome_get<uint8> (b))
    val g = $UNSAFE.castvwtp0{uint32} (opt_unsome_get<uint8> (g))
    val r = $UNSAFE.castvwtp0{uint32} (opt_unsome_get<uint8> (r))
    val () = res := (($UNSAFE.castvwtp0{uint32}(255u) lsl 24) lor (b lsl 16) lor (g lsl 8) lor r)
    prval () = opt_some (res)
  in
    true
  end else let
    prval () = opt_clear_mac (b, g, r)
    prval () = opt_none (res)
  in
    false
  end
end // end of [file_read_rgb]
//
implement
file_read_rgba (fp, res) = let
  var b: uint8
  and g: uint8
  and r: uint8
  and a: uint8
  val bgra = file_read_tup4<uint8,uint8,uint8,uint8> (fp, b, g, r, a)
in
  if :(b: uint8?, g: uint8?, r: uint8?, a: uint8?) => bgra then let
    val b = $UNSAFE.castvwtp0{uint32} (opt_unsome_get<uint8> (b))
    val g = $UNSAFE.castvwtp0{uint32} (opt_unsome_get<uint8> (g))
    val r = $UNSAFE.castvwtp0{uint32} (opt_unsome_get<uint8> (r))
    val a = $UNSAFE.castvwtp0{uint32} (opt_unsome_get<uint8> (a))
    val () = res := ((a lsl 24) lor (b lsl 16) lor (g lsl 8) lor r)
    prval () = opt_some (res)
  in
    true
  end else let
    prval () = opt_clear_mac (b, g, r, a)
    prval () = opt_none (res)
  in
    false
  end // end of [if]
end // end of [file_read_rgba]
//
extern
fun
file_read_TGA_image {m,n:int} {i:pos | i <= 4} (
  FILEref
, int(i)(*pixelsize, bytes*)
, bool(*rle*)
, bool(*topdown*)
, size_t m
, size_t n
, &(@[uint32?][m*n]) >> opt (@[uint32][m*n], r)): #[r:bool] bool (r)
//
implement
file_read_TGA_image {m,n} (fp, ps, rle, td, width, height, res) = (
//
case+ ps of
| 1 => let
//
  implement
  file_read_array$elt<uint32> (fp, x) = file_read_grayscale (fp, x)
in
  if rle then let
    implement
    file_read_matrix_row<uint32> (fp, A, asz) = row_read_rle<uint32> (fp, A, asz)
  in
    if td then file_read_matrix_td<uint32> (fp, res, width, height)
    else file_read_matrix_bu<uint32> (fp, res, width, height)
  end else let
    implement
    file_read_matrix_row<uint32> {n} (fp, A, asz) = row_read<uint32> (fp, A, asz)
  in
    if td then file_read_matrix_td<uint32> (fp, res, width, height)
    else file_read_matrix_bu<uint32> (fp, res, width, height)
  end // end of [if]
end // end of [let]
//
| 3 => let
//
  implement
  file_read_array$elt<uint32> (fp, x) = file_read_rgb (fp, x)
in
  if rle then let
    implement
    file_read_matrix_row<uint32> {n} (fp, A, asz) = row_read_rle<uint32> (fp, A, asz)
  in
    if td then file_read_matrix_td<uint32> (fp, res, width, height)
    else file_read_matrix_bu<uint32> (fp, res, width, height)
  end else let
    implement
    file_read_matrix_row<uint32> {n} (fp, A, asz) = row_read<uint32> (fp, A, asz)
  in
    if td then file_read_matrix_td<uint32> (fp, res, width, height)
    else file_read_matrix_bu<uint32> (fp, res, width, height)
  end // end of [if]
end // end of [let]
//
| 4 => let
//
  implement
  file_read_array$elt<uint32> (fp, x) = file_read_rgba (fp, x)
in
  if rle then let
    implement
    file_read_matrix_row<uint32> {n} (fp, A, asz) = row_read_rle<uint32> (fp, A, asz)
  in
    if td then file_read_matrix_td<uint32> (fp, res, width, height)
    else file_read_matrix_bu<uint32> (fp, res, width, height)
  end else let
    implement
    file_read_matrix_row<uint32> {n} (fp, A, asz) = row_read<uint32> (fp, A, asz)
  in
    if td then file_read_matrix_td<uint32> (fp, res, width, height)
    else file_read_matrix_bu<uint32> (fp, res, width, height)
  end // end of [if]
end // end of [let]
//
| _ => let
  prval () = opt_none (res)
  val () = println!("[image_input]: illegal pixel size ", ps)
in
  false
end // end of [let]
//
) (* end of [file_read_TGA_image] *)
//
implement
load_TGA
(fp, w, h, p) = let
  var hdr: TGA_hdr
  val b = file_read<TGA_hdr> (fp, hdr)
in
  if :(hdr: TGA_hdr?) => b then let
    prval () = opt_unsome (hdr)
  in
    if g0uint_isneqz_uint8 (hdr.color_map_type) then let
      val () = println!("[image_input]: color-mapped TGA images not supported")
      prval () = opt_none (w)
      prval () = opt_none (h)
      prval () = opt_none (p)
    in
      (None_v () | false)
    end else let
      val imt = g0uint2int_uint8_int (hdr.image_type)
    in
      if imt <> 2 andalso imt <> 3 andalso imt <> 10 andalso imt <> 11 then let
        val () = println!("[image_input]: only type 2 (RGB), 3 (grayscale), 10 (RGB RLE), 11 (grayscale RLE) TGA images are supported")
        prval () = opt_none (w)
        prval () = opt_none (h)
        prval () = opt_none (p)
      in
        (None_v () | false)
      end else let
        val rle = (imt = 10) || (imt = 11)
        val width = g0uint2uint_uint16_uint (hdr.width)
        val width = (g1ofg0)width
        val width = (u2sz)width
        val height = g0uint2uint_uint16_uint (hdr.height)
        val height = (g1ofg0)height
        val height = (u2sz)height
        prval [m:int] EQINT () = eqint_make_guint (width)
        prval [n:int] EQINT () = eqint_make_guint (height)
        prval () = lemma_g1uint_param (width)
        prval () = lemma_g1uint_param (height)
      in
        if g1uint_iseqz(width) || g1uint_iseqz(height) then let
          val () = println!("[image_input]: zero width or height!")
          prval () = opt_none_mac (w, h, p)
        in
          (None_v () | false)
        end else let
          val attribs = g0uint2uint_uint8_uint (hdr.attribs)
          val attribs = attribs land 0x20u
          val td = iseqz(attribs)
          val ps = hdr.pixel_size
          val ps = g0uint2int_uint8_int (ps)
          val psz = (g1ofg0)ps
          val sz2 = width * height
          val (pf_arr, pf_gc | p_arr) = array_ptr_alloc<uint32> (sz2)
          prval [l:addr] EQADDR () = eqaddr_make_ptr (p_arr)
          val b = (
          ) (* end of [val] *)
        in
          if implies (imt = 2 || imt = 10, ps = 32 || ps = 24) then let
            val psz = (if ps = 32 then 4 else 3): [i:int | i>0; i <= 4] int(i)
            val b = file_read_TGA_image (fp, psz, rle, td, width, height, !p_arr)
          in
            if b then let
              val () = w := width
              and () = h := height
              prval () = opt_some_mac (w, h)
              prval () = opt_unsome {@[uint32][m*n]} (!p_arr)
              prval pf_mat = array2matrix_v {uint32} (pf_arr)
              val () = p := p_arr
              prval () = opt_some {ptr(l)} (p)
            in
              (Some_v @(pf_mat, pf_gc) | true)
            end else let
              prval () = opt_none_mac (w, h, p)
              prval () = opt_unnone {@[uint32][m*n]} (!p_arr)
              val () = array_ptr_free {uint32} (pf_arr, pf_gc | p_arr)
            in
              (None_v () | false)
            end // end of [if]
          end else if implies (imt = 3 orelse imt = 11, ps = 8) then let
            val psz = 1
            val b = file_read_TGA_image (fp, psz, rle, td, width, height, !p_arr)
          in
            if b then let
              val () = w := width
              and () = h := height
              prval () = opt_some_mac (w, h)
              prval () = opt_unsome {@[uint32][m*n]} (!p_arr)
              prval pf_mat = array2matrix_v {uint32} (pf_arr)
              val () = p := p_arr
              prval () = opt_some {ptr(l)} (p)
            in
              (Some_v @(pf_mat, pf_gc) | true)
            end else let
              prval () = opt_none_mac (w, h, p)
              prval () = opt_unnone {@[uint32][m*n]} (!p_arr)
              val () = array_ptr_free {uint32} (pf_arr, pf_gc | p_arr)
            in
              (None_v () | false)
            end // end of [if]
          end else let
            prval () = opt_none_mac (w, h, p)
            val () = array_ptr_free {uint32} (pf_arr, pf_gc | p_arr)
          in
            (None_v () | false)
          end // end of [if]
        end // end of [if]
      end // end of [if]
    end // end of [if]
  end else let
    prval () = opt_unnone (hdr)
    prval () = opt_none_mac (w, h, p)
  in
    (None_v () | false)
  end // end of [if]
end // end of [load_TGA]
//
end // of [local]

(* ****** ****** *)

