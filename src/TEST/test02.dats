#include
"share/atspre_staload.hats"

staload STDIO = "libats/libc/SATS/stdio.sats"

staload FU = "./../SATS/file_util.sats" // opt_unsome

staload TGA = "./../SATS/image/TGA.sats"
staload PPM = "./../SATS/image/PPM.sats"

implement
main0 (argc, argv) = let
in
  if argc >= 2 then let
    val inp = fileref_open_exn (argv[1], file_mode_r)
    var w: size_t(0)
    var h: size_t(0)
    var p_mat: ptr(null)
    val (pf_res | res) = $TGA.load_TGA (inp, w, h, p_mat)
  in
    if res then let
      prval Some_v @(pf_mat, pf_gc) = pf_res
      prval () = $FU.opt_unsome_mac (w, h, p_mat)
      prval [l:addr] EQADDR () = eqaddr_make_ptr (p_mat)
      prval [w:int] EQINT () = eqint_make_guint (w)
      prval [h:int] EQINT () = eqint_make_guint (h)
      val () = println!("open: success! width: ", w, ", height: ", h)
      //
      val () =
        if argc >= 3 then let
          val out = fileref_open_exn (argv[2], file_mode_w)      
          val () = $PPM.save_PPM (out, !p_mat, w, h)
        in
          fileref_close (out)
        end // end of [if]
      //
      prval pf_arr = matrix2array_v {uint32}{l}{w,h} (pf_mat)
      prval () = topize {@[uint32][w*h]} (!p_mat)
      prval pf_mat = array2matrix_v {uint32?}{l}{w,h} (pf_arr)
      val () = matrix_ptr_free (pf_mat, pf_gc | p_mat)
      //
      val () = w := (i2sz)0
      val () = h := (i2sz)0
      val () = p_mat := the_null_ptr
      //
    in
      fileref_close (inp)
    end else let
      prval None_v () = pf_res
      prval () = $FU.opt_clear_mac (w, h, p_mat)
      val () = println!("open: failed")
      val () = w := (i2sz)0
      val () = h := (i2sz)0
      val () = p_mat := the_null_ptr
    in
      fileref_close (inp)
    end
  end else let
    val () = println!(
    "not enough arguments! usage: ", argv[0], " <tga file> [<ppm file>] (optionally specify where to save the converted data"
    ) (* end of [val] *)
  in
    // nothing
  end // end of [if]
end
