#include
"share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload PM = "./../SATS/image/pixmap.sats"
staload _ = "./../DATS/image/pixmap.dats"

staload OBJ = "./../SATS/mesh/OBJ.sats"
staload _ = "./../DATS/mesh/OBJ.dats"

staload "./../SATS/vector.sats"
staload _ = "./../DATS/vec2f.dats"
staload _ = "./../DATS/vec3f.dats"
staload _ = "./../DATS/vec3i.dats"


(* ****** ****** *)

implement
main0 (argc, argv) = let
in
  if argc >= 2 then let
    val inp = fileref_open_exn (argv[1], file_mode_r)
    var mesh: $OBJ.mesh
    val res = $OBJ.load_OBJ (inp, mesh)
    val () = fileref_close (inp)
  in
    if :(mesh: $OBJ.mesh?) => res then let
      prval () = opt_unsome (mesh)
      val () = println!("success")
      val () = $OBJ.mesh_delete (mesh)
    in
    end else let
      prval () = opt_unnone (mesh)
      val () = println!("failure")
    in
    end
  end else let
    val () = println!(
    "not enough arguments! usage: ", argv[0], " <obj file>"
    ) (* end of [val] *)
  in
    // nothing
  end // end of [if]
end // end of [main0]
