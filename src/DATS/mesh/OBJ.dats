#include
"share/atspre_staload.hats"

staload STR = "libats/libc/SATS/string.sats"
staload "libats/libc/SATS/stdio.sats"
staload
UN="prelude/SATS/unsafe.sats"

staload DA = "libats/SATS/dynarray.sats"
staload _(*anon*) = "libats/DATS/dynarray.dats"

staload "./../../SATS/file_util.sats"
staload "./../../SATS/mesh/OBJ.sats"

staload "./../../SATS/vector.sats"
staload _ = "./../../DATS/vec2f.dats"
staload _ = "./../../DATS/vec3f.dats"
staload _ = "./../../DATS/vec3i.dats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0

(* ****** ****** *)

fun{}
string_advance{n,i:nat | i <= n} .< >. (
  str: string(n), i: size_t(i)
):<> string(n-i) = let
  val str2 =
    $UN.cast{string(n-i)}(ptr_add<char>(string2ptr(str), i))
in
  str2
end

(* ****** ****** *)

typedef
parsefn (a:vt@ype) = (&string, &a? >> opt (a, b)) -> #[b:bool] bool b

extern
fun
atoi : parsefn (int)
extern
fun
atof : parsefn (double)
extern
fun
skip_string (&string, string): bool
extern
fun
skip_whitespace (&string): void

extern
fun{a:t@ype}
string_read : parsefn (a)
extern
fun{}
string_read$skip (&string): void
extern
fun{a,b:t@ype}
string_read_tup2 (&string, &a? >> opt (a, r), &b? >> opt (b, r)): #[r:bool] bool (r)
extern
fun{a,b,c:t@ype}
string_read_tup3 (&string, &a? >> opt (a, r), &b? >> opt (b, r), &c? >> opt (c, r)): #[r:bool] bool (r)

implement
atoi (str, res) = let
//
var env: int = 0
implement{env}
string_foreach$cont (c, env) = isdigit (c)
implement
string_foreach$fwork<int>
  (c, env) = env := 10 * env + (c - '0')
//
val str0 = g1ofg0(str)
val i = string_foreach_env<int> (str0, env)
//
in
  if isgtz(i) then let
    val () = res := env
    prval () = opt_some {int} (res)
    val () = str := g0ofg1 (string_advance (str0, i)) in
    true
  end else let
    prval () = opt_none {int} (res) in
    false
  end
end // end of [atoi]

(* ****** ****** *)

macdef double = g0int2float_int_double

extern
fun atof_frac (
  str: &string, res: &double? >> opt (double, b)
): #[b:bool] bool b

local

implement
atof_frac (str, res) = let
//
fun loop
(
  p: ptr, num: double, den: double, res: &size_t
) : double = let
  val c = $UN.ptr0_get<char> (p)
in
//
if isdigit (c) then let
  val () = res := succ(res)
in
  loop (ptr_succ<char> (p), 10 * num + double(c - '0'), 10 * den, res)
end else (num / den) // end of [if]
//
end // end of [loop]
//
val p0 = string2ptr (str)
val c0 = $UN.ptr0_get<char> (p0)
//
var i = g0ofg1 ((i2sz)1)
//
in
//
if c0 = '.' then let
  val () = res := loop (ptr_succ<char> (p0), 0.0, 1.0, i)
  val () = str := $UN.cast{string} (ptr0_add_guint<char> (p0, i))
  prval () = opt_some {double} (res)
in
  true
end else let
  prval () = opt_none {double} (res)
in
  false
end
//
end // end of [atof_frac]

in // in of [local]

implement
atof (str, res) = let
//
typedef tenv = double
//
val str0 = g1ofg0_string (str)
//
var env: tenv = 0.0
implement{env}
string_foreach$cont (c, env) = isdigit (c)
implement
string_foreach$fwork<tenv>
  (c, env) = env := 10 * env + double(c - '0')
val n = string_foreach_env<tenv> (str0, env)
var str1 = g0ofg1 (string_advance (str0, n))
var dbl : double
val r1 = atof_frac (str1, dbl)
//
in
  if :(dbl: double?) => isgtz(n) then begin
    if :(dbl: double?) => r1 then let
      prval () = opt_unsome {double} (dbl)
      val () = str := str1
      val () = res := env + dbl
      prval () = opt_some {double} (res)
    in
      true
    end else let
      prval () = opt_unnone {double} (dbl)
      val () = str := str1
      val () = res := env
      prval () = opt_some {double} (res)
    in
      true
    end
  end else begin
    if :(dbl: double?) => r1 then let // fractional part only
      prval () = opt_unsome {double} (dbl)
      val () = str := str1
      val () = res := dbl
      prval () = opt_some {double} (res)
    in
      true
    end else let
      prval () = opt_unnone {double} (dbl)
      prval () = opt_none {double} (res)
    in
      false // could not read anything
    end
  end
end // end of [atof]

end // end of [local]

(* ****** ****** *)

implement
skip_string (str, pat) = let
  val l = strlen (pat)
in
  if $STR.strncmp (str, pat, l) = 0 then let
    val () = str := $UN.cast{string}(ptr_add<char>(string2ptr(str), l))
  in
    true
  end else false
end // end of [skip_string]

implement
skip_whitespace (str) = let
//
fun loop
(
  p: ptr
) : ptr = let
  val c = $UN.ptr0_get<char> (p)
in
//
if isspace (c) then
  loop (ptr_succ<char> (p))
else p // end of [if]
//
end // end of [loop]
//
val p0 = string2ptr (str)
val p1 = loop (p0)
//
in
  str := $UN.cast{string} (p1)
end // end of [skip_whitespace]

implement{a,b}
string_read_tup2 (fp, a, b) = let
  val ar = string_read<a> (fp, a) in
  if ar then let
    val () = string_read$skip<> (fp)
    val br = string_read<b> (fp, b) in
    if br then true
    else let
      prval () = opt_reset_mac (a) in
      false
    end
  end else let
    prval () = opt_none {b} (b) in
    false
  end
end // end of [string_read_tup2]

implement{a,b,c}
string_read_tup3 (fp, a, b, c) = let
  val abr = string_read_tup2<a,b> (fp, a, b) in
  if abr then let
    val () = string_read$skip<> (fp)
    val cr = string_read<c> (fp, c) in
    if cr then true
    else let
      prval () = opt_reset_mac (a, b) in
      false
    end
  end else let
    prval () = opt_none {c} (c) in
    false
  end
end // end of [string_read_tup3]

(* ****** ****** *)

  implement
  string_read<float> (s, res) = let
    var d: double
  in
    if :(d: double?) => atof (s, d) then let
      prval () = opt_unsome {double} (d)
      val () = res := g0float2float_double_float (d)
      prval () = opt_some {float} (res)
    in
      true
    end else let
      prval () = opt_unnone {double} (d)
      prval () = opt_none {float} (res)
    in
      false
    end
  end // end of [string_read]

  implement
  string_read<int> = atoi

implement
  string_read<vec2f> (str, v) = let
    implement
    string_read$skip<> = skip_whitespace
    var x: float and y: float
    val res = string_read_tup2<float,float> (str, x, y)
  in
    if :(x: float?, y: float?) => res then let
      prval () = opt_unsome_mac (x, y)
      val () = vec2f_init2 (v, x, y)
      prval () = opt_some {vec2f} (v) in
      true
    end else let
      prval () = opt_clear_mac (x, y)
      prval () = opt_none {vec2f} (v) in
      false
    end
  end // end of [string_read<vec2f>]
//
implement
  string_read<vec3f> (str, v) = let
    implement
    string_read$skip<> = skip_whitespace
    var x: float and y: float and z: float
    val res = string_read_tup3<float,float,float> (str, x, y, z)
  in
    if :(x: float?, y: float?, z: float?) => res then let
      prval () = opt_unsome_mac (x, y, z)
      val () = vec3f_init3 (v, x, y, z)
      prval () = opt_some {vec3f} (v) in
      true
    end else let
      prval () = opt_clear_mac (x, y, z)
      prval () = opt_none {vec3f} (v) in
      false
    end
  end // end of [string_read<vec3f>]

implement
  string_read<vec3i> (str, v) = let
    implement
    string_read$skip<> (str) = ignoret (skip_string (str, "/"))
    var x: int and y: int and z: int
    val res = string_read_tup3<int,int,int> (str, x, y, z)
  in
    if :(x: int?, y: int?, z: int?) => res then let
      prval () = opt_unsome_mac (x, y, z)
      val () = vec3i_init3 (v, x, y, z)
      prval () = opt_some {vec3i} (v) in
      true
    end else let
      prval () = opt_clear_mac (x, y, z)
      prval () = opt_none {vec3i} (v) in
      false
    end
  end // end of [string_read<vec3i>]


(* ****** ****** *)

typedef face3 = @{a= vec3i, b= vec3i, c= vec3i}

implement
  string_read<face3> (str, v) = let
    implement
    string_read$skip<> = skip_whitespace
    var x: vec3i and y: vec3i and z: vec3i
    val res = string_read_tup3<vec3i,vec3i,vec3i> (str, x, y, z)
  in
    if :(x: vec3i?, y: vec3i?, z: vec3i?) => res then let
      prval () = opt_unsome_mac (x, y, z)
      val () = v.a := x
      val () = v.b := y
      val () = v.c := z
      prval () = opt_some {face3} (v) in
      true
    end else let
      prval () = opt_clear_mac (x, y, z)
      prval () = opt_none {face3} (v) in
      false
    end    
  end // end of [string_read<face3>]

vtypedef state = @{
  verts= $DA.dynarray (vec3f),
  normals= $DA.dynarray (vec3f),
  texcoords= $DA.dynarray (vec2f),
  faces= $DA.dynarray (face3)
}

fun
state_new (x: &state? >> state): void = let
  val () = x.verts := $DA.dynarray_make_nil<vec3f>((i2sz)256)
  val () = x.normals := $DA.dynarray_make_nil<vec3f>((i2sz)256)
  val () = x.texcoords := $DA.dynarray_make_nil<vec2f>((i2sz)256)
  val () = x.faces := $DA.dynarray_make_nil<face3>((i2sz)256)
in
end
fun
state_delete (x: &state >> state?): void = let
  val () = $DA.dynarray_free (x.verts)
  val () = $DA.dynarray_free (x.normals)
  val () = $DA.dynarray_free (x.texcoords)
  val () = $DA.dynarray_free (x.faces)
in
end
fun
state_line (st: &state >> _, line: string): bool = let
  var ln = line
in
  case+ :(st: state, ln: string) => 0 of
  | _ when skip_string (ln, "vn") => let
      val () = skip_whitespace (ln)
      var v: vec3f
    in
      if :(st: state, ln: string, v: vec3f?) => string_read<vec3f> (ln, v) then let
        prval () = opt_unsome {vec3f} (v)
        val () = $DA.dynarray_insert_atend_exn (st.normals, v)
      in
        true
      end else let
        prval () = opt_unnone {vec3f} (v)
      in
        false
      end // end of [if]
    end // end of [vn]
  | _ when skip_string (ln, "vt") => let
      val () = skip_whitespace (ln)
      var v: vec2f
    in
      if :(st: state, ln: string, v: vec2f?) => string_read<vec2f> (ln, v) then let
        prval () = opt_unsome {vec2f} (v)
        val () = $DA.dynarray_insert_atend_exn (st.texcoords, v)
      in
        true
      end else let
        prval () = opt_unnone {vec2f} (v)
      in
        false
      end // end of [if]
    end // end of [vt]
  | _ when skip_string (ln, "v") => let
      val () = skip_whitespace (ln)
      var v: vec3f
    in
      if :(st: state, ln: string, v: vec3f?) => string_read<vec3f> (ln, v) then let
        prval () = opt_unsome {vec3f} (v)
        val () = $DA.dynarray_insert_atend_exn (st.verts, v)
      in
        true
      end else let
        prval () = opt_unnone {vec3f} (v)
      in
        false
      end // end of [if]
    end // end of [v]
  | _ when skip_string (ln, "f") => let
      val () = skip_whitespace (ln)
      var f: face3
    in
      if :(st: state, ln: string, f: face3?) => string_read<face3> (ln, f) then let
        prval () = opt_unsome {face3} (f)
        val () = $DA.dynarray_insert_atend_exn (st.faces, f)
      in
        true
      end else let
        prval () = opt_unnone {face3} (f)
      in
        false
      end // end of [if]
    end
  | _ (*otherwise*) => true // skip it
end // end of [state_line]

fun loop
  {sz:pos} (
  inp: FILEref
, buf: &bytes(sz)? >> _
, sz: int sz
, state: &state >> _
) : bool = let
  val p = fgets0 (buf, sz, inp)
in
  if :(state: state) => p > 0 then let // p=addr@(buf) or NULL
    var mystr = $UN.cast{string}(p)
  in
      if :(state: state, mystr: string) => state_line (state, mystr) then
        (println!("failed"); false)
      else let
        var fr : double
        val res = atof (mystr, fr)
        val () =
          if :(fr: double?) => res then let
            prval () = opt_unsome {double} (fr)
            val () = println!("read double: ", fr)
	    val () = println!("rest of string: ", mystr)
          in
          end else let
            prval () = opt_unnone {double} (fr)
	    val () = println!("failed to read a double")
	    val () = println!("rest of string: ", mystr)
          in
          end // end of [if]
      in
        loop (inp, buf, sz, state)
      end
  end else true // end of [if]
end // end of [loop]

(* ****** ****** *)

assume mesh = @{
  verts= $DA.dynarray (vec3f),
  normals= $DA.dynarray (vec3f),
  texcoords= $DA.dynarray (vec2f),
  faces= arrayptrsz (vec3i)
} (* end of [mesh] *)

(* ****** ****** *)

extern
fun{a:t@ype}{env:vt@ype}
initize_array_env$elt (
  &env >> _
, size_t // index
, &a? >> opt (a, r)
): #[r:bool] bool (r)

extern
fun{a:t@ype}{env:vt@ype}
initize_array_env_lr {n:int} ( // left to right
  &env >> _
, &(@[a?][n]) >> opt (@[a][n], r)
, size_t n
) : #[r:bool] bool (r)

implement{a}{env}
initize_array_env_lr {n} (env, A, asz) = let
//
fun
aux {i,n:nat | i <= n} {l:addr} (
  pf1_arr: array_v (a, l, i), pf2_arr: array_v (a?, l+i*sizeof(a), n-i)
| p_arr: ptr (l+i*sizeof(a)), i: size_t i, n: size_t n, env: &env >> _
): [r:int] (vor (array_v (a, l, n), array_v (a?, l, n), r) | int r) = (
  if i < n then let
    prval (pf_at, pf2_arr) = array_v_uncons {a?} (pf2_arr)
    prval [l0:addr] EQADDR () = ptr_get_index (p_arr)
    val p_at = p_arr : ptr l0
    prval pf_at = pf_at : a? @ l0
    val res = initize_array_env$elt<a><env> (env, (g0ofg1)i, !p_at)
  in
    if res then let
      prval () = opt_unsome {a} (!p_at)
      prval pf1_arr = array_v_extend {a} (pf1_arr, pf_at)
      val p1_arr = ptr1_succ<a> (p_arr)
    in
      aux (pf1_arr, pf2_arr | p1_arr, succ(i), n, env)
    end else let
      prval p_bas = praxi_ptr {l} ()
      prval () = opt_unnone {a} (!p_at)
      prval () = topize {@[a][i]} (!p_bas)
      prval pf2_arr = array_v_cons {a?} (pf_at, pf2_arr)
      prval pf_res = array_v_unsplit {a?} (pf1_arr, pf2_arr)
    in
      #[.. | (VOR_r (pf_res) | 1)]
    end
  end else let
    prval () = array_v_unnil {a?} (pf2_arr)
    prval pf_res = pf1_arr
  in
    #[.. | (VOR_l (pf_res) | 0)]
  end // end of [if]
) (* end of [aux] *)
//
prval pf1_arr = array_v_nil {a} ()
prval pf2_arr = view@(A)
val p_arr = addr@(A)
prval [l:addr] EQADDR () = eqaddr_make_ptr (p_arr)
prval () = lemma_g1uint_param (asz)
val (pf_vor | r) = aux {0,n} {l} (pf1_arr, pf2_arr |  p_arr, (i2sz)0, asz, env)
//
in
//
if r = 0 then let
  prval VOR_l pf_arr = pf_vor
  prval () = view@A := pf_arr
  prval () = opt_some {@[a][n]} (A)
in
  true
end else let
  prval VOR_r pf_arr = pf_vor
  prval () = view@A := pf_arr
  prval () = opt_none {@[a][n]} (A)
in
  false
end // end of [if]
//
end // end of [initize_array_env_lr]

(* ****** ****** *)

extern
fun
state2mesh (
  src: &state >> opt (state, ~b)
, dst: &mesh? >> opt (mesh, b)
): #[b:bool] bool b

implement
state2mesh (src, dst) = let
(*
  var numverts: size_t
  val verts = $DA.dynarray_getfree_arrayptr (src.verts, numverts)
  var numnormals: size_t
  val normals = $DA.dynarray_getfree_arrayptr (src.normals, numnormals)
  var numtexcoords: size_t
  val texcoords = $DA.dynarray_getfree_arrayptr (src.texcoords, numtexcoords)
  var numfaces: size_t
  val faces = $DA.dynarray_getfree_arrayptr (src.faces, numfaces)
*)
  var numverts: size_t
  val (pf_verts, fpf_verts | p_verts) = $DA.dynarray_get_array (src.verts, numverts)
  var numnormals: size_t
  val (pf_normals, fpf_normals | p_normals) = $DA.dynarray_get_array (src.normals, numnormals)
  var numtexcoords: size_t
  val (pf_texcoords, fpf_texcoords | p_texcoords) = $DA.dynarray_get_array (src.texcoords, numtexcoords)
  var numfaces: size_t
  val (pf_faces, fpf_faces | p_faces) = $DA.dynarray_get_array (src.faces, numfaces)

  prval [numfaces:int] EQINT () = eqint_make_guint (numfaces)

  val (pf_indices, pf_free_indices | p_indices) = array_ptr_alloc<vec3i> (numfaces)

  fun
  get_vec3i_face {l:addr} (
    pf_at : !face3 @ l
  | p_f: ptr l
  , x: &int? >> int
  , y: &int? >> int
  , z: &int? >> int
  ): void = {
    val () = x := vec3i_get_x (!p_f.a)
    val () = y := vec3i_get_y (!p_f.b)
    val () = z := vec3i_get_z (!p_f.c)
  } (* end of [get_vec3i_face] *)

  val nverts = (sz2i)numverts
  // go over all faces, ensure bounds
  implement(env)
  initize_array_env$elt<vec3i><env> (env, i, face) = let
    fun checkbounds (x: int): bool = (x >= 1 && x < (g0ofg1)nverts)

    val i = $UN.cast{sizeLt(numfaces)} (i)
    prval (pf_faces, fpf_faces) = decode($vcopyenv_v(pf_faces))
    val (pf_at_fc, fpf_fc | p_fc) = array_ptr_takeout (pf_faces | p_faces, i)
    var x: int and y: int and z: int
    val () = get_vec3i_face (pf_at_fc | p_fc, x, y, z)
    prval () = pf_faces := fpf_fc (pf_at_fc)
    prval () = fpf_faces (pf_faces)
  in
    if ~checkbounds (x) ||
       ~checkbounds (y) ||
       ~checkbounds (z) then let
      prval () = opt_none {vec3i} (face)
    in
      false
    end else let
      val () = vec3i_init3 (face, x, y, z)
      prval () = opt_some {vec3i} (face)
    in
      true
    end // end of [if]
  end // end of [initize_array_env$elt]
  var myenv : void = ()
  val res = initize_array_env_lr<vec3i><void> (myenv, !p_indices, numfaces)
  //
  prval () = fpf_verts (pf_verts)
  prval () = fpf_normals (pf_normals)
  prval () = fpf_texcoords (pf_texcoords)
  prval () = fpf_faces (pf_faces)
  //
in
  if res then let
    prval () = opt_unsome {@[vec3i][numfaces]} (!p_indices)

    val () = dst.verts := src.verts
    val () = dst.normals := src.normals
    val () = dst.texcoords := src.texcoords
    val () = $DA.dynarray_free (src.faces)
    val indices = arrayptr_encode (pf_indices, pf_free_indices | p_indices)
    val () = dst.faces := arrayptrsz_encode @(indices, numfaces)
    
    prval () = opt_none {state} (src)
    prval () = opt_some {mesh} (dst)
  in
    true
  end else let
    prval () = opt_unnone {@[vec3i][numfaces]} (!p_indices)

    val () = array_ptr_free {vec3i} (pf_indices, pf_free_indices | p_indices)

    prval () = opt_some {state} (src)
    prval () = opt_none {mesh} (dst)
  in
    false
  end
end

(* ****** ****** *)

implement
mesh_delete (mesh) = {
  val () = $DA.dynarray_free (mesh.verts)
  val () = $DA.dynarray_free (mesh.normals)
  val () = $DA.dynarray_free (mesh.texcoords)
  val @(faces, nfaces) = arrayptrsz_decode (mesh.faces)
  val () = arrayptr_free {vec3i} (faces)
}

implement
load_OBJ (input, dest) = let
  #define BUFSZ 256
  var buf = @[byte][BUFSZ]()
  var x: state
  val () = state_new (x)
  val res = loop (input, buf, BUFSZ, x)
in
  if :(x: state?) => res then (
    if :(x: state?) => state2mesh (x, dest) then let
      prval () = opt_unnone {state} (x)
    in
      true
    end else let
      prval () = opt_unsome {state} (x)
      val () = state_delete (x)
    in
      false
    end
  ) else let
    val () = state_delete (x)
    prval () = opt_none (dest)
  in
    false
  end
end
