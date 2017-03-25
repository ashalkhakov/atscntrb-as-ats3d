#include
"share/atspre_staload.hats"

staload STR = "libats/libc/SATS/string.sats"
staload "libats/libc/SATS/stdio.sats"
staload
UN="prelude/SATS/unsafe.sats"

staload "./../../SATS/mesh/OBJ.sats"

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
end

(* ****** ****** *)

fun loop
  {sz:pos} (
  inp: FILEref
, buf: &bytes(sz)? >> _
, sz: int sz
) : void = let
  val p = fgets0 (buf, sz, inp)
in
  if p > 0 then let // p=addr@(buf) or NULL
    //val () = test_drive ($UN.cast{string}(p))
    var mystr = $UN.cast{string}(p)

    val () =
      if :(mystr: string) => ~skip_string (mystr, "=") then println!("failed, please input leading '='")
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
      end
    // end of [val]
  in
    loop (inp, buf, sz)
  end else () // end of [if]
end // end of [loop]

fun
read_from_stdin (): void = {
  #define BUFSZ 128
  var buf = @[byte][BUFSZ]()
  val () = loop (stdin_ref, buf, BUFSZ)
}

(* ****** ****** *)

implement
load_OBJ (input) = let
in
end

implement
main0 () = read_from_stdin ()
