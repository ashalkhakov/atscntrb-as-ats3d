#include
"share/atspre_staload.hats"

staload "libats/libc/SATS/math.sats" // NOTE: requires -lm during compilation
staload _ = "libats/libc/DATS/math.dats"

staload "../SATS/vector.sats"
staload _ = "../DATS/vec3f.dats"

implement
main0 () = let
//
var normal : vec3f
val () = normal.init (1.0f, 0.0f, 0.0f)
//
var light_color : vec3f
val () = light_color.init (1.0f, 1.0f, 0.0f)
var light_dir : vec3f
val () = light_dir.init (~1.0f, 0.0f, 0.0f)
val light_intensity = 3.0f
//
val diffuse_factor = dotprod (normal, neg_light_dir) where {
    var normal = normal.normalize ()
    var neg_light_dir = ~light_dir
} (* end of [val] *)
//
val diffuse_factor = if diffuse_factor <= 0.0f then 0.0f else diffuse_factor
//
var diffuse_color : vec3f
val () = diffuse_color := g0float_mul_float (light_intensity, diffuse_factor) * light_color
//
val () = println!("diffuse color = ", diffuse_color)

in
end