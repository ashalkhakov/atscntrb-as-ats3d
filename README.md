# ATS3D

ATS3D is a low-dimensional vector algebra library for
[ATS2](http://www.ats-lang.org/).

## Features

* 2- 3- and 4-dimensional vectors
* 3x3- and 4x4-dimensional matrices
* function templates for iterating over components of a vector

## Usage examples

Working with 3D vectors:

```
var a : vec3f
val () = a.init (~1.0f)
var b : vec3f
val () = b.init (1.0f, 0.0f, 0.0f)
val () = println!("a.x = ", a.x())
var c = a + b
val () = println!("c = ", c)

```

## Running tests

Simply run `make` in `src/TEST`.

## Some results

Gouraud-shaded, untextured Wavefront OBJ meshes rendered using
`test04.exe`:

* ![cow2](/doc/cow2.jpg?raw=true "cow2")
* ![african_head](/doc/african_head.jpg?raw=true "african_head")

## License

Distributed under the 3-clause BSD (see the LICENSE file)

Mesh files are copyright their respective owners.

--Artyom Shalkhakov
