// what would it be?

datatype pixelfmt (t@ype) =
  | PFrgb_3ub (@[byte][3])
  | PFrgba_4ub (@[byte][4])
  | PFgrayscale_ub (byte)
typedef pixelfmt0 = [t:t@ype] pixelfmt (t)

// what are the functions?
// - get at row/col
// - set at row/col
// - initialize with a dummy?
// - initialize through some proof manipulation (?)
//   - e.g. row by row or some such

// right, so instead of thinking it through...
// just port your old TGA code, for now
// - it is interesting to learn how G3D manages this, but... that's too long a wait
// - to be clear, for the task at hand (pure software renderer), we only want TGA load/save code
//   - we do not want to touch other formats

abst@ype image (t@ype, int, int) = @{
  width= int
, height= int
, pimg= ptr
, pixelfmt= pixelfmt0
}

fun


typedef image = [w,h:nat] [t:t@ype] [l:addr] @{
  pfmat= matrix_v (t, l, w, h)
, pfree= mfree_gc_v (l)
, width= int w
, height= int h
, pimg= ptr l
, pixelfmt= pixelfmt t
}