//
#define ATS_DYNLOADFLAG 0
//
#include
"share/atspre_staload.hats"

staload "./../SATS/matrix.sats"

implement
gdet_treshold<float> () = 1e-7f
implement
gdet_treshold<double> () = 1e-15

