staload "./vector.sats"

#define ATS_PACKNAME "ATSCNTRB.as.ats3d"
#define ATS_EXTERN_PREFIX "atscntrb_as_ats3d_" // prefix for external names

absvtype sampler2D (a:t@ype) = ptr

fun
sampler_new (basename: string, extension: string, res: &ptr? >> opt (sampler2D(uint32), b)): #[b:bool] bool b

fun{a:t@ype}
sampler_delete (sampler2D(INV(a))): void

fun{a:t@ype}
sampler_lookup (!sampler2D(a), INV(a), &vec2f): a
