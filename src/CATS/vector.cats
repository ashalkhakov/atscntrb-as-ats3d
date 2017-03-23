#ifndef ATS3D_CATS_VECTOR
#define ATS3D_CATS_VECTOR

typedef
struct {
  float V[2];
} vec2f_t;

typedef
struct {
  float V[3];
} vec3f_t;

typedef
struct {
  float V[4];
}
/* __attribute__((__aligned__(16))) */
vec4f_t;

#endif // ifndef ATS3D_CATS_VECTOR
