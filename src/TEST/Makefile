#
# A simple Makefile
#

######

include \
$(PATSHOME)/share/atsmake-pre.mk

######

CFLAGS += -O3

######

LDFLAGS := -lm

######

PATSCCOMP = $(CC) -D_XOPEN_SOURCE

######

SOURCES_SATS += \
  ../SATS/vector.sats \
  ../SATS/matrix.sats \
  ../SATS/quaternion.sats \

SOURCES_DATS += \
  ../DATS/vec2f.dats \
  ../DATS/vec3f.dats \
  ../DATS/vec4f.dats \
  ../DATS/matrix.dats \
  ../DATS/mat3x3f.dats \
  ../DATS/mat4x4f.dats \
  ../DATS/quaternion.dats \
  test01.dats

######

# TODO: there can be only one target per makefile!
# we want many targets! (test01, test02, test03)
MYTARGET=test01

######
#
MYPORTDIR=MYPORTDIR
#
#MYPORTCPP=MYPORTCPP
#
######

include $(PATSHOME)/share/atsmake-post.mk

######

testall:: all
testall:: cleanall

######

cleanall:: ; $(RMF) MYPORTDIR/*

######

###### end of [Makefile] ######

