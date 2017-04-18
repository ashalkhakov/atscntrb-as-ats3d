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

TEST00_SOURCES_SATS = \
  ../SATS/vector.sats
TEST00_SOURCES_DATS = \
  ../DATS/vec3f.dats \
  test00.dats

test00_C_O := \
  $(patsubst %.c, %_c.o, $(SOURCES_C))
test00_SATS_O := \
  $(patsubst %.sats, %_sats.o, $(TEST00_SOURCES_SATS))
test00_DATS_O := \
  $(patsubst %.dats, %_dats.o, $(TEST00_SOURCES_DATS))
all:: test00.exe
test00.exe: \
  $(test00_C_O) \
  $(test00_SATS_O) \
  $(test00_DATS_O) ; \
  $(PATSCC) $(INCLUDE) $(MALLOCFLAG) $(CFLAGS) -o $@ $^ $(LDFLAGS) $(EXTRAFLAGS)
regress:: test00.exe; ./$<
cleanall:: ; $(RMF) test00.exe

######

TEST01_SOURCES_SATS = \
  ../SATS/vector.sats \
  ../SATS/matrix.sats \
  ../SATS/quaternion.sats

TEST01_SOURCES_DATS = \
  ../DATS/vec2f.dats \
  ../DATS/vec3f.dats \
  ../DATS/vec4f.dats \
  ../DATS/matrix.dats \
  ../DATS/mat3x3f.dats \
  ../DATS/mat4x4f.dats \
  ../DATS/quaternion.dats \
  test01.dats

test01_C_O := \
  $(patsubst %.c, %_c.o, $(SOURCES_C))
test01_SATS_O := \
  $(patsubst %.sats, %_sats.o, $(TEST01_SOURCES_SATS))
test01_DATS_O := \
  $(patsubst %.dats, %_dats.o, $(TEST01_SOURCES_DATS))
all:: test01.exe
test01.exe: \
  $(test01_C_O) \
  $(test01_SATS_O) \
  $(test01_DATS_O) ; \
  $(PATSCC) $(INCLUDE) $(MALLOCFLAG) $(CFLAGS) -o $@ $^ $(LDFLAGS) $(EXTRAFLAGS)
regress:: test01.exe; ./$<
cleanall:: ; $(RMF) test01.exe

######

TEST02_SOURCES_SATS = \
  ../SATS/file_util.sats \
  ../SATS/image/TGA.sats \
  ../SATS/image/PPM.sats
TEST02_SOURCES_DATS = \
  ../DATS/file_util.dats \
  ../DATS/image/TGA.dats \
  ../DATS/image/PPM.dats \
  test02.dats

test02_C_O := \
  $(patsubst %.c, %_c.o, $(SOURCES_C))
test02_SATS_O := \
  $(patsubst %.sats, %_sats.o, $(TEST02_SOURCES_SATS))
test02_DATS_O := \
  $(patsubst %.dats, %_dats.o, $(TEST02_SOURCES_DATS))
all:: test02.exe
test02.exe: \
  $(test02_C_O) \
  $(test02_SATS_O) \
  $(test02_DATS_O) ; \
  $(PATSCC) $(INCLUDE) $(MALLOCFLAG) $(CFLAGS) -o $@ $^ $(LDFLAGS) $(EXTRAFLAGS)
regress:: test02.exe; ./$<
cleanall:: ; $(RMF) test02.exe

######

TEST03_SOURCES_SATS = \
  ../SATS/image/PPM.sats \
  ../SATS/image/pixmap.sats \
  ../SATS/vector.sats
TEST03_SOURCES_DATS = \
  ../DATS/image/PPM.dats \
  ../DATS/image/pixmap.dats \
  ../DATS/vec3i.dats \
  test03.dats

test03_C_O := \
  $(patsubst %.c, %_c.o, $(SOURCES_C))
test03_SATS_O := \
  $(patsubst %.sats, %_sats.o, $(TEST03_SOURCES_SATS))
test03_DATS_O := \
  $(patsubst %.dats, %_dats.o, $(TEST03_SOURCES_DATS))
all:: test03.exe
test03.exe: \
  $(test03_C_O) \
  $(test03_SATS_O) \
  $(test03_DATS_O) ; \
  $(PATSCC) $(INCLUDE) $(MALLOCFLAG) $(CFLAGS) -o $@ $^ $(LDFLAGS) $(EXTRAFLAGS)
regress:: test03.exe; ./$<
cleanall:: ; $(RMF) test03.exe

######

TEST04_SOURCES_SATS = \
  ../SATS/mesh/OBJ.sats \
  ../SATS/image/pixmap.sats \
  ../SATS/image/PPM.sats \
  ../SATS/vector.sats \
  ../SATS/matrix.sats
TEST04_SOURCES_DATS = \
  ../DATS/mesh/OBJ.dats \
  ../DATS/image/pixmap.dats \
  ../DATS/image/PPM.dats \
  ../DATS/vec2i.dats \
  ../DATS/vec3i.dats \
  ../DATS/vec2f.dats \
  ../DATS/vec3f.dats \
  ../DATS/vec4f.dats \
  ../DATS/mat3x3f.dats \
  ../DATS/mat4x4f.dats \
  test04.dats

test04_C_O := \
  $(patsubst %.c, %_c.o, $(SOURCES_C))
test04_SATS_O := \
  $(patsubst %.sats, %_sats.o, $(TEST04_SOURCES_SATS))
test04_DATS_O := \
  $(patsubst %.dats, %_dats.o, $(TEST04_SOURCES_DATS))
all:: test04.exe
test04.exe: \
  $(test04_C_O) \
  $(test04_SATS_O) \
  $(test04_DATS_O) ; \
  $(PATSCC) $(INCLUDE) $(MALLOCFLAG) $(CFLAGS) -o $@ $^ $(LDFLAGS) $(EXTRAFLAGS)
regress:: test04.exe; ./$<
cleanall:: ; $(RMF) test04.exe

TEST04_MESH_FILES = \
tri2d.obj floor.obj african_head.obj
test04_MESH_PPMS := \
  $(patsubst %.ppm, %.obj, $(TEST04_MESH_FILES))

%.ppm: %.obj test04.exe
	./test04.exe $< $@
regress:: %.ppm
cleanall:: ; $(RMF) *.ppm

######

include $(PATSHOME)/share/atsmake-post.mk

######

testall:: all
testall:: cleanall

######

cleanall:: ; $(RMF) MYPORTDIR/*

######

###### end of [Makefile] ######

