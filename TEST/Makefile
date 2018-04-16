#
# A simple Makefile
#

######

include \
$(PATSHOME)/share/atsmake-pre.mk

######

CFLAGS += -O3
CFLAGS += -I./../../

######

LDFLAGS := -lm

######

PATSCCOMP = $(CC) -D_XOPEN_SOURCE

######

SOURCES_SATS += \
  ../src/SATS/vector.sats \
  ../src/SATS/matrix.sats

SOURCES_DATS += \
  ../src/DATS/vec2f.dats \
  ../src/DATS/vec3f.dats \
  ../src/DATS/vec4f.dats \
  ../src/DATS/matrix.dats \
  ../src/DATS/mat3x3f.dats \
  ../src/DATS/mat4x4f.dats \
  test01.dats

######

# TODO: there can be only one target per makefile!
# we want many targets! (test01, test02, test03)
MYTARGET=test01

######
#
MYCCRULE=MYCCRULE

%_sats.o: %.sats ; \
  $(PATSCC) $(INCLUDE) $(INCLUDE_ATS) $(CFLAGS) -o $@ -c $<
%_dats.o: %.dats ; \
  $(PATSCC) $(INCLUDE) $(INCLUDE_ATS) $(MALLOCFLAG) $(CFLAGS) -o $@ -c $<
#
MYPORTDIR=MYPORTDIR
#
#MYPORTCPP=MYPORTCPP
#
######

TEST00_SOURCES_SATS = \
  ../src/SATS/vector.sats
TEST00_SOURCES_DATS = \
  ../src/DATS/vec3f.dats \
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
cleanall:: ; $(RMF) $(test00_C_O) $(test00_SATS_O) $(test00_DATS_O)
cleanall:: ; $(RMF) test00.exe test00_dats.c

######

TEST01_SOURCES_SATS = \
  ../src/SATS/vector.sats \
  ../src/SATS/matrix.sats \

TEST01_SOURCES_DATS = \
  ../src/DATS/vec2f.dats \
  ../src/DATS/vec3f.dats \
  ../src/DATS/vec4f.dats \
  ../src/DATS/matrix.dats \
  ../src/DATS/mat3x3f.dats \
  ../src/DATS/mat4x4f.dats \
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
cleanall:: ; $(RMF) $(test01_C_O) $(test01_SATS_O) $(test01_DATS_O)
cleanall:: ; $(RMF) test01.exe test01_dats.c

######

TEST02_SOURCES_SATS = \
  ../src/SATS/file_util.sats \
  ../src/SATS/image/TGA.sats \
  ../src/SATS/image/PPM.sats
TEST02_SOURCES_DATS = \
  ../src/DATS/file_util.dats \
  ../src/DATS/image/TGA.dats \
  ../src/DATS/image/PPM.dats \
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
cleanall:: ; $(RMF) $(test02_C_O) $(test02_SATS_O) $(test02_DATS_O)
cleanall:: ; $(RMF) test02.exe

######

TEST03_SOURCES_SATS = \
  ../src/SATS/image/PPM.sats \
  ../src/SATS/image/pixmap.sats \
  ../src/SATS/vector.sats
TEST03_SOURCES_DATS = \
  ../src/DATS/image/PPM.dats \
  ../src/DATS/image/pixmap.dats \
  ../src/DATS/vec3i.dats \
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
cleanall:: ; $(RMF) $(test03_C_O) $(test03_SATS_O) $(test03_DATS_O) output.ppm
cleanall:: ; $(RMF) test03.exe

######

TEST04_SOURCES_SATS = \
  ../src/SATS/mesh/OBJ.sats \
  ../src/SATS/image/pixmap.sats \
  ../src/SATS/file_util.sats \
  ../src/SATS/image/TGA.sats \
  ../src/SATS/image/PPM.sats \
  ../src/SATS/raster.sats \
  ../src/SATS/sampler.sats \
  ../src/SATS/shader_sample.sats \
  ../src/SATS/vector.sats \
  ../src/SATS/matrix.sats
TEST04_SOURCES_DATS = \
  ../src/DATS/mesh/OBJ.dats \
  ../src/DATS/image/pixmap.dats \
  ../src/DATS/file_util.dats \
  ../src/DATS/image/TGA.dats \
  ../src/DATS/image/PPM.dats \
  ../src/DATS/raster.dats \
  ../src/DATS/sampler.dats \
  ../src/DATS/shader_sample.dats \
  ../src/DATS/vec2i.dats \
  ../src/DATS/vec3i.dats \
  ../src/DATS/vec2f.dats \
  ../src/DATS/vec3f.dats \
  ../src/DATS/vec4f.dats \
  ../src/DATS/mat3x3f.dats \
  ../src/DATS/mat4x4f.dats \
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
cleanall:: ; $(RMF) $(test04_C_O) $(test04_SATS_O) $(test04_DATS_O)
cleanall:: ; $(RMF) test04.exe

TEST04_MESH_FILES = \
data/tri2d.obj data/floor.obj data/cow2.obj data/african_head.obj
test04_MESH_PPMS := \
  $(patsubst %.obj, %-out.ppm, $(TEST04_MESH_FILES))

%-out.ppm: %.obj test04.exe
	./test04.exe $< $@
regress:: $(test04_MESH_PPMS)
cleanall:: ; $(RMF) $(test04_MESH_PPMS)

######

VTERRAIN_SOURCES_SATS = \
  ../src/SATS/file_util.sats \
  ../src/SATS/image/TGA.sats \
  ../src/SATS/image/PPM.sats
VTERRAIN_SOURCES_DATS = \
  ../src/DATS/file_util.dats \
  ../src/DATS/image/TGA.dats \
  ../src/DATS/image/PPM.dats \
  vterrain.dats

vterrain_C_O := \
  $(patsubst %.c, %_c.o, $(SOURCES_C))
vterrain_SATS_O := \
  $(patsubst %.sats, %_sats.o, $(VTERRAIN_SOURCES_SATS))
vterrain_DATS_O := \
  $(patsubst %.dats, %_dats.o, $(VTERRAIN_SOURCES_DATS))
all:: vterrain.exe
vterrain.exe: \
  $(vterrain_C_O) \
  $(vterrain_SATS_O) \
  $(vterrain_DATS_O) ; \
  $(PATSCC) $(INCLUDE) $(MALLOCFLAG) $(CFLAGS) -o $@ $^ $(LDFLAGS) $(EXTRAFLAGS)
regress:: vterrain.exe; ./$<
cleanall:: ; $(RMF) $(vterrain_C_O) $(vterrain_SATS_O) $(vterrain_DATS_O)
cleanall:: ; $(RMF) vterrain.exe

######

include $(PATSHOME)/share/atsmake-post.mk

######

testall:: all
testall:: cleanall

######

cleanall:: ; $(RMF) *_dats.c *_sats.c
cleanall:: ; $(RMF) MYPORTDIR/*

######

###### end of [Makefile] ######

