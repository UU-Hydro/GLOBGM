BINDIR = ./bin/
PROGRAM = mf6ggm
F90FLAGS = -O3 -fpp -DLINUX -DIFORT -assume buffered_io
F90 = ifort
USRLIBS = -L//metis-5.1.0_64bit/build/Linux-x86_64/libmetis/ -lmetis
OBJECTS = \
	imod_idf_par.o \
	metis_enum_5.1.0.o \
	imod_utl.o \
	metis_module.o \
	imod_idf_lib.o \
	imod_idf_mod.o \
	utils_mod.o \
	pcrlib.o \
	ehdr_lib.o \
	mf6_module.o \
	mf6ggm.o \
	utl.o \
	utils.o \
	geometry.o
all: mf6ggm_181121
mf6ggm_181121: $(OBJECTS)
	-$(F90) $(F90FLAGS) -o $(BINDIR)$(PROGRAM) $(OBJECTS) $(USRLIBS) $(SYSLIBS)
%.o: %.f
	$(F90) $(F90FLAGS) -c $<
%.o: %.F
	$(F90) $(F90FLAGS) -c $<
%.o: %.for
	$(F90) $(F90FLAGS) -c $<
%.o: %.FOR
	$(F90) $(F90FLAGS) -c $<
%.o: %.f90
	$(F90) $(F90FLAGS) -c $<
%.o: %.F90
	$(F90) $(F90FLAGS) -c $<
%.o: %.fpp
	$(F90) $(F90FLAGS) -c $<
clean:
	rm -f *.o *.mod