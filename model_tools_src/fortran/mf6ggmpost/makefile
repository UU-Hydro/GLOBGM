BINDIR = ./bin/
PROGRAM = mf6ggmpost
F90FLAGS = -O3 -fpp -DLINUX -DIFORT -assume buffered_io
F90 = ifort
OBJECTS = \
	imod_idf_par.o \
	imod_utl.o \
	imod_idf_lib.o \
	imod_idf_mod.o \
	utils_mod.o \
	ehdr_lib.o \
	pcrlib.o \
	mf6_post_module.o \
	mf6ggmpost.o \
	utl.o \
	utils.o \
	geometry.o
all: mf6ggmpost
mf6ggmpost: $(OBJECTS)
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