BINDIR = ./bin/
PROGRAM = mf6
F90FLAGS = -O3 -fpp -DMPI_PARALLEL -assume buffered_io
F90 = mpiifort
OBJECTS = \
	kind.o \
	OpenSpec.o \
	HashTable.o \
	Sparse.o \
	compilerversion.o \
	Constants.o \
	ims8reordering.o \
	BaseGeometry.o \
	defmacro.o \
	SmoothingFunctions.o \
	MpiWrapper.o \
	PackageBudget.o \
	Xt3dAlgorithm.o \
	SimVariables.o \
	MpiExchangeGen.o \
	genericutils.o \
	List.o \
	Timer.o \
	version.o \
	ArrayHandlers.o \
	Message.o \
	mf6lists.o \
	TimeSeriesRecord.o \
	StringList.o \
	ObsOutput.o \
	Sim.o \
	sort.o \
	Iunit.o \
	MemoryHelper.o \
	Budget.o \
	InputOutput.o \
	CircularGeometry.o \
	BlockParser.o \
	HeadFileReader.o \
	comarg.o \
	TableTerm.o \
	BudgetFileReader.o \
	DisvGeom.o \
	ObsOutputList.o \
	RectangularGeometry.o \
	PrintSaveManager.o \
	ArrayReaders.o \
	Table.o \
	NameFile.o \
	TimeSeries.o \
	TimeSeriesLink.o \
	Memory.o \
	TimeSeriesFileList.o \
	ListReader.o \
	MpiWrapperMemory.o \
	MemoryList.o \
	MemoryManager.o \
	MpiExchangeCol.o \
	BaseModel.o \
	Connections.o \
	MpiExchange.o \
	MemorySetHandler.o \
	tdis.o \
	ims8linear.o \
	PackageMover.o \
	UzfCellGroup.o \
	BaseSolution.o \
	TimeSeriesManager.o \
	DiscretizationBase.o \
	SolutionGroup.o \
	Mover.o \
	BaseExchange.o \
	gwf3disu8.o \
	MpiMvr.o \
	gwf3dis8.o \
	Xt3dInterface.o \
	OutputControlData.o \
	NumericalPackage.o \
	BudgetTerm.o \
	gwf3disv8.o \
	Observe.o \
	TimeArray.o \
	gwf3ic8.o \
	gwf3hfb8.o \
	ObsContainer.o \
	TimeArraySeries.o \
	gwf3sto8.o \
	BudgetObject.o \
	OutputControl.o \
	ObsUtility.o \
	TimeArraySeriesLink.o \
	gwt1ic1.o \
	gwf3npf8.o \
	gwf3mvr8.o \
	Obs3.o \
	gwf3oc8.o \
	gwt1oc1.o \
	TimeArraySeriesManager.o \
	gwf3csub8.o \
	gwt1obs1.o \
	gwf3obs8.o \
	BoundaryPackage.o \
	gwt1cnc1.o \
	gwf3lak8.o \
	gwf3ghb8.o \
	gwf3sfr8.o \
	gwf3evt8.o \
	gwf3rch8.o \
	gwf3wel8.o \
	gwt1fmi1.o \
	gwf3chd8.o \
	gwf3drn8.o \
	gwf3riv8.o \
	NumericalModel.o \
	gwf3uzf8.o \
	gwt1src1.o \
	gwf3maw8.o \
	gwt1mst1.o \
	gwt1adv1.o \
	GhostNode.o \
	gwt1dsp.o \
	gwt1ssm1.o \
	gwt1apt1.o \
	gwt1mvt1.o \
	gwf3buy8.o \
	NumericalExchange.o \
	gwt1uzt1.o \
	gwt1lkt1.o \
	gwt1sft1.o \
	gwt1ist1.o \
	gwf3.o \
	gwt1mwt1.o \
	gwt1.o \
	MpiExchangeGwf.o \
	GwfGwtExchange.o \
	GwfHalo.o \
	GwfGwfExchange.o \
	NumericalSolution.o \
	SimulationCreate.o \
	mf6core.o \
	mf6.o
all: mf6
mf6: $(OBJECTS)
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
