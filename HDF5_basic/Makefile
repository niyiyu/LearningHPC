FC = gfortran
FCFLAGS = -O3
# DFLAG = _with_mpi
BLASDIR = #/opt/intel/blas
IBLAS = #-I$(BLASDIR)/include/intel64/lp64/
LBLAS = 


MKLROOT = #/opt/intel/mkl
IMKL = #-I${MKLROOT}/include
LMKL = #-L${MKLROOT}/lib -Wl,-rpath,${MKLROOT}/lib -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -lpthread -lm -ldl

INC = $(IMKL) $(IBLAS) -I/opt/hdf5/include/
LIB = $(LMKL) $(LBLAS) -L/opt/hdf5/lib -lhdf5 -lhdf5_fortran

 
all:	
	$(FC) $(INC)  $(FCFLAGS) $(DFLAG) main.f90 -o main  $(LIB)

# %.o:	%.f90
#	$(FC)  $(INC) $(FCFLAGS) -D$(DFLAG) -c $< 
#$(LIB)

.PHONY:	clean

clean:
	@rm -f $(OBJ)
	@rm -f *.mod
	@rm -f *.h5
