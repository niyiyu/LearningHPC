FC = mpifort
FCFLAGS = -O3
DFLAG = WITHMPI
BLASDIR = /opt/intel/blas
IBLAS = -I$(BLASDIR)/include/intel64/lp64/
LBLAS = -L$(BLASDIR)/lib -lmkl_blas95_lp64


MKLROOT = /opt/intel/mkl
IMKL = -I${MKLROOT}/include
LMKL = -L${MKLROOT}/lib -Wl,-rpath,${MKLROOT}/lib -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -lpthread -lm -ldl

INC = $(IMKL) $(IBLAS)
LIB = $(LMKL) $(LBLAS)



OBJ = MPI_TIME.o\
hello.o\
simulate.o
 
main:	$(OBJ)
	$(FC) $(INC) $(FCFLAGS) -D$(DFLAG) -o main $(OBJ)  $(LIB)

%.o:	%.F90
	$(FC)  $(INC) $(FCFLAGS) -D$(DFLAG) -c $< 
#$(LIB)

.PHONY:	clean

clean:
	@rm -f $(OBJ)
	@rm -f *.mod
