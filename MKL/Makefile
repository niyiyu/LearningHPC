FC = mpif90
FCFLAGS = -O3
DFLAG = _with_mpi

BLASDIR = /opt/intel/oneapi/mkl/2021.3.0
IBLAS = -I$(BLASDIR)/include/intel64/lp64/
LBLAS = -L$(BLASDIR)/lib -lmkl_blas95_lp64

MKLROOT = /opt/intel/oneapi/mkl/2021.3.0
IMKL = -I${MKLROOT}/include
LMKL = -L${MKLROOT}/lib -Wl,-rpath,${MKLROOT}/lib -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread -lm -ldl

INC = $(IMKL) $(IBLAS)
LIB = $(LMKL) $(LBLAS)



OBJ = cg_par.o \
MPI_TIME.o\
main.o \
diag.o \
random_metrix.o \
orth.o \
random_vector.o

 
main:	$(OBJ)
	$(FC) $(INC) $(FCFLAGS) -D$(DFLAG) -o main $(OBJ)  $(LIB)

%.o:	%.F90
	$(FC) $(INC) $(FCFLAGS) -D$(DFLAG) -c $< 
#$(LIB)

.PHONY:	clean

clean:
	@rm -f $(OBJ)
	@rm -f *.mod