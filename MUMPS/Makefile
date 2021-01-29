FC = mpif90
FCFLAGS = -O3 
BLASDIR = /opt/intel/blas
IBLAS = -I$(BLASDIR)/include/intel64/lp64/
LBLAS = -L$(BLASDIR)/lib -lmkl_blas95_lp64
# LBLAS = -L//usr/local/Cellar/openblas/0.3.10_1/lib -lopenblas

LPORDDIR = /Users/niyiyu/Desktop/Senior_thesis/TOMO2TOC/MUMPS_5.3.0/PORD
IPORD    = -I$(LPORDDIR)/include
LPORD    = -L$(LPORDDIR)/lib -lpord

MKLROOT = /opt/intel/mkl
IMKL = -I${MKLROOT}/include
LMKL = -L$(MKLROOT)/lib -Wl,-rpath,${MKLPATH}/lib -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core  -lmkl_lapack95_lp64 -fopenmp -liomp5 -lpthread -lm -ldl -lmkl_blacs_mpich_lp64 -lmkl_scalapack_lp64

# LMUMPS = -L/usr/local/Cellar/mumps/5.3.0/libexec/lib/ -lcmumps -lmumps_common -L/usr/local/Cellar/gcc/10.2.0/lib/gcc/10/ -lgfortran
# IMUMPS = -I/usr/local/Cellar/mumps/5.3.0/include 

LMUMPS = -L/Users/niyiyu/Desktop/Senior_thesis/TOMO2TOC/MUMPS_5.3.0/lib -lcmumps -lmumps_common -L/usr/local/Cellar/gcc/10.2.0/lib/gcc/10/ -lgfortran
IMUMPS = -I/Users/niyiyu/Desktop/Senior_thesis/TOMO2TOC/MUMPS_5.3.0/include

INC = $(IMKL) $(IBLAS) $(IMUMPS) $(IPORD)
LIB = $(LMKL) $(LBLAS) $(LMUMPS) $(LPORD)


test:	csimpletest.F
	$(FC) $(INC) $(FCFLAGS) -o test $(OBJ)  $(LIB)
OBJ = main.f90
 
main:	$(OBJ)
	$(FC) $(INC) $(FCFLAGS) -o main $(OBJ)  $(LIB)
#$(LIB)

.PHONY:	clean

clean:
	@rm -f main
	@rm -f test
	@rm -f *.mod