    program conjugate_gradient
    use f95_precision
    use blas95
    use MPI_TIMES
    use cg_par


#ifdef _with_mpi
    include 'mpif.h'
    call mpi_init(ierror)
    call mpi_comm_size(MPI_COMM_WORLD,comm_sz,ierror)
    call mpi_comm_rank(MPI_COMM_WORLD,my_rank,ierror)

    if(my_rank.eq.0)    then
        write(*,*)"Using MPI"
        write(*,*)"Input the dimension of equation:"
        read(*,*)dim 
    endif
    call MPI_Barrier(MPI_COMM_WORLD, ierror)
    call MPI_Bcast(dim, 1, MPI_INT, 0, MPI_COMM_WORLD, ierror)
#else
    write(*,*)"Not using MPI"
    my_rank = 0
    comm_sz = 1
    write(*,*)"Input the dimension of equation:"
    read *,dim
#endif
    call mpi_time(0)
    open(unit=22, file = './log.dat', status='replace')
    open(unit=23, file = './misfit.dat', status='replace')

    allocate(D(1:dim,1:dim), U(1:dim,1:dim), A(1:dim,1:dim), stat=ierror)
    allocate(x(1:dim), x_real(1:dim), b(1:dim),residual(1:dim), stat=ierror)
    allocate(res_tmp(1:dim), stat=ierror)
    
    if(ierr.ne.0) then
        stop
    end if

    call diag(D,dim)

    call random_metrix(U,dim)

    call orth(U,dim)

    call sgemm('N','N',dim,dim,dim,1.0,D,dim,U,dim,0,D,dim)
    call sgemm('T','N',dim,dim,dim,1.0,U,dim,D,dim,0,A,dim)

    call random_vector(x_real,dim)
    call sscal(dim,100.0,x_real,1)
    call sgemv('N', dim, dim, 1.0, A,dim,x_real,1,0.0,b,1)
    
    call random_vector(x,dim) 
    call sscal(dim,100.0,x,1)
    
    write(22,*)A(1,1:2)
    write(22,*)A(2,1:2)
    write(22,*)x_real
    write(22,*)b
    iloop : do i=1,MAX_ITER
        call sgemv('N', dim, dim, -1.0, A, dim, x, 1, 0.0, residual, 1)
        call saxpy(dim,1.0,b,1,residual,1)

        res = snrm2(dim,residual,1)
        write(23,*)res
        write(22,*)x
        if(res.le.tol)   then
            print *,"Tolerance reached. Finishing."
            exit iloop
        end if 

        dot_tmp = dot(residual,residual)
        call sgemv('N', dim, dim, 1.0, A,dim,residual,1,0.0,res_tmp,1)
        dot_res_tmp = dot(residual,res_tmp)
        
        alpha = dot_tmp/dot_res_tmp

        call saxpy(dim,alpha,residual,1,x,1)
    
    end do iloop





    call mpi_time(1)
#ifdef _with_mpi
    call mpi_finalize(ierror)
#endif
    
    end program