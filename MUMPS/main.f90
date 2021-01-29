    program example

    implicit none
    include 'mpif.h'
    include 'cmumps_struc.h'

    type(cmumps_struc) mumps_par
    
    integer ierr, i, i8, my_rank, comm_sz
    logical flag

    call mpi_init(ierr)
    call mpi_comm_size(MPI_COMM_WORLD,comm_sz,ierr)
    call mpi_comm_rank(MPI_COMM_WORLD,my_rank,ierr)
    


    mumps_par%COMM = MPI_COMM_WORLD
    mumps_par%JOB = -1
    mumps_par%SYM = 0
    mumps_par%PAR = 1
    
    call cmumps(mumps_par)
    write(*,*)mumps_par%MYID
    if(mumps_par%MYID.eq.0)then
        ! write(*,*)mumps_par%NPROCS
        read(5,*)mumps_par%N
        read(5,*)mumps_par%NNZ
        
        allocate(mumps_par%IRN(mumps_par%NNZ))
        allocate(mumps_par%JCN(mumps_par%NNZ))
        allocate(mumps_par%A(mumps_par%NNZ))
        allocate(mumps_par%RHS(mumps_par%NNZ))
        do i8=1,mumps_par%NNZ
            read(5,*)mumps_par%IRN(i8),mumps_par%JCN(i8),mumps_par%A(i8)
        end do
        read(5,*)(mumps_par%RHS(I),I=1,mumps_par%N)
    end if
    call mpi_barrier(MPI_COMM_WORLD, ierr)

    mumps_par%JOB = 6
    call cmumps(mumps_par)

    if(mumps_par%MYID.eq.0)then
        write(6,*)(mumps_par%RHS(I),I=1,mumps_par%N)
    end if
    if(mumps_par%MYID.eq.0)then
        deallocate(mumps_par%IRN)
        deallocate(mumps_par%JCN)
        deallocate(mumps_par%A)
        deallocate(mumps_par%RHS)
    end if

    mumps_par%JOB = -2
    call cmumps(mumps_par)

    call mpi_finalize(ierr)
    end program example