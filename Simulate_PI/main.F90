    program simu_mi
    !use f95_precision
    !use blas95
    use mpi_times
    !include 'mpif.h'

    integer(kind=4) ierror,my_rank,comm_sz
    integer(kind=8) n, number_in_circle, local_n, local_number_in_circle
    real(kind=8) simu_pi

#ifdef _with_mpi
    include 'mpif.h'
    call mpi_init(ierror)
    call mpi_comm_size(MPI_COMM_WORLD,comm_sz,ierror)
    call mpi_comm_rank(MPI_COMM_WORLD,my_rank,ierror)
    if(my_rank.eq.0)then
        write(*,*)"Enable mpi!"
        write(*,*)"Input the times of simulation:"
        read *,n
        local_n = n/comm_sz
    end if

    call mpi_bcast(local_n, 1, MPI_LONG, 0, MPI_COMM_WORLD, ierror)
    
    call mpi_time(0)

    call simulate(local_n, local_number_in_circle)

    call mpi_reduce(local_number_in_circle, number_in_circle, 1, MPI_LONG, MPI_SUM, 0, MPI_COMM_WORLD, ierror)
    
    call mpi_time(1)
    
    if(my_rank.eq.0)then
        simu_pi = 4*real(number_in_circle)/real(n)
        write(*,*)simu_pi
    end if
    call mpi_finalize(ierror)  
    
#else
    write(*,*)"No mpi!"
    write(*,*)"Input the times of simulation:"
    read *,n

    my_rank = 0

    call simulate(n, number_in_circle)

    if(my_rank.eq.0)then
        simu_pi = 4*real(number_in_circle)/real(n)
        write(*,*)simu_pi
    end if
#endif
    end program
