    program hello_world
    ! use f95_precision
    ! use blas95
    use omp_lib
    ! use mpi_times
    integer res, ierror
    integer(kind=8) ii
#ifdef WITHMPI
    write(*,*) WITHMPI
#endif
    ! call mpi_init(ierror)
    ! call mpi_time(0)
    !$OMP PARALLEL do reduction(+: res) 
    do ii = 1, 1000000000
    call cal_sum(ii)
    end do

    !$OMP END PARALLEL do

    ! call mpi_time(1)
    ! call mpi_finalize(ierror)

    contains
    subroutine cal_sum(i)
    integer(kind=8) i, ii, sum
    sum = 0
    do ii=1,i
        sum = sum + ii
    end do 
    end subroutine cal_sum
    end program