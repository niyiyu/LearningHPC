program Jabobiiter
    use mpi_times
    include 'mpif.h'
    ! use mpi

    integer(kind=4) :: ierror,my_rank,comm_sz
    integer(kind=4) :: i, j
    integer(kind=4) :: iter, MAXITER, DIM, local_DIM
    real(kind=4) :: epsilon, rst2, rst2_tmp, local_rst2
    ! real(kind=8), dimension(:,:), allocatable :: local_A
    real(kind=4), dimension(:), allocatable :: A, b, y, local_y, local_A

    MAXITER = 100
    DIM = 1000
    epsilon = 1e-8

    call mpi_init(ierror)
    call mpi_comm_size(MPI_COMM_WORLD,comm_sz,ierror)
    call mpi_comm_rank(MPI_COMM_WORLD,my_rank,ierror)
    
    if(my_rank.eq.0)then
        write(*,*) "===Enable MPI==="
        if(mod(DIM, comm_sz).ne.0)then
            write(*, 100) comm_sz
            call mpi_finalize(ierror)
            stop 
        end if
        100 format("Task cannot be distributed to ", I0, " cores.") 
    end if  
    call MPI_Barrier(MPI_COMM_WORLD, ierror)
    local_DIM = DIM/comm_sz

    allocate(b(1:DIM), stat = ierror)
    do i = 1, DIM
        b(i) = 1
    end do

    if(my_rank.eq.0)then
        open(unit=22, file = '../Code/As.dat', status = 'old')
        allocate(A(1:(DIM*DIM)), stat = ierror)
            do i = 1, DIM
                read(22, 101)A(((i-1)*DIM+1):(i*DIM))
            end do
        101 format(1000(2X, ES14.7),\)     !Change as DIM change!
        write(*,*) "Finish loading A."
    end if
    
    allocate(local_A(1:local_DIM*DIM), stat = ierror)
    allocate(y(1:DIM), stat = ierror)
    do i = 1, DIM
        y(i) = 0
    end do
    allocate(local_y(1:local_DIM), stat = ierror)
    call MPI_Barrier(MPI_COMM_WORLD, ierror)
    call MPI_Scatter(A, local_DIM*DIM, MPI_REAL, local_A, local_DIM*DIM, MPI_REAL, 0, MPI_COMM_WORLD, ierror)
    call MPI_Barrier(MPI_COMM_WORLD, ierror)
    if(my_rank.eq.0)then
        deallocate(A, stat = ierror)
    end if

    iloop: do i = 1, MAXITER
        call mpi_time(0)
        if(my_rank.eq.0)then
            write(*,102) i 
            102 format(" ========Begining iteration  ", I0, "========") 
        end if
        call MPI_Barrier(MPI_COMM_WORLD, ierror)
        call LocalParallelJacobi(y, local_y, local_A, b, my_rank, local_DIM, DIM)

        call MPI_Barrier(MPI_COMM_WORLD, ierror)
        call MPI_AllGather(local_y, local_DIM, MPI_REAL, y, local_DIM, MPI_REAL, MPI_COMM_WORLD, ierror)

        
        call L2misfit(local_A, y, b, local_DIM, DIM, local_rst2)
        call MPI_AllReduce(local_rst2, rst2, 1, MPI_REAL, MPI_SUM, MPI_COMM_WORLD, ierror)
        if(my_rank.eq.0)then
            write(*,*) rst2
        end if
        if(i.eq.1)then
            rst2_tmp = rst2
        else
            if((rst2.le.epsilon).or.((abs(rst2_tmp-rst2)/rst2).le.0.05)) then
                exit iloop
            end if
            rst2_tmp = rst2
        end if
       

    end do iloop
    call mpi_time(1)
    if(my_rank.eq.0)then
        write(*,*) "Finish all iterations....."
    end if

    call mpi_finalize(ierror)

end program