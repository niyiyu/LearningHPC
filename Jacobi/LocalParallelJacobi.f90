    subroutine LocalParallelJacobi(y, local_y, local_A, b, my_rank, local_DIM, DIM)
        integer(kind=4) :: DIM, local_DIM, my_rank, ierror
        real(kind=4), dimension(local_DIM) ::  D, local_y
        real(kind=4), dimension(DIM) :: y
        real(kind=4), dimension(DIM) :: b
        real(kind=4), dimension(local_DIM*DIM) :: local_A, local_A_tmp
        integer(kind=4) :: i, j
        
        local_A_tmp = local_A
        do i = 0, local_DIM-1
            
            D(i+1) = local_A(local_DIM*my_rank+1+(DIM+1)*i)
            local_A_tmp(local_DIM*my_rank+1+(DIM+1)*i) = 0
            call VectorXVector(y(local_DIM*my_rank+i+1), local_A_tmp((i*DIM+1):((i+1)*DIM)), y, DIM)
            local_y(i+1) = (1.-y(local_DIM*my_rank+i+1))/D(i+1)
        end do
        
    end subroutine