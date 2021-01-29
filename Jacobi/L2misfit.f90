    subroutine L2misfit(local_A, y, b, local_DIM, DIM, local_rst2)
        integer(kind=4) :: DIM, local_DIM
        real(kind=4), dimension(local_DIM*DIM) :: local_A
        real(kind=4), dimension(local_DIM) :: b_tmp
        real(kind=4), dimension(DIM) :: y
        real(kind=4) :: local_rst2
        integer(kind=4) :: i
        local_rst2 = 0
        do i = 1, local_DIM
            call VectorXVector(b_tmp(i),local_A(((i-1)*DIM+1):(i*DIM)), y, DIM)
            local_rst2 = local_rst2 + (b_tmp(i)-1)**2
        end do
    end subroutine