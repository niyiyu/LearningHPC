    subroutine VectorXVector(rst, v1, v2, DIM)
        integer(kind=4) :: DIM, i
        real(kind=4), dimension(DIM) :: v1, v2
        real(kind=4) :: rst

        rst = 0
        do i =1, DIM
            rst = rst + v1(i) * v2(i)
        end do
    end subroutine