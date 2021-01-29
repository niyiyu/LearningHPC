    subroutine random_metrix(U,dim)
    
    integer i,j,dim
    real(kind=4), dimension(dim,dim) :: U

    call random_seed()

    do i=1,dim
        do j=1,dim
            call random_number(U(i,j))
        end do
    end do
    end subroutine