    subroutine diag(D,dim)
    
    integer i,dim
    real(kind=4), dimension(dim,dim) :: D

    call random_seed()

    do i=1,dim
        call random_number(D(i,i))
    end do
    end subroutine