    subroutine random_vector(x,dim)
    
    integer i,dim
    real(kind=4), dimension(dim) :: x

    call random_seed()

    do i=1,dim
        call random_number(x(i))
    end do
    end subroutine