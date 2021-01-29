    subroutine simulate(n, number_in_circle)

    integer(kind=4) n, number_in_circle,i
    real(kind=4) x, y
    call random_seed()

    number_in_circle = 0
    do i=1,n
        call random_number(x)
        call random_number(y)
        x = 2 * x - 1
        y = 2 * y - 1
        if((x**2 + y**2).le.1)then
            number_in_circle = number_in_circle + 1
        end if
    end do


    end subroutine simulate