    subroutine orth(U,dim)
    use f95_precision
    use blas95
    integer i,j,dim
    real(kind=4), dimension(dim,dim) :: U
    real res

    do i=1,dim
        res = nrm2(U(1:dim,i))
        call sscal(dim,1.0/res,U(1:dim,i),1) 
    end do
    end subroutine