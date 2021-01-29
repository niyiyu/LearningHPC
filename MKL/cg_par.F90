module cg_par
    implicit none 

    integer(kind=4) dim,i,iter
    integer, parameter :: MAX_ITER = 100
    real(kind=4), parameter :: tol = 1e-3
    real(kind=4), dimension(:,:), allocatable :: D,U,A
    real(kind=4), dimension(:), allocatable :: x,x_real,b,residual,res_tmp
    real(kind=4) res,alpha,dot_tmp,dot_res_tmp

    integer(kind=4) ierror,my_rank,comm_sz
end module cg_par