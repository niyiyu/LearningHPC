program hdf5test
    ! use ISO_C_BINDING
    use hdf5

    character(LEN=100) :: filename 
    integer(HID_T) :: fid, sid, did
    integer(HSIZE_T), dimension(1) :: dims = (/45/)
    integer, dimension(45) :: indata, outdata
    integer :: hdferr
    integer :: i, j
    

    filename = "test.h5"
    do i = 1, 45
           indata(i) = i * 10
    end do

    call h5open_f(hdferr)
    call H5Fcreate_f(filename, H5F_ACC_TRUNC_F, fid, hdferr)
    call H5Screate_simple_f(1, dims, sid, hdferr)
    call H5Dcreate_f(fid, "dset1", H5T_NATIVE_INTEGER, sid, did, hdferr)

    call H5Dwrite_f(did, H5T_NATIVE_INTEGER, indata, dims, hdferr)
    call H5Dread_f(did, H5T_NATIVE_INTEGER, outdata, dims, hdferr)

    do i = 1, 5
        ! do j = 1, 9
            write(*, *) outdata(((i-1)*9+j) : (i*9+j))
        ! end do
        write(*, *) "---------------------"
    end do


    call H5Dclose_f(did, hdferr)
    call H5Sclose_f(sid, hdferr)
    call H5Fclose_f(fid, hdferr)
    call h5close_f(hdferr)
    
end program