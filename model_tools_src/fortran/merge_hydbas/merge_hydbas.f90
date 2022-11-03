program merge_hydbas
  ! modules
  !use utilsmod, only: open_file, getdigits, writeasc, readflt, writeflt, writeidf, &
  !  calc_unique, ta, errmsg, logmsg, i1b, r4b, r8b, DZERO, tUnp, fileexist, fillgap
  
  use utilsmod, only: mxslen, i4b, r4b, r8b, open_file, logmsg, ta
  use ehdrModule, only: tEhdr, writeflt

  implicit none
  
  type(tEhdr), pointer :: ehdr => null()
  character(len=mxslen) :: f, s, fo
  integer(i4b) :: iu, i, nm, mv, ic, ir, nc, nr, idmax, idoff, i4v
  integer(i4b), dimension(:,:), allocatable :: i4wk2d, xmerge
  real(r8b) :: xll, yll, cs
! ------------------------------------------------------------------------------
  
  call getarg(1,f)
  call logmsg('Reading '//trim(f)//'...')
  call open_file(f, iu, 'r')
  read(iu,'(a)') fo
  read(iu,'(a)') s; read(s,*) nm
  !
  allocate(ehdr)
  idoff = 0
  do i = 1, nm
    read(iu,'(a)') f
    call ehdr%read_grid(f, i4wk2d, mv, xll, yll, cs)
    !
    if (i == 1) then
      nc = size(i4wk2d,1); nr = size(i4wk2d,2)
      allocate(xmerge(nc,nr))
      do ir = 1, nr
        do ic = 1, nc
          xmerge(ic,ir) = 0
        end do
      end do
    end if
    !
    ! determine the maximum
    idmax = 0
    do ir = 1, nr
      do ic = 1, nc
        i4v = i4wk2d(ic,ir)
        if (i4v /= mv) then
          idmax = max(idmax, i4v)
        end if
      end do
    end do
    call logmsg('Maximum ID: '//ta((/idmax/)))
    !
    do ir = 1, nr
      do ic = 1, nc
        i4v = i4wk2d(ic,ir)
        if (i4v /= mv) then
           xmerge(ic,ir) = i4v + idoff
        end if
      end do
    end do
    idoff = idoff + idmax
  end do
  close(iu)
  !
  call writeflt(fo, xmerge, nc, nr, xll, yll, cs, 0)
end program
