program filter_hydbas
  ! modules
  use utilsmod, only: mxslen, i1b, i4b, r4b, r8b, logmsg, ta, errmsg, &
    fillgap, fill_with_nearest, writeidf, tBb, tUnp, calc_unique, &
    open_file
  use ehdrModule, only: tEhdr, writeflt

  implicit none
  
  type(tEhdr), pointer :: ehdr => null()
  type(tBb), pointer :: bb => null()
  type(tBb), dimension(:), pointer :: bba => null()
  type(tUnp), dimension(:), allocatable :: regbb
  !
  character(len=mxslen) :: fcat, fhb, fo, f
  logical :: ldebug, lok
  integer(i1b), dimension(:,:), allocatable :: i1wk2d
  integer(i4b) :: gic0, gic1, gir0, gir1
  integer(i4b) :: iu, ic, ir, jc, jr, nc, nr, n, m, mm, id, id2, maxid, i, idum, ireg, nreg
  integer(i4b), dimension(:), allocatable :: i4wk1d, i4wk1d2
  integer(i4b), dimension(:,:), allocatable :: i4wk2d, pid, regun
  !
  real(r4b), dimension(:,:), allocatable :: p, x
  real(r4b) :: mvp, mvx, r4v
  real(r8b) :: xll, yll, cs
! ------------------------------------------------------------------------------
  ldebug = .false.
  !gir0 = 4300; gir1 = 4700; gic0 = 22000; gic1 = 22500; ldebug = .true.
  !gir0 = 8300; gir1 = 9300; gic0 = 25900; gic1 = 26900; ldebug = .true.
  !
  call getarg(1,fcat)
  call getarg(2,fhb)
  call getarg(3,fo)
  !
  allocate(ehdr)
  if (.not.ldebug) then
    call ehdr%read_grid(fcat, p, mvp, xll, yll, cs)
    call ehdr%read_grid(fhb,  x, mvx, xll, yll, cs)
  else
    call ehdr%read_block(fcat, gir0, gir1, gic0, gic1, p, mvp, xll, yll, cs)
    call ehdr%read_block(fhb,  gir0, gir1, gic0, gic1, x, mvx, xll, yll, cs)
  end if
  !
  nc = size(p,1); nr = size(p,2)
  if ((size(x,1) /= nc).or.(size(x,2) /= nr)) then
    call errmsg('Invalid number of rows/cols.')
  end if
  !
  allocate(i1wk2d(nc,nr))
  n = 0
  do ir = 1, nr
    do ic = 1, nc
      i1wk2d(ic,ir) = 0
      if (p(ic,ir) /= mvp) then ! there should be a value
        if (x(ic,ir) == mvx) then
          n = n + 1
          i1wk2d(ic,ir) = 1
          x(ic,ir) = -1.0
        end if
      end if
    end do
  end do
  call logmsg('# gaps: '//ta((/n/)))
  !
  call fillgap(x, mvx, -1.0)
  call fill_with_nearest(x, mvx, -1.0)
  !
  ! check if all gaps are filled
  lok = .true.
  do ir = 1, nr
    do ic = 1, nc
      if (p(ic,ir) /= mvp) then
        if (x(ic,ir) == -1.0) then
          lok = .false.
        end if
      end if
    end do
  end do
  if (.not.lok) then
    do ir = 1, nr
      do ic = 1, nc
        if (x(ic,ir) /= mvx) then
          if (x(ic,ir) /= -1.0) then
            x(ic,ir) = 0.0
          end if
        end if
      end do
    end do
    call writeflt('error', x, nc, nr, xll, yll, cs, 0.0)
    call errmsg('Not all gaps filled')
  end if
  !
  ! determine maximum ID
  maxid = 0
  do ir = 1, nr
    do ic = 1, nc
      r4v = x(ic,ir)
      if (r4v /= mvx) then
        id = int(r4v,i4b); maxid = max(maxid, id)
      end if
    end do
  end do
  call logmsg('Maximum ID: '//ta((/maxid/)))
  !
  ! label the ids
  allocate(i4wk1d(maxid))
  do i = 1, maxid
    i4wk1d(i) = 0
  end do
  !
  do ir = 1, nr
    do ic = 1, nc
      r4v = x(ic,ir)
      if (r4v /= mvx) then
        id = int(r4v,i4b)
        i4wk1d(id) = 1
      end if
    end do
  end do
  !
  ! renumber the ids
  n = 0
  do i = 1, maxid
    if (i4wk1d(i) == 1) then
      n = n + 1
      i4wk1d(i) = n
    end if
  end do
  call logmsg('# IDs: '//ta((/n/)))
  !
  ! convert
  allocate(i4wk2d(nc,nr))
  do ir = 1, nr
    do ic = 1, nc
      if (p(ic,ir) /= mvp) then
        r4v = x(ic,ir)
        id = int(r4v,i4b)
        i4wk2d(ic,ir) = i4wk1d(id)
      else
        i4wk2d(ic,ir) = 0
      end if
    end do
  end do
  !
  ! determine the bounding boxes
  allocate(bba(n))
  do ir = 1, nr
    do ic = 1, nc
      id = i4wk2d(ic,ir)
      if (id /= 0) then
        bb => bba(id)
        bb%ic0 = min(bb%ic0,ic); bb%ic1 = max(bb%ic1,ic)
        bb%ir0 = min(bb%ir0,ir); bb%ir1 = max(bb%ir1,ir)
      end if
    end do
  end do
  do id = 1, n
    bb => bba(id)
    bb%ncol = bb%ic1 - bb%ic0 + 1
    bb%nrow = bb%ir1 - bb%ir0 + 1
  end do
  !
  ! determine the unique regions
  id2 = 0
  do id = 1, n
    bb => bba(id)
    !write(*,*) mod(i,n/10)
    if ((id == 1).or.(id == n).or.mod(id,n/10)==1) then
      call logmsg('Processing '//ta((/id/),'(i10.10)')//'/'//ta((/n/),'(i10.10)')//'...')
    end if
    if (allocated(pid)) deallocate(pid)
    allocate(pid(bb%ncol,bb%nrow))
    do ir = bb%ir0, bb%ir1
      do ic = bb%ic0, bb%ic1
        jr = ir - bb%ir0 + 1; jc = ic - bb%ic0 + 1
        if (i4wk2d(ic,ir) == id) then
          pid(jc,jr) = 1
        else
          pid(jc,jr) = 0
        end if
      end do
    end do
    !call logmsg('--> computing unique values, size ('//ta((/bb%ncol,bb%nrow/))//')')
    call calc_unique(pid, 9, regun, regbb, nreg, idum, 0., 0., 0.)
    do ireg = 1, nreg
      id2 = id2 + 1
      do ir = bb%ir0, bb%ir1
        do ic = bb%ic0, bb%ic1
          jr = ir - bb%ir0 + 1; jc = ic - bb%ic0 + 1
          if (regun(jc,jr) == ireg) then
            i4wk2d(ic,ir) = -id2
          end if
        end do
      end do
    end do
  end do
  call logmsg('# IDs final: '//ta((/id2/)))
  !
  do ir = 1, nr
    do ic = 1, nc
      i4wk2d(ic,ir) = abs(i4wk2d(ic,ir))
    end do
  end do
  !
  call writeflt(fo, i4wk2d, nc, nr, xll, yll, cs, 0)
  call writeidf(trim(fo)//'.idf', i4wk2d, nc, nr, xll, yll, cs, 0.d0)
  !
  ! write the bounding boxes
  n = id2
  deallocate(bba)
  allocate(bba(n))
  do ir = 1, nr
    do ic = 1, nc
      id = i4wk2d(ic,ir)
      if (id /= 0) then
        bb => bba(id)
        bb%ic0 = min(bb%ic0,ic); bb%ic1 = max(bb%ic1,ic)
        bb%ir0 = min(bb%ir0,ir); bb%ir1 = max(bb%ir1,ir)
      end if
    end do
  end do
  do id = 1, n
    bb => bba(id)
    bb%ncol = bb%ic1 - bb%ic0 + 1
    bb%nrow = bb%ir1 - bb%ir0 + 1
  end do
  !
  f = trim(fo)//'.csv'
  call open_file(f, iu, 'w')
  write(iu,'(a)') 'OBJECTID,gic0,gic1,gir0,gir1'
  do i = 1, n
    bb => bba(i)
    write(iu,'(a)') ta((/i/))//','//ta((/bb%ic0/))//',' &
                                  //ta((/bb%ic1/))//',' &
                                  //ta((/bb%ir0/))//',' &
                                  //ta((/bb%ir1/))
  end do
  close(iu)
  !
end program
