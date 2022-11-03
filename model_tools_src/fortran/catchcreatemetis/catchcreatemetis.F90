program  catchcreatemetis
! ******************************************************************************
   ! -- modules
  use imod_idf_par, only: idfobj
  use imod_idf
  use metis_module, only: tMetis
  use utilsmod, only: i1b, i4b, i8b, r4b, r8b, open_file, errmsg, getdigits, &
    writeasc, writeidf, chkexist, DZERO, mxslen, readidf_block, &
    addboundary, calc_unique, tUnp, quicksort_d, quicksort_r, errmsg, logmsg, ta, tBb

  implicit none

  !--  general parameters
  logical, parameter :: ldebug = .false.
  integer, parameter :: maxid = 100000

  ! -- some METIS parameters
  logical, parameter :: pgrec    = .true.   ! multilevel recursive bisection

  ! -- locals
  type(idfobj) :: idf
  type(tMetis), pointer :: met => null()
  type(tUnp), dimension(:), allocatable :: regbb
  type(tBb) :: bb
  logical :: lrb
  character(len=mxslen) :: str, ptrfile, vwgtfile, partfile, f
  integer(i4b) :: iaddbnd,  gnparts, nparts, n, m, ip, gir0, gir1, gic0, gic1
  integer(i4b):: ntot1, nbnd1, ntot, nbnd
  integer(i4b) :: gnc, gnr, nc, nr, mc, mr, ic, ir, id, nid, newid
  integer(i4b) :: ireg, jreg, jc, jr, kc, kr, iu, ip0, ip1, i0, i1
  integer(i4b) :: idum, nreg, nreg_np, i, j, ir0, ir1, ic0, ic1
  integer(i4b), dimension(:), allocatable :: reg_np, i4wk1d
  integer(i1b), dimension(:,:), pointer :: bnd
  integer(i4b), dimension(:,:), pointer :: i4wk2d, i4wk2d2, wgt
  integer(i4b), dimension(:,:), allocatable :: regun
  integer(i4b), dimension(:), allocatable :: regind
  real(r4b), dimension(:,:), pointer :: r4wk2d
  real(r8b), dimension(:), allocatable :: regarea
  real(r4b) :: r4val
  real(r8b) :: xll, yll, cs, tgtload, load, regarea_rest
  real(r8b), dimension(:), allocatable :: id2part
! ------------------------------------------------------------------------------
  !
  lrb = .false.
  !
  !gir0 = 10000; gir1 = 16300; gic0 = 33000; gic1 = 42000; lrb = .true. !AUS
  !gir0 = 4300; gir1 = 4600; gic0 = 5900; gic1 = 6200; lrb = .true. !AUS
  !gir0 = 810; gir1 = 1700; gic0 = 10500; gic1 = 14300; lrb = .true.
  
  ! read command line arguments
  n = nargs()-1
  if (n < 4) then
    call errmsg('invalid number of arguments')
  end if

  ! check if the partitioning is for a dummy block
  call getarg(1,str); read(str,*) iaddbnd
  call getarg(2,ptrfile)
  call getarg(3,vwgtfile)
  call getarg(4,partfile)
  call getarg(5,str); read(str,*) nreg_np
  
  if (nreg_np > 0) then
    allocate(reg_np(nreg_np))
    do i = 1, nreg_np
      call getarg(5+i,str); read(str,*) reg_np(i)
    end do
  end if
  !
  ! read IDF header
  if (.not.idfread(idf, ptrfile, 0)) then
   call errmsg('Could not read '//trim(ptrfile))
  end if
  if (.not.lrb) then
    gir0 = 1; gir1 = idf%nrow; gic0 = 1; gic1 = idf%ncol
  end if
  call readidf_block(idf, gir0, gir1, gic0, gic1, i4wk2d)
  gnc = size(i4wk2d,1); gnr = size(i4wk2d,2)
  cs = idf%dx; xll = idf%xmin + (gic0 - 1)*cs; yll = idf%ymin + (gir0 - 1)*cs
  !
  ! read IDF header
  call idfdeallocatex(idf)
  if (.not.idfread(idf, vwgtfile, 0)) then
   call errmsg('Could not read '//trim(vwgtfile))
  end if
  call readidf_block(idf, gir0, gir1, gic0, gic1, r4wk2d, 0.)
  !
  allocate(wgt(gnc,gnr))
  !
  ! set weights; init boundary flag
  m = 0
  do ir = 1, gnr
    do ic = 1, gnc
      if (i4wk2d(ic,ir) /= 0) i4wk2d(ic,ir) = 1
      wgt(ic,ir) = 1
      r4val = r4wk2d(ic,ir)
      if (r4val /= 0.) then
        if (r4val > 0.1001) then
          m = m + 1
          wgt(ic,ir) = 2
        end if
      end if
    end do
  end do
  deallocate(r4wk2d)
  call logmsg('# layer 2 cells : '//ta((/m/)))
  !
  ! add the boundary
  if (iaddbnd == 1) then
    call logmsg('Adding boundary')
    call addboundary(i4wk2d, gnc, gnr)
  end if
  !
  ! store boundary position
  allocate(bnd(gnc,gnr))
  ntot1 = 0; nbnd1 = 0; ntot = 0; nbnd = 0
  do ir = 1, gnr
    do ic = 1, gnc
      if (i4wk2d(ic,ir) /= 0) then
        ntot1 = ntot1 + 1
        ntot = ntot + wgt(ic,ir)
      end if
      if (i4wk2d(ic,ir) < 0) then
        bnd(ic,ir) = 1
        nbnd1 = nbnd1 + 1
        nbnd  = nbnd  + wgt(ic,ir)
      else
        bnd(ic,ir) = 0
      end if
    end do
  end do
  !
  ! some statistics
  m = ntot1-nbnd1 ! land layer 1
  call logmsg('***** BEGIN cell statistics *****')
  call logmsg('gnc, gnr: '//ta((/gnc,gnr/)))
  call logmsg('')
  call logmsg('For 1 layer:')
  call logmsg('# non-bnd cells: '//ta((/m/)))
  call logmsg('# ALL bnd cells: '//ta((/gnc*gnr-m/)))
  call logmsg('# bnd     cells: '//ta((/nbnd1/)))
  call logmsg('# bnd + non-bnd cells: '//ta((/ntot1/)))
  
  call logmsg('For 2 layers:')
  call logmsg('# non-bnd cells: '//ta((/ntot-nbnd/)))
  call logmsg('# bnd     cells: '//ta((/nbnd/)))
  call logmsg('# bnd + non-bnd cells: '//ta((/ntot/)))
  
  !stop
  
  !
  call calc_unique(i4wk2d, 5, regun, regbb, nreg, idum, 0., 0., 0.)
  !
  allocate(regarea(nreg), regind(nreg))
  do i = 1, nreg
    regind(i) = i
    regarea(i) = -real(regbb(i)%n,r8b)
  end do
  call quicksort_d(regarea, regind, nreg)
  do i = 1, nreg
    regarea(i) = abs(regarea(i))
  end do
  regarea_rest = DZERO
  do i = nreg_np, nreg
    regarea_rest = regarea_rest + regarea(i) 
  end do
  !
  gnparts = 0
  do ireg = 1, nreg_np-1
    jreg = regind(ireg)
    ir0 = regbb(jreg)%ir0; ir1 = regbb(jreg)%ir1
    ic0 = regbb(jreg)%ic0; ic1 = regbb(jreg)%ic1
    nr = ir1 - ir0 + 1; nc = ic1 - ic0 + 1
    !
    nparts = reg_np(ireg)
    if (nparts < 0) then
      do ir = ir0, ir1
        do ic = ic0, ic1
          jr = ir - ir0 + 1; jc = ic - ic0 + 1
          if (regun(ic,ir) == jreg) then
            regun(ic,ir) = 0
          end if
        end do
      end do
      cycle
    end if
    !
    load = regbb(jreg)%n/nparts
    !
    if (nparts == 1) then
      do ir = ir0, ir1
        do ic = ic0, ic1
          jr = ir - ir0 + 1; jc = ic - ic0 + 1
          if (regun(ic,ir) == jreg) then
            regun(ic,ir) = -gnparts
          end if
        end do
      end do
    elseif (nparts > 1) then ! METIS
      call logmsg('Region '//ta((/ireg/))//' (size '//ta((/regbb(jreg)%n/))//'): realized load '// &
        ta((/int(load)/)))
      if (associated(i4wk2d)) deallocate(i4wk2d); i4wk2d => null()
      allocate(i4wk2d(nc,nr))
      do ir = ir0, ir1
        do ic = ic0, ic1
          jr = ir - ir0 + 1; jc = ic - ic0 + 1
          if (abs(regun(ic,ir)) == jreg) then ! also include the boundary!
            i4wk2d(jc,jr) = wgt(ic,ir) ! uniform weight
          else
            i4wk2d(jc,jr) = 0
          end if
        end do
      end do
      !
      allocate(met)
      call met%init(i4wk2d, nparts)
      call met%set_opts()
      if (met%nparts > 1) then
        if (pgrec) then
          call met%recur()
        else
          call met%kway()
       end if
      end if
      !
      ! set the partitions
      n = 0
      do ir = ir0, ir1
        do ic = ic0, ic1
          if (abs(regun(ic,ir)) == jreg) then
            n = n + 1
            ip = met%part(n) + 1 + gnparts
            regun(ic,ir) = -ip
          end if
        end do
      end do
      !
      ! clean-up
      call met%clean()
      deallocate(met)
      !
    else
      call errmsg("")
    end if
    gnparts = gnparts + nparts
    !
  end do
  !
  ! the remainder
  nparts = reg_np(nreg_np)
  if (nparts > 0) then ! METIS
    !
    do ireg = nreg_np, nreg
      jreg = regind(ireg)
      ir0 = regbb(jreg)%ir0; ir1 = regbb(jreg)%ir1
      ic0 = regbb(jreg)%ic0; ic1 = regbb(jreg)%ic1
      bb%ic0 = min(bb%ic0,ic0); bb%ic1 = max(bb%ic1,ic1)
      bb%ir0 = min(bb%ir0,ir0); bb%ir1 = max(bb%ir1,ir1)
    end do
    nr = bb%ir1 - bb%ir0 + 1; nc = bb%ic1 - bb%ic0 + 1
    if (associated(i4wk2d)) deallocate(i4wk2d); i4wk2d => null()
    allocate(i4wk2d(nc,nr))
    do ir = 1, nr
      do ic = 1, nc
        i4wk2d(ic,ir) = 0
      end do
    end do
    !
    do ireg = nreg_np, nreg
      jreg = regind(ireg)
      ir0 = regbb(jreg)%ir0; ir1 = regbb(jreg)%ir1
      ic0 = regbb(jreg)%ic0; ic1 = regbb(jreg)%ic1
      do ir = ir0, ir1
        do ic = ic0, ic1
          jr = ir - bb%ir0 + 1; jc = ic - bb%ic0 + 1
          if (abs(regun(ic,ir)) == jreg) then ! also include the boundary!
            i4wk2d(jc,jr) = wgt(ic,ir) ! uniform weight
          end if
        end do
      end do
    end do
    allocate(met)
    call met%init(i4wk2d, nparts)
    call met%set_opts()
    if (met%nparts > 1) then
      if (pgrec) then
        call met%recur()
      else
        call met%kway()
     end if
    end if
    !
    ! set work array to partitions
    n = 0
    do jr = 1, nr
      do jc = 1, nc
        if (i4wk2d(jc,jr) > 0) then
          n = n + 1
          ip = met%part(n) + 1
          i4wk2d(jc,jr) = ip
        end if
      end do
    end do
    !
    ! clean-up
    call met%clean()
    deallocate(met)
    allocate(i4wk1d(nparts), id2part(maxid))
    id2part = 0
    newid = gnparts
    gnparts = gnparts + nparts
    
    do ireg = nreg_np, nreg
      jreg = regind(ireg)
      ir0 = regbb(jreg)%ir0; ir1 = regbb(jreg)%ir1
      ic0 = regbb(jreg)%ic0; ic1 = regbb(jreg)%ic1
      mr = ir1 - ir0 + 1; mc = ic1 - ic0 + 1
      if (associated(i4wk2d2)) deallocate(i4wk2d2); i4wk2d2 => null()
      allocate(i4wk2d2(mc,mr))
      !
      i4wk1d = 0
      do ir = ir0, ir1
        do ic = ic0, ic1
          kr = ir - ir0 + 1; kc = ic - ic0 + 1
          i4wk2d2(kc,kr) = 0
          if (abs(regun(ic,ir)) == jreg) then ! also include the boundary!
            jr = ir - bb%ir0 + 1; jc = ic - bb%ic0 + 1
            ip = i4wk2d(jc,jr) ! region parition number
            if (ip <= 0) then
              call errmsg('Program error.')
            end if
            i4wk2d2(kc,kr) = ip
            i4wk1d(ip) = 1
          end if
        end do
      end do
      !
      do i = 1, nparts
        if (i4wk1d(i) == 1) then
          newid = newid + 1
          i4wk1d(i) = newid
        end if
      end do
      !
      do ir = ir0, ir1
        do ic = ic0, ic1
          if (abs(regun(ic,ir)) == jreg) then ! also include the boundary!
            kr = ir - ir0 + 1; kc = ic - ic0 + 1
            ip = i4wk2d2(kc,kr)
            id = i4wk1d(ip)
            regun(ic,ir) = -id
            if (id > maxid) then
              call errmsg('Increase maxid')
            else
              id2part(id) = real(ip,r8b)
            end if
            !
          end if
        end do
      end do
    end do
    !
    if (allocated(i4wk1d)) deallocate(i4wk1d)
    allocate(i4wk1d(newid))
    do i = 1, newid
      i4wk1d(i) = i
    end do
    !
    call quicksort_d(id2part, i4wk1d, newid)
    !
    f = 's'//ta((/nreg_np/))//'_np'//ta((/nparts/))//'_ids.bin'
    call open_file(f, iu, 'w',.true.)
    !write(iu,'(a)') nparts
    write(iu) nparts
    !
    ! skip leading zero
    ip0 = 1; i0 = -1
    do i = 1, newid
      ip = int(id2part(i))
      if (ip == ip0) then
        i0 = i
        exit
      end if
    end do
    !
    do while(.true.)
      i = i0
      do while(.true.)
        i = i + 1
        ip = int(id2part(i))
        if (ip /= ip0) then
          i1 = i-1
          exit
        end if
      end do
      !write(iu,'(a)') ta((/i1-i0+1/))
      write(iu) i1-i0+1
      do j = i0, i1
        !write(iu,'(a)') ta((/i4wk1d(j)/))
        write(iu) i4wk1d(j)
      end do
      ip0 = ip0 + 1
      if (ip0 > nparts) exit
      i0 = i1 + 1
    end do
    close(iu)
  else
    do ireg = nreg_np, nreg
      jreg = regind(ireg)
      ir0 = regbb(jreg)%ir0; ir1 = regbb(jreg)%ir1
      ic0 = regbb(jreg)%ic0; ic1 = regbb(jreg)%ic1
      nr = ir1 - ir0 + 1; nc = ic1 - ic0 + 1
      do ir = ir0, ir1
        do ic = ic0, ic1
          jr = ir - ir0 + 1; jc = ic - ic0 + 1
          if (regun(ic,ir) == jreg) then
            regun(ic,ir) = 0
          end if
        end do
      end do
    end do
  end if
  !
  write(*,*) 'Total # partitions: '//ta((/gnparts/))
  !
  ! filter for the bnd
  do ir = 1, gnr
    do ic = 1, gnc
      regun(ic,ir) = abs(regun(ic,ir))
      if (bnd(ic,ir) == 1) then
        regun(ic,ir) = 0
      end if
    enddo
  end do
  !
  if (index(partfile,'.asc',back=.true.) > 0) then
    call writeasc(trim(partfile), regun, gnc, gnr, xll, yll, cs, DZERO)
  else
    call writeidf(trim(partfile), regun, gnc, gnr, xll, yll, cs, DZERO)
  end if
  !

end program