program datamap
! ******************************************************************************
  ! --- local
  use imod_idf_par, only: idfobj
  use imod_idf
  use utilsmod, only: mxslen, i1b, i2b, i4b, r4b, r8b, DZERO, DONE, &
    readidf_block, writeasc, writeidf, tBB, errmsg, quicksort_d, addboundary, open_file, &
    chkexist, readgen, tPol, logmsg, ta
  !
  implicit none
  !
  integer(i4b) :: sea_opt
  
  interface
    recursive subroutine label_node(ia, ja, id, i4wk1d, ireg)
      use utilsmod, only: i4b
      implicit none
      integer(i4b), dimension(:), intent(in) :: ia
      integer(i4b), dimension(:), intent(in) :: ja
      integer(i4b), intent(in) :: id
      integer(i4b), dimension(:), intent(inout) :: i4wk1d
      integer(i4b), intent(in) :: ireg
    end subroutine label_node
  end interface
  !
  ! -- parameters
  integer(i4b), parameter :: js = 1
  integer(i4b), parameter :: jn = 2
  integer(i4b), parameter :: je = 3
  integer(i4b), parameter :: jw = 4
  integer(i4b), parameter :: nst = jw
  integer(i4b), dimension(2,nst), parameter :: st = (/0, 1, 0, -1, 1, 0, -1, 0/)
  integer(i4b), parameter :: mxnnb = 100000
  !
  ! --- types
  type tIntf
    integer(i4b)                          :: nb_gid = 0 ! neighboring local id
    integer(i4b)                          :: nb_lid = 0 ! neighboring local id
    integer(i4b)                          :: n = 0 ! number of nodes
    integer(i4b)                          :: w = 0 ! weight
    integer(i4b), dimension(:,:), pointer :: my_gicir => null()
    integer(i4b), dimension(:),   pointer :: my_nlay  => null()
    integer(i4b), dimension(:,:), pointer :: nb_gicir => null()
    integer(i4b), dimension(:),   pointer :: nb_nlay  => null()
  end type tIntf
  !
  type tCat
    integer(i4b)                       :: gid = 0   ! global ID
    integer(i4b)                       :: lid = 0   ! local ID
    integer(i4b)                       :: ireg = 0  ! region number
    integer(i4b)                       :: nlnd = 0  ! number of land cells
    integer(i4b)                       :: nsea  = 0 ! number of sea cells
    integer(i4b)                       :: w = 0     ! weight
    type(tBB), pointer                 :: gbb => null() ! global bounding box
    type(tBB), pointer                 :: lbb => null() ! local bounding box
    integer(i4b)                       :: nnb  = 0! number of neighbors
    type(tIntf), dimension(:), pointer :: intf => null() ! interfaces
  end type tCat
  type(tCat), dimension(:), pointer :: cat => null()
  !
  type treg
    integer(i4b) :: n = 0
    integer(i4b), dimension(:), pointer :: ind => null()
    real(r8b), dimension(:), pointer :: xid => null()
  end type treg
  !
  ! --- functions
  integer(i4b) :: cfn_unique_i
  !
  ! --- local
  type(idfobj) :: idf, idf2
  type(tBB), pointer :: bb => null()
  type(tIntf), pointer :: intf => null()
  type(treg), dimension(:), pointer :: regwk => null()
  type(tPol), dimension(:), allocatable :: pol
  !
  character(len=mxslen) :: f, out_pref, s, tile_pref
  logical :: ldone, lfound, lskip, linitpol, lin
  integer(i4b), dimension(:,:), allocatable :: landmask
  integer(i1b), dimension(:,:), allocatable :: nlay
  integer(i2b), dimension(:,:), allocatable :: itile
  integer(i4b) :: p, ofs, iread, ios
  integer(i4b) :: gir0, gir1, gic0, gic1, idum
  integer(i4b) :: ir, ic, jr, jc, kr, kc, nbr, m, n, na, iact, nja, mja, nreg, nlnd, nsea, ncell
  integer(i4b) :: i, j, k, ist, nc, nr, nb, ir0, ir1, ic0, ic1, mxgid, mxlid
  integer(i4b) :: jc1, jr1, jc2, jr2, ntile, gnrow, gncol
  integer(i4b) :: id, id1, id2, iu, nct, nrt
  integer(i1b), dimension(:), allocatable :: i1wk1d
  integer(i2b), dimension(:), allocatable :: i2wk1d
  integer(i4b), dimension(:), allocatable :: i4wk1d1, i4wk1d2, i4wk1d3
  integer(i4b), dimension(:,:), allocatable :: i4wk2d
  integer(i4b), dimension(:), allocatable :: g2l, l2g, cnt, ia, gja, lja, reg, regncat
  integer(i4b), dimension(:,:), pointer :: xid
  integer(i4b), dimension(:,:), pointer :: xid2
  real(r4b) :: r4val
  real(r8b) :: gxmin, gymin, gcs, x, y
  real(r4b), dimension(:,:), pointer :: r4wk2d
  real(r8b), dimension(:), allocatable :: regncell, r8wk1d
! ------------------------------------------------------------------------------
  !
  sea_opt = 1
  !
  !gir0 = 1; gir1 = 9000; gic0 = 1; gic1 = 16000
  !gir0 = 1; gir1 = 9000; gic0 = 6500; gic1 = 16000 !USA
  !gir0 = 10000; gir1 = 16300; gic0 = 33000; gic1 = 42000 !AUS
  !gir0 = 2820; gir1 = 3210; gic0 = 18650; gic1 = 19985 !ICE
  !gir0 = 12230; gir1 = 13880; gic0 = 26780; gic1 = 27660 !MADA
  
  ! count the arguments
  
  na = nargs() - 1
  call getarg(1,s); read(s,*) sea_opt
  !
  if (sea_opt == 1) then
    call logmsg("***** Using exclusive sea option *****")
  else
    call logmsg("***** Using inclusive sea option *****")
  end if
  !
  !1             2                     3                       4                                                5                                         6
  !.\map_ireland ..\hydrosheds\cat.idf ..\hydrosheds\d_top.idf ..\hydrosheds\make_clonemap\128\tile\rcb_128.txt ..\hydrosheds\make_clonemap\128\tile\rcb_ ireland.gen
  !1             2                     3                       4                                                5                                         6     7     8     9
  !.\map_ireland ..\hydrosheds\cat.idf ..\hydrosheds\d_top.idf ..\hydrosheds\make_clonemap\128\tile\rcb_128.txt ..\hydrosheds\make_clonemap\128\tile\rcb_ 12230 13880 26780 27660
  linitpol = .false.
  iread = 1
  if (na > 6) then
    iread = 0
    call getarg(7,f)
    read(f,*,iostat=ios) idum
    if (ios /= 0) then
      call logmsg('Reading '//trim(f)//'...')
      pol = readgen(f)
      linitpol = .true.
    end if
    if (.not.linitpol) then
      call getarg(7,s); read(s,*) gir0
      call getarg(8,s); read(s,*) gir1
      call getarg(9,s); read(s,*) gic0
      call getarg(10,s); read(s,*) gic1
      write(*,'(a,4(i5.5,a))') &
        '*** Processing for global bounding box (gir0,gir1,gic0,gic1) = (', &
      gir0,',',gir1,',',gic0,',',gic1,')...'
    end if
  end if
  !
  call getarg(2,out_pref)
  call getarg(3,f)
  if (iread == 1) write(*,'(a)') 'Reading entire '//trim(f)//'...'
  if (.not.idfread(idf, f, iread)) then
    call errmsg('Could not read '//trim(f))
  end if
  gnrow = idf%nrow; gncol = idf%ncol; gxmin = idf%xmin; gymin = idf%ymin; gcs = idf%dx
  !
  if (linitpol) then
    gir0 = gnrow+1; gir1 = 0; gic0 = gncol+1; gic1 = 0
    do i  = 1, size(pol)
      ic = (pol(i)%xmin-gxmin)/gcs;           gic0 = min(gic0, ic)
      ic = (pol(i)%xmax-gxmin)/gcs;           gic1 = max(gic1, ic)
      ir = (gymin+gnrow*gcs-pol(i)%ymax)/gcs; gir0 = min(gir0, ir)
      ir = gnrow-(pol(i)%ymin-gymin)/gcs;     gir1 = max(gir1, ir)
    end do
    gic0 = max(gic0,1); gic1 = min(gic1, gncol)
    gir0 = max(gir0,1); gir1 = min(gir1, gnrow)
  end if
  !
  if (iread == 0) then
    call readidf_block(idf, gir0, gir1, gic0, gic1, xid)
    nc = size(xid,1); nr = size(xid,2)
  else
    gir0 = 1; gir1 = idf%nrow; gic0 = 1; gic1 = idf%ncol
    nc = idf%ncol; nr = idf%nrow
    allocate(xid(nc,nr))
    do ir = 1, nr
      do ic = 1, nc
        xid(ic,ir) = int(idf%x(ic,ir),i4b)
      end do
    end do
  end if
  !
  if (.false.) then
    call writeidf('tmp.idf', xid, nc, nr, &
      gxmin + (gic0-1)*gcs, gymin + (gnrow-gir1)*idf%dx, &
      gcs, 0.D0); stop
  end if
  
  ! set the land mask
  allocate(landmask(nc,nr))
  do ir = 1, nr
    do ic = 1, nc
      if (xid(ic,ir) > 0) then
        landmask(ic,ir) = 1
      else if (xid(ic,ir) < 0) then
        landmask(ic,ir) = -1
      else
        landmask(ic,ir) = 0
      end if
    end do
  end do
  !
  ! remove catchment outside of polygon(s)
  if (linitpol .and. (na == 7)) then
    do ir = 1, nr
      do ic = 1, nc
        xid(ic,ir) = abs(xid(ic,ir))
      end do
    end do
    
    do ir = 1, nr
      do ic = 1, nc
        if (xid(ic,ir) /= 0) then
          jr = gir0 + ir - 1; jc = gic0 + ic - 1
          x = gxmin + jc*gcs - 0.5*gcs
          y = gymin+gnrow*gcs - jr*gcs + 0.5*gcs
          do i = 1, size(pol)
            call polygon_contains_point_2d ( pol(i)%n, pol(i)%xy, (/x,y/), lin)
            if (.not.lin) then
              xid(ic,ir) = -abs(xid(ic,ir))
            end if
          end do
        end if
      end do
    end do
    n = maxval(abs(xid))
    allocate(i4wk1d1(n))
    do i = 1, n
      i4wk1d1(i) = 0
    end do
    do ir = 1, nr
      do ic = 1, nc
        id = xid(ic,ir)
        if (id < 0) i4wk1d1(abs(id)) = 1
      end do
    end do
    do ir = 1, nr
      do ic = 1, nc
        id = abs(xid(ic,ir))
        if (id > 0) then
          if (i4wk1d1(id) == 1) xid(ic,ir) = 0
        end if
      end do
    end do
    deallocate(i4wk1d1)
    !
    do ir = 1, nr
      do ic = 1, nc
        id = abs(xid(ic,ir))
        if ((id > 0).and.(landmask(ic,ir) == -1)) then
          xid(ic,ir) = -abs(xid(ic,ir))
        end if  
      end do
    end do
  end if
  !
  if (.false.) then
    call writeidf('tmp.idf', xid, nc, nr, &
      gxmin + (gic0-1)*gcs, gymin + (gnrow-gir1)*idf%dx, &
      gcs, 0.D0); stop
  end if
  !
  if (linitpol .and. (na > 7)) then
    call getarg(8,f)
    if (.not.idfread(idf2, f, 0)) then
      call errmsg('Could not read '//trim(f))
    end if
    call getarg(9,f); read(f,*) n
    allocate(i4wk1d1(n))
    do i = 1, n
      call getarg(9+i,f); read(f,*) i4wk1d1(i)
    end do
    call readidf_block(idf2, gir0, gir1, gic0, gic1, xid2)
    do ir = 1, nr
      do ic = 1, nc
        if (xid(ic,ir) /= 0) then
          lfound = .false.
          do i = 1, n
            if (xid2(ic,ir) == i4wk1d1(i)) then
              lfound = .true.
            end if
          end do
          if (.not.lfound) then
            xid(ic,ir) = 0
          end if
        end if  
      end do
    end do
    call idfdeallocatex(idf2)
    deallocate(i4wk1d1)
  end if
  !
  !if (linitpol .and. (na > 7)) then
  !  call getarg(8,f)
  !  if (.not.idfread(idf2, f, 0)) then
  !    call errmsg('Could not read '//trim(f))
  !  end if
  !  call getarg(9,f); read(f,*) n
  !  allocate(i4wk1d1(n))
  !  do i = 1, n
  !    call getarg(9+i,f); read(f,*) i4wk1d1(i)
  !  end do
  !  call readidf_block(idf2, gir0, gir1, gic0, gic1, xid2)
  !  do ir = 1, nr
  !    do ic = 1, nc
  !      if (xid(ic,ir) /= 0) then
  !        lfound = .false.
  !        do i = 1, n
  !          if (xid2(ic,ir) == i4wk1d1(i)) then
  !            lfound = .true.
  !          end if
  !        end do
  !        if (.not.lfound) then
  !          xid(ic,ir) = -xid(ic,ir)
  !        end if
  !      end if  
  !    end do
  !  end do
  !  call idfdeallocatex(idf2)
  !  deallocate(i4wk1d1)
  !  n = maxval(abs(xid))
  !  allocate(i4wk1d1(n))
  !  do i = 1, n
  !    i4wk1d1(i) = 0
  !  end do
  !  do ir = 1, nr
  !    do ic = 1, nc
  !      id = xid(ic,ir)
  !      if (id > 0) i4wk1d1(abs(id)) = 1
  !    end do
  !  end do
  !  do ir = 1, nr
  !    do ic = 1, nc
  !      id = abs(xid(ic,ir))
  !      if (id > 0) then
  !        if (i4wk1d1(id) == 1) then
  !          xid(ic,ir) = abs(xid(ic,ir))
  !        else
  !          xid(ic,ir) = 0
  !        end if
  !      end if
  !    end do
  !  end do
  !  deallocate(i4wk1d1)
  !end if
  !
  if (.false.) then
    call writeidf('tmp.idf', xid, nc, nr, &
      gxmin + (gic0-1)*gcs, gymin + (gnrow-gir1)*idf%dx, &
      gcs, 0.D0); stop
  end if
  !
  call idfdeallocatex(idf)
  !
  allocate(nlay(nc,nr))
  call getarg(4,f)
  if (iread == 1) write(*,'(a)') 'Reading entire '//trim(f)//'...'
  if (.not.idfread(idf, f, iread)) then
    call errmsg('Could not read '//trim(f))
  end if
  if (iread == 0) then
    call readidf_block(idf, gir0, gir1, gic0, gic1, r4wk2d, 0.)
  else
    allocate(r4wk2d(nc,nr))
    do ir = 1, nr
      do ic = 1, nc
        r4wk2d(ic,ir) = real(idf%x(ic,ir))
      end do
    end do
  end if
  do ir = 1, nr
    do ic = 1, nc
      nlay(ic,ir) = 1
      r4val = r4wk2d(ic,ir)
      if (r4val /= 0.) then
        if (r4val > 0.1001) then
          nlay(ic,ir) = 2
        end if
      end if
    end do
  end do
  deallocate(r4wk2d)
  !
  if (.false.) then
    call writeidf('nlay.idf', nlay, nc, nr, &
    gxmin + (gic0-1)*gcs, gymin + (gnrow-gir1)*gcs, gcs, 0.D0)
    stop
  end if
  call idfdeallocatex(idf)
  !
  ! label the sea cells
  if (sea_opt == 1) call addboundary(xid, nc, nr)
  !
  do ir = 1, nr
    do ic = 1, nc
      if (xid(ic,ir) < 0) then !labeled as CHD
        if (landmask(ic,ir) == 1) then !land
          xid(ic,ir) = 0
        else
          landmask(ic,ir) = 2 !sea
        end if
      end if
    end do
  end do
  !
  if (.false.) then
    do ir = 1, nr
      do ic = 1, nc
        if (xid(ic,ir) /= 1) then !labeled as CHD
          xid(ic,ir) = 0
        end if
      end do
    end do
    call writeidf('tmp.idf', xid, nc, nr, &
      gxmin + (gic0-1)*gcs, gymin + (gnrow-gir1)*idf%dx, &
      gcs, 0.D0)
    call writeasc('tmp.asc', xid, nc, nr, &
      gxmin + (gic0-1)*gcs, gymin + (gnrow-gir1)*idf%dx, &
      gcs, 0.D0)
  end if
  !
  mxgid = maxval(xid)
  !
  allocate(g2l(mxgid))
  do i = 1, mxgid
    g2l(i) = 0
  end do
  !
  ! determine global <--> local ID mapping
  n = 0
  do ir = 1, nr
    do ic = 1, nc
      id = abs(xid(ic,ir))
      if (id /= 0) then
        g2l(id) = 1
      else
        n = n + 1
      end if
    end do
  end do
  !
  write(*,'(a,f10.2)') 'Cells other than land/sea (%): ',100.*real(n)/real(nr*nc)
  !
  mxlid = 0
  do i = 1, mxgid
    if (g2l(i) > 0) then
      mxlid = mxlid + 1
      g2l(i) = mxlid
    end if
  end do
  !
  allocate(l2g(mxlid))
  do i = 1, mxgid
    id = g2l(i)
    if (id > 0) then
      l2g(id) = i
    end if
  end do
  !
  write(*,*) 'Total # catchments (M):', real(mxlid)/1000000.
  !
  ! determine the bounding boxes and counts
  allocate(cat(mxlid))
  do i = 1, mxlid
    allocate(cat(i)%gbb, cat(i)%lbb)
    cat(i)%lid = i
    cat(i)%gid = l2g(i)
  end do
  !
  nlnd = 0
  nsea = 0
  do ir = 1, nr
    do ic = 1, nc
      id1 = abs(xid(ic,ir))
      if (id1 /= 0) then
        id2 = g2l(id1); bb => cat(id2)%lbb
        bb%ir0 = min(bb%ir0, ir); bb%ir1 = max(bb%ir1, ir)
        bb%ic0 = min(bb%ic0, ic); bb%ic1 = max(bb%ic1, ic)
        if (xid(ic,ir) > 0) then
          cat(id2)%nlnd = cat(id2)%nlnd + 1; nlnd = nlnd + 1
        else
          cat(id2)%nsea = cat(id2)%nsea + 1; nsea = nsea + 1
        end if
        if (xid(ic,ir) /= 0) then
          if (nlay(ic,ir) == 0) then
            call errmsg('Zero weight.')
          end if
          cat(id2)%w = cat(id2)%w + int(nlay(ic,ir),i4b)
        end if
      end if
    end do
  end do
  !
  ! check for zero weights
  do i = 1, mxlid
    if (cat(i)%w <= DZERO) then
      call errmsg('Program error: zero weight found.')
    end if
  end do
  !
  do i = 1, mxlid
    bb => cat(i)%lbb
    bb%nrow = bb%ir1 - bb%ir0 + 1
    bb%ncol = bb%ic1 - bb%ic0 + 1
    !
    ! set global bounding box
    cat(i)%gbb%ir0 = bb%ir0 + gir0 - 1; cat(i)%gbb%ir1 = cat(i)%gbb%ir0 + bb%nrow - 1
    cat(i)%gbb%ic0 = bb%ic0 + gic0 - 1; cat(i)%gbb%ic1 = cat(i)%gbb%ic0 + bb%ncol - 1
    cat(i)%gbb%nrow = bb%nrow; cat(i)%gbb%ncol = bb%ncol
  end do
  !
  write(*,*) 'Total # land cells (M):', real(nlnd)/1000000.
  write(*,*) 'Total #  sea cells (M):', real(nsea)/1000000.
  !
  ! determine the neighboring cells
  allocate(i4wk1d1(mxnnb), i4wk2d(5,mxnnb))
  allocate(ia(mxlid+1))
  do iact = 1, 2
    nja = 0
    do i = 1, mxlid
      id1 = l2g(i) ! global ID
      bb => cat(i)%lbb
      ir0 = bb%ir0; ir1 = bb%ir1; ic0 = bb%ic0; ic1 = bb%ic1 !BB
      !write(*,'(a)') ta((/i/))//'('//ta((/id1/))//')/'//ta((/mxlid/))//': ('//ta((/ic0,ic1,ir0,ir1/))//')'
      call logmsg('Processing for catchment '//ta((/i/))//'/'//&
        ta((/mxlid/))//', (nc,nr)=('//ta((/bb%ncol/))//','//ta((/bb%nrow/))//')')
      !
      ! label the neighboring cells
      nbr = 0 ! initialize number of neighbors
      do ir = ir0, ir1
        do ic = ic0, ic1
          if (id1 == abs(xid(ic,ir))) then
            do ist = 1, nst ! loop over the 8 neighbors
              jc = ic+st(1,ist); jc = max(1,jc); jc = min(nc,jc)
              jr = ir+st(2,ist); jr = max(1,jr); jr = min(nr,jr)
              id2 = abs(xid(jc,jr)) ! neighboring global ID
              if (id1 /= id2) then ! neighbor found
                if (nbr > mxnnb) then
                  call errmsg('Increase mxnnb')
                end if
                nbr = nbr + 1
                i4wk1d1(nbr) = id2; i4wk2d(1,nbr) = id2
                i4wk2d(2,nbr) = ic; i4wk2d(3,nbr) = ir
                i4wk2d(4,nbr) = jc; i4wk2d(5,nbr) = jr
              end if
            end do
          end if
        end do
      end do
      !
      ! get the unique values
      n = cfn_unique_i(i4wk1d1(1:nbr), nbr, 0)
      !
      if (iact == 1) then ! init
        cat(i)%nnb = n
        if (n > 0) then
          allocate(cat(i)%intf(n))
        end if
        nja = nja + n + 1
      else ! store
        nja = nja + 1
        ia(i) = nja
        gja(nja) = l2g(i)
        lja(nja) = i
        do j = 1, n
          intf => cat(i)%intf(j) ! get interface
          id = i4wk1d1(j) ! global neighbor ID
          intf%nb_gid = id; intf%nb_lid = g2l(id)
          nja = nja + 1
          gja(nja) = id
          lja(nja) = g2l(id)
          do k = 1, nbr
            if (i4wk2d(1,k) == id) then
              intf%n = intf%n + 1
            end if
          end do
          allocate(intf%my_gicir(2,intf%n), intf%nb_gicir(2,intf%n))
          allocate(intf%my_nlay(intf%n),    intf%nb_nlay(intf%n))
          intf%n = 0
          do k = 1, nbr
            if (i4wk2d(1,k) == id) then
              intf%n = intf%n + 1
              jc1 = i4wk2d(2,k); jr1 = i4wk2d(3,k)
              jc2 = i4wk2d(4,k); jr2 = i4wk2d(5,k)
              intf%w = intf%w + int(max(nlay(jc1,jr1),nlay(jc2,jr2)),i4b)
              intf%my_gicir(1,intf%n) = jc1+gic0-1; intf%my_gicir(2,intf%n) = jr1+gir0-1
              intf%nb_gicir(1,intf%n) = jc2+gic0-1; intf%nb_gicir(2,intf%n) = jr2+gir0-1
              intf%my_nlay(intf%n) = nlay(jc1,jr1)
              intf%nb_nlay(intf%n) = nlay(jc2,jr2)
            end if
          end do
        end do
      end if
    end do
    if (iact == 1) then
      allocate(gja(nja), lja(nja))
    end if
  end do
  ia(mxlid+1) = nja+1
  deallocate(i4wk2d, i4wk1d1)
  !
  ! check if the CRS is sorted
  do i = 2, mxlid
    id1 = lja(ia(i-1))
    id2 = lja(ia(i))
    if (id2 <= id1) then
      call errmsg('CRS not sorted.')
    end if
  end do
  do i = 1, mxlid
    do j = ia(i)+2, ia(i+1)-1
      id1 = lja(j-1)
      id2 = lja(j)
      if (id2 <= id1) then
        call errmsg('CRS not sorted.')
      end if
    end do
  end do
  !
  ! determine the unique regions
  nreg = 0
  allocate(reg(mxlid))
  do i = 1, mxlid
    reg(i) = 0
  end do
  nreg = 0
  do i = 1, mxlid
    if (reg(i) == 0) then
      nreg = nreg + 1
      call label_node(ia, lja, i, reg, nreg)
    end if
  end do
  !
  ! sort the regions by size
  allocate(i4wk1d1(nreg), i4wk1d2(nreg), regncell(nreg), regncat(nreg))
  do i = 1, nreg
    i4wk1d1(i) = i
    regncell(i) = DZERO
    regncat(i) = 0
  end do
  ncell = 0
  do i = 1, mxlid
    j = reg(i)
!    regncell(j) = regncell(j) - real(cat(i)%nlnd + cat(i)%nsea,r8b)
    regncell(j) = regncell(j) - real(cat(i)%w,r8b) 
    ncell = ncell + cat(i)%w
  end do
  !
  call quicksort_d(regncell, i4wk1d1, nreg)
  !
  ! relabel the regions
  do i = 1, nreg
    regncell(i) = abs(regncell(i))
    j = i4wk1d1(i)
    i4wk1d2(j) = i
  end do
  !
  do i = 1, mxlid
    j = reg(i)
    reg(i) = i4wk1d2(j)
    cat(i)%ireg = reg(i)
  end do
  !
  ! count the number of catchments per region
  do i = 1, mxlid
    j = reg(i)
    regncat(j) = regncat(j) + 1
  end do 
  !
  deallocate(i4wk1d1, i4wk1d2)
  !
  write(*,'(a,f10.2)') 'Total # active cells (M):', real(ncell)/1000000.
  n = min(nreg,10)
  write(*,*) 'Top '//ta((/n/))//' regions:'
  do i = 1, min(nreg,10)
    write(*,'(i2.2,a,f7.2,a,i6,a,f6.2,a)') i, &
      ' - # cells (M):', real(regncell(i))/1000000., &
      '; # catchments:', regncat(i), &
      '; % cells cum.:', 100.*sum(regncell(1:i))/real(ncell),')'
  end do
  !
  ! debug output
  if (.false.) then
  do ir = 1, nr
    do ic = 1, nc
      id = xid(ic,ir)
      if (abs(id) /= 0) then
        if (id > 0) then
          xid(ic,ir) = reg(g2l(id))
        else
          xid(ic,ir) = -reg(g2l(-id))
        end if
      end if
    end do
  end do
  call writeidf('reg.idf', xid, nc, nr, &
    idf%xmin + (gic0-1)*idf%dx, idf%ymin + (idf%nrow-gir1)*idf%dx, &
    idf%dx, 0.D0)
  end if
  !
  allocate(i4wk1d1(mxlid + 1))
  allocate(i4wk1d2(7*mxlid + nja*4))
  allocate(i4wk1d3(nja))
  !
  mja = 0
  do i = 1, mxlid
    bb => cat(i)%gbb
    mja = mja + 1
    i4wk1d1(i) = mja
    i4wk1d2(mja) = bb%ir0; mja = mja + 1 ! 1
    i4wk1d2(mja) = bb%ir1; mja = mja + 1 ! 2
    i4wk1d2(mja) = bb%ic0; mja = mja + 1 ! 3
    i4wk1d2(mja) = bb%ic1; mja = mja + 1 ! 4
    i4wk1d2(mja) = cat(i)%nlnd; mja = mja + 1 ! 5
    i4wk1d2(mja) = cat(i)%nsea; mja = mja + 1 ! 6
    i4wk1d2(mja) = l2g(lja(ia(i))) ! 7
    !
    ! part 1: add the IDs
    do j = ia(i), ia(i+1)-1
      mja = mja + 1
      i4wk1d2(mja) = lja(j)
    end do
    ! part 2: add number of cells
    mja = mja + 1; i4wk1d2(mja) = cat(i)%nsea + cat(i)%nlnd
    k = 0
    do j = ia(i)+1, ia(i+1)-1
      k = k + 1; intf => cat(i)%intf(k)
      ! check
      if (intf%nb_lid /= lja(j)) then
        call errmsg('Program error.')
      end if
      mja = mja + 1
      i4wk1d2(mja) = intf%n
    end do
    ! part 3: add the weights
    mja = mja + 1; i4wk1d2(mja) = cat(i)%w
    k = 0
    do j = ia(i)+1, ia(i+1)-1
      k = k + 1; intf => cat(i)%intf(k)
      ! check
      if (intf%nb_lid /= lja(j)) then
        call errmsg('Program error.')
      end if
      mja = mja + 1
      i4wk1d2(mja) = intf%w
    end do
    ! part 4: add pointer to cells
    do j = ia(i), ia(i+1)-1
      mja = mja + 1
      i4wk1d2(mja) = -1
    end do
  end do
  i4wk1d1(mxlid+1) = mja+1
  !
  ! set pointers to nodes in map.nodes.bin
  p = 1
  do i = 1, mxlid
    j = i4wk1d1(i) + 4; nlnd = i4wk1d2(j)
    j = i4wk1d1(i) + 5; nsea = i4wk1d2(j)
    n = (i4wk1d1(i+1) - i4wk1d1(i) - 7)/4
    j = i4wk1d1(i) + 7 + 3*n
    i4wk1d2(j) = p
    !p = p + 2*(nlnd+nsea)
    p = p + nlnd+nsea
  end do 

  ! set pointers to nodes in map.intfnodes.bin
  p = 1
  do i = 1, mxlid
    n = (i4wk1d1(i+1) - i4wk1d1(i) - 7)/4
    do j = i4wk1d1(i) + 7 + 3*n + 1, i4wk1d1(i+1) - 1
      i4wk1d2(j) = p
      m = i4wk1d2(j-2*n)
      !p = p + 4*m
      p = p + m
    end do
  end do 
  !
  ! ---------------
  ! write the catchments ia/ja
  f = trim(out_pref)//'.catch.bin'
  call open_file(f, iu, 'w', .true.)
  write(iu) mxlid
  write(iu) mja
  write(iu) (i4wk1d1(i),i=1,mxlid + 1)
  write(iu) (i4wk1d2(i),i=1,mja)
  !
  ! ---------------
  ! write the regions
  if (allocated(i4wk1d1)) deallocate(i4wk1d1)
  if (allocated(i4wk1d2)) deallocate(i4wk1d2)
  allocate(i4wk1d1(nreg+1))
  allocate(i4wk1d2(mxlid))
  allocate(regwk(nreg))
  do i = 1, mxlid
    j = reg(i)
    regwk(j)%n = regwk(j)%n + 1
  end do
  do i = 1, nreg
    n = regwk(i)%n
    allocate(regwk(i)%ind(n), regwk(i)%xid(n))
    do j = 1, n
      regwk(i)%ind(j) = j
    end do
    regwk(i)%n = 0
  end do
  do i = 1, mxlid
    j = reg(i)
    regwk(j)%n = regwk(j)%n + 1
    n = regwk(j)%n
    regwk(j)%xid(n) = real(i,r8b)
  end do
  mja = 1
  do i = 1, nreg
    call quicksort_d(regwk(i)%xid, regwk(i)%ind, regwk(i)%n)
    i4wk1d1(i) = mja
    do j = 1, regwk(i)%n
      i4wk1d2(mja) = int(regwk(i)%xid(j),i4b)
      mja = mja + 1
    end do
  end do
  i4wk1d1(nreg+1) = mja
  f = trim(out_pref)//'.reg.bin'
  call open_file(f, iu, 'w', .true.)
  write(iu) nreg
  write(iu) mja - 1
  write(iu) (regncell(i),i=1,nreg)
  write(iu)(i4wk1d1(i),i=1,nreg+1)
  write(iu)(i4wk1d2(i),i=1,mja-1)
  close(iu)
  !
  ! prepare for the tiles
  if (allocated(i4wk2d)) deallocate(i4wk2d)
  allocate(i4wk2d(nc,nr))
  allocate(itile(nc,nr))
  do ir = 1, nr
    do ic = 1, nc
      i4wk2d(ic,ir) = 0
      itile(ic,ir) = 0
    end do
  end do  
  call getarg(5, f); call chkexist(f)
  call open_file(f, iu, 'r')
  read(iu,*) ntile
  call getarg(6, tile_pref)
  !
  do i = 1, ntile
    read(iu,*) ic0, ic1, ir0, ir1
    !
    lskip = .false.
    if (ic1 < gic0) lskip = .true.
    if (ic0 > gic1) lskip = .true.
    if (ir1 < gir0) lskip = .true.
    if (ir0 > gir1) lskip = .true.
    if (lskip) then
      !write(*,'(a)') 'Skipping '//trim(f)//'...'
      cycle
    end if
    !
    nct = ic1-ic0+1; nrt = ir1-ir0+1
    write(f,'(a,2(i3.3,a))') trim(tile_pref), i, '-', ntile, '.idf'; 
    call chkexist(f)
    write(*,'(a)') 'Reading '//trim(f)//'...'
    if (.not.idfread(idf, f, 1)) then
      call errmsg('Could not read '//trim(f))
    end if
    !
    ! check sizes
    if ((idf%nrow /= nrt).or.(idf%ncol /= nct)) then
      call errmsg('Invalid tile.')
    end if
    !
    ! add the sea boundary
    if (sea_opt == 1) call addboundary(idf%x, idf%ncol, idf%nrow, idf%nodata)
    !
    do ir = 1, idf%nrow
      do ic = 1, idf%ncol
        jc = ic0 + ic - 1; jr = ir0 + ir - 1 ! global
        kc = jc - gic0 + 1; kr = jr - gir0 + 1
        !if ((jc == 14995).and.(jr == 8943)) then
        !  write(*,*) '@@@@'
        !end if
        if ((kc >= 1).and.(kc <= nc).and.(kr >= 1).and.(kr <= nr)) then
          r4val = idf%x(ic,ir)
          !if ((r4val /= idf%nodata) .and.(r4val > 0.)) then
          !  itile(kc,kr) = i
          !  if (landmask(kc,kr) == 2) then
          !    if ((r4val /= idf%nodata) .and.(r4val < 0.).and.(i4wk2d(kc,kr) <= 0.)) then
          !      i4wk2d(kc,kr) = i
          !    else
          !      i4wk2d(kc,kr) = -i
          !    end if
          !  end if
          !end if
          if ((r4val /= idf%nodata) .and.(r4val > 0.)) then
            itile(kc,kr) = i
          end if
          if ((landmask(kc,kr) == 2).and.(sea_opt == 1)) then
            if ((r4val /= idf%nodata) .and.(r4val < 0.).and.(i4wk2d(kc,kr) <= 0.)) then
              i4wk2d(kc,kr) = i
            else
              i4wk2d(kc,kr) = -i
            end if
          end if
        end if
      end do
    end do
    !
    if (.not.idfallocatex(idf)) then
      call errmsg('Could not deallocate idf.')
    end if
  end do
  close(iu)
  !
  do ir = 1, nr
    do ic = 1, nc
      if (i4wk2d(ic,ir) /= 0) then
        itile(ic,ir) = -abs(i4wk2d(ic,ir))
      end if
      if ((landmask(ic,ir) == 2).and.(itile(ic,ir) == 0)) then
        call errmsg("Some sea-cells do not have an associated tile.")
      end if
    end do
  end do
  !
  if (.false.) then
    call writeidf('tile.idf', itile, nc, nr, &
      gxmin+(gic0-1)*gcs, gymin+(gnrow-gir1)*gcs, gcs, 0.D0)
    stop
  end if
  
  ! prepare the nodal data
  n = 0
  do i = 1, mxlid
    n = n + cat(i)%nlnd + cat(i)%nsea
  end do
  if (allocated(i4wk2d)) deallocate(i4wk2d)
  if (allocated(i1wk1d)) deallocate(i1wk1d)
  if (allocated(i2wk1d)) deallocate(i2wk1d)
  allocate(i4wk2d(2,n),i1wk1d(n),i2wk1d(n))
  n = 0
  do i = 1, mxlid
    !if (i == 203686) then
    !  write(*,*) 'break'
    !end if
    bb => cat(i)%lbb
    do ir = bb%ir0, bb%ir1
      do ic = bb%ic0, bb%ic1
        id = xid(ic,ir)
        if ((id /= 0).and.(abs(id) == l2g(i))) then
          n = n + 1
          jc = ic + gic0 - 1; jr = ir + gir0 - 1
          i4wk2d(1,n) = jc; i4wk2d(2,n) = jr ! global node number
          i1wk1d(n) = nlay(ic,ir)
          i2wk1d(n) = itile(ic,ir)  !tile
          ! check
          if (i2wk1d(n) == 0) then
            call errmsg("No tile found.")
          end if
          if (id < 0) then
            i4wk2d(1,n) = -i4wk2d(1,n)
          end if
        end if
      end do
    end do
  end do
  deallocate(xid)
  
  ! ---------------
  ! write the nodes
  f = trim(out_pref)//'.nodes.bin'
  call open_file(f, iu, 'w', .true.)
  write(iu)((i4wk2d(j,i),j=1,2),i=1,n)
  close(iu)
  !
  ! ---------------
  ! write the number of layers
  f = trim(out_pref)//'.nlay.bin'
  call open_file(f, iu, 'w', .true.)
  write(iu)(i1wk1d(i),i=1,n)
  close(iu)
  !
  ! ---------------
  ! write the tiles
  f = trim(out_pref)//'.tiles.bin'
  call open_file(f, iu, 'w', .true.)
  write(iu)(i2wk1d(i),i=1,n)
  close(iu)
  !
  if (allocated(i1wk1d)) deallocate(i1wk1d)
  allocate(i1wk1d(ntile))
  do i = 1, ntile
    i1wk1d(i) = 0
  end do
  do i = 1, n
    j = int(i2wk1d(i),i4b)
    j = abs(j)
    i1wk1d(j) = 1
  end do
  m = 0
  do i = 1, ntile
    if (i1wk1d(i) == 1) m = m + 1
  end do
  if (allocated(i4wk1d1)) deallocate(i4wk1d1)
  allocate(i4wk1d1(m))
  m = 0
  do i = 1, ntile
    if (i1wk1d(i) == 1) then
      m = m + 1
      i4wk1d1(m) = i
    end if
  end do
  write(*,'(a)') 'Tiles found: '//ta(i4wk1d1)
  !
  ! ---------------
  ! write the interfaces
  m = 0
  do i = 1, mxlid
    do j = 1, cat(i)%nnb
      m = m + cat(i)%intf(j)%n
    end do
  end do
  if (allocated(i4wk2d)) deallocate(i4wk2d)
  allocate(i4wk2d(6,m))
  m = 0
  do i = 1, mxlid
    do j = 1, cat(i)%nnb
      intf => cat(i)%intf(j)
      !if ((l2g(i) == 1676128).and.(intf%nb_gid == 1676916)) then
      !  write(*,*) '@@ Debug'
      !end if
      do k = 1, intf%n
        m = m + 1
        i4wk2d(1,m) = intf%my_gicir(1,k)
        i4wk2d(2,m) = intf%my_gicir(2,k)
        i4wk2d(3,m) = intf%my_nlay(k)
        i4wk2d(4,m) = intf%nb_gicir(1,k)
        i4wk2d(5,m) = intf%nb_gicir(2,k)
        i4wk2d(6,m) = intf%nb_nlay(k)
      end do
    end do
  end do
  f = trim(out_pref)//'.intfnodes.bin'
  call open_file(f, iu, 'w', .true.)
  write(iu)((i4wk2d(j,i),j=1,6),i=1,m)
  close(iu)
  
end program

recursive subroutine label_node(ia, ja, id1, i4wk1d, ireg)
  use utilsmod, only: i4b
  implicit none
  integer(i4b), dimension(:), intent(in) :: ia
  integer(i4b), dimension(:), intent(in) :: ja
  integer(i4b), intent(in) :: id1
  integer(i4b), dimension(:), intent(inout) :: i4wk1d
  integer(i4b), intent(in) :: ireg
  !
  integer(i4b) :: i, id2
  !
  i4wk1d(id1) = ireg
  !
  do i = ia(id1)+1, ia(id1+1)-1
    id2 = ja(i)
    if (i4wk1d(id2) == 0) then
      call label_node(ia, ja, id2, i4wk1d, ireg)
    end if
  end do
end subroutine label_node
  