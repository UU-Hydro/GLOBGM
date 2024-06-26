program mf6ggm
  ! modules
  use pcrModule, only: tMap
  use utilsmod, only: i1b, i2b, i4b, i8b, r4b, r8b, mxslen, open_file, chkexist, errmsg, &
    logmsg, quicksort_d, label_node, tBB, DZERO, writeidf, create_dir, swap_slash, &
    change_work_dir, get_work_dir, ta, fileexist
  use ehdrModule, only: writeflt 
  use metis_module, only: tMetis, tolimbal
  use mf6_module, only:  tMf6_sol, tMf6_mod, tReg, raw, tDistMap, tExchange, ntile, tilebb, &
    gncol, gnrow, gnlay, gxmin, gymin, gcs, cam2 !debug
  !
  implicit none
  !
  type(tMf6_mod), dimension(:), pointer :: smod => null()
  !
  type tCatNodIntf
    integer(i4b) :: nb_lcatid = 0
    integer(i4b) :: nb_gcatid = 0
    integer(i4b) :: nb_modid = 0
    integer(i4b) :: n = 0
    integer(i4b), dimension(:), pointer   :: my_ireg  => null()
    integer(i4b), dimension(:,:), pointer :: my_gicir => null()
    integer(i4b), dimension(:), pointer   :: my_nlay  => null()
    integer(i4b), dimension(:), pointer   :: nb_ireg  => null()
    integer(i4b), dimension(:,:), pointer :: nb_gicir => null()
    integer(i4b), dimension(:), pointer   :: nb_nlay  => null()
  end type tCatNodIntf
  !
  type tCatIntf
    integer(i4b) :: nintf = 0
    type(tCatNodIntf), dimension(:), pointer :: intf => null()
  end type tCatIntf
  !
  type tModReg
    integer(i4b) :: ncat     = 0
    integer(i4b) :: ncatintf = 0
    integer(i4b), dimension(:), pointer   :: lcatid  => null()
    integer(i4b), dimension(:), pointer   :: gcatid  => null()
    type(tCatIntf), dimension(:), pointer :: catintf => null()
  end type tModReg
  !
  type tMod
    integer(i4b) :: lmodid = 0
    integer(i4b) :: gmodid = 0
    integer(i4b) :: ncat  = 0
    integer(i4b) :: nreg  = 0
    integer(i4b) :: ncell = 0
    integer(i4b),  dimension(:), pointer :: lcatid => null()
    integer(i4b),  dimension(:), pointer :: gcatid => null()
    integer(i4b),  dimension(:), pointer :: regid  => null()
    type(tModReg), dimension(:), pointer :: reg    => null()
  end type tMod
  !
  type tSol
    integer(i4b) :: iact = 1
    integer(i4b) :: iwrite_modid = 0
    logical      :: lmm = .false. ! T: multi-model; F: single-model
    integer(i4b) :: ncat = 0
    integer(i4b) :: nmod = 1
    integer(i4b) :: np   = 1
    character(len=mxslen) :: fpart2cat = ''
    type(tMod), dimension(:), pointer :: mod => null()
  end type tSol
  integer(i4b), parameter :: iureg  = 1
  integer(i4b), parameter :: iucat  = 2
  integer(i4b), parameter :: iunod  = 3
  integer(i4b), parameter :: iuintf = 4
  integer(i4b), parameter :: iunlay = 5
  integer(i4b), parameter :: iutil  = 6
  integer(i4b), parameter :: iutot  = iutil
  integer(i4b), dimension(iutot) :: miu
  !
  ! --- local
  type(tSol), pointer :: s => null()
  type(tMf6_sol), pointer :: ms => null()
  type(tSol), dimension(:), pointer :: sol => null()
  type(tMetis), pointer :: m => null(), mm => null()
  type(tMetis), dimension(:), pointer :: solmet => null()
  type(tMod), pointer :: md => null()
  type(tMf6_mod), pointer :: mmd => null(), mmd2 => null()
  type(tBB), pointer :: bb => null(), mbb => null(), sbb => null()
  type(tReg), pointer :: r => null()
  type(tModReg), pointer :: mr => null()
  type(tCatIntf), pointer :: intf => null()
  type(tCatNodIntf), pointer :: nodintf => null()
  type(tExchange), pointer :: xch => null(), xch2 => null()
  type(tMap), pointer :: map => null()
  !
  integer(i4b), dimension(:),     allocatable :: i4wk1d, i4wk1d2
  integer(i4b), dimension(:,:),   allocatable :: i4wk2d
  integer(i4b), dimension(:,:,:), allocatable :: i4wk3d
  real(r4b),    dimension(:,:),   allocatable :: r4wk2d
  real(r4b),    dimension(:,:,:), allocatable :: r4wk3d
  real(r8b),    dimension(:),     allocatable :: r8wk1d
  !
  character(len=mxslen) :: str, f, d, out_dir, map_pref, mod_inp
  logical :: lwmod, lwsol, lok, lmv, llastsol
  integer(i1b) :: nlay
  integer(i2b) :: itile, i2v
  integer(i4b) :: iu, ju, nsol, nsol_mm, nsol_sm, isol, i, j, k, kk, k0, k1, n, nn, nb, n_reg, n_reg_last, nja_reg
  integer(i4b) :: solncat, solmxlid, mxlid, nja_cat, m_reg, iact, ncell, i4v, i4mv
  integer(i4b) :: lid, gid, lid1, gid1, lid2, nja, p, pp, ppr, p0, w, wtot, nmod, ipart, ireg
  integer(i4b) :: il0, il1, ir0, ir1, ic0, ic1, nlay0, nlay1, ios
  integer(i4b) :: mxncol, mxnrow, nc, nr, ic, ir, il, jc, jr, kc, kr, ixch, jxch, idum
  integer(i4b) :: gir0, gir1, gic0, gic1, pnod, ptil, pnlay, nodsign, modid, nintf, modid0, modid1
  integer(i4b), dimension(:), allocatable :: ia_reg, ja_reg, ia_cat, catlid, catgid
  integer(i4b), dimension(:), allocatable :: ia, ja
  integer(i4b), dimension(:), allocatable :: mapping
  integer(i4b), dimension(:,:), allocatable :: solmodid
  integer(i8b) :: pp8
  real(r8b) :: xmin, ymin
  real(r8b), dimension(:), allocatable :: ncell_reg
! ------------------------------------------------------------------------------
  !
  lwmod = .true.
  lwsol = .true.
  modid0 = 0; modid1 = huge(0)
  !
  ! read the input file
  n = nargs() - 1
  if (n == 2) then
    lwmod = .false.
  end if
  if (n == 3) then
    lwsol = .false.
    call getarg(2, f); read(f,*) modid0
    call getarg(3, f); read(f,*) modid1
  end if
  !
  call getarg(1, f)
  call open_file(f, iu, 'r')
  read(iu,*) gncol, gnrow, gnlay, gxmin, gymin, gcs
  read(iu,*) f
  if (.false.) then !map file
    allocate(cam2(gnrow))
    allocate(map)
    lok = map%init(f)
    ic = 1
    do ir = 1, gnrow
      call map%get_val(ic, ir, i4v, lmv)
      if (lmv) then
        cam2(ir) = i4mv
      end if
    end do
    call map%clean()
    call map%close()
    deallocate(map)
    f = 'cam2.bin'
    call open_file(f, ju, 'w', .true.)
    write(ju)(cam2(ir),ir=1,gnrow)
    close(ju)
  else
    if (fileexist(f)) then
      allocate(cam2(gnrow))
      call open_file(f, ju, 'r', .true.)
      read(ju)(cam2(ir),ir=1,gnrow)
      close(ju)
    end if
  end if
  read(iu,'(a)') out_dir
  call create_dir(out_dir, .true.)
  read(iu,'(a)') map_pref
  read(iu,*) nsol_mm, nsol_sm
  llastsol = .false.
  if (nsol_mm < 0) then
    llastsol = .true.
  end if
  nsol_mm = abs(nsol_mm)
  nsol_sm = max(nsol_sm, 0)
  nsol = nsol_mm + nsol_sm
  allocate(sol(nsol), solmet(nsol_mm+1))
  do i = 1, nsol
    sol(i)%fpart2cat = ''
  end do
  !
  do i = 1, nsol_mm
    sol(i)%lmm = .true.
    read(iu,'(a)') str
    read(str,*,iostat=ios) sol(i)%iact, sol(i)%nmod, sol(i)%np, sol(i)%iwrite_modid
    if (ios /= 0) then
      read(str,*,iostat=ios) sol(i)%iact, sol(i)%nmod, sol(i)%np
    end if
    if (ios /= 0) then
      read(str,*,iostat=ios) sol(i)%iact, sol(i)%nmod
      if (sol(i)%nmod < 0) then
        read(iu,'(a)') sol(i)%fpart2cat
        call open_file(sol(i)%fpart2cat, ju, 'r', .true.)
        read(ju) sol(i)%nmod
        close(ju)
        call logmsg('*** data read for '//ta((/sol(i)%nmod/))//' submodels ***')
      end if
      sol(i)%np = sol(i)%nmod
    end if
    sol(i)%iact = max(sol(i)%iact, 0)
  end do
  read(iu,'(a)') mod_inp
  close(iu)
  !
  ! number of models
  modid = 0
  nmod = 0
  do i = 1, nsol
    s => sol(i)
    if (s%iact == 1) then
      allocate(s%mod(s%nmod))
    end if
    do j = 1, s%nmod
      modid = modid + 1
      if (s%iact == 1) then
        nmod = nmod + 1
        s%mod(j)%lmodid = nmod
        s%mod(j)%gmodid = modid
      end if
    end do
  end do
  allocate(smod(nmod))
  !
  ! open all mapping files
  miu = 0
  f = trim(map_pref)//'.reg.bin';       call open_file(f, miu(iureg),  'r', .true.)
  f = trim(map_pref)//'.catch.bin';     call open_file(f, miu(iucat),  'r', .true.)
  f = trim(map_pref)//'.nodes.bin';     call open_file(f, miu(iunod),  'r', .true.)
  f = trim(map_pref)//'.tiles.bin';     call open_file(f, miu(iutil),  'r', .true.)
  f = trim(map_pref)//'.nlay.bin';      call open_file(f, miu(iunlay), 'r', .true.)
  f = trim(map_pref)//'.intfnodes.bin'; call open_file(f, miu(iuintf), 'r', .true.)
  !
  ! Read the region data
  read(miu(iureg)) n_reg
  read(miu(iureg)) nja_reg
  allocate(ncell_reg(n_reg), ia_reg(n_reg+1), ja_reg(nja_reg))
  read(miu(iureg))(ncell_reg(i),i=1,n_reg)
  read(miu(iureg))(ia_reg(i),i=1,n_reg+1)
  read(miu(iureg))(ja_reg(i),i=1,nja_reg)
  !
  ! Read the catchment data
  read(miu(iucat)) mxlid
  read(miu(iucat)) nja_cat
  allocate(ia_cat(mxlid+1))
  read(miu(iucat))(ia_cat(i),i=1,mxlid+1); p0 = (mxlid+1)*i4b
  !
  ! multi-model solution: METIS partitioning of catchments
  do isol = 1, nsol_mm
    s => sol(isol)
    if (s%iact == 0) cycle
    !
    n_reg_last = isol
    if (llastsol) then
      if (isol == nsol_mm) then
!       debug
!        n_reg_last = isol + 1
        n_reg_last = n_reg
      end if
    end if
    !
    solncat = 0; nn = 0
    do i = isol, n_reg_last
      solncat = solncat + ia_reg(i+1) - ia_reg(i)
      nn = nn + ncell_reg(i)
    end do
    call logmsg('Solution '//ta((/isol/))//': '//ta((/nn/))//' cells, '//ta((/solncat/))//' catchments')
    !
    s%ncat = solncat
    if (allocated(catlid)) deallocate(catlid)
    allocate(catlid(solncat))
    k = 0
    do i = isol, n_reg_last
      do j = ia_reg(i), ia_reg(i+1)-1
        k = k + 1
        catlid(k) = ja_reg(j)
      end do
    end do
    solmxlid = maxval(catlid)
    !
    if (allocated(mapping)) deallocate(mapping)
    allocate(mapping(solmxlid))
    mapping = 0
    do j = 1, solncat
      lid = catlid(j)
      mapping(lid) = j
    end do
    !
    m => solmet(isol)
    allocate(m%nparts); m%nparts = sol(isol)%nmod
    allocate(m%nvtxs); m%nvtxs = solncat
    allocate(m%vwgt(m%nvtxs), m%vsize(m%nvtxs)); m%vsize = 1
    allocate(m%part(m%nvtxs)); m%part = 0
    allocate(m%xadj(m%nvtxs+1))
    !
    nja = 0
    do j = 1, solncat
      lid = catlid(j)
      n = (ia_cat(lid+1) - ia_cat(lid) - 7)/4
      nja = nja + n - 1
    end do
    !
    allocate(m%adjncy(nja), m%adjwgt(nja), m%ncon); m%ncon = 1
    allocate(m%tpwgts(m%nparts*m%ncon)); m%tpwgts = 1./real(m%nparts)
    allocate(m%ubvec(m%ncon)); m%ubvec(1) = tolimbal
    !
    m%xadj(1) = 0; nn = 0
    do j = 1, solncat
      lid1 = catlid(j) ! catchment ID
      n = (ia_cat(lid1+1) - ia_cat(lid1) - 7)/4
      k0 = ia_cat(lid1) + 7 + 2; k1 = ia_cat(lid1) + 7 + n
      k = k0 - 1; p = p0 + k*i4b + 1
      read(miu(iucat),pos=p+2*n*i4b) w ! read the nodal weight
      if (w <= 0) then
        call errmsg('Program error: read negative nodal weights.')
      end if
      m%vwgt(j) = int(w,i8b)
      m%xadj(j+1) = m%xadj(j)
      do k = k0, k1
        p = p0 + k*i4b + 1
        read(miu(iucat),pos=p) lid2
        if ((lid2 < 0).or.(lid2 > mxlid)) then
          call errmsg('Program error: read invalid catchment ID ')
        end if
        read(miu(iucat),pos=p+2*n*i4b) w ! read the edge weight
        nn = nn + 1
        m%adjncy(nn) = mapping(lid2) - 1 ! convert to local number
        m%xadj(j+1) = m%xadj(j+1) + 1
        if (w <= 0) then
          call errmsg('Program error: read invalid edge weight.')
        end if
        m%adjwgt(nn) = int(w,i8b)
      end do
    end do
    call m%set_opts()
    if (m%nparts > 1) then
      if (m%nparts == m%nvtxs) then
        do j = 1, m%nvtxs
          m%part(j) = j-1
        end do
      else
        if (len_trim(s%fpart2cat) > 0) then
          call open_file(s%fpart2cat, ju, 'r', .true.)
          read(ju) idum
          if (m%nparts /= idum) call errmsg('Program error')
          do ipart = 1, m%nparts
            read(ju) nn
            do i = 1, nn
              read(ju) j ! global id
              lid = mapping(j)
              m%part(lid) = ipart-1
            end do
          end do
          close(ju)
          call m%calc_imbal()
        else
          call m%recur()
        end if
      end if
    end if
    !
    ! count and allocate
    do j = 1, solncat
      ipart = m%part(j) + 1
      s%mod(ipart)%ncat  = s%mod(ipart)%ncat + 1
      s%mod(ipart)%ncell = s%mod(ipart)%ncell + int(m%vwgt(j),i4b)
    end do
    do j = 1, s%nmod
      n = s%mod(j)%ncat; md => s%mod(j)
      allocate(md%lcatid(max(n,1)), md%gcatid(max(n,1)))
      md%ncat = 0
    end do
    ! store
    do j = 1, solncat
      ipart = m%part(j) + 1; md => s%mod(ipart)
      n = md%ncat; n = n + 1; md%ncat = n
      md%lcatid(n) = catlid(j)
    end do
  end do
  !
  ! single model solution: partitioning of regions
  if (nsol_sm > 0) then
    m => solmet(nsol_mm + 1)
    m_reg = n_reg - nsol_mm
    allocate(m%nparts); m%nparts = nsol_sm
    allocate(m%nvtxs); m%nvtxs = m_reg
    allocate(m%vwgt(m%nvtxs), m%vsize(m%nvtxs)); m%vsize = 1
    allocate(m%part(m%nvtxs)); m%part = 0
    allocate(m%xadj(m%nvtxs+1)); m%xadj = 0
    nja = 1
    allocate(m%adjncy(nja), m%adjwgt(nja), m%ncon)
    m%xadj = 0; m%adjwgt = 0; m%ncon = 1
    allocate(m%tpwgts(m%nparts*m%ncon)); m%tpwgts = 1./real(m%nparts)
    allocate(m%ubvec(m%ncon)); m%ubvec(1) = tolimbal
    nn = 0
    do i = nsol_mm + 1, n_reg
      wtot = 0
      do j = ia_reg(i), ia_reg(i+1)-1
        lid = ja_reg(j)
        n = (ia_cat(lid+1) - ia_cat(lid) - 7)/4
        k0 = ia_cat(lid) + 7 + 2; k1 = ia_cat(lid) + 7 + n
        k = k0 - 1; p = p0 + k*i4b + 1
        read(miu(iucat),pos=p+2*n*i4b) w ! read the nodal weight
        wtot = wtot + w
      end do
      nn = nn + 1; m%vwgt(nn) = int(wtot,i8b)
    end do
    if (m%nparts > 1) then
      call m%set_opts(); call m%recur()
    end if
    !
    ! store
    nn = 0
    do i = nsol_mm + 1, n_reg
      nn = nn + 1
      ipart = m%part(nn) + 1; isol = nsol_mm + ipart
      s => sol(isol)
      solncat = ia_reg(i+1) - ia_reg(i)
      s%ncat = s%ncat + solncat; s%mod(1)%ncat = s%ncat
    end do
    do i = nsol_mm + 1, nsol
      s => sol(i); md => s%mod(1)
      n = md%ncat
      allocate(md%lcatid(n),md%gcatid(n))
      md%ncat = 0
    end do
    ! store
    nn = 0
    do i = nsol_mm + 1, n_reg
      nn = nn + 1
      ipart = m%part(nn) + 1; isol = nsol_mm + ipart
      s => sol(isol)
      n = s%mod(1)%ncat
      do j = ia_reg(i), ia_reg(i+1)-1
        n = n + 1
        s%mod(1)%lcatid(n) = ja_reg(j) ! store the local catchment ID
      end do
      s%mod(1)%ncat = n
    end do
  !
  end if
  !
  ! sort the catchments by local ID and store global catchment ID
  allocate(catgid(mxlid))
  do i = 1, mxlid
    catgid(i) = 0
  end do
  do i = 1, nsol
    s => sol(i)
    if (s%iact == 0) cycle
    do j = 1, s%nmod
      md => s%mod(j)
      allocate(i4wk1d(md%ncat),r8wk1d(md%ncat))
      do k = 1, md%ncat
        i4wk1d(k) = k
        r8wk1d(k) = real(md%lcatid(k),r8b)
      end do
      call quicksort_d(r8wk1d, i4wk1d, md%ncat)
      do n = 1, md%ncat
        md%lcatid(n) = int(r8wk1d(n),i4b)
        lid = md%lcatid(n); k = ia_cat(lid) + 7; p = p0 + k*i4b + 1
        read(miu(iucat),pos=p) gid ! read the global catchment ID
        md%gcatid(n) = gid
        catgid(lid) = gid
      end do
      deallocate(i4wk1d,r8wk1d)
    end do
  end do
  !
  ! Debug:
  if (.false.) then
    gir0 = 10000; gir1 = 16300; gic0 = 33000; gic1 = 42000 !AUS
    nr = gir1-gir0+1; nc = gic1-gic0+1
    allocate(r4wk2d(nc,nr))
    do ir = 1, nr
      do ic = 1, nc
        r4wk2d(ic,ir) = 0.
      end do
    end do
    do i = 1, 1
      s => sol(i)
      do j = 1, s%nmod
        md => s%mod(j); ipart = md%gmodid
        do k = 1, md%ncat
          lid = md%lcatid(k); gid = md%gcatid(k)
          n = (ia_cat(lid+1) - ia_cat(lid) - 7)/4; nn = ia_cat(lid)
          p =  2*i4b + p0 + (nn-1)*i4b + 1
          read(miu(iucat),pos=p) ir0
          read(miu(iucat),pos=p+1*i4b) ir1
          read(miu(iucat),pos=p+2*i4b) ic0
          read(miu(iucat),pos=p+3*i4b) ic1
          do ir = ir0, ir1
            do ic = ic0, ic1
              jc = ic - gic0 + 1; jr = ir - gir0 + 1
              if ((jc >= 1).and.(jc <= nc).and.(jr >= 0).and.(jr <= nr)) then
                !r4wk2d(jc,jr) = real(ip,r4b)
              end if
            end do
          end do
          !
          p = 2*i4b + p0 + (nn-1)*i4b + 7*i4b + n*i4b + 1
          read(miu(iucat),pos=p) ncell
          read(miu(iucat),pos=p+2*n*i4b) pp
          p = 2*(pp-1)*i4b + 1
          do kk = 1, ncell
            read(miu(iunod),pos=p) ic; p = p + i4b
            read(miu(iunod),pos=p) ir; p = p + i4b
            jc = abs(ic) - gic0 + 1; jr = ir -gir0 + 1
            if ((jc >= 1).and.(jc <= nc).and.(jr >= 0).and.(jr <= nr)) then
              r4wk2d(jc,jr) = real(ipart,r4b)
            end if
          end do
        end do
      end do
    end do
    xmin = gxmin + (gic0-1)*gcs
    ymin = gymin + (gnrow-gir1)*gcs
    call writeidf('tmp.idf', r4wk2d, nc, nr, dble(xmin), dble(ymin), dble(gcs), DZERO)
    stop
  end if
  
  ! determine the model regions
  mxncol = 0; mxnrow = 0
  if (allocated(catlid)) deallocate(catlid)
  allocate(catlid(mxlid))
  do isol = 1, nsol
    s => sol(isol)
    if (s%iact == 0) cycle
    do ipart = 1, s%nmod
      md => s%mod(ipart)
      do i = 1, mxlid
        catlid(i) = 0
      end do
      do i = 1, md%ncat
        lid = md%lcatid(i)
        catlid(lid) = i
      end do
      allocate(ia(md%ncat+1))
      do iact = 1, 2
        nja = 0
        do nn = 1, md%ncat
          lid1 = md%lcatid(nn)
          n = (ia_cat(lid1+1) - ia_cat(lid1) - 7)/4
          k0 = ia_cat(lid1) + 7 + 1; k1 = ia_cat(lid1) + 7 + n
          k = k0 - 1; p = p0 + k*i4b + 1
          do k = k0, k1
            p = p0 + k*i4b + 1
            read(miu(iucat),pos=p) lid2
            !
            ! check
            if (k == k0) then
              if (lid1 /= lid2) then
                call errmsg('Program error: non-matchming catchment ID')
              end if
            end if
            !
            if (catlid(lid2) > 0) then
              nja = nja + 1
              if (k == k0) then
                ia(nn) = nja
              end if
              if (iact == 2) then
                ja(nja) = catlid(lid2)
              end if
            end if
          end do
        end do
        if (iact == 1) then
          allocate(ja(nja))
        end if
        ia(md%ncat+1) = nja + 1
      end do !iact
      !
      allocate(md%regid(md%ncat))
      do i = 1, md%ncat
        md%regid(i) = 0
      end do
      md%nreg = 0
      do i = 1, md%ncat
        if (md%regid(i) == 0) then
          md%nreg = md%nreg + 1
          call label_node(ia, ja, i, md%regid, md%nreg)
        end if
      end do
      if (md%nreg == 0) then
        call errmsg('Program error: no regions found.')
      end if
      allocate(md%reg(md%nreg))
      !
      if (allocated(i4wk1d)) deallocate(i4wk1d)
      allocate(i4wk1d(md%nreg))
      do ireg = 1, md%nreg
        i4wk1d(ireg) = 0
      end do
      do i = 1, md%ncat
        ireg = md%regid(i)
        i4wk1d(ireg) = i4wk1d(ireg) + 1
      end do
      !
      do ireg = 1, md%nreg
        mr => md%reg(ireg)
        mr%ncat = i4wk1d(ireg)
        allocate(mr%lcatid(mr%ncat))
        allocate(mr%gcatid(mr%ncat))
        allocate(mr%catintf(mr%ncat))
        mr%ncat = 0
        do i = 1, md%ncat
          if (md%regid(i) == ireg) then
            mr%ncat = mr%ncat + 1
            mr%lcatid(mr%ncat) = md%lcatid(i)
            mr%gcatid(mr%ncat) = catgid(md%lcatid(i))
          end if
        end do
      end do
      !
      deallocate(ia,ja)
    end do
  end do
  !
  ! set the bounding boxes and init the MODFLOW model
  do i = 1, nsol
    s => sol(i)
    if (s%iact == 0) cycle
    do j = 1, s%nmod
      md => s%mod(j); mmd => smod(md%lmodid)
      allocate(mmd%isol); mmd%isol = i
      mmd%imod = md%gmodid
      mmd%write_budget = .false.
      allocate(mmd%modelname); write(mmd%modelname,'(a,i5.5)') 'm', mmd%imod
      allocate(mmd%bb); mbb => mmd%bb
      allocate(mmd%nreg); mmd%nreg = md%nreg
      allocate(mmd%reg(mmd%nreg))
      do ireg = 1, mmd%nreg
        allocate(mmd%reg(ireg)%bb)
      end do
      !
      do k = 1, md%ncat
        lid = md%lcatid(k); ireg = md%regid(k); bb => mmd%reg(ireg)%bb
        nn = ia_cat(lid)
        p =  2*i4b + p0 + (nn-1)*i4b + 1
        read(miu(iucat),pos=p) ir0
        read(miu(iucat),pos=p+1*i4b) ir1
        read(miu(iucat),pos=p+2*i4b) ic0
        read(miu(iucat),pos=p+3*i4b) ic1
        bb%ir0 = min(bb%ir0, ir0); bb%ir1 = max(bb%ir1, ir1)
        bb%ic0 = min(bb%ic0, ic0); bb%ic1 = max(bb%ic1, ic1)
      end do
      !
      do ireg = 1, md%nreg
        bb => mmd%reg(ireg)%bb
        bb%nrow = bb%ir1 - bb%ir0 + 1
        bb%ncol = bb%ic1 - bb%ic0 + 1
        mxnrow = max(mxnrow, bb%nrow); mxncol = max(mxncol, bb%ncol)
        mbb%ir0 = min(bb%ir0,mbb%ir0); mbb%ir1 = max(bb%ir1,mbb%ir1)
        mbb%ic0 = min(bb%ic0,mbb%ic0); mbb%ic1 = max(bb%ic1,mbb%ic1)
      end do
      mbb%nrow = mbb%ir1 - mbb%ir0 + 1; mbb%ncol = mbb%ic1 - mbb%ic0 + 1
    end do
  end do
  write(*,'(2(a,i5.5),a)') 'Maximum bounding box (ncol,nrow): (',mxncol,',',mxnrow,')'
  !
  ! ==============================================================================
  ! initialize the interfaces
  ! ==============================================================================
  if (allocated(i4wk1d)) deallocate(i4wk1d)
  allocate(i4wk1d(mxlid),i4wk1d2(mxlid))
  do i = 1, mxlid
    i4wk1d(i) = 0
  end do
  do isol = 1, nsol
    s => sol(isol)
    if (s%iact == 0) cycle
    do ipart = 1, s%nmod
      md => s%mod(ipart)
      do i = 1, md%ncat
        lid = md%lcatid(i)
        i4wk1d(lid) = md%lmodid ! label the catchments with model ID
        i4wk1d2(lid) = md%regid(i) ! label the catchments with model region ID
      end do
    end do
  end do
  !
  do isol = 1, nsol
    s => sol(isol)
    if (s%iact == 0) cycle
    do ipart = 1, s%nmod
      md => s%mod(ipart); mmd => smod(md%lmodid); mbb => mmd%bb
      do ireg = 1, md%nreg
        mr => md%reg(ireg)
        do i = 1, mr%ncat
          lid1 = mr%lcatid(i); gid1 =  mr%gcatid(i)
          ! check
          if (i4wk1d(lid1) /= md%lmodid) then
            call errmsg('Program error: non matching catchment ID.')
          end if
          n = (ia_cat(lid1+1) - ia_cat(lid1) - 7)/4; nn = ia_cat(lid1)
          nb = 0
          do j = 2, n
            p = 2*i4b + p0 + (nn-1)*i4b + 7*i4b + 1 + (j-1)*i4b
            read(miu(iucat),pos=p) lid2
            if (i4wk1d(lid2) /= md%lmodid) then
              nb = nb + 1
            end if
          end do
          if (nn > 0) then
            intf => mr%catintf(i)
            intf%nintf = nb
            allocate(intf%intf(nb))
            nb = 0
            do j = 2, n
              p = 2*i4b + p0 + (nn-1)*i4b + 7*i4b + 1 + (j-1)*i4b
              read(miu(iucat),pos=p) lid2
              if (i4wk1d(lid2) /= md%lmodid) then
                nb = nb + 1
                nodintf => intf%intf(nb)
                nodintf%nb_lcatid = lid2
                nodintf%nb_gcatid = catgid(lid2)
               ! if ((gid1 == 1676128).and.(nodintf%nb_gcatid==1676916))then
               !   write(*,*) 'Break'
               ! end if
                
                nodintf%nb_modid = i4wk1d(lid2)
                !
                read(miu(iucat),pos=p+  n*i4b) nodintf%n
                allocate(nodintf%my_ireg(nodintf%n))
                allocate(nodintf%my_gicir(2,nodintf%n))
                allocate(nodintf%my_nlay(nodintf%n))
                allocate(nodintf%nb_ireg(nodintf%n))
                allocate(nodintf%nb_gicir(2,nodintf%n))
                allocate(nodintf%nb_nlay(nodintf%n))
                read(miu(iucat),pos=p+3*n*i4b) ppr
                !pp = 1 + (ppr-1)*6*i4b
                pp8 = 1 + (int(ppr,i8b)-1)*6*i4b
                do k = 1, nodintf%n
                  !read(miu(iuintf),pos=pp) ic0, ir0, nlay0, &
                  !                         ic1, ir1, nlay1
                  read(miu(iuintf),pos=pp8) ic0, ir0, nlay0, &
                                           ic1, ir1, nlay1
                  !
                  ! first check
                  if ((ic0 < mbb%ic0).or.(ir0 < mbb%ir0)) then
                    call errmsg('Program error: ic/ir out of bound for init interfaces.')
                  end if
                  nodintf%my_ireg(k) = ireg
                  nodintf%nb_ireg(k) = i4wk1d2(lid2)
                  nodintf%my_gicir(1,k) = ic0;   nodintf%my_gicir(2,k) = ir0
                  nodintf%nb_gicir(1,k) = ic1;   nodintf%nb_gicir(2,k) = ir1
                  nodintf%my_nlay(k)    = nlay0; nodintf%nb_nlay(k)    = nlay1
                  !pp = pp + 6*i4b
                  pp8 = pp8 + 6*i4b
                end do
              end if
            end do
          end if
        end do
      end do
    end do
  end do
  !
  ! set directory structures
  f = trim(out_dir)//'models'
  call create_dir(f,.true.)
  do i = 1, nsol
    s => sol(i)
    if (s%iact == 0) cycle
    do j = 1, s%nmod
      md => s%mod(j); mmd => smod(md%lmodid)
      allocate(mmd%rootdir, mmd%bindir)
      !mmd%rootdir = trim(out_dir)//'models\'//trim(mmd%modelname)//'\'
      mmd%rootdir = '..\..\models\run_input\'//trim(mmd%modelname)//'\'
      call swap_slash(mmd%rootdir)
      !mmd%bindir = trim(mmd%rootdir)//'bin\'; call swap_slash(mmd%bindir)
      mmd%bindir = trim(mmd%rootdir); call swap_slash(mmd%bindir)
    end do
  end do
  !
  call raw%init(mod_inp)
  f = raw%getc('tile_bb'); call chkexist(f)
  call open_file(f, iu)
  allocate(ntile)
  read(iu,*) ntile
  allocate(tilebb(ntile))
  do i = 1, ntile
    bb => tilebb(i)
    read(iu,*) bb%ic0, bb%ic1, bb%ir0, bb%ir1
    bb%ncol = bb%ic1 - bb%ic0 + 1
    bb%nrow = bb%ir1 - bb%ir0 + 1
  end do
  close(iu)
  
  ! initialize for the interfaces
  if (allocated(i4wk1d)) deallocate(i4wk1d)
  allocate(i4wk1d(nmod))
  do isol = 1, nsol ! BEGIN loop over all solutions
    s => sol(isol)
    if (s%iact == 0) cycle
    do ipart = 1, s%nmod ! BEGIN loop over models
      do i = 1, nmod
        i4wk1d(i) = 0
      end do
      !
      md => s%mod(ipart); mmd => smod(md%lmodid)
      !
      ! count the models for exchanging data and allocate
      allocate(mmd%nxch); mmd%nxch = 0
      do ireg = 1, md%nreg
        mr => md%reg(ireg)
        do i = 1, mr%ncat
          intf => mr%catintf(i)
          do j = 1, intf%nintf
            nodintf => intf%intf(j)
            i4wk1d(nodintf%nb_modid) = 1
          end do
        end do
      end do !ireg
      !
      !create map model ID -> interface index
      mmd%nxch = 0
      do i = 1, nmod
        if (i4wk1d(i) == 1) then
          mmd%nxch = mmd%nxch + 1
          i4wk1d(i) = mmd%nxch
        end if
      end do
      if (mmd%nxch > 0) then
        allocate(mmd%xch(mmd%nxch))
      end if
      !
      do i = 1, nmod
        ixch = i4wk1d(i)
        if (ixch /= 0) then
          xch => mmd%xch(ixch)
          allocate(xch%m1mod, xch%m2mod, xch%m2modelname, xch%nexg)
          xch%m1mod = md%lmodid
          xch%m2mod = i
          xch%m2modelname = smod(i)%modelname
          xch%nexg  = 0
        end if
      end do
      !
      ! count exchange node
      do iact = 1, 2
        do ireg = 1, md%nreg
          mr => md%reg(ireg)
          do i = 1, mr%ncat
            intf => mr%catintf(i)
            do j = 1, intf%nintf
              nodintf => intf%intf(j)
              ixch = i4wk1d(nodintf%nb_modid)
              xch => mmd%xch(ixch)
              do k = 1, nodintf%n
                ic0 = nodintf%my_gicir(1,k); ir0 = nodintf%my_gicir(2,k); nlay0 = nodintf%my_nlay(k)
                ic1 = nodintf%nb_gicir(1,k); ir1 = nodintf%nb_gicir(2,k); nlay1 = nodintf%nb_nlay(k)
                if (iact == 2) then
                  xch%bb%ic0 = min(xch%bb%ic0,ic0); xch%bb%ic1 = max(xch%bb%ic1,ic0)
                  xch%bb%ic0 = min(xch%bb%ic0,ic1); xch%bb%ic1 = max(xch%bb%ic1,ic1)
                  xch%bb%ir0 = min(xch%bb%ir0,ir0); xch%bb%ir1 = max(xch%bb%ir1,ir0)
                  xch%bb%ir0 = min(xch%bb%ir0,ir1); xch%bb%ir1 = max(xch%bb%ir1,ir1)
                  xch%bb%ncol = xch%bb%ic1-xch%bb%ic0+1; xch%bb%nrow = xch%bb%ir1-xch%bb%ir0+1
                end if
                if ((nlay0 == 1) .or. (nlay1 == 1)) then
                  il0 = gnlay; il1 = gnlay
                  xch%nexg = xch%nexg + 1
                  if (iact == 2) then
                    xch%m1reg(xch%nexg) = nodintf%my_ireg(k)
                    xch%m2reg(xch%nexg) = nodintf%nb_ireg(k)
                    xch%gicirilm1(1,xch%nexg) = ic0
                    xch%gicirilm1(2,xch%nexg) = ir0
                    xch%gicirilm1(3,xch%nexg) = il0
                    xch%gicirilm2(1,xch%nexg) = ic1
                    xch%gicirilm2(2,xch%nexg) = ir1
                    xch%gicirilm2(3,xch%nexg) = il1
                  end if
                else if ((nlay0 == gnlay) .and. (nlay1 == gnlay)) then
                  do il = 1, gnlay
                    il0 = il; il1 = il
                    xch%nexg = xch%nexg + 1
                    if (iact == 2) then
                      xch%m1reg(xch%nexg) = nodintf%my_ireg(k)
                      xch%m2reg(xch%nexg) = nodintf%nb_ireg(k)
                      xch%gicirilm1(1,xch%nexg) = ic0
                      xch%gicirilm1(2,xch%nexg) = ir0
                      xch%gicirilm1(3,xch%nexg) = il0
                      xch%gicirilm2(1,xch%nexg) = ic1
                      xch%gicirilm2(2,xch%nexg) = ir1
                      xch%gicirilm2(3,xch%nexg) = il1
                    end if
                  end do
                else
                  call errmsg('Program error: invalid layer.')
                end if
              end do
            end do
          end do
        end do
        if (iact == 1) then
          do ixch = 1, mmd%nxch
            xch => mmd%xch(ixch)
            if (xch%nexg == 0) then
              call errmsg('Program error: nexg = 0.')
            end if
            allocate(xch%bb)
            allocate(xch%m1reg(xch%nexg), xch%m2reg(xch%nexg))
            allocate(xch%gicirilm1(3,xch%nexg), xch%gicirilm2(3,xch%nexg))
            xch%nexg = 0
          end do 
        end if
      end do !iact
    end do !ipart
  end do !isol
  !
  ! write the MODFLOW models and store exchange node numbers
  !
  ! create empty output directories
  d = trim(out_dir)//'log'; call create_dir(d,.true.);
  d = trim(out_dir)//'models\run_output_lst'; call create_dir(d,.true.);
  d = trim(out_dir)//'models\run_output_bin'; call create_dir(d,.true.);
  d = trim(out_dir)//'solutions\run_output'; call create_dir(d,.true.);
  d = trim(out_dir)//'solutions\post_mappings'; call create_dir(d,.true.);
  !
  ! change the cwd to directory with mfsim nam files.
  d = trim(out_dir)//'solutions\run_input'
  call create_dir(d,.true.); call change_work_dir(d)
  !
  do isol = 1, nsol ! BEGIN loop over all solutions
    s => sol(isol)
    if (s%iact == 0) cycle
    
    ! Init writing the model ID
    if (s%iwrite_modid == 1) then
      ! deterime solutlion bounding box
      if (.not.associated(sbb)) then
         allocate(sbb)
      end if
      do ipart = 1, s%nmod ! BEGIN loop over models
        md => s%mod(ipart); modid = md%lmodid; mmd => smod(modid)
        do ireg = 1, md%nreg
          r => mmd%reg(ireg); bb => r%bb
          sbb%ic0 = min(sbb%ic0, bb%ic0)
          sbb%ic1 = max(sbb%ic1, bb%ic1)
          sbb%ir0 = min(sbb%ir0, bb%ir0)
          sbb%ir1 = max(sbb%ir1, bb%ir1)
        end do
      end do
      sbb%ncol= sbb%ic1 - sbb%ic0 + 1
      sbb%nrow= sbb%ir1 - sbb%ir0 + 1
      if (allocated(solmodid)) deallocate(solmodid)
      allocate(solmodid(sbb%ncol,sbb%nrow))
      do ir = 1, sbb%nrow
        do ic = 1, sbb%ncol
         solmodid(ic,ir) = 0
        end do
      end do
    end if 
    !
    do ipart = 1, s%nmod ! BEGIN loop over models
      md => s%mod(ipart); modid = md%lmodid; mmd => smod(modid)
      !
      call create_dir(mmd%rootdir,.true.)
      call create_dir(mmd%bindir,.true.)
      !
      ! allocate and initialize
      do ireg = 1, md%nreg
        mr => md%reg(ireg) !debug
        r => mmd%reg(ireg); bb => r%bb
        allocate(r%itile(bb%ncol,bb%nrow))
        allocate(r%nodmap(bb%ncol,bb%nrow,gnlay))
        allocate(r%bndmap(bb%ncol,bb%nrow,gnlay))
        do ir = 1, bb%nrow
          do ic = 1, bb%ncol
            r%itile(ic,ir) = 0
          end do
        end do
        do il = 1, gnlay
          do ir = 1, bb%nrow
            do ic = 1, bb%ncol
              r%nodmap(ic,ir,il) = 0
              r%bndmap(ic,ir,il) = 0
            end do
          end do
        end do
      end do
      !
      do k = 1, md%ncat ! BEGIN loop over catchments
        ireg = md%regid(k); r => mmd%reg(ireg); bb => r%bb ! get the region
        lid = md%lcatid(k)
        n = (ia_cat(lid+1) - ia_cat(lid) - 7)/4; nn = ia_cat(lid)
        p = 2*i4b + p0 + (nn-1)*i4b + 7*i4b + n*i4b + 1
        read(miu(iucat),pos=p) ncell
        read(miu(iucat),pos=p+2*n*i4b) pp
        ! set the nodal pointers
        pnod  = 2*(pp-1)*i4b + 1
        ptil  =   (pp-1)*i2b + 1
        pnlay =   (pp-1)*i1b + 1
        do kk = 1, ncell ! BEGIN loop over catchments cells
          ! read the nodal data
          read(miu(iunod), pos=pnod)  ic;    pnod  = pnod  + i4b
          read(miu(iunod), pos=pnod)  ir;    pnod  = pnod  + i4b
          read(miu(iutil), pos=ptil)  itile; ptil  = ptil  + i2b
          read(miu(iunlay),pos=pnlay) nlay;  pnlay = pnlay + i1b
          !
          jc = abs(ic) - bb%ic0 + 1; jr = ir - bb%ir0 + 1
          if ((jc < 1).or.(jc > bb%ncol).or.(jr < 1).or.(jr > bb%nrow)) then
            call errmsg('Program error: jc/jc out of bound.')
          end if
          !
          nodsign = 1
          if (ic < 0) then
            nodsign = -1 ! constant head boundary
          end if
          !nodsign = catgid(lid) !DEBUG
          if (nlay == 2) then
            r%nodmap(jc,jr,1) = nodsign
            r%nodmap(jc,jr,2) = nodsign
          else if (nlay == 1) then
            r%nodmap(jc,jr,gnlay) = nodsign
          else
            call errmsg('Invalid number of layers read.')
          end if
          ! 
          r%itile(jc,jr) = itile
        end do ! END loop over catchments cells
      end do ! END loop over catchments
      !
      ! Set the tile number for this model
      if (allocated(i4wk1d)) deallocate(i4wk1d)
      allocate(i4wk1d(ntile))
      do i = 1, ntile
        i4wk1d(i) = 0
      end do
      do ireg = 1, md%nreg
        r => mmd%reg(ireg); bb => r%bb
        do ir = 1, bb%nrow
          do ic = 1, bb%ncol
            i2v = abs(r%itile(ic,ir))
            if (i2v /= 0) then
              i4wk1d(i2v) = 1
            end if
          end do
        end do
      end do
      allocate(mmd%ntile)
      mmd%ntile = sum(i4wk1d)
      if (mmd%ntile > 0) then
        allocate(mmd%itile(mmd%ntile))
        j = 0
        do i = 1, ntile
          if (i4wk1d(i) == 1) then
            j = j + 1
            mmd%itile(j) = int(i,i2b)
          end if
        end do
      end if
      !
      ! reorder the nodes
      allocate(mmd%layer_nodes(gnlay)); mmd%layer_nodes = 0
      do ireg = 1, md%nreg
        r => mmd%reg(ireg)
        allocate(r%layer_nodes(gnlay)); r%layer_nodes = 0
      end do
      n = 0
      do il = 1, gnlay
        nn = 0
        do ireg = 1, md%nreg
          r => mmd%reg(ireg); bb => r%bb
          do ir = 1, bb%nrow
            do ic = 1, bb%ncol
              i4v = r%nodmap(ic,ir,il)
              if (i4v /= 0) then
                r%layer_nodes(il) = r%layer_nodes(il) + 1
                n = n + 1; nn = nn + 1
                if (i4v > 0) then
                  r%nodmap(ic,ir,il) = n
                else
                  r%nodmap(ic,ir,il) = -n
                end if
              end if
            end do
          end do ! icol
        end do ! irow
        mmd%layer_nodes(il) = nn
      end do ! ilay
      !
      ! check if all vertical nodes are constant head
      do il = 1, gnlay
        nn = 0
        do ireg = 1, md%nreg
          r => mmd%reg(ireg); bb => r%bb
          allocate(i4wk2d(bb%ncol,bb%nrow))
          do ir = 1, bb%nrow
            do ic = 1, bb%ncol
             i4wk2d(ic,ir) = 0
            end do
          end do
          do ir = 1, bb%nrow
            do ic = 1, bb%ncol
              i4v = r%nodmap(ic,ir,il)
              if (i4v /= 0) then
                if ((i4wk2d(ic,ir) == 0).and.(i4v < 0)) then
                  i4wk2d(ic,ir) = 1 
                end if
                if ((i4v > 0).and.(i4wk2d(ic,ir)==1)) then
                  call errmsg('Program error: vertical node.')
                end if
              end if
            end do
        end do
        deallocate(i4wk2d)
        end do
      end do
      !
      ! store model ID
      if (s%iwrite_modid == 1) then
        do il = 1, gnlay
          do ireg = 1, md%nreg
            r => mmd%reg(ireg); bb => r%bb
            do ir = 1, bb%nrow
              do ic = 1, bb%ncol
                i4v = r%nodmap(ic,ir,il)
                if (i4v /= 0) then
                  jc = ic + bb%ic0 - 1; jr = ir + bb%ir0 - 1
                  kc = jc - sbb%ic0 + 1; kr = jr - sbb%ir0 + 1
                  solmodid(kc,kr) = md%gmodid
                end if
              end do
            end do
          end do
        end do
      end if
      !
      if (.false.) then
        !if (s%lmm) then
          do ireg = 1, md%nreg
            r => mmd%reg(ireg); bb => r%bb
            xmin = gxmin + (bb%ic0-1)*gcs
            ymin = gymin + (gnrow-bb%ir1)*gcs
            do il = 1, gnlay
              write(f,'(2a,i3.3,a,i1,a)') trim(mmd%modelname), &
                '_r', ireg, '_l', il, '.idf'
              call writeidf(f, r%nodmap(:,:,il), bb%ncol, bb%nrow, &
                dble(xmin), dble(ymin), dble(gcs), DZERO)
            end do
          end do
        !end if
      end if
      !
      ! Set internal local node numbers for the interfaces
      do ixch = 1, mmd%nxch
        xch => mmd%xch(ixch)
        allocate(xch%cellidm1(xch%nexg))
        do i = 1, xch%nexg
          ireg = xch%m1reg(i); r => mmd%reg(ireg); bb => r%bb
          ic = xch%gicirilm1(1,i) - bb%ic0 + 1
          ir = xch%gicirilm1(2,i) - bb%ir0 + 1
          if ((ic < 1).or.(ic > bb%ncol).or.&
              (ir < 1).or.(ir > bb%nrow)) then
            call errmsg('Program error: ic/ir for M1 out of bound.')
          end if
          il = xch%gicirilm1(3,i)
          n = abs(r%nodmap(ic,ir,il))
          if (n == 0) then
            call errmsg('Program error: no node number found for M1.')
          end if
          xch%cellidm1(i) = n
        end do
        mmd2 => smod(xch%m2mod)
        do jxch = 1, mmd2%nxch
          xch2 => mmd2%xch(jxch)
          if (xch2%m2mod == modid) then
            allocate(xch2%cellidm2(xch2%nexg))
            do i = 1, xch2%nexg
              ireg = xch2%m2reg(i); r => mmd%reg(ireg); bb => r%bb
              ic = xch2%gicirilm2(1,i) - bb%ic0 + 1
              ir = xch2%gicirilm2(2,i) - bb%ir0 + 1
              if ((ic < 1).or.(ic > bb%ncol).or.&
                  (ir < 1).or.(ir > bb%nrow)) then
                call errmsg('Program error: ic/ir out of bound.')
              end if
              il = xch2%gicirilm2(3,i)
              n = abs(r%nodmap(ic,ir,il))
              if (n == 0) then
                call errmsg('Program error: node not found.')
              end if
              xch2%cellidm2(i) = n
            end do
          end if
        end do
      end do !ixch
      !
      ! set the CHD interior boundary by labeling M1
      do ixch = 1, mmd%nxch
        xch => mmd%xch(ixch)
        do i = 1, xch%nexg
          ireg = xch%m1reg(i); r => mmd%reg(ireg); bb => r%bb
          ic = xch%gicirilm1(1,i); ir = xch%gicirilm1(2,i); il = xch%gicirilm1(3,i)
          jc = ic - bb%ic0 + 1; jr = ir - bb%ir0 + 1
          ! check 1
          if ((jc < 1).or.(jc > bb%ncol).or.(jr < 1).or.(jr > bb%nrow)) then
            call errmsg('Program error: jc/jr out of bound for bndmap.')
          end if
          n = r%nodmap(jc,jr,il); nn = xch%cellidm1(i)
          ! check 2
          if (abs(n) /= nn) then
            call errmsg('Program error: non-matching node number for bndmap')
          end if
          r%bndmap(jc,jr,il) = n
        end do
      end do
      !
      ! write the model TODO
      if ((lwmod).and.(modid0 <= md%gmodid).and.(md%gmodid <= modid1)) then
        call mmd%write()
        call mmd%write_post_map(s%iwrite_modid)
      end if
      !
      ! clean up regions
      call mmd%clean_regions()
      !
    end do ! END loop over all models
    !
    if (s%iwrite_modid == 1) then
      call logmsg('**************************************************************')
      call logmsg('***** Writing model IDs... *****')
      call logmsg('**************************************************************')
      f = '..\post_mappings\s'//ta((/isol/),'(i2.2)')//'.modmap'
      call swap_slash(f)
      xmin = gxmin + (sbb%ic0-1)*gcs
      ymin = gymin + (gnrow-sbb%ir1)*gcs
      call writeflt(f, solmodid, sbb%ncol, sbb%nrow, &
        dble(xmin), dble(ymin), dble(gcs), 0)
      deallocate(solmodid)
      sbb => null()
    end if
  end do ! BEGIN loop over all solutions
  !
  if (.not.lwsol) then
    ! close the mapping-files
    do i = 1, size(miu)
      if (miu(i) > 0) close(miu(i))
    end do
    stop 0
  end if
  !
  call logmsg('**************************************************************')
  call logmsg('***** Writing exchange files... *****')
  call logmsg('**************************************************************')
  !
  ! determine the symmetric interfaces
  do isol = 1, nsol ! BEGIN loop over all solutions
    s => sol(isol)
    if (s%iact == 0) cycle
    do ipart = 1, s%nmod ! BEGIN loop over models
      md => s%mod(ipart); modid = md%lmodid; mmd => smod(modid)
      do ixch = 1, mmd%nxch
        xch => mmd%xch(ixch)
        if (.not.xch%loutput) cycle
        mmd2 => smod(xch%m2mod)
        do jxch = 1, mmd2%nxch
          xch2 => mmd2%xch(jxch)
          if (xch2%m2mod == modid) then
            xch2%loutput = .false.
          end if
        end do
      end do
    end do !ipart
  end do !isol
  !
  ! count the total number of interface
  do isol = 1, nsol ! BEGIN loop over all solutions
    s => sol(isol)
    if (s%iact == 0) cycle
    nintf = 0
    do ipart = 1, s%nmod ! BEGIN loop over models
      md => s%mod(ipart); modid = md%lmodid; mmd => smod(modid)
      do ixch = 1, mmd%nxch
        xch => mmd%xch(ixch)
        if (.not.xch%loutput) cycle
        nintf = nintf + 1
        !
        if (.false.) then !!! DEBUG OUTPUT !!
          if (allocated(i4wk3d)) deallocate(i4wk3d)
          allocate(i4wk3d(xch%bb%ncol,xch%bb%nrow, gnlay))
          do il = 1, gnlay
            do ir = 1, xch%bb%nrow
              do ic = 1, xch%bb%ncol
                i4wk3d(ic,ir,il) = 0
              end do
            end do
          end do
          do i = 1, xch%nexg
            ! M1
            ic = xch%gicirilm1(1,i); ir = xch%gicirilm1(2,i); il = xch%gicirilm1(3,i)
            jc = ic - xch%bb%ic0 + 1; jr = ir - xch%bb%ir0 + 1
            i4wk3d(jc,jr,il) = xch%cellidm1(i)
            ! M2
            ic = xch%gicirilm2(1,i); ir = xch%gicirilm2(2,i); il = xch%gicirilm2(3,i)
            jc = ic - xch%bb%ic0 + 1; jr = ir - xch%bb%ir0 + 1
            i4wk3d(jc,jr,il) = xch%cellidm2(i)
          end do
          !
          xmin = gxmin + (xch%bb%ic0-1)*gcs
          ymin = gymin + (gnrow-xch%bb%ir1)*gcs
          mmd2 => smod(xch%m2mod)
          do il = 1, gnlay
            write(f,'(5a,i1,a)') 'xch_',trim(mmd%modelname),'-',&
              trim(mmd2%modelname),'_l', il, '.idf'
            call writeidf(f, i4wk3d(:,:,il), xch%bb%ncol, xch%bb%nrow, &
              dble(xmin), dble(ymin), dble(gcs), DZERO)
          end do
        end if 
        !
      end do
    end do !ipart
    write(*,'(a,i2.2,a,i3.3)') 'Solution ',isol,' # output interface: ',nintf
  end do !isol
  !
  !
  if (.false.) then !!! DEBUG OUTPUT !!!
    s => sol(1)
    do i = 1, s%nmod ! loop over models
      md => s%mod(i); mmd => smod(md%lmodid); mbb => mmd%bb
      nc = mbb%ncol; nr = mbb%nrow
      allocate(r4wk3d(nc,nr,gnlay))
      do il = 1, gnlay
        do ir = 1, nr
          do ic = 1, nc
            r4wk3d(ic,ir,il) = 0.
          end do
        end do
      end do
      xmin = gxmin + (mbb%ic0-1)*gcs
      ymin = gymin + (gnrow-mbb%ir1)*gcs
      do ireg = 1, mmd%nreg
        r => mmd%reg(ireg); bb => r%bb
        do ir = 1, bb%nrow
          do ic = 1, bb%ncol
            jr = ir + bb%ir0  - 1; jc = ic + bb%ic0 - 1
            kr = jr - mbb%ir0 + 1; kc = jc - mbb%ic0 + 1
            r4wk3d(kc,kr,1) = real(r%nodmap(ic,ir,1),r4b)
            r4wk3d(kc,kr,2) = real(r%nodmap(ic,ir,2),r4b)
          end do
        end do
      end do
      write(f,'(a,i2.2,a)') 'mod_',i,'_l1.idf'
      call writeidf(f, r4wk3d(:,:,1), nc, nr, dble(xmin), dble(ymin), dble(gcs), DZERO)
      write(f,'(a,i2.2,a)') 'mod_',i,'_l2.idf'
      call writeidf(f, r4wk3d(:,:,2), nc, nr, dble(xmin), dble(ymin), dble(gcs), DZERO)
      deallocate(r4wk3d)
    end do
    stop
  end if
  !
  ! check if the model is multi-model
  if (llastsol) then
    isol = nsol
    s => sol(isol)
    s%lmm = .false.
    if (s%iact /= 0) then
      do i = 1, s%nmod ! loop over models
        md => s%mod(i); mmd => smod(md%lmodid)
        if (mmd%nxch > 0) then
          s%lmm = .true.
          exit
        end if
      end do
    end if
  end if
  !
  ! write the exchange files for all models  
  do isol = 1, nsol
    s => sol(isol)
    if (s%iact == 0) cycle
    if (.not.s%lmm) cycle
    write(f,'(a,i2.2,a)') 's', isol, '.exchanges.asc'
    call open_file(f, iu, 'w')
    do i = 1, s%nmod ! loop over models
      md => s%mod(i); mmd => smod(md%lmodid)
      call mmd%write_exchanges(iu)
    end do
    close(iu)
  end do !isol
  !
  ! write the solutions
  do isol = 1, nsol ! BEGIN loop over all solutions
    call logmsg('**************************************************************')
    call logmsg('***** Writing solution '//ta((/isol/))//'... *****')
    call logmsg('**************************************************************')
    s => sol(isol)
    if (s%iact == 0) cycle
    allocate(ms)
    ms%isol = isol
    allocate(ms%solname); write(ms%solname,'(a,i2.2)') 's', isol
    allocate(ms%lmm); ms%lmm = s%lmm
    allocate(ms%nmod); ms%nmod = s%nmod
    allocate(ms%mod_id(ms%nmod), ms%mod_part(ms%nmod))
    allocate(ms%npart); ms%npart = s%np 
    allocate(ms%mod_part(ms%nmod))
    do i = 1, s%nmod
      ms%mod_id(i) = s%mod(i)%gmodid
      ms%mod_part(i) = 0
      if (ms%npart == 1) ms%mod_part(i) = 1
      if (ms%npart == s%nmod) ms%mod_part(i) = i
    end do
    !
    ! METIS
    if ((ms%npart > 1).and.(ms%npart < ms%nmod)) then
      call logmsg('Solution: '//ta((/isol/))//' dividing '//ta((/ms%nmod/))//&
        ' models into '//ta((/ms%npart/))//' parts...')
      m => solmet(isol)
      !
      allocate(mm)
      allocate(mm%nparts); mm%nparts = ms%npart
      allocate(mm%nvtxs); mm%nvtxs = ms%nmod
      allocate(mm%vwgt(mm%nvtxs), mm%vsize(mm%nvtxs)); mm%vsize = 1
      allocate(mm%part(mm%nvtxs)); mm%part = 0
      allocate(mm%xadj(mm%nvtxs+1))
      !
      nja = 0
      do i = 1, s%nmod
        md => s%mod(i); mmd => smod(md%lmodid)
        mm%vwgt(i) = int(md%ncell,i8b)
        nja = nja + mmd%nxch
      end do
      !
      allocate(mm%adjncy(nja), mm%adjwgt(nja), mm%ncon); mm%ncon = 1
      allocate(mm%tpwgts(mm%nparts*m%ncon)); mm%tpwgts = 1./real(mm%nparts)
      allocate(mm%ubvec(mm%ncon)); mm%ubvec(1) = tolimbal
      !
      mm%xadj(1) = 0; nja = 0
      do i = 1, s%nmod
        md => s%mod(i); mmd => smod(md%lmodid)
        mm%xadj(i+1) = mm%xadj(i)
        do j = 1, mmd%nxch
          nja = nja + 1
          mm%adjncy(nja) = int(mmd%xch(j)%m2mod - 1,i8b) ! convert to local number
          mm%adjwgt(nja) = int(mmd%xch(j)%nexg,i8b)
          mm%xadj(i+1) = mm%xadj(i+1) + 1
        end do
      end do
      !
      call mm%set_opts()
      call mm%recur()
      !
      do i = 1, s%nmod
        ipart = int(mm%part(i),i4b)
        ms%mod_part(i) = ipart + 1
      end do
      !
      ! clean up:
      call mm%clean(); deallocate(mm); mm => null()
    end if
    !
    call ms%write()
    call ms%clean()
    deallocate(ms)
  end do
  !
  ! close the mapping-files
  do i = 1, size(miu)
    if (miu(i) > 0) close(miu(i))
  end do
  
end program

