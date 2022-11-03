module metis_module
  ! -- modules
  use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, &
    i4b => int32, i8b => int64, r4b => real32, r8b => real64
  use metis_enum
  
  implicit none
  
  private
  
  integer(i4b), parameter :: mxslen = 1024
  
  real(r4b), parameter :: tolimbal = 1.00001  ! load imbalance tolerance
  public :: tolimbal
  logical :: lump = .false.
  logical :: lump_indep = .false.
  real(r8b), parameter :: DZERO = 0.D0
  
  type tMetis
    ! locals (METIS arguments, see manual p. 26)
    !integer(kind=i4b),               pointer :: nvtxs  => null() ! number of vertices in the graph
    !integer(kind=i4b),               pointer :: ncon   => null() ! number of balancing constraints
    !integer(kind=i4b), dimension(:), pointer :: xadj   => null() ! adjacency structure of the graph
    !integer(kind=i4b), dimension(:), pointer :: adjncy => null() ! adjacency structure of the graph
    !integer(kind=i4b), dimension(:), pointer :: vwgt   => null() ! the weights of the vertices (dim: nvtxs)
    !integer(kind=i4b), dimension(:), pointer :: vsize  => null() ! size of vertices for computing the total communication volume (dim: nvtxs)
    !integer(kind=i4b), dimension(:), pointer :: adjwgt => null() ! weights of the edges
    !integer(kind=i4b),               pointer :: nparts => null() ! the number of partitions in the graph
    !real(kind=r4b)   , dimension(:), pointer :: tpwgts => null() ! desired weight for each partition and constraint (dim: nparts*ncon)
    !real(kind=r4b)   , dimension(:), pointer :: ubvec  => null() ! allowed load imbalance for each constraint (dim: ncon)
    !integer(kind=i4b), dimension(:), pointer :: opts   => null() ! option array (options in METIS manual, length METIS_NOPTIONS)
    !integer(kind=i4b),               pointer :: objval => null() ! edge-cut ot total communication volume
    !integer(kind=i4b), dimension(:), pointer :: part   => null() ! partition vector of the graph (dim: nvtxs)
    
    integer(kind=i8b),               pointer :: nvtxs  => null() ! number of vertices in the graph
    integer(kind=i8b),               pointer :: ncon   => null() ! number of balancing constraints
    integer(kind=i8b), dimension(:), pointer :: xadj   => null() ! adjacency structure of the graph
    integer(kind=i8b), dimension(:), pointer :: adjncy => null() ! adjacency structure of the graph
    integer(kind=i8b), dimension(:), pointer :: vwgt   => null() ! the weights of the vertices (dim: nvtxs)
    integer(kind=i8b), dimension(:), pointer :: vsize  => null() ! size of vertices for computing the total communication volume (dim: nvtxs)
    integer(kind=i8b), dimension(:), pointer :: adjwgt => null() ! weights of the edges
    integer(kind=i8b),               pointer :: nparts => null() ! the number of partitions in the graph
    real(kind=r4b)   , dimension(:), pointer :: tpwgts => null() ! desired weight for each partition and constraint (dim: nparts*ncon)
    real(kind=r4b)   , dimension(:), pointer :: ubvec  => null() ! allowed load imbalance for each constraint (dim: ncon)
    integer(kind=i8b), dimension(:), pointer :: opts   => null() ! option array (options in METIS manual, length METIS_NOPTIONS)
    integer(kind=i8b),               pointer :: objval => null() ! edge-cut ot total communication volume
    integer(kind=i8b), dimension(:), pointer :: part   => null() ! partition vector of the graph (dim: nvtxs)
    !
    real(r8b), pointer :: totload => null()
    real(r8b), pointer :: imbal   => null()
    !
    integer(i4b), dimension(:), pointer :: idmap    => null()
    integer(i4b), dimension(:), pointer :: idmapinv => null()
    
  contains
    procedure :: init            => metis_init
    procedure :: init_lump       => metis_init_lump
    procedure :: init_lump_indep => metis_init_lump_indep
    procedure :: set_opts        => metis_set_opts
    procedure :: recur           => metis_recursive
    procedure :: kway            => metis_kway
    procedure :: clean           => metis_clean
    procedure :: calc_imbal      => metis_calc_imbal
    procedure :: check_empty     => metis_check_empty
    procedure :: set_ids         => metis_set_ids
    procedure :: graph_stat      => metis_graph_stat
  end type tMetis
  
  public :: tMetis
  
  save
  
contains
  
  subroutine metis_init(this, load, nparts)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMetis) :: this
    integer(i4b), dimension(:,:), intent(in) :: load
    integer(i4b), intent(in) :: nparts
    ! -- local
    integer(i4b), parameter :: inorth = 1, isouth = 2, iwest = 3, ieast = 4
    !
    integer(i4b) :: nc, nr, ic, ir, nja, n, m, i
    integer(i4b), dimension(:,:), allocatable :: iwrk
    integer(i4b), dimension(3,4) :: inbr
! ------------------------------------------------------------------------------
  
    allocate(this%nparts)
    this%nparts = nparts
    !
    nc = size(load,1); nr = size(load,2)
    !
    allocate(this%nvtxs)
    this%nvtxs = 0
    do ir = 1, nr
      do ic = 1, nc
        if (load(ic,ir) > 0.) then
          this%nvtxs = this%nvtxs + 1
        end if
      end do
    end do
    !
    allocate(this%vwgt(this%nvtxs))
    allocate(this%vsize(this%nvtxs))
    allocate(this%part(this%nvtxs))
    allocate(this%xadj(this%nvtxs+1))
    nja = 4*this%nvtxs
    allocate(this%adjncy(nja))
    allocate(this%adjwgt(nja))
    this%part = 0
    !
    allocate(this%ncon)
    this%ncon = 1
    allocate(this%tpwgts(this%nparts*this%ncon))
    allocate(this%ubvec(this%ncon))
    !
    ! set vertex weights
    n = 0
    do ir = 1, nr
      do ic = 1, nc
        if (load(ic,ir) > 0) then
          n = n + 1
          this%vwgt(n) = load(ic,ir)
        endif
      end do
    end do
    !
    this%ubvec(1) = tolimbal
    this%tpwgts = 1./real(this%nparts)
    this%vsize = 1
    !
    allocate(iwrk(nc,nr))
    n = 0
    do ir = 1, nr
      do ic = 1, nc
        if (load(ic,ir) > 0.) then
          n = n + 1
          iwrk(ic,ir) = n
        else
          iwrk(ic,ir) = 0
        end if
      end do
    end do
    !
    ! fill xadj and adjncy
    this%xadj(1) = 0
    n = 1
    m = 0
    do ir = 1, nr
      do ic = 1, nc
        if (iwrk(ic,ir) /= 0) then
          inbr = 0
          ! north
          if (ir > 1) then
            inbr(1,inorth) = iwrk(ic,ir-1)
          endif
          ! south
          if (ir < nr) then
            inbr(1,isouth) = iwrk(ic,ir+1)
          endif
          ! west
          if (ic > 1) then
            inbr(1,iwest)  = iwrk(ic-1,ir)
          endif
          ! east
          if (ic < nc) then
            inbr(1,ieast) = iwrk(ic+1,ir)
          endif
          n = n + 1
          this%xadj(n) = this%xadj(n-1)
          do i = 1, 4
            if (inbr(1,i) > 0) then
              m = m + 1
              this%adjncy(m) = inbr(1,i) - 1
              this%xadj(n) = this%xadj(n) + 1
              this%adjwgt(m) = 1
            end if
          end do
        end if
      end do
    end do
    deallocate(iwrk)
     
  end subroutine metis_init
  
  subroutine metis_init_lump(this, ids, nparts)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
     ! -- dummy
    class(tMetis) :: this
    integer(i4b), dimension(:,:), intent(in) :: ids
    integer(i4b), intent(in) :: nparts
    ! -- local
    integer(i4b) :: nc, nr, ic, ir, i, j, id, maxid, nja, n, ir0, ir1, ic0, ic1
    integer(i4b) :: icw, ice, irn, irs, ivalw, ivale, ivaln, ivals
    integer(i4b), dimension(:), allocatable :: iwrk
    integer(i4b), dimension(:,:), allocatable :: iwrk2, idbb
    character(len=mxslen) :: s
! ------------------------------------------------------------------------------
    lump = .true.
    
    allocate(this%nparts)
    this%nparts = nparts
    !
    nc = size(ids,1); nr = size(ids,2)
    !
    allocate(this%nvtxs)
    this%nvtxs = 0
    !
    maxid = int(maxval(ids))
    allocate(this%idmap(maxid))
    do i = 1, maxid
      this%idmap(i) = 0
    end do
    do ir = 1, nr
      do ic = 1, nc
        i = abs(ids(ic,ir))
        if (i /= 0) then
          this%idmap(i) = 1
        end if
      end do
    end do
    this%nvtxs = sum(this%idmap)
    write(s,*) this%nvtxs
    write(*,*) 'Number of lumped nodes: '//trim(adjustl(s))
    !
    n = 0
    do i = 1, maxid
      if (this%idmap(i) == 1) then
        n = n + 1
        this%idmap(i) = n
      end if
    end do
    allocate(this%idmapinv(n))
    do i = 1, maxid
      j = this%idmap(i)
      if (j > 0) then
        this%idmapinv(j) = i
      end if
    end do
    !
    if (n == 0) then
      write(*,*) 'Error, no IDs found.'
      stop 1
    end if
    !
    allocate(this%vwgt(this%nvtxs)); this%vwgt = 0
    allocate(this%vsize(this%nvtxs))
    allocate(this%part(this%nvtxs)); this%part = 0
    allocate(this%xadj(this%nvtxs+1))
    !
    allocate(iwrk2(nc,nr))
    do ir = 1, nr
      do ic = 1, nc
        iwrk2(ic,ir) = 0
      end do
    end do
    !
    ! determine bounding box
    allocate(idbb(4,n))
    do i = 1, n
      idbb(1,i) = nr !ir0
      idbb(2,i) = 0  !ir1
      idbb(3,i) = nc !ic0
      idbb(4,i) = 0  !ic1
    end do
    !
    do ir = 1, nr
      do ic = 1, nc
        i = abs(ids(ic,ir))
        if (i /= 0) then
          j = this%idmap(i)
          idbb(1,j) = min(idbb(1,j), ir)
          idbb(2,j) = max(idbb(2,j), ir)
          idbb(3,j) = min(idbb(3,j), ic)
          idbb(4,j) = max(idbb(4,j), ic)
        end if
      end do
    end do
    !
    nja = 0
    allocate(iwrk(n))
    do i = 1, n
      id = this%idmapinv(i)
      do j = 1, n
        iwrk(j) = 0
      end do
      ir0 = idbb(1,i); ir1 = idbb(2,i)
      ic0 = idbb(3,i); ic1 = idbb(4,i)
      do ir = ir0, ir1
        do ic = ic0, ic1
          if (abs(ids(ic,ir)) == id) then
            j = this%idmap(id)
            this%vwgt(j) = this%vwgt(j) + 1
            irn = max(ir-1, 1); irs = min(ir+1, nr)
            icw = max(ic-1, 1); ice = min(ic+1, nc)
            ivaln = abs(ids(ic,irn)); ivals = abs(ids(ic,irs))
            ivalw = abs(ids(icw,ir)); ivale = abs(ids(ice,ir))
            if (ivaln /= 0) then
              j = this%idmap(ivaln)
              iwrk(j) = iwrk(j) + 1
              iwrk2(ic,irn) = 1
            end if
            if (ivals /= 0) then
              j = this%idmap(ivals)
              iwrk(j) = iwrk(j) + 1
              iwrk2(ic,irs) = 1
            end if
            if (ivalw /= 0) then
              j = this%idmap(ivalw)
              iwrk(j) = iwrk(j) + 1
              iwrk2(icw,ir) = 1
            end if
            if (ivale /= 0) then
              j = this%idmap(ivale)
              iwrk(j) = iwrk(j) + 1
              iwrk2(ice,ir) = 1
            end if
          end if
        end do
      end do
      !
      do j = 1, n
        if (iwrk(j) > 0) then
          nja = nja + 1
        end if
      end do
    end do
    !
    allocate(this%adjncy(nja))
    allocate(this%adjwgt(nja))
    allocate(this%ncon)
    this%ncon = 1
    allocate(this%tpwgts(this%nparts*this%ncon))
    allocate(this%ubvec(this%ncon))
    this%ubvec(1) = tolimbal
    this%tpwgts = 1./real(this%nparts)
    this%vsize = 1
    !
    nja = 0
    this%xadj(1) = 0
    do i = 1, n
      id = this%idmapinv(i)
      do j = 1, n
        iwrk(j) = 0
      end do
      ir0 = idbb(1,i); ir1 = idbb(2,i)
      ic0 = idbb(3,i); ic1 = idbb(4,i)
      do ir = ir0, ir1
        do ic = ic0, ic1
          if ((abs(ids(ic,ir)) == id).and.(iwrk2(ic,ir) == 1)) then
            j = this%idmap(id)
            irn = max(ir-1, 1); irs = min(ir+1, nr)
            icw = max(ic-1, 1); ice = min(ic+1, nc)
            ivaln = abs(ids(ic,irn)); ivals = abs(ids(ic,irs))
            ivalw = abs(ids(icw,ir)); ivale = abs(ids(ice,ir))
            if (ivaln /= 0) then
              j = this%idmap(ivaln)
              iwrk(j) = iwrk(j) + 1
              iwrk2(ic,irn) = 1
            end if
            if (ivals /= 0) then
              j = this%idmap(ivals)
              iwrk(j) = iwrk(j) + 1
              iwrk2(ic,irs) = 1
            end if
            if (ivalw /= 0) then
              j = this%idmap(ivalw)
              iwrk(j) = iwrk(j) + 1
              iwrk2(icw,ir) = 1
            end if
            if (ivale /= 0) then
              j = this%idmap(ivale)
              iwrk(j) = iwrk(j) + 1
              iwrk2(ice,ir) = 1
            end if
          end if
        end do
      end do
      !
      this%xadj(i+1) = this%xadj(i)
      do j = 1, n
        if (iwrk(j) > 0) then
          nja = nja + 1
          this%adjncy(nja) = j - 1
          this%xadj(i+1) = this%xadj(i+1) + 1
          this%adjwgt(nja) = iwrk(j)
        end if
      end do
    end do

    deallocate(iwrk, iwrk2, idbb)
    
  end subroutine metis_init_lump
  
  subroutine metis_init_lump_indep(this, vwgt, nparts)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
     ! -- dummy
    class(tMetis) :: this
    integer(i4b), dimension(:), intent(in) :: vwgt
    integer(i4b), intent(in) :: nparts
    ! -- local
    integer(i4b) :: i, nja
! ------------------------------------------------------------------------------
    lump_indep = .true.
    
    allocate(this%nparts)
    this%nparts = nparts
    allocate(this%nvtxs)
    this%nvtxs = size(vwgt)
    allocate(this%vwgt(this%nvtxs))
    do i = 1, this%nvtxs
      this%vwgt(i) = vwgt(i)
    end do
    allocate(this%vsize(this%nvtxs))
    allocate(this%part(this%nvtxs)); this%part = 0
    allocate(this%xadj(this%nvtxs+1))
    nja = 1
    allocate(this%adjncy(nja))
    allocate(this%adjwgt(nja))
    allocate(this%ncon)
    this%ncon = 1
    allocate(this%tpwgts(this%nparts*this%ncon))
    allocate(this%ubvec(this%ncon))
    this%ubvec(1) = tolimbal
    this%tpwgts = 1./real(this%nparts)
    this%vsize = 1
    !
    do i = 1, this%nvtxs
      this%xadj(i) = 0
    end do
    this%adjncy = 0
    this%adjwgt = 1
    
  end subroutine metis_init_lump_indep
  
  subroutine metis_set_opts(this, niter_in, contig_in)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMetis) :: this
    integer(i8b), intent(in), optional :: niter_in
    integer(i8b), intent(in), optional :: contig_in
    ! -- local
    integer(i8b), parameter :: niter    = 100      ! number of iterations for the refinement algorithms at each stage of the uncoarsening process.
    integer(i8b), parameter :: contig   = 1        ! try to produce partitions that are contiguous
    
! ------------------------------------------------------------------------------
   if (.not.associated(this%opts)) then
     allocate(this%opts(METIS_NOPTIONS))
   end if
   !
   call METIS_SetDefaultOptions(this%opts)
   !this%opts(METIS_OPTION_DBGLVL) = METIS_DBG_INFO
   !
   if (present(niter_in)) then
     this%opts(METIS_OPTION_NITER) = niter_in
   else
     this%opts(METIS_OPTION_NITER) = niter
   end if
   !
   if (present(contig_in)) then
     this%opts(METIS_OPTION_CONTIG) = contig_in
   else
     this%opts(METIS_OPTION_CONTIG) = contig
   end if
  end subroutine metis_set_opts
  
  subroutine metis_recursive(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMetis) :: this
    ! -- local
    character(len=mxslen) :: s
    logical :: lempty
! ------------------------------------------------------------------------------
    if (.not.associated(this%objval)) then
      allocate(this%objval)
    end if
    write(s,*) this%nparts
    write(*,'(a)') 'Begin calling METIS using recursive bisection for '// &
      trim(adjustl(s))//' parts...'
    call METIS_PARTGRAPHRECURSIVE(this%nvtxs, this%ncon, this%xadj, this%adjncy, &
      this%vwgt, this%vsize, this%adjwgt, this%nparts, this%tpwgts, this%ubvec,  &
      this%opts, this%objval, this%part)
    
    call this%check_empty(lempty)
    if (lempty) then
      ! try 1
      write(*,*) 'Error, empty partition detected.'
      stop 1
    end if
    call this%calc_imbal()
    
  end subroutine metis_recursive

  subroutine metis_kway(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMetis) :: this
    ! -- local
    character(len=mxslen) :: s
    logical :: lempty
! ------------------------------------------------------------------------------
    if (.not.associated(this%objval)) then
      allocate(this%objval)
    end if
    write(s,*) this%nparts
    write(*,'(a)') 'Begin calling METIS using KWAY for '// &
      trim(adjustl(s))//' parts...'
    call METIS_PARTGRAPHKWAY( this%nvtxs, this%ncon, this%xadj, this%adjncy,    &
      this%vwgt, this%vsize, this%adjwgt, this%nparts, this%tpwgts, this%ubvec, &
      this%opts, this%objval, this%part)
    
    call this%check_empty(lempty)
    if (lempty) then
      ! try 1
      write(*,*) 'Error, empty partition detected.'
      stop 1
    end if
    call this%calc_imbal()
    
  end subroutine metis_kway
  
  subroutine metis_check_empty(this, lempty)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMetis) :: this
    logical, intent(out) :: lempty
    ! -- local
    integer(i4b) :: i, j
    integer(i4b), dimension(:), allocatable :: iwrk
! ------------------------------------------------------------------------------
    allocate(iwrk(this%nparts))
    do i = 1, this%nparts
      iwrk(i) = 0
    end do
    !
    lempty = .false.
    do i = 1, this%nvtxs
      j = this%part(i) + 1
      iwrk(j) = 1
    end do
    if (sum(iwrk) /= this%nparts) then
      lempty = .true.
    end if
    deallocate(iwrk)
    
  end subroutine metis_check_empty
  
  subroutine metis_calc_imbal(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMetis) :: this
    ! -- local
    real(r8b), dimension(:), allocatable :: load
    integer(i4b) :: i, ip
! ------------------------------------------------------------------------------
    !
    if (.not.associated(this%totload)) allocate(this%totload)
    if (.not.associated(this%imbal))   allocate(this%imbal)
    !
    allocate(load(this%nparts))
    do ip = 1, this%nparts
      load(ip) = DZERO
    end do
    !
    do i = 1, this%nvtxs
      ip = this%part(i) + 1
      load(ip) = load(ip) + this%vwgt(i)
    end do
    this%totload = sum(load)
    do ip = 1, this%nparts
      load(ip) = load(ip)/(this%totload*this%tpwgts(ip))
    end do
    this%imbal = maxval(load)
    write(*,*) 'Load imbalance:', this%imbal
    
    deallocate(load)

  end subroutine metis_calc_imbal
  
  subroutine metis_graph_stat(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMetis) :: this
    ! -- local
    integer(i4b) :: i, nja, mindeg, maxdeg, minvwgt, maxvwgt, minewgt, maxewgt
    real(r8b) :: avgdeg, stddeg, avgvwgt, stdvwgt, avgewgt, stdewgt, sparsity
    real(r8b), dimension(:), allocatable :: r8wk
    character(len=mxslen) :: s, s2
! ------------------------------------------------------------------------------
    !
    write(*,*) '================================'
    write(*,*) 'Graph Statistics:'
    !
    ! edges
    allocate(r8wk(this%nvtxs))
    do i = 1, this%nvtxs
      r8wk(i) = real(this%xadj(i+1)-this%xadj(i),r8b)
    end do
    mindeg = int(minval(r8wk),i4b)
    maxdeg = int(maxval(r8wk),i4b)
    avgdeg = sum(r8wk)/size(r8wk)
    stddeg = sqrt(sum(r8wk**2)/size(r8wk) - avgdeg**2)
    sparsity = sum(r8wk)/(this%nvtxs*(this%nvtxs-1))
    !
    ! vertex weights
    minvwgt = int(minval(this%vwgt),i4b)
    maxvwgt = int(maxval(this%vwgt),i4b)
    avgvwgt = real(sum(this%vwgt),r8b)/size(this%vwgt)
    stdvwgt = sqrt(real(sum(this%vwgt**2),r8b)/size(this%vwgt) - avgvwgt**2)
    !
    ! edge weights
    minewgt = int(minval(this%adjwgt),i4b)
    maxewgt = int(maxval(this%adjwgt),i4b)
    avgewgt = real(sum(this%adjwgt),r8b)/size(this%adjwgt)
    stdewgt = sqrt(real(sum(this%adjwgt**2),r8b)/size(this%adjwgt) - avgewgt**2)
    !
    write(s,*) this%nvtxs
    write(*,*) '# vertices: '//trim(adjustl(s))
    write(s,*) int(sum(r8wk),i4b)
    write(*,*) '# edges:    '//trim(adjustl(s))
    !
    write(s,*) real(sparsity,r4b)
    write(*,*) '# sparsity: '//trim(adjustl(s))
    !
    write(s,*) mindeg
    write(*,*) 'degree min: '//trim(adjustl(s))
    write(s,*) maxdeg
    write(*,*) 'degree max: '//trim(adjustl(s))
    write(s,*) real(avgdeg,r4b)
    write(*,*) 'degree avg: '//trim(adjustl(s))
    write(s,*) real(stddeg,r4b)
    write(*,*) 'degree std: '//trim(adjustl(s))
    !
    write(s,*) minvwgt; write(s2,*) minewgt
    write(*,*) 'weight min: '//trim(adjustl(s))//'/'//trim(adjustl(s2))
    write(s,*) maxvwgt; write(s2,*) maxewgt
    write(*,*) 'weight max: '//trim(adjustl(s))//'/'//trim(adjustl(s2))
    write(s,*) real(avgvwgt,r4b); write(s2,*) real(avgewgt,r4b)
    write(*,*) 'weight avg: '//trim(adjustl(s))//'/'//trim(adjustl(s2))
    write(s,*) real(stdvwgt,r4b); write(s2,*) real(stdewgt,r4b)
    write(*,*) 'weight std: '//trim(adjustl(s))//'/'//trim(adjustl(s2))
    write(*,*) '================================'
    !
    deallocate(r8wk)
    return
    !
  end subroutine metis_graph_stat
  
  subroutine metis_set_ids(this, ids, id_offset_in)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMetis) :: this
    integer(i4b), dimension(:,:), intent(inout) :: ids
    integer(i4b), intent(in), optional :: id_offset_in
    ! -- local
    integer(i4b) :: n, nr, nc, ir, ic, id_offset, id, i
! ------------------------------------------------------------------------------  
    if (present(id_offset_in)) then
      id_offset = id_offset_in
    else
      id_offset = 0
    end if
    !
    do i = 1, this%nvtxs
      this%part(i) = this%part(i) + 1
    end do
    
    nc = size(ids, 1); nr = size(ids, 2)
    if (lump) then
      do ir = 1, nr
        do ic = 1, nc
          id = abs(ids(ic,ir))
          if (id /= 0) then
            i = this%idmap(id)
            ids(ic,ir) = abs(this%part(i)) + id_offset
            this%part(i) = -abs(this%part(i))
          end if
        end do
      end do
    else
      n = 0
      do ir = 1, nr
        do ic = 1, nc
          if (abs(ids(ic,ir)) /= 0) then
            n = n + 1
            ids(ic,ir) = abs(this%part(n)) + id_offset
            this%part(n) = -abs(this%part(n))
          end if
        end do
      end do
    end if
    !
    ! check
    do i = 1, this%nparts
      if (this%part(i) > 0) then
        write(*,*) 'Warning, partition not set!'
      end if
      this%part(i) = abs(this%part(i))
    end do
    
  end subroutine metis_set_ids
  
  subroutine metis_clean(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMetis) :: this
    ! -- local
! ------------------------------------------------------------------------------
  
    if (associated(this%nvtxs))  deallocate(this%nvtxs)
    if (associated(this%ncon))   deallocate(this%ncon)
    if (associated(this%xadj))   deallocate(this%xadj)
    if (associated(this%adjncy)) deallocate(this%adjncy)
    if (associated(this%vwgt))   deallocate(this%vwgt)
    if (associated(this%vsize))  deallocate(this%vsize)
    if (associated(this%adjwgt)) deallocate(this%adjwgt)
    if (associated(this%nparts)) deallocate(this%nparts)
    if (associated(this%tpwgts)) deallocate(this%tpwgts)
    if (associated(this%ubvec))  deallocate(this%ubvec)
    if (associated(this%opts))   deallocate(this%opts)
    if (associated(this%objval)) deallocate(this%objval)
    if (associated(this%part))   deallocate(this%part)
    !
    if (associated(this%idmap))    deallocate(this%idmap)
    if (associated(this%idmapinv)) deallocate(this%idmapinv)
    lump = .false.
  end subroutine metis_clean
  
end module metis_module
