module mf6_post_module
  ! -- modules
  use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, &
     i1b => int8, i2b => int16, i4b => int32, i8b => int64, r4b => real32, r8b => real64
  use utilsmod, only: IZERO, DZERO, DONE, tBB, errmsg, logmsg, swap_slash, open_file, chkexist, &
    get_jd, get_ymd_from_jd, ta, writeasc, writeidf, replacetoken, linear_regression, &
    tTimeSeries, parse_line, insert_tab, create_dir, change_case, count_dir_files, bb_overlap, &
    quicksort_d
  use pcrModule, only: tmap
  use ehdrModule, only: writeflt
  use imod_idf
  !
  implicit none
  !
  private
  !
  ! -- parameters
  logical, parameter :: include_sea = .true.
  !
  character(len=1), parameter :: comment = '#'
  real(r8b), parameter :: r8nodata = -9999.D0
  integer(i4b), parameter :: mxslen = 1024
  !
  integer(i4b), parameter :: i_out_idf  = 1
  integer(i4b), parameter :: i_out_asc  = 2
  integer(i4b), parameter :: i_out_ipf  = 3
  integer(i4b), parameter :: i_out_txt  = 4
  integer(i4b), parameter :: i_out_flt  = 5
  integer(i4b), parameter :: i_out_flts = 6
  !
  integer(i4b), parameter :: sf = 10
  !
  ! -- global variables
  integer(i4b) :: gnsol = IZERO
  integer(i4b) :: gncol = IZERO
  integer(i4b) :: gnrow = IZERO
  integer(i4b) :: gnlay = IZERO
  real(r8b)    :: gxmin = DZERO
  real(r8b)    :: gymin = DZERO
  real(r8b)    :: gcs   = DZERO
  character(len=mxslen) :: sdate = ''
  character(len=mxslen) :: tilebb = ''
  character(len=mxslen) :: top = ''
  character(len=mxslen) :: mask = ''
  !
  integer(i4b)            :: top_type, top_kper
  integer(i4b), parameter :: i_tiled  = 1
  integer(i4b), parameter :: i_mf6    = 2
  !
  integer(i4b), parameter :: i_idf = 1
  integer(i4b), parameter :: i_map = 2
  
  type tTop
    integer(i4b), pointer               :: i_type => null()
    integer(i4b), pointer               :: ntile => null()
    type(tBb), dimension(:), pointer    :: tilebb => null()
    type(tMap), dimension(:), pointer   :: maps => null()
    type(idfobj), dimension(:), pointer :: idfs => null()
  end type tTop
  type(tTop), pointer :: topmap => null()
  
  type(tMap), pointer :: maskmap => null() ! mask, e.g. unconsolidated soils
  type(tMap), pointer :: statmap => null() ! map for comparision
  !
  type tBin
    character(len=mxslen)                :: id = ''
    real(r8b)                            :: min = -huge(DZERO)
    real(r8b)                            :: max =  huge(DZERO)
    integer(i4b)                         :: n = 0
    real(r8b), dimension(:), allocatable :: val
  end type tBin
  !
  ! types
  type tGen
    character(len=mxslen), pointer        :: in_dir    => null()
    character(len=mxslen), pointer        :: in_postf  => null()
    character(len=mxslen), pointer        :: out_dir   => null()
    character(len=mxslen), pointer        :: out_pref  => null()
    logical, pointer                      :: lwritebb  => null()
    integer(i4b), pointer                 :: gic0      => null()
    integer(i4b), pointer                 :: gic1      => null()
    integer(i4b), pointer                 :: gir0      => null()
    integer(i4b), pointer                 :: gir1      => null()
    integer(i4b), pointer                 :: il_min    => null()
    integer(i4b), pointer                 :: il_max    => null()
    character(len=6), pointer             :: date_beg  => null()
    character(len=6), pointer             :: date_end  => null()
    integer(i4b), pointer                 :: kper_beg  => null()
    integer(i4b), pointer                 :: kper_end  => null()
    integer(i4b), pointer                 :: itype     => null()
    logical, pointer                      :: top_tiled => null()
    logical, pointer                      :: top_mf6   => null()
    integer(i4b), pointer                 :: i_out     => null()
    integer(i4b), pointer                 :: nbin      => null()
    type(tBin), dimension(:), pointer     :: bins      => null()
    logical, pointer                      :: year_avg  => null()
    integer(i4b), pointer                 :: year_beg  => null()
  end type tGen
  !
  type tPostMod
    integer(i4b), pointer                 :: modid      => null()
    character(len=mxslen), pointer        :: modname    => null()
    character(len=mxslen), pointer        :: f          => null()
    integer(i4b), pointer                 :: iu         => null()
    type(tBB), pointer                    :: bb         => null()
    integer(i4b), pointer                 :: nodes      => null()
    integer(i4b), dimension(:,:), pointer :: giliric    => null()
    integer(i4b), pointer                 :: kper_read  => null()
    real(r8b), pointer                    :: totim_read => null()
    real(r8b), dimension(:), pointer      :: top        => null()
    real(r8b), dimension(:), pointer      :: r8buff     => null()
    real(r8b), dimension(:,:), pointer    :: r8buff2d   => null()
    !
    type(tGen), pointer :: gen => null()
  contains
    procedure :: init           => mf6_post_mod_init
    procedure :: read_nodbin    => mf6_post_mod_read_nodbin
    procedure :: read_top_tiled => mf6_post_mod_read_top_tiled
    procedure :: read_top_mf6   => mf6_post_mod_read_top_mf6
    procedure :: read           => mf6_post_mod_read
    procedure :: read_nod       => mf6_post_mod_read_nod
    procedure :: calc_slope     => mf6_post_mod_calc_slope
    procedure :: calc_average   => mf6_post_mod_calc_average
    procedure :: calc_iqr       => mf6_post_mod_calc_iqr
    procedure :: get_data       => mf6_post_mod_get_data
    procedure :: write          => mf6_post_mod_write
    procedure :: clean          => mf6_post_mod_clean
  end type tPostMod
  !
  type tPostSol
    logical, pointer                      :: lwritesol   => null()
    logical, pointer                      :: lwritemodid => null()
    logical, pointer                      :: lmask       => null()
    integer(i4b), pointer                 :: solid       => null()
    character(len=mxslen), pointer        :: solname     => null()
    type(tBB), pointer                    :: bb          => null()
    integer(i4b), pointer                 :: nmod        => null()
    type(tPostMod), dimension(:), pointer :: mod         => null()
    integer(i4b), dimension(:,:), pointer :: mask        => null()
    type(tBB), pointer                    :: maskbb      => null()
    !
    type(tGen), pointer :: gen => null()
  contains
    procedure :: init        => mf6_post_sol_init
    procedure :: init_select => mf6_post_sol_init_select
    procedure :: read_modbin => mf6_post_sol_read_modbin
    procedure :: write       => mf6_post_sol_write
    procedure :: write_stat  => mf6_post_sol_write_statistics
    procedure :: clean       => mf6_post_sol_clean
  end type tPostSol
  !
  type tPostSerMod
    integer(i4b), pointer               :: nts   => null()
    integer(i4b), dimension(:), pointer :: tsidx => null() 
  end type tPostSerMod
  type tPostSer
    character(len=mxslen), pointer           :: f_in    => null()
    type(tTimeSeries), dimension(:), pointer :: ts      => null()
    integer(i4b), pointer                    :: nts     => null()
    type(tPostMod), dimension(:), pointer    :: mod     => null()
    type(tPostSerMod), dimension(:), pointer :: tsmod   => null()
    type(tGen), pointer                      :: gen     => null()
    logical, pointer                         :: writets => null() 
  contains
    procedure :: init          => mf6_post_ser_init
    procedure :: read_stat     => mf6_post_ser_read_stat
    procedure :: write         => mf6_post_ser_write
    procedure :: write_summary => mf6_post_ser_write_summary
    procedure :: clean         => mf6_post_ser_clean
  end type tPostSer
  !
  save
  !
  public :: include_sea
  public :: comment
  public :: tPostMod, tPostSol, tPostSer
  public :: gnsol, gncol, gnrow, gnlay, gxmin, gymin, gcs, sdate, tilebb, top
  public :: mask, maskmap
  public :: top_type, top_kper, i_tiled, i_mf6
  
  contains
  !
  subroutine r8_scale_avg(r8x_in, r8nodata, bb, r8x_out, xmin, ymin, cs)
! ******************************************************************************  
    ! -- arguments
    real(r8b), dimension(:,:), intent(in) :: r8x_in
    real(r8b), intent(in) :: r8nodata
    type(tBB), intent(in) :: bb
    real(r8b), dimension(:,:), allocatable :: r8x_out
    real(r8b), intent(out) :: cs
    real(r8b), intent(out) :: xmin
    real(r8b), intent(out) :: ymin
    ! --- local
    integer(i4b) :: ic0, ic1, ir0, ir1, nc, nr, ir, ic, jr, jc
    integer(i4b) :: gic, gir, gjc, gjr
    real(i4b), dimension(:,:), allocatable :: i4wk
    real(r8b) :: r8v
! ------------------------------------------------------------------------------
    ic0 = int((bb%ic0-1)/sf) + 1; ic1 = int((bb%ic1-1)/sf) + 1
    ir0 = int((bb%ir0-1)/sf) + 1; ir1 = int((bb%ir1-1)/sf) + 1
    nc = ic1 - ic0 + 1; nr = ir1 - ir0 + 1
    allocate(i4wk(nc,nr), r8x_out(nc,nr))
    do jr = 1, nr
      do jc = 1, nc
        i4wk(jc,jr) = 0
        r8x_out(jc,jr) = r8nodata
      end do
    end do
    !
    do ir = 1, bb%nrow
      do ic = 1, bb%ncol
        gic = bb%ic0 + ic - 1; gir = bb%ir0 + ir - 1
        gjc = int((gic-1)/sf) + 1; gjr = int((gir-1)/sf) + 1
        jc = gjc - ic0 + 1; jr = gjr - ir0 + 1
        if ((jc < 1).or.(jc > nc).or.(jr < 1).or.(jr > nr)) then
          call errmsg('Program error writing scaled flt-file.')
        end if
        r8v = r8x_in(ic,ir)
        if (r8v /= r8nodata) then
          if (r8x_out(jc,jr) == r8nodata) then
            r8x_out(jc,jr) = DZERO
          end if
          r8x_out(jc,jr) = r8x_out(jc,jr) + r8v
          i4wk(jc,jr) = i4wk(jc,jr) + 1
        end if
      end do
    end do
    !
    do jr = 1, nr
      do jc = 1, nc
        if (r8x_out(jc,jr) /= r8nodata) then
          r8x_out(jc,jr) = r8x_out(jc,jr) / i4wk(jc,jr)
        end if
      end do
    end do
    !
    cs = real(sf,r8b) * gcs; xmin = gxmin + (ic0-1)*cs
    ymin = gymin + (gnrow/sf-ir1)*cs
    !
    return
  end subroutine r8_scale_avg
  
  subroutine init_top()
! ******************************************************************************  
    ! -- arguments
    ! --- local
    type(tBB), pointer :: bb => null()
    character(len=mxslen) :: f
    logical :: lex, lok
    integer(i4b) :: i, iu
! ------------------------------------------------------------------------------
    !
    allocate(topmap)
    call open_file(tilebb, iu, 'r')
    allocate(topmap%ntile)
    read(iu,*) topmap%ntile
    allocate(topmap%tilebb(topmap%ntile))
    do i = 1, topmap%ntile
      bb => topmap%tilebb(i)
      read(iu,*) bb%ic0, bb%ic1, bb%ir0, bb%ir1
      bb%ncol = bb%ic1 - bb%ic0 + 1
      bb%nrow = bb%ir1 - bb%ir0 + 1
    end do
    close(iu)
    allocate(topmap%i_type)
    if (index(top,'.map',back=.true.) > 0) then
      topmap%i_type = i_map
    else if (index(top,'.idf',back=.true.) > 0) then
      topmap%i_type = i_idf
    else
      call errmsg("Unrecognize top file format.")
    end if
    if (topmap%i_type == i_map) then
      allocate(topmap%maps(topmap%ntile))
    else
      allocate(topmap%idfs(topmap%ntile))
    end if
    do i = 1, topmap%ntile
      f = top
      call replacetoken(f, '?', i)
      call swap_slash(f)
      inquire(file=f,exist=lex)
      if (.not.lex) then
        call logmsg("Could not find "//trim(f))
      else
        if (topmap%i_type == i_map) then
          lok = topmap%maps(i)%init(f, 1)
        else
          lok = idfread(topmap%idfs(i), f, 0)
        end if
        if (.not.lok) then
          call errmsg('Initializing top.')
        end if
      end if
    end do
    !
    return
  end subroutine init_top

! ==============================================================================
! subroutines/functions type tPostSeries
! ==============================================================================
  
  subroutine mf6_post_ser_init(this, opt)
! ******************************************************************************  
    ! -- arguments
    class(tPostSer) :: this
    character(len=*), dimension(:), intent(in) :: opt
    ! --- local
    integer(i4b), parameter :: i_in_dir   =  1
    integer(i4b), parameter :: i_f_in     =  3
    integer(i4b), parameter :: i_idcol    =  4
    integer(i4b), parameter :: i_xcol     =  5
    integer(i4b), parameter :: i_ycol     =  6
    integer(i4b), parameter :: i_smcol    =  7
    integer(i4b), parameter :: i_isol_beg =  8
    integer(i4b), parameter :: i_isol_end =  9
    integer(i4b), parameter :: i_in_postf = 10
    integer(i4b), parameter :: i_itype    = 11
    integer(i4b), parameter :: i_il_min   = 12
    integer(i4b), parameter :: i_il_max   = 13
    integer(i4b), parameter :: i_date_beg = 14
    integer(i4b), parameter :: i_date_end = 15
    integer(i4b), parameter :: i_i_out    = 16
    integer(i4b), parameter :: i_out_dir  = 17
    !
    type(tBB), pointer :: tsbb => null()
    type(tGen), pointer :: gen => null()
    type(tTimeSeries), pointer :: ts => null()
    type(tPostMod), pointer :: m => null()
    type(tPostSerMod), pointer :: tsm => null()
    character(len=mxslen) :: solname,  f, st
    integer(i4b) :: idcol, xcol, ycol, smcol, i, j, k, iu, isol, isol_beg, isol_end, nmod, iact, mxmod
    integer(i4b) :: il, ir, ic, gil, gir, gic, n, nn
    integer(i4b) :: ys, mns, y, mn, kper_beg, kper_end
    integer(i4b), dimension(gnlay) :: nod
    integer(i4b), dimension(:), allocatable :: g2lmod, l2gmod, i4wk, modflg ,mismod
    integer(i4b), dimension(:,:,:), allocatable :: nodmap
    integer(i4b) :: i4val
    logical :: lmv
! ------------------------------------------------------------------------------
    ! general parameters
    allocate(this%gen); gen => this%gen
    allocate(gen%itype); read(opt(i_itype),*) gen%itype
    allocate(gen%in_dir); read(opt(i_in_dir),'(a)') gen%in_dir
    allocate(gen%gic0); gen%gic0 = 1
    allocate(gen%gic1); gen%gic1 = gncol
    allocate(gen%gir0); gen%gir0 = 1
    allocate(gen%gir1); gen%gir1 = gnrow
    allocate(gen%top_tiled); gen%top_tiled = .true.; call init_top()
    read(opt(i_isol_beg),*) isol_beg
    read(opt(i_isol_end),*) isol_end
    allocate(gen%in_postf); gen%in_postf = opt(i_in_postf)
    allocate(gen%il_min); read(opt(i_il_min),*) gen%il_min
    allocate(this%writets); this%writets = .true. !TODO
    allocate(gen%il_max); read(opt(i_il_max),*) gen%il_max
    allocate(gen%date_beg); read(opt(i_date_beg),*) gen%date_beg
    allocate(gen%date_end); read(opt(i_date_end),*) gen%date_end
    allocate(gen%i_out)
    select case(trim(opt(i_i_out)))
      case('txt')
        gen%i_out = i_out_txt
      case('ipf')
        gen%i_out = i_out_ipf
      case default
        call errmsg('mf6_post_ser_init')
    end select    
    allocate(gen%out_dir); read(opt(i_out_dir),'(a)') gen%out_dir
    call create_dir(gen%out_dir, .true.)
    !
    read(sdate(1:4),*) ys; read(sdate(5:6),*) mns
    read(gen%date_beg(1:4),*) y; read(gen%date_beg(5:6),*) mn
    kper_beg = y*12 + (mn -1 ) - (ys*12 + mns - 1) + 1
    read(gen%date_end(1:4),*) y; read(gen%date_end(5:6),*) mn
    kper_end = y*12 + (mn - 1) - (ys*12 + mns - 1) + 1
    !
    allocate(gen%kper_beg); gen%kper_beg = kper_beg
    allocate(gen%kper_end); gen%kper_end = kper_end
    
    allocate(this%f_in)
    this%f_in = opt(i_f_in)
    read(opt(i_idcol),*) idcol
    read(opt(i_xcol),*) xcol
    read(opt(i_ycol),*) ycol
    read(opt(i_smcol),*) smcol
    !
    ! read the station file
    call this%read_stat(idcol, xcol, ycol, smcol)
    !
    ! set the row and columns
    allocate(tsbb)
    do i = 1, this%nts
      ts => this%ts(i)
      allocate(ts%ic, ts%ir)
      ts%ic = int((ts%x - gxmin)/gcs, i4b) + 1
      ts%ir = gnrow - int((ts%y - gymin)/gcs, i4b)
      if ((ts%ic < 0).or.(ts%ic > gncol).or. &
          (ts%ir < 0).or.(ts%ir > gnrow)) then
        ts%act = .false.
      else
        tsbb%ic0 = min(tsbb%ic0, ts%ic); tsbb%ic1 = max(tsbb%ic1, ts%ic)
        tsbb%ir0 = min(tsbb%ir0, ts%ir); tsbb%ir1 = max(tsbb%ir1, ts%ir)
      end if
    end do
    !
    ! determine the models in the solutions
    do iact = 1, 2
      mxmod = 0
      do isol = isol_beg, isol_end
        write(solname,'(a,i2.2)') 's',isol
        f = trim(gen%in_dir)//'solutions\post_mappings\'//trim(solname)//'.modmap.bin'
        call swap_slash(f)
        call chkexist(f)
        !
        call open_file(f, iu, 'r', .true.)
        read(iu) nmod
        if (allocated(i4wk)) deallocate(i4wk)
        allocate(i4wk(nmod))
        read(iu)(i4wk(i),i=1,nmod); close(iu)
        mxmod = max(mxmod,maxval(i4wk))
        if (iact == 2) then
          do i = 1, nmod
            modflg(i4wk(i)) = 1
          end do
        end if
      end do
      if (iact == 1) then
        allocate(modflg(mxmod))
        modflg = 0
      end if
    end do
    !
    nmod = sum(modflg)
    allocate(this%mod(nmod))
    allocate(g2lmod(mxmod), l2gmod(nmod))
    j = 0
    do i = 1, mxmod
      if (modflg(i) == 1) then
        modflg(i) = -1
        j = j + 1
        g2lmod(i) = j
        l2gmod(j) = i
      else
        g2lmod(i) = 0
      end if
    end do
    !
    do i = 1, mxmod
      if (modflg(i) == 0) cycle
      j = g2lmod(i); m => this%mod(j)
      allocate(m%modid); m%modid = i
      m%gen => this%gen
      call m%init(.true.)
    end do
    !
    ! selection 1: select based of bounding box
    do i = 1, mxmod
      if (modflg(i) == 0) cycle
      j = g2lmod(i); m => this%mod(j)
      if ((m%bb%ic0 > tsbb%ic1).or.(m%bb%ic1 < tsbb%ic0).or.&
          (m%bb%ir0 > tsbb%ir1).or.(m%bb%ir1 < tsbb%ir0)) then
        modflg(i) = 0
      end if
    end do
    !
    ! selection 2: determine models that have point candidates
    do i = 1, mxmod
      if (modflg(i) == 0) cycle
      j = g2lmod(i); m => this%mod(j)
      !
      do k = 1, this%nts
        ts => this%ts(k)
        if ((m%bb%ic0 <= ts%ic).and.(ts%ic <= m%bb%ic1).and.&
            (m%bb%ir0 <= ts%ir).and.(ts%ir <= m%bb%ir1)) then
          modflg(i) = 1
        end if
      end do
    end do
    !
    ! selection 3: read the nodes for the models that have point candidates
    do i = 1, mxmod
      if (modflg(i) <= 0) cycle
      j = g2lmod(i); m => this%mod(j)
      call m%clean()
      allocate(m%modid); m%modid = i
      m%gen => this%gen
      call m%init()
    end do
    !
    ! selection 4: map the nodes
    do k = 1, this%nts
      ts => this%ts(k)
      allocate(ts%im); ts%im = 0
    end do
    !
    do i = 1, mxmod
      if (modflg(i) <= 0) cycle
      j = g2lmod(i); m => this%mod(j)
      allocate(nodmap(m%bb%ncol, m%bb%nrow, gnlay))
      do il = 1, gnlay
        do ir = 1,  m%bb%nrow
          do ic = 1,  m%bb%ncol
            nodmap(ic,ir,il) = 0
          end do
        end do
      end do
      do k = 1, m%nodes
        gil = abs(m%giliric(1,k))
        gir = abs(m%giliric(2,k))
        gic = abs(m%giliric(3,k))
        ic = gic - m%bb%ic0 + 1; ir = gir - m%bb%ir0 + 1; il = gil
        nodmap(ic,ir,il) = k
      end do 
      !
      do k = 1, this%nts
        ts => this%ts(k)
        if (ts%im /= 0) cycle !already found
        !
        ! check if point is within model bounding box
        if ((m%bb%ic0 <= ts%ic).and.(ts%ic <= m%bb%ic1).and.&
            (m%bb%ir0 <= ts%ir).and.(ts%ir <= m%bb%ir1)) then
          ic = ts%ic - m%bb%ic0 + 1; ir = ts%ir - m%bb%ir0 + 1
          nod = 0; ts%nlay = 0
          do il = 1, gnlay
            nod(il) = nodmap(ic,ir,il)
            if (nod(il) > 0) then
              ts%nlay = ts%nlay + 1
            end if
          end do
          if (maxval(nod) > 0) then !found
            ts%im = j
            allocate(ts%nod(gnlay)); ts%nod = nod
          end if
        end if
      end do
      deallocate(nodmap)
    end do
    !
    ! deactivate time series that do not have associated model data
    n = 0; nn = 0; allocate(mismod(mxmod))
    do k = 1, mxmod
      mismod(k) = 0
    end do
    do k = 1, this%nts
      ts => this%ts(k)
      if (ts%im == 0) then
        n = n + 1
        ts%act = .false.
      else
        !j = g2lmod(ts%im); m => this%mod(j)
        m => this%mod(ts%im)
        if (m%iu <= 0) then
          j = g2lmod(ts%im); mismod(j) = 1
          nn = nn + 1
          ts%act = .false.
        end if
      end if
    end do
    call logmsg('* Number of time-series removed by non-existing partition: '// &
      ta((/n/))//'/'//ta((/this%nts/))//'...')
    call logmsg('* Number of time-series removed by non-existing data: '// &
      ta((/nn/))//'/'//ta((/this%nts/))//'...')
    if (nn > 0) then
      do k = 1, mxmod
        if (mismod(k) == 1) then
          call logmsg('** Non-existing model: '//ta((/k/)))
        end if
      end do
    end if
    deallocate(mismod)
    !
    ! deactivate using mask
    if (associated(maskmap)) then
      n = 0
      do k = 1, this%nts
        ts => this%ts(k)
        if (ts%act) then
          ic = ts%ic; ir = ts%ir
          call maskmap%get_val(ic, ir, i4val, lmv)
          if (i4val <= 0) then
            n = n + 1
            ts%act = .false.
          end if
        end if
      end do
      call logmsg('* Number of time-series removed by mask: '// &
        ta((/n/))//'/'//ta((/this%nts/))//'...')
    end if
    !
    ! selection 5: flag the models containing timeseries
    do i = 1, mxmod
      modflg(i) = 0
    end do
    n = 0
    do k = 1, this%nts
      ts => this%ts(k)
      if (ts%act) then
        n = n + 1
        modflg(l2gmod(ts%im)) = 1
      end if
    end do
    call logmsg('***** Total number of time-series to process: '// &
      ta((/n/))//'/'//ta((/this%nts/))//'...')
    !
    ! create the model to time-series mapping
    allocate(this%tsmod(nmod))
    do i = 1, nmod
      tsm => this%tsmod(i)
      allocate(tsm%nts); tsm%nts = 0
    end do
    do iact = 1, 2
      do k = 1, this%nts
        ts => this%ts(k)
        if (ts%act) then
          tsm => this%tsmod(ts%im)
          tsm%nts = tsm%nts + 1
          if (iact == 2) then
            tsm%tsidx(tsm%nts) = k
          end if
        end if
      end do
      if (iact == 1) then
        do i = 1, nmod
          tsm => this%tsmod(i)
          if (tsm%nts > 0) then
            allocate(tsm%tsidx(tsm%nts))
          end if
          tsm%nts = 0
        end do
      end if
    end do
    !
    ! clean the models that are not being used
    do i = 1, mxmod
      j = g2lmod(i)
      if (j == 0) cycle
      if (modflg(i) == 0) then
        m => this%mod(j)
        call m%clean()
      end if
    end do
    !
    return
  end subroutine mf6_post_ser_init

  subroutine mf6_post_ser_read_stat(this, idcol, xcol, ycol, smcol)
! ******************************************************************************  
    ! -- arguments
    class(tPostSer) :: this
    integer(i4b), intent(in) :: idcol
    integer(i4b), intent(in) :: xcol
    integer(i4b), intent(in) :: ycol
    integer(i4b), intent(in) :: smcol
    ! --- local
    type(tTimeSeries), pointer :: ts => null()
    character(len=mxslen) :: hdr, s
    character(len=mxslen), dimension(:), allocatable :: sa
    integer(i4b) :: iu, i, ios, iact
! ------------------------------------------------------------------------------
    !
    ! read the station file
    call chkexist(this%f_in)
    call open_file(this%f_in, iu, 'r')
    allocate(this%nts)
    !
    do iact = 1, 2
      this%nts = 0
      read(unit=iu,fmt='(a)',iostat=ios) hdr; call parse_line(hdr, sa)
      do while(ios == 0)
        read(unit=iu,fmt='(a)',iostat=ios) s; 
        if ((ios /= 0).or.(len_trim(s) == 0)) exit
        call parse_line(s, sa)
        this%nts = this%nts + 1
        if (iact == 2) then
          ts => this%ts(this%nts)
          allocate(ts%act, ts%read, ts%rawhdr, ts%raw, ts%id, ts%x, ts%y, ts%glev, ts%sm_corr, ts%nlay)
          ts%act = .true.
          ts%read = .false.
          ts%rawhdr = hdr
          ts%raw = s
          read(sa(idcol),*) ts%id
          read(sa(xcol),*) ts%x
          read(sa(ycol),*) ts%y
          ts%glev = huge(DZERO)
          if (smcol > 0) then
            read(sa(smcol),*) ts%sm_corr
          else
            ts%sm_corr = 1
          end if
          ts%nlay = 0
        end if
      end do
      if (iact == 1) then
        if (this%nts == 0) then
          call errmsg('No time series data read')
        end if
        allocate(this%ts(this%nts))
        rewind(iu)
      end if
    end do
    close(iu)
    !
    return
  end subroutine mf6_post_ser_read_stat
  
 subroutine mf6_post_ser_write(this, lwrite)
! ******************************************************************************  
    ! -- arguments
    class(tPostSer) :: this
    logical, intent(in) :: lwrite
    ! --- local
    integer(i4b), parameter :: mxnodread = 5
    !
    type(tTimeSeries), pointer :: ts => null()
    type(tPostMod), pointer :: m => null()
    type(tPostSerMod), pointer :: tsm => null()
    integer(i4b) :: i, j, il, nod, nper, kper, n
    !real(r8b), dimension(:), pointer :: r8wk2d
! --!----------------------------------------------------------------------------
    !
    if (.not.this%writets) return
    !
    nper =  this%gen%kper_end - this%gen%kper_beg + 1
    !
    ! read raster first
    do i = 1, size(this%tsmod)
      tsm => this%tsmod(i)
      if ((tsm%nts > 0).and.(tsm%nts > mxnodread)) then
        m => this%mod(i)
        call logmsg('Reading all heads for sub-model '//ta((/i/))// &
          ' for '//ta((/tsm%nts/))//' time-series...')
        call m%read()
        do j = 1, tsm%nts
          ts => this%ts(tsm%tsidx(j))
          allocate(ts%val(gnlay,nper))
          do il = this%gen%il_min, this%gen%il_max
            nod = ts%nod(il)
            if (nod > 0) then
              if (associated(m%top)) then
                ts%glev = m%top(nod)
              end if
              do kper = 1, nper
                ts%val(il,kper) = m%r8buff2d(kper,nod)
              end do
            end if
          end do
          ts%read = .true.
          if (lwrite) call mf6_post_ser_write_series(this%gen, ts)
        end do
        call m%clean()
      end if
    end do
    !
    ! read point directly
    n = 0
    do i = 1, this%nts
      ts => this%ts(i)
      ! debug
      !ts%read = .true.
      !ts => this%ts(6652); ts%read = .false.
      !
      if (ts%act.and.(.not.ts%read)) then
        n = n + 1
        m => this%mod(ts%im)
        allocate(ts%val(gnlay,nper))
        !allocate(r8wk2d(nper))
        do il = this%gen%il_min, this%gen%il_max
          nod = ts%nod(il)
          if (nod > 0) then
            call logmsg('Reading data for time series ('//ta((/i/))//'/'// &
              ta((/this%nts/))//') layer '//ta((/il/))//' node '//ta((/nod/))// &
              ' from model '//ta((/m%modid/))//'...')
            if (associated(m%top)) then
              ts%glev = m%top(nod)
            end if
            call m%read_nod(nod, ts%val(il,:))
            !call m%read_nod(nod, r8wk2d)
          end if
        end do
        !deallocate(r8wk2d)
        ts%read = .true.
        if (lwrite) call mf6_post_ser_write_series(this%gen, ts)
        !deallocate(ts%val)
        !if (mod(n,100) == 0) then
        !  call logmsg('**** sleeping ****')
        !  call sleep(5)
        !end if
      end if
    end do
    !
    ! set the nodes
    if ((.not.lwrite).and.(this%gen%itype ==4)) then
      do i = 1, this%nts
        ts => this%ts(i)
        if (.not.ts%act) cycle
        if (ts%nod(1) > 0) then
          if (ts%nod(2) > 0) then
            ts%nod(2) = 0
          end if
        end if
      end do
    end if
    !
    return
  end subroutine mf6_post_ser_write

  subroutine mf6_post_ser_write_series(gen, ts)
! ******************************************************************************  
    ! -- arguments
    type(tGen), pointer, intent(in) :: gen
    type(tTimeSeries), pointer, intent(in) :: ts 
    ! --- local
    character(len=1) :: datsep
    character(len=mxslen) :: f, s, s1, s2, s3, laystr
    character(len=mxslen), dimension(:), allocatable :: datestr
    logical :: lfirst, lipf, lwtd, lwrite
    integer(i4b) :: iper, nper, i, j, il, ys, mns, date, y, m, ye, me, de, iu, n
    real(r8b) :: jd, head, wtd
! --!----------------------------------------------------------------------------
    !
    if (.not.ts%act) return
    !
    nper =  gen%kper_end - gen%kper_beg + 1
    allocate(datestr(nper))
    !
    select case(gen%i_out)
      case(i_out_txt)
        datsep = '-'
      case(i_out_ipf)
        datsep = ''
    end select
    !
    read(gen%date_beg(1:4),*) ys; read(gen%date_beg(5:6),*) mns
    y = ys; m = mns
    do iper = 1, nper
      m = m + 1
      if (m > 12) then
        m = 1
        y = y + 1
      end if
      jd = get_jd(y, m, 1) - DONE
      call get_ymd_from_jd(jd, date, ye, me, de)
      write(datestr(iper),'(i4,2(a,i2.2))') ye, trim(datsep) , me, trim(datsep), de
    end do
    !
    if (associated(ts%glev)) then
      lwtd = .true.
    else
      lwtd = .false.
    end if
    !
    lwrite = .false.
    do il = gen%il_min, gen%il_max
      if ((ts%nod(il) > 0)) then
        if (gen%itype == 4) then 
          if ((ts%nlay > 1).and.(il == gen%il_min).and.(ts%sm_corr == 0)) then !corelation with soil moisture
            ts%nod(il) = 0
            cycle
          end if
          if (lwrite) then ! top only for itype = 4
            ts%nod(il) = 0 ! label nodes for lower layers zero
            cycle
          end if
        end if
        lwrite = .true.
        if ((gen%il_min == gen%il_max).or.(gen%itype == 4)) then
          laystr = ''
        else
          laystr = '_l'//ta((/il/))
        end if
        select case(gen%i_out)
          case(i_out_txt)
            f = trim(gen%out_dir)//trim(ts%id)//trim(laystr)//'.txt'
            call open_file(f, iu, 'w')
            if (lwtd) then
              s = 'date head wtd'
            else
              s = 'date head'
            end if
            call insert_tab(s)
            write(iu,'(a)') trim(s)
            do j = 1, nper
              head = ts%val(il,j)
              if (lwtd) then
                s = trim(datestr(j))//' '//ta((/head, ts%glev-head/))
              else
                s = trim(datestr(j))//' '//ta((/head/))
              end if
              call insert_tab(s)
              write(iu,'(a)') trim(s)
            end do
            close(iu)
          case(i_out_ipf)
            f = trim(gen%out_dir)//trim(ts%id)//trim(laystr)//'.txt'
            call open_file(f, iu, 'w')
            write(iu,'(a)') ta((/nper/))
            if (lwtd) then
              write(iu,'(a)') '3,1'
            else
              write(iu,'(a)') '2,1'
            end if
            write(iu,'(a)') 'date,-999999'
            write(iu,'(a)') 'head,-999999'
            if (lwtd) then
              write(iu,'(a)') 'wtd,-999999'
            end if
            do j = 1, nper
              head = ts%val(il,j)
              if (lwtd) then
                s = trim(datestr(j))//' '//ta((/head, ts%glev-head/))
              else
                s = trim(datestr(j))//' '//ta((/head/))
              end if
              write(iu,'(a)') trim(s)
            end do
            close(iu)
        end select
      end if
    end do
    !
    deallocate(datestr)
    return
  end subroutine mf6_post_ser_write_series
 
  subroutine mf6_post_ser_write_summary(this, lss)
! ******************************************************************************  
    ! -- arguments
    class(tPostSer) :: this
    logical, intent(in) :: lss
    ! --- local
    type(tTimeSeries), pointer :: ts => null()
    character(len=1) :: datsep
    character(len=3) :: ext
    character(len=mxslen) :: f, s, s1, s2, s3, laystr
    character(len=mxslen), dimension(:), allocatable :: datestr
    logical :: lipf, lwrite, lwtd
    logical, dimension(:), allocatable :: lfirst
    integer(i4b) :: iper, nper, i, j, il, jl, ys, mns, date, y, m, ye, me, de, iu
    integer(i4b) :: n
    integer(i4b), dimension(:), allocatable :: iuarr
    real(r8b) :: jd
    character(len=mxslen), dimension(:), allocatable :: sa
! --!----------------------------------------------------------------------------
    !
    nper =  this%gen%kper_end - this%gen%kper_beg + 1
    allocate(datestr(nper))
    !
    select case(this%gen%i_out)
      case(i_out_txt)
        datsep = '-'
        ext    = 'txt'
      case(i_out_ipf)
        datsep = ''
        ext    = 'ipf'
    end select
    !
    read(this%gen%date_beg(1:4),*) ys; read(this%gen%date_beg(5:6),*) mns
    y = ys; m = mns
    do iper = 1, nper
      m = m + 1
      if (m > 12) then
        m = 1
        y = y + 1
      end if
      jd = get_jd(y, m, 1) - DONE
      call get_ymd_from_jd(jd, date, ye, me, de)
      write(datestr(iper),'(i4,2(a,i2.2))') ye, trim(datsep) , me, trim(datsep), de
    end do
    !
    allocate(iuarr(gnlay), lfirst(gnlay))
    iuarr = 0; lfirst = .true.
    if (this%gen%itype == 4) then
      lfirst = .false.; lfirst(1) = .true.
      f = trim(this%gen%out_dir)//'summary.'//ext
      call open_file(f, iuarr(1), 'w')
    else
      if (this%gen%il_min == this%gen%il_max) then
        il = this%gen%il_min
        f = trim(this%gen%out_dir)//'summary.'//ext
        call open_file(f, iuarr(il), 'w')
      else
        do il = this%gen%il_min, this%gen%il_max
          f = trim(this%gen%out_dir)//'summary_l'//ta((/il/))//'.'//ext
          call open_file(f, iuarr(il), 'w')
        end do
      end if
    end if
    !
    select case(this%gen%i_out)
      case(i_out_txt)
        do i = 1, this%nts
          ts => this%ts(i)
          if (associated(ts%glev)) then
            lwtd = .true.
          else
            lwtd = .false.
          end if
          do il = this%gen%il_min, this%gen%il_max
            if (this%gen%itype == 4) then
              jl = 1
            else
              jl = il
            end if
            if (ts%act .and. (ts%nod(il) > 0)) then
              if (lfirst(jl)) then
                if (lss) then
                  write(iuarr(jl),'(a)') trim(ts%rawhdr)//' val'
                else
                  write(iuarr(jl),'(a)') trim(ts%rawhdr)
                end if
                lfirst(jl) = .false.
              end if
              if (lss) then
                if (lwtd) then
                  write(iuarr(jl),'(a)') trim(ts%raw)//' '//ta((/ts%glev-ts%val(il,1)/))
                else
                  write(iuarr(jl),'(a)') trim(ts%raw)//' '//ta((/ts%val(il,1)/))
                end if
              else
                write(iuarr(jl),'(a)') trim(ts%raw)
              end if
            end if
          end do
        end do
      case(i_out_ipf)
        n = 0
        do i = 1, this%nts
          ts => this%ts(i)
          do il = this%gen%il_min, this%gen%il_max
            if (ts%act .and. (ts%nod(il) > 0)) then
              n = n + 1
            end if
          end do
        end do
        write(iu,'(a)') ta((/n/))
        n = 0
        do i = 1, this%nts
          ts => this%ts(i)
          do il = this%gen%il_min, this%gen%il_max
            if (this%gen%itype == 4) then
              jl = 1
            else
              jl = il
            end if
            if (ts%act .and. (ts%nod(il) > 0)) then
              if (lfirst(jl)) then
                call parse_line(ts%rawhdr, sa)
                write(iuarr(jl),'(a)') ta((/size(sa)+3/))
                write(iuarr(jl),'(a)') 'x'
                write(iuarr(jl),'(a)') 'y'
                write(iuarr(jl),'(a)') 'model_results'
                do j = 1, size(sa)
                  write(iu,'(a)') '"'//trim(sa(j))//'"'
                end do
                write(iuarr(jl),'(a)') '3,txt'
                lfirst(jl) = .false.
              end if
              !
              call parse_line(ts%raw, sa)
              s1 = ta((/ts%x, ts%y/))
              s2 = trim(ts%id)//trim(laystr)
              s3 = ta(sa)
              write(iuarr(jl),'(a)') trim(s1)//' "'//trim(s2)//'" '//trim(s3)
            end if
          end do
        end do
    end select
    ! 
    do il = 1, gnlay
      if (iuarr(il) > 0) close(iuarr(il))
    end do
    !
    deallocate(datestr, iuarr, lfirst)
    if (allocated(sa)) deallocate(sa)
    !
    return
  end subroutine mf6_post_ser_write_summary
  !
  subroutine mf6_post_ser_clean(this)
! ******************************************************************************  
    ! -- arguments
    class(tPostSer) :: this
    ! --- local
    type(tMap), pointer :: map => null()
    integer(i4b) :: i
! --!----------------------------------------------------------------------------
    !
    if (associated(this%f_in)) deallocate(this%f_in)
    if (associated(this%ts)) then
      do i = 1, size(this%ts)
        call this%ts(i)%clean()
      end do
      deallocate(this%ts)
    end if
    if (associated(this%nts)) deallocate(this%nts)
    !
    if (associated(topmap)) then
      if (associated(topmap%maps)) then
        do i = 1, topmap%ntile
          map => topmap%maps(i)
          call map%clean()
        end do
        deallocate(topmap%maps)
      end if
      if (associated(topmap%idfs)) then
        call idfdeallocate(topmap%idfs, topmap%ntile) 
      end if
      deallocate(topmap%ntile)
      deallocate(topmap%tilebb)
      deallocate(topmap); topmap => null()
    end if
    if (associated(statmap)) then
      call statmap%clean()
      deallocate(statmap); statmap => null()
    end if
    !
    if (associated(this%gen)) then
      call mf6_post_clean_gen(this%gen)
      deallocate(this%gen)
      this%gen => null()
    end if
    !
    if (associated(this%mod)) then
      do i = 1, size(this%mod)
        call this%mod(i)%clean()
      end do
      deallocate(this%mod)
    end if
    if (associated(this%tsmod)) then
      do i = 1, size(this%tsmod)
        if (associated(this%tsmod(i)%nts))   deallocate(this%tsmod(i)%nts)
        if (associated(this%tsmod(i)%tsidx)) deallocate(this%tsmod(i)%tsidx)
      end do
      deallocate(this%tsmod)
    end if
    if (associated(this%writets)) deallocate(this%writets)
    !
    this%f_in    => null()
    this%ts      => null()
    this%nts     => null()
    this%mod     => null()
    this%tsmod   => null()
    this%writets => null()
    !
    return
  end subroutine mf6_post_ser_clean
  
! ==============================================================================
! subroutines/functions type tPostMod
! ==============================================================================
  
  subroutine mf6_post_mod_init(this, lbbonly_in)
! ******************************************************************************
    ! -- arguments
    class(tPostMod) :: this
    logical, intent(in), optional :: lbbonly_in
    ! --- local
    logical :: lex, lbbonly
    character(len=mxslen) :: f
! ------------------------------------------------------------------------------
    if (present(lbbonly_in)) then
      lbbonly = lbbonly_in
    else
      lbbonly = .false.
    end if
    !
    allocate(this%modname)
    write(this%modname,'(a,i5.5)') 'm', this%modid
    !
    call this%read_nodbin(lbbonly)
    !
    if (this%gen%top_tiled .and. .not.lbbonly) then
      call this%read_top_tiled()
    end if
    if (this%gen%top_mf6 .and. .not.lbbonly) then
      call this%read_top_mf6()
    end if
    !
    allocate(this%f); this%f = ''
    allocate(this%iu); this%iu = -1
    allocate(this%kper_read); this%kper_read = 0
    allocate(this%totim_read); this%totim_read = DZERO
    !
    if (this%nodes == 0) then
      return
    end if
    !
    ! open the hds file
    f = trim(this%gen%in_dir)//'models\run_output_bin\'// &
      trim(this%modname)//trim(this%gen%in_postf)
    call swap_slash(f)
    !
    inquire(file=f, exist=lex)
    if (lex) then
      this%f = f
      call open_file(f, this%iu, 'r', .true.)
    end if
    !
    return
  end subroutine mf6_post_mod_init
  
  subroutine mf6_post_mod_read_nodbin(this, lbbonly_in)
! ******************************************************************************
    ! -- arguments
    class(tPostMod) :: this
    logical, intent(in), optional :: lbbonly_in
    ! --- local
    type(tBb), pointer :: bb => null()
    logical :: lbbonly
    integer(i4b) :: iu, i, j
    character(len=mxslen) :: f
! ------------------------------------------------------------------------------
    if (present(lbbonly_in)) then
      lbbonly = lbbonly_in
    else
      lbbonly = .false.
    end if
      
    !if (this%modid == 372) then
    !  write(*,*) 'Break'
    !end if
    !
    f = trim(this%gen%in_dir)//'models\post_mappings\'//trim(this%modname)// &
      '.nodmap.bin'
    call swap_slash(f)
    !
    if (associated(this%bb)) deallocate(this%bb)
    allocate(this%bb); bb => this%bb
    if (associated(this%nodes)) deallocate(this%nodes)
    allocate(this%nodes); this%nodes = 0
    call open_file(f, iu, 'r', .true.)
    read(iu) bb%ic0, bb%ic1, bb%ir0, bb%ir1
    bb%ncol = bb%ic1 - bb%ic0 + 1; bb%nrow = bb%ir1 - bb%ir0 + 1
    !
    if (this%gen%gic0 > bb%ic1) lbbonly = .true.
    if (this%gen%gic1 < bb%ic0) lbbonly = .true.
    if (this%gen%gir0 > bb%ir1) lbbonly = .true.
    if (this%gen%gir1 < bb%ir0) lbbonly = .true.
    !
    if (lbbonly) then
      !call logmsg('***** skipping *****')
      close(iu)
      return
    end if
    read(iu) this%nodes
    if (associated(this%giliric)) deallocate(this%giliric)
    allocate(this%giliric(3,this%nodes))
    read(iu)((this%giliric(j,i),j=1,3),i=1,this%nodes)
    close(iu)
    !
    return
  end subroutine mf6_post_mod_read_nodbin
  
  subroutine mf6_post_mod_read_top_tiled(this)
! ******************************************************************************
    ! -- arguments
    class(tPostMod) :: this
    ! --- local
    type(tBb), pointer :: bb => null()
    type(tMap), pointer :: map => null()
    type(idfobj), pointer :: idf => null()
    logical :: linbb, lfound
    integer(i4b) :: i, j, gic, gir, ic, ir 
    real(r4b) :: r4val
    real(r8b) :: r8val
    logical :: lmv
! ------------------------------------------------------------------------------
    if (.not.associated(this%top)) then
      allocate(this%top(this%nodes))
    end if
    !
    do i = 1, this%nodes
      gir = this%giliric(2,i); gic = this%giliric(3,i)
      linbb = .false.; lfound = .false.
      do j = 1, topmap%ntile
        bb => topmap%tilebb(j)
        if ((bb%ir0 <= gir).and.(gir <= bb%ir1).and. &
            (bb%ic0 <= gic).and.(gic <= bb%ic1)) then
          linbb = .true.
        end if
        if (linbb) then
          ic = gic - bb%ic0 + 1; ir = gir - bb%ir0 + 1
          if (topmap%i_type == i_map) then
            map => topmap%maps(j)
            !if (.not.associated(map%r4mv)) then !!WORKAROUND!!
            !  allocate(map%r4mv)
            !  map%r4mv = r4mv
            !end if
            call map%get_val(ic, ir, r4val, lmv)
            if (.not.lmv) then
              lfound = .true.
              this%top(i) = real(r4val,r8b)
            else
              linbb = .false.
            end if
          else !idf
            idf => topmap%idfs(j)
            r8val = idfgetval(idf,ir,ic)
            if (r8val /= idf%nodata) then
              lfound = .true.
              !if (r8val /= DZERO) then !workaround
                this%top(i) = r8val
              !end if
            else
              linbb = .false.
            end if
          end if
        end if
        if (lfound) exit
      end do
      if (.not.lfound) then
        call errmsg('mf6_post_mod_read_top_tiled')
      end if
    end do
    !
    return
  end subroutine mf6_post_mod_read_top_tiled
!  
  subroutine mf6_post_mod_read_top_mf6(this)
! ******************************************************************************
    ! -- arguments
    class(tPostMod) :: this
    ! --- local
    ! mf6 header:
    character(len=16) :: text_in ! ulasav
    integer(i4b) :: kstp_in, kper_in, ncol_in, nrow_in, ilay_in
    real(r8b) :: pertim_in, totim_in
    !
    integer(i8b), parameter :: nbhdr = i4b + i4b + r8b + r8b + 16 + i4b + i4b + i4b
    !
    character(len=mxslen) :: f
    logical :: lex
    integer(i4b) :: iu, ios
    integer(i8b) :: kper_block, offset, hdr_pos, dat_pos
! ------------------------------------------------------------------------------
    if (.not.associated(this%top)) then
      allocate(this%top(this%nodes))
    end if
    !
    f = top
    call replacetoken(f, '?', this%modid)
    call swap_slash(f)
    !
    inquire(file=f, exist=lex)
    if (lex) then
      call open_file(f, iu, 'r', .true.)
    else
      call errmsg('Top file could not be found.')
    end if
    !
    kper_block = nbhdr + r8b*this%nodes
    offset = (top_kper - 1)*kper_block
    hdr_pos = offset + 1 
    dat_pos = offset + nbhdr + 1
    !
    ! read header
    read(unit=iu,iostat=ios,pos=hdr_pos+i4b) kper_in
    if (top_kper /= kper_in) then
      call errmsg('Invalid stress period reading for top.')
    end if
    !
    ! read heads
    call logmsg('Reading top for stress period '//ta((/top_kper/))//'...')
    read(unit=iu,iostat=ios,pos=dat_pos) this%top
    !
    close(iu)
    return
  end subroutine mf6_post_mod_read_top_mf6
    
  subroutine mf6_post_mod_read(this)
! ******************************************************************************
    ! -- arguments
    class(tPostMod) :: this
    ! --- local
    ! mf6 header:
    character(len=16) :: text_in ! ulasav
    integer(i4b) :: kstp_in, kper_in, ncol_in, nrow_in, ilay_in
    real(r8b) :: pertim_in, totim_in
    !
    integer(i8b), parameter :: nbhdr = i4b + i4b + r8b + r8b + 16 + i4b + i4b + i4b
    logical :: lop, lread_all
    integer(i4b) :: ios, i, kper, kper_beg, kper_end, nper, stat
    integer(i8b) :: ipos
    real(r8b), dimension(:), allocatable :: r8wk
! ------------------------------------------------------------------------------
    !
    inquire(unit=this%iu, opened=lop)
    if (.not.lop) return
    !
    if ((this%gen%itype >= 3)) then
      lread_all = .true.
      kper_beg = this%gen%kper_beg
      kper_end = this%gen%kper_end
      nper = kper_end - kper_beg + 1
      ipos = (kper_beg - 1)*(nbhdr + r8b*this%nodes) + 1
      write(unit=this%iu, pos=ipos, iostat=stat)
      inquire(unit=this%iu, pos=ipos)
    else
      lread_all = .false.
      kper_beg = 1
      kper_end = 1
    end if
    !
    if (this%gen%year_avg) then
      kper_beg = 1
      kper_end = 12
      nper = kper_end - kper_beg + 1
    end if
    !
    kper = 0
    do kper = kper_beg, kper_end
      read(unit=this%iu,iostat=ios) kstp_in, kper_in, pertim_in, totim_in, &
        text_in, ncol_in, nrow_in, ilay_in
      !
      if (ios /= 0) then
        this%totim_read = -abs(this%totim_read)
        return
      end if
      !
      ! check
      if (ncol_in /= this%nodes) then
        call errmsg('Invalid number of nodes reading for model '// &
          trim(this%modname)//'.')
      end if
      if (lread_all .and. (kper_in /= kper)) then
        call errmsg('invalid kper')
      end if
      !
      this%kper_read  = kper_in
      this%totim_read = totim_in
      !
      if (lread_all) then
        if(.not.associated(this%r8buff2d)) then
          allocate(this%r8buff2d(nper,this%nodes))
        end if
        read(unit=this%iu,iostat=ios)(this%r8buff2d(kper-kper_beg+1,i),i=1,this%nodes)
      else
        if (.not.associated(this%r8buff)) then
          allocate(this%r8buff(this%nodes))
        end if
        if (this%gen%year_avg) then
          if (kper == kper_beg) then
            do i = 1, this%nodes
              this%r8buff(i) = DZERO
            end do
          end if
          if (.not.allocated(r8wk)) then
            allocate(r8wk(this%nodes))
          end if
          read(unit=this%iu,iostat=ios)(r8wk(i),i=1,this%nodes)
          do i = 1, this%nodes
            this%r8buff(i) = this%r8buff(i) + r8wk(i)
          end do
        else
          read(unit=this%iu,iostat=ios)(this%r8buff(i),i=1,this%nodes)
        end if
      end if
      !
      if (ios /= 0) then
        call errmsg('Could not read data.')
      end if
      !
    end do
    !
    if (this%gen%year_avg) then
      do i = 1, this%nodes
        this%r8buff(i) = this%r8buff(i)/nper
      end do
      if (allocated(r8wk)) deallocate(r8wk)
    end if
    !
    return
  end subroutine mf6_post_mod_read
  
  subroutine mf6_post_mod_read_nod(this, nod, r8v1d, lcheck)
! ******************************************************************************
    ! -- arguments
    class(tPostMod) :: this
    integer(i4b), intent(in) :: nod
    real(r8b), dimension(:), intent(out) :: r8v1d
    logical, intent(in), optional :: lcheck
    ! --- local
    ! mf6 header:
    character(len=16) :: text_in ! ulasav
    integer(i4b) :: kstp_in, kper_in, ncol_in, nrow_in, ilay_in
    real(r8b) :: pertim_in, totim_in
    !
    integer(i8b), parameter :: nbhdr = i4b + i4b + r8b + r8b + 16 + i4b + i4b + i4b
    logical :: lop, lchk
    character(len=mxslen) :: f
    integer(i4b) :: ios, kper, jper, kper_beg, kper_end, nper, stat
    integer(i8b) :: offset, kper_block
    integer(i8b), dimension(:), allocatable :: hdr_pos, dat_pos
! ------------------------------------------------------------------------------
    !
    if (present(lcheck)) then
      lchk = lcheck
      !call logmsg('**** start 1 mf6_post_mod_read_nod ****')
    else
      lchk = .false.
      !call logmsg('**** start 2 mf6_post_mod_read_nod ****')
    end if
    
    inquire(unit=this%iu, opened=lop)
    if (.not.lop) return
    !
    !close(this%iu)
    !call open_file(this%f, this%iu, 'r', .true.)
    !rewind(this%iu)
    !write(unit=this%iu, pos=1, iostat=stat)
    !
    kper_beg = this%gen%kper_beg
    kper_end = this%gen%kper_end
    nper = kper_end - kper_beg + 1

    kper_block = nbhdr + r8b*this%nodes
    offset = (kper_beg - 1)*kper_block
    !
    ! fill the positions
    allocate(hdr_pos(nper), dat_pos(nper))
    
    do kper = 1, nper
      hdr_pos(kper) = offset + (kper-1)*kper_block + 1 
      dat_pos(kper) = offset + (kper-1)*kper_block + nbhdr + (nod-1)*r8b + 1
    end do
    !call logmsg('first pos '//ta((/dat_pos(1)/)))
    !
    !call logmsg('**** start mf6_post_mod_read_nod ****')
    !call logmsg('*** nper '//ta((/nper/)))
    
    jper = 0
    do kper = kper_beg, kper_end
      jper = jper + 1
      if (lchk) then
        ! kstp_in, kper_in, pertim_in, totim_in, text_in, ncol_in, nrow_in, ilay_in
        read(unit=this%iu,iostat=ios,pos=hdr_pos(jper)+i4b) kper_in
        if (kper /= kper_in) then
          call errmsg('Invalid stress period reading for model '// &
            trim(this%modname)//'.')
        end if
      end if
      !call logmsg('kper = '//ta((/kper/))//' pos '//ta((/dat_pos(jper)/)))
      read(unit=this%iu,iostat=ios,pos=dat_pos(jper)) r8v1d(jper)
      if (ios /= 0) then
        call errmsg('Could not read file')
      end if
    end do
    !
    deallocate(hdr_pos, dat_pos)
    !
    !call logmsg('**** end mf6_post_mod_read_nod ****')
    
    return
  end subroutine mf6_post_mod_read_nod
  
  subroutine mf6_post_mod_calc_slope(this)
! ******************************************************************************
    ! -- arguments
    class(tPostMod) :: this
    ! --- local
    integer(i4b) :: i, j, jj, k, nper, nyear, myear, syear
    integer(i4b), parameter :: monthyear = 12
    real(r8b) :: slope, yint, corr
    real(r8b), dimension(:), allocatable :: r8wk, havg
! ------------------------------------------------------------------------------
    !
    if (.not.associated(this%r8buff)) then
      allocate(this%r8buff(this%nodes))
    end if
    !
    nper = size(this%r8buff2d,1)
    !
    if (mod(nper,monthyear) /= 0) then
      call errmsg('The number of output stress periods should be a multiple of 12.')
    end if
    !
    nyear = nper / monthyear
    !
    call logmsg('Computing slope starting from '//ta((/this%gen%year_beg/))//'...')
    read(this%gen%date_beg(1:4),*) syear
    syear = this%gen%year_beg - syear + 1
    myear = nyear - syear + 1
    allocate(r8wk(myear), havg(myear))
    do i = 1, myear
      r8wk(i) = real(i,r8b)
    end do
    !
    do i = 1, this%nodes
      do j = 1, myear
        havg(j) = DZERO
      end do
      k = 0
      do j = 1, nyear
        do jj = 1, monthyear
          k = k + 1
          if (j >= syear) then
            havg(j-syear+1) = havg(j-syear+1) + this%r8buff2d(k,i)
          end if
        end do
      end do
      do j = 1, myear
        havg(j) = havg(j) / monthyear
      end do
      call linear_regression(r8wk, havg, slope, yint, corr)
      this%r8buff(i) = slope
    end do
    !
    deallocate(r8wk, havg)
    deallocate(this%r8buff2d)
    this%r8buff2d => null()
    !
    return
  end subroutine mf6_post_mod_calc_slope
  
  subroutine mf6_post_mod_calc_average(this)
! ******************************************************************************
    ! -- arguments
    class(tPostMod) :: this
    ! --- local
    integer(i4b) :: i, j, n
    real(r8b) :: sumhead, sumwtd
! ------------------------------------------------------------------------------
    !
    if (.not.associated(this%r8buff)) then
      allocate(this%r8buff(this%nodes))
    end if
    !
    n = size(this%r8buff2d,1)
    !
    if (associated(this%top)) then ! water table depths
      do i = 1, this%nodes ! loop over nodes
        sumwtd = DZERO
        do j = 1, n ! loop over periods
          sumwtd = sumwtd + (this%top(i) - this%r8buff2d(j,i))
        end do
        this%r8buff(i) = sumwtd / n
      end do
    else ! heads
      do i = 1, this%nodes ! loop over nodes
        sumhead = DZERO ! loop over periods
        do j = 1, n
          sumhead = sumhead + this%r8buff2d(j,i)
        end do
        this%r8buff(i) = sumhead / n
      end do
    end if
    !
    deallocate(this%r8buff2d)
    this%r8buff2d => null()
    !
    return
  end subroutine mf6_post_mod_calc_average
  
  subroutine mf6_post_mod_calc_iqr(this)
! ******************************************************************************
    ! -- arguments
    class(tPostMod) :: this
    ! --- local
    integer(i4b) :: i, j, d, k, k1, k2, nper, mper, n
    integer(i4b) :: kper_beg, y, m, ys, mns, iper
    integer(i4b), dimension(:), allocatable :: i4wk
    real(r8b) :: q1, q3, iqr
    real(r8b), dimension(:), allocatable :: r8wk
! ------------------------------------------------------------------------------
    !
    if (.not.associated(this%r8buff)) then
      allocate(this%r8buff(this%nodes))
    end if
    !
    call logmsg('Computing IQR starting from '//ta((/this%gen%year_beg/))//'...')
    nper = this%gen%kper_end - this%gen%kper_beg + 1
    read(this%gen%date_beg(1:4),*) ys; read(this%gen%date_beg(5:6),*) mns
    y = ys; m = mns
    do iper = 1, nper
      if (m > 12) then
        m = 1
        y = y + 1
      end if
      if (y == this%gen%year_beg) then
        kper_beg = iper
        exit
      end if
      m = m + 1
    end do
    !
    mper = nper - kper_beg + 1
    allocate(i4wk(mper), r8wk(mper))
    !
    do i = 1, this%nodes ! loop over nodes
      ! fill the buffer
      do j = 1, mper ! loop over periods
        iper = j + kper_beg - 1
        i4wk(j) = j
        r8wk(j) = this%r8buff2d(iper,i)
      end do
      call quicksort_d(r8wk, i4wk, mper)
      !
      if (mod(mper,2) == 0) then  !even
        n = mper / 2
      else ! odd
        n = (mper - 1)/2
      end if
      !
      if (mod(n,2) == 0) then ! even
        d = n/2
        k1 = d; k2 = k1 +1
        q1 = (r8wk(k1) + r8wk(k2))/2.d0
        k1 = mper - d; k2 = k1 + 1
        q3 = (r8wk(k1) + r8wk(k2))/2.d0
      else ! odd
        d = (n-1)/2
        k1 = d + 1; k2 = mper - d
        q1 = r8wk(k1)
        q3 = r8wk(k2)
      end if
      iqr = q3 - q1
      this%r8buff(i) = iqr
    end do
    !
    deallocate(i4wk, r8wk)
    deallocate(this%r8buff2d)
    this%r8buff2d => null()
    !
    return
  end subroutine mf6_post_mod_calc_iqr
!  
  function mf6_post_get_out_pref(name, totim, il, gen) result(f)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: name
    real(r8b), intent(in) :: totim
    integer(i4b), intent(in) :: il
    type(tGen), pointer, intent(in) :: gen
    character(len=:), allocatable :: f
    ! --- local
    character(len=1) :: us
    character(len=mxslen) :: ts
    integer(i4b) :: y, m, d, idum
    real(r8b) :: jd
! ------------------------------------------------------------------------------
    read(sdate(1:4),*) y; read(sdate(5:6),*) m!; read(sdate(7:8),*) d
    d = 1
    jd = get_jd(y, m, d) + totim - DONE
    call get_ymd_from_jd(jd, idum, y, m, d)
    ts = ta((/y/))//ta((/m/),'(i2.2)')//ta((/d/),'(i2.2)')
    !
    if (len_trim(name) == 0) then
      us = ''
    else
      us = '_'
    end if
    !
    if (il /= 0) then
      f = trim(gen%out_dir)//trim(name)//trim(us)//trim(gen%out_pref)// &
        trim(ts)//'_l'//ta((/il/))
    else
      f = trim(gen%out_dir)//trim(name)//trim(us)//trim(gen%out_pref)// &
        trim(ts)
    end if
    !
    call swap_slash(f)
    return
  end function mf6_post_get_out_pref
  !
  function mf6_post_mod_get_data(this, il, ldummy) result(r8x)
! ******************************************************************************
    ! -- arguments
    class(tPostMod) :: this
    integer(i4b), intent(in) :: il
    real(r8b), dimension(:,:), allocatable :: r8x
    logical, intent(in), optional :: ldummy
    ! --- local
    type(tGen), pointer :: gen => null()
    type(tBb), pointer :: bb => null()
    logical :: ldum, ladd
    integer(i4b) :: gil, gir, gic, ir, ic, i, nadd
    integer(i4b), dimension(:,:), allocatable :: laytop
    
! ------------------------------------------------------------------------------
    ldum = .false.
    if (present(ldummy)) then
      ldum = ldummy
    end if
    !
    bb => this%bb; gen => this%gen
    !
    if (allocated(r8x)) deallocate(r8x)
    allocate(r8x(bb%ncol,bb%nrow), laytop(bb%ncol,bb%nrow))
    !
    do ir = 1, bb%nrow
      do ic = 1, bb%ncol
        r8x(ic,ir) = r8nodata
        laytop(ic,ir) = 0
      end do
    end do
    !
    nadd = 0
    do i = 1, this%nodes
      gil = this%giliric(1,i); gir = this%giliric(2,i); gic = this%giliric(3,i) ! global
      if (include_sea) gil = abs(gil)
      ir = gir - bb%ir0 + 1; ic = gic - bb%ic0 + 1 ! local
      !check
      if ((ir < 1).or.(ir > bb%nrow).or.(ic < 1).or.(ic > bb%ncol)) then
        call errmsg('mf6_post_mod_write')
      end if
      ladd = .true.
      if (il == 0) then
        if (laytop(ic,ir) == 1) then
          ladd = .false.
        else
          laytop(ic,ir) = 1
        end if
      else
        if (gil /= il) ladd = .false.
      end if
      if (ladd) then
        nadd = nadd + 1
        if (.not.ldum) then
          if ((gen%top_tiled).or.(gen%top_mf6)) then
            r8x(ic,ir) = this%top(i) - this%r8buff(i) !Water table depth > 0!
          else
            r8x(ic,ir) = this%r8buff(i)
          end if
        else
          !write(*,*) 'ic, ir', ic, ir
          r8x(ic,ir) = DONE
        end if
      end if
    end do
    !
    !write(*,*) '@@@ nadd =',nadd
    ! clean up
    deallocate(laytop)
    !
    return
  end function mf6_post_mod_get_data
  
  subroutine mf6_post_mod_write(this)
! ******************************************************************************
    ! -- arguments
    class(tPostMod) :: this
    ! --- local
    type(tGen), pointer :: gen => null()
    type(tBb), pointer :: bb => null()
    character(len=mxslen) :: f
    integer(i4b) :: il 
    real(r8b) :: xmin, ymin, cs
    real(r8b), dimension(:,:), allocatable :: r8wk, r8wks
! ------------------------------------------------------------------------------
    !
    bb => this%bb; gen => this%gen
    !
    do il = gen%il_min, gen%il_max
      r8wk = this%get_data(il)
      !
      ! write the file
      f = mf6_post_get_out_pref(this%modname, this%totim_read, il, this%gen)
      !
      xmin = gxmin + (bb%ic0-1)*gcs
      ymin = gymin + (gnrow-bb%ir1)*gcs
      !
      select case(gen%i_out)
      case(i_out_asc)
          call writeasc(trim(f)//'.asc',r8wk, bb%ncol, bb%nrow, &
            xmin, ymin, gcs, r8nodata)
        case(i_out_idf)
          call writeidf(trim(f)//'.idf', r8wk, bb%ncol, bb%nrow, &
            xmin, ymin, gcs, r8nodata)
        case(i_out_flt)
          call writeflt(trim(f), r8wk, bb%ncol, bb%nrow, &
            xmin, ymin, gcs, r8nodata)
        case(i_out_flts) ! scale factor 10; average
          call r8_scale_avg(r8wk, r8nodata, bb, r8wks, xmin, ymin, cs)
          call writeflt(trim(f), r8wks, size(r8wks,1), size(r8wks,2), xmin, ymin, cs, r8nodata)
          deallocate(r8wks)
      end select
    end do
    !
    deallocate(r8wk, this%r8buff)
    !
    return
  end subroutine mf6_post_mod_write
  
  subroutine mf6_post_mod_clean(this)
! ******************************************************************************
    ! -- arguments
    class(tPostMod) :: this
    ! --- local
    logical :: lop
! ------------------------------------------------------------------------------
    if (associated(this%modid))     deallocate(this%modid)
    if (associated(this%modname))   deallocate(this%modname)
    if (associated(this%f))         deallocate(this%f)
    if (associated(this%iu)) then
      inquire(unit=this%iu, opened=lop)
      if (lop) close(this%iu)
      deallocate(this%iu)
    end if
    if (associated(this%bb))         deallocate(this%bb)
    if (associated(this%nodes))      deallocate(this%nodes)
    if (associated(this%giliric))    deallocate(this%giliric)
    if (associated(this%kper_read))  deallocate(this%kper_read)
    if (associated(this%totim_read)) deallocate(this%totim_read)
    if (associated(this%r8buff))     deallocate(this%r8buff)
    if (associated(this%r8buff2d))   deallocate(this%r8buff2d)
    if (associated(this%top))        deallocate(this%top)
    !
    this%modid      => null()
    this%modname    => null()
    this%f          => null()
    this%iu         => null()
    this%bb         => null()
    this%nodes      => null()
    this%giliric    => null()
    this%kper_read  => null()
    this%totim_read => null()
    this%r8buff     => null()
    this%top        => null()
    this%gen        => null()
    !
    return
  end subroutine mf6_post_mod_clean
  
! ==============================================================================
! subroutines/functions type tPostSol
! ==============================================================================
  
  subroutine mf6_post_sol_init(this, sa)
! ******************************************************************************
    ! -- arguments
    class(tPostSol) :: this
    character(len=*), dimension(:), intent(in) :: sa
    ! --- local
    type(tGen), pointer :: gen => null()
    type(tPostMod), pointer :: m => null()
    type(tBb), pointer :: bb => null(), sbb => null(), mbb => null()
    type(tBin), pointer :: bin => null()
    logical :: lok, lex
    character(len=mxslen) :: f, st, s
    integer(i4b) :: i, j, iu, na, ys, mns, y, mn, kper_beg, kper_end, ir, ic
    integer(i4b) :: ic0, ic1, ir0, ir1, ios, iact
    integer(i4b), dimension(:), allocatable :: i4wk
    real(r8b) :: xmin, xmax, ymin, ymax, r8v
! ------------------------------------------------------------------------------
    !
    !
    ! 1          2 3 4       5 6 7 8   9                                    10
    ! .\mada_ss\ m 3 .ss.hds 1 2 1 idf .\mada_ss\models\run_output_bin_idf\ head_ss_
    
    !call parse_line(s, sa); 
    na = size(sa)
    !i = 1; f = s
    !do i = 1, 12
    !  f = adjustl(f)
    !  j = index(f,' ')
    !  sa(i) = f(1:j-1)
    !  f = adjustl(f(j:))
    !end do
!    sa(12) = f(1:len_trim(f))
    
    !read(s,*)(sa(i),i=1,10)
    !
    allocate(this%gen); gen => this%gen
    allocate(gen%in_dir);   gen%in_dir = sa(1)
    allocate(gen%in_postf); gen%in_postf = sa(4)
    allocate(gen%itype); read(sa(5),*) gen%itype
    allocate(gen%il_min); read(sa(6),*) gen%il_min
    allocate(gen%il_max); read(sa(7),*) gen%il_max
    allocate(gen%date_beg); read(sa(8),*) gen%date_beg
    allocate(gen%date_end)
    allocate(gen%year_avg)
    gen%year_avg = .false.
    if (gen%itype < 0) then
      gen%year_avg = .true.
      gen%itype = -gen%itype
    end if
    if (gen%itype == 2) then ! statistics for on1y 1 time step
      gen%date_end = gen%date_beg
    else
      read(sa(9),*) gen%date_end
    end if
    !
    read(sdate(1:4),*) ys; read(sdate(5:6),*) mns
    read(gen%date_beg(1:4),*) y; read(gen%date_beg(5:6),*) mn
    kper_beg = y*12 + (mn -1 ) - (ys*12 + mns - 1) + 1
    read(gen%date_end(1:4),*) y; read(gen%date_end(5:6),*) mn
    kper_end = y*12 + (mn - 1) - (ys*12 + mns - 1) + 1
    !
    allocate(gen%year_beg)
    gen%year_beg = ys
    if (na == 13) then
      read(sa(13),*) gen%year_beg
    end if
    !
    allocate(gen%kper_beg); gen%kper_beg = kper_beg
    allocate(gen%kper_end); gen%kper_end = kper_end
    allocate(gen%i_out)
    select case(trim(sa(10)))
      case('asc')
        gen%i_out = i_out_asc
      case('idf')
        gen%i_out = i_out_idf
      case('flt')
        gen%i_out = i_out_flt
      case('flts')
        gen%i_out = i_out_flts
      case default
        call errmsg('mf6_post_sol_init')
    end select
    allocate(gen%out_dir); gen%out_dir = sa(11)
    allocate(gen%out_pref); gen%out_pref = sa(12)
    allocate(gen%gic0); gen%gic0 = 1
    allocate(gen%gic1); gen%gic1 = gncol
    allocate(gen%gir0); gen%gir0 = 1
    allocate(gen%gir1); gen%gir1 = gnrow
    allocate(gen%lwritebb); gen%lwritebb = .false. 
    if (na == 16) then
      gen%lwritebb = .true.
      read(sa(13:),*) xmin, xmax, ymin, ymax
      if (xmin > xmax) call errmsg('xmin > xmax')
      if (ymin > ymax) call errmsg('ymin > ymax')
      gen%gic0 = int((xmin - gxmin)/gcs, i4b) + 1;     gen%gic0 = max(1, gen%gic0); gen%gic0 = min(gncol, gen%gic0)
      gen%gic1 = gen%gic0 + int((xmax - xmin)/gcs);    gen%gic1 = max(1, gen%gic1); gen%gic1 = min(gncol, gen%gic1)
      gen%gir1 = gnrow - int((ymin - gymin)/gcs, i4b); gen%gir1 = max(1, gen%gir1); gen%gir1 = min(gnrow, gen%gir1)
      gen%gir0 = gen%gir1 - int((ymax - ymin)/gcs);    gen%gic0 = max(1, gen%gic0); gen%gir0 = min(gnrow, gen%gir0)
    end if
    
    allocate(gen%top_tiled, gen%top_mf6)
    !
    gen%top_tiled = .false.
    gen%top_mf6 = .false.
    select case(gen%itype)
    case(1, 7)
      if (top_type == i_tiled) then
        gen%top_tiled = .true.
        allocate(topmap)
        call open_file(tilebb, iu, 'r')
        allocate(topmap%ntile)
        read(iu,*) topmap%ntile
        allocate(topmap%tilebb(topmap%ntile))
        do i = 1, topmap%ntile
          bb => topmap%tilebb(i)
          read(iu,*) bb%ic0, bb%ic1, bb%ir0, bb%ir1
          bb%ncol = bb%ic1 - bb%ic0 + 1
          bb%nrow = bb%ir1 - bb%ir0 + 1
        end do
        close(iu)
        allocate(topmap%i_type)
        if (index(top,'.map',back=.true.) > 0) then
          topmap%i_type = i_map
        else if (index(top,'.idf',back=.true.) > 0) then
          topmap%i_type = i_idf
        else
          call errmsg("Unrecognize top file format.")
        end if
        if (topmap%i_type == i_map) then
          allocate(topmap%maps(topmap%ntile))
        else
          allocate(topmap%idfs(topmap%ntile))
        end if
        do i = 1, topmap%ntile
          f = top
          call replacetoken(f, '?', i)
          call swap_slash(f)
          inquire(file=f,exist=lex)
          if (.not.lex) then
            call logmsg("Could not find "//trim(f))
          else
            if (topmap%i_type == i_map) then
              lok = topmap%maps(i)%init(f, 1)
            else
              lok = idfread(topmap%idfs(i), f, 0)
            end if
            if (.not.lok) then
              call errmsg('Initializing top.')
            end if
          end if
        end do
      else
        if (top_type /= i_mf6) then
          call errmsg('Top was not enabled.')
        end if
        gen%top_mf6 = .true.
      end if
    case(2)
      f = sa(9)
      call open_file(f, iu, 'r')
      read(iu,'(a)') f
      allocate(statmap)
      lok = statmap%init(f)
      call statmap%set_nodata()
      allocate(gen%nbin); gen%nbin = 0
      do iact = 1, 2
        rewind(iu); read(iu,'(a)') f
        gen%nbin = 0
        do while(.true.)
          read(unit=iu,fmt='(a)',iostat=ios) s
          if (ios /= 0) exit
          if (len_trim(s) == 0) cycle
          gen%nbin = gen%nbin + 1
          if (iact == 2) then
            read(s,*) r8v
            gen%bins(gen%nbin)%max   = r8v
            gen%bins(gen%nbin+1)%min = r8v
          end if
        end do
        gen%nbin = gen%nbin + 1
        if (iact == 1) then
          allocate(gen%bins(gen%nbin))
        end if
      end do
      close(iu)
      !
      ! set the labels
      do i = 1, gen%nbin
        bin => gen%bins(i)
        if (i == 1) then
          bin%id = 'le-'//trim(adjustl(ta((/bin%max/),'(f10.2)')))
        else if(i == gen%nbin) then
          bin%id = 'gt-'//trim(adjustl(ta((/bin%min/),'(f10.2)')))
        else
          bin%id = trim(adjustl(ta((/bin%min/),'(f10.2)')))//'-'//&
                   trim(adjustl(ta((/bin%max/),'(f10.2)')))
        end if
      end do
    end select
    !
    allocate(this%lwritesol, this%lwritemodid, this%lmask, this%nmod)
    this%lwritemodid = .false.
    this%lmask = .false.
    select case(trim(sa(2)))
      case('m')
        this%lwritesol = .false.
        this%nmod = 1
        allocate(this%mod(1))
        m => this%mod(1)
        allocate(m%modid); read(sa(3),*) m%modid
      case('s','smodid')
        this%lwritesol = .true.
        if (trim(sa(2)) == 'smodid') then !solution + tile
          this%lwritemodid = .true.
        end if
        allocate(this%solid, this%solname)
        read(sa(3),*) this%solid
        write(this%solname,'(a,i2.2)') 's',this%solid
        f = trim(gen%in_dir)//'solutions\post_mappings\'//trim(this%solname)//'.modmap.bin'
        call swap_slash(f)
        call chkexist(f)
        !
        call open_file(f, iu, 'r', .true.)
        read(iu) this%nmod
        allocate(this%mod(this%nmod))
        allocate(i4wk(this%nmod))
        read(iu)(i4wk(i),i=1,this%nmod)
        do i = 1, this%nmod
          m => this%mod(i)
          allocate(m%modid); m%modid = i4wk(i)
        end do
        deallocate(i4wk)
        close(iu)
      case('r')
        read(sa(3),*) f
        call this%init_select(f)
      case default
        call errmsg('mf6_post_sol_init')
      end select
    !
    ! initialize the models
    do i = 1, this%nmod
      m => this%mod(i)
      m%gen => this%gen
      call m%init()
    end do
    !
    ! check
    do i = 1, this%nmod
      if ((this%mod(i)%iu > 0).and.(this%mod(i)%nodes == 0)) then
        call errmsg('Program error')
      end if
    end do
    !
    ! determine the bounding box
    allocate(this%bb); sbb => this%bb
    do i = 1, this%nmod
      if (this%mod(i)%nodes == 0) cycle
      mbb => this%mod(i)%bb
      sbb%ic0 = min(sbb%ic0, mbb%ic0)
      sbb%ic1 = max(sbb%ic1, mbb%ic1)
      sbb%ir0 = min(sbb%ir0, mbb%ir0)
      sbb%ir1 = max(sbb%ir1, mbb%ir1)
    end do
    sbb%ncol = sbb%ic1 - sbb%ic0 + 1
    sbb%nrow = sbb%ir1 - sbb%ir0 + 1
    !
    return
  end subroutine mf6_post_sol_init
!  
 subroutine mf6_post_sol_init_select(this, fsel)
! ******************************************************************************
    ! -- arguments
    class(tPostSol) :: this
    character(len=mxslen), intent(inout) :: fsel
    ! --- local
    type tSel
      logical   :: lact = .false.
      type(tBB), pointer :: bb => null()
    end type tSel
    !
    type(tSel), dimension(:), pointer :: selbb => null()
    type(tPostMod), dimension(:), pointer :: selmod => null()
    type(tPostMod), pointer :: m => null()
    type(tBB), pointer :: bb => null(), sbb => null(), mbb => null()
    type(tGen), pointer :: gen => null()
    type(tMap), pointer :: selmap => null()
    !
    character(len=mxslen) :: f, d, s, s1, s2, fmap, fcsv, objcolstr, solname
    character(len=mxslen), dimension(:), allocatable :: sa
    character(len=mxslen), dimension(:,:), allocatable :: sel
    logical :: lfound, loverlap, lok
    integer(i4b) :: iu, iucsv, iact, nsel, ios, nccsv, i, j, n, objid, nmod, isol
    integer(i4b) :: mxobjid, objcol, gic0col, gic1col, gir0col, gir1col 
    integer(i4b), dimension(:), allocatable :: actcol, curcol, i4wk, i4wk2
    integer(i4b), dimension(:,:), allocatable :: selcol, i4wk2d
    integer(i4b) :: ic, ir, jc, jr, kc, kr, nc, nr
! ------------------------------------------------------------------------------
    gen => this%gen
    this%lwritesol = .true.
    this%lmask = .false.
    allocate(this%solid, this%solname)
    !
    call open_file(fsel, iu, 'r')
    do iact = 1, 2
      read(unit=iu,fmt=*,iostat=ios) this%solid
      read(unit=iu,fmt=*,iostat=ios) fmap, objcolstr
      objcolstr = change_case(objcolstr,'l')
      read(unit=iu,fmt=*,iostat=ios) fcsv
     
      ! open the CSV file and read header
      if (iact == 1) then
        call open_file(fcsv, iucsv, 'r')
        read(unit=iucsv,fmt='(a)',iostat=ios) s
        call parse_line(s, sa, ','); nccsv = size(sa)-4
        allocate(curcol(nccsv+4))
        curcol = 0
        n = 0
        do i = 1, nccsv+4
          sa(i) = change_case(sa(i),'l')
          if (sa(i) == objcolstr) then
            objcol = i; n = n + 1
          end if
          if (sa(i) == 'gic0') then
            gic0col = i; n = n + 1
          end if
          if (sa(i) == 'gic1') then
            gic1col = i; n = n + 1
          end if
          if (sa(i) == 'gir0') then
            gir0col = i; n = n + 1
          end if
          if (sa(i) == 'gir1') then
            gir1col = i;  n = n + 1
          end if
        end do
        ! checks
        if (n /= 5) then
          call errmsg("Could not parse csv header file.")
        end if
      end if
      !
      nsel = 0
      do while (.true.)
        read(unit=iu,fmt='(a)',iostat=ios) s
        if (ios /= 0) exit
        if (len_trim(s) == 0) cycle
        if (s(1:1) == comment) cycle
        nsel = nsel + 1
        if (iact == 2) then
          call parse_line(s, sa, ',')
          if (size(sa) /= nccsv) then
            call errmsg("Invalid selection")
          end if
          do i = 1, nccsv
            sel(i,nsel) = change_case(sa(i),'l')
            if (len_trim(sel(i,nsel)) > 0) then
              selcol(i,nsel) = 1
            end if
          end do
        end if
      end do
      if (iact == 1) then
        if (nsel == 0) then
          call errmsg("No selections found")
        end if
        allocate(sel(nccsv,nsel),selcol(nccsv,nsel))
        selcol = 0
        rewind(iu)
      end if
    end do
    close(iu)
    !
    ! maximum number of ids
    do iact = 1, 2
      mxobjid = 0
      rewind(iucsv); read(unit=iucsv,fmt='(a)',iostat=ios) s
      if (ios /= 0) call errmsg("Could not read "//trim(s))
      do while(.true.)
        read(unit=iucsv,fmt='(a)',iostat=ios) s
        if (ios /= 0) exit
        if (len_trim(s) == 0) cycle
        call parse_line(s, sa, ',')
        do i = 1, size(sa)
          sa(i) = change_case(sa(i),'l')
        end do
        read(sa(1),fmt=*,iostat=ios) objid
        if (ios /= 0) call errmsg("Could not read "//trim(sa(1)))
        mxobjid = max(objid, mxobjid)
        if (iact == 2) then
          do i = 1, nsel
            curcol = 0
            do j = 1, nccsv
              if (sa(j) == sel(j,i)) then
                if (len_trim(sel(j,i)) > 0) then
                  curcol(j) = 1
                end if
              end if
            end do
            lfound = .true.
            do j = 1, nccsv
              if (selcol(j,i) > 0) then
                if (curcol(j) == 0) then
                  lfound = .false.
                end if
              end if
            end do
            if (lfound) exit
          end do
          if (lfound) then
            call logmsg("Found: "//trim(s)//"...")
            selbb(objid)%lact = .true.
            allocate(selbb(objid)%bb)
            sbb => selbb(objid)%bb
            read(sa(gic0col),*) sbb%ic0
            read(sa(gic1col),*) sbb%ic1
            read(sa(gir0col),*) sbb%ir0
            read(sa(gir1col),*) sbb%ir1
          end if
        end if
      end do
      if (iact == 1) then
        allocate(selbb(mxobjid))
      end if
    end do
    close(iucsv)
    !
    d = trim(gen%in_dir)//'models\post_mappings\'
    !
    if (this%solid == 0) then ! loop over all solutions
      do iact = 1, 2
        nmod = 0
        do isol = 1, gnsol
          write(solname,'(a,i2.2)') 's',isol
          f = trim(gen%in_dir)//'solutions\post_mappings\'//trim(solname)//'.modmap.bin'
          call open_file(f, iu, 'r', .true.)
          read(iu) n
          allocate(i4wk2(n))
          read(iu)(i4wk2(i),i=1,n)
          do i = 1, n
            nmod = nmod + 1
            if (iact == 2) then
              m => selmod(nmod)
              allocate(m%modid); m%modid = i4wk2(i)
              m%gen => this%gen
              call m%init(.true.)
              i4wk(nmod) = 0
            end if
          end do
          close(iu); deallocate(i4wk2)
        end do
        if (iact == 1) then
          allocate(selmod(nmod),i4wk(nmod))
        end if
      end do
      this%solname = ''
    else
      write(this%solname,'(a,i2.2)') 's',this%solid
      f = trim(gen%in_dir)//'solutions\post_mappings\'//trim(this%solname)//'.modmap.bin'
      call open_file(f, iu, 'r', .true.)
      read(iu) nmod
      allocate(i4wk(nmod))
      read(iu)(i4wk(i),i=1,nmod)
      close(iu)
      allocate(selmod(nmod))
      do i = 1, nmod
        m => selmod(i)
        allocate(m%modid); m%modid = i4wk(i)
        m%gen => this%gen
        call m%init(.true.)
        i4wk(i) = 0
      end do
    end if
    !
    allocate(bb)
    do i = 1, mxobjid
      if (selbb(i)%lact) then
        sbb => selbb(i)%bb
        loverlap = .false.
        do j = 1, nmod
          mbb => selmod(j)%bb
          loverlap = bb_overlap(sbb, mbb)
          if (loverlap) then
            i4wk(j) = 1
            bb%ic0 = min(bb%ic0, mbb%ic0); bb%ic1 = max(bb%ic1, mbb%ic1)
            bb%ir0 = min(bb%ir0, mbb%ir0); bb%ir1 = max(bb%ir1, mbb%ir1)
          end if
        end do
      end if
    end do
    !
    ! initialize the models
    allocate(this%nmod)
    this%nmod = sum(i4wk)
    if (this%nmod == 0) then
      call errmsg('No models found.')
    end if
    allocate(this%mod(this%nmod))
    j = 0
    do i = 1, nmod
      if (i4wk(i) > 0) then
        j = j + 1
        m => this%mod(j)
        allocate(m%modid); m%modid = i
      end if
    end do
    !
    ! initialize the mask
    bb%ncol = bb%ic1 - bb%ic0 + 1
    bb%nrow = bb%ir1 - bb%ir0 + 1
    allocate(this%mask(bb%ncol,bb%nrow))
    allocate(this%maskbb)
    mbb => this%maskbb
    do ir = 1, bb%nrow
      do ic = 1, bb%ncol
        this%mask(ic,ir) = 0
      end do
    end do
    !
    ! open the map selection file and read the mask
    allocate(selmap)
    lok = selmap%init(fmap)
    do i = 1, mxobjid
      if (selbb(i)%lact) then
        sbb => selbb(i)%bb
        nc = sbb%ic1 - sbb%ic0 + 1
        nr = sbb%ir1 - sbb%ir0 + 1
        if (allocated(i4wk2d)) deallocate(i4wk2d)
        allocate(i4wk2d(nc,nr))
        call selmap%read_block(i4wk2d, sbb%ir0, sbb%ir1, sbb%ic0, sbb%ic1, 0)
        do ir = 1, nr
          do ic = 1, nc
            if (i4wk2d(ic,ir) == i) then
              jc = (sbb%ic0 + ic - 1) - bb%ic0 + 1
              jr = (sbb%ir0 + ir - 1) - bb%ir0 + 1
              kc = jc + bb%ic0 - 1; kr = jr + bb%ir0 - 1
              mbb%ic0 = min(mbb%ic0,kc); mbb%ic1 = max(mbb%ic1,kc)
              mbb%ir0 = min(mbb%ir0,kr); mbb%ir1 = max(mbb%ir1,kr)
              this%mask(jc,jr) = i
            end if
          end do
        end do
      end if
    end do
    mbb%ncol = mbb%ic1 - mbb%ic0 + 1
    mbb%nrow = mbb%ir1 - mbb%ir0 + 1
    !
    if (.false.) then
      call writeasc('mask.asc', this%mask, bb%ncol, bb%nrow, &
        gxmin + (bb%ic0-1)*gcs, gymin + (gnrow-bb%ir1)*gcs, gcs, DZERO)
      stop
    end if
    !
    ! clean up allocables
    if (allocated(sa))     deallocate(sa)
    if (allocated(sel))    deallocate(sel)
    if (allocated(selcol)) deallocate(selcol)
    if (allocated(actcol)) deallocate(actcol)
    if (allocated(curcol)) deallocate(curcol)
    if (allocated(i4wk))   deallocate(i4wk)
    if (allocated(i4wk2d)) deallocate(i4wk2d)
    !
    ! clean up pointer
    if (associated(selmod)) then
      do i = 1, nmod
        m => selmod(i)
        call m%clean()
      end do
      deallocate(selmod)
    end if
    if (associated(bb)) deallocate(bb)
    if (associated(selmap)) then
      call selmap%clean()
      deallocate(selmap)
    end if
    !
    return
  end subroutine mf6_post_sol_init_select
  
  subroutine mf6_post_sol_read_modbin(this)
! ******************************************************************************
    ! -- arguments
    class(tPostSol) :: this
    ! --- local
! ------------------------------------------------------------------------------
    return
  end subroutine mf6_post_sol_read_modbin
!  
  subroutine mf6_post_sol_write(this)
! ******************************************************************************
    ! -- arguments
    class(tPostSol) :: this
    ! --- local
    type(tBb), pointer :: sbb => null(), wbb => null()
    type(tBb), pointer :: mbb => null()
    type(tPostMod), pointer :: m => null()
    logical :: lread, lmask, lclip, lmv
    character(len=mxslen) :: f, fbb
    integer(i4b) :: i, il, ic, ir, jc, jr, gic, gir, iu, kper, ic0, ic1, ir0, ir1, n
    integer(i4b) :: i4val
    integer(i4b), dimension(:,:), allocatable :: si4wk, wsi4wk
    real(r8b) :: t, xmin, ymin, r8val, cs
    real(r8b) :: mintotimmod, maxtotimmod
    real(r8b), dimension(:,:), allocatable :: sr8wk, wsr8wk, wsr8wk2, mr8wk
! ------------------------------------------------------------------------------
    if (associated(this%mask)) then
      lmask = .true.
      lclip = .true.
    end if
    !
    if (.not.associated(this%nmod)) return
    !
    do while(.true.)
      lread = .false.
      mintotimmod = huge(DZERO); maxtotimmod = DZERO; kper = 0
      do i = 1, this%nmod
        m => this%mod(i)
        if (m%iu <= 0) cycle
        lread = .true.
        call m%read()
        if ((this%gen%itype == 3).or.(this%gen%itype == 4)) then
          call m%calc_slope()
        end if
        if ((this%gen%itype == 6).or.(this%gen%itype == 7)) then
          call m%calc_average()
        end if
        if (this%gen%itype == 8) then
          call m%calc_iqr()
        end if
        mintotimmod = min(mintotimmod, m%totim_read)
        maxtotimmod = max(maxtotimmod, m%totim_read)
        kper = m%kper_read
        if ((.not.this%lwritesol).and.(m%totim_read > DZERO).and.&
          (kper >= this%gen%kper_beg).and.(kper <= this%gen%kper_end)) then
          call m%write()
        end if
      end do
      !
      if (.not.lread) then
        t = DONE
      end if
      !
      ! check
      if (lread.and.(mintotimmod /= maxtotimmod)) then
        call errmsg('Inconsistent kper.')
      end if
      t = mintotimmod
      !
      if (t < DZERO) exit
      !
      if ((this%lwritesol).and.&
         (kper >= this%gen%kper_beg).and.(kper <= this%gen%kper_end)) then
        sbb => this%bb
        !
        if (this%lwritemodid) then
          allocate(si4wk(sbb%ncol,sbb%nrow))
          do ir = 1, sbb%nrow
            do ic = 1, sbb%ncol
              si4wk(ic,ir) = 0
            end do
          end do
        end if
        !
        allocate(sr8wk(sbb%ncol,sbb%nrow))
        do il = this%gen%il_min, this%gen%il_max
          do ir = 1, sbb%nrow
            do ic = 1, sbb%ncol
              sr8wk(ic,ir) = r8nodata
            end do
          end do
          if (this%lwritemodid) then
            do i = 1, this%nmod
              n = 0
              m => this%mod(i)
              if (m%nodes == 0) cycle
              mbb => m%bb
              mr8wk = m%get_data(il, .true.)
              do ir = 1, mbb%nrow
                do ic = 1, mbb%ncol
                  if (mr8wk(ic,ir) > DZERO) then
                    gir = ir + mbb%ir0 - 1; gic = ic + mbb%ic0 - 1
                    jr = gir - sbb%ir0 + 1; jc = gic - sbb%ic0 + 1
                    if ((jr < 1).or.(jr > sbb%nrow).or.(jc < 1).or.(jc > sbb%ncol)) then
                      cycle !call errmsg('mf6_post_sol_write')
                    end if
                   si4wk(jc,jr) = m%modid
                   n = n + 1
                  end if
                end do
              end do
            end do
          end if
          do i = 1, this%nmod
            m => this%mod(i)
            if (m%iu <= 0) cycle
            mbb => m%bb
            mr8wk = m%get_data(il)
            do ir = 1, mbb%nrow
              do ic = 1, mbb%ncol
                if (mr8wk(ic,ir) /= r8nodata) then
                  gir = ir + mbb%ir0 - 1; gic = ic + mbb%ic0 - 1
                  jr = gir - sbb%ir0 + 1; jc = gic - sbb%ic0 + 1
                  if ((jr < 1).or.(jr > sbb%nrow).or.(jc < 1).or.(jc > sbb%ncol)) then
                    cycle !call errmsg('mf6_post_sol_write')
                  end if
                  sr8wk(jc,jr) = mr8wk(ic,ir)
                end if
              end do
            end do
          end do
          f = mf6_post_get_out_pref(this%solname, t, il, this%gen)
          !
          allocate(wbb)
          if (this%gen%lwritebb) then
            wbb%ic0 = this%gen%gic0; wbb%ic1 = this%gen%gic1
            wbb%ir0 = this%gen%gir0; wbb%ir1 = this%gen%gir1
            wbb%ncol = wbb%ic1 - wbb%ic0 + 1
            wbb%nrow = wbb%ir1 - wbb%ir0 + 1
            ic0 = wbb%ic0 - sbb%ic0 + 1; ic1 = ic0 + wbb%ncol - 1
            ir0 = wbb%ir0 - sbb%ir0 + 1; ir1 = ir0 + wbb%nrow - 1
            ! check
            if ((ic1 - ic0 + 1) /= wbb%ncol) then
              call errmsg('Program error: invalid number of columns')
            end if
            if ((ir1 - ir0 + 1) /= wbb%nrow) then
              call errmsg('Program error: invalid number of rows')
            end if
          else
            if (lmask) then
              wbb%ic0 = this%maskbb%ic0; wbb%ic1 = this%maskbb%ic1
              wbb%ir0 = this%maskbb%ir0; wbb%ir1 = this%maskbb%ir1
              wbb%ncol = this%maskbb%ncol; wbb%nrow = this%maskbb%nrow
              ic0 = wbb%ic0 - sbb%ic0 + 1; ic1 = ic0 + wbb%ncol - 1
              ir0 = wbb%ir0 - sbb%ir0 + 1; ir1 = ir0 + wbb%nrow - 1
            else
              wbb%ic0 = sbb%ic0; wbb%ic1 = sbb%ic1
              wbb%ir0 = sbb%ir0; wbb%ir1 = sbb%ir1
              wbb%ncol = sbb%ncol; wbb%nrow = sbb%nrow
              ir0 = 1; ir1 = wbb%nrow; ic0 = 1; ic1 = wbb%ncol
            end if
          end if
          !
          if (allocated(wsr8wk)) deallocate(wsr8wk)
          allocate(wsr8wk(wbb%ncol,wbb%nrow))
          if (associated(statmap)) then
            allocate(wsr8wk2(wbb%ncol,wbb%nrow))
            do ir = 1, wbb%nrow
              do ic = 1, wbb%ncol
                wsr8wk2(ic,ir) = r8nodata
              end do
            end do
          end if
          !
          do ir = ir0, ir1
            do ic = ic0, ic1
              jc = ic - ic0 + 1; jr = ir - ir0 + 1
              wsr8wk(jc,jr) = sr8wk(ic,ir)
            end do
          end do
          if (this%lwritemodid) then
            if (allocated(wsi4wk)) deallocate(wsi4wk) 
            allocate(wsi4wk(wbb%ncol,wbb%nrow))
            do ir = ir0, ir1
              do ic = ic0, ic1
                jc = ic - ic0 + 1; jr = ir - ir0 + 1
                wsi4wk(jc,jr) = si4wk(ic,ir)
              end do
            end do
          end if
          !
          ! apply the mask by selection
          if (lmask) then
            do ir = ir0, ir1
              do ic = ic0, ic1
                r8val = sr8wk(ic,ir)
                if (this%mask(ic,ir) <= 0) then
                  jc = ic - ic0 + 1; jr = ir - ir0 + 1
                  wsr8wk(jc,jr) = r8nodata
                end if
              end do
            end do
            if (this%lwritemodid) then
              do ir = ir0, ir1
                do ic = ic0, ic1
                  i4val = si4wk(ic,ir)
                  if (this%mask(ic,ir) <= 0) then
                    jc = ic - ic0 + 1; jr = ir - ir0 + 1
                    wsi4wk(jc,jr) = 0
                  end if
                end do
              end do
            end if
          end if
          !
          ! apply the global mask
          if (associated(maskmap)) then
            do ir = ir0, ir1
              do ic = ic0, ic1
                gic = sbb%ic0 + ic - 1; gir = sbb%ir0 + ir - 1
                jc = ic - ic0 + 1; jr = ir - ir0 + 1
                call maskmap%get_val(gic, gir, r8val, lmv)
                if (lmv) then
                  wsr8wk(jc,jr) = r8nodata
                else
                  if (r8val <= DZERO) then
                    wsr8wk(jc,jr) = r8nodata
                  end if
                end if
              end do
            end do
          end if
          !
          ! apply the statistics map
          if (associated(statmap)) then
            do ir = ir0, ir1
              do ic = ic0, ic1
                jc = ic - ic0 + 1; jr = ir - ir0 + 1
                if (wsr8wk(jc,jr) /= r8nodata) then
                  gic = sbb%ic0 + ic - 1; gir = sbb%ir0 + ir - 1
                  call statmap%get_val(gic, gir, r8val, lmv)
                  if (.not.lmv) then
                    wsr8wk(jc,jr)  = r8val - wsr8wk(jc,jr)
                    wsr8wk2(jc,jr) = r8val
                  end if
                end if
              end do
            end do
          end if
          !
          xmin = gxmin + (wbb%ic0-1)*gcs
          ymin = gymin + (gnrow-wbb%ir1)*gcs
          !
          if (this%gen%i_out /= i_out_flts) then
            fbb = trim(f)//'.bb.asc'
            call logmsg('Writing '//trim(fbb)//'...')
            call open_file(fbb, iu, 'w')
            write(iu,'(a)') ta((/wbb%ic0, wbb%ic1, wbb%ir0, wbb%ir1/))
            close(iu)
          end if
          !
          select case(this%gen%i_out)
            case(i_out_asc)
              if (this%lwritemodid) then
                call writeasc(trim(f)//'_modid.asc',wsi4wk, &
                  wbb%ncol, wbb%nrow, &
                  xmin, ymin, gcs, DZERO)
              end if
              call writeasc(trim(f)//'.asc',wsr8wk, &
                wbb%ncol, wbb%nrow, &
                xmin, ymin, gcs, r8nodata)
            case(i_out_idf)
              if (this%lwritemodid) then
                call writeidf(trim(f)//'_modid.idf', wsi4wk, &
                  wbb%ncol, wbb%nrow, &
                  xmin, ymin, gcs, DZERO)
              end if
              call writeidf(trim(f)//'.idf', wsr8wk, &
                wbb%ncol, wbb%nrow, &
                xmin, ymin, gcs, r8nodata)
            case(i_out_flt)
              if (this%lwritemodid) then
                call writeflt(trim(f)//'_modid', wsi4wk, &
                  wbb%ncol, wbb%nrow, &
                  xmin, ymin, gcs, IZERO)
              end if
              call writeflt(trim(f), wsr8wk, &
                wbb%ncol, wbb%nrow, &
                xmin, ymin, gcs, r8nodata)
            case(i_out_flts)
              if (allocated(wsr8wk2)) deallocate(wsr8wk2)
              call r8_scale_avg(wsr8wk, r8nodata, wbb, wsr8wk2, xmin, ymin, cs)
              call writeflt(trim(f), wsr8wk2, &
                size(wsr8wk2,1), size(wsr8wk2,2), &
                xmin, ymin, cs, r8nodata)
          end select
        end do
        if (this%gen%itype == 2) then
          call this%write_stat(wsr8wk, wsr8wk2, r8nodata, f)
        end if
        deallocate(sr8wk, wsr8wk, mr8wk, wbb)
        if (allocated(si4wk))   deallocate(si4wk)
        if (allocated(wsi4wk))  deallocate(wsi4wk)
        if (allocated(wsr8wk2)) deallocate(wsr8wk2)
      end if
      !
      if (.not.lread) then
        exit
      end if
      !
      if (kper >= this%gen%kper_end) then
        exit
      end if
    end do
    !
    return
  end subroutine mf6_post_sol_write
  
  subroutine mf6_post_sol_write_statistics(this, xr, xo, xmv, fp)
! ******************************************************************************
    ! -- arguments
    class(tPostSol) :: this
    real(r8b), dimension(:,:), intent(in) :: xr ! residual
    real(r8b), dimension(:,:), intent(in) :: xo ! observation
    real(r8b), intent(in) :: xmv
    character(len=*) :: fp
    ! --- local
    type(tBin), pointer :: b => null()
    character(len=mxslen) :: f
    logical :: lfound
    integer(i4b) :: iact, nc, nr, ir, ic, i, j, iu
    integer(i4b), dimension(:,:), allocatable :: i4wk
    real(r8b) :: r8v
! ------------------------------------------------------------------------------
    !
    nc = size(xr,1); nr = size(xr,2)
    allocate(i4wk(nc,nr))
    do ir = 1, nr
      do ic = 1, nc
        i4wk(ic,ir) = 0
      end do
    end do
    !
    do ir = 1, nr
      do ic = 1, nc
        r8v = xo(ic,ir)
        if (r8v /= xmv) then
          lfound = .false.
          do i = 1, this%gen%nbin
            b => this%gen%bins(i)
            if ((b%min < r8v).and.(r8v <= b%max)) then
              b%n = b%n + 1
              i4wk(ic,ir) = i
              lfound = .true.
              exit
            end if
          end do
        end if
      end do
    end do
    !
    do i = 1, this%gen%nbin
      b => this%gen%bins(i)
      if (b%n > 0) then
        allocate(b%val(b%n))
        b%n = 0
      end if
    end do
    !
    do ir = 1, nr
      do ic = 1, nc
        i = i4wk(ic,ir)
        if (i > 0) then
          b => this%gen%bins(i)
          b%n = b%n + 1
          b%val(b%n) = xr(ic,ir)
        end if
      end do
    end do
    deallocate(i4wk)
    !
    do i = 1, this%gen%nbin
      b => this%gen%bins(i)
      f = trim(fp)//'.'//trim(b%id)//'.bin'
      call open_file(f, iu, 'w', .true.)
      write(iu)(b%val(j), j = 1, b%n)
      close(iu)
    end do
    !
    return
  end subroutine mf6_post_sol_write_statistics
  
  subroutine mf6_post_clean_gen(gen)
! ******************************************************************************
    ! -- arguments
    type(tGen), intent(inout), pointer :: gen
    ! --- local
! ------------------------------------------------------------------------------
    !
    if (.not.associated(gen)) return
    !
    if (associated(gen%in_dir))   deallocate(gen%in_dir)
    if (associated(gen%in_postf)) deallocate(gen%in_postf)
    if (associated(gen%out_dir))  deallocate(gen%out_dir)
    if (associated(gen%out_pref)) deallocate(gen%out_pref)
    if (associated(gen%lwritebb)) deallocate(gen%lwritebb)
    if (associated(gen%gic0))     deallocate(gen%gic0)
    if (associated(gen%gic1))     deallocate(gen%gic1)
    if (associated(gen%gir0))     deallocate(gen%gir0)
    if (associated(gen%gir1))     deallocate(gen%gir1)
    if (associated(gen%il_min))   deallocate(gen%il_min)
    if (associated(gen%il_max))   deallocate(gen%il_max)
    if (associated(gen%date_beg)) deallocate(gen%date_beg)
    if (associated(gen%date_end)) deallocate(gen%date_end)
    if (associated(gen%kper_beg)) deallocate(gen%kper_beg)
    if (associated(gen%kper_end)) deallocate(gen%kper_end)
    if (associated(gen%top_tiled))deallocate(gen%top_tiled)
    if (associated(gen%top_mf6))  deallocate(gen%top_mf6)
    if (associated(gen%itype))    deallocate(gen%itype)
    if (associated(gen%i_out))    deallocate(gen%i_out)
    if (associated(gen%nbin))     deallocate(gen%nbin)
    if (associated(gen%bins))     deallocate(gen%bins)
    if (associated(gen%year_avg)) deallocate(gen%year_avg)
    if (associated(gen%year_beg)) deallocate(gen%year_beg)
    !
    gen%in_dir   => null()
    gen%in_postf => null()
    gen%out_dir  => null()
    gen%out_pref => null()
    gen%il_min   => null()
    gen%il_max   => null()
    gen%lwritebb => null()
    gen%gic0     => null()
    gen%gic1     => null()
    gen%gir0     => null()
    gen%gir1     => null()
    gen%date_beg => null()
    gen%date_end => null()
    gen%kper_beg => null()
    gen%kper_end => null()
    gen%top_tiled=> null()
    gen%top_mf6  => null()
    gen%itype    => null()
    gen%i_out    => null()
    gen%nbin     => null()
    gen%bins     => null()
    gen%year_avg => null()
    gen%year_beg => null()
    !
    return
  end subroutine mf6_post_clean_gen
  
  subroutine mf6_post_sol_clean(this)
! ******************************************************************************
    ! -- arguments
    class(tPostSol) :: this
    ! --- local
    type(tMap), pointer :: map => null()
    integer(i4b) :: i
! ------------------------------------------------------------------------------
    !
    if (associated(this%lwritesol))   deallocate(this%lwritesol)
    if (associated(this%lwritemodid)) deallocate(this%lmask)
    if (associated(this%lmask))       deallocate(this%lmask)
    if (associated(this%solid))       deallocate(this%solid)
    if (associated(this%solname))     deallocate(this%solname)
    if (associated(this%bb))          deallocate(this%bb)
    !
    if (associated(topmap)) then
      if (associated(topmap%maps)) then
        do i = 1, topmap%ntile
          map => topmap%maps(i)
          call map%clean()
        end do
        deallocate(topmap%maps)
      end if
      if (associated(topmap%idfs)) then
        call idfdeallocate(topmap%idfs, topmap%ntile) 
      end if
      deallocate(topmap%ntile)
      deallocate(topmap%tilebb)
      deallocate(topmap); topmap => null()
    end if
    !
    if (associated(this%gen)) then
      call mf6_post_clean_gen(this%gen)
      deallocate(this%gen)
      this%gen => null()
    end if
    !
    if (associated(this%nmod)) then
      do i = 1, this%nmod
        call this%mod(i)%clean()
      end do
      deallocate(this%nmod)
      deallocate(this%mod)
    end if
    if (associated(this%mask)) then
      deallocate(this%mask)
    end if
    if (associated(this%maskbb)) deallocate(this%maskbb)
    !
    if (associated(statmap)) then
      call statmap%clean()
      deallocate(statmap); statmap => null()
    end if
    !
    this%lwritesol => null()
    this%solid     => null()
    this%solname   => null()
    this%bb        => null()
    this%nmod      => null()
    this%mod       => null()
    this%maskbb    => null()
    !
    return
  end subroutine mf6_post_sol_clean
  
end module mf6_post_module
