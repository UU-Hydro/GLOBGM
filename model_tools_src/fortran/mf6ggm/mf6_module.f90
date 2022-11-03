! ==============================================================================
module mf6_module
   ! -- modules
  use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, &
     i1b => int8, i2b => int16, i4b => int32, i8b => int64, r4b => real32, r8b => real64
  use utilsmod, only: getlun, chkexist, readline, change_case, errmsg, logmsg, &
    readidf_block, readidf_r8val, writeidf, checkdim, addboundary, tUnp, &
    calc_unique, sa, open_file, create_dir, swap_slash, tas, ta, get_rel_up, &
    writebin, get_jd, jd_next_month, get_month_days, getwords, getminmax, &
    replacetoken, IZERO, RZERO, DZERO, DONE, tBB, calc_unique, tI4grid, count_i1a, &
    get_jd, get_ymd_from_jd, jd_next_month, get_abs_path, get_month_days_s, get_slash
  use ehdrModule, only: writeflt
  use imod_idf
  use pcrModule, only: tMap
  
  implicit none 

  ! data keys
  integer(i4b), parameter :: i_top               = 1; public :: i_top !debug
  integer(i4b), parameter :: i_bot               = 2
  integer(i4b), parameter :: i_k                 = 3
  integer(i4b), parameter :: i_k33               = 4
  integer(i4b), parameter :: i_strt              = 5; public :: i_strt !debug
  integer(i4b), parameter :: i_drn_elev          = 6
  integer(i4b), parameter :: i_drn_cond          = 7
  integer(i4b), parameter :: i_riv_stage         = 8
  integer(i4b), parameter :: i_riv_cond          = 9
  integer(i4b), parameter :: i_riv_rbot          = 10
  integer(i4b), parameter :: i_wel               = 11
  integer(i4b), parameter :: i_recharge          = 12
  integer(i4b), parameter :: i_part              = 13
  integer(i4b), parameter :: i_sol               = 14
  integer(i4b), parameter :: i_print_option      = 15
  integer(i4b), parameter :: i_complexity        = 16
  integer(i4b), parameter :: i_outer_hclose      = 17
  integer(i4b), parameter :: i_outer_maximum     = 18
  integer(i4b), parameter :: i_inner_maximum     = 19
  integer(i4b), parameter :: i_inner_hclose      = 20
  integer(i4b), parameter :: i_inner_rclose      = 21
  integer(i4b), parameter :: i_relaxation_factor = 22
  integer(i4b), parameter :: i_prim_sto          = 23
  integer(i4b), parameter :: i_ghb1_bhead        = 24
  integer(i4b), parameter :: i_ghb1_cond         = 25
  integer(i4b), parameter :: i_ghb2_bhead        = 26
  integer(i4b), parameter :: i_ghb2_cond         = 27
  integer(i4b), parameter :: nkey                = i_ghb2_cond
  !  
  character(len=20), dimension(nkey) :: keys
            !12345678901234567890    12345678901234567890
  data keys/'top                 ', 'bot                 ', &
             'k                  ', 'k_33                ', &
             'strt               ', &
             'drn_elev           ', 'drn_cond            ', &
             'riv_stage          ', 'riv_cond            ', &
             'riv_rbot           ', 'wel_q               ', &
             'recharge           ', 'partitions          ', &
             'solutions          ', &
             'print_option       ', 'complexity          ', &
             'outer_hclose       ', 'outer_maximum       ', &
             'inner_maximum      ', 'inner_hclose        ', &
             'inner_rclose       ', 'relaxation_factor   ', &
             'prim_sto           ',                         &
             'ghb_bhead          ', 'ghb_cond            ', &
             'ghb2_bhead         ', 'ghb2_cond           '/
  
  ! parameters
  integer(i4b),          parameter :: mxslen = 1024
  character(len=mxslen), parameter :: resultsbindir = '..\..\models\run_output_bin'
  character(len=mxslen), parameter :: resultslstdir = '..\..\models\run_output_lst'
  logical,               parameter :: writemapidf = .false.
  logical                          :: ltransient = .false.
  character(len=2)                 :: ctim = 'ss'
  !
  integer(i4b), parameter :: inam  =  1
  integer(i4b), parameter :: itdis =  2
  integer(i4b), parameter :: idisu =  3
  integer(i4b), parameter :: iic   =  4
  integer(i4b), parameter :: ioc   =  5
  integer(i4b), parameter :: inpf  =  6
  integer(i4b), parameter :: isto  =  7
  integer(i4b), parameter :: ichd1 =  8
  integer(i4b), parameter :: ichd2 =  9
  integer(i4b), parameter :: idrn  = 10
  integer(i4b), parameter :: iriv  = 11
  integer(i4b), parameter :: irch  = 12
  integer(i4b), parameter :: iwel  = 13
  integer(i4b), parameter :: ighb1 = 14
  integer(i4b), parameter :: ighb2 = 15
  integer(i4b), parameter :: npck = ighb2
  character(len=4), dimension(npck) :: pck
  data pck/'nam ', 'tdis', 'disu', 'ic  ', 'oc  ', 'npf ', 'sto ', 'chd ', 'chd ',&
           'drn ', 'riv ', 'rch ', 'wel ', 'ghb ', 'ghb '/
  integer(i4b), dimension(npck) :: pckact
  
  integer(i4b), parameter :: maxrun = 6
  character(len=10), dimension(npck,maxrun) :: pr
  integer(i4b), parameter :: irun0ss = 1, irun1ss = 3, irun0tr = 4, irun1tr = 6
  !
    !------------------------------------------------------------------------------------------------------------
    ! ss/tr |run| nam      | tdis | disu | ic  | oc | npf| sto | chd1 | chd2 | drn | riv | rch | wel | ghb | ghb2|
    !------------------------------------------------------------------------------------------------------------
    ! ss    | 1 | chd_intf | #    | #    | #   | sm | #  | #   | #    | intf | #   | #   | #   | #   | #   | #   |
    ! ss    | 2 | ic_sm    | #    | #    | sm  | #  | #  | #   | #    | -    | #   | #   | #   | #   | #   | #   |
    ! ss    | 3 | ic_sh0   | #    | #    | #   | #  | #  | #   | #    | -    | #   | #   | #   | #   | #   | #   |
    ! tr    | 1 | spu      | spu  | #    | ss  | spu| #  | #   | #    | -    | spu | spu | spu | spu | spu | spu |
    ! tr    | 2 | ic_spu   | #    | #    | spu | #  | #  | #   | #    | -    | #   | #   | #   | #   | #   | #   |
    ! tr    | 3 | ic_ss    | #    | #    | ss  | #  | #  | #   | #    | -    | #   | #   | #   | #   | #   | #   |
  !
  !       inam         itdis        idisu        iic          ioc         inpf          isto         ichd1        ichd2        idrn         iriv         irch         iwel         ighb         ighb2        
  !       1234567890   1234567890   1234567890   1234567890   1234567890   1234567890   1234567890   1234567890   1234567890   1234567890   1234567890   1234567890   1234567890   1234567890   1234567890
  data pr/'.chd_intf ','          ','          ','          ','.sm       ','          ','          ','.sea      ','.intf     ','          ','          ','          ','          ','.1        ','.2        ', &
          '.ic_sm    ','          ','          ','.sm       ','          ','          ','          ','.sea      ','-         ','          ','          ','          ','          ','.1        ','.2        ', &
          '.ic_sh0   ','          ','          ','          ','          ','          ','          ','.sea      ','-         ','          ','          ','          ','          ','.1        ','.2        ', &
          '.spu      ','.spu      ','          ','.ss       ','.spu      ','          ','          ','.sea      ','-         ','.spu      ','.spu      ','.spu      ','.spu      ','.1.spu    ','.2.spu    ', &
          '.ic_spu   ','          ','          ','.spu      ','          ','          ','          ','.sea      ','-         ','          ','          ','          ','          ','.1        ','.2        ', &
          '.ic_ss    ','          ','          ','.ss       ','          ','          ','          ','.sea      ','-         ','          ','          ','          ','          ','.1        ','.2        '/
  !
  ! stencil
  integer(i4b), parameter :: jp = 1
  integer(i4b), parameter :: jt = 2
  integer(i4b), parameter :: jn = 3
  integer(i4b), parameter :: jw = 4
  integer(i4b), parameter :: je = 5
  integer(i4b), parameter :: js = 6
  integer(i4b), parameter :: jb = 7
  integer(i4b), parameter :: ns = jb
  
  private
  
  integer(i4b), parameter :: i_undefined = 0
  integer(i4b), parameter :: i_idf       = 1
  integer(i4b), parameter :: i_distidf   = 2
  integer(i4b), parameter :: i_map       = 3
  integer(i4b), parameter :: i_distmap   = 4
  integer(I4B), parameter :: maxnraw = 1000
  
  type tData
    character(mxslen)       :: s = ''
    integer(I4B)            :: file_type = i_undefined
    type(idfobj),   pointer :: idf     => null()
    type(tDistIDF), pointer :: distidf => null()
    type(tMap),     pointer :: map     => null()
    type(tDistMap), pointer :: distmap => null()
    real(r8b)               :: r8mult = DONE
    real(r8b)               :: r8add = DZERO
  end type tData
  
  type tRaw
    character(mxslen) :: key = ''
    type(tData), pointer :: dat => null()
    integer(I4B)      :: ilay_min = 0
    integer(I4B)      :: ilay_max = 0
    integer(I4B)      :: iper_min = 0
    integer(I4B)      :: iper_max = 0
  end type tRaw
  type tRawDat
    type(tRaw), dimension(maxnraw) :: raw
    integer(I4B)      :: nraw = 0
    integer(I4B)      :: nper = 0
    character(len=mxslen):: sdate = ''
    character(len=mxslen), dimension(:), allocatable :: perdate
  contains
    procedure :: init => mf6_raw_init
    procedure :: mf6_raw_init_dist
    procedure :: mf6_raw_get_index
    procedure :: mf6_raw_key_exists
    procedure :: mf6_raw_get_name_char, mf6_raw_get_name_i4b
    generic   :: exists => mf6_raw_key_exists
    generic   :: getc  => mf6_raw_get_name_char
    generic   :: geti  => mf6_raw_get_name_i4b
    procedure :: get_idf => mf6_raw_get_index_idf
    procedure :: mf6_raw_read_block_i4b, mf6_raw_read_block_r4b
    generic   :: read_block => mf6_raw_read_block_i4b, mf6_raw_read_block_r4b
  end type tRawDat
  type(tRawDat) :: raw
  !  
  type(tBB), dimension(:), pointer :: tilebb => null()
  integer(i4b), pointer :: ntile => null()
  !
  type tDistMap
    logical                             :: linit  = .false.
    integer(i4b),               pointer :: gntile => null()
    integer(i2b), dimension(:), pointer :: g2ltile => null()
    integer(i4b),               pointer :: ntile  => null()
    real(r4b),                  pointer :: r4def  => null()
    logical,                    pointer :: larea  => null()
    type(tBB), dimension(:),    pointer :: tilebb => null()
    type(tMap), dimension(:),   pointer :: maps   => null()
  contains
    procedure :: init   => mf6_distmap_init
    procedure :: clean  => mf6_distmap_clean
    procedure :: getval => mf6_distmap_getval_r4
  end type tDistMap
  
  type tDistIDF
    logical                             :: linit  = .false.
    integer(i4b),                pointer :: gntile => null()
    integer(i2b), dimension(:),  pointer :: g2ltile => null()
    integer(i4b),                pointer :: ntile  => null()
    real(r8b),                   pointer :: r8def  => null()
    logical,                     pointer :: larea  => null()
    type(tBB), dimension(:),     pointer :: tilebb => null()
    type(idfobj), dimension(:),  pointer :: idfs   => null()
  contains
    procedure :: init   => mf6_distidf_init
    procedure :: clean  => mf6_distidf_clean
    procedure :: getval => mf6_distidf_getval_r8
  end type tDistIDF
  !
  integer(i4b) :: gncol      = 0
  integer(i4b) :: gnrow      = 0
  integer(i4b) :: gnlay      = 0
  real(r8b)    :: gxmin      = DZERO
  real(r8b)    :: gymin      = DZERO
  real(r8b)    :: gcs        = DZERO
  integer(i4b), dimension(:), pointer :: cam2 => null()
  !
  real(r4b), dimension(:,:), pointer :: r4a => null()
  !
  ! work arrays
  character(len=mxslen), dimension(:),   pointer :: cwrk1d => null()
  character(len=mxslen), dimension(:,:), pointer :: cwrk2d => null()
  integer(i1b),          dimension(:),   pointer :: i1wrk   => null()
  integer(i1b),          dimension(:),   pointer :: i1wrk2  => null()
  integer(i4b),          dimension(:),   pointer :: i4wrk1d => null()
  integer(i4b),          dimension(:,:), pointer :: i4wrk2d => null()
  real(r8b),             dimension(:),   pointer :: r8wrk   => null()
  real(r8b),             dimension(:),   pointer :: r8wrk2  => null()
  real(r8b),             dimension(:),   pointer :: r8wrk3  => null()
  
  type tDisu
    integer(i4b),               pointer :: nodes => null()
    integer(i4b),               pointer :: nja   => null()
    integer(i4b), dimension(:), pointer :: iac   => null() !length nodes
    integer(i4b), dimension(:), pointer :: ja    => null() !length nja
    integer(i4b), dimension(:), pointer :: ihc   => null() !length nja
    real(r8b),    dimension(:), pointer :: cl12  => null() !length nja
    real(r8b),    dimension(:), pointer :: hwva  => null() !length nja
  end type tDisu
  
  type tReg
    type(tBb),                      pointer :: bb          => null()
    integer(i2b), dimension(:,:),   pointer :: itile       => null()
    integer(i4b), dimension(:),     pointer :: layer_nodes => null()
    integer(i4b), dimension(:,:,:), pointer :: nodmap      => null()
    integer(i4b), dimension(:,:,:), pointer :: bndmap      => null()
  end type tReg
  
  type tExchange
    logical                               :: loutput = .true.
    type(tBb),                    pointer :: bb          => null()
    integer(i4b), dimension(:),   pointer :: m1reg       => null() !region ID M1
    integer(i4b), dimension(:),   pointer :: m2reg       => null() !region ID M1
    integer(i4b),                 pointer :: m1mod       => null() !local model index for solution
    integer(i4b),                 pointer :: m2mod       => null()
    character(len=mxslen),        pointer :: m2modelname => null()
    integer(i4b),                 pointer :: nexg        => null()
    integer(i4b), dimension(:),   pointer :: cellidm1    => null()
    integer(i4b), dimension(:),   pointer :: cellidm2    => null()
    integer(i4b), dimension(:,:), pointer :: gicirilm1   => null() !help
    integer(i4b), dimension(:,:), pointer :: gicirilm2   => null() !help
  end type tExchange
    
  type tMf6_mod
    integer(i4b)                           :: imod        = -1      !model ID
    integer(i4b),                  pointer :: isol        => null() !solution ID
    integer(i4b),                  pointer :: ntile       => null() !number of attached tiles
    integer(i2b), dimension(:),    pointer :: itile       => null() !global tile numbers
    type(tBb),                     pointer :: bb          => null()
    character(len=mxslen),         pointer :: modelname   => null()
    character(len=mxslen),         pointer :: rootdir     => null()
    character(len=mxslen),         pointer :: bindir      => null()
    integer(i4b),                  pointer :: nreg        => null()
    type(tReg), dimension(:),      pointer :: reg         => null()
    type(tDisu),                   pointer :: disu        => null()
    integer(i4b),                  pointer :: nxch        => null()
    type(tExchange), dimension(:), pointer :: xch         => null()
    integer(i4b), dimension(:),    pointer :: layer_nodes => null()

    character(len=mxslen),         pointer :: fbin        => null()
    integer(i4b),                  pointer :: iubin       => null()
  contains
    procedure :: get_model_name => mf6_mod_get_model_name
    procedure :: set_disu       => mf6_mod_set_disu
    procedure :: get_i_raw      => mf6_mod_get_i_raw
    procedure :: get_val        => mf6_mod_get_val_r8
    procedure :: get_array      => mf6_mod_get_array_r8
    generic   :: write_array    => mf6_mod_write_array_i4, &
                                   mf6_mod_write_array_r8
    procedure :: mf6_mod_write_array_i4
    procedure :: mf6_mod_write_array_r8
    generic   :: write_list    => mf6_mod_write_list_1, &
                                  mf6_mod_write_list_2, &
                                  mf6_mod_write_list_3
    procedure :: mf6_mod_write_list_1
    procedure :: mf6_mod_write_list_2
    procedure :: mf6_mod_write_list_3
    !
    procedure :: write       => mf6_mod_write
    procedure :: write_nam   => mf6_mod_write_nam
    procedure :: write_disu  => mf6_mod_write_disu
    procedure :: write_ic    => mf6_mod_write_ic
    procedure :: write_oc    => mf6_mod_write_oc
    procedure :: write_npf   => mf6_mod_write_npf
    procedure :: write_sto   => mf6_mod_write_sto
    procedure :: write_chd   => mf6_mod_write_chd
    procedure :: write_drn   => mf6_mod_write_drn
    procedure :: write_riv   => mf6_mod_write_riv
    procedure :: write_ghb   => mf6_mod_write_ghb
    procedure :: write_rch   => mf6_mod_write_rch
    procedure :: write_wel   => mf6_mod_write_wel
    !
    procedure :: write_exchanges => mf6_mod_write_exchanges
    !
    procedure :: write_post_map  => mf6_mod_write_post_map
    !
    procedure :: clean_regions  => mf6_mod_clean_regions
    !
    procedure :: count_i1a => mf6_mod_count_i1a
  end type tMf6_mod
  
  type tMf6_sol
    integer(i4b)                          :: isol = -1
    character(len=mxslen),        pointer :: solname  => null()
    logical,                      pointer :: lmm      => null()
    integer(i4b),                 pointer :: nmod     => null() !number of models
    integer(i4b), dimension(:),   pointer :: mod_id   => null() !model ids
    integer(i4b),                 pointer :: npart    => null() !number of partitions
    integer(i4b), dimension(:),   pointer :: mod_part => null() !model partition numbers
  contains
    procedure :: write           => mf6_sol_write
    procedure :: write_tdis      => mf6_sol_write_tdis
    procedure :: write_ims       => mf6_sol_write_ims
    procedure :: write_wrap      => mf6_sol_write_wrap
    procedure :: write_mfsim     => mf6_sol_write_mfsim
    procedure :: write_post_map  => mf6_sol_write_post_map
    procedure :: clean           => mf6_sol_clean
  end type tMf6_sol
  
  ! public variables
  public :: ntile, tilebb
  public :: tMf6_mod
  public :: tMf6_sol
  public :: tExchange
  public :: tReg
  public :: i1b, i4b, i8b, r4b, r8b
  public :: raw
  public :: tDistMap
  public :: tMap
  public :: gncol, gnrow, gnlay, gxmin, gymin, gcs, cam2
  
  save
  
  contains
  
! ==============================================================================
  
  subroutine mf6_raw_init(this, f_in)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tRawDat) :: this
    character(len=*), intent(inout) :: f_in
    ! -- local
    character(len=mxslen) :: s
    integer(I4B) :: iu, ios, nw, n, i, y, m ,d, ymd
    real(R8B) :: jd
    character(len=mxslen), dimension(:), allocatable :: words
    type(tData), pointer :: dat
! ------------------------------------------------------------------------------
    call logmsg('Reading '//trim(f_in)//'...')
    call open_file(f_in, iu)
    ios = 0
    n = 0
    do while (ios == 0)
      ios = readline(iu, s)
      !write(*,*) 'Reading ',trim(s)
      if (ios == 0) then
        words = getwords(s)
        nw = size(words)
        if (nw > 1) then
          n = n + 1; this%nraw = n
          this%raw(n)%key = change_case(words(1), 'l')
          call getminmax(words(1), '_', 'L', this%raw(n)%ilay_min, this%raw(n)%ilay_max)
          call getminmax(words(1), '_', 'P', this%raw(n)%iper_min, this%raw(n)%iper_max)
          !
          ! set data
          allocate(this%raw(n)%dat)
          dat => this%raw(n)%dat
          dat%s = words(2)
          !
          !set absolute paths
          call get_abs_path(dat%s)
          !
          ! check for supported file types
          s = change_case(words(2), 'l')
          i = index(s, '.idf', back=.true.)
          if (i > 0) then
            if (nw > 2) then
              s = change_case(words(3), 'u')
              if (trim(s) == 'DIST') then
                dat%file_type = i_distidf
              end if
            else
              dat%file_type = i_idf
              allocate(dat%idf)
            end if
          end if
          i = index(s, '.map', back=.true.)
          if (i > 0) then
             if (nw > 2) then
               s = change_case(words(3), 'u')
               if (trim(s) == 'DIST') then
                 dat%file_type = i_distmap
               end if
             else
               dat%file_type = i_map
             end if
          endif
          !
          if (nw > 3) then
            s = change_case(words(4), 'u')
            if (trim(s) == '*') then
              call logmsg('**** Multiplication factor found **** ')
              read(words(5),*) dat%r8mult
            end if
            if (trim(s) == '+') then
              call logmsg('**** Addition factor found **** ')
              read(words(5),*) dat%r8add
            end if
            if (nw > 5) then
              s = change_case(words(6), 'u')
              if (trim(s) == '*') then
                read(words(7),*) dat%r8mult
              end if
              if (trim(s) == '+') then
                read(words(7),*) dat%r8add
              end if
            end if
          end if
          !
          ! allocate and read header
          select case(dat%file_type)
          case(i_idf)
            allocate(dat%idf)
            if (.not.idfread(dat%idf, dat%s, 0)) then
              call errmsg('Could not read '//trim(dat%s))
            end if 
          case(i_distidf)
            allocate(dat%distidf)
            !if (nw > 3) then
            !  allocate(dat%distidf%r8def, dat%distidf%larea)
            !  read(words(4),*) dat%distidf%r8def
            !  dat%distidf%larea = .false.
            !  if (nw > 4) then
            !    if (words(5) == 'AREA') then
            !      dat%distidf%larea = .true.
            !    end if
            !  end if
            ! end if
          case(i_map)
            allocate(dat%map)
          case(i_distmap)
            allocate(dat%distmap)
            !if (nw > 3) then
            !  allocate(dat%distmap%r4def, dat%distmap%larea)
            !  read(words(4),*) dat%distmap%r4def
            !  dat%distmap%larea = .false.
            !  if (nw > 4) then
            !    if (words(5) == 'AREA') then
            !      dat%distmap%larea = .true.
            !    end if
            !  end if
            ! end if
          end select
        end if
      end if
    end do
    !
    this%nper = this%geti('nper')
    if (this%nper > 1) then
      ltransient = .true.; ctim = 'tr'
    end if
    this%sdate = this%getc('startdate')
    allocate(this%perdate(this%nper))
    !
    read(this%sdate(1:4),*) y
    read(this%sdate(5:6),*) m
    read(this%sdate(7:8),*) d
    !
    jd = get_jd(y, m, d)
    do i = 1, this%nper
      call  get_ymd_from_jd(jd, ymd, y, m, d)
      write(this%perdate(i),'(i4,i2.2)') y, m
      call jd_next_month(jd)
    end do
    !
    return
  end subroutine mf6_raw_init
  
  subroutine mf6_raw_init_dist(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tRawDat) :: this
    ! -- local
    type(tDistMap), pointer :: distmap => null()
    type(tDistIDF), pointer :: distidf => null()
    integer(i4b) :: i, j
! ------------------------------------------------------------------------------
    do i = 1, this%nraw
      distmap => this%raw(i)%dat%distmap
      if (associated(distmap)) then
        distmap%linit = .false.
        if (associated(distmap%maps)) then
          do j = 1, distmap%ntile
            call distmap%maps(j)%clean()
          end do
        end if
      end if
      distidf => this%raw(i)%dat%distidf
      if (associated(distidf)) then
        distidf%linit = .false.
        if (associated(distidf%idfs)) then
          call idfdeallocate(distidf%idfs, distidf%ntile)
        end if
      end if
    end do
    !
    return
  end subroutine mf6_raw_init_dist
  
  function mf6_raw_get_index(this, key, ilay, iper) result(ind)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tRawDat) :: this
    character(len=*), intent(in) :: key
    integer(I4B), intent(in) :: ilay
    integer(I4B), intent(in) :: iper
    integer(I4B) :: ind
    ! -- local
    integer :: i, j, il_min, il_max, ip_min, ip_max
    character(len=mxslen) :: s, lckey
    logical :: lilay, liper
! ------------------------------------------------------------------------------
    !
    lckey = change_case(key, 'l')
    !
    ind = 0
    do i = 1, this%nraw
      s = this%raw(i)%key
      j = max(index(s,'_p'),index(s,'_l'))
      if (j > 0) then
        s = s(1:j-1)
      end if
      if (index(s, trim(lckey)) > 0) then
        il_min = this%raw(i)%ilay_min; il_max = this%raw(i)%ilay_max
        ip_min = this%raw(i)%iper_min; ip_max = this%raw(i)%iper_max
        lilay = .false.; liper = .false.
        if ((il_min == 0).and.(il_max == 0)) then
          lilay = .true.
        else
          if ((il_min <= ilay).and.(ilay <= il_max)) then
            lilay = .true.
          end if
        end if
        if ((ip_min == 0).and.(ip_max == 0)) then
          liper = .true.
        else
          if ((ip_min <= iper).and.(iper <= ip_max)) then
            liper = .true.
          end if
        end if
        if (lilay.and.liper) then
          if (ind == 0) then
            ind = i
          else
            call errmsg('Error, '//trim(key)//' found twice')
          end if
          exit
        end if
      end if
    end do
    
    !if (ind == 0) then
    !  call errmsg('Program error mf6_get_index')
    !end if
    !
    return
  end function mf6_raw_get_index

  function mf6_raw_key_exists(this, key, ilay, iper) result(lex)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tRawDat) :: this
    character(len=*), intent(in) :: key
    integer(I4B), optional, intent(in) :: ilay
    integer(I4B), optional, intent(in) :: iper
    logical :: lex
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: jlay, jper
! ------------------------------------------------------------------------------
    jlay = 0
    jper = 0
    if (present(ilay)) jlay = ilay
    if (present(iper)) jper = iper
    !
    i = this%mf6_raw_get_index(key, jlay, jper)
    if (i > 0) then
      lex = .true.
    else
      lex = .false.
    endif
    !
    return
  end function mf6_raw_key_exists
  
  function mf6_raw_get_name_char(this, key, ilay, iper, cdef) result(cval)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tRawDat) :: this
    character(len=*), intent(in) :: key
    integer(I4B), optional, intent(in) :: ilay
    integer(I4B), optional, intent(in) :: iper
    character(len=*), optional, intent(in) :: cdef
    character(len=:), allocatable :: cval
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: jlay, jper
! ------------------------------------------------------------------------------
    jlay = 0
    jper = 0
    if (present(ilay)) jlay = ilay
    if (present(iper)) jper = iper
    !
    if (.not.this%exists(key, jlay, jper)) then
      if (present(cdef)) then
        cval = trim(cdef)
        return
      else
        call errmsg('Error: key '//trim(key)// 'not found.')
      end if
    end if
    i = this%mf6_raw_get_index(key, jlay, jper)
    cval = trim(this%raw(i)%dat%s)
    !
    return
  end function mf6_raw_get_name_char
  
! ==============================================================================
  function mf6_raw_get_name_i4b(this, key, ilay, iper, idef) result(ival)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tRawDat) :: this
    character(len=*), intent(in) :: key
    integer(I4B), optional, intent(in) :: ilay
    integer(I4B), optional, intent(in) :: iper
    integer(I4B) :: ival
    integer(I4B), optional, intent(in) :: idef
    ! -- local
    character(len=mxslen) :: s
    integer(I4B) :: ios
    integer(I4B) :: jlay, jper
! ------------------------------------------------------------------------------
    jlay = 0
    jper = 0
    if (present(ilay)) jlay = ilay
    if (present(iper)) jper = iper
    
    if (.not.this%exists(key, jlay, jper)) then
      if (present(idef)) then
        ival = idef
        return
      else
        call errmsg('Error: key '//trim(key)// 'not found.')
      end if
    end if
    s = this%mf6_raw_get_name_char(key, ilay, iper)
    read(s,*,iostat=ios) ival
    if (ios /= 0) then
      call errmsg('Could not read target for '//trim(key)//' as integer 4 bytes')
    end if
    !
    return
  end function mf6_raw_get_name_i4b

! ==============================================================================
  function mf6_raw_get_name_r4b(this, key) result(rval)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tRawDat) :: this
    character(len=*), intent(in) :: key
    integer(R4B) :: rval
    ! -- local
    character(len=mxslen) :: s
    integer(I4B) :: ios
! ------------------------------------------------------------------------------
    s = this%mf6_raw_get_name_char(key)
    read(s,*,iostat=ios) rval
    if (ios /= 0) then
      call errmsg('Could not read target for '//trim(key)//' as real 4 bytes')
    end if
    !
    return
  end function mf6_raw_get_name_r4b

! ==============================================================================
  function mf6_raw_get_name_r8b(this, key) result(rval)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tRawDat) :: this
    character(len=*), intent(in) :: key
    integer(R8B) :: rval
    ! -- local
    character(len=mxslen) :: s
    integer(I4B) :: ios
! ------------------------------------------------------------------------------
    s = this%mf6_raw_get_name_char(key)
    read(s,*,iostat=ios) rval
    if (ios /= 0) then
      call errmsg('Could not read target for '//trim(key)//' as real 8 bytes')
    end if
    !
    return
  end function mf6_raw_get_name_r8b

  subroutine mf6_raw_read_block_i4b(this, ikey, ir0, ir1, ic0, ic1, arr, nodata, &
   ilay, iper)
! ******************************************************************************
    ! -- modules
    use imod_idf, only: idfobj
    ! -- arguments
    class(tRawDat) :: this
    integer(I4B), intent(in) :: ikey
    integer(I4B), intent(in) :: ir0, ir1, ic0, ic1
    integer(I4B), dimension(:,:), pointer, intent(inout) :: arr
    integer(I4B), intent(in), optional :: nodata
    integer(I4B), optional, intent(in) :: ilay
    integer(I4B), optional, intent(in) :: iper
    ! --- local
    integer(I4B) :: jlay, jper, i
    type(idfobj), pointer :: idf
    type(tData), pointer :: dat
! ------------------------------------------------------------------------------
    jlay = 0
    jper = 0
    if (present(ilay)) jlay = ilay
    if (present(iper)) jper = iper
    !
    if (.not.this%exists(keys(ikey), jlay, jper)) then
      call errmsg('Error: key '//trim(keys(ikey))// 'not found.')
    end if
    i = this%mf6_raw_get_index(keys(ikey), jlay, jper)
    dat => this%raw(i)%dat
    !
    select case(dat%file_type)
    case(i_idf)
      idf => dat%idf
      if (present(nodata)) then
        call readidf_block(idf, ir0, ir1, ic0, ic1, arr, nodata)
      else
        call readidf_block(idf, ir0, ir1, ic0, ic1, arr)
      end if
    case(i_map)
      call errmsg("Error mf6_raw_read_block_i4b: map file not supported")
    case(i_undefined)
      call errmsg("Error mf6_raw_read_block_i4b: not a recognized file")
    end select
    !
    return
  end subroutine mf6_raw_read_block_i4b
   
  subroutine mf6_raw_read_block_r4b(this, ikey, ir0, ir1, ic0, ic1, arr, nodata, &
   ilay, iper, itile)
! ******************************************************************************
    ! -- modules
    use imod_idf, only: idfobj
    ! -- arguments
    class(tRawDat) :: this
    integer(I4B), intent(in) :: ikey
    integer(I4B), intent(in) :: ir0, ir1, ic0, ic1
    real(R4B), dimension(:,:), pointer, intent(inout) :: arr
    real(R4B), intent(in), optional :: nodata
    integer(I4B), optional, intent(in) :: ilay
    integer(I4B), optional, intent(in) :: iper
    integer(I2B), dimension(:,:), pointer, optional :: itile
    ! --- local
    integer(I4B) :: jlay, jper, i, ic, ir, jc, jr, nc, nr
    real(R4B) :: r4v
    type(idfobj), pointer :: idf
    type(tData), pointer :: dat => null()
! ------------------------------------------------------------------------------
    jlay = 0
    jper = 0
    if (present(ilay)) jlay = ilay
    if (present(iper)) jper = iper
    !
    if (.not.this%exists(keys(ikey), jlay, jper)) then
      call errmsg('Error: key '//trim(keys(ikey))// 'not found.')
    end if
    i = this%mf6_raw_get_index(keys(ikey), jlay, jper)
    dat => this%raw(i)%dat
    !
    select case(dat%file_type)
    case(i_idf)
      idf => dat%idf
      if (present(nodata)) then
        call readidf_block(idf, ir0, ir1, ic0, ic1, arr, nodata)
      else
        call readidf_block(idf, ir0, ir1, ic0, ic1, arr)
      end if
    case(i_map)
      call errmsg("Error mf6_raw_read_block_r4b: map file not supported")
    case(i_distmap)
      call errmsg("Error mf6_raw_read_block_r4b: distributed map file not supported")
      !if (.not.present(itile)) then
      !  call errmsg('Could not read tiles.')
      !end if
      !call dat%distmap%init(dat%s, jlay, jper)
      !nc = ic1 - ic0 + 1; nr = ir1 - ir0 + 1
      !allocate(arr(nc,nr))
      !do ir = 1, nr
      !  do ic = 1, nc
      !    jc = ic + ic0 - 1; jr = ir + ir0 - 1
      !    i = int(itile(ic,ir),i4b)
      !    if (i < 0) then
      !      i = -i
      !    end if
      !    if (i > 0) then
      !      r4v = dat%distmap%getval(jc, jr, i)
      !      arr(ic,ir) = r4v
      !    end if
      !  end do
      !end do
    case(i_undefined)
      call errmsg("Error mf6_raw_read_block_r4b: not a recognized file")
    end select
    !
    return
 end subroutine mf6_raw_read_block_r4b

 function mf6_raw_get_index_idf(this, ikey, ilay, iper) result(idf)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tRawDat) :: this
    integer(I4B), intent(in) :: ikey
    integer(I4B), optional, intent(in) :: ilay
    integer(I4B), optional, intent(in) :: iper
    type(idfobj), pointer :: idf
    ! -- local
    integer(I4B) :: jlay, jper, i
    type(tData), pointer :: dat
! ------------------------------------------------------------------------------
    jlay = 0
    jper = 0
    if (present(ilay)) jlay = ilay
    if (present(iper)) jper = iper
    !
    if (.not.this%exists(keys(ikey), jlay, jper)) then
      call errmsg('Error: key '//trim(keys(ikey))// 'not found.')
    end if
    i = this%mf6_raw_get_index(keys(ikey), jlay, jper)
    dat => this%raw(i)%dat
    
    if (dat%file_type == i_idf) then
      idf => dat%idf
    else
      idf => null()
    end if
    !
    return
  end function mf6_raw_get_index_idf
  
  subroutine clear_wrk()
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
! ------------------------------------------------------------------------------
    if (associated(cwrk1d)) then
      deallocate(cwrk1d)
      cwrk1d => null()
    end if
    if (associated(cwrk2d)) then
      deallocate(cwrk2d)
      cwrk2d => null()
    end if
    if (associated(i1wrk)) then
      deallocate(i1wrk)
      i1wrk => null()
    end if
    if (associated(i1wrk2)) then
      deallocate(i1wrk2)
      i1wrk2 => null()
    end if
    if (associated(i4wrk1d)) then
      deallocate(i4wrk1d)
      i4wrk1d => null()
    end if
    if (associated(i4wrk2d)) then
      deallocate(i4wrk2d)
      i4wrk2d => null()
    end if
    if (associated(r8wrk)) then
      deallocate(r8wrk)
      r8wrk => null()
    end if
    if (associated(r8wrk2)) then
      deallocate(r8wrk2)
      r8wrk2 => null()
    end if
    if (associated(r8wrk3)) then
      deallocate(r8wrk3)
      r8wrk3 => null()
    end if
    !
    return
  end subroutine clear_wrk
  
  subroutine mf6_sol_write(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_sol) :: this
    ! -- local
! ------------------------------------------------------------------------------
    call this%write_wrap()
    call this%write_tdis()
    call this%write_ims()
    call this%write_mfsim()
    call this%write_post_map()
    !
    return
  end subroutine mf6_sol_write
!    
  subroutine mf6_sol_clean(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_sol) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    if (associated(this%solname))  deallocate(this%solname)
    if (associated(this%lmm))      deallocate(this%lmm)
    if (associated(this%nmod))     deallocate(this%nmod)
    if (associated(this%mod_id))   deallocate(this%mod_id)
    if (associated(this%npart))    deallocate(this%npart)
    if (associated(this%mod_part)) deallocate(this%mod_part)
    !
    this%solname  => null()
    this%lmm      => null()
    this%nmod     => null()
    this%mod_id   => null()
    this%npart    => null()
    this%mod_part => null()
    !
    return
  end subroutine mf6_sol_clean
  !
  subroutine mf6_sol_write_wrap(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_sol) :: this
    ! -- local
    integer(i4b), parameter :: iser = 1
    integer(i4b), parameter :: ipar = 2
    logical, dimension(2) :: lcpu
    character(len=4), dimension(2) :: pcpu
    data pcpu/'.ser','.par'/
    !
    character(len=mxslen) :: f, ms, s
    integer(i4b) :: iu, i, icpu, irun, irun0, irun1
! ------------------------------------------------------------------------------
    !
    if (ltransient) then
      irun0 = irun0tr
      irun1 = irun1tr
    else
      irun0 = irun0ss
      irun1 = irun1ss
    end if
    !
    lcpu = .true.
    if (this%nmod == 1) then
      lcpu(ipar) = .false.
    end if
    !
    ! models.asc
    do icpu = 1, 2
      if (.not.lcpu(icpu)) cycle
      do irun = irun0, irun1
        f = trim(this%solname)//pcpu(icpu)//'.models'//trim(pr(inam,irun))//'.asc'
        call open_file(f, iu, 'w')
        do i = 1, this%nmod
          ms =  'm'//ta((/this%mod_id(i)/),'(i5.5)')
          s = 'GWF6 ..\..\models\run_input\'//trim(ms)// &
            '\'//trim(ms)//trim(pr(inam,irun))//'.nam '//trim(ms)
          call swap_slash(s)
          if (icpu == ipar) then
            write(iu,'(a)') trim(s)//' '//ta((/this%mod_part(i)/))
          else
            write(iu,'(a)') trim(s)
          end if
        end do
        close(iu)
      end do
    end do
    !
    ! solmodels.asc
    f = trim(this%solname)//'.solmodels.asc'
    call open_file(f, iu, 'w')
    do i = 1, this%nmod
      ms = 'm'//ta((/this%mod_id(i)/),'(i5.5)'); write(iu,'(a)') trim(ms)
    end do
    close(iu)
    !
    ! cgc.solmodels.asc
    f = trim(this%solname)//'.cgc.solmodels.asc'
    call open_file(f, iu, 'w')
    do i = 1, this%nmod
      ms = 'm'//ta((/this%mod_id(i)/),'(i5.5)')
      write(iu,'(a)') trim(ms)//' '//ta((/i/))
    end do
    close(iu)
    !
    ! solmodels.asc.wrp
    f = trim(this%solname)//'.solmodels.wrp.asc'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN MODELS'
    s = '..\run_input\'//trim(this%solname)//'.solmodels.asc'
    call swap_slash(s)
    write(iu,'(2x,a)') 'OPEN/CLOSE '//trim(s)
    write(iu,'(   a)') 'END MODELS'
    close(iu)
    !
    ! cgc.solmodels.asc.wrp
    f = trim(this%solname)//'.cgc.solmodels.wrp.asc'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN MODELS'
    s = '..\run_input\'//trim(this%solname)//'.cgc.solmodels.asc'
    call swap_slash(s)
    write(iu,'(2x,a)') 'OPEN/CLOSE '//trim(s)
    write(iu,'(   a)') 'END MODELS'
    close(iu)
    !
    return
  end subroutine mf6_sol_write_wrap
  
  subroutine mf6_sol_write_mfsim(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_sol) :: this
    ! -- local
#ifdef LINUX
    logical, parameter :: lwritebat = .false.
#else
    logical, parameter :: lwritebat = .true.
#endif 
    integer(i4b), parameter :: iser = 1
    integer(i4b), parameter :: ipar = 2
    logical, dimension(2) :: lcpu
    character(len=4), dimension(2) :: pcpu
    data pcpu/'.ser','.par'/
    !
    character(len=mxslen) :: d, nam, ms, f, s, mpi_exe, mf6_exe
    integer(i4b) :: iu, ju, i, icpu, irun, irun0, irun1
! ------------------------------------------------------------------------------
    !
    d = '..\run_input\'; call swap_slash(d)
    !
    if (lwritebat) then
      mpi_exe = raw%getc('mpi_exe',cdef='mpiexec.exe')
      mf6_exe = raw%getc('mf6_exe',cdef='mf6.exe')
    end if
    !
    lcpu = .true.
    if (this%nmod == 1) then
      lcpu(ipar) = .false.
    end if
    !
    if (ltransient) then
      irun0 = irun0tr
      irun1 = irun1tr
    else
      irun0 = irun0ss+1
      irun1 = irun1ss
      if (.not.this%lmm) irun0 = 3
    end if
    !
    if (.not.ltransient .and.this%lmm) then
      if (lwritebat) then
        f = '..\run_output\run.'//trim(this%solname)//pcpu(iser)//trim(pr(inam,irun0ss))//'.bat'
        call swap_slash(f)
        call open_file(f, ju, 'w')
        write(ju,'(a)') 'set exe="'//trim(mf6_exe)//'"'
        write(ju,'(a)')
        write(ju,'(a)') '@echo off'
        write(ju,'(a)')
      end if
      do i = 1, this%nmod
        ms =  'm'//ta((/this%mod_id(i)/),'(i5.5)')
        nam = trim(this%solname)//pcpu(iser)//'.mfsim.'//trim(ms)//trim(pr(inam,irun0ss))//'.nam'
        call open_file(nam, iu, 'w')
        write(iu,'(   a)') 'BEGIN OPTIONS'
        write(iu,'(2x,a)') 'MEMORY_PRINT_OPTION SUMMARY'
        write(iu,'(   a)') 'END OPTIONS'
        write(iu,'(a)')
        write(iu,'(   a)') 'BEGIN TIMING'
        write(iu,'(2x,a)') 'TDIS6 '//trim(d)//trim(this%solname)//trim(pr(itdis,irun0ss))//'.tdis'
        write(iu,'(   a)') 'END TIMING'
        write(iu,'(a)')
        write(iu,'(   a)') 'BEGIN MODELS'
        s = 'GWF6 ..\..\models\run_input\'//trim(ms)// &
          '\'//trim(ms)//trim(pr(inam,irun0ss))//'.nam '//trim(ms)
        call swap_slash(s)
        write(iu,'(2x,a)') trim(s)
        write(iu,'(   a)') 'END MODELS'
        write(iu,'(a)')
        write(iu,'(   a)') 'BEGIN EXCHANGES'
        write(iu,'(   a)') 'END EXCHANGES'
        write(iu,'(a)')
        write(iu,'(   a)') 'BEGIN SOLUTIONGROUP 1'
        write(iu,'(2x,a)') 'IMS6 '//trim(d)//trim(this%solname)//'.ims '//trim(ms)
        write(iu,'(   a)') 'END SOLUTIONGROUP'
        close(iu)
        !
        if (lwritebat) then
          write(ju,'(a)') '%exe% -s ..\run_input\'//trim(nam)
        end if
      end do
      if (lwritebat) then
        write(ju,'(a)')
        write(ju,'(a)') 'echo Serial runs complete. Press any key to continue.'
        write(ju,'(a)') 'pause>nul'
        close(ju)
      end if
    end if
    !
    ! if parallel: also write the serial mfsim.nam
    do icpu = 1, 2
      if (.not.lcpu(icpu)) cycle
      do irun = irun0, irun1
        nam = trim(this%solname)//pcpu(icpu)//'.mfsim'//trim(pr(inam,irun))//'.nam'
        call open_file(nam, iu, 'w')
        write(iu,'(   a)') 'BEGIN OPTIONS'
        write(iu,'(2x,a)') 'MEMORY_PRINT_OPTION SUMMARY'
        if (icpu == ipar) write(iu,'(2x,a)') 'DOMAIN_DECOMPOSITION '//ta((/this%npart/))
        write(iu,'(   a)') 'END OPTIONS'
        write(iu,'(a)')
        write(iu,'(   a)') 'BEGIN TIMING'
        write(iu,'(2x,a)') 'TDIS6 '//trim(d)//trim(this%solname)//trim(pr(itdis,irun))//'.tdis'
        write(iu,'(   a)') 'END TIMING'
        write(iu,'(a)')
        write(iu,'(   a)') 'BEGIN MODELS'
        write(iu,'(2x,a)') 'OPEN/CLOSE '//trim(d)//trim(this%solname)//pcpu(icpu)//'.models'//trim(pr(inam,irun))//'.asc'
        write(iu,'(   a)') 'END MODELS'
        write(iu,'(a)')
        write(iu,'(   a)') 'BEGIN EXCHANGES'
        if (this%lmm) then
          write(iu,'(2x,a)') 'OPEN/CLOSE '//trim(d)//trim(this%solname)//'.exchanges.asc'
        end if
        write(iu,'(   a)') 'END EXCHANGES'
        write(iu,'(a)')
        write(iu,'(   a)') 'BEGIN SOLUTIONGROUP 1'
        if (this%lmm) then
          write(iu,'(2x,a)') '# IMS6_CGC '//trim(d)//trim(this%solname)//'.ims FILEIN '//trim(d)// &
            trim(this%solname)//'.cgc.solmodels.wrp.asc'
          write(iu,'(2x,a)') 'IMS6 '//trim(d)//trim(this%solname)//'.ims FILEIN '//trim(d)// &
            trim(this%solname)//'.solmodels.wrp.asc'
        else
          write(iu,'(2x,a)') 'IMS6 '//trim(d)//trim(this%solname)//'.ims FILEIN '//trim(d)// &
            trim(this%solname)//'.solmodels.wrp.asc'
        end if
        write(iu,'(   a)') 'END SOLUTIONGROUP'
        close(iu)
        !
        ! write the DOS batch file
        if (lwritebat) then
          f = '..\run_output\run.'//trim(this%solname)//pcpu(icpu)//trim(pr(inam,irun))//'.bat'
          call swap_slash(f)
          call open_file(f, iu, 'w')
          write(iu,'(a)') 'set exe="'//trim(mf6_exe)//'"'
          write(iu,'(a)') 'set nam=..\run_input\'//trim(nam)
          if (icpu == ipar) then
            write(iu,'(a)') 'set mpi="'//trim(mpi_exe)//'"'
            write(iu,'(a)') 'set np='//ta((/this%npart/))
          end if
          write(iu,'(a)')
          write(iu,'(a)') '@echo off'
          write(iu,'(a)')
          if (icpu == ipar) then
             write(iu,'(a)') '%mpi% -np %np% %exe% -s %nam%'
          else
             write(iu,'(a)') '%exe% -s %nam%'
          end if
          write(iu,'(a)') 'echo.'
          if (icpu == ipar) then
            write(iu,'(a)') 'echo Parallel run complete. Press any key to continue.'
          else
            write(iu,'(a)') 'echo Serial run complete. Press any key to continue.'
          end if
          write(iu,'(a)') '::pause>nul'
          close(iu)
        end if
      end do
    end do
    !
    return
  end subroutine mf6_sol_write_mfsim

  subroutine mf6_sol_write_post_map(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_sol) :: this 
    ! -- local
    character(len=mxslen) :: d, f
    integer(i4b) :: i, iu
! ------------------------------------------------------------------------------
    !
    d = '..\post_mappings\'; call create_dir(d, .true.)
    f = trim(d)//trim(this%solname)//'.modmap.bin'; call swap_slash(f)
    !
    call open_file(f, iu, 'w', .true.)
    write(iu) this%nmod
    write(iu)(this%mod_id(i),i=1, this%nmod)
    close(iu)
    !
    return
  end subroutine mf6_sol_write_post_map  
! 
  subroutine mf6_sol_write_tdis(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_sol) :: this
    ! -- local
    character(len=mxslen) :: f
    integer(i4b) :: iu, iper, jper, nstp_fac, nd, nperspu
! ------------------------------------------------------------------------------
    !
    if (.not.ltransient) then !SS
      f = trim(this%solname)//'.tdis'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(2x,a)') 'TIME_UNITS DAYS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'NPER 1'
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN PERIODDATA'
      write(iu,'(2x,a)') '1 1 1'
      write(iu,'(   a)') 'END PERIODDATA'
      close(iu)
    !
    else !TR
      nstp_fac = raw%geti('nstp_fac',idef=1)
      !
      nperspu = raw%geti('nyear_spinup')*12
      f = trim(this%solname)//trim(pr(itdis,irun0tr))//'.tdis'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(2x,a)') 'TIME_UNITS DAYS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'NPER '//ta((/nperspu/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN PERIODDATA'
      do iper = 1, nperspu
        !jper = mod(iper,nperspu)
        !if (jper == 0) jper = nperspu
        jper = mod(iper,12)
        if (jper == 0) jper = 1
        nd = get_month_days_s(raw%perdate(jper))
        write(iu,'(2x,a)') ta((/nd/))//' '//ta((/nd*nstp_fac/))//' 1'
      end do
      write(iu,'(   a)') 'END PERIODDATA'
      close(iu)
      !
      f = trim(this%solname)//'.tdis'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(2x,a)') 'TIME_UNITS DAYS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'NPER '//ta((/raw%nper/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN PERIODDATA'
      do iper = 1, raw%nper
        nd = get_month_days_s(raw%perdate(iper))
        write(iu,'(2x,a)') ta((/nd/))//' '//ta((/nd*nstp_fac/))//' 1'
      end do
      write(iu,'(   a)') 'END PERIODDATA'
      close(iu)
    end if
    !
    return
  end subroutine mf6_sol_write_tdis
  !
  subroutine mf6_sol_write_ims(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_sol) :: this
    ! -- local
    character(len=mxslen) :: f
    integer(i4b) :: iu
    logical :: lcomplex
    !
    integer(i4b), parameter :: nnlvar = 11
    integer(i4b), parameter :: nlvar  = 10
    !
    type tCompl
      character(len=mxslen) :: name = ''
      character(mxslen), dimension(2,nnlvar) :: nlvar
      character(mxslen), dimension(2,nlvar)  :: lvar
    end type tCompl
    !
    type(tCompl), pointer :: c => null()
    type(tCompl), dimension(:), pointer :: compl => null()
    integer(i4b) :: ncompl, i, j
    character(mxslen) :: cname, rclose_option
! ------------------------------------------------------------------------------
    !
    ! define the complexities
    ncompl = 3
    allocate(compl(ncompl))
    !
    ! SIMPLE
    i = 1; c => compl(i); c%name = 'SIMPLE'
    j =     1; c%nlvar(:,j) = (/'OUTER_HCLOSE'                  , '0.001'/)
    j = j + 1; c%nlvar(:,j) = (/'OUTER_MAXIMUM '                , '25'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION '             , 'NONE'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_THETA '       , '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_KAPPA '       , '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_GAMMA '       , '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_MOMENTUM '    , '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_NUMBER '          , '0'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_TOLERANCE '       , '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_REDUCTION_FACTOR ', '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_RESIDUAL_LIMIT '  , '0.0'/)
    !
    j =     1; c%lvar(:,j) = (/'INNER_MAXIMUM'                 , '50'/)
    j = j + 1; c%lvar(:,j) = (/'INNER_HCLOSE'                  , '0.001'/)
    j = j + 1; c%lvar(:,j) = (/'INNER_RCLOSE'                  , '0.1'/)
    j = j + 1; c%lvar(:,j) = (/'LINEAR_ACCELERATION'           , 'CG'/)
    j = j + 1; c%lvar(:,j) = (/'RELAXATION_FACTOR'             , '0.0'/)
    j = j + 1; c%lvar(:,j) = (/'PRECONDITIONER_LEVELS'         , '0'/)
    j = j + 1; c%lvar(:,j) = (/'PRECONDITIONER_DROP_TOLERANCE' , '0.0'/)
    j = j + 1; c%lvar(:,j) = (/'NUMBER_ORTHOGONALIZATIONS'     , '0'/)
    j = j + 1; c%lvar(:,j) = (/'SCALING_METHOD'                , 'NONE'/)
    j = j + 1; c%lvar(:,j) = (/'REORDERING_METHOD'             , 'NONE'/)
    !
    ! MODERATE
    i = 2; c => compl(i); c%name = 'MODERATE'
    j =     1; c%nlvar(:,j) = (/'OUTER_HCLOSE'                  , '0.01'/)
    j = j + 1; c%nlvar(:,j) = (/'OUTER_MAXIMUM '                , '50'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION '             , 'DBD'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_THETA '       , '0.9'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_KAPPA '       , '0.0001'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_GAMMA '       , '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_MOMENTUM '    , '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_NUMBER '          , '0'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_TOLERANCE '       , '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_REDUCTION_FACTOR ', '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_RESIDUAL_LIMIT '  , '0.0'/)
    !
    j =     1; c%lvar(:,j) = (/'INNER_MAXIMUM'                 , '100'/)
    j = j + 1; c%lvar(:,j) = (/'INNER_HCLOSE'                  , '0.01'/)
    j = j + 1; c%lvar(:,j) = (/'INNER_RCLOSE'                  , '0.1'/)
    j = j + 1; c%lvar(:,j) = (/'LINEAR_ACCELERATION'           , 'BICGSTAB'/)
    j = j + 1; c%lvar(:,j) = (/'RELAXATION_FACTOR'             , '0.97'/)
    j = j + 1; c%lvar(:,j) = (/'PRECONDITIONER_LEVELS'         , '0'/)
    j = j + 1; c%lvar(:,j) = (/'PRECONDITIONER_DROP_TOLERANCE' , '0.0'/)
    j = j + 1; c%lvar(:,j) = (/'NUMBER_ORTHOGONALIZATIONS'     , '0'/)
    j = j + 1; c%lvar(:,j) = (/'SCALING_METHOD'                , 'NONE'/)
    j = j + 1; c%lvar(:,j) = (/'REORDERING_METHOD'             , 'NONE'/)
    !
    ! COMPLEX
    i = 3; c => compl(i); c%name = 'COMPLEX'
    j =     1; c%nlvar(:,j) = (/'OUTER_HCLOSE'                  , '0.1'/)
    j = j + 1; c%nlvar(:,j) = (/'OUTER_MAXIMUM '                , '100'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION '             , 'DBD'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_THETA '       , '0.8'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_KAPPA '       , '0.0001'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_GAMMA '       , '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_MOMENTUM '    , '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_NUMBER '          , '20'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_TOLERANCE '       , '1.05'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_REDUCTION_FACTOR ', '0.1'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_RESIDUAL_LIMIT '  , '0.002'/)
    !
    j =     1; c%lvar(:,j) = (/'INNER_MAXIMUM'                 , '500'/)
    j = j + 1; c%lvar(:,j) = (/'INNER_HCLOSE'                  , '0.1'/)
    j = j + 1; c%lvar(:,j) = (/'INNER_RCLOSE'                  , '0.1'/)
    j = j + 1; c%lvar(:,j) = (/'LINEAR_ACCELERATION'           , 'BICGSTAB'/)
    j = j + 1; c%lvar(:,j) = (/'RELAXATION_FACTOR'             , '0.0'/)
    j = j + 1; c%lvar(:,j) = (/'PRECONDITIONER_LEVELS'         , '5'/)
    j = j + 1; c%lvar(:,j) = (/'PRECONDITIONER_DROP_TOLERANCE' , '0.0001'/)
    j = j + 1; c%lvar(:,j) = (/'NUMBER_ORTHOGONALIZATIONS'     , '2'/)
    j = j + 1; c%lvar(:,j) = (/'SCALING_METHOD'                , 'NONE'/)
    j = j + 1; c%lvar(:,j) = (/'REORDERING_METHOD'             , 'NONE'/)
    !
    cname = raw%getc('complexity',cdef='SIMPLE')
    select case (cname)
    case('SIMPLE')
      c => compl(1)
    case('MODERATE')
      c => compl(2)
    case('COMPLEX')
      c => compl(3)
    case default
      call errmsg("Invalid complexity: "//trim(cname))
    end select
    !
    ! overwrite the defaults
    do j = 1, nnlvar
      c%nlvar(2,j) = raw%getc(trim(c%nlvar(1,j)),cdef=trim(c%nlvar(2,j)))
    end do
    do j = 1, nlvar
      c%lvar(2,j) = raw%getc(trim(c%lvar(1,j)),cdef=trim(c%lvar(2,j)))
    end do
    rclose_option = raw%getc('rclose_option',cdef='')
    if (len_trim(rclose_option) > 0) then
      c%lvar(2,3) = trim(c%lvar(2,3))//' '//trim(rclose_option)
    end if
    !
    f = trim(this%solname)//'.ims'
    call open_file(f, iu, 'w')
    !
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(2x,a)') 'PRINT_OPTION '//raw%getc('print_option',cdef='ALLITER')
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN NONLINEAR'
    do j = 1, nnlvar
      write(iu,'(2x,a)') trim(c%nlvar(1,j))//' '//trim(trim(c%nlvar(2,j)))
    end do
    write(iu,'(   a)') 'END NONLINEAR'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN LINEAR'
    do j = 1, nlvar
      write(iu,'(2x,a)') trim(c%lvar(1,j))//' '//trim(trim(c%lvar(2,j)))
    end do
    write(iu,'(   a)') 'END LINEAR'
    close(iu)
    !
    deallocate(compl)
    !
    return
  end subroutine mf6_sol_write_ims

! ==============================================================================
! ==============================================================================
! ==============================================================================
! ==============================================================================
! ==============================================================================
  
  function mf6_mod_get_i_raw(this, i_dat, ilay_dat, iper_dat) &
    result(i_raw)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: i_dat
    integer(i4b), intent(in) :: ilay_dat
    integer(i4b), intent(in) :: iper_dat
    integer(i4b) :: i_raw
    ! -- local
! ------------------------------------------------------------------------------
    i_raw = raw%mf6_raw_get_index(keys(i_dat), ilay_dat, iper_dat)
    if (i_raw <= 0) then
      call errmsg('mf6_mod_get_array_r8: program error 1')
    end if
    !
    return
  end function mf6_mod_get_i_raw
  !
  function mf6_mod_get_val_r8(this, i_raw, i_dat, ilay_dat, iper_dat, ireg, ic, ir) &
    result(r8val)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: i_raw
    integer(i4b), intent(in) :: i_dat
    integer(i4b), intent(in) :: ilay_dat
    integer(i4b), intent(in) :: iper_dat
    integer(i4b), intent(in) :: ireg
    integer(i4b), intent(in) :: ic
    integer(i4b), intent(in) :: ir
    real(r8b) :: r8val
    ! -- local
    type(tReg), pointer :: reg => null()
    type(tData), pointer :: dat => null()
    type(idfobj), pointer :: idf
    integer(i4b) :: itile, gir, gic
    real(r4b) :: r4val
! ------------------------------------------------------------------------------
    dat => raw%raw(i_raw)%dat
    !
    select case(dat%file_type)
      case(i_idf)
        idf => dat%idf
        r8val = readidf_r8val(idf, gic, gir)
      case(i_distidf)
        call dat%distidf%init(dat%s, this%itile, ilay_dat, iper_dat, .true.)
        reg => this%reg(ireg)
        gir = ir + reg%bb%ir0 - 1; gic = ic + reg%bb%ic0 - 1
        itile = int(reg%itile(ic,ir),i4b)
        if (itile == 0) then
          if (associated(dat%distidf%r8def)) then
            call logmsg("WARNING: default value is taken!")
            r8val = dat%distidf%r8def
            if (dat%distidf%larea) then
              if (.not.associated(cam2)) then
                call logmsg("WARNING: cam2 is not defined!")
              end if
              r8val = r8val * cam2(ir)/(gcs*gcs)
            end if
          else
            r8val = DZERO
          end if
        else
          if (itile < 0) then
            itile = -itile
          end if
          r8val = dat%distidf%getval(gic, gir, itile)
        end if
      case(i_map)
        call errmsg("Error mf6_raw_read_block_r4b: map file not supported")
      case(i_distmap)
        call dat%distmap%init(dat%s, this%itile, ilay_dat, iper_dat, .true.)
        reg => this%reg(ireg)
        gir = ir + reg%bb%ir0 - 1; gic = ic + reg%bb%ic0 - 1
        itile = int(reg%itile(ic,ir),i4b)
        if (itile == 0) then
          if (associated(dat%distmap%r4def)) then
            call logmsg("WARNING: default value is taken!")
            r4val = dat%distmap%r4def
            if (dat%distmap%larea) then
              if (.not.associated(cam2)) then
                call logmsg("WARNING: cam2 is not defined!")
              end if
              r4val = r4val * cam2(ir)/(gcs*gcs)
            end if
          else
            r4val = RZERO
          end if
        else
          if (itile < 0) then
            itile = -itile
          end if
          r4val = dat%distmap%getval(gic, gir, itile)
        end if
        r8val = real(r4val,r8b)
    end select
    !
    r8val = r8val *dat%r8mult + dat%r8add
      
    return
  end function mf6_mod_get_val_r8
  
  subroutine mf6_mod_get_array_r8(this, i_dat, ilay_dat, iper_dat, ilay_tgt, &
    arrflg, arr, ib_in, toponly_in)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: i_dat
    integer(i4b), intent(in) :: ilay_dat
    integer(i4b), intent(in) :: iper_dat
    integer(i4b), intent(in) :: ilay_tgt
    integer(i1b), dimension(:), pointer, intent(inout) :: arrflg
    real(r8b), dimension(:), pointer, intent(inout) :: arr
    integer(i4b), intent(in), optional :: ib_in
    logical, intent(in), optional :: toponly_in
    ! -- local
    real(r4b), parameter :: r4nodata = -12345.
    real(r8b), parameter :: r8nodata = -12345.d0
    type(tReg), pointer :: reg
    type(tData), pointer :: dat => null()
    type(idfobj), pointer :: idf
    integer(i4b) :: n, nt, i, ireg, ir, ic, jr, jc, ib, itile, ilay, nlay
    integer(i4b) :: arrsiz
    real(r4b) :: r4val
    real(r8b) :: r8val
    logical :: lfirst, ltile, toponly, found, ldefault
! ------------------------------------------------------------------------------
    lfirst = .true.
    ltile = .true.
    ldefault = .false.
    !
    if (present(ib_in)) then
      ib = ib_in
    else
      ib = 0
    end if
    !
    if (present(toponly_in)) then
      toponly = toponly_in
    else
      toponly = .false.
    end if
    !
    i = raw%mf6_raw_get_index(keys(i_dat), ilay_dat, iper_dat)
    if (i <= 0) then
      call errmsg('mf6_mod_get_array_r8: program error 1')
    end if
    dat => raw%raw(i)%dat
    !
    n = sum(this%layer_nodes)
    if (.not.associated(arr)) then
      allocate(arr(n))
      do i = 1, size(arr)
        arr(i) = DZERO
      end do
    end if
    if (.not.associated(arrflg)) then
      allocate(arrflg(n))
      do i = 1, size(arrflg)
        arrflg(i) = 0
      end do
    end if
    !
    if (size(arrflg) /= size(arr)) then
      call errmsg('mf6_mod_get_array_r8: program error 1')
    end if
    arrsiz = size(arrflg)
    
    select case(dat%file_type)
      case(i_idf)
        idf => dat%idf
      case(i_distidf)
        call dat%distidf%init(dat%s, this%itile, ilay_dat, iper_dat)
      case(i_map)
        call errmsg("Error mf6_raw_read_block_r4b: map file not supported")
      case(i_distmap)
        call dat%distmap%init(dat%s, this%itile, ilay_dat, iper_dat)
    end select
    
    do ireg = 1, this%nreg
      reg => this%reg(ireg)
      nlay = size(reg%nodmap,3)
      do ir = reg%bb%ir0, reg%bb%ir1
        do ic = reg%bb%ic0, reg%bb%ic1
          jr = ir - reg%bb%ir0 + 1; jc = ic - reg%bb%ic0 + 1
          select case(ib)
            case(0) ! all cells including sea cells
              n = reg%nodmap(jc,jr,ilay_tgt)
              n = abs(n)
              !if (n == 116498) then
              !  write(*,*) '@@@@'
              !end if
            case(1) ! sea cells only
              n = reg%nodmap(jc,jr,ilay_tgt)
              if (n < 0) then
                n = abs(n)
              else
                n = 0
              end if
            case(2) ! all cells excluding sea cells
              n = reg%nodmap(jc,jr,ilay_tgt)
              if (n < 0) then
                n = 0
              end if
            case(3) ! internal M1 interface cells only (skip sea)
              n = reg%bndmap(jc,jr,ilay_tgt)
              if (n < 0) then
                n = 0
              end if
            case default
              call errmsg('mf6_mod_get_array_r8: program error 1')
          end select
          !
          ! only select active cells seen from top
          if (toponly.and.(n /= 0)) then
            found = .false.
            do ilay = 1, nlay
              nt = reg%nodmap(jc,jr,ilay)
              if (nt /= 0) then 
                found = .true.
                exit
              end if
            end do
            if (found .and. (nt /= n)) then
              n = 0
            end if
          end if
          !
          if (n < 0) then 
            call errmsg('mf6_mod_get_array_r8: program error 2')
          end if
          if (n /= 0) then
            select case(dat%file_type)
              case(i_idf)
                r8val = readidf_r8val(idf, ic, ir)
                if (r8val == idf%nodata) then
                  if (lfirst) then
                    call logmsg('WARNING, using nodata from '//trim(idf%fname)//'!')
                    lfirst = .false.
                  end if
                end if
              case(i_distidf)
                itile = int(reg%itile(jc,jr),i4b)
                if (itile == 0) then
                  if (associated(dat%distidf%r8def)) then
                    ldefault = .true.
                    r8val = dat%distidf%r8def
                    if (dat%distidf%larea) then
                      if (.not.associated(cam2)) then
                        call logmsg("WARNING: cam2 was not defined!")
                      end if
                      r8val = r8val * cam2(ir)/(gcs*gcs)
                    end if
                  else
                    r8val = DZERO
                    ltile = .false.
                  end if
                else
                  if (itile < 0) then
                    itile = -itile
                  end if
                  r8val = dat%distidf%getval(ic, ir, itile, r8nodata)
                  if (r8val == r8nodata) then
                    n = 0
                  end if
                end if
              case(i_distmap)
                !if (n == 2330) then
                !  write(*,*) '@@@@@'
                !end if
                itile = int(reg%itile(jc,jr),i4b)
                if (itile == 0) then
                  if (associated(dat%distmap%r4def)) then
                    ldefault = .true.
                    r4val = dat%distmap%r4def
                    if (dat%distmap%larea) then
                      if (.not.associated(cam2)) then
                        call logmsg("WARNING: cam2 was not defined!")
                      end if
                      r4val = r4val * cam2(ir)/(gcs*gcs)
                    end if
                  else
                    r4val = RZERO
                    ltile = .false.
                  end if
                else
                  if (itile < 0) then
                    itile = -itile
                  end if
                  r4val = dat%distmap%getval(ic, ir, itile, r4nodata)
                  if (r4val == r4nodata) then
                    n = 0
                  end if
                end if
                r8val = real(r4val,r8b)
            end select
            if (n /= 0) then
              if ((n < 1) .or. (n > arrsiz)) then
                call errmsg('mf6_mod_get_array_r8: program error 3 '//ta((/ib/))//' '//&
                  ta((/n/))//' '//ta((/arrsiz/)))
              end if
              arr(n) = r8val * dat%r8mult + dat%r8add
              arrflg(n) = 1
            end if
          end if
        end do
      end do
    end do
    !
    if (.not.ltile) then
      call logmsg('WARNING, for some points the tile was not found!')
    end if
    if (ldefault) then
      call logmsg('WARNING, for some points default values were taken!')
    end if
    !
    return
  end subroutine mf6_mod_get_array_r8
!
  subroutine mf6_mod_write_post_map(this, iwrite)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: iwrite
    ! -- local
    character(len=mxslen) :: d
    type(tReg), pointer :: reg => null()
    type(tBb), pointer :: bb => null()
    character(len=mxslen) :: f
    integer(i4b) :: ireg, nodes, n, iu, il, ir, ic, kr, kc, nlay, gic, gir, gil, i, j
    real(r8b) :: xmin, ymin
! ------------------------------------------------------------------------------
    !
    nodes = 0
    do ireg = 1, this%nreg
      reg => this%reg(ireg)
      do il = 1, size(reg%layer_nodes)
        nodes = nodes + reg%layer_nodes(il)
      end do
    end do
    !
    call clear_wrk()
    allocate(i4wrk2d(3,nodes))
    do i = 1, nodes
      do j = 1, 3
        i4wrk2d(j,i) = 0
      end do
    end do
    !
    do ireg = 1, this%nreg
      reg => this%reg(ireg)
      do il = 1, size(reg%layer_nodes)
        bb => reg%bb
        nlay = size(reg%nodmap,3)
        do ir = 1, bb%nrow
          do ic = 1, bb%ncol
            n = reg%nodmap(ic,ir,il)
            gil = il; gic = ic + bb%ic0 - 1; gir = ir + bb%ir0 - 1
            if (n < 0) then
              gil = -gil
            end if
            if (n /= 0) then
              n = abs(n)
              if (i4wrk2d(1,n) /= 0) then
                call errmsg('mf6_mod_write_post_map: program error')
              end if
              i4wrk2d(1,n) = gil; i4wrk2d(2,n) = gir; i4wrk2d(3,n) = gic
            end if
          end do
        end do
      end do
    end do
    !
    ! check
    do i = 1, nodes
      if (i4wrk2d(1,i) == 0) then
        call errmsg('mf6_mod_write_post_map: program error')
      end if
    end do
    !
    d = '..\..\models\post_mappings\'
    call create_dir(d, .true.)
    f = trim(d)//trim(this%modelname)//'.nodmap.bin'
    call swap_slash(f)
    call open_file(f, iu, 'w', .true.)
    write(iu) this%bb%ic0, this%bb%ic1, this%bb%ir0, this%bb%ir1
    write(iu) nodes ! number of nodes
    write(iu)((i4wrk2d(j,i),j=1,3),i=1,nodes)
    close(iu)
    !
    if (iwrite == 1) then
      call clear_wrk()
      allocate(i4wrk2d(this%bb%ncol, this%bb%nrow))
      do il = 1, gnlay
        do ir = 1, this%bb%nrow
          do ic = 1, this%bb%ncol
            i4wrk2d(ic,ir) = 0
          end do
        end do
        do ireg = 1, this%nreg
          reg => this%reg(ireg)
          bb => reg%bb
          do ir = 1, bb%nrow
            do ic = 1, bb%ncol
              n = reg%nodmap(ic,ir,il)
              gic = ic + bb%ic0 - 1; gir = ir + bb%ir0 - 1
              kc = gic - this%bb%ic0 + 1
              kr = gir - this%bb%ir0 + 1
              i4wrk2d(kc,kr) = n
            end do
          end do
        end do
        f = trim(d)//trim(this%modelname)//'.nodmap.l'//ta((/il/),'(i2.2)')
        call swap_slash(f)
        xmin = gxmin + (this%bb%ic0-1)*gcs
        ymin = gymin + (gnrow-this%bb%ir1)*gcs
        call writeflt(f, i4wrk2d, this%bb%ncol, this%bb%nrow, &
          dble(xmin), dble(ymin), dble(gcs), 0)
      end do
    end if
    !
    return
  end subroutine mf6_mod_write_post_map
!
  subroutine mf6_mod_clean_regions(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    ! -- local
    type(tReg), pointer :: reg
    integer(i4b) :: ireg
! ------------------------------------------------------------------------------
    !
    if (.not.associated(this%nreg)) return
    !
    do ireg = 1, this%nreg
      reg => this%reg(ireg)
      if (associated(reg%bb))          deallocate(reg%bb)
      if (associated(reg%itile))       deallocate(reg%itile)
      if (associated(reg%layer_nodes)) deallocate(reg%layer_nodes)
      if (associated(reg%nodmap))     deallocate(reg%nodmap)
      if (associated(reg%bndmap))     deallocate(reg%bndmap)
    end do
    !
    deallocate(this%nreg); this%nreg => null()
    deallocate(this%reg);  this%reg  => null()
    !
    return
  end subroutine mf6_mod_clean_regions
    
  function mf6_mod_count_i1a(this, i1a) result(cnt)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i1b), dimension(:), intent(in) :: i1a
    integer(i4b), dimension(gnlay) :: cnt
    ! -- local
    type(tReg), pointer :: reg
    type(tBb), pointer :: bb => null()
    integer(i4b) :: ireg, il, ir, ic, n
! ------------------------------------------------------------------------------
    !
    cnt = 0
    !
    if (.not.associated(this%nreg)) return
    !
    do ireg = 1, this%nreg
      reg => this%reg(ireg)
      bb => reg%bb
      do il = 1, gnlay
        do ir = 1, bb%nrow
          do ic = 1, bb%ncol
            n = abs(reg%nodmap(ic,ir,il))
            if (n > 0) then
              if (i1a(n) == 1) then
                cnt(il) = cnt(il) + 1
              end if
            end if
          end do
        end do
      end do
    end do
    !
    call logmsg('#cells assigned to layers: '//ta(cnt))
    !
    return
  end function mf6_mod_count_i1a
  
  subroutine mf6_mod_get_model_name(this, i, modelname)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: i
    character(len=*), intent(out) :: modelname
    ! -- local
! ------------------------------------------------------------------------------
    write(modelname,'(a,i5.5)') 'm', i
    !
    return
  end subroutine mf6_mod_get_model_name
  !  
  subroutine mf6_mod_set_disu(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    ! -- local
    real(r8b), parameter :: nodata = -9999.d0
    real(r8b), parameter :: thkmin = 0.1d0
    !
    integer(i4b), dimension(ns) :: ihc
    !        p t n w e s b
    data ihc/0,0,1,1,1,1,0/
    type(tBb), pointer :: bb => null()
    real(r8b), dimension(ns) :: hwva, cl12
    type(tDisu), pointer :: disu => null()
    type(tReg), pointer :: reg => null()
    integer(i4b) :: ireg, n, ilay, iact, nja
    integer(i4b), dimension(ns) :: s
    integer(i4b) :: ic, ir, i, top_i_raw, bot_i_raw
    real(r8b) :: topval, botval
! ------------------------------------------------------------------------------
    !
    hwva(jp) = DZERO
    hwva(jn) = gcs
    hwva(js) = gcs
    hwva(jw) = gcs
    hwva(je) = gcs
    hwva(jt) = gcs*gcs
    hwva(jb) = gcs*gcs
    !
    allocate(this%disu)
    disu => this%disu
    !
    allocate(disu%nodes)
    disu%nodes = 0
    do ireg = 1, this%nreg
      reg => this%reg(ireg)
      disu%nodes = disu%nodes + sum(reg%layer_nodes)
    end do
    !
    ! iac
    allocate(disu%iac(disu%nodes))
    allocate(disu%nja)
    !
    do iact = 1, 2
      do n = 1, disu%nodes
        disu%iac(n) = 0
      end do
      disu%nja = 0
      s = 0; cl12 = DZERO
      !
      do ilay = 1, gnlay
        do ireg = 1, this%nreg
          reg => this%reg(ireg); bb => reg%bb
          if (ilay == 1) then
            top_i_raw = this%get_i_raw(i_top, 0, 0)
            bot_i_raw = this%get_i_raw(i_bot, 1, 0)
          else if (ilay == 2) then
            top_i_raw = this%get_i_raw(i_bot, 1, 0)
            bot_i_raw = this%get_i_raw(i_bot, 2, 0)
          end if
          !
          do ir = 1, reg%bb%nrow
            do ic = 1, reg%bb%ncol
              n = reg%nodmap(ic,ir,ilay)
              n = abs(n)
              if (n /= 0) then
                s(jp) = n !CENTER
                if (ir > 1) then !NORTH
                  s(jn) = abs(reg%nodmap(ic,ir-1,ilay))
                  if (s(jn) /= 0) then
                    cl12(jn) = gcs/2
                  end if
                end if
                if (ir < reg%bb%nrow) then !SOUTH
                  s(js) = abs(reg%nodmap(ic,ir+1,ilay))
                  if (s(js) /= 0) then
                    cl12(js) = gcs/2
                  end if
                end if
                if (ic > 1) then !WEST
                  s(jw) = abs(reg%nodmap(ic-1,ir,ilay))
                  if (s(jw) /= 0) then
                    cl12(jw) = gcs/2
                  end if
                end if
                if (ic < reg%bb%ncol) then !EAST
                  s(je) = abs(reg%nodmap(ic+1,ir,ilay))
                  if (s(je) /= 0) then
                    cl12(je) = gcs/2
                  end if
                end if
                !if (n == 7864) then
                !  write(*,*) '@@@@@'
                !end if
                if (ilay == 1) then
                  topval = this%get_val(top_i_raw, i_top, 0, 0, ireg, ic, ir)
                  botval = this%get_val(bot_i_raw, i_bot, 1, 0, ireg, ic, ir)
                else if (ilay == 2) then
                  topval = this%get_val(top_i_raw, i_bot, 1, 0, ireg, ic, ir)
                  botval = this%get_val(bot_i_raw, i_bot, 2, 0, ireg, ic, ir)
                end if
                !
                if (ilay > 1) then !TOP
                  s(jt) = abs(reg%nodmap(ic,ir,ilay-1))
                  if ((topval == nodata).or.(botval == nodata)) then
                    call errmsg('mf6_mod_set_disu: program error (top/bot).')
                  end if
                  cl12(jt) = max(thkmin, topval-botval)
                  cl12(jt) = cl12(jt)/2
                end if
                if (ilay < gnlay) then !BOT
                  s(jb) = abs(reg%nodmap(ic,ir,ilay+1))
                  if ((topval == nodata).or.(botval == nodata)) then
                    call errmsg('mf6_mod_set_disu: program error (top/bot).')
                  end if
                  cl12(jb) = max(thkmin, topval-botval)
                  cl12(jb) = cl12(jb)/2
                end if
                if ((n < 1).or.(n > disu%nodes)) then
                 call errmsg('mf6_mod_set_disu: program error')
                end if
                do i = 1, ns
                  if (s(i) /= 0) then
                    if (s(i) < 0) then
                      call errmsg('mf6_mod_set_disu: program error')
                    end if
                    disu%iac(n) = disu%iac(n) + 1
                    disu%nja = disu%nja + 1
                    if (iact == 2) then
                      if ((disu%nja < 1).or.(disu%nja > nja)) then
                       call errmsg('mf6_mod_set_disu: program error')
                      end if
                      disu%ja(disu%nja)   = s(i)
                      disu%ihc(disu%nja)  = ihc(i)
                      disu%cl12(disu%nja) = cl12(i)
                      disu%hwva(disu%nja) = hwva(i)
                    end if
                  end if
                  s(i) = 0
                  cl12(i) = DZERO
                end do
                !
              end if
            end do
          end do
        end do !ilay
      end do !rreg
      if (iact == 1) then
        nja = disu%nja
        allocate(disu%ja(disu%nja))
        allocate(disu%ihc(disu%nja))
        allocate(disu%cl12(disu%nja))
        allocate(disu%hwva(disu%nja))
        do i = 1, disu%nja
          disu%ihc(i) = 0
        end do
      end if
    end do !iact
    !
    return
  end subroutine mf6_mod_set_disu
  
  subroutine mf6_mod_write_array_i4(this, iu, nx, f, arr, lbin, lbinpos_in)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: iu 
    integer(i4b), intent(in) :: nx
    character(len=*), intent(inout) :: f
    integer(i4b), dimension(:), intent(in) :: arr
    logical, intent(in) :: lbin
    logical, intent(in), optional :: lbinpos_in
    ! -- local
    integer(i4b), parameter :: i4dum = 1
    real(r8b), parameter :: r8dum = DZERO
    character(len=16) :: c16dum = ''
    integer(i4b) :: ju, i, narr
    integer(i8b) :: p0, p1
    character(len=mxslen) :: fmt
    logical :: lbinpos
! ------------------------------------------------------------------------------
    if (present(lbinpos_in)) then
      lbinpos = lbinpos_in
    else
      lbinpos = .false.
    end if
    !    
    write(fmt,'(a,i,a)') '(',nx,'x,a)'
    if (lbin) then
      if (.not.lbinpos) then
        f = trim(f)//'.bin'
        call open_file(f, ju, 'w', .true.)
      else
        f = this%fbin
        ju = this%iubin
      end if
      narr = size(arr)
      if (lbinpos) inquire(ju,pos=p0)
      write(ju) i4dum, i4dum, r8dum, r8dum, c16dum, narr, i4dum, i4dum
      write(ju) arr
      if (lbinpos) then
        inquire(ju,pos=p1)
      else
        close(ju)
      end if
      !call get_rel_up(f, 2)
      if (.not.lbinpos) then
        write(iu,fmt) 'OPEN/CLOSE '//trim(f)//' (BINARY)'
      else
        write(iu,fmt) 'OPEN/CLOSE '//trim(f)//' (BINARY) '//ta((/p0,p1/))
      end if
    else
      f = trim(f)//'.asc'
      call open_file(f, ju, 'w')
      do i = 1, size(arr)
        write(ju,'(a)') ta((/arr(i)/))
      end do
      close(ju)
      !call get_rel_up(f, 2)
      write(iu,fmt) 'OPEN/CLOSE '//trim(f)
    end if
    !
    return
  end subroutine mf6_mod_write_array_i4
  
  subroutine mf6_mod_write_array_r8(this, iu, nx, f, arr, lbin, lbinpos_in)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: iu 
    integer(i4b), intent(in) :: nx
    character(len=*), intent(inout) :: f
    real(r8b), dimension(:), intent(in) :: arr
    logical, intent(in) :: lbin
    logical, intent(in), optional :: lbinpos_in
    ! -- local
    integer(i4b), parameter :: i4dum = 1
    real(r8b), parameter :: r8dum = DZERO
    character(len=16) :: c16dum = ''
    integer(i4b) :: ju, i, narr
    integer(i8b) :: p0, p1
    character(len=mxslen) :: fmt
    logical :: lbinpos
! ------------------------------------------------------------------------------
    if (present(lbinpos_in)) then
      lbinpos = lbinpos_in
    else
      lbinpos = .false.
    end if
    !
    write(fmt,'(a,i,a)') '(',nx,'x,a)'
    if (lbin) then
      if (.not.lbinpos) then
        f = trim(f)//'.bin'
        call open_file(f, ju, 'w', .true.)
      else
        f = this%fbin
        ju = this%iubin
      end if
      if (lbinpos) inquire(ju,pos=p0)
      narr = size(arr)
      write(ju) i4dum, i4dum, r8dum, r8dum, c16dum, narr, i4dum, i4dum
      write(ju) arr
      if (lbinpos) then
        inquire(ju,pos=p1)
      else
        close(ju)
      end if
      !call get_rel_up(f, 2)
      if (.not.lbinpos) then
        write(iu,fmt) 'OPEN/CLOSE '//trim(f)//' (BINARY)'
      else
        write(iu,fmt) 'OPEN/CLOSE '//trim(f)//' (BINARY) '//ta((/p0,p1/))
      end if
    else
      f = trim(f)//'.asc'
      call open_file(f, ju, 'w')
      do i = 1, size(arr)
        write(ju,'(a)') ta((/arr(i)/))
      end do
      close(ju)
      !call get_rel_up(f, 2)
      write(iu,fmt) 'OPEN/CLOSE '//trim(f)
    end if
    !
    return
  end subroutine mf6_mod_write_array_r8
  
  subroutine mf6_mod_write_list_1(this, iu, nx, f, arrflg, arr, lbin, lbinpos, s)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: iu 
    integer(i4b), intent(in) :: nx
    character(len=*), intent(inout) :: f
    integer(i1b), dimension(:), intent(in) :: arrflg
    real(r8b), dimension(:), intent(in) :: arr
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    character(len=*), intent(out), optional :: s
    ! -- local
    integer(i4b), dimension(:), allocatable :: i4w
    real(r8b), dimension(:), allocatable :: r8w1
    integer(i4b) :: ju, i, n
    integer(i8b) :: p0, p1
    character(len=mxslen) :: fmt
    logical :: liu
! ------------------------------------------------------------------------------
    !
    if (present(s)) then
      liu = .false.
    else
      liu = .true.
    end if
    !
    write(fmt,'(a,i,a)') '(',nx,'x,a)'
    if (lbin) then
      if (.not.lbinpos) then
        f = trim(f)//'.bin'
        call open_file(f, ju, 'w', .true.)
      else
        f = this%fbin
        ju = this%iubin
      end if
      n = 0
      do i = 1, size(arrflg)
        if (arrflg(i) == 1) then
          n = n + 1
        end if
      end do
      if (lbinpos) then
        inquire(ju,pos=p0)
      end if
      if (n > 0) then
        allocate(i4w(n),r8w1(n))
        n = 0
        do i = 1, size(arrflg)
          if (arrflg(i) == 1) then
            n = n + 1
            i4w(n) = i; r8w1(n) = arr(i)
          end if
        end do
        write(ju)((i4w(i),r8w1(i)),i=1,n)
        deallocate(i4w,r8w1)
      end if
      if (lbinpos) then
        inquire(ju,pos=p1)
      else
        close(ju)
      end if
      !call get_rel_up(f, 2)
      if (liu) then
        if (lbinpos) then
          write(iu,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY) '//ta((/p0,p1/))
        else
          write(iu,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY)'
        end if
      else
        if (lbinpos) then
          write(s,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY) '//ta((/p0,p1/))
        else
          write(s,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY)'
        end if
      end if
    else
      f = trim(f)//'.asc'
      call open_file(f, ju, 'w')
      do i = 1, size(arrflg)
        if (arrflg(i) == 1) then
          write(ju,'(a)') ta((/i/))//' '//ta((/arr(i)/))
        end if
      end do
      close(ju)
      !call get_rel_up(f, 2)
      if (liu) then
        write(iu,fmt) 'OPEN/CLOSE '//trim(f)
      else
        write(s,fmt) 'OPEN/CLOSE '//trim(f)
      end if
    end if
    !
    return
  end subroutine mf6_mod_write_list_1
  
  subroutine mf6_mod_write_list_2(this, iu, nx, f, arrflg, arr, arr2, lbin, lbinpos, s)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: iu 
    integer(i4b), intent(in) :: nx
    character(len=*), intent(inout) :: f
    integer(i1b), dimension(:), intent(in) :: arrflg
    real(r8b), dimension(:), intent(in) :: arr
    real(r8b), dimension(:), intent(in) :: arr2
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    character(len=*), intent(out), optional :: s
    ! -- local
    integer(i4b), dimension(:), allocatable :: i4w
    real(r8b), dimension(:), allocatable :: r8w1, r8w2
    integer(i4b) :: ju, i, n
    integer(i8b) :: p0, p1
    character(len=mxslen) :: fmt
    logical :: liu
! ------------------------------------------------------------------------------
    !
    if (present(s)) then
      liu = .false.
    else
      liu = .true.
    end if
    !
    write(fmt,'(a,i,a)') '(',nx,'x,a)'
    if (lbin) then
      if (.not.lbinpos) then
        f = trim(f)//'.bin'
        call open_file(f, ju, 'w', .true.)
      else
        f = this%fbin
        ju = this%iubin
      end if
      n = 0
      do i = 1, size(arrflg)
        if (arrflg(i) == 1) then
          n = n + 1
        end if
      end do
      if (lbinpos) then
        inquire(ju,pos=p0)
      end if
      if (n > 0) then
        allocate(i4w(n),r8w1(n),r8w2(n))
        n = 0
        do i = 1, size(arrflg)
          if (arrflg(i) == 1) then
            n = n + 1
            i4w(n) = i; r8w1(n) = arr(i); r8w2(n) = arr2(i)
          end if
        end do
        write(ju)((i4w(i),r8w1(i),r8w2(i)),i=1,n)
        deallocate(i4w,r8w1,r8w2)
      end if
      if (lbinpos) then
        inquire(ju,pos=p1)
      else
        close(ju)
      end if
      !call get_rel_up(f, 2)
      if (liu) then
        if (lbinpos) then
          write(iu,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY) '//ta((/p0,p1/))
        else
          write(iu,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY)'
        end if
      else
        if (lbinpos) then
          write(s,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY) '//ta((/p0,p1/))
        else
          write(s,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY)'
        end if
      end if
    else
      f = trim(f)//'.asc'
      call open_file(f, ju, 'w')
      do i = 1, size(arrflg)
        if (arrflg(i) == 1) then
          write(ju,'(a)') ta((/i/))//' '//ta((/arr(i)/))//' '//ta((/arr2(i)/))
        end if
      end do
      close(ju)
      !call get_rel_up(f, 2)
      if (liu) then
        write(iu,fmt) 'OPEN/CLOSE '//trim(f)
      else
        write(s,fmt) 'OPEN/CLOSE '//trim(f)
      end if
    end if
    !
    return
  end subroutine mf6_mod_write_list_2
  
  subroutine mf6_mod_write_list_3(this, iu, nx, f, arrflg, arr, arr2, arr3, &
    lbin, lbinpos, s)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: iu 
    integer(i4b), intent(in) :: nx
    character(len=*), intent(inout) :: f
    integer(i1b), dimension(:), intent(in) :: arrflg
    real(r8b), dimension(:), intent(in) :: arr
    real(r8b), dimension(:), intent(in) :: arr2
    real(r8b), dimension(:), intent(in) :: arr3
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    character(len=*), intent(out), optional :: s
    ! -- local
    integer(i4b), dimension(:), allocatable :: i4w
    real(r8b), dimension(:), allocatable :: r8w1, r8w2, r8w3
    integer(i4b) :: ju, i, n
    integer(i8b) :: p0, p1
    character(len=mxslen) :: fmt
    logical :: liu
! ------------------------------------------------------------------------------
    if (present(s)) then
      liu = .false.
    else
      liu = .true.
    end if
    !
    write(fmt,'(a,i,a)') '(',nx,'x,a)'
    if (lbin) then
      if (.not.lbinpos) then
        f = trim(f)//'.bin'
        call open_file(f, ju, 'w', .true.)
      else
        f = this%fbin
        ju = this%iubin
      end if
      n = 0
      do i = 1, size(arrflg)
        if (arrflg(i) == 1) then
          n = n + 1
        end if
      end do
      if (lbinpos) then
        inquire(ju,pos=p0)
      end if
      if (n > 0) then
        allocate(i4w(n),r8w1(n),r8w2(n),r8w3(n))
        n = 0
        do i = 1, size(arrflg)
          if (arrflg(i) == 1) then
            n = n + 1
            i4w(n) = i; r8w1(n) = arr(i); r8w2(n) = arr2(i); r8w3(n) = arr3(i)
          end if
        end do
        write(ju)((i4w(i),r8w1(i),r8w2(i),r8w3(i)),i=1,n)
        deallocate(i4w,r8w1,r8w2,r8w3)
      end if
      if (lbinpos) then
        inquire(ju,pos=p1)
      else
        close(ju)
      end if
      !call get_rel_up(f, 2)
      if (liu) then
        if (lbinpos) then
          write(iu,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY) '//ta((/p0,p1/))
        else
          write(iu,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY)'
        end if
      else
        if (lbinpos) then
          write(s,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY) '//ta((/p0,p1/))
        else
          write(s,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY)'
        end if
      end if
    else
      f = trim(f)//'.asc'
      call open_file(f, ju, 'w')
      do i = 1, size(arrflg)
        if (arrflg(i) == 1) then
          write(ju,'(a)') ta((/i/))//' '// &
            ta((/arr(i)/))//' '//ta((/arr2(i)/))//' '//ta((/arr3(i)/))
        end if
      end do
      close(ju)
      !call get_rel_up(f, 2)
      if (liu) then
        write(iu,fmt) 'OPEN/CLOSE '//trim(f)
      else
        write(s,fmt) 'OPEN/CLOSE '//trim(f)
      end if
    end if
    !
    return
  end subroutine mf6_mod_write_list_3
  
  subroutine mf6_mod_write(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    ! -- local
    logical, parameter :: lbin    = .true.
    logical, parameter :: lbinpos = .true.
    character(len=mxslen) :: f, pack
    integer(i4b) :: iu
! ------------------------------------------------------------------------------
    call logmsg('**************************************************************')
    if (raw%nper == 1) then 
      call logmsg('***** Writing for steady-state model '//trim(this%modelname)//'...')
    else
      call logmsg('***** Writing for transient model '//trim(this%modelname)//'...')
    end if
    call logmsg('**************************************************************')
    !
    ! set flag for reinitializing all distributed files
    call raw%mf6_raw_init_dist()
    !
    ! open the binary file
    if (lbinpos) then
      allocate(this%fbin, this%iubin)
      this%fbin = trim(this%bindir)//trim(this%modelname)//'.bin'
      call open_file(this%fbin, this%iubin, 'w', .true.)
    end if
    !
    ! set the defaults
    pckact = 1
    pckact(isto) = raw%geti('act_sto',idef=1)
    pckact(iriv) = raw%geti('act_riv',idef=1)
    pckact(iwel) = raw%geti('act_wel',idef=1)
    pckact(ighb1) = raw%geti('act_ghb',idef=0)
    pckact(ighb2) = raw%geti('act_ghb2',idef=0)
    pckact(ichd1) = raw%geti('act_chd',idef=1)
    pckact(ichd2) = raw%geti('act_chd2',idef=1)
    !
    call this%write_disu(lbin, lbinpos)
    call this%write_ic(lbin, lbinpos)
    call this%write_oc()
    call this%write_npf(lbin, lbinpos)
    call this%write_sto(lbin, lbinpos)
    call this%write_chd(lbin, lbinpos, ichd1)
    call this%write_chd(lbin, lbinpos, ichd2)
    call this%write_drn(lbin, lbinpos)
    call this%write_riv(lbin, lbinpos)
    call this%write_ghb(lbin, lbinpos, ighb1)
    call this%write_ghb(lbin, lbinpos, ighb2)
    call this%write_rch(lbin, lbinpos)
    call this%write_wel(lbin, lbinpos)
    call this%write_nam()
    !
    ! close the binary file
    if (lbinpos) then
      close(this%iubin)
    end if
    !
    f = '..\..\log\done_'//trim(this%modelname)
    call open_file(f, iu, 'w')
    close(iu)
    !
    return
  end subroutine mf6_mod_write
  
  subroutine mf6_mod_write_nam(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    ! -- local
    !
    character(len=mxslen) :: f, mn
    integer(i4b) :: iu, irun, irun0, irun1, ipck, jpck
! ------------------------------------------------------------------------------
    !
    if (ltransient) then
      irun0 = irun0tr
      irun1 = irun1tr
    else
      irun0 = irun0ss
      irun1 = irun1ss
    end if
    !
    do irun = irun0, irun1
      mn = this%modelname
      f = trim(this%rootdir)//trim(mn)//trim(pr(inam,irun))//'.nam'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      f = trim(resultslstdir)//'\'//trim(this%modelname)//'.'//trim(ctim)//trim(pr(inam,irun))//'.lst'
      call swap_slash(f)
      write(iu,'(2x,a)') 'LIST '//trim(f)
      if (raw%geti('newton',idef=0) == 1) then
        write(iu,'(2x,a)') 'NEWTON UNDER_RELAXATION'
      end if
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN PACKAGES'
      do ipck = 3, npck
        if (pckact(ipck) == 0) cycle
        if (trim(pr(ipck,irun)) == '-') cycle
        f = trim(this%rootdir)//trim(mn)//trim(pr(ipck,irun))//'.'//trim(pck(ipck))
        call swap_slash(f)
        write(iu,'(2x,a)') trim(change_case(pck(ipck),'u'))//'6 '//trim(f)
      end do
      write(iu,'(   a)') 'END PACKAGES'
      close(iu)
    end do
    !
    return
  end subroutine mf6_mod_write_nam
  
  subroutine mf6_mod_write_disu(this, lbin, lbinpos)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    ! -- local
    character(len=mxslen) :: p, pb, f
    integer(i4b) :: iu, i, j, n, m
    real(r8b) :: tn, bn, tm, bm, d
    type(tDisu), pointer :: disu
! ------------------------------------------------------------------------------
    if (pckact(idisu) == 0) return
    !
    call clear_wrk()
    !
    ! set disu
    call this%set_disu()
    disu => this%disu
    !
    p = trim(this%rootdir)//trim(this%modelname)
    if (lbin) then
      pb =  trim(this%bindir)//trim(this%modelname)
    else
      pb = p
    end if
    !
    f = trim(p)//'.disu'
    call open_file(f, iu, 'w')
    !
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(2x,a)') 'LENGTH_UNITS METERS'
    write(iu,'(2x,a)') 'NOGRB'
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN DIMENSIONS'
    write(iu,'(2x,a)') 'NODES '//ta((/disu%nodes/))
    write(iu,'(2x,a)') 'NJA '//ta((/disu%nja/))
    write(iu,'(   a)') 'END DIMENSIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN GRIDDATA'
    call this%get_array(i_top, 0, 0, 1, i1wrk, r8wrk)
    if (gnlay == 2) call this%get_array(i_bot, 1, 0, 2, i1wrk, r8wrk) !i_bot_l1
    call this%get_array(i_bot, 1, 0, 1, i1wrk2, r8wrk2) !i_bot_l1
    if (gnlay == 2) call this%get_array(i_bot, 2, 0, 2, i1wrk2, r8wrk2) !i_bot_l2
    !
    ! do a check
    allocate(i4wrk1d(disu%nodes+1)); i4wrk1d(1) = 1
    do i = 1, disu%nodes
      i4wrk1d(i+1) = i4wrk1d(i) + disu%iac(i)
    end do
    do i = 1, disu%nodes
      n = disu%ja(i4wrk1d(i))
      tn = r8wrk(n); bn = r8wrk2(n)
      !if (tn < -1000D0) then
        !call errmsg('Program error mf6_mod_write_disu: top(n) = nodata')
      !end if
      !if (bn < -1000D0) then
        !call errmsg('Program error mf6_mod_write_disu: bot(n) = nodata')
      !end if
      if (bn > tn) then
        call errmsg('Program error mf6_mod_write_disu: bot(n) > top(n)')
      end if
      d = tn - bn
      if (d <= DZERO) then
        call errmsg('Program error mf6_mod_write_disu: top(n) - bot(n) <= 0')
      end if
        
      do j = i4wrk1d(i)+1, i4wrk1d(i+1)-1
        m =  disu%ja(j)
        tm = r8wrk(m); bm = r8wrk2(m)
        if (bm > tm) then
          call errmsg('Program error mf6_mod_write_disu: bot(m) > top(m)')
        end if
        if (disu%ihc(j) == 0) then
          if (m > n) then
            if (tm > bn) then
              call errmsg('Program error mf6_mod_write_disu: top(m) > bot(n)')
            end if  
          else
            if (tn > bm) then
              call errmsg('Program error mf6_mod_write_disu: top(n) > bot(m)')
            end if
          end if
        end if
      end do
    end do
    !
    write(iu,'(2x,a)') 'TOP'
    f = trim(pb)//'.disu.top'; call this%write_array(iu, 4, f, r8wrk, lbin, lbinpos)
    write(iu,'(2x,a)') 'BOT'
    f = trim(pb)//'.disu.bot'; call this%write_array(iu, 4, f, r8wrk2, lbin, lbinpos)
    call clear_wrk()
    write(iu,'(2x,a)') 'AREA'
    write(iu,'(4x,a)') 'CONSTANT '//ta((/gcs*gcs/))
    write(iu,'(   a)') 'END GRIDDATA'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN CONNECTIONDATA'
    write(iu,'(2x,a)') 'IAC'
    f = trim(pb)//'.disu.iac'; call this%write_array(iu, 4, f, disu%iac, lbin, lbinpos)
    write(iu,'(2x,a)') 'JA'
    f = trim(pb)//'.disu.ja'; call this%write_array(iu, 4, f, disu%ja, lbin, lbinpos)
    write(iu,'(2x,a)') 'IHC'
    f = trim(pb)//'.disu.ihc'; call this%write_array(iu, 4, f, disu%ihc, lbin, lbinpos)
    write(iu,'(2x,a)') 'CL12'
    f = trim(pb)//'.disu.cl12'; call this%write_array(iu, 4, f, disu%cl12, lbin, lbinpos)
    write(iu,'(2x,a)') 'HWVA'
    f = trim(pb)//'.disu.hwva'; call this%write_array(iu, 4, f, disu%hwva, lbin, lbinpos)
    write(iu,'(   a)') 'END CONNECTIONDATA'
    close(iu)
    !
    ! clean up memory
    deallocate(disu%iac);  disu%iac  => null()
    deallocate(disu%ja);   disu%ja   => null()
    deallocate(disu%ihc);  disu%ihc  => null()
    deallocate(disu%cl12); disu%cl12 => null()
    deallocate(disu%hwva); disu%hwva => null()
    !
    return
  end subroutine mf6_mod_write_disu

  subroutine mf6_mod_write_ic(this, lbin, lbinpos)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    ! -- local
    character(len=mxslen) :: p, pb, f, d
    integer(i4b) :: iu, irun0
! ------------------------------------------------------------------------------
    if (pckact(iic) == 0) return
    !
    if (ltransient) then
      irun0 = irun0tr
    else
      irun0 = irun0ss
    end if
    !
    call clear_wrk()
    !
    p = trim(this%rootdir)//trim(this%modelname)
    if (lbin) then
      pb =  trim(this%bindir)//trim(this%modelname)
    else
      pb = p
    end if
    !
    if (.not.ltransient) then !SS
      f = trim(p)//'.ic'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN GRIDDATA'
      write(iu,'(2x,a)') 'STRT'
      call this%get_array(i_strt, 1, 0, 1, i1wrk, r8wrk) !i_strt_l1
      if (gnlay == 2) call this%get_array(i_strt, 2, 0, 2, i1wrk, r8wrk) !i_strt_l2
      f = trim(pb)//'.ic'; call this%write_array(iu, 4, f, r8wrk, lbin, lbinpos)
      write(iu,'(   a)') 'END GRIDDATA'
      close(iu)
      !
      f = trim(p)//trim(pr(iic,irun0+1))//'.ic'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN GRIDDATA'
      write(iu,'(2x,a)') 'STRT'
      !f = '.\'//trim(resultsdir)//'\'//trim(this%modelname)//'.ss.smhead.hds'
      f = trim(resultsbindir)//'\'//trim(this%modelname)//'.ss'//trim(pr(iic,irun0+1))//'.hds'
      call swap_slash(f)
      write(iu,'(2x,a)') 'OPEN/CLOSE '//trim(f)//' (BINARY)'
      write(iu,'(   a)') 'END GRIDDATA'
      close(iu)
    else !TR
      f = trim(p)//trim(pr(iic,irun0))//'.ic'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN GRIDDATA'
      write(iu,'(2x,a)') 'STRT'
      !f = '.\'//trim(resultsdir)//'\'//trim(this%modelname)//'.ss.hds'
      d = raw%getc('ss_strt_dir')
      f = trim(d)//trim(this%modelname)//'.ss.hds'
      call swap_slash(f)
      write(iu,'(2x,a)') 'OPEN/CLOSE '//trim(f)//' (BINARY)'
      write(iu,'(   a)') 'END GRIDDATA'
      close(iu)
      !
      f = trim(p)//trim(pr(iic,irun0+1))//'.ic'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN GRIDDATA'
      write(iu,'(2x,a)') 'STRT'
      !f = '.\'//trim(resultsdir)//'\'//trim(this%modelname)//'.tr.spuhead.hds'
      f = '.\'//trim(resultsbindir)//'\'//trim(this%modelname)//'.tr'//trim(pr(iic,irun0+1))//'.hds'
      call swap_slash(f)
      write(iu,'(2x,a)') 'OPEN/CLOSE '//trim(f)//' (BINARY)'
      write(iu,'(   a)') 'END GRIDDATA'
      close(iu)
    end if
    !
    return
  end subroutine mf6_mod_write_ic
  
  subroutine mf6_mod_write_oc(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    ! -- local
    character(len=mxslen) :: p, f
    integer(i4b) :: iu, iper, nperspu, irun0, irun
! ------------------------------------------------------------------------------
    if (pckact(ioc) == 0) return
    !
    if (ltransient) then
      irun0 = irun0tr
    else
      irun0 = irun0ss
    end if
    !
    p = trim(this%rootdir)//trim(this%modelname)
    !
    if (.not.ltransient) then !SS
      do irun = irun0, irun0 + 1
        f = trim(p)//trim(pr(ioc,irun))//'.oc'
        call open_file(f, iu, 'w')
        write(iu,'(   a)') 'BEGIN OPTIONS'
        f = trim(resultsbindir)//'\'//trim(this%modelname)//'.ss'//trim(pr(ioc,irun))//'.hds'
        call swap_slash(f)
        write(iu,'(2x,a)') 'HEAD FILEOUT '//trim(f)
        write(iu,'(   a)') 'END OPTIONS'
        write(iu,'(a)')
        write(iu,'(   a)') 'BEGIN PERIOD 1'
        write(iu,'(2x,a)') 'SAVE HEAD LAST'
        write(iu,'(   a)') 'END PERIOD'
        close(iu)
      end do
    else !TR
      f = trim(p)//trim(pr(ioc,irun0))//'.oc'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      f = trim(resultsbindir)//'\'//trim(this%modelname)//'.tr'//trim(pr(ioc,irun0))//'.hds'
      call swap_slash(f)
      write(iu,'(2x,a)') 'HEAD FILEOUT '//trim(f)
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      nperspu = raw%geti('nyear_spinup')*12
      write(iu,'(   a)') 'BEGIN PERIOD '//ta((/nperspu/))
      write(iu,'(2x,a)') 'SAVE HEAD LAST'
      write(iu,'(   a)') 'END PERIOD'
      close(iu)
      !
      f = trim(p)//'.oc'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      f = trim(resultsbindir)//'\'//trim(this%modelname)//'.tr.hds'
      call swap_slash(f)
      write(iu,'(2x,a)') 'HEAD FILEOUT '//trim(f)
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      do iper = 1, raw%nper
        write(iu,'(   a)') 'BEGIN PERIOD '//ta((/iper/))
        write(iu,'(2x,a)') 'SAVE HEAD LAST'
        write(iu,'(   a)') 'END PERIOD'
      end do
      close(iu)
    end if
    !
    return
  end subroutine mf6_mod_write_oc
  
  subroutine mf6_mod_write_npf(this, lbin, lbinpos)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    ! -- local
    character(len=mxslen) :: p, pb, f
    integer(i4b) :: iu, icelltype
! ------------------------------------------------------------------------------
    if (pckact(inpf) == 0) return
    !
    call clear_wrk()
    !
    p = trim(this%rootdir)//trim(this%modelname)
    if (lbin) then
      pb =  trim(this%bindir)//trim(this%modelname)
    else
      pb = p
    end if
    !
    f = trim(p)//'.npf'
    call open_file(f, iu, 'w')
    !
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN GRIDDATA'
    write(iu,'(2x,a)') 'ICELLTYPE'
    icelltype = raw%geti('icelltype',idef=0)
    write(iu,'(4x,a)') 'CONSTANT '//ta((/icelltype/))
    write(iu,'(2x,a)') 'K'
    call this%get_array(i_k, 1, 0, 1, i1wrk, r8wrk) !i_k_l1
    if (gnlay == 2) call this%get_array(i_k, 2, 0, 2, i1wrk, r8wrk) !i_k_l2
    f = trim(pb)//'.npf.k'; call this%write_array(iu, 4, f, r8wrk, lbin, lbinpos)
    call clear_wrk()
    write(iu,'(2x,a)') 'K33'
    if (gnlay == 2) then
      call this%get_array(i_k33, 1, 0, 1, i1wrk, r8wrk) !i_k33_l1
      call this%get_array(i_k33, 2, 0, 2, i1wrk, r8wrk) !i_k33_l2
      f = trim(pb)//'.npf.k33'; call this%write_array(iu, 4, f, r8wrk, lbin, lbinpos)
    else
      write(iu,'(4x,a)') 'CONSTANT 0.1'
    end if
    call clear_wrk()
    write(iu,'(   a)') 'END GRIDDATA'
    close(iu)
    !
    return
  end subroutine mf6_mod_write_npf
  
  subroutine mf6_mod_write_sto(this, lbin, lbinpos)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    ! -- local
    character(len=mxslen) :: p, pb, f
    integer(i4b) :: iu
! ------------------------------------------------------------------------------
    if (pckact(isto) == 0) return
    !
    call clear_wrk()
    !
    p = trim(this%rootdir)//trim(this%modelname)
    if (lbin) then
      pb =  trim(this%bindir)//trim(this%modelname)
    else
      pb = p
    end if
    !
    f = trim(p)//'.sto'
    call open_file(f, iu, 'w')
    !
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(2x,a)') 'STORAGECOEFFICIENT'
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN GRIDDATA'
    write(iu,'(2x,a)') 'ICONVERT'
    write(iu,'(4x,a)') 'CONSTANT 0'
    write(iu,'(2x,a)') 'SS'
    call this%get_array(i_prim_sto, 1, 0, 1, i1wrk, r8wrk)
    call this%get_array(i_prim_sto, 2, 0, 2, i1wrk, r8wrk)
    f = trim(pb)//'.sto.ps'; call this%write_array(iu, 4, f, r8wrk, lbin, lbinpos)
    call clear_wrk()
    write(iu,'(2x,a)') 'SY'
    write(iu,'(4x,a)') 'CONSTANT 0'
    write(iu,'(   a)') 'END GRIDDATA'
    !
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN PERIOD 1'
    if (ltransient) then
      write(iu,'(2x,a)') 'TRANSIENT'
    else
      write(iu,'(2x,a)') 'STEADY-STATE'
    end if
    write(iu,'(   a)') 'END PERIOD'
    close(iu)
    !
    return
  end subroutine mf6_mod_write_sto
  
 subroutine mf6_mod_write_chd(this, lbin, lbinpos, ipack)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    integer(i4b), intent(in) :: ipack
    ! -- local
    character(len=mxslen) :: p, pb, f, chd_type, packstr
    integer(i4b) :: iu, maxbound, i, pack_ib_in
    integer(i4b), dimension(gnlay) :: nbound_lay
! ------------------------------------------------------------------------------
    if (pckact(ipack) == 0) return
    if (ltransient.and.(ipack == ichd2)) return
    !
    packstr = trim(pr(ipack,irun0ss))//'.chd' !.sea .intf
    if (ipack == ichd1) then
      pack_ib_in = 1
    elseif (ipack == ichd2) then
      pack_ib_in = 3
    end if
    !
    call clear_wrk()
    !
    p = trim(this%rootdir)//trim(this%modelname)
    if (lbin) then
      pb =  trim(this%bindir)//trim(this%modelname)
    else
      pb = p
    end if
    !
    call this%get_array(i_strt, 1, 0, 1, i1wrk, r8wrk, ib_in=pack_ib_in) !i_strt_l1
    if (gnlay == 2) call this%get_array(i_strt, 2, 0, 2, i1wrk, r8wrk, ib_in=pack_ib_in) !i_strt_l2
    nbound_lay = this%count_i1a(i1wrk); maxbound = sum(nbound_lay)
    !
    if (maxbound > 0) then
      f = trim(p)//trim(packstr)
      call open_file(f, iu, 'w')
      !
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN PERIOD 1'
      if ((ipack == ichd1).and.(raw%geti('force_sea',idef=0) == 1)) then
        call logmsg('***** Forcing zero head sea-level! *****')
        do i = 1, size(r8wrk)
          r8wrk(i) = DZERO
        end do
      end if
      f = trim(pb)//trim(packstr)
      call this%write_list(iu, 2, f, i1wrk, r8wrk, lbin, lbinpos)
      write(iu,'(   a)') 'END PERIOD'
      close(iu)
    end if
    call clear_wrk()
    !
    if (maxbound == 0) then
      call logmsg('No constant-head boundaries found: '//trim(pr(ipack,irun0ss)))
      pckact(ipack) = 0
      return
    end if
    !
    return
  end subroutine mf6_mod_write_chd
  
  subroutine mf6_mod_write_drn(this, lbin, lbinpos)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    ! -- local
    character(len=mxslen), dimension(:), allocatable :: cwk
    character(len=mxslen) :: p, pb, f
    integer(i4b) :: i, n, iu, nbound, maxbound, iper, jper, nper, nperspu
    integer(i4b), dimension(gnlay) :: nbound_lay
    logical, dimension(:), allocatable :: lact
! ------------------------------------------------------------------------------
    if (pckact(idrn) == 0) return
    !
    call clear_wrk()
    !
    p = trim(this%rootdir)//trim(this%modelname)
    if (lbin) then
      pb =  trim(this%bindir)//trim(this%modelname)
    else
      pb = p
    end if
    !
    ! write all binary files and store the file strings
    nper = raw%nper
    allocate(cwk(nper), lact(nper))
    maxbound = 0
    do iper = 1, nper
      call this%get_array(i_drn_elev, 1, iper, 1, i1wrk, r8wrk, ib_in=2) !i_drn_elev_l1
      if (gnlay == 2) call this%get_array(i_drn_elev, 2, iper, 2, i1wrk, r8wrk, ib_in=2) !i_drn_elev_l1
      call this%get_array(i_drn_cond, 1, iper, 1, i1wrk, r8wrk2, ib_in=2)
      if (gnlay == 2) call this%get_array(i_drn_cond, 2, iper, 2, i1wrk, r8wrk2, ib_in=2)
      !
      ! check
      do i = 1, size(i1wrk)
        if (r8wrk(i) < -1000D0) then
          call errmsg("Invalid range for drain.")
        end if
      end do
      !
      ! filter for zero conductance
      n = 0
      do i = 1, size(i1wrk)
        if (r8wrk2(i) == DZERO) then
          if (i1wrk(i) == 1) n = n + 1
          i1wrk(i) = 0
        end if
        if (r8wrk2(i) < DZERO) then
          call errmsg("Negative drain conductance.")
        end if
      end do
      if (n > 0) then
        call logmsg('Removed '//ta((/n/))//' drains with zero conductance.')
      end if
      !
      nbound_lay = this%count_i1a(i1wrk); nbound = sum(nbound_lay)
      maxbound = max(nbound,maxbound)
      if (nbound == 0) then
        lact(iper) = .false.
        call logmsg('No drains found.')
      else
        lact(iper) = .true.
        f = trim(pb)//'.drn.sp'//ta((/iper/),'(i3.3)')
        call this%write_list(iu, 4, f, i1wrk, r8wrk, r8wrk2, lbin, lbinpos, cwk(iper))
      end if
      call clear_wrk()
    end do
    !
    if (maxbound == 0) then
      call logmsg('No drains found.')
      pckact(idrn) = 0
      return
    end if
    
    f = trim(p)//'.drn'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN DIMENSIONS'
    write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
    write(iu,'(   a)') 'END DIMENSIONS'
    write(iu,'(a)')
    do iper = 1, nper
      write(iu,'(   a)') 'BEGIN PERIOD '//ta((/iper/))
      if (lact(iper)) write(iu,'(a)') trim(cwk(iper))
      write(iu,'(   a)') 'END PERIOD'
    end do
    close(iu)
    !
    if (ltransient) then
      f = trim(p)//trim(pr(idrn,irun0tr))//'.drn'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      nperspu = raw%geti('nyear_spinup')*12
      do iper = 1, nper
        write(iu,'(   a)') 'BEGIN PERIOD '//ta((/iper/))
        jper = mod(iper, nperspu)
        if (jper == 0) jper = nperspu
        if (lact(jper)) write(iu,'(a)') trim(cwk(jper))
        write(iu,'(   a)') 'END PERIOD'
      end do
      close(iu)
    end if
    !
    deallocate(cwk, lact)
    call clear_wrk()
    !
    return
  end subroutine mf6_mod_write_drn
  
  subroutine mf6_mod_write_ghb(this, lbin, lbinpos, ipack)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    integer(i4b), intent(in) :: ipack
    ! -- local
    character(len=mxslen), dimension(:), allocatable :: cwk
    character(len=mxslen) :: p, pb, f, packstr
    integer(i4b) :: i, n, iu, nbound, maxbound, iper, jper, nper, nperspu
    integer(i4b) :: j_ghb_bhead, j_ghb_cond
    integer(i4b), dimension(gnlay) :: nbound_lay
    logical, dimension(:), allocatable :: lact
! ------------------------------------------------------------------------------
    if (pckact(ipack) == 0) return
    !
    packstr = trim(pr(ipack,irun0ss))//'.ghb' 
    if (ipack == ighb1) then
      j_ghb_bhead = i_ghb1_bhead
      j_ghb_cond  = i_ghb1_cond
    elseif(ipack == ighb2) then
      j_ghb_bhead = i_ghb2_bhead
      j_ghb_cond  = i_ghb2_cond
    end if
    !
    call clear_wrk()
    !
    p = trim(this%rootdir)//trim(this%modelname)
    if (lbin) then
      pb =  trim(this%bindir)//trim(this%modelname)
    else
      pb = p
    end if
    !
    ! write all binary files and store the file strings
    nper = raw%nper
    allocate(cwk(nper), lact(nper))
    maxbound = 0
    do iper = 1, nper
      call this%get_array(j_ghb_bhead, 1, iper, 1, i1wrk, r8wrk, ib_in=2) !j_ghb_bhead_l1
      if (gnlay == 2) call this%get_array(j_ghb_bhead, 2, iper, 2, i1wrk, r8wrk, ib_in=2) !j_ghb_bhead_l1
      call this%get_array(j_ghb_cond, 1, iper, 1, i1wrk, r8wrk2, ib_in=2)
      if (gnlay == 2) call this%get_array(j_ghb_cond, 2, iper, 2, i1wrk, r8wrk2, ib_in=2)
      !
      ! filter for zero conductance
      n = 0
      do i = 1, size(i1wrk)
        if (r8wrk2(i) == DZERO) then
          if (i1wrk(i) == 1) n = n + 1
          i1wrk(i) = 0
        end if
        if (r8wrk2(i) < DZERO) then
          call errmsg("Negative general-head boundary conductance.")
        end if
      end do
      if (n > 0) then
        call logmsg('Removed '//ta((/n/))//' general-head boundaries with zero conductance.')
      end if
      !
      nbound_lay = this%count_i1a(i1wrk); nbound = sum(nbound_lay)
      maxbound = max(nbound,maxbound)
      if (nbound == 0) then
        lact(iper) = .false.
        call logmsg('No general-head boundaries found.')
      else
        lact(iper) = .true.
        f = trim(pb)//trim(packstr)//'.sp'//ta((/iper/),'(i3.3)')
        call this%write_list(iu, 4, f, i1wrk, r8wrk, r8wrk2, lbin, lbinpos, cwk(iper))
      end if
      call clear_wrk()
    end do
    !
    if (maxbound == 0) then
      call logmsg('No general-head boundaries found: '//trim(pr(ipack,irun0ss)))
      pckact(ipack) = 0
      return
    end if
    !
    f = trim(p)//trim(packstr)
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN DIMENSIONS'
    write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
    write(iu,'(   a)') 'END DIMENSIONS'
    write(iu,'(a)')
    do iper = 1, nper
      write(iu,'(   a)') 'BEGIN PERIOD '//ta((/iper/))
      if (lact(iper)) write(iu,'(a)') trim(cwk(iper))
      write(iu,'(   a)') 'END PERIOD'
    end do
    close(iu)
    !
    if (ltransient) then
      f = trim(p)//trim(pr(ipack,irun0tr))//trim(packstr)
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      nperspu = raw%geti('nyear_spinup')*12
      do iper = 1, nper
        write(iu,'(   a)') 'BEGIN PERIOD '//ta((/iper/))
        jper = mod(iper, nperspu)
        if (jper == 0) jper = nperspu
        if (lact(jper)) write(iu,'(a)') trim(cwk(jper))
        write(iu,'(   a)') 'END PERIOD'
      end do
      close(iu)
    end if
    !
    deallocate(cwk, lact)
    call clear_wrk()
    !
    return
  end subroutine mf6_mod_write_ghb
  
  subroutine mf6_mod_write_riv(this, lbin, lbinpos)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    ! -- local
    character(len=mxslen), dimension(:), allocatable :: cwk
    character(len=mxslen) :: p, pb, f
    integer(i4b) :: iu, nbound, maxbound, i, n, iper, jper, nper, nperspu
    integer(i4b), dimension(gnlay) :: nbound_lay
    real(r8b) :: stage, rbot, cond
    logical, dimension(:), allocatable :: lact
! ------------------------------------------------------------------------------
    if (pckact(iriv) == 0) return
    !
    call clear_wrk()
    !
    p = trim(this%rootdir)//trim(this%modelname)
    if (lbin) then
      pb =  trim(this%bindir)//trim(this%modelname)
    else
      pb = p
    end if
    !
    ! write all binary files and store the file strings
    nper = raw%nper
    allocate(cwk(nper), lact(nper))
    maxbound = 0
    !
    do iper = 1, nper
      call this%get_array(i_riv_stage, 1, iper, 1, i1wrk, r8wrk,  ib_in=2, toponly_in=.true.) !i_riv_stage_l1
      call this%get_array(i_riv_stage, 1, iper, 2, i1wrk, r8wrk,  ib_in=2, toponly_in=.true.) !i_riv_stage_l1
      call this%get_array(i_riv_cond,  1, iper, 1, i1wrk, r8wrk2, ib_in=2, toponly_in=.true.) !i_riv_cond_l1
      call this%get_array(i_riv_cond,  1, iper, 2, i1wrk, r8wrk2, ib_in=2, toponly_in=.true.) !i_riv_cond_l1
      call this%get_array(i_riv_rbot,  1, iper, 1, i1wrk, r8wrk3, ib_in=2, toponly_in=.true.) !i_riv_rbot_l1
      call this%get_array(i_riv_rbot,  1, iper, 2, i1wrk, r8wrk3, ib_in=2, toponly_in=.true.) !i_riv_rbot_l1
      !
      ! checks and filter for zero conductance
      n = 0
      do i = 1, size(i1wrk)
        stage = r8wrk(i); cond = r8wrk2(i); rbot = r8wrk3(i)
        if (i1wrk(i) == 1) then
          if (stage < rbot) then
            call errmsg('Inconsistent river stage/rbot.')
          end if
          if (cond < 0) then
            call errmsg('Negative river conductance.')
          end if
        end if
        if (cond == DZERO) then
          if (i1wrk(i) == 1) n = n + 1
          i1wrk(i) = 0
        end if
      end do
      if (n > 0) then
        call logmsg('Removed '//ta((/n/))//' rivers with zero conductance.')
      end if
      !
      nbound_lay = this%count_i1a(i1wrk); nbound = sum(nbound_lay)
      maxbound = max(nbound,maxbound)
      if (nbound == 0) then
        lact(iper) = .false.
        call logmsg('No rivers found.')
      else
        lact(iper) = .true.
        f = trim(pb)//'.riv.sp'//ta((/iper/),'(i3.3)')
        call this%write_list(iu, 2, f, i1wrk, r8wrk, r8wrk2, r8wrk3, lbin, lbinpos, cwk(iper))
      end if
      call clear_wrk()
    end do 
    !
    if (maxbound == 0) then
      call logmsg('No rivers found.')
      pckact(iriv) = 0
      return
    end if
    !
    f = trim(p)//'.riv'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN DIMENSIONS'
    write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
    write(iu,'(   a)') 'END DIMENSIONS'
    write(iu,'(a)')
    do iper = 1, nper
      write(iu,'(   a)') 'BEGIN PERIOD '//ta((/iper/))
       if (lact(iper)) write(iu,'(a)') trim(cwk(iper))
      write(iu,'(   a)') 'END PERIOD'
    end do
    close(iu)
    !
    if (ltransient) then
      f = trim(p)//trim(pr(idrn,irun0tr))//'.riv'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      nperspu = raw%geti('nyear_spinup')*12
      do iper = 1, nper
        write(iu,'(   a)') 'BEGIN PERIOD '//ta((/iper/))
        !jper = mod(iper,nperspu)
        !if (jper == 0) jper = nperspu
        jper = mod(iper,12)
        if (jper == 0) jper = 1
         if (lact(jper)) write(iu,'(a)') trim(cwk(jper))
        write(iu,'(   a)') 'END PERIOD'
      end do
      close(iu)
    end if
    !
    deallocate(cwk, lact)
    call clear_wrk()
    !
    return
  end subroutine mf6_mod_write_riv
  
  subroutine mf6_mod_write_rch(this, lbin, lbinpos)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    ! -- local
    character(len=mxslen), dimension(:), allocatable :: cwk
    character(len=mxslen) :: p, pb, f
    integer(i4b) :: iu, i, n, nbound, maxbound, iper, jper, nper, nperspu
    integer(i4b), dimension(gnlay) :: nbound_lay
    logical, dimension(:), allocatable :: lact
! ------------------------------------------------------------------------------
    if (pckact(irch) == 0) return
    !
    call clear_wrk()
    !
    p = trim(this%rootdir)//trim(this%modelname)
    if (lbin) then
      pb =  trim(this%bindir)//trim(this%modelname)
    else
      pb = p
    end if
    !
    ! write all binary files and store the file strings
    nper = raw%nper
    allocate(cwk(nper), lact(nper))
    maxbound = 0
    !
    do iper = 1, nper
      call this%get_array(i_recharge, 0, iper, 1, i1wrk, r8wrk, ib_in=2, toponly_in=.true.)
      if (gnlay == 2) call this%get_array(i_recharge, 0, iper, 2, i1wrk, r8wrk, ib_in=2, toponly_in=.true.)
      !
      ! check and filter for zero recharge
      n = 0
      do i = 1, size(i1wrk)
        if (r8wrk(i) == DZERO) then
          if (i1wrk(i) == 1) n = n + 1
          i1wrk(i) = 0
        end if
        if (r8wrk(i) < DZERO) then
          call logmsg('Negative recharge found.')
        end if
      end do
      if (n > 0) then
        call logmsg('Removed '//ta((/n/))//' cells with zero recharge.')
      end if
      !
      nbound_lay = this%count_i1a(i1wrk); nbound = sum(nbound_lay)
      maxbound = max(nbound,maxbound)
      if (nbound == 0) then
        lact(iper) = .false.
        call logmsg('No recharge found.')
      else
        lact(iper) = .true.
        f = trim(pb)//'.rch.sp'//ta((/iper/),'(i3.3)')
        call this%write_list(iu, 2, f, i1wrk, r8wrk, lbin, lbinpos, cwk(iper))
      end if
      call clear_wrk()
    end do
    !
    if (maxbound == 0) then
      call logmsg('No recharge found.')
      pckact(irch) = 0
      return
    end if
    !
    f = trim(p)//'.rch'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN DIMENSIONS'
    write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
    write(iu,'(   a)') 'END DIMENSIONS'
    write(iu,'(a)')
    do iper = 1, nper
      write(iu,'(   a)') 'BEGIN PERIOD '//ta((/iper/))
      if (lact(iper)) write(iu,'(a)') trim(cwk(iper))
      write(iu,'(   a)') 'END PERIOD'
    end do
    close(iu)
    !
    if (ltransient) then
      f = trim(p)//trim(pr(irch,irun0tr))//'.rch'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      nperspu = raw%geti('nyear_spinup')*12
      do iper = 1, nper
        write(iu,'(   a)') 'BEGIN PERIOD '//ta((/iper/))
        !jper = mod(iper,nperspu)
        !if (jper == 0) jper = nperspu
        jper = mod(iper,12)
        if (jper == 0) jper = 1
        if (lact(jper)) write(iu,'(a)') trim(cwk(jper))
        write(iu,'(   a)') 'END PERIOD'
      end do
      close(iu)
    end if
    !
    deallocate(cwk, lact)
    call clear_wrk()
    !
    return
  end subroutine mf6_mod_write_rch
  
  subroutine mf6_mod_write_wel(this, lbin, lbinpos)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    ! -- local
    character(len=mxslen), dimension(:), allocatable :: cwk
    character(len=mxslen) :: p, pb, f
    integer(i4b) :: iu, i, n, nbound, maxbound, iper, jper, nper, nperspu
    integer(i4b), dimension(gnlay) :: nbound_lay
    logical, dimension(:), allocatable :: lact
! ------------------------------------------------------------------------------
    if (pckact(iwel) == 0) return
    !
    call clear_wrk()
    !
    p = trim(this%rootdir)//trim(this%modelname)
    if (lbin) then
      pb =  trim(this%bindir)//trim(this%modelname)
    else
      pb = p
    end if
    !
    ! write all binary files and store the file strings
    nper = raw%nper
    allocate(cwk(nper), lact(nper))
    maxbound = 0
    !
    do iper = 1, nper
      call this%get_array(i_wel, 1, iper, 1, i1wrk, r8wrk, ib_in=2)
      call this%get_array(i_wel, 2, iper, 2, i1wrk, r8wrk, ib_in=2)
      !
      ! filter for zero flux
      n = 0
      do i = 1, size(i1wrk)
        if (r8wrk(i) == DZERO) then
          if (i1wrk(i) == 1) n = n + 1
          i1wrk(i) = 0
        end if
      end do
      if (n > 0) then
        call logmsg('Removed '//ta((/n/))//' wells with zero flux.')
      end if
      nbound_lay = this%count_i1a(i1wrk); nbound = sum(nbound_lay)
      maxbound = max(nbound, maxbound)
      if (nbound > 0) then
        lact(iper) = .true.
        f = trim(pb)//'.wel.sp'//ta((/iper/),'(i3.3)')
        call this%write_list(iu, 2, f, i1wrk, r8wrk, lbin, lbinpos, cwk(iper))
      else
        lact(iper) = .false.
      end if
      call clear_wrk()
    end do
    !
    if (maxbound == 0) then
      call logmsg('No wells found')
      pckact(iwel) = 0
      return
    end if
    !
    f = trim(p)//'.wel'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN DIMENSIONS'
    write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
    write(iu,'(   a)') 'END DIMENSIONS'
    write(iu,'(a)')
    do iper = 1, nper
      write(iu,'(   a)') 'BEGIN PERIOD '//ta((/iper/))
      if (lact(iper)) write(iu,'(a)') trim(cwk(iper))
      write(iu,'(   a)') 'END PERIOD'
    end do
    close(iu)
    !
    if (ltransient) then
      f = trim(p)//trim(pr(irch,irun0tr))//'.wel'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      nperspu = raw%geti('nyear_spinup')*12
      do iper = 1, nper
        write(iu,'(   a)') 'BEGIN PERIOD '//ta((/iper/))
        !jper = mod(iper,nperspu)
        !if (jper == 0) jper = nperspu
        jper = mod(iper,12)
        if (jper == 0) jper = 1
        if (lact(jper)) write(iu,'(a)') trim(cwk(jper))
        write(iu,'(   a)') 'END PERIOD'
      end do
      close(iu)
    end if
    !
    deallocate(cwk, lact)
    call clear_wrk()
    !
    return
  end subroutine mf6_mod_write_wel
  
  subroutine mf6_mod_write_exchanges(this, ju)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: ju
    ! -- local
    character(len=mxslen) :: s, d, f, fexg, m1s, m2s
    integer(i4b) :: iu, ixch, iexg
    type(tExchange), pointer :: xch => null()
! ------------------------------------------------------------------------------
    if (this%nxch == 0) return
    
    d = '..\..\models\run_input\exchanges\'
    call create_dir(d)
    
    do ixch = 1, this%nxch
      xch => this%xch(ixch)
      if (.not.xch%loutput) cycle !symmetric only
      !
      m1s = this%modelname
      m2s = xch%m2modelname
      !
      fexg = trim(m1s)//'-'//trim(m2s)//'.exg'
      !
      ! .exg file
      f = trim(d)//trim(fexg); call swap_slash(f)
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      if (raw%geti('exchange_newton',idef=0) == 1) then
        write(iu,'(2x,a)') 'NEWTON'
      end if
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'NEXG '//ta((/xch%nexg/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN EXCHANGEDATA'
      f = trim(d)//trim(fexg)//'.asc'; call swap_slash(f)
      write(iu,'(2x,a)') 'OPEN/CLOSE '//trim(f)
      write(iu,'(   a)') 'END EXCHANGEDATA'
      close(iu)
      !
      ! .asc file
      f = trim(d)//trim(fexg)//'.asc'; call swap_slash(f)
      call open_file(f, iu, 'w')
      do iexg = 1, xch%nexg
        write(iu,'(a)') ta((/xch%cellidm1(iexg), xch%cellidm2(iexg)/))//' 1 '//&
                        ta((/gcs/2, gcs/2, gcs/))
      end do
      close(iu)
      !
      !GWF6-GWF6 .\exchanges\m000039-m000040.exg m000039 m000040
      write(s,'(a,3(1x,a))') 'GWF6-GWF6',  trim(d)//trim(fexg), trim(m1s), trim(m2s)
      write(ju,'(a)') trim(s)
    end do
    !
    return
  end subroutine mf6_mod_write_exchanges
  
  subroutine mf6_distmap_init(this, map_file_in, litile, ilay, iper, lreuse_in)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tDistMap) :: this
    character(len=*), intent(in) :: map_file_in
    integer(i2b), dimension(:), optional, intent(in) :: litile
    integer(i4b), intent(in), optional :: ilay
    integer(i4b), intent(in), optional :: iper
    logical, intent(in), optional :: lreuse_in
    ! -- local
    logical :: lok, lreuse
    character(len=1) :: slash
    character(len=mxslen) :: map_dir, map_file, f
    integer(i4b) :: i, j, k, jlay, jper, lntile
! ------------------------------------------------------------------------------
    !
    if ((.not.associated(ntile)).or.(.not.associated(tilebb))) then
      call errmsg('mf6_distmap_init: no tile information found.')
    end if
    !
    if (present(ilay)) then
      jlay = ilay
    else
      jlay = 0
    end if
    if (present(iper)) then
      jper = iper
    else
      jper = 0
    end if
    !
    map_file = map_file_in
    if (jper > 0) then
      i = index(map_file,'??????')
    else
      i = 0
    end if
    !
    lreuse = .false.
    if ((i > 0).and.(jper > 0)) then
      map_file(i:i+5) = trim(raw%perdate(jper))
    else
      if (present(lreuse_in)) then
        lreuse = lreuse_in
      end if
      if (lreuse.and.this%linit) then
        return
      end if
    end if
    !
    if (this%linit) then
      call this%clean()
    end if
    !
    map_dir = raw%getc('input_dist_dir')
    !
    lntile = size(litile)
    allocate(this%gntile); this%gntile = ntile
    allocate(this%ntile); this%ntile = lntile
    allocate(this%g2ltile(ntile))
    do i = 1, ntile
      this%g2ltile(i) = 0
    end do
    do i = 1, lntile
      j = litile(i)
      this%g2ltile(j) = i
    end do
    this%tilebb => tilebb
    allocate(this%maps(lntile))
    !
    do k = 1, lntile
      i = litile(k)
      f = trim(map_dir)//trim(map_file)
      call replacetoken(f, '?', i)
      call chkexist(f)
      !
      if (k == 1) then
        slash = get_slash()
        j = index(f,slash,back=.true.)
        call logmsg('Initializing (tile '//ta((/i/))//'): '// trim(f(j+1:)) //'...')
      end if
      !
      lok = this%maps(k)%init_light(f,1)
      if (.not.lok) then
        call errmsg('Could not initialize '//trim(f))
      end if
    end do
    this%linit = .true.
    !
    return
  end subroutine mf6_distmap_init

  subroutine mf6_distmap_clean(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tDistMap) :: this
    ! -- local
    integer(i4b) :: i
! ------------------------------------------------------------------------------
    do i = 1, this%ntile
      call this%maps(i)%clean()
    end do
    deallocate(this%maps)
    deallocate(this%ntile)
    deallocate(this%gntile)
    deallocate(this%g2ltile)
    this%tilebb => null()
    this%linit = .false.
    !
    return
  end subroutine mf6_distmap_clean
  
  function mf6_distmap_getval_r4(this, gic, gir, itile, nodata_in) result(r4val)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tDistMap) :: this
    integer(i4b), intent(in) :: gic
    integer(i4b), intent(in) :: gir
    integer(i4b), intent(in) :: itile
    real(r4b), optional, intent(in) :: nodata_in
    real(r4b) :: r4val
    ! -- local
    logical :: lmv
    integer(i4b) :: ic, ir, jtile
    real(r4b) :: nodata
! ------------------------------------------------------------------------------
    !
    if (present(nodata_in)) then
      nodata = nodata_in
    else
      nodata = -12345.
    end if
    !
    ic = gic - this%tilebb(itile)%ic0 + 1
    ir = gir - this%tilebb(itile)%ir0 + 1
    !
    jtile = this%g2ltile(itile)
    if (jtile == 0) then
      call errmsg("Program error: mf6_distmap_getval_r4")
    end if
    call this%maps(jtile)%get_val(ic, ir, r4val, lmv)
    !
    if (lmv) then
      r4val = nodata
    end if
    !
    return
  end function mf6_distmap_getval_r4
  !
  subroutine mf6_distidf_init(this, idf_file_in, litile, ilay, iper, lreuse_in)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tDistIDF) :: this
    character(len=*), intent(in) :: idf_file_in
    integer(i2b), dimension(:), optional, intent(in) :: litile
    integer(i4b), intent(in), optional :: ilay
    integer(i4b), intent(in), optional :: iper
    logical, intent(in), optional :: lreuse_in
    ! -- local
    logical :: lok, lreuse
    character(len=1) :: slash
    character(len=mxslen) :: idf_dir, idf_file, f
    integer(i4b) :: i, j, k, jlay, jper, lntile
! ------------------------------------------------------------------------------
    !
    if ((.not.associated(ntile)).or.(.not.associated(tilebb))) then
      call errmsg('mf6_distidf_init: no tile information found.')
    end if
    !
    if (present(ilay)) then
      jlay = ilay
    else
      jlay = 0
    end if
    if (present(iper)) then
      jper = iper
    else
      jper = 0
    end if
    !
    idf_file = idf_file_in
    if (jper > 0) then
      i = index(idf_file,'??????')
    else
      i = 0
    end if
    !
    lreuse = .false.
    if ((i > 0).and.(jper > 0)) then
      idf_file(i:i+5) = trim(raw%perdate(jper))
    else
      if (present(lreuse_in)) then
        lreuse = lreuse_in
      end if
      if (lreuse.and.this%linit) then
        return
      end if
    end if
    !
    if (this%linit) then
      call this%clean()
    end if
    !
    idf_dir = raw%getc('input_dist_dir')
    !
    lntile = size(litile)
    allocate(this%gntile); this%gntile = ntile
    allocate(this%ntile); this%ntile = lntile
    allocate(this%g2ltile(ntile))
    do i = 1, ntile
      this%g2ltile(i) = 0
    end do
    do i = 1, lntile
      j = litile(i)
      this%g2ltile(j) = i
    end do
    this%tilebb => tilebb
    allocate(this%idfs(lntile))
    !
    do k = 1, lntile
      i = litile(k)
      f = trim(idf_dir)//trim(idf_file)
      call replacetoken(f, '?', i)
      call chkexist(f)
      !
      if (k == 1) then
        slash = get_slash()
        j = index(f,slash,back=.true.)
        call logmsg('Initializing (tile '//ta((/i/))//'): '// trim(f(j+1:)) //'...')
      end if
      !
      if (.not.idfread(this%idfs(k), f, 0)) then
        call errmsg('Could not initialize '//trim(f))
      end if
    end do
    this%linit = .true.
    !
    return
  end subroutine mf6_distidf_init

  subroutine mf6_distidf_clean(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tDistIDF) :: this
    ! -- local
    integer(i4b) :: i
! ------------------------------------------------------------------------------
    call idfdeallocate(this%idfs, this%ntile)
    deallocate(this%idfs); this%idfs => null()
    deallocate(this%ntile); this%ntile => null()
    deallocate(this%gntile); this%gntile => null()
    deallocate(this%g2ltile); this%g2ltile => null()
    this%tilebb => null()
    this%linit = .false.
    !
    return
  end subroutine mf6_distidf_clean
  
  function mf6_distidf_getval_r8(this, gic, gir, itile, nodata_in) result(r8val)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tDistIDF) :: this
    integer(i4b), intent(in) :: gic
    integer(i4b), intent(in) :: gir
    integer(i4b), intent(in) :: itile
    real(r8b), optional, intent(in) :: nodata_in
    real(r8b) :: r8val
    ! -- local
    integer(i4b) :: ic, ir, jtile
    real(r8b) :: nodata
! ------------------------------------------------------------------------------
    !
    if (present(nodata_in)) then
      nodata = nodata_in
    else
      nodata = -12345.d0
    end if
    !
    ic = gic - this%tilebb(itile)%ic0 + 1
    ir = gir - this%tilebb(itile)%ir0 + 1
    !
    jtile = this%g2ltile(itile)
    if (jtile == 0) then
      call errmsg("Program error: mf6_distmap_getval_r4")
    end if
    !
    r8val = idfgetval(this%idfs(jtile),ir,ic)
    if (r8val == this%idfs(jtile)%nodata) then
      r8val = nodata
    end if
    !
    return
  end function mf6_distidf_getval_r8
end module