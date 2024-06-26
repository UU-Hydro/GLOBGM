module utilsmod
  use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, &
    i1b => int8, i2b => int16, i4b => int32, i8b => int64, &
    r4b => real32, r8b => real64

  implicit none

  integer(i4b), parameter :: i_i1 = 1
  integer(i4b), parameter :: i_i2 = 2
  integer(i4b), parameter :: i_i4 = 3
  integer(i4b), parameter :: i_r4 = 4
  integer(i4b), parameter :: i_r8 = 5
  integer(i4b), parameter :: i_n = i_r8
  !
  type tNum
    logical, dimension(i_n) :: flg
    integer(i1b) :: i1v
    integer(i2b) :: i2v
    integer(i4b) :: i4v
    real(r4b)    :: r4v
    real(r8b)    :: r8v
  contains
    procedure :: read  => tNum_read
  end type tNum
  
  integer(i4b), dimension(:,:), allocatable :: i4wk2d

#ifdef LINUX
  integer(i4b), parameter :: os = 2
#else
  integer(i4b), parameter :: os = 1
#endif

  character(len=1), parameter :: win_slash = '\'
  character(len=1), parameter :: lin_slash = '/'

  integer(i4b), parameter :: mxslen = 1024
  character(len=mxslen), dimension(100) :: sa
  character(len=1), parameter :: comment = '#'

  integer(i4b), parameter :: IZERO = 0
  real(r4b), parameter :: RZERO = 0.0
  real(r4b), parameter :: RONE = 1.0
  real(r8b), parameter :: DZERO = 0.D0
  real(r8b), parameter :: DONE  = 1.D0
  real(r8b), parameter :: DHALF  = 0.5D0

  interface fseek_stream
    module procedure :: fseek_stream_loc
  end interface fseek_stream
  private :: fseek_stream_loc
  
  interface fillgap
    module procedure :: fillgap_r4
  end interface fillgap
  private :: fillgap_r4

  interface fill_with_nearest
    module procedure :: fill_with_nearest_r4
  end interface fill_with_nearest
  private :: fill_with_nearest_r4
  
  interface readidf_block
    module procedure :: readidf_block_i4
    module procedure :: readidf_block_r4
    module procedure :: readidf_block_r8
  end interface readidf_block
  private :: readidf_block_i4, readidf_block_r4, readidf_block_r8

  interface readidf
    module procedure :: readidf_i_r
    module procedure :: readidf_r_r
    module procedure :: readidf_r_d
  end interface
  private :: readidf_i_r, readidf_r_d, readidf_r_r

  interface writebin
    module procedure :: writebin_i
  end interface writebin
  private writebin_i

!  interface writeidf
!    module procedure :: writeidf_i_r
!    module procedure :: writeidf_r_r
!    module procedure :: writeidf_r_d
!  end interface
!  private :: writeidf_i_r, writeidf_r_r, writeidf_r_d
  interface writeidf
     module procedure :: writeidf_i1_r8
     module procedure :: writeidf_i2_r8
     module procedure :: writeidf_i4_r8
     module procedure :: writeidf_i8_r8
     module procedure :: writeidf_r4_r8
     module procedure :: writeidf_r8_r8
  end interface
  private :: writeidf_i1_r8, writeidf_i2_r8, writeidf_i4_r8, writeidf_i8_r8, writeidf_r8_r8

  interface readasc
    module procedure :: readasc_r_r
    module procedure :: readasc_r_d
  end interface
  private :: readasc_r_r, readasc_r_d

  interface writeasc
    module procedure :: writeasc_i4_r4
    module procedure :: writeasc_i4_r8
    module procedure :: writeasc_r4_r8
    module procedure :: writeasc_r8_r8
  end interface
  private :: writeasc_i4_r4, writeasc_i4_r8, writeasc_r4_r8, writeasc_r8_r8

  interface readflt
    module procedure :: readflt_i1
    module procedure :: readflt_i4
    module procedure :: readflt_r4
  end interface
  private :: readflt_i1, readflt_i4, readflt_r4
  
  interface writeflt
    module procedure :: writeflt_i1_r4
    module procedure :: writeflt_i4_r4
    module procedure :: writeflt_r4_r4
    module procedure :: writeflt_i1_r8
    module procedure :: writeflt_i4_r8
    module procedure :: writeflt_r4_r8
    module procedure :: writeflt_r8_r8
  end interface
  private :: writeflt_i1_r4, writeflt_i4_r4, writeflt_r4_r4
  private :: writeflt_i1_r8, writeflt_i4_r8, writeflt_r4_r8, writeflt_r8_r8
  
  interface addboundary
    module procedure :: addboundary_i
    module procedure :: addboundary_r
    module procedure :: addboundary_d
    module procedure :: addboundary_i_list
  end interface
  private :: addboundary_i, addboundary_r, addboundary_d, addboundary_i_list

  interface calc_unique
    module procedure :: calc_unique_i
    module procedure :: calc_unique_r
  end interface calc_unique
  private :: calc_unique_i, calc_unique_r

  interface get_grid_bb
    module procedure :: get_r4grid_bb
  end interface get_grid_bb
  private :: get_r4grid_bb
  
  interface get_bb_extent
    module procedure :: get_bb_extent_r8
  end interface get_bb_extent
  private :: get_bb_extent_r8
  
  interface writetofile
    module procedure :: writetofile_i4
    module procedure :: writetofile_r4
    module procedure :: writetofile_r8
  end interface writetofile
  private :: writetofile_i4, writetofile_r4, writetofile_r8

  interface ta
    module procedure :: ta_i1
    module procedure :: ta_i4
    module procedure :: ta_i8
    module procedure :: ta_r4
    module procedure :: ta_r8
    module procedure :: ta_c
  end interface
  private :: ta_i1, ta_i4, ta_i8, ta_r4, ta_r8

  type tPol
    integer(i4b) :: id = 0
    integer(i4b) :: n = 0
    real(r4b) :: xmin =  huge(0.), ymin =  huge(0.)
    real(r4b) :: xmax = -huge(0.), ymax = -huge(0.)
    real(r8b), dimension(:,:), allocatable :: xy
  end type

  type tBb
    integer(i4b) :: ic0  = huge(0)
    integer(i4b) :: ic1  = 0
    integer(i4b) :: ir0  = huge(0)
    integer(i4b) :: ir1  = 0
    integer(i4b) :: ncol = 0
    integer(i4b) :: nrow = 0
  end type tBb
  public :: tBb

  type tI4grid
     integer(i4b), dimension(:,:), allocatable :: x
     real(r4b) :: xll, yll, cs, nodata
     type(tBb), pointer :: bb => null()
  end type tI4grid
  public :: tI4grid

  type tUnp
    integer :: n = 0

    integer :: ic0 = huge(0)
    integer :: ic1 = 0
    integer :: ir0 = huge(0)
    integer :: ir1 = 0
    !
    integer :: ncol = 0
    integer :: nrow = 0
    !
    integer :: nbnd = 0
    integer, dimension(:,:), allocatable :: bnd

    integer, dimension(:), allocatable :: out_itnn
    integer, dimension(:,:), allocatable :: out_bndnn
    double precision, dimension(:), allocatable :: out_d
    !
    ! ingoing links
    integer :: nin = 0
    integer, dimension(:), allocatable :: in_map
    integer, dimension(:,:), allocatable :: in_bndnn
    double precision, dimension(:), allocatable :: in_d
    integer, dimension(:), allocatable :: in_flag
    integer, dimension(:), allocatable :: in_srcp
    integer, dimension(:), allocatable :: in_srci

    ! global all-2-all connection for the first node
    integer, dimension(:,:), allocatable :: all_bndnn
    double precision, dimension(:), allocatable :: all_d

    integer, dimension(:,:), allocatable :: bndmap
  end type tUnp

  type tTimeSeries
    logical, pointer                    :: act    => null()
    logical, pointer                    :: read  => null()
    character(len=mxslen), pointer      :: rawhdr => null()
    character(len=mxslen), pointer      :: raw    => null()
    character(len=mxslen), pointer      :: id     => null()
    real(r8b), pointer                  :: x      => null()
    real(r8b), pointer                  :: y      => null()
    integer(i4b), pointer               :: ic     => null()
    integer(i4b), pointer               :: ir     => null()
    integer(i4b), pointer               :: im     => null()
    integer(i4b), dimension(:), pointer :: nod    => null()
    real(r8b), pointer                  :: glev   => null()
    real(r8b), dimension(:,:), pointer  :: val    => null()
    integer(i4b), pointer               :: sm_corr=> null()
    integer(i4b), pointer               :: nlay   => null()
  contains
    procedure :: clean => timeseries_clean
  end type tTimeSeries
  
  save

  contains
    
  subroutine tNum_read(this,s)
! ******************************************************************************  
    ! -- arguments
    class(tNum) :: this
    character(len=*), intent(in) :: s
    !
    integer(i1b) :: i1v
    integer(i2b) :: i2v
    integer(i4b) :: i4v
    real(r4b) :: r4v
    real(r8b) :: r8v
    !
    integer(i4b) :: ios
! ------------------------------------------------------------------------------
    !
    this%flg = .false.
    !
    read(s,*,iostat=ios) i1v
    if (ios == 0) then
      this%i1v = i1v; this%flg(i_i1) = .true.
    end if
    read(s,*,iostat=ios) i2v
    if (ios == 0) then
      this%i2v = i2v; this%flg(i_i2) = .true.
    end if
    read(s,*,iostat=ios) i4v
    if (ios == 0) then
      this%i4v = i4v; this%flg(i_i4) = .true.
    end if
    read(s,*,iostat=ios) r4v
    if (ios == 0) then
      this%r4v = r4v; this%flg(i_r4) = .true.
    end if
    read(s,*,iostat=ios) r8v
    if (ios == 0) then
      this%r8v = r8v; this%flg(i_r8) = .true.
    end if
    !
    return
  end subroutine tNum_read
  
  subroutine timeseries_clean(this)
! ******************************************************************************  
    ! -- arguments
    class(tTimeSeries) :: this
! ------------------------------------------------------------------------------
    if (associated(this%act))    deallocate(this%act)
    if (associated(this%read))   deallocate(this%read)
    if (associated(this%rawhdr)) deallocate(this%rawhdr)
    if (associated(this%raw))    deallocate(this%raw)
    if (associated(this%id))     deallocate(this%id)
    if (associated(this%x))      deallocate(this%x)
    if (associated(this%y))      deallocate(this%y)
    if (associated(this%ic))     deallocate(this%ic)
    if (associated(this%ir) )    deallocate(this%ir)
    if (associated(this%im) )    deallocate(this%im)
    if (associated(this%nod))    deallocate(this%nod)
    if (associated(this%glev))   deallocate(this%glev)
    if (associated(this%val))    deallocate(this%val)
    if (associated(this%sm_corr))deallocate(this%sm_corr)
    if (associated(this%nlay))   deallocate(this%nlay)
    !
    this%act    => null()
    this%rawhdr => null()
    this%raw    => null()
    this%id     => null()
    this%x      => null()
    this%y      => null()
    this%ic     => null()
    this%ir     => null()
    this%im     => null()
    this%nod    => null()
    this%glev   => null()
    this%val    => null()
    this%sm_corr=> null()
    this%nlay   => null()
    !
    return
  end subroutine timeseries_clean
  
  subroutine remove_tab(s)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(inout) :: s
    
    ! -- locals
    integer(i4b) :: i
! ------------------------------------------------------------------------------
    !
    do i = 1, len_trim(s)
      if (s(i:i) == achar(9)) then
        s(i:i) = ' '
      end if
    end do
    !
    return
  end subroutine remove_tab

  subroutine insert_tab(s)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(inout) :: s
    
    ! -- locals
    integer(i4b) :: i
! ------------------------------------------------------------------------------
    !
    do i = 1, len_trim(s)
      if (s(i:i) == ' ') then
        s(i:i) = achar(9)
      end if
    end do
    !
    return
  end subroutine insert_tab
  
  subroutine parse_line(s, sa, token_in)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: s
    character(len=mxslen), dimension(:), allocatable, intent(inout) :: sa
    character(len=1), intent(in), optional :: token_in
    
    ! -- locals
    character(len=1) :: token
    character(len=mxslen) :: st
    logical :: ltab
    integer(i4b) :: n, m, i, iact
! ------------------------------------------------------------------------------
    if (present(token_in)) then
      token = token_in
    else
      token = ' '
    end if
    !
    ! find tabs and overule token
    ltab = .false.
    do i = 1, len_trim(s)
      if (s(i:i) == achar(9)) then
        ltab = .true.
      end if
    end do
    if (ltab) then
      token = achar(9)
    end if
    !
    if (allocated(sa)) deallocate(sa)
    !
    do iact = 1, 2
      n = 0; st = adjustl(s)
      m = len_trim(st)
      if (st(m+1:m+1) /= token) then
        st(m+1:m+1) = token
      end if
      do while(.true.)
        m = len_trim(st)
        if (m == 0) then
          exit
        else
          n = n + 1
        end if
        i = index(st,token)
        if ((i <= 0)) then
          exit
        end if
        if (iact == 2) then
          sa(n) = st(1:i-1)
        end if
        st = adjustl(st(i:))
        if (token /= ' ') then
          if (st(1:1) == token) then
            st = adjustl(st(2:))
          end if
        end if
      end do
      if (iact == 1) then
        if (n > 0) then
          allocate(sa(n))
        end if
      end if
    end do
    !
    if (n == 0) then
      call errmsg('parse_line: empty string')
    end if
    !
    return
  end subroutine parse_line
  
  function get_args() result(args)
! ******************************************************************************
    ! -- arguments
    character(len=mxslen), dimension(:), allocatable :: args
    ! -- locals
    integer(i4b) :: na, i
! ------------------------------------------------------------------------------
    na = nargs()-1
    if (allocated(args)) deallocate(args)
    allocate(args(na))
    do i = 1, na
      call getarg(i, args(i))
    end do
    !
    return
  end function get_args
  
  subroutine linear_regression(x, y, slope, yint, corr)
! ******************************************************************************
    ! -- arguments
    real(r8b), dimension(:), intent(in) :: x
    real(r8b), dimension(:), intent(in) :: y
    real(r8b), intent(out) :: slope
    real(r8b), intent(out) :: yint
    real(r8b), intent(out) :: corr
    !
    ! -- locals
    integer(i4b) :: i, n
    real(r8b) :: r8n, sumx, sumx2, sumxy, sumy, sumy2
! ------------------------------------------------------------------------------
    sumx  = DZERO  ! sum of x
    sumx2 = DZERO  ! sum of x**2
    sumxy = DZERO  ! sum of x * y
    sumy  = DZERO  ! sum of y
    sumy2 = DZERO  ! sum of y**2
    
    n = size(x); r8n = real(n,r8b)
    if (n /= size(y)) then
      call errmsg('linear_regression: size of x and y do not match.')
    end if
    !
    do i = 1, n
      sumx  = sumx  + x(i)
      sumx2 = sumx2 + x(i)*x(i)
      sumxy = sumxy + x(i)*y(i)
      sumy  = sumy  + y(i)
      sumy2 = sumy2 + y(i)*y(i)
    end do
    
    slope = (r8n  * sumxy -  sumx * sumy)  / (r8n * sumx2 - sumx**2) ! slope
    yint  = (sumy * sumx2 -  sumx * sumxy) / (r8n * sumx2 - sumx**2) ! y-intercept
    corr  = (sumxy - sumx * sumy / r8n) / &                          ! correlation coefficient
            sqrt((sumx2 - sumx**2/r8n) * (sumy2 - sumy**2/r8n))
    !
    return
  end subroutine linear_regression
  
  recursive subroutine label_node(ia, ja, id1, i4wk1d, ireg)
! ******************************************************************************
    ! -- arguments
    integer(i4b), dimension(:), intent(in) :: ia
    integer(i4b), dimension(:), intent(in) :: ja
    integer(i4b), intent(in) :: id1
    integer(i4b), dimension(:), intent(inout) :: i4wk1d
    integer(i4b), intent(in) :: ireg
    !
    ! -- locals
    integer(i4b) :: i, id2
! ------------------------------------------------------------------------------
    !
    i4wk1d(id1) = ireg
    !
    do i = ia(id1)+1, ia(id1+1)-1
      id2 = ja(i)
      if (i4wk1d(id2) == 0) then
        call label_node(ia, ja, id2, i4wk1d, ireg)
      end if
    end do
    !
    return
  end subroutine label_node

  subroutine get_r4grid_bb(a, mv, id, bb)
! ******************************************************************************
    ! -- arguments
    real(r4b), dimension(:,:), intent(in) :: a
    real(r4b), intent(in) :: mv
    integer(i4b), dimension(:), allocatable, intent(out) :: id
    type(tBb), dimension(:), allocatable, intent(out) :: bb
    !
    ! -- locals
    integer(i4b) :: nr, nc, ir, ic, i, j, n, mxid, nid, i4v
    !
    integer(i4b), dimension(:), allocatable :: i4wk1d
! ------------------------------------------------------------------------------
    nc = size(a,1); nr = size(a,2)
    !
    mxid = 0
    do ir = 1, nc
      do ic = 1, nc
        if (a(ic,ir) /= mv) then
          i4v = int(a(ic,ir),i4b)
          mxid = max(mxid,i4v)
        end if
      end do
    end do
    !
    if (mxid <= 0) call errmsg('get_r4grid_bb: error 1')
    allocate(i4wk1d(mxid))
    do i = 1, mxid
      i4wk1d(i) = 0
    end do
    !
    do ir = 1, nc
      do ic = 1, nc
        if (a(ic,ir) /= mv) then
          i4v = int(a(ic,ir),i4b)
          i4wk1d(i4v) = 1
        end if
      end do
    end do
    !
    nid = 0
    do i = 1, mxid
      if (i4wk1d(i) == 1) then
        nid = nid + 1
        i4wk1d(i) = nid
      end if
    end do
    !
    if (nid == 0) call errmsg('get_r4grid_bb: error 2')
    allocate(id(nid), bb(nid))
    !
    do i = 1, mxid
      j = i4wk1d(i)
      if (j > 0) then
        id(j) = i
      end if
    end do
    !
    do ir = 1, nc
      do ic = 1, nc
        if (a(ic,ir) /= mv) then
          i4v = int(a(ic,ir),i4b)
          i = i4wk1d(i4v)
          bb(i)%ic0 = min(bb(i)%ic0,ic); bb(i)%ic1 = max(bb(i)%ic1,ic)
          bb(i)%ir0 = min(bb(i)%ir0,ir); bb(i)%ir1 = max(bb(i)%ir1,ir)
        end if
      end do
    end do
    do i = 1, nid
      bb(i)%ncol = bb(i)%ic1-bb(i)%ic0+1
      bb(i)%nrow = bb(i)%ir1-bb(i)%ir0+1
    end do
    !
    return
  end subroutine get_r4grid_bb
  
  subroutine get_bb_extent_r8(xll, yll, cs, ncol, nrow, xmin, xmax, ymin, ymax, &
    ic0, ic1, ir0, ir1)
! ******************************************************************************
    ! -- arguments
    real(r8b), intent(in) :: xll, yll, cs
    integer(i4b), intent(in) :: ncol, nrow
    real(r8b), intent(in) :: xmin, xmax, ymin, ymax
    integer(i4b), intent(out) :: ic0, ic1, ir0, ir1
    ! -- locals
    real(r8b) :: yul
! ------------------------------------------------------------------------------
    yul = yll + nrow*cs
    ic0 = int((xmin-xll)/cs)+1; ic1 = int((xmax-xll)/cs)
    ir0 = int((yul-ymax)/cs)+1; ir1 = int((yul-ymin)/cs)
    ic0 = max(ic0,1); ic0 = min(ic0,ncol)
    ic1 = max(ic1,1); ic1 = min(ic1,ncol)
    ir0 = max(ir0,1); ir0 = min(ir0,nrow)
    ir1 = max(ir1,1); ir1 = min(ir1,nrow)
    !
    return
  end subroutine get_bb_extent_r8
  
  subroutine grid_to_graph(a, mv, ia, ja, id, bb, label_bnd_id)
! ******************************************************************************
    ! -- arguments
    real(r4b),    dimension(:,:),              intent(in)  :: a
    real(r4b),                                 intent(in)  :: mv
    integer(i4b), dimension(:),   allocatable, intent(out) :: ia
    integer(i4b), dimension(:),   allocatable, intent(out) :: ja
    integer(i4b), dimension(:),   allocatable, intent(out) :: id
    type(tBb),    dimension(:),   allocatable, intent(out) :: bb
    logical, optional                                      :: label_bnd_id
    !
    ! -- locals
    integer(i4b), parameter :: jp = 1
    integer(i4b), parameter :: jn = 2
    integer(i4b), parameter :: js = 3
    integer(i4b), parameter :: je = 4
    integer(i4b), parameter :: jw = 5
    integer(i4b), parameter :: ns = jw
    !
    integer(i4b), dimension(2,ns) :: st
    data st/ 0,  0, &
             0, -1, &
             0,  1, &
             1,  0, &
            -1,  0/
    !
    logical :: lidbnd, lbnd
    !
    integer(i4b) :: i, j, nc, nr, ic, ir, jc, jr, mxid, i4v, nid, nbr, nja
    integer(i4b) :: ir0, ir1, ic0, ic1, iact
    !
    integer(i4b), dimension(:),   allocatable :: i4wk1d
    integer(i4b), dimension(:),   allocatable :: i4wk1d2
    integer(i4b), dimension(:,:), allocatable :: i4wk2d
! ------------------------------------------------------------------------------
    !
    ! clean
    if (allocated(ia)) deallocate(ia)
    if (allocated(ja)) deallocate(ja)
    if (allocated(id)) deallocate(id)
    if (allocated(bb)) deallocate(bb)
    if (present(label_bnd_id)) then
      lidbnd = label_bnd_id
    else
      lidbnd = .false.
    end if
    !
    nc = size(a,1); nr = size(a,2)
    !
    ! determine the maximum
    mxid = 0
    do ir = 1, nr
      do ic = 1, nc
        if (a(ic,ir) /= mv) then
          i4v = int(a(ic,ir),i4b)
          mxid = max(mxid,i4v)
        end if
      end do
    end do
    !
    ! label the ids
    allocate(i4wk1d(mxid))
    do i = 1, mxid
      i4wk1d(i) = 0
    end do
    do ir = 1, nr
      do ic = 1, nc
        if (a(ic,ir) /= mv) then
          i4v = int(a(ic,ir),i4b)
          i4wk1d(i4v) = 1
        end if
      end do
    end do
    !
    nid = 0
    do i = 1, mxid
      if (i4wk1d(i) == 1) then
        nid = nid + 1
        i4wk1d(i) = nid
       end if
    end do
    !
    allocate(id(nid))
    do i = 1, mxid
      j = i4wk1d(i)
      if (j > 0) then
        id(j) = i
      end if
    end do
    !
    ! create local matrix; set bb
    allocate(i4wk2d(nc,nr))
    allocate(bb(nid))
    !
    do ir = 1, nr
      do ic = 1, nc
        if (a(ic,ir) /= mv) then
          i4v = int(a(ic,ir),i4b)
          i = i4wk1d(i4v) ! local id
          i4wk2d(ic,ir) = i
          bb(i)%ic0 = min(bb(i)%ic0,ic); bb(i)%ic1 = max(bb(i)%ic1,ic)
          bb(i)%ir0 = min(bb(i)%ir0,ir); bb(i)%ir1 = max(bb(i)%ir1,ir)
        else
          i4wk2d(ic,ir) = 0
        end if
      end do
    end do
    do i = 1, nid
      bb(i)%ncol = bb(i)%ic1-bb(i)%ic0+1
      bb(i)%nrow = bb(i)%ir1-bb(i)%ir0+1
    end do
    !
    ! determine the number of neighbors
    allocate(i4wk1d2(nid))
    do iact = 1, 2
      nja = 0
      do i = 1, nid
        do j = 1, nid
          i4wk1d2(j) = 0
        end do
        lbnd = .false.
        do ir = max(1,bb(i)%ir0-1), min(nr,bb(i)%ir1+1)
          do ic = max(1,bb(i)%ic0-1), min(nc,bb(i)%ic1+1)
           if (i4wk2d(ic,ir) == i) then
             do j = 2, ns
               jc = ic+st(1,j); jc = max(jc,1); jc = min(jc,nc)
               jr = ir+st(2,j); jr = max(jr,1); jr = min(jr,nr)
               i4v = i4wk2d(jc,jr)
               if (i4v > 0) then
                 if (i4v /= i) then
                   i4wk1d2(i4v) = 1 ! neighbor found
                end if
               else
                 lbnd = .true.
               end if
             end do
           end if
          end do
        end do
        !
        ! label catchment
        if (lbnd.and.lidbnd) then
          id(i) = -abs(id(i))
        end if
        !
        nja = nja + 1
        if (iact == 2) then ! center id
          ja(nja) = i
        end if
        nbr = 0
        do j = 1, nid
          if (i4wk1d2(j) == 1) then
            nbr = nbr + 1; nja = nja + 1
            if (iact == 2) then ! neighbor id
              ja(nja) = j
            end if
          end if
        end do
        if (iact == 2) then
          ia(i+1) = ia(i) + nbr + 1
        end if
      end do
      if (iact == 1) then
        allocate(ia(nid+1), ja(nja))
        ia(1) = 1
      end if
    end do !act
    !
    ! clean up
    deallocate(i4wk1d, i4wk1d2, i4wk2d)
    !
    return
  end subroutine grid_to_graph
  
  recursive subroutine balance_graph(ia, ja, id1, lev1, lev, n)
! ******************************************************************************
    ! -- arguments
    integer(i4b), dimension(:), intent(in)    :: ia
    integer(i4b), dimension(:), intent(in)    :: ja
    integer(i4b),               intent(in)    :: id1
    integer(i4b),               intent(in)    :: lev1
    integer(i4b), dimension(:), intent(inout) :: lev
    integer(i4b),               intent(inout) :: n
    !
    ! -- locals
    integer(i4b) :: i, id2, lev2, tlev, dlev
! ------------------------------------------------------------------------------
    !
    lev(id1) = lev1
    !
    do i = ia(id1)+1, ia(id1+1)-1
      id2 = ja(i); lev2 = lev(id2); dlev = lev1-lev2
      if (dlev > 1) then
        tlev = lev2 + 1
        call balance_graph(ia, ja, id2, tlev, lev, n)
      end if
      if (abs(dlev) > 1) then
        n = n + 1
      end if
    end do
    !
    return
  end subroutine balance_graph
  
  function get_jd(y, m, d) result(jd)
! ******************************************************************************
    ! -- arguments
    integer(i4b), intent(in) :: y
    integer(i4b), intent(in) :: m
    integer(i4b), intent(in) :: d
    real(r8b) :: jd
    ! -- locals
    integer(i4b) :: date
    ! -- functions

! ------------------------------------------------------------------------------
    date = y*10000+m*100+d
    call cfn_datehms2mjd(date,0,0,0,jd)
    !
    return
  end function get_jd

 subroutine get_ymd_from_jd(jd, date, y, m, d)
! ******************************************************************************
    ! -- arguments
    real(r8b), intent(in) :: jd
    integer(i4b), intent(out) :: date
    integer(i4b), intent(out) :: y
    integer(i4b), intent(out) :: m
    integer(i4b), intent(out) :: d
    ! -- locals
    integer(i4b) :: idum
    character(len=100) :: s
! ------------------------------------------------------------------------------
    call cfn_mjd2datehms(jd,date,idum,idum,idum)
    write(s,*) date
    s = adjustl(s)
    read(s(1:4),*) y
    read(s(5:6),*) m
    read(s(7:8),*) d
    !
    return
  end subroutine get_ymd_from_jd

  subroutine jd_next_month(jd)
! ******************************************************************************
    ! -- arguments
    real(r8b), intent(inout) :: jd
    ! -- locals
    integer(i4b) :: date, y, m, d
! ------------------------------------------------------------------------------
    call get_ymd_from_jd(jd, date, y, m, d)

    if (m == 12) then
      y = y + 1
      m = 1
    else
      m = m + 1
    end if
    jd = get_jd(y, m, d)
    !
    return
  end subroutine jd_next_month
!  
  function get_month_days_s(date) result(nd)
! ******************************************************************************
    ! -- arguments
    character(len=mxslen), intent(in) :: date
    integer(i4b) :: nd
    ! -- locals
    real(r8b) :: jd
    ! -- functions
    integer(i4b) :: y, m, d, ios
! ------------------------------------------------------------------------------
    !
    read(date(1:4),*,iostat=ios) y
    read(date(5:6),*,iostat=ios) m
    read(date(7:8),*,iostat=ios) d
    if (ios /= 0) then
      d = 1
    end if
    jd = get_jd(y, m, d)
    nd = get_month_days(jd)
    !
    return
  end function get_month_days_s
  !
  function get_month_days(jd) result(nd)
! ******************************************************************************
    ! -- arguments
    real(r8b), intent(in) :: jd
    integer(i4b) :: nd
    ! -- locals
    integer(i4b) :: date1, date2
    real(r8b) :: jd1, jd2, djd
    ! -- functions
    integer(i4b) :: y, m, d
    real(r8b) :: cfn_jd_delta
! ------------------------------------------------------------------------------
    call get_ymd_from_jd(jd, date1, y, m, d)
    if (m == 12) then
      date2 = (y+1)*10000+1*100+1
    else
      date2 = y*10000+(m+1)*100+1
    end if
    !
    call cfn_datehms2mjd(date1,0,0,0,jd1)
    call cfn_datehms2mjd(date2,0,0,0,jd2)
    djd = cfn_jd_delta(jd2,jd1)
    !
    nd = nint(djd)
    !
    return
  end function get_month_days

  function readgen(f) result(p)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(inout) :: f
    type(tPol), dimension(:), allocatable :: p
    ! -- locals
    integer(i4b) :: iu, id, n, m, ios, iact, i
    real(r4b) :: x, y
    character(len=mxslen) :: s
! ------------------------------------------------------------------------------
    !
    call open_file(f, iu, 'r')
    !
    do iact = 1, 3
      n = 0
      do while(.true.)
        read(iu,'(a)',iostat=ios) s
        if (ios /= 0) exit
        if (s == 'END') exit
        n = n + 1
        read(s,*) id
        if (iact == 2) p(n)%id = id
        m = 0
        do while(.true.)
          read(iu,'(a)',iostat=ios) s
          if (ios /= 0) exit
          if (s == 'END') exit
          m = m + 1
          if (iact == 2) then
            p(n)%n = p(n)%n + 1
          end if
          if (iact == 3) then
            read(s,*) x, y
            p(n)%xmin = min(x, p(n)%xmin); p(n)%ymin = min(y, p(n)%ymin)
            p(n)%xmax = max(x, p(n)%xmax); p(n)%ymax = max(y, p(n)%ymax)
            p(n)%xy(1,m) = x; p(n)%xy(2,m) = y
          end if
        end do
      end do
      if (iact == 1) then
        rewind iu
        allocate(p(max(1,n)))
      end if
      if (iact == 2) then
        rewind iu
        do i = 1, n
          m = max(1,p(i)%n)
          allocate(p(i)%xy(2,m))
        end do
      end if
    end do
    !
    close(iu)
    !
    return
  end function readgen

  subroutine filtergen_i1(p, i1a, xmin, ymin, cs, i1mv)
! ******************************************************************************
    ! -- arguments
    type(tPol), dimension(:), intent(in) :: p
    integer(i1b), dimension(:,:), intent(inout) :: i1a
    real(r8b), intent(in) :: xmin, ymin, cs
    integer(i1b) :: i1mv
    ! -- locals
    integer(i4b) :: nc, nr, i, ic0, ic1, ir0, ir1, ir, ic
    real(r4b) :: xmax, ymax, x0, x1, y0, y1
    real(r8b), dimension(2) :: point
    logical :: lin
! ------------------------------------------------------------------------------
    !
    write(*,*) 'Filtering for gen-file...'
    !
    nc = size(i1a,1); nr = size(i1a,2)
    xmax = xmin + nc*cs; ymax = ymin + nr*cs
    !
    do i = 1, size(p)
      ! determine bounding boxes
      x0 = p(i)%xmin; x1 = p(i)%xmax
      y0 = p(i)%ymin; y1 = p(i)%ymax
      x0 = max(xmin, x0); y0 = max(ymin, y0)
      x1 = min(xmax, x1); y1 = min(ymax, y1)
      ic0 = (x0 - xmin)/cs; ic0 = max(ic0-1,1)
      ic1 = (x1 - xmin)/cs; ic1 = min(ic1+1,nc)
      ir0 = (ymax - y1)/cs; ir0 = max(ir0-1,1)
      ir1 = (ymax - y0)/cs; ir1 = min(ir1+1,nr)
      !
      do ir = ir0, ir1
        do ic = ic0, ic1
          point(1) = xmin + ic*cs - cs/2
          point(2) = ymax - ir*cs + cs/2
          call polygon_contains_point_2d (p(i)%n, p(i)%xy, point, lin)
          if (lin) then
            i1a(ic,ir) = i1mv
          end if
        end do
      end do
    end do
    !
    return
  end subroutine filtergen_i1

  subroutine writebin_i(f, x, nodata)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(inout) :: f
    integer(i4b), dimension(:,:), intent(in) :: x
    integer(i4b), intent(in) :: nodata
    ! -- locals
    integer(i4b) :: nc, nr, n, ic, ir, iu
! ------------------------------------------------------------------------------

    call open_file(f, iu, 'w', .true.)
    !
    ! count
    nc = size(x,1); nr = size(x,2); n = 0
    do ir = 1, nr
      do ic = 1, nc
        if (x(ic,ir) /= nodata) then
          n = n + 1
        end if
      end do
    end do
    !
    ! write
    write(iu) n
    do ir = 1, nr
      do ic = 1, nc
        if (x(ic,ir) /= 0) then
          write(iu) ic, ir, x(ic,ir)
        end if
      end do
    end do
    close(iu)
    !
    return
  end subroutine writebin_i

  function tas(s_in) result(s_out)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: s_in
    character(len=:), allocatable :: s_out
    ! -- locals
! ------------------------------------------------------------------------------
    s_out = trim(adjustl(s_in))
    !
    return
  end function tas

 function ta_i1(arr, fmt_in) result(s)
! ******************************************************************************
    ! -- arguments
    integer(i1b), dimension(:), intent(in) :: arr
    character(len=:), allocatable :: s
    character(len=*), intent(in), optional :: fmt_in
    ! -- locals
    logical :: lfmt
    integer(i4b) :: i
    character(len=mxslen) :: w, fmt
! ------------------------------------------------------------------------------
    lfmt = .false.
    if (present(fmt_in)) then
      fmt = fmt_in
      lfmt = .true.
    end if
    if (lfmt) then
      write(w,fmt) arr(1)
      s = trim(w)
    else
      write(w,*) arr(1)
      s = trim(adjustl(w))
    end if
    do i = 2, size(arr)
      if (lfmt) then
        write(w,fmt) arr(i)
        s = s//' '//trim(w)
      else
        write(w,*) arr(i)
        s = s//' '//trim(adjustl(w))
      end if
    end do
    !
    return
 end function ta_i1
 
  function ta_i4(arr, fmt_in) result(s)
! ******************************************************************************
    ! -- arguments
    integer(i4b), dimension(:), intent(in) :: arr
    character(len=:), allocatable :: s
    character(len=*), intent(in), optional :: fmt_in
    ! -- locals
    logical :: lfmt
    integer(i4b) :: i
    character(len=mxslen) :: w, fmt
! ------------------------------------------------------------------------------
    lfmt = .false.
    if (present(fmt_in)) then
      fmt = fmt_in
      lfmt = .true.
    end if
    if (lfmt) then
      write(w,fmt) arr(1)
      s = trim(w)
    else
      write(w,*) arr(1)
      s = trim(adjustl(w))
    end if
    do i = 2, size(arr)
      if (lfmt) then
        write(w,fmt) arr(i)
        s = s//' '//trim(w)
      else
        write(w,*) arr(i)
        s = s//' '//trim(adjustl(w))
      end if
    end do
    !
    return
  end function ta_i4

  function ta_i8(arr, fmt_in) result(s)
! ******************************************************************************
    ! -- arguments
    integer(i8b), dimension(:), intent(in) :: arr
    character(len=:), allocatable :: s
    character(len=*), intent(in), optional :: fmt_in
    ! -- locals
    logical :: lfmt
    integer(i4b) :: i
    character(len=mxslen) :: w, fmt
! ------------------------------------------------------------------------------
    lfmt = .false.
    if (present(fmt_in)) then
      fmt = fmt_in
      lfmt = .true.
    end if
    if (lfmt) then
      write(w,fmt) arr(1)
      s = trim(w)
    else
      write(w,*) arr(1)
      s = trim(adjustl(w))
    end if
    do i = 2, size(arr)
      if (lfmt) then
        write(w,fmt) arr(i)
        s = s//' '//trim(w)
      else
        write(w,*) arr(i)
        s = s//' '//trim(adjustl(w))
      end if
    end do
    !
    return
  end function ta_i8
  
  function ta_r4(arr, fmt_in) result(s)
! ******************************************************************************
    ! -- arguments
    real(r4b), dimension(:), intent(in) :: arr
    character(len=:), allocatable :: s
    character(len=*), intent(in), optional :: fmt_in
    ! -- locals
    logical :: lfmt
    integer(i4b) :: i
    character(len=mxslen) :: w, fmt
! ------------------------------------------------------------------------------
    lfmt = .false.
    if (present(fmt_in)) then
      fmt = fmt_in
      lfmt = .true.
    end if
    if (lfmt) then
      write(w,fmt) arr(1)
      s = trim(w)
    else
      write(w,*) arr(1)
      s = trim(adjustl(w))
    end if
    do i = 2, size(arr)
      if (lfmt) then
        write(w,fmt) arr(i)
        s = s//' '//trim(w)
      else
        write(w,*) arr(i)
        s = s//' '//trim(adjustl(w))
      end if
    end do
    !
    return
  end function ta_r4

  function ta_r8(arr, fmt_in) result(s)
! ******************************************************************************
    ! -- arguments
    real(r8b), dimension(:), intent(in) :: arr
    character(len=:), allocatable :: s
    character(len=*), intent(in), optional :: fmt_in
    ! -- locals
    logical :: lfmt
    integer(i4b) :: i
    character(len=mxslen) :: w, fmt
! ------------------------------------------------------------------------------
    lfmt = .false.
    if (present(fmt_in)) then
      fmt = fmt_in
      lfmt = .true.
    end if
    if (lfmt) then
      write(w,fmt) arr(1)
      s = trim(w)
    else
      write(w,*) arr(1)
      s = trim(adjustl(w))
    end if
    do i = 2, size(arr)
      if (lfmt) then
        write(w,fmt) arr(i)
        s = s//' '//trim(w)
      else
        write(w,*) arr(i)
        s = s//' '//trim(adjustl(w))
      end if
    end do
    !
    return
  end function ta_r8
  
  function ta_c(arr, add_quotes) result(s)
! ******************************************************************************
    ! -- arguments
    character(len=*), dimension(:), intent(in) :: arr
    logical, optional, intent(in) :: add_quotes
    character(len=:), allocatable :: s
    ! -- locals
    integer(i4b) :: i
    character(len=1) :: q
    character(len=mxslen) :: w
! ------------------------------------------------------------------------------
    if (present(add_quotes)) then
      q = '"'
    else
      q =''
    end if
    !
    w = arr(1)
    s = trim(q)//trim(adjustl(w))//trim(q)
    do i = 2, size(arr)
      w = arr(i)
      s = s//' '//trim(q)//trim(adjustl(w))//trim(q)
    end do
    !
    return
  end function ta_c

  function bb_overlap(bb1, bb2) result(loverlap)
! ******************************************************************************
    ! -- arguments
    type(tBB), pointer, intent(in) :: bb1
    type(tBB), pointer, intent(in) :: bb2
    logical :: loverlap
    ! -- locals
! ------------------------------------------------------------------------------
    loverlap = .true.
    if ((bb1%ic0 >= bb2%ic1).or.(bb2%ic0 >= bb1%ic1)) then
      loverlap = .false.
    end if
    if ((bb1%ir0 >= bb2%ir1).or.(bb2%ir0 >= bb1%ir1)) then
      loverlap = .false.
    end if
    !
    return
  end function bb_overlap
  
  subroutine swap_slash(s)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(inout) :: s
    ! -- locals
    character(len=1) :: src_slash, tgt_slash
    integer(i4b) :: i
! ------------------------------------------------------------------------------
    if (os == 1) then ! windows
      src_slash = lin_slash
      tgt_slash = win_slash
    else ! linux
      src_slash = win_slash
      tgt_slash = lin_slash
    end if
    !
    do i = 1, len_trim(s)
      if (s(i:i) == src_slash) then
        s(i:i) = tgt_slash
      end if
    end do
    !
    return
  end subroutine swap_slash
  !
  function get_slash() result(slash)
! ******************************************************************************
    ! -- arguments
    character(len=1) :: slash
    ! -- locals
! ------------------------------------------------------------------------------
    if (os == 1) then ! windows
      slash = win_slash
    else ! linux
      slash = lin_slash
    end if
    !
    return
  end function get_slash

  function count_dir_files(d) result(n)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(inout) :: d
    integer(i4b) :: n
    ! -- locals
    character(len=mxslen) :: tf, s
    integer(i4b) :: iu, ios
! ------------------------------------------------------------------------------
    tf = 'tmp.txt'
    call swap_slash(d)
    if (os == 1) then ! windows
      call system('dir /b '//trim(d)//' > '//trim(tf))
    else
      call system('ls -1 '//trim(d)//' > '//trim(tf))
    end if
    !
    n = 0
    call open_file(tf, iu, 'r')
    do while(.true.)
      read(unit=iu,fmt='(a)',iostat=ios) s
      if (ios /= 0) exit
      n = n + 1
    end do
    close(iu,status='delete')
    !
    return
  end function count_dir_files
  
  subroutine get_rel_up(f, n)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(inout) :: f
    integer(i4b), intent(in) :: n
    ! -- locals
    integer(i4b) :: i, j
    character(len=1) :: slash
! ------------------------------------------------------------------------------
    call swap_slash(f)
    slash = get_slash()
    do i = 1, n
      j = index(f, slash)
      if (j < 0) then
        call errmsg('Could not determine relative path')
      end if
      f = f(j+1:)
    end do
    f = '.'//slash//trim(f)
    !
    return
  end subroutine get_rel_up

  subroutine create_dir(d, lverb_in)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(inout) :: d
    logical, intent(in), optional :: lverb_in
    ! -- locals
    logical :: ldirexist, lverb
    integer(i4b) :: ios
! ------------------------------------------------------------------------------
    if (present(lverb_in)) then
      lverb = lverb_in
    else
      lverb = .false.
    end if
    !
    call swap_slash(d)
    inquire(directory=d, exist=ldirexist, iostat=ios)
    if (ios.ne.0) ldirexist=.false.
    if (ldirexist) then
      if (.not.lverb) then
        call logmsg('Directory '//trim(d)//' already already exists.')
      end if
      return
    end if
    !
    if (.not.lverb) then
      call logmsg('Creating directory '//trim(d)//'.')
    end if
    if (os == 1) then !windows
      call system('mkdir '//trim(d))
    end if
    if (os == 2) then !linux
      call system('mkdir -p '//trim(d))
    end if
    !
    return
  end subroutine create_dir
  !
  subroutine change_work_dir(d, lverb_in)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(inout) :: d
    logical, intent(in), optional :: lverb_in
    ! -- locals
    character(len=mxslen) :: cd
    logical :: lverb, ldirexist
    integer(i4b) :: ios
! ------------------------------------------------------------------------------
    if (present(lverb_in)) then
      lverb = lverb_in
    else
      lverb = .false.
    end if
    !
    call swap_slash(d)
    inquire(directory=d, exist=ldirexist, iostat=ios)
    if (ios.ne.0) ldirexist=.false.
    if (.not.ldirexist) then
      call errmsg('Directory '//trim(d)//' does not exist.')
    end if
    !
    if (.not.lverb) then
      call get_work_dir(cd)
      call logmsg('Changing working directory '//trim(cd)//' -> '//trim(d)//'.')
    end if
    call chdir(d)
    !
    return
  end subroutine change_work_dir
  !
  subroutine get_work_dir(d)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(inout) :: d
! ------------------------------------------------------------------------------
    call getcwd(d)
    !
    return
  end subroutine get_work_dir
  !
  subroutine get_abs_path(f)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(inout) :: f
    ! -- locals
    character(len=1) :: slash
    character(len=mxslen) :: d, s
    integer(i4b) :: i, j, n
! ------------------------------------------------------------------------------
    !
    call swap_slash(f)
    if (f(1:1) /= '.') return
    !
    call get_work_dir(d)
    !
    s = f
    !
    slash = get_slash()
    !
    n = 0
    if (s(1:3) == '..'//slash) then
      n = 1; s = s(4:)
      do while (s(1:3) == '..'//slash)
        n = n + 1; s = s(4:)
      end do
    end if
    !
    if (n == 0) then 
      f = trim(d)//trim(s(3:))
    else
      do i = 1, n
        j = index(d, slash, back=.true.)
        if (j <= 0) then
          call errmsg('get_abs_path: '//trim(f)//'.')
        end if
        d = d(1:j-1)
      end do
      f = trim(d)//slash//trim(s)
    end if
    !
    return
  end subroutine
  !
  subroutine open_file(f, iu, act_in, lbin_in)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(inout) :: f
    integer(i4b), intent(inout) :: iu
    character(len=1), optional :: act_in
    logical, intent(in), optional :: lbin_in
    ! -- locals
    character(len=1) :: act
    logical :: lbin, lex, lop
! ------------------------------------------------------------------------------
    if (present(act_in)) then
      act = change_case(act_in, 'l')
    else
      act = 'r'
    end if
    if (present(lbin_in)) then
      lbin = lbin_in
    else
      lbin= .false.
    end if
    !
    call swap_slash(f)
    !
    inquire(file=f, exist=lex)
    if (lex) then
      inquire(file=f, opened=lop, number=iu)
      if (lop) then
        call logmsg('Warning: file '//trim(f)//' is already opened.')
        return
      end if
    end if
    !
    if (act == 'r') then
      inquire(file=f, exist=lex)
      if (.not.lex) then
        call errmsg('File '//trim(f)//' does not exist.')
      end if
    end if

    iu = getlun()
    if ((act == 'r') .and.(.not.lbin)) then
      call logmsg('Reading ascii file '//trim(f)//'...')
      open(unit=iu, file=f, form='formatted', access='sequential', action='read', status='old',share='denynone')
    else if ((act == 'w') .and.(.not.lbin)) then
      call logmsg('Writing ascii file '//trim(f)//'...')
      open(unit=iu, file=f, form='formatted', access='sequential', action='write', status='replace')
    else if ((act == 'r') .and.(lbin)) then
      call logmsg('Reading binary file '//trim(f)//'...')
      open(unit=iu, file=f, form='unformatted', access='stream', action='readwrite', status='old',share='denynone')
    else if ((act == 'w') .and.(lbin)) then
      call logmsg('Writing binary file '//trim(f)//'...')
      open(unit=iu, file=f, form='unformatted', access='stream', action='write', status='replace')
    else
      call errmsg('Subroutine open_file called with invalid option')
    end if
    !
    return
  end subroutine open_file

  subroutine checkdim(nc1, nr1, nc2, nr2)
! ******************************************************************************
    ! -- arguments
    integer(i4b), intent(in) :: nc1, nr1, nc2, nr2
! ------------------------------------------------------------------------------
    if (nc1 /= nc2) then
      call errmsg('Inconsistent number of columns.')
    end if
    if (nr1 /= nr2) then
      call errmsg('Inconsistent number of rows.')
    end if
    !
    return
  end subroutine checkdim

  function readline(iu, so) result(ios)
! ******************************************************************************
    ! -- arguments
    integer(i4b), intent(in) :: iu
    character(len=*), intent(out), optional :: so
    integer(I4B) :: ios
    ! -- locals
    character(len=mxslen) :: s
    integer(i4b) :: i
! ------------------------------------------------------------------------------
    do while(.true.)
      read(unit=iu, iostat=ios, fmt='(a)') s
      if (ios /= 0) exit
      so = trim(adjustl(s))
      if ((so(1:1) /= comment) .and. (len_trim(so) > 0)) then
        i = index(so, comment, back=.true.)
        if (i > 0) then
          so = so(1:i-1)
        end if
        exit
      end if
    end do
    !
    return
  end function readline

  function createtoken() result(t)
! ******************************************************************************
    ! -- arguments
    character(len=1) :: t
! ------------------------------------------------------------------------------
    t = '?'
    !
    return
  end function createtoken

  function counttoken(s, t) result(n)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: s
    character(len=1), optional, intent(in) :: t
    integer(i4b) :: n
    ! -- locals
    character(len=1) :: tloc
    integer(i4b) :: i
! ------------------------------------------------------------------------------
    tloc = createtoken()
    if (present(t)) then
      tloc = t
    end if
    !
    n = 0
    do i = 1, len(s)
      if (s(i:i) == tloc) then
        n = n + 1
      endif
    enddo
    !
    return
  end function counttoken

  subroutine replacetoken(s, t, i)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(inout) :: s
    character(len=1), intent(in) :: t
    integer(i4b) :: i
    ! -- locals
    integer(i4b) :: i1, i2, n
    character(len=mxslen) :: ns, is, fmt
! ------------------------------------------------------------------------------
    i1 = index(s, t)
    i2 = index(s, t, back=.true.)
    n = i2 - i1 + 1
    write(ns,*) n
    fmt = '(i'//trim(ns)//'.'//trim(ns)//')'
    write(is,fmt) i
    is = adjustl(is)
    s(i1:i2) = is(1:n)
    !
    return
  end subroutine

  subroutine getminmax(key, sep, token, imin, imax)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: key
    character(len=1), intent(in) :: sep
    character(len=*), intent(in) :: token
    integer(I4B), intent(out) :: imin
    integer(I4B), intent(out) :: imax
    ! -- locals
    character(len=mxslen) :: s
    integer(I4B) :: i, j, n, ios1, ios2, ival1, ival2
    character(len=mxslen), dimension(:), allocatable :: words
! ------------------------------------------------------------------------------
    words = getwords(key, sep)
    n = size(words)
    if (n == 0) return
    !
    imin = 0
    imax = 0
    do i = 1, n
      s = words(i)
      if (s(1:1) == token) then
        j = index(s,':')
        if (j == 0) then
          read(s(2:),*,iostat=ios1) ival1
          if (ios1 == 0) then
            imin = ival1
            imax = imin
          else
            !write(*,*) '@@@ debug'
          end if
        else
          read(s(2:j-1),*,iostat=ios1) ival1
          read(s(j+1:),*,iostat=ios2) ival2
          if ((ios1 == 0).and.(ios2 == 0)) then
            imin = ival1
            imax = ival2
          else
            call errmsg("Could not read "//trim(s))
          end if
        end if
      end if
    end do
    !
    return
  end subroutine getminmax

  function getwords(s_in, token) result(words)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in), optional :: s_in
    character(len=mxslen), dimension(:), allocatable :: words
    character(len=1), optional, intent(in) :: token
    ! -- locals
    integer(i4b) :: i, j, n, i0, i1
    character(len=1) :: tokenLocal
    character(len=1), parameter :: quote = '"'
    character(len=mxslen) :: s, s1, s2
    integer(I4B), parameter :: maxwords = 100
    integer(I4B), dimension(maxwords) :: ind
    logical :: lquote
! ------------------------------------------------------------------------------
    !
    s = s_in
    !
    tokenLocal = ' '
    if (present(token)) then
      tokenLocal = token
    endif
    !
    ! first check for quotes
    lquote = .false.
    i0 = index(s_in,quote)
    if (i0 > 0) then
      i1 = index(s,quote,back=.true.)
      if ((i1 > 0).and.(i0 /= i1)) lquote = .true.
    end if
    if (lquote) then
      do i = i0, i1
        if (s(i:i) == tokenLocal) s(i:i) = quote
      end do
    end if
    !
    i = index(s_in,comment)
    if (i > 0) then
      !s1 = trim(s_in(1:i))
      s1 = trim(s(1:i))
    else
      !s1 = trim(s_in)
      s1 = trim(s)
    endif
    s1 = adjustl(s1)
    if (len_trim(s1) == n) then
       call errmsg('getwords.')
    end if
    if (len_trim(s1) == 1) then
      n = 1
      allocate(words(1))
      words(1) = s1
      return
    endif
    !
    ! count
    ind(1) = 1
    n = 1
    do i = 2, len_trim(s1)
      if ((s1(i:i) == tokenLocal).and. (s1(i-1:i-1) /= tokenLocal)) then
        n = n + 1
        ind(n) = i
      end if
    end do
    ind(n+1) = len_trim(s1)+1
    if (n > 0) then
      if (allocated(words)) deallocate(words)
      allocate(words(n))
    endif
    do i = 1, n
      read(s1(ind(i):ind(i+1)-1),'(a)') s2
      j = index(trim(s2), tokenLocal, back=.true.)+1
!      words(i) = s2(j:)
      s2 = s2(j:)
      if (lquote) then
        do j = 1, len_trim(s2)
          if (s2(j:j) == quote) s2(j:j) = ' '
          s2 = adjustl(s2)
        end do
      end if
      words(i) = s2
    end do
    !
    return
  end function getwords

  subroutine writetofile_i4(lun, arr, pre, post)
! ******************************************************************************
    ! -- arguments
    integer(i4b), intent(in) :: lun
    integer(i4b), dimension(:), intent(in) :: arr
    character(len=*), intent(in), optional :: pre
    character(len=*), intent(in), optional :: post
    ! -- locals
    integer(i4b) :: i, n
    character(len=mxslen) :: fmt
! ------------------------------------------------------------------------------
    n = size(arr)
    do i = 1, n
      write(sa(i),*) arr(i)
    end do
    !
    if ((.not.present(pre)).and.(.not.present(post))) then
      write(fmt,'(a,i,a)') '(', n-1, '(a,1x),a)'
      write(lun,fmt) (tas(sa(i)),i=1,n)
    end if
    if ((present(pre)).and.(.not.present(post))) then
      write(fmt,'(a,i,a)') '(a,1x', n-1, '(a,1x),a)'
      write(lun,fmt) tas(pre), (tas(sa(i)),i=1,n)
    end if
    if ((.not.present(pre)).and.(present(post))) then
      write(fmt,'(a,i,a)') '(', n-1, '(a,1x),a,1x,a)'
      write(lun,fmt) (tas(sa(i)),i=1,n), tas(post)
    end if
    if ((present(pre)).and.(present(post))) then
      write(fmt,'(a,i,a)') '(a,1x,', n-1, '(a,1x),a,1x,a)'
      write(lun,fmt) tas(pre), (tas(sa(i)),i=1,n), &
        trim(adjustl(post))
    end if
    flush(lun)
    !
    return
  end subroutine writetofile_i4

   subroutine writetofile_r4(lun, arr, pre, post)
! ******************************************************************************
    ! -- arguments
    integer(i4b), intent(in) :: lun
    real(r4b), dimension(:), intent(in) :: arr
    character(len=*), intent(in), optional :: pre
    character(len=*), intent(in), optional :: post
    ! -- locals
    integer(i4b) :: i, n
    character(len=mxslen) :: fmt
! ------------------------------------------------------------------------------
    n = size(arr)
    do i = 1, n
      write(sa(i),*) arr(i)
    end do
    !
    if ((.not.present(pre)).and.(.not.present(post))) then
      write(fmt,'(a,i,a)') '(', n-1, '(a,1x),a)'
      write(lun,fmt) (trim(adjustl(sa(i))),i=1,n)
    end if
    if ((present(pre)).and.(.not.present(post))) then
      write(fmt,'(a,i,a)') '(a,1x', n-1, '(a,1x),a)'
      write(lun,fmt) trim(adjustl(pre)), (trim(adjustl(sa(i))),i=1,n)
    end if
    if ((.not.present(pre)).and.(present(post))) then
      write(fmt,'(a,i,a)') '(', n-1, '(a,1x),a,1x,a)'
      write(lun,fmt) (trim(adjustl(sa(i))),i=1,n),  trim(adjustl(post))
    end if
    if ((present(pre)).and.(present(post))) then
      write(fmt,'(a,i,a)') '(a,1x,', n-1, '(a,1x),a,1x,a)'
      write(lun,fmt) trim(adjustl(pre)), (trim(adjustl(sa(i))),i=1,n), &
        trim(adjustl(post))
    end if
    flush(lun)
    !
    return
  end subroutine writetofile_r4

   subroutine writetofile_r8(lun, arr, pre, post)
! ******************************************************************************
    ! -- arguments
    integer(i4b), intent(in) :: lun
    real(r8b), dimension(:), intent(in) :: arr
    character(len=*), intent(in), optional :: pre
    character(len=*), intent(in), optional :: post
    ! -- locals
    integer(i4b) :: i, n
    character(len=mxslen) :: fmt
! ------------------------------------------------------------------------------
    n = size(arr)
    do i = 1, n
      write(sa(i),*) arr(i)
    end do
    !
    if ((.not.present(pre)).and.(.not.present(post))) then
      write(fmt,'(a,i,a)') '(', n-1, '(a,1x),a)'
      write(lun,fmt) (trim(adjustl(sa(i))),i=1,n)
    end if
    if ((present(pre)).and.(.not.present(post))) then
      write(fmt,'(a,i,a)') '(a,1x', n-1, '(a,1x),a)'
      write(lun,fmt) trim(adjustl(pre)), (trim(adjustl(sa(i))),i=1,n)
    end if
    if ((.not.present(pre)).and.(present(post))) then
      write(fmt,'(a,i,a)') '(', n-1, '(a,1x),a,1x,a)'
      write(lun,fmt) (trim(adjustl(sa(i))),i=1,n),  trim(adjustl(post))
    end if
    if ((present(pre)).and.(present(post))) then
      write(fmt,'(a,i,a)') '(a,1x,', n-1, '(a,1x),a,1x,a)'
      write(lun,fmt) trim(adjustl(pre)), (trim(adjustl(sa(i))),i=1,n), &
        trim(adjustl(post))
    end if
    flush(lun)
    !
    return
  end subroutine writetofile_r8

  subroutine setuniweight(ncol, nrow, rwrk, nodata, iwrk)
! ******************************************************************************
    ! -- arguments
    integer, intent(in) :: ncol, nrow
    real, dimension(ncol,nrow), intent(in) :: rwrk
    real, intent(in) :: nodata
    integer, dimension(:,:), allocatable, intent(out) :: iwrk
    ! -- locals
    integer :: irow, icol
! ------------------------------------------------------------------------------
    if (.not.allocated(iwrk)) then
      allocate(iwrk(ncol,nrow))
    end if

    do irow = 1, nrow
      do icol = 1, ncol
        if (rwrk(icol,irow) /= nodata) then
          iwrk(icol,irow) = 1
        else
          iwrk(icol,irow) = 0
        end if
      end do
    end do
    !
    return
  end subroutine setuniweight

  function getlun() result(lun)
! ******************************************************************************
    ! -- arguments
    integer :: lun
    ! -- locals
    logical :: lex
! ------------------------------------------------------------------------------
    do lun=20, 5000
      inquire(unit=lun,opened=lex)
      if(.not.lex)exit
    end do
    !
    return
  end function getlun

  function fileexist(fname) result(lex)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: fname
    logical :: lex
! ------------------------------------------------------------------------------
    inquire(file=fname,exist=lex)
    !
    return
  end function fileexist
  
  subroutine chkexist(fname)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: fname
    ! -- locals
    logical :: lex
! ------------------------------------------------------------------------------
    inquire(file=fname,exist=lex)
    if (.not.lex) then
      call errmsg('cannot find '//trim(fname))
    end if
    !
    return
  end subroutine chkexist

  function getdigits(n) result(ndig)
! ******************************************************************************
    ! -- arguments
    integer(kind=8), intent(in) :: n
    integer :: ndig
! ------------------------------------------------------------------------------
    ndig = 1
    if (abs(n) > 10) ndig = 2
    if (abs(n) > 100) ndig = 3
    if (abs(n) > 1000) ndig = 4
    if (n < 0) ndig = ndig + 1
    !
    return
  end function

  subroutine writeascheader(lun, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
    ! -- arguments
    integer, intent(in) :: lun, ncol, nrow
    double precision, intent(in) :: xll, yll, cs, nodata
    ! -- locals
    character(len=mxslen) :: s
! ------------------------------------------------------------------------------
    !
    write(s,*) ncol; write(lun,'(a)') 'ncols '//trim(adjustl(s))
    write(s,*) nrow; write(lun,'(a)') 'nrows '//trim(adjustl(s))
    write(s,*) xll; write(lun,'(a)') 'xllcorner '//trim(adjustl(s))
    write(s,*) yll; write(lun,'(a)') 'yllcorner '//trim(adjustl(s))
    write(s,*) cs; write(lun,'(a)') 'cellsize '//trim(adjustl(s))
    write(s,*) nodata; write(lun,'(a)') 'nodata_value '//trim(adjustl(s))
    !
    return
  end subroutine writeascheader

  subroutine writeasc_i4_r4(f, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: f
    integer(i4b), intent(in) :: ncol, nrow
    integer(i4b), dimension(ncol,nrow), intent(in) :: x
    real(r4b), intent(in) :: xll, yll, cs, nodata
    ! -- locals
    integer(i4b) :: lun, icol, irow
! ------------------------------------------------------------------------------

    write(*,'(1x,a,1x,2a)') 'Writing',trim(f),'...'

    lun = getlun(); open(unit=lun,file=f,status='replace')
    call writeascheader(lun, ncol, nrow, real(xll,r8b), real(yll,r8b), real(cs,r8b),        &
      dble(nodata))
    write(lun,*)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(lun)
    !
    return
  end subroutine writeasc_i4_r4

  subroutine writeasc_r4_r8(f, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: f
    integer(i4b), intent(in) :: ncol, nrow
    real(r4b), dimension(ncol,nrow), intent(in) :: x
    real(r8b), intent(in) :: xll, yll, cs, nodata
    ! -- locals
    integer(i4b) :: lun, icol, irow
! ------------------------------------------------------------------------------

    write(*,'(1x,a,1x,2a)') 'Writing',trim(f),'...'

    lun = getlun(); open(unit=lun,file=f,status='replace')
    call writeascheader(lun, ncol, nrow, xll, yll, cs, nodata)
    write(lun,*)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(lun)
    !
    return
  end subroutine writeasc_r4_r8

  subroutine readflt_header(iu, ncol, nrow, xll, yll, cs, nodata, &
    nbits, pixeltype)
! ******************************************************************************
    ! -- arguments
    integer(i4b), intent(in) :: iu
    integer(i4b), intent(out) :: ncol, nrow
    character(len=*), intent(inout) :: nodata
    real(r8b), intent(out) :: xll, yll, cs
    integer(i4b), intent(out) :: nbits
    character(len=*), intent(inout) :: pixeltype
    ! -- locals
    !
    integer(i4b), parameter :: i_nrows         =  1 
    integer(i4b), parameter :: i_ncols         =  2
    integer(i4b), parameter :: i_nbits         =  3
    integer(i4b), parameter :: i_pixeltype     =  4
    integer(i4b), parameter :: i_ulxmap        =  5
    integer(i4b), parameter :: i_ulymap        =  6
    integer(i4b), parameter :: i_xllcorner     =  7
    integer(i4b), parameter :: i_yllcorner     =  8
    integer(i4b), parameter :: i_xdim          =  9
    integer(i4b), parameter :: i_ydim          = 10
    integer(i4b), parameter :: i_cellsize      = 11
    integer(i4b), parameter :: i_nodata        = 12
    integer(i4b), parameter :: i_nodata_value  = 13
    integer(i4b), parameter :: nkey = i_nodata_value
    
    logical :: lx, ly
    character(len=1) :: cdum
    character(len=mxslen) :: s, k
    integer(i4b) :: ios, nrewind, nfound
    integer(i4b), dimension(nkey) :: flag
    real(r8b) :: xcul, ycul
! ------------------------------------------------------------------------------
    flag = 0
    nrewind = 0
    lx = .false.; ly = .false.
    do while(.true.)
      read(unit=iu,iostat=ios,fmt='(a)') s
      if (ios == 0) then
        read(s,*) k
        k = change_case(k, 'l')
        select case(k)
        case('ncols')
          read(s,*) cdum, ncol; flag(i_ncols) = 1
        case('nrows')
          read(s,*) cdum, nrow; flag(i_nrows) = 1
        case('xllcorner')
          read(s,*) cdum, xll; flag(i_xllcorner) = 1; flag(i_ulxmap) = 1
        case('ulxmap') ! center
          read(s,*) cdum, xcul; flag(i_xllcorner) = 1; flag(i_ulxmap) = 1
          lx = .true.
        case('yllcorner')
          read(s,*) cdum, yll; flag(i_yllcorner) = 1; flag(i_ulymap) = 1
        case('ulymap') ! center
          read(s,*) cdum, ycul; flag(i_yllcorner) = 1; flag(i_ulymap) = 1
          ly = .true.
        case('nodata','nodata_value')
          read(s,*) cdum, nodata; flag(i_nodata) = 1; flag(i_nodata_value) = 1
        case('cellsize','xdim', 'ydim')
          read(s,*) cdum, cs; flag(i_cellsize) = 1; flag(i_xdim) = 1; flag(i_ydim) = 1
        case('nbits')
          read(s,*) cdum, nbits; flag(i_nbits) = 1
        case('pixeltype')
          read(s,*) cdum, pixeltype; flag(i_pixeltype) = 1
          pixeltype = change_case(pixeltype, 'l')
        end select
      end if
      !
      nfound = sum(flag)
      if (nfound == nkey) exit
      if (ios /= 0) then
        if (nrewind < nfound) then
          nrewind = nrewind + 1; rewind(iu)
        else
          if ((flag(i_nodata) == 0).or.(flag(i_nodata_value) == 0)) then
            flag(i_nodata) = 1; flag(i_nodata_value) = 1
            nodata = '0'
            nrewind = nrewind + 1; rewind(iu)
          else
            call errmsg('Could not parse Ehdr header file.')
          end if
        end if
      end if
    end do
    close(iu)
    !
    if (lx) then
      xll = xcul - cs/2.d0
    end if
    if (ly) then
      yll = ycul - cs*nrow + cs/2.d0
    end if
    !
    return
    end subroutine readflt_header
  
  subroutine readflt_i1(fp, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: fp
    integer(i4b), intent(out) :: ncol, nrow
    integer(i1b), dimension(:,:), allocatable, intent(inout) :: x
    integer(i1b), intent(out) :: nodata
    real(r8b), intent(out) :: xll, yll, cs
    ! -- locals
    character(len=1) :: cdum
    character(len=mxslen) :: f, nodata_s, pixeltype
    integer(i4b) :: iu, nbits, icol, irow
! ------------------------------------------------------------------------------
    f = trim(fp)//'.hdr'
    call open_file(f, iu)
    call readflt_header(iu, ncol, nrow, xll, yll, cs, nodata_s, nbits, pixeltype)
    if ((nbits /= 8).or.(pixeltype /= 'signedint')) then
      call errmsg('Could not read '//trim(f))
    end if
    read(nodata_s,*) nodata
    !
    if (allocated(x)) deallocate(x)
    allocate(x(ncol,nrow))
    f = trim(fp)//'.flt'
    call open_file(f, iu, 'r', .true.)
    read(iu)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(iu)
    !
    return
  end subroutine readflt_i1
  
  subroutine readflt_i4(fp, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: fp
    integer(i4b), intent(out) :: ncol, nrow
    integer(i4b), dimension(:,:), allocatable, intent(inout) :: x
    integer(i4b), intent(out) :: nodata
    real(r8b), intent(out) :: xll, yll, cs
    ! -- locals
    character(len=1) :: cdum
    character(len=mxslen) :: f, nodata_s, pixeltype
    integer(i4b) :: iu, nbits, icol, irow
! ------------------------------------------------------------------------------
    f = trim(fp)//'.hdr'
    call open_file(f, iu)
    call readflt_header(iu, ncol, nrow, xll, yll, cs, nodata_s, nbits, pixeltype)
    if ((nbits /= 32).or.(pixeltype /= 'signedint')) then
      call errmsg('Could not read '//trim(f))
    end if
    read(nodata_s,*) nodata
    !
    if (allocated(x)) deallocate(x)
    allocate(x(ncol,nrow))
    f = trim(fp)//'.flt'
    call open_file(f, iu, 'r', .true.)
    read(iu)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(iu)
    !
    return
  end subroutine readflt_i4
  
  subroutine readflt_r4(fp, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: fp
    integer(i4b), intent(out) :: ncol, nrow
    real(r4b), dimension(:,:), allocatable, intent(inout) :: x
    real(r4b), intent(out) :: nodata
    real(r8b), intent(out) :: xll, yll, cs
    ! -- locals
    character(len=1) :: cdum
    character(len=mxslen) :: f, nodata_s, pixeltype
    integer(i4b) :: iu, nbits, icol, irow
! ------------------------------------------------------------------------------
    f = trim(fp)//'.hdr'
    call open_file(f, iu)
    call readflt_header(iu, ncol, nrow, xll, yll, cs, nodata_s, nbits, pixeltype)
    if ((nbits /= 32).or.(pixeltype /= 'float')) then
      call errmsg('Could not read '//trim(f))
    end if
    read(nodata_s,*) nodata
    !
    if (allocated(x)) deallocate(x)
    allocate(x(ncol,nrow))
    f = trim(fp)//'.flt'
    call open_file(f, iu, 'r', .true.)
    read(iu)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(iu)
    !
    return
  end subroutine readflt_r4
  
  subroutine writeflt_header_r4(iu, ncol, nrow, xll, yll, cs, nodata, &
  nbits, pixeltype)
! ******************************************************************************
    ! -- arguments
    integer(i4b), intent(in) :: iu, ncol, nrow
    character(len=*), intent(in) :: nodata
    real(r4b), intent(in) :: xll, yll, cs
    integer(i4b), intent(in) :: nbits
    character(len=*), intent(in) :: pixeltype
    ! -- locals
! ------------------------------------------------------------------------------
    write(iu,'(a)') 'ncols '//ta((/ncol/))
    write(iu,'(a)') 'nrows '//ta((/nrow/))
    write(iu,'(a)') 'xllcorner '//ta((/xll/))
    write(iu,'(a)') 'yllcorner '//ta((/yll/))
    write(iu,'(a)') 'cellsize '//ta((/cs/))
    write(iu,'(a)') 'nodata_value '//trim(nodata)
    write(iu,'(a)') 'nbits '//ta((/nbits/))
    write(iu,'(a)') 'pixeltype '//trim(pixeltype)
    write(iu,'(a)') 'byteorder lsbfirst'
    close(iu)
    !
    return
  end subroutine writeflt_header_r4
  
  subroutine writeflt_header_r8(iu, ncol, nrow, xll, yll, cs, nodata, &
  nbits, pixeltype)
! ******************************************************************************
    ! -- arguments
    integer(i4b), intent(in) :: iu, ncol, nrow
    character(len=*), intent(in) :: nodata
    real(r8b), intent(in) :: xll, yll, cs
    integer(i4b), intent(in) :: nbits
    character(len=*), intent(in) :: pixeltype
    ! -- locals
! ------------------------------------------------------------------------------
    write(iu,'(a)') 'ncols '//ta((/ncol/))
    write(iu,'(a)') 'nrows '//ta((/nrow/))
    write(iu,'(a)') 'xllcorner '//ta((/xll/))
    write(iu,'(a)') 'yllcorner '//ta((/yll/))
    write(iu,'(a)') 'cellsize '//ta((/cs/))
    write(iu,'(a)') 'nodata_value '//trim(nodata)
    write(iu,'(a)') 'nbits '//ta((/nbits/))
    write(iu,'(a)') 'pixeltype '//trim(pixeltype)
    write(iu,'(a)') 'byteorder lsbfirst'
    close(iu)
    !
    return
  end subroutine writeflt_header_r8
!
  subroutine writeflt_i1_r4(fp, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
! see: https://gdal.org/drivers/raster/ehdr.html#raster-ehdr
    ! -- arguments
    character(len=*), intent(in) :: fp
    integer(i4b), intent(in) :: ncol, nrow
    integer(i1b), dimension(ncol,nrow), intent(in) :: x
    integer(i1b), intent(in) :: nodata
    real(r4b), intent(in) :: xll, yll, cs
    ! -- locals
    character(len=mxslen) :: f
    integer(i4b) :: iu, icol, irow, n
! ------------------------------------------------------------------------------
    f = trim(fp)//'.hdr'
    call open_file(f, iu, 'w')
    call writeflt_header_r4(iu, ncol, nrow, xll, yll, cs, ta((/nodata/)), 8, 'signedint')
    close(iu)
    !
    ! count
    n = 0
    do irow = 1,nrow
      do icol = 1, ncol
        if (x(icol,irow) /= nodata) n = n + 1
      end do
    end do
    call logmsg('# data cells: '//ta((/n/)))
    !
    f = trim(fp)//'.flt'
    call open_file(f, iu, 'w', .true.)
    write(iu)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(iu)
    !
    return
  end subroutine writeflt_i1_r4
  
 subroutine writeflt_i4_r4(fp, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
! see: https://gdal.org/drivers/raster/ehdr.html#raster-ehdr
    ! -- arguments
    character(len=*), intent(in) :: fp
    integer(i4b), intent(in) :: ncol, nrow
    integer(i4b), dimension(ncol,nrow), intent(in) :: x
    integer(i4b), intent(in) :: nodata
    real(r4b), intent(in) :: xll, yll, cs
    ! -- locals
    character(len=mxslen) :: f
    integer(i4b) :: iu, icol, irow, n
! ------------------------------------------------------------------------------
    f = trim(fp)//'.hdr'
    call open_file(f, iu, 'w')
    call writeflt_header_r4(iu, ncol, nrow, xll, yll, cs, ta((/nodata/)), 32, 'signedint')
    close(iu)
    !
    ! count
    n = 0
    do irow = 1,nrow
      do icol = 1, ncol
        if (x(icol,irow) /= nodata) n = n + 1
      end do
    end do
    call logmsg('# data cells: '//ta((/n/)))
    !
    f = trim(fp)//'.flt'
    call open_file(f, iu, 'w', .true.)
    write(iu)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(iu)
    !
    return
  end subroutine writeflt_i4_r4
 
  subroutine writeflt_r4_r4(fp, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
! see: https://gdal.org/drivers/raster/ehdr.html#raster-ehdr
    ! -- arguments
    character(len=*), intent(in) :: fp
    integer(i4b), intent(in) :: ncol, nrow
    real(r4b), dimension(ncol,nrow), intent(in) :: x
    real(r4b), intent(in) :: nodata
    real(r4b), intent(in) :: xll, yll, cs
    ! -- locals
    character(len=mxslen) :: f
    integer(i4b) :: iu, icol, irow, n
! ------------------------------------------------------------------------------
    f = trim(fp)//'.hdr'
    call open_file(f, iu, 'w')
    call writeflt_header_r4(iu, ncol, nrow, xll, yll, cs, ta((/nodata/)), 32, 'float')
    close(iu)
    !
    ! count
    n = 0
    do irow = 1,nrow
      do icol = 1, ncol
        if (x(icol,irow) /= nodata) n = n + 1
      end do
    end do
    call logmsg('# data cells: '//ta((/n/)))
    !
    f = trim(fp)//'.flt'
    call open_file(f, iu, 'w', .true.)
    write(iu)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(iu)
    !
    return
  end subroutine writeflt_r4_r4
  
  subroutine writeflt_i1_r8(fp, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
! see: https://gdal.org/drivers/raster/ehdr.html#raster-ehdr
    ! -- arguments
    character(len=*), intent(in) :: fp
    integer(i4b), intent(in) :: ncol, nrow
    integer(i1b), dimension(ncol,nrow), intent(in) :: x
    integer(i1b), intent(in) :: nodata
    real(r8b), intent(in) :: xll, yll, cs
    ! -- locals
    character(len=mxslen) :: f
    integer(i4b) :: iu, icol, irow, n
! ------------------------------------------------------------------------------
    f = trim(fp)//'.hdr'
    call open_file(f, iu, 'w')
    call writeflt_header_r8(iu, ncol, nrow, xll, yll, cs, ta((/nodata/)), 8, 'signedint')
    close(iu)
    !
    ! count
    n = 0
    do irow = 1,nrow
      do icol = 1, ncol
        if (x(icol,irow) /= nodata) n = n + 1
      end do
    end do
    call logmsg('# data cells: '//ta((/n/)))
    !
    f = trim(fp)//'.flt'
    call open_file(f, iu, 'w', .true.)
    write(iu)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(iu)
    !
    return
  end subroutine writeflt_i1_r8
  
 subroutine writeflt_i4_r8(fp, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
! see: https://gdal.org/drivers/raster/ehdr.html#raster-ehdr
    ! -- arguments
    character(len=*), intent(in) :: fp
    integer(i4b), intent(in) :: ncol, nrow
    integer(i4b), dimension(ncol,nrow), intent(in) :: x
    integer(i4b), intent(in) :: nodata
    real(r8b), intent(in) :: xll, yll, cs
    ! -- locals
    character(len=mxslen) :: f
    integer(i4b) :: iu, icol, irow, n
! ------------------------------------------------------------------------------
    f = trim(fp)//'.hdr'
    call open_file(f, iu, 'w')
    call writeflt_header_r8(iu, ncol, nrow, xll, yll, cs, ta((/nodata/)), 32, 'signedint')
    close(iu)
    !
    ! count
    n = 0
    do irow = 1,nrow
      do icol = 1, ncol
        if (x(icol,irow) /= nodata) n = n + 1
      end do
    end do
    call logmsg('# data cells: '//ta((/n/)))
    !
    f = trim(fp)//'.flt'
    call open_file(f, iu, 'w', .true.)
    write(iu)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(iu)
    !
    return
  end subroutine writeflt_i4_r8
 
  subroutine writeflt_r4_r8(fp, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
! see: https://gdal.org/drivers/raster/ehdr.html#raster-ehdr
    ! -- arguments
    character(len=*), intent(in) :: fp
    integer(i4b), intent(in) :: ncol, nrow
    real(r4b), dimension(ncol,nrow), intent(in) :: x
    real(r4b), intent(in) :: nodata
    real(r8b), intent(in) :: xll, yll, cs
    ! -- locals
    character(len=mxslen) :: f
    integer(i4b) :: iu, icol, irow, n
! ------------------------------------------------------------------------------
    f = trim(fp)//'.hdr'
    call open_file(f, iu, 'w')
    call writeflt_header_r8(iu, ncol, nrow, xll, yll, cs, ta((/nodata/)), 32, 'float')
    close(iu)
    !
    ! count
    n = 0
    do irow = 1,nrow
      do icol = 1, ncol
        if (x(icol,irow) /= nodata) n = n + 1
      end do
    end do
    call logmsg('# data cells: '//ta((/n/)))
    !
    f = trim(fp)//'.flt'
    call open_file(f, iu, 'w', .true.)
    write(iu)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(iu)
    !
    return
  end subroutine writeflt_r4_r8
!
  subroutine writeflt_r8_r8(fp, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
! see: https://gdal.org/drivers/raster/ehdr.html#raster-ehdr
    ! -- arguments
    character(len=*), intent(in) :: fp
    integer(i4b), intent(in) :: ncol, nrow
    real(r8b), dimension(ncol,nrow), intent(in) :: x
    real(r8b), intent(in) :: nodata
    real(r8b), intent(in) :: xll, yll, cs
    ! -- locals
    character(len=mxslen) :: f
    integer(i4b) :: iu, icol, irow, n
    real(r4b), dimension(:,:), allocatable :: r4x
    real(r4b) :: r4nodata
! ------------------------------------------------------------------------------
    f = trim(fp)//'.hdr'
    call open_file(f, iu, 'w')
    r4nodata = real(nodata,r4b)
    call writeflt_header_r8(iu, ncol, nrow, xll, yll, cs, ta((/r4nodata/)), 32, 'float')
    close(iu)
    !
    f = trim(fp)//'.flt'
    call open_file(f, iu, 'w', .true.)
    allocate(r4x(ncol,nrow))
    do irow = 1, nrow
      do icol = 1, ncol
        if (x(icol,irow) /= nodata) then
          r4x(icol,irow) = real(x(icol,irow),r4b)
        else
          r4x(icol,irow) = r4nodata
        end if
      end do
    end do
    !
    ! count
    n = 0
    do irow = 1,nrow
      do icol = 1, ncol
        if (r4x(icol,irow) /= r4nodata) n = n + 1
      end do
    end do
    call logmsg('# data cells: '//ta((/n/)))
    !
    write(iu)((r4x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(iu)
    deallocate(r4x)
    !
    return
  end subroutine writeflt_r8_r8
!
  subroutine writeidf_i1_r8(f, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
    use imod_idf
    ! -- arguments
    character(len=*), intent(in) :: f
    integer(i4b), intent(in) :: ncol, nrow
    integer(i1b), dimension(ncol,nrow), intent(in) :: x
    real(r8b), intent(in) :: xll, yll, cs, nodata
    ! -- locals
    real(r8b), dimension(:,:), allocatable :: wrk
    integer(i4b) :: ic, ir
! ------------------------------------------------------------------------------
    allocate(wrk(ncol,nrow))
    do ir = 1, nrow
      do ic = 1, ncol
        wrk(ic,ir) = real(x(ic,ir),r8b)
      end do
    end do
    !
    call imod_utl_printtext('Writing '//trim(f),0)
    if (.not.idfwrite_wrapper(ncol, nrow, wrk, (/cs/), (/cs/), xll, yll, nodata, '', f, 4)) then
      call imod_utl_printtext('Could not write '//trim(f),2)
    end if
    deallocate(wrk)
    !
    return
  end subroutine writeidf_i1_r8

  subroutine writeidf_i2_r8(f, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
    use imod_idf
    ! -- arguments
    character(len=*), intent(in) :: f
    integer(i4b), intent(in) :: ncol, nrow
    integer(i2b), dimension(ncol,nrow), intent(in) :: x
    real(r8b), intent(in) :: xll, yll, cs, nodata
    ! -- locals
    real(r8b), dimension(:,:), allocatable :: wrk
    integer(i4b) :: ic, ir
! ------------------------------------------------------------------------------
    allocate(wrk(ncol,nrow))
    do ir = 1, nrow
      do ic = 1, ncol
        wrk(ic,ir) = real(x(ic,ir),r8b)
      end do
    end do
    !
    call imod_utl_printtext('Writing '//trim(f),0)
    if (.not.idfwrite_wrapper(ncol, nrow, wrk, (/cs/), (/cs/), xll, yll, nodata, '', f, 4)) then
      call imod_utl_printtext('Could not write '//trim(f),2)
    end if
    deallocate(wrk)
    !
    return
  end subroutine writeidf_i2_r8

  subroutine writeidf_i4_r8(f, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
    use imod_idf
    ! -- arguments
    character(len=*), intent(in) :: f
    integer(i4b), intent(in) :: ncol, nrow
    integer(i4b), dimension(ncol,nrow), intent(in) :: x
    real(r8b), intent(in) :: xll, yll, cs, nodata
    ! -- locals
    real(r8b), dimension(:,:), allocatable :: wrk
    integer(i4b) :: ic, ir
! ------------------------------------------------------------------------------
    allocate(wrk(ncol,nrow))
    do ir = 1, nrow
      do ic = 1, ncol
        wrk(ic,ir) = real(x(ic,ir),r8b)
      end do
    end do
    !
    call imod_utl_printtext('Writing '//trim(f),0)
    if (.not.idfwrite_wrapper(ncol, nrow, wrk, (/cs/), (/cs/), xll, yll, nodata, '', f, 4)) then
      call imod_utl_printtext('Could not write '//trim(f),2)
    end if
    deallocate(wrk)
    !
    return
  end subroutine writeidf_i4_r8

  subroutine writeidf_i8_r8(f, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
    use imod_idf
    ! -- arguments
    character(len=*), intent(in) :: f
    integer(i4b), intent(in) :: ncol, nrow
    integer(i8b), dimension(ncol,nrow), intent(in) :: x
    real(r8b), intent(in) :: xll, yll, cs, nodata
    ! -- locals
    real(r8b), dimension(:,:), allocatable :: wrk
    integer(i4b) :: ic, ir
! ------------------------------------------------------------------------------
    allocate(wrk(ncol,nrow))
    do ir = 1, nrow
      do ic = 1, ncol
        wrk(ic,ir) = real(x(ic,ir),r8b)
      end do
    end do
    !
    call imod_utl_printtext('Writing '//trim(f),0)
    if (.not.idfwrite_wrapper(ncol, nrow, wrk, (/cs/), (/cs/), xll, yll, nodata, '', f, 8)) then
      call imod_utl_printtext('Could not write '//trim(f),2)
    end if
    deallocate(wrk)
    !
    return
  end subroutine writeidf_i8_r8

  subroutine writeidf_r4_r8(f, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
    use imod_idf
    ! -- arguments
    character(len=*), intent(in) :: f
    integer(i4b), intent(in) :: ncol, nrow
    real(r4b), dimension(ncol,nrow), intent(in) :: x
    real(r8b), intent(in) :: xll, yll, cs, nodata
    ! -- locals
    real(r8b), dimension(:,:), allocatable :: wrk
    integer(i4b) :: ic, ir
! ------------------------------------------------------------------------------
    allocate(wrk(ncol,nrow))
    do ir = 1, nrow
      do ic = 1, ncol
        wrk(ic,ir) = real(x(ic,ir),r8b)
      end do
    end do
    !
    call imod_utl_printtext('Writing '//trim(f),0)
    if (.not.idfwrite_wrapper(ncol, nrow, wrk, (/cs/), (/cs/), xll, yll, nodata, '', f, 4)) then
      call imod_utl_printtext('Could not write '//trim(f),2)
    end if
    deallocate(wrk)
    !
    return
  end subroutine writeidf_r4_r8

  subroutine writeidf_r8_r8(f, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
    use imod_idf
    ! -- arguments
    character(len=*), intent(in) :: f
    integer(i4b), intent(in) :: ncol, nrow
    real(r8b), dimension(ncol,nrow), intent(in) :: x
    real(r8b), intent(in) :: xll, yll, cs, nodata
    ! -- locals
! ------------------------------------------------------------------------------
    call imod_utl_printtext('Writing '//trim(f),0)
    if (.not.idfwrite_wrapper(ncol, nrow, x, (/cs/), (/cs/), xll, yll, nodata, '', f, 8)) then
      call imod_utl_printtext('Could not write '//trim(f),2)
    end if
    !
    return
  end subroutine writeidf_r8_r8

  subroutine writeasc_i4_r8(f, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: f
    integer(i4b), intent(in) :: ncol, nrow
    integer(i4b), dimension(ncol,nrow), intent(in) :: x
    real(r8b), intent(in) :: xll, yll, cs, nodata
    ! -- locals
    integer(i4b) :: lun, icol, irow
! ------------------------------------------------------------------------------

    write(*,'(1x,a,1x,2a)') 'Writing',trim(f),'...'

    lun = getlun(); open(unit=lun,file=f,status='replace')
    call writeascheader(lun, ncol, nrow, xll, yll, cs, nodata)
    write(lun,*)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(lun)
    !
    return
  end subroutine writeasc_i4_r8

  subroutine writeasc_r8_r8(f, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: f
    integer(i4b), intent(in) :: ncol, nrow
    real(r8b), dimension(ncol,nrow), intent(in) :: x
    real(r8b), intent(in) :: xll, yll, cs, nodata
    ! -- locals
    integer(i4b) :: lun, icol, irow
! ------------------------------------------------------------------------------

    write(*,'(1x,a,1x,2a)') 'Writing',trim(f),'...'

    lun = getlun(); open(unit=lun,file=f,status='replace')
    call writeascheader(lun, ncol, nrow, xll, yll, cs, nodata)
    write(lun,*)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(lun)
    !
    return
  end subroutine writeasc_r8_r8

  subroutine readasc_r_d(f, x, ncol, nrow, xll, yll, cs, nodata, idebug)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: f
    real, dimension(:,:), allocatable :: x
    integer, intent(out) :: ncol, nrow
    double precision, intent(out) :: xll, yll, cs, nodata
    integer, intent(in), optional :: idebug
    ! --- local
    character(len=1) :: cdum
    integer :: lun, icol, irow, ideb
! ------------------------------------------------------------------------------
    ideb = 0
    if (present(idebug)) then
      ideb = idebug
    end if

    if (allocated(x)) then
      deallocate(x)
    end if

    write(*,*) 'Reading ',trim(f), '...'
    open(newunit=lun,file=f,action='read',status='old')
    read(lun,*) cdum, ncol
    read(lun,*) cdum, nrow
    read(lun,*) cdum, xll
    read(lun,*) cdum, yll
    read(lun,*) cdum, cs
    read(lun,*) cdum, nodata
    allocate(x(ncol,nrow))
    if(ideb == 1) then
      !ncol = ncol/10; nrow = nrow/10; cs=cs*10
      !deallocate(x); allocate(x(ncol,nrow))
      do irow = 1, nrow
        do icol = 1, ncol
          x(icol,irow) = 1
        end do
      end do
      write(*,*) '@@@@ debug mode readasc, setting grid to 1!'
      return
    end if
    read(lun,*)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(lun)
    !
    return
  end subroutine readasc_r_d

  subroutine readasc_r_r(f, x, ncol, nrow, xll, yll, cs, nodata, idebug)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: f
    real, dimension(:,:), allocatable :: x
    integer, intent(out) :: ncol, nrow
    real, intent(out) :: xll, yll, cs, nodata
    integer, intent(in), optional :: idebug
    ! --- local
    character(len=1) :: cdum
    integer :: lun, icol, irow, ideb
! ------------------------------------------------------------------------------
    ideb = 0
    if (present(idebug)) then
      ideb = idebug
    end if

    if (allocated(x)) then
      deallocate(x)
    end if

    write(*,*) 'Reading ',trim(f), '...'
    open(newunit=lun,file=f,action='read',status='old')
    read(lun,*) cdum, ncol
    read(lun,*) cdum, nrow
    read(lun,*) cdum, xll
    read(lun,*) cdum, yll
    read(lun,*) cdum, cs
    read(lun,*) cdum, nodata
    allocate(x(ncol,nrow))
    read(lun,*)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(lun)
    !
    return
  end subroutine readasc_r_r

  function readidf_r8val(idf, icol, irow) result (r8val)
! ******************************************************************************
    ! -- modules
    use imod_idf, only: idfobj
    ! -- arguments

    type(idfobj), intent(in) :: idf
    integer(i4b), intent(in) :: icol, irow
    real(r8b) :: r8val
    ! --- local
    integer(i4b), parameter :: icf = 1
    integer(i4b) :: irec
    real(r4b) :: r4val
! ------------------------------------------------------------------------------
    if ((icol < 1).or.(icol > idf%ncol)) then
      call errmsg('readidf_val: invalid column: '//ta((/icol/)))
    end if
    if ((irow < 1).or.(irow > idf%nrow)) then
      call errmsg('readidf_val: invalid row: '//ta((/irow/)))
    end if
    !
    irec=icf +10  +abs(idf%ieq-1) *2    +idf%ieq*(idf%nrow+idf%ncol) +idf%itb*2
    irec=irec+  ((irow-1)*idf%ncol)+icol
    !
    if (idf%itype == 4) then
      read(idf%iu,rec=irec) r4val
      r8val = real(r4val,r8b)
    else
      read(idf%iu,rec=irec) r8val
    end if
    !
    return
  end function readidf_r8val
  
  subroutine readidf_block_i4(idf, ir0, ir1, ic0, ic1, arr, nodata_in)
! ******************************************************************************
    ! -- modules
    use imod_idf, only: idfobj
    ! -- arguments
    type(idfobj), intent(in) :: idf
    integer(i4b), intent(in) :: ir0, ir1, ic0, ic1
    integer(i4b), dimension(:,:), pointer, intent(inout) :: arr
    integer(i4b), intent(in), optional :: nodata_in
    ! --- local
    integer(i4b) :: nc, nr, ic, ir, jc, jr
    integer(i4b) :: nodata
    real(r4b) :: r4val
    real(r8b) :: r8val
! ------------------------------------------------------------------------------
    if (present(nodata_in)) then
      nodata = nodata_in
    else
      nodata = int(idf%nodata)
    end if
    !
    nc = ic1-ic0+1; nr = ir1-ir0+1
    write(sa(1),*) nc; write(sa(2),*) nr;
    call logmsg('Reading i4 block for '//trim(idf%fname)//' ('//trim(adjustl(sa(1)))//&
      ','//trim(adjustl(sa(2)))//')...')
    if (associated(arr)) deallocate(arr)
    allocate(arr(nc,nr))
    !
    do ir = ir0, ir1
      do ic = ic0, ic1
        jr = ir-ir0+1; jc = ic-ic0+1
        r8val = readidf_r8val(idf, ic, ir)
        if (r8val /= idf%nodata) then
          arr(jc,jr) = int(r8val)
        else
          arr(jc,jr) = nodata
        end if
      end do
    end do
    call logmsg('Done reading i4 block')
    !
    return
  end subroutine readidf_block_i4

  subroutine readidf_block_r4(idf, ir0, ir1, ic0, ic1, arr, nodata_in)
! ******************************************************************************
    ! -- modules
    use imod_idf, only: idfobj
    ! -- arguments
    type(idfobj), intent(in) :: idf
    integer(i4b), intent(in) :: ir0, ir1, ic0, ic1
    real(r4b), dimension(:,:), pointer, intent(inout) :: arr
    real(r4b), intent(in), optional :: nodata_in
    ! --- local
    integer(i4b) :: nc, nr, ic, ir, jc, jr
    real(r4b) :: nodata
    real(r4b) :: r4val
    real(r8b) :: r8val
! ------------------------------------------------------------------------------
    if (present(nodata_in)) then
      nodata = nodata_in
    else
      nodata = real(idf%nodata,r4b)
    end if
    !
    nc = ic1-ic0+1; nr = ir1-ir0+1
    write(sa(1),*) nc; write(sa(2),*) nr;
    call logmsg('Reading r4 block for '//trim(idf%fname)//' ('//trim(adjustl(sa(1)))//&
      ','//trim(adjustl(sa(2)))//')...')
    if (associated(arr)) deallocate(arr)
    allocate(arr(nc,nr))
    !
    do ir = ir0, ir1
      do ic = ic0, ic1
        jr = ir-ir0+1; jc = ic-ic0+1
        r8val = readidf_r8val(idf, ic, ir)
        if (r8val /= idf%nodata) then
          arr(jc,jr) = real(r8val,r4b)
        else
          arr(jc,jr) = nodata
        end if
      end do
    end do
    call logmsg('Done reading r4 block')
    !
    return
  end subroutine readidf_block_r4

  subroutine readidf_block_r8(idf, ir0, ir1, ic0, ic1, arr, nodata_in)
! ******************************************************************************
    ! -- modules
    use imod_idf, only: idfobj
    ! -- arguments
    type(idfobj), intent(in) :: idf
    integer(i4b), intent(in) :: ir0, ir1, ic0, ic1
    real(r8b), dimension(:,:), pointer, intent(inout) :: arr
    real(r8b), intent(in), optional :: nodata_in
    ! --- local
    integer(i4b) :: nc, nr, ic, ir, jc, jr
    real(r8b) :: nodata
    real(r4b) :: r4val
    real(r8b) :: r8val
! ------------------------------------------------------------------------------
    if (present(nodata_in)) then
      nodata = nodata_in
    else
      nodata = int(idf%nodata)
    end if
    !
    nc = ic1-ic0+1; nr = ir1-ir0+1
    write(sa(1),*) nc; write(sa(2),*) nr;
    call logmsg('Reading r8 block for '//trim(idf%fname)//' ('//trim(adjustl(sa(1)))//&
      ','//trim(adjustl(sa(2)))//')...')
    if (associated(arr)) deallocate(arr)
    allocate(arr(nc,nr))
    !
    do ir = ir0, ir1
      do ic = ic0, ic1
        jr = ir-ir0+1; jc = ic-ic0+1
        r8val = readidf_r8val(idf, ic, ir)
        if (r8val /= idf%nodata) then
          arr(jc,jr) = r8val
        else
          arr(jc,jr) = nodata
        end if
      end do
    end do
    call logmsg('Done reading r8 block')
    !
    return
  end subroutine readidf_block_r8

  subroutine readidf_i_r(f, x, ncol, nrow, xll, yll, cs, nodata, &
     in_ir0, in_ir1, in_ic0, in_ic1, in_idebug)
! ******************************************************************************
    ! -- modules
    use imod_idf
    ! -- arguments
    character(len=*), intent(in) :: f
    integer, dimension(:,:), allocatable :: x
    integer, intent(out) :: ncol, nrow
    real, intent(out) :: xll, yll, cs, nodata
    integer, intent(in), optional :: in_ir0, in_ir1, in_ic0, in_ic1, in_idebug
    ! --- local
    integer :: idebug, ir0, ir1, ic0, ic1
    integer :: rddata, icol, irow, jcol, jrow
    type(idfobj) :: idf
! ------------------------------------------------------------------------------
    idebug = 0
    if (present(in_idebug)) then
      idebug = in_idebug
    end if
    !
    rddata = 1
    if (present(in_ir0).or.present(in_ir1).or. &
        present(in_ic0).or.present(in_ic1)) then
      rddata = 0
    end if
    call imod_utl_printtext('Reading '//trim(f),0)
    if (.not.idfread(idf,f,rddata)) then
      call imod_utl_printtext('Could not read '//trim(f),2)
    end if
    if (rddata == 1) close(idf%iu)
    !
    ir0 = 1; if (present(in_ir0)) ir0 = max(1, in_ir0)
    ir1 = 1; if (present(in_ir1)) ir1 = min(idf%nrow, in_ir1)
    ic0 = 1; if (present(in_ic0)) ic0 = max(1, in_ic0)
    ic1 = 1; if (present(in_ic1)) ic1 = min(idf%ncol, in_ic1)
    !
    if (allocated(x)) then
      deallocate(x)
    end if
    !
    ncol   = ic1 - ic0 + 1
    nrow   = ir1 - ir0 + 1
    cs     = idf%dx
    xll    = idf%xmin + (ic0-1)*cs
    yll    = idf%ymin + (idf%nrow - ir1)*cs
    nodata = idf%nodata
    allocate(x(ncol,nrow))
    if(idebug == 1) then
      do irow = 1, nrow
        do icol = 1, ncol
          x(icol,irow) = 1
        end do
      end do
      write(*,*) '@@@@ debug mode readasc, setting grid to 1!'
      call idfdeallocatex(idf)
      return
    end if
    !
    if (rddata == 0) then
      allocate(idf%x(ncol,nrow))
      do irow = ir0, ir1
        do icol = ic0, ic1
          jrow = irow - ir0 + 1; jcol = icol - ic0 + 1
          idf%x(jcol,jrow) = readidf_r8val(idf, icol, irow)
        end do
      end do
      close(idf%iu)
    end if
    !
    do irow = 1, nrow
      do icol = 1, ncol
        if (idf%x(icol,irow) /= idf%nodata) then
          x(icol,irow) = int(idf%x(icol,irow))
        else
           x(icol,irow) = 0
        end if
      end do
    end do
    nodata = 0.
    call idfdeallocatex(idf)
    !
    return
  end subroutine readidf_i_r

  subroutine readidf_r_r(f, x, ncol, nrow, xll, yll, cs, nodata, idebug)
! ******************************************************************************
    ! -- modules
    use imod_idf
    ! -- arguments
    character(len=*), intent(in) :: f
    real, dimension(:,:), allocatable :: x
    integer, intent(out) :: ncol, nrow
    real, intent(out) :: xll, yll, cs, nodata
    integer, intent(in), optional :: idebug
    ! --- local
    integer :: icol, irow, ideb
    type(idfobj) :: idf
! ------------------------------------------------------------------------------
    ideb = 0
    if (present(idebug)) then
      ideb = idebug
    end if

    if (allocated(x)) then
      deallocate(x)
    end if

    call imod_utl_printtext('Reading '//trim(f),0)
    if (.not.idfread(idf,f,1)) then
      call imod_utl_printtext('Could not read '//trim(f),2)
    end if
    close(idf%iu)

    ncol   = idf%ncol
    nrow   = idf%nrow
    xll    = idf%xmin
    yll    = idf%ymin
    cs     = idf%dx
    nodata = idf%nodata
    allocate(x(ncol,nrow))
    if(ideb == 1) then
      do irow = 1, nrow
        do icol = 1, ncol
          x(icol,irow) = 1
        end do
      end do
      write(*,*) '@@@@ debug mode readasc, setting grid to 1!'
    else
      do irow = 1, nrow
        do icol = 1, ncol
          if (idf%x(icol,irow) /= idf%nodata) then
            x(icol,irow) = idf%x(icol,irow)
          else
            x(icol,irow) = 0.
          end if
        end do
      end do
      nodata = 0.d0
    end if
    call idfdeallocatex(idf)
    !
    return
  end subroutine readidf_r_r

  subroutine readidf_r_d(f, x, ncol, nrow, xll, yll, cs, nodata, idebug)
! ******************************************************************************
    ! -- modules
    use imod_idf
    ! -- arguments
    character(len=*), intent(in) :: f
    real, dimension(:,:), allocatable :: x
    integer, intent(out) :: ncol, nrow
    double precision, intent(out) :: xll, yll, cs, nodata
    integer, intent(in), optional :: idebug
    ! --- local
    integer :: icol, irow, ideb
    type(idfobj) :: idf
! ------------------------------------------------------------------------------

    ideb = 0
    if (present(idebug)) then
      ideb = idebug
    end if

    if (allocated(x)) then
      deallocate(x)
    end if

    call imod_utl_printtext('Reading '//trim(f),0)
    if (.not.idfread(idf,f,1)) then
      call imod_utl_printtext('Could not read '//trim(f),2)
    end if
    close(idf%iu)

    ncol   = idf%ncol
    nrow   = idf%nrow
    xll    = idf%xmin
    yll    = idf%ymin
    cs     = idf%dx
    nodata = idf%nodata
    allocate(x(ncol,nrow))
    if(ideb == 1) then
      do irow = 1, nrow
        do icol = 1, ncol
          x(icol,irow) = 1
        end do
      end do
      write(*,*) '@@@@ debug mode readasc, setting grid to 1!'
    else
      do irow = 1, nrow
        do icol = 1, ncol
          if (idf%x(icol,irow) /= idf%nodata) then
            x(icol,irow) = idf%x(icol,irow)
          else
            x(icol,irow) = 0.
          end if
        end do
      end do
      nodata = 0.d0
    end if
    call idfdeallocatex(idf)
    !
    return
  end subroutine readidf_r_d

  subroutine errmsg(msg)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: msg
! ------------------------------------------------------------------------------
    write(*,'(a)') 'Error: '//trim(msg)
    stop 1
  end subroutine errmsg

  subroutine logmsg(msg)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: msg
! ------------------------------------------------------------------------------
    write(*,'(a)') trim(msg)
    !
    return
  end subroutine logmsg

! $Id: quicksort.f90 558 2015-03-25 13:44:47Z larsnerger $

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!    Collection of subroutines to sort and return a one-dimensional array
!!!    as well as corresponding sorted index of the array a. Original code
!!!    (distributed under GNU Free licence 1.2) was taken from
!!!    http://rosettacode.org/wiki/Quicksort#Fortran and modified to
!!!    also return sorted index of the original array a.
!!!    Copyright (C) 2015  Sanita Vetra-Carvalho
!!!
!!!    This program is distributed under the Lesser General Public License (LGPL) version 3,
!!!    for more details see <https://www.gnu.org/licenses/lgpl.html>.
!!!
!!!    Email: s.vetra-carvalho @ reading.ac.uk
!!!    Mail:  School of Mathematical and Physical Sciences,
!!!    	      University of Reading,
!!!	      Reading, UK
!!!	      RG6 6BB
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> subroutine to sort using the quicksort algorithm
!! @param[in,out] a, an array of doubles to be sorted
!! @param[out] idx_a, an array of sorted indecies of original array a
!! @param[in] na, dimension of the array a

recursive subroutine quicksort_d(a,idx_a,na)

! DUMMY ARGUMENTS
integer(i4b), intent(in) :: na ! nr or items to sort
real(r8b), dimension(nA), intent(inout) :: a ! vector to be sorted
integer(i4b), dimension(nA), intent(inout) :: idx_a ! sorted indecies of a

! LOCAL VARIABLES
integer(i4b) :: left, right, mid
real(r8b) :: pivot, temp
integer(i4b) :: marker, idx_temp

if (nA > 1) then
! insertion sort limit of 47 seems best for sorting 10 million
! integers on Intel i7-980X CPU.  Derived data types that use
! more memory are optimized with smaller values - around 20 for a 16
! -byte type.
  if (nA > 47) then
  ! Do quicksort for large groups
  ! Get median of 1st, mid, & last points for pivot (helps reduce
  ! long execution time on some data sets, such as already
  ! sorted data, over simple 1st point pivot)
    mid = (nA+1)/2
    if (a(mid) >= a(1)) then
      if (a(mid) <= a(nA)) then
        pivot = a(mid)
      else if (a(nA) > a(1)) then
        pivot = a(nA)
      else
        pivot = a(1)
      end if
    else if (a(1) <= a(nA)) then
      pivot = a(1)
    else if (a(nA) > a(mid)) then
      pivot = a(nA)
    else
      pivot = a(mid)
    end if

    left = 0
    right = nA + 1

    do while (left < right)
      right = right - 1
      do while (A(right) > pivot)
        right = right - 1
      end do
      left = left + 1
      do while (A(left) < pivot)
        left = left + 1
      end do
      if (left < right) then
        temp = A(left)
        idx_temp = idx_a(left)
        A(left) = A(right)
        idx_a(left) = idx_a(right)
        A(right) = temp
        idx_a(right) = idx_temp
      end if
    end do

    if (left == right) then
      marker = left + 1
    else
      marker = left
    end if

    call quicksort_d(A(:marker-1),idx_A(:marker-1),marker-1)
    call quicksort_d(A(marker:),idx_A(marker:),nA-marker+1)

  else
      call InsertionSort_d(A,idx_a,nA)    ! Insertion sort for small groups is
      !  faster than Quicksort
  end if
end if

return
end subroutine quicksort_d

!> subroutine to sort using the insertionsort algorithm and return indecies
!! @param[in,out] a, an array of doubles to be sorted
!! @param[in,out] idx_a, an array of integers of sorted indecies
!! @param[in] na, dimension of the array a
subroutine InsertionSort_d(a,idx_a,na)

  ! DUMMY ARGUMENTS
  integer(i4b),intent(in) :: na
  real(r8b), dimension(nA), intent(inout) :: a
  integer(i4b),dimension(nA), intent(inout) :: idx_a

! LOCAL VARIABLES
  real(r8b) :: temp
  integer(i4b):: i, j
  integer(i4b):: idx_tmp

  do i = 2, nA
     j = i - 1
     temp = A(i)
     idx_tmp = idx_a(i)
     do
        if (j == 0) exit
        if (a(j) <= temp) exit
        A(j+1) = A(j)
        idx_a(j+1) = idx_a(j)
        j = j - 1
     end do
     a(j+1) = temp
     idx_a(j+1) = idx_tmp
  end do
  return
end subroutine InsertionSort_d

recursive subroutine quicksort_r(a,idx_a,na)

! DUMMY ARGUMENTS
integer(i4b), intent(in) :: na ! nr or items to sort
real(r4b), dimension(nA), intent(inout) :: a ! vector to be sorted
integer(i4b), dimension(nA), intent(inout) :: idx_a ! sorted indecies of a

! LOCAL VARIABLES
integer(i4b) :: left, right, mid
real(r4b) :: pivot, temp
integer(i4b) :: marker, idx_temp

if (nA > 1) then
! insertion sort limit of 47 seems best for sorting 10 million
! integers on Intel i7-980X CPU.  Derived data types that use
! more memory are optimized with smaller values - around 20 for a 16
! -byte type.
  if (nA > 47) then
  ! Do quicksort for large groups
  ! Get median of 1st, mid, & last points for pivot (helps reduce
  ! long execution time on some data sets, such as already
  ! sorted data, over simple 1st point pivot)
    mid = (nA+1)/2
    if (a(mid) >= a(1)) then
      if (a(mid) <= a(nA)) then
        pivot = a(mid)
      else if (a(nA) > a(1)) then
        pivot = a(nA)
      else
        pivot = a(1)
      end if
    else if (a(1) <= a(nA)) then
      pivot = a(1)
    else if (a(nA) > a(mid)) then
      pivot = a(nA)
    else
      pivot = a(mid)
    end if

    left = 0
    right = nA + 1

    do while (left < right)
      right = right - 1
      do while (A(right) > pivot)
        right = right - 1
      end do
      left = left + 1
      do while (A(left) < pivot)
        left = left + 1
      end do
      if (left < right) then
        temp = A(left)
        idx_temp = idx_a(left)
        A(left) = A(right)
        idx_a(left) = idx_a(right)
        A(right) = temp
        idx_a(right) = idx_temp
      end if
    end do

    if (left == right) then
      marker = left + 1
    else
      marker = left
    end if

    call quicksort_r(A(:marker-1),idx_A(:marker-1),marker-1)
    call quicksort_r(A(marker:),idx_A(marker:),nA-marker+1)

  else
      call InsertionSort_r(A,idx_a,nA)    ! Insertion sort for small groups is
      !  faster than Quicksort
  end if
end if

return
end subroutine quicksort_r

subroutine InsertionSort_r(a,idx_a,na)

  ! DUMMY ARGUMENTS
  integer(i4b),intent(in) :: na
  real(r4b), dimension(nA), intent(inout) :: a
  integer(i4b),dimension(nA), intent(inout) :: idx_a

! LOCAL VARIABLES
  real(r4b) :: temp
  integer(i4b):: i, j
  integer(i4b):: idx_tmp

  do i = 2, nA
     j = i - 1
     temp = A(i)
     idx_tmp = idx_a(i)
     do
        if (j == 0) exit
        if (a(j) <= temp) exit
        A(j+1) = A(j)
        idx_a(j+1) = idx_a(j)
        j = j - 1
     end do
     a(j+1) = temp
     idx_a(j+1) = idx_tmp
  end do
  return
end subroutine InsertionSort_r

subroutine addboundary_i(wrk, ncol, nrow)
! ******************************************************************************
  integer, intent(in) :: ncol, nrow
  integer, dimension(ncol,nrow), intent(inout) :: wrk

  integer :: icol, irow, jp

  !write(*,*) 'ADDING CONSTANT HEAD BOUNDARY!'
  do irow = 1, nrow
    do icol = 1, ncol
      jp = wrk(icol,irow)
      if (jp > 0) then
        ! N
        if (irow > 1) then
          if (wrk(icol,irow-1) == 0) then
            wrk(icol,irow-1) = -jp
          end if
        end if
        ! S
        if (irow < nrow) then
          if (wrk(icol,irow+1) == 0) then
            wrk(icol,irow+1) = -jp
          end if
        end if
        ! W
        if (icol > 1) then
          if (wrk(icol-1,irow) == 0) then
            wrk(icol-1,irow) = -jp
          end if
        end if
        ! E
        if (icol < ncol) then
          if (wrk(icol+1,irow) == 0) then
            wrk(icol+1,irow) = -jp
          end if
        end if
      end if
    end do
  end do

  !do irow = 1, nrow
  !  do icol = 1, ncol
  !    jp = wrk(icol,irow)
  !    wrk(icol,irow) = abs(jp)
  !  end do
  !end do
  !
  return
end subroutine addboundary_i

subroutine addboundary_i_list(wrk, ncol, nrow, icir, nicir)
! ******************************************************************************
  integer, intent(in) :: ncol, nrow
  integer, dimension(ncol,nrow), intent(inout) :: wrk
  integer, intent(in) :: nicir
  integer, dimension(:,:), intent(in) :: icir
  
  integer :: i, icol, irow, jp

  !write(*,*) 'ADDING CONSTANT HEAD BOUNDARY!'
  do i = 1, nicir
    icol = icir(1,i); irow = icir(2,i)
    jp = wrk(icol,irow)
    if (jp > 0) then
      ! N
      if (irow > 1) then
        if (wrk(icol,irow-1) == 0) then
          wrk(icol,irow-1) = -jp
        end if
      end if
      ! S
      if (irow < nrow) then
        if (wrk(icol,irow+1) == 0) then
          wrk(icol,irow+1) = -jp
        end if
      end if
      ! W
      if (icol > 1) then
        if (wrk(icol-1,irow) == 0) then
          wrk(icol-1,irow) = -jp
        end if
      end if
      ! E
      if (icol < ncol) then
        if (wrk(icol+1,irow) == 0) then
          wrk(icol+1,irow) = -jp
        end if
      end if
    end if
  end do

  !do irow = 1, nrow
  !  do icol = 1, ncol
  !    jp = wrk(icol,irow)
  !    wrk(icol,irow) = abs(jp)
  !  end do
  !end do
  !
  return
end subroutine addboundary_i_list

subroutine addboundary_d(wrk, ncol, nrow, nodata)
! ******************************************************************************
  integer, intent(in) :: ncol, nrow
  double precision, dimension(ncol,nrow), intent(inout) :: wrk
  double precision, intent(in) :: nodata

  integer :: icol, irow, jp

  !write(*,*) 'ADDING CONSTANT HEAD BOUNDARY!'
  do irow = 1, nrow
    do icol = 1, ncol
      jp = int(wrk(icol,irow))
      if (jp > 0) then
        ! N
        if (irow > 1) then
          if (wrk(icol,irow-1) == nodata) then
            wrk(icol,irow-1) = -dble(jp)
          end if
        end if
        ! S
        if (irow < nrow) then
          if (wrk(icol,irow+1) == nodata) then
            wrk(icol,irow+1) = -dble(jp)
          end if
        end if
        ! W
        if (icol > 1) then
          if (wrk(icol-1,irow) == nodata) then
            wrk(icol-1,irow) = -dble(jp)
          end if
        end if
        ! E
        if (icol < ncol) then
          if (wrk(icol+1,irow) == nodata) then
            wrk(icol+1,irow) = -dble(jp)
          end if
        end if
      end if
    end do
  end do

  !do irow = 1, nrow
  !  do icol = 1, ncol
  !    jp = wrk(icol,irow)
  !    wrk(icol,irow) = abs(jp)
  !  end do
  !end do
  !
  return
end subroutine addboundary_d

subroutine addboundary_r(wrk, nodata)
! ******************************************************************************
  real, dimension(:,:), intent(inout) :: wrk
  real, intent(in) :: nodata

  integer :: ncol, nrow, icol, irow, jp

  ncol = size(wrk,1); nrow = size(wrk,2)

  !write(*,*) 'ADDING CONSTANT HEAD BOUNDARY!'
  do irow = 1, nrow
    do icol = 1, ncol
      if (wrk(icol,irow) /= nodata) then
        jp = int(wrk(icol,irow))
        if (jp > 0) then
          ! N
          if (irow > 1) then
            if (wrk(icol,irow-1) == nodata) then
              wrk(icol,irow-1) = -real(jp)
            end if
          end if
          ! S
          if (irow < nrow) then
            if (wrk(icol,irow+1) == nodata) then
              wrk(icol,irow+1) = -real(jp)
            end if
          end if
          ! W
          if (icol > 1) then
            if (wrk(icol-1,irow) == nodata) then
              wrk(icol-1,irow) = -real(jp)
            end if
          end if
          ! E
          if (icol < ncol) then
            if (wrk(icol+1,irow) == nodata) then
              wrk(icol+1,irow) = -real(jp)
            end if
          end if
        end if
      end if
    end do
  end do

  !do irow = 1, nrow
  !  do icol = 1, ncol
  !    jp = wrk(icol,irow)
  !    wrk(icol,irow) = abs(jp)
  !  end do
  !end do
  !
  return
end subroutine addboundary_r

  subroutine calc_unique_i(p, ps, pu, unp, id, nbnd, xll, yll, cs, lbnd_in)
! ******************************************************************************
    ! -- arguments
    integer, dimension(:,:), intent(in) :: p
    integer, intent(in) :: ps
    integer, dimension(:,:), allocatable, intent(inout) :: pu
    type(tUnp), dimension(:), allocatable, intent(inout) :: unp
    integer, intent(out) :: id
    integer, intent(out) :: nbnd
    real, intent(in) :: xll, yll, cs
    logical, intent(in), optional :: lbnd_in
    ! --- local
    integer, parameter ::jp = 1, jn = 2, js = 3, jw = 4, je = 5
    integer, parameter ::jnw = 6, jne = 7, jsw = 8, jse = 9
    integer, parameter :: nsten = jse
    integer, dimension(2,nsten) :: s1

    integer, parameter :: nnmax = 100
    integer :: nst, ic, ir, jc, jr, ncol, nrow, n1, n2, i, j, k, iact, n, m
    integer :: ics, irs, ict, irt, is, it, jt, ic0, ic1, ir0, ir1, itmin, imin
    integer :: ictmin, irtmin, nc, nr, nlst
    integer, dimension(:,:), allocatable :: lst1, lst2, wrk
    double precision, dimension(:), allocatable :: ds
    integer, dimension(:), allocatable :: dsi
    double precision :: d, dmin
    logical :: ldone, lbnd
! ------------------------------------------------------------------------------
    if (ps == 5) then
      nst = je
    else
      nst = nsten
    end if

    lbnd = .false.
    if (present(lbnd_in)) then
      lbnd = lbnd_in
    end if

    ncol = size(p,1); nrow = size(p,2)
    if (allocated(pu)) then
      deallocate(pu)
    end if
    allocate(pu(ncol,nrow))
    
    nlst = max(nst,ncol*nrow)
    allocate(lst1(2,nlst), lst2(2,nlst), wrk(ncol,nrow))

    do ir = 1, nrow
      do ic = 1, ncol
        pu(ic,ir) = 0
        wrk(ic,ir) = 0
      end do
    end do

    !write(*,*) 'Computing unique parts...'
    !write(*,*) 'Min/max=',minval(p), maxval(p)

    id = 0
    do ir = 1, nrow
      do ic = 1, ncol
        if ((p(ic,ir) /= 0) .and. (pu(ic,ir) == 0)) then

          ! set stencil
          s1(1,jp) = ic;             s1(2,jp) = ir
          s1(1,jn) = ic;             s1(2,jn) = max(1,   ir-1)
          s1(1,js) = ic;             s1(2,js) = min(nrow,ir+1)
          s1(1,jw) = max(1,   ic-1); s1(2,jw) = ir
          s1(1,je) = min(ncol,ic+1); s1(2,je) = ir
          s1(1,jnw) = s1(1,jw); s1(2,jnw) = s1(2,jn)
          s1(1,jne) = s1(1,je); s1(2,jne) = s1(2,jn)
          s1(1,jsw) = s1(1,jw); s1(2,jsw) = s1(2,js)
          s1(1,jse) = s1(1,je); s1(2,jse) = s1(2,js)
          !
          id = id + 1

          n1 = 0
          jc = s1(1,1); jr = s1(2,1)
          pu(jc,jr) = id
          do i = 2, nst
            jc = s1(1,i); jr = s1(2,i)
            if ((abs(p(jc,jr)) > 0) .and. (pu(jc,jr) == 0)) then
              n1 = n1 + 1
              lst1(1,n1) = jc; lst1(2,n1) = jr
            end if
          end do

          ldone = .false.
          do while (.not.ldone)
            n2 = 0
            do i = 1, n1
              jc = lst1(1,i); jr = lst1(2,i)
              s1(1,jp) = jc;             s1(2,jp) = jr
              s1(1,jn) = jc;             s1(2,jn) = max(1,   jr-1)
              s1(1,js) = jc;             s1(2,js) = min(nrow,jr+1)
              s1(1,jw) = max(1,   jc-1); s1(2,jw) = jr
              s1(1,je) = min(ncol,jc+1); s1(2,je) = jr
              s1(1,jnw) = s1(1,jw); s1(2,jnw) = s1(2,jn)
              s1(1,jne) = s1(1,je); s1(2,jne) = s1(2,jn)
              s1(1,jsw) = s1(1,jw); s1(2,jsw) = s1(2,js)
              s1(1,jse) = s1(1,je); s1(2,jse) = s1(2,js)
              pu(jc,jr) = id
              do j = 2, nst
                jc = s1(1,j); jr = s1(2,j)
                if ((abs(p(jc,jr)) > 0) .and. (pu(jc,jr) == 0) .and. (wrk(jc,jr) == 0)) then
                  n2 = n2 + 1
                  lst2(1,n2) = jc; lst2(2,n2) = jr
                  wrk(jc,jr) = 1
                end if
              end do
            end do
            !
            if (n2 == 0) then
              !write(*,*) 'id =',id
              ldone = .true.
              exit
            end if
            !
            ! set list 1
            do i = 1, n1
              jc = lst1(1,i); jr = lst1(2,i)
              pu(jc,jr) = id
            end do
            !
            ! copy list, set work
            do i = 1, n2
              jc = lst2(1,i); jr = lst2(2,i)
              lst1(1,i) = jc; lst1(2,i) = jr
              wrk(jc,jr) = 0
            end do
            n1 = n2
          end do
        end if
      end do
    end do
    !
    !write(*,*) '# unique parts found:',id
    !
    ! cleanup
    deallocate(lst1, lst2)
    !
    ! determine bounding box
    if (allocated(unp)) then
      deallocate(unp)
    end if
    allocate(unp(id))
    !
    do i = 1, id
      unp(i)%ic0 = ncol
      unp(i)%ic1 = 0
      unp(i)%ir0 = nrow
      unp(i)%ir1 = 0
    end do

    do ir = 1, nrow
      do ic = 1, ncol
        i = pu(ic,ir)
        if (i /= 0) then
          unp(i)%n = unp(i)%n + 1
          if (p(ic,ir) < 0) then
            pu(ic,ir) = -pu(ic,ir)
          end if
          unp(i)%ic0 = min(unp(i)%ic0, ic)
          unp(i)%ic1 = max(unp(i)%ic1, ic)
          unp(i)%ir0 = min(unp(i)%ir0, ir)
          unp(i)%ir1 = max(unp(i)%ir1, ir)
        end if
      end do
    end do
    !
    !n = 0
    !do ir = 1, nrow
    !  do ic = 1, ncol
    !    i = p(ic,ir)
    !    if (i /= 0) then
    !      n = n + 1
    !    end if
    !  end do
    !end do

    do i = 1, id
      unp(i)%ncol = unp(i)%ic1 - unp(i)%ic0 + 1
      unp(i)%nrow = unp(i)%ir1 - unp(i)%ir0 + 1
      unp(i)%ncol = max(1, unp(i)%ncol)
      unp(i)%nrow = max(1, unp(i)%nrow)
    end do
    !
    ! count the bounds
    do is = 1, id
      ir0 = unp(is)%ir0; ir1 = unp(is)%ir1
      ic0 = unp(is)%ic0; ic1 = unp(is)%ic1
      do ir = max(ir0-1,1), min(ir1+1,nrow)
        do ic = max(ic0-1,1), min(ic1+1,ncol)
          wrk(ic,ir) = 0
        end do
      end do
      do ir = ir0, ir1
        do ic = ic0, ic1
          j = pu(ic,ir)
          if ((j > 0).and.(is == j)) then
            ! set stencil
            s1(1,jp) = ic;             s1(2,jp) = ir
            s1(1,jn) = ic;             s1(2,jn) = max(1,   ir-1)
            s1(1,js) = ic;             s1(2,js) = min(nrow,ir+1)
            s1(1,jw) = max(1,   ic-1); s1(2,jw) = ir
            s1(1,je) = min(ncol,ic+1); s1(2,je) = ir
            s1(1,jnw) = s1(1,jw); s1(2,jnw) = s1(2,jn)
            s1(1,jne) = s1(1,je); s1(2,jne) = s1(2,jn)
            s1(1,jsw) = s1(1,jw); s1(2,jsw) = s1(2,js)
            s1(1,jse) = s1(1,je); s1(2,jse) = s1(2,js)
            do i = 2, nst
              jc = s1(1,i); jr = s1(2,i)
              if ((pu(jc,jr) == 0).and.(wrk(jc,jr) == 0)) then
                unp(is)%nbnd = unp(is)%nbnd + 1
                wrk(jc,jr) = 1
              end if
            end do
          end if
        end do
      end do
    end do
    !
    deallocate(wrk)

    !call writeidf('unp.idf',pu,ncol,nrow,xll,yll,cs,0.)
    !stop
    !
    ! set boundary
    nbnd = 0

    if (.not.lbnd) return

    do i = 1, id
      do iact = 1, 2
        unp(i)%nbnd = 0
        do ir = unp(i)%ir0, unp(i)%ir1
          do ic = unp(i)%ic0, unp(i)%ic1
            if (pu(ic,ir) == -i) then
              unp(i)%nbnd = unp(i)%nbnd + 1
              if (iact == 2) then
                j = unp(i)%nbnd
                unp(i)%bnd(1,j) = ic
                unp(i)%bnd(2,j) = ir
              end if
            end if
          end do
        end do
        if (iact == 1) then
          n = max(unp(i)%nbnd, 1)
          allocate(unp(i)%bnd(2,n))
          allocate(unp(i)%out_bndnn(2,n))
          allocate(unp(i)%out_itnn(n))
          allocate(unp(i)%out_d(n))
          do j = 1, n
            unp(i)%out_itnn(j) = 0
            unp(i)%out_d(j) = 0
          end do
          nbnd = nbnd + n
        end if
      end do !iact
    end do !i
    !
    ! all to all
    do is = 1, id
      allocate(unp(is)%all_bndnn(2,id))
      allocate(unp(is)%all_d(id))
      ics = unp(is)%bnd(1,1); irs = unp(is)%bnd(2,1)
      do it = 1, id
        ict = unp(it)%bnd(1,1); irt = unp(it)%bnd(2,1)
        unp(is)%all_bndnn(1,it) = ict
        unp(is)%all_bndnn(2,it) = irt
        unp(is)%all_d(it) = sqrt( real((ics-ict)**2 + (irs-irt)**2 )) !P
      end do
    end do

    allocate(ds(id), dsi(id))
    !
    do is = 1, id
      do j = 1, unp(is)%nbnd
        do i = 1, id
          ds(i) = huge(0)
          dsi(i) = i
        end do
        !
        ics = unp(is)%bnd(1,j); irs = unp(is)%bnd(2,j)
        !
        ! distances to bounding box of neighboring parts
        do it = 1, id
          if (it == is) then
            ds(is) = huge(0.d0)
            cycle
          end if
          ic0 = unp(it)%ic0; ic1 =  unp(it)%ic1
          ir0 = unp(it)%ir0; ir1 =  unp(it)%ir1
          nc = unp(it)%ncol; nr = unp(it)%nrow

          ds(it) = min(ds(it), sqrt( real((ics-(ic0+nc/2))**2 + (irs-(ir0+nr/2))**2 ))) !P
          ds(it) = min(ds(it), sqrt( real((ics-ic0)**2 + (irs-ir0)**2 ))) !NW
          ds(it) = min(ds(it), sqrt( real((ics-ic1)**2 + (irs-ir0)**2 ))) !NE
          ds(it) = min(ds(it), sqrt( real((ics-ic0)**2 + (irs-ir1)**2 ))) !SW
          ds(it) = min(ds(it), sqrt( real((ics-ic1)**2 + (irs-ir1)**2 ))) !SE
        end do
        !
        call quicksort_d(ds, dsi, id)
        !
        dmin = huge(0)
!        do k = 1, nnmax
        do k = 1, id
          it = dsi(k)
          do i = 1, unp(it)%nbnd
            ict = unp(it)%bnd(1,i); irt = unp(it)%bnd(2,i)
            d = sqrt( real((ics-ict)**2 + (irs-irt)**2 ))
            if (d <= dmin) then
              itmin = it; imin = i; dmin = d
              ictmin = ict; irtmin = irt
              unp(is)%out_bndnn(1,j) = ict
              unp(is)%out_bndnn(2,j) = irt
              unp(is)%out_d(j)       = dmin
              unp(is)%out_itnn(j)    = itmin
            end if
          end do
        end do
      end do
    end do

    ! allocate for boundary map and fill
    do i = 1, id
      !unp(i)%ncol = unp(i)%ic1 - unp(i)%ic0 + 1
      !unp(i)%nrow = unp(i)%ir1 - unp(i)%ir0 + 1
      !unp(i)%ncol = max(1, unp(i)%ncol)
      !unp(i)%nrow = max(1, unp(i)%nrow)
      !
      allocate(unp(i)%bndmap(unp(i)%ncol, unp(i)%nrow))
      do ir = 1, unp(i)%nrow
        do ic = 1, unp(i)%ncol
          unp(i)%bndmap(ic,ir) = 0
        end do
      end do
      do j = 1, unp(i)%nbnd
        ic = unp(i)%bnd(1,j)
        ir = unp(i)%bnd(2,j)
        ic = ic - unp(i)%ic0 + 1
        ir = ir - unp(i)%ir0 + 1
        if (unp(i)%bndmap(ic,ir) /= 0) then
          write(*,*) 'Program error'
          stop 1
        end if
        unp(i)%bndmap(ic,ir) = j
      end do
    end do

    do iact = 1, 2
      m = 0
      do is = 1, id
        do j = 1, unp(is)%nbnd
          ic = unp(is)%bnd(1,j);       ir = unp(is)%bnd(2,j)
          jc = unp(is)%out_bndnn(1,j); jr = unp(is)%out_bndnn(2,j)
          it = unp(is)%out_itnn(j)
          !
          jc = jc - unp(it)%ic0 + 1; jr = jr - unp(it)%ir0 + 1
          if ((jc < 0).or.(jc > unp(it)%ncol)) then
            write(*,*) 'Program error, column out of range.'
            stop 1
          end if
          if ((jr < 0).or.(jr > unp(it)%nrow)) then
            write(*,*) 'Program error, row out of range.'
            stop 1
          end if
          k = unp(it)%bndmap(jc,jr)
          if (k < 0) then
            write(*,*) 'Program error, mapping out of range.'
            stop 1
          end if
          jc = unp(it)%out_bndnn(1,k); jr = unp(it)%out_bndnn(2,k)
          jt = unp(it)%out_itnn(k)
          !
          if ((is == jt).and.(ic == jc).and.(ir == jr)) then
            ! do nothing
          else
            unp(jt)%nin = unp(jt)%nin + 1
            if (iact == 2) then
              m = m + 1
              n = unp(jt)%nin
              unp(jt)%in_map(n) = k
              unp(jt)%in_srcp(n) = it
              unp(jt)%in_srci(n) = j
              unp(jt)%in_bndnn(1,n) = ic
              unp(jt)%in_bndnn(2,n) = ir
              unp(jt)%in_d(n) = unp(is)%out_d(j)
            end if
          end if
        end do
      end do
      if (iact == 1) then
        do i = 1, id
          n = max(1, unp(i)%nin)
          allocate(unp(i)%in_map(n))
          allocate(unp(i)%in_flag(n))
          allocate(unp(i)%in_srcp(n))
          allocate(unp(i)%in_srci(n))
          allocate(unp(i)%in_bndnn(2,n))
          allocate(unp(i)%in_d(n))
          unp(i)%nin = 0
          do j = 1, n
            unp(i)%in_flag(j) = 0
          end do
        end do
      end if
    end do
    !
    return
  end subroutine calc_unique_i

  subroutine calc_unique_r(p, ps, pu, unp, id, nbnd, xll, yll, cs, lbnd_in)
! ******************************************************************************
    ! -- arguments
    real, dimension(:,:), intent(in) :: p
    integer, intent(in) :: ps
    integer, dimension(:,:), allocatable, intent(inout) :: pu
    type(tUnp), dimension(:), allocatable, intent(inout) :: unp
    integer, intent(out) :: id
    integer, intent(out) :: nbnd
    real, intent(in) :: xll, yll, cs
    logical, intent(in), optional :: lbnd_in
    ! --- local
    integer, parameter ::jp = 1, jn = 2, js = 3, jw = 4, je = 5
    integer, parameter ::jnw = 6, jne = 7, jsw = 8, jse = 9
    integer, parameter :: nsten = jse
    integer, dimension(2,nsten) :: s1

    integer, parameter :: nnmax = 100
    integer :: nst, ic, ir, jc, jr, ncol, nrow, n1, n2, i, j, k, iact, n, m
    integer :: ics, irs, ict, irt, is, it, jt, ic0, ic1, ir0, ir1, itmin, imin
    integer :: ictmin, irtmin, nc, nr
    integer, dimension(:,:), allocatable :: lst1, lst2, wrk
    double precision, dimension(:), allocatable :: ds
    integer, dimension(:), allocatable :: dsi
    double precision :: d, dmin
    logical :: ldone, lbnd
! ------------------------------------------------------------------------------
    if (ps == 5) then
      nst = je
    else
      nst = nsten
    end if

    lbnd = .false.
    if (present(lbnd_in)) then
      lbnd = lbnd_in
    end if

    ncol = size(p,1); nrow = size(p,2)
    if (allocated(pu)) then
      deallocate(pu)
    end if
    allocate(pu(ncol,nrow))
    allocate(lst1(2,ncol*nrow), lst2(2,ncol*nrow), wrk(ncol,nrow))

    do ir = 1, nrow
      do ic = 1, ncol
        pu(ic,ir) = 0
        wrk(ic,ir) = 0
      end do
    end do

    write(*,*) 'Computing unique parts...'
    !write(*,*) 'Min/max=',minval(p), maxval(p)

    id = 0
    do ir = 1, nrow
      do ic = 1, ncol
        if ((int(p(ic,ir)) /= 0) .and. (pu(ic,ir) == 0)) then

          ! set stencil
          s1(1,jp) = ic;             s1(2,jp) = ir
          s1(1,jn) = ic;             s1(2,jn) = max(1,   ir-1)
          s1(1,js) = ic;             s1(2,js) = min(nrow,ir+1)
          s1(1,jw) = max(1,   ic-1); s1(2,jw) = ir
          s1(1,je) = min(ncol,ic+1); s1(2,je) = ir
          s1(1,jnw) = s1(1,jw); s1(2,jnw) = s1(2,jn)
          s1(1,jne) = s1(1,je); s1(2,jne) = s1(2,jn)
          s1(1,jsw) = s1(1,jw); s1(2,jsw) = s1(2,js)
          s1(1,jse) = s1(1,je); s1(2,jse) = s1(2,js)
          !
          id = id + 1

          n1 = 0
          jc = s1(1,1); jr = s1(2,1)
          pu(jc,jr) = id
          do i = 2, nst
            jc = s1(1,i); jr = s1(2,i)
            if ((abs(int(p(jc,jr))) > 0) .and. (pu(jc,jr) == 0)) then
              n1 = n1 + 1
              lst1(1,n1) = jc; lst1(2,n1) = jr
            end if
          end do

          ldone = .false.
          do while (.not.ldone)
            n2 = 0
            do i = 1, n1
              jc = lst1(1,i); jr = lst1(2,i)
              s1(1,jp) = jc;             s1(2,jp) = jr
              s1(1,jn) = jc;             s1(2,jn) = max(1,   jr-1)
              s1(1,js) = jc;             s1(2,js) = min(nrow,jr+1)
              s1(1,jw) = max(1,   jc-1); s1(2,jw) = jr
              s1(1,je) = min(ncol,jc+1); s1(2,je) = jr
              s1(1,jnw) = s1(1,jw); s1(2,jnw) = s1(2,jn)
              s1(1,jne) = s1(1,je); s1(2,jne) = s1(2,jn)
              s1(1,jsw) = s1(1,jw); s1(2,jsw) = s1(2,js)
              s1(1,jse) = s1(1,je); s1(2,jse) = s1(2,js)
              pu(jc,jr) = id
              do j = 2, nst
                jc = s1(1,j); jr = s1(2,j)
                if ((abs(p(jc,jr)) > 0) .and. (pu(jc,jr) == 0) .and. (wrk(jc,jr) == 0)) then
                  n2 = n2 + 1
                  lst2(1,n2) = jc; lst2(2,n2) = jr
                  wrk(jc,jr) = 1
                end if
              end do
            end do
            !
            if (n2 == 0) then
              !write(*,*) 'id =',id
              ldone = .true.
              exit
            end if
            !
            ! set list 1
            do i = 1, n1
              jc = lst1(1,i); jr = lst1(2,i)
              pu(jc,jr) = id
            end do
            !
            ! copy list, set work
            do i = 1, n2
              jc = lst2(1,i); jr = lst2(2,i)
              lst1(1,i) = jc; lst1(2,i) = jr
              wrk(jc,jr) = 0
            end do
            n1 = n2
          end do
        end if
      end do
    end do
    !
    write(*,*) '# unique parts found:',id
    !
    ! cleanup
    deallocate(lst1, lst2)
    !
    ! determine bounding box
    if (allocated(unp)) then
      deallocate(unp)
    end if
    allocate(unp(id))
    !
    do i = 1, id
      unp(i)%ic0 = ncol
      unp(i)%ic1 = 0
      unp(i)%ir0 = nrow
      unp(i)%ir1 = 0
    end do

    do ir = 1, nrow
      do ic = 1, ncol
        i = pu(ic,ir)
        if (i /= 0) then
          unp(i)%n = unp(i)%n + 1
          if (p(ic,ir) < 0) then
            pu(ic,ir) = -pu(ic,ir)
          end if
          unp(i)%ic0 = min(unp(i)%ic0, ic)
          unp(i)%ic1 = max(unp(i)%ic1, ic)
          unp(i)%ir0 = min(unp(i)%ir0, ir)
          unp(i)%ir1 = max(unp(i)%ir1, ir)
        end if
      end do
    end do
    !
    !n = 0
    !do ir = 1, nrow
    !  do ic = 1, ncol
    !    i = p(ic,ir)
    !    if (i /= 0) then
    !      n = n + 1
    !    end if
    !  end do
    !end do

    do i = 1, id
      unp(i)%ncol = unp(i)%ic1 - unp(i)%ic0 + 1
      unp(i)%nrow = unp(i)%ir1 - unp(i)%ir0 + 1
      unp(i)%ncol = max(1, unp(i)%ncol)
      unp(i)%nrow = max(1, unp(i)%nrow)
    end do
    !
    ! count the bounds
    do is = 1, id
      ir0 = unp(is)%ir0; ir1 = unp(is)%ir1
      ic0 = unp(is)%ic0; ic1 = unp(is)%ic1
      do ir = max(ir0-1,1), min(ir1+1,nrow)
        do ic = max(ic0-1,1), min(ic1+1,ncol)
          wrk(ic,ir) = 0
        end do
      end do
      do ir = ir0, ir1
        do ic = ic0, ic1
          j = pu(ic,ir)
          if ((j > 0).and.(is == j)) then
            ! set stencil
            s1(1,jp) = ic;             s1(2,jp) = ir
            s1(1,jn) = ic;             s1(2,jn) = max(1,   ir-1)
            s1(1,js) = ic;             s1(2,js) = min(nrow,ir+1)
            s1(1,jw) = max(1,   ic-1); s1(2,jw) = ir
            s1(1,je) = min(ncol,ic+1); s1(2,je) = ir
            s1(1,jnw) = s1(1,jw); s1(2,jnw) = s1(2,jn)
            s1(1,jne) = s1(1,je); s1(2,jne) = s1(2,jn)
            s1(1,jsw) = s1(1,jw); s1(2,jsw) = s1(2,js)
            s1(1,jse) = s1(1,je); s1(2,jse) = s1(2,js)
            do i = 2, nst
              jc = s1(1,i); jr = s1(2,i)
              if ((pu(jc,jr) == 0).and.(wrk(jc,jr) == 0)) then
                unp(is)%nbnd = unp(is)%nbnd + 1
                wrk(jc,jr) = 1
              end if
            end do
          end if
        end do
      end do
    end do
    !
    deallocate(wrk)

    !call writeidf('unp.idf',pu,ncol,nrow,xll,yll,cs,0.)
    !stop
    !
    ! set boundary
    nbnd = 0

    if (.not.lbnd) return

    do i = 1, id
      do iact = 1, 2
        unp(i)%nbnd = 0
        do ir = unp(i)%ir0, unp(i)%ir1
          do ic = unp(i)%ic0, unp(i)%ic1
            if (pu(ic,ir) == -i) then
              unp(i)%nbnd = unp(i)%nbnd + 1
              if (iact == 2) then
                j = unp(i)%nbnd
                unp(i)%bnd(1,j) = ic
                unp(i)%bnd(2,j) = ir
              end if
            end if
          end do
        end do
        if (iact == 1) then
          n = max(unp(i)%nbnd, 1)
          allocate(unp(i)%bnd(2,n))
          allocate(unp(i)%out_bndnn(2,n))
          allocate(unp(i)%out_itnn(n))
          allocate(unp(i)%out_d(n))
          do j = 1, n
            unp(i)%out_itnn(j) = 0
            unp(i)%out_d(j) = 0
          end do
          nbnd = nbnd + n
        end if
      end do !iact
    end do !i
    !
    ! all to all
    do is = 1, id
      allocate(unp(is)%all_bndnn(2,id))
      allocate(unp(is)%all_d(id))
      ics = unp(is)%bnd(1,1); irs = unp(is)%bnd(2,1)
      do it = 1, id
        ict = unp(it)%bnd(1,1); irt = unp(it)%bnd(2,1)
        unp(is)%all_bndnn(1,it) = ict
        unp(is)%all_bndnn(2,it) = irt
        unp(is)%all_d(it) = sqrt( real((ics-ict)**2 + (irs-irt)**2 )) !P
      end do
    end do

    allocate(ds(id), dsi(id))
    !
    do is = 1, id
      do j = 1, unp(is)%nbnd
        do i = 1, id
          ds(i) = huge(0)
          dsi(i) = i
        end do
        !
        ics = unp(is)%bnd(1,j); irs = unp(is)%bnd(2,j)
        !
        ! distances to bounding box of neighboring parts
        do it = 1, id
          if (it == is) then
            ds(is) = huge(0.d0)
            cycle
          end if
          ic0 = unp(it)%ic0; ic1 =  unp(it)%ic1
          ir0 = unp(it)%ir0; ir1 =  unp(it)%ir1
          nc = unp(it)%ncol; nr = unp(it)%nrow

          ds(it) = min(ds(it), sqrt( real((ics-(ic0+nc/2))**2 + (irs-(ir0+nr/2))**2 ))) !P
          ds(it) = min(ds(it), sqrt( real((ics-ic0)**2 + (irs-ir0)**2 ))) !NW
          ds(it) = min(ds(it), sqrt( real((ics-ic1)**2 + (irs-ir0)**2 ))) !NE
          ds(it) = min(ds(it), sqrt( real((ics-ic0)**2 + (irs-ir1)**2 ))) !SW
          ds(it) = min(ds(it), sqrt( real((ics-ic1)**2 + (irs-ir1)**2 ))) !SE
        end do
        !
        call quicksort_d(ds, dsi, id)
        !
        dmin = huge(0)
!        do k = 1, nnmax
        do k = 1, id
          it = dsi(k)
          do i = 1, unp(it)%nbnd
            ict = unp(it)%bnd(1,i); irt = unp(it)%bnd(2,i)
            d = sqrt( real((ics-ict)**2 + (irs-irt)**2 ))
            if (d <= dmin) then
              itmin = it; imin = i; dmin = d
              ictmin = ict; irtmin = irt
              unp(is)%out_bndnn(1,j) = ict
              unp(is)%out_bndnn(2,j) = irt
              unp(is)%out_d(j)       = dmin
              unp(is)%out_itnn(j)    = itmin
            end if
          end do
        end do
      end do
    end do

    ! allocate for boundary map and fill
    do i = 1, id
      !unp(i)%ncol = unp(i)%ic1 - unp(i)%ic0 + 1
      !unp(i)%nrow = unp(i)%ir1 - unp(i)%ir0 + 1
      !unp(i)%ncol = max(1, unp(i)%ncol)
      !unp(i)%nrow = max(1, unp(i)%nrow)
      !
      allocate(unp(i)%bndmap(unp(i)%ncol, unp(i)%nrow))
      do ir = 1, unp(i)%nrow
        do ic = 1, unp(i)%ncol
          unp(i)%bndmap(ic,ir) = 0
        end do
      end do
      do j = 1, unp(i)%nbnd
        ic = unp(i)%bnd(1,j)
        ir = unp(i)%bnd(2,j)
        ic = ic - unp(i)%ic0 + 1
        ir = ir - unp(i)%ir0 + 1
        if (unp(i)%bndmap(ic,ir) /= 0) then
          write(*,*) 'Program error'
          stop 1
        end if
        unp(i)%bndmap(ic,ir) = j
      end do
    end do

    do iact = 1, 2
      m = 0
      do is = 1, id
        do j = 1, unp(is)%nbnd
          ic = unp(is)%bnd(1,j);       ir = unp(is)%bnd(2,j)
          jc = unp(is)%out_bndnn(1,j); jr = unp(is)%out_bndnn(2,j)
          it = unp(is)%out_itnn(j)
          !
          jc = jc - unp(it)%ic0 + 1; jr = jr - unp(it)%ir0 + 1
          if ((jc < 0).or.(jc > unp(it)%ncol)) then
            write(*,*) 'Program error, column out of range.'
            stop 1
          end if
          if ((jr < 0).or.(jr > unp(it)%nrow)) then
            write(*,*) 'Program error, row out of range.'
            stop 1
          end if
          k = unp(it)%bndmap(jc,jr)
          if (k < 0) then
            write(*,*) 'Program error, mapping out of range.'
            stop 1
          end if
          jc = unp(it)%out_bndnn(1,k); jr = unp(it)%out_bndnn(2,k)
          jt = unp(it)%out_itnn(k)
          !
          if ((is == jt).and.(ic == jc).and.(ir == jr)) then
            ! do nothing
          else
            unp(jt)%nin = unp(jt)%nin + 1
            if (iact == 2) then
              m = m + 1
              n = unp(jt)%nin
              unp(jt)%in_map(n) = k
              unp(jt)%in_srcp(n) = it
              unp(jt)%in_srci(n) = j
              unp(jt)%in_bndnn(1,n) = ic
              unp(jt)%in_bndnn(2,n) = ir
              unp(jt)%in_d(n) = unp(is)%out_d(j)
            end if
          end if
        end do
      end do
      if (iact == 1) then
        do i = 1, id
          n = max(1, unp(i)%nin)
          allocate(unp(i)%in_map(n))
          allocate(unp(i)%in_flag(n))
          allocate(unp(i)%in_srcp(n))
          allocate(unp(i)%in_srci(n))
          allocate(unp(i)%in_bndnn(2,n))
          allocate(unp(i)%in_d(n))
          unp(i)%nin = 0
          do j = 1, n
            unp(i)%in_flag(j) = 0
          end do
        end do
      end if
    end do
    !
    return
  end subroutine calc_unique_r

  subroutine calc_unique_grid_r4(a, mv, id, min_id, max_id, bb_a, gnc)
! ******************************************************************************
    ! -- arguments
    real(r4b), dimension(:,:), intent(in) :: a
    real(r4b), intent(in) :: mv
    integer(i8b), dimension(:,:), allocatable, intent(out) :: id
    integer(i8b), intent(out) :: min_id
    integer(i8b), intent(out) :: max_id
    type(tBb), intent(in) :: bb_a
    integer(i4b), intent(in) :: gnc
    ! --- local
    logical :: lfnd, lfull
    !
    type(tUnp), dimension(:), allocatable :: unp
    type(tBb), dimension(:), allocatable :: bb
    integer(i4b) :: nc, nr, ir, ic, mxid, nid, i, j, i4v, mc, mr, jr, jc, i4dum
    integer(i4b) :: newid
    integer(i4b) :: n
    integer(i8b) :: uid
    !
    integer(i4b), dimension(:), allocatable :: i4wk1d, i4wk1d2, i4wk1d3
    integer(i4b), dimension(:,:), allocatable :: i4wk2d, pu
    !
    real(r4b) :: r4dum
! ------------------------------------------------------------------------------
    !
    nc = size(a,1); nr = size(a,2)
    !
    if (allocated(id)) deallocate(id)
    allocate(id(nc,nr))
    do ir = 1, nr
      do ic = 1, nc
        id(ic,ir) = 0
      end do
    end do
    !
    mxid = 0
    do ir = 1, nr
      do ic = 1, nc
        if (a(ic,ir) /= mv) then
          mxid = max(mxid,int(a(ic,ir),i4b))
        end if
      end do
    end do
    newid = mxid
    !
    allocate(i4wk1d(mxid))
    do i = 1, mxid
      i4wk1d(i) = 0
    end do
    do ir = 1, nr
      do ic = 1, nc
        if (a(ic,ir) /= mv) then
          i4v = int(a(ic,ir),i4b)
          i4wk1d(i4v) = 1
        end if
      end do
    end do
    !
    nid = 0
    do i = 1, mxid
      if (i4wk1d(i) == 1) then
        nid = nid + 1; i4wk1d(i) = nid
      end if
    end do
    !
    allocate(bb(nid), i4wk1d3(nid))
    do ir = 1, nr
      do ic = 1, nc
        if (a(ic,ir) /= mv) then
          i4v = int(a(ic,ir),i4b)
          i = i4wk1d(i4v)
          i4wk1d3(i) = i4v ! local id --> global id
          bb(i)%ir0 = min(ir,bb(i)%ir0); bb(i)%ir1 = max(ir,bb(i)%ir1)
          bb(i)%ic0 = min(ic,bb(i)%ic0); bb(i)%ic1 = max(ic,bb(i)%ic1)
        end if
      end do
    end do
    do i = 1, nid
      call logmsg('Processing '//ta((/i/))//'/'//ta((/nid/))//'...')
      bb(i)%ncol = bb(i)%ic1-bb(i)%ic0+1; bb(i)%nrow = bb(i)%ir1-bb(i)%ir0+1
      mc = bb(i)%ncol; mr = bb(i)%nrow
      allocate(i4wk2d(mc,mr))
      lfull = .true.
      do ir = bb(i)%ir0, bb(i)%ir1
        do ic = bb(i)%ic0, bb(i)%ic1
          jr = ir-bb(i)%ir0+1; jc = ic-bb(i)%ic0+1
          if (a(ic,ir) == i4wk1d3(i)) then
            i4wk2d(jc,jr) = 1
          else
            i4wk2d(jc,jr) = 0
            lfull = .false.
          end if
        end do
      end do
      !
      if (lfull) then
        jr = 1; jc = 1
        ir = jr+bb(i)%ir0-1; ic = jc+bb(i)%ic0-1 ! index a
        jr = ir+bb_a%ir0-1; jc = ic+bb_a%ic0-1 ! global extent
        !uid = int(jc,i8b)+ int((jr-1),i8b)*int(gnc,i8b)
        uid = int(i4wk1d3(i),i8b)
        do ir = bb(i)%ir0, bb(i)%ir1
          do ic = bb(i)%ic0, bb(i)%ic1
            if (a(ic,ir) == i4wk1d3(i)) then
              id(ic,ir) = uid
            end if
          end do
        end do
      else
        n = 0
        call calc_unique_i(i4wk2d, 9, pu, unp, n, i4dum, r4dum, r4dum, r4dum)
        jc = -1; jr = -1
        do j = 1, n ! loop over separate parts
          lfnd = .false.
          do ir = unp(j)%ir0, unp(j)%ir1
            do ic = unp(j)%ic0, unp(j)%ic1
              if (pu(ic,ir) == j) then
                jc = ic; jr = ir
                lfnd = .true.
              end if
              if (lfnd) exit
            end do
            if (lfnd) exit
          end do
          if ((jc < 0).or.(jr < 0)) then
            call errmsg('Program error')
          end if
          ir = jr+bb(i)%ir0-1; ic = jc+bb(i)%ic0-1 ! index a
          jr = ir+bb_a%ir0-1; jc = ic+bb_a%ic0-1 ! global extent
          !uid = int(jc,i8b)+ int((jr-1),i8b)*int(gnc,i8b)
          if (j > 1) then
            newid = newid + 1
            uid = int(newid,i8b)
          else
            uid = int(i4wk1d3(i),i8b)
          end if
          !
          do ir = unp(j)%ir0, unp(j)%ir1
            do ic = unp(j)%ic0, unp(j)%ic1
              if (pu(ic,ir) == j) then
                jr = ir+bb(i)%ir0-1; jc = ic+bb(i)%ic0-1
                id(jc,jr) = uid
              end if
            end do
          end do
        end do
      end if
      deallocate(i4wk2d)
    end do
    !
    min_id = huge(min_id); max_id = -huge(max_id)
    do ir = 1, nr
      do ic = 1, nc
        if (id(ic,ir) /= 0) then
          min_id = min(min_id,id(ic,ir))
          max_id = max(max_id,id(ic,ir))
        end if
      end do
    end do
    !
    call logmsg('-->'//ta((/(max_id-min_id)/1000000/))//' M')
    ! 
    return
  end subroutine calc_unique_grid_r4
  
  subroutine getidmap(wrk, ir0, ir1, ic0, ic1, maxid, idmap, ncat, idmapinv, catarea, idbb)
! ******************************************************************************
    ! -- arguments
    integer, dimension(:,:), intent(in) :: wrk
    integer, intent(out) :: maxid
    integer, intent(in) :: ir0, ir1, ic0, ic1
    integer, dimension(:), allocatable, intent(inout) :: idmap
    integer, intent(out) :: ncat
    integer, dimension(:), allocatable, intent(inout) :: idmapinv
    double precision, dimension(:), allocatable, intent(inout) :: catarea
    integer, dimension(:,:), allocatable, intent(inout) :: idbb
    ! --- local
    integer :: nc, nr, ic, ir, j, k, jc, jr, id
! ------------------------------------------------------------------------------
    nc = size(wrk,1); nr = size(wrk,2)
    !
    ! determine the mappings from/to the catchment IDs
    maxid = maxval(wrk)
    if (allocated(idmap)) deallocate(idmap)
    allocate(idmap(maxid))
    do j = 1, maxid
      idmap(j) = 0
    end do
    do ir = 1, nr
      do ic = 1, nc
        id = abs(wrk(ic,ir))
        if (id /= 0) then
          idmap(id) = idmap(id) + 1
        end if
      end do
    end do
    ncat = 0
    do j = 1, maxid
      if (idmap(j) > 0) then
        ncat = ncat + 1
      end if
    end do
    if (allocated(catarea)) deallocate(catarea)
    allocate(catarea(ncat))
    if (allocated(idmapinv)) deallocate(idmapinv)
    allocate(idmapinv(ncat))
    ncat = 0
    do j = 1, maxid
      if (idmap(j) > 0) then
        ncat = ncat + 1
        idmapinv(ncat) = j
        catarea(ncat) = idmap(j)
        idmap(j) = ncat
      end if
    end do
    !
    ! determine bounding box
    if (allocated(idbb)) deallocate(idbb)
    allocate(idbb(4,ncat))
    do j = 1, ncat
      idbb(1,j) = nr !ir0
      idbb(2,j) = 0  !ir1
      idbb(3,j) = nc !ic0
      idbb(4,j) = 0  !ic1
    end do
    !
    do ir = ir0, ir1
      do ic = ic0, ic1
        jr = ir-ir0+1; jc = ic-ic0+1
        j = abs(wrk(jc,jr))
        if (j /= 0) then
          k = idmap(j)
          idbb(1,k) = min(idbb(1,k), ir)
          idbb(2,k) = max(idbb(2,k), ir)
          idbb(3,k) = min(idbb(3,k), ic)
          idbb(4,k) = max(idbb(4,k), ic)
        end if
      end do
    end do
    !
    return
  end subroutine getidmap

  !###====================================================================
  function change_case(str, opt) result (string)
  !###====================================================================
    character(*), intent(in) :: str
    character(len=1) :: opt
    character(len(str))      :: string

    integer :: ic, i

    character(26), parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(26), parameter :: low = 'abcdefghijklmnopqrstuvwxyz'

  !   Capitalize each letter if it is lowecase
    string = str
    if ((opt == 'U') .or. (opt == 'u')) then
      do i = 1, LEN_TRIM(str)
        ic = INDEX(low, str(i:i))
        if (ic > 0) string(i:i) = cap(ic:ic)
      end do
    end if
    if ((opt == 'L') .or. (opt == 'l')) then
      do i = 1, LEN_TRIM(str)
        ic = INDEX(cap, str(i:i))
        if (ic > 0) string(i:i) = low(ic:ic)
      end do
    end if
    !
    return
  end function change_case

  function count_i1a(a, v) result(n)
! ******************************************************************************
    ! -- arguments
    integer(i1b), dimension(:), intent(in) :: a
    integer(i1b), intent(in) :: v 
    integer(i4b) :: n
    ! -- locals
    integer(i4b) :: i
! ------------------------------------------------------------------------------
    n = 0
    do i = 1, size(a)
      if (a(i) == v) n = n + 1
    end do
    !
    return
  end function count_i1a
  
  subroutine fillgap_r4(x, nodata, xtgt)
! ******************************************************************************
    ! -- arguments
    real(r4b), dimension(:,:), intent(inout) :: x
    real(r4b), intent(in) :: nodata
    real(r4b), intent(in) :: xtgt
    ! -- locals
    integer(i4b), parameter :: maxiter = 1000
    !
    integer(i4b), parameter :: i_e = 1
    integer(i4b), parameter :: i_w = 2
    integer(i4b), parameter :: i_s = 3
    integer(i4b), parameter :: i_n = 4
    integer(i4b), parameter :: i_sw = 5
    integer(i4b), parameter :: i_se = 6
    integer(i4b), parameter :: i_nw = 7
    integer(i4b), parameter :: i_ne = 8
    integer(i4b), parameter :: nsten = i_ne
    integer(i4b), dimension(2,nsten) :: sicir
    logical, dimension(nsten) :: lsten
    !
    logical :: tgt_flag
    integer(i1b), dimension(:,:), allocatable :: i1wrk
    integer(i4b) :: nc, nr, ic, ir, n, m, ic0, ic1, ir0, ir1, nbr, i, j, maxcnt
    integer(i4b) :: ntgt, iter, nnodata, jc, jr
    integer(i4b) :: bbic0, bbic1, bbir0, bbir1, bbjc0, bbjc1, bbjr0, bbjr1
    integer(i4b), dimension(:,:), allocatable :: i4wrk
    integer(i4b), dimension(8) :: i4idx, ucnt
    real(r4b), dimension(:), allocatable :: r4wrk
    real(r4b), dimension(8) :: r4nbr, r4ucnt
    real(r4b) :: r4huge, rval, rvalp, rval_min, rval_max
    real(r4b), parameter :: my_nodata  = -12345.
! ------------------------------------------------------------------------------
    r4huge = huge(r4huge)
    !
    nc = size(x,1); nr = size(x,2)
    allocate(i1wrk(nc,nr),i4wrk(2,nc*nr), r4wrk(nc*nr))
    bbir0 = 1; bbir1 = nr; bbic0 = 1; bbic1 = nc
    !
    iter = 0; n = 1
    do while(.true.)
      iter = iter + 1
      !
      do ir = 1, nr
        do ic = 1, nc
          i1wrk(ic,ir) = 0
        end do
      end do
      !
      if (iter == 1) then
        rvalp = xtgt
       else
        rvalp = my_nodata
      end if
      !
      n = 0
      do ir = bbir0, bbir1
        do ic = bbic0, bbic1
          if (x(ic,ir) == rvalp) then
            ic0 = ic - 1; ic1 = ic + 1
            ir0 = ir - 1; ir1 = ir + 1
            nbr = 0; lsten = .false.
            !
            if (ic1 <= nc) then ! E
              jc = ic1; jr = ir; rval = x(jc,jr); sicir(1,i_e) = jc; sicir(2,i_e) = jr; lsten(i_e) = .true.
              if ((rval /= xtgt).and.(rval /= nodata).and.(rval /= my_nodata)) then
                nbr = nbr + 1; r4nbr(nbr) = rval
              end if
              if (ir1 <= nr) then !S
                jc = ic1; jr = ir1; rval = x(jc,jr); sicir(1,i_se) = jc; sicir(2,i_se) = jr; lsten(i_se) = .true.
                if ((rval /= xtgt).and.(rval /= nodata).and.(rval /= my_nodata)) then
                  nbr = nbr + 1; r4nbr(nbr) = rval
                end if
              end if
              if (ir0 >= 1) then !N
                jc = ic1; jr = ir0; rval = x(jc,jr); sicir(1,i_ne) = jc; sicir(2,i_ne) = jr; lsten(i_ne) = .true.
                if ((rval /= xtgt).and.(rval /= nodata).and.(rval /= my_nodata)) then
                  nbr = nbr + 1; r4nbr(nbr) = rval
                end if
              end if
            end if
            if (ic0 >= 1) then ! W
              jc = ic0; jr = ir; rval = x(jc,jr); sicir(1,i_w) = jc; sicir(2,i_w) = jr; lsten(i_w) = .true.
              if ((rval /= xtgt).and.(rval /= nodata).and.(rval /= my_nodata)) then
                nbr = nbr + 1; r4nbr(nbr) = rval
              end if
              if (ir1 <= nr) then !S
                jc = ic0; jr = ir1; rval = x(jc,jr); sicir(1,i_sw) = jc; sicir(2,i_sw) = jr; lsten(i_sw) = .true.
                if ((rval /= xtgt).and.(rval /= nodata).and.(rval /= my_nodata)) then
                  nbr = nbr + 1; r4nbr(nbr) = rval
                end if
              end if
              if (ir0 >= 1) then !N
                jc = ic0; jr = ir0; rval = x(jc,jr); sicir(1,i_nw) = jc; sicir(2,i_nw) = jr; lsten(i_nw) = .true.
                if ((rval /= xtgt).and.(rval /= nodata).and.(rval /= my_nodata)) then
                  nbr = nbr + 1; r4nbr(nbr) = rval
                end if
              end if
            end if
            if (ir1 <= nr) then !S
              jc = ic; jr = ir1; rval = x(jc,jr); sicir(1,i_s) = jc; sicir(2,i_s) = jr; lsten(i_s) = .true.
              if ((rval /= xtgt).and.(rval /= nodata).and.(rval /= my_nodata)) then
                nbr = nbr + 1; r4nbr(nbr) = rval
              end if
            end if
            if (ir0 >= 1) then !N
              jc = ic; jr = ir0; rval = x(jc,jr); sicir(1,i_n) = jc; sicir(2,i_n) = jr; lsten(i_n) = .true.
              rval = x(ic,ir0)
              if ((rval /= xtgt).and.(rval /= nodata).and.(rval /= my_nodata)) then
                nbr = nbr + 1; r4nbr(nbr) = rval
              end if
            end if
            !
            ! neighbors found
            if (nbr > 0) then
              do i = 1, nsten
                if (lsten(i)) then
                  jc = sicir(1,i); jr = sicir(2,i)
                  if (x(jc,jr) == xtgt) then
                    i1wrk(jc,jr) = 1
                  end if
                end if
              end do
              !
              n = n + 1
              i4wrk(1,n) = ic; i4wrk(2,n) = ir
              if (nbr <= 2) then
                r4wrk(n) = r4nbr(1)
              else
                ! check if all are the same
                rval_min = r4huge; rval_max = -r4huge
                do i = 1, nbr
                  rval_min = min(rval_min,r4nbr(i))
                  rval_max = max(rval_max,r4nbr(i))
                end do
                if (rval_min == rval_max) then
                  r4wrk(n) = rval_min
                else
                  do i = 1, nbr
                    i4idx(i) = i
                  end do
                  call quicksort_r(r4nbr, i4idx, nbr)
                  !
                  ucnt = 0; r4ucnt(1) = r4nbr(1); m = 1
                  do i = 1, nbr
                    if (r4nbr(i) /= r4ucnt(m)) then
                      m = m + 1
                      r4ucnt(m) = r4nbr(i)
                    end if
                    ucnt(m) = ucnt(m) + 1
                  end do
                  if (m == 1) then
                    r4wrk(n) = r4ucnt(1)
                  else
                    maxcnt = 0
                    do i = 1, m
                      maxcnt = max(maxcnt,ucnt(i))
                    end do
                    do i = 1, m
                      if (ucnt(i) == maxcnt) then
                        r4wrk(n) = r4ucnt(i)
                        exit
                      end if
                    end do
                  end if
                end if
              end if
            end if
          end if
        end do
      end do
      !
      ! set the target value
      !call logmsg('# cells labeled: '//ta((/100.*n/(6000*6000)/)))
      do i = 1, n
        ic = i4wrk(1,i); ir = i4wrk(2,i)
        x(ic,ir) = r4wrk(i)
        i1wrk(ic,ir) = 0
      end do
      !
      bbjr0 = nr+1; bbjr1 = 0; bbjc0 = nc+1; bbjc1 = 0
      do ir = 1, nr
        do ic = 1, nc
          if (i1wrk(ic,ir) == 1) then
            x(ic,ir) = my_nodata
            bbjr0 = min(bbjr0,ir); bbjr1 = max(bbjr1,ir)
            bbjc0 = min(bbjc0,ic); bbjc1 = max(bbjc1,ic)
          end if
        end do
      end do
      !
      ! set loop bounding box
      bbir0 = max(bbjr0-1,1); bbir1 = min(bbjr1+1,nr)
      bbic0 = max(bbjc0-1,1); bbic1 = min(bbjc1+1,nc)
      !
      ! count the remaining target values
      ntgt = 0; nnodata = 0
      do ir = 1, nr
        do ic = 1, nc
          if (x(ic,ir) == xtgt) then
            ntgt = ntgt + 1
          end if
          if (x(ic,ir) == my_nodata) then
            nnodata = nnodata + 1
          end if
        end do
      end do
      call logmsg('Iteration '//ta((/iter/))//'; # added: '//ta((/n/))//'; # remaining: '//ta((/ntgt,nnodata/)))
      ntgt = ntgt + nnodata
      !
      !if (iter == 1) exit
      if (iter == maxiter) then
        call errmsg('fillgap_r4: maximum iterations of '//ta((/iter/))//' reached.')
        exit
      end if
      if (n == 0) exit
    end do
    call logmsg('Total iterations: '//ta((/iter/))//'; # not filled: '//ta((/ntgt/)))
    !
    deallocate(i1wrk,i4wrk,r4wrk)
    return
  end subroutine fillgap_r4
  
  subroutine fill_with_nearest_r4(x, nodata, xtgt)
! ******************************************************************************
    ! -- arguments
    real(r4b), dimension(:,:), intent(inout) :: x
    real(r4b), intent(in) :: nodata
    real(r4b), intent(in) :: xtgt
    ! -- locals
    logical :: lfound
    integer(i4b), dimension(:), allocatable :: cnt
    integer(i4b), dimension(:,:), allocatable :: icir
    integer(i4b), dimension(1) :: mloc
    integer(i4b) :: n, m, nc, nr, mc, mr, ic, ir, jc, jr, kc, kr, ic0, ic1, ir0, ir1, &
      jc0, jc1, jr0, jr1,  ntgt, iact, i, j, k, nb, id0, id1, id
    real(r4b) :: r4v, r4vmin, r4vmax
    real(r4b), dimension(:), allocatable :: r4vi, r4vb
! ------------------------------------------------------------------------------
    !
    nc = size(x,1); nr = size(x,2)
    allocate(r4vb(2*nc + 2*nr))
    !
    ! store the location to intepolate
    do iact = 1, 2
      ntgt = 0
      do ir = 1, nr
        do ic = 1, nc
          r4v = x(ic,ir)
          if (r4v /= nodata) then
            if (r4v == xtgt) then
              ntgt = ntgt + 1
              if (iact == 2) then
                icir(1,ntgt) = ic
                icir(2,ntgt) = ir
              end if
            end if
          end if
        end do
      end do
      if (iact == 1) then
        if (ntgt > 0) then
          allocate(icir(2,ntgt), r4vi(ntgt))
          do i = 1, ntgt
            r4vi(i) = nodata
          end do
        end if
      end if
    end do
    !
    if (ntgt == 0) then
      return
    else
      call logmsg('# interpolation cells: '//ta((/ntgt/)))
    end if
    !
    do i = 1, ntgt
      jc = icir(1,i); jr = icir(2,i)
      !
      lfound = .false.; n = 0
      do while(.not.lfound)
        n = n + 1
        ir0 = jr - n; ir1 = jr + n; ic0 = jc - n; ic1 = jc + n
        ir0 = max(1,ir0); ir1 = min(nr,ir1); ic0 = max(1,ic0); ic1 = min(nc,ic1); 
        nb = 0; r4vmin = huge(r4vmin); r4vmax = -huge(r4vmax)
        !
        do j = 1, 4
          select case(j)
          case(1) !N
            jr0 = ir0; jr1 = ir0; jc0 = ic0; jc1 = ic1
          case(2) !S
            jr0 = ir1; jr1 = ir1; jc0 = ic0; jc1 = ic1
          case(3) !W
            jr0 = ir0 + 1; jr1 = ir1 - 1; jc0 = ic0; jc1 = ic0
          case(4) !E
            jr0 = ir0 + 1; jr1 = ir1 - 1; jc0 = ic1; jc1 = ic1
           end select
          !
          mr = ir1 - ir0 + 1; mc = ic1 - ic0 + 1
          if (.not.lfound) then
            do ir = jr0, jr0
              do ic = jc0, jc1
                r4v = x(ic,ir)
                if ((r4v /= nodata).and.(r4v /= xtgt)) then
                  lfound = .true.
                  nb = nb + 1
                  r4vb(nb) = r4v
                  r4vmin = min(r4vmin, r4v); r4vmax = max(r4vmax, r4v)
                end if
              end do
            end do
          end if
        end do
        !
        if (lfound) then
          if (nb == 1) then
            r4vi(i) = r4vb(1)
          else
            if (r4vmin == r4vmax) then
              r4vi(i) = r4vb(1)
            else
              id0 = int(r4vmin,i4b); id1 = int(r4vmax,i4b)
              m = id1 - id0 + 1
              allocate(cnt(m))
              do k = 1, m
                cnt(k) = 0
              end do
              do k = 1, nb
                id = int(r4vb(k),i4b) - id0 + 1
                cnt(id) = cnt(id) + 1
              end do
              mloc = maxloc(cnt); id = mloc(1) + id0 - 1
              r4vi(i) = real(id,r4b)
              deallocate(cnt)
            end if
          end if
        end if
      end do
    end do
    !
    do i = 1, ntgt
      jc = icir(1,i); jr = icir(2,i)
      r4v = r4vi(i)
      if (r4v == nodata) then
        call errmsg('Invalid interpolated value')
      end if
      x(jc,jr) = r4v
    end do
    !
    if (allocated(icir)) deallocate(icir)
    if (allocated(r4vi)) deallocate(r4vi)
    !
    return
  end subroutine fill_with_nearest_r4
    
  subroutine fseek_stream_loc(iu, offset, whence, status)
! ******************************************************************************
! Move the file pointer.  Patterned after fseek, which is not 
! supported as part of the fortran standard.  For this subroutine to work
! the file must have been opened with access='stream' and action='readwrite'.
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    integer(I4B), intent(in) :: iu
    integer(I8B), intent(in) :: offset !BINPOS
    integer(I4B), intent(in) :: whence
    integer(I4B), intent(inout) :: status
    integer(I8B) :: ipos !BINPOS
! ------------------------------------------------------------------------------
    !
    inquire(unit=iu, size=ipos)
    
    select case(whence)
    case(0)
      !
      ! -- whence = 0, offset is relative to start of file
      ipos = 0 + offset
    case(1)
      !
      ! -- whence = 1, offset is relative to current pointer position
      inquire(unit=iu, pos=ipos)
      ipos = ipos + offset
    case(2)
      !
      ! -- whence = 2, offset is relative to end of file
      inquire(unit=iu, size=ipos)
      ipos = ipos + offset
    end select
    !
    ! -- position the file pointer to ipos
    write(iu, pos=ipos, iostat=status)
    inquire(unit=iu, pos=ipos)
    !
    ! -- return
    return
  end subroutine fseek_stream_loc
  
end module utilsmod
