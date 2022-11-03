module pcrModule
  ! modules
  use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, &
    i1b => int8, i2b => int16, i4b => int32, i8b => int64, r4b => real32, r8b => real64
  use ieee_arithmetic 
  use utilsmod
  
  implicit none
  
  private
  public :: i1b, i2b, i4b, i8b, r4b, r8b
  
  integer(i1b), parameter :: jsw = 1
  integer(i1b), parameter :: js  = 2
  integer(i1b), parameter :: jse = 3
  integer(i1b), parameter :: jw  = 4
  integer(i1b), parameter :: jp  = 5
  integer(i1b), parameter :: je  = 6
  integer(i1b), parameter :: jnw = 7
  integer(i1b), parameter :: jn  = 8
  integer(i1b), parameter :: jne = 9
  integer(i4b), dimension(2,9), parameter :: st = (/ -1,  1, 0,  1, 1,  1, &
                                                     -1,  0, 0,  0, 1,  0, &
                                                     -1, -1, 0, -1, 1, -1 /)
  integer(i4b), dimension(9) :: jperm = (/ 9, 8, 7, 6, 5, 4, 3, 2, 1 /)
  public :: jsw, js, jse, jw, jp, je, jnw, jn, jne, st, jperm 
  
  integer, parameter :: cr_int1  = 1 ! 4
  integer, parameter :: cr_int2  = 2 ! 21
  integer, parameter :: cr_int4  = 3 ! 38
  integer, parameter :: ncrint = cr_int4
  integer, parameter :: cr_uint1 = 4 ! 0
  integer, parameter :: cr_uint2 = 5 ! 17
  integer, parameter :: cr_uint4 = 6 ! 34
  integer, parameter :: cr_real4 = 7 ! 90
  integer, parameter :: cr_real8 = 8 ! 219
  integer, parameter :: cr_undef = 9 ! 100
  integer, parameter :: ncr = cr_undef
  
  integer, dimension(ncr) :: crval
  data crval/4,21,38,0,17,34,90,219,100/
  
  character(len=5), dimension(ncr) :: crstr
  data crstr/'int1 ','int2 ','int4 ','uint1','uint2','uint4','real4','real8','undef'/
  
  integer(kind=4), dimension(ncrint) :: crintmv
  data crintmv/-2147483648,-256,-32768/
  
  integer, parameter :: pt_xy     = 1
  integer, parameter :: pt_utm    = 2
  integer, parameter :: pt_latlon = 3
  integer, parameter :: pt_cart   = 4
  integer, parameter :: pt_rdm    = 5
  integer, parameter :: v1npt       = pt_rdm
  
  integer, dimension(v1npt) :: v1ptval
  data v1ptval/0,1,2,3,4/
  
  character(len=4), dimension(v1npt) :: v1ptstr
  data v1ptstr/'xy    ','utm   ','latlon','cart  ','rdm   '/
  
  integer, parameter :: pt_yinct2b = 1
  integer, parameter :: pt_ydect2b = 2
  integer, parameter :: v2npt      = pt_ydect2b
  
  integer, dimension(v2npt) :: v2ptval
  data v2ptval/0,1/
  
  integer, parameter :: vs_boolean   = 1
  integer, parameter :: vs_nominal   = 2
  integer, parameter :: vs_ordinal   = 3
  integer, parameter :: vs_scalar    = 4
  integer, parameter :: vs_direction = 5
  integer, parameter :: vs_ldd       = 6
  integer, parameter :: vs_vector    = 7
  integer, parameter :: nvs = vs_vector
  
  integer, dimension(nvs) :: vsval
  data vsval/224,226,242,235,251,240,236/
  
  character(len=13), dimension(v2npt) :: v2ptstr
  data v2ptstr/'y increasing ','y decreasing'/

  type tMapHdr
    ! main header
    character(len=32) :: signature
!    character(len=27) :: signature
    integer(kind=2)   :: version
    integer(kind=4)   :: gisFileId
    integer(kind=2)   :: projection
    integer(kind=4)   :: attrTable
    integer(kind=2)   :: dataType
    integer(kind=4)   :: byteOrder
    ! raster header
    integer(kind=2)  :: valueScale
    integer(kind=2)  :: cellRepr
    real(kind=8)     :: xUL
    real(kind=8)     :: yUL
    integer(kind=4)  :: nrRows
    integer(kind=4)  :: nrCols
    real(kind=8)     :: cellSizeX
    real(kind=8)     :: cellSizeY
    real(kind=8)     :: angle
    !
    integer(kind=8) :: i4minVal, i4maxVal
    real(kind=8)     :: r4minVal, r4maxVal
  end type tMapHdr
  
  type tMap
    character(len=mxslen), pointer        :: f  => null()
    integer, pointer                      :: iu => null()
    type(tMapHdr), pointer                :: header => null()
    integer(i1b),                 pointer :: i1mv  => null()
    integer(i1b), dimension(:,:), pointer :: i1a   => null()
    integer(i2b),                 pointer :: i2mv  => null()
    integer(i2b), dimension(:,:), pointer :: i2a   => null()
    integer(i4b),                 pointer :: i4mv  => null()
    integer(i4b), dimension(:,:), pointer :: i4a   => null()
    real(r4b),                    pointer :: r4mv  => null()
    real(r4b),    dimension(:,:), pointer :: r4a   => null()
    real(r8b),                    pointer :: r8mv  => null()
    real(r8b),    dimension(:,:), pointer :: r8a   => null()
    !
    integer(i4b), pointer :: gir0  => null() !global
    integer(i4b), pointer :: gir1  => null() !global
    integer(i4b), pointer :: gic0  => null() !global
    integer(i4b), pointer :: gic1  => null() !global
  contains
    procedure :: init         => map_init
    procedure :: read_header  => map_read_full_header
    procedure :: init_light         => map_init_light
    procedure :: read_light_header  => map_read_light_header
    procedure :: set_bb       => map_set_bounding_box
    procedure :: read_data    => map_read_data
    procedure :: write_header => map_write_header
    procedure :: write_data   => map_write_data
    procedure :: set_nodata   => map_set_nodata
    procedure :: correct_nodata  => map_correct_nodata
    procedure :: close        => map_close
    procedure :: clean        => map_clean
    procedure :: clean_data   => map_clean_data
    procedure :: idf_export   => map_idf_export
    procedure :: get_i1ar     => map_get_i1ar
    procedure :: get_r4ar     => map_get_r4ar
    generic   :: get_val      => map_geti1val, map_geti4val, map_getr4val, map_getr8val
    procedure :: map_geti1val
    procedure :: map_geti4val
    procedure :: map_getr4val
    procedure :: map_getr8val
    generic   :: read_block   => map_readblock_i1, map_readblock_i4, map_readblock_r4
    procedure :: map_readblock_i1
    procedure :: map_readblock_i4
    procedure :: map_readblock_r4
  end type tMap
  public :: tMap
  
contains
  
  function is_map_file(fname) result(ok)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    character(len=*), intent(in) :: fname
    logical :: ok
    ! -- local
    integer :: ios, iu
    character(len=27) :: signature
! ------------------------------------------------------------------------------
    
    ok = .false.
    
     ! open file in stream mode
    iu=getlun()
    open(unit=iu,file=fname,form='unformatted',access='stream',status='old',iostat=ios)
    if (ios.ne.0) return
    
    ! READ HEADER
    
    ! main header
    read(iu,pos=  0+1, iostat=ios) signature
    if (ios /= 0) then
       close(iu)
       return
    end if    
    if (signature == 'RUU CROSS SYSTEM MAP FORMAT') ok = .true.
    close(iu)
    !
    return
  end function is_map_file

  function map_init(this, f, verb_in) result(ok)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    character(len=*), intent(in) :: f
    integer(i4b), intent(in), optional :: verb_in
    !
    logical :: ok
    ! -- local
    integer(i4b) :: verb
    logical :: lok
! ------------------------------------------------------------------------------
    if (present(verb_in)) then
      verb = verb_in
    else
      verb = 0
    end if
    !
    lok = this%read_header(f, verb)
    !
    allocate(this%gic0, this%gic1, this%gir0, this%gir1)
    !
    this%gic0 = IZERO
    this%gic1 = IZERO
    this%gir0 = IZERO
    this%gir1 = IZERO
    !
    ! set return value
    ok = .true.
    !
    return
  end function map_init
  

  function map_init_light(this, f, verb_in) result(ok)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    character(len=*), intent(in) :: f
    integer(i4b), intent(in), optional :: verb_in
    !
    logical :: ok
    ! -- local
    integer(i4b) :: verb
    logical :: lok
! ------------------------------------------------------------------------------
    if (present(verb_in)) then
      verb = verb_in
    else
      verb = 0
    end if
    !
    lok = this%read_light_header(f, verb)
    !
    allocate(this%gic0, this%gic1, this%gir0, this%gir1)
    !
    this%gic0 = IZERO
    this%gic1 = IZERO
    this%gir0 = IZERO
    this%gir1 = IZERO
    !
    ! set return value
    ok = .true.
    !
    return
  end function map_init_light
  
  function map_set_bounding_box(this, gic0, gic1, gir0, gir1, &
    gxll, gyll, gnrow, gncol) result(ok)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    integer(i4b), intent(in) :: gir0
    integer(i4b), intent(in) :: gir1
    integer(i4b), intent(in) :: gic0
    integer(i4b), intent(in) :: gic1
    real(r8b), optional, intent(in) :: gxll
    real(r8b), optional, intent(in) :: gyll
    integer(i4b), optional, intent(in) :: gncol
    integer(i4b), optional, intent(in) :: gnrow
    !
    logical :: ok
    ! -- local
    integer(i4b) :: ncol, nrow
    type(tMapHdr), pointer :: hdr
! ------------------------------------------------------------------------------
    hdr => this%header
    !
    this%gic0 = gic0
    this%gic1 = gic1
    this%gir0 = gir0
    this%gir1 = gir1
    !
    ! first check
    ncol = gic1-gic0+1
    nrow = gir1-gir0+1
    if (ncol /= hdr%NrCols) then
      call errmsg('Inconsistent number of bb-columns for MAP-file: '//trim(this%f))
    end if
    if (nrow /= hdr%NrRows) then
      call errmsg('Inconsistent number of bb-rows for MAP-file: '//trim(this%f))
    end if
    
    ! check
    if (present(gxll).and.present(gyll).and.present(gnrow).and.present(gncol)) then
       write(*,*) 'Checking...'
    end if
    
    ! set return value
    ok = .true.
    !
    return
  end function map_set_bounding_box
  
  function map_read_full_header(this, f, verb) result(ok)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    character(len=*), intent(in) :: f
    integer(i4b), intent(in) :: verb
    logical :: ok
    ! -- local
    integer :: i, iinc
    real, parameter :: nodata = -9999. ! should be exactly the same as in rdrsmodule !
    type(tMapHdr), pointer :: hdr
! ------------------------------------------------------------------------------
    call chkexist(f)
    allocate(this%f)
    this%f = f
    
    ! open file in stream mode
    if (.not.associated(this%iu)) allocate(this%iu)
    this%iu=getlun()
    if (verb == 0) write(*,*) 'Reading '//trim(f)//'...'
    open(unit=this%iu,file=f,form='unformatted',access='stream',status='old')
    
    ! READ HEADER
    allocate(this%header)
    hdr => this%header
    
    ! main header
    read(this%iu,pos=  0+1) hdr%signature
    if (hdr%signature(1:27).ne.'RUU CROSS SYSTEM MAP FORMAT')then
       call errmsg('File not recognized as MAP-file: ')
    end if   
    read(this%iu,pos= 32+1) hdr%version
    read(this%iu,pos= 34+1) hdr%gisFileId
    read(this%iu,pos= 38+1) hdr%projection
    iinc = 1
    if (hdr%version.eq.1) then
      do i = 1, v1npt
         if(v1ptval(i).eq.hdr%projection)then
            !write(*,*) 'Version 1 projection: ',trim(v1ptstr(i))
         end if  
      end do   
    else
      do i = 1, v2npt
         if(v2ptval(i).eq.hdr%projection)then
            !write(*,*) 'Version 2 projection: ',trim(v2ptstr(i))
         end if  
      end do   
      if (hdr%projection.eq.v2ptval(pt_yinct2b)) iinc = -1
    end if    
    read(this%iu,pos= 40+1) hdr%attrTable
    read(this%iu,pos= 44+1) hdr%dataType
    read(this%iu,pos= 46+1) hdr%byteOrder
    ! raster header
    read(this%iu,pos= 64+1) hdr%valueScale
    read(this%iu,pos= 66+1) hdr%cellRepr
    do i = 1, ncr
       if(crval(i).eq.hdr%cellRepr)then
          !write(*,*) 'Cell representation: ',trim(crstr(i))
          hdr%cellRepr=i
          exit
       end if   
    end do
    select case(hdr%cellRepr)
    case(cr_int4)
       read(this%iu,pos= 68+1) hdr%i4minVal
       read(this%iu,pos= 76+1) hdr%i4maxVal
    case(cr_real4)
       read(this%iu,pos= 68+1) hdr%r4minVal
       read(this%iu,pos= 76+1) hdr%r4maxVal
    end select
    read(this%iu,pos= 84+1) hdr%xUL
    read(this%iu,pos= 92+1) hdr%yUL
    read(this%iu,pos=100+1) hdr%nrRows
    read(this%iu,pos=104+1) hdr%nrCols
    read(this%iu,pos=108+1) hdr%cellSizeX
    read(this%iu,pos=116+1) hdr%cellSizeY
    read(this%iu,pos=124+1) hdr%angle
    
    ! checks
    select case(hdr%cellRepr)
    case(cr_uint2,cr_uint4,cr_undef)
       call errmsg('Unsupported cell representation for MAP-file: '//crstr(hdr%cellRepr))
    end select
    
    ! set return value
    ok = .true.
    !
    return
  end function map_read_full_header

  function map_read_light_header(this, f, verb) result(ok)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    character(len=*), intent(in) :: f
    integer(i4b), intent(in) :: verb
    logical :: ok
    ! -- local
    integer :: i, iinc
    real, parameter :: nodata = -9999. ! should be exactly the same as in rdrsmodule !
    type(tMapHdr), pointer :: hdr
! ------------------------------------------------------------------------------
    call chkexist(f)
    allocate(this%f)
    this%f = f
    
    ! open file in stream mode
    if (.not.associated(this%iu)) allocate(this%iu)
    this%iu=getlun()
    if (verb == 0) write(*,*) 'Reading '//trim(f)//'...'
    open(unit=this%iu,file=f,form='unformatted',access='stream',status='old')
    
    ! READ HEADER
    allocate(this%header)
    hdr => this%header
    read(this%iu,pos= 66+1) hdr%cellRepr
    do i = 1, ncr
       if(crval(i).eq.hdr%cellRepr)then
          hdr%cellRepr=i
          exit
       end if   
    end do    
    read(this%iu,pos=104+1) hdr%nrCols
    
    ! set return value
    ok = .true.
    !
    return
  end function map_read_light_header
  
  function map_write_header(this, f) result(ok)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use iso_c_binding, only: c_int16_t, c_int32_t
    ! -- dummy
    class(tMap) :: this
    character(len=*), intent(in) :: f
    logical :: ok
    ! -- local
    integer(kind=c_int16_t) :: uint2
    integer(kind=c_int32_t) :: uint4
    
    real, parameter :: nodata = -9999. ! should be exactly the same as in rdrsmodule !
    type(tMapHdr), pointer :: hdr
! ------------------------------------------------------------------------------
    
    ! open file in stream mode
    if (.not.associated(this%iu)) allocate(this%iu)
    this%iu=getlun()
    write(*,*) 'Opening '//trim(f)//'...'
    open(unit=this%iu,file=f,form='unformatted',access='stream',status='replace')
    
    ! READ HEADER
    write(*,*) 'Writing header...'
    hdr => this%header
    
    ! main header
    write(this%iu,pos=  0+1) hdr%signature
    uint2 = hdr%version;    write(this%iu,pos= 32+1) uint2
    uint4 = hdr%gisFileId;  write(this%iu,pos= 34+1) uint4
    uint2 = hdr%projection; write(this%iu,pos= 38+1) uint2
    uint4 = hdr%attrTable;  write(this%iu,pos= 40+1) uint4
    uint2 = hdr%dataType;   write(this%iu,pos= 44+1) uint2
    uint4 =  hdr%byteOrder; write(this%iu,pos= 46+1) uint4
    ! raster header
    uint2 = hdr%valueScale; write(this%iu,pos= 64+1) uint2
    uint2 = hdr%cellRepr;   write(this%iu,pos= 66+1) uint2
    select case(hdr%cellRepr)
    case(cr_int4)
       write(this%iu,pos= 68+1) hdr%i4minVal
       write(this%iu,pos= 76+1) hdr%i4maxVal
    case(cr_real4)
       write(this%iu,pos= 68+1) hdr%r4minVal
       write(this%iu,pos= 76+1) hdr%r4maxVal
    end select
    write(this%iu,pos= 84+1) hdr%xUL
    write(this%iu,pos= 92+1) hdr%yUL
    uint4 = hdr%nrRows; write(this%iu,pos=100+1) uint4
    uint4 = hdr%nrCols; write(this%iu,pos=104+1) uint4
    write(this%iu,pos=108+1) hdr%cellSizeX
    write(this%iu,pos=116+1) hdr%cellSizeY
    write(this%iu,pos=124+1) hdr%angle
    
    ! set return value
    ok = .true.
    !
    return
  end function map_write_header
  
  subroutine map_read_data(this, rval)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    real(r8b), optional, intent(in) :: rval
    ! -- local
    integer(i4b) :: nc, nr, ic, ir, p
    character(len=1), dimension(:,:), allocatable :: wrk
! ------------------------------------------------------------------------------
    nc = this%header%nrCols; nr = this%header%nrRows
    p = 256+1
    
    ! allocate
    select case(this%header%cellrepr)
      case(cr_uint1)
        allocate(this%i1a(nc,nr))
      case(cr_int1)
        allocate(this%i1a(nc,nr))
      case(cr_int2)
        allocate(this%i2a(nc,nr))
      case(cr_int4)
        allocate(this%i4a(nc,nr))
      case(cr_real4)
        allocate(this%r4a(nc,nr))
      case(cr_real8)
        allocate(this%r8a(nc,nr))
      case default
          call errmsg('Kind of MAP-file not supported.')
      end select
      
    ! read
    if (present(rval)) then
      select case(this%header%cellrepr)
        case(cr_uint1)
          do ir = 1, nr
            do ic = 1, nc
              this%i1a(ic,ir) = int(rval, i1b)
            end do
          end do
          deallocate(wrk)
        case(cr_int1)
          do ir = 1, nr
            do ic = 1, nc
              this%i1a(ic,ir) = int(rval, i1b)
            end do
          end do
        case(cr_int2)
          do ir = 1, nr
            do ic = 1, nc
              this%i2a(ic,ir) = int(rval, i2b)
            end do
          end do
        case(cr_int4)
          do ir = 1, nr
            do ic = 1, nc
              this%i4a(ic,ir) = int(rval, i4b)
            end do
          end do
        case(cr_real4)
          do ir = 1, nr
            do ic = 1, nc
              this%r4a(ic,ir) = real(rval, r4b)
            end do
          end do
        case(cr_real8)
          do ir = 1, nr
            do ic = 1, nc
              this%r8a(ic,ir) = int(rval, r8b)
            end do
          end do
        case default
            call errmsg('Kind of MAP-file not supported.')
      end select
    else
      select case(this%header%cellrepr)
        case(cr_uint1)
          allocate(wrk(nc,nr))
          read(unit=this%iu,pos=p)((wrk(ic,ir),ic=1,nc),ir=1,nr)
          do ir = 1, nr
            do ic = 1, nc
              this%i1a(ic,ir) = ichar(wrk(ic,ir))
            end do
          end do
          deallocate(wrk)
        case(cr_int1)
          read(unit=this%iu,pos=p)((this%i1a(ic,ir),ic=1,nc),ir=1,nr)
        case(cr_int2)
          read(unit=this%iu,pos=p)((this%i2a(ic,ir),ic=1,nc),ir=1,nr)
        case(cr_int4)
          read(unit=this%iu,pos=p)((this%i4a(ic,ir),ic=1,nc),ir=1,nr)
        case(cr_real4)
          read(unit=this%iu,pos=p)((this%r4a(ic,ir),ic=1,nc),ir=1,nr)
        case(cr_real8)
          read(unit=this%iu,pos=p)((this%r8a(ic,ir),ic=1,nc),ir=1,nr)
        case default
            call errmsg('Kind of MAP-file not supported.')
      end select
        
      ! set nodata
      call this%set_nodata()
      call this%correct_nodata()
    end if
    !
    return
  end subroutine map_read_data

  subroutine map_write_data(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    ! -- local
    integer(i4b) :: nc, nr, ic, ir, p
! ------------------------------------------------------------------------------
    nc = this%header%nrCols; nr = this%header%nrRows
    p = 256+1
    
    write(*,*) 'Writing array...'
    select case(this%header%cellrepr)
      case(cr_int1)
        write(unit=this%iu,pos=p)((this%i1a(ic,ir),ic=1,nc),ir=1,nr)
      case(cr_int2)
        write(unit=this%iu,pos=p)((this%i2a(ic,ir),ic=1,nc),ir=1,nr)
      case(cr_int4)
        write(unit=this%iu,pos=p)((this%i4a(ic,ir),ic=1,nc),ir=1,nr)
      case(cr_real4)
        write(unit=this%iu,pos=p)((this%r4a(ic,ir),ic=1,nc),ir=1,nr)
      case(cr_real8)
        write(unit=this%iu,pos=p)((this%r8a(ic,ir),ic=1,nc),ir=1,nr)
      case default
        call errmsg('Kind of MAP-file not supported.')
    end select 
    !
    return
  end subroutine map_write_data
  
  subroutine map_set_nodata(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    ! -- local
    integer(i4b) :: nc, nr, ic, ir
    real(r4b) :: r4huge
    real(r8b) :: r8huge
! ------------------------------------------------------------------------------
    nc = this%header%nrCols; nr = this%header%nrRows
    r4huge = huge(r4huge); r8huge = huge(r8huge)
    
    select case(this%header%cellrepr)
    case(cr_uint1)
        allocate(this%i1mv)
        this%i1mv = 255
      case(cr_int1)
        allocate(this%i1mv)
        this%i1mv = -128
      case(cr_int2)
        allocate(this%i2mv)
        this%i2mv = 0
        this%i2mv = -huge(this%i2mv)-1
      case(cr_int4)
        allocate(this%i4mv)
        this%i4mv = 0
        this%i4mv = -huge(this%i4mv)-1
      case(cr_real4)
        allocate(this%r4mv)
        this%r4mv = 0
        this%r4mv = huge(this%r4mv)
        if (associated(this%r4a)) then
          do ir = 1, nr
            do ic = 1, nc
              if (ieee_is_nan(this%r4a(ic,ir))) then
                this%r4a(ic,ir) = this%r4mv
              end if
              if (.not.ieee_is_finite(this%r4a(ic,ir))) then
                if (this%r4a(ic,ir) > 0) then
                  this%r4a(ic,ir) = r4huge
                else
                  this%r4a(ic,ir) = -r4huge
                end if
              end if
            end do
          end do
        end if
      case(cr_real8)
        allocate(this%r8mv)
        this%r8mv = 0
        this%r8mv = huge(this%r8mv)
        if (associated(this%r8a)) then
          do ir = 1, nr
            do ic = 1, nc
              if (ieee_is_nan(this%r8a(ic,ir))) then
                this%r8a(ic,ir) = this%r8mv
              end if
              if (.not.ieee_is_finite(this%r8a(ic,ir))) then
                if (this%r8a(ic,ir) > 0) then
                  this%r8a(ic,ir) = r8huge
                else
                  this%r8a(ic,ir) = -r8huge
                end if
              end if
            end do
          end do
        end if
    end select 
    !
    return
  end subroutine map_set_nodata
  
  subroutine map_correct_nodata(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    ! -- local
    integer(i4b) :: nc, nr, ic, ir
    real(r4b) :: r4huge
    real(r8b) :: r8huge
! ------------------------------------------------------------------------------
    nc = this%header%nrCols; nr = this%header%nrRows
    r4huge = huge(r4huge); r8huge = huge(r8huge)
    
    select case(this%header%cellrepr)
      case(cr_real4)
        do ir = 1, nr
          do ic = 1, nc
            if (ieee_is_nan(this%r4a(ic,ir))) then
              this%r4a(ic,ir) = this%r4mv
            end if
            if (.not.ieee_is_finite(this%r4a(ic,ir))) then
              if (this%r4a(ic,ir) > 0) then
                this%r4a(ic,ir) = r4huge
              else
                this%r4a(ic,ir) = -r4huge
              end if
            end if
          end do
        end do
      case(cr_real8)
        do ir = 1, nr
          do ic = 1, nc
            if (ieee_is_nan(this%r8a(ic,ir))) then
              this%r8a(ic,ir) = this%r8mv
            end if
            if (.not.ieee_is_finite(this%r8a(ic,ir))) then
              if (this%r8a(ic,ir) > 0) then
                this%r8a(ic,ir) = r8huge
              else
                this%r8a(ic,ir) = -r8huge
              end if
            end if
          end do
        end do
    end select 
    !
    return
  end subroutine map_correct_nodata
  
  subroutine map_get_i1ar(this, i1a, i1mv, i1min, i1max)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    integer(i1b), dimension(:,:), allocatable, intent(out) :: i1a
    integer(i1b), intent(out) :: i1mv, i1min, i1max
    ! -- local
    integer(i4b) :: nc, nr, ic, ir
    !
    integer(i1b) :: vi1
    integer(i2b) :: vi2
    integer(i4b) :: vi4
    real(r4b)   :: vr4
    real(r8b)   :: vr8
! ------------------------------------------------------------------------------
  
    ! set nodata value
    i1min = huge(1)
    i1max = -i1min
    
    ! allocate
    nc = this%header%nrCols; nr = this%header%nrRows
    if (allocated(i1a)) deallocate(i1a)
    allocate(i1a(nc,nr))
    
    ! set the array
    select case(this%header%cellrepr)
      case(cr_uint1,cr_int1)
        i1mv = int(this%i1mv,i1b)
        do ir = 1, nr
          do ic = 1, nc
            vi1 = this%i1a(ic,ir)
            i1a(ic,ir) = int(vi1,i1b)
            if (vi1 /= this%i1mv) then
              i1min = min(i1min,int(vi1,i1b))
              i1max = max(i1max,int(vi1,i1b))
            end if
          end do
        end do
      case(cr_int2)
        i1mv = int(this%i2mv,i1b)
        do ir = 1, nr
          do ic = 1, nc
            vi2 = this%i2a(ic,ir)
            i1a(ic,ir) = int(vi2,i1b)
            if (vi2 /= this%i2mv) then
              i1min = min(i1min,int(vi2,i1b))
              i1max = max(i1max,int(vi2,i1b))
            end if
          end do
        end do
      case(cr_int4)
        i1mv = int(this%i4mv,i1b)
        do ir = 1, nr
          do ic = 1, nc
            vi4 = this%i4a(ic,ir)
            i1a(ic,ir) = int(vi4,i1b)
            if (vi4 /= this%i4mv) then
              i1min = min(i1min,int(vi4,i1b))
              i1max = max(i1max,int(vi4,i1b))
            end if
          end do
        end do
      case(cr_real4)
        i1mv = int(this%r4mv,i1b)
        do ir = 1, nr
          do ic = 1, nc
            vr4 = this%r4a(ic,ir)
            i1a(ic,ir) = int(vr4,i1b)
            if (vr4 /= this%r4mv) then
              i1min = min(i1min,int(vr4,i1b))
              i1max = max(i1max,int(vr4,i1b))
            end if
          end do
        end do
      case(cr_real8)
        i1mv = int(this%r8mv,i1b)
        do ir = 1, nr
          do ic = 1, nc
            vr8 = this%r8a(ic,ir)
            i1a(ic,ir) = int(vr8,i1b)
            if (vr8 /= this%r8mv) then
              i1min = min(i1min,int(vr8,i1b))
              i1max = max(i1max,int(vr8,i1b))
            end if
          end do
        end do
      case default
          call errmsg('Kind of MAP-file not supported.')
    end select 
    !
    return
  end subroutine map_get_i1ar
  
  subroutine map_get_r4ar(this, r4a, r4mv, r4min, r4max)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    real(r4b), dimension(:,:), allocatable, intent(out) :: r4a
    real(r4b), intent(out) :: r4mv, r4min, r4max
    ! -- local
    integer(i4b) :: nc, nr, ic, ir
    !
    integer(i1b) :: vi1
    integer(i2b) :: vi2
    integer(i4b) :: vi4
    real(r4b)   :: vr4
    real(r8b)   :: vr8
! ------------------------------------------------------------------------------
  
    ! set nodata value
    r4min = huge(1.)
    r4max = -r4min
    
    ! allocate
    nc = this%header%nrCols; nr = this%header%nrRows
    if (allocated(r4a)) deallocate(r4a)
    allocate(r4a(nc,nr))
    
    ! set the array
    select case(this%header%cellrepr)
      case(cr_uint1, cr_int1)
        r4mv = real(this%i1mv,r4b)
        do ir = 1, nr
          do ic = 1, nc
            vi1 = this%i1a(ic,ir)
            r4a(ic,ir) = real(vi1,r4b)
            if (vi1 /= this%i1mv) then
              r4min = min(r4min,real(vi1,r4b))
              r4max = max(r4max,real(vi1,r4b))
            end if
          end do
        end do
      case(cr_int2)
        r4mv = real(this%i2mv,r4b)
        do ir = 1, nr
          do ic = 1, nc
            vi2 = this%i2a(ic,ir)
            r4a(ic,ir) = real(vi2,r4b)
            if (vi2 /= this%i2mv) then
              r4min = min(r4min,real(vi2,r4b))
              r4max = max(r4max,real(vi2,r4b))
            end if
          end do
        end do
      case(cr_int4)
        r4mv = real(this%i4mv,r4b)
        do ir = 1, nr
          do ic = 1, nc
            vi4 = this%i4a(ic,ir)
            r4a(ic,ir) = real(vi4,r4b)
            if (vi4 /= this%i4mv) then
              r4min = min(r4min,real(vi4,r4b))
              r4max = max(r4max,real(vi4,r4b))
            end if
          end do
        end do
      case(cr_real4)
        r4mv = real(this%r4mv,r4b)
        do ir = 1, nr
          do ic = 1, nc
            vr4 = this%r4a(ic,ir)
            r4a(ic,ir) = real(vr4,r4b)
            if (vr4 /= this%r4mv) then
              r4min = min(r4min,real(vr4,r4b))
              r4max = max(r4max,real(vr4,r4b))
            end if
          end do
        end do
      case(cr_real8)
        r4mv = real(this%r8mv,r4b)
        do ir = 1, nr
          do ic = 1, nc
            vr8 = this%r8a(ic,ir)
            r4a(ic,ir) = real(vr8,r4b)
            if (vr8 /= this%r8mv) then
              r4min = min(r4min,real(vr8,r4b))
              r4max = max(r4max,real(vr8,r4b))
            end if
          end do
        end do
      case default
          call errmsg('Kind of MAP-file not supported.')
    end select 
    !
    return
  end subroutine map_get_r4ar
  
  subroutine map_idf_export(this, f)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    character(len=*), intent(in) :: f
    ! -- local
    type(tMapHdr), pointer :: hdr
    integer(i4b) :: iu, ios, p, ic, ir
    real(r4b) :: nodata, dmin, dmax
    real(r4b), dimension(:,:), allocatable :: r4a
! ------------------------------------------------------------------------------

    ! check for equidistant grids
    hdr => this%header; p = 1
    if (hdr%cellSizeX /= hdr%cellSizeY) then
       call errmsg('Exporting IDF for non-equidistant grids is not yet supported.')
    end if
    
    ! get real data
    call this%get_r4ar(r4a, nodata, dmin, dmax)
    
    iu = getlun()
    write(*,*) 'Writing '//trim(f)//'...'
    open(unit=iu,file=f,form='unformatted',access='stream', &
      status='replace',iostat=ios)

    p = 1
    write(iu,pos=p) int(1271,i4b); p = p + 4 !1271
    write(iu,pos=p) int(hdr%nrCols,i4b); p = p + 4 !ncol
    write(iu,pos=p) int(hdr%nrRows,i4b); p = p + 4 !nrow
    write(iu,pos=p) real(hdr%xUL,r4b); p = p + 4 !xmin
    write(iu,pos=p) real(hdr%xUL+hdr%nrCols*hdr%cellSizeX,r4b); p = p + 4 !xmax
    write(iu,pos=p) real(hdr%yUL-hdr%nrRows*hdr%cellSizeY,r4b); p = p + 4 !ymin
    write(iu,pos=p) real(hdr%yUL,r4b); p = p + 4 !ymax
    write(iu,pos=p) real(dmin,r4b); p = p + 4 !dmin
    write(iu,pos=p) real(dmax,r4b); p = p + 4 !dmax
    write(iu,pos=p) real(nodata,r4b); p = p + 4 !nodata
    write(iu,pos=p) int(0,i1b); p = p + 1 !ieq
    write(iu,pos=p) int(0,i1b); p = p + 1 !itp
    write(iu,pos=p) int(0,i1b); p = p + 1 !i
    write(iu,pos=p) int(0,i1b); p = p + 1 !not used
    write(iu,pos=p) real(hdr%cellSizeX,r4b); p = p + 4 !dx
    write(iu,pos=p) real(hdr%cellSizeY,r4b); p = p + 4 !dy
    write(iu,pos=p)((r4a(ic,ir),ic=1,hdr%nrCols),ir=1,hdr%nrRows) !x
    close(iu)
    
    deallocate(r4a)
    
    close(iu)
    !
    return
  end subroutine map_idf_export
  
  subroutine map_close(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    ! -- local
    logical :: lop
! ------------------------------------------------------------------------------
    if (associated(this%iu)) then
      inquire(unit=this%iu,opened=lop)
      if (lop) then
        !write(*,*) 'Closing map-file...'
        close(this%iu)
      end if
    end if
    !
    return
  end subroutine map_close

  subroutine map_clean_data(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    ! -- local
! ------------------------------------------------------------------------------
    ! close the file
    !write(*,*) 'Cleaning map-file data structures...'
    if (associated(this%i1a))    deallocate(this%i1a)
    if (associated(this%i1a))    deallocate(this%i1a)
    if (associated(this%i2a))    deallocate(this%i2a)
    if (associated(this%i4a))    deallocate(this%i4a)
    if (associated(this%r4a))    deallocate(this%r4a)
    if (associated(this%r8a))    deallocate(this%r8a)
    !
    return
  end subroutine map_clean_data
  
  subroutine map_clean(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    ! -- local
! ------------------------------------------------------------------------------
    ! close the file
    !write(*,*) 'Cleaning map-file data structures...'
    call this%close()
    if (associated(this%f))      deallocate(this%f)
    if (associated(this%iu))     deallocate(this%iu)
    if (associated(this%header)) deallocate(this%header)
    if (associated(this%gic0))  deallocate(this%gic0)
    if (associated(this%gic1))  deallocate(this%gic1)
    if (associated(this%gir0))  deallocate(this%gir0)
    if (associated(this%gir1))  deallocate(this%gir1)
    !
    call this%clean_data()
    !
    return
  end subroutine map_clean
  
  subroutine map_geti1val(this, icol, irow, i1val, lmv_out)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ieee_arithmetic 
    ! -- dummy
    class(tMap) :: this
    integer(i4b), intent(in) :: icol
    integer(i4b), intent(in) :: irow
    integer(i1b), intent(out) :: i1val
    logical, intent(out), optional :: lmv_out
    ! -- local
    integer(i1b) :: vi1
    integer(i2b) :: vi2
    integer(i4b) :: vi4
    real(r4b)   :: vr4
    real(r8b)   :: vr8
    logical :: lmv
    character(len=1) :: wrk
    integer(i8b) :: p, n
! ------------------------------------------------------------------------------
    p = 256+1
    n = (irow-1)*this%header%nrcols + icol - 1
    lmv = .false.; i1val = 0
    select case(this%header%cellrepr)
      case(cr_uint1, cr_int1)
        read(unit=this%iu,pos=p+n) wrk
        vi1 = ichar(wrk)
        if (vi1 == this%i1mv) then
          lmv = .true.
        else
          i1val = int(vi1,i1b)
        end if
      case(cr_int2)
        read(unit=this%iu,pos=p+2*n) vi2
        if (vi2 == this%i2mv) then
          lmv = .true.
        else
          i1val = int(vi2,i1b)
        end if
      case(cr_int4)
        read(unit=this%iu,pos=p+4*n) vi4
        if (vi4 == this%i4mv) then
          lmv = .true.
        else
          i1val = int(vi4,i1b)
        end if
      case(cr_real4)
        read(unit=this%iu,pos=p+4*n) vr4
        if (ieee_is_nan(vr4)) then
          lmv = .true.
        else
          i1val = int(vr4,i1b)
        end if
      case(cr_real8)
        read(unit=this%iu,pos=p+8*n) vr8
        if (ieee_is_nan(vr8)) then
          lmv = .true.
        else
          i1val = int(vr8,i1b)
        end if
      case default
        call errmsg('Kind of MAP-file not supported.')
      end select 
    !
    if (present(lmv_out)) then
      lmv_out = lmv
    end if
    return
  end subroutine map_geti1val
  !
 subroutine map_geti4val(this, icol, irow, i4val, lmv_out)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ieee_arithmetic 
    ! -- dummy
    class(tMap) :: this
    integer(i4b), intent(in) :: icol
    integer(i4b), intent(in) :: irow
    integer(i4b), intent(out) :: i4val
    logical, intent(out), optional :: lmv_out
    ! -- local
    integer(i1b) :: vi1
    integer(i2b) :: vi2
    integer(i4b) :: vi4
    real(r4b)   :: vr4
    real(r8b)   :: vr8
    logical :: lmv
    character(len=1) :: wrk
    integer(i8b) :: p, n
! ------------------------------------------------------------------------------
    p = 256+1
    n = (irow-1)*this%header%nrcols + icol - 1
    lmv = .false.; i4val = 0
    select case(this%header%cellrepr)
      case(cr_uint1, cr_int1)
        read(unit=this%iu,pos=p+n) wrk
        vi1 = ichar(wrk)
        if (vi1 == this%i1mv) then
          lmv = .true.
        else
          i4val = int(vi1,i4b)
        end if
      case(cr_int2)
        read(unit=this%iu,pos=p+2*n) vi2
        if (vi2 == this%i2mv) then
          lmv = .true.
        else
          i4val = int(vi2,i4b)
        end if
      case(cr_int4)
        read(unit=this%iu,pos=p+4*n) vi4
        if (vi4 == this%i4mv) then
          lmv = .true.
        else
          i4val = int(vi4,i4b)
        end if
      case(cr_real4)
        read(unit=this%iu,pos=p+4*n) vr4
        if (ieee_is_nan(vr4)) then
          lmv = .true.
        else
          i4val = int(vr4,i4b)
        end if
      case(cr_real8)
        read(unit=this%iu,pos=p+8*n) vr8
        if (ieee_is_nan(vr8)) then
          lmv = .true.
        else
          i4val = int(vr8,i4b)
        end if
      case default
        call errmsg('Kind of MAP-file not supported.')
      end select 
    !
    if (present(lmv_out)) then
      lmv_out = lmv
    end if
    return
  end subroutine map_geti4val
  !
 subroutine map_getr4val(this, icol, irow, r4val, lmv_out)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ieee_arithmetic 
    ! -- dummy
    class(tMap) :: this
    integer(i4b), intent(in) :: icol
    integer(i4b), intent(in) :: irow
    real(r4b), intent(out) :: r4val
    logical, intent(out), optional :: lmv_out
    ! -- local
    integer(i1b) :: vi1
    integer(i2b) :: vi2
    integer(i4b) :: vi4
    real(r4b)   :: vr4
    real(r8b)   :: vr8
    logical :: lmv
    character(len=1) :: wrk
    integer(i8b) :: p, n
! ------------------------------------------------------------------------------
    p = 256+1
    n = (irow-1)*this%header%nrcols + icol - 1
    lmv = .false.; r4val = 0.0
    select case(this%header%cellrepr)
      case(cr_uint1, cr_int1)
        read(unit=this%iu,pos=p+n) wrk
        vi1 = ichar(wrk)
        if (vi1 == this%i1mv) then
          lmv = .true.
        else
          r4val = real(vi1,r4b)
        end if
      case(cr_int2)
        read(unit=this%iu,pos=p+2*n) vi2
        if (vi2 == this%i2mv) then
          lmv = .true.
        else
          r4val = real(vi2,r4b)
        end if
      case(cr_int4)
        read(unit=this%iu,pos=p+4*n) vi4
        if (vi4 == this%i4mv) then
          lmv = .true.
        else
          r4val = real(vi4,r4b)
        end if
      case(cr_real4)
        read(unit=this%iu,pos=p+4*n) vr4
        if (ieee_is_nan(vr4)) then
          lmv = .true.
        else
          r4val = real(vr4,r4b)
        end if
      case(cr_real8)
        read(unit=this%iu,pos=p+8*n) vr8
        if (ieee_is_nan(vr8)) then
          lmv = .true.
        else
          r4val = real(vr8,r4b)
        end if
      case default
        call errmsg('Kind of MAP-file not supported.')
      end select 
    !
    if (present(lmv_out)) then
      lmv_out = lmv
    end if
    return
   end subroutine map_getr4val
  
 subroutine map_getr8val(this, icol, irow, r8val, lmv_out)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ieee_arithmetic 
    ! -- dummy
    class(tMap) :: this
    integer(i4b), intent(in) :: icol
    integer(i4b), intent(in) :: irow
    real(r8b), intent(out) :: r8val
    logical, intent(out), optional :: lmv_out
    ! -- local
    integer(i1b) :: vi1
    integer(i2b) :: vi2
    integer(i4b) :: vi4
    real(r4b)   :: vr4
    real(r8b)   :: vr8
    logical :: lmv
    character(len=1) :: wrk
    integer(i8b) :: p, n
! ------------------------------------------------------------------------------
    p = 256+1
    n = (irow-1)*this%header%nrcols + icol - 1
    lmv = .false.; r8val = DZERO
    select case(this%header%cellrepr)
      case(cr_uint1, cr_int1)
        read(unit=this%iu,pos=p+n) wrk
        vi1 = ichar(wrk)
        if (vi1 == this%i1mv) then
          lmv = .true.
        else
          r8val = real(vi1,r8b)
        end if
      case(cr_int2)
        read(unit=this%iu,pos=p+2*n) vi2
        if (vi2 == this%i2mv) then
          lmv = .true.
        else
          r8val = real(vi2,r8b)
        end if
      case(cr_int4)
        read(unit=this%iu,pos=p+4*n) vi4
        if (vi4 == this%i4mv) then
          lmv = .true.
        else
          r8val = real(vi4,r8b)
        end if
      case(cr_real4)
        read(unit=this%iu,pos=p+4*n) vr4
        if (ieee_is_nan(vr4)) then
          lmv = .true.
        else
          r8val = real(vr4,r8b)
        end if
      case(cr_real8)
        read(unit=this%iu,pos=p+8*n) vr8
        if (ieee_is_nan(vr8)) then
          lmv = .true.
        else
          r8val = real(vr8,r8b)
        end if
      case default
        call errmsg('Kind of MAP-file not supported.')
      end select 
    !
    if (present(lmv_out)) then
      lmv_out = lmv
    end if
    return
  end subroutine map_getr8val 
 
  subroutine map_readblock_i1(this, i1a, ir0, ir1, ic0, ic1, i1mv)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(tMap) :: this
    integer(i4b), intent(in) :: ir0
    integer(i4b), intent(in) :: ir1
    integer(i4b), intent(in) :: ic0
    integer(i4b), intent(in) :: ic1
    integer(i1b), intent(in) :: i1mv
    integer(i1b), dimension(:,:), allocatable, intent(out) :: i1a
    ! -- local
    integer(i1b) :: i1v
    integer(i4b) :: nc, nr, ic, ir, jc, jr
    logical :: lmv
! ------------------------------------------------------------------------------
    call this%set_nodata()
    nr = ir1 - ir0 + 1
    nc = ic1 - ic0 + 1
    if (allocated(i1a)) deallocate(i1a)
    allocate(i1a(nc,nr))
    do ir = ir0, ir1
      do ic = ic0, ic1
        jc = ic - ic0 + 1; jr = ir - ir0 + 1
        call this%map_geti1val(ic, ir, i1v, lmv)
        if (lmv) then
          i1a(jc,jr) = i1mv
        else
          i1a(jc,jr) = i1v
        end if
      end do
    end do
    !
    return
  end subroutine map_readblock_i1
 
  subroutine map_readblock_i4(this, i4a, ir0, ir1, ic0, ic1, i4mv)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(tMap) :: this
    integer(i4b), intent(in) :: ir0
    integer(i4b), intent(in) :: ir1
    integer(i4b), intent(in) :: ic0
    integer(i4b), intent(in) :: ic1
    integer(i4b), intent(in) :: i4mv
    integer(i4b), dimension(:,:), allocatable, intent(out) :: i4a
    ! -- local
    integer(i4b) :: i4v, i4mv_dat
    integer(i4b) :: nc, nr, ic, ir, jc, jr
    logical :: lmv
! ------------------------------------------------------------------------------
    call this%set_nodata()
    nr = ir1 - ir0 + 1
    nc = ic1 - ic0 + 1
    if (allocated(i4a)) deallocate(i4a)
    allocate(i4a(nc,nr))
    do ir = ir0, ir1
      do ic = ic0, ic1
        jc = ic - ic0 + 1; jr = ir - ir0 + 1
        call this%map_geti4val(ic, ir, i4v, lmv)
        if (lmv) then
          i4a(jc,jr) = i4mv
        else
          i4a(jc,jr) = i4v
        end if
      end do
    end do
    !
    return
  end subroutine map_readblock_i4
  
  subroutine map_readblock_r4(this, r4a, ir0, ir1, ic0, ic1, r4mv)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(tMap) :: this
    integer(i4b), intent(in) :: ir0
    integer(i4b), intent(in) :: ir1
    integer(i4b), intent(in) :: ic0
    integer(i4b), intent(in) :: ic1
    integer(i1b), intent(in) :: r4mv
    real(r4b), dimension(:,:), allocatable, intent(out) :: r4a
    ! -- local
    real(r4b) :: r4v
    integer(i4b) :: nc, nr, ic, ir, jc, jr
    logical :: lmv
! ------------------------------------------------------------------------------
    call this%set_nodata()
    nr = ir1 - ir0 + 1
    nc = ic1 - ic0 + 1
    if (allocated(r4a)) deallocate(r4a)
    allocate(r4a(nc,nr))
    do ir = ir0, ir1
      do ic = ic0, ic1
        jc = ic - ic0 + 1; jr = ir - ir0 + 1
        call this%map_getr4val(ic, ir, r4v, lmv)
        if (lmv) then
          r4a(jc,jr) = r4mv
        else
          r4a(jc,jr) = r4v
        end if
      end do
    end do
    !
    return
  end subroutine map_readblock_r4
  
end module pcrModule
