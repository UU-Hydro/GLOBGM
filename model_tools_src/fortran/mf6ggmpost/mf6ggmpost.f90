program mf6ggmpost
  ! -- modules
  use utilsmod, only: i4b, mxslen, open_file, logmsg, ta, parse_line, errmsg
  use mf6_post_module, only: tPostSol, tPostSer, &
    gncol, gnrow, gnlay, gxmin, gymin, gcs, gnsol, &
    sdate, tilebb, top, comment, mask, maskmap, include_sea, &
    top_type, top_kper, i_tiled, i_mf6
  !
  implicit none
  !
  ! -- locals
  type(tPostSol), pointer :: postsol => null()
  type(tPostSer), pointer :: postser => null()
  character(len=1) :: cdum, ssol
  character(len=mxslen) :: f, s, in_dir
  character(len=mxslen), dimension(:), allocatable :: sap, sa
  logical :: lex, lok
  integer(i4b) :: iu, i, j0, j1, j, npost, mpost, na
! ------------------------------------------------------------------------------
  !
  if (include_sea) then
    call logmsg('***** WARNING: output for sea cells enabled! *****')
  end if
  !
  call getarg(1, f)
  call open_file(f, iu, 'r')
  read(iu,*) gncol, gnrow, gnlay, gxmin, gymin, gcs, gnsol
  read(iu,*) sdate
  read(iu,'(a)') tilebb
  read(iu,'(a)') mask
  read(iu,'(a)') s
  call parse_line(s, sa)
  top_type = 0; top_kper = 0
  if (size(sa) >= 2) then
    select case(sa(1))
      case('tiled')
        top_type = i_tiled
      case('mf6')
        top_type = i_mf6
        read(sa(3),*) top_kper
      case default
        call errmsg('Unrecognized top file.')
    end select
    top = sa(2)
  end if
  read(iu,*) npost
  !
  ! read the mask file
  inquire(file=mask, exist=lex)
  if (lex) then
    allocate(maskmap)
    lok = maskmap%init(mask)
    call maskmap%set_nodata()
    call logmsg('***** Found mask! *****')
  else
    call logmsg('***** Could not find mask! *****')
  end if
  !
  allocate(postsol, postser)
  do i = 1, npost
    call logmsg('***** Processing '//ta((/i/))//'/'//ta((/npost/))//'...')
    read(iu,'(a)') s
    !
    !return in case of comment
    if (s(1:1) == comment) then
      call logmsg('Skipping...')
      cycle
    end if
    !
    call parse_line(s, sap)
    na = size(sap)
    if (allocated(sa)) deallocate(sa)
    !
    select case(sap(2))
      case('m', 's','smodid','r', 'mr', 'sr')
        if ((sap(2) == 'mr').or.(sap(2) == 'sr')) then
          read(sap(3:4),*) j0, j1
          allocate(sa(na-1))
          sa(1) = sap(1); sa(2) = sap(2)(1:1)
          do j = 4, na-1
            sa(j) = sap(j+1)
          end do
        else
          allocate(sa(na))
          sa = sap
          read(sa(3),*) j0; j1 = j0
        end if
        do j = j0, j1
          sa(3) = ta((/j/))
          call postsol%init(sa)
          call postsol%write()
          call postsol%clean()
        end do
      case('t')
        call postser%init(sa)
        call postser%write(.true.)
        call postser%write_summary(.false.)
        call postser%clean()
      case('p')
        call postser%init(sa)
        call postser%write(.false.)
        call postser%write_summary(.true.)
        call postser%clean()
    end select
  end do
  close(iu)
  !
  if (associated(maskmap)) then
    call maskmap%clean()
    deallocate(maskmap); maskmap => null()
  end if
end program