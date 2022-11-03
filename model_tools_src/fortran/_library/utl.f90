            function cfn_cla_fnd(arg)
! description:
! ------------------------------------------------------------------------------
! search for the position of a Command Line Argument
! Whenever the argument occurs multiple times, the last one will be taken
! Search for string independently of case

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   cfn_cla_fnd   ! return value: <0: not present
                              !               >0: argument number


! arguments
      character arg*(*)       ! (I) string to search


! local variables
      integer   larg1,larg2,narg,i,n

      character targ1*256,targ2*256


! functions
      integer   cfn_length,&
                osd_iargc

      character cfn_upcase*256,&
                cfn_trim*256


! include files


! program section
! ------------------------------------------------------------------------------


! query number of arguments
      narg=osd_iargc()


! make argument uppercase
      targ1=cfn_trim(cfn_upcase(arg))
      larg1=cfn_length(targ1)


! query index number of last argument
      n=-1
      do i=1,narg,1
         call osd_getarg(i,targ2)
         targ2=cfn_trim(cfn_upcase(targ2))
         larg2=cfn_length(targ2)
         if (targ1(1:larg1).eq.targ2(1:larg2)) then
            n=i
         endif
      enddo


! assign function value
      cfn_cla_fnd=n


! end of program
      return
      end

! ******************************************************************************

      function cfn_cla_gti(arg,val,nval)

! description:
! ------------------------------------------------------------------------------
! query a number of integer arguments from a Command Line Argument
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer    cfn_cla_gti  ! return value: -3: too few values
                              !               -2: values of wrong type
                              !               -1: CLA not found
                              !                0: OK


! arguments
      character arg*(*)       ! (I) string to search

      integer   nval          ! (I) number of values

      integer   val(nval)     ! (I) default values
                              ! (O) return value  0   : found values
                              !     return value -1   : default  values
                              !     return value -2,-3: undefined


! local variables
      integer   tval

      integer   i,ip,ret,narg,ios

      character targ1*256



! functions
      integer   cfn_cla_fnd,&
                cfn_length,&
                osd_iargc


! include files


! program section
! ------------------------------------------------------------------------------

! init
      ret=0


! check if CLA occurs
      ip=cfn_cla_fnd(arg)


      if (ip.lt.0) then
         ! not found
         ret=-1
      else

         ! query number of arguments
         narg=osd_iargc()

         if ((ip+nval).gt.narg) then
            ! too few arguments
            write(*,*) ' CLA ',arg(1:cfn_length(arg)),&
                       ' too few arguments!'
            ret=-3
         else
            do i=1,nval
               call osd_getarg(ip+i,targ1)
               read(targ1,*,iostat=ios) tval
               if (ios.eq.0) then
                  val(i)=tval
               else
                  write(*,*) ' CLA ',arg(1:cfn_length(arg)),&
                       ' argument ',i,' wrong type. ',&
                       targ1(1:cfn_length(targ1))
                  ret=-2
               endif
            enddo
         endif
      endif


! assign function value
      cfn_cla_gti=ret


! end of program
      return
      end

! ******************************************************************************

      function cfn_cla_gtr(arg,val,nval)

! description:
! ------------------------------------------------------------------------------
! query a number of real arguments from a Command Line Argument
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer    cfn_cla_gtr  ! return value: -3: too few values
                              !               -2: values of wrong type
                              !               -1: CLA not found
                              !                0: OK


! arguments
      character arg*(*)       ! (I) string to search

      integer   nval          ! (I) number of values

      real      val(nval)     ! (I) default waarden
                              ! (O) return value  0   : found values
                              !     return value -1   : default  values
                              !     return value -2,-3: undefined

! local variables
      real      tval

      integer   i,ip,ret,narg,ios

      character targ1*256



! functions
      integer   cfn_cla_fnd,&
                cfn_length,&
                osd_iargc


! include files


! program section
! ------------------------------------------------------------------------------

! init
      ret=0


! check if CLA occurs
      ip=cfn_cla_fnd(arg)


      if (ip.lt.0) then
         ! not found
         ret=-1
      else

         ! query number of arguments
         narg=osd_iargc()

         if ((ip+nval).gt.narg) then
            ! too few arguments
            write(*,*) ' CLA ',arg(1:cfn_length(arg)),&
                       ' too few arguments!'
            ret=-3
         else
            do i=1,nval
               call osd_getarg(ip+i,targ1)
               read(targ1,*,iostat=ios) tval
               if (ios.eq.0) then
                  val(i)=tval
               else
                  write(*,*) ' CLA ',arg(1:cfn_length(arg)),&
                       ' argument ',i,' wrong type. ',&
                       targ1(1:cfn_length(targ1))
                  ret=-2
               endif
            enddo
         endif
      endif


! assign function value
      cfn_cla_gtr=ret


! end of program
      return
      end

! ******************************************************************************

      function cfn_cla_gtd(arg,val,nval)

! description:
! ------------------------------------------------------------------------------
! query a number of double precision arguments from a Command Line Argument
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer    cfn_cla_gtd  ! return value: -3: too few values
                              !               -2: values of wrong type
                              !               -1: CLA not found
                              !                0: OK


! arguments
      character arg*(*)       ! (I) string to search

      integer   nval          ! (I) number of values

      double precision val(nval)     ! (I) default values
                              ! (O) return value  0   : found values
                              !     return value -1   : default  values
                              !     return value -2,-3: undefined

! local variables
      double precision tval

      integer   i,ip,ret,narg,ios

      character targ1*256



! functions
      integer   cfn_cla_fnd,&
                cfn_length,&
                osd_iargc


! include files


! program section
! ------------------------------------------------------------------------------

! init
      ret=0


! check if CLA occurs
      ip=cfn_cla_fnd(arg)


      if (ip.lt.0) then
         ! not found
         ret=-1
      else

         ! query number of arguments
         narg=osd_iargc()

         if ((ip+nval).gt.narg) then
            ! too few arguments
            write(*,*) ' CLA ',arg(1:cfn_length(arg)),&
                       ' too few arguments!'
            ret=-3
         else
            do i=1,nval
               call osd_getarg(ip+i,targ1)
               read(targ1,*,iostat=ios) tval
               if (ios.eq.0) then
                  val(i)=tval
               else
                  write(*,*) ' CLA ',arg(1:cfn_length(arg)),&
                       ' argument ',i,' wrong type. ',&
                       targ1(1:cfn_length(targ1))
                  ret=-2
               endif
            enddo
         endif
      endif


! assign function value
      cfn_cla_gtd=ret


! end of program
      return
      end

! ******************************************************************************

      function cfn_cla_gtc(arg,val,nval)

! description:
! ------------------------------------------------------------------------------
! query a number of character arguments from a Command Line Argument
!


! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer    cfn_cla_gtc  ! return value: -3: too few values
                              !               -2: values of wrong type
                              !               -1: CLA not found
                              !                0: OK


! arguments
      character arg*(*)       ! (I) string to search

      integer   nval          ! (I) number of values

      character val(nval)*(*) ! (I) default values
                              ! (O) return value  0   : found values
                              !     return value -1   : default  values
                              !     return value -2,-3: undefined

! local variables
      integer   i,ip,ret,narg

      character targ1*256



! functions
      integer   cfn_cla_fnd,&
                cfn_length,&
                osd_iargc


! include files


! program section
! ------------------------------------------------------------------------------

! init
      ret=0


! check if CLA occurs
      ip=cfn_cla_fnd(arg)


      if (ip.lt.0) then
         ! not found
         ret=-1
      else

         ! query number of arguments
         narg=osd_iargc()

         if ((ip+nval).gt.narg) then
            ! too few arguments
            write(*,*) ' CLA ',arg(1:cfn_length(arg)),&
                       ' too few arguments!'
            ret=-3
         else
            do i=1,nval
               call osd_getarg(ip+i,targ1)
               val(i)=targ1
            enddo
         endif
      endif


! assign function value
      cfn_cla_gtc=ret


! end of program
      return
      end

! ******************************************************************************

      subroutine cfn_clas_ini()

! description:
! ------------------------------------------------------------------------------
! initialize the command line in a string
! That string will later be used to read out values by means of cfn_clas_*
! routines. Once a value is read it will be deleted from the string.
! This routine must be called upon before another cfn_clas_*
! routine will be used

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments


! local variables
      integer   i,p,narg


! functions
      integer   osd_iargc,&
                cfn_length


! include files
      include 'utl1.inc'


! program section
! ------------------------------------------------------------------------------


      ! query number of arguments
      narg=osd_iargc()

      ! query all arguments
      p=0
      do i=0,narg
         p=p+1
         call osd_getarg(i,cfn_clas_cla(p:))
         p=cfn_length(cfn_clas_cla)+1
         cfn_clas_cla(p:p)=' '
      enddo



      cfn_clas_len =p-1    ! length of command line
      cfn_clas_narg=narg   ! number of arguments in the command line

! end of program
      return
      end

! ******************************************************************************

      function cfn_clas_fnd(arg)

! description:
! ------------------------------------------------------------------------------
! search for the position of a Command Line Argument
! Whenever the argument occurs multiple times, the last one will be taken
! Search for string independently of case

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   cfn_clas_fnd  ! return value: <0: not present
                              !               >0: argument number


! arguments
      character arg*(*)       ! (I) string to search


! local variables
      integer   larg1,larg2,narg,i,n

      character targ1*256,targ2*256


! functions
      integer   cfn_length

      character cfn_upcase*256,&
                cfn_trim*256,&
                cfn_elem*256


! include files
      include 'utl1.inc'


! program section
! ------------------------------------------------------------------------------


! query number of arguments
      narg=cfn_clas_narg


! turn argument to uppercase
      targ1=cfn_trim(cfn_upcase(arg))
      larg1=cfn_length(targ1)


! query index number of last argument
      n=-1
      do i=1,narg,1
         ! there is searched from Command Line Argument number 1 = position 2
         ! in cfn_clas_cla, position 1 is argument 0 = command
         targ2=cfn_elem(i+1,' ,',2,cfn_clas_cla)
         targ2=cfn_trim(cfn_upcase(targ2))
         larg2=cfn_length(targ2)
         if (targ1(1:larg1).eq.targ2(1:larg2)) then
            n=i
         endif
      enddo


! assign function value
      cfn_clas_fnd=n


! end of program
      return
      end

! ******************************************************************************

      function cfn_clas_gti(arg,val,nval)

! description:
! ------------------------------------------------------------------------------
! query a number of integer arguments from a Command Line Argument String
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer    cfn_clas_gti ! return value: -3: too few values
                              !               -2: values of wrong type
                              !               -1: CLA not found
                              !                0: OK


! arguments
      character arg*(*)       ! (I) string to search

      integer   nval          ! (I) number of values

      integer   val(nval)     ! (O) found values


! local variables
      integer   tval

      integer   i,ip,ret,narg,ios

      character targ1*256



! functions
      integer   cfn_clas_fnd,&
                cfn_length

      character cfn_elem*256


! include files
      include 'utl1.inc'


! program section
! ------------------------------------------------------------------------------

! init
      ret=0


! check if CLA occurs
      ip=cfn_clas_fnd(arg)


      if (ip.lt.0) then
         ! not found
         ret=-1
      else

         ! query number of arguments
         narg=cfn_clas_narg

         if ((ip+nval).gt.narg) then
            ! too few arguments
            write(*,*) ' CLA ',arg(1:cfn_length(arg)),&
                       ' too few arguments!'
            ret=-3
         else
            do i=1,nval
               ! there is searched from Command Line Argument number 1 = position 2
               ! in cfn_clas_cla, position 1 is argument 0 = command
               targ1=cfn_elem(ip+i+1,' ,',2,cfn_clas_cla)
               read(targ1,*,iostat=ios) tval
               if (ios.eq.0) then
                  val(i)=tval
               else
                  write(*,*) ' CLA ',arg(1:cfn_length(arg)),&
                       ' argument ',i,' wrong type. ',&
                       targ1(1:cfn_length(targ1))
                  ret=-2
               endif
            enddo
            if (ret.eq.0) then
               ! everything went OK, remove used fields of string
               call cfn_clas_clr(ip,ip+nval)
            endif
         endif
      endif


! assign function value
      cfn_clas_gti=ret


! end of program
      return
      end

! ******************************************************************************

      function cfn_clas_gtr(arg,val,nval)

! description:
! ------------------------------------------------------------------------------
! query a number of real arguments from a Command Line Argument String
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer    cfn_clas_gtr ! return value: -3: too few values
                              !               -2: values of wrong type
                              !               -1: CLA not found
                              !                0: OK


! arguments
      character arg*(*)       ! (I) string to search

      integer   nval          ! (I) number of values

      real      val(nval)     ! (O) found values


! local variables
      real      tval

      integer   i,ip,ret,narg,ios

      character targ1*256



! functions
      integer   cfn_clas_fnd,&
                cfn_length

      character cfn_elem*256


! include files
      include 'utl1.inc'


! program section
! ------------------------------------------------------------------------------

! init
      ret=0


! check if CLA occurs
      ip=cfn_clas_fnd(arg)


      if (ip.lt.0) then
         ! not found
         ret=-1
      else

         ! query number of arguments
         narg=cfn_clas_narg

         if ((ip+nval).gt.narg) then
            ! too few arguments
            write(*,*) ' CLA ',arg(1:cfn_length(arg)),&
                       ' too few arguments!'
            ret=-3
         else
            do i=1,nval
               ! there is searched from Command Line Argument number 1 = position 2
               ! in cfn_clas_cla, position 1 is argument 0 = command
               targ1=cfn_elem(ip+i+1,' ,',2,cfn_clas_cla)
               read(targ1,*,iostat=ios) tval
               if (ios.eq.0) then
                  val(i)=tval
               else
                  write(*,*) ' CLA ',arg(1:cfn_length(arg)),&
                       ' argument ',i,' wrong type. ',&
                       targ1(1:cfn_length(targ1))
                  ret=-2
               endif
            enddo
            if (ret.eq.0) then
               ! everything went OK, remove used fields of string
               call cfn_clas_clr(ip,ip+nval)
            endif
         endif
      endif


! assign function value
      cfn_clas_gtr=ret


! end of program
      return
      end

! ******************************************************************************

      function cfn_clas_gtc(arg,val,nval)

! description:
! ------------------------------------------------------------------------------
! query a number of character arguments from a Command Line Argument String
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer    cfn_clas_gtc ! return value: -3: too few values
                              !               -2: values of wrong type
                              !               -1: CLA not found
                              !                0: OK


! arguments
      character arg*(*)       ! (I) string to search

      integer   nval          ! (I) number of values

      character val(nval)*(*) ! (O) found values


! local variables
      integer   i,ip,ret,narg

      character targ1*256



! functions
      integer   cfn_clas_fnd,&
                cfn_length

      character cfn_elem*256


! include files
      include 'utl1.inc'


! program section
! ------------------------------------------------------------------------------

! init
      ret=0


! check if CLA occurs
      ip=cfn_clas_fnd(arg)


      if (ip.lt.0) then
         ! not found
         ret=-1
      else

         ! query number of arguments
         narg=cfn_clas_narg

         if ((ip+nval).gt.narg) then
            ! too few arguments
            write(*,*) ' CLA ',arg(1:cfn_length(arg)),&
                       ' too few arguments!'
            ret=-3
         else
            do i=1,nval
               ! there is searched from Command Line Argument number 1 = position 2
               ! in cfn_clas_cla, position 1 is argument 0 = command
               targ1=cfn_elem(ip+i+1,' ,',2,cfn_clas_cla)
               val(i)=targ1
            enddo
            if (ret.eq.0) then
               ! everything went OK, remove used fields of string
               call cfn_clas_clr(ip,ip+nval)
            endif
         endif
      endif


! assign function value
      cfn_clas_gtc=ret


! end of program
      return
      end


! ******************************************************************************

      subroutine cfn_clas_clr(beg,end)

! description:
! ------------------------------------------------------------------------------
! removal of arguments beg to end from the CLA-string
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   beg,&     ! (I) initial argument to remove
                end       ! (I) end  argument to remove


! local variables
      integer   pb1,pe1,pb2,pe2,tbeg,tend


! functions


! include files
      include 'utl1.inc'


! program section
! ------------------------------------------------------------------------------

! put the positions inside the borders, beg may not be 0 (=command)
      tbeg=max(1,beg)
      tend=min(end,cfn_clas_narg)


! run only if there is something that needs to be done, otherwise, finish
      if (tend.lt.tbeg) return


! determine end position of field beg-1, but never less than 0
      call cfn_elem_be(tbeg-1+1,' ,',2,cfn_clas_cla,&
                       cfn_clas_len,pb1,pe1)

      if (tend.eq.cfn_clas_narg) then
         ! only keep the starting part, remove the rest
         cfn_clas_cla=cfn_clas_cla(1:pe1)
         cfn_clas_narg=tbeg-1
      else
         ! query the start position of the first field after 'end'
         call cfn_elem_be(tend+1+1,' ,',2,cfn_clas_cla,&
                       cfn_clas_len,pb2,pe2)
         cfn_clas_cla=cfn_clas_cla(1:pe1+1)//cfn_clas_cla(pb2:)
         cfn_clas_narg=cfn_clas_narg-(tend-tbeg+1)
      endif


! end of program
      return
      end

! ******************************************************************************

      function cfn_clas_iargc()

! description:
! ------------------------------------------------------------------------------
! query current number of arguments in CLA-string
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer cfn_clas_iargc   ! return value: number of elements
                               !               0=only command


! arguments


! local variables


! functions


! include files
      include 'utl1.inc'


! program section
! ------------------------------------------------------------------------------


! assign function value
      cfn_clas_iargc=cfn_clas_narg


! end of program
      return
      end

! ******************************************************************************

      subroutine cfn_clas_getarg(iarg,arg)

! description:
! ------------------------------------------------------------------------------
! query argument number iarg from CLA-string
! after query this element will be removed from the string

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   iarg          ! (I) argument number

      character arg*(*)       ! (O) value


! local variables


! functions
      character cfn_elem*256


! include files
      include 'utl1.inc'


! program section
! ------------------------------------------------------------------------------


      if (iarg.le.cfn_clas_narg .and. iarg.ge.0) then
         arg=cfn_elem(iarg+1,' ,',2,cfn_clas_cla)

         call cfn_clas_clr(iarg,iarg)

      else
         arg=' '
      endif



! end of program
      return
      end

! ******************************************************************************

      subroutine cfn_scla_gti(arg,val,nval,found,exitcode)

! description:
! ------------------------------------------------------------------------------
! query a number of real arguments from a Command Line Argument
! subroutine version of cfn_cla_gtr(arg,val,nval)
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none

! arguments
      character arg*(*)       ! (I) string to search

      integer   nval          ! (I) number of values

      integer   val(nval)     ! (I) default values
                              ! (O) exitcode  0  : found .true. : found values
                              !                          .false.: default  values
                              !               <>0: undefined

      integer    exitcode     ! (O)  3: too few values
                              !      2: values of wrong type
                              !      0: OK

      logical    found        ! (O) .true.  argument found
                              !     .false. argument not found or corrupt


! local variables
      integer   ret


! functions
      integer   cfn_cla_gti


! include files


! program section
! ------------------------------------------------------------------------------

! init
      exitcode = 0


! query arguments
      ret=cfn_cla_gti(arg,val,nval)


! assign status
      if (ret.eq.0) then
         found=.true.
      else if (ret.eq.-1) then
         found=.false.
      else
         ! ERROR
         found=.false.
         exitcode=abs(ret)
      endif


! end of program
      return
      end


! ******************************************************************************

      subroutine cfn_scla_gtr(arg,val,nval,found,exitcode)

! description:
! ------------------------------------------------------------------------------
! query a number of real arguments form a Command Line Argument
! subroutine version of cfn_cla_gtr(arg,val,nval)
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none

! arguments
      character arg*(*)       ! (I) string to search

      integer   nval          ! (I) number of values

      real      val(nval)     ! (I) default values
                              ! (O) exitcode  0  : found .true. : found values
                              !                          .false.: default  values
                              !               <>0: undefined

      integer    exitcode     ! (O)  3: too few values
                              !      2: values of wrong type
                              !      0: OK

      logical    found        ! (O) .true.  argument found
                              !     .false. argument not found or corrupt


! local variables
      integer   ret


! functions
      integer   cfn_cla_gtr


! include files


! program section
! ------------------------------------------------------------------------------

! init
      exitcode = 0


! query arguments
      ret=cfn_cla_gtr(arg,val,nval)


! assign status
      if (ret.eq.0) then
         found=.true.
      else if (ret.eq.-1) then
         found=.false.
      else
         ! ERROR
         found=.false.
         exitcode=abs(ret)
      endif


! end of program
      return
      end

!
!*******************************************************************************

      function cfn_compress(c)

! description:
! ------------------------------------------------------------------------------
! compressing a string
! multiple consecutive spaces or null-characters are
! compressed to 1 character
! leading spaces are removed

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      character cfn_compress*(*) ! return value: compressed value


! arguments
      character c*(*)          ! (I) string to compress


! local variables
      integer   i,n


! functions
      integer   chf_copy


! include files


! program section
! ------------------------------------------------------------------------------

      i=len(cfn_compress)

      n=chf_copy(c,len(c),cfn_compress,i)

      call cfn_s_compress(cfn_compress,i)

      return
      end


!*******************************************************************************

      function cfn_compress2(c,lc)

! description:
! ------------------------------------------------------------------------------
! compressing a string
! multiple consecutive spaces or null-characters are
! compressed to 1 character
! leading spaces are removed

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      character cfn_compress2*(*) ! return value: compressed value


! arguments
      integer   lc             ! (I) number of characters in c

      character c(lc)*1        ! (I) string to compress


! local variables
      integer   i,n


! functions
      integer   chf_copy


! include files


! program section
! ------------------------------------------------------------------------------

      i=len(cfn_compress2)

      n=chf_copy(c,lc,cfn_compress2,i)

      call cfn_s_compress(cfn_compress2,i)

      return
      end

! ******************************************************************************

      subroutine cfn_s_compress(c,lc)

! description:
! ------------------------------------------------------------------------------
! compressing a string
! multiple consecutive spaces or null-characters are
! compressed to 1 character
! leading spaces are removed
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   lc        ! (I) number of characters in c

      character c(lc)*1   ! (I/O) string to compress


! local variables
      integer   j,k

      logical   sp        ! to indicate that a space already passed


! functions


! include files


! program section
! ------------------------------------------------------------------------------




! compress
      sp=.true.   ! start as .true. to remove leading spaces

      j=1   ! position to which it should be copied
      do k=1,lc

         if (c(k).eq.' ' .or. c(k).eq.char(0)) then
            if (.not. sp) then
               sp=.true.
               c(j)=c(k)
               j=j+1
            endif
         else
            sp=.false.
            c(j)=c(k)
            j=j+1
         endif
      enddo


! fill the end of the string with spaces
      do k=j,lc
         c(k)=' '
      enddo



! end of program
      return
      end

      subroutine cfn_cp_i2i(inarr,outarr,n)

! description:
! ------------------------------------------------------------------------------
! copy n values of array inarr (integer) to outarr (integer)
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   n          ! (I) number of elements in arrays

      integer   inarr(n)   ! (I) input array
      integer   outarr(n)  ! (O) output array


! local variables
      integer   i


! functions


! include files


! program section
! ------------------------------------------------------------------------------


! copy
      do i=1,n
         outarr(i)=inarr(i)
      enddo



! end of program
      return
      end

! ******************************************************************************

      subroutine cfn_cp_i2r(inarr,outarr,n)

! description:
! ------------------------------------------------------------------------------
! copy n values of array inarr (integer) to outarr (real)
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   n          ! (I) number of elements in arrays

      integer   inarr(n)   ! (I) input array
      real      outarr(n)  ! (O) output array


! local variables
      integer   i


! functions


! include files


! program section
! ------------------------------------------------------------------------------


! copy
      do i=1,n
         outarr(i)=inarr(i)
      enddo



! end of program
      return
      end


! ******************************************************************************

      subroutine cfn_cp_i2d(inarr,outarr,n)

! description:
! ------------------------------------------------------------------------------
! copy n values of array inarr (integer) to outarr (real*8)
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   n          ! (I) number of elements in arrays

      integer   inarr(n)   ! (I) input array
      real*8    outarr(n)  ! (O) output array


! local variables
      integer   i


! functions


! include files


! program section
! ------------------------------------------------------------------------------


! copy, in reverse order in case of both arrays do occupy the same space
      do i=n,1,-1
         outarr(i)=inarr(i)
      enddo



! end of program
      return
      end

! ******************************************************************************

      subroutine cfn_cp_r2i(inarr,outarr,n)

! description:
! ------------------------------------------------------------------------------
! copy n values of array inarr (real) to outarr (integer)
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   n          ! (I) number of elements in arrays

      real      inarr(n)   ! (I) input array
      integer   outarr(n)  ! (O) output array


! local variables
      integer   i


! functions


! include files


! program section
! ------------------------------------------------------------------------------


! copy
      do i=1,n
         outarr(i)=inarr(i)
      enddo



! end of program
      return
      end

! ******************************************************************************

      subroutine cfn_cp_r2r(inarr,outarr,n)

! description:
! ------------------------------------------------------------------------------
! copy n values of array inarr (real) to outarr (real)
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   n          ! (I) number of elements in arrays

      real      inarr(n)   ! (I) input array
      real      outarr(n)  ! (O) output array


! local variables
      integer   i


! functions


! include files


! program section
! ------------------------------------------------------------------------------


! copy
      do i=1,n
         outarr(i)=inarr(i)
      enddo



! end of program
      return
      end


! ******************************************************************************

      subroutine cfn_cp_r2d(inarr,outarr,n)

! description:
! ------------------------------------------------------------------------------
! copy n values of array inarr (real) to outarr (real*8)
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   n          ! (I) number of elements in arrays

      real      inarr(n)   ! (I) input array
      real*8    outarr(n)  ! (O) output array


! local variables
      integer   i


! functions


! include files


! program section
! ------------------------------------------------------------------------------


! copy, in reverse order in case of both arrays do occupy the same space
      do i=n,1,-1
         outarr(i)=inarr(i)
      enddo



! end of program
      return
      end

! ******************************************************************************

      subroutine cfn_cp_d2i(inarr,outarr,n)

! description:
! ------------------------------------------------------------------------------
! copy n values of array inarr (real*8) to outarr (integer)
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   n          ! (I) number of elements in arrays

      real*8    inarr(n)   ! (I) input array
      integer   outarr(n)  ! (O) output array


! local variables
      integer   i


! functions


! include files


! program section
! ------------------------------------------------------------------------------


! copy
      do i=1,n
         outarr(i)=inarr(i)
      enddo



! end of program
      return
      end

! ******************************************************************************

      subroutine cfn_cp_d2r(inarr,outarr,n)

! description:
! ------------------------------------------------------------------------------
! copy n values of array inarr (real*8) to outarr (real)
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   n          ! (I) number of elements in arrays

      real*8    inarr(n)   ! (I) input array
      real      outarr(n)  ! (O) output array


! local variables
      integer   i


! functions


! include files


! program section
! ------------------------------------------------------------------------------


! copy
      do i=1,n
         outarr(i)=inarr(i)
      enddo



! end of program
      return
      end


! ******************************************************************************

      subroutine cfn_cp_d2d(inarr,outarr,n)

! description:
! ------------------------------------------------------------------------------
! copy n values of array inarr (real*8) to outarr (real*8)
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   n          ! (I) number of elements in arrays

      real*8    inarr(n)   ! (I) input array
      real*8    outarr(n)  ! (O) output array


! local variables
      integer   i


! functions


! include files


! program section
! ------------------------------------------------------------------------------


! copy
      do i=1,n
         outarr(i)=inarr(i)
      enddo



! end of program
      return
      end

! ******************************************************************************

      subroutine cfn_debug_ini(commandlinestring,ltype)

! description:
! ------------------------------------------------------------------------------
! read debug value from command line and store it into a common block
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      character commandlinestring*(*) ! (I) search string in the command line

      logical   ltype     ! (I) type command line readout that must be
                          !     used.
                          !     .true.  read out the saved string
                          !             cfn_cals_* routines
                          !             cfn_clas_ini must be
                          !             called upon for this!!!
                          !     .false. directly read from command line

! local variables
      integer   ret


! functions
      integer   cfn_clas_gti,&
                cfn_cla_gti


! include files


! common block
      integer   debugcode
      common    /cfndebugcom/debugcode


! program section
! ------------------------------------------------------------------------------

      if (ltype) then
         ret=cfn_clas_gti(commandlinestring,(/debugcode/),1)
      else
         ret=cfn_cla_gti (commandlinestring,(/debugcode/),1)
      endif

      if (ret.ne.0) then
         debugcode=0
      endif



! end of program
      return
      end

! ******************************************************************************

      function cfn_debug(code)

! description:
! ------------------------------------------------------------------------------
! routine to check if particular debug parts of a program
! must be used or not
!
! in case debugcode<0:
!        if code.eq.abs(debugcode)   then  .TRUE. else .FALSE.
!
! in case debugcode>=0:
!        if code.le.debugcode        then  .TRUE. else .FALSE.
!
!
! recommendation for use, debugcode =
!   0  : do nothing
!   1-3: only with 	ERROR
!   4-6: also with	WARNINGS
!   7-9: and also	MESSAGES
!
! use as 'code' mainly the values 2,5 and 8, in different
! cases 1 higher or lower value can be chosen
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      logical    cfn_debug   ! return value:
                             !      .true. : code satisfies the given
                             !               value
                             !      .false.: code does not


! arguments
      integer    code        ! (I) code to test


! local variables


! functions


! include files


! common block
      integer   debugcode
      common    /cfndebugcom/debugcode


! program section
! ------------------------------------------------------------------------------


! assign function value
      if (debugcode.lt.0) then
         cfn_debug=(code.eq.abs(debugcode))
      else
         cfn_debug=(code.le.debugcode)
      endif


! end of program
      return
      end

      subroutine cfn_findword(words,nwords,word,iword)

! description:
! ------------------------------------------------------------------------------
! search a given word in an array
! The given word is tested case independently and does not
! have to be complete, a unique beginning is sufficient
! related routine: cfn_findwordx

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   nwords,&           ! (I) number of words in WORDS
                iword              ! (O) >1: found position in WORDS
                                   !      0: not found
                                   !     -1: not unique

      character words(nwords)*(*),& ! (I) words which WORD must satisfy
                                   !     This words do not have to be sorted
                word*(*)           ! (I) word to search


! local variables
      integer   i,iw,lw,lws,l

      integer   lcw
      parameter (lcw=16)
      character cw1*(lcw),cw2*(lcw)

      logical   equal


! functions
      integer   cfn_length


! include files


! program section
! ------------------------------------------------------------------------------

! init
      lw =cfn_length(word)


! test
      iword=0
      do iw=1,nwords

         lws=cfn_length(words(iw))
         ! if length WORD <= length WORDS than it must be tested, otherwise, finish
         if (lw.le.lws) then

            ! test, maximum parts per length of llw
            equal=.true.
            do i=1,lw,lcw
               ! determine the end of a part of the string
               l=i+lcw-1
               l=min(l,lw)
               ! copy and make it uppercase
               cw1=words(iw)(i:l)
               cw2=word (i:l)
               call cfn_s_upcase(cw1)
               call cfn_s_upcase(cw2)
               ! test
               if (cw1.ne.cw2) equal=.false.
            enddo

            ! yes or yes
            if (equal) then
               ! check if this is the first one
               if (iword.eq.0) then
                  ! very good, found it
                  iword=iw
               else
                  ! ERROR, this is not the first one, so it is not unique
                  iword=-1
               endif
            endif

         endif

      enddo


! end of program
      return
      end

! ******************************************************************************

      subroutine cfn_findwordx(words,nwords,word,iword)

! description:
! ------------------------------------------------------------------------------
! search a given word in an array
! The given word is tested case independently and must
! completely correspond to a value from words (eXact)
! related routine: cfn_findword

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   nwords,&           ! (I) number of words in WORDS
                iword              ! (O) >1: found position in WORDS
                                   !      0: not found
                                   !     -1: not unique

      character words(nwords)*(*),& ! (I) words which WORD must satisfy
                                    !     This words do not have to be sorted
                word*(*)           ! (I) word to search


! local variables
      integer   i,iw,lw,lws,l

      integer   lcw
      parameter (lcw=16)
      character cw1*(lcw),cw2*(lcw)

      logical   equal


! functions
      integer   cfn_length


! include files


! program section
! ------------------------------------------------------------------------------

! init
      lw =cfn_length(word)


! test
      iword=0
      do iw=1,nwords

         lws=cfn_length(words(iw))
         ! if length WORD <= length WORDS than it must be tested, otherwise, finish
         if (lw.eq.lws) then

            ! test, maximum parts per length of llw
            equal=.true.
            do i=1,lw,lcw
               ! determine the end of a part of the string
               l=i+lcw-1
               l=min(l,lw)
               ! copy and make it uppercase
               cw1=words(iw)(i:l)
               cw2=word (i:l)
               call cfn_s_upcase(cw1)
               call cfn_s_upcase(cw2)
               ! test
               if (cw1.ne.cw2) equal=.false.
            enddo

            ! yes or yes
            if (equal) then
               ! check if this is the first one
               if (iword.eq.0) then
                  ! very good, found it
                  iword=iw
               else
                  ! ERROR, this is not the first time, so it is not unique
                  iword=-1
               endif
            endif

         endif

      enddo


! end of program
      return
      end

      subroutine cfn_getrec(lun,record,comm,cont)

! description:
! ------------------------------------------------------------------------------
! read in a record from a file in a character variable
! Whenever a line contains a 'comment' character, the line will be ignored
! as from that point. Whenever the last character of a line (for
! any remarks) is a 'continuation mark' then the next
! line will be read in as well.
! It will keep reading until data is found or EOF
! is reached

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   lun          ! (I) unit number from which must be
                             !     read

      character record*(*),& ! (O) character variable for output
                             !     empty if end of file has been reached
                comm*(*),&   ! (I) comment character
                             !     in case the string is longer than 1,
                             !     only the first character is used)
                cont*(*)     ! (I) continuation character
                             !     in case the string is longer than 1,
                             !     only the first character is used)

! local variables
      integer   lrec,ios


! functions


! include files


! program section
! ------------------------------------------------------------------------------

      call cfn_getrec2(lun,record,ios,lrec,comm,cont)

! end of program
      return
      end

! ******************************************************************************

      subroutine cfn_getrec2(lun,record,ios,lrec,comm,cont)

! description:
! ------------------------------------------------------------------------------
! read in a record from a file in a character variable
! Whenever a line contains a 'comment' character, the line will be ignored
! as from that point. Whenever the last character of a line (for
! any remarks) is a 'continuation mark' then the next
! line will be read in as well.
! It will keep reading until data is found or EOF
! is reached

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   lun,&        ! (I) unit number from which must be
                             !     read
                ios,&        ! (O) I/O status, 0=OK, otherwise error
                lrec         ! (O) length of the record
                             !     in case 0 than the end of the file is reached

      character record*(*),& ! (O) character variable for output
                             !     empty if end of file has been reached
                             !     comment character
                comm*(*),&   ! (I) in case the string is longer than 1,
                             !     only the first character is used)
                             !     continuation character
                cont*(*)     ! (I) in case the string is longer than 1,
                             !     only the first character is used)

! local variables
      integer   b,e,i,ls,lr,p,lmin

      character cm*1,ct*1

      logical   continue


! functions
      integer   cfn_length,&
                osd_ios


! include files


! program section
! ------------------------------------------------------------------------------

! init
      ls  = 0

! for the convenience the first characters of comm and cont are copied in
! a local variable
      cm = comm(1:1)
      ct = cont(1:1)

! empty record
      record = char(0)

      lr = len(record)   ! length of record in bytes

      p   =  1      ! first free position in the record where the next data can be stored
      lmin=p-1      ! minimum length that a record must have after reading

      continue = .true.

      do while (continue)

         if (lun.gt.0) then
            read(lun,'(a)',iostat=ios) record(p:lr)
         else
            read(*,  '(a)',iostat=ios) record(p:lr)
         endif

         if (ios.eq.0) then

            ! remove comments if necessary
            call cfn_elem_be(1,cm,1,record(p:lr),lr-p+1,b,e)
            b=b+p-1
            e=e+p-1
            ! whenever the comments start on position 1 of the imported line
            ! nothing can be added to the record
            if (b.gt.0 .and. e.ge.b) then
               record(p:)=record(b:e)
            else
               record(p:)=' '
            endif

            ! determine the length of the string
            ls = cfn_length(record)
            ls = max(ls,lmin)

            ! remove if necessary ^M (CR) at the end of the record
            if (ls.gt.0) then
               if (record(ls:ls).eq.char(13)) then
                  record(ls:ls)=char(0)
                  ls=ls-1
               endif
            endif

            ! remove if necessary leading spaces
            i=p
            do while (i.le.ls .and. record(i:i).eq.' ')
               i=i+1
            enddo
            record(p:)=record(i:)

            ls = cfn_length(record)
            ls = max(ls,lmin)
            p  = ls+1  ! next position

            ! check if there is a continuation mark
            if (ls.gt.0) then
               if (record(ls:ls).eq.ct .and. ls.gt.lmin) then
                  record(ls:)=' '
                  ls=ls-1
                  p=p-1

               else

                  ! nope, if length record > 0 at the moment, finish
                  if (ls.gt.0) then
                     continue=.false.
                  endif

               endif

               ! record full?
               if (p.gt.lr .and. continue) then
                  write(*,*) 'ERROR. record variable too small!'
                  continue=.false.
               endif
            endif

            ! determine new minimum length
            lmin=ls

         else

! ios<>0, check what happened
            if (ios.ne.osd_ios('EOF ')) then
               write(*,*)&
                 'ERROR. reading in record ended with I/O status ',ios
!         read(lun,'(a)') record(p:lr)
            else
               ios=0   ! EOF is good
            endif

            continue=.false.

         endif

      enddo

      if (ios.eq.0) then
         lrec=ls
      else
         lrec=0
      endif


! end of program
      return
      end

!> description
!! Utility routines to perform calculations with (Modified) Julian Date

!> calculate Julian Date from yyyymmdd:HH:MM:SS
subroutine cfn_datehms2jd(date,hour,minute,seconds,jd)

! declaration section
! ------------------------------------------------------------------------------

 implicit none


! arguments
 integer  , intent(in)         :: date     !> date in format yyyymmdd
 integer  , intent(in)         :: hour     !> hour    part of the time
 integer  , intent(in)         :: minute   !> minute  part of the time
 integer  , intent(in)         :: seconds  !> seconds part of the time

 double precision, intent(out) :: jd       !> Julian Date value calculated from yyyymmdd:HH:MM:SS


! local variables
 double precision  :: year,month,day,a,y,m,jdn

 double precision, parameter :: jdnodata=-9.9998D307
 integer         , parameter :: datenodata=-2147483646

! program section
! ------------------------------------------------------------------------------

 if (date.ne.datenodata) then
   ! Algorithm from wikipedia (6-Jun-2011)
    year =int(date/10000)
    month=mod(int(date/100),100)
    day  =mod(date,100)

    a = int((14-month)/12)
    y = year + 4800 - a
    m = month + 12*a - 3

   ! julian day number
    jdn = day + int((153*m + 2)/5) + 365*y + int(y/4) - int(y/100) + int(y/400) - 32045
   ! julian date
    jd  = jdn + (hour-12.D0)/24.D0 + minute/1440.D0 + seconds/86400.D0
 else
    jd = jdnodata
 endif

! end of program
 return
end

! ******************************************************************************

!> calculate  yyyymmdd:HH:MM:SS from Julian Date
subroutine cfn_jd2datehms(jd,date,hour,minute,seconds)

! declaration section
! ------------------------------------------------------------------------------

 implicit none


! arguments
 integer  , intent(out)        :: date     !> date in format yyyymmdd
 integer  , intent(out)        :: hour     !> hour    part of the time
 integer  , intent(out)        :: minute   !> minute  part of the time
 integer  , intent(out)        :: seconds  !> seconds part of the time

 double precision, intent(in)  :: jd       !> Julian Date value to calculate yyyymmdd:HH:MM:SS from


! local variables
 integer  :: year,month,day,j,g,dg,c,dc,b,db,a,da,y,m,d !,s

 double precision            :: fjd                     ! fraction jd+0.5
 double precision, parameter :: jdnodata=-9.9998D307
 integer         , parameter :: datenodata=-2147483646

! program section
! ------------------------------------------------------------------------------

 if (jd.ne.jdnodata) then
   ! Algorithm from wikipedia (6-Jun-2011)
    j = jd + 0.5 + 32044
    g = j/146097
    dg= mod(j,146097)
    c = int(dg/36524 + 1 ) * 3 /4
    dc= dg-c*36524
    b = dc/1461
    db= mod(dc,1461)
    a = int(db/365 + 1 ) * 3 / 4
    da= db - a*365
    y = g*400 + c*100 + b*4 + a
    m = (da*5 + 308)/153 - 2
    d = da - (m+4)*153/5 + 122

    year = y - 4800 + (m+2)/12
    month= mod(m+2,12) + 1
    day  = d + 1

    date = year*10000 + month*100 + day


   ! hour,minute,seconds
    fjd     = jd+0.5d0
    fjd     = fjd - dnint(fjd)   ! fractional day
    fjd     = fjd*24.d0          ! day -> hours
    hour    = int(fjd)
    fjd     = fjd - hour
    fjd     = fjd*60.d0          ! hours -> minutes
    minute  = int(fjd)
    fjd     = fjd - minute
    fjd     = fjd*60.d0          ! minutes -> seconds
    seconds = nint(fjd)
 else
    ! missing values
    date    = datenodata
    hour    = datenodata
    minute  = datenodata
    seconds = datenodata
 endif


! end of program
 return
end

! ******************************************************************************

!> calculate Modified Julian Date from yyyymmdd:HH:MM:SS
subroutine cfn_datehms2mjd(date,hour,minute,seconds,mjd)

! declaration section
! ------------------------------------------------------------------------------

 implicit none


! arguments
 integer  , intent(in)         :: date     !> date in format yyyymmdd
 integer  , intent(in)         :: hour     !> hour    part of the time
 integer  , intent(in)         :: minute   !> minute  part of the time
 integer  , intent(in)         :: seconds  !> seconds part of the time

 double precision, intent(out) :: mjd      !> Modified Julian Date value calculated from yyyymmdd:HH:MM:SS


! local variables
 double precision  :: jd

 double precision, parameter :: jdnodata=-9.9998D307


! program section
! ------------------------------------------------------------------------------

! MJD = JD - 2400000.5
! Algorithm from wikipedia (6-Jun-2011)
 call cfn_datehms2jd(date,hour,minute,seconds,jd)

! result
 if (jd.ne.jdnodata) then
    mjd = jd - 2400000.5D0
 else
    mjd = jdnodata
 endif


! end of program
 return
end

! ******************************************************************************

!> calculate Modified Julian Date from yyyymmdd:HH:MM:SS
subroutine cfn_mjd2datehms(mjd,date,hour,minute,seconds)

! declaration section
! ------------------------------------------------------------------------------

 implicit none


! arguments
 integer  , intent(out)        :: date     !> date in format yyyymmdd
 integer  , intent(out)        :: hour     !> hour    part of the time
 integer  , intent(out)        :: minute   !> minute  part of the time
 integer  , intent(out)        :: seconds  !> seconds part of the time

 double precision, intent(in)  :: mjd      !> Modified Julian Date value calculated from yyyymmdd:HH:MM:SS


! local variables
 double precision  :: jd

 double precision, parameter :: jdnodata=-9.9998D307


! program section
! ------------------------------------------------------------------------------

! MJD = JD - 2400000.5
! Algorithm from wikipedia (6-Jun-2011)

 ! result
 if (mjd.ne.jdnodata) then
    jd = mjd + 2400000.5D0
 else
    jd = jdnodata
 endif

 call cfn_jd2datehms(jd,date,hour,minute,seconds)

! end of program
 return
end

! ******************************************************************************

!> description
!! Round the value of jd to whole seconds.
!! The binary representation of whole seconds will be in most cases
!! a recurring decimal in binary representation.
!! This routine rounds the fraction of jd to the nearest value with a binary
!! representation of at most 17 bits.
!! This will round the value to an accuracy of about 0.3 seconds
function cfn_jd_round(jd)

! declaration section
! ------------------------------------------------------------------------------

 implicit none


! function declaration
 double precision  cfn_jd_round   !> return value: value of jd round to seconds


! arguments
 double precision, intent(in)    :: jd              !> Julian Date


! local variables
 double precision, parameter :: roundfactor=131072. !> this is the first power
                                                    !! of 2 (2**17) after the number of
                                                    !! seconds in one day (86400)

 double precision, parameter :: jdnodata=-9.9998D307


! program section
! ------------------------------------------------------------------------------


! assign function value
 if (jd.ne.jdnodata) then
    cfn_jd_round  = dnint(jd*roundfactor)/roundfactor
 else
    cfn_jd_round  = jd
 endif

! end of program
 return
end


! ******************************************************************************

!> description
!! Round the value of mjd to whole seconds.
!! Modified Julian Date version of cfn_jd_round() function
function cfn_mjd_round(mjd)

! declaration section
! ------------------------------------------------------------------------------

 implicit none


! function declaration
 double precision  cfn_mjd_round   !> return value: value of jd round to seconds


! arguments
 double precision, intent(in)    :: mjd              !> Julian Date


! local variables
 double precision, parameter :: jdnodata=-9.9998D307

! functions
 double precision :: cfn_jd_round

! program section
! ------------------------------------------------------------------------------


! assign function value
 if (mjd.ne.jdnodata) then
    cfn_mjd_round = cfn_jd_round(mjd + 2400000.5D0) - 2400000.5D0
 else
    cfn_mjd_round = mjd
 endif


! end of program
 return
end

! ******************************************************************************

!> function to get the difference between Julian Dates jd1 and jd2
!! If one or both of the values contain a nodata value the result will be nodata
double precision function cfn_jd_delta(jd1,jd2)
 implicit none
 double precision, intent(in) :: jd1,jd2
 ! local variables
 double precision             :: ljd1,ljd2,nodata
 double precision, parameter  :: roundfactor=131072.d0

 ! functions
 double precision :: cfn_jd_nodata

 ! ----
 nodata=cfn_jd_nodata()
 if (jd1.eq.nodata .or. jd2.eq.nodata) then
    ! result is nodata
    cfn_jd_delta=nodata
 else
    ljd1=dnint(jd1*roundfactor)
    ljd2=dnint(jd2*roundfactor)
    cfn_jd_delta=(ljd1-ljd2)/roundfactor
 endif
 return
end
! ****
double precision function cfn_mjd_delta(mjd1,mjd2)
 implicit none
 double precision, intent(in) :: mjd1,mjd2
 ! functions
 double precision :: cfn_jd_delta
 ! same routine may be used as for jd
 cfn_mjd_delta=cfn_jd_delta(mjd1,mjd2)
 return
end
! ******************************************************************************

!> function to get the nodata value used for Julian Dates
double precision function cfn_jd_nodata()
 implicit none

! assign function value
 cfn_jd_nodata = -9.9998D307

 return
end

! ******************************************************************************

!> function to get the nodata value used for Modified Julian Dates
double precision function cfn_mjd_nodata()
 implicit none
 double precision cfn_jd_nodata   ! function

! assign function value
 cfn_mjd_nodata = cfn_jd_nodata()

 return
end


      function cfn_n_elem(st,as,string)

! description:
! ------------------------------------------------------------------------------
! count the number of elements in a string
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer cfn_n_elem        ! return value: : number of elements
                                !               :


! arguments
      integer   as              ! (I)

      character st(as)*1,&      ! (I)
                string*(*)      ! (I)


! local variables
      integer   lstring,i


! functions
      integer   cfn_length,&
                cfn_n_elem2


! include files


! program section
! ------------------------------------------------------------------------------


      lstring = cfn_length(string)

! assign function value
      cfn_n_elem = cfn_n_elem2(st,as,string,lstring)


! end of program
      return
      end

! ******************************************************************************

      function cfn_n_elem2(st,as,string,lstring)

! description:
! ------------------------------------------------------------------------------
! count the number of elements in a string
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer cfn_n_elem2          ! return value: : number of elements
                                   !               :


! arguments
      integer   as,&               ! (I)
                lstring            ! (I)

      character st(as)*1,&         ! (I)
                string(lstring)*1  ! (I)


! local variables
      integer   i,n,nr

      logical   continue


! functions
      logical   in_char,cfn_een_van


! include files


! program section
! ------------------------------------------------------------------------------


      n  = 0                    ! counted number of elements

      nr = 0 ! element number on which I am working now

      i  = 1

      do while (i.le.lstring)
         nr=nr+1

! whenever st(1) = ' ' then the spaces at the beginning must be skipped

         if (st(1).eq.' ') then
            do while(string(i).eq.' ' .and. i.lt.lstring)
               i=i+1
            enddo
         endif

! search for a separator

!         do while (i.le.lstring  .and.
!     1             .not.cfn_een_van(string(i),st,as))

         continue=.true.
         if (i.gt.lstring) continue=.false.
         do while (continue)

            if (cfn_een_van(string(i),st,as)) then

               continue=.false.

            else

               if (string(i).eq.'''') then
!                we are entering a character string. we need to get
!                out of it as soon as possible.
                  in_char=.true.
                  do while (in_char .and. i.lt.lstring)
                     i=i+1
                     if (string(i).eq.'''') then
                        i=i+1
                        if (i.le.lstring) then
                           if (string(i).ne.'''') then
!              we are out
                              in_char=.false.
                           endif
                        endif
                     endif
                  enddo
               else

                  i=i+1
               endif
               if (i.gt.lstring) continue=.false.
            endif

         enddo

!

         if (st(1).eq.' ' .and. i.lt.lstring) then
! skip the spaces until the next non-space
            do while(i.lt.lstring .and. string(i).eq.' ')
               i=i+1
            enddo
         endif

!
         if (i.le.lstring) then
            if (cfn_een_van(string(i),st,as)) then
               i=i+1 ! this will be the start of the next element
            endif
         endif

      enddo


! assign function value
      cfn_n_elem2 = nr


! end of program
      return
      end

      subroutine cfn_rtt_init(dbgcode)

! description:
! ------------------------------------------------------------------------------
! routine timer init
! by means of cfn_rttimer_* the elapsed time of different routines and
! parts of a program can be easily determined.
!    cfn_rtt_init    initialize the timer, the timer is only used
!                        whenever the dbgcode satisfies the debug
!                        status of the process (see cfn_debug)
!    cfn_rtt_strt    Start timing a routine
!    cfn_rtt_end     End timing a routine
!    cfn_rtt_list    Gives a overview of everything that has been timed so far

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   dbgcode       ! (I) debugcode by which the timer starts
                              !     dbgcode>=0: see cfn_debug
                              !             -1: timer on
                              !             -2: timer off

! local variables


! functions
      integer   osd_time

      logical   cfn_debug


! include files
      include 'utl2.inc'


! program section
! ------------------------------------------------------------------------------

! test if timer must be on or off
      if (dbgcode.lt.0) then
         if (dbgcode.eq.-1) then
            lrtton=.true.
         else
            lrtton=.false.
         endif
      else
         if (cfn_debug(dbgcode)) then
            lrtton=.true.
         else
            lrtton=.false.
         endif
      endif


! init whenever the timer must switch on
      if (lrtton) then
         ! init
         nrtt  =0
         lrttoverflow=.false.
         crttids(0)  ='Total   '
         irtttot(1,0)=0
         irtttot(2,0)=0
         irtttot(3,0)=osd_time()
      endif


! end of program
      return
      end

! ******************************************************************************

      subroutine cfn_rtt_strt(rttid)

! description:
! ------------------------------------------------------------------------------
! routine timer init
! by means of cfn_rttimer_* the elapsed time of different routines and
! parts of a program can be easily determined.
!    cfn_rtt_init    initialize the timer, the timer is only used
!                        whenever the dbgcode satisfies the debug
!                        status of the process (see cfn_debug)
!    cfn_rtt_strt    Start timing a routine
!    cfn_rtt_end     End timing a routine
!    cfn_rtt_list    Gives a overview of everything that has been timed so far

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      character rttid*(*)     ! (I) routine id


! local variables
      integer   i,j

      character lrttid*8


! functions
      integer   osd_time

      character cfn_lowcase*8,cfn_trim*8


! include files
      include 'utl2.inc'


! program section
! ------------------------------------------------------------------------------

! is the timer on?
      if (.not. lrtton) return

! init lrttid
      lrttid=rttid
      lrttid=cfn_lowcase(cfn_trim(lrttid))


! search whether or not lrttid already occurs
      j=nrtt+1
      do i=1,nrtt
         if (crttids(i).eq.lrttid) j=i
      enddo

! check if there still is space in the array
      if (j.gt.mxtimr) then
         lrttoverflow=.true.
      else

         if (j.gt.nrtt) then
            ! new
            nrtt=j
            crttids(j)=lrttid
            irtttot(1,j)=0
            irtttot(2,j)=0
         else
            ! existing
         endif

         irtttot(3,j)=osd_time()

      endif


! end of program
      return
      end


! ******************************************************************************

      subroutine cfn_rtt_end(rttid)

! description:
! ------------------------------------------------------------------------------
! routine timer init
! by means of cfn_rttimer_* the elapsed time of different routines and
! parts of a program can be easily determined.
!    cfn_rtt_init    initialize the timer, the timer is only used
!                        whenever the dbgcode satisfies the debug
!                        status of the process (see cfn_debug)
!    cfn_rtt_strt    Start timing a routine
!    cfn_rtt_end     End timing a routine
!    cfn_rtt_list    Gives a overview of everything that has been timed so far

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      character rttid*(*)     ! (I) routine id


! local variables
      integer   i,j,d

      character lrttid*8


! functions
      integer   osd_time

      character cfn_lowcase*8,cfn_trim*8


! include files
      include 'utl2.inc'


! program section
! ------------------------------------------------------------------------------

! is the timer on?
      if (.not. lrtton) return

! init lrttid
      lrttid=rttid
      lrttid=cfn_lowcase(cfn_trim(lrttid))


! search whether or not lrttid already occurs
      j=0
      do i=1,nrtt
         if (crttids(i).eq.lrttid) j=i
      enddo


! check if the timer had started
      if (j.gt.0) then

         d=osd_time()-irtttot(3,j)

         irtttot(1,j)=irtttot(1,j)+d
         irtttot(2,j)=irtttot(2,j)+1
         irtttot(3,j)=0

      endif


! end of program
      return
      end

! ******************************************************************************

      subroutine cfn_rtt_list(lun)

! description:
! ------------------------------------------------------------------------------
! routine timer init
! by means of cfn_rttimer_* the elapsed time of different routines and
! parts of a program can be easily determined.
!    cfn_rtt_init    initialize the timer, the timer is only used
!                        whenever the dbgcode satisfies the debug
!                        status of the process (see cfn_debug)
!    cfn_rtt_strt    Start timing a routine
!    cfn_rtt_end     End timing a routine
!    cfn_rtt_list    Gives a overview of everything that has been timed so far

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   lun    ! (I) unit number to write output files to
                       !     <=0 is standard output


! local variables
      integer   j,l1,l2,l3,d,t

      character string1*80,string2*80,string3*80

      real      p


! functions


! include files
      integer   osd_time,&
                cfn_length

      include 'utl2.inc'


! program section
! ------------------------------------------------------------------------------

! is the timer on?
      if (.not. lrtton) return

! init strings
      string1=' RTT overview'
      string2=' id         time (s)          %     number'
      string3=' -------- ---------- ---------- ----------'

      l1=cfn_length(string1)
      l2=cfn_length(string2)
      l3=cfn_length(string3)


! total time
      j=0
      t=osd_time()
      d=t-irtttot(3,j)

      irtttot(1,j)=irtttot(1,j)+d
      irtttot(2,j)=irtttot(2,j)+1
      irtttot(3,j)=t


! overview
      if (lun.gt.0) then
         write(lun,'(3(/,a))') string1(1:l1),string2(1:l2),string3(1:l3)
         do j=1,nrtt
            p=100.*irtttot(1,j)/max(1,irtttot(1,0))
            write(lun,'(1x,a8,1x,i10,1x,f10.2,1x,i10)')&
                  crttids(j),irtttot(1,j),p,irtttot(2,j)
         enddo
         write(lun,'(a,/)') string3(1:l3)
         write(lun,'(1x,a8,1x,i10,1x,f10.2,1x,i10)')&
                  crttids(0),irtttot(1,0),100.,irtttot(2,0)
         write(lun,'(a,/)') string3(1:l3)
      else
         write(*  ,'(3(/,a))') string1(1:l1),string2(1:l2),string3(1:l3)
         do j=1,nrtt
            p=100.*irtttot(1,j)/max(1,irtttot(1,0))
            write(*  ,'(1x,a8,1x,i10,1x,f10.2,1x,i10)')&
                  crttids(j),irtttot(1,j),p,irtttot(2,j)
         enddo
         write(*  ,'(a,/)') string3(1:l3)
         write(*  ,'(1x,a8,1x,i10,1x,f10.2,1x,i10)')&
                  crttids(0),irtttot(1,0),100.,irtttot(2,0)
         write(*  ,'(a,/)') string3(1:l3)
      endif


! end of program
      return
      end

      subroutine cfn_token(string,oper)

! description:
! ------------------------------------------------------------------------------
! combination of one or more string operations
! operation consist of one or more letters, one letter for an operation
!   t: trim
!   l: lowercase
!   u: uppercase
!   c: collapse, remove all spaces
!   p: compress, substitute multiple spaces by 1 space
!   q: unquote

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      character (len=*), intent(inout) :: string          ! to be converted string
      character (len=*), intent(in)    :: oper            ! operations to perform


! local variables
      integer   i,ls,lo


! functions
      integer   cfn_length,cfn_unquote2


! program section
! ------------------------------------------------------------------------------

! get number of operations
      lo=cfn_length(oper)
      ls=cfn_length(string)


! perform
      do i=1,lo
         if (ls.gt.0) then
            select case( oper(i:i) )
               case( 't','T' )
                  call cfn_s_trim2(string,ls)

               case( 'l','L' )
                  call cfn_s_lowcase(string(1:ls))

               case( 'u','U' )
                  call cfn_s_upcase2(string,ls)

               case( 'c','C' )
                  call cfn_s_collapse2(string,ls)

               case( 'p','P' )
                  call cfn_s_compress(string,ls)

               case( 'q','Q' )
                  ls=cfn_unquote2(string,ls)

               case default
                  ! ERROR???

            end select
         endif
      enddo


! end of program
      return
      end

      function cfn_unique_i(array,nin,mv)

! description:
! ------------------------------------------------------------------------------
! takes the unique values from an array and places them sorted in
! the first cells

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer    cfn_unique_i ! return value: number of unique values
                              !               0 only with mv in array


! arguments
      integer   nin           ! (I) length array

      integer   array(nin),&  ! (I/O) in : input array
                              !       out: unique, sorted values
                mv            ! (I)   missing value


! local variables
      integer   n,nf,i,j,index

      integer   val           ! must be the same type as array()


! functions
      integer   cfn_idx_get_i


! include files


! program section
! ------------------------------------------------------------------------------

! find unique values
      n=0   ! number of unique values found
      do i=1,nin
         val=array(i)
         if (val.ne.mv) then
            if (n.eq.0) then
               ! first not "missing value" found
               n=n+1
               array(n)=val
            else
               nf=cfn_idx_get_i(val,array,n,index)
               if (nf.eq.0) then
                  ! none found, index now is the position where the element
                  ! must be inserted
                  do j=n,index,-1
                     array(j+1)=array(j)
                  enddo
                  n=n+1
                  array(index)=val
               endif
            endif
         endif
      enddo


! number of found values returned as output
      cfn_unique_i=n


! end of program
      return
      end

! ******************************************************************************

      function cfn_unique_r(array,nin,mv)

! description:
! ------------------------------------------------------------------------------
! takes the unique values from an array and places them sorted in
! the first cells

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer    cfn_unique_r ! return value: number of unique values
                              !               0 only with mv in array


! arguments
      integer   nin           ! (I) length array

      real      array(nin),&  ! (I/O) in : input array
                              !       out: unique, sorted values
                mv            ! (I)   missing value


! local variables
      integer   n,nf,i,j,index

      real      val     ! must be the same type as array()


! functions
      integer   cfn_idx_get_r


! include files


! program section
! ------------------------------------------------------------------------------

! find unique values
      n=0   ! number of unique values found
      do i=1,nin
         val=array(i)
         if (val.ne.mv) then
            if (n.eq.0) then
               ! first not "missing value" found
               n=n+1
               array(n)=val
            else
               nf=cfn_idx_get_r(val,array,n,index)
               if (nf.eq.0) then
                  ! none found, index now is the position where the element
                  ! must be inserted
                  do j=n,index,-1
                     array(j+1)=array(j)
                  enddo
                  n=n+1
                  array(index)=val
               endif
            endif
         endif
      enddo


! number of found values returned as output
      cfn_unique_r=n


! end of program
      return
      end

! ******************************************************************************

      function cfn_unique_d(array,nin,mv)

! description:
! ------------------------------------------------------------------------------
! takes the unique values from an array and places them sorted in
! the first cells

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer    cfn_unique_d ! return value: number of unique values
                              !               0 only with mv in array


! arguments
      integer   nin           ! (I) length array

      double precision &
                array(nin),&  ! (I/O) in : input array
                              !       out: unique, sorted values
                mv            ! (I)   missing value


! local variables
      integer   n,nf,i,j,index

      double precision val     ! must be the same type as array()


! functions
      integer   cfn_idx_get_d


! include files


! program section
! ------------------------------------------------------------------------------

! find unique values
      n=0   ! number of unique values found
      do i=1,nin
         val=array(i)
         if (val.ne.mv) then
            if (n.eq.0) then
               ! first not "missing value" found
               n=n+1
               array(n)=val
            else
               nf=cfn_idx_get_d(val,array,n,index)
               if (nf.eq.0) then
                  ! none found, index now is the position where the element
                  ! must be inserted
                  do j=n,index,-1
                     array(j+1)=array(j)
                  enddo
                  n=n+1
                  array(index)=val
               endif
            endif
         endif
      enddo


! number of found values returned as output
      cfn_unique_d=n


! end of program
      return
      end

! ******************************************************************************

      function cfn_unique_c(array,la,nin,mv,lm)

! description:
! ------------------------------------------------------------------------------
! takes the unique values from an array and places them sorted in
! the first cells

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer    cfn_unique_c ! return value: number of unique values
                              !               0 only with mv in array


! arguments
      integer   nin,&         ! (I) length array
                la,&          ! (I) number of characters per element
                lm            ! (I) number of characters in mv

      character array(la,nin)*1,&! (I/O) in : input array
                              !       out: unique, sorted values
                mv(lm)*1      ! (I)   missing value


! local variables
      integer   n,nf,i,j,k,index,lar,lmv

      character val*1024      ! must be the same type as array()


! functions
      integer   cfn_idx_get_c,&
                cfn_length2,&
                chf_copy

      logical   chf_ne


! include files


! program section
! ------------------------------------------------------------------------------

!
!      write(*,*) loc(mv),len(mv)
      lmv=cfn_length2(mv,lm)

! find unique values
      n=0   ! number of unique values found
      do i=1,nin
         lar=cfn_length2(array(1,i),la)
         if (chf_ne(array(1,i),lar,mv,lmv)) then
            if (n.eq.0) then
               ! first not "missing value" found
               n=n+1
               k=chf_copy(array(1,i),la,array(1,n),la)
            else
               nf=cfn_idx_get_c(array(1,i),lar,array,la,n,index)
               if (nf.eq.0) then
                  ! none found, index now is the position where the element
                  ! must be inserted
                  k=chf_copy(array(1,i),la,val,len(val))
                  do j=n,index,-1
                     k=chf_copy(array(1,j),la,array(1,j+1),la)
                  enddo
                  n=n+1
                  k=chf_copy(val,len(val),array(1,index),la)
               endif
            endif
         endif
      enddo


! number of found values returned as output
      cfn_unique_c=n


! end of program
      return
      end


      function cfn_unquote(string)

! description:
! ------------------------------------------------------------------------------
! unquote a character string
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   cfn_unquote    ! return value: >=0: number of characters left


! arguments
      character string*(*)            ! (I/O) to be unquoted string
                                      !       it will be returned unquoted


! local variables
      integer   lstring


! functions
      integer   cfn_unquote2


! include files


! program section
! ------------------------------------------------------------------------------


      lstring=len(string)

      cfn_unquote=cfn_unquote2(string,lstring)


! end of program
      return
      end

! ******************************************************************************

      function cfn_unquote2(string,lstring)

! description:
! ------------------------------------------------------------------------------
! unquote a character string
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   cfn_unquote2   ! return value: >=0: number of characters left


! arguments
      integer   lstring               ! (I)   number of character in string

      character string(lstring)*1     ! (I/O) to be unquoted string
                                      !       it will be returned unquoted


! local variables
      integer   l,i,pt,pf

      character quote*1


! functions
      integer   cfn_length2


! include files


! program section
! ------------------------------------------------------------------------------


      quote =''''


      call cfn_s_trim2(string,lstring)

      l=cfn_length2(string,lstring)

      if (string(1).eq.quote .and. string(l).eq.quote) then
         pt=1 ! position to which it must be copied
         pf=2 ! position from which it must be copied


         do while (pf.lt.l)
            string(pt)=string(pf)
            pt=pt+1
            pf=pf+1
            if (string(pf).eq.quote .and. string(pf+1).eq.quote) then
! make single quotes from double quotes (skip the next quote)
               pf=pf+1
            endif
         enddo

! fill last part with spaces
         do i=pt,l
            string(i)=' '
         enddo

      endif


! fill in return value
      cfn_unquote2=cfn_length2(string,lstring)


! end of program
      return
      end
!> Usefull vcl-routines
!! ====================
!! call cfn_vcl_set(cl,ivcl)                                                    ! init command line
!! call cfn_vcl_narg(ivcl,narg)                                                 ! get (current) number of arguments
!! call cfn_vcl_arg(ivcl,iarg,arg,larg)                                         ! get argument number iarg
!! call cfn_vcl_fnd(ivcl,iarg,arg,remove[,found])                               ! find an argument and return iarg
!! call cfn_vcl_fnd{i,r,d}(ivcl,iarg,arg,remove,value,nval[,found])             ! find an argument and return iarg and nval values
!! call cfn_vcl_inp{i,r,c}(ivcl,string,value,ldefault,lval,uval,ninterval,iarg) ! get argument/input
!! call cfn_vcl_eva(ivcl,eva,remove)                                            ! get all eva-variables from command line
!! call cfn_vcl_argsleft(ivcl,lprint,exitcode)                                  ! check or any arguments are left


module m_vcl
   ! Virtual command line storage
   ! ===========================
   ! Several Components of a program may use a different command line string
   ! Advantages:
   ! - specific parts of a program can use different 'command lines'
   ! - when a command line argument is used it can be removed from the 'line'
   ! - When the program part is ready processing the command line it can
   !   check if unused arguments are left. This may cause an error.
   ! - specific options can be used from the command line first before
   !   processing the remaining arguments.

   integer, parameter :: lcl=128     ! length of command line parts
   type vclstruct
      integer                          :: narg     ! number of args left
      integer                          :: mxarg    ! number of args stored
   !   integer, allocatable             :: pos(:)   ! start position in cl
   !   integer, allocatable             :: arg(:)   ! argument numbers which are left
   !                                                ! after use
   !   character (len=lcl), allocatable :: cl(:)
      integer, pointer                 :: pos(:)   ! start position in cl
      integer, pointer                 :: arg(:)   ! argument numbers which are left
                                                   ! after use
      character (len=lcl), pointer     :: cl(:)
   end type
   type pvclstruct
      type(vclstruct), pointer :: pvcl
   end type
   type(pvclstruct), pointer, save :: vcl(:)

   integer, save :: nvcl=0
   integer, save :: mxvcl=0

   character (len=1024) :: targ    ! variable to store arguments temporarily

end module
! ******************************************************************************
subroutine cfn_vcl_set(cl,ivcl)

! description:
! ------------------------------------------------------------------------------
! Virtual command line
! Set a virtual command line

! declaration section
! ------------------------------------------------------------------------------
 use m_vcl

 implicit none


! arguments
 character (len=*), intent(in)   :: cl    ! text for the command line to store
                                          ! If it is empty the real command line is used
 integer  , intent(out)          :: ivcl  ! Index of data structure where the data is stored
                                          ! This index is needed by the calling program
                                          ! when trying to get an argument

! local variables
 integer   i,na,ns,b,e,ic,ir,p,n,n1,n2,l

 integer   as
 character st*1

 logical   usecl

 type(pvclstruct), pointer :: tvcl(:)
 type(vclstruct) , pointer :: pv

! functions
 integer   cfn_length,osd_iargc,cfn_n_elem,cfn_elem_pos


! program section
! ------------------------------------------------------------------------------

! init
 st=' '
 as=1


! check allocation of pointer array
 if (.not.associated(vcl)) then
    ! allocate structure
    mxvcl=10
    allocate(vcl(mxvcl))
 endif

! check size of pointer array
 if (nvcl.ge.mxvcl) then
    ! create larger array
    mxvcl=int(1.5*max(nvcl,mxvcl))
    allocate(tvcl(mxvcl))
    ! copy data
    do i=1,nvcl
       tvcl(i)%pvcl=>vcl(i)%pvcl
       nullify(vcl(i)%pvcl)
    enddo
    ! move pointer
    nullify(vcl)
    vcl=>tvcl
    nullify(tvcl)
 endif


! store cl
 nvcl=nvcl+1
 ivcl=nvcl
 ! pointer
 allocate(vcl(ivcl)%pvcl)
 pv=>vcl(ivcl)%pvcl

 l=cfn_length(cl)
 if (l.gt.0) then
    ! cl contains data, use this
    usecl=.true.
 else
    ! use real command line data
    usecl=.false.
 endif

! determine array sizes
 if (usecl) then

    ! number of arguments
    na=cfn_n_elem(st,as,cl)

 else
     ! number of arguments
    na=osd_iargc()

    ! get total command line length
    l=0
    do i=1,na
       call osd_getarg(i,targ)
       l=l+cfn_length(targ)
    enddo

 endif

 ! calculate number of sub-strings to be used
 ns=int((l+lcl-1)/lcl)


! allocate
 allocate(pv%pos(na+1))
 allocate(pv%arg(na))
 allocate(pv%cl(ns))

! store
 ir=1  ! row    number in pv%cl
 ic=1  ! column number in pv%cl
 p =1  ! position in cl to search for the next argument
 do i=1,na
    ! get argument
    if (usecl) then
       n=cfn_elem_pos(1,st,as,cl(p:l),l-p+1,b,e)
       ! copy sub string
       targ=cl(b+p-1:e+p-1)
       ! new position for p
       p=p+e

       e=e-b+1
       b=1
    else
       call osd_getarg(i,targ)
       e=cfn_length(targ)
       b=1
    endif

    ! store
    pv%pos(i)=(ir-1)*lcl+ic
    do while (b.le.e)
       n1=e-b+1     ! number of characters left to store
       n2=lcl-ic+1  ! number of positions left at current row
       n=min(n1,n2) ! number of characters to store in current row

       pv%cl(ir)(ic:ic+n-1)=targ(b:b+n-1)
       ! next
       b=b+n
       ic=ic+n
       if (ic.gt.lcl) then
          ic=ic-lcl
          ir=ir+1
       endif
    enddo

 enddo
 ! store last position
 pv%pos(na+1)=(ir-1)*lcl+ic
 ! store number of arguments
 pv%narg =na
 pv%mxarg=na

 ! fill arg
 do i=1,na
    pv%arg(i)=i
 enddo


! end of program
 return
end

! ******************************************************************************

subroutine cfn_vcl_narg(ivcl,narg)

! description:
! ------------------------------------------------------------------------------
! get number of arguments stored for ivcl
!

! declaration section
! ------------------------------------------------------------------------------
 use m_vcl

 implicit none


! arguments
 integer  , intent(in)     :: ivcl      ! index structure number
 integer  , intent(out)    :: narg      ! current number of arguments available


! local variables
 type(vclstruct) , pointer :: pv


! program section
! ------------------------------------------------------------------------------

! check ivcl
 if (ivcl.lt.1 .or. ivcl.gt.nvcl) then
    ! ERROR, index not defined
    write(*,*) ' ERROR, cfn_vcl, ivcl not defined: ',ivcl
    narg=-1
 else
    pv=>vcl(ivcl)%pvcl
    narg=pv%narg
 endif


! end of program
 return
end


! ******************************************************************************

subroutine cfn_vcl_arg(ivcl,iarg,arg,larg)

! description:
! ------------------------------------------------------------------------------
! get one argument of vcl
!

! declaration section
! ------------------------------------------------------------------------------
 use m_vcl

 implicit none


! arguments
 integer  , intent(in)          :: ivcl    ! index structure number
 integer  , intent(in)          :: iarg    ! argument number to get
                                           !  >0: get argument number iarg
                                           !  <0: get argument number abs(iarg)
                                           !      remove argument from the list

 character (len=*), intent(out) :: arg     ! argument string
 integer          , intent(out) :: larg    ! length of argument


! local variables
 integer   ia,n1,n2,n,ic,ir,p,b,e,narg,i

 type(vclstruct) , pointer :: pv


! program section
! ------------------------------------------------------------------------------

! init
 arg=' '
 larg=-1


! check ivcl
 if (ivcl.lt.1 .or. ivcl.gt.nvcl) then
    ! ERROR, index not defined
    write(*,*) ' ERROR, cfn_vcl, ivcl not defined: ',ivcl
 else
    pv=>vcl(ivcl)%pvcl
    narg=pv%narg
    if (abs(iarg).le.narg) then
       ! OK
       ! get data position
       ia=pv%arg(abs(iarg))
       b =pv%pos(ia)     ! start position
       e =pv%pos(ia+1)-1 ! end position
       larg=e-b+1

       ic=mod(b-1,lcl)+1
       ir=int((b-ic)/lcl)+1
       p=1
       do while (b.le.e)
          n1=e-b+1     ! number of characters left to store
          n2=lcl-ic+1  ! number of positions left at current row
          n=min(n1,n2) ! number of characters to store in current row

          arg(p:)=pv%cl(ir)(ic:ic+n-1)
          ! next
          p=p+n
          b=b+n
          ic=ic+n
          if (ic.gt.lcl) then
             ic=ic-lcl
             ir=ir+1
          endif
       enddo

       ! remove used argument when iarg<0
       if (iarg.lt.0) then
          do i=abs(iarg),narg-1
             pv%arg(i)=pv%arg(i+1)
          enddo
          narg=narg-1
          pv%narg=narg
       endif
    else
       larg=0
   !    ! ERROR
   !    write(*,*) ' ERROR, cfn_vcl, iarg not defined: ',iarg
    endif
 endif


! end of program
 return
end

! ******************************************************************************

subroutine cfn_vcl_fnd(ivcl,iarg,arg,remove)

! description:
! ------------------------------------------------------------------------------
! find the position of a command line argument
!

! declaration section
! ------------------------------------------------------------------------------
 use m_vcl

 implicit none


! arguments
 integer  , intent(in)            :: ivcl    !> index structure number
 integer  , intent(out)           :: iarg    !> argument position
                                             !!  >0: argument position
                                             !!   0: not found
                                             !!  <0: ERROR

 character (len=*), intent(in)    :: arg     !> argument string

 logical  , intent(in)            :: remove  !> .true.  remove argument from list if found
                                             !! .false. don't remove


! local variables
 integer   narg,larg,ltarg,i,is

 type(vclstruct) , pointer :: pv


! functions
 integer   cfn_length


! program section
! ------------------------------------------------------------------------------

! init
 iarg=0
 larg=cfn_length(arg)


! check ivcl
 if (ivcl.lt.1 .or. ivcl.gt.nvcl) then
    ! ERROR, index not defined
    write(*,*) ' ERROR, cfn_vcl, ivcl not defined: ',ivcl
    iarg=-1
 else

    ! get position of * in arg (if available)
    is=index(arg,'*')
    if (is.eq.larg) then
       ! * is at last position, this makes no sence, pretent it is not available
       is   = 0
       larg = larg-1
    endif

    pv=>vcl(ivcl)%pvcl
    narg=pv%narg
    do i=1,narg
       call cfn_vcl_arg(ivcl,i,targ,ltarg)
       if (is.gt.0) then
          ! check first part
          if (arg(1:is-1).eq.targ(1:is-1)) then
             ! first part is OK
             ! try last part
             if (arg(is+1:ltarg+1).eq.targ(is:ltarg)) then
                ! OK
                iarg=i
             endif
          endif
       else
          if (arg(1:larg).eq.targ(1:ltarg)) then
             ! OK
             iarg=i
          endif
       endif
    enddo

    ! remove argument if wanted
    if (remove .and. iarg.gt.0) then
       do i=abs(iarg),narg-1
          pv%arg(i)=pv%arg(i+1)
       enddo
       narg=narg-1
       pv%narg=narg
    endif

 endif


! end of program
 return
end

! ******************************************************************************

subroutine cfn_vcl_inpi(ivcl,string,value,ldefault,lval,uval,ninterval,iarg)

! description:
! ------------------------------------------------------------------------------
! get an INTEGER value from the command line
! when no argument found, the argument will be asked for interactively
!

! declaration section
! ------------------------------------------------------------------------------
 use m_vcl, only: targ

 implicit none


! arguments
 integer  , intent(in)          :: ivcl            ! index structure number
 integer  , intent(in)          :: iarg            ! argument number to get
                                                   !  >0: get argument number iarg
                                                   !  <0: get argument number abs(iarg)
                                                   !      remove argument from the list

 character (len=*), intent(in)  :: string          ! string to be displayed when asking for a value

 integer  , intent(in)          :: ninterval       ! number of intervals

 integer  , intent(in)          :: lval(*)         ! lower interval of allowed values
 integer  , intent(in)          :: uval(*)         ! upper interval of allowed values
 integer  , intent(inout)       :: value           ! variable to store argument value
                                                   ! on entry: the default value

 logical  , intent(in)          :: ldefault        ! .true.  default value is allowed
                                                   ! .false. a value must be deliverd by this routine


! local variables
 integer   larg

 character pstring*256

 logical   cont,ok


! functions
 integer   cfn_length


! program section
! ------------------------------------------------------------------------------

! print string
 if (ldefault) then
     write(pstring,*) value                                              !type
     call cfn_token(pstring,'t')
     pstring=string(1:cfn_length(string))//' ('//pstring(1:cfn_length(pstring))//')'
 else
     pstring=string
 endif


! get command line argument
 call cfn_vcl_arg(ivcl,iarg,targ,larg)


! get argument interactive
 cont=.true.
 do while (cont)

    ! get value
    cont=cfn_length(targ).le.0
    do while (cont)
       write(*,'(1x,a,a,$)') pstring(1:cfn_length(pstring)),' '
       read(*,'(a)') targ
       ! check
       if (cfn_length(targ).gt.0 .or. ldefault) cont=.false.
    enddo

    ! check
    if (cfn_length(targ).le.0) then
       ! use default value
       cont=.false.
    else
       ! check value
       call scfn_vcl_chki(targ,value,lval,uval,ninterval,ok)
       if (ok) then
          cont=.false.
       else
          cont=.true.
       endif
    endif
 enddo


! end of program
 return
end


! ******************************************************************************

subroutine cfn_vcl_inpr(ivcl,string,value,ldefault,lval,uval,ninterval,iarg)

! description:
! ------------------------------------------------------------------------------
! get a REAL value from the command line
! when no argument found, the argument will be asked for interactively
!

! declaration section
! ------------------------------------------------------------------------------
 use m_vcl, only: targ

 implicit none


! arguments
 integer  , intent(in)          :: ivcl            ! index structure number
 integer  , intent(in)          :: iarg            ! argument number to get
                                                   !  >0: get argument number iarg
                                                   !  <0: get argument number abs(iarg)
                                                   !      remove argument from the list

 character (len=*), intent(in)  :: string          ! string to be displayed when asking for a value

 integer  , intent(in)          :: ninterval       ! number of intervals

 real     , intent(in)          :: lval(*)         ! lower interval of allowed values
 real     , intent(in)          :: uval(*)         ! upper interval of allowed values
 real     , intent(inout)       :: value           ! variable to store argument value
                                                   ! on entry: the default value

 logical  , intent(in)          :: ldefault        ! .true.  default value is allowed
                                                   ! .false. a value must be deliverd by this routine


! local variables
 integer   larg

 character pstring*256

 logical   cont,ok


! functions
 integer   cfn_length


! program section
! ------------------------------------------------------------------------------

! print string
 if (ldefault) then
     write(pstring,*) value                                              !type
     call cfn_token(pstring,'t')
     pstring=string(1:cfn_length(string))//' ('//pstring(1:cfn_length(pstring))//')'
 else
     pstring=string
 endif


! get command line argument
 call cfn_vcl_arg(ivcl,iarg,targ,larg)


! get argument interactive
 cont=.true.
 do while (cont)

    ! get value
    cont=cfn_length(targ).le.0
    do while (cont)
       write(*,'(1x,a,a,$)') pstring(1:cfn_length(pstring)),' '
       read(*,'(a)') targ
       ! check
       if (cfn_length(targ).gt.0 .or. ldefault) cont=.false.
    enddo

    ! check
    if (cfn_length(targ).le.0) then
       ! use default value
       cont=.false.
    else
       ! check value
       call scfn_vcl_chkr(targ,value,lval,uval,ninterval,ok)
       if (ok) then
          cont=.false.
       else
          cont=.true.
       endif
    endif
 enddo


! end of program
 return
end


! ******************************************************************************

subroutine cfn_vcl_inpc(ivcl,string,value,ldefault,lval,uval,ninterval,iarg)

! description:
! ------------------------------------------------------------------------------
! get an INTEGER value from the command line
! when no argument found, the argument will be asked for interactively
!

! declaration section
! ------------------------------------------------------------------------------
 use m_vcl, only: targ

 implicit none


! arguments
 integer  , intent(in)          :: ivcl            ! index structure number
 integer  , intent(in)          :: iarg            ! argument number to get
                                                   !  >0: get argument number iarg
                                                   !  <0: get argument number abs(iarg)
                                                   !      remove argument from the list

 character (len=*), intent(in)  :: string          ! string to be displayed when asking for a value

 integer  , intent(in)          :: ninterval       ! number of intervals

 character (len=*), intent(in)   :: lval(*)        ! lower interval of allowed values
 character (len=*), intent(in)   :: uval(*)        ! upper interval of allowed values
 character (len=*), intent(inout):: value          ! variable to store argument value
                                                   ! on entry: the default value

 logical  , intent(in)          :: ldefault        ! .true.  default value is allowed
                                                   ! .false. a value must be deliverd by this routine


! local variables
 integer   larg

 character pstring*256

 logical   cont,ok


! functions
 integer   cfn_length


! program section
! ------------------------------------------------------------------------------

! print string
 if (ldefault) then
     write(pstring,*) value                                              !type
     call cfn_token(pstring,'t')
     pstring=string(1:cfn_length(string))//' ('//pstring(1:cfn_length(pstring))//')'
 else
     pstring=string
 endif


! get command line argument
 call cfn_vcl_arg(ivcl,iarg,targ,larg)


! get argument interactive
 cont=.true.
 do while (cont)

    ! get value
    cont=cfn_length(targ).le.0
    do while (cont)
       write(*,'(1x,a,a,$)') pstring(1:cfn_length(pstring)),' '
       read(*,'(a)') targ
       ! check
       if (cfn_length(targ).gt.0 .or. ldefault) cont=.false.
    enddo

    ! check
    if (cfn_length(targ).le.0) then
       ! use default value
       cont=.false.
    else
       ! check value
       call scfn_vcl_chkc(targ,value,lval,uval,ninterval,ok)
       if (ok) then
          cont=.false.
       else
          cont=.true.
       endif
    endif
 enddo


! end of program
 return
end

! ******************************************************************************

subroutine scfn_vcl_chki(arg,value,lval,uval,ninterval,ok)

! description:
! ------------------------------------------------------------------------------
! check INTEGER value
!

! declaration section
! ------------------------------------------------------------------------------

 implicit none


! arguments
 character (len=*), intent(in)  :: arg             !

 integer  , intent(in)          :: ninterval       ! number of intervals

 integer  , intent(in)          :: lval(*)         ! lower interval of allowed values
 integer  , intent(in)          :: uval(*)         ! upper interval of allowed values
 integer  , intent(inout)       :: value           ! variable to store argument value
                                                   ! on entry: the default value

 logical  , intent(out)         :: ok              ! .true.  value found


! local variables
 integer   i,ios

 integer   tvalue


! program section
! ------------------------------------------------------------------------------


 read(arg,*,iostat=ios) tvalue                            !type
 if (ninterval.gt.0 .and. ios.eq.0) then
    ! check tvalue
    ok = .false.
    do i=1,ninterval
       if (tvalue.ge.lval(i) .and. tvalue.le.uval(i)) then
          ok=.true.
       endif
    enddo
 else
    if (ios.ne.0) then
       ok=.false.
    else
       ! ok
       ok=.true.
       value=tvalue
    endif

 endif

 if (.not. ok) then
    write(*,'(/,1x,2a,/)') char(7),'Value not allowed, choose: '
    do i=1,ninterval
       if (lval(i).eq.uval(i)) then
          write(*,'(5x,i10)') lval(i)                                !type
       else
          write(*,'(i10,a,i10)') lval(i),'...',uval(i)               !type
       endif
    enddo

    write(*,'(/)')
 endif


! end of program
 return
end

! ******************************************************************************

subroutine scfn_vcl_chkr(arg,value,lval,uval,ninterval,ok)

! description:
! ------------------------------------------------------------------------------
! check value
!

! declaration section
! ------------------------------------------------------------------------------

 implicit none


! arguments
 character (len=*), intent(in)  :: arg             !

 integer  , intent(in)          :: ninterval       ! number of intervals

 real     , intent(in)          :: lval(*)         ! lower interval of allowed values
 real     , intent(in)          :: uval(*)         ! upper interval of allowed values
 real     , intent(inout)       :: value           ! variable to store argument value
                                                   ! on entry: the default value

 logical  , intent(out)         :: ok              ! .true.  value found


! local variables
 integer   i,ios

 real      tvalue


! program section
! ------------------------------------------------------------------------------


 read(arg,*,iostat=ios) tvalue                            !type
 if (ninterval.gt.0 .and. ios.eq.0) then
    ! check tvalue
    ok = .false.
    do i=1,ninterval
       if (tvalue.ge.lval(i) .and. tvalue.le.uval(i)) then
          ok=.true.
       endif
    enddo
 else
    if (ios.ne.0) then
       ok=.false.
    else
       ! ok
       ok=.true.
       value=tvalue
    endif

 endif

 if (.not. ok) then
    write(*,'(/,1x,2a,/)') char(7),'Value not allowed, choose: '
    do i=1,ninterval
       if (lval(i).eq.uval(i)) then
          write(*,'(5x,g12.5)') lval(i)                                !type
       else
          write(*,'(g12.5,a,g12.5)') lval(i),'...',uval(i)             !type
       endif
    enddo

    write(*,'(/)')
 endif


! end of program
 return
end

! ******************************************************************************

subroutine scfn_vcl_chkc(arg,value,lval,uval,ninterval,ok)

! description:
! ------------------------------------------------------------------------------
! check value
!

! declaration section
! ------------------------------------------------------------------------------

 implicit none


! arguments
 character (len=*), intent(in)  :: arg             !

 integer  , intent(in)          :: ninterval       ! number of intervals

 character (len=*), intent(in)   :: lval(*)        ! lower interval of allowed values
 character (len=*), intent(in)   :: uval(*)        ! upper interval of allowed values
 character (len=*), intent(inout):: value          ! variable to store argument value
                                                   ! on entry: the default value

 logical  , intent(out)         :: ok              ! .true.  value found


! local variables
 integer   i,ios
 integer   l1,l2

 character (len=256) :: tvalue

! functions
 integer   cfn_length


! program section
! ------------------------------------------------------------------------------

 ios=0
 tvalue=arg                            !type
 if (ninterval.gt.0 .and. ios.eq.0) then
    ! check tvalue
    ok = .false.
    do i=1,ninterval
       if (tvalue.ge.lval(i) .and. tvalue.le.uval(i)) then
          ok=.true.
       endif
    enddo
 else
    if (ios.ne.0) then
       ok=.false.
    else
       ! ok
       ok=.true.
       value=tvalue
    endif

 endif

 if (.not. ok) then
    write(*,'(/,1x,2a,/)') char(7),'Value not allowed, choose: '
    do i=1,ninterval
       if (lval(i).eq.uval(i)) then
          l1=cfn_length(lval(i))
          write(*,'(5x,a)') lval(i)(1:l1)                              !type
       else
          l1=cfn_length(lval(i))
          l2=cfn_length(uval(i))
          write(*,'(a,a,a)') lval(i)(1:l1),'...',uval(i)(1:l2)         !type
       endif
    enddo

    write(*,'(/)')
 endif


! end of program
 return
end

! ******************************************************************************

subroutine cfn_vcl_eva(ivcl,eva,rma)

! description:
! ------------------------------------------------------------------------------
! find variables at the command line argument and store them into eva-variables
! variables are defined by: <variable>=<value>

! declaration section
! ------------------------------------------------------------------------------
 use m_vcl

 implicit none


! arguments
 integer  , intent(in)          :: ivcl    ! index structure number

 character(len=*), intent(inout):: eva(*)  ! eva variables

 logical  , intent(in)          :: rma     ! .true.  remove argument from list if found
                                           ! .false. don't remove

! local variables
 integer   narg,ltarg,i,ne,ret,iarg
 integer   beg(3),end(3)

 type(vclstruct) , pointer :: pv


! functions
 integer   cfn_elem_pos


! program section
! ------------------------------------------------------------------------------

! init


! check ivcl
 if (ivcl.lt.1 .or. ivcl.gt.nvcl) then
    ! ERROR, index not defined
    write(*,*) ' ERROR, cfn_vcl, ivcl not defined: ',ivcl
 else

    pv=>vcl(ivcl)%pvcl
    narg=pv%narg
    iarg=1
    do while (iarg.le.narg)

       ! get argument iarg
       call cfn_vcl_arg(ivcl,iarg,targ,ltarg)

       ! find out if argument can be split up into two parts
       ! devided by a '=' sign
       ne=cfn_elem_pos(3,'=',1,targ,ltarg,beg,end)
       if (ne.eq.2) then
          ! found, store into eva-variable
          call eva_put_c(eva,targ(beg(1):end(1)),targ(beg(2):end(2)),ret)

          ! remove argument if wanted
          if (rma) then
             ! remove argument, no need to increase iarg
             do i=abs(iarg),narg-1
                pv%arg(i)=pv%arg(i+1)
             enddo
             narg=narg-1
             pv%narg=narg
          else
             ! not removed, increase iarg
             iarg=iarg+1
          endif
       else
          ! no match, increase iarg
          iarg=iarg+1
       endif

    enddo

 endif


! end of program
 return
end

! ******************************************************************************

!> description
!! find a specific command line option and store the INTEGER values into value-array
subroutine cfn_vcl_fndi(ivcl,iarg,arg,remove,value,nval)

! declaration section
! ------------------------------------------------------------------------------
 use m_vcl, only: targ

 implicit none


! arguments
 integer  , intent(in)            :: ivcl        !> index structure number
 integer  , intent(out)           :: iarg        !> argument position
                                                 !!  >0: argument position
                                                 !!   0: not found
                                                 !!  <0: ERROR

 character (len=*), intent(in)    :: arg         !> argument string

 logical  , intent(in)            :: remove      !> .true.  remove argument from list if found
                                                 !! .false. don't remove

 integer  , intent(in)            :: nval        !> number of values to read when arg was found
 integer  , intent(out)           :: value(nval) !> return values


! local variables
 integer   tiarg,stp,i,j,larg,ios


! program section
! ------------------------------------------------------------------------------

! init
 iarg = 0


! find arg
 call cfn_vcl_fnd(ivcl,tiarg,arg,remove)


 if (tiarg.gt.0) then
    ! argument found, get the values
     if (remove) then
        j=-1*tiarg
        stp=0
     else
        j=tiarg
        stp=1
     endif

    do i=1,nval
       j=j+stp
       call cfn_vcl_arg(ivcl,j,targ,larg)
       if (larg.gt.0) then
          read(targ,*,iostat=ios) value(i)
          if (ios.ne.0) iarg=-1                 ! ERROR, wrong data type
       endif
    enddo

    ! set value for iarg when no error occurred
    if (iarg.eq.0) iarg=tiarg
 endif


! end of program
 return
end

! ******************************************************************************

!> description
!! find a specific command line option and store the REAL values into value-array
subroutine cfn_vcl_fndr(ivcl,iarg,arg,remove,value,nval)

! declaration section
! ------------------------------------------------------------------------------
 use m_vcl, only: targ

 implicit none


! arguments
 integer  , intent(in)            :: ivcl        !> index structure number
 integer  , intent(out)           :: iarg        !> argument position
                                                 !!  >0: argument position
                                                 !!   0: not found
                                                 !!  <0: ERROR

 character (len=*), intent(in)    :: arg         !> argument string

 logical  , intent(in)            :: remove      !> .true.  remove argument from list if found
                                                 !! .false. don't remove

 integer  , intent(in)            :: nval        !> number of values to read when arg was found
 real     , intent(out)           :: value(nval) !> return values


! local variables
 integer   tiarg,stp,i,j,larg,ios


! program section
! ------------------------------------------------------------------------------

! init
 iarg = 0


! find arg
 call cfn_vcl_fnd(ivcl,tiarg,arg,remove)


 if (tiarg.gt.0) then
    ! argument found, get the values
     if (remove) then
        j=-1*tiarg
        stp=0
     else
        j=tiarg
        stp=1
     endif

    do i=1,nval
       j=j+stp
       call cfn_vcl_arg(ivcl,j,targ,larg)
       if (larg.gt.0) then
          read(targ,*,iostat=ios) value(i)
          if (ios.ne.0) iarg=-1                 ! ERROR, wrong data type
       endif
    enddo

    ! set value for iarg when no error occurred
    if (iarg.eq.0) iarg=tiarg
 endif


! end of program
 return
end

! ******************************************************************************

!> description
!! find a specific command line option and store the DOUBLE PRECISION values into value-array
subroutine cfn_vcl_fndd(ivcl,iarg,arg,remove,value,nval)

! declaration section
! ------------------------------------------------------------------------------
 use m_vcl, only: targ

 implicit none


! arguments
 integer  , intent(in)            :: ivcl        !> index structure number
 integer  , intent(out)           :: iarg        !> argument position
                                                 !!  >0: argument position
                                                 !!   0: not found
                                                 !!  <0: ERROR

 character (len=*), intent(in)    :: arg         !> argument string

 logical  , intent(in)            :: remove      !> .true.  remove argument from list if found
                                                 !! .false. don't remove

 integer          , intent(in)    :: nval        !> number of values to read when arg was found
 double precision , intent(out)   :: value(nval) !> return values


! local variables
 integer   tiarg,stp,i,j,larg,ios


! program section
! ------------------------------------------------------------------------------

! init
 iarg = 0


! find arg
 call cfn_vcl_fnd(ivcl,tiarg,arg,remove)


 if (tiarg.gt.0) then
    ! argument found, get the values
     if (remove) then
        j=-1*tiarg
        stp=0
     else
        j=tiarg
        stp=1
     endif

    do i=1,nval
       j=j+stp
       call cfn_vcl_arg(ivcl,j,targ,larg)
       if (larg.gt.0) then
          read(targ,*,iostat=ios) value(i)
          if (ios.ne.0) iarg=-1                 ! ERROR, wrong data type
       endif
    enddo

    ! set value for iarg when no error occurred
    if (iarg.eq.0) iarg=tiarg
 endif


! end of program
 return
end

! ******************************************************************************

!> description
!! find a specific command line option and store the CHARACTER values into value-array
subroutine cfn_vcl_fndc(ivcl,iarg,arg,remove,value,nval)

! declaration section
! ------------------------------------------------------------------------------
 use m_vcl, only: targ

 implicit none


! arguments
 integer  , intent(in)            :: ivcl        !> index structure number
 integer  , intent(out)           :: iarg        !> argument position
                                                 !!  >0: argument position
                                                 !!   0: not found
                                                 !!  <0: ERROR

 character (len=*), intent(in)    :: arg         !> argument string

 logical  , intent(in)            :: remove      !> .true.  remove argument from list if found
                                                 !! .false. don't remove

 integer          , intent(in)    :: nval        !> number of values to read when arg was found
 character (len=*), intent(out)   :: value(nval) !> return values


! local variables
 integer   tiarg,stp,i,j,larg


! program section
! ------------------------------------------------------------------------------

! init
 iarg = 0


! find arg
 call cfn_vcl_fnd(ivcl,tiarg,arg,remove)


 if (tiarg.gt.0) then
    ! argument found, get the values
     if (remove) then
        j=-1*tiarg
        stp=0
     else
        j=tiarg
        stp=1
     endif

    do i=1,nval
       j=j+stp
       call cfn_vcl_arg(ivcl,j,targ,larg)
       if (larg.gt.0) then
          value(i)=targ(1:larg)
       else
          value(i)=' '
       endif
    enddo

    ! set value for iarg when no error occurred
    if (iarg.eq.0) iarg=tiarg
 endif


! end of program
 return
end

! ******************************************************************************

!> description
!! check if any arguments are left
!! when no arguments are left: exitcode=0
subroutine cfn_vcl_argsleft(ivcl,lprint,exitcode)

! declaration section
! ------------------------------------------------------------------------------
 use m_vcl

 implicit none


! arguments
 integer  , intent(in)          :: ivcl        !> index structure number
 logical  , intent(in)          :: lprint      !> print the left arguments (.true.) or not (.false.)

 integer  , intent(out)         :: exitcode    !>  0: no arguments left
                                               !! >0: number of arguments left
                                               !! <0: error

! local variables
 integer   i,ltarg

 type(vclstruct) , pointer :: pv


! program section
! ------------------------------------------------------------------------------

! init
 exitcode = 0

 if (ivcl.ge.1 .and. ivcl.le.nvcl) then

    pv=>vcl(ivcl)%pvcl

    if (pv%narg.gt.0) then
       if (lprint) then
          write(*,'(a,i5)') ' Number of Virtual Command Line arguments not used: ',pv%narg
          do i=1,pv%narg
             call cfn_vcl_arg(ivcl,i,targ,ltarg)
             write(*,'(4x,a)') targ(1:ltarg)
          enddo
          write(*,'(1x)')
       endif
       exitcode=pv%narg
    endif
 else
    ! ERROR, ivcl not defined
    exitcode = -1
 endif


! end of program
 return
end

      function chf_EQ(char1,l1,char2,l2)

! evaluate the relational expression EQ

      implicit none

! function
      logical chf_EQ    ! return value: .true. : char1=char2
                        !               .false.: otherwise

! arguments
      integer     l1,&  ! (I) number of characters in char1
                  l2    ! (I) number of characters in char2

      character*1 char1(l1),&! (I) variable 1
                  char2(l2)  ! (I) variable 2


! local variables
      logical less,&   ! when char1<char2 this variable will be .true.
                       !                        else it will be .false.
              equal,&  ! when char1=char2 this variable will be .true.
                       !                        else it will be .false.
              greater  ! when char1>char2 this variable will be .true.
                       !                        else it will be .false.


! program section
! ------------------------------------------------------------------------------

! find out the relation
      call chf_bool(char1,l1,char2,l2,less,equal,greater)


! assign function value
      chf_EQ=(equal)

      return
      end

!*******************************************************************************


      function chf_NE(char1,l1,char2,l2)

! evaluate the relational expression NE

      implicit none

! function
      logical chf_NE    ! return value: .true. : char1<>char2
                        !               .false.: otherwise

! arguments
      integer     l1,&  ! (I) number of characters in char1
                  l2    ! (I) number of characters in char2

      character*1 char1(l1),&! (I) variable 1
                  char2(l2)  ! (I) variable 2


! local variables
      logical less,&   ! when char1<char2 this variable will be .true.
                       !                        else it will be .false.
              equal,&  ! when char1=char2 this variable will be .true.
                       !                        else it will be .false.
              greater  ! when char1>char2 this variable will be .true.
                       !                        else it will be .false.


! program section
! ------------------------------------------------------------------------------

! find out the relation
      call chf_bool(char1,l1,char2,l2,less,equal,greater)


! assign function value
      chf_NE=(.not.equal)

      return
      end

!*******************************************************************************


      function chf_LT(char1,l1,char2,l2)

! evaluate the relational expression LT

      implicit none

! function
      logical chf_LT    ! return value: .true. : char1<char2
                        !               .false.: otherwise

! arguments
      integer     l1,&  ! (I) number of characters in char1
                  l2    ! (I) number of characters in char2

      character*1 char1(l1),&! (I) variable 1
                  char2(l2)  ! (I) variable 2


! local variables
      logical less,&   ! when char1<char2 this variable will be .true.
                       !                        else it will be .false.
              equal,&  ! when char1=char2 this variable will be .true.
                       !                        else it will be .false.
              greater  ! when char1>char2 this variable will be .true.
                       !                        else it will be .false.


! program section
! ------------------------------------------------------------------------------

! find out the relation
      call chf_bool(char1,l1,char2,l2,less,equal,greater)


! assign function value
      chf_LT=(less)

      return
      end

!*******************************************************************************


      function chf_LE(char1,l1,char2,l2)

! evaluate the relational expression LE

      implicit none

! function
      logical chf_LE    ! return value: .true. : char1<=char2
                        !               .false.: otherwise

! arguments
      integer     l1,&  ! (I) number of characters in char1
                  l2    ! (I) number of characters in char2

      character*1 char1(l1),&! (I) variable 1
                  char2(l2)  ! (I) variable 2


! local variables
      logical less,&   ! when char1<char2 this variable will be .true.
                       !                        else it will be .false.
              equal,&  ! when char1=char2 this variable will be .true.
                       !                        else it will be .false.
              greater  ! when char1>char2 this variable will be .true.
                       !                        else it will be .false.


! program section
! ------------------------------------------------------------------------------

! find out the relation
      call chf_bool(char1,l1,char2,l2,less,equal,greater)


! assign function value
      chf_LE=(less.or.equal)

      return
      end

!*******************************************************************************


      function chf_GT(char1,l1,char2,l2)

! evaluate the relational expression GT

      implicit none

! function
      logical chf_GT    ! return value: .true. : char1>char2
                        !               .false.: otherwise

! arguments
      integer     l1,&  ! (I) number of characters in char1
                  l2    ! (I) number of characters in char2

      character*1 char1(l1),&! (I) variable 1
                  char2(l2)  ! (I) variable 2


! local variables
      logical less,&   ! when char1<char2 this variable will be .true.
                       !                        else it will be .false.
              equal,&  ! when char1=char2 this variable will be .true.
                       !                        else it will be .false.
              greater  ! when char1>char2 this variable will be .true.
                       !                        else it will be .false.


! program section
! ------------------------------------------------------------------------------

! find out the relation
      call chf_bool(char1,l1,char2,l2,less,equal,greater)


! assign function value
      chf_GT=(greater)

      return
      end

!*******************************************************************************


      function chf_GE(char1,l1,char2,l2)

! evaluate the relational expression GE

      implicit none

! function
      logical chf_GE    ! return value: .true. : char1>=char2
                        !               .false.: otherwise

! arguments
      integer     l1,&  ! (I) number of characters in char1
                  l2    ! (I) number of characters in char2

      character*1 char1(l1),&! (I) variable 1
                  char2(l2)  ! (I) variable 2


! local variables
      logical less,&   ! when char1<char2 this variable will be .true.
                       !                        else it will be .false.
              equal,&  ! when char1=char2 this variable will be .true.
                       !                        else it will be .false.
              greater  ! when char1>char2 this variable will be .true.
                       !                        else it will be .false.


! program section
! ------------------------------------------------------------------------------

! find out the relation
      call chf_bool(char1,l1,char2,l2,less,equal,greater)


! assign function value
      chf_GE=(greater.or.equal)

      return
      end

!*******************************************************************************

      function chf_LK0(char1,l1,char2,l2)

! evaluate the  relational expression LIKE
! as wild cards the variables wildc (character) and wilds (string) will be used
! wildc='?'
! wilds='*'
!
! char1 may contain the wild cards, char2 only contains text

      implicit none

      integer     l1,&  ! number of characters in char1
                  l2    ! number of characters in char2

      character*1 char1(l1),&
                  char2(l2)

      logical chf_LK0


      character*1 null
!c      parameter   (null=char(0))  ! Lahey, Sun-Unix, pgf
!c      parameter   (null=0)        ! Sun-Unix, gnu

      character*1 wildc,wilds
      parameter   (wildc='?',wilds='*')

      logical ok,star,estar,cont,cont2

      integer i,j,is,js,ls1,ls2



      null=char(0)

      ok=.true.
      star=.false.
      i=0
      j=1

!!! test if l1>0 and l2>0


      ls1=l1
      ls2=l2

      ! check if the ends of char1 also contains wild cards
      cont=.true.
      estar=.false.
      do while(cont)

         if (ls1.lt.1) then
            cont=.false.
         else if (char1(ls1).eq.wildc) then
            ls2=ls2-1
            ls1=ls1-1
         else if (char1(ls1).eq.wilds) then
            estar=.true.
            ls1=ls1-1
         else
            cont=.false.
         endif

      enddo



      if (ls1.eq.0) then
         ! char1 consist uniquely of wild cards
         if (ls2.lt.0) then
            ! char2 consist of too few characters
            ok=.false.
         else if (ls2.eq.0) then
            ! char2 consist exactly of enough characters
            ok=.true.
         else
            ! there are still characters left that needs to be matched
            ! this can only be done in case estar=.true.
            if (estar) then
               ok=.true.
            else
               ok=.false.
            endif
         endif
      else
         if (ls2.le.0) then
            ! char2 contains too few characters
            ok=.false.
         else
            ! whenever char1 does not end with "wilds", then first match the end of char1
            if (.not. estar) then
               cont =.true.
               cont2=.false.
               do while (cont)

                  if (char1(ls1).ne.wilds .and.&
                      char1(ls1).ne.wildc) then
                     if (char1(ls1).eq.char2(ls2)) then
                        ls1=ls1-1
                        ls2=ls2-1
                        if (ls1.eq.0) then
                           if (ls2.eq.0) then
                              ! match complete
                              ok=.true.
                              cont=.false.
                           else
                              ! char1 is empty but ls2 still has some left
                              ! this can never be matched
                              ok=.false.
                              cont=.false.
                           endif
                        else
                           if (ls2.eq.0) then
                              ! match can now only be possible when char1 just
                              ! consists of "wilds"
                              do while (ok .and. ls1.gt.0)
                                 if (char1(ls1).ne.wilds) then
                                    ok=.false.
                                 endif
                                 ls1=ls1-1
                              enddo
                              cont=.false.
                           endif
                        endif
                     else
                        ok=.false.
                        cont=.false.
                     endif
                  else
                     ! wild cards are back in the game
                     ! let other part do this
                     cont =.false.
                     cont2=.true.
                  endif

               enddo
            else
               cont2=.true.
            endif



            if (cont2) then
               ! continue the search from the start
               cont=.true.
               i=1
               j=1

               do while(cont)

                  ! check if a string-wild card becomes active
                  ! it does not become inactive again
                  if (char1(i).eq.wilds) then
                     star=.true.
                     i=i+1
                     is=i  ! save position in case it goes wrong
                     js=j  ! save position in case it goes wrong
                  else if (char1(i).eq.wildc) then
                     j=j+1
                     i=i+1
                  else
                     if (char1(i).ne.char2(j)) then
                        if (star) then
!!!!!!!!!!!!!!!!!!!!!
                           js=js+1
                           j=js
                           i=is
                        else
                           ok=.false.
                           cont=.false.
                        endif
                     else
                        j=j+1
                        i=i+1
                     endif
                  endif


                  if (j.gt.ls2) then
                     cont=.false.
                     if (i.gt.ls1) then
                        ! apparently we are there
                        ok=.true.
                     else
                        ! match can now only be possible when char1 just
                        ! consists of "wilds"
                        do while (ok .and. i.le.ls1)
                           if (char1(i).ne.wilds) then
                              ok=.false.
                           endif
                           i=i+1
                        enddo
                     endif
                  else

                     if (i.gt.ls1) then
                        cont=.false.
                        ! only match when estar=.true.
                        if (estar) then
                           ok=.true.
                        else
                           ok=.false.
                        endif
                     endif

                  endif

               enddo

            endif
         endif

      endif


      chf_LK0=ok

      return
      end

!*******************************************************************************

      function chf_COPY(char1,l1,char2,l2)

! copy char1 to char2

!     function: char2(1:l2)=char1(1:l1)


      implicit none

      integer     l1,&  ! number of characters in char1
                  l2    ! number of characters in char2

      character*1 char1(l1),&
                  char2(l2)

      integer chf_COPY


      character*1 spatie
!      parameter   (space=char(32))


      integer i,lmin



      spatie=char(32)

      lmin=min(l1,l2)

      ! copy least part
      do i=1,lmin
         char2(i)=char1(i)
      enddo

      ! in case char2 is longer than char1 then char2 is completed with spaces
      do i=lmin+1,l2
         char2(i)=spatie
      enddo

      chf_COPY=0 ! for now the return value does not have significance

      return
      end

!*******************************************************************************

      function chf_APPEND(char1,lb,l1,char2,l2)

! append of char2 to char1

!     function: char1(1:l1) = char1(1:lb)//char2(1:l2)


      implicit none

      integer     l1,&  ! number of characters in char1
                  l2,&  ! number of characters in char2
                  lb    ! current filled length of char1

      character*1 char1(l1),&
                  char2(l2)

      integer chf_APPEND


      character*1 spatie
!      parameter   (space=char(32))


      integer i,j,lmin



      spatie=char(32)

      chf_APPEND=0

      lmin=min(l1-lb,l2)

      if (lmin.lt.1) then
         chf_APPEND=-1
      else
         ! copy least part
         j=1
         do i=lb+1,lb+lmin
            char1(i)=char2(j)
            j=j+1
         enddo

         ! fill in spaces if needed
         do i=lb+lmin+1,l1
            char1(i)=spatie
         enddo
      endif

      return
      end

!*******************************************************************************

      subroutine chf_bool(char1,l1,char2,l2,less,equal,greater)

! description
! ------------------------------------------------------------------------------
! evaluate the relational expressions
! depending on char1 towards char2 becomes less, equal of greater .true.
! and the other two variables .false.

! declarations

      implicit none

! arguments

      integer     l1,&  ! number of characters in char1
                  l2    ! number of characters in char2

      character*1 char1(l1),&
                  char2(l2)

      logical less,&   ! (O) when char1<char2 this variable will be .true.
                       !                            else it will be .false.
              equal,&  ! (O) when char1=char2 this variable will be .true.
                       !                            else it will be .false.
              greater  ! (O) when char1>char2 this variable will be .true.
                       !                            else it will be .false.

! local variables

      character*1 null
!c      parameter   (null=char(0))  ! Lahey, Sun-Unix, pgf
!c      parameter   (null=0)        ! Sun-Unix, gnu

      integer i,lmin


!      null=char(0)

! program section
! -------------------------------------------------------------------------


! init
      less    = .false.
      equal   = .false.
      greater = .false.

      null    = char(0)

      i=1

! search through the least part
      lmin=min(l1,l2)

      if (lmin.gt.0) then

         do while(char1(i).eq.char2(i) .and. i.lt.lmin)
            i=i+1
         enddo

         if (char1(i).ne.char2(i)) then
            ! not equal, definitive output is known
            if (char1(i).lt.char2(i)) then
               less    = .true.
            else
               greater = .true.
            endif
         else
            ! equal,
            ! further on er must be checked if
            ! "supplement with null-s" changed this
            equal = .true.
         endif

      else
         ! the first part is equal (of course, two strings of 0 characters are always equal),
         ! further on er must be checked if
         ! "supplement with null-s" changed this
         equal = .true.
      endif


! whenever the first part char1(1..lmin) = char2(1..lmin) then there must be
! checked if the remnant changes this
      if (equal) then

         ! in case one variable is longer than the other one it pretends as if
         ! the shortest variable is filled with "null" characters

         ! if l1=l2 then we are done, otherwise, we need to continue
         if (l1.ne.l2) then
            if (l1.gt.l2) then

               do while(char1(i).EQ.null .and. i.lt.l1)
                  i=i+1
               enddo

               ! if char1(i)=null then the answer stays "equal"
               ! otherwise, it still needs to be figured out
               if (char1(i).ne.null) then
                  equal  = .false.
                  if (char1(i).gt.null) then
                     greater = .true.
                  else
                     ! this shall never occur because <null does not exist
                     less    = .true.
                  endif
               endif

            else

               do while(char2(i).EQ.null .and. i.lt.l2)
                  i=i+1
               enddo

               ! if char2(i)=null the the answer stays "equal"
               ! otherwise, it still needs to be figured out
               if (char2(i).ne.null) then
                  equal  = .false.
                  if (char2(i).gt.null) then
                     less    = .true.
                  else
                     ! this shall never occur because <null does not exist
                     greater = .true.
                  endif
               endif

            endif
         endif

      endif


      return
      end

! ******************************************************************************

      function chf_lk(char1,l1,char2,l2)

! description:
! ------------------------------------------------------------------------------
! evaluate the relational expression LIKE
! as wild cards the variables wildc (character) and wilds (string) will be used
! wildc='?'
! wilds='*'
!
! char1 may contain the wild cards, char2 only contains text
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      logical   chf_lk   ! return value: .true. :
                         !               .false.:


! arguments
      integer   l1,&          ! (I) number of characters in char1
                l2            ! (I) number of characters in char2

      character char1*(*),&   ! (I) string to test, may contain wild cards
                char2*(*)     ! (I) string to compare, without wild cards


! local variables
      character wildc*1,wilds*1
      parameter (wildc='?',wilds='*')

      integer   lblok,bblok,eblok,p,&
                ib1,ie1,ib2,ie2

!dd      character debugstring*256                                      ! debug

      logical   res,tres,lblk0,lblk1,lblk2,lblk3


! functions
      integer   cfn_lindex

      logical   chf_lk_blk


! include files


! program section
! ------------------------------------------------------------------------------

!dd      debugstring='                                                ' ! debug
! algorithm
! ---------
! char1 can consist of different blocks which are separated by
!       wild card wilds. A block can also contain a wild card of exact 1
!       character(wildc), this is part of the block.
!
!     -------     -------     -------     -------
!     |  1  |.....|  2  |.....|  2  |.....|  3  |  char1: number of wilds>0
!     -------     -------     -------     -------
!
!     -------------------------------------------
!     |                    0                    |  char1: number of wilds=0
!     -------------------------------------------
!
! The blocks 1 and 3 are attached to the border because on the outside are
! no wild card wilds present. Those blocks cannot 'shift' over char2.
! The blocks of type 2 can shift and so they have multiple
! possibilities to fit into char2.
!
! - if block 1 is present, then check if it fits into char2, if not, then .false.
! - if block 3 is present, then check if it fits into char2, if not, then .false.
! - if block(s) 2 is/are present, then fit all, if not, then .false.
! - if no wilds are present, then just test block0

! init
      res=.true.  ! in the beginning by default the output is .true., once
                  ! the contrary is proven, the output turns .false. => end

      ib1=1       ! start position of first block to test in char1
      ie1=l1      ! end  position of last block to test in char1

      ib2=1       ! start position of part to test in char2
      ie2=l2      ! end  position of part to test in char2


      lblk0=.false.
      lblk1=.false.
      lblk2=.false.
      lblk3=.false.


! char1 must at least contain 1 character
      if (l1.le.0) res=.false.


! determine which blocks exist
      if (res) then

         ! test on block0 or remaining blocks
         p=index(char1(ib1:ie1),wilds)
         if (p.gt.0) then
            lblk0=.false.
            lblk1=.true.
            lblk2=.true.
            lblk3=.true.
         else
            lblk0=.true.
            lblk1=.false.
            lblk2=.false.
            lblk3=.false.
         endif

         ! search the first not wilds character
         do while (char1(ib1:ib1).eq.wilds .and. ib1.lt.ie1)
            ib1=ib1+1
         enddo

         ! search the last not wilds character
         do while (char1(ie1:ie1).eq.wilds .and. ib1.lt.ie1)
            ie1=ie1-1
         enddo

         ! whenever ib1>1 : block1 does not exist
         if (ib1.gt.1 ) lblk1=.false.

         ! whenever ie1<l1: block3 does not exist
         if (ie1.lt.l1) lblk3=.false.

         ! whenever char1(ib1:ib1).eq.wilds then char1 just exists of
         ! wilds characters. continue testing is not necessary, everything satisfies
         if (char1(ib1:ib1).eq.wilds) then
            lblk0=.false.
            lblk1=.false.
            lblk2=.false.
            lblk3=.false.
         endif

         ! at this moment we are still not a 100% sure about the
         ! existence of block2. whenever lblk2=.false. then block2 does not exist
         ! whenever lblk2=.true. it still is possible that block2 does not exist.
         ! This becomes clear after testing block1 and block3

      endif

!dd      write(*,*) '   res,lblok0123: ',res,lblk0,lblk1,lblk2,lblk3    ! debug

! block0
      if (res .and. lblk0) then

         ! completely test char1 against char2, they have to be equally long
         if (ie1.ne.ie2) then
            res=.false.
         else

            ! test block
            res=chf_lk_blk(char1,ib1,ie1,&
                           char2,ib2,ie2,wildc)
!dd      if (res) debugstring(ib2:ib2+ie1-ib1+1)=char1(ib1:ie1)         ! debug
         endif

      endif   ! test end of block0

!dd      write(*,*) ' 0 res,lblok0123: ',res,lblk0,lblk1,lblk2,lblk3    ! debug

! block1
      if (res .and. lblk1) then

         ! search end of block1
         p=index(char1(ib1:ie1),wilds)
         if (p.le.0) then
            ! apparently there is only 1 block present which contains just wilds characters at the end
            eblok=ie1
         else
            eblok=ib1+p-2
         endif

         ! test block
         res=chf_lk_blk(char1,ib1,eblok,&
                        char2,ib2,ie2,wildc)
!dd      if (res) debugstring(ib2:ib2+eblok-ib1)=char1(ib1:eblok)       ! debug
!dd      write(*,*) ' tst1: ',ib1,eblok,ib2,ie2,res                     ! debug

         ! this part is tested, start next part
         lblok=eblok-ib1+1
         ib1=ib1+lblok
         ib2=ib2+lblok

         ! test if block2 or block3 still can exist
         if (ib1.eq.(ie1+1)) then
            ! there was just 1 block present, the one that is already tested
            ! the remnant of char1 certainly exists of 1 or more wilds characters
            ! continue testing is not necessary any more!
            lblk2=.false.
            lblk3=.false.
         endif

         ! if block3 exists then there must be a
         ! part of char2 left to test. if that is not the case, finish
         if (ib2.gt.ie2 .and. lblk3) res=.false.

         ! start searching the next block, it is certain that the first character
         ! after block1 a wilds character is. is also is certain that this character exists,
         ! otherwise block1 did not exist!
         do while (char1(ib1:ib1).eq.wilds .and. ib1.lt.ie1)
            ib1=ib1+1
         enddo

      endif   ! test end of block1
!dd      write(*,*) ' 1 res,lblok0123: ',res,lblk0,lblk1,lblk2,lblk3   ! debug


! block3
      if (res .and. lblk3) then

         ! start searching block3. Whenever block2 does not occur in char1
         ! then there won't be any wilds characters in the remaining part.
         p=cfn_lindex(char1(ib1:ie1),wilds)
         if (p.le.0) then
            ! more wilds characters do not occur, char1 must be completely used,
            ! so block2 does not occur
            lblk2=.false.
            bblok=ib1
         else
            ! convert bblock to absolute value
            bblok=ib1+p
         endif

         ! test block3
         lblok=ie1-bblok+1
         p=ie2-lblok+1
         p=max(p,ib2)
         res=chf_lk_blk(char1,bblok,ie1,&
                        char2,p,ie2,wildc)
!dd      if (res) debugstring(p:p+ie1-bblok)=char1(bblok:ie1)           ! debug
!dd      write(*,*) ' tst3: ',bblok,ie1,p,ie2,res                       ! debug

         ! this part is tested, the end can be removed
         ie1=bblok-1
         ie2=p-1


         ! test if block2 can still exist
         if (ib1.eq.(ie1+1)) then
            ! there was just 1 block present, the one that is already tested
            ! the remnant of char1 certainly exists of 1 or more wilds characters
            ! continue testing is not necessary any more!
            lblk2=.false.
         endif


         ! search the end of previous block
         do while (char1(ie1:ie1).eq.wilds .and. ib1.lt.ie1)
            ie1=ie1-1
         enddo

      endif   ! test end block3

!dd      write(*,*) ' 3 res,lblok0123: ',res,lblk0,lblk1,lblk2,lblk3    ! debug

! blocks2
      if (res .and. lblk2) then

         do while (ib1.le.ie1 .and. res)

            ! search the end of block2
            p=index(char1(ib1:ie1),wilds)
            if (p.le.0) then
               ! last block! use remnant of char1.
               eblok=ie1
            else
               eblok=ib1+p-2
            endif

            ! test block
            lblok=eblok-ib1+1
            tres =.false.
            p    =ie2-lblok+1   ! last position in char2 in which the block still fits
            do while(.not.tres .and. ib2.le.p)
               tres=chf_lk_blk(char1,ib1,eblok,&
                               char2,ib2,ie2,wildc)
               if (.not.tres) ib2=ib2+1 ! block has not fitted yet, try again
                                        ! 1 character further on
!dd      write(*,*) ' tst2: ',ib1,eblok,ie1,ib2,ie2,tres                ! debug
            enddo
            res=tres
!dd      if (res) debugstring(ib2:ib2+eblok-ib1)=char1(ib1:eblok)       ! debug

            ! this part is tested, start next part
            ib1=ib1+lblok
            ib2=ib2+lblok

            ! start searching the next block, it is certain that the first character
            ! after block2 a wilds character is. is also is certain that this character exists,
            ! otherwise block2 did not exist!
            do while (char1(ib1:ib1).eq.wilds .and. ib1.lt.ie1)
               ib1=ib1+1
            enddo

         enddo

         ! whenever char1 is fully tested and there is still a part of char2 not tested
         ! then that is not a problem any more. The remnant of
         ! char2 is now part of a wilds character, so it always right!

      endif   ! test end of block2
!dd      write(*,*) ' 2 res,lblok0123: ',res,lblk0,lblk1,lblk2,lblk3    ! debug

!dd      write(*,'(1x,l,3(/,1x,a))') res,char2(1:l2),debugstring(1:l2), ! debug
!dd     1                            char1(1:l1)                        ! debug
! assign function value
      chf_lk = res


! end of program
      return
      end

! ******************************************************************************

      function chf_lk_blk(char1,bblk,eblk,char2,bstr,estr,wildc)

! description:
! ------------------------------------------------------------------------------
!    char1(bblk,eblk) is being compared with char2(bstr,estr)
!
! compare two strings with one another at which the first one may contain
! multiple wild cards for 1 character
! whenever char2 is too small to cover the entire block
! then the answer is in any case .false.

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      logical   chf_lk_blk     ! return value: .true. :
                               !               .false.:


! arguments
      integer   bblk,&          ! (I) start position block
                eblk,&          ! (I) end  position block
                bstr,&          ! (I) start position string
                estr            ! (I) end  position string

      character char1*(*),&     ! (I)
                char2*(*),&     ! (I)
                wildc*1         ! (I)


! local variables
      integer   i,p

      logical   res


! functions


! include files


! program section
! ------------------------------------------------------------------------------

! init
      res=.true.

! test
      ! first check if the block is not longer than char2
      if ((eblk-bblk).gt.(estr-bstr)) then
         ! it is impossible to give a resemblance between the strings, so it is better to quit right away
         res=.false.
      else
         ! test block
         p=bstr-bblk
         do i=bblk,eblk
            if (char1(i:i).ne.char2(i+p:i+p) .and.&
                char1(i:i).ne.wildc) res=.false.
         enddo
      endif


! assign function value
      chf_lk_blk = res


! end of program
      return
      end

!
! DATUM: 14/04/92
!        15/11/95 adapted
!
      function cfn_collapse(arg)

! description:
! ------------------------------------------------------------------------------

! declaration section
! ------------------------------------------------------------------------------

      implicit none

! function
      character cfn_collapse*(*)


! arguments
      character arg*(*)


! local variables
      integer larg


! functions


! program section
! ------------------------------------------------------------------------------

      larg=len(cfn_collapse)

      cfn_collapse=arg

      call cfn_s_collapse2(cfn_collapse,larg)

      return
      end

! ******************************************************************************

      function cfn_collapse2(arg,larg)

! description:
! ------------------------------------------------------------------------------

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function
      character cfn_collapse2*(*)


! arguments
      integer   larg

      character arg(larg)*1


! local variables
      integer i,l


! functions


! include files


! program section
! ------------------------------------------------------------------------------


      cfn_collapse2=' '
      l=min(len(cfn_collapse2),larg)
      do i=1,l
         cfn_collapse2(i:i)=arg(i)
      enddo
      call cfn_s_collapse2(cfn_collapse2,larg)


      return
      end

! ******************************************************************************

      subroutine cfn_s_collapse2(arg,larg)

! description:
! ------------------------------------------------------------------------------
! collapse function in a subroutine programmed to prevent
! declaration problems

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   larg          ! (I) length argument arg

      character arg(larg)*1   ! (I/O) string to 'collapse'


! local variables
      integer   j,k


! functions


! include files


! program section
! ------------------------------------------------------------------------------


! letting the string collapse
      j=1
      do k=1,larg
         if (arg(k).ne.' ' .and. arg(k).ne.char(0)) then
            arg(j)=arg(k)
            j=j+1
         endif
      enddo


! fill up with spaces
      do k=j,larg
         arg(k)=' '
      enddo



! end of program
      return
      end

!
! DATUM: 11/01/93
!        10/01/95 element2 changed because of "subscript out of range"
!                 chf_copy function added
! A.L.		27 May 1997	v2r0	ELEMENT renamed to CFN_ELEM
! A.L.		12 Aug 1998	v2r0	replace cfn_elem2 by cfn_s_elem2
!
      function cfn_elem(en,st,as,string)

      implicit none

      integer en,&                 ! element number to search
              as                   ! # separators

      character*(*) cfn_elem,string
      character*1   st(as)         ! separators

      integer  lstring


      lstring = len(string)

      call cfn_s_elem2(en,st,as,string,lstring,cfn_elem)

      return
      end

! ******************************************************************************

      function cfn_elem2(en,st,as,string,lstring)

! A.L.		12 Aug 1998	v2r0	replace function by cfn_s_elem2

      implicit none

      integer en,&                 ! element number to search
              as,&                 ! # separators
              lstring

      character*(*) cfn_elem2
      character*1   string(lstring)
      character*1   st(as)         ! separators


      call cfn_s_elem2(en,st,as,string,lstring,cfn_elem2)

      return
      end

! ******************************************************************************

      subroutine cfn_elem_be(enx,st,as,string,lstring,begin,eind)


! description:
! ------------------------------------------------------------------------------
! query start and end position of field <enx> in <string>
! whenever element ENX does not exist, the value 0 will be assigned to
! the start and end positions

! declaration section
! ------------------------------------------------------------------------------

      implicit none

! arguments
      integer lstring,&            ! length string
              enx,&                ! given element number
              as                   ! # separators

      character string(lstring)*1
      character st(as)*1           ! separators

      integer begin,&              ! start position element
              eind                 ! end position element


! local variables
      integer i,&                  ! position meter
              en,&                 ! element number to search
              nr                   ! element number during search

      logical continue


! functions
      logical in_char,cfn_een_van


! program section
! ------------------------------------------------------------------------------

! init
      begin = 0
      eind  = 0
      i=1


! whenever enx < 1  =>  and 1

      if (enx.lt.1) then
         en=1
      else
         en=enx
      endif


! number of times
      ! test if the string is long enough to contain EN elements
      if (en.gt.lstring) then
         nr = en  ! by this the program quits,
                  ! START and END positions stay 0
      else
         nr = 0   ! element number on which I am working now
      endif

      do while (nr.lt.en)
         nr=nr+1

! whenever st(1) = ' ' then the spaces at the start must be skipped

         if (st(1).eq.' ') then
            do while(string(i).eq.' ' .and. i.lt.lstring)
               i=i+1
            enddo
         endif

! determine start position in case this is applicable

         if (nr.eq.en) then
            begin=i
         endif

! searching for a separator

!         do while (i.le.lstring  .and.
!     1             .not.cfn_een_van(string(i),st,as))

         continue=.true.
         if (i.gt.lstring) continue=.false.
         do while (continue)

            if (cfn_een_van(string(i),st,as)) then

               continue=.false.

            else

               if (string(i).eq.'''') then
!                we are entering a character string. we need to get
!                out of it as soon as possible.
                  in_char=.true.
                  do while (in_char .and. i.lt.lstring)
                     i=i+1
                     if (string(i).eq.'''') then
                        i=i+1
                        if (i.le.lstring) then
                           if (string(i).ne.'''') then
!              we are out
                              in_char=.false.
                           endif
                        endif
                     endif
                  enddo
                  ! whenever a string ends with a open quoted-string
                  ! then the program can never leave this loop
                  ! therefore this extra test
                  if (in_char) continue=.false.
               else

                  i=i+1
               endif
               if (i.gt.lstring) continue=.false.
            endif

         enddo

!

         if (nr.eq.en) then
!    check if separator is found even if at the end of the string
            if (i.gt.lstring) then
               eind=lstring
            else
               eind=i-1
            endif
         else
            if (st(1).eq.' ') then
! skip the spaces until the next non-space
               do while(i.lt.lstring .and. string(i).eq.' ')
                  i=i+1
               enddo
            endif

!
            if (i.le.lstring) then
               if (cfn_een_van(string(i),st,as)) then
                  i=i+1 ! this is going to be the start of the next element
               endif
            endif

            if (i.gt.lstring) then
!              no string is found. an empty string has to return
               nr=en
            endif

         endif

      enddo

      return
      end

! ******************************************************************************

      subroutine cfn_s_elem(en,st,as,string,elem)

! description:
! ------------------------------------------------------------------------------
! subroutine with element function
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   en,&               ! (I) element number to query
                as                 ! (I) number of separators

      character st*(*),&           ! (I) separators
                string*(*),&       ! (I) input string
                elem*(*)           ! (O) output element


! local variables
      integer   lstring


! program section
! ------------------------------------------------------------------------------

      lstring = len(string)

      call cfn_s_elem2(en,st,as,string,lstring,elem)


! end of program
      return
      end

! ******************************************************************************

      subroutine cfn_s_elem2(en,st,as,string,lstring,elem)

! description:
! ------------------------------------------------------------------------------
! subroutine with element function
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   en,&               ! (I) element number to query
                as,&               ! (I) number of separators
                lstring            ! (I) length of string in bytes

      character st(as)*1,&         ! (I) separators
                string(lstring)*1,&! (I) input string
                elem*(*)           ! (O) output element


! local variables
      integer   begin,&            ! start position element
                eind,&             ! end position element
                l,it


! functions
      integer    chf_copy          ! copy function


! include files


! program section
! ------------------------------------------------------------------------------


      call cfn_elem_be(en,st,as,string,lstring,begin,eind)

      l=len(elem)

      if ((eind-begin+1).gt.l) then
         eind=begin+l-1
      endif


      if (begin.ne.0 .and. begin.le.eind) then
         it=chf_copy(string(begin),eind-begin+1,elem,l)
      else
         it=chf_copy(char(0),1,elem,l)
      endif



! end of program
      return
      end

! ******************************************************************************

      FUNCTION cfn_EEN_VAN(CHAR,TEKENS,NT)

! A.L.		20 Oct 2003	v2r0	adapted by means of index function

      implicit none

! function declaration
      LOGICAL cfn_EEN_VAN

! arguments
      INTEGER   NT
      CHARACTER CHAR*1,TEKENS(NT)*1


! local variables

      INTEGER   I,j


      logical   l(0:1)
      data      l/.false.,.true./

! program section
! ------------------------------------------------------------------------------

!      i=min(1,index(tekens(1:nt),char))  ! 0=not found, 1=found
      i=0
      do j=1,nt
         if (tekens(j).eq.char) i=1
      enddo

      cfn_EEN_VAN=l(i)

      RETURN
      END

! ******************************************************************************

      function cfn_elem_pos(ne,st,as,string,lstring,begin,eind)

! description:
! ------------------------------------------------------------------------------
! query the start and end positions of the first 'ne'
! elements in 'string'
! Whenever 'string' is empty, then there are 0 fields found
! Whenever a field is empty, then end(*)=start(*)-1


! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   cfn_elem_pos  ! return value: number of found elements


! arguments
      integer lstring,&     ! (I) length string
              ne,&          ! (I) maximum number of elements to search
              as            ! (I) # separators

      character*1 string(lstring) ! (I) string in which needs to be searched
      character*1 st(as)          ! (I) separators

      integer begin(ne),&         ! (O) start positions elements
              eind(ne)            ! (O) end positions elements


! local variables
      integer   i,nr

      logical   continue1,continue,in_char


! functions
      logical   cfn_een_van


! include files


! program section
! ------------------------------------------------------------------------------


! number of times

      nr = 0 ! element number on which I am working now

      if (lstring.gt.0) then
         continue1=.true.
      else
         continue1=.false.
      endif

      i=1

      do while (continue1)

         nr=nr+1

! ! whenever st(1) = ' ' then the spaces at the start must be skipped

         if (st(1).eq.' ') then
            do while(string(i).eq.' ' .and. i.lt.lstring)
               i=i+1
            enddo
         endif

! skip start position

         begin(nr)=i


! searchin for a separator

!         do while (i.le.lstring  .and.
!     1             .not.cfn_een_van(string(i),st,as))

         continue=.true.
         if (i.gt.lstring) continue=.false.
         do while (continue)

            if (cfn_een_van(string(i),st,as)) then

               continue=.false.

            else

               if (string(i).eq.'''') then
!                we are entering a character string. we need to get
!                out of it as soon as possible.
                  in_char=.true.
                  do while (in_char .and. i.lt.lstring)
                     i=i+1
                     if (string(i).eq.'''') then
                        i=i+1
                        if (i.le.lstring) then
                           if (string(i).ne.'''') then
!              we are out
                              in_char=.false.
                           endif
                        endif
                     endif
                  enddo
               else

                  i=i+1
               endif
               if (i.gt.lstring) continue=.false.
            endif

         enddo

!

!    check if a separator is found even if at the end of the string
         if (i.gt.lstring) then
            eind(nr)=lstring
         else
            eind(nr)=i-1
         endif

         if (st(1).eq.' ' .and. i.lt.lstring) then
! skip the spaces until the next non-space
            do while(i.lt.lstring .and. string(i).eq.' ')
               i=i+1
            enddo
         endif

!
         if (i.le.lstring) then
            if (cfn_een_van(string(i),st,as)) then
               i=i+1 ! this is going to be the start of the next element
            endif
         endif

         if (i.gt.lstring) then
!           no string is found. an empty string has to return
            continue1=.false.
         endif

         if (nr.ge.ne) continue1=.false.

      enddo

! check if the last element is empty
      if (lstring.gt.0) then
         if (cfn_een_van(string(lstring),st,as) .and. nr.lt.ne) then
            nr=nr+1
            begin(nr)=lstring+1
            eind(nr) =lstring
            endif
      endif

! maybe there are no elements at all, check if this is the case
!      if (nr.eq.1) then
!         if (start(1).gt.end(1)) then
!            nr=0
!            start(1)=1
!            end(1)=0
!         endif
!      endif


! return value
      cfn_elem_pos=nr


! end of program
      return
      end

      function eva_chk_var(eva,varnme)

! description:
! ------------------------------------------------------------------------------
! test variable name and make it uppercase
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   eva_chk_var    ! return value:>0: number of characters of the name given
                               !              -1: no name given
                               !              -2: name too long
                               !              -3: name contains wrong characters
                               !              -4: name does not start with a letter

! arguments
      character eva(*)*(*)     ! (I) array in which everything is saved

      character varnme*(*)     ! (I/O) variable name


! local variables
      integer   mxvlen,mxrec,nrec,leva

      integer   i,l,ret,d

      character c*1


! functions
      integer   cfn_length,&
                eva_get_set


! include files


! program section
! ------------------------------------------------------------------------------

! init
      ret=0


! query fixed data
      ret=eva_get_set(eva,leva,mxrec,mxvlen,nrec)


! test
      ! length
      l=cfn_length(varnme)

      if (l.le.0) then
! ERROR 1: no name given
         ret=-1
      endif


      ! trim variable
      if (ret.eq.0) then
         call cfn_s_trim(varnme)
         l=cfn_length(varnme)
         if (l.gt.mxvlen) then
! ERROR 2: name too long
            ret=-2
         endif
      endif


      ! test characters and adjust lowercase=>uppercase
      if (ret.eq.0) then
         d=ichar('A')-ichar('a')  ! adaptation for lower- to upper-case

         i=0
         do while(ret.eq.0 .and. i.lt.l)
            i=i+1
            c=varnme(i:i)
            if ((c.ge.'a' .and. c.le.'z')) then

               ! OK, it is a letter. now it just needs to be made uppercase
               varnme(i:i)=char(ichar(c)+d)

            else if (.not. (c.ge.'A' .and. c.le.'Z')) then
               if (.not. (c.ge.'0' .and. c.le.'9')) then
                  if (.not. (c.eq.'_')) then
! ERROR 3: name contains wrong characters
                     ret=-3
                  endif
               endif
            endif
         enddo

      endif


      ! test the first letter, it is supposed to be a character
      if (ret.eq.0) then
         c=varnme(1:1)
         if (.not. (c.ge.'A' .and. c.le.'Z')) then
! ERROR 4: name does not start with a letter
            ret=-4
         else
            ! whenever OK: ret=length variable name
            ret=l
         endif
      endif


! assign function value
      eva_chk_var = ret


! end of program
      return
      end

      function eva_def_var(eva,varnme,lvar)

! description:
! ------------------------------------------------------------------------------
! (re)define a variable, create space in the array
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   eva_def_var   ! return value: >0: position in eva array
                              !           -1..-4: error in name
                              !              -10: no space in eva


! arguments
      character eva(*)*(*)     ! (I) array in which everything is saved

      character varnme*(*)     ! (I) variable name

      integer   lvar           ! (I) number of characters space needed


! local variables
      integer   mxvlen,mxrec,nrec,leva

      integer   ret,nc,nr,ncr,pos,d,i,lvnm

      character lvarnme*128


! functions
      integer   cfn_length,&
                eva_fnd_var,&
                eva_get_set

! include files


! program section
! ------------------------------------------------------------------------------

! init
      ret =0


! query fixed data
      ret=eva_get_set(eva,leva,mxrec,mxvlen,nrec)


! determine the number of lines a variable needs to consist of
!     total number of characters: 1+1+len(varnme)+1+lvar+1
      lvarnme=varnme
      call cfn_s_upcase(lvarnme)
      lvnm=cfn_length(lvarnme)
      nc=lvnm+lvar+4
      nr=int((nc+leva-1)/leva)


! test if the name of the variable is not too long
      if (lvnm.gt.mxvlen) then
         ! ERROR, name of the variable too long
         ret=-15
      endif


! check if the variable already exists
      if (ret.eq.0) then
         pos=eva_fnd_var(eva,lvarnme)
         if (pos.gt.0) then
            ! current number of records in use for this variable
            ncr=ichar(eva(pos)(1:1))
         else if (pos.lt.0) then
            ! ERROR, well we can quit now
            ret=pos
         endif
      endif


! check if there is still enough space and if the number is not larger than 127
      if (ret.eq.0) then
         if (nr.gt.127) then
            ! number of records may not consist of more than 127 per variable
            ret=-11
         else if (pos.gt.0) then
            ! this variable is already defined, let's take a look if it is big enough
            if (ncr.ge.nr) then
               ! place enough, it can be filled in
               nr=ncr
            else
               ! not yet big enough
               ! is there still enough space to extend?
               d=nr-ncr
               if ((mxrec-nrec).ge.(nr-ncr)) then
                  ! yes, just proceed
                  do i=nrec,pos+ncr,-1
                     eva(i+d)=eva(i)
                  enddo

                  ! new nrec
                  nrec=nrec+d

                  ! fill in new length of variable
                  eva(pos)(1:1)=char(nr)

               else
                  ! ERROR, stop
                  ret=-12
               endif

            endif
         else
            ! define new variable, check if there is space
            if ((mxrec-nrec).lt.nr) then
               ! no
               ret=-10
            else
               ! yes, define it
               pos=nrec+1

               ! define var
               eva(pos)(1:1)=char(nr)             ! number of lines
               eva(pos)(2:2)=char(0)              ! reserved
               eva(pos)(3:2+lvnm)=lvarnme(1:lvnm) ! name variable
               eva(pos)(3+lvnm:3+lvnm)=char(0)    ! finish name
               i=lvnm+4                           ! position 1 value

               ! initialize value of space (= finish with char(0))
               if (i.gt.leva) then
                  ! first position of next line
                  eva(pos+1)(1:1)=char(0)
               else
                  ! first empty character
                  eva(pos)(i:i)  =char(0)
               endif

               ! new nrec
               nrec=nrec+nr
            endif
         endif
      endif


! fill in new eva prefix
      if (ret.eq.0) then
         ! nrec   = ichar(eva(1)(6:6))*256 + ichar(eva(1)(7:7))
         eva(1)(7:7)=char(mod(nrec,256))
         eva(1)(6:6)=char(int(nrec/256))
      endif


! determine return value
      if (ret.eq.0) then
         ret=pos
      endif


! assign function value
      eva_def_var = ret


! end of program
      return
      end
      function eva_fnd_var(eva,varnme)

! description:
! ------------------------------------------------------------------------------
! search a variable in EVA
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   eva_fnd_var    ! return value: >1: position in EVA where it is found
                               !                0: not found
                               !               <0: other error


! arguments
      character eva(*)*(*)     ! (I) array in which everything is saved

      character varnme*(*)     ! (I/O) variable name


! local variables
      integer   mxvlen,mxrec,nrec,leva

      integer   ret,lvar,pos,p

      character lvarnme*128


! functions
      integer   eva_chk_var,&
                eva_get_set

! include files


! program section
! ------------------------------------------------------------------------------

! init
      ret=0


! query fixed data
      ret=eva_get_set(eva,leva,mxrec,mxvlen,nrec)


! test the given variable name
      lvarnme=varnme
      lvar=eva_chk_var(eva,lvarnme)
      if (lvar.le.0) then
! ERROR *: wrong name
         ret=lvar
         if (ret.eq.0) ret=-10
      endif


! search for value
      if (ret.eq.0) then
         ! adding a char(0) to the name, this is how it is saved
         ! to detect the end of the name
         lvar=lvar+1
         lvarnme(lvar:lvar)=char(0)

         pos=0
         p=2     ! in 'record' 2 the first variable is located
         do while(p.le.nrec .and. pos.eq.0)
            if (eva(p)(3:lvar+2).eq.lvarnme(1:lvar)) then
               ! found, done
               pos=p
            else
               ! add the number of 'records' that are occupied by the
               ! current variable to p
               p=p+ichar(eva(p)(1:1))
            endif
         enddo

         if (pos.gt.0) then
            ! found, assign value
            ret=pos
         else
            ! 0: not found, no error
            ret=0
         endif
      endif


! assign function value
      eva_fnd_var = ret


! end of program
      return
      end

      function eva_get_set(eva,leva,mxrec,mxvlen,nrec)

! description:
! ------------------------------------------------------------------------------
! query the settings of the eva array
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   eva_get_set   ! return value:  0: OK
                              !              <>0: error


! arguments
      character eva(*)*(*)     ! (I) array in which everything is saved

      integer   leva,&         ! (O)   number of characters in a record
                mxrec,&        ! (O)   maximum number of records in eva-array
                mxvlen,&       ! (O)   maximum allowed length of a variable name
                nrec           ! (O)   number of records in use from eva-array


! local variables
      integer   ret


! functions


! include files


! program section
! ------------------------------------------------------------------------------

! init
      ret =0


! test array
      if (eva(1)(1:3).ne.'EVA') then
         ret=-1
      endif


! query fixed data
      if (ret.eq.0) then

         ! maximum allowed length of a variable name
         mxvlen = ichar(eva(1)(8:8))

         ! maximum number of records in eva-array
         mxrec  = ichar(eva(1)(4:4))*256 + ichar(eva(1)(5:5))

         ! number of records in use from eva-array
         nrec   = ichar(eva(1)(6:6))*256 + ichar(eva(1)(7:7))

         ! number of characters in a record
         leva   = len(eva(1))

      endif


! assign function value
      eva_get_set = ret


! end of program
      return
      end

      subroutine eva_put_i(eva,varnme,var,ret)

! description:
! ------------------------------------------------------------------------------
! save an integer value in the eva array under the name varnme
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      character eva(*)*(*)     ! (I) array in which everything is saved

      character varnme*(*)     ! (I) variable name

      integer   ret            ! (O) 0: OK

      integer   var            ! (I) value assigned to varnme


! local variables
      integer   l

      character cvar*32


! functions
      integer   cfn_length


! include files


! program section
! ------------------------------------------------------------------------------


! fill value in character
      write(cvar,'(i10)') var
      call cfn_s_trim(cvar)
      l=cfn_length(cvar)


! save via eva_put_c
      call eva_put_c(eva,varnme,cvar(1:l),ret)


! end of program
      return
      end

! ******************************************************************************
      subroutine eva_put_r(eva,varnme,var,ret)

! description:
! ------------------------------------------------------------------------------
! save a real value in the eva array under the name varnme
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      character eva(*)*(*)     ! (I) array in which everything is saved

      character varnme*(*)     ! (I) variable name

      integer   ret            ! (O) 0: OK

      real      var            ! (I) value assigned to varnme


! local variables
      integer   l

      character cvar*32


! functions
      integer   cfn_length


! include files


! program section
! ------------------------------------------------------------------------------


! fill value in character
      write(cvar,'(g15.7)') var
      call cfn_s_trim(cvar)
      l=cfn_length(cvar)


! save via eva_put_c
      call eva_put_c(eva,varnme,cvar(1:l),ret)


! end of program
      return
      end

! ******************************************************************************

      subroutine eva_put_c(eva,varnme,var,ret)

! description:
! ------------------------------------------------------------------------------
! save a character value in the eva array under the name varnme
! PAY ATTENTION! the length of the saved string contains len(var) characters!
!         that means: the value of var is being saved, no trim

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      character eva(*)*(*)     ! (I) array in which everything is saved

      character varnme*(*)     ! (I) variable name

      integer   ret            ! (O) 0: OK

      character var*(*)        ! (I) value assigned to varnme


! local variables
      integer   lvar,pos

      character lvarnme*128


! functions
      integer   eva_def_var,&
                eva_put_str


! include files


! program section
! ------------------------------------------------------------------------------


! take over varnme in local variable because it is possible that it is adjusted
      lvarnme=varnme
      lvar=len(var)


! (re)define variable
      pos=eva_def_var(eva,lvarnme,lvar)


! fill in value
      if (pos.gt.0) then
         ret=eva_put_str(eva,pos,var,lvar)
      else
         ret=pos
      endif


! end of program
      return
      end

! ******************************************************************************

      function eva_put_str(eva,pos,var,lvar)

! description:
! ------------------------------------------------------------------------------
! fill in a string in record pos of the eva array
! in record pos is also the name of the variable defined


! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer    eva_put_str   ! return value: 0: OK                        !*SSD*
                               !             <>0: ERROR                     !*SFD*


! arguments
      character eva(*)*(*)     ! (I/O) array in which everything is saved

      integer   pos,&          ! (I) record position in eva where data needs to be saved
                lvar           ! (I) number of characters of var that needs to be saved

      character var*(*)        ! (I) value to save


! local variables
      integer   mxvlen,mxrec,nrec,leva

      integer   ret,iv,nt,nn,nd,p,n,nrest,i


! functions
      integer   eva_get_set


! include files


! program section
! ------------------------------------------------------------------------------

! init
      ret=0


! query fixed data
      ret=eva_get_set(eva,leva,mxrec,mxvlen,nrec)


! calculate the number of needed records
      ! search for the end of the name of the variable
      ! it is not tested if the variable is defined right
      ! according to eva(2...) records (see readme.txt)
      iv=index(eva(pos)(3:leva),char(0))+2

      nt=leva-iv  ! number of characters left at the end of this record
      nn=int((lvar+1-nt+leva-1)/leva)+1  ! number of records needed

      nd=ichar(eva(pos)(1:1))          ! number of records defined

      if (nn.gt.nd) then
         ! too small, define again
         ! being developed, for now error
         ret=1
      endif


! fill in
      if (ret.eq.0) then

         if (nt.gt.0) then
            ! there is still space in the first line
            p=pos
            iv=iv+1
         else
            ! first line is full, go on with the next one
            p=pos+1
            iv=1
         endif

         i=1         ! position of variable that still needs to be filled in
         nrest=lvar  ! number of characters that still need to be filled in
         do while (i.le.lvar)

            ! determine number of characters that still fit in the current line
            n=min(nrest,leva-iv+1)  ! at the most the rest of the line

            ! fill in part-value
            eva(p)(iv:iv+n-1)=var(i:i+n-1)

            ! new numbers
            iv=iv+n
            i =i +n
            if (iv.gt.leva) then
               p=p+1
               iv=1
            endif
            nrest=nrest-n

         enddo

         ! finish with a 0-character
         eva(p)(iv:iv)=char(0)

      endif


! assign function value
      eva_put_str = ret


! end of program
      return
      end

      function cfn_getlun(lunb,lune)

! A.L.		27 May 1998	v2r0	renamed function get_lun -> cfn_getlun

! query a free unit number
! this number needs to be in the interval [lunb,lune]

! return value: >0 : free unit number
!                <=0: no number available


      integer cfn_getlun
      integer lunb,lune

      logical opened
      integer lun



      if (lunb.le.lune) then

         lun=lunb
         inquire(unit=lun,opened=opened)
         do while (opened.and.lun.lt.lune)
            lun=lun+1
            inquire(unit=lun,opened=opened)
         enddo

         if (.not. opened) then
            cfn_getlun = lun
         else
            cfn_getlun = -1
         endif

      else
         cfn_getlun = -1
      endif


      return
      end

      function cfn_idx_get_i(sv,sid,nid,index)

! description:
! ------------------------------------------------------------------------------
! find the index number of the value sv in array sid
! search method is binary search

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   cfn_idx_get_i   ! return value: #: number of found
                                !                  matched elements


! arguments
      integer   index,&         ! (O) return index number
                                !     the index number is the first
                                !     matched value in array sid
                                !     if no value matched index is the
                                !     position where sv can be inserted
                                !     If sv > sid(nin) then index=nin+1
                nid             ! (I) number of elements in sid array

      integer   sv,&            ! (I) value to be searched in sid
                sid(nid)        ! (I) values array


! local variables
      integer   i,ib,im,ie,ntry

      real      log2
      parameter (log2=0.6931471)

      logical   continue


! functions


! include files


! program section
! ------------------------------------------------------------------------------


! binary search

      ib=0
      ie=nid

      ntry=2+int(log(nid*1.0)/log2)

      do i=1,ntry
         im=(ie+1 + ib)/2
         if (sv.gt.sid(im)) then
            ib=im
         else
            ie=im
         endif
      enddo


! find full interval
      ie=im
      ib=im

!      do while (ib.gt.1 .and. sid(ib-1).ge.sv)
!         ib=ib-1
!      enddo
!
!      do while (ie.lt.nid .and. sid(ie+1).eq.sv)
!         ie=ie+1
!      enddo

      continue = ib.gt.1
      do while (continue)
         if (sid(ib-1).ge.sv) then
            ib=ib-1
            continue = ib.gt.1
         else
            continue = .false.
         endif
      enddo

      continue = ie.lt.nid
      do while (continue)
         if (sid(ie+1).eq.sv) then
            ie=ie+1
            continue = ie.lt.nid
         else
            continue = .false.
         endif
      enddo


! fill in results
      if (sid(ib).eq.sv) then
         index=ib
         cfn_idx_get_i=ie-ib+1
      else
         ! value not found
         index=ib
         if (sv.gt.sid(index)) index=index+1
         cfn_idx_get_i=0
      endif


! end of program
      return
      end

! ******************************************************************************

      function cfn_idx_get_r(sv,sid,nid,index)

! description:
! ------------------------------------------------------------------------------
! find the index number of the value sv in array sid
! search method is binary search


! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   cfn_idx_get_r   ! return value: #: number of found
                                !                  matched elements


! arguments
      integer   index,&         ! (O) return index number
                                !     the index number is the first
                                !     matched value in array sid
                nid             ! (I) number of elements in sid array

      real      sv,&            ! (I) value to be searched in sid
                sid(nid)        ! (I) values array


! local variables
      integer   i,ib,im,ie,ntry

      real      log2
      parameter (log2=0.6931471)

      logical   continue


! functions


! include files


! program section
! ------------------------------------------------------------------------------


! binary search

      ib=0
      ie=nid

      ntry=2+int(log(nid*1.0)/log2)

      do i=1,ntry
         im=(ie+1 + ib)/2
         if (sv.gt.sid(im)) then
            ib=im
         else
            ie=im
         endif
      enddo


! find full interval
      ie=im
      ib=im

!      do while (ib.gt.1 .and. sid(ib-1).ge.sv)
!         ib=ib-1
!      enddo
!
!      do while (ie.lt.nid .and. sid(ie+1).eq.sv)
!         ie=ie+1
!      enddo

      continue = ib.gt.1
      do while (continue)
         if (sid(ib-1).ge.sv) then
            ib=ib-1
            continue = ib.gt.1
         else
            continue = .false.
         endif
      enddo

      continue = ie.lt.nid
      do while (continue)
         if (sid(ie+1).eq.sv) then
            ie=ie+1
            continue = ie.lt.nid
         else
            continue = .false.
         endif
      enddo


! fill in results
      if (sid(ib).eq.sv) then
         index=ib
         cfn_idx_get_r=ie-ib+1
      else
         ! value not found
         index=ib
         if (sv.gt.sid(index)) index=index+1
         cfn_idx_get_r=0
      endif

! test
      if (index.lt.1) then
         write(*,*) ' ERROR cfn_idx_get_r: index ',&
                       index
      else if (index.eq.1) then
         if (sv.gt.sid(1)) then
            write(*,*) ' ERROR cfn_idx_get_r: index,nid,sv,sid(nid) ',&
                       index,nid,sv,sid(nid)
         endif
      else if (index.gt.nid) then
         if (sv.le.sid(nid)) then
            write(*,*) ' ERROR cfn_idx_get_r: index,nid,sv,sid(nid) ',&
                       index,nid,sv,sid(nid)
         endif
      else
         if (sv.gt.sid(index) .or. sv.le.sid(index-1)) then
            write(*,*) ' ERROR cfn_idx_get_r: index,nid,sv,sid(index) ',&
                       index,nid,sv,sid(index)
         endif
      endif


! end of program
      return
      end

! ******************************************************************************

      function cfn_idx_get_d(sv,sid,nid,index)

! description:
! ------------------------------------------------------------------------------
! find the index number of the value sv in array sid
! search method is binary search


! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   cfn_idx_get_d   ! return value: #: number of found
                                !                  matched elements


! arguments
      integer   index,&         ! (O) return index number
                                !     the index number is the first
                                !     matched value in array sid
                nid             ! (I) number of elements in sid array

      double precision &
                sv,&            ! (I) value to be searched in sid
                sid(nid)        ! (I) values array


! local variables
      integer   i,ib,im,ie,ntry

      real      log2
      parameter (log2=0.6931471)

      logical   continue


! functions


! include files


! program section
! ------------------------------------------------------------------------------


! binary search

      ib=0
      ie=nid

      ntry=2+int(log(nid*1.0)/log2)

      do i=1,ntry
         im=(ie+1 + ib)/2
         if (sv.gt.sid(im)) then
            ib=im
         else
            ie=im
         endif
      enddo


! find full interval
      ie=im
      ib=im

!      do while (ib.gt.1 .and. sid(ib-1).ge.sv)
!         ib=ib-1
!      enddo
!
!      do while (ie.lt.nid .and. sid(ie+1).eq.sv)
!         ie=ie+1
!      enddo

      continue = ib.gt.1
      do while (continue)
         if (sid(ib-1).ge.sv) then
            ib=ib-1
            continue = ib.gt.1
         else
            continue = .false.
         endif
      enddo

      continue = ie.lt.nid
      do while (continue)
         if (sid(ie+1).eq.sv) then
            ie=ie+1
            continue = ie.lt.nid
         else
            continue = .false.
         endif
      enddo


! fill in results
      if (sid(ib).eq.sv) then
         index=ib
         cfn_idx_get_d=ie-ib+1
      else
         ! value not found
         index=ib
         if (sv.gt.sid(index)) index=index+1
         cfn_idx_get_d=0
      endif


! end of program
      return
      end

! ******************************************************************************

      function cfn_idx_get_c(sv,lsv,sid,lsid,nid,index)

! description:
! ------------------------------------------------------------------------------
! find the index number of the value sv in array sid
! search method is binary search

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   cfn_idx_get_c   ! return value: #: number of found
                                !                  matched elements


! arguments
      integer   index,&         ! (O) return index number
                                !     the index number is the first
                                !     matched value in array sid
                nid,&           ! (I) number of elements in sid array
                lsv,&           ! (I) number of characters of sv
                lsid            ! (I) number of characters of sid

      character sv(lsv)*1,&     ! (I) value to be searched in sid
                sid(lsid,nid)*1 ! (I) values array


! local variables
      integer   i,ib,im,ie,ntry,l1,l2

      real      log2
      parameter (log2=0.6931471)

      logical   continue


! functions
      integer   cfn_length2

      logical   chf_ge,&
                chf_eq,&
                chf_gt


! include files


! program section
! ------------------------------------------------------------------------------


! binary search

      ib=0
      ie=nid

      ntry=2+int(log(nid*1.0)/log2)

      l1=cfn_length2(sv,lsv)

      do i=1,ntry
         im=(ie+1 + ib)/2
         l2=cfn_length2(sid(1,im),lsid)
         if (chf_gt(sv,l1,sid(1,im),l2)) then
            ib=im
         else
            ie=im
         endif
      enddo


! find full interval
      ie=im
      ib=im

!      do while (ib.gt.1   .and.
!     1          chf_ge(sid(1,ib-1),cfn_length2(sid(1,ib-1),lsid),sv,l1))
!         ib=ib-1
!      enddo
!
!      do while (ie.lt.nid .and.
!     1          chf_eq(sid(1,ie+1),cfn_length2(sid(1,ie+1),lsid),sv,l1))
!         ie=ie+1
!      enddo

      continue = ib.gt.1
      do while (continue)
       if (chf_ge(sid(1,ib-1),cfn_length2(sid(1,ib-1),lsid),sv,l1)) then
            ib=ib-1
            continue = ib.gt.1
       else
            continue = .false.
       endif
      enddo

      continue = ie.lt.nid
      do while (continue)
       if (chf_eq(sid(1,ie+1),cfn_length2(sid(1,ie+1),lsid),sv,l1)) then
            ie=ie+1
            continue = ie.lt.nid
       else
            continue = .false.
       endif
      enddo


! fill in results
      if (chf_eq(sid(1,ib),cfn_length2(sid(1,ib),lsid),sv,l1)) then
         index=ib
         cfn_idx_get_c=ie-ib+1
      else
         ! value not found
         index=ib
         if (chf_gt(sv,l1,sid(1,index),cfn_length2(sid(1,index),lsid)))&
                           index=index+1
         cfn_idx_get_c=0
      endif


! end of program
      return
      end

! ******************************************************************************

      function cfn_fidx_i(sv,sid,nid)

! description:
! ------------------------------------------------------------------------------
! find the first index number of the value sv in array sid
! search method is binary search

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   cfn_fidx_i      ! return value: >0: first index found
                                !                0: value not found


! arguments
      integer   nid             ! (I) number of elements in sid array

      integer   sv,&            ! (I) value to be searched in sid
                sid(nid)        ! (I) values array

      logical   continue


! local variables
      integer   i,ib,im,ie,ntry

      real      log2
      parameter (log2=0.6931471)


! functions


! include files


! program section
! ------------------------------------------------------------------------------


! binary search

      ib=0
      ie=nid

      ntry=2+int(log(nid*1.0)/log2)

      do i=1,ntry
         im=(ie+1 + ib)/2
         if (sv.gt.sid(im)) then
            ib=im
         else
            ie=im
         endif
      enddo


! find first value
      ib=im

!      do while (ib.gt.1 .and. sid(ib-1).ge.sv)
!         ib=ib-1
!      enddo

      continue = ib.gt.1
      do while (continue)
         if (sid(ib-1).ge.sv) then
            ib=ib-1
            continue = ib.gt.1
         else
            continue = .false.
         endif
      enddo


! assign function value
      if (sid(ib).eq.sv) then
         cfn_fidx_i=ib
      else
         cfn_fidx_i=0
      endif


! end of program
      return
      end

! ******************************************************************************

      function cfn_fidx_r(sv,sid,nid)

! description:
! ------------------------------------------------------------------------------
! find the first index number of the value sv in array sid
! search method is binary search

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   cfn_fidx_r      ! return value: >0: first index found
                                !                0: value not found


! arguments
      integer   nid             ! (I) number of elements in sid array

      real      sv,&            ! (I) value to be searched in sid
                sid(nid)        ! (I) values array


! local variables
      integer   i,ib,im,ie,ntry

      real      log2
      parameter (log2=0.6931471)

      logical   continue


! functions


! include files


! program section
! ------------------------------------------------------------------------------


! binary search

      ib=0
      ie=nid

      ntry=2+int(log(nid*1.0)/log2)

      do i=1,ntry
         im=(ie+1 + ib)/2
         if (sv.gt.sid(im)) then
            ib=im
         else
            ie=im
         endif
      enddo


! find first value
      ib=im

!      do while (ib.gt.1 .and. sid(ib-1).ge.sv)
!         ib=ib-1
!      enddo

      continue = ib.gt.1
      do while (continue)
         if (sid(ib-1).ge.sv) then
            ib=ib-1
            continue = ib.gt.1
         else
            continue = .false.
         endif
      enddo


! assign function value
      if (sid(ib).eq.sv) then
         cfn_fidx_r=ib
      else
         cfn_fidx_r=0
      endif


! end of program
      return
      end


! ******************************************************************************

      function cfn_fidxrev_r(sv,sid,nid)

! description:
! ------------------------------------------------------------------------------
! find the first index number of the value sv in array sid
! search method is binary search
! array SID is ordered in reverse order

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   cfn_fidxrev_r   ! return value: >0: first index found
                                !                0: value not found


! arguments
      integer   nid             ! (I) number of elements in sid array

      real      sv,&            ! (I) value to be searched in sid
                sid(nid)        ! (I) values array


! local variables
      integer   i,ib,im,ie,ntry

      real      log2
      parameter (log2=0.6931471)

      logical   continue


! functions


! include files


! program section
! ------------------------------------------------------------------------------


! binary search

      ib=0
      ie=nid

      ntry=2+int(log(nid*1.0)/log2)

      do i=1,ntry
         im=(ie+1 + ib)/2
         if (sv.lt.sid(im)) then
            ib=im
         else
            ie=im
         endif
      enddo


! find first value
      ib=im

!      do while (ib.gt.1 .and. sid(ib-1).ge.sv)
!         ib=ib-1
!      enddo

      continue = ib.lt.nid
      do while (continue)
         if (sid(ib+1).le.sv) then
            ib=ib+1
            continue = ib.lt.nid
         else
            continue = .false.
         endif
      enddo


! assign function value
      if (sid(ib).eq.sv) then
         cfn_fidxrev_r=ib
      else
         cfn_fidxrev_r=0
      endif


! end of program
      return
      end


! ******************************************************************************

      function cfn_fidxnear_r(sv,sid,nid)

! description:
! ------------------------------------------------------------------------------
! find the index number of the value sv in array sid which is nearest
! search method is binary search
! SID may be in ascending or descending order

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   cfn_fidxnear_r  ! return value: >0: first index found
                                !                0: value not found


! arguments
      integer   nid             ! (I) number of elements in sid array

      real      sv,&            ! (I) value to be searched in sid
                sid(nid)        ! (I) values array


! local variables
      integer   i,ib,im,ie,ntry,idir

      real      log2
      parameter (log2=0.6931471)

      real      del1,del2

      logical   continue


! functions


! include files


! program section
! ------------------------------------------------------------------------------


! check ascending or descending order
      if (sid(1).le.sid(nid)) then
         ! ascending order
         idir=1
      else
         idir=-1
      endif


! binary search

      ib=0
      ie=nid

      ntry=2+int(log(nid*1.0)/log2)

      do i=1,ntry
         im=(ie+1 + ib)/2
         if ((idir*sv).gt.(idir*sid(im))) then
            ib=im
         else
            ie=im
         endif
      enddo


! find nearest value
      ib=im

      del1=abs(sid(ib)-sv)
      ! search downwards
      continue = ib.gt.1
      do while (continue)
         del2=abs(sid(ib-1)-sv)
         if (del2.le.del1) then
            ib=ib-1
            del1=del2
            continue = ib.gt.1
         else
            continue = .false.
         endif
      enddo
      ! search upwards
      continue = ib.lt.nid
      do while (continue)
         del2=abs(sid(ib+1)-sv)
         if (del2.lt.del1) then
            ib=ib+1
            del1=del2
            continue = ib.lt.nid
         else
            continue = .false.
         endif
      enddo


! assign function value
      cfn_fidxnear_r=ib


! end of program
      return
      end


! ******************************************************************************

      function cfn_fidxlow_r(sv,sid,nid)

! description:
! ------------------------------------------------------------------------------
! Find the first InDeX number in real array sid from which the value is within the interval [sv,...]
! search method is binary search
! SID must be in ascending order

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   cfn_fidxlow_r    ! return value: >0: found index number
                                 !                0: value not found


! arguments
      integer   nid             ! (I) number of elements in sid array

      real      sv,&            ! (I) value to be searched in sid
                sid(nid)        ! (I) values array


! local variables
      integer   i,ib,im,ie,ntry

      real      log2
      parameter (log2=0.6931471)

      logical   continue


! functions


! include files


! program section
! ------------------------------------------------------------------------------



! binary search

      ib=0
      ie=nid

      ntry=2+int(log(nid*1.0)/log2)

      do i=1,ntry
         im=(ie+1 + ib)/2
         if (sv.gt.sid(im)) then
            ib=im
         else
            ie=im
         endif
      enddo


! find exact value
      ib=im

      ! search downwards
      continue = ib.gt.1
      do while (continue)
         if (sv.ge.sid(ib-1)) then
            ib=ib-1
            continue = ib.gt.1
         else
            continue = .false.
         endif
      enddo
      ! search upwards
      continue = ib.lt.nid
      do while (continue)
         if (sv.gt.sid(ib)) then
            ib=ib+1
            continue = ib.lt.nid
         else
            continue = .false.
         endif
      enddo


! assign function value
      if (sv.gt.sid(ib)) ib=0   ! no values within interval
      cfn_fidxlow_r=ib


! end of program
      return
      end


! ******************************************************************************

      function cfn_fidxupp_r(sv,sid,nid)

! description:
! ------------------------------------------------------------------------------
! Find the last InDeX number in real array sid from which the value is within the interval [...,sv]
! search method is binary search
! SID must be in ascending order

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   cfn_fidxupp_r    ! return value: >0: found index number
                                 !                0: value not found


! arguments
      integer   nid             ! (I) number of elements in sid array

      real      sv,&            ! (I) value to be searched in sid
                sid(nid)        ! (I) values array


! local variables
      integer   i,ib,im,ie,ntry

      real      log2
      parameter (log2=0.6931471)

      logical   continue


! functions


! include files


! program section
! ------------------------------------------------------------------------------



! binary search

      ib=0
      ie=nid

      ntry=2+int(log(nid*1.0)/log2)

      do i=1,ntry
         im=(ie+1 + ib)/2
         if (sv.gt.sid(im)) then
            ib=im
         else
            ie=im
         endif
      enddo


! find exact value
      ib=im

      ! search downwards
      continue = ib.gt.1
      do while (continue)
         if (sv.lt.sid(ib)) then
            ib=ib-1
            continue = ib.gt.1
         else
            continue = .false.
         endif
      enddo
      ! search upwards
      continue = ib.lt.nid
      do while (continue)
         if (sv.ge.sid(ib+1)) then
            ib=ib+1
            continue = ib.lt.nid
         else
            continue = .false.
         endif
      enddo


! assign function value
      if (sv.lt.sid(ib)) ib=0   ! no values within interval
      cfn_fidxupp_r=ib


! end of program
      return
      end


! ******************************************************************************

      function cfn_fidxint_r(sv,sid,nid)

! description:
! ------------------------------------------------------------------------------
! Find the last InDeX number in real array sid for which sv is element of [sid(idx),sid(idx+1)>
! or sv element of [sid(idx),sid(idx+1)] when sid(idx).eq.sid(idx+1)
! search method is binary search
! SID must be in ascending order

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   cfn_fidxint_r    ! return value: >0: found index number
                                 !                0: value not found


! arguments
      integer   nid             ! (I) number of elements in sid array

      real      sv,&            ! (I) value to be searched in sid
                sid(nid)        ! (I) values array


! local variables
      integer   i,ib,im,ie,ntry

      real      log2
      parameter (log2=0.6931471)

      logical   continue


! functions


! include files


! program section
! ------------------------------------------------------------------------------



! binary search

      ib=0
      ie=nid

      ntry=2+int(log(nid*1.0)/log2)

      do i=1,ntry
         im=(ie+1 + ib)/2
         if (sv.gt.sid(im)) then
            ib=im
         else
            ie=im
         endif
      enddo


! find exact value
      ib=im

      ! search downwards
      continue = ib.gt.1
      do while (continue)
         if (sv.lt.sid(ib) .or. sv.eq.sid(ib-1)) then
            ib=ib-1
            continue = ib.gt.1
         else
            continue = .false.
         endif
      enddo
      ! search upwards
      continue = ib.lt.nid
      do while (continue)
         if (sv.gt.sid(ib+1).or.(sv.gt.sid(ib).and.sv.eq.sid(ib+1)))then
            ib=ib+1
            continue = ib.lt.nid
         else
            continue = .false.
         endif
      enddo

! final check
      if (sv.lt.sid(ib)) then
         ib=0
      endif

! assign function value
      cfn_fidxint_r=ib


! end of program
      return
      end

!
! DATUM: 11/01/93
!        28/10/93 function lengte 10 added
! A.L.		14 May 1997	v2r0	functions provided with cfn_<...>
!					lengte -> length
!
      integer function cfn_length(charac)

      implicit none

      character charac*(*)
      integer i

      integer cfn_length2 ! function

      i=len(charac)

      cfn_length=cfn_length2(charac,i)

      return
      end
!*******************************************************************************
      integer function cfn_length2(charac,lc)

      implicit none

      integer   lc
      character charac(lc)*1

      integer   i
      logical   doorgaan

      character null*1
!c      parameter (null=char(0))  ! Lahey, Sun-Unix, pgf
!c      parameter (null=0)        ! Sun-Unix, gnu


      null=char(0)
      i=lc

      if (i.gt.0) then
         doorgaan=.true.
      else
         doorgaan=.false.
         i=0
      endif

      do while(doorgaan)
         if (i.gt.0) then
            if (charac(i).eq.' ' .or. charac(i).eq.null) then
               i=i-1
            else
               doorgaan=.false.
            endif
         else
            doorgaan=.false.
         endif
      enddo

      cfn_length2=i

      return
      end
!*******************************************************************************
      integer function cfn_length0(charac,lc)

      implicit none

      integer   lc
      character null*1
      character charac(lc)*1
      integer i

      null=char(0)
      i=lc

      do while(i.gt.0 .and. (charac(i).eq.null))
         i=i-1
      enddo

      cfn_length0=i

      return
      end
!*******************************************************************************
      integer function cfn_length10(charac,lc)

      implicit none

      integer   lc
      character null*1
      character charac(lc)*1
      integer i

      null=char(0)
      i=1

      do while((charac(i).ne.null).and.i.lt.lc)
         i=i+1
      enddo

      if (charac(i).eq.null) i=i-1

      cfn_length10=i

      return
      end

      function cfn_lindex(string,subs)

! description:
! ------------------------------------------------------------------------------
! search the last index of subs in string
! an inverse 'index' function


! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer cfn_lindex        ! return value: 0: index not found
                                !              >0: start position


! arguments
      character string*(*),&    ! (I) string in which must be searched
                subs*(*)        ! (I) string to search


! local variables
!      integer   lstring,lsubs,i


! functions
!      integer   cfn_length
      integer   osd_rindex


! include files


! program section
! ------------------------------------------------------------------------------

      cfn_lindex=osd_rindex(string,subs)

!      lstring=cfn_length(string)
!      lsubs  =cfn_length(subs)
!
!      if (lsubs.gt.lstring) then
!c ERROR not possible
!         cfn_lindex=0
!      else
!         i=lstring-lsubs+1
!         do while (i.gt.0 .and. string(i:i+lsubs-1).ne.subs(1:lsubs))
!            i=i-1
!         enddo
!
!         if (i.gt.0) then
!            if (string(i:i+lsubs-1).eq.subs(1:lsubs)) then
!               cfn_lindex=i
!            else
!               cfn_lindex=0
!            endif
!         else
!            cfn_lindex=0
!         endif
!
!      endif


! end of program
      return
      end

!
! DATUM: 14/04/92
!        15/11/95 lowcase2 changed because of "subscript out of range"
! A.L.		14 May 1997	v2r0	functions provided with cfn_<...>
!
      function cfn_lowcase(arg)

! Function to convert all capital letters in a character variable
! into small letters.

      character arg*(*),cfn_lowcase*(*),cfn_lowcase2*256

      integer   l

      l=len(arg)

      cfn_lowcase=cfn_lowcase2(arg,l)

      return
      end
!*******************************************************************************
      function cfn_lowcase2(arg,larg)

      implicit none

! Function to convert all capital letters in a character variable
! into small letters.

      integer   larg
      character arg(larg)*1,cfn_lowcase2*(*),hc*1

      integer   i,l,n

      cfn_lowcase2=' '

      l=larg

      do i=1,l
         hc=arg(i)
         if (hc.le.'Z' .and. hc.ge.'A') then
            n=ichar(hc)
            n=n+32
            cfn_lowcase2(i:i)=char(n)
         else
            cfn_lowcase2(i:i)=hc
         endif
      enddo

      return
      end
!*******************************************************************************
      subroutine cfn_s_lowcase(arg)

      implicit none

! subroutine to convert all capital letters in a character variable
! into small letters.

      character arg*(*)

      character hc*1

      integer   i,l,n

      l=len(arg)

      do i=1,l
         hc=arg(i:i)
         if (hc.le.'Z' .and. hc.ge.'A') then
            n=ichar(hc)
            n=n+32
            arg(i:i)=char(n)
         endif
      enddo

      return
      end

!*******************************************************************************

      subroutine cfn_s_lowcase2(arg,l)

      implicit none

! subroutine to convert all capital letters in a character variable
! into small letters.

      character arg*(*)
      integer   l


      character hc*1

      integer   i,n


      do i=1,l
         hc=arg(i:i)
         if (hc.le.'Z' .and. hc.ge.'A') then
            n=ichar(hc)
            n=n+32
            arg(i:i)=char(n)
         endif
      enddo

      return
      end

      subroutine modellhs1(PDELR,ORGNCOL,newncol,&
                          IC1,IC2,OC1,OC2,INC,fincr,powr,&
                          NOMINCELL,NOMaxCELL,lclip)

! description:
! ------------------------------------------------------------------------------
!
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      INTEGER    ORGNCOL,&        ! (I) original number of columns
                 newncol,&        ! (O) new number of columns
                 NOMINCELL,&      ! (I) minimum number of cells (rows/columns)
                                  !     in an upscaled cell
                 NOMaxCELL,&      ! (I) maximum number of cells (rows/columns)
                                  !     in an upscaled cell
                 IC1,&            ! (I) column number 1 of unscaled area
                 IC2,&            ! (I) column number 2 of unscaled area
                 OC1,&            ! (O) column number 1 of scaled area
                 OC2              ! (O) column number 2 of scaled area

      REAL       INC,&            ! (I) start increment factor
                 powr             ! (I) power
      double precision fincr      ! (I) factor increment factor
                                  !     Used scale factor:
                                  !        f=fincr*(x-1)^powr+inc
                                  !        step=f*step
                                  !    x: cell position offset from AOI

      INTEGER    PDELR(ORGNCOL)   ! (O) pointer from unscaled to scaled
                                  !     column numbers

      logical   lclip             ! .true.  use clip-edges
                                  ! .false. don't use clip edges
                                  ! clip edge: first and last upscaled cell
                                  !            exist of one unscaled cell

! local variables
      INTEGER    ISC,L,I,J,ICOL,CCOL,ncel,idir

      integer    maxscc   ! maximum number of rows/columns to put together

      real       x
      double precision  STEP,f


! functions


! include files


! program section
! ------------------------------------------------------------------------------


! get Area Of Interest
      isc = min(ic1,ic2)
      ccol= max(ic1,ic2)

      maxscc=nomaxcell


!##ccol fits minimal cell value
      I=((INT((CCOL-ISC)/NOMINCELL)+1)*NOMINCELL)-1
      CCOL=ISC+I


!##correct if ccol.gt.orgncol:i<0
      I=ORGNCOL-CCOL
      IF(I.LT.0)THEN
         ISC=MAX(1,ISC+I)
         CCOL=MAX(1,CCOL+I)
      ENDIF

!##correct if ccol.lt.1
      I=CCOL-1
      IF(I.LT.0)THEN
!         ISC =MIN(NCOL,ISC-I)
!         CCOL=MIN(NCOL,CCOL-I)
         ISC =MIN(ORGNCOL,ISC-I)
         CCOL=MIN(ORGNCOL,CCOL-I)
      ENDIF


!##computation of column-definition
! Area Of Interest
      L=0
      I=1
      DO ICOL=ISC,CCOL
         PDELR(ICOL)=L
!         write(*,*) ' AOI icol,L: ',icol,L
         I=I+1
         IF(I.GT.NOMINCELL)THEN
            I=1
            L=L+1
         ENDIF
      END DO
      IF(I.NE.1)L=L+1

! Area 'higher' and 'lower' than AOI

! 'lower' part
      idir=-1
      icol=isc
      !--------
      j   =pdelr(icol)
      icol=icol+idir
      step=inc
      x=1.

      do while(icol.ge.1)

         ncel=nint(step)*nomincell
         ncel=min(ncel,maxscc)
         j=j+idir

         do i=icol,min(orgncol,max(icol+idir*(ncel-1),1)),idir
!      write(*,*) ' i,j,ncel,step: ',i,j,ncel,step
            pdelr(i)=j
!         write(*,*) ' LOW icol,L: ',icol,L
         enddo
         icol=icol+idir*ncel

         if (ncel.lt.maxscc) then
            x=x+ncel
            f=fincr*(x-1.)**powr+inc
            step=f*step
         endif

      enddo

! 'upper' part
      idir=+1
      icol=ccol
      !--------
      j   =pdelr(icol)
      icol=icol+idir
      step=inc
      x=1.

      do while(icol.le.orgncol)

         ncel=nint(step)*nomincell
         ncel=min(ncel,maxscc)
         j=j+idir

         do i=icol,min(orgncol,max(icol+idir*(ncel-1),1)),idir
!      write(*,*) ' i,j,ncel,step: ',i,j,ncel,step
            pdelr(i)=j
!         write(*,*) ' UPP icol,L: ',icol,L
         enddo
         icol=icol+idir*ncel

         if (ncel.lt.maxscc) then
            x=x+ncel
            f=fincr*(x-1.)**powr+inc
            step=f*step
         endif

      enddo


! when clip option is active (lclip=.true.)
! The outermost active upscaled cell must consist at only 1 unscaled cell!
      if (lclip) then

         ! set edge cells to a width of 1
         if (pdelr(1).eq.pdelr(2)) then
            pdelr(1)=pdelr(1)-1
         endif

         if (pdelr(orgncol).eq.pdelr(orgncol-1)) then
            pdelr(orgncol)=pdelr(orgncol)+1
         endif

      endif


! renumber pdelr to let it undefined with value 1 in pdelr(1)
      J=1-PDELR(1)
      DO ICOL=1,orgncol
         PDELR(ICOL)=PDELR(ICOL)+J
      END DO


! assign number of scaled are cells
      newncol=PDELR(orgncol)


! Area Of Interest in scaled area
      oc1=PDELR(ic1)
      oc2=PDELR(ic2)

!      write(*,*) ' modellhs: nnew nold AOI ',newncol,orgncol,ic1,ic2
!      write(*,*) PDELR

! end of program
      return
      end
!
#include "utl.h"

      subroutine osd_chdir(dir)

! description:
! ------------------------------------------------------------------------------
! change directory
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      character dir*(*)          ! (I)


! local variables


! functions


! include files


! program section
! ------------------------------------------------------------------------------


#if   (defined(OSD_CMP_IFORT))
! Intel Fortran compiler (DOS)
      call chdir(dir)                                        ! OSD_CMP_IFORT

#elif (defined(OSD_CMP_CVF))
! Compaq Visual Fortran (DOS)
      call chdir(dir)                                        ! OSD_CMP_CVF

#elif (defined(OSD_CMP_LF90))
! Lahey 90 (DOS)
      call iosdirchange(dir)                                 ! OSD_CMP_LF90

#elif (defined(OSD_CMP_PGF))
! Portland Group Fortran (LINUX)
      call chdir(dir)                                        ! OSD_CMP_PGF

#elif (defined(OSD_CMP_IFC))
! Intel Fortran (LINUX)
      call chdir(dir)                                        ! OSD_CMP_IFC

#elif (defined(OSD_CMP_GNU))
! GNU (...)
      call chdir(dir)                                        ! OSD_CMP_GNU

#elif (defined(OSD_CMP_SUN))
! SUN compiler
      call chdir(dir)                                        ! OSD_CMP_SUN

#else
! the next line is included to let the compiler crash when no symbol matched
      ERROR, unknown compiler
#endif


! end of program
      return
      end
!
#include "utl.h"

      function osd_direxists(dir)

! description:
! ------------------------------------------------------------------------------
! query if a directory exist
!

! declaration section
! ------------------------------------------------------------------------------

!:sel:lf90:      use winteracter

      implicit none


! function declaration
      logical    osd_direxists   ! return value: .true. : directory exists
                                 !               .false.: directory doesn't exist

! arguments
      character dir*(*)          ! (I)


! local variables
      integer   ios

      logical   lexist


! functions
#if   (defined(OSD_CMP_LF90))
! Lahey 90 (DOS)
      logical   iosdirexists                                 ! OSD_CMP_LF90
#elif (defined(OSD_CMP_CVF))
      logical   iosdirexists                                 ! OSD_CMP_CVF && IFORT
#endif

! include files


! program section
! ------------------------------------------------------------------------------


! query result

#if   (defined(OSD_CMP_IFORT))
! Intel Fortran compiler (DOS)
      inquire(directory=dir,exist=lexist,iostat=ios)         ! OSD_CMP_IFORT

#elif (defined(OSD_CMP_CVF))
! Compaq Visual Fortran (DOS)
      ! inquire(file=dir,exist=lexist)                       ! OSD_CMP_CVF
      lexist=iosdirexists(dir)
      ios=0

#elif (defined(OSD_CMP_LF90))
! Lahey 90 (DOS)
      lexist=iosdirexists(dir)                               ! OSD_CMP_LF90
      ios=0

#elif (defined(OSD_CMP_PGF))
! Portland Group Fortran (LINUX)
      inquire(file=dir,exist=lexist,iostat=ios)              ! OSD_CMP_PGF

#elif (defined(OSD_CMP_IFC))
! Intel Fortran (LINUX)
      inquire(file=dir,exist=lexist,iostat=ios)              ! OSD_CMP_IFC

#elif (defined(OSD_CMP_GNU))
! GNU (...)
      inquire(file=dir,exist=lexist,iostat=ios)              ! OSD_CMP_GNU

#elif (defined(OSD_CMP_SUN))
! SUN compiler
      inquire(file=dir,exist=lexist,iostat=ios)              ! OSD_CMP_SUN

#else
! the next line is included to let the compiler crash when no symbol matched
      ERROR, unknown compiler
#endif


! assign function value
      if (ios.ne.0) lexist=.false.
      osd_direxists = lexist


! end of program
      return
      end
      function osd_filename(file)

! description:
! ------------------------------------------------------------------------------
! translate a file name from one operating system to another
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      character osd_filename*(*) ! return value: translated name of 'file'


! arguments
      character file*(*)         ! (I) file name to be translated


! local variables
      integer   lfile


! functions
      integer   cfn_length


! include files


! program section
! ------------------------------------------------------------------------------


! get file length
      lfile = cfn_length(file)


! copy file name to function value
      osd_filename=file(1:lfile)


! call subroutine to do the job
      call osd_s_filename(osd_filename)


! end of program
      return
      end

! ******************************************************************************

      subroutine osd_s_filename(file)

! description:
! ------------------------------------------------------------------------------
! translate a file name from one operating system to another
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      character file*(*)   ! (I/O) file name to be translated


! local variables
      integer   i,lfile,os

      character del*1,curdel*1

      character osdel*4


! functions
      integer   osd_get_os,&
                cfn_length


! include files


! program section
! ------------------------------------------------------------------------------

! init
!     OS dependent delimiter: VMS, Unix, DOS, Linux
      osdel=']/'//char(92)//'/'


! get current OS
      os=osd_get_os()


! get file length and remove eventually trailing ^M (char(13))
      lfile = cfn_length(file)
      if (file(lfile:lfile).eq.char(13)) then
         file(lfile:lfile)=' '
         lfile=lfile-1
      endif


! determine character to be used
!      if (os.eq.1) then
!c VMS
!         curdel=']'
!      else if (os.eq.2 .or. os.eq.4) then
!c Unix
!         curdel='/'
!      else if (os.eq.3) then
!c DOS
!         curdel=char(92)    ! char(92)=\
!      endif
      curdel=osdel(os:os)


! translate from DOS to current-OS
      del=char(92)   ! character to be replaced if file name is in DOS format
      if (del.ne.curdel) then
         i=index(file(1:lfile),del)
         do while(i.gt.0)
            file(i:i)=curdel
            i=index(file(1:lfile),del)
         enddo
      endif


! translate from Unix/Linux to current-OS
      del='/'        ! character to be replaced if file name is in Unix/Linux format
      if (del.ne.curdel) then
         i=index(file(1:lfile),del)
         do while(i.gt.0)
            file(i:i)=curdel
            i=index(file(1:lfile),del)
         enddo
      endif


! end of program
      return
      end
!
#include "utl.h"

      subroutine osd_fseek(lun,offset,whence)

! description:
! ------------------------------------------------------------------------------
! routine to put a file pointer at a given position
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer,   intent(in) :: lun      ! logical unit number
      integer,   intent(in) :: offset   ! The number of bytes away from
                                        ! whence to place the pointer
      integer,   intent(in) :: whence   ! 0: SEEK_SET offset from the
                                        !             beginning of file
                                        ! 1: SEEK_CUR offset from the current
                                        !             position of the file pointer
                                        ! 2: SEEK_END Offset from the end of the file

! local variables
#if (defined(OSD_CMP_GNU))
      integer  pos,lwhence
#endif


! functions
#if (defined(OSD_CMP_GNU))
      integer   ftell
#endif


! include files


! program section
! ------------------------------------------------------------------------------


#if   (defined(OSD_CMP_IFORT))
! Intel Fortran compiler (DOS and LINUX)
      call fseek(lun,offset,whence)                                      ! OSD_CMP_IFORT

#elif (defined(OSD_CMP_CVF))
! Compaq Visual Fortran (DOS)
      call fseek(lun,offset,whence)                                      ! OSD_CMP_CVF

#elif (defined(OSD_CMP_LF90))
! Lahey 90 (DOS)
      ! IFileSeek is a routine from the Winteracter library
      !call IFileSeek(lun,offset,whence)                                  ! OSD_CMP_LF90
      ERROR, no functionality for this compiler
#elif (defined(OSD_CMP_PGF))
! Portland Group Fortran (LINUX)
      call fseek(lun,offset,whence)                                      ! OSD_CMP_PGF

#elif (defined(OSD_CMP_IFC))
! Intel Fortran (LINUX)
      call fseek(lun,offset,whence)                                      ! OSD_CMP_IFC

#elif (defined(OSD_CMP_GNU))
! GNU (...)

      ! for GNU the fseek seems not to work
      ! create a workaround here
      select case( whence )
         case( 0 )
            pos=offset
            lwhence=0
            rewind(lun)

         case( 1 )
            pos=ftell(lun)
            pos=pos+offset
            lwhence=0
            rewind(lun)

         case( 2 )
            pos=offset
            lwhence=whence

         case default
            pos=offset
            lwhence=whence

      end select

      call fseek(lun,pos,lwhence)                                        ! OSD_CMP_GNU

#elif (defined(OSD_CMP_SUN))
! SUN compiler
      call fseek(lun,offset,whence)                                      ! OSD_CMP_SUN

#else
! the next line is included to let the compiler crash when no symbol matched
      ERROR, unknown compiler
#endif


! end of program
      return
      end

!
#include "utl.h"

      function osd_fsplit(file,dir,name)

! description:
! ------------------------------------------------------------------------------
! divide a file-identification in directory and name
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer osd_fsplit        ! return value: 0: OK
                                !              -1: error


! arguments
      character file*(*),&      ! (I) complete file name
                dir*(*),&       ! (O) directory part
                name*(*)        ! (O) file name part


! local variables
      integer   indx,ios


! functions
      integer   osd_rindex


! include files


! program section
! ------------------------------------------------------------------------------

! init
      ios=0


#if   (defined(OSD_OS_VMS))
! VMS
      indx=osd_rindex(file,']')

      if (indx.gt.0) then
         dir =file(1:indx)
         name=file(indx+1:)
      else
         dir ='[]'
         name=file(1:)
      endif

#elif (defined(OSD_OS_UNIX) || defined(OSD_OS_LINUX))
! Unix/Linux

      indx=osd_rindex(file,'/')

      if (indx.gt.0) then
         dir =file(1:indx)
         name=file(indx+1:)
      else
         dir ='./'
         name=file(1:)
      endif

#elif (defined(OSD_OS_DOS))
! DOS

      indx=osd_rindex(file,char(92))  ! char(92)=\

      if (indx.gt.0) then
         dir =file(1:indx)
         name=file(indx+1:)
      else
         dir ='.'//char(92)
         name=file(1:)
      endif

#else
! the next line is included to let the compiler crash when no symbol matched
      ERROR, unknown compiler
#endif


! assign function value
      osd_fsplit=ios


! end of program
      return
      end

!
#include "utl.h"

      subroutine osd_fstat(lun,ifstat)

! description:
! ------------------------------------------------------------------------------
! get file information
!    ifstat( 1): Device on which the file resides
!    ifstat( 2): I-node number of the file
!    ifstat( 3): Protection mode of the file
!    ifstat( 4): Number of hard links to the file
!    ifstat( 5): User  identification of the file's owner
!    ifstat( 6): Group identification of the file's owner
!    ifstat( 7): Device type of the file if it is a device
!    ifstat( 8): Total size of the file in bytes
!    ifstat( 9): Time the file was last accessed
!    ifstat(10): Time the file was last modified
!    ifstat(11): Time the file's status was last changed
!                          /* Times measured in seconds since */
!                          /* 00:00:00 UTC, Jan. 1, 1970 */
!    ifstat(12): Optimal block size for file system operations
!    ifstat(13): actual number of blocks allocated (only Sun??)

! declaration section
! ------------------------------------------------------------------------------

#if   (defined(OSD_CMP_CVF))
      use dfport
#endif

      implicit none

! arguments
      integer   lun,&         ! (I) unit number
                ifstat(13)    ! (O) file information


! local variables
      integer   i

#if   (defined(OSD_CMP_PGF))
      integer   lfs(26)
#endif

!      integer   otyp   ! layout of lfs: 1:sun, 2:beowulf-pgf

#if   (defined(OSD_CMP_PGF) || defined(OSD_CMP_CVF) || defined(OSD_CMP_IFORT) || defined(OSD_CMP_GNU))
      integer ios
#endif


! functions
#if   (defined(OSD_CMP_PGF) || defined(OSD_CMP_CVF) || defined(OSD_CMP_IFORT) || defined(OSD_CMP_GNU))
      integer fstat
#endif


! include files


! program section
! ------------------------------------------------------------------------------

! init output
      do i=1,13
         ifstat(i)=-1
      enddo

!      otyp=1

#if   (defined(OSD_CMP_SUN))
      call fstat(lun,ifstat)
#elif (defined(OSD_CMP_GNU))
      ios=fstat(lun,ifstat)
#elif (defined(OSD_CMP_PGF))

! order in beowulf (see: man fstat)
!            struct stat {
!                dev_t         st_dev;      /* device */
!                ino_t         st_ino;      /* inode */
!                mode_t        st_mode;     /* protection */
!                nlink_t       st_nlink;    /* number of hard links */
!                uid_t         st_uid;      /* user ID of owner */
!                gid_t         st_gid;      /* group ID of owner */
!                dev_t         st_rdev;     /* device type (if inode device) */
!                off_t         st_size;     /* total size, in bytes */
!                unsigned long st_blksize;  /* block size for file system I/O */
!                unsigned long st_blocks;   /* number of blocks allocated */
!                time_t        st_atime;    /* time of last access */
!                time_t        st_mtime;    /* time of last modification */
!                time_t        st_ctime;    /* time of last change */
!            };
!      ios=fstat(lun,lfs)   ! not sure of this routine!!!!
!      otyp=2
#if   OSD_CMP_VERSION == 9
      ios=fstat(lun,ifstat) ! 20091012
#elif OSD_CMP_VERSION == 4
      ios=fstat(lun,lfs)
      ifstat( 1)=lfs(1)
      ifstat( 2)=lfs(4)
      ifstat( 3)=lfs(5)
      ifstat( 4)=lfs(6)
      ifstat( 5)=lfs(7)
      ifstat( 6)=lfs(8)
      ifstat( 7)=0
      ifstat( 8)=lfs(12)
      ifstat( 9)=lfs(15)
      ifstat(10)=lfs(17)
      ifstat(11)=lfs(19)
      ifstat(12)=lfs(13)
      ifstat(13)=lfs(14)
!      write(*,*) ' lfs: ',lfs
#else
      ERROR, unknown Portland Group Compiler Version
#endif

#elif (defined(OSD_CMP_IFC))
      call fstat(lun,ifstat)   ! not suer of this routine!!!!

! order for VisualFortran, idem sun, except from ifstat(13) does not exist
!      statb(1)      Device the file resides on, W*32, W*64: Always 0
!      statb(2)      File inode number         , W*32, W*64: Always 0
!      statb(3)      Access mode of the file   , See the table in Results
!      statb(4)      Number of hard links to the file, W*32, W*64: Always 1
!      statb(5)      User ID of owner          , W*32, W*64: Always 1
!      statb(6)      Group ID of owner         , W*32, W*64: Always 1
!      statb(7)      Raw device the file resides on , W*32, W*64: Always 0
!      statb(8)      Size of the file          ,
!      statb(9)      Time when the file was last accessed1,
!                    W*32, W*64: Only available on non-FAT file systems; undefined on FAT systems
!      statb(10)      Time when the file was last modified1 ,
!      statb(11)      Time of last file status change1, W*32, W*64: Same as stat(10)
!      statb(12)      Block size for file system I/O operations , W*32, W*64: Always 1
#elif (defined(OSD_CMP_CVF) || defined(OSD_CMP_IFORT))
      ios=fstat(lun,ifstat)
#elif (defined(OSD_CMP_LF90))
      inquire(unit=lun,flen=ifstat(8))  ! file size
#elif (defined(OSD_OS_VMS))
      call fstat(lun,ifstat)   ! not sure of this routine!!!!
#else
      ERROR: compiler unknown!
#endif

!      if (otyp.eq.2) then
!c beowulf structure
!         ifstat( 1)=0
!         ifstat( 2)=lfs(4)
!         ifstat( 3)=lfs(5)
!         ifstat( 4)=lfs(6)
!         ifstat( 5)=lfs(7)
!         ifstat( 6)=lfs(8)
!         ifstat( 7)=0
!         ifstat( 8)=lfs(12)
!         ifstat( 9)=lfs(15)
!         ifstat(10)=lfs(17)
!         ifstat(11)=lfs(19)
!         ifstat(12)=lfs(13)
!         ifstat(13)=lfs(14)
!      endif

! end of program
      return
      end

!
#include "utl.h"

      subroutine osd_ftell(lun,offset)

! description:
! ------------------------------------------------------------------------------
! Return the current position of a file
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer,   intent(in)  :: lun      ! logical unit number
      integer,   intent(out) :: offset   ! position off file pointer in the
                                         ! file (bytes)


! local variables


! functions
#if ! defined(OSD_CMP_LF90)
      integer   ftell
#endif


! include files


! program section
! ------------------------------------------------------------------------------


#if   (defined(OSD_CMP_IFORT))
! Intel Fortran compiler (DOS and LINUX)
      offset=ftell(lun)                                                  ! OSD_CMP_IFORT

#elif (defined(OSD_CMP_CVF))
! Compaq Visual Fortran (DOS)
      offset=ftell(lun)...                                               ! OSD_CMP_CVF

#elif (defined(OSD_CMP_LF90))
! Lahey 90 (DOS)
      ! IFileSeek is a routine from the Winteracter library
      !offset=0
      !call IFileSeek(lun,offset,1)                                       ! OSD_CMP_LF90
      ERROR, no functionality for this compiler
#elif (defined(OSD_CMP_PGF))
! Portland Group Fortran (LINUX)
      offset=ftell(lun)                                                  ! OSD_CMP_PGF

#elif (defined(OSD_CMP_IFC))
! Intel Fortran (LINUX)
      offset=ftell(lun)                                                  ! OSD_CMP_IFC

#elif (defined(OSD_CMP_GNU))
! GNU (...)
      offset=ftell(lun)                                                  ! OSD_CMP_GNU

#elif (defined(OSD_CMP_SUN))
! SUN compiler
      offset=ftell(lun)                                                  ! OSD_CMP_SUN

#else
! the next line is included to let the compiler crash when no symbol matched
      ERROR, unknown compiler
#endif


! end of program
      return
      end

!
#include "utl.h"

      subroutine osd_getarg(n,arg)

! description:
! ------------------------------------------------------------------------------
! get the n-th argument of the command line
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   n          ! (I) argument number

      character arg*(*)    ! (O) argument string


! local variables
#if   (defined(OSD_CMP_LF90))
!    Lahey F90 compiler
      character c*1024
#endif


! functions
#if   (defined(OSD_CMP_LF90))
!    Lahey F90 compiler
      character cfn_elem*1024
#endif


! include files


! program section
! ------------------------------------------------------------------------------


#if   (defined(OSD_CMP_LF90))
! Lahey F90 compiler
      call getcl(c)
      arg=cfn_elem(n,' ',1,c)
#else
! unix...
      call getarg(n,arg)
!:sel:gnu:      call getarg(n,arg)
!:sel:ifc:      call getarg(n,arg)
!:sel:pgf:      call getarg(n,arg)
!:sel:vif:      call getarg(n,arg)    !???
#endif


! end of program
      return
      end

!
#include "utl.h"

      subroutine osd_getcwd(dirname)

! description:
! ------------------------------------------------------------------------------
! get current working directory name
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      character dirname*(*)          ! (O) name of current directory


! local variables


! functions


! include files


! program section
! ------------------------------------------------------------------------------


#if   (defined(OSD_CMP_LF90))
      call iosdirname(dirname)
#else
      call getcwd(dirname)
#endif


! end of program
      return
      end

!
#include "utl.h"

      function osd_get_os()

! description:
! ------------------------------------------------------------------------------
! query architecture of the machine
! output: DOS, LINUX, SUN4SOL2

! declaration section
! ------------------------------------------------------------------------------

      implicit none

! function
      integer   osd_get_os   ! return value: 2 Unix
                             !               3 DOS
                             !               4 Linux

! arguments


! local variables


! functions


! include files


! program section
! ------------------------------------------------------------------------------



!c:sel:vms:      osd_get_os=1 ! VMS
!c:sel:gnu:      osd_get_os=2 ! Unix/Linux
!c:sel:sun:      osd_get_os=2 ! Unix/Linux
!c:sel:pgf:      osd_get_os=2 ! Unix/Linux
!c:sel:ifc:      osd_get_os=2 ! Unix/Linux
!c:sel:lf90:      osd_get_os=3 ! DOS
!c:sel:vif:      osd_get_os=3 ! DOS

#if   (defined(OSD_OS_UNIX))

      osd_get_os=2                  ! UNIX

#elif (defined(OSD_OS_DOS))

      osd_get_os=3                  ! DOS

#elif (defined(OSD_OS_LINUX))

      osd_get_os=4                  ! LINUX

#else
      ERROR, can not compile routine OSD_GTOS, compiler directives unknown
      osd_get_os=-1
      write(*,*) ' ERROR in compilation of routine OSD_GTOS.'
      call exit(1)
#endif

      return
      end

!
#include "utl.h"

      function osd_iargc()

! description:
! ------------------------------------------------------------------------------
! query number of command line arguments
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer    osd_iargc ! return value: number of command line arguments


! arguments


! local variables
#if   (defined(OSD_CMP_LF90))
!    Lahey F90 compiler
      character c*1024               ! Lahey F90 compiler
#endif


! functions
#if   (defined(OSD_CMP_SUN))
      integer   iargc                 ! sun
#elif (defined(OSD_CMP_GNU))
      integer   iargc                 ! gnu
#elif (defined(OSD_CMP_PGF))
      integer   iargc                 ! Portland Group
#elif (defined(OSD_CMP_IFC))
      integer   iargc                 ! Intel compiler
#elif (defined(OSD_CMP_CVF))
      integer   iargc                 ! Visual Fortran?
#elif (defined(OSD_CMP_IFORT))
      integer   iargc                 ! Intel Fortran?
#elif (defined(OSD_CMP_LF90))
      integer   cfn_n_elem            ! Lahey F90 compiler
#else
      ERROR, compiler unknown!
#endif



! include files


! program section
! ------------------------------------------------------------------------------

#if   (defined(OSD_CMP_SUN))
      osd_iargc=iargc()               ! sun
#elif (defined(OSD_CMP_GNU))
      osd_iargc=iargc()               ! gnu
#elif (defined(OSD_CMP_PGF))
      osd_iargc=iargc()               ! Portland Group
#elif (defined(OSD_CMP_IFC))
      osd_iargc=iargc()               ! Intel compiler
#elif (defined(OSD_CMP_CVF))
      osd_iargc=iargc()               ! Visual Fortran?
#elif (defined(OSD_CMP_IFORT))
      osd_iargc=iargc()               ! Intel Fortran?
#elif (defined(OSD_CMP_LF90))
      call getcl(c)                   ! Lahey F90 compiler
      osd_iargc=cfn_n_elem(' ',1,c)
#else
      ERROR, compiler unknown!
#endif



! end of program
      return
      end
!
#include "utl.h"

      function osd_ios(status)

! description:
! ------------------------------------------------------------------------------
! translate a standard I/O status into an OS dependent code
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer osd_ios           ! return value: : translated code
                                !          -9999: status not found


! arguments
      character status*(*)      ! (I) standard code to be translated code


! local variables
      integer   nstat
      parameter (nstat=2)

      integer   codes(nstat)
      character allstat(nstat)*4

      character upcst*4         ! uppercase status

      integer   l,idx

      logical   notfound

      integer   ierr
      parameter (ierr=-9999)    ! return code in case not found


! functions
      integer   cfn_length


! include files


! data block
      data allstat  /'OK  ','EOF '/
#if   (defined(OSD_CMP_LF90))
      data codes    / 0    , -1   /
#elif (defined(OSD_CMP_IFORT))
      data codes    / 0    , -1   /                          ! OSD_CMP_IFORT
#elif (defined(OSD_CMP_CVF))
      data codes    / 0    , -1   /                          ! OSD_CMP_CVF
#elif (defined(OSD_CMP_PGF))
      data codes    / 0    , -1   /                          ! OSD_CMP_PGF
#elif (defined(OSD_CMP_IFC))
      data codes    / 0    , -1   /                          ! OSD_CMP_IFC
#elif (defined(OSD_CMP_GNU))
      data codes    / 0    , -1   /                          ! OSD_CMP_GNU
#elif (defined(OSD_CMP_SUN))
      data codes    / 0    , -1   /                          ! OSD_CMP_SUN
#elif (defined(OSD_CMP_LF90))
      data codes    / 0    , -1   /                          ! OSD_CMP_LF90
#else
! the next line is included to let the compiler crash when no symbol matched
      ERROR, unknown compiler
#endif


! program section
! ------------------------------------------------------------------------------

! take over the status and make it uppercase
      upcst=status
      call cfn_s_upcase(upcst)
      l=cfn_length(upcst)
      upcst=upcst(1:l)//'    '


! search
      idx=0
      notfound=.true.
      do while(idx.lt.nstat .and. notfound)
         idx=idx+1
         if (upcst.eq.allstat(idx)) notfound=.false.
      enddo


! assign function value
      if (notfound) then
         ! not found
         osd_ios=ierr
      else
         ! take over found value
         osd_ios=codes(idx)
      endif


! end of program
      return
      end

!
#include "utl.h"

      subroutine osd_mkdir(dir,ios)

! description:
! ------------------------------------------------------------------------------
! make a new directory which contains a couple of subdirectories
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   ios       ! (O) I/O status

      character dir*(*)   ! (I) directory


! local variables
      integer   l

      character comm*1024,ldir*1024

      logical   exist


! functions
      integer   cfn_length

      character osd_filename*1024

      logical   osd_direxists


! include files


! program section
! ------------------------------------------------------------------------------

! init
      ios  = 0
      ldir = dir


!cc      write(*,*) ' osd_mkdir os ',os

! if necessary convert to os type
      ldir=osd_filename(ldir)
      l=cfn_length(ldir)


! check if the directory exists
      exist=osd_direxists(ldir)


! make a new dir
      if (.not.exist) then
!cc      write(*,*) ' osd_mkdir 1'


#if   (defined(OSD_OS_VMS))
! VMS
         comm='create /directory '//ldir(1:l)
#elif (defined(OSD_OS_UNIX) || defined(OSD_OS_LINUX))
! Unix/Linux
         comm='mkdir -p '//ldir(1:l)
#elif (defined(OSD_OS_DOS))
! DOS
         comm='mkdir '//'"'//ldir(1:l)//'"'
#else
! the next line is included to let the compiler crash when no symbol matched
      ERROR, unknown compiler
#endif


         if (ios.eq.0) then

            l=cfn_length(comm)
            call system(comm(1:l))
!cc      write(*,*) ' osd_mkdir 3: ',ios

! test if the directory exists now
            exist=osd_direxists(ldir)
            if (.not.exist) then
               ios=-2
            endif
!cc      write(*,*) ' osd_mkdir 4: ',ios

         endif
!cc      write(*,*) ' osd_mkdir 5: ',ios

      endif

!cc      write(*,*) ' osd_mkdir 6: ',ios

! end of program
      return
      end

!
#include "utl.h"

      function osd_open2(lun,arecl,file,opts)

! description:
! ------------------------------------------------------------------------------
! open a file with OS and/or compiler dependent options
! available values for opts:
!    attribute        values                      default     compilers
!    =========        =========================   =======     =============
!    status         : NEW,OLD,UNKNOWN,REPLACE     UNKNOWN     all
!    form           : FORMATTED,UNFORMATTED       FORMATTED   all
!    access         : SEQUENTIAL,APPEND,DIRECT    SEQUENTIAL  all
!    carriagecontrol: FORTRAN,NONE,LIST           1)          vif,ifc
!    READONLY                                     -           vms,lf90,vif,ifc
!    SHARED                                       -           vms,lf90
!
! 1) default for carriage control: UNFORMATTED file: NONE
!                                 FORMATTED file  : LIST
! compilers:
!    sun     : sun conpiler
!    vms     : VMS compiler
!    lf90    : Lahey Fortran 90
!    vif     : Visual Fortran
!    gnu     : gnu compiler
!    ifc     : Intel

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   osd_open2   ! return value: I/O-status


! arguments
      integer   lun,&       ! (I) logical unit number
                arecl       ! (I) length of direct access record in BYTES

      character file*(*),&  ! (I) file name
                opts*(*)    ! (I) options
                            !     all options are defined in one string
                            !     separated by a space ' ' or comma ','

! local variables
      integer   l,ls,lf,la,lc,lp,recl,ios

      character upopts*256,&
                option*16,status*16,form*16,access*16,position*16,&
                carriagecontrol*16,&
                del*1

      logical   readonly,shared,replace,&
                exist


! functions
      integer   cfn_length


! include files


! program section
! ------------------------------------------------------------------------------

! On behalf of Alterra an extra tran:
!:tran:alterra:vif:

!:sel:sun:      call cfn_rtt_strt('osdopen2')
!:sel:gnu:      call cfn_rtt_strt('osdopen2')
!:sel:pgf:      call cfn_rtt_strt('osdopen2')
!:sel:ifc:      call cfn_rtt_strt('osdopen2')
!:sel:lf90:      call cfn_rtt_strt('osdopen2')

!      write(*,'(2a)') ' ***** osd_open2 ',file(1:cfn_length(file))

! default values
      status          = 'UNKNOWN'
      form            = 'FORMATTED'
      access          = 'SEQUENTIAL'
      position        = 'ASIS'       ! default value for PGF compiler
      replace         = .false.
      carriagecontrol = ' '
      readonly        = .false.
      shared          = .false.


! find options
      upopts=opts
      call cfn_s_upcase(upopts)

      do while (cfn_length(upopts).gt.0)

         call cfn_par_ext(option,upopts,' ,',2,del)
         l=cfn_length(option)

         if ( &
            option(1:l).eq.'NEW'        .or. &
            option(1:l).eq.'OLD'        .or. &
            option(1:l).eq.'UNKNOWN' &
            ) then
            status=option                        ! *** STATUS ***
         else if ( &
            option(1:l).eq.'REPLACE' &
            ) then
            replace=.true.                       ! *** REPLACE ***
         else if ( &
            option(1:l).eq.'FORMATTED'  .or. &
            option(1:l).eq.'UNFORMATTED' &
            ) then
            form=option                          ! *** FORM ***
         else if ( &
            option(1:l).eq.'SEQUENTIAL' .or. &
            option(1:l).eq.'APPEND'     .or. &
            option(1:l).eq.'DIRECT' &
            ) then
            access=option                        ! *** ACCESS ***
         else if ( &
            option(1:l).eq.'FORTRAN'    .or. &
            option(1:l).eq.'NONE'       .or. &
            option(1:l).eq.'LIST' &
            ) then
            carriagecontrol=option               ! *** CARRIAGECONTROL ***
         else if ( &
            option(1:l).eq.'READONLY' &
            ) then
            ! Unix (Sun): NO
            ! VMS       : YES
            ! Lahey     : YES (action='READ')
            readonly=.true.                      ! *** READONLY ***
         else if ( &
            option(1:l).eq.'SHARED' &
            ) then
            ! Unix (Sun): NO
            ! VMS       : YES
            ! Lahey     : 'YES' (action='READ,DENYWRITE')
            shared=.true.                        ! *** SHARED ***
         else
            ! option not found. take no action
!            write(*,'(3a)') ' WARNING, osd_open2 option ',option(1:l),
!     1                      ' not known!'
         endif

      enddo


! check for replace option
!    for compilers which do not know the REPLACE status the file will be deleted
!    first if it exists
      if (replace) then

#if   (defined(OSD_CMP_CVF) || defined(OSD_CMP_IFC) || defined(OSD_CMP_LF90))

      status='REPLACE'

#else

      inquire(file=file,exist=exist)
      if (exist) then
         open(unit=lun,file=file,status='UNKNOWN')
         close(lun,status='DELETE')
      endif

#endif

      endif


      l =cfn_length(file)
      ls=cfn_length(status)
      lf=cfn_length(form)
      la=cfn_length(access)
      lp=cfn_length(position)
      lc=cfn_length(carriagecontrol)


! check for APPEND in case of PGF compiler
#if   (defined(OSD_CMP_PGF))
!      if (access(1:la).eq.'APPEND') then
!         access  ='SEQUENTIAL'
!         position='APPEND'
!         la=cfn_length(access)
!         lp=cfn_length(position)
!      endif
#endif


!    default value for carriage control
      if (lc.eq.0) then
         if (form(1:lf).eq.'UNFORMATTED') then
            carriagecontrol='NONE'
         else
            carriagecontrol='LIST'
         endif
         lc=cfn_length(carriagecontrol)
      endif


! select right open statement
      if (access(1:la).eq.'DIRECT') then
!    use the record length
         recl=arecl

!   some compilers define the record length in longwords instead of bytes
!   for UNFORMATTED files??
#if   (defined(OSD_CMP_IFORT) || defined(OSD_CMP_CVF))
         if (form(1:lf).eq.'UNFORMATTED') then
            recl=recl/4
         endif
#endif


         if (readonly) then
            if (shared) then

               ! READONLY,SHARED,RECL

               open(unit=lun,file=file(1:l),iostat=ios,&
                   status=status(1:ls),&
                   form=form(1:lf),&
                   access=access(1:la),&
                   recl=recl &
#if   (defined(OSD_CMP_IFORT) || defined(OSD_CMP_CVF))
                   ,carriagecontrol=carriagecontrol(1:lc) &
                   ,READONLY,SHARED &
#elif (defined(OSD_CMP_PGF))
                   ,READONLY &
!     1             ,position=position(1:lp)
#elif (defined(OSD_CMP_IFC))
                   ,action='READ',SHARED &
#elif (defined(OSD_OS_VMS))
                   ,READONLY,SHARED &
#elif (defined(OSD_CMP_LF90))
                   ,action='READ,DENYWRITE' &
#endif
                   )

            else

               ! READONLY,RECL

               open(unit=lun,file=file(1:l),iostat=ios,&
                   status=status(1:ls),&
                   form=form(1:lf),&
                   access=access(1:la),&
                   recl=recl &
#if   (defined(OSD_CMP_IFORT) || defined(OSD_CMP_CVF))
                   ,carriagecontrol=carriagecontrol(1:lc) &
                   ,READONLY &
#elif (defined(OSD_CMP_PGF))
                   ,READONLY &
!     1             ,position=position(1:lp)
#elif (defined(OSD_CMP_IFC))
                   ,action='READ' &
#elif (defined(OSD_OS_VMS))
                   ,READONLY &
#elif (defined(OSD_CMP_LF90))
                   ,action='READ' &
#endif
                   )

            endif
         else
            if (shared) then

               ! SHARED,RECL

               open(unit=lun,file=file(1:l),iostat=ios,&
                   status=status(1:ls),&
                   form=form(1:lf),&
                   access=access(1:la),&
                   recl=recl &
#if   (defined(OSD_CMP_IFORT) || defined(OSD_CMP_CVF))
                   ,carriagecontrol=carriagecontrol(1:lc) &
                   ,SHARED &
#elif (defined(OSD_CMP_PGF))
!     1             ,position=position(1:lp)
#elif (defined(OSD_CMP_IFC))
                   ,SHARED &
#elif (defined(OSD_OS_VMS))
                   ,SHARED &
#elif (defined(OSD_CMP_LF90))
                   ,action='READ,DENYWRITE' &
#endif
                   )

            else

               ! RECL

               open(unit=lun,file=file(1:l),iostat=ios,&
                   status=status(1:ls),&
                   form=form(1:lf),&
                   access=access(1:la),&
                   recl=recl &
#if   (defined(OSD_CMP_IFORT) || defined(OSD_CMP_CVF))
                   ,carriagecontrol=carriagecontrol(1:lc) &
#elif (defined(OSD_CMP_PGF))
!     1             ,position=position(1:lp)
#elif (defined(OSD_CMP_IFC))
#elif (defined(OSD_OS_VMS))
#elif (defined(OSD_CMP_LF90))
#endif
                   )

            endif
         endif
      else

!    NO use of record length

         if (readonly) then
            if (shared) then

               ! READONLY,SHARED

               open(unit=lun,file=file(1:l),iostat=ios,&
                   status=status(1:ls),&
                   form=form(1:lf),&
                   access=access(1:la) &
#if   (defined(OSD_CMP_IFORT) || defined(OSD_CMP_CVF))
                   ,carriagecontrol=carriagecontrol(1:lc) &
                   ,READONLY,SHARED &
#elif (defined(OSD_CMP_PGF))
                   ,READONLY &
!     1             ,position=position(1:lp)
#elif (defined(OSD_CMP_IFC))
                   ,action='READ',SHARED &
#elif (defined(OSD_OS_VMS))
                   ,READONLY,SHARED &
#elif (defined(OSD_CMP_LF90))
                   ,action='READ,DENYWRITE' &
#endif
                   )

            else

               ! READONLY

               open(unit=lun,file=file(1:l),iostat=ios,&
                   status=status(1:ls),&
                   form=form(1:lf),&
                   access=access(1:la) &
#if   (defined(OSD_CMP_IFORT) || defined(OSD_CMP_CVF))
                   ,carriagecontrol=carriagecontrol(1:lc) &
                   ,READONLY &
#elif (defined(OSD_CMP_PGF))
                   ,READONLY &
!     1             ,position=position(1:lp)
#elif (defined(OSD_CMP_IFC))
                   ,action='READ' &
#elif (defined(OSD_OS_VMS))
                   ,READONLY &
#elif (defined(OSD_CMP_LF90))
                   ,action='READ' &
#endif
                   )

            endif
         else
            if (shared) then

               ! SHARED

               open(unit=lun,file=file(1:l),iostat=ios,&
                   status=status(1:ls),&
                   form=form(1:lf),&
                   access=access(1:la) &
#if   (defined(OSD_CMP_IFORT) || defined(OSD_CMP_CVF))
                   ,carriagecontrol=carriagecontrol(1:lc) &
                   ,SHARED &
#elif (defined(OSD_CMP_PGF))
!     1             ,position=position(1:lp)
#elif (defined(OSD_CMP_IFC))
                   ,SHARED &
#elif (defined(OSD_OS_VMS))
                   ,SHARED &
#elif (defined(OSD_CMP_LF90))
                   ,action='READ,DENYWRITE' &
#endif
                   )

            else

               !

               open(unit=lun,file=file(1:l),iostat=ios,&
                   status=status(1:ls),&
                   form=form(1:lf),&
                   access=access(1:la) &
#if   (defined(OSD_CMP_IFORT) || defined(OSD_CMP_CVF))
                   ,carriagecontrol=carriagecontrol(1:lc) &
#elif (defined(OSD_CMP_PGF))
!     1             ,position=position(1:lp)
#elif (defined(OSD_CMP_IFC))
#elif (defined(OSD_OS_VMS))
#elif (defined(OSD_CMP_LF90))
#endif
                   )

            endif
         endif
      endif



! assign function value
      osd_open2=ios

!:sel:sun:      call cfn_rtt_end('osdopen2')
!:sel:gnu:      call cfn_rtt_end('osdopen2')
!:sel:pgf:      call cfn_rtt_end('osdopen2')
!:sel:ifc:      call cfn_rtt_end('osdopen2')
!:sel:lf90:      call cfn_rtt_end('osdopen2')


! end of program
      return
      end
!
#include "utl.h"

      subroutine osd_rename(from,to,ios)

! description:
! ------------------------------------------------------------------------------
! rename a file
!

! declaration section
! ------------------------------------------------------------------------------
#if   (defined(OSD_CMP_IFORT))
      use ifport
#endif
#if   (defined(OSD_CMP_IFC))
      use ifport
#endif


      implicit none


! arguments
      character (len=*), intent(in)   :: from        ! old file name
      character (len=*), intent(in)   :: to          ! new file name

      integer          , intent(out)  :: ios         ! I/O status
                                                     !  0: OK
                                                     ! -1: ERROR

! local variables

! functions
#if (defined(OSD_CMP_PGF))
      integer   rename
#endif

! program section
! ------------------------------------------------------------------------------

! init
      ios=-1


#if   (defined(OSD_CMP_IFORT))
! Intel Fortran compiler (DOS and LINUX)
      ios=rename(from,to)                                               ! OSD_CMP_IFORT

#elif (defined(OSD_CMP_CVF))
! Compaq Visual Fortran (DOS)
      ERROR, not implemented yet                                                           ! OSD_CMP_CVF

#elif (defined(OSD_CMP_LF90))
! Lahey 90 (DOS)
      ERROR, not implemented yet                                                           ! OSD_CMP_LF90

#elif (defined(OSD_CMP_PGF))
! Portland Group Fortran (LINUX)
      ios=rename(from,to)                                               ! OSD_CMP_PGF

#elif (defined(OSD_CMP_IFC))
! Intel Fortran (LINUX)
      ios=rename(from,to)                                               ! OSD_CMP_IFC

#elif (defined(OSD_CMP_GNU))
! GNU (...)
      ios=rename(from,to)                                               ! OSD_CMP_GNU

#elif (defined(OSD_CMP_SUN))
! SUN compiler
      ios=rename(from,to)                                               ! OSD_CMP_SUN

#else
! the next line is included to let the compiler crash when no symbol matched
      ERROR, unknown compiler
#endif



! end of program
      return
      end
!
#include "utl.h"

      function osd_rindex(string,substring)

! description:
! ------------------------------------------------------------------------------
! rindex (last occurrence of a sub string in a string)
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer     osd_rindex  ! return value:
                              !      0: sub string not found
                              !     >0: start position of 'sub string'
                              !         in 'string'


! arguments
      character string*(*),&    ! (I) string in which must be searched
                substring*(*)   ! (I) string to search in 'string'


! local variables

#if   (! defined(OSD_CMP_SUN) && ! defined(OSD_CMP_LF90))
      integer   p1,p2,pt,l    ! gnu...
#endif

! functions
#if   (defined(OSD_CMP_SUN))
      integer   rindex      ! Unix
#endif


! include files


! program section
! ------------------------------------------------------------------------------


! Unix
#if   (defined(OSD_CMP_SUN))
      osd_rindex=rindex(string,substring)

#elif (defined(OSD_CMP_LF90))
      osd_rindex=index(string,substring,.true.)

#else
! gnu...

      pt=1
      p2=0
      l =len(string)
      do while (pt.le.l)
         p1=index(string(pt:l),substring)
         if (p1.gt.0) then
            p2=pt+p1-1
            pt=p2+1
         else
            pt=l+1
         endif
      enddo
      osd_rindex=p2

#endif


! end of program
      return
      end
!
#include "utl.h"

      function osd_time()

! description:
! ------------------------------------------------------------------------------
! get system clock time in seconds
!

! declaration section
! ------------------------------------------------------------------------------

!      implicit none


! function declaration
      integer    osd_time   ! return value: system time in seconds


! arguments


! local variables
#if   (defined(OSD_CMP_LF90))
      integer   c,cr,cm
#elif (defined(OSD_CMP_IFC))
      double precision   dtime
#endif
      integer   itime

! functions
#if   (! defined(OSD_CMP_IFC) && ! defined(OSD_CMP_LF90))
      integer   time
#endif


! include files


! program section
! ------------------------------------------------------------------------------


! get time information
#if   (defined(OSD_CMP_IFC))
      call clockx(dtime)
      itime=nint(dtime/1000000.)
#elif (defined(OSD_CMP_LF90))
      call system_clock(c,cr,cm)
      itime=int(c/100)
#else
      itime=time()
#endif


! assign function value
      osd_time=itime


! end of program
      return
      end
!
! DATUM: 15/12/92
!        10/01/95 par_extract2 changed because of "subscript out of range"
!                 chf_copy function added
! A.L.		27 May 1997	v2r0	par_extract -> cfn_par_ext
!
      subroutine cfn_par_ext(par,arg,tekens,nt,del)

      implicit none

! subroutine to cut the first parameter of a row (arg) and paste it
! in a variable (par)
! if the first character is a space then it may occur in combination with
! one of the others

      integer   lp,la,nt

      character arg*(*),par*(*),del*1
      character tekens(nt)*1


      lp=len(par)
      la=len(arg)

      call cfn_par_ext2(par,lp,arg,la,tekens,nt,del)

      return
      end

!**********

      subroutine cfn_par_ext2(par,lpar,arg,larg,tekens,nt,del)

      implicit none

! subroutine to cut the first parameter of a row (arg) and paste it
! in a variable (par)
! if the first character is a space then it may occur in combination with
! one of the others

      integer   l,i1,i2,nt,begin,eind,keind

      integer     lpar,larg
      character   arg(larg)*1,par(lpar)*1,del*1
      character   tekens(nt)*1

      integer     it        ! return value for chf_copy

! functions
      logical     cfn_een_van

      integer     chf_copy  ! copy function


      call cfn_trim2s(arg,larg)
      l=larg

      call cfn_elem_be(1,tekens,nt,arg,l,begin,eind)

! if the length of "par" is smaller than "eind-begin+1" then make "keind"
! smaller too
      if ((eind-begin+1).gt.lpar) then
         keind=begin+lpar-1
      else
         keind=eind
      endif

      i1=keind-begin+1
      i2=l-eind

!c      par(1)(1:i1)=arg(1)(begin:keind)
      it=chf_copy(arg(begin),i1,par(1),lpar)
      if (i2.gt.0) then
!c         arg(1)(1:i2)=arg(1)((eind+1):l)
         it=chf_copy(arg(eind+1),l-eind,arg(1),i2)
      else
!c         arg(1)(1:l)=char(0)
         it=chf_copy(char(0),1,arg(1),l)
      endif

      del=arg(1)

! empty the last part of the variables
!c      if (i1.lt.lpar) par(1)((i1+1):lpar)=char(0)
!c      if (i2.lt.l)    arg(1)((i2+1):l)=char(0)
      if (i1.lt.lpar) it=chf_copy(char(0),1,par(i1+1),lpar-i1)
      if (i2.lt.l)    it=chf_copy(char(0),1,arg(i2+1),l-i2)

      if (tekens(1).eq.' ') then
         call cfn_trim2s(arg,larg)
      endif

      if (cfn_een_van(arg(1),tekens,nt)) then
! whenever the previous allocation of del was a space then one of the other
! delimiters can be taken. the non-spaces have the right of way over
! the space.
         del=arg(1)
!c         arg(1)(1:larg)=arg(1)(2:larg)
         it=chf_copy(arg(2),larg-1,arg(1),larg)
      endif

      return
      end

!
! DATUM: 15/12/92
!        10/01/95 trim2 changed because of "subscript out of range"
!                 trim2s added
! A.L.		14 May 1997	v2r0	functions provided with cfn_<...>
! A.L.		24 Aug 1998	v2r0	cfn_s_trim2 added
!
      function cfn_trim(arg)

      implicit none

! Function to remove previous spaces in a character variable

      character arg*(*),cfn_trim*(*)

      integer   l

      l=len(arg)

      cfn_trim=arg
      call cfn_s_trim2(cfn_trim,l)

      return
      end
!*******************************************************************************
      function cfn_trim2(arg,larg)

! A.L.		24 Aug 1998	v2r0	content replaced by cfn_s_trim2

      implicit none

! Function to remove previous spaces in a character variable

      integer   larg
      character arg(larg)*1,cfn_trim2*(*)

      integer   l,i


      cfn_trim2=' '
      l=min(len(cfn_trim2),larg)
      do i=1,l
         cfn_trim2(i:i)=arg(i)
      enddo
      call cfn_s_trim2(cfn_trim2,larg)

      return
      end
!*******************************************************************************
      subroutine cfn_trim2s(arg,larg)

! A.L.		24 Aug 1998	v2r0	content replaced by cfn_s_trim2

      implicit none

! subroutine to remove previous spaces in a character variable

      integer   larg
      character arg(larg)*1

      call cfn_s_trim2(arg,larg)

      return
      end

!*******************************************************************************

      subroutine cfn_s_trim2(arg,larg)

! description:
! ------------------------------------------------------------------------------
! subroutine with trim function
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   larg                       ! (I)   length arg
      character arg(larg)*1                ! (I/O) string to trim


! local variables
      integer   i,j


! functions


! program section
! ------------------------------------------------------------------------------



! count number of leading spaces
      i=1
!      do while(i.le.larg .and. arg(i).eq.' ')
! 20080410 replace above mentioned '.le.' by '.lt.' because of a problem in
!          intel visual fortran compiler, whenever the first part is .false. it
!          still start to evaluate the second part (after .and.) and then gives
!          an error on boundary check
!          It is no problem to skip the test of the last element, if it
!          appears to be space it may be moved to position 1.
      do while(i.lt.larg .and. arg(i).eq.' ')
         i=i+1
      enddo

! shifting the string
      do j=1,larg-i+1
         arg(j)=arg(j+i-1)
      enddo

! fill up with spaces
      do j=larg-i+2,larg
         arg(j)=' '
      enddo


! end of program
      return
      end


!*******************************************************************************

      subroutine cfn_s_trim(arg)

! description:
! ------------------------------------------------------------------------------
! subroutine with trim function
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      character arg*(*)               ! (I/O) string to trim


! local variables
      integer   larg


! functions


! program section
! ------------------------------------------------------------------------------

      larg=len(arg)

      call cfn_s_trim2(arg,larg)


! end of program
      return
      end
!
! DATUM: 15/12/92
!        10/01/95 upcase2 changed because of "subscript out of range"
! A.L.		14 May 1997	v2r0	function provided with cfn_<...>
! A.L.		25 Aug 1998	v2r0	cfn_s_upcase2 added
!
      function cfn_upcase(arg)

! A.L.			25 Aug 1998	v2r0	cfn_s_upcase2 added

      implicit none

! Function to convert all small letters in a character variable in
! capital letters.

      character arg*(*),cfn_upcase*(*)

      integer   larg


      larg=len(arg)

!    copy string
      cfn_upcase=arg

!    convert
      call cfn_s_upcase2(cfn_upcase,larg)

      return
      end

! ******************************************************************************

      function cfn_upcase2(arg,larg)

! A.L.			25 Aug 1998	v2r0	cfn_s_upcase2 added

      implicit none

! Function to convert all small letters in a character variable in
! capital letters.

      integer   larg
      character arg(larg)*1,cfn_upcase2*(*)

      integer   i,l

!    copy string
      cfn_upcase2=' '
      l=min(len(cfn_upcase2),larg)
      do i=1,l
         cfn_upcase2(i:i)=arg(i)
      enddo

!    convert
      call cfn_s_upcase2(cfn_upcase2,l)

      return
      end

! ******************************************************************************

      subroutine cfn_s_upcase2(arg,larg)

! description:
! ------------------------------------------------------------------------------
! convert string to uppercase
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      integer   larg             ! (I) number of characters of arg

      character arg(larg)*1      ! (I) string to convert


! local variables
      integer   i,n

      character hc*1


! functions


! include files


! program section
! ------------------------------------------------------------------------------


      do i=1,larg
         hc=arg(i)
         if (hc.le.'z' .and. hc.ge.'a') then
            n=ichar(hc)
            n=n-32
            arg(i)=char(n)
         endif
      enddo


! end of program
      return
      end


! ******************************************************************************

      subroutine cfn_s_upcase(arg)

! description:
! ------------------------------------------------------------------------------
! convert string to uppercase
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! arguments
      character arg*(*)          ! (I) string to convert


! local variables
      integer   i,n,larg

      character hc*1


! functions


! include files


! program section
! ------------------------------------------------------------------------------

      larg=len(arg)

      do i=1,larg
         hc=arg(i:i)
         if (hc.le.'z' .and. hc.ge.'a') then
            n=ichar(hc)
            n=n-32
            arg(i:i)=char(n)
         endif
      enddo


! end of program
      return
      end

      subroutine cfn_determ(par,ii,rr,ll,cc,n)

      implicit none

! figure out which data type the (character) variable par contains
!          - for the logical there is no solution yet
!
! in case integer: n=1
!   ''   real   : n=2
!   ''   logical: n=3
!   '' character: n=4 in case character between '', anders n=5

      integer ii,l,ios,n,ls

      real rr

      logical ll

      character par*(*),cc*(*),quote*1,dquote*2,c*1

      character format*16
      integer   lf


! functions
      integer   cfn_length,&! function
                cfn_determ_type2

      character cfn_trim*256


      quote=''''
      dquote=quote//quote


! determine type
! ------------
      lf=cfn_length(par)

      n=cfn_determ_type2(par,lf)


! read out the value
! ----------------------
      if (n.ge.1 .and. n.le.2) then
! par is at least a real
         ! the Sun compiler has difficulty reading a 1 character format
         ! the VAX on the other hand has no difficulty at all reading in a
         ! 1 character number via a 2 character format.
         ! that is why the 1 character number gets a different treatment
         if (lf.eq.1) then
            c=par(1:1)
            if (c.ge.'0' .and. c.le.'9') then
               ii=ichar(c)-ichar('0')
               rr=ii
            else
               ii=0
               rr=0.0
            endif
         else
            if (n.eq.1) then
               ! integer
               write(format,'(a,i3.3,a)') '(I',lf,')'
               read(par(1:lf),format,iostat=ios) ii
               rr=ii
            else
               ! real
               write(format,'(a,i3.3,a)') '(F',lf,'.0)'
               read(par(1:lf),format,iostat=ios) rr
               ii=rr
            endif
         endif
         cc=par
         ll=.true.
!         if (w.lt.3) then
!            ! integer
!            n=1
!         else
!            ! real
!            n=2
!         endif
      else
! so a character
         ii=0
         rr=0.0
!         n=5
         ll=.false.  ! I don't no why either
! seek out if character with quote starts and ends
         cc=cfn_trim(par)
         l=cfn_length(cc)
         ls=l
!         if (cc(1:1).eq.quote .and. cc(l:l).eq.quote) then
         if (n.eq.4) then
            cc=cc(2:l-1)
            ls=ls-2
! turn double quotes into single
            l=1
            do while (index(cc(l:),dquote).ne.0)
               l=index(cc(l:),dquote)+l
               cc(l:)=cc(l+1:)
               l=l+1
               ls=ls-1
            enddo
!            n=4
            ii=ls
         endif
      endif

! *** test
!      write(*,'(a,i5)') ' determine',n
!      write(*,'(a,i10)') '    ii',ii
!      write(*,'(a,f10.4)') '    rr',rr
!      write(*,'(a,a)') '    cc ',cc(1:50)

      return
      end


! ******************************************************************************

      function cfn_determ_type(string)

! description:
! ------------------------------------------------------------------------------
! determine the type of a string
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   cfn_determ_type   ! return value: 1: integer
                                  !               2: real
                                  !               3: logical
                                  !               4: 'character'
                                  !               5: character


! arguments
      character string*(*)        ! (I) string to test


! local variables
      integer   lstring


! functions
      integer   cfn_length,&
                cfn_determ_type2


! include files


! program section
! ------------------------------------------------------------------------------


! query the length of the string
      lstring=cfn_length(string)


! assign function value
      cfn_determ_type=cfn_determ_type2(string,lstring)


! end of program
      return
      end

! ******************************************************************************

      function cfn_determ_type2(string,lstring)

! description:
! ------------------------------------------------------------------------------
! determine the type of a string
!

! declaration section
! ------------------------------------------------------------------------------

      implicit none


! function declaration
      integer   cfn_determ_type2  ! return value: 1: integer
                                  !               2: real
                                  !               3: logical
                                  !               4: 'character'
                                  !               5: character


! arguments
      integer   lstring           ! (I) number of characters in the
                                  !     string to test
                                  !     LET OP!
                                  !     ALL characters take part in the
                                  !     test, so 'trailing' spaces and
                                  !     null-characters too!!!

      character string(lstring)*1 ! (I) string to test


! local variables
      integer   arr1(0:8),&  ! array to settle 0123456789
                arr2(0:8),&  ! array to settle +-
                arr3(0:8),&  ! array to settle .
                arr4(0:8),&  ! array to settle eEdD
                arr5(0:8),&  ! array to convert w to type
                i,w

      character c*1


! functions


! include files


! data
! conversion of 0 1 2 3 4 5 6 7 8
      data arr1/2,2,2,4,4,7,7,7,8/     ! 0123456789
      data arr2/1,8,8,8,8,6,8,8,8/     ! +-
      data arr3/3,3,3,8,8,8,8,8,8/     ! .
      data arr4/8,8,5,5,5,8,8,8,8/     ! eEdD

      data arr5/1,1,1,2,2,2,2,2,4/     ! w 0...8 -> type 1...5


! program section
! ------------------------------------------------------------------------------


! there will be ran through the string from front to back
! at each character is determined what var is going to be
!     +123.456E+01
! w: 012  34  567 8     changing points of sort
      w=0
      i=0
      do while (i.lt.lstring .and. w.lt.8)
         i=i+1
         c=string(i)
         if (c.ge.'0' .and. c.le.'9') then          ! 0123456789
            ! 0...1 -> w=2
            ! 2...2 -> w=w
            ! 3...3 -> w=4
            ! 4...4 -> w=w
            ! 5...6 -> w=7
            ! 7...8 -> w=w
!            if (w.lt.2) then
!               w=2
!            else if (w.eq.3) then
!               w=4
!            else if (w.eq.5 .or. w.eq.6) then
!               w=7
!            endif
             w=arr1(w)
         else if (c.eq.'+' .or. c.eq.'-') then      ! +-
            ! 0...0 -> w=1
            ! 1...4 -> w=8
            ! 5...5 -> w=6
            ! 6...8 -> w=8
!            if (w.eq.0) then
!               w=1
!            else if (w.eq.5) then
!               w=6
!            else
!               w=8
!            endif
             w=arr2(w)
         else if (c.eq.'.') then                    ! .
            ! 0...2 -> w=3
            ! 3...8 -> w=8
!            if (w.lt.3) then
!               w=3
!            else
!               w=8
!            endif
             w=arr3(w)
         else if (c.eq.'e' .or. c.eq.'E' .or. &     ! eE
                  c.eq.'d' .or. c.eq.'D') then      ! dD
            ! 0...1 -> w=8
            ! 2...4 -> w=5
            ! 5...8 -> w=8
!            if (w.lt.5 .and. w.gt.1) then
!               w=5
!            else
!               w=8
!            endif
             w=arr4(w)
         else
            w=8
         endif
      enddo


! convert w from 0...8 to 1...5
      w=arr5(w)


! in case w=4 check if it does not needs to be 5
      if (w.eq.4) then
         if (string(1).ne.'''' .or. string(lstring).ne.'''') w=5
      endif


! assign function value
      cfn_determ_type2=w


! end of program
      return
      end

      function cfn_dat2cen(datum)

!c description:
!c ------------------------------------------------------------------------------
!c convert date (yyyyddmm or yyddmm) to century day (1900/01/01=1)
!c date yymmdd is interpreted as 19yymmdd

!c declaration section
!c ------------------------------------------------------------------------------

      implicit none


!c function declaration
      integer   cfn_dat2cen  ! return value: century day number (1900/01/01=1)


!c arguments
      integer   datum        ! (I) date to convert


!c local variables
      integer   ldatum,yy,mm,dd,l4,l100,l400,leap,lcend

      integer   lmm(13,0:1)

      integer   errdate
      parameter (errdate=-999999999)


!c functions


!c include files


!c data
      data lmm / 0,31,60,91,121,152,182,213,244,274,305,335,366, &
                 0,31,59,90,120,151,181,212,243,273,304,334,365/


!c program section
!c ------------------------------------------------------------------------------

!c take over date
      ldatum=datum


!c render if necessary from yymmdd to yyyymmdd
      if (ldatum.lt.1000000) ldatum=ldatum+19000000


!c split up in year, month, day
      yy = int(ldatum/10000)
      mm = int(ldatum/100)-100*yy
      dd = ldatum-10000*yy-100*mm


!c leap year or not
!c function: lx = min(1,mod(yy,x)) at which x=(4,100,400)
!c          leap=mod(l4+l100+l400,2)  this function satisfies below mentioned
!c                                    truth table, that means it is convenient
!c                                    to use, there is no theoretical distraction
!c truth table
!c    in the truth table not all combinations of 1 and 0 occur because
!c    some combinations do not exist.
!c    for example: if l400.eq.0 then l100=0
!c          if l100.eq.0 then l4  =0
!c          if l4  .eq.1 then l100=1
!c          if l100.eq.1 then l400=1
!c     l4   |  l100  | l400  || leap year?
!c  -------------------------||---------------
!c     0   <=   0   <=   0   ||   0    yes
!c  -------------------------||---------------
!c     0   <=   0    |   1   ||   1    no
!c  -------------------------||---------------
!c     0    |   1    =>  1   ||   0    yes
!c  -------------------------||---------------
!c     1    =>  1    =>  1   ||   1    no
!c  -------------------------||---------------

      l4  =min(1,mod(yy,  4))
      l100=min(1,mod(yy,100))
      l400=min(1,mod(yy,400))
      leap=mod(l4+l100+l400,2)


!c century day
      if (mm.ge.1 .and. mm.le.12) then
         lcend=lmm(mm,leap)+dd
         if (lcend.gt.lmm(mm,leap) .and. lcend.le.lmm(mm+1,leap)) then
            lcend=lcend+int((yy-1901)*365.251+365)
         else
            ! ERROR, day does not exist
            lcend=errdate
         endif
      else
         ! ERROR, month does not exist
         lcend=errdate
      endif


!c assign function value
      cfn_dat2cen=lcend


!c end of program
      return
      end

      function cfn_perc_r(array,np,perc)

!c description:
!c ------------------------------------------------------------------------------
!c calculate percentile value from a sorted value (real)
!c in case necessary a linear interpolation takes place between 2 values


!c declaration section
!c ------------------------------------------------------------------------------

      implicit none


!c function declaration
      real   cfn_perc_r   ! return value: percentile value


!c arguments
      integer   np        ! (I) number of values in de array

      real      perc, &   ! (I) searched percentile value (in percentages)
                array(np) ! (I) data array


!c local variables
      integer   p1,p2

      real      f,v1,v2


!c functions


!c include files


!c program section
!c ------------------------------------------------------------------------------



      f =1.+(np-1.)*perc/100.                ! exact position of percentile
      p1=max(1,int(f))                       ! position <= percentile
      p2=min(np,p1+1)                        ! position >= percentile
      v1=array(p1)                           ! value <= percentile value
      v2=array(p2)                           ! value >= percentile value


!c assign function value
      cfn_perc_r=v1+(f-p1)*(v2-v1)


!c end of program
      return
      end

      function osd_basename(file,ext)

!c description:
!c ------------------------------------------------------------------------------
!c return the base name of a file name.
!c Removing the directory part and eventually the extension.


!c declaration section
!c ------------------------------------------------------------------------------

      implicit none


!c function declaration
      character osd_basename*(*) ! return value: base name of 'file'
                                 !     when an extension is supplied this
                                 !     is extracted from the file name too


!c arguments
      character file*(*), & ! (I) file name where the
                ext*(*)     ! (I) file extension to be removed


!c local variables
      integer   ibegin,iend,l,indx,os

      character del*1


!c functions
      integer   osd_get_os, &
                cfn_length, &
                cfn_lindex


!c include files


!c program section
!c ------------------------------------------------------------------------------

      os=osd_get_os()

      del=' '

      if (os.eq.1) then
!c VMS
         del=']'
      else if (os.eq.2 .or. os.eq.4) then
!c Unix,Linux
         del='/'
      else if (os.eq.3) then
!c DOS
         del=char(92)    ! char(92)='\'
      endif


      ibegin=1
      iend  =cfn_length(file)

      if (del.ne.' ') then

         ! remove extension
         l=cfn_length(ext)
         if (l.gt.0 .and. iend.ge.l) then
            if (ext(1:l).eq.file(iend-l+1:iend)) then
               ! file contains the supplied extension, remove it
               iend=iend-l
            endif
         endif

         ! remove leading directory
         indx=cfn_lindex(file(ibegin:iend),del)

         ibegin=ibegin+indx

      endif

      ! minimum length
      l=min(len(osd_basename),iend-ibegin+1)
      iend=ibegin+l-1

      osd_basename=file(ibegin:iend)


!c end of program
      return
      end
