!     include file cfn_rttimer.inc

! description:
! ------------------------------------------------------------------------------
!
!

! declaration section
! ------------------------------------------------------------------------------

!      implicit none


! arguments
      integer    mxtimr
      parameter (mxtimr=50)  !maximum number of subroutines for which time can be stored

      integer   nrtt,&               ! (C) aantal te timen routines
                irtttot(3,0:mxtimr)  ! (C) statistieken
                                     !     irtttot(1,*) total time
                                     !     irtttot(2,*) number of times
                                     !     irtttot(3,*) start time

      character crttids(0:mxtimr)*8  ! (C) routine ids

      logical   lrtton,&             ! (C) .true.  timer is active
                                     !     .false. not
                lrttoverflow         ! (C) .true.  too much subroutine trying
                                     !             to time, mxtimr is too small

      common /rttn/nrtt,irtttot,lrtton,lrttoverflow
      common /rttc/crttids
