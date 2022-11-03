!     include file cfn_clas.inc

! description:
! ------------------------------------------------------------------------------
! common block voor cfn_clas_* routines
!


! declaration section
! ------------------------------------------------------------------------------

!      implicit none


! arguments
      integer   cfn_clas_len,&      ! (C) length of the commandline
                cfn_clas_narg       ! (C) number of arguments left in the string.
                                    !     The numbering of the arguments starts
                                    !     from 0= command
                                    !     This will never be removed from the
                                    !     string after the request

      character cfn_clas_cla*2048   ! (C) total commandline


      common /cfn_clas_c/cfn_clas_cla

      common /cfn_clas_n/cfn_clas_len,cfn_clas_narg

