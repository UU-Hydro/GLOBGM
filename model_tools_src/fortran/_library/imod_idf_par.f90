!!  Copyright (C) Stichting Deltares, 2005-2017.
!!
!!  This file is part of iMOD.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License as published by
!!  the Free Software Foundation, either version 3 of the License, or
!!  (at your option) any later version.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!!
!!  Contact: imod.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands.

MODULE IMOD_IDF_PAR

!## specify kind for double-precision real with 15 digits and a point ranging up to exponent 307
INTEGER,PARAMETER :: DP_KIND=SELECTED_REAL_KIND(15,307) 
!## specify kind for single-precision real with 6  digits and a point ranging up to exponent 37
INTEGER,PARAMETER :: SP_KIND=SELECTED_REAL_KIND(6 ,37 ) 

TYPE IDFOBJ
 INTEGER :: ITYPE   !## type of IDF 0=4 bytes; 1=8 bytes
 INTEGER :: IU
 INTEGER :: NCOL
 INTEGER :: NROW
 INTEGER :: IEQ     !=0:equi =1:non-equi
 INTEGER :: ITB     !=0: =1:usage of top/bot information
 INTEGER :: IXV     !=0:storage in x =1:storage in v
 INTEGER :: JD      !=julian date (if neccessary)
 INTEGER :: IDY,IMH,IYR  !## days,months,years
 INTEGER :: IHR,IMT,ISC  !## hours,minutes,seconds
 REAL(KIND=DP_KIND) :: DAYFRACTION !## daily fraction 
 INTEGER :: ILAY    !=ilay of idf (if neccessary)
 INTEGER :: UNITS   !=units
 REAL(KIND=DP_KIND) :: XMIN,YMIN,XMAX,YMAX
 REAL(KIND=DP_KIND) :: DX,DY        !equi.distance if ieq=0
 REAL(KIND=DP_KIND) :: TOP,BOT      !top and bot information
 REAL(KIND=DP_KIND) :: NODATA,DMIN,DMAX
 REAL(KIND=DP_KIND),DIMENSION(:),POINTER :: SX   !x.coord. network
 REAL(KIND=DP_KIND),DIMENSION(:),POINTER :: SY   !y.coord. network
 REAL(KIND=DP_KIND),DIMENSION(:,:),POINTER :: X  !idfvalues in matrix
 REAL(KIND=DP_KIND),DIMENSION(:),POINTER :: V    !idfvalues in vector
 INTEGER(KIND=2),DIMENSION(:,:),POINTER :: YSEL    !idfvalues in vector, irow/icol
 CHARACTER(LEN=4),DIMENSION(:),POINTER :: COMMENT    !comments
 INTEGER :: NTHREAD
 CHARACTER(LEN=256) :: FNAME  ! name of the idf
END TYPE IDFOBJ

END MODULE IMOD_IDF_PAR
