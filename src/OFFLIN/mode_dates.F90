!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
MODULE MODE_DATES
!     ########################################################################
!
!    Module to manipulate dates
!
!-----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TYPE_DATE_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
INTEGER, PARAMETER :: JP_GREGOIRE=1582 ! Année où le pape Grégoire XIII décida de passer du calendrier julien au calendrier grégorien
                                       ! Avant cette date tous les années multiples de 4 sont des années bissextiles
                                       ! Au-delà de cette date les années multiples de 100 ne sont pas bissextiles sauf les années multiples de 400 qui le sont
                                       ! De plus les journées du 5 au 14 octobre 1582 ont été supprimées pour compenser le retard accumulé
!
CONTAINS
!
!-----------------------------------------------------------------------------
!
!     ##################
SUBROUTINE ADD_DAYS(IYEAR,IMONTH,IDAY,KDAYS,IMONTH_OUT,IDAY_OUT,IYEAR_OUT)
!     ########################################################################
!
!!****  * ADD_DAYS *
!!
!!    PURPOSE
!!    -------
!!
!!    Obtain the good month / day afther ADD or REMOVE days
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      A. Druel      *CNRM*    2018-12-21
!!
!!    MODIFICATIONS
!!    ------
!!
!!
!!      
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!!
IMPLICIT NONE
!
INTEGER, INTENT(IN)  :: IYEAR       ! Current year
INTEGER, INTENT(IN)  :: IMONTH      ! Current month
INTEGER, INTENT(IN)  :: IDAY        ! Current day
INTEGER, INTENT(IN)  :: KDAYS       ! Days number to had or remove
!
INTEGER, INTENT(OUT) :: IMONTH_OUT  ! Month after addition
INTEGER, INTENT(OUT) :: IDAY_OUT    ! Day after addition
INTEGER, OPTIONAL, INTENT(OUT) :: IYEAR_OUT ! Year after addition
!
INTEGER,DIMENSION(12):: INB_DM      ! Number of days per months
INTEGER              :: IREM_DAYS   ! Number of remaining days to add
INTEGER              :: IYEAR_TMP   ! Year after addition
!
REAL(KIND=JPRB)                             :: ZHOOK_HANDLE    !! For THE doctor
!
IF (LHOOK) CALL DR_HOOK('MODE_DATES:ADD_DAYS',0,ZHOOK_HANDLE)
!
!*       1.    INITIALIZATION
!              --------------
IYEAR_TMP = IYEAR
IMONTH_OUT = IMONTH
IDAY_OUT = IDAY
IREM_DAYS = KDAYS
!
IF ( IYEAR_TMP == JP_GREGOIRE ) THEN
  INB_DM=(/31,28,31,30,31,30,31,31,30,21,30,31/)
ELSE
  IF (LEAPYEAR(IYEAR_TMP)) THEN
    INB_DM=(/31,29,31,30,31,30,31,31,30,31,30,31/)
  ELSE
    INB_DM=(/31,28,31,30,31,30,31,31,30,31,30,31/)
  ENDIF
ENDIF
!
IF ( IMONTH<1 .OR. IMONTH>13 ) THEN
  CALL ABOR1_SFX('MODE_DATES:ADD_DAYS, THIS MONTH DO NOT EXIST.')
ELSEIF (IDAY<1 .OR. IDAY>INB_DM(IMONTH) ) THEN
  CALL ABOR1_SFX('MODE_DATES:ADD_DAYS, THIS DAY DO NOT EXIST (FOR THIS MONTH AND YEAR')
ENDIF
!
!
!*       2. COMPUTE DAY BY DAY
!           ------------------
!
DO
  !
  ! To add days
  IF ( IREM_DAYS > 0 ) THEN
    ! We stay the same month if there is still enougth days
    IF ( IDAY_OUT + IREM_DAYS <= INB_DM(IMONTH_OUT) ) THEN
      IDAY_OUT = IDAY_OUT + IREM_DAYS
      EXIT
      !
    ELSE
      ! We add the good day number to reach the first of nex month
      IREM_DAYS = IREM_DAYS - ( INB_DM(IMONTH_OUT) - IDAY_OUT + 1 )
      IDAY_OUT = 1
      !
      ! If we stay the same year
      IF ( IMONTH_OUT < 12 ) THEN
        ! Next month
        IMONTH_OUT = IMONTH_OUT + 1
        !
      ELSE
        ! Next Year
        IMONTH_OUT = 1
        IYEAR_TMP = IYEAR_TMP + 1
        !
        ! Find the days number
        ! Special case of 1582
        IF ( IYEAR_TMP == JP_GREGOIRE ) THEN
          INB_DM=(/31,28,31,30,31,30,31,31,30,21,30,31/)
        ELSE
          IF (LEAPYEAR(IYEAR_TMP)) THEN
            INB_DM=(/31,29,31,30,31,30,31,31,30,31,30,31/)
          ELSE
            INB_DM=(/31,28,31,30,31,30,31,31,30,31,30,31/)
          ENDIF
        ENDIF
        !
      ENDIF
    ENDIF
    !
  ! To remove days
  ELSEIF ( IREM_DAYS < 0 ) THEN
          ! We stay the same month if there is still enougth days
    IF ( IDAY_OUT + IREM_DAYS >= 1 ) THEN
      IDAY_OUT = IDAY_OUT + IREM_DAYS
      EXIT
      !
    ELSE
      ! We remove the good day number to come back at the month before
      IREM_DAYS = IREM_DAYS + IDAY_OUT
      !
      ! If we stay the same year
      IF ( IMONTH_OUT > 1 ) THEN
        ! Month Before
        IMONTH_OUT = IMONTH_OUT - 1
        IDAY_OUT = INB_DM(IMONTH_OUT)
        !
      ELSE
        ! Previous year
        IYEAR_TMP = IYEAR_TMP - 1
        IMONTH_OUT = 12
        IDAY_OUT = INB_DM(IMONTH_OUT)
        !
        ! Find the days number
        ! Special case of 1582
        IF ( IYEAR_TMP == JP_GREGOIRE ) THEN
          INB_DM=(/31,28,31,30,31,30,31,31,30,21,30,31/)
        ELSE
          IF (LEAPYEAR(IYEAR_TMP)) THEN
            INB_DM=(/31,29,31,30,31,30,31,31,30,31,30,31/)
          ELSE
            INB_DM=(/31,28,31,30,31,30,31,31,30,31,30,31/)
          ENDIF
        ENDIF
        !
      ENDIF
    ENDIF
    !
  ELSE
    EXIT
  ENDIF
  !
ENDDO
!
IF ( PRESENT(IYEAR_OUT) ) IYEAR_OUT = IYEAR_TMP
!
IF (LHOOK) CALL DR_HOOK('MODE_DATES:ADD_DAYS',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
END SUBROUTINE ADD_DAYS
!
!------------------------------------------------------------------------------
!
LOGICAL FUNCTION LEAPYEAR (IYEAR)
!
! Author : Matthieu Lafaysse (from MODE_DATES_NETCDF)
! Creation : 2012-11-12
!
INTEGER, INTENT(IN) :: IYEAR        ! Is the year a leap year ?
REAL(KIND=JPRB)     :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_DATES:LEAPYEAR',0,ZHOOK_HANDLE)
!
IF ( IYEAR > JP_GREGOIRE) THEN
  LEAPYEAR = (((MOD(IYEAR,4)==0).AND.(MOD(IYEAR,100)/=0)).OR.(MOD(IYEAR,400)==0))
ELSE
  LEAPYEAR = (MOD(IYEAR,4)==0)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_DATES:LEAPYEAR',1,ZHOOK_HANDLE)
!
END FUNCTION LEAPYEAR
!
!--------------------------------------------------------------------------------------------------------------
!
END MODULE MODE_DATES
