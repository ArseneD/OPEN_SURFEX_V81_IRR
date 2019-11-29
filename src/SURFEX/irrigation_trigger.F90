!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE IRRIGATION_TRIGGER(AG, PEK, PF2, PTSTEP, TPTIME)
!     ########################################################################
!
!!****  *IRRIGATION_TRIGGER* 
!!
!!    PURPOSE
!!    -------
!!    
!!    Determines at each time step where irrigation water must be applied.
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
!!      J. Etchanchu  *CESBIO*
!!      A. Druel      *CNRM*
!!
!!    MODIFICATIONS
!!    -------------
!!      Local application               01/2018
!!      Global scale use & update       06/2018
!!      
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ISBA_n, ONLY : ISBA_PE_t
USE MODD_AGRI_n, ONLY : AGRI_t
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODD_AGRI,   ONLY   : XTHRESHOLD, NIRR_STOP_BTR
!
USE MODD_TYPE_DATE_SURF, ONLY : DATE_TIME
!
USE MODE_DATES
!
USE YOMHOOK   ,ONLY   : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY   : JPRB
!
IMPLICIT NONE
!
TYPE(AGRI_t), INTENT(INOUT)                 :: AG              !! Content use here: NIRR_TSC (time step counter for irrigation, save),
                                                               !!                   LIRRIGATE (mainoutput, T/F)
TYPE(ISBA_PE_t), INTENT(IN)                 :: PEK             !! Content use here: XIRRIGTYPE (0,1,2,3), TSEED (date), TREAP(date),
                                                               !!                   XIRRIGFREQ (irrig freq max in s),
                                                               !!                   XIRRIGTIME (time of an irrigation in s)
TYPE(DATE_TIME), INTENT(IN)                 :: TPTIME          !! Current date (content use: TDATE%MONTH and TDATE%DAY)

REAL, DIMENSION(:), INTENT(IN)              :: PF2             !! Water stress coefficient
REAL,    INTENT(IN)                         :: PTSTEP          !! model time step (s)
!
LOGICAL, DIMENSION(SIZE(PEK%XIRRIGTYPE(:))) :: GINV            !! Check seed/reap inversion (T/F)
REAL,    DIMENSION(SIZE(PEK%XIRRIGTYPE(:))) :: ZTHRESHOLD      !! Threshold use
INTEGER, DIMENSION(SIZE(PEK%XIRRIGTYPE(:))) :: IMONTH_STOP, IDAY_OUT_STOP ! Date to stop irrigation 
INTEGER                                     :: IJ
!
REAL(KIND=JPRB)                             :: ZHOOK_HANDLE    !! For THE doctor
!
IF (LHOOK) CALL DR_HOOK('IRRIGATION_TRIGGER',0,ZHOOK_HANDLE)
!
!
! 1. Determining if there are inversions in seeding and reaping date (winter crops)
! ---------------------------------------------------------------------------------
!
GINV = .FALSE.
!
WHERE ( PEK%TSEED(:)%TDATE%MONTH >  PEK%TREAP(:)%TDATE%MONTH .OR. &
      ( PEK%TSEED(:)%TDATE%MONTH == PEK%TREAP(:)%TDATE%MONTH .AND. PEK%TSEED(:)%TDATE%DAY > PEK%TREAP(:)%TDATE%DAY ) ) 
  GINV(:) = .TRUE.
END WHERE
!
!
! 2. Reinitialisate NIRRINUM if needeed (bewteen irrigation)
! ----------------------------------------------------------
!
WHERE ( PEK%XIRRIGTYPE(:) > 0. .AND. AG%NIRR_TSC(:)<=0 .AND. AG%NIRRINUM(:) /= 1 )
  WHERE ( ( TPTIME%TDATE%MONTH == 1 .AND. TPTIME%TDATE%DAY == 1 .AND. .NOT.GINV(:) ) .OR.                                       &  ! Summer crops: 1st January
        ( ( TPTIME%TDATE%MONTH <  PEK%TSEED(:)%TDATE%MONTH .OR.                                                                 &  ! Before seeding date
            ( TPTIME%TDATE%MONTH == PEK%TSEED(:)%TDATE%MONTH .AND. TPTIME%TDATE%DAY < PEK%TSEED(:)%TDATE%DAY ) ) .AND.          &  !
          ( .NOT. GINV(:)                                                                                                       &  ! Summer crops: before seeding ONLY
          .OR. ( GINV(:) .AND. ( TPTIME%TDATE%MONTH >  PEK%TREAP(:)%TDATE%MONTH                                                 &  ! Winter crops: before seeding AND after reaping
                    .OR. (TPTIME%TDATE%MONTH==PEK%TREAP(:)%TDATE%MONTH .AND. TPTIME%TDATE%DAY>PEK%TREAP(:)%TDATE%DAY) ) ) ) ) ) &
    AG%NIRRINUM(:) = 1
ENDWHERE
!
!
! 3. Initialization of the time count:
!                  - for possibly irrigated arrea
!                  - no irrigation has been triggered during the minimal return time since last irrigation
!                  - when F2 is under the threshold and the counter is null,
!                  - AND after seeding date and before reaping date
!                  NOTE: It's possible to add here a constrain of human PF2 test (after 6:00 and until 20:00?)
! --------------------------------------------------------------------------------------------------------
!
! Compute 2 weeks before irrigation
DO IJ = 1, SIZE(PEK%XIRRIGTYPE(:))
  CALL ADD_DAYS(TPTIME%TDATE%YEAR, PEK%TREAP(IJ)%TDATE%MONTH, PEK%TREAP(IJ)%TDATE%DAY, -NIRR_STOP_BTR, &
                                   IMONTH_STOP(IJ), IDAY_OUT_STOP(IJ))
ENDDO
!
WHERE ( PEK%XIRRIGTYPE(:).GT.0 .AND. AG%NIRR_TSC(:)<=0 ) 
  !
  ! Chose bewteen XF2THRESHOLD (if define) and XTHRESHOLD (else), if needeed after
  WHERE ( ABS( (PEK%XF2THRESHOLD(:)/XUNDEF) - 1 ) > 1.E-5) ! PEK%XF2THRESHOLD(:)  < 1.1 ) !/= XUNDEF )
    ZTHRESHOLD(:) = PEK%XF2THRESHOLD(:)
  ELSEWHERE
    ZTHRESHOLD(:) = XTHRESHOLD(MIN(AG%NIRRINUM(:),SIZE(XTHRESHOLD,1)))
  ENDWHERE

  WHERE ( PF2(:) < ZTHRESHOLD(:) )
    !
    ! after seeding date AND 2 weeks before reaping date (summer crops)
    ! after seeding date OR  2 weeks before reaping date (winter crops)
    WHERE ( ( ( (TPTIME%TDATE%MONTH == PEK%TSEED(:)%TDATE%MONTH .AND. TPTIME%TDATE%DAY >= PEK%TSEED(:)%TDATE%DAY).OR.         &  ! after seeding date
               TPTIME%TDATE%MONTH > PEK%TSEED(:)%TDATE%MONTH )                                                                &  
            .AND. ( GINV(:) .OR. TPTIME%TDATE%MONTH <  IMONTH_STOP(:)                                                         &  ! AND 2 weeks before reaping date (summer crops)
                    .OR. ( TPTIME%TDATE%MONTH == IMONTH_STOP(:) .AND. TPTIME%TDATE%DAY < IDAY_OUT_STOP(:) ) ) )               &
          .OR. ( GINV(:)                                                                                                      &  ! OR 2 weeks before reaping date (winter crops)
                 .AND. ( TPTIME%TDATE%MONTH <  IMONTH_STOP(:)                                                                 &
                          .OR.( TPTIME%TDATE%MONTH == IMONTH_STOP(:) .AND. TPTIME%TDATE%DAY<IMONTH_STOP(:) ) ) )              &
          .OR. ( PEK%TSEED(:)%TDATE%MONTH == 1  .AND. PEK%TSEED(:)%TDATE%DAY == 1 .AND.                                       &  ! Ajout de l'exeption si toute l'année: irrigation continue
                 PEK%TREAP(:)%TDATE%MONTH == 12 .AND. PEK%TREAP(:)%TDATE%DAY == 31 ) )
      !
      WHERE ( PEK%XIRRIGFREQ>0. ) !! If a minimum frequence is define. Mostly for spinkler or flood
        !
        AG%NIRR_TSC(:) = PEK%XIRRIGFREQ(:)/PTSTEP         !! NIRR_TSC is the counter of time steps number MIN BEFORE ANOTHER IRRIGATION
        !
      ELSE WHERE                  !! When a minimum frequence is not define. Mostly for drip
        !
        AG%NIRR_TSC(:) = PEK%XIRRIGTIME(:)/PTSTEP         !! NIRR_TSC is the counter of time steps number OF IRRIGATION
        !
      END WHERE
      !
      AG%NIRRINUM (:) = AG%NIRRINUM (:) + 1
      !
    END WHERE
    !
  END WHERE
  !
END WHERE
!
!
! 4. Determining if irrigation must be applied for the current time step, depending on the value of the time step counter NIRR_TSC
! --------------------------------------------------------------------------------------------------------------------------------
!
! Points where the application time is ended but not the minimal time between two irrigations (spraying and flooding)
!
WHERE ( PEK%XIRRIGTYPE(:)/=0. .AND. AG%NIRR_TSC(:)>0 ) 
  !
  WHERE ( PEK%XIRRIGFREQ>0. ) !! If a minimum frequence is define. Mostly for spinkler or flood
    !
    ! Test if the time if irrigation is not over 
    WHERE ( AG%NIRR_TSC(:) <= (PEK%XIRRIGFREQ(:) - PEK%XIRRIGTIME(:)) / PTSTEP )
      !
      AG%LIRRIGATE(:) = .FALSE.
      !
    ELSE WHERE
      !
      ! The application time is not ended
      AG%LIRRIGATE(:) = .TRUE.
      !
    END WHERE
    !
  ELSE WHERE                  !! When a minimum frequence is not define. Mostly for drip
    !
    ! The application time is not ended
    AG%LIRRIGATE(:) = .TRUE.
    !
  ENDWHERE  
  !
  ! Reduce time counter for next time step
  AG%NIRR_TSC(:) = AG%NIRR_TSC(:) - 1
  !
ELSE WHERE !( AG%NIRR_TSC(:)<=0 .OR. PEK%XIRRIGFREQ<=0. ) !! When a minimum frequence is not define. Mostly for drip
  !
  ! Points where the application time is ended
  AG%LIRRIGATE(:) = .FALSE.
  !
END WHERE
!
IF (LHOOK) CALL DR_HOOK('IRRIGATION_TRIGGER',1,ZHOOK_HANDLE)
!
END SUBROUTINE IRRIGATION_TRIGGER
