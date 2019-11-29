!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#### ########################################################
SUBROUTINE VEGTYPE_TO_PATCH_IRRIG(IVEGTYPE,INPATCH,NPAR_VEG_IRR_USE,IPATCH_NB,IPATCH_NAME)
!#############################################################
!
!!****  *VEGTYPE_TO_PATCH_IRRIG* - routine to attribute patch numbre (and name) with the nvegtype and irrigation with ECOSG
!!
!!    PURPOSE
!!    -------
!
!  If there is irrigation (from ECOCLIMAP-SG), used to calculate the patch indice
!          corresponding to different vegtype, for the initial and irrigated vegtype,
!          according to the  number of patch  (NPATCH).
!             
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!    A. DRUEL        10/2018 
!
!!    MODIFICATIONS
!!    -------------
!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
USE MODI_VEGTYPE_TO_PATCH
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_AGRI,        ONLY : NVEG_IRR, NPATCH_TREE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,          INTENT(IN) :: IVEGTYPE !indices of vegetation type           
INTEGER,          INTENT(IN) :: INPATCH  !total number of PATCHES used 
INTEGER,DIMENSION(:), INTENT(IN) :: NPAR_VEG_IRR_USE ! vegtype with irrigation
!
INTEGER, INTENT(OUT)             :: IPATCH_NB    ! PATCH index corresponding to the vegtype IVEGTYPE  
CHARACTER(LEN=45),DIMENSION(2), OPTIONAL, INTENT(OUT) :: IPATCH_NAME ! Name of patch (dim 1: patch name, dim 2: vegtype inside)
!
INTEGER                      :: INPATCH_ORI  !equivalent number of PATCHES without irrigation
INTEGER,DIMENSION(NVEGTYPE)  :: IPATCH_LIST  ! List of irrigated patch
INTEGER                      :: IPATCH_TEMP  ! Local IPATCH_NB for irrigated vegtype. Use to compute IPATCH_LIST
INTEGER                      :: JIRR,JC      ! loop on patches and J counter
!
CHARACTER(LEN=2) :: YVEGTYPE
REAL(KIND=JPRB)  :: ZHOOK_HANDLE
!
!*      0.2    declarations of local variables
!
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('VEGTYPE_TO_PATCH_IRRIG',0,ZHOOK_HANDLE)
!
! 0. Check input.
! 0.1 If no irrigation, directly go to VEGTYPE_TO_PATCH
IF ( NVEG_IRR == 0 ) THEN !.OR. ANY(NPAR_VEG_IRR_USE(:)==0.) ) THEN
  IF (PRESENT(IPATCH_NAME)) THEN
    CALL VEGTYPE_TO_PATCH(IVEGTYPE, INPATCH,IPATCH_NB,IPATCH_NAME)
  ELSE
    CALL VEGTYPE_TO_PATCH(IVEGTYPE, INPATCH,IPATCH_NB)
  ENDIF
  !
! 0.2 Check wrong input case
ELSEIF ( ANY(NPAR_VEG_IRR_USE==0) .AND. ANY(NPAR_VEG_IRR_USE/=0) ) THEN
  CALL ABOR1_SFX('VEGTYPE_TO_PATCH_IRRIG: NPAR_VEG_IRR_USE NOT WEEL IMPLEMENTED (no one or all values are to be defined)')
  !
! 0.3 If call by a case without taking account irrigation
ELSEIF ( ANY(NPAR_VEG_IRR_USE(:)==0.) ) THEN
  IF (PRESENT(IPATCH_NAME)) THEN
    CALL VEGTYPE_TO_PATCH(IVEGTYPE, INPATCH,IPATCH_NB,IPATCH_NAME)
  ELSE
    CALL VEGTYPE_TO_PATCH(IVEGTYPE, INPATCH,IPATCH_NB)
  ENDIF
  !
! 2.0 bis - in specific case: when impose NPATCH = 1 for specific routines (ex: PUT_ON_ALL_VEGTYPES or GET_PREP_INTERP)
ELSEIF ( INPATCH == 1 ) THEN
  IF (PRESENT(IPATCH_NAME)) CALL ABOR1_SFX('VEGTYPE_TO_PATCH_IRRIG: INPATCH = 1 IS NOT ACCEPTABLE FOR OTHER ROUTINES')
  IPATCH_NB = 1
  !
! Real start of this routine
ELSE
  !
  ! 1. Find the number of PATCH WITHOUT irrigation (if not given)
  ! 1.1 If not given (only if defaults irrigated vegtype is used)
  IF ( NPATCH_TREE == 0 .AND. NVEG_IRR > 0 ) THEN
    IF (INPATCH==2) THEN
      INPATCH_ORI=1
    ELSEIF (INPATCH==5) THEN
      INPATCH_ORI=3
    ELSEIF (INPATCH==10) THEN
      INPATCH_ORI=7
    ELSEIF (INPATCH==12) THEN
      INPATCH_ORI=9
    ELSEIF (INPATCH==14) THEN
      INPATCH_ORI=10
    ELSEIF (INPATCH==15) THEN
      INPATCH_ORI=12
    ELSEIF (INPATCH==19) THEN
      INPATCH_ORI=13
    ELSEIF (INPATCH==26) THEN
      INPATCH_ORI=20
    ENDIF
  !
  ! 1.2 There is an input number of tree patch
  ELSEIF ( NVEG_IRR > 0 ) THEN
    INPATCH_ORI = NPATCH_TREE
  ENDIF
  !
  ! 
  ! 2. Find IPATCH_NB for vegtype + vegtype irrigated
  ! 2.1 If all vegtype are possibely irrigated (IVEG=IPATCH) (not necessary, only to win compute time...
  IF (INPATCH == 2 * INPATCH_ORI .AND. INPATCH_ORI == NVEGTYPE ) THEN
    IF ( .NOT.PRESENT(IPATCH_NAME) ) THEN
      IPATCH_NB = IVEGTYPE
    ELSEIF ( IVEGTYPE <= NVEGTYPE ) THEN
      CALL VEGTYPE_TO_PATCH(IVEGTYPE, INPATCH_ORI,IPATCH_NB,IPATCH_NAME)
    ELSE
      CALL VEGTYPE_TO_PATCH(NPAR_VEG_IRR_USE(IVEGTYPE-NVEGTYPE), INPATCH_ORI,IPATCH_NB,IPATCH_NAME)
      IPATCH_NB = IPATCH_NB + INPATCH_ORI
    ENDIF
  !
  ! 2.2 Else, for non irrigated vegetype
  ELSEIF ( IVEGTYPE <= NVEGTYPE ) THEN
    IF (PRESENT(IPATCH_NAME)) THEN
      CALL VEGTYPE_TO_PATCH(IVEGTYPE, INPATCH_ORI,IPATCH_NB,IPATCH_NAME)
    ELSE
      CALL VEGTYPE_TO_PATCH(IVEGTYPE, INPATCH_ORI,IPATCH_NB)
    ENDIF
  !
  ! 2.3 Else, for irrigated vegtype
  ELSEIF ( IVEGTYPE <= NVEGTYPE+SIZE(NPAR_VEG_IRR_USE) ) THEN
!
!  ! 2.2.a If only one other patch with irrigation
!!  IF ( INPATCH - INPATCH_ORI == 1 ) THEN
!     IPATCH_NB = INPATCH
!  !
!  ! 2.2.b If the irrigated vegtype are patched (and separated)
!  ELSE
    ! Calculate correspondig IPATCH for non irrigated vegtype
    IF (PRESENT(IPATCH_NAME)) THEN
      CALL VEGTYPE_TO_PATCH(NPAR_VEG_IRR_USE(IVEGTYPE-NVEGTYPE), INPATCH_ORI,IPATCH_NB,IPATCH_NAME)
    ELSE
      CALL VEGTYPE_TO_PATCH(NPAR_VEG_IRR_USE(IVEGTYPE-NVEGTYPE), INPATCH_ORI,IPATCH_NB)
    ENDIF
    !
    ! List all irrigated patch
    IPATCH_LIST(:) = 0
    JC = 0
    IPATCH_TEMP = 0
    DO JIRR=1,SIZE(NPAR_VEG_IRR_USE)
      IF ( IPATCH_NB == IPATCH_TEMP ) CYCLE
      !
      CALL VEGTYPE_TO_PATCH(NPAR_VEG_IRR_USE(JIRR), INPATCH_ORI,IPATCH_TEMP)
      !
      IF ( .NOT.ANY(IPATCH_LIST(:) == IPATCH_TEMP) ) THEN
        JC = JC + 1
        IPATCH_LIST(JC)=IPATCH_TEMP
      ENDIF
    !
    ENDDO
    !
    ! Give the IPATCH_NB of irrigated patch: JC
    IPATCH_NB = JC + INPATCH_ORI
!  ENDIF
!
! 3. If no one of these cases
  ELSE
    CALL ABOR1_SFX('VEGTYPE_TO_PATCH_IRRIG: FATAL ERROR DUE TO IVEG > NVEG + NVEG_IRR')
  ENDIF
ENDIF
!
IF (LHOOK) CALL DR_HOOK('VEGTYPE_TO_PATCH_IRRIG',1,ZHOOK_HANDLE)
!
END SUBROUTINE VEGTYPE_TO_PATCH_IRRIG
