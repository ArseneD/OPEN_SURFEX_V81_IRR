!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE GET_PREP_INTERP(KNP_IN,KNP_OUT,PVEGTYPE,PPATCH_IN,PPATCH_OUT,NPAR_VEG_IRR_USE,KMASK_IN)
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_AGRI,           ONLY : NVEG_IRR
!
USE MODI_VEGTYPE_TO_PATCH_IRRIG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KNP_IN
INTEGER, INTENT(IN) :: KNP_OUT
REAL, DIMENSION(:,:), INTENT(IN)  :: PVEGTYPE
REAL, DIMENSION(:,:), INTENT(IN)  :: PPATCH_IN
REAL, DIMENSION(:,:), INTENT(OUT) :: PPATCH_OUT
INTEGER,DIMENSION(:), INTENT(IN)  :: NPAR_VEG_IRR_USE ! vegtype with irrigation
INTEGER, DIMENSION(:,:), INTENT(IN), OPTIONAL :: KMASK_IN
!
INTEGER, DIMENSION(SIZE(PPATCH_OUT,1),SIZE(PPATCH_IN,2)) :: IMASK_IN
REAL, DIMENSION(SIZE(PPATCH_OUT,1),SIZE(PPATCH_OUT,2)) :: ZPATCH_OUT
INTEGER :: JP, JVEG, IP_I, IP_O, JI, IMASK
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('GET_PREP_INTERP',0,ZHOOK_HANDLE)
!
IF (PRESENT(KMASK_IN)) THEN
  IMASK_IN(:,:) = KMASK_IN(:,:)
ELSE
  DO JI = 1,SIZE(IMASK_IN,1)
    IMASK_IN(JI,:) = JI
  ENDDO
ENDIF
! 
ZPATCH_OUT(:,:) = 0.
!
! if NPATCH (in) == NVEGTYPE (with irrigation), the arrays of patches is this of vegtypes
IF (KNP_IN==NVEGTYPE+NVEG_IRR ) THEN
  !
  DO JVEG = 1,NVEGTYPE+NVEG_IRR
    ! mask of the output patch in which is this vegtype
    CALL VEGTYPE_TO_PATCH_IRRIG(JVEG,KNP_OUT,NPAR_VEG_IRR_USE,IP_O)
    DO JI = 1,SIZE(IMASK_IN,1)
      IMASK = IMASK_IN(JI,IP_O)
      IF (IMASK/=0) ZPATCH_OUT(IMASK,JVEG) = PVEGTYPE(JI,JVEG)
    ENDDO
  ENDDO
  !
! if there is the same number of patches before / after, patch_out = patch_in
ELSEIF (KNP_IN==KNP_OUT) THEN
  !
  ! the mask can be applied to the patches before and after
  DO JP = 1,SIZE(IMASK_IN,2)
    DO JI = 1,SIZE(IMASK_IN,1)
      IMASK = IMASK_IN(JI,JP)
      IF (IMASK/=0) ZPATCH_OUT(IMASK,JP) = PPATCH_IN(JI,JP)
    ENDDO
  ENDDO
  !
! less patches before than after
ELSEIF (KNP_IN<KNP_OUT) THEN
  !
  ! to which input patch contributes each output patch?  
  DO JP = 1,KNP_OUT
    !
    ! which vegtype is in this output patch? 
    DO JVEG = 1,NVEGTYPE+NVEG_IRR
      ! output patch in which is this vegtype
      CALL VEGTYPE_TO_PATCH_IRRIG(JVEG,KNP_OUT,NPAR_VEG_IRR_USE,IP_O)
      ! input patch in which is this vegtype
!      IF ( KNP_IN /= 1 ) THEN
      CALL VEGTYPE_TO_PATCH_IRRIG(JVEG,KNP_IN,NPAR_VEG_IRR_USE,IP_I)
!      ELSE
!        IP_I = KNP_IN
!      ENDIF
      !
      ! if VEG is in JP
      IF (IP_O==JP) THEN
        ! the input patch takes the contribution of the output patch
        DO JI = 1,SIZE(IMASK_IN,1)
          ! the mask is this of the current output patch
          IMASK = IMASK_IN(JI,IP_O)
          IF (IMASK/=0) ZPATCH_OUT(IMASK,IP_I) = ZPATCH_OUT(IMASK,IP_I) + PPATCH_IN(JI,IP_O)
        ENDDO
        ! just one time if several vegtypes are in this patch JP
        EXIT
      ENDIF
    ENDDO
  ENDDO
  !
! more patches before than after
ELSEIF (KNP_IN>KNP_OUT) THEN
  !
  ! for each input patch, what is the corresponding output patch? 
  DO JP = 1,KNP_IN
    ! which vegtype is in this output patch? 
    DO JVEG = 1,NVEGTYPE+NVEG_IRR
      ! input patch in which is this vegtype
      CALL VEGTYPE_TO_PATCH_IRRIG(JVEG,KNP_IN,NPAR_VEG_IRR_USE,IP_I)
      ! output patch in which is this vegtype
!      IF ( KNP_OUT /= 1 ) THEN
      CALL VEGTYPE_TO_PATCH_IRRIG(JVEG,KNP_OUT,NPAR_VEG_IRR_USE,IP_O)
!      ELSE
!        IP_O = KNP_OUT
!      ENDIF
      !
      ! if VEG is in JP
      IF (IP_I==JP) THEN
        ! this input patch gets the value of the corresponding output patch
        DO JI = 1,SIZE(IMASK_IN,1)
          IMASK = IMASK_IN(JI,IP_O)
          IF (IMASK/=0) ZPATCH_OUT(IMASK,IP_I) = PPATCH_IN(JI,IP_O)
        ENDDO
        ! just one time because another vegtype of IP_I will be also in IP_O
        EXIT
      ENDIF
    ENDDO
  ENDDO
  !
ENDIF
!
PPATCH_OUT(:,:) = ZPATCH_OUT(:,:)
!
IF (LHOOK) CALL DR_HOOK('GET_PREP_INTERP',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_PREP_INTERP
