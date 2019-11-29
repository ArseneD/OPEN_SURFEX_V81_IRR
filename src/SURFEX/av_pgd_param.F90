!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################################################################
      SUBROUTINE AV_PGD_PARAM (PLAI_IN, PVEG_IN, &
                               PFIELD,PVEGTYPE,PDATA,NPAR_VEG_IRR_USE,HSFTYPE,HATYPE,KMASK,KNPATCH,KPATCH,PDZ,KDECADE)
!     ################################################################
!
!!**** *AV_PATCH_PGD* average for each surface patch a secondary physiographic 
!!                    variable from the
!!              fractions of coverage class.
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!    The averaging is performed with one way into three:
!!
!!    - arithmetic averaging (HATYPE='ARI')
!!
!!    - inverse    averaging (HATYPE='INV')
!!
!!    - inverse of square logarithm averaging (HATYPE='CDN') :
!!
!!      1 / ( ln (dz/data) )**2
!!
!!      This latest uses (if available) the height of the first model mass
!!      level. In the other case, 20m is chosen. It works for roughness lengths.
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    F.Solmon /V. Masson       
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    15/12/97
!!    V. Masson   01/2004  Externalization
!!    R. Alkama   04/2012  add 6 new tree vegtype (9 instead 3)
!!    A. Druel    02/2019  Compatibility with new irrigation (duplication of patches) and add MA1 and ARV possibility (without taking into account the zeros)
!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD, NVT_TEBE,  &
                                NVT_TENE, NVT_BOBD, NVT_BOND, NVT_SHRB, NVEGTYPE,  &
                                XCDREF
USE MODD_AGRI,           ONLY : NVEG_IRR
!
USE MODI_VEGTYPE_TO_PATCH_IRRIG 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
REAL, DIMENSION(:,:,:), INTENT(IN) :: PLAI_IN
REAL, DIMENSION(:,:,:), INTENT(IN) :: PVEG_IN
!
REAL, DIMENSION(:),   INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:), INTENT(IN)  :: PVEGTYPE  ! fraction of each cover class
REAL, DIMENSION(:,:), INTENT(IN)  :: PDATA   ! secondary field value for each class
INTEGER,DIMENSION(:), INTENT(IN)  :: NPAR_VEG_IRR_USE ! vegtype with irrigation
 CHARACTER(LEN=3),     INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),     INTENT(IN) :: HATYPE  ! Type of averaging
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
INTEGER, INTENT(IN) :: KNPATCH
INTEGER, INTENT(IN) :: KPATCH
REAL, DIMENSION(:),   INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,              INTENT(IN), OPTIONAL :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
! nbe of vegtype
! nbre of patches
INTEGER :: JV! loop on vegtype
INTEGER :: JJ, JI, JP, JK, IMASK
!
REAL, DIMENSION(SIZE(PFIELD,1),NVEGTYPE+NVEG_IRR)  :: ZWEIGHT
!
REAL, DIMENSION(SIZE(PFIELD,1))   :: ZSUM_WEIGHT_PATCH
!
REAL, DIMENSION(SIZE(PFIELD,1))   :: ZWORK
REAL, DIMENSION(SIZE(PFIELD,1))   :: ZDZ
!
REAL, DIMENSION(0:31) :: ZCOUNT       !! ! Take care: MAJ or MA1 are valable only from 0 to 31.
INTEGER, DIMENSION(SIZE(PFIELD,1))  :: NMASK
INTEGER ::  PATCH_LIST(NVEGTYPE+NVEG_IRR)
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('AV_PGD_PARAM',0,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0 .AND. LHOOK) CALL DR_HOOK('AV_PGD_PARAM',1,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
!
IF (PRESENT(PDZ)) THEN
  ZDZ(:)=PDZ(:)
ELSE
  ZDZ(:)=XCDREF
END IF
!
PFIELD(:)=XUNDEF
!
ZWORK(:)=0.
ZWEIGHT(:,:)=0.
ZSUM_WEIGHT_PATCH(:)=0.
!
DO JV=1,NVEGTYPE+NVEG_IRR
  CALL VEGTYPE_TO_PATCH_IRRIG(JV,KNPATCH,NPAR_VEG_IRR_USE,PATCH_LIST(JV))
ENDDO

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!*    2.     Selection of the weighting function for vegtype
!            -----------------------------------
!
DO JV=1,NVEGTYPE+NVEG_IRR
  JP= PATCH_LIST(JV)
  IF (JP/=KPATCH) CYCLE
  !
  JK = JV
  IF (JV > NVEGTYPE) JK = NPAR_VEG_IRR_USE( JV - NVEGTYPE )
  !
  DO JI=1,SIZE(PFIELD)
    IMASK = KMASK(JI)
    
    IF (HSFTYPE=='NAT'.OR.HSFTYPE=='GRD') THEN
      ZWEIGHT(JI,JV) = PVEGTYPE(IMASK,JV)
    ELSEIF (HSFTYPE=='VEG'.OR.HSFTYPE=='GRV') THEN
      ZWEIGHT(JI,JV) = PVEGTYPE(IMASK,JV)*PVEG_IN(IMASK,KDECADE,JK)     ! PVEG_IN with JK: what is it ?
    ELSEIF (HSFTYPE=='BAR'.OR.HSFTYPE=='GRB') THEN
      ZWEIGHT(JI,JV)=PVEGTYPE(IMASK,JV)*(1.-PVEG_IN(IMASK,KDECADE,JK))  ! PVEG_IN with JK: what is it ?
    ELSEIF (HSFTYPE=='DVG'.OR.HSFTYPE=='GDV') THEN
      IF (SUM(PLAI_IN(JI,:,JK)).GT.0.) ZWEIGHT(JI,JV) = PVEGTYPE(IMASK,JV)
    ELSEIF (HSFTYPE=='LAI'.OR.HSFTYPE=='GRL') THEN
      IF (JV>=4) ZWEIGHT(JI,JV)=PVEGTYPE(IMASK,JV)*PLAI_IN(IMASK,KDECADE,JK)
    ELSEIF (HSFTYPE=='TRE'.OR.HSFTYPE=='GRT') THEN
      IF (JK==NVT_TEBD.OR.JK==NVT_BONE.OR.JK==NVT_TRBE.OR.JK==NVT_TRBD.OR.&
          JK==NVT_TEBE.OR.JK==NVT_TENE.OR.JK==NVT_BOBD.OR.JK==NVT_BOND.OR.&
          JK==NVT_SHRB) ZWEIGHT(JI,JV) = PVEGTYPE(IMASK,JV)
    ELSE
      CALL ABOR1_SFX('AV_PGD_PARAM_1D: WEIGHTING FUNCTION FOR VEGTYPE NOT ALLOWED')
    ENDIF

  ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
!
!*    3.     Averaging
!            ---------
!
!*    3.1    Work arrays given for each patch
!            -----------
!
!*    3.2    Selection of averaging type
!            ---------------------------
!
SELECT CASE (HATYPE)
!
!-------------------------------------------------------------------------------
!
!*    3.3    Arithmetic averaging
!            --------------------
!
  CASE ('ARI', 'ARV')
!
    DO JV=1,NVEGTYPE+NVEG_IRR
      JP= PATCH_LIST(JV)
      IF (JP/=KPATCH) CYCLE
      !
      JK = JV
      IF (JV > NVEGTYPE) JK = NPAR_VEG_IRR_USE( JV - NVEGTYPE )
      !
      DO JJ=1,SIZE(PFIELD)
        IMASK = KMASK(JJ)
        ZSUM_WEIGHT_PATCH(JJ) = ZSUM_WEIGHT_PATCH(JJ) + ZWEIGHT(JJ,JV)
        ZWORK(JJ) =  ZWORK(JJ) + PDATA(IMASK,JK)  * ZWEIGHT(JJ,JV)
      ENDDO
    END DO
!
!-------------------------------------------------------------------------------
!
!*    3.4    Inverse averaging
!            -----------------
!
  CASE('INV' )
!
    DO JV=1,NVEGTYPE+NVEG_IRR 
      JP=PATCH_LIST(JV) 
      IF (JP/=KPATCH) CYCLE
      !
      JK = JV
      IF (JV > NVEGTYPE) JK = NPAR_VEG_IRR_USE( JV - NVEGTYPE )
      !
      DO JJ=1,SIZE(PFIELD)
        IMASK = KMASK(JJ)     
        ZSUM_WEIGHT_PATCH(JJ) = ZSUM_WEIGHT_PATCH(JJ)+ZWEIGHT(JJ,JV)
        IF (PDATA(IMASK,JK).NE.0.) THEN
          ZWORK(JJ)= ZWORK(JJ) + 1./ PDATA(IMASK,JK) * ZWEIGHT(JJ,JV)
        ENDIF
      ENDDO
    END DO
!
!-------------------------------------------------------------------------------!
!
!*    3.5    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    DO JV=1,NVEGTYPE+NVEG_IRR
      JP=PATCH_LIST(JV)
      IF (JP/=KPATCH) CYCLE
      !
      JK = JV
      IF (JV > NVEGTYPE) JK = NPAR_VEG_IRR_USE( JV - NVEGTYPE )
      !
      DO JJ=1,SIZE(PFIELD)
        IMASK = KMASK(JJ)        
        ZSUM_WEIGHT_PATCH(JJ) =  ZSUM_WEIGHT_PATCH(JJ)+ ZWEIGHT(JJ,JV)
        IF (PDATA(JJ,JK).NE.0.) THEN
          ZWORK(JJ)= ZWORK(JJ) + 1./(LOG(ZDZ(JJ)/ PDATA(IMASK,JK)))**2    &
                            * ZWEIGHT(JJ,JV)
        ENDIF
      ENDDO
    END DO   
!
  CASE ('MAJ', 'MA1')
!
    ZWORK(:) = 0.
    DO JJ=1,SIZE(PFIELD)
      ZCOUNT(:) = 0.
      DO JV=1,NVEGTYPE+NVEG_IRR
        JP= PATCH_LIST(JV)
        IF (JP/=KPATCH) CYCLE
        !
        JK = JV
        IF (JV > NVEGTYPE) JK = NPAR_VEG_IRR_USE( JV - NVEGTYPE )
        !
        IMASK = KMASK(JJ)
        IF (PDATA(IMASK,JK)/=XUNDEF) THEN
          IF (NINT(PDATA(IMASK,JK))/=NUNDEF) &
                ZCOUNT(NINT(PDATA(IMASK,JK))) = ZCOUNT(NINT(PDATA(IMASK,JK))) + ZWEIGHT(JJ,JV)
        ENDIF
      ENDDO
      IF ( HATYPE == 'MA1' ) ZCOUNT(0)=0 ! don't take into account the "0" value
      IF (ALL(ZCOUNT(:)==0.)) THEN
        ZWORK(JJ) = NUNDEF
      ELSE
        ZWORK(JJ) = FLOAT(MAXLOC(ZCOUNT,1)-1)
      ENDIF
    END DO
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PGD_PARAM_1D: (1) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
!
!*    4.1    Selection of averaging type
!            ---------------------------
!
SELECT CASE (HATYPE)
!
!-------------------------------------------------------------------------------
!
!*    4.2    Arithmetic averaging
!            --------------------
!
  CASE ('ARI', 'ARV')
!   
    DO JI=1,SIZE(PFIELD)
      IF (ZSUM_WEIGHT_PATCH(JI)>0.) PFIELD(JI) = ZWORK(JI) / ZSUM_WEIGHT_PATCH(JI)
    ENDDO
!
!-------------------------------------------------------------------------------
!
!*    4.3    Inverse averaging
!            -----------------
!
  CASE('INV' )
!
    DO JI=1,SIZE(PFIELD)
      IF (ZSUM_WEIGHT_PATCH(JI)>0.) PFIELD(JI) = ZSUM_WEIGHT_PATCH(JI) / ZWORK(JI)
    ENDDO
!-------------------------------------------------------------------------------!
!
!*    4.4    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    DO JI=1,SIZE(PFIELD)
      IF (ZSUM_WEIGHT_PATCH(JI)>0.) THEN
        PFIELD(JI) = ZDZ(JI) * EXP( - SQRT(ZSUM_WEIGHT_PATCH(JI)/ZWORK(JI)) )
      ENDIF
    ENDDO
!
  CASE ('MAJ', 'MA1')
!
    DO JI=1,SIZE(PFIELD)
      PFIELD(JI) = ZWORK(JI)
    ENDDO
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PGD_PARAM: (2) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
IF (LHOOK) CALL DR_HOOK('AV_PGD_PARAM',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!   
END SUBROUTINE AV_PGD_PARAM
