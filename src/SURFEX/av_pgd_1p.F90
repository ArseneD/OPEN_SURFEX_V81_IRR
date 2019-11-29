!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODI_AV_PGD_1P
!     ##################
INTERFACE AV_PGD_1P
!
      SUBROUTINE AV_PGD_1D_1P (DTCO, &
                            PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,OCOVER,KMASK,KPATCH,KNPATCH,PDZ,KDECADE)
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
REAL, DIMENSION(:),     INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:),   INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:),     INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),       INTENT(IN) :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN) :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:),  INTENT(IN)  :: OCOVER
INTEGER, DIMENSION(:), INTENT(IN)   :: KMASK
INTEGER, INTENT(IN) :: KNPATCH
INTEGER, INTENT(IN) :: KPATCH
REAL, DIMENSION(:),     INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
END SUBROUTINE AV_PGD_1D_1P
!     ################################################################
      SUBROUTINE AV_PATCH_PGD_1D_1P (DTCO, &
                                 PFIELD,PCOVER,PDATA,PIRRIG,NPAR_VEG_IRR_USE,HSFTYPE,HATYPE,OCOVER,KMASK,KNPATCH,KPATCH,PDZ,KDECADE)
!     ################################################################
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
REAL, DIMENSION(:), INTENT(OUT)   :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:,:), INTENT(IN)  :: PDATA   ! secondary field value for each class
REAL, DIMENSION(:,:), INTENT(IN)  :: PIRRIG  ! fraction of irrigation for each vegtype
INTEGER,DIMENSION(:), INTENT(IN)  :: NPAR_VEG_IRR_USE ! vegtype with irrigation
 CHARACTER(LEN=3),     INTENT(IN) :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),     INTENT(IN) :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:), INTENT(IN) :: OCOVER
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
INTEGER, INTENT(IN) :: KNPATCH
INTEGER, INTENT(IN) :: KPATCH
REAL, DIMENSION(:),   INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,              INTENT(IN), OPTIONAL :: KDECADE ! current month
!
END SUBROUTINE AV_PATCH_PGD_1D_1P
!
!     ################################################################
      SUBROUTINE MAJOR_PATCH_PGD_1D_1P(TFIELD,PCOVER,TDATA,PIRRIG,NPAR_VEG_IRR_USE,HSFTYPE,HATYPE,&
                      OCOVER,KMASK,KNPATCH,KPATCH,KDECADE)
!     ################################################################
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_TYPE_DATE_SURF
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE (DATE_TIME), DIMENSION(:), INTENT(OUT)  :: TFIELD  ! secondary field to construct
REAL, DIMENSION(:,:), INTENT(IN)             :: PCOVER  ! fraction of each cover class
TYPE (DATE_TIME), DIMENSION(:,:), INTENT(IN) :: TDATA   ! secondary field value for each class
REAL, DIMENSION(:,:), INTENT(IN)  :: PIRRIG  ! fraction of irrigation for each vegtype
INTEGER,DIMENSION(:), INTENT(IN)  :: NPAR_VEG_IRR_USE ! vegtype with irrigation
 CHARACTER(LEN=3),     INTENT(IN) :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),     INTENT(IN) :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:), INTENT(IN) :: OCOVER
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
INTEGER, INTENT(IN) :: KNPATCH
INTEGER, INTENT(IN) :: KPATCH
INTEGER,     INTENT(IN), OPTIONAL :: KDECADE ! current month
!
END SUBROUTINE MAJOR_PATCH_PGD_1D_1P
!
END INTERFACE
END MODULE MODI_AV_PGD_1P
!
!     ################################################################
      SUBROUTINE AV_PGD_1D_1P (DTCO, PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,OCOVER,&
                               KMASK, KPATCH, KNPATCH, PDZ, KDECADE)
!     ################################################################
!
!!**** *AV_PGD* average a secondary physiographic variable from the
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
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!
!     F.Solmon patch modif: remove the case 'veg' as veg is defined for patches 
!
!!    Original    15/12/97
!!    V. Masson   01/2004  Externalization
!!    R. Alkama   05/2012  Add 6 tree vegtypes (9 rather than 3)
!!    A. Druel    02/2019  Compatibility with new irrigation (duplication of patches) and add MA1 and ARV possibility (without taking into account the zeros)
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_PAR, ONLY : XCDREF
!
USE MODD_DATA_COVER_n, ONLY   : DATA_COVER_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODE_AV_PGD
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
TYPE(DATA_COVER_t), INTENT(INOUT)   :: DTCO
!
REAL, DIMENSION(:),     INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:),   INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:),     INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),       INTENT(IN) :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN) :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:),  INTENT(IN)  :: OCOVER
INTEGER, DIMENSION(:), INTENT(IN)   :: KMASK
INTEGER, INTENT(IN) :: KNPATCH
INTEGER, INTENT(IN) :: KPATCH
REAL, DIMENSION(:),     INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: JJ, JI, ID0, IMASK0
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
INTEGER, DIMENSION(SIZE(PCOVER,2)) :: IMASK
REAL, DIMENSION(SIZE(PFIELD))      :: ZWORK, ZDZ, ZVAL
REAL, DIMENSION(SIZE(PCOVER,2))    :: ZWEIGHT, ZWEIGHT_TMP
REAL                               :: ZCOVER_WEIGHT
REAL, DIMENSION(SIZE(PFIELD))      :: ZSUM_COVER_WEIGHT
REAL(KIND=JPRB)                    :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD_1P:AV_PGD_1D_1P',0,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD_1P:AV_PGD_1D_1P',1,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
ICOVER=SIZE(PCOVER,2)
!
IF (PRESENT(PDZ)) THEN
  ZDZ(:)=PDZ(:)
ELSE
  ZDZ(:)=XCDREF
END IF
!
PFIELD(:)=XUNDEF
IF (HSFTYPE=='TRE' .OR. HSFTYPE=='GRT') PFIELD(:) = 0.
!
ZWORK(:)=0.
ZSUM_COVER_WEIGHT(:)=0.
!
JCOVER = 0
DO JJ = 1,SIZE(OCOVER)
  IF (OCOVER(JJ)) THEN
    JCOVER=JCOVER+1
    IMASK(JCOVER) = JJ
  ENDIF
ENDDO
!
 CALL GET_WEIGHT(DTCO,ICOVER,IMASK,HSFTYPE,ZWEIGHT)
!
!-------------------------------------------------------------------------------
!
!*    3.     Averaging
!            ---------
!
!*    3.1    Work arrays
!            -----------
!
IF (HATYPE=='ARI' .OR. HATYPE=='ARV' .OR. HATYPE=='INV' .OR. HATYPE=='CDN') THEN
  !
  DO JCOVER=1,ICOVER
    IF (ZWEIGHT(JCOVER)/=0.) THEN
      !
      JJ = IMASK(JCOVER)
      !
      IF (HATYPE=='ARI' .OR. HATYPE=='ARV') THEN
        ZVAL(:) = PDATA(JJ)
      ELSEIF (HATYPE=='INV') THEN
        ZVAL(:) = 1./PDATA(JJ)
      ELSEIF (HATYPE=='CDN') THEN
        ZVAL(:) = 1./(LOG(ZDZ(:)/PDATA(JJ)))**2 
      ENDIF
      !
      DO JI = 1,SIZE(KMASK)
        !
        IMASK0 = KMASK(JI)
        !
        IF (PCOVER(IMASK0,JCOVER)/=0.) THEN
          ZCOVER_WEIGHT = PCOVER(IMASK0,JCOVER) * ZWEIGHT(JCOVER)
          ZSUM_COVER_WEIGHT(JI) = ZSUM_COVER_WEIGHT(JI) + ZCOVER_WEIGHT
          ZWORK(JI) = ZWORK(JI) + ZVAL(JI) * ZCOVER_WEIGHT
        ENDIF
        !
      ENDDO
      !
    ENDIF
  ENDDO
ELSEIF (HATYPE=='MAJ' .OR. HATYPE=='MA1') THEN
  !
  DO JI = 1,SIZE(KMASK)
    !
    IMASK0 = KMASK(JI)
    !
    ID0 = MAXLOC(PCOVER(IMASK0,:)*ZWEIGHT(:),1)
    IF ( HATYPE=='MA1' .AND. PDATA(IMASK(ID0))==0. ) THEN ! tested ?
      ZWEIGHT_TMP(:)=ZWEIGHT(:)
      ZWEIGHT_TMP(ID0)=0.
      ID0 = MAXLOC(PCOVER(IMASK0,:)*ZWEIGHT_TMP(:),1)
    ENDIF
    ZWORK(JI) = PDATA(IMASK(ID0))
    ZSUM_COVER_WEIGHT(JI) = 1.
    !
  ENDDO
  !
ELSE
  CALL ABOR1_SFX('AV_PGD_1D_1P: (1) AVERAGING TYPE NOT ALLOWED : "'//HATYPE//'"')
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD_1P:AV_PGD_1D_1P',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
!*    4.     End of Averaging
!            ----------------
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
    WHERE ( ZSUM_COVER_WEIGHT(:) >0. )
      PFIELD(:) = ZWORK(:) / ZSUM_COVER_WEIGHT(:)
    END WHERE
!
!-------------------------------------------------------------------------------
!
!*    4.3    Inverse averaging
!            -----------------
!
  CASE('INV' )
!
    WHERE ( ZSUM_COVER_WEIGHT(:) >0. )
      PFIELD(:) = ZSUM_COVER_WEIGHT(:) / ZWORK(:)
    END WHERE
!
!-------------------------------------------------------------------------------!
!
!*    4.4    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    WHERE ( ZSUM_COVER_WEIGHT(:) >0. )
      PFIELD(:) = ZDZ(:) * EXP( - SQRT(ZSUM_COVER_WEIGHT(:)/ZWORK(:)) )
    END WHERE
!
!-------------------------------------------------------------------------------
!
!*    4.4    Majoritary averaging
!            --------------------
!
  CASE('MAJ','MA1')
!
    WHERE ( ZSUM_COVER_WEIGHT(:) >0. )
      PFIELD(:) = ZWORK(:)
    END WHERE
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PGD_1D_1P: (2) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD_1P:AV_PGD_1D_1P_4',1,ZHOOK_HANDLE)
! 
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AV_PGD_1D_1P
!
!     ################################################################
      SUBROUTINE AV_PATCH_PGD_1D_1P (DTCO, PFIELD,PCOVER,PDATA,PIRRIG,NPAR_VEG_IRR_USE,HSFTYPE,HATYPE,OCOVER,KMASK,&
                                     KNPATCH,KPATCH,PDZ,KDECADE)
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
!!    R. Alkama   05/2012  Add 6 tree vegtypes (9 rather than 3)
!!    A. Druel    02/2019  Compatibility with new irrigation (duplication of patches) and add MA1 and ARV possibility (without taking into account the zeros)
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_DATA_COVER_n,   ONLY : DATA_COVER_t
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE, XCDREF
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_AGRI,           ONLY : NVEG_IRR
!
USE MODE_AV_PGD
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
REAL, DIMENSION(:), INTENT(OUT)   :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:,:), INTENT(IN)  :: PDATA   ! secondary field value for each class
REAL, DIMENSION(:,:), INTENT(IN)  :: PIRRIG  ! fraction of irrigation for each vegtype
INTEGER,DIMENSION(:), INTENT(IN)  :: NPAR_VEG_IRR_USE ! vegtype with irrigation
 CHARACTER(LEN=3),     INTENT(IN) :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),     INTENT(IN) :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:), INTENT(IN) :: OCOVER
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
INTEGER, INTENT(IN) :: KNPATCH
INTEGER, INTENT(IN) :: KPATCH
REAL, DIMENSION(:),   INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,              INTENT(IN), OPTIONAL :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
! nbe of vegtype
! nbre of patches
INTEGER :: JVEG! loop on vegtype
INTEGER :: JJ, JI, JK
!
REAL    :: ZCOVER_WEIGHT
!
REAL, DIMENSION(SIZE(PFIELD))   :: ZVAL
!
REAL, DIMENSION(SIZE(PCOVER,2),NVEGTYPE) :: ZWEIGHT
!
REAL, DIMENSION(SIZE(PFIELD))   :: ZSUM_COVER_WEIGHT_PATCH
!
REAL, DIMENSION(SIZE(PFIELD))   :: ZWORK
REAL, DIMENSION(SIZE(PFIELD))   :: ZDZ
REAL, DIMENSION(SIZE(PIRRIG,1)) :: ZXIRRIG
!
REAL                               :: ZMAX0, ZMAX1, ZXIRRIG2
INTEGER                            :: IMASK, JP, ID0, JVEG0, ID
INTEGER, DIMENSION(SIZE(PCOVER,2)) :: IMASK0
INTEGER                            ::  PATCH_LIST(NVEGTYPE+NVEG_IRR)
REAL(KIND=JPRB)                    :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP

!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD_1P:AV_PATCH_PGD_1D_1P',0,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD_1P:AV_PATCH_PGD_1D_1P',1,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
ICOVER=SIZE(PCOVER,2)
!
IF (PRESENT(PDZ)) THEN
  ZDZ(:)=PDZ(:)
ELSE
  ZDZ(:)=XCDREF
END IF
!
PFIELD(:)=XUNDEF
!
ZWORK(:) = 0.
ZWEIGHT(:,:) = 0.0
ZSUM_COVER_WEIGHT_PATCH(:) = 0.
!
DO JVEG=1,NVEGTYPE+NVEG_IRR
  CALL VEGTYPE_TO_PATCH_IRRIG(JVEG,KNPATCH,NPAR_VEG_IRR_USE,PATCH_LIST(JVEG))
ENDDO
!
JCOVER = 0
DO JJ = 1,SIZE(OCOVER)
  IF (OCOVER(JJ)) THEN
    JCOVER=JCOVER+1
    IMASK0(JCOVER) = JJ
  ENDIF
ENDDO
!
 CALL GET_WEIGHT_PATCH(DTCO,ICOVER,IMASK0,KDECADE,HSFTYPE,ZWEIGHT)
!
!-------------------------------------------------------------------------------
  !
  !
  !*    2.     Selection of the weighting function for vegtype
  !            -----------------------------------
  !
JCOVER=0
!
IF (HATYPE=='ARI' .OR. HATYPE=='ARV' .OR. HATYPE=='INV' .OR. HATYPE=='CDN') THEN
  !
  DO JCOVER=1,ICOVER
    !
    JJ = IMASK0(JCOVER)
    !
    DO JVEG=1,NVEGTYPE+NVEG_IRR
      !
      JP= PATCH_LIST(JVEG)
      IF (JP/=KPATCH) CYCLE
      !
      JK = JVEG
      IF (JVEG > NVEGTYPE) JK = NPAR_VEG_IRR_USE( JVEG - NVEGTYPE )
      !
      IF (ZWEIGHT(JCOVER,JK)/=0.) THEN
        !
        IF (HATYPE=='ARI' .OR. HATYPE=='ARV') THEN
          ZVAL(:) = PDATA(JJ,JK)
        ELSEIF (HATYPE=='INV') THEN
          ZVAL(:) = 1. / PDATA(JJ,JK)
        ELSEIF (HATYPE=='CDN') THEN
          DO JI=1,SIZE(ZVAL)
            ZVAL(JI) = 1./(LOG(ZDZ(JI)/PDATA(JJ,JK)))**2 
          ENDDO
        ENDIF
        !
        IF (  NVEG_IRR /= 0 ) THEN
          ZXIRRIG(:) = 0.
          WHERE ( PIRRIG(:,JK) /= XUNDEF ) ZXIRRIG(:) = PIRRIG(:,JK)
        ENDIF
        !
        DO JI=1,SIZE(PFIELD)

          IMASK = KMASK(JI)

          IF (PCOVER(IMASK,JCOVER)/=0.) THEN
            !
            IF ( NVEG_IRR == 0 ) THEN
              ! case without irrigation
              ZCOVER_WEIGHT =  PCOVER(IMASK,JCOVER) * ZWEIGHT(JCOVER,JK)
            ELSEIF ( JVEG <= NVEGTYPE .AND. ZXIRRIG(IMASK) == 0. ) THEN  ! JI or IMASK?
              ! case for a vegtype non irrigated or if it is not irrigation for this vegtype in this point
              ZCOVER_WEIGHT =  PCOVER(IMASK,JCOVER) * ZWEIGHT(JCOVER,JK)
            ELSEIF ( JVEG <= NVEGTYPE ) THEN
              ! case the irrigation patch have to be taken into account and a fraction of the vegtype is irrigated (not this part)
              ZCOVER_WEIGHT =  PCOVER(IMASK,JCOVER) * ( 1 - ZXIRRIG(IMASK) ) * ZWEIGHT(JCOVER,JK)
            ELSE
              ! case the irrigation patch have to be taken into account and a fraction of the vegtype is irrigated (this part)
              ZCOVER_WEIGHT =  PCOVER(IMASK,JCOVER) * ZXIRRIG(IMASK) * ZWEIGHT(JCOVER,JK)
            ENDIF
            !
            !ZCOVER_WEIGHT =  PCOVER(IMASK,JCOVER) * ZWEIGHT(JCOVER,JK)      
            ZSUM_COVER_WEIGHT_PATCH(JI) = ZSUM_COVER_WEIGHT_PATCH(JI) + ZCOVER_WEIGHT
            ZWORK(JI) = ZWORK(JI) + ZVAL(JI) * ZCOVER_WEIGHT
          ENDIF
        ENDDO
        !
      ENDIF
      !
    ENDDO
    !
  ENDDO
ELSEIF (HATYPE=='MAJ' .OR. HATYPE=='MA1') THEN
  !
  DO JI = 1,SIZE(KMASK)
    !
    IMASK = KMASK(JI)
    !
    ZMAX0 = 0.
    JVEG0 = 0
    !
    DO JVEG = 1,NVEGTYPE+NVEG_IRR
      !
      JP = PATCH_LIST(JVEG)
      IF (JP/=KPATCH) CYCLE
      !
      JK = JVEG
      IF (JVEG > NVEGTYPE) JK = NPAR_VEG_IRR_USE( JVEG - NVEGTYPE )
      !
      !ZMAX1 = MAXVAL(PCOVER(IMASK,:)*ZWEIGHT(:,JK))
      ZMAX1 = 0.
      ID = 0
      ZXIRRIG2 = 0.
      IF (  NVEG_IRR /= 0 .AND. PIRRIG(IMASK,JK) /= XUNDEF ) ZXIRRIG2 = PIRRIG(IMASK,JK)
      !  
      DO JCOVER=1,ICOVER
        !
        IF ( JVEG <= NVEGTYPE .AND. ZXIRRIG2 == 0. ) THEN  ! JI or IMASK?
          ! case (1) without irrigation, or (2) for a vegtype non irrigated or if it is not irrigation for this vegtype in this point
          IF ( ZMAX1 <= PCOVER(IMASK,JCOVER) * ZWEIGHT(JCOVER,JK)                   &
                  .AND. ( HATYPE=='MAJ' .OR. (PDATA(IMASK0(JCOVER),JK)/=0..AND.PDATA(IMASK0(JCOVER),JK)/=XUNDEF) ) ) THEN
            ZMAX1 = PCOVER(IMASK,JCOVER) * ZWEIGHT(JCOVER,JK)
            ZCOVER_WEIGHT =  PCOVER(IMASK,JCOVER) * ZWEIGHT(JCOVER,JK)
            ID = JCOVER
          ENDIF
          !
        ELSEIF ( JVEG <= NVEGTYPE ) THEN
          ! case the irrigation patch have to be taken into account and a fraction of the vegtype is irrigated (not this part)
          IF ( ZMAX1 <= PCOVER(IMASK,JCOVER) * ( 1 - ZXIRRIG2 ) * ZWEIGHT(JCOVER,JK) &
                  .AND. ( HATYPE=='MAJ' .OR. (PDATA(IMASK0(JCOVER),JK)/=0..AND.PDATA(IMASK0(JCOVER),JK)/=XUNDEF) ) ) THEN
            ZMAX1 = PCOVER(IMASK,JCOVER) * ( 1 - ZXIRRIG2 ) * ZWEIGHT(JCOVER,JK)
            ID = JCOVER
          ENDIF
          !
        ELSE
          ! case the irrigation patch have to be taken into account and a fraction of the vegtype is irrigated (this part)
          IF ( ZMAX1 <= PCOVER(IMASK,JCOVER) * ZXIRRIG2 * ZWEIGHT(JCOVER,JK)        &
                  .AND. ( HATYPE=='MAJ' .OR. (PDATA(IMASK0(JCOVER),JK)/=0..AND.PDATA(IMASK0(JCOVER),JK)/=XUNDEF) ) ) THEN
            ZMAX1 = PCOVER(IMASK,JCOVER) * ZXIRRIG2 * ZWEIGHT(JCOVER,JK)
            ID = JCOVER
          ENDIF
          !
        ENDIF
      ENDDO
      !
      IF ( ZMAX1>ZMAX0 ) THEN !.AND. ( HATYPE=='MAJ' .OR. PDATA(IMASK0(MAXVAL(MAXLOC(PCOVER(IMASK,:)*ZWEIGHT(:,JK)))),JK)/=0. ) ) THEN ! Tested ?
        !ID0 = MAXVAL(MAXLOC(PCOVER(IMASK,:)*ZWEIGHT(:,JK)))
        ID0 = ID
        ZMAX0 = ZMAX1
        JVEG0 = JK
      ENDIF
      !
    ENDDO
    !
    IF (JVEG0>0) THEN
      ZWORK(JI) = PDATA(IMASK0(ID0),JVEG0)
      ZSUM_COVER_WEIGHT_PATCH(JI) = 1.
    ENDIF
    !
  ENDDO
  !
ELSE
  CALL ABOR1_SFX('AV_PATCH_PGD_1D_1P: (1) AVERAGING TYPE NOT ALLOWED : "'//HATYPE//'"')
ENDIF  
!
!-------------------------------------------------------------------------------
  
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
  CASE ('ARI','ARV')
!   
    DO JI=1,SIZE(PFIELD)
      IF (ZSUM_COVER_WEIGHT_PATCH(JI)>0.) PFIELD(JI) =  ZWORK(JI) / ZSUM_COVER_WEIGHT_PATCH(JI)
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
      IF (ZSUM_COVER_WEIGHT_PATCH(JI)>0.) PFIELD(JI) = ZSUM_COVER_WEIGHT_PATCH(JI) / ZWORK(JI)
    ENDDO
!-------------------------------------------------------------------------------!
!
!*    4.4    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    DO JI=1,SIZE(PFIELD)
      IF (ZSUM_COVER_WEIGHT_PATCH(JI)>0.) PFIELD(JI) = ZDZ(JI) * EXP( - SQRT(ZSUM_COVER_WEIGHT_PATCH(JI)/ZWORK(JI)) )
    ENDDO
!
!-------------------------------------------------------------------------------
!
!*    4.4    Majoritary averaging
!            --------------------
!
  CASE('MAJ','MA1' )
!
    WHERE ( ZSUM_COVER_WEIGHT_PATCH(:) >0. )
      PFIELD(:) = ZWORK(:)
    END WHERE
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_1PATCH_PGD_1D_1P: (2) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD_1P:AV_1PATCH_PGD_1D_1P',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!   
END SUBROUTINE AV_PATCH_PGD_1D_1P
!
!     ################################################################
      SUBROUTINE MAJOR_PATCH_PGD_1D_1P(TFIELD,PCOVER,TDATA,PIRRIG,NPAR_VEG_IRR_USE,HSFTYPE,HATYPE,&
                      OCOVER,KMASK,KNPATCH,KPATCH,KDECADE)
!     ################################################################
!
!!**** *MAJOR_PATCH_PGD* find the dominant date for each vegetation type
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
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
!!    P. LE MOIGNE
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    06/2006
!!    A. Druel    02/2019  Compatibility with new irrigation (duplication of patches) and add MA1 and ARV possibility (without taking into account the zeros)
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_TYPE_DATE_SURF
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_AGRI,           ONLY : NVEG_IRR
!
USE MODI_VEGTYPE_TO_PATCH_IRRIG
USE MODE_AV_PGD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE (DATE_TIME), DIMENSION(:),   INTENT(OUT) :: TFIELD  ! secondary field to construct
REAL,             DIMENSION(:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
TYPE (DATE_TIME), DIMENSION(:,:), INTENT(IN)  :: TDATA   ! secondary field value for each class
REAL,             DIMENSION(:,:), INTENT(IN)  :: PIRRIG  ! fraction of irrigation for each vegtype
INTEGER,          DIMENSION(:),   INTENT(IN)  :: NPAR_VEG_IRR_USE ! vegtype with irrigation
 CHARACTER(LEN=3),                INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),                INTENT(IN)  :: HATYPE  ! Type of averaging
LOGICAL,          DIMENSION(:),   INTENT(IN)  :: OCOVER
INTEGER,          DIMENSION(:),   INTENT(IN)  :: KMASK
INTEGER,                          INTENT(IN)  :: KNPATCH
INTEGER,                          INTENT(IN)  :: KPATCH
INTEGER, INTENT(IN), OPTIONAL                 :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: JJ, IMASK
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
INTEGER :: JVEG, JK! loop on vegtype
!
REAL    :: ZXIRRIG
INTEGER, DIMENSION(SIZE(PCOVER,2),NVEGTYPE)      :: IDATA_DOY
INTEGER, DIMENSION(SIZE(PCOVER,1))               :: IDOY
REAL,    DIMENSION(365)                          :: ZCOUNT
INTEGER                                          :: JP, IMONTH, IDAY
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD_1P:MAJOR_PATCH_PGD_1D_1P',0,ZHOOK_HANDLE)
IF (SIZE(TFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD_1P:MAJOR_PATCH_PGD_1D_1P',1,ZHOOK_HANDLE)
IF (SIZE(TFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
TFIELD(:)%TDATE%YEAR  = NUNDEF
TFIELD(:)%TDATE%MONTH = NUNDEF
TFIELD(:)%TDATE%DAY   = NUNDEF
TFIELD(:)%TIME        = XUNDEF
!
IDOY(:) = 0
!
 CALL DATE2DOY(TDATA,OCOVER,IDATA_DOY)
!-------------------------------------------------------------------------------
DO JP = 1,SIZE(TFIELD)
  !
  IMASK = KMASK(JP)
  !
  ZCOUNT(:) = 0.
  !
  DO JVEG=1,NVEGTYPE+NVEG_IRR
    !
    CALL VEGTYPE_TO_PATCH_IRRIG(JVEG,KNPATCH,NPAR_VEG_IRR_USE,JJ)
    !
    JK = JVEG
    IF (JVEG > NVEGTYPE) JK = NPAR_VEG_IRR_USE( JVEG - NVEGTYPE )
    !
    IF( KPATCH == JJ ) THEN
      !
      ZXIRRIG = 0.
      IF (  NVEG_IRR /= 0 .AND. PIRRIG(IMASK,JK) /= XUNDEF ) ZXIRRIG = PIRRIG(IMASK,JK)
      !
      DO JCOVER = 1,SIZE(PCOVER,2)
        !
        IF (IDATA_DOY(JCOVER,JK) /= NUNDEF .AND. PCOVER(IMASK,JCOVER)/=0.) THEN
          !
          !ZCOUNT(IDATA_DOY(JCOVER,JK)) = ZCOUNT(IDATA_DOY(JCOVER,JK)) + PCOVER(IMASK,JCOVER)
          IF ( JVEG <= NVEGTYPE .AND. ZXIRRIG == 0. ) THEN
            ! case (1) without irrigation, or (2) for a vegtype non irrigated or if it is not irrigation for this vegtype in this point
            ZCOUNT(IDATA_DOY(JCOVER,JK)) = ZCOUNT(IDATA_DOY(JCOVER,JK)) + PCOVER(IMASK,JCOVER)
          ELSEIF ( JVEG <= NVEGTYPE ) THEN
            ! case the irrigation patch have to be taken into account and a fraction of the vegtype is irrigated (not this part)
            ZCOUNT(IDATA_DOY(JCOVER,JK)) = ZCOUNT(IDATA_DOY(JCOVER,JK)) + PCOVER(IMASK,JCOVER) * ( 1 - ZXIRRIG )
          ELSE
            ! case the irrigation patch have to be taken into account and a fraction of the vegtype is irrigated (this part)
            ZCOUNT(IDATA_DOY(JCOVER,JK)) = ZCOUNT(IDATA_DOY(JCOVER,JK)) + PCOVER(IMASK,JCOVER) * ZXIRRIG
          ENDIF
          !
        END IF
        !
      END DO
      !
    ENDIF
    !
  ENDDO
  !
  IDOY(JP) = 0
  IF (ANY(ZCOUNT(:)/=0.)) IDOY(JP) = MAXLOC(ZCOUNT,1)
  !
  CALL DOY2DATE(IDOY(JP),IMONTH,IDAY)
  !
  TFIELD(JP)%TDATE%MONTH = IMONTH
  TFIELD(JP)%TDATE%DAY   = IDAY
  IF (IMONTH/=NUNDEF) TFIELD(JP)%TIME   = 0.
  !
END DO
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD_1P:MAJOR_PATCH_PGD_1D_1P',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAJOR_PATCH_PGD_1D_1P
!
!-------------------------------------------------------------------------------
!
