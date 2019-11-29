!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!
! Description (Druel A., 2018): Les diverses routines av_pgd (ou non par patch) sont appellées un certains nombres de fois, 
!                                 mais la structure globale similaire cache des objectifs objectifs differents, et donc des
!                                 utilisations différents. 
!                               A cette date, les routines AV_PGD_2D et AV_PATCH_PGD ne sont plustuilisées.
!                               La routine hors patcht (AV_PGD_1D) est appelé par 8 routines (compute_isba_parameters.F90,
!                                 convert_cover_frac.F90, convert_patch_teb.F90, get_teb_depth.F90, init_teb_garden_pgdn.F90,
!                                 init_teb_greenroof_pgdn.F90, pgd_isba.F90 et pgd_isba_par).
!                               Les deux autres routines (avec patch: AV_PATCH_PGD_1D AND MAJOR_PATCH_PGD_1D) sont appellées
!                                 par 3 routinesi (modifiées pour l'irrigation), mais différements:
!                                 - Sans utiliser les patchs (mais les vegtypes) par extrapol_fields.F90 et init_isba_mixpar.F90
!                                 - En utilisant les patchs dans convert_cover_isba.F90 (appel modifié pour l'irrigation)
!                                 MAIS: dans les dimension "(:,:)", il n'y a pas de carte de végétation !!!! Il s'agit en réalité 
!                                       dans la première dimention de toutes les surfaces concidérées (ex. 33) qui vont être 
!                                       transformées en nveg ou npatch (ex. 20).
!
!     ##################
      MODULE MODI_AV_PGD
!     ##################
INTERFACE AV_PGD
!
!      SUBROUTINE AV_PGD_2D (DTCO, &                                                      ! JAMAIS APPELLEE
!                            PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,OCOVER,PDZ,KDECADE)       ! INUTILE ?
!      
!!
!USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!!
!REAL, DIMENSION(:,:),   INTENT(OUT) :: PFIELD  ! secondary field to construct
!REAL, DIMENSION(:,:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
!REAL, DIMENSION(:),     INTENT(IN)  :: PDATA   ! secondary field value for each class
! CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
!                                               ! is defined
! CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
!LOGICAL, DIMENSION(:), INTENT(IN) :: OCOVER
!REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL :: PDZ    ! first model half level
!INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!!
!END SUBROUTINE AV_PGD_2D
!!
!      SUBROUTINE AV_PATCH_PGD (DTCO, &                                                                            ! JAMAIS APPELLEE
!                               PFIELD,PCOVER,PDATA,NPAR_VEG_IRR_USE,PIRRIG,HSFTYPE,HATYPE,OCOVER,PDZ,KDECADE)     ! INUTILE ?
!      
!!
!USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!!
!REAL, DIMENSION(:,:,:), INTENT(OUT) :: PFIELD  ! secondary field to construct for each patch
!REAL, DIMENSION(:,:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
!REAL, DIMENSION(:,:),   INTENT(IN)  :: PDATA   ! secondary field value for each class in each vegtype
!INTEGER,DIMENSION(:),   INTENT(IN)  :: NPAR_VEG_IRR_USE ! vegtype with irrigation
!REAL,DIMENSION(:,:),    INTENT(IN)  :: PIRRIG  ! fraction of irrigation for each vegtype
! CHARACTER(LEN=3),       INTENT(IN) :: HSFTYPE ! Type of surface where the field
!                                               ! is defined
! CHARACTER(LEN=3),       INTENT(IN) :: HATYPE  ! Type of averaging
!LOGICAL, DIMENSION(:), INTENT(IN)   :: OCOVER
!REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL :: PDZ    ! first model half level
!INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!!
!END SUBROUTINE AV_PATCH_PGD
!
      SUBROUTINE AV_PGD_1D (DTCO, &
                            PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,OCOVER,PDZ,KDECADE)
      
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
REAL, DIMENSION(:),     INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:),   INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:),     INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
  LOGICAL, DIMENSION(:), INTENT(IN) :: OCOVER
REAL, DIMENSION(:),     INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
END SUBROUTINE AV_PGD_1D
!
      SUBROUTINE AV_PATCH_PGD_1D (DTCO, &
                                  PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,OCOVER,PDZ,KDECADE,NPAR_VEG_IRR_USE,PIRRIG)
      
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
REAL, DIMENSION(:,:),   INTENT(OUT) :: PFIELD  ! secondary field to construct for each patch
REAL, DIMENSION(:,:),   INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:,:),   INTENT(IN)  :: PDATA   ! secondary field value for each class in each vegtype
CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:), INTENT(IN) :: OCOVER
REAL, DIMENSION(:),     INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
INTEGER,DIMENSION(:),   INTENT(IN), OPTIONAL :: NPAR_VEG_IRR_USE ! vegtype with irrigation
REAL,DIMENSION(:,:),    INTENT(IN), OPTIONAL :: PIRRIG  ! fraction of irrigation for each vegtype
!
END SUBROUTINE AV_PATCH_PGD_1D
!
      SUBROUTINE MAJOR_PATCH_PGD_1D(TFIELD,PCOVER,TDATA,HSFTYPE,HATYPE,OCOVER,KDECADE,NPAR_VEG_IRR_USE,PIRRIG)
      
!
USE MODD_TYPE_DATE_SURF
TYPE (DATE_TIME), DIMENSION(:,:), INTENT(OUT) :: TFIELD  ! secondary field to construct for each patch
REAL, DIMENSION(:,:),  INTENT(IN)             :: PCOVER  ! fraction of each cover class
TYPE (DATE_TIME), DIMENSION(:,:), INTENT(IN)  :: TDATA   ! secondary field to construct for each patch
CHARACTER(LEN=3),      INTENT(IN)             :: HSFTYPE ! Type of surface where the field
                                                         ! is defined
CHARACTER(LEN=3),      INTENT(IN)             :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:), INTENT(IN)             :: OCOVER
INTEGER,               INTENT(IN), OPTIONAL   :: KDECADE ! current month
INTEGER,DIMENSION(:),  INTENT(IN), OPTIONAL   :: NPAR_VEG_IRR_USE ! vegtype with irrigation
REAL,DIMENSION(:,:),   INTENT(IN), OPTIONAL   :: PIRRIG  ! fraction of irrigation for each vegtype!
END SUBROUTINE MAJOR_PATCH_PGD_1D
!

!
END INTERFACE
END MODULE MODI_AV_PGD
!
!     ################################################################
      SUBROUTINE AV_PGD_1D (DTCO, &
                            PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,OCOVER,PDZ,KDECADE)
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
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODD_SURFEX_OMP, ONLY : NBLOCKTOT
USE MODD_SURFEX_MPI, ONLY : NRANK
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER,     ONLY : XDATA_BLD_HEIGHT 
USE MODD_DATA_COVER_PAR, ONLY : NVT_TEBD, NVT_BONE, NVT_TRBE, XCDREF, NVT_TRBD, &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND, NVT_SHRB
!            
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
REAL, DIMENSION(:),     INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:),   INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:),     INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:),  INTENT(IN) :: OCOVER
REAL, DIMENSION(:),     INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: JJ, JI, ID0
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
INTEGER :: ISIZE_OMP
INTEGER, DIMENSION(SIZE(PCOVER,2)) :: IMASK
REAL, DIMENSION(SIZE(PCOVER,1)) :: ZWORK, ZDZ, ZVAL
REAL, DIMENSION(SIZE(PCOVER,2)) :: ZWEIGHT, ZWEIGHT_TMP
REAL :: ZCOVER_WEIGHT
REAL, DIMENSION(SIZE(PCOVER,1)) :: ZSUM_COVER_WEIGHT
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP
!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D_1',0,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D_1',1,ZHOOK_HANDLE)
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
CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D_1',1,ZHOOK_HANDLE)
!
IF (HATYPE=='ARI' .OR. HATYPE=='ARV' .OR. HATYPE=='INV' .OR. HATYPE=='CDN') THEN
  !
  ISIZE_OMP = MAX(1,ICOVER/NBLOCKTOT)
!!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP) 
 CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D_2',0,ZHOOK_HANDLE_OMP)
!!$OMP DO SCHEDULE(STATIC,ISIZE_OMP) PRIVATE(JCOVER,JJ,ZVAL,JI,ZCOVER_WEIGHT) &
!!$OMP & REDUCTION(+:ZSUM_COVER_WEIGHT,ZWORK)
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
      DO JI = 1,SIZE(PCOVER,1)
        IF (PCOVER(JI,JCOVER)/=0.) THEN
          ZCOVER_WEIGHT = PCOVER(JI,JCOVER) * ZWEIGHT(JCOVER)
          ZSUM_COVER_WEIGHT(JI) = ZSUM_COVER_WEIGHT(JI) + ZCOVER_WEIGHT
          ZWORK(JI) = ZWORK(JI) + ZVAL(JI) * ZCOVER_WEIGHT
        ENDIF
      ENDDO
      !
    ENDIF
  ENDDO
!!$OMP END DO
 CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D_2',1,ZHOOK_HANDLE_OMP)
!!$OMP END PARALLEL
ELSEIF (HATYPE=='MAJ' .OR. HATYPE=='MA1') THEN
  !
!!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP)
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D_3',0,ZHOOK_HANDLE_OMP)
!!$OMP DO SCHEDULE(DYNAMIC,1) PRIVATE(JI,ID0)
  DO JI = 1,SIZE(PCOVER,1) 
    ID0 = MAXVAL(MAXLOC(PCOVER(JI,:)*ZWEIGHT(:)))
    IF ( HATYPE=='MA1' .AND. PDATA(IMASK(ID0))==0. ) THEN ! tested ?
      ZWEIGHT_TMP(:)=ZWEIGHT(:)
      ZWEIGHT_TMP(ID0)=0.
      ID0 = MAXVAL(MAXLOC(PCOVER(JI,:)*ZWEIGHT_TMP(:)))
    ENDIF
    ZWORK(JI) = PDATA(IMASK(ID0))
    ZSUM_COVER_WEIGHT(JI) = ZSUM_COVER_WEIGHT(JI) + SUM(PCOVER(JI,:)*ZWEIGHT(:))
  ENDDO
!!$OMP END DO
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D_3',1,ZHOOK_HANDLE_OMP)
!!$OMP END PARALLEL
  !
ELSE
  CALL ABOR1_SFX('AV_PGD_1D: (1) AVERAGING TYPE NOT ALLOWED : "'//HATYPE//'"')
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D_4',0,ZHOOK_HANDLE)
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
  CASE ('ARI','ARV')
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
  CASE('MAJ', 'MA1' )
!
    WHERE ( ZSUM_COVER_WEIGHT(:) >0. )
      PFIELD(:) = ZWORK(:)
    END WHERE
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PGD_1D: (2) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_1D_4',1,ZHOOK_HANDLE)
! 
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AV_PGD_1D
!
!     ################################################################
      SUBROUTINE AV_PATCH_PGD_1D (DTCO, &
                                  PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,OCOVER,PDZ,KDECADE,NPAR_VEG_IRR_USE,PIRRIG)
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
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER,     ONLY : XDATA_VEG, XDATA_LAI
USE MODD_DATA_COVER_PAR, ONLY : NVT_TEBD, NVT_BONE, NVT_TRBE, NVEGTYPE, XCDREF, NVT_TRBD, &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND, NVT_SHRB
USE MODD_AGRI,           ONLY : NVEG_IRR
!
USE MODI_VEGTYPE_TO_PATCH
USE MODI_VEGTYPE_TO_PATCH_IRRIG 
USE MODE_AV_PGD
!
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
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:,:), INTENT(IN)  :: PDATA   ! secondary field value for each class
CHARACTER(LEN=3),     INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),     INTENT(IN)  :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:), INTENT(IN)  :: OCOVER
REAL, DIMENSION(:),   INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,              INTENT(IN), OPTIONAL :: KDECADE ! current month
INTEGER,DIMENSION(:), INTENT(IN), OPTIONAL :: NPAR_VEG_IRR_USE ! vegtype with irrigation
REAL, DIMENSION(:,:), INTENT(IN), OPTIONAL :: PIRRIG  ! fraction of irrigation for each vegtype
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
! nbe of vegtype
! nbre of patches
INTEGER :: JVEG    ! loop on vegtype
INTEGER :: IPATCH  ! number of patches
INTEGER :: JP  ! PATCH index
INTEGER :: JJ, JI, JK, ID
REAL    :: ZXIRRIG
LOGICAL :: ZLIRRIG
!
INTEGER, DIMENSION(SIZE(PFIELD,2)) :: ID0, JVEG0
REAL, DIMENSION(SIZE(PFIELD,2)) :: ZMAX0, ZMAX1
REAL         :: ZCOVER_WEIGHT
!
INTEGER, DIMENSION(SIZE(PCOVER,2)) :: IMASK0
REAL, DIMENSION(SIZE(PCOVER,1)) :: ZVAL
!
REAL, DIMENSION(SIZE(PCOVER,2),NVEGTYPE)         :: ZWEIGHT
!
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PFIELD,2))   :: ZSUM_COVER_WEIGHT_PATCH
!
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PFIELD,2))   :: ZWORK
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PFIELD,2))   :: ZDZ
!
INTEGER, DIMENSION(SIZE(PCOVER,1),SIZE(PFIELD,2)):: IMASK
INTEGER, DIMENSION(SIZE(PFIELD,2)) :: JCOUNT
INTEGER, DIMENSION(:), ALLOCATABLE               :: PATCH_LIST
INTEGER                            ::  NVEG_ADD
REAL(KIND=JPRB) :: ZHOOK_HANDLE, ZHOOK_HANDLE_OMP

!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D_1',0,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D_1',1,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
ICOVER=SIZE(PCOVER,2)
IPATCH=SIZE(PFIELD,2)
!
IF (PRESENT(PDZ)) THEN
  DO JP=1,IPATCH
      ZDZ(:,JP)=PDZ(:)
  END DO
ELSE
  ZDZ(:,:)=XCDREF
END IF
!
PFIELD(:,:)=XUNDEF
!
ZWORK(:,:) = 0.
ZWEIGHT(:,:) = 0.0
ZSUM_COVER_WEIGHT_PATCH(:,:) = 0.
!
!
! Check if irrigation (and ECOSG) AND not in case of patching, else we don't have to not modified !
!
IF ((PRESENT(NPAR_VEG_IRR_USE) .AND. .NOT.PRESENT(PIRRIG)) .OR. (.NOT.PRESENT(NPAR_VEG_IRR_USE) .AND. PRESENT(PIRRIG) ) ) & 
        CALL ABOR1_SFX('AV_PATCH_PGD_1D: IF THERE IS NPAR_VEG_IRR_USE, PIRRIG IS NEEDED, AND VICE VERSA')
!
IF ( NVEG_IRR /= 0 .AND. PRESENT(NPAR_VEG_IRR_USE) ) THEN
  ! 
  NVEG_ADD = NVEG_IRR
  !
ELSE
  !
  NVEG_ADD = 0
  !
ENDIF
!
ALLOCATE(PATCH_LIST(NVEGTYPE+NVEG_ADD))
DO JVEG=1,NVEGTYPE+NVEG_ADD
  IF ( NVEG_ADD == 0 ) THEN
    CALL VEGTYPE_TO_PATCH(JVEG,IPATCH,PATCH_LIST(JVEG))
  ELSE
    CALL VEGTYPE_TO_PATCH_IRRIG(JVEG,IPATCH,NPAR_VEG_IRR_USE,PATCH_LIST(JVEG))
  ENDIF
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
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D_1',1,ZHOOK_HANDLE)
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D_2',0,ZHOOK_HANDLE)
!
 CALL GET_WEIGHT_PATCH(DTCO,ICOVER,IMASK0,KDECADE,HSFTYPE,ZWEIGHT)
!
!
!-------------------------------------------------------------------------------
  !
  !
  !*    2.     Selection of the weighting function for vegtype
  !            -----------------------------------
  !
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D_2',1,ZHOOK_HANDLE)
!
IF (HATYPE=='ARI' .OR. HATYPE=='ARV' .OR. HATYPE=='INV' .OR. HATYPE=='CDN') THEN
!!$OMP PARALLEL PRIVATE(ZHOOK_HANDLE_OMP) REDUCTION(+:ZSUM_COVER_WEIGHT_PATCH,ZWORK)
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D_3',0,ZHOOK_HANDLE_OMP)
!!$OMP DO SCHEDULE(DYNAMIC,1) PRIVATE(JCOVER,JJ,JVEG,JP, &
!!$OMP   ZVAL,JI,ZCOVER_WEIGHT)
  DO JCOVER=1,ICOVER
    !
    JJ = IMASK0(JCOVER)
    !
    DO JVEG=1,NVEGTYPE+NVEG_ADD
      !

      JP= PATCH_LIST(JVEG)
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
          DO JI=1,SIZE(PCOVER,1)
            ZVAL(JI) = 1./(LOG(ZDZ(JI,JP)/PDATA(JJ,JK)))**2 
          ENDDO
     !   ELSE
     !     CALL ABOR1_SFX('AV_PATCH_PGD_1D: (1) AVERAGING TYPE NOT ALLOWED')
        ENDIF
        !
        DO JI=1,SIZE(PCOVER,1)
          IF (PCOVER(JI,JCOVER)/=0.) THEN
            !
            ZXIRRIG=0.
            ZLIRRIG = .FALSE.
            IF (  NVEG_ADD /= 0 ) THEN
              IF ( PIRRIG(JI,JK)/=XUNDEF ) ZXIRRIG = PIRRIG(JI,JK)   ! NB: XDATA_IRRIGFRAC(JPCOVER,NVEGTYPE)
              IF ( PRESENT(NPAR_VEG_IRR_USE) )  ZLIRRIG = ANY(NPAR_VEG_IRR_USE(:)==JVEG)
            ENDIF
            !
            IF ( JVEG <= NVEGTYPE .AND. ( .NOT.ZLIRRIG .OR. ZXIRRIG==0. ) ) THEN
              ! case (1) without irrigation, or (2) call by a non-patched routine, or (3) patched, but for a vegtype non irrigated or if it is not irrigation for this vegtype in this point
              ZCOVER_WEIGHT =  PCOVER(JI,JCOVER) * ZWEIGHT(JCOVER,JK)
              !
            ELSEIF ( JVEG <= NVEGTYPE ) THEN
              ! case the irrigation patch have to be taken into account and a fraction of the vegtype is irrigated (not this part)  (implicit: ANY(NPAR_VEG_IRR_USE(:)==JVEG)
              ZCOVER_WEIGHT =  PCOVER(JI,JCOVER) * ( 1 - ZXIRRIG ) * ZWEIGHT(JCOVER,JK)
              !
            ELSE 
              ! case the irrigation patch have to be taken into account and a fraction of the vegtype is irrigated (this part) (and it's define)
              ZCOVER_WEIGHT =  PCOVER(JI,JCOVER) * ZXIRRIG * ZWEIGHT(JCOVER,JK)
              !
            ENDIF
            ZSUM_COVER_WEIGHT_PATCH(JI,JP) = ZSUM_COVER_WEIGHT_PATCH(JI,JP) + ZCOVER_WEIGHT
            ZWORK(JI,JP) = ZWORK(JI,JP) + ZVAL(JI) * ZCOVER_WEIGHT
          ENDIF
        ENDDO
        !
      ENDIF
      !
    ENDDO
    !
  ENDDO
!!$OMP END DO
  IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D_3',1,ZHOOK_HANDLE_OMP)
!!$OMP END PARALLEL
ELSEIF (HATYPE=='MAJ' .OR. HATYPE=='MA1') THEN
  !
  DO JI = 1,SIZE(PCOVER,1)
    !
    ZMAX0(:) = 0.
    JVEG0(:) = 0
    !
    DO JVEG = 1,NVEGTYPE+NVEG_ADD
      !
      JP = PATCH_LIST(JVEG)
      JK = JVEG
      IF (JVEG > NVEGTYPE) JK = NPAR_VEG_IRR_USE( JVEG - NVEGTYPE )
      !
      ! ZMAX1(JP) = MAXVAL(PCOVER(JI,:)*ZWEIGHT(:,JK))
      ZMAX1(JP) = 0.
      ID=0
      !
      ZXIRRIG=0.
      ZLIRRIG = .FALSE.
      IF ( NVEG_ADD /= 0 ) THEN
        IF ( PIRRIG(JI,JK)/=XUNDEF ) ZXIRRIG = PIRRIG(JI,JK) ! NB: XDATA_IRRIGFRAC(JPCOVER,NVEGTYPE)
        IF ( PRESENT(NPAR_VEG_IRR_USE) ) ZLIRRIG = ANY(NPAR_VEG_IRR_USE(:)==JVEG)
      ENDIF
      !
      DO JCOVER=1,ICOVER
        !
        IF ( JVEG <= NVEGTYPE .AND. ( .NOT.ZLIRRIG .OR. ZXIRRIG==0. ) ) THEN
          ! case (1) without irrigation, or (2) call by a non-patched routine, or (3) patched, but for a vegtype non irrigated or if it is not irrigation for this vegtype in this point
          !ZMAX1(JP) = MAXVAL( ZMAX1(JP) , PCOVER(JI,JCOVER)*ZWEIGHT(JCOVER,JK) )
          IF ( ZMAX1(JP) <= PCOVER(JI,JCOVER) * ZWEIGHT(JCOVER,JK)                  &
                  .AND. ( HATYPE=='MAJ' .OR. (PDATA(IMASK0(JCOVER),JK)/=0..AND.PDATA(IMASK0(JCOVER),JK)/=XUNDEF) ) ) THEN
            ZMAX1(JP) = PCOVER(JI,JCOVER) * ZWEIGHT(JCOVER,JK)
            ID = JCOVER
          ENDIF
          !
        ELSEIF ( JVEG <= NVEGTYPE ) THEN
          ! case the irrigation patch have to be taken into account and a fraction of the vegtype is irrigated (not this part)  (implicit: ANY(NPAR_VEG_IRR_USE(:)==JVEG) 
          !ZMAX1(JP) = MAXVAL( ZMAX1(JP) , PCOVER(JI,JCOVER) * ( 1 - ZXIRRIG ) * ZWEIGHT(JCOVER,JK) )
          IF ( ZMAX1(JP) <= PCOVER(JI,JCOVER) * ( 1 - ZXIRRIG ) * ZWEIGHT(JCOVER,JK) &
                  .AND. ( HATYPE=='MAJ' .OR. (PDATA(IMASK0(JCOVER),JK)/=0..AND.PDATA(IMASK0(JCOVER),JK)/=XUNDEF) ) ) THEN 
            ZMAX1(JP) = PCOVER(JI,JCOVER) * ( 1 - ZXIRRIG ) * ZWEIGHT(JCOVER,JK) 
            ID = JCOVER
          ENDIF
          !
        ELSE
          ! case the irrigation patch have to be taken into account and a fraction of the vegtype is irrigated (this part) (and it's define)
          !ZMAX1(JP) = MAXVAL( ZMAX1(JP) , PCOVER(JI,JCOVER) * ZXIRRIG * ZWEIGHT(JCOVER,JK) )
          IF ( ZMAX1(JP) <= PCOVER(JI,JCOVER) * ZXIRRIG * ZWEIGHT(JCOVER,JK)         &
                  .AND. ( HATYPE=='MAJ' .OR. (PDATA(IMASK0(JCOVER),JK)/=0..AND.PDATA(IMASK0(JCOVER),JK)/=XUNDEF) ) ) THEN
            ZMAX1(JP) = PCOVER(JI,JCOVER) * ZXIRRIG * ZWEIGHT(JCOVER,JK)
            ID = JCOVER
          ENDIF
          !
        ENDIF
      ENDDO
      !
      IF ( ZMAX1(JP)>ZMAX0(JP) ) THEN !.AND. ( HATYPE=='MAJ' .OR. PDATA(IMASK0(ID),JK)/=0. ) ) THEN !(IMASK0(MAXVAL(MAXLOC(PCOVER(JI,:)*ZWEIGHT(:,JK)))),JK)/=0. ) ) 
        !ID0(JP) = MAXVAL(MAXLOC(PCOVER(JI,:)*ZWEIGHT(:,JK)))
        ID0(JP) = ID
        ZMAX0(JP) = ZMAX1(JP)
        JVEG0(JP) = JK
      ENDIF
      !
    ENDDO
    !
    DO JP = 1,SIZE(ZWORK,2)
      IF (JVEG0(JP)>0) THEN
        ZWORK(JI,JP) = PDATA(IMASK0(ID0(JP)),JVEG0(JP))
        ZSUM_COVER_WEIGHT_PATCH(JI,JP) = 1.
      ENDIF
    ENDDO
    !
  ENDDO
  !
ELSE
  CALL ABOR1_SFX('AV_PATCH_PGD: (1) AVERAGING TYPE NOT ALLOWED : "'//HATYPE//'"')
ENDIF  
DEALLOCATE(PATCH_LIST)
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D_4',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
!*    4.     End of Averaging
!            ----------------
!
IMASK(:,:)=0
JCOUNT(:)=0
DO JP=1,IPATCH
  DO JI=1,SIZE(PCOVER,1)
    IF ( ZSUM_COVER_WEIGHT_PATCH(JI,JP) >0.) THEN
      JCOUNT(JP)=JCOUNT(JP)+1
      IMASK(JCOUNT(JP),JP)=JI
    ENDIF
  ENDDO
ENDDO
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
  CASE ('ARI', 'ARV')
!   
    DO JP=1,IPATCH
!cdir nodep
      DO JJ=1,JCOUNT(JP)
          JI = IMASK(JJ,JP)
          PFIELD(JI,JP) =  ZWORK(JI,JP) / ZSUM_COVER_WEIGHT_PATCH(JI,JP)
      ENDDO
    ENDDO
!
!-------------------------------------------------------------------------------
!
!*    4.3    Inverse averaging
!            -----------------
!
  CASE('INV' )
!
    DO JP=1,IPATCH
!cdir nodep
      DO JJ=1,JCOUNT(JP)
        JI = IMASK(JJ,JP)
        PFIELD(JI,JP) = ZSUM_COVER_WEIGHT_PATCH(JI,JP) / ZWORK(JI,JP)
      ENDDO
    ENDDO
!-------------------------------------------------------------------------------!
!
!*    4.4    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    DO JP=1,IPATCH
!cdir nodep
      DO JJ=1,JCOUNT(JP)
        JI = IMASK(JJ,JP)
        PFIELD(JI,JP) = ZDZ(JI,JP) * EXP( - SQRT(ZSUM_COVER_WEIGHT_PATCH(JI,JP)/ZWORK(JI,JP)) )
      ENDDO
    ENDDO
!
!-------------------------------------------------------------------------------
!
!*    4.4    Majoritary averaging
!            --------------------
!
  CASE('MAJ','MA1' )
!
    DO JP=1,IPATCH
      WHERE ( ZSUM_COVER_WEIGHT_PATCH(:,JP) >0. )
        PFIELD(:,JP) = ZWORK(:,JP)
      END WHERE
    ENDDO

!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PATCH_PGD_1D: (2) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D_4',1,ZHOOK_HANDLE)
!
!IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD_1D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!   
END SUBROUTINE AV_PATCH_PGD_1D
!
!     ################################################################
      SUBROUTINE AV_PGD_2D (DTCO, &
                            PFIELD,PCOVER,PDATA,HSFTYPE,HATYPE,OCOVER,PDZ,KDECADE)
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
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVT_TEBD, NVT_BONE, NVT_TRBE, XCDREF, NVT_TRBD, &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND, NVT_SHRB
!
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
REAL, DIMENSION(:,:),   INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:),     INTENT(IN)  :: PDATA   ! secondary field value for each class
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
 LOGICAL, DIMENSION(:), INTENT(IN) :: OCOVER
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: JJ
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2)) :: ZWORK, ZDZ
REAL                                           :: ZWEIGHT
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2)) :: ZCOVER_WEIGHT
REAL                                           :: ZDATA
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2)) :: ZSUM_COVER_WEIGHT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_2D',0,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_2D',1,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
ICOVER=SIZE(OCOVER)
!
IF (PRESENT(PDZ)) THEN
  ZDZ(:,:)=PDZ(:,:)
ELSE
  ZDZ(:,:)=XCDREF
END IF
!
PFIELD(:,:)=XUNDEF
!
ZWORK(:,:)=0.
ZSUM_COVER_WEIGHT(:,:)=0.
!-------------------------------------------------------------------------------
JCOVER = 0
DO JJ=1,ICOVER
  !
  IF (.NOT.OCOVER(JJ)) CYCLE
  !
  JCOVER = JCOVER + 1
  !
!-------------------------------------------------------------------------------
!
!*    2.     Selection of the weighting function
!            -----------------------------------
!
  SELECT CASE (HSFTYPE)
       CASE('ALL')
         ZWEIGHT=1.

       CASE('NAT')
         ZWEIGHT=DTCO%XDATA_NATURE(JJ)

       CASE('GRD')
         ZWEIGHT=DTCO%XDATA_TOWN (JJ) * DTCO%XDATA_GARDEN(JJ)

       CASE('TWN')
         ZWEIGHT=DTCO%XDATA_TOWN  (JJ)

       CASE('WAT')
         ZWEIGHT=DTCO%XDATA_WATER (JJ)

       CASE('SEA')
         ZWEIGHT=DTCO%XDATA_SEA   (JJ)

       CASE('BLD')
         ZWEIGHT=DTCO%XDATA_TOWN  (JJ) *        DTCO%XDATA_BLD(JJ)

       CASE('STR')
         ZWEIGHT=DTCO%XDATA_TOWN  (JJ) * ( 1. - DTCO%XDATA_BLD(JJ) )

       CASE('TRE')
         PFIELD(:,:)=0.
         ZWEIGHT=DTCO%XDATA_NATURE(JJ) * (  DTCO%XDATA_VEGTYPE(JJ,NVT_TEBD) &
                                       + DTCO%XDATA_VEGTYPE(JJ,NVT_TRBE) &
                                       + DTCO%XDATA_VEGTYPE(JJ,NVT_TRBD) &
                                       + DTCO%XDATA_VEGTYPE(JJ,NVT_TEBE) &
                                       + DTCO%XDATA_VEGTYPE(JJ,NVT_TENE) &
                                       + DTCO%XDATA_VEGTYPE(JJ,NVT_BOBD) &
                                       + DTCO%XDATA_VEGTYPE(JJ,NVT_BOND) &
                                       + DTCO%XDATA_VEGTYPE(JJ,NVT_SHRB) &                                           
                                       + DTCO%XDATA_VEGTYPE(JJ,NVT_BONE) )  

       CASE('GRT')
         PFIELD(:,:)=0.
         ZWEIGHT=DTCO%XDATA_TOWN (JJ) * DTCO%XDATA_GARDEN(JJ)  &
                          * (  DTCO%XDATA_VEGTYPE(JJ,NVT_TEBD)  &
                             + DTCO%XDATA_VEGTYPE(JJ,NVT_TRBE)  &
                             + DTCO%XDATA_VEGTYPE(JJ,NVT_TRBD) &
                             + DTCO%XDATA_VEGTYPE(JJ,NVT_TEBE) &
                             + DTCO%XDATA_VEGTYPE(JJ,NVT_TENE) &
                             + DTCO%XDATA_VEGTYPE(JJ,NVT_BOBD) &
                             + DTCO%XDATA_VEGTYPE(JJ,NVT_BOND) &
                             + DTCO%XDATA_VEGTYPE(JJ,NVT_SHRB) &                             
                             + DTCO%XDATA_VEGTYPE(JJ,NVT_BONE) )  

       CASE DEFAULT
         CALL ABOR1_SFX('AV_PGD: WEIGHTING FUNCTION NOT ALLOWED')
  END SELECT
!
!-------------------------------------------------------------------------------
!
!*    3.     Averaging
!            ---------
!
!*    3.1    Work arrays
!            -----------
!
  ZCOVER_WEIGHT(:,:) = PCOVER(:,:,JCOVER) * ZWEIGHT
!
  ZSUM_COVER_WEIGHT(:,:) = ZSUM_COVER_WEIGHT(:,:) + ZCOVER_WEIGHT(:,:)
!
  ZDATA = PDATA(JJ)
!
!*    3.2    Selection of averaging type
!            ---------------------------
!
  SELECT CASE (HATYPE)
!
!-------------------------------------------------------------------------------
!
!*    3.4    Arithmetic averaging
!            --------------------
!
  CASE ('ARI', 'ARV')
!
    ZWORK(:,:) = ZWORK(:,:) + ZDATA * ZCOVER_WEIGHT(:,:) 
!
!-------------------------------------------------------------------------------
!
!*    3.5    Inverse averaging
!            -----------------
!
  CASE('INV' )
!
    ZWORK (:,:)= ZWORK(:,:) + 1./ZDATA * ZCOVER_WEIGHT(:,:)
!
!-------------------------------------------------------------------------------!
!
!*    3.6    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    ZWORK (:,:)= ZWORK(:,:) + 1./(LOG(ZDZ(:,:)/ZDATA))**2 * ZCOVER_WEIGHT(:,:)
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PGD: (1) AVERAGING TYPE NOT ALLOWED')
!
  END SELECT
!
END DO
!
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
    WHERE ( ZSUM_COVER_WEIGHT(:,:) >0. )
      PFIELD(:,:) = ZWORK(:,:) / ZSUM_COVER_WEIGHT(:,:)
    END WHERE
!
!-------------------------------------------------------------------------------
!
!*    4.3    Inverse averaging
!            -----------------
!
  CASE('INV' )
!
    WHERE ( ZSUM_COVER_WEIGHT(:,:) >0. )
      PFIELD(:,:) = ZSUM_COVER_WEIGHT(:,:) / ZWORK(:,:)
    END WHERE
!
!-------------------------------------------------------------------------------!
!
!*    4.4    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    WHERE ( ZSUM_COVER_WEIGHT(:,:) >0. )
      PFIELD(:,:) = ZDZ(:,:) * EXP( - SQRT(ZSUM_COVER_WEIGHT(:,:)/ZWORK(:,:)) )
    END WHERE
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PGD_2D: (2) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PGD_2D',1,ZHOOK_HANDLE)
! 
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AV_PGD_2D
!
!
!
!     ################################################################
      SUBROUTINE AV_PATCH_PGD (DTCO, &
                               PFIELD,PCOVER,PDATA,NPAR_VEG_IRR_USE,PIRRIG,HSFTYPE,HATYPE,OCOVER,PDZ,KDECADE)
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
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER,     ONLY : XDATA_VEG, XDATA_LAI  
USE MODD_DATA_COVER_PAR, ONLY : NVT_TEBD, NVT_BONE, NVT_TRBE, NVEGTYPE, XCDREF, NVT_TRBD, &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND, NVT_SHRB
USE MODD_AGRI,           ONLY : NVEG_IRR
!
USE MODI_VEGTYPE_TO_PATCH_IRRIG 
!
!
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
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PFIELD  ! secondary field to construct
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
REAL, DIMENSION(:,:),   INTENT(IN)  :: PDATA   ! secondary field value for each class
INTEGER,DIMENSION(:),   INTENT(IN)  :: NPAR_VEG_IRR_USE ! vegtype with irrigation
REAL, DIMENSION(:,:),   INTENT(IN)  :: PIRRIG  ! fraction of irrigation for each vegtype
 CHARACTER(LEN=3),       INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                               ! is defined
 CHARACTER(LEN=3),       INTENT(IN)  :: HATYPE  ! Type of averaging
 LOGICAL, DIMENSION(:), INTENT(IN) :: OCOVER
REAL, DIMENSION(:,:),   INTENT(IN), OPTIONAL :: PDZ    ! first model half level
INTEGER,                INTENT(IN), OPTIONAL :: KDECADE ! current month
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: JJ, JK, JN
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
! nbe of vegtype
! nbre of patches
INTEGER :: JVEG! loop on vegtype
INTEGER :: IPATCH  ! number of patches
INTEGER :: JP  ! PATCH index
!
REAL, DIMENSION(NVEGTYPE)                                       :: ZWEIGHT
REAL, DIMENSION(:,:,:), ALLOCATABLE                             :: ZCOVER_WEIGHT
!
REAL, DIMENSION(SIZE(PCOVER,1))                                 :: ZIRRIG
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2))                  :: ZCOVER
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2),SIZE(PFIELD,3))   :: ZCOVER_WEIGHT_PATCH
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2),SIZE(PFIELD,3))   :: ZSUM_COVER_WEIGHT_PATCH
REAL, DIMENSION(NVEGTYPE)                                       :: ZDATA
!
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2),SIZE(PFIELD,3))   :: ZWORK
REAL, DIMENSION(SIZE(PCOVER,1),SIZE(PCOVER,2),SIZE(PFIELD,3))   :: ZDZ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
INTEGER         ::  NVEG_ADD
!
!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD',0,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD',1,ZHOOK_HANDLE)
IF (SIZE(PFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
ICOVER=SIZE(OCOVER)
IPATCH=SIZE(PFIELD,3)
!
! Check if irrigation (and ECOSG) AND not in case where we have to not patch !
IF ( NVEG_IRR /= 0 .AND. .NOT.ANY(NPAR_VEG_IRR_USE(:) == 0. )) THEN
  NVEG_ADD = NVEG_IRR
ELSE
  IF ( ANY(NPAR_VEG_IRR_USE(:)==0. .AND. IPATCH/=NVEGTYPE )) CALL ABOR1_SFX('AV_PATCH_PGD: ERROR1 (OR WRONG MESSAGE... CHECK')  ! a supprimer (notement pour quand NPATCH /= NVEGTYPE et quand il n'y a pas d'irrigation)
!  IF ( ANY(NPAR_VEG_IRR_USE(:)==0. .AND. ICOVER/=NVEGTYPE )) CALL ABOR1_SFX('AV_PATCH_PGD: ERROR2 (OR WRONG MESSAGE... CHECK')
  IF ( ANY(NPAR_VEG_IRR_USE(:)==0) .AND. ANY(PIRRIG(:,:)/=0) ) &
          CALL ABOR1_SFX('AV_PATCH_PGD: IF NPAR_VEG_IRR_USE=(/0/), PIRRIG HAVE to be =(/0/)')

  NVEG_ADD = 0
ENDIF
!
ALLOCATE(ZCOVER_WEIGHT(SIZE(PCOVER,1),SIZE(PCOVER,2),NVEGTYPE+NVEG_ADD))
!
IF (PRESENT(PDZ)) THEN
  DO JP=1,IPATCH
    ZDZ(:,:,JP)=PDZ(:,:)
  END DO
ELSE
  ZDZ(:,:,:)=XCDREF
END IF
!
PFIELD(:,:,:)=XUNDEF
!
ZWORK(:,:,:)=0.
ZSUM_COVER_WEIGHT_PATCH(:,:,:)=0.
!
!-------------------------------------------------------------------------------
JCOVER = 0
DO JJ=1,ICOVER
  !
  IF (.NOT.OCOVER(JJ)) CYCLE
  !
  JCOVER = JCOVER + 1
  !
!-------------------------------------------------------------------------------
!
!*    2.     Selection of the weighting function for vegtype
!            -----------------------------------
!
  SELECT CASE (HSFTYPE)

     CASE('NAT')
       DO JVEG=1,NVEGTYPE
         ZWEIGHT(JVEG)=DTCO%XDATA_NATURE(JJ)*DTCO%XDATA_VEGTYPE(JJ,JVEG)
       END DO

     CASE('GRD')
       DO JVEG=1,NVEGTYPE
         ZWEIGHT(JVEG)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ)*DTCO%XDATA_VEGTYPE(JJ,JVEG)
       END DO

     CASE('VEG')
       DO JVEG=1,NVEGTYPE  
         ZWEIGHT(JVEG)=DTCO%XDATA_NATURE(JJ)*DTCO%XDATA_VEGTYPE(JJ,JVEG)*&
                             XDATA_VEG(JJ,KDECADE,JVEG)  
       END DO

     CASE('BAR')
       DO JVEG=1,NVEGTYPE  
         ZWEIGHT(JVEG)=DTCO%XDATA_NATURE(JJ)*DTCO%XDATA_VEGTYPE(JJ,JVEG)*&
                             (1.-XDATA_VEG(JJ,KDECADE,JVEG)) 
       END DO

     CASE('GRV')
       DO JVEG=1,NVEGTYPE  
         ZWEIGHT(JVEG)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ)*DTCO%XDATA_VEGTYPE(JJ,JVEG)*&
                             XDATA_VEG(JJ,KDECADE,JVEG)  
       END DO

     CASE('GRB')
       DO JVEG=1,NVEGTYPE  
         ZWEIGHT(JVEG)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ)*DTCO%XDATA_VEGTYPE(JJ,JVEG)*&
                             (1.-XDATA_VEG(JJ,KDECADE,JVEG))
       ENDDO 
       
     CASE('DVG') ! average only on vegetated area
       ZWEIGHT(:) = 0.0
       DO JVEG=1,NVEGTYPE
         IF ( SUM(XDATA_LAI(JJ,:,JVEG)).GT.0.) &
           ZWEIGHT(JVEG)=DTCO%XDATA_NATURE(JJ)*DTCO%XDATA_VEGTYPE(JJ,JVEG)
       END DO     

     CASE('GDV') ! average only on vegetated area
       ZWEIGHT(:) = 0.0             
       DO JVEG=1,NVEGTYPE
         IF ( SUM(XDATA_LAI(JJ,:,JVEG)).GT.0.) &
           ZWEIGHT(JVEG)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ)*DTCO%XDATA_VEGTYPE(JJ,JVEG)
       END DO     

     CASE('LAI')
       DO JVEG=1,NVEGTYPE  
         ZWEIGHT(JVEG)=DTCO%XDATA_NATURE(JJ)*DTCO%XDATA_VEGTYPE(JJ,JVEG)*&
                             XDATA_LAI(JJ,KDECADE,JVEG)  
       END DO

     CASE('GRL')
       DO JVEG=1,NVEGTYPE  
         ZWEIGHT(JVEG)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ)*DTCO%XDATA_VEGTYPE(JJ,JVEG)*&
                             XDATA_LAI(JJ,KDECADE,JVEG)  
       END DO

      CASE('TRE')
        ZWEIGHT(:)=0.
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_TEBD)>0.) THEN
          ZWEIGHT(NVT_TEBD)=DTCO%XDATA_NATURE(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_TEBD)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_BONE)>0.) THEN
          ZWEIGHT(NVT_BONE)=DTCO%XDATA_NATURE(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_BONE)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_TRBE)>0.) THEN
          ZWEIGHT(NVT_TRBE)=DTCO%XDATA_NATURE(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_TRBE)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_TRBD)>0.) THEN
          ZWEIGHT(NVT_TRBD)=DTCO%XDATA_NATURE(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_TRBD)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_TEBE)>0.) THEN
          ZWEIGHT(NVT_TEBE)=DTCO%XDATA_NATURE(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_TEBE)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_TENE)>0.) THEN
          ZWEIGHT(NVT_TENE)=DTCO%XDATA_NATURE(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_TENE)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_BOBD)>0.) THEN
          ZWEIGHT(NVT_BOBD)=DTCO%XDATA_NATURE(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_BOBD)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_BOND)>0.) THEN
          ZWEIGHT(NVT_BOND)=DTCO%XDATA_NATURE(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_BOND)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_SHRB)>0.) THEN
          ZWEIGHT(NVT_SHRB)=DTCO%XDATA_NATURE(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_SHRB)
        END IF

      CASE('GRT')
        ZWEIGHT(:)=0.
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_TEBD)>0.) THEN
          ZWEIGHT(NVT_TEBD)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_TEBD)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_BONE)>0.) THEN
          ZWEIGHT(NVT_BONE)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_BONE)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_TRBE)>0.) THEN
          ZWEIGHT(NVT_TRBE)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_TRBE)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_TRBD)>0.) THEN
          ZWEIGHT(NVT_TRBD)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_TRBD)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_TEBE)>0.) THEN
          ZWEIGHT(NVT_TEBE)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_TEBE)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_TENE)>0.) THEN
          ZWEIGHT(NVT_TENE)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_TENE)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_BOBD)>0.) THEN
          ZWEIGHT(NVT_BOBD)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_BOBD)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_BOND)>0.) THEN
          ZWEIGHT(NVT_BOND)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_BOND)
        END IF
        IF (DTCO%XDATA_VEGTYPE(JJ,NVT_SHRB)>0.) THEN
          ZWEIGHT(NVT_SHRB)=DTCO%XDATA_TOWN(JJ)*DTCO%XDATA_GARDEN(JJ) * DTCO%XDATA_VEGTYPE(JJ,NVT_SHRB)
        END IF

      CASE DEFAULT
         CALL ABOR1_SFX('AV_PATCH_PGD: WEIGHTING FUNCTION FOR VEGTYPE NOT ALLOWED')
  END SELECT
!
!-------------------------------------------------------------------------------
!
!*    3.     Averaging
!            ---------
!
!*    3.1    Work arrays given for each patch
!            -----------
! 
  ZCOVER_WEIGHT(:,:,:)=0. 
  ZCOVER_WEIGHT_PATCH(:,:,:)=0.
 
  DO JVEG=1,NVEGTYPE+NVEG_ADD
    !
    CALL VEGTYPE_TO_PATCH_IRRIG(JVEG,IPATCH,NPAR_VEG_IRR_USE,JP)
    !
    JK = JVEG
    IF (JVEG > NVEGTYPE) JK = NPAR_VEG_IRR_USE( JVEG - NVEGTYPE )
    !
    ZIRRIG(:)=0.
    IF ( NVEG_ADD/=0 ) THEN
      IF ( ANY(PIRRIG /= 0 ) ) WHERE ( PIRRIG(:,JK)/=XUNDEF ) ZIRRIG(:) = PIRRIG(:,JK)
!     IF ( PRESENT(PIRRIG) ) 
    ENDIF
    !
    DO JP=1,SIZE(PCOVER,2)
      !
      IF ( NVEG_ADD == 0 ) THEN
        ! case without irrigation
        ZCOVER(:,JP) = PCOVER(:,JP,JCOVER) * ZWEIGHT(JK)
      ELSEIF ( JVEG <= NVEGTYPE .AND. .NOT.ANY(NPAR_VEG_IRR_USE(:)==JVEG) ) THEN
        ! case (1) call by a non-patched routine, or (2) patched, but for a vegtype non irrigated
        ZCOVER(:,JP) = PCOVER(:,JP,JCOVER) * ZWEIGHT(JK)
      ELSEIF ( JVEG <= NVEGTYPE ) THEN
        ! case the irrigation patch have to be taken into account and a fraction of the vegtype is irrigated (not this part)  (implicit: ANY(NPAR_VEG_IRR_USE(:)==JVEG)
        ZCOVER(:,JP) =  PCOVER(:,JP,JCOVER) * ( 1 - ZIRRIG(:) ) * ZWEIGHT(JK)
      ELSE
        ! case the irrigation patch have to be taken into account and a fraction of the vegtype is irrigated (this part) (and it's define)
        ZCOVER(:,JP) =  PCOVER(:,JP,JCOVER) * ZIRRIG(:) * ZWEIGHT(JK)
      ENDIF
    ENDDO
    !
    !ZCOVER_WEIGHT(:,:,JVEG) =  ZCOVER_WEIGHT(:,:,JVEG) +&            ! PROBABLY THAT THIS "ZCOVER_WEIGHT(:,:,JVEG) +" IS NOT NEEDED
    !                                 PCOVER(:,:,JCOVER) * ZWEIGHT(JK) 
    ZCOVER_WEIGHT(:,:,JVEG)     =  ZCOVER(:,:)
    ZCOVER_WEIGHT_PATCH(:,:,JP) =  ZCOVER_WEIGHT_PATCH(:,:,JP) + ZCOVER(:,:)
    !
  END DO 
!
  ZSUM_COVER_WEIGHT_PATCH(:,:,:) = ZSUM_COVER_WEIGHT_PATCH(:,:,:) + ZCOVER_WEIGHT_PATCH(:,:,:)
!
  ZDATA(:) = PDATA(JJ,:)
!
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
      DO JVEG=1,NVEGTYPE+NVEG_ADD
        JK = JVEG
        IF (JVEG > NVEGTYPE) JK = NPAR_VEG_IRR_USE( JVEG - NVEGTYPE )
        !
        CALL VEGTYPE_TO_PATCH_IRRIG(JVEG,IPATCH,NPAR_VEG_IRR_USE,JP)
        ZWORK(:,:,JP) =  ZWORK(:,:,JP) + ZDATA(JK) * ZCOVER_WEIGHT(:,:,JVEG)
      END DO
!
!-------------------------------------------------------------------------------
!
!*    3.4    Inverse averaging
!            -----------------
!
    CASE('INV' )
!
      DO JVEG=1,NVEGTYPE+NVEG_ADD
        JK = JVEG
        IF (JVEG > NVEGTYPE) JK = NPAR_VEG_IRR_USE( JVEG - NVEGTYPE )
        !
        CALL VEGTYPE_TO_PATCH_IRRIG(JVEG,IPATCH,NPAR_VEG_IRR_USE,JP) 
        ZWORK(:,:,JP)= ZWORK(:,:,JP) + 1./ ZDATA(JK)* ZCOVER_WEIGHT(:,:,JVEG)
      END DO    
!
!-------------------------------------------------------------------------------!
!
!*    3.5    Roughness length averaging
!            --------------------------

!
    CASE('CDN')
!
      DO JVEG=1,NVEGTYPE+NVEG_ADD
        JK = JVEG
        IF (JVEG > NVEGTYPE) JK = NPAR_VEG_IRR_USE( JVEG - NVEGTYPE )
        !
        CALL VEGTYPE_TO_PATCH_IRRIG(JVEG,IPATCH,NPAR_VEG_IRR_USE,JP)
        ZWORK(:,:,JP)= ZWORK(:,:,JP) + 1./(LOG(ZDZ(:,:,JP)/ ZDATA(JK)))**2    &
                                * ZCOVER_WEIGHT(:,:,JVEG)  
      END DO   
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PATCH_PGD: (1) AVERAGING TYPE NOT ALLOWED')
!
  END SELECT
!
END DO
!
DEALLOCATE(ZCOVER_WEIGHT)
!
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
    WHERE ( ZSUM_COVER_WEIGHT_PATCH(:,:,:) >0. )
      PFIELD(:,:,:) =  ZWORK(:,:,:) / ZSUM_COVER_WEIGHT_PATCH(:,:,:)
    END WHERE
!
!-------------------------------------------------------------------------------
!
!*    4.3    Inverse averaging
!            -----------------
!
  CASE('INV' )
!
    WHERE ( ZSUM_COVER_WEIGHT_PATCH(:,:,:) >0. )
      PFIELD(:,:,:) = ZSUM_COVER_WEIGHT_PATCH(:,:,:) / ZWORK(:,:,:)
    END WHERE
!-------------------------------------------------------------------------------!
!
!*    4.4    Roughness length averaging
!            --------------------------

!
  CASE('CDN')
!
    WHERE ( ZSUM_COVER_WEIGHT_PATCH(:,:,:) >0. )
      PFIELD(:,:,:) = ZDZ(:,:,:) * EXP( - SQRT(ZSUM_COVER_WEIGHT_PATCH(:,:,:)/ZWORK(:,:,:)) )
    END WHERE
!
!-------------------------------------------------------------------------------
!
  CASE DEFAULT
    CALL ABOR1_SFX('AV_PATCH_PGD: (2) AVERAGING TYPE NOT ALLOWED')
!
END SELECT
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:AV_PATCH_PGD',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE AV_PATCH_PGD
!
!
!     ################################################################
      SUBROUTINE MAJOR_PATCH_PGD_1D(TFIELD,PCOVER,TDATA,HSFTYPE,HATYPE,OCOVER,KDECADE,NPAR_VEG_IRR_USE,PIRRIG)
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
USE MODI_VEGTYPE_TO_PATCH
USE MODI_VEGTYPE_TO_PATCH_IRRIG
!
USE MODE_AV_PGD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE (DATE_TIME), DIMENSION(:,:), INTENT(OUT) :: TFIELD  ! secondary field to construct
REAL, DIMENSION(:,:), INTENT(IN)  :: PCOVER  ! fraction of each cover class
TYPE (DATE_TIME), DIMENSION(:,:), INTENT(IN)  :: TDATA   ! secondary field value for each class
CHARACTER(LEN=3),     INTENT(IN)  :: HSFTYPE ! Type of surface where the field
                                             !   is defined
CHARACTER(LEN=3),     INTENT(IN)  :: HATYPE  ! Type of averaging
LOGICAL, DIMENSION(:), INTENT(IN) :: OCOVER
INTEGER,     INTENT(IN), OPTIONAL :: KDECADE ! current month
INTEGER,DIMENSION(:), INTENT(IN), OPTIONAL :: NPAR_VEG_IRR_USE ! vegtype with irrigation
REAL, DIMENSION(:,:), INTENT(IN), OPTIONAL :: PIRRIG  ! fraction of irrigation for each vegtype
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: JJ, JK
INTEGER :: ICOVER  ! number of cover classes
INTEGER :: JCOVER  ! loop on cover classes
!
INTEGER :: JVEG! loop on vegtype
!
REAL    :: ZXIRRIG
LOGICAL :: ZLIRRIG
INTEGER, DIMENSION(SIZE(PCOVER,2),NVEGTYPE)      :: IDATA_DOY
INTEGER, DIMENSION(SIZE(PCOVER,1))               :: IDOY
REAL,    DIMENSION(365)                          :: ZCOUNT
INTEGER                                          :: JP, IMONTH, IDAY
INTEGER                                          :: IPATCH, JI
INTEGER                                          :: NVEG_ADD
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.1    field does not exist
!            --------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:MAJOR_PATCH_PGD_1D',0,ZHOOK_HANDLE)
IF (SIZE(TFIELD)==0 .AND. LHOOK) CALL DR_HOOK('MODI_AV_PGD:MAJOR_PATCH_PGD_1D',1,ZHOOK_HANDLE)
IF (SIZE(TFIELD)==0) RETURN
!
!-------------------------------------------------------------------------------
!
!*    1.2    Initializations
!            ---------------
!
IPATCH=SIZE(TFIELD,2)
!
! Check if irrigation (and ECOSG) AND not in case of patching, else we don't have to not modified !
!
IF ((PRESENT(NPAR_VEG_IRR_USE) .AND. .NOT.PRESENT(PIRRIG)) .OR. (.NOT.PRESENT(NPAR_VEG_IRR_USE) .AND. PRESENT(PIRRIG) ) ) &
        CALL ABOR1_SFX('MAJOR_PATCH_PGD_1D: IF THERE IS NPAR_VEG_IRR_USE, PIRRIG IS NEEDED, AND VICE VERSA')
!
IF ( NVEG_IRR /= 0 .AND. PRESENT(NPAR_VEG_IRR_USE) ) THEN
  NVEG_ADD = NVEG_IRR
ELSE
  NVEG_ADD = 0
ENDIF
!
TFIELD(:,:)%TDATE%YEAR  = NUNDEF
TFIELD(:,:)%TDATE%MONTH = NUNDEF
TFIELD(:,:)%TDATE%DAY   = NUNDEF
TFIELD(:,:)%TIME        = XUNDEF
!
IDOY(:) = 0
!
 CALL DATE2DOY(TDATA,OCOVER,IDATA_DOY)
!-------------------------------------------------------------------------------
DO JI = 1,SIZE(PCOVER,1)
  !
  DO JP=1,IPATCH
    !
    ZCOUNT(:) = 0.
    !
    DO JVEG=1,NVEGTYPE+NVEG_ADD
      !
      JK = JVEG
      IF ( NVEG_ADD == 0 ) THEN
        CALL VEGTYPE_TO_PATCH(JVEG,IPATCH,JJ)
      ELSE
        CALL VEGTYPE_TO_PATCH_IRRIG(JVEG,IPATCH,NPAR_VEG_IRR_USE,JJ)
        IF (JVEG > NVEGTYPE) JK = NPAR_VEG_IRR_USE( JVEG - NVEGTYPE )
      ENDIF
      !
      IF(JP == JJ) THEN
        !
        ZXIRRIG=0.
        IF ( NVEG_ADD /= 0 ) THEN
          IF ( PIRRIG(JI,JK)/=XUNDEF ) ZXIRRIG = PIRRIG(JI,JK)
        ENDIF
        !
        ZLIRRIG = .FALSE.
        IF ( PRESENT(NPAR_VEG_IRR_USE) .AND. NVEG_ADD /= 0 ) ZLIRRIG = ANY(NPAR_VEG_IRR_USE(:)==JVEG)
        !
        DO JCOVER = 1,SIZE(PCOVER,2)
          !
          IF (IDATA_DOY(JCOVER,JK) /= NUNDEF .AND. PCOVER(JI,JCOVER)/=0.) THEN
            !
            IF ( JVEG <= NVEGTYPE .AND. ( .NOT.ZLIRRIG .OR. ZXIRRIG==0. ) ) THEN
              ! case (1) without irrigation, or (2) call by a non-patched routine, or (3) patched, but for a vegtype non irrigated or if it is not irrigation for this vegtype in this point
              ZCOUNT(IDATA_DOY(JCOVER,JK)) = ZCOUNT(IDATA_DOY(JCOVER,JK)) + PCOVER(JI,JCOVER)
            ELSEIF ( JVEG <= NVEGTYPE ) THEN
              ! case the irrigation patch have to be taken into account and a fraction of the vegtype is irrigated (not this part)  (implicit: ANY(NPAR_VEG_IRR_USE(:)==JVEG)
              ZCOUNT(IDATA_DOY(JCOVER,JK)) = ZCOUNT(IDATA_DOY(JCOVER,JK)) + PCOVER(JI,JCOVER) * ( 1 - ZXIRRIG )
            ELSE
              ! case the irrigation patch have to be taken into account and a fraction of the vegtype is irrigated (this part) (and it's define)
              ZCOUNT(IDATA_DOY(JCOVER,JK)) = ZCOUNT(IDATA_DOY(JCOVER,JK)) + PCOVER(JI,JCOVER) * ZXIRRIG
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
    IDOY(JI) = 0
    IF (ANY(ZCOUNT(:)/=0.)) IDOY(JI) = MAXLOC(ZCOUNT,1)
    !
    CALL DOY2DATE(IDOY(JI),IMONTH,IDAY)
    !
    TFIELD(JI,JP)%TDATE%MONTH = IMONTH
    TFIELD(JI,JP)%TDATE%DAY   = IDAY
    IF (IMONTH/=NUNDEF) TFIELD(JI,JP)%TIME   = 0.
    !
  END DO
  !
END DO
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_AV_PGD:MAJOR_PATCH_PGD_1D',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAJOR_PATCH_PGD_1D
!-------------------------------------------------------------------------------
!
