!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#######################
MODULE MODI_VEG_HEIGHT_FROM_LAI
!#######################
!
INTERFACE VEG_HEIGHT_FROM_LAI
!
    FUNCTION VEG_HEIGHT_FROM_LAI_0D(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS,NPAR_VEG_IRR_USE) RESULT(PH_VEG)
!
REAL,                 INTENT(IN) :: PLAI         ! Leaf area Index
REAL,                 INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,              INTENT(IN) :: OAGRI_TO_GRASS
INTEGER,DIMENSION(:), INTENT(IN) :: NPAR_VEG_IRR_USE ! vegtype with irrigation
!
REAL,   DIMENSION(SIZE(PVEGTYPE))  :: PH_VEG          ! vegetation height
!
END FUNCTION VEG_HEIGHT_FROM_LAI_0D
!
!
    FUNCTION VEG_HEIGHT_FROM_LAI_1D(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS,NPAR_VEG_IRR_USE) RESULT(PH_VEG)
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:),   INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                INTENT(IN) :: OAGRI_TO_GRASS
INTEGER,DIMENSION(:), INTENT(IN) :: NPAR_VEG_IRR_USE ! vegtype with irrigation
!
REAL,   DIMENSION(SIZE(PVEGTYPE,1),SIZE(PVEGTYPE,2))  :: PH_VEG          ! vegetation height
!
END FUNCTION VEG_HEIGHT_FROM_LAI_1D
!
!
! A. Druel: Never Used?
!    FUNCTION VEG_HEIGHT_FROM_LAI_2D(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PH_VEG)
!!
!REAL,   DIMENSION(:,:),   INTENT(IN) :: PLAI         ! Leaf area Index
!REAL,   DIMENSION(:,:),   INTENT(IN) :: PH_TREE      ! height of trees
!REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
!LOGICAL,                  INTENT(IN) :: OAGRI_TO_GRASS
!!
!REAL,   DIMENSION(SIZE(PVEGTYPE,1),SIZE(PVEGTYPE,2),SIZE(PVEGTYPE,3))  :: PH_VEG          ! vegetation height
!!
!END FUNCTION VEG_HEIGHT_FROM_LAI_2D
!
    FUNCTION VEG_HEIGHT_FROM_LAI_VEGTYPE(PLAI,PH_TREE,OAGRI_TO_GRASS) RESULT(PH_VEG)
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:),   INTENT(IN) :: PH_TREE      ! height of trees
LOGICAL,                INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI))  :: PH_VEG  ! vegetation height
!
END FUNCTION VEG_HEIGHT_FROM_LAI_VEGTYPE
!
END INTERFACE
!
END MODULE MODI_VEG_HEIGHT_FROM_LAI
!

!   ###########################################################
    FUNCTION VEG_HEIGHT_FROM_LAI_0D(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS,NPAR_VEG_IRR_USE) RESULT(PH_VEG)
!   ###########################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates vegetation height from leaf
!    area index and type of vegetation
!    (most of types; forest and vineyards; grassland)
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
!!
!!      V. Masson and A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!      P. Samuelsson 02/2012 MEB
!!      A. Druel      02/2019  streamlines the code and adapt it to be compatible with new irrigation (+fix bug)
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_PARK,        &
                                NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD,      &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND,      &
                                NVT_SHRB, NVT_C3, NVT_C4, NVT_IRR,           &
                                NVT_GRAS, NVT_BOGR, NVT_TROG, NVT_C3W,       &
                                NVT_C3S, NVT_FLTR, NVT_FLGR
USE MODD_TREEDRAG,       ONLY : LTREEDRAG
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_AGRI,           ONLY : NVEG_IRR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,                 INTENT(IN) :: PLAI         ! Leaf area Index
REAL,                 INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,              INTENT(IN) :: OAGRI_TO_GRASS
INTEGER,DIMENSION(:), INTENT(IN) :: NPAR_VEG_IRR_USE ! vegtype with irrigation
!
REAL,   DIMENSION(SIZE(PVEGTYPE))  :: PH_VEG          ! vegetation height
!
!*      0.2    declarations of local variables
!
REAL                            :: ZALLEN_H    ! Allen formula for height
REAL                            :: ZLAI        ! LAI for vegetated areas
REAL                            :: PNOVEG      ! fraction of no veg
!
REAL                            :: ZAVG_H      ! averaged height
REAL                            :: ZZREF       ! reference height        
!
INTEGER                         :: JTYPE, JTYPE2       ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_VEG_HEIGHT_FROM_LAI:VEG_HEIGHT_FROM_LAI_0D',0,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
PH_VEG(:) = XUNDEF ! fix bug
!
ZLAI = PLAI
IF ( NVEG_IRR == 0 ) THEN
  IF ( PVEGTYPE(NVT_NO  ) + PVEGTYPE(NVT_ROCK) + PVEGTYPE(NVT_SNOW) < 1.) THEN
    ZLAI = PLAI / (1.-PVEGTYPE(NVT_NO)-PVEGTYPE(NVT_ROCK)-PVEGTYPE(NVT_SNOW))
  END IF
ELSE
  PNOVEG = 0.
  DO JTYPE = 1, NVEG_IRR + NVEGTYPE ! = SIZE(PVEGTYPE,1)
    JTYPE2 = JTYPE
    IF ( JTYPE > NVEGTYPE) JTYPE2 = NPAR_VEG_IRR_USE( JTYPE - NVEGTYPE )
    IF ( JTYPE2 == NVT_NO .OR. JTYPE2 == NVT_ROCK .OR. JTYPE2 == NVT_SNOW ) PNOVEG = PNOVEG + PVEGTYPE(JTYPE)
  ENDDO
  IF ( PNOVEG < 1.) ZLAI = PLAI / (1. - PNOVEG)
ENDIF
!
ZALLEN_H = 0.
IF ( PLAI /= XUNDEF) THEN
  ZALLEN_H = EXP((ZLAI-3.5)/(1.3))
END IF
!
!
DO JTYPE = 1, NVEG_IRR + NVEGTYPE ! = SIZE(PVEGTYPE,1)
  JTYPE2 = JTYPE
  IF ( JTYPE > NVEGTYPE) JTYPE2 = NPAR_VEG_IRR_USE( JTYPE - NVEGTYPE )
  !
  IF ( JTYPE2==NVT_PARK .OR. JTYPE2==NVT_FLGR ) PH_VEG(JTYPE) = ZLAI / 6.                                                  ! irr. grassland
  !
  IF ( JTYPE2==NVT_TEBD .OR. JTYPE2==NVT_BONE .OR. JTYPE2==NVT_TRBE .OR. JTYPE2==NVT_TRBD .OR. JTYPE2==NVT_TEBE .OR. & 
       JTYPE2==NVT_TENE .OR. JTYPE2==NVT_BOBD .OR. JTYPE2==NVT_BOND .OR. JTYPE2==NVT_SHRB .OR. JTYPE2==NVT_FLTR ) THEN     ! forest 
    IF (LTREEDRAG) THEN
      PH_VEG(JTYPE) = ZLAI / 6.
    ELSE
      PH_VEG(JTYPE) = PH_TREE
    END IF
  ENDIF
  !
  IF ( JTYPE2==NVT_GRAS .OR. JTYPE2==NVT_BOGR .OR. JTYPE2==NVT_TROG ) PH_VEG(JTYPE) = ZLAI / 6.                            ! grassland
  !
  IF(OAGRI_TO_GRASS) THEN                                                                                                  ! crops
    IF ( JTYPE2==NVT_C3 .OR. JTYPE2==NVT_C3W .OR. JTYPE2==NVT_C3S .OR. & 
         JTYPE2==NVT_C4 .OR. JTYPE2==NVT_IRR ) PH_VEG(JTYPE) = ZLAI / 6.
  ELSE
    IF ( JTYPE2==NVT_C3 .OR. JTYPE2==NVT_C3W .OR. JTYPE2==NVT_C3S ) PH_VEG(JTYPE) = MIN(1. , ZALLEN_H )
    IF ( JTYPE2==NVT_C4 .OR. JTYPE2==NVT_IRR ) PH_VEG(JTYPE) = MIN(2.5, ZALLEN_H )
  ENDIF
  !
  IF ( JTYPE2==NVT_NO  ) PH_VEG(JTYPE) = 0.1                          ! no vegetation (smooth) 
  IF ( JTYPE2==NVT_ROCK) PH_VEG(JTYPE) = 1.                           ! no vegetation (rocks)
  IF ( JTYPE2==NVT_SNOW) PH_VEG(JTYPE) = 0.01                         ! no vegetation (snow)
  !
ENDDO
!
PH_VEG(:) = MAX(PH_VEG(:),0.001)
!
IF (LHOOK) CALL DR_HOOK('MODI_VEG_HEIGHT_FROM_LAI:VEG_HEIGHT_FROM_LAI_0D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END FUNCTION VEG_HEIGHT_FROM_LAI_0D
!
!   ###########################################################
    FUNCTION VEG_HEIGHT_FROM_LAI_1D(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS,NPAR_VEG_IRR_USE) RESULT(PH_VEG)
!   ###########################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates vegetation height from leaf
!    area index and type of vegetation
!    (most of types; forest and vineyards; grassland)
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
!!
!!      V. Masson and A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     25/03/99
!!      A. Druel     02/2019  streamlines the code and adapt it to be compatible with new irrigation
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_PARK,        &
                                NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD,      &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND,      &
                                NVT_SHRB, NVT_C3, NVT_C4, NVT_IRR,           &
                                NVT_GRAS, NVT_BOGR, NVT_TROG, NVT_C3W,       &
                                NVT_C3S, NVT_FLTR, NVT_FLGR
USE MODD_TREEDRAG,       ONLY : LTREEDRAG
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_AGRI,           ONLY : NVEG_IRR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:),   INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                INTENT(IN) :: OAGRI_TO_GRASS
INTEGER,DIMENSION(:),   INTENT(IN) :: NPAR_VEG_IRR_USE ! vegtype with irrigation
!
REAL,   DIMENSION(SIZE(PVEGTYPE,1),SIZE(PVEGTYPE,2))  :: PH_VEG          ! vegetation height
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PLAI))                  :: ZALLEN_H    ! Allen formula for height
REAL, DIMENSION(SIZE(PLAI))                  :: ZLAI        ! LAI for vegetated areas
REAL, DIMENSION(SIZE(PLAI))                  :: PNOVEG      ! no vegetation frac
!
REAL, DIMENSION(SIZE(PLAI))                  :: ZAVG_H      ! averaged height
REAL                                         :: ZZREF       ! reference height        
!
INTEGER                                      :: JTYPE, JTYPE2   ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_VEG_HEIGHT_FROM_LAI:VEG_HEIGHT_FROM_LAI_1D',0,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
PH_VEG(:,:) = XUNDEF
!
ZLAI(:) = PLAI(:)
IF ( NVEG_IRR == 0 ) THEN
  WHERE ( PVEGTYPE(:,NVT_NO  ) + PVEGTYPE(:,NVT_ROCK) + PVEGTYPE(:,NVT_SNOW) < 1.) 
    ZLAI(:) = PLAI(:) / (1.-PVEGTYPE(:,NVT_NO)-PVEGTYPE(:,NVT_ROCK)-PVEGTYPE(:,NVT_SNOW))
  END WHERE
ELSE
  PNOVEG(:) = 0.
  DO JTYPE = 1, NVEG_IRR + NVEGTYPE ! = SIZE(PVEGTYPE,2)
    JTYPE2 = JTYPE
    IF ( JTYPE > NVEGTYPE) JTYPE2 = NPAR_VEG_IRR_USE( JTYPE - NVEGTYPE )
    IF ( JTYPE2 == NVT_NO .OR. JTYPE2 == NVT_ROCK .OR. JTYPE2 == NVT_SNOW ) PNOVEG(:) = PNOVEG(:) + PVEGTYPE(:,JTYPE)
  ENDDO
  WHERE ( PNOVEG(:) < 1.) ZLAI(:) = PLAI(:) / (1. - PNOVEG(:) )
ENDIF
!
ZALLEN_H(:) = 0.
WHERE (PLAI(:) /= XUNDEF)
  ZALLEN_H(:) = EXP((ZLAI(:)-3.5)/(1.3))
END WHERE
!
!
DO JTYPE = 1, NVEG_IRR + NVEGTYPE ! = SIZE(PVEGTYPE,2)
  JTYPE2 = JTYPE
  IF ( JTYPE > NVEGTYPE) JTYPE2 = NPAR_VEG_IRR_USE( JTYPE - NVEGTYPE )
  !
  IF ( JTYPE2==NVT_PARK .OR. JTYPE2==NVT_FLGR ) PH_VEG(:,JTYPE) = ZLAI(:) / 6.                                             ! irr. grassland
  !
  IF ( JTYPE2==NVT_TEBD .OR. JTYPE2==NVT_BONE .OR. JTYPE2==NVT_TRBE .OR. JTYPE2==NVT_TRBD .OR. JTYPE2==NVT_TEBE .OR. &
       JTYPE2==NVT_TENE .OR. JTYPE2==NVT_BOBD .OR. JTYPE2==NVT_BOND .OR. JTYPE2==NVT_SHRB .OR. JTYPE2==NVT_FLTR ) THEN     ! forest 
    IF (LTREEDRAG) THEN
      PH_VEG(:,JTYPE) = ZLAI(:) / 6.
    ELSE
      PH_VEG(:,JTYPE) = PH_TREE(:)
    END IF
  ENDIF
  !
  IF ( JTYPE2==NVT_GRAS .OR. JTYPE2==NVT_BOGR .OR. JTYPE2==NVT_TROG ) PH_VEG(:,JTYPE) = ZLAI(:) / 6.                       ! grassland
  !
  IF(OAGRI_TO_GRASS) THEN                                                                                                  ! crops
    IF ( JTYPE2==NVT_C3 .OR. JTYPE2==NVT_C3W .OR. JTYPE2==NVT_C3S .OR. &
         JTYPE2==NVT_C4 .OR. JTYPE2==NVT_IRR ) PH_VEG(:,JTYPE) = ZLAI(:) / 6.
  ELSE
    IF ( JTYPE2==NVT_C3 .OR. JTYPE2==NVT_C3W .OR. JTYPE2==NVT_C3S ) PH_VEG(:,JTYPE) = MIN(1. , ZALLEN_H(:) )
    IF ( JTYPE2==NVT_C4 .OR. JTYPE2==NVT_IRR ) PH_VEG(:,JTYPE) = MIN(2.5, ZALLEN_H(:))
  ENDIF
  !
  IF ( JTYPE2==NVT_NO  ) PH_VEG(:,JTYPE) = 0.1                          ! no vegetation (smooth) 
  IF ( JTYPE2==NVT_ROCK) PH_VEG(:,JTYPE) = 1.                           ! no vegetation (rocks)
  IF ( JTYPE2==NVT_SNOW) PH_VEG(:,JTYPE) = 0.01                         ! no vegetation (snow)
  !
ENDDO
!
WHERE ( ABS(PH_VEG(:,:) - XUNDEF/6) < 1 ) PH_VEG = XUNDEF ! test to avoid result with XUNDEF / 6...
!
PH_VEG(:,:) = MAX(PH_VEG(:,:),0.001)
!
IF (LHOOK) CALL DR_HOOK('MODI_VEG_HEIGHT_FROM_LAI:VEG_HEIGHT_FROM_LAI_1D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END FUNCTION VEG_HEIGHT_FROM_LAI_1D
!
!   ###########################################################
    FUNCTION VEG_HEIGHT_FROM_LAI_2D(PLAI,PH_TREE,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PH_VEG)
!   ###########################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates vegetation height from leaf
!    area index and type of vegetation
!    (most of types; forest and vineyards; grassland)
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
!!
!!      V. Masson and A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!      Druel A.    Never used ???
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_PARK,        &
                                NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD,      &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND,      &
                                NVT_SHRB, NVT_C3, NVT_C4, NVT_IRR,           &
                                NVT_GRAS, NVT_BOGR, NVT_TROG, NVT_C3W,       &
                                NVT_C3S, NVT_FLTR, NVT_FLGR
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_TREEDRAG,       ONLY : LTREEDRAG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:,:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:),   INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                  INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PVEGTYPE,1),SIZE(PVEGTYPE,2),SIZE(PVEGTYPE,3))  :: PH_VEG          ! vegetation height
!
!*      0.2    declarations of local variables
!

REAL, DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2))                  :: ZALLEN_H ! Allen formula for height
REAL, DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2))                  :: ZLAI     ! LAI for vegetated areas
!
REAL, DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2))                  :: ZAVG_H   ! averaged height
REAL                                                        :: ZZREF    ! reference height        
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_VEG_HEIGHT_FROM_LAI:VEG_HEIGHT_FROM_LAI_2D',0,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
PH_VEG(:,:,:)=XUNDEF
!
ZLAI(:,:) = PLAI(:,:)
WHERE ( PVEGTYPE(:,:,NVT_NO  ) + PVEGTYPE(:,:,NVT_ROCK) + PVEGTYPE(:,:,NVT_SNOW) < 1.) 
  ZLAI(:,:) = PLAI(:,:) / (1.-PVEGTYPE(:,:,NVT_NO)-PVEGTYPE(:,:,NVT_ROCK)-PVEGTYPE(:,:,NVT_SNOW))
END WHERE
!
ZALLEN_H(:,:) = 0.
WHERE(PLAI(:,:)/=XUNDEF)
  ZALLEN_H(:,:) = EXP((ZLAI(:,:)-3.5)/(1.3))
END WHERE
!
!
IF (NVT_PARK>0) THEN
  PH_VEG(:,:,NVT_PARK) = ZLAI(:,:) / 6.               ! irr. grassland
ELSEIF (NVT_FLGR>0) THEN
  PH_VEG(:,:,NVT_FLGR) = ZLAI(:,:) / 6. 
ENDIF
!
IF (LTREEDRAG) THEN
  PH_VEG(:,:,NVT_TEBD) = ZLAI(:,:) / 6.         ! forest
  PH_VEG(:,:,NVT_BONE) = ZLAI(:,:) / 6.         ! forest
  PH_VEG(:,:,NVT_TRBE) = ZLAI(:,:) / 6.         ! forest
  PH_VEG(:,:,NVT_TRBD) = ZLAI(:,:) / 6.         ! forest
  PH_VEG(:,:,NVT_TEBE) = ZLAI(:,:) / 6.         ! forest
  PH_VEG(:,:,NVT_TENE) = ZLAI(:,:) / 6.         ! forest
  PH_VEG(:,:,NVT_BOBD) = ZLAI(:,:) / 6.         ! forest
  PH_VEG(:,:,NVT_BOND) = ZLAI(:,:) / 6.         ! forest
  PH_VEG(:,:,NVT_SHRB) = ZLAI(:,:) / 6.         ! forest  
  IF (NVT_FLTR>0) PH_VEG(:,:,NVT_FLTR) = ZLAI(:,:) / 6.
ELSE
  PH_VEG(:,:,NVT_TEBD) = PH_TREE(:,:)           ! forest
  PH_VEG(:,:,NVT_BONE) = PH_TREE(:,:)           ! forest
  PH_VEG(:,:,NVT_TRBE) = PH_TREE(:,:)           ! forest
  PH_VEG(:,:,NVT_TRBD) = PH_TREE(:,:)           ! forest
  PH_VEG(:,:,NVT_TEBE) = PH_TREE(:,:)           ! forest
  PH_VEG(:,:,NVT_TENE) = PH_TREE(:,:)           ! forest
  PH_VEG(:,:,NVT_BOBD) = PH_TREE(:,:)           ! forest
  PH_VEG(:,:,NVT_BOND) = PH_TREE(:,:)           ! forest
  PH_VEG(:,:,NVT_SHRB) = PH_TREE(:,:)           ! forest   
  IF (NVT_FLTR>0) PH_VEG(:,:,NVT_FLTR) = PH_TREE(:,:)
END IF
PH_VEG(:,:,NVT_GRAS) = ZLAI(:,:) / 6.               ! grassland
PH_VEG(:,:,NVT_BOGR) = ZLAI(:,:) / 6.               ! boreal grassland
PH_VEG(:,:,NVT_TROG) = ZLAI(:,:) / 6.               ! tropical grassland
IF(OAGRI_TO_GRASS)THEN
  IF (NVT_C3>0) THEN
    PH_VEG(:,:,NVT_C3  ) = ZLAI(:,:) / 6.
  ELSEIF (NVT_C3W>0 .AND. NVT_C3S>0) THEN
    PH_VEG(:,:,NVT_C3W ) = ZLAI(:,:) / 6.
    PH_VEG(:,:,NVT_C3S ) = ZLAI(:,:) / 6.
  ENDIF
  PH_VEG(:,:,NVT_C4  ) = ZLAI(:,:) / 6.
  IF (NVT_IRR>0) PH_VEG(:,:,NVT_IRR ) = ZLAI(:,:) / 6.
ELSE
  IF (NVT_C3>0) THEN
    PH_VEG(:,:,NVT_C3  ) = MIN(1. , ZALLEN_H(:,:) )          ! cultures
  ELSEIF (NVT_C3W>0 .AND. NVT_C3S>0) THEN
    PH_VEG(:,:,NVT_C3W ) = MIN(2.5, ZALLEN_H(:,:) )
    PH_VEG(:,:,NVT_C3S ) = MIN(2.5, ZALLEN_H(:,:) )
  ENDIF
  PH_VEG(:,:,NVT_C4  ) = MIN(2.5, ZALLEN_H(:,:) )          ! C4 types
  IF (NVT_IRR>0) PH_VEG(:,:,NVT_IRR ) = MIN(2.5, ZALLEN_H(:,:) )          ! irrigated crops (as C4)
ENDIF
PH_VEG(:,:,NVT_NO  ) = 0.1                          ! no vegetation (smooth)
PH_VEG(:,:,NVT_ROCK) = 1.                           ! no vegetation (rocks)
PH_VEG(:,:,NVT_SNOW) = 0.01                         ! no vegetation (snow)
!
PH_VEG(:,:,:) = MAX(PH_VEG(:,:,:),0.001)
!
IF (LHOOK) CALL DR_HOOK('MODI_VEG_HEIGHT_FROM_LAI:VEG_HEIGHT_FROM_LAI_2D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END FUNCTION VEG_HEIGHT_FROM_LAI_2D
!
!
!
!   ###########################################################
    FUNCTION VEG_HEIGHT_FROM_LAI_VEGTYPE(PLAI,PH_TREE,OAGRI_TO_GRASS) RESULT(PH_VEG)
!   ###########################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates vegetation height from leaf
!    area index and type of vegetation for each patch
!    (most of types; forest and vineyards; grassland)
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
!!        F.Solmon
!!      V. Masson and A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!      
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_PARK,        &
                                NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD,      &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND,      &
                                NVT_SHRB, NVT_C3, NVT_C4, NVT_IRR,           &
                                NVT_GRAS, NVT_BOGR, NVT_TROG, NVT_C3W,       &
                                NVT_C3S, NVT_FLTR, NVT_FLGR
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_TREEDRAG,       ONLY : LTREEDRAG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:),   INTENT(IN) :: PH_TREE      ! height of trees
LOGICAL,                INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI))  :: PH_VEG          ! vegetation height
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PLAI)) :: ZALLEN_H    ! Allen formula for height
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_VEG_HEIGHT_FROM_LAI:VEG_HEIGHT_FROM_LAI_VEGTYPE',0,ZHOOK_HANDLE)
!
!
!-----------------------------------------------------------------
!
PH_VEG(:) = XUNDEF
!
ZALLEN_H(:) = XUNDEF
WHERE (PLAI(:)/= XUNDEF)
  ZALLEN_H(:) = EXP((PLAI(:)-3.5)/(1.3))
END WHERE
!
!
IF (NVT_PARK>0) THEN
  IF (PLAI(NVT_PARK)/=XUNDEF) PH_VEG(NVT_PARK) = PLAI(NVT_PARK) / 6.          ! irr. grasslands
ELSEIF (NVT_FLGR>0) THEN
  IF (PLAI(NVT_FLGR)/=XUNDEF) PH_VEG(NVT_FLGR) = PLAI(NVT_FLGR) / 6.    
ENDIF
IF (LTREEDRAG) THEN
  IF (PLAI(NVT_TEBD)/=XUNDEF) PH_VEG(NVT_TEBD) = PLAI(NVT_TEBD) / 6.        ! broadleaf forest
  IF (PLAI(NVT_BONE)/=XUNDEF) PH_VEG(NVT_BONE) = PLAI(NVT_BONE) / 6.        ! coniferous forest
  IF (PLAI(NVT_TRBE)/=XUNDEF) PH_VEG(NVT_TRBE) = PLAI(NVT_TRBE) / 6.        ! euqatorial forest
  IF (PLAI(NVT_TRBD)/=XUNDEF) PH_VEG(NVT_TRBD) = PLAI(NVT_TRBD) / 6.        ! broadleaf forest
  IF (PLAI(NVT_TEBE)/=XUNDEF) PH_VEG(NVT_TEBE) = PLAI(NVT_TEBE) / 6.        ! coniferous forest
  IF (PLAI(NVT_TENE)/=XUNDEF) PH_VEG(NVT_TENE) = PLAI(NVT_TENE) / 6.        ! euqatorial forest
  IF (PLAI(NVT_BOBD)/=XUNDEF) PH_VEG(NVT_BOBD) = PLAI(NVT_BOBD) / 6.        ! broadleaf forest
  IF (PLAI(NVT_BOND)/=XUNDEF) PH_VEG(NVT_BOND) = PLAI(NVT_BOND) / 6.        ! coniferous forest
  IF (PLAI(NVT_SHRB)/=XUNDEF) PH_VEG(NVT_SHRB) = PLAI(NVT_SHRB) / 6.        ! euqatorial forest  
  IF (NVT_FLTR>0) THEN
    IF (PLAI(NVT_FLTR)/=XUNDEF) PH_VEG(NVT_FLTR) = PLAI(NVT_FLTR) / 6.
  ENDIF
ELSE
  IF (PH_TREE(NVT_TEBD)/=XUNDEF) PH_VEG(NVT_TEBD) = PH_TREE(NVT_TEBD)          ! broadleaf forest
  IF (PH_TREE(NVT_BONE)/=XUNDEF) PH_VEG(NVT_BONE) = PH_TREE(NVT_BONE)          ! coniferous forest
  IF (PH_TREE(NVT_TRBE)/=XUNDEF) PH_VEG(NVT_TRBE) = PH_TREE(NVT_TRBE)          ! euqatorial forest
  IF (PH_TREE(NVT_TRBD)/=XUNDEF) PH_VEG(NVT_TRBD) = PH_TREE(NVT_TRBD)          ! broadleaf forest
  IF (PH_TREE(NVT_TEBE)/=XUNDEF) PH_VEG(NVT_TEBE) = PH_TREE(NVT_TEBE)          ! coniferous forest
  IF (PH_TREE(NVT_TENE)/=XUNDEF) PH_VEG(NVT_TENE) = PH_TREE(NVT_TENE)          ! euqatorial forest
  IF (PH_TREE(NVT_BOBD)/=XUNDEF) PH_VEG(NVT_BOBD) = PH_TREE(NVT_BOBD)          ! broadleaf forest
  IF (PH_TREE(NVT_BOND)/=XUNDEF) PH_VEG(NVT_BOND) = PH_TREE(NVT_BOND)          ! coniferous forest
  IF (PH_TREE(NVT_SHRB)/=XUNDEF) PH_VEG(NVT_SHRB) = PH_TREE(NVT_SHRB)          ! euqatorial forest  
  IF (NVT_FLTR>0) THEN
    IF (PH_TREE(NVT_FLTR)/=XUNDEF) PH_VEG(NVT_FLTR) = PH_TREE(NVT_FLTR)
  ENDIF
END IF
IF (PLAI(NVT_GRAS)/=XUNDEF) PH_VEG(NVT_GRAS) = PLAI(NVT_GRAS) / 6.          ! grassland
IF (PLAI(NVT_BOGR)/=XUNDEF) PH_VEG(NVT_BOGR) = PLAI(NVT_BOGR) / 6.          ! boreal grassland
IF (PLAI(NVT_TROG)/=XUNDEF) PH_VEG(NVT_TROG) = PLAI(NVT_TROG) / 6.          ! tropical grassland
IF(OAGRI_TO_GRASS)THEN
  IF (NVT_C3>0) THEN
    IF (PLAI(NVT_C3  )/=XUNDEF) PH_VEG(NVT_C3  ) = PLAI(NVT_C3)  / 6.  ! cultures
  ELSEIF (NVT_C3W>0 .AND. NVT_C3S>0) THEN
    IF (PLAI(NVT_C3W )/=XUNDEF) PH_VEG(NVT_C3W ) = PLAI(NVT_C3W) / 6.
    IF (PLAI(NVT_C3S )/=XUNDEF) PH_VEG(NVT_C3S ) = PLAI(NVT_C3S) / 6.
  ENDIF
  IF (PLAI(NVT_C4  )/=XUNDEF) PH_VEG(NVT_C4  ) = PLAI(NVT_C4)  / 6.  ! C4 types
  IF (NVT_IRR>0) THEN
    IF (PLAI(NVT_IRR )/=XUNDEF) PH_VEG(NVT_IRR ) = PLAI(NVT_IRR) / 6.  ! irrigated crops (as C4)
  ENDIF
ELSE
  IF (NVT_C3>0) THEN
    IF (ZALLEN_H(NVT_C3  )/=XUNDEF) PH_VEG(NVT_C3  ) = MIN(1. , ZALLEN_H(NVT_C3) )  ! cultures
  ELSEIF (NVT_C3W>0 .AND. NVT_C3S>0) THEN
    IF (ZALLEN_H(NVT_C3W )/=XUNDEF) PH_VEG(NVT_C3W ) = MIN(1. , ZALLEN_H(NVT_C3W) ) 
    IF (ZALLEN_H(NVT_C3S )/=XUNDEF) PH_VEG(NVT_C3S ) = MIN(1. , ZALLEN_H(NVT_C3S) )
  ENDIF
  IF (ZALLEN_H(NVT_C4  )/=XUNDEF) PH_VEG(NVT_C4  ) = MIN(2.5, ZALLEN_H(NVT_C4) )  ! C4 types
  IF (NVT_IRR>0) THEN
    IF (ZALLEN_H(NVT_IRR )/=XUNDEF) PH_VEG(NVT_IRR ) = MIN(2.5, ZALLEN_H(NVT_IRR) ) ! irrigated crops (as C4)
  ENDIF
ENDIF
PH_VEG(NVT_NO  ) = 0.1                          ! no vegetation (smooth)
PH_VEG(NVT_ROCK) = 1.                           ! no vegetation (rocks)
PH_VEG(NVT_SNOW) = 0.01                         ! no vegetation (snow)
!
PH_VEG(:) = MAX(PH_VEG(:),0.001)
!
IF (LHOOK) CALL DR_HOOK('MODI_VEG_HEIGHT_FROM_LAI:VEG_HEIGHT_FROM_LAI_VEGTYPE',1,ZHOOK_HANDLE)
!
END FUNCTION VEG_HEIGHT_FROM_LAI_VEGTYPE
!
