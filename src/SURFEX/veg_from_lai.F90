!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#######################
MODULE MODI_VEG_FROM_LAI
!#######################
!
INTERFACE VEG_FROM_LAI
!
    FUNCTION VEG_FROM_LAI_0D(PLAI,PVEGTYPE,OAGRI_TO_GRASS,NPAR_VEG_IRR_USE) RESULT(PVEG)
!
REAL,                 INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,              INTENT(IN) :: OAGRI_TO_GRASS
INTEGER,DIMENSION(:), INTENT(IN) :: NPAR_VEG_IRR_USE ! vegtype with irrigation
!
REAL                             :: PVEG         ! vegetation fraction
!
END FUNCTION VEG_FROM_LAI_0D
!
!
    FUNCTION VEG_FROM_LAI_1D(PLAI,PVEGTYPE,OAGRI_TO_GRASS,NPAR_VEG_IRR_USE) RESULT(PVEG)
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                INTENT(IN) :: OAGRI_TO_GRASS
INTEGER,DIMENSION(:), INTENT(IN) :: NPAR_VEG_IRR_USE ! vegtype with irrigation
!
REAL,   DIMENSION(SIZE(PLAI))      :: PVEG         ! vegetation fraction
!
END FUNCTION VEG_FROM_LAI_1D
!
!
! A. Druel: Never use ?
!    FUNCTION VEG_FROM_LAI_2D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PVEG)
!!
!REAL,   DIMENSION(:,:),   INTENT(IN) :: PLAI         ! Leaf area Index
!REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
!LOGICAL,                  INTENT(IN) :: OAGRI_TO_GRASS
!!
!REAL,   DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2)) :: PVEG ! vegetation fraction
!!
!END FUNCTION VEG_FROM_LAI_2D
!

    FUNCTION VEG_FROM_LAI_VEGTYPE_1D(PLAI,OAGRI_TO_GRASS) RESULT(PVEG)
!
REAL,   DIMENSION(:), INTENT(IN) :: PLAI         ! Leaf area Index for each vegtype
LOGICAL,              INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI)) :: PVEG ! vegetation fraction
!
END FUNCTION VEG_FROM_LAI_VEGTYPE_1D
!
END INTERFACE
!
END MODULE MODI_VEG_FROM_LAI
!
!   ####################################################
    FUNCTION VEG_FROM_LAI_0D(PLAI,PVEGTYPE,OAGRI_TO_GRASS,NPAR_VEG_IRR_USE) RESULT(PVEG)
!   ####################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates coverage of soil by vegetation from leaf
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
!!     
!!      R. Alkama    05/2012 : extantion from 12 to 19 vegtypes
!!      B. Decharme  05/2013  new param for equatorial forest
!!      A. Druel     02/2019  streamlines the code and adapt it to be compatible with new irrigation
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY :NVT_NO, NVT_ROCK, NVT_SNOW, NVT_TEBD,     & 
                                 NVT_BONE, NVT_TRBE, NVT_C3, NVT_C4,     &
                                 NVT_IRR, NVT_GRAS, NVT_TROG, NVT_PARK,  &
                                 NVT_TRBD, NVT_TEBE, NVT_TENE, NVT_BOBD, &
                                 NVT_BOND, NVT_BOGR, NVT_SHRB, NVT_C3W,  &
                                 NVT_C3S, NVT_FLTR, NVT_FLGR
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_AGRI,           ONLY : NVEG_IRR
!
USE MODD_REPROD_OPER,    ONLY : XEVERG_VEG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,                 INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,              INTENT(IN) :: OAGRI_TO_GRASS
INTEGER,DIMENSION(:), INTENT(IN) :: NPAR_VEG_IRR_USE ! vegtype with irrigation
!
REAL                             :: PVEG         ! vegetation fraction
!
!*      0.2    declarations of local variables
!
REAL                             :: ZLAI, ZNOVEG, ZAGRI, ZSUM1, ZSUM2, ZSUM3
INTEGER                          :: JTYPE, JTYPE2       ! loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_VEG_FROM_LAI:VEG_FROM_LAI_0D',0,ZHOOK_HANDLE)
ZLAI = PLAI
!
IF ( NVEG_IRR == 0 ) THEN
  IF ( PVEGTYPE(NVT_NO  ) + PVEGTYPE(NVT_ROCK) + PVEGTYPE(NVT_SNOW)< 1.) THEN
    ZLAI = PLAI / (1.-PVEGTYPE(NVT_NO)-PVEGTYPE(NVT_ROCK)-PVEGTYPE(NVT_SNOW))
  END IF
ELSE
  ZNOVEG=0.
  DO JTYPE = 1, SIZE(PVEGTYPE,1) ! = NVEG_IRR + NVEGTYPE
    JTYPE2 = JTYPE
    IF ( JTYPE > NVEGTYPE) JTYPE2 = NPAR_VEG_IRR_USE( JTYPE - NVEGTYPE )
    IF ( JTYPE2 == NVT_NO .OR. JTYPE2 == NVT_ROCK .OR. JTYPE2 == NVT_SNOW ) ZNOVEG = ZNOVEG + PVEGTYPE(JTYPE)
  ENDDO
  IF ( ZNOVEG < 1.) ZLAI = PLAI / (1. - ZNOVEG)
ENDIF
!
IF(OAGRI_TO_GRASS)THEN
  ZAGRI = 0.95
ELSE
  ZAGRI = (1. - EXP( -0.6 * ZLAI ))
ENDIF
!
IF ( NVEG_IRR == 0 ) THEN
  ZSUM1 = PVEGTYPE(NVT_C4)
  IF (NVT_IRR>0 .AND. NVT_C3>0) THEN
    ZSUM1 = ZSUM1 + PVEGTYPE(NVT_IRR) + PVEGTYPE(NVT_C3)
  ELSEIF (NVT_C3W>0 .AND. NVT_C3S>0) THEN
    ZSUM1 = ZSUM1 + PVEGTYPE(NVT_C3W) + PVEGTYPE(NVT_C3S)
  ENDIF
  !
  ZSUM2 = PVEGTYPE(NVT_TEBD) + PVEGTYPE(NVT_TRBD) + PVEGTYPE(NVT_TEBE) +   &
          PVEGTYPE(NVT_BOBD) + PVEGTYPE(NVT_SHRB) + PVEGTYPE(NVT_BONE) +   &
          PVEGTYPE(NVT_TENE) +  PVEGTYPE(NVT_BOND)
  IF (NVT_FLTR>0) ZSUM2 = ZSUM2 + PVEGTYPE(NVT_FLTR)
  !
  ZSUM3 = PVEGTYPE(NVT_GRAS) + PVEGTYPE(NVT_BOGR) + PVEGTYPE(NVT_TROG)
  IF (NVT_PARK>0) THEN
    ZSUM3 = ZSUM3 + PVEGTYPE(NVT_PARK)
  ELSEIF (NVT_FLGR>0) THEN
    ZSUM3 = ZSUM3 + PVEGTYPE(NVT_FLGR)
  ENDIF
  !
  PVEG = ZAGRI                      * ZSUM1   &!
         + 0.95                     * ZSUM2   &! 
         + XEVERG_VEG               * PVEGTYPE(NVT_TRBE)     &! EVER 
         + 0.95                     * ZSUM3   &! 
         + 0.                       * PVEGTYPE(NVT_NO  )     &! no vegetation (smooth)
         + 0.                       * PVEGTYPE(NVT_SNOW)     &! no vegetation (snow)
         + 0.                       * PVEGTYPE(NVT_ROCK)      ! no vegetation (rocks)  
ELSE
  ZSUM1 = 0.
  ZSUM2 = 0.
  ZSUM3 = 0.
  PVEG = 0.
  DO JTYPE = 1, SIZE(PVEGTYPE,1) ! = NVEG_IRR + NVEGTYPE
    JTYPE2 = JTYPE
    IF ( JTYPE > NVEGTYPE) JTYPE2 = NPAR_VEG_IRR_USE( JTYPE - NVEGTYPE )
    !
    IF ( JTYPE2 == NVT_C4 ) ZSUM1 = ZSUM1 + PVEGTYPE(JTYPE)
    IF (NVT_IRR > 0 .AND. NVT_C3 > 0) THEN
      IF ( JTYPE2 == NVT_IRR .OR. JTYPE2 == NVT_C3  ) ZSUM1 = ZSUM1 + PVEGTYPE(JTYPE)
    ELSEIF (NVT_C3W > 0 .AND. NVT_C3S > 0) THEN
      IF ( JTYPE2 == NVT_C3W .OR. JTYPE2 == NVT_C3S ) ZSUM1 = ZSUM1 + PVEGTYPE(JTYPE)
    ENDIF
    !
    IF ( JTYPE2 == NVT_TEBD .OR. JTYPE2 == NVT_TRBD .OR. JTYPE2 == NVT_TEBE .OR. JTYPE2 == NVT_BOBD .OR. &
         JTYPE2 == NVT_SHRB .OR. JTYPE2 == NVT_BONE .OR. JTYPE2 == NVT_TENE .OR. JTYPE2 == NVT_BOND .OR. &
         JTYPE2 == NVT_FLTR ) ZSUM2 = ZSUM2 + PVEGTYPE(JTYPE)
    !
    IF ( JTYPE2 == NVT_GRAS .OR. JTYPE2 == NVT_BOGR .OR. JTYPE2 == NVT_TROG .OR.       &
         JTYPE2 == NVT_PARK .OR. JTYPE2 == NVT_FLGR )  ZSUM3 = ZSUM3 + PVEGTYPE(JTYPE)
    !
    IF     ( JTYPE2 == NVT_TRBE ) THEN
      PVEG = PVEG + XEVERG_VEG               * PVEGTYPE(JTYPE) ! EVER
    ELSEIF ( JTYPE2 == NVT_NO   ) THEN
      PVEG = PVEG + 0.                       * PVEGTYPE(JTYPE) ! no vegetation (smooth)
    ELSEIF ( JTYPE2 == NVT_SNOW ) THEN
      PVEG = PVEG + 0.                       * PVEGTYPE(JTYPE) ! no vegetation (snow)
    ELSEIF ( JTYPE2 == NVT_ROCK ) THEN
      PVEG = PVEG + 0.                       * PVEGTYPE(JTYPE) ! vegetation (rocks)
    ENDIF
    !
  ENDDO
  !
  PVEG = PVEG  +  ZAGRI                      * ZSUM1   &
               +  0.95                       * ZSUM2   &
               +  0.95                       * ZSUM3
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODI_VEG_FROM_LAI:VEG_FROM_LAI_0D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END FUNCTION VEG_FROM_LAI_0D
!
!   ####################################################
    FUNCTION VEG_FROM_LAI_1D(PLAI,PVEGTYPE,OAGRI_TO_GRASS,NPAR_VEG_IRR_USE) RESULT(PVEG)
!   ####################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates coverage of soil by vegetation from leaf
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
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_TEBD,    & 
                                 NVT_BONE, NVT_TRBE, NVT_C3, NVT_C4,     &
                                 NVT_IRR, NVT_GRAS, NVT_TROG, NVT_PARK,  &
                                 NVT_TRBD, NVT_TEBE, NVT_TENE, NVT_BOBD, &
                                 NVT_BOND, NVT_BOGR, NVT_SHRB, NVT_C3W,  &
                                 NVT_C3S, NVT_FLTR, NVT_FLGR 
!
USE MODD_REPROD_OPER,    ONLY : XEVERG_VEG
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
REAL,   DIMENSION(:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                INTENT(IN) :: OAGRI_TO_GRASS
INTEGER,DIMENSION(:),   INTENT(IN) :: NPAR_VEG_IRR_USE ! vegtype with irrigation
!
REAL,   DIMENSION(SIZE(PLAI))      :: PVEG         ! vegetation fraction
!
!*      0.2    declarations of local variables
!
REAL,   DIMENSION(SIZE(PLAI))      :: ZLAI, ZNOVEG, ZAGRI, ZSUM1, ZSUM2, ZSUM3
INTEGER                            :: JTYPE, JTYPE2       ! loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_VEG_FROM_LAI:VEG_FROM_LAI_1D',0,ZHOOK_HANDLE)
ZLAI(:) = PLAI(:)
IF ( NVEG_IRR == 0 ) THEN
  WHERE ( PVEGTYPE(:,NVT_NO  ) + PVEGTYPE(:,NVT_ROCK) + PVEGTYPE(:,NVT_SNOW) < 1.) 
    ZLAI(:) = PLAI(:) / (1.-PVEGTYPE(:,NVT_NO)-PVEGTYPE(:,NVT_ROCK)-PVEGTYPE(:,NVT_SNOW))
  END WHERE
ELSE
  ZNOVEG(:)=0.
  DO JTYPE = 1, SIZE(PVEGTYPE,2) ! = NVEG_IRR + NVEGTYPE
    JTYPE2 = JTYPE
    IF ( JTYPE > NVEGTYPE) JTYPE2 = NPAR_VEG_IRR_USE( JTYPE - NVEGTYPE )
    IF ( JTYPE2 == NVT_NO .OR. JTYPE2 == NVT_ROCK .OR. JTYPE2 == NVT_SNOW ) ZNOVEG(:) = ZNOVEG(:) + PVEGTYPE(:,JTYPE)
  ENDDO
  WHERE ( ZNOVEG(:) < 1.) ZLAI(:) = PLAI(:) / (1. - ZNOVEG(:) )
ENDIF
!
IF(OAGRI_TO_GRASS)THEN
  ZAGRI(:) = 0.95
ELSE
  ZAGRI(:) = (1. - EXP( -0.6 * ZLAI(:) ))
ENDIF
!
IF ( NVEG_IRR == 0 ) THEN
  ZSUM1(:) = PVEGTYPE(:,NVT_C4)
  IF (NVT_IRR>0 .AND. NVT_C3>0) THEN
    ZSUM1(:) = ZSUM1(:) + PVEGTYPE(:,NVT_IRR) + PVEGTYPE(:,NVT_C3)
  ELSEIF (NVT_C3W>0 .AND. NVT_C3S>0) THEN
    ZSUM1(:) = ZSUM1(:) + PVEGTYPE(:,NVT_C3W) + PVEGTYPE(:,NVT_C3S)
  ENDIF
  !
  ZSUM2(:) = PVEGTYPE(:,NVT_TEBD) + PVEGTYPE(:,NVT_TRBD) + PVEGTYPE(:,NVT_TEBE) +   &  
             PVEGTYPE(:,NVT_BOBD) + PVEGTYPE(:,NVT_SHRB) + PVEGTYPE(:,NVT_BONE) +   &
             PVEGTYPE(:,NVT_TENE) +  PVEGTYPE(:,NVT_BOND)
  IF (NVT_FLTR>0) ZSUM2(:) = ZSUM2(:) + PVEGTYPE(:,NVT_FLTR)
  !
  ZSUM3(:) = PVEGTYPE(:,NVT_GRAS) + PVEGTYPE(:,NVT_BOGR) + PVEGTYPE(:,NVT_TROG)
  IF (NVT_PARK>0) THEN
    ZSUM3(:) = ZSUM3(:) + PVEGTYPE(:,NVT_PARK)
  ELSEIF (NVT_FLGR>0) THEN
    ZSUM3(:) = ZSUM3(:) + PVEGTYPE(:,NVT_FLGR)
  ENDIF
  !
  PVEG(:) = ZAGRI(:)                * ZSUM1(:) &
         + 0.95                     * ZSUM2(:) &
         + XEVERG_VEG               * PVEGTYPE(:,NVT_TRBE)     &! EVER 
         + 0.95                     * ZSUM3(:) &
         + 0.                       * PVEGTYPE(:,NVT_NO  )     &! no vegetation (smooth)
         + 0.                       * PVEGTYPE(:,NVT_SNOW)     &! no vegetation (snow)
         + 0.                       * PVEGTYPE(:,NVT_ROCK)      ! no vegetation (rocks)
  !
ELSE
  ZSUM1(:) = 0.
  ZSUM2(:) = 0.
  ZSUM3(:) = 0.
  PVEG(:) = 0.
  DO JTYPE = 1, SIZE(PVEGTYPE,2) ! = NVEG_IRR + NVEGTYPE
    JTYPE2 = JTYPE
    IF ( JTYPE > NVEGTYPE) JTYPE2 = NPAR_VEG_IRR_USE( JTYPE - NVEGTYPE )
    !
    IF ( JTYPE2 == NVT_C4 ) ZSUM1(:) = ZSUM1(:) + PVEGTYPE(:,JTYPE)
    IF (NVT_IRR > 0 .AND. NVT_C3 > 0) THEN
      IF ( JTYPE2 == NVT_IRR .OR. JTYPE2 == NVT_C3  ) ZSUM1(:) = ZSUM1(:) + PVEGTYPE(:,JTYPE)
    ELSEIF (NVT_C3W > 0 .AND. NVT_C3S > 0) THEN
      IF ( JTYPE2 == NVT_C3W .OR. JTYPE2 == NVT_C3S ) ZSUM1(:) = ZSUM1(:) + PVEGTYPE(:,JTYPE)
    ENDIF
    !
    IF ( JTYPE2 == NVT_TEBD .OR. JTYPE2 == NVT_TRBD .OR. JTYPE2 == NVT_TEBE .OR. JTYPE2 == NVT_BOBD .OR. &
         JTYPE2 == NVT_SHRB .OR. JTYPE2 == NVT_BONE .OR. JTYPE2 == NVT_TENE .OR. JTYPE2 == NVT_BOND .OR. &
         JTYPE2 == NVT_FLTR ) ZSUM2(:) = ZSUM2(:) + PVEGTYPE(:,JTYPE)
    !
    IF ( JTYPE2 == NVT_GRAS .OR. JTYPE2 == NVT_BOGR .OR. JTYPE2 == NVT_TROG .OR.       &
         JTYPE2 == NVT_PARK .OR. JTYPE2 == NVT_FLGR )  ZSUM3(:) = ZSUM3(:) + PVEGTYPE(:,JTYPE)
    !
    IF     ( JTYPE2 == NVT_TRBE ) THEN
      PVEG(:) = PVEG(:) + XEVERG_VEG               * PVEGTYPE(:,JTYPE) ! EVER
    ELSEIF ( JTYPE2 == NVT_NO   ) THEN
      PVEG(:) = PVEG(:) + 0.                       * PVEGTYPE(:,JTYPE) ! no vegetation (smooth)
    ELSEIF ( JTYPE2 == NVT_SNOW ) THEN
      PVEG(:) = PVEG(:) + 0.                       * PVEGTYPE(:,JTYPE) ! no vegetation (snow)
    ELSEIF ( JTYPE2 == NVT_ROCK ) THEN
      PVEG(:) = PVEG(:) + 0.                       * PVEGTYPE(:,JTYPE) ! vegetation (rocks)
    ENDIF
    !
  ENDDO
  !
  PVEG(:) = PVEG(:)  +  ZAGRI(:)                   * ZSUM1(:)   &
                     +  0.95                       * ZSUM2(:)   &
                     +  0.95                       * ZSUM3(:)
  !
ENDIF
! 
IF (LHOOK) CALL DR_HOOK('MODI_VEG_FROM_LAI:VEG_FROM_LAI_1D',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END FUNCTION VEG_FROM_LAI_1D
!
!   ####################################################
    FUNCTION VEG_FROM_LAI_2D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PVEG)
!   ####################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates coverage of soil by vegetation from leaf
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
!!      Druel A.   02/2019 - Never Used ?
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_TEBD,    & 
                                 NVT_BONE, NVT_TRBE, NVT_C3, NVT_C4,     &
                                 NVT_IRR, NVT_GRAS, NVT_TROG, NVT_PARK,  &
                                 NVT_TRBD, NVT_TEBE, NVT_TENE, NVT_BOBD, &
                                 NVT_BOND, NVT_BOGR, NVT_SHRB, NVT_C3W,  &
                                 NVT_C3S, NVT_FLTR, NVT_FLGR 
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_REPROD_OPER,    ONLY : XEVERG_VEG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:,:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                  INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2)) :: PVEG ! vegetation fraction
!
!*      0.2    declarations of local variables
!
REAL,   DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2)) :: ZLAI, ZAGRI, ZSUM1, ZSUM2, ZSUM3
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_VEG_FROM_LAI:VEG_FROM_LAI_2D',0,ZHOOK_HANDLE)
ZLAI(:,:) = PLAI(:,:)
WHERE ( PVEGTYPE(:,:,NVT_NO  ) + PVEGTYPE(:,:,NVT_ROCK) + PVEGTYPE(:,:,NVT_SNOW) < 1.) 
  ZLAI(:,:) = PLAI(:,:) / (1.-PVEGTYPE(:,:,NVT_NO)-PVEGTYPE(:,:,NVT_ROCK)-PVEGTYPE(:,:,NVT_SNOW))
END WHERE
!
PVEG(:,:) = XUNDEF
!
IF(OAGRI_TO_GRASS)THEN
  ZAGRI(:,:) = 0.95
ELSE
  WHERE (PLAI(:,:) /= XUNDEF)
        ZAGRI(:,:) = (1. - EXP( -0.6 * ZLAI(:,:) ))
  ELSEWHERE
        ZAGRI(:,:) = XUNDEF
  ENDWHERE
ENDIF
!
ZSUM1(:,:) = PVEGTYPE(:,:,NVT_C4)
IF (NVT_IRR>0 .AND. NVT_C3>0) THEN
  ZSUM1(:,:) = ZSUM1(:,:) + PVEGTYPE(:,:,NVT_IRR) + PVEGTYPE(:,:,NVT_C3)
ELSEIF (NVT_C3W>0 .AND. NVT_C3S>0) THEN
  ZSUM1(:,:) = ZSUM1(:,:) + PVEGTYPE(:,:,NVT_C3W) + PVEGTYPE(:,:,NVT_C3S)
ENDIF
!
ZSUM2(:,:) = PVEGTYPE(:,:,NVT_TEBD) + PVEGTYPE(:,:,NVT_TRBD) + PVEGTYPE(:,:,NVT_TEBE) +   &   
             PVEGTYPE(:,:,NVT_BOBD) + PVEGTYPE(:,:,NVT_SHRB) + PVEGTYPE(:,:,NVT_BONE) +   &
             PVEGTYPE(:,:,NVT_TENE) +  PVEGTYPE(:,:,NVT_BOND)
IF (NVT_FLTR>0) ZSUM2(:,:) = ZSUM2(:,:) + PVEGTYPE(:,:,NVT_FLTR)
!
ZSUM3(:,:) = PVEGTYPE(:,:,NVT_GRAS) + PVEGTYPE(:,:,NVT_BOGR) + PVEGTYPE(:,:,NVT_TROG)
IF (NVT_PARK>0) THEN
  ZSUM3(:,:) = ZSUM3(:,:) + PVEGTYPE(:,:,NVT_PARK)
ELSEIF (NVT_FLGR>0) THEN
  ZSUM3(:,:) = ZSUM3(:,:) + PVEGTYPE(:,:,NVT_FLGR)
ENDIF
!
WHERE (PLAI(:,:) /= XUNDEF)
PVEG(:,:) = ZAGRI(:,:)               * ZSUM1(:,:) &
       + 0.95                        * ZSUM2(:,:) &
       + XEVERG_VEG                  * PVEGTYPE(:,:,NVT_TRBE)     &! EVER 
       + 0.95                        * ZSUM3(:,:) &
       + 0.                          * PVEGTYPE(:,:,NVT_NO  )     &! no vegetation (smooth)
       + 0.                          * PVEGTYPE(:,:,NVT_SNOW)     &! no vegetation (snow)
       + 0.                          * PVEGTYPE(:,:,NVT_ROCK)      ! no vegetation (rocks)
END WHERE
IF (LHOOK) CALL DR_HOOK('MODI_VEG_FROM_LAI:VEG_FROM_LAI_2D',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END FUNCTION VEG_FROM_LAI_2D
!
!
!
!   ####################################################
    FUNCTION VEG_FROM_LAI_VEGTYPE_1D(PLAI,OAGRI_TO_GRASS) RESULT(PVEG)
!   ####################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates coverage of soil by vegetation from leaf
!    area index and type of vegetation for each vegetation patch
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
!!    F.Solmon/V.Masson
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
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_TEBD,    & 
                                 NVT_BONE, NVT_TRBE, NVT_C3, NVT_C4,     &
                                 NVT_IRR, NVT_GRAS, NVT_TROG, NVT_PARK,  &
                                 NVT_TRBD, NVT_TEBE, NVT_TENE, NVT_BOBD, &
                                 NVT_BOND, NVT_BOGR, NVT_SHRB, NVT_C3W,  &
                                 NVT_C3S, NVT_FLTR, NVT_FLGR 

USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_REPROD_OPER,    ONLY : XEVERG_VEG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:), INTENT(IN) :: PLAI         ! Leaf area Index
LOGICAL,              INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI)) :: PVEG ! vegetation fraction
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_VEG_FROM_LAI:VEG_FROM_LAI_VEGTYPE_1D',0,ZHOOK_HANDLE)
PVEG(:) = XUNDEF
!
IF(OAGRI_TO_GRASS)THEN
  PVEG(NVT_C4  )= 0.95
  IF (NVT_IRR>0) THEN
    PVEG(NVT_IRR )= 0.95
  ENDIF
  IF (NVT_C3>0) THEN
    PVEG(NVT_C3  )= 0.95
  ELSEIF (NVT_C3W>0 .AND. NVT_C3S>0) THEN
    PVEG(NVT_C3W )= 0.95
    PVEG(NVT_C3S )= 0.95
  ENDIF
ELSE
  IF (PLAI(NVT_C4)/=XUNDEF) PVEG(NVT_C4  )= 1. - EXP( -0.6 * PLAI(NVT_C4  ) )
  IF (NVT_IRR>0) THEN
    IF (PLAI(NVT_IRR)/=XUNDEF) PVEG(NVT_IRR )= 1. - EXP( -0.6 * PLAI(NVT_IRR ) )
  ENDIF
  IF (NVT_C3>0) THEN
    IF (PLAI(NVT_C3)/=XUNDEF) PVEG(NVT_C3  )= 1. - EXP( -0.6 * PLAI(NVT_C3  ) )
  ELSEIF (NVT_C3W>0 .AND. NVT_C3S>0) THEN
    IF (PLAI(NVT_C3W)/=XUNDEF) PVEG(NVT_C3W )= 1. - EXP( -0.6 * PLAI(NVT_C3W ) )
    IF (PLAI(NVT_C3S)/=XUNDEF) PVEG(NVT_C3S )= 1. - EXP( -0.6 * PLAI(NVT_C3S ) )
  ENDIF
ENDIF
!
PVEG(NVT_TEBD)=  0.95
PVEG(NVT_TRBD)=  0.95
PVEG(NVT_TEBE)=  0.95
PVEG(NVT_BOBD)=  0.95
PVEG(NVT_SHRB)=  0.95
PVEG(NVT_BONE)=  0.95
PVEG(NVT_TENE)=  0.95
PVEG(NVT_BOND)=  0.95
IF (NVT_FLTR>0) THEN
  PVEG(NVT_FLTR)=  0.95
ENDIF
PVEG(NVT_TRBE)=  XEVERG_VEG
!
PVEG(NVT_GRAS)=  0.95
PVEG(NVT_BOGR)=  0.95
PVEG(NVT_TROG)=  0.95
IF (NVT_PARK>0) THEN
  PVEG(NVT_PARK)=  0.95
ELSEIF (NVT_FLGR>0) THEN
  PVEG(NVT_FLGR)=  0.95
ENDIF
!
PVEG(NVT_NO  )= 0.
PVEG(NVT_SNOW)= 0.
PVEG(NVT_ROCK)= 0.  
IF (LHOOK) CALL DR_HOOK('MODI_VEG_FROM_LAI:VEG_FROM_LAI_VEGTYPE_1D',1,ZHOOK_HANDLE)
!
END FUNCTION VEG_FROM_LAI_VEGTYPE_1D
!
!--------------------------------------------
!
