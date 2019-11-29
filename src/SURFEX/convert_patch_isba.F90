!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CONVERT_PATCH_ISBA (DTCO, DTV, IO, KMONTH, KDAY, KDEC, KDEC2, PCOVER, OCOVER,              &
                                     OAGRIP, OECOSG, OIRRIGMODE, HSFTYPE, KPATCH, KK, PK, PEK, OFIX, OTIME, &
                                     OMEB, OIRR, OALB, OUPDATE_ALB, PSOILGRID, PWG1, PWSAT, PPERM )
!     ##############################################################
!
!!**** *CONVERT_PATCH_ISBA* 
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
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
!!    S. Faroux        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    16/11/10
!!      V. Masson    04/14 Garden and Greenroofs can only be initialized by ecoclimap 
!!                         in this routine (not from user specified parameters from
!!                         the nature tile, as the number of points is not the same)
!!    B. Decharme  04/2013  Add CDGAVG (method to average depth)
!!                          Soil depth = Root depth with ISBA-DF
!!                           except for bare soil pft (but limited to 1m)
!!                          With TR_ML (new radiative transfert) and modis
!!                           albedo, UV albedo not defined (conserv nrj when
!!                           coupled to atmosphere)
!!    P Samuelsson  10/2014  MEB
!!    J. ETchanchu  01/2018  add irrigation parameters
!!    A. Druel      02/2019  Adapt the code to be compatible with irrigation (mainly for CALL and 
!!                            new parameters)
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
!
USE MODD_ISBA_n, ONLY : ISBA_P_t, ISBA_PE_t, ISBA_K_t
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE, NVT_NO, NVT_ROCK, NVT_SNOW, JPCOVER
!
USE MODD_SURF_PAR, ONLY : XUNDEF, NUNDEF
USE MODD_TYPE_DATE_SURF
!
!
USE MODD_DATA_COVER,     ONLY : XDATA_LAI, XDATA_H_TREE,                &
                                XDATA_VEG, XDATA_Z0, XDATA_Z0_O_Z0H,    &
                                XDATA_EMIS_ECO, XDATA_GAMMA, XDATA_CV,  &
                                XDATA_RGL, XDATA_RSMIN,                 &
                                XDATA_ALBNIR_VEG, XDATA_ALBVIS_VEG,     &
                                XDATA_ALBUV_VEG,                        &
                                XDATA_ALB_VEG_NIR, XDATA_ALB_VEG_VIS,   &
                                XDATA_ALB_SOIL_NIR, XDATA_ALB_SOIL_VIS, &
                                XDATA_GMES, XDATA_BSLAI, XDATA_LAIMIN,  &
                                XDATA_SEFOLD, XDATA_GC, XDATA_WRMAX_CF, &
                                XDATA_STRESS,                           &
                                XDATA_DMAX, XDATA_F2I, XDATA_RE25,      &
                                XDATA_CE_NITRO, XDATA_CF_NITRO,         &
                                XDATA_CNA_NITRO, XDATA_DICE,            &
                                XDATA_GMES_ST, XDATA_BSLAI_ST,          &
                                XDATA_SEFOLD_ST, XDATA_GC_ST,           &
                                XDATA_DMAX_ST, XDATA_WATSUP,            &
                                XDATA_GNDLITTER, XDATA_Z0LITTER, XDATA_H_VEG,  &
                                TDATA_SEED, TDATA_REAP,XDATA_IRRIGTYPE, &
                                XDATA_IRRIGFRAC, XDATA_IRRIGFREQ,       &
                                XDATA_IRRIGTIME, XDATA_F2THRESHOLD,     &
                                XDATA_ROOT_DEPTH, XDATA_GROUND_DEPTH,   &
                                XDATA_ROOT_EXTINCTION, XDATA_ROOT_LIN
!   
!
USE MODD_TREEDRAG,       ONLY : LTREEDRAG
!
USE MODI_AV_PGD_PARAM
USE MODI_AV_PGD_1P
USE MODI_SOIL_ALBEDO
!
USE MODD_AGRI,           ONLY : NVEG_IRR, LMULTI_SEASON
USE MODI_VEGTYPE_TO_PATCH_IRRIG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_ISBA_t), INTENT(IN)     :: DTV
TYPE(ISBA_OPTIONS_t), INTENT(IN)  :: IO
!
INTEGER,                INTENT(IN)    :: KMONTH    ! Month
INTEGER,                INTENT(IN)    :: KDAY      ! Day
INTEGER,                INTENT(IN)    :: KDEC
INTEGER,                INTENT(IN)    :: KDEC2
REAL, DIMENSION(:,:),   INTENT(IN)    :: PCOVER
LOGICAL, DIMENSION(:),  INTENT(IN)    :: OCOVER
LOGICAL,                INTENT(IN)    :: OAGRIP
LOGICAL,                INTENT(IN)    :: OECOSG
LOGICAL,                INTENT(IN)    :: OIRRIGMODE
CHARACTER(LEN=*),       INTENT(IN)    :: HSFTYPE ! nature / garden
INTEGER, INTENT(IN) :: KPATCH
!
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
LOGICAL, INTENT(IN) :: OFIX
LOGICAL, INTENT(IN) :: OTIME
LOGICAL, INTENT(IN) :: OMEB
LOGICAL, INTENT(IN) :: OIRR
LOGICAL, INTENT(IN) :: OALB
LOGICAL, INTENT(IN) :: OUPDATE_ALB
!
REAL, DIMENSION(:),   OPTIONAL, INTENT(IN)   :: PWG1
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(IN)   :: PWSAT
REAL, DIMENSION(:),   OPTIONAL, INTENT(IN)   :: PPERM
!
REAL, DIMENSION(:)  ,   OPTIONAL, INTENT(IN)    :: PSOILGRID
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL,    DIMENSION(:), ALLOCATABLE :: ZWORKI
INTEGER, DIMENSION(:), ALLOCATABLE :: ISEASON
 CHARACTER(LEN=3)  :: YTREE, YNAT, YLAI, YVEG, YBAR, YDIF
!
INTEGER               :: JLAYER    ! loop counter on layers
INTEGER               :: JVEG, JK  ! loop counter on vegtypes
!
LOGICAL               :: GDATA     ! Flag where initialization can be done
!                                  ! either with ecoclimap of data fields specified
!                                  ! by user on the natural points (GDTA=T)
!                                  ! For fields in town, only ecoclimap option
!                                  ! is treated in this routine (GDATA=F)
INTEGER               :: JJ        ! loop counter
!
INTEGER               :: ISIZE_LMEB_PATCH  ! Number of patches with MEB=true
!
REAL, ALLOCATABLE, DIMENSION(:) :: ZH_VEG
!
!
!*    0.3    Declaration of namelists
!            ------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('CONVERT_PATCH_ISBA',0,ZHOOK_HANDLE)
!
IF (ASSOCIATED(DTCO%XDATA_WEIGHT)) DEALLOCATE(DTCO%XDATA_WEIGHT)
!
IF (HSFTYPE=='NAT') THEN
  YNAT='NAT'
  YTREE='TRE'
  YLAI='LAI'
  YVEG='VEG'
  YBAR='BAR'
  YDIF='DVG'
  GDATA=.TRUE.
  ISIZE_LMEB_PATCH = COUNT(IO%LMEB_PATCH(:))
ELSEIF (HSFTYPE=='GRD') THEN
  YNAT='GRD'
  YTREE='GRT'
  YLAI='GRL'
  YVEG='GRV'
  YBAR='GRB'
  YDIF='GDV'
  GDATA=.FALSE.
  ISIZE_LMEB_PATCH = 0
ENDIF
!
IF (OFIX) THEN
  !
  !* soil layers and root fraction
!  -----------------------------
  !
  !   compute soil layers (and root fraction if DIF)
  !
  CALL SET_GRID_PARAM(SIZE(PK%XDG,1),SIZE(PK%XDG,2))
!
!        D ICE
!        -----
!
  IF (IO%CISBA/='DIF') THEN
    IF (GDATA .AND. ANY(DTV%LDATA_DICE)) THEN
      CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                        PK%XD_ICE,DTV%XPAR_VEGTYPE2,DTV%XPAR_DICE,DTV%NPAR_VEG_IRR_USE,YNAT,'ARI',PK%NR_P,IO%NPATCH,KPATCH)
    ELSE
      CALL AV_PGD_1P(DTCO, PK%XD_ICE,PCOVER,XDATA_DICE(:,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YNAT,'ARI',OCOVER,&
                PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
    ENDIF
  ENDIF
!
  IF (GDATA .AND. ANY(DTV%LDATA_Z0_O_Z0H)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PK%XZ0_O_Z0H,DTV%XPAR_VEGTYPE2,DTV%XPAR_Z0_O_Z0H,DTV%NPAR_VEG_IRR_USE,YNAT,'ARI',PK%NR_P,IO%NPATCH,KPATCH)
  ELSE
    CALL AV_PGD_1P(DTCO, PK%XZ0_O_Z0H,PCOVER,XDATA_Z0_O_Z0H,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YNAT,'ARI',OCOVER,&
                PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
  ENDIF
!
  IF (IO%CPHOTO/='NON'.OR.LTREEDRAG) THEN
    IF (GDATA .AND. ANY(DTV%LDATA_H_TREE)) THEN
      CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PK%XH_TREE,DTV%XPAR_VEGTYPE2,DTV%XPAR_H_TREE,DTV%NPAR_VEG_IRR_USE,YTREE,'ARI',PK%NR_P,IO%NPATCH,KPATCH)
    ELSE
      CALL AV_PGD_1P(DTCO, PK%XH_TREE,PCOVER,XDATA_H_TREE(:,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YTREE,'ARI',OCOVER,&
                PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
    ENDIF
  ENDIF
!
  IF (IO%CPHOTO/='NON') THEN
    !
    IF (SIZE(PK%XRE25)>0) THEN
      IF (GDATA .AND. ANY(DTV%LDATA_RE25)) THEN
        CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PK%XRE25,DTV%XPAR_VEGTYPE2,DTV%XPAR_RE25,DTV%NPAR_VEG_IRR_USE,YNAT,'ARI',PK%NR_P,IO%NPATCH,KPATCH)      
      ELSE
        CALL AV_PGD_1P(DTCO, PK%XRE25,PCOVER,XDATA_RE25,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YNAT,'ARI',OCOVER,&
                PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
      ENDIF
    ENDIF
    !
    IF (SIZE(PK%XDMAX)>0) THEN
      IF (GDATA .AND. ANY(DTV%LDATA_DMAX)) THEN
        CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PK%XDMAX,DTV%XPAR_VEGTYPE2,DTV%XPAR_DMAX,DTV%NPAR_VEG_IRR_USE,YTREE,'ARI',PK%NR_P,IO%NPATCH,KPATCH)
      ELSE
        CALL AV_PGD_1P(DTCO, PK%XDMAX,PCOVER,XDATA_DMAX_ST,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YTREE,'ARI',OCOVER,&
                PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
      ENDIF
    ENDIF
    !
  ENDIF
!
ENDIF
!
IF (OTIME) THEN
!
 IF (.NOT.OUPDATE_ALB) THEN
!   VEG
!   ----
  IF (GDATA .AND. ANY(DTV%LDATA_VEG)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, PEK%XVEG,DTV%XPAR_VEGTYPE2,DTV%XPAR_VEG(:,KDEC2,:),&
            DTV%NPAR_VEG_IRR_USE,YNAT,'ARI',PK%NR_P,IO%NPATCH,KPATCH)
  ELSE
    CALL AV_PGD_1P(DTCO, PEK%XVEG,PCOVER,XDATA_VEG(:,KDEC,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YNAT,'ARI',OCOVER,&
                PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
  ENDIF
!
!   LAI
!   ----
  IF (GDATA .AND. ANY(DTV%LDATA_LAI)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XLAI,DTV%XPAR_VEGTYPE2,DTV%XPAR_LAI(:,KDEC2,:),DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
  ELSE
    CALL AV_PGD_1P(DTCO, PEK%XLAI,PCOVER,XDATA_LAI(:,KDEC,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
  ENDIF
!
!           EMIS
!           ----
!emis needs VEG by vegtypes is changed at this step
  IF (GDATA .AND. ANY(DTV%LDATA_EMIS)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XEMIS ,DTV%XPAR_VEGTYPE2,DTV%XPAR_EMIS(:,KDEC2,:),DTV%NPAR_VEG_IRR_USE,YNAT,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH)
  ELSE
    CALL AV_PGD_1P(DTCO, PEK%XEMIS ,PCOVER ,XDATA_EMIS_ECO (:,KDEC,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YNAT,'ARI',OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
  ENDIF
!
!    Z0V
!    ----
  IF (GDATA .AND. ANY(DTV%LDATA_Z0)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XZ0,DTV%XPAR_VEGTYPE2,DTV%XPAR_Z0(:,KDEC2,:),DTV%NPAR_VEG_IRR_USE,YNAT,'CDN',&
                      PK%NR_P,IO%NPATCH,KPATCH)
  ELSE
    CALL AV_PGD_1P(DTCO, PEK%XZ0 ,PCOVER ,XDATA_Z0 (:,KDEC,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YNAT,'CDN',OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
  ENDIF
!
 ENDIF

  IF (GDATA .AND. ANY(DTV%LDATA_ALBNIR_VEG)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XALBNIR_VEG,DTV%XPAR_VEGTYPE2,DTV%XPAR_ALBNIR_VEG(:,KDEC2,:),DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
  ELSEIF (IO%CALBEDO=='CM13') THEN
    CALL AV_PGD_1P(DTCO, PEK%XALBNIR_VEG,PCOVER,XDATA_ALB_VEG_NIR(:,KDEC,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE, &
            YVEG,'ARI',OCOVER,PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)    
  ELSE
    CALL AV_PGD_1P(DTCO, PEK%XALBNIR_VEG,PCOVER,XDATA_ALBNIR_VEG,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
  ENDIF
!
  IF (GDATA .AND. ANY(DTV%LDATA_ALBVIS_VEG)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XALBVIS_VEG,DTV%XPAR_VEGTYPE2,DTV%XPAR_ALBVIS_VEG(:,KDEC2,:),DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
  ELSEIF (IO%CALBEDO=='CM13') THEN
    CALL AV_PGD_1P(DTCO, PEK%XALBVIS_VEG,PCOVER,XDATA_ALB_VEG_VIS(:,KDEC,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE, &
            YVEG,'ARI',OCOVER,PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)      
  ELSE
    CALL AV_PGD_1P(DTCO, PEK%XALBVIS_VEG,PCOVER,XDATA_ALBVIS_VEG,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
  ENDIF
!
  IF ((IO%CALBEDO=='CM13'.OR.IO%LTR_ML)) THEN
    PEK%XALBUV_VEG(:)=PEK%XALBVIS_VEG(:)
  ELSEIF (GDATA .AND. ANY(DTV%LDATA_ALBUV_VEG)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XALBUV_VEG,DTV%XPAR_VEGTYPE2,DTV%XPAR_ALBUV_VEG(:,KDEC2,:),DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
  ELSE
    CALL AV_PGD_1P(DTCO, PEK%XALBUV_VEG,PCOVER,XDATA_ALBUV_VEG,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
  ENDIF
!
 IF (.NOT.OUPDATE_ALB) THEN
!        Other parameters
!        ----------------
  IF( SIZE(PEK%XRSMIN)>0) THEN
    IF (GDATA .AND. ANY(DTV%LDATA_RSMIN)) THEN
      CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XRSMIN,DTV%XPAR_VEGTYPE2,DTV%XPAR_RSMIN,DTV%NPAR_VEG_IRR_USE,YLAI,'INV',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
    ELSE
      CALL AV_PGD_1P(DTCO, PEK%XRSMIN,PCOVER,XDATA_RSMIN,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YLAI,'INV',&
              OCOVER,PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
    ENDIF
  ENDIF
!
  IF (GDATA .AND. ANY(DTV%LDATA_GAMMA)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XGAMMA,DTV%XPAR_VEGTYPE2,DTV%XPAR_GAMMA,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
  ELSE
    CALL AV_PGD_1P(DTCO, PEK%XGAMMA,PCOVER,XDATA_GAMMA,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
  ENDIF
!
  IF (GDATA .AND. ANY(DTV%LDATA_WRMAX_CF)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XWRMAX_CF,DTV%XPAR_VEGTYPE2,DTV%XPAR_WRMAX_CF,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
  ELSE
    CALL AV_PGD_1P(DTCO, PEK%XWRMAX_CF,PCOVER,XDATA_WRMAX_CF,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
  ENDIF
!
  IF (GDATA .AND. ANY(DTV%LDATA_RGL)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XRGL,DTV%XPAR_VEGTYPE2,DTV%XPAR_RGL,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
  ELSE
    CALL AV_PGD_1P(DTCO, PEK%XRGL,PCOVER,XDATA_RGL,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
  ENDIF
!
  IF (GDATA .AND. ANY(DTV%LDATA_CV)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XCV,DTV%XPAR_VEGTYPE2,DTV%XPAR_CV,DTV%NPAR_VEG_IRR_USE,YVEG,'INV',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
  ELSE
    CALL AV_PGD_1P(DTCO, PEK%XCV,PCOVER,XDATA_CV,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'INV',OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
  ENDIF
!
  IF (ISIZE_LMEB_PATCH>0 .OR. IO%CPHOTO/='NON') THEN

    IF( SIZE(PEK%XBSLAI)>0) THEN
      IF (GDATA .AND. ANY(DTV%LDATA_BSLAI)) THEN
        CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XBSLAI,DTV%XPAR_VEGTYPE2,DTV%XPAR_BSLAI,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
      ELSE
        CALL AV_PGD_1P(DTCO, PEK%XBSLAI,PCOVER,XDATA_BSLAI_ST,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
                PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
      ENDIF
    ENDIF
  ENDIF
!
  IF (IO%CPHOTO/='NON') THEN
  !
    IF (SIZE(PEK%XLAIMIN)>0) THEN
      IF (GDATA .AND. ANY(DTV%LDATA_LAIMIN)) THEN
        CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XLAIMIN,DTV%XPAR_VEGTYPE2,DTV%XPAR_LAIMIN,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
      ELSE
        CALL AV_PGD_1P(DTCO, PEK%XLAIMIN,PCOVER,XDATA_LAIMIN,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
                PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
      ENDIF
    ENDIF    
  !
    IF (SIZE(PEK%XSEFOLD)>0) THEN
      IF (GDATA .AND. ANY(DTV%LDATA_SEFOLD)) THEN
        CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XSEFOLD,DTV%XPAR_VEGTYPE2,DTV%XPAR_SEFOLD,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
      ELSE
        CALL AV_PGD_1P(DTCO, PEK%XSEFOLD,PCOVER,XDATA_SEFOLD_ST,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
                PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
      ENDIF
    ENDIF
    !
    IF ( SIZE(PEK%XGMES)>0) THEN
      IF (GDATA .AND. ANY(DTV%LDATA_GMES)) THEN
        CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XGMES,DTV%XPAR_VEGTYPE2,DTV%XPAR_GMES,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
      ELSE
        CALL AV_PGD_1P(DTCO, PEK%XGMES,PCOVER,XDATA_GMES_ST,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
                PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
      ENDIF
    ENDIF
    !
    IF ( SIZE(PEK%XGC)>0) THEN
      IF (GDATA .AND. ANY(DTV%LDATA_GC)) THEN
        CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                          PEK%XGC,DTV%XPAR_VEGTYPE2,DTV%XPAR_GC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                          PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
      ELSE
        CALL AV_PGD_1P(DTCO, PEK%XGC,PCOVER,XDATA_GC_ST,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
                PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
      ENDIF
    ENDIF
    !
    IF (SIZE(PEK%XF2I)>0) THEN
      IF (GDATA .AND. ANY(DTV%LDATA_F2I)) THEN
        CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                    PEK%XF2I,DTV%XPAR_VEGTYPE2,DTV%XPAR_F2I,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                    PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
      ELSE
        CALL AV_PGD_1P(DTCO, PEK%XF2I,PCOVER,XDATA_F2I,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
                PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
      ENDIF
    ENDIF
    !
    IF (IO%CPHOTO=='NIT' .OR. IO%CPHOTO=='NCB') THEN
      !
      IF (SIZE(PEK%XCE_NITRO)>0) THEN
        IF (GDATA .AND. ANY(DTV%LDATA_CE_NITRO)) THEN
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                    PEK%XCE_NITRO,DTV%XPAR_VEGTYPE2,DTV%XPAR_CE_NITRO,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                    PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
        ELSE
          CALL AV_PGD_1P(DTCO, PEK%XCE_NITRO,PCOVER,XDATA_CE_NITRO,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
                  PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
        ENDIF
      ENDIF
      !
      IF (SIZE(PEK%XCF_NITRO)>0) THEN
        IF (GDATA .AND. ANY(DTV%LDATA_CF_NITRO)) THEN
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                    PEK%XCF_NITRO,DTV%XPAR_VEGTYPE2,DTV%XPAR_CF_NITRO,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                    PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
        ELSE
          CALL AV_PGD_1P(DTCO, PEK%XCF_NITRO,PCOVER,XDATA_CF_NITRO,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
                  PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
        ENDIF
      ENDIF
      !
      IF (SIZE(PEK%XCNA_NITRO)>0) THEN
        IF (GDATA .AND. ANY(DTV%LDATA_CNA_NITRO)) THEN
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                    PEK%XCNA_NITRO,DTV%XPAR_VEGTYPE2,DTV%XPAR_CNA_NITRO,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                    PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
        ELSE
          CALL AV_PGD_1P(DTCO, PEK%XCNA_NITRO,PCOVER,XDATA_CNA_NITRO,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
                  PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
        ENDIF
      ENDIF
      !
    ENDIF
    !
  ENDIF
!
!       STRESS
!       --------
  IF (SIZE(PEK%LSTRESS)>0) THEN
    CALL SET_STRESS
  ENDIF
!
 ENDIF
!
ENDIF
!
IF (OMEB .AND. .NOT.OUPDATE_ALB) THEN
  !
!   GNDLITTER
!   ---------
  IF (GDATA .AND. ANY(DTV%LDATA_GNDLITTER)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, PEK%XGNDLITTER,DTV%XPAR_VEGTYPE2,&
                        DTV%XPAR_GNDLITTER(:,KDEC2,:),DTV%NPAR_VEG_IRR_USE,YNAT,'ARI',PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
  ELSE
    CALL AV_PGD_1P(DTCO, PEK%XGNDLITTER,PCOVER,XDATA_GNDLITTER(:,KDEC,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YNAT,'ARI',OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
  ENDIF
!
!        H_VEG
!        -----
  IF (GDATA .AND. ANY(DTV%LDATA_H_VEG)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XH_VEG,DTV%XPAR_VEGTYPE2,DTV%XPAR_H_VEG(:,KDEC2,:),DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
  ELSE
    CALL AV_PGD_1P(DTCO, PEK%XH_VEG,PCOVER,XDATA_H_VEG(:,KDEC,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
  ENDIF
! In case of MEB, force 0<PH_VEG<XUNDEF for those patches where LMEB_PATCH=.T.
  IF(IO%LMEB_PATCH(KPATCH))THEN
    ALLOCATE(ZH_VEG(SIZE(PEK%XH_VEG)))
    ZH_VEG=PEK%XH_VEG(:)
    WHERE(ZH_VEG>1000.) ZH_VEG=0.
    ZH_VEG=MAX(ZH_VEG,1.0E-3)
    PEK%XH_VEG(:)=ZH_VEG
    DEALLOCATE(ZH_VEG)
  ENDIF
!
!    Z0LITTER
!    --------
  IF (GDATA .AND. ANY(DTV%LDATA_Z0LITTER)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XZ0LITTER,DTV%XPAR_VEGTYPE2,DTV%XPAR_Z0LITTER(:,KDEC2,:),DTV%NPAR_VEG_IRR_USE,YNAT,'CDN',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
  ELSE
    CALL AV_PGD_1P(DTCO, PEK%XZ0LITTER ,PCOVER ,XDATA_Z0LITTER (:,KDEC,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE, &
            YNAT,'CDN',OCOVER,PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
  ENDIF
!
ENDIF
!
!
IF (OIRR .AND. .NOT.OUPDATE_ALB) THEN
  !
  IF ( OAGRIP .OR. OIRRIGMODE )  THEN
    !
    !
    ! LOAD irrigation date in case of only one season per year
    ! --------------------------------------------------------
    !
    IF (.NOT.LMULTI_SEASON) THEN
      !
      ! date of seeding
      ! ---------------
      !
      ALLOCATE(ZWORKI(SIZE(PEK%TSEED,1)))
      !
      IF(SIZE(PEK%TSEED)>0) THEN
        IF (GDATA .AND. ANY(DTV%LDATA_SEED_M) .AND. ANY(DTV%LDATA_SEED_D)) THEN 
          ! Take care: MAJ or  MA1 are valable only from 0 to 31 (cf ZCOUNT in av_pgd_param.F90).
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                            ZWORKI,DTV%XPAR_VEGTYPE2,DTV%XPAR_SEED_M(:,:),DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',&
                            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
          PEK%TSEED(:)%TDATE%MONTH = NINT(ZWORKI(:))
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                            ZWORKI,DTV%XPAR_VEGTYPE2,DTV%XPAR_SEED_D(:,:),DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',&
                            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
          PEK%TSEED(:)%TDATE%DAY = NINT(ZWORKI(:))
        ELSE            
          CALL AV_PGD_1P (PEK%TSEED,PCOVER,TDATA_SEED(:,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',OCOVER,&
                          PK%NR_P,IO%NPATCH, KPATCH, KDECADE=KDEC)
        ENDIF
        !
      ENDIF
      !
      ! date of reaping
      ! ---------------
      !
      IF (SIZE(PEK%TREAP)>0) THEN
        IF (GDATA .AND. ANY(DTV%LDATA_REAP_M) .AND. ANY(DTV%LDATA_REAP_D)) THEN 
          ! Take care: MAJ or  MA1 are valable only from 0 to 31 (cf ZCOUNT in av_pgd_param.F90).
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                            ZWORKI,DTV%XPAR_VEGTYPE2,DTV%XPAR_REAP_M(:,:),DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',&
                            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
          PEK%TREAP(:)%TDATE%MONTH = NINT(ZWORKI(:))
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                            ZWORKI,DTV%XPAR_VEGTYPE2,DTV%XPAR_REAP_D(:,:),DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',&
                            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
          PEK%TREAP(:)%TDATE%DAY = NINT(ZWORKI(:))          
        ELSE               
          CALL AV_PGD_1P (PEK%TREAP,PCOVER,TDATA_REAP(:,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',OCOVER,&
                          PK%NR_P,IO%NPATCH, KPATCH, KDECADE=KDEC) 
        ENDIF
        !
      ENDIF
      !
      DEALLOCATE(ZWORKI)
      !
      ! Put no irrigation date for non irrigated patchs if ECOSG
      IF ( SIZE(PEK%TSEED)>0 .OR. SIZE(PEK%TREAP)>0 ) THEN
        IF ( OECOSG .AND. NVEG_IRR > 0 ) THEN ! == IF (OIRRIGMODE )
          CALL VEGTYPE_TO_PATCH_IRRIG(NVEGTYPE+1,IO%NPATCH,DTV%NPAR_VEG_IRR_USE,JK)
          IF ( KPATCH < JK ) THEN
            IF( SIZE(PEK%TSEED)>0 ) THEN
              PEK%TSEED(:)%TDATE%MONTH = NUNDEF
              PEK%TSEED(:)%TDATE%DAY   = NUNDEF
            ENDIF
            IF ( SIZE(PEK%TREAP)>0 ) THEN
              PEK%TREAP(:)%TDATE%MONTH = NUNDEF
              PEK%TREAP(:)%TDATE%DAY   = NUNDEF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    !
    !
    ! LOAD others dates in case of multi-seasons per year
    ! ---------------------------------------------------
    !
    ELSE ! = IF (LMULTI_SEASON) THEN
      !
      ! date of seeding and reaping season 1
      !
      !
      ! date of seeding
      ! ---------------
      !
      ALLOCATE(ZWORKI(SIZE(PEK%MULTI_TSEED,1)))
      !
      IF(SIZE(PEK%MULTI_TSEED(:,1))>0) THEN
        IF (GDATA .AND. ANY(DTV%LDATA_SEED_M) .AND. ANY(DTV%LDATA_SEED_D)) THEN
          ! Take care: MAJ or  MA1 are valable only from 0 to 31 (cf ZCOUNT in av_pgd_param.F90).
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                            ZWORKI,DTV%XPAR_VEGTYPE2,DTV%XPAR_SEED_M(:,:),DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',&
                            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
          PEK%MULTI_TSEED(:,1)%TDATE%MONTH = NINT(ZWORKI(:))
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                            ZWORKI,DTV%XPAR_VEGTYPE2,DTV%XPAR_SEED_D(:,:),DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',&
                            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
          PEK%MULTI_TSEED(:,1)%TDATE%DAY = NINT(ZWORKI(:))
        ELSE
          CALL AV_PGD_1P (PEK%MULTI_TSEED(:,1),PCOVER,TDATA_SEED(:,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',OCOVER,&
                          PK%NR_P,IO%NPATCH, KPATCH, KDECADE=KDEC)
        ENDIF
        !
      ENDIF
      !
      ! date of reaping
      ! ---------------
      !
      IF (SIZE(PEK%MULTI_TREAP(:,1))>0) THEN
        IF (GDATA .AND. ANY(DTV%LDATA_REAP_M) .AND. ANY(DTV%LDATA_REAP_D)) THEN
          ! Take care: MAJ or  MA1 are valable only from 0 to 31 (cf ZCOUNT in av_pgd_param.F90).
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                            ZWORKI,DTV%XPAR_VEGTYPE2,DTV%XPAR_REAP_M(:,:),DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',&
                            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
          PEK%MULTI_TREAP(:,1)%TDATE%MONTH = NINT(ZWORKI(:))
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                            ZWORKI,DTV%XPAR_VEGTYPE2,DTV%XPAR_REAP_D(:,:),DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',&
                            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
          PEK%MULTI_TREAP(:,1)%TDATE%DAY = NINT(ZWORKI(:))
        ELSE
          CALL AV_PGD_1P (PEK%MULTI_TREAP(:,1),PCOVER,TDATA_REAP(:,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',OCOVER,&
                          PK%NR_P,IO%NPATCH, KPATCH, KDECADE=KDEC)
        ENDIF
        !
      ENDIF
      !
      ! date of seeding season 2
      !
      IF(SIZE(PEK%MULTI_TSEED,1)>0) THEN
        IF (GDATA .AND. ANY(DTV%LDATA_SEED_S2_M) .AND. ANY(DTV%LDATA_SEED_S2_D)) THEN
          ! Take care: MAJ or  MA1 are valable only from 0 to 31 (cf ZCOUNT in av_pgd_param.F90).
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                            ZWORKI,DTV%XPAR_VEGTYPE2,DTV%XPAR_SEED_S2_M(:,:),DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',&
                            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
          PEK%MULTI_TSEED(:,2)%TDATE%MONTH = NINT(ZWORKI(:))
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                            ZWORKI,DTV%XPAR_VEGTYPE2,DTV%XPAR_SEED_S2_D(:,:),DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',&
                            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
          PEK%MULTI_TSEED(:,2)%TDATE%DAY = NINT(ZWORKI(:))
        ELSE
          CALL ABOR1_SFX('CONVERT_PATCH_ISBA: IF LMULTI_SEASON, INPUT DATA ARE NEEDING  (MULTI_TSEED)!')
        ENDIF
        !
      ENDIF
      !
      ! date of seeding season 3
      !
      IF(SIZE(PEK%MULTI_TSEED,1)>0) THEN
        IF (GDATA .AND. ANY(DTV%LDATA_SEED_S3_M) .AND. ANY(DTV%LDATA_SEED_S3_D)) THEN
          ! Take care: MAJ or  MA1 are valable only from 0 to 31 (cf ZCOUNT in av_pgd_param.F90).
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                            ZWORKI,DTV%XPAR_VEGTYPE2,DTV%XPAR_SEED_S3_M(:,:),DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',&
                            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
          PEK%MULTI_TSEED(:,3)%TDATE%MONTH = NINT(ZWORKI(:))
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                            ZWORKI,DTV%XPAR_VEGTYPE2,DTV%XPAR_SEED_S3_D(:,:),DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',&
                            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
          PEK%MULTI_TSEED(:,3)%TDATE%DAY = NINT(ZWORKI(:))
        ELSE
          PEK%MULTI_TSEED(:,3)%TDATE%MONTH = NUNDEF
          PEK%MULTI_TSEED(:,3)%TDATE%DAY   = NUNDEF
        ENDIF
        !
      ENDIF
      !
      ! date of reaping season 2
      !
      IF (SIZE(PEK%MULTI_TREAP,1)>0) THEN
        IF (GDATA .AND. ANY(DTV%LDATA_REAP_S2_M) .AND. ANY(DTV%LDATA_REAP_S2_D)) THEN
          ! Take care: MAJ or  MA1 are valable only from 0 to 31 (cf ZCOUNT in av_pgd_param.F90).
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                            ZWORKI,DTV%XPAR_VEGTYPE2,DTV%XPAR_REAP_S2_M(:,:),DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',&
                            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
          PEK%MULTI_TREAP(:,2)%TDATE%MONTH = NINT(ZWORKI(:))
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                            ZWORKI,DTV%XPAR_VEGTYPE2,DTV%XPAR_REAP_S2_D(:,:),DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',&
                            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
          PEK%MULTI_TREAP(:,2)%TDATE%DAY = NINT(ZWORKI(:))
        ELSE
          CALL ABOR1_SFX('CONVERT_PATCH_ISBA: IF LMULTI_SEASON, INPUT DATA ARE NEEDING (MULTI_TREAP)!')
        ENDIF
        !
      ENDIF
      !
      ! date of reaping season 3
      !
      IF (SIZE(PEK%MULTI_TREAP,1)>0) THEN
        IF (GDATA .AND. ANY(DTV%LDATA_REAP_S3_M) .AND. ANY(DTV%LDATA_REAP_S3_D)) THEN
          ! Take care: MAJ or  MA1 are valable only from 0 to 31 (cf ZCOUNT in av_pgd_param.F90).
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                            ZWORKI,DTV%XPAR_VEGTYPE2,DTV%XPAR_REAP_S3_M(:,:),DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',&
                            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
          PEK%MULTI_TREAP(:,3)%TDATE%MONTH = NINT(ZWORKI(:))
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                            ZWORKI,DTV%XPAR_VEGTYPE2,DTV%XPAR_REAP_S3_D(:,:),DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',&
                            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
          PEK%MULTI_TREAP(:,3)%TDATE%DAY = NINT(ZWORKI(:))
        ELSE
          PEK%MULTI_TREAP(:,3)%TDATE%MONTH = NUNDEF
          PEK%MULTI_TREAP(:,3)%TDATE%DAY   = NUNDEF
        ENDIF
        !
      ENDIF
      !
      DEALLOCATE(ZWORKI)
      !
      ! Put no irrigation date for non irrigated patchs if ECOSG
      IF ( SIZE(PEK%MULTI_TSEED,1)>0 .OR. SIZE(PEK%MULTI_TREAP,1)>0 ) THEN
        IF ( OECOSG .AND. NVEG_IRR > 0 ) THEN ! == IF (OIRRIGMODE )
          CALL VEGTYPE_TO_PATCH_IRRIG(NVEGTYPE+1,IO%NPATCH,DTV%NPAR_VEG_IRR_USE,JK)
          IF ( KPATCH < JK ) THEN
            IF( SIZE(PEK%TSEED)>0 ) THEN
              PEK%MULTI_TSEED(:,:)%TDATE%MONTH = NUNDEF
              PEK%MULTI_TSEED(:,:)%TDATE%DAY   = NUNDEF
            ENDIF
            IF ( SIZE(PEK%TREAP)>0 ) THEN
              PEK%MULTI_TREAP(:,:)%TDATE%MONTH = NUNDEF
              PEK%MULTI_TREAP(:,:)%TDATE%DAY   = NUNDEF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      !
      !
      ! Define the current season (for irrigation or agricultural practices)
      ! -------------------------
      !
      ! Select the good season
      ALLOCATE(ISEASON(SIZE(PEK%TSEED,1)))
      ISEASON(:)=1
      WHERE ( PEK%MULTI_TSEED(:,1)%TDATE%MONTH /= NUNDEF .AND. PEK%MULTI_TSEED(:,2)%TDATE%MONTH /= NUNDEF ) ! Case of multi-seasons
        !
        WHERE ( PEK%MULTI_TSEED(:,3)%TDATE%MONTH /= NUNDEF )                                                ! Case of 3 seasons
          !
          WHERE ( ( PEK%MULTI_TREAP(:,3)%TDATE%MONTH < PEK%MULTI_TSEED(:,1)%TDATE%MONTH .OR.       &        ! 3 seasons with inversion
                    ( PEK%MULTI_TREAP(:,3)%TDATE%MONTH == PEK%MULTI_TSEED(:,1)%TDATE%MONTH .AND.   &
                      PEK%MULTI_TREAP(:,3)%TDATE%DAY   <  PEK%MULTI_TSEED(:,1)%TDATE%DAY ) ) .AND. &
                  ( KMONTH < PEK%MULTI_TREAP(:,3)%TDATE%MONTH .OR.                                 &        ! AND during inversion
                    ( KMONTH == PEK%MULTI_TREAP(:,3)%TDATE%MONTH .AND. KDAY <  PEK%MULTI_TREAP(:,3)%TDATE%DAY ) ) )
            ISEASON(:)=3
          ELSEWHERE ( KMONTH < PEK%MULTI_TREAP(:,1)%TDATE%MONTH .OR. &                                      ! Before or during 1st season
                    ( KMONTH == PEK%MULTI_TREAP(:,1)%TDATE%MONTH .AND. KDAY <  PEK%MULTI_TREAP(:,1)%TDATE%DAY ) )
            ISEASON(:) = 1
          ELSEWHERE ( KMONTH < PEK%MULTI_TREAP(:,2)%TDATE%MONTH .OR. &                                      ! Before or during 2nd season
                    ( KMONTH == PEK%MULTI_TREAP(:,2)%TDATE%MONTH .AND. KDAY <  PEK%MULTI_TREAP(:,2)%TDATE%DAY ) )
            ISEASON(:) = 2
          ELSEWHERE ( ( PEK%MULTI_TREAP(:,3)%TDATE%MONTH < PEK%MULTI_TSEED(:,1)%TDATE%MONTH .OR.  &         ! 3 seasons with inversion
                    ( PEK%MULTI_TREAP(:,3)%TDATE%MONTH == PEK%MULTI_TSEED(:,1)%TDATE%MONTH .AND.  &
                      PEK%MULTI_TREAP(:,3)%TDATE%DAY   <  PEK%MULTI_TSEED(:,1)%TDATE%DAY ) ) .OR. &
                    ( KMONTH < PEK%MULTI_TREAP(:,3)%TDATE%MONTH .OR. &                                      ! OR Before or during 3st season
                    ( KMONTH == PEK%MULTI_TREAP(:,3)%TDATE%MONTH .AND. KDAY <  PEK%MULTI_TREAP(:,3)%TDATE%DAY ) ) )
            ISEASON(:) = 3
          ELSEWHERE                                                                                         ! After 3st season and without inversion
            ISEASON(:) = 1
          ENDWHERE
          ! 
        ELSEWHERE                                                                                           ! Case of 2 seasons
          !
          WHERE ( ( PEK%MULTI_TREAP(:,2)%TDATE%MONTH < PEK%MULTI_TSEED(:,1)%TDATE%MONTH .OR.       &        ! 2 seasons with inversion
                    ( PEK%MULTI_TREAP(:,2)%TDATE%MONTH == PEK%MULTI_TSEED(:,1)%TDATE%MONTH .AND.   &
                      PEK%MULTI_TREAP(:,2)%TDATE%DAY   <  PEK%MULTI_TSEED(:,1)%TDATE%DAY ) ) .AND. &
                  ( KMONTH < PEK%MULTI_TREAP(:,2)%TDATE%MONTH .OR.                                 &        ! AND during inversion
                    ( KMONTH == PEK%MULTI_TREAP(:,2)%TDATE%MONTH .AND. KDAY <  PEK%MULTI_TREAP(:,2)%TDATE%DAY ) ) )
            ISEASON(:)=2
          ELSEWHERE ( KMONTH < PEK%MULTI_TREAP(:,1)%TDATE%MONTH .OR. &                                      ! Before or during 1st season
                    ( KMONTH == PEK%MULTI_TREAP(:,1)%TDATE%MONTH .AND. KDAY <  PEK%MULTI_TREAP(:,1)%TDATE%DAY )  ) 
            ISEASON(:) = 1
          ELSEWHERE ( ( PEK%MULTI_TREAP(:,2)%TDATE%MONTH < PEK%MULTI_TSEED(:,1)%TDATE%MONTH .OR.  &         ! 2 seasons with inversion
                    ( PEK%MULTI_TREAP(:,2)%TDATE%MONTH == PEK%MULTI_TSEED(:,1)%TDATE%MONTH .AND.  &
                      PEK%MULTI_TREAP(:,2)%TDATE%DAY   <  PEK%MULTI_TSEED(:,1)%TDATE%DAY ) ) .OR. &
                    ( KMONTH < PEK%MULTI_TREAP(:,2)%TDATE%MONTH .OR. &                                      ! OR Before or during 3th season
                    ( KMONTH == PEK%MULTI_TREAP(:,2)%TDATE%MONTH .AND. KDAY <  PEK%MULTI_TREAP(:,2)%TDATE%DAY ) ) )
            ISEASON(:) = 2
          ELSEWHERE                                                                                         ! After 2nd season and without inversion
            ISEASON(:) = 1
          ENDWHERE
          ! 
        ENDWHERE
        !
      ENDWHERE
      !
      ! Attribute TSEED and TREAP following the season
      DO JK = 1, SIZE(PEK%TSEED,1)
        PEK%TSEED(JK)%TDATE%MONTH = PEK%MULTI_TSEED(JK,ISEASON(JK))%TDATE%MONTH
        PEK%TSEED(JK)%TDATE%DAY   = PEK%MULTI_TSEED(JK,ISEASON(JK))%TDATE%DAY
        PEK%TREAP(JK)%TDATE%MONTH = PEK%MULTI_TREAP(JK,ISEASON(JK))%TDATE%MONTH
        PEK%TREAP(JK)%TDATE%DAY   = PEK%MULTI_TREAP(JK,ISEASON(JK))%TDATE%DAY
      ENDDO
      DEALLOCATE(ISEASON)
      !
    ENDIF
    !
  ENDIF
  !
  !
  IF ( OIRRIGMODE )  THEN
    !
    ! Irrigation type
    ! ---------------
    !
    IF (SIZE(PEK%XIRRIGTYPE)>0) THEN
      !
      ! Take care: MAJ or  MA1 are valable only from 0 to 31 (cf ZCOUNT in av_pgd_param.F90).
      IF ( OECOSG ) THEN
        !
        IF (GDATA .AND. ANY(DTV%LDATA_IRRIGTYPE)) THEN
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                        PEK%XIRRIGTYPE,DTV%XPAR_VEGTYPE2,DTV%XPAR_IRRIGTYPE(:,:),DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',& 
                        PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
        ELSE
          CALL AV_PGD_1P(DTCO, PEK%XIRRIGTYPE,PCOVER,XDATA_IRRIGTYPE,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'MA1',&
              OCOVER,PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
        ENDIF
        !
        ! Put no irrigation for non irrigated patchs
        JK = 0
        IF ( NVEG_IRR > 0 ) CALL VEGTYPE_TO_PATCH_IRRIG(NVEGTYPE+1,IO%NPATCH,DTV%NPAR_VEG_IRR_USE,JK)  ! == IF (OIRRIGMODE )
        IF ( KPATCH < JK .OR. JK == 0 ) THEN
          PEK%XIRRIGTYPE = 0
        ENDIF
        !
      ELSE
        IF (GDATA .AND. ANY(DTV%LDATA_IRRIGTYPE)) THEN
          CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XIRRIGTYPE,DTV%XPAR_VEGTYPE2,DTV%XPAR_IRRIGTYPE(:,:),DTV%NPAR_VEG_IRR_USE,YVEG,'MAJ',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
        ELSE
          CALL AV_PGD_1P(DTCO, PEK%XIRRIGTYPE,PCOVER,XDATA_IRRIGTYPE,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'MAJ',&
                      OCOVER,PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
        ENDIF
      ENDIF
      WHERE ( PEK%XIRRIGTYPE == NUNDEF ) PEK%XIRRIGTYPE = 0
    ENDIF
    !
    ! irrigation amount
    ! -----------------
    !
    IF (SIZE(PEK%XWATSUP)>0) THEN
      IF (GDATA .AND. ANY(DTV%LDATA_WATSUP)) THEN
        CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XWATSUP,DTV%XPAR_VEGTYPE2,DTV%XPAR_WATSUP(:,KDEC2,:),DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
      ELSE
        CALL AV_PGD_1P(DTCO, PEK%XWATSUP,PCOVER,XDATA_WATSUP,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
                PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
      ENDIF
    ENDIF
    !
    ! minimum time between two irrigation
    ! -----------------------------------
    !
    IF (SIZE(PEK%XIRRIGFREQ)>0) THEN
      IF (GDATA .AND. ANY(DTV%LDATA_IRRIGFREQ)) THEN
        CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                          PEK%XIRRIGFREQ,DTV%XPAR_VEGTYPE2,DTV%XPAR_IRRIGFREQ(:,:),DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                          PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
      ELSE
        CALL AV_PGD_1P(DTCO, PEK%XIRRIGFREQ,PCOVER,XDATA_IRRIGFREQ,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
                       PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)  
      ENDIF
    ENDIF
    !
    ! irrigation amount application time
    ! ----------------------------------
    !
    IF (SIZE(PEK%XIRRIGTIME)>0) THEN
      IF (GDATA .AND. ANY(DTV%LDATA_IRRIGTIME)) THEN
        CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XIRRIGTIME,DTV%XPAR_VEGTYPE2,DTV%XPAR_IRRIGTIME(:,:),DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
      ELSE
        CALL AV_PGD_1P(DTCO, PEK%XIRRIGTIME,PCOVER,XDATA_IRRIGTIME,DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',OCOVER,&
                PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
      ENDIF
    ENDIF
    !
    ! F2 threshold for irrigation triggering
    ! --------------------------------------
    !
    IF (SIZE(PEK%XF2THRESHOLD)>0) THEN
      IF (GDATA .AND. ANY(DTV%LDATA_F2THRESHOLD)) THEN
        CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                          PEK%XF2THRESHOLD,DTV%XPAR_VEGTYPE2,DTV%XPAR_F2THRESHOLD(:,KDEC2,:),&
                          DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
      ELSE
        CALL AV_PGD_1P(DTCO, PEK%XF2THRESHOLD,PCOVER,XDATA_F2THRESHOLD(:,KDEC2,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE, &
                       YVEG,'ARI',OCOVER,PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
      ENDIF
    ENDIF
    !
  ENDIF
  !
ENDIF
!
!
IF (OALB) THEN
!
  IF (GDATA .AND. ANY(DTV%LDATA_ALBNIR_SOIL)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XALBNIR_SOIL,DTV%XPAR_VEGTYPE2,DTV%XPAR_ALBNIR_SOIL(:,KDEC2,:),DTV%NPAR_VEG_IRR_USE,YBAR,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
  ELSEIF (IO%CALBEDO=='CM13') THEN
    CALL AV_PGD_1P(DTCO, PEK%XALBNIR_SOIL,PCOVER,XDATA_ALB_SOIL_NIR(:,KDEC,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE, &
            YBAR,'ARI',OCOVER,PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
  ELSE
    CALL SOIL_ALBEDO (IO%CALBEDO, PWSAT(:,1), PWG1, KK, PEK, "NIR" )  
  ENDIF
!
  IF (GDATA .AND. ANY(DTV%LDATA_ALBVIS_SOIL)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XALBVIS_SOIL,DTV%XPAR_VEGTYPE2,DTV%XPAR_ALBVIS_SOIL(:,KDEC2,:),DTV%NPAR_VEG_IRR_USE,YBAR,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
  ELSEIF (IO%CALBEDO=='CM13') THEN
    CALL AV_PGD_1P(DTCO, PEK%XALBVIS_SOIL,PCOVER,XDATA_ALB_SOIL_VIS(:,KDEC,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE, &
            YBAR,'ARI',OCOVER,PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
  ELSE
    CALL SOIL_ALBEDO (IO%CALBEDO, PWSAT(:,1), PWG1, KK, PEK, "VIS" )
  ENDIF
!

  IF (IO%CALBEDO=='CM13'.OR.IO%LTR_ML) THEN
    PEK%XALBUV_SOIL(:)=PEK%XALBVIS_SOIL(:)
  ELSEIF (GDATA .AND. ANY(DTV%LDATA_ALBUV_SOIL)) THEN
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PEK%XALBUV_SOIL,DTV%XPAR_VEGTYPE2,DTV%XPAR_ALBUV_SOIL(:,KDEC2,:),DTV%NPAR_VEG_IRR_USE,YNAT,'ARI',&
                      PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
  ELSE
    CALL SOIL_ALBEDO (IO%CALBEDO, PWSAT(:,1), PWG1, KK, PEK, "UV"  )  
  ENDIF
!
ENDIF
!
IF (ASSOCIATED(DTCO%XDATA_WEIGHT)) DEALLOCATE(DTCO%XDATA_WEIGHT)
!
IF (LHOOK) CALL DR_HOOK('CONVERT_PATCH_ISBA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE SET_STRESS
!
IMPLICIT NONE
!
REAL, DIMENSION(PK%NSIZE_P)   :: ZWORK
REAL, DIMENSION(SIZE(DTV%LPAR_STRESS,1),NVEGTYPE) :: ZSTRESS
INTEGER :: JI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('CONVERT_PATCH_ISBA:SET_STRESS',0,ZHOOK_HANDLE)
!
IF (GDATA .AND. ANY(DTV%LDATA_STRESS)) THEN
  ZSTRESS(:,:)=0.
  DO JVEG=1,NVEGTYPE
    DO JI = 1,PK%NSIZE_P
      IF (DTV%LPAR_STRESS(JI,JVEG)) ZSTRESS(PK%NR_P(JI),JVEG) = 1.
    ENDDO
  ENDDO
  CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      ZWORK,DTV%XPAR_VEGTYPE2,ZSTRESS,DTV%NPAR_VEG_IRR_USE,YVEG,'ARI',PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC2)
ELSE
  CALL AV_PGD_1P(DTCO, ZWORK,PCOVER,XDATA_STRESS(:,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE, &
                      YVEG,'ARI',OCOVER,PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
ENDIF
!
WHERE (ZWORK(:)<0.5)
  PEK%LSTRESS(:) = .FALSE.
ELSEWHERE
  PEK%LSTRESS(:) = .TRUE.
END WHERE
!
IF (LHOOK) CALL DR_HOOK('CONVERT_PATCH_ISBA:SET_STRESS',1,ZHOOK_HANDLE)
END SUBROUTINE SET_STRESS
!
!-------------------------------------------------------------------------------
SUBROUTINE SET_GRID_PARAM(KNI,KGROUND)
!
USE MODD_PGDWORK, ONLY : XPREC
!
USE MODD_SURF_PAR, ONLY : XUNDEF, NUNDEF
USE MODD_ISBA_PAR, ONLY : XPERMFRAC
!
USE MODD_REPROD_OPER, ONLY : CDGAVG, CDGDIF
!
USE MODI_INI_DATA_ROOTFRAC
USE MODI_INI_DATA_SOIL
USE MODI_PERMAFROST_DEPTH
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KNI
INTEGER, INTENT(IN) :: KGROUND
!
REAL,    DIMENSION (SIZE(XDATA_GROUND_DEPTH,1),NVEGTYPE) :: ZDATA_GROUND_DEPTH
!
REAL,    DIMENSION (KNI)         :: ZDTOT, ZDG2, ZROOT_EXT, ZROOT_LIN
!
INTEGER :: JJ, JL
!
! flags taking general surface type flag into account
LOGICAL :: GDATA_DG, GDATA_GROUND_DEPTH, GDATA_ROOT_DEPTH, GDATA_ROOTFRAC, &
           GNOECO, GMEB
!-------------------------------------------------------------------------!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('CONVERT_PATCH_ISBA:SET_GRID_PARAM',0,ZHOOK_HANDLE)
!
IF(IO%CISBA=='DIF')THEN
  IF(.NOT.OFIX) CALL ABOR1_SFX('CONVERT_PATCH_ISBA: SET_GRID_PARAM: KWG_LAYER, PDROOT and PGD2 must be present with DIF')
 
ENDIF
!
GMEB = (OMEB .AND. (ISIZE_LMEB_PATCH>0))
!
ZDTOT    (:) = XUNDEF
ZDG2     (:) = XUNDEF
!
PK%NWG_LAYER(:) = NUNDEF
PK%XROOTFRAC(:,:) = XUNDEF
!
ZDATA_GROUND_DEPTH(:,:) = XDATA_GROUND_DEPTH(:,:)
!
GDATA_DG           = GDATA .AND. ANY(DTV%LDATA_DG)
GDATA_GROUND_DEPTH = GDATA .AND. ANY(DTV%LDATA_GROUND_DEPTH)
GDATA_ROOT_DEPTH   = GDATA .AND. ANY(DTV%LDATA_ROOT_DEPTH)
GDATA_ROOTFRAC     = GDATA .AND. ANY(DTV%LDATA_ROOTFRAC)
!
!####################################################################################
!
!CDGAVG : old for reprod = 'ARI' Arithmetic average for all depth 
!         recommended    = 'INV' Harmonic average for all depth (default)
!
!CDGDIF : old for reprod = 'SOIL' d3 soil depth from ecoclimap for isba-df
!         recommended    = 'ROOT' d2 soil depth from ecoclimap for isba-df (default)
!
!####################################################################################
!n
!DG IN NAMELIST => GROUND_DEPTH KNOWN, ROOT_DEPTH UNKNOWN 
IF (GDATA_DG) THEN
  !
  DO JLAYER=1,KGROUND
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PK%XDG(:,JLAYER),DTV%XPAR_VEGTYPE2,DTV%XPAR_DG(:,JLAYER,:),DTV%NPAR_VEG_IRR_USE,YNAT,CDGAVG,&
                      PK%NR_P,IO%NPATCH,KPATCH)
  ENDDO
  !
ENDIF
!
IF(.NOT.GDATA_GROUND_DEPTH.AND.IO%CISBA=='DIF'.AND.CDGDIF=='ROOT')THEN
  !
  DO JVEG=1,NVEGTYPE
    IF(JVEG==NVT_NO)THEN
      WHERE(XDATA_GROUND_DEPTH(:,JVEG)/=XUNDEF)
        ZDATA_GROUND_DEPTH(:,JVEG) = MIN(1.0,XDATA_GROUND_DEPTH(:,JVEG))
      ENDWHERE
    ELSEIF(JVEG/=NVT_ROCK.AND.JVEG/=NVT_SNOW)THEN
      ZDATA_GROUND_DEPTH(:,JVEG) = MAX(1.0,XDATA_ROOT_DEPTH(:,JVEG))
    ELSE
      ZDATA_GROUND_DEPTH(:,JVEG) = XDATA_ROOT_DEPTH(:,JVEG)
    ENDIF
  ENDDO
  !
ENDIF
!
!CALCULATION OF GROUND_DEPTH IN ZDTOT : ECOCLMAP OR LDATA_GROUND_DEPTH
IF (IO%CISBA/='2-L') THEN 
  !
  IF (GDATA_GROUND_DEPTH .AND. (IO%CISBA=='DIF' .OR. .NOT.GDATA_DG)) THEN
    !GROUND DEPTH IN NAMELIST
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      ZDTOT(:),DTV%XPAR_VEGTYPE2,DTV%XPAR_GROUND_DEPTH(:,:),DTV%NPAR_VEG_IRR_USE,YNAT,CDGAVG,&
                      PK%NR_P,IO%NPATCH,KPATCH)
    !Error Due to machine precision
    WHERE(ZDTOT(:)/=XUNDEF) ZDTOT(:)=NINT(ZDTOT(:)*XPREC)/XPREC
    !CONSISTENCY CHECK
    IF (GDATA_DG) ZDTOT(:) = MIN(ZDTOT(:),PK%XDG(:,KGROUND))
  ELSEIF (GDATA_DG) THEN
    !GROUND DEPTH FROM NAMELIST DG
    ZDTOT(:) = PK%XDG(:,KGROUND)
  ELSE
    !GROUND DEPTH FROM ECOCLMAP
    CALL AV_PGD_1P(DTCO, ZDTOT(:),PCOVER,ZDATA_GROUND_DEPTH(:,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YNAT,CDGAVG,OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
    IF(IO%CISBA=='DIF'.AND.CDGDIF=='ROOT')ZDG2(:)=ZDTOT(:)
  ENDIF
  !
ENDIF
!
!CALCULATION OF GROUND_DEPTH : Permafrost depth put to 12m
IF(IO%CISBA=='DIF'.AND.IO%LPERM) CALL PERMAFROST_DEPTH(PK%NSIZE_P,KPATCH,PPERM,ZDTOT)
!
!IN BOTH CASES, ROOT_DEPTH IS NEEDED: PUT IN DG2
IF (IO%CISBA=='DIF' .OR. .NOT.GDATA_DG) THEN
  !
  GNOECO=(GDATA_ROOT_DEPTH .AND. .NOT.GDATA_ROOTFRAC)
  IF (GNOECO) THEN
    !ROOT_DEPTH IN NAMELIST
    CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      ZDG2(:),DTV%XPAR_VEGTYPE2,DTV%XPAR_ROOT_DEPTH(:,:),DTV%NPAR_VEG_IRR_USE,YNAT,CDGAVG,&
            PK%NR_P,IO%NPATCH,KPATCH)
    !Error Due to machine precision
    WHERE(ZDG2(:)/=XUNDEF) ZDG2(:)=NINT(ZDG2(:)*XPREC)/XPREC  
    !CONSISTENCY CHECKS
    IF (ANY(DTV%LDATA_DG)) ZDG2(:) = MIN(ZDG2(:),PK%XDG(:,KGROUND))
    ZDTOT(:) = MAX(ZDG2(:),ZDTOT(:))
    IF (IO%CISBA=='DIF') THEN
      CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PK%XDROOT(:),DTV%XPAR_VEGTYPE2,DTV%XPAR_ROOT_DEPTH(:,:),DTV%NPAR_VEG_IRR_USE,YDIF,CDGAVG,&
            PK%NR_P,IO%NPATCH,KPATCH)
     !Error Due to machine precision
      WHERE(PK%XDROOT(:)/=XUNDEF)
          PK%XDROOT(:)=NINT(PK%XDROOT(:)*XPREC)/XPREC
      ENDWHERE 
       IF(CDGDIF=='ROOT')THEN
         WHERE(PK%XDROOT(:).NE.XUNDEF) ZDTOT(:) = MAX(PK%XDROOT(:),ZDTOT(:))
         WHERE(PK%XDROOT(:).NE.XUNDEF) ZDG2 (:) = MAX(PK%XDROOT(:),ZDG2 (:))
       ELSE
         CALL AV_PGD_1P(DTCO, ZDG2(:),PCOVER,XDATA_ROOT_DEPTH(:,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YNAT,CDGAVG,OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
       ENDIF      
     !CONSISTENCY CHECKS
      IF (GDATA_DG) WHERE (PK%XDROOT(:).NE.XUNDEF) PK%XDROOT(:) = MIN(PK%XDROOT(:),PK%XDG(:,KGROUND))   
    ENDIF
  ELSE 
    !ROOT_DEPTH FROM ECOCLMAP
    IF (IO%CISBA=='DIF')THEN
       CALL AV_PGD_1P(DTCO, PK%XDROOT(:),PCOVER,XDATA_ROOT_DEPTH(:,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YDIF,CDGAVG,OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
       IF(CDGDIF=='ROOT')THEN
         WHERE(PK%XDROOT(:).NE.XUNDEF) ZDTOT(:) = MAX(PK%XDROOT(:),ZDTOT(:))
         WHERE(PK%XDROOT(:).NE.XUNDEF) ZDG2 (:) = MAX(PK%XDROOT(:),ZDG2 (:))
       ELSE
         CALL AV_PGD_1P(DTCO, ZDG2(:),PCOVER,XDATA_ROOT_DEPTH(:,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YNAT,CDGAVG,OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
       ENDIF
    ELSE
       CALL AV_PGD_1P(DTCO, ZDG2(:),PCOVER,XDATA_ROOT_DEPTH(:,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YNAT,CDGAVG,OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
    ENDIF
    IF ( GDATA_GROUND_DEPTH .OR. GDATA_DG ) THEN
      ZDG2  (:) = MIN(ZDG2  (:),ZDTOT(:))
      IF (IO%CISBA=='DIF') WHERE (PK%XDROOT(:).NE.XUNDEF) PK%XDROOT(:) = MIN(PK%XDROOT(:),ZDTOT(:))
    ENDIF
  ENDIF
  !
  !CALCULATION OF DG IF NOT IN NAMELIST
  IF (.NOT.GDATA_DG) THEN
    !
    IF (IO%CISBA=='DIF') THEN
      IF( MAXVAL(ZDTOT,ZDTOT/=XUNDEF)>PSOILGRID(KGROUND) ) THEN
        CALL ABOR1_SFX('CONVERT_PATCH_ISBA: not enough soil layer with optimized grid')
      ENDIF
    ENDIF
    !
    WHERE(ZDG2(:)==XUNDEF.AND.ZDTOT(:)/=XUNDEF) ZDG2(:)=0.0 !No vegetation
    !
    !IF CISBA=DIF CALCULATES ALSO KWG_LAYER WITH USE OF SOILGRID $
    CALL INI_DATA_SOIL(IO%CISBA, PK%XDG,PROOTDEPTH=ZDG2, PSOILDEPTH=ZDTOT,&
                       PSOILGRID=PSOILGRID, KWG_LAYER=PK%NWG_LAYER )
    IF (IO%CISBA=='DIF'.AND.CDGDIF=='ROOT')THEN
          DO JJ=1,KNI
             IF(IO%LPERM.AND.PK%NWG_LAYER(JJ)/=NUNDEF)THEN
               IF(PPERM(JJ)<XPERMFRAC) ZDG2(JJ)=PK%XDG(JJ,PK%NWG_LAYER(JJ))
             ELSEIF(PK%NWG_LAYER(JJ)/=NUNDEF)THEN
               ZDG2(JJ)=PK%XDG(JJ,PK%NWG_LAYER(JJ))
             ELSE
               ZDG2(JJ)=XUNDEF
             ENDIF
          ENDDO
    ENDIF
               
    !
  ELSEIF ( IO%CISBA=='DIF') THEN
    !
    !CALCULATION OF KWG_LAYER IF DG IN NAMELIST
    IF(GDATA_GROUND_DEPTH)THEN
        DO JJ=1,KNI
          DO JL=1,KGROUND
            IF( PK%XDG(JJ,JL) <= ZDTOT(JJ) .AND. ZDTOT(JJ) < XUNDEF ) &
                PK%NWG_LAYER(JJ) = JL
          ENDDO
        ENDDO              
    ELSE
      PK%NWG_LAYER(:) = KGROUND
    ENDIF
    !
  ENDIF
  !
  ! DROOT AND DG2 LMITED BY KWG_LAYER
  IF (IO%CISBA=='DIF' .AND. .NOT.ANY(DTV%LDATA_ROOTFRAC)) THEN
    !
      DO JJ=1,KNI
        IF(PK%NWG_LAYER(JJ)/=NUNDEF) THEN
          JL = PK%NWG_LAYER(JJ)
          ZDG2  (JJ)=MIN(ZDG2  (JJ),PK%XDG(JJ,JL))
          IF (PK%XDROOT(JJ)/=XUNDEF) PK%XDROOT(JJ)=MIN(PK%XDROOT(JJ),PK%XDG(JJ,JL))    
        ENDIF
      ENDDO
    !
  ENDIF
  !
ENDIF
!
!CALCULATION OF ROOTFRAC
IF (IO%CISBA=='DIF') THEN
  !
  IF (GDATA_ROOTFRAC) THEN
    !
    !ROOTFRAC IN NAMELIST
    DO JL=1,KGROUND
      CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      PK%XROOTFRAC(:,JL),DTV%XPAR_VEGTYPE2,DTV%XPAR_ROOTFRAC(:,JL,:),DTV%NPAR_VEG_IRR_USE,YNAT,'ARI',&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
    ENDDO
    !    
    ZDG2  (:)=0.0
    PK%XDROOT(:)=0.0    
      DO JJ=1,KNI
        !
        !DROOT DEPENDS ON ROOTFRAC
        DO JL=KGROUND,1,-1
          IF( PK%XROOTFRAC(JJ,JL)>=1.0 )THEN
            ZDG2  (JJ) = PK%XDG(JJ,JL)
            PK%XDROOT(JJ) = PK%XDG(JJ,JL)
          ELSEIF (JL<KGROUND.AND.PK%XROOTFRAC(JJ,JL)>0.0) THEN
            IF (PK%NWG_LAYER(JJ)<=JL) PK%NWG_LAYER(JJ) = JL+1
            EXIT
          ENDIF
        ENDDO
        !
        IF(PK%XDROOT(JJ)==0.0.AND.ZDG2(JJ)==0.0)THEN
          JL=PK%NWG_LAYER(JJ)
          ZDG2(JJ)=MIN(0.6,PK%XDG(JJ,JL))
        ENDIF
        !
      ENDDO
    !
  ELSE
    !
    !DEPENDS ON DROOT
    IF (GDATA .AND. ANY(DTV%LDATA_ROOT_LIN)) THEN
      CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                      ZROOT_LIN(:),DTV%XPAR_VEGTYPE2,DTV%XPAR_ROOT_LIN(:,:),DTV%NPAR_VEG_IRR_USE,YDIF,'ARI',&
            PK%NR_P,IO%NPATCH,KPATCH)
    ELSE
      CALL AV_PGD_1P(DTCO, ZROOT_LIN(:),PCOVER,XDATA_ROOT_LIN(:,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE,YDIF,'ARI',OCOVER,&
            PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
    ENDIF
    !
    IF (GDATA .AND. ANY(DTV%LDATA_ROOT_EXTINCTION)) THEN
      CALL AV_PGD_PARAM(DTV%XPAR_LAI, DTV%XPAR_VEG, &
                    ZROOT_EXT(:),DTV%XPAR_VEGTYPE2,DTV%XPAR_ROOT_EXTINCTION(:,:),DTV%NPAR_VEG_IRR_USE,YDIF,'ARI',&
            PK%NR_P,IO%NPATCH,KPATCH)
    ELSE
      CALL AV_PGD_1P(DTCO, ZROOT_EXT(:),PCOVER,XDATA_ROOT_EXTINCTION(:,:),DTV%XPAR_IRRIGFRAC,DTV%NPAR_VEG_IRR_USE, &
            YDIF,'ARI',OCOVER,PK%NR_P,IO%NPATCH,KPATCH,KDECADE=KDEC)
    ENDIF
    !
    CALL INI_DATA_ROOTFRAC(PK%XDG,PK%XDROOT,ZROOT_EXT,ZROOT_LIN,PK%XROOTFRAC)
    !
  ENDIF
  !
  WHERE(PK%XROOTFRAC(:,:)/=XUNDEF) PK%XROOTFRAC(:,:)=NINT(PK%XROOTFRAC(:,:)*XPREC)/XPREC
  !
  PK%XDG2(:) = ZDG2(:)
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('CONVERT_PATCH_ISBA:SET_GRID_PARAM',1,ZHOOK_HANDLE)
!
END SUBROUTINE SET_GRID_PARAM
!-------------------------------------------------------------------------------
END SUBROUTINE CONVERT_PATCH_ISBA
