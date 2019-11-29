!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!!   ###############################
     SUBROUTINE CH_BVOCEM_n (SV, NGB, GB, IO, S, NP, NPE, PSW_FORBIO, PRHOA, NPAR_VEG_IRR_USE, PSFTS)
!!   ###############################
!!
!!***  *BVOCEM*
!! 
!!    PURPOSE
!!    -------
!!    Calculate the biogenic emission fluxes according to the 
!!    subgrid vegetation given by the soil interface
!!
!!    METHOD
!!    ------
!!
!!
!!    AUTHOR
!!    ------
!!    F. Solmon (LA) & V. Masson (CNRM)
!!    
!!    MODIFICATIONS
!!    -------------
!!    Original: 25/10/00
!!    P. Tulet  30/07/03 externalisation of biogenics fluxes (2D => 1D)
!!    R. Alkama 04/2012  add 7 new vegtype (19 instead 12)
!!    A. Druel  02/2019  streamlines the code and adapt it to be compatible with new irrigation
!!
!!    EXTERNAL
!!    --------
!
USE MODD_SV_n, ONLY : SV_t
!
USE MODD_GR_BIOG_n, ONLY : GR_BIOG_t, GR_BIOG_NP_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t, ISBA_NP_t, ISBA_NPE_t
!
USE MODI_VEGTYPE_TO_PATCH_IRRIG
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_BVOC_PAR
USE MODD_CSTS,ONLY : XMD, XAVOGADRO
USE MODD_CO2V_PAR
USE MODD_SURF_PAR,ONLY:XUNDEF
USE MODD_ISBA_PAR
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE, NVT_TEBD, NVT_BONE, NVT_TRBE, &
                                NVT_TRBD, NVT_TEBE, NVT_TENE, NVT_BOBD, &
                                NVT_BOND, NVT_SHRB, NVT_BOGR, NVT_GRAS, & 
                                NVT_TROG, NVT_PARK, NVT_FLTR, NVT_FLGR, &
                                NVT_C3, NVT_C3W, NVT_C3S, NVT_C4, NVT_IRR 
USE MODD_AGRI,           ONLY : NVEG_IRR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
!
IMPLICIT NONE
!
TYPE(SV_t),           INTENT(INOUT) :: SV
TYPE(GR_BIOG_NP_t),   INTENT(INOUT) :: NGB
TYPE(GR_BIOG_t),      INTENT(INOUT) :: GB
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t),       INTENT(INOUT) :: S
TYPE(ISBA_NP_t),      INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t),     INTENT(INOUT) :: NPE
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSW_FORBIO
REAL, DIMENSION(:),   INTENT(IN)    :: PRHOA
INTEGER,DIMENSION(:), INTENT(IN)    :: NPAR_VEG_IRR_USE ! vegtype with irrigation
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSFTS
!
!*       0.1  declaration of arguments
!
!*   0.1 Declaration of local variables
!
REAL, DIMENSION(SIZE(PSW_FORBIO,1)) :: ZRAD_PAR,  ZLCOR_RAD
!                            PAR radiation in case of ISBA-STD use
!
REAL, DIMENSION(SIZE(PSW_FORBIO,1)) :: ZFISO_FOR  , ZFMONO_FOR,   &
                                       ZFISO_GRASS, ZFMONO_GRASS, &
                                       ZFISO_CROP , ZFMONO_CROP     
!                                Fluxes coming from different landuse
REAL, DIMENSION(SIZE(PSW_FORBIO,1), NVEGTYPE+NVEG_IRR) :: ZTCOR ,ZTCORM
!
!REAL, DIMENSION(SIZE(PSW_FORBIO,1),SIZE(S%XABC),NVEGTYPE) :: ZBVOCPAR 
!!                                PAR at gauss level in micromolphot/m2/s
!
REAL, DIMENSION(SIZE(PSW_FORBIO,1)) :: ZISOPOT, ZMONOPOT, ZRATIO
!
INTEGER:: KNGAUSS     
!                        nbre of gauss level in integration
!                        index of patch corresponding to forest(+ligneaous)
INTEGER:: JP, JSV, IMASK, JI, JK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N',0,ZHOOK_HANDLE)
!
!* 1. Contribution of forest and ligneous vegetation 
!   from ISOPOT and MONOPOT maps 
!   ------------------------------------------------
!
!* 1.0 Preliminary : patch index corresponding to forest
!
!2.Contribution of other types of vegetation than forest, consider the vegtype fraction in the pixel 
!------------------------------------------------------------------------------------------
!
!* 2.0 Preliminary : patch index corresponding to grassland, crops (C3+C4)
!
!1.1.1 Using ISBA_Ags explicit light attenuation 
! number of g Gauss level for the integration 
IF (IO%CPHOTO/='NON') THEN
  KNGAUSS = SIZE(S%XABC)
ELSE
  !1.1.2 using isba std version 
  ZRAD_PAR (:)= 0.
  DO JP = 1,IO%NPATCH
    ZRAD_PAR (:)= ZRAD_PAR (:) +(PSW_FORBIO(:,JP)*S%XPATCH(:,JP) ) * XPARCF * 4.7 
  END DO
  ZLCOR_RAD (:) = ZLCOR_FUNC(ZRAD_PAR(:))
ENDIF
!  
!
ZRATIO (:) = 0.
DO JP = 1,NVEGTYPE+NVEG_IRR
  JK = JP
  IF ( JP > NVEGTYPE ) JK = NPAR_VEG_IRR_USE( JP - NVEGTYPE )
  !
  IF ( JK == NVT_TEBD .OR. JK == NVT_BONE .OR. JK == NVT_TRBE .OR. JK == NVT_TRBD .OR. JK == NVT_TEBE .OR. &
       JK == NVT_TENE .OR. JK == NVT_BOBD .OR. JK == NVT_BOND .OR. JK == NVT_SHRB .OR. JK == NVT_BOGR .OR. &
       JK == NVT_GRAS .OR. JK == NVT_TROG .OR. JK == NVT_PARK .OR. JK == NVT_C3   .OR. JK == NVT_C4   .OR. &
       ( NVT_PARK==0 .AND. NVT_FLTR/=0 .AND. NVT_FLGR/=0 .AND. (JK == NVT_FLTR .OR. JK == NVT_FLGR))  .OR. &
       ( NVT_C3  ==0 .AND. NVT_C3W /=0 .AND. NVT_C3S /=0 .AND. (JK == NVT_C3W  .OR. JK == NVT_C3S ))  .OR. &
       JK == NVT_IRR ) CALL BY_PATCH(JP, NPAR_VEG_IRR_USE, ZTCOR(:,JP), ZTCORM(:,JP))
  !
  IF ( JK == NVT_TEBD .OR. JK == NVT_BONE .OR. JK == NVT_TRBE .OR. JK == NVT_TRBD .OR. JK == NVT_TEBE .OR. &
       JK == NVT_TENE .OR. JK == NVT_BOBD .OR. JK == NVT_BOND .OR. JK == NVT_SHRB ) ZRATIO(:) = ZRATIO(:) + S%XVEGTYPE(:,JP)
  !
END DO
!
!
WHERE (ZRATIO(:)/=0.)
  ZISOPOT (:) = GB%XISOPOT (:) / ZRATIO(:)
  ZMONOPOT(:) = GB%XMONOPOT(:) / ZRATIO(:) 
ELSEWHERE
  ZISOPOT (:) = 0.
  ZMONOPOT(:) = 0.
END WHERE
!                       
CALL BY_VEG9(NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD, NVT_TEBE, NVT_TENE, NVT_BOBD, &
             NVT_BOND, NVT_SHRB, NVT_FLTR, ZISOPOT, ZMONOPOT, ZFISO_FOR, ZFMONO_FOR)
!
ZISOPOT (:) = XISOPOT_GRASS
ZMONOPOT(:) = XMONOPOT_GRASS
IF (NVT_PARK/=0) THEN
  CALL BY_VEG4(NVT_GRAS, NVT_TROG, NVT_PARK, NVT_BOGR, ZISOPOT, ZMONOPOT, ZFISO_GRASS, ZFMONO_GRASS)
ELSEIF (NVT_FLGR/=0) THEN
  CALL BY_VEG4(NVT_GRAS, NVT_TROG, NVT_FLGR, NVT_BOGR, ZISOPOT, ZMONOPOT, ZFISO_GRASS, ZFMONO_GRASS)
ENDIF
!
ZISOPOT (:) = XISOPOT_CROP
ZMONOPOT(:) = XMONOPOT_CROP!
IF (NVT_C3/=0 .AND. NVT_IRR/=0) THEN
  CALL BY_VEG3(NVT_C3 , NVT_C4 , NVT_IRR, ZISOPOT, ZMONOPOT, ZFISO_CROP, ZFMONO_CROP)
ELSEIF (NVT_C3W/=0 .AND. NVT_C3S/=0) THEN
  CALL BY_VEG3(NVT_C3W, NVT_C3S, NVT_C4 , ZISOPOT, ZMONOPOT, ZFISO_CROP, ZFMONO_CROP)
ENDIF
!
!---------------------------------------------------------------------------------------
!
!3.Summation of different contribution for fluxes 
!------------------------------------------------
!
!isoprene in ppp.m.s-1
GB%XFISO (:)=(3.0012E-10/3600.) * ( ZFISO_FOR (:) + ZFISO_GRASS(:) + ZFISO_CROP(:) ) + 1E-17
!monoterpenes
GB%XFMONO(:)=(1.5006E-10/3600.) * ( ZFMONO_FOR(:) + ZFMONO_GRASS(:)+ ZFMONO_CROP(:) ) + 1E-17
!
! conversion in molecules/m2/s
!
GB%XFISO(:)  = GB%XFISO(:)  * XAVOGADRO * PRHOA(:) / XMD
GB%XFMONO(:) = GB%XFMONO(:) * XAVOGADRO * PRHOA(:) / XMD
!
DO JSV=SV%NSV_CHSBEG,SV%NSV_CHSEND
  IF (SV%CSV(JSV) == "BIO") THEN
    ! RELACS CASE
    PSFTS(:,JSV) = PSFTS(:,JSV) + (GB%XFISO(:) + GB%XFMONO(:)) 
  ELSE IF (SV%CSV(JSV) == "ISO" .OR. SV%CSV(JSV) == "ISOP") THEN
    ! RACM CASE
    PSFTS(:,JSV) = PSFTS(:,JSV) + GB%XFISO(:)  
  ELSE IF (SV%CSV(JSV) == "API"  .OR. SV%CSV(JSV) == "LIM" .OR. &
           SV%CSV(JSV) == "BIOL" .OR. SV%CSV(JSV) == "BIOH" ) THEN
    ! RACM CASE
    ! CACM or RELACS 2 CASE     
    PSFTS(:,JSV) = PSFTS(:,JSV) + 0.5 * GB%XFMONO(:) 
  ENDIF
END DO
!
!**********************************************************************************
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N',1,ZHOOK_HANDLE)
CONTAINS
!
SUBROUTINE BY_PATCH(NVT_VEG, NPAR_VEG_IRR_USE, PTCOR, PTCORM)
!
IMPLICIT NONE
!
INTEGER,              INTENT(IN)  :: NVT_VEG
INTEGER,DIMENSION(:), INTENT(IN)  :: NPAR_VEG_IRR_USE ! vegtype with irrigation
REAL,   DIMENSION(:), INTENT(OUT) :: PTCOR
REAL,   DIMENSION(:), INTENT(OUT) :: PTCORM
!
REAL, DIMENSION(SIZE(PSW_FORBIO,1))              :: ZBVOCSG
REAL, DIMENSION(SIZE(PSW_FORBIO,1),SIZE(S%XABC)) :: ZBVOCPAR
INTEGER         :: IPATCH, JLAYER, IT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:BY_PATCH',0,ZHOOK_HANDLE)
!
CALL VEGTYPE_TO_PATCH_IRRIG(NVT_VEG, IO%NPATCH, NPAR_VEG_IRR_USE, IPATCH)

!
PTCOR  (:) = 0.
PTCORM (:) = 0.
ZBVOCPAR(:,:) = 0.
!
DO IT=1,SIZE(NPE%AL(IPATCH)%XTG,1)
  !
  IMASK = NP%AL(IPATCH)%NR_P(IT)
  IF (NPE%AL(IPATCH)%XTG(IT,1).LE.1000.) THEN
    PTCORM(IMASK) = ZTCORM0_FUNC(NPE%AL(IPATCH)%XTG(IT,1))
    PTCOR (IMASK) = ZTCOR0_FUNC (NPE%AL(IPATCH)%XTG(IT,1))
  ENDIF
  !PAR over Forest canopies, in micro-molE.m-2.s-1
  IF (IO%CPHOTO/='NON') THEN
    ZBVOCPAR(IMASK,:) = NGB%AL(IPATCH)%XIACAN(IT,:)*4.7
  ENDIF
ENDDO
!
IF (IO%CPHOTO/='NON') THEN
  !Calculation of radiative attenuation effect in the canopy on correction factor
  ZBVOCSG(:) = 0.
  DO JLAYER=1,KNGAUSS
    ZBVOCSG(:) = ZBVOCSG(:) + S%XPOI(JLAYER) * ZLCOR_FUNC(ZBVOCPAR(:,JLAYER)) 
  ENDDO
  PTCOR(:) = PTCOR(:) * ZBVOCSG(:)
ELSE
  PTCOR(:) = PTCOR(:) * XCANFAC * ZLCOR_RAD(:)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:BY_PATCH',1,ZHOOK_HANDLE)
!
END SUBROUTINE BY_PATCH
!--------------------------------------------------------------------------
SUBROUTINE BY_VEG3(NVT_V1, NVT_V2, NVT_V3, &
                  PISOPOT, PMONOPOT, PFISO, PFMONO)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: NVT_V1
INTEGER, INTENT(IN) :: NVT_V2
INTEGER, INTENT(IN) :: NVT_V3
REAL, DIMENSION(:), INTENT(IN)  :: PISOPOT
REAL, DIMENSION(:), INTENT(IN)  :: PMONOPOT
REAL, DIMENSION(:), INTENT(OUT) :: PFISO
REAL, DIMENSION(:), INTENT(OUT) :: PFMONO
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:BY_VEG3',0,ZHOOK_HANDLE)
!
!isoprene flux 
!!
!! warning, XISOPOT external map accounts for the total forest fraction
!
PFISO(:) = 0.
PFMONO(:) = 0.
!
DO JP = 1,NVEGTYPE+NVEG_IRR
  JK = JP
  IF ( JP > NVEGTYPE ) JK = NPAR_VEG_IRR_USE( JP - NVEGTYPE )
  !
  IF ( JK == NVT_V1 .OR.  JK == NVT_V2 .OR. JK == NVT_V3 ) THEN
    !
    WHERE ( S%XVEGTYPE(:,JP) > 0. )
      !
      PFISO(:) = PFISO(:) + PISOPOT(:) * ZTCOR(:,JP) * S%XVEGTYPE(:,JP)
      !
      PFMONO(:) = PFMONO(:) + PMONOPOT(:) * ZTCORM(:,JP) * S%XVEGTYPE(:,JP)
      !
    ENDWHERE
    !
  ENDIF
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:BY_VEG3',1,ZHOOK_HANDLE)
!
END SUBROUTINE BY_VEG3
!--------------------------------------------------------------------------
SUBROUTINE BY_VEG4(NVT_V1, NVT_V2, NVT_V3, NVT_V4,&
                  PISOPOT, PMONOPOT, PFISO, PFMONO)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: NVT_V1
INTEGER, INTENT(IN) :: NVT_V2
INTEGER, INTENT(IN) :: NVT_V3
INTEGER, INTENT(IN) :: NVT_V4
REAL, DIMENSION(:), INTENT(IN)  :: PISOPOT
REAL, DIMENSION(:), INTENT(IN)  :: PMONOPOT
REAL, DIMENSION(:), INTENT(OUT) :: PFISO
REAL, DIMENSION(:), INTENT(OUT) :: PFMONO
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:BY_VEG4',0,ZHOOK_HANDLE)
!
!isoprene flux 
!!
!! warning, XISOPOT external map accounts for the total forest fraction
!
PFISO(:) = 0.
PFMONO(:) = 0.
!
DO JP = 1,NVEGTYPE+NVEG_IRR
  JK = JP
  IF ( JP > NVEGTYPE ) JK = NPAR_VEG_IRR_USE( JP - NVEGTYPE )
  !
  IF ( JK == NVT_V1 .OR.  JK == NVT_V2 .OR. JK == NVT_V3 .OR. JK == NVT_V4 ) THEN
    !
    WHERE ( S%XVEGTYPE(:,JP) > 0. )
      !
      PFISO(:) = PFISO(:) + PISOPOT(:) * ZTCOR(:,JP) * S%XVEGTYPE(:,JP)
      !
      PFMONO(:) = PFMONO(:) + PMONOPOT(:) * ZTCORM(:,JP) * S%XVEGTYPE(:,JP)
      !
    ENDWHERE
    !
  ENDIF
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:BY_VEG4',1,ZHOOK_HANDLE)
!
END SUBROUTINE BY_VEG4
!--------------------------------------------------------------------------
SUBROUTINE BY_VEG9(NVT_V1, NVT_V2, NVT_V3, NVT_V4, NVT_V5, NVT_V6,     &
               NVT_V7, NVT_V8, NVT_V9, NVT_V10, PISOPOT, PMONOPOT, PFISO, PFMONO)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: NVT_V1
INTEGER, INTENT(IN) :: NVT_V2
INTEGER, INTENT(IN) :: NVT_V3
INTEGER, INTENT(IN) :: NVT_V4
INTEGER, INTENT(IN) :: NVT_V5
INTEGER, INTENT(IN) :: NVT_V6
INTEGER, INTENT(IN) :: NVT_V7
INTEGER, INTENT(IN) :: NVT_V8
INTEGER, INTENT(IN) :: NVT_V9
INTEGER, INTENT(IN) :: NVT_V10
REAL, DIMENSION(:), INTENT(IN)  :: PISOPOT
REAL, DIMENSION(:), INTENT(IN)  :: PMONOPOT
REAL, DIMENSION(:), INTENT(OUT) :: PFISO
REAL, DIMENSION(:), INTENT(OUT) :: PFMONO
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:BY_VEG9',0,ZHOOK_HANDLE)
!
!isoprene flux 
!!
!! warning, XISOPOT external map accounts for the total forest fraction
!
PFISO(:) = 0.
PFMONO(:) = 0.
!
DO JP = 1,NVEGTYPE+NVEG_IRR
  JK = JP
  IF ( JP > NVEGTYPE ) JK = NPAR_VEG_IRR_USE( JP - NVEGTYPE )
  !
  IF ( JK == NVT_V1 .OR.  JK == NVT_V2 .OR. JK == NVT_V3 .OR. JK == NVT_V4 .OR. JK == NVT_V5 .OR. &
       JK == NVT_V6 .OR.  JK == NVT_V7 .OR. JK == NVT_V8 .OR. JK == NVT_V9 .OR. JK == NVT_V10 ) THEN
    !
    WHERE ( S%XVEGTYPE(:,JP) > 0. )
      !
      PFISO(:) = PFISO(:) + PISOPOT(:) * ZTCOR(:,JP) * S%XVEGTYPE(:,JP)
      !
      PFMONO(:) = PFMONO(:) + PMONOPOT(:) * ZTCORM(:,JP) * S%XVEGTYPE(:,JP)
      !
    ENDWHERE
    !
  ENDIF
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:BY_VEG9',1,ZHOOK_HANDLE)
!
END SUBROUTINE BY_VEG9
!--------------------------------------------------------------------------
FUNCTION ZLCOR_FUNC(ZX)

REAL, DIMENSION(:)          :: ZX
REAL, DIMENSION(SIZE(ZX))   :: ZLCOR_FUNC
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:ZLCOR_FUNC',0,ZHOOK_HANDLE)
ZLCOR_FUNC(:)=0.
ZLCOR_FUNC(:) = ZX(:)*XISO_CL*XISO_ALF/(1+(XISO_ALF**2)*(ZX(:)**2))**0.5
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:ZLCOR_FUNC',1,ZHOOK_HANDLE)
!
END FUNCTION ZLCOR_FUNC
!---------------------------------------------------------------------------
FUNCTION ZTCOR0_FUNC(ZX)

REAL, PARAMETER :: R   = 8.314
REAL            :: ZX
REAL            :: ZTCOR0_FUNC
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:ZTCOR0_FUNC',0,ZHOOK_HANDLE)
!
ZTCOR0_FUNC=0.
ZTCOR0_FUNC = EXP(XISO_CT1*(ZX-XISO_BTS)/(R*XISO_BTS*ZX))     &
          /(1+EXP(XISO_CT2*(ZX-XISO_BTM)/(R*XISO_BTS*ZX)))
       !
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:ZTCOR0_FUNC',1,ZHOOK_HANDLE)
END FUNCTION ZTCOR0_FUNC
!---------------------------------------------------------------------------
FUNCTION ZTCORM0_FUNC(ZX)

REAL           :: ZX
REAL  :: ZTCORM0_FUNC
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!      
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:ZTCORM0_FUNC',0,ZHOOK_HANDLE)
ZTCORM0_FUNC= 0.
ZTCORM0_FUNC = EXP(XMONO_BETA*(ZX-XMONO_T3))
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:ZTCORM0_FUNC',1,ZHOOK_HANDLE)
!
END FUNCTION ZTCORM0_FUNC
!
!---------------------------------------------------------------------------
!
END SUBROUTINE CH_BVOCEM_n
