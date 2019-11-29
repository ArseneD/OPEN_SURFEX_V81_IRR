!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################
      SUBROUTINE READ_SURF_ISBA_PAR_n (DTCO, U, GCP, KPATCH, NPAR_VEG_IRR_USE, HPROGRAM, HREC, KLUOUT, KSIZE, &
                                       KVERSION, KBUGFIX, ODATA, PFIELD, KRESP, HCOMMENT, HDIR)
!     #######################
!
!!    MODIFICATIONS
!!    -------------
!
USE MODD_DATA_COVER_n,     ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n,       ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_DATA_COVER_PAR,   ONLY : NVEGTYPE
USE MODD_AGRI,             ONLY : NVEG_IRR, LAGRIP, LIRRIGMODE
!
USE MODI_READ_SURF
USE MODI_HOR_INTERPOL
USE MODI_PUT_ON_ALL_VEGTYPES
USE MODI_VEGTYPE_TO_PATCH
USE MODI_VEGTYPE_TO_PATCH_IRRIG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
INTEGER, INTENT(IN) :: KPATCH
!
INTEGER, DIMENSION(:),   INTENT(IN) :: NPAR_VEG_IRR_USE ! vegtype with irrigation
 CHARACTER(LEN=6),       INTENT(IN) :: HPROGRAM ! calling program
 CHARACTER(LEN=*),       INTENT(IN) :: HREC   ! name of the article to be read
!
INTEGER,                 INTENT(IN) :: KLUOUT
INTEGER,                 INTENT(IN) :: KSIZE
INTEGER,                 INTENT(IN) :: KVERSION
INTEGER,                 INTENT(IN) :: KBUGFIX
LOGICAL, DIMENSION(:),   INTENT(INOUT) :: ODATA
!
REAL, DIMENSION(:,:),    INTENT(OUT):: PFIELD ! array containing the data field  

INTEGER                  ,INTENT(OUT) :: KRESP      ! KRESP  : return-code if a problem appears
 CHARACTER(LEN=*),OPTIONAL,INTENT(OUT) :: HCOMMENT   ! name of the article to be read
 CHARACTER(LEN=1),OPTIONAL,INTENT(IN)  :: HDIR       ! type of field :
!                                                   ! 'H' : field with
!                                                   !       horizontal spatial dim.
!                                                   ! '-' : no horizontal dim.
!
!* local variables
!  ---------------
!
 CHARACTER(LEN=12) :: YREC
 CHARACTER(LEN=3)  :: YVEG
REAL, DIMENSION(:,:)  , ALLOCATABLE :: ZFIELD
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZFIELD_PATCH
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZFIELD_VEGTYPE
 CHARACTER(LEN=1)   :: YDIR
INTEGER             :: INI, JP, IPATCH, JV, JV2, NVEG_ALL
REAL(KIND=JPRB)     :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_SURF_ISBA_PAR_n',0,ZHOOK_HANDLE)
!
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
INI = SIZE(PFIELD,1)
!
IF ( U%LECOSG .AND. (LIRRIGMODE .OR. LAGRIP) .AND. .NOT.ANY(NPAR_VEG_IRR_USE==0) ) THEN
  NVEG_ALL=NVEGTYPE+NVEG_IRR 
ELSE
  NVEG_ALL=NVEGTYPE
ENDIF
ALLOCATE(ZFIELD(KSIZE, NVEG_ALL))
ALLOCATE(ZFIELD_PATCH(SIZE(PFIELD,1),1,KPATCH))
ALLOCATE(ZFIELD_VEGTYPE(SIZE(PFIELD,1),1,NVEG_ALL))
!
ZFIELD(:,:) = 0.
!
IF (KVERSION<7) THEN
  !
  ! fields were written by patch
  CALL READ_SURF(HPROGRAM,HREC,ZFIELD(:,1:KPATCH),KRESP,HCOMMENT=HCOMMENT,HDIR=YDIR)
  ! case zoom
  IF (INI.NE.KSIZE) THEN
    CALL HOR_INTERPOL(DTCO, U, GCP, KLUOUT,ZFIELD(:,1:KPATCH),PFIELD(:,1:KPATCH))
  ELSE
    ! classical case
    PFIELD(:,1:KPATCH) = ZFIELD(:,1:KPATCH)
  ENDIF
  !
  ! classical case
  IF (SIZE(PFIELD,2)==NVEG_ALL) THEN
    DO JP = 1, KPATCH
      ZFIELD_PATCH(:,1,JP) = PFIELD(:,JP)
    ENDDO         
    ! patchs shared on vegtypes
    CALL PUT_ON_ALL_VEGTYPES(INI,1,KPATCH,NVEG_ALL,NPAR_VEG_IRR_USE,ZFIELD_PATCH,ZFIELD_VEGTYPE)
    PFIELD(:,:) = ZFIELD_VEGTYPE(:,1,:)
  ENDIF
  !
ELSE
  !
  IF (KVERSION>8 .OR. (KVERSION==8 .AND. KBUGFIX>=1)) THEN
    !
    DO JV = 1,NVEG_ALL
      IF (ODATA(JV)) THEN
        WRITE(YVEG,FMT='(A1,I2.2)') 'V',JV
        YREC = TRIM(ADJUSTL(HREC))//YVEG
        CALL READ_SURF(HPROGRAM,YREC,ZFIELD(:,JV),KRESP,HCOMMENT=HCOMMENT,HDIR=YDIR)
        !!ELSE
        !! Druel Arsène: These lines below permit to transfert value for one vegtype for the next one (if it is not define)
        !!               But that is difficil to controle if you do not want that ! 
        !!               So, removed (the best is to define good values in pgd_isba_par.F90)
        !!IF (.NOT.ODATA(JV)) THEN
        !!  DO JV2=JV,1,-1
        !!    IF (ODATA(JV2)) THEN
        !!      ZFIELD(:,JV) = ZFIELD(:,JV2)
        !!      EXIT
        ! !   ENDIF
        !!  ENDDO
        !!ENDIF
      ENDIF
    ENDDO
    !
  ELSE
    !
    ! field written by vegtype
    CALL READ_SURF(HPROGRAM,HREC,ZFIELD(:,:),KRESP,HCOMMENT=HCOMMENT,HDIR=YDIR)
    !
  ENDIF
  !
  ! case zoom
  IF (INI.NE.KSIZE) THEN
    CALL HOR_INTERPOL(DTCO, U, GCP, KLUOUT,ZFIELD(:,:),ZFIELD_VEGTYPE(:,1,:))
  ELSE
    ! classical case
    ZFIELD_VEGTYPE(:,1,:) = ZFIELD(:,:)
  ENDIF
  !
  ! case mode_read_extern
  IF (SIZE(PFIELD,2).NE.NVEG_ALL) THEN
    IPATCH = SIZE(PFIELD,2)
    PFIELD(:,:) = 0.
    DO JV = 1, NVEG_ALL
      IF ( NVEG_IRR /= 0 .AND. U%LECOSG .AND. (LIRRIGMODE .OR. LAGRIP) .AND. .NOT.ANY(NPAR_VEG_IRR_USE==0) ) THEN
        CALL VEGTYPE_TO_PATCH_IRRIG(JV,IPATCH,NPAR_VEG_IRR_USE,JP)
      ELSE
        CALL VEGTYPE_TO_PATCH(JV,IPATCH,JP)
      ENDIF
      ! artefact to simplify in mode_read_extern: we take the upper value
      PFIELD(:,JP) = MAX(PFIELD(:,JP),ZFIELD_VEGTYPE(:,1,JV))
    ENDDO
  ELSE
    ! classical case
    PFIELD(:,:) = ZFIELD_VEGTYPE(:,1,:)
  ENDIF        
ENDIF
!
DEALLOCATE(ZFIELD_PATCH, ZFIELD, ZFIELD_VEGTYPE)
!
IF (LHOOK) CALL DR_HOOK('READ_SURF_ISBA_PAR_n',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------
!
END SUBROUTINE READ_SURF_ISBA_PAR_n
