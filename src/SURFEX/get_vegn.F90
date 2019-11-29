!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################################################################
      SUBROUTINE GET_VEG_n(HPROGRAM, KI, U, IO, S, NP, NPE, NPAR_VEG_IRR_USE, PLAI, PVH)
!     #######################################################################
!
!!****  *GET_VEG_n* - gets some veg fields on atmospheric grid
!!
!!    PURPOSE
!!    -------
!!
!!    This program returns some veg variables needed by the atmosphere
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
!!	P. Aumond	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2009
!!      A. Druel    02/2019 : streamlines the code and adapt it to be compatible with irrigation
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_ATM_n,     ONLY : SURF_ATM_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_S_t, ISBA_P_t, ISBA_PE_t, ISBA_NP_t, ISBA_NPE_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_PAR
USE MODD_AGRI,           ONLY : NVEG_IRR
!
USE MODI_GET_LUOUT
USE MODI_VEGTYPE_TO_PATCH
!                                
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
CHARACTER(LEN=6),   INTENT(IN)   :: HPROGRAM    
INTEGER,            INTENT(IN)   :: KI         ! number of points
!
TYPE(SURF_ATM_t),     INTENT(INOUT) :: U
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t),       INTENT(INOUT) :: S
TYPE(ISBA_NP_t),      INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t),     INTENT(INOUT) :: NPE
!
INTEGER,DIMENSION(:), INTENT(IN) :: NPAR_VEG_IRR_USE ! vegtype with irrigation

!
REAL, DIMENSION(KI), INTENT(OUT) :: PVH    ! Tree height 
REAL, DIMENSION(KI), INTENT(OUT) :: PLAI   
!-------------------------------------------------------------------------------
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!  Arrays defined for each tile
!  
!
TYPE(ISBA_P_t), POINTER  :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
INTEGER                            :: JI, JJ           ! loop index over tiles
INTEGER                            :: ILUOUT       ! unit numberi
REAL, DIMENSION(U%NSIZE_NATURE)    :: ZH_TREE, ZLAI, ZWORK
!INTEGER:: IPATCH_TRBE, IPATCH_TRBD, IPATCH_TEBE, IPATCH_TEBD, IPATCH_TENE, &
!          IPATCH_BOBD, IPATCH_BONE, IPATCH_BOND, IMASK, JP, JVEG
INTEGER                            :: IPATCH, IMASK, JP, JVEG, JVEG2
LOGICAL, DIMENSION(IO%NPATCH)      :: L_IS_TREE
! 
!-------------------------------------------------------------------------------
!
!*   0. Logical unit for writing out
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*       1. Passage dur le masque global
!              -------------------------------
!
L_IS_TREE(:) = .FALSE.
DO JVEG = 1, NVEGTYPE+NVEG_IRR
  !
  JVEG2 = JVEG
  IF ( JVEG > NVEGTYPE ) JVEG2 = NPAR_VEG_IRR_USE( JVEG - NVEGTYPE )
  !
  IF ( JVEG2 == NVT_TRBE .OR. JVEG2 == NVT_TRBD .OR. JVEG2 == NVT_TEBE .OR. JVEG2 == NVT_TEBD .OR. &
       JVEG2 == NVT_TENE .OR. JVEG2 == NVT_BOBD .OR. JVEG2 == NVT_BONE .OR. JVEG2 == NVT_BOND) THEN
    CALL VEGTYPE_TO_PATCH_IRRIG(JVEG, IO%NPATCH, NPAR_VEG_IRR_USE, IPATCH)
    L_IS_TREE(IPATCH) = .TRUE.
  ENDIF
  !
ENDDO
!
ZH_TREE(:) = 0.
ZLAI(:) = 0.
ZWORK(:) = 0.
!
DO JP = 1,IO%NPATCH
  !
  IF ( L_IS_TREE(JP) ) THEN
    !
    PK => NP%AL(JP)
    PEK => NPE%AL(JP)
    !
    DO JJ=1,PK%NSIZE_P
      !
      IMASK = PK%NR_P(JJ)
      !
      IF (PK%XH_TREE(JJ)/=XUNDEF) THEN
        !
        ZH_TREE(IMASK) = ZH_TREE(IMASK) + PK%XH_TREE(JJ) * PK%XPATCH(JJ)
        !
        ZLAI(IMASK)  = ZLAI(IMASK) + PEK%XLAI(JJ) * PK%XPATCH(JJ)
        !
        ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JJ)
        !
      ENDIF
      !
    ENDDO
    !
  ENDIF
  !
ENDDO  
!
WHERE(ZWORK(:)/=0.) 
  ZH_TREE(:) = ZH_TREE(:)/ZWORK(:)
  ZLAI(:) = ZLAI(:)/ZWORK(:)
END WHERE
!
!DO JJ = 1,SIZE(ZLAI)
!  ZLAI(JJ) = U%XNATURE(U%NR_NATURE(JJ)) * ZLAI(JJ)
!ENDDO
!
!*       2. Envoi les variables vers mesonH 
!             ------------------------------

IF ( SIZE(PVH) /= SIZE(ZH_TREE) ) THEN
  WRITE(ILUOUT,*) 'try to get VH field from atmospheric model, but size is not correct'
  WRITE(ILUOUT,*) 'size of field expected by the atmospheric model (PVH) :', SIZE(PVH)
  WRITE(ILUOUT,*) 'size of field inthe surface                     (XVH) :', SIZE(ZH_TREE)
  CALL ABOR1_SFX('GET_VHN: VH SIZE NOT CORRECT')
ELSE
  PVH = ZH_TREE
END IF
!
!==============================================================================
!
!-------------------------------------------------------------------------------
!
IF ( SIZE(PLAI) /= SIZE(ZLAI) ) THEN
  WRITE(ILUOUT,*) 'try to get LAI field from atmospheric model, but size is not correct'
  WRITE(ILUOUT,*) 'size of field expected by the atmospheric model (PLAI) :', SIZE(PLAI)
  WRITE(ILUOUT,*) 'size of field inthe surface                     (XLAI) :', SIZE(ZLAI)
  CALL ABOR1_SFX('GET_LAIN: LAI SIZE NOT CORRECT')
ELSE
  PLAI = ZLAI
END IF
!
!==============================================================================
!
!-------------------------------------------------------------------------------
!
!==============================================================================
!
END SUBROUTINE GET_VEG_n
