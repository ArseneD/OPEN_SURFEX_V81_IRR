!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_ISBA_PAR (DTCO, UG, U, USS, DTV, IO, S, KDIM, HPROGRAM)
!     ##############################################################
!
!!**** *PGD_ISBA_PAR* monitor for averaging and interpolations of cover fractions
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
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/12/97
!!
!!       Modified 08/12/05, P. Le Moigne : user defined fields
!!                 05/2012  R. Alkama    : 19 vegtypes rather than 12    
!!                 02/2012, P. Samuelsson: MEB
!!                 01/2018, J.Etchanchu  : irrigation parameters
!!                 02/2019, A. Druel     : Important adds and adaptation of the code to be compatible with irrigation
!!                                          and new patches (with new verification)
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE, NVT_IRR,                                 &
                                NUT_CPHR, NUT_CPMR, NUT_CPLR, NUT_OPHR, NUT_OPMR,  &  !#rustine
                                NUT_OPLR, NUT_LWLR, NUT_LALR, NUT_SPAR, NUT_INDU,  &  !#rustine
                                NVT_NO, NVT_ROCK, NVT_SNOW, NVT_TEBD, NVT_BONE,    &  !#rustine
                                NVT_TRBE, NVT_C3, NVT_C4, NVT_IRR, NVT_GRAS,       &  !#rustine
                                NVT_TROG,NVT_PARK, NVT_TRBD, NVT_TEBE, NVT_TENE,   &  !#rustine
                                NVT_BOBD, NVT_BOND, NVT_BOGR, NVT_SHRB, NVT_C3W,   &  !#rustine
                                NVT_C3S, NVT_FLTR, NVT_FLGR, NTYPE                    !#rustine

USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
!
USE MODD_AGRI,        ONLY : LAGRIP, LIRRIGMODE, LMULTI_SEASON, JPSTAGE, XTHRESHOLD, XTHRESHOLD_DEFAULT, &
                             NVEG_IRR, NVEG_IRR_DEFAULT, NVEG_IRR_USE_DEFAULT, NPATCH_TREE, NIRR_TYPE, &
                             IRRIGFREQ_CTYPE_DEFAULT, DATES_IRRIG_DEFAULT
USE MODI_VEGTYPE_TO_PATCH
USE MODI_VEGTYPE_TO_PATCH_IRRIG
!
USE MODI_AV_PGD
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_INI_VAR_FROM_DATA
USE MODI_EXTRAPOL_FIELDS
!
USE MODE_POS_SURF
!
!USE MPI
USE MODE_DATES
USE MODD_SURFEX_MPI, ONLY : NRANK
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTV
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
!
INTEGER, INTENT(IN) :: KDIM
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER               :: ILUOUT    ! output listing logical unit
INTEGER               :: ILUNAM    ! namelist file  logical unit
LOGICAL               :: GFOUND    ! true if namelist is found
!
INTEGER               :: JVEG, JL, JK, JN  ! loop counter on patch
LOGICAL               :: GPAR_STRESS   ! type of stress
!
REAL                  :: RAND
INTEGER               :: NRAND, IMONTH, IDAY
INTEGER, DIMENSION(8) :: VALUES
INTEGER, DIMENSION(:), ALLOCATABLE :: SEED
INTEGER, DIMENSION(2) :: TM
!
INTEGER               :: ISIZE_LMEB_PATCH  ! Number of patches with MEB=true
INTEGER               :: ISIZE_PATCH_ORI   ! Number of patches with MEB=true
INTEGER, DIMENSION(IO%NPATCH)    :: NPATCH_LIST
CHARACTER(LEN=45),DIMENSION(2)   :: PATCH_NAME_TMP ! Name of patch (dim 1: patch name, dim 2: vegtype inside)
REAL, DIMENSION (:), ALLOCATABLE :: ZWORK
!
INTEGER, DIMENSION(NVEGTYPE):: LIST_NVT_GRAS, LIST_NVT_NO, LIST_NVT_TEBD, NVEG_LIST ! List with specific of order corresponding to frac town #rustine
INTEGER               :: CHOICE_GRAS, CHOICE_NO, CHOICE_TEBD        ! Choice of corresponding vegetation type  to frac town  #rustine
!
INTEGER, PARAMETER, DIMENSION(12) ::  INB_DM=(/31,28,31,30,31,30,31,31,30,31,30,31/)
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER :: NTIME
INTEGER, PARAMETER :: NTIME_MAX    = 36
INTEGER, PARAMETER :: NGROUND_MAX  = 150
INTEGER, PARAMETER :: NVEGTYPE_MAX = 20
!
REAL, DIMENSION(NVEGTYPE_MAX)   :: XSTRESS   ! 1. if defensive /0. if offensive
!
REAL, DIMENSION(NVEGTYPE_MAX)             :: XUNIF_VEGTYPE    ! fractions of each vegtypes
!
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_VEG        ! vegetation fraction
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_LAI        ! LAI
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_Z0         ! roughness length of vegetation
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_EMIS       ! emissivity
!
REAL, DIMENSION(NVEGTYPE_MAX,NGROUND_MAX)   :: XUNIF_DG         ! soil depths
REAL, DIMENSION(NVEGTYPE_MAX,NGROUND_MAX)   :: XUNIF_ROOTFRAC   ! root fraction profiles
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_GROUND_DEPTH! ground depth
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_ROOT_DEPTH ! root depth
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_ROOT_EXTINCTION! root extinction parameter
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_ROOT_LIN   ! root linear parameter
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_DICE       ! soil ice depth for runoff
!
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_RSMIN      ! minimal stomatal resistance
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_GAMMA      ! gamma parameter
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_WRMAX_CF   ! coefficient for interception
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_RGL        !Rgl
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_CV         ! Cv
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_Z0_O_Z0H   ! ratio of roughness lengths
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_ALBNIR_VEG ! albedo of vegetation (near-infra-red)
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_ALBVIS_VEG ! albedo of vegetation (visible)
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_ALBUV_VEG  ! albedo of vegetation (UV)
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_ALBNIR_SOIL! albedo of soil (near-infra-red)
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_ALBVIS_SOIL! albedo of soil (visible)
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_ALBUV_SOIL ! albedo of soil (UV)
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_GMES       ! Gmes
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_BSLAI      ! Biomass over LAI
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_LAIMIN     ! minimum LAI
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_SEFOLD     ! Sefold
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_GC         ! Gc
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_DMAX       ! Dmax
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_F2I        ! F2I
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_H_TREE     ! height of trees
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_RE25       ! soil respiration parameter
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_CE_NITRO   ! CE for nitrogen
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_CF_NITRO   ! CF for nitrogen
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_CNA_NITRO  ! CNA for nitrogen
!
REAL                                        :: XUNIF_IRRIGTYPE_C
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_IRRIGTYPE
REAL                                        :: XUNIF_WATSUP_C
REAL, DIMENSION(NTIME_MAX)                  :: XUNIF_WATSUP_CTIME
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_WATSUP
REAL                                        :: XUNIF_IRRIGFRAC_C
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_IRRIGFRAC
REAL                                        :: XUNIF_IRRIGFREQ_C
REAL, DIMENSION(NIRR_TYPE)                  :: XUNIF_IRRIGFREQ_CTYPE
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_IRRIGFREQ
REAL                                        :: XUNIF_IRRIGTIME_C
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_IRRIGTIME
REAL                                        :: XUNIF_F2THRESHOLD_C
REAL, DIMENSION(NTIME_MAX)                  :: XUNIF_F2THRESHOLD_CTIME
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_F2THRESHOLD
REAL                                        :: XUNIF_SEED_M_C
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_SEED_M
REAL                                        :: XUNIF_SEED_D_C
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_SEED_D
REAL                                        :: XUNIF_SEED_D_DELTA
REAL                                        :: XUNIF_REAP_M_C
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_REAP_M
REAL                                        :: XUNIF_REAP_D_C
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_REAP_D
REAL                                        :: XUNIF_REAP_D_DELTA
REAL                                        :: XUNIF_SEED_S2_M_C
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_SEED_S2_M
REAL                                        :: XUNIF_SEED_S2_D_C
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_SEED_S2_D
REAL                                        :: XUNIF_SEED_S2_D_DELTA
REAL                                        :: XUNIF_REAP_S2_M_C
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_REAP_S2_M
REAL                                        :: XUNIF_REAP_S2_D_C
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_REAP_S2_D
REAL                                        :: XUNIF_REAP_S2_D_DELTA
REAL                                        :: XUNIF_SEED_S3_M_C
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_SEED_S3_M
REAL                                        :: XUNIF_SEED_S3_D_C
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_SEED_S3_D
REAL                                        :: XUNIF_SEED_S3_D_DELTA
REAL                                        :: XUNIF_REAP_S3_M_C
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_REAP_S3_M
REAL                                        :: XUNIF_REAP_S3_D_C
REAL, DIMENSION(NVEGTYPE_MAX)               :: XUNIF_REAP_S3_D
REAL                                        :: XUNIF_REAP_S3_D_DELTA
INTEGER, DIMENSION(NVEG_IRR)                :: NUNIF_VEG_IRR_USE
!
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_Z0LITTER
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_GNDLITTER
REAL, DIMENSION(NVEGTYPE_MAX,NTIME_MAX)     :: XUNIF_H_VEG
!
LOGICAL, DIMENSION(NVEGTYPE_MAX)            :: LUNIF_STRESS     ! stress type
!
REAL, DIMENSION(NGROUND_MAX)   :: XUNIF_CONDSAT
REAL, DIMENSION(NGROUND_MAX)   :: XUNIF_MPOTSAT
REAL, DIMENSION(NGROUND_MAX)   :: XUNIF_BCOEF
REAL, DIMENSION(NGROUND_MAX)   :: XUNIF_WWILT
REAL, DIMENSION(NGROUND_MAX)   :: XUNIF_WFC
REAL, DIMENSION(NGROUND_MAX)   :: XUNIF_WSAT
!
! name of files containing data
!
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)           :: CFNAM_VEGTYPE    ! fractions of each vegtypes
!
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_VEG        ! vegetation fraction
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_LAI        ! LAI
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_Z0         ! roughness length
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_EMIS       ! emissivity
!
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NGROUND_MAX) :: CFNAM_DG         ! soil depth
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NGROUND_MAX) :: CFNAM_ROOTFRAC   ! root fraction profiles
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_GROUND_DEPTH! ground depth
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_ROOT_DEPTH ! root depth
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_ROOT_EXTINCTION! root extinction parameter
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_ROOT_LIN   ! root linear parameter
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_DICE       ! soil ice depth for runoff (m)
!
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_RSMIN      ! minimal stomatal resistance
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_GAMMA      ! gamma parameter
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_WRMAX_CF   ! coefficient for interception
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_RGL        ! Rgl
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_CV         ! Cv
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_Z0_O_Z0H   ! ratio of roughness lengths
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_ALBNIR_VEG ! albedo of vegetation (near-infra-red)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_ALBVIS_VEG ! albedo of vegetation (visible)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_ALBUV_VEG  ! albedo of vegetation (UV)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_ALBNIR_SOIL! albedo of soil (near-infra-red)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_ALBVIS_SOIL! albedo of soil (visible)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_ALBUV_SOIL ! albedo of soil (UV)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_GMES       ! Gmes
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_BSLAI      ! Biomass over LAI
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_LAIMIN     ! minimum LAI
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_SEFOLD     ! e-folding time for senesence
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_GC         ! cuticular conductance
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_DMAX       ! Dmax
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_F2I        ! F2I
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_H_TREE     ! height of trees
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_RE25       ! soil respiration parameter
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_CE_NITRO   ! CE for nitrogen
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_CF_NITRO   ! CF for nitrogen
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_CNA_NITRO  ! CNA for nitrogen
!
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_IRRIGTYPE  ! irrigation type
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_IRRIGFRAC  ! fraction of irrigation
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_WATSUP     ! irrigation amount (mm)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_IRRIGFREQ  ! minimum time between two irrigation (s)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_IRRIGTIME  ! irrigation amount application time (s)
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_F2THRESHOLD! F2 threshold for irrigation triggering
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_SEED_M  ! 
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_SEED_D  ! 
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_REAP_M  ! 
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_REAP_D  !  
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_SEED_S2_M  ! 
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_SEED_S2_D  ! 
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_REAP_S2_M  ! 
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_REAP_S2_D  !
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_SEED_S3_M  !
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_SEED_S3_D  !
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_REAP_S3_M  !
 CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX)             :: CFNAM_REAP_S3_D  !
!
CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_Z0LITTER
CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_GNDLITTER
CHARACTER(LEN=28), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFNAM_H_VEG
!
CHARACTER(LEN=28), DIMENSION(NGROUND_MAX)   :: CFNAM_CONDSAT
CHARACTER(LEN=28), DIMENSION(NGROUND_MAX)   :: CFNAM_MPOTSAT
CHARACTER(LEN=28), DIMENSION(NGROUND_MAX)   :: CFNAM_BCOEF
CHARACTER(LEN=28), DIMENSION(NGROUND_MAX)   :: CFNAM_WWILT
CHARACTER(LEN=28), DIMENSION(NGROUND_MAX)   :: CFNAM_WFC
CHARACTER(LEN=28), DIMENSION(NGROUND_MAX)   :: CFNAM_WSAT
!
! types of file containing data
!
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)           :: CFTYP_VEGTYPE    ! fractions of each vegtypes
!
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_VEG        ! vegetation fraction
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_LAI        ! LAI
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_Z0         ! roughness length
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_EMIS       ! emissivity
!
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NGROUND_MAX) :: CFTYP_DG         ! soil depth
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NGROUND_MAX) :: CFTYP_ROOTFRAC   ! root fraction profiles
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_GROUND_DEPTH! ground depth
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_ROOT_DEPTH ! root depth
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_ROOT_EXTINCTION! root extinction parameter
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_ROOT_LIN   ! root linear parameter
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_DICE       ! soil ice depth for runoff
!
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_RSMIN      ! minimal stomatal resistance
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_GAMMA      ! gamma parameter
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_WRMAX_CF   ! coefficient for interception
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_RGL        ! Rgl
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_CV         ! Cv
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_Z0_O_Z0H   ! ratio of roughness lengths
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_ALBNIR_VEG ! albedo of vegetation (near-infra-red)
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_ALBVIS_VEG ! albedo of vegetation (visible)
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_ALBUV_VEG  ! albedo of vegetation (UV)
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_ALBNIR_SOIL! albedo of soil (near-infra-red)
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_ALBVIS_SOIL! albedo of soil (visible)
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_ALBUV_SOIL ! albedo of soil (UV)
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_GMES       ! Gmes
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_BSLAI      ! Biomass over LAI
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_LAIMIN     ! minimum LAI
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_SEFOLD     ! e-folding time for senesence
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_GC         ! cuticular conductance
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_DMAX       ! Dmax
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_F2I        ! F2I
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_H_TREE     ! height of trees
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_RE25       ! soil respiration parameter
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_CE_NITRO   ! CE for nitrogen
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_CF_NITRO   ! CF for nitrogen
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_CNA_NITRO  ! CNA for nitrogen
!
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_IRRIGTYPE 
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_IRRIGFRAC
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_WATSUP
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_IRRIGFREQ  
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_IRRIGTIME  
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_F2THRESHOLD  
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_SEED_M
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_SEED_D
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_REAP_M
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_REAP_D
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_SEED_S2_M
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_SEED_S2_D
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_REAP_S2_M
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_REAP_S2_D
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_SEED_S3_M
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_SEED_S3_D
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_REAP_S3_M
 CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX)             :: CFTYP_REAP_S3_D
!
CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_Z0LITTER
CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_GNDLITTER
CHARACTER(LEN=6), DIMENSION(NVEGTYPE_MAX,NTIME_MAX)   :: CFTYP_H_VEG
!
CHARACTER(LEN=6), DIMENSION(NGROUND_MAX)   :: CFTYP_CONDSAT
CHARACTER(LEN=6), DIMENSION(NGROUND_MAX)   :: CFTYP_MPOTSAT
CHARACTER(LEN=6), DIMENSION(NGROUND_MAX)   :: CFTYP_BCOEF
CHARACTER(LEN=6), DIMENSION(NGROUND_MAX)   :: CFTYP_WWILT
CHARACTER(LEN=6), DIMENSION(NGROUND_MAX)   :: CFTYP_WFC
CHARACTER(LEN=6), DIMENSION(NGROUND_MAX)   :: CFTYP_WSAT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_DATA_ISBA/NTIME, XUNIF_VEGTYPE, XUNIF_DG, XUNIF_ROOTFRAC, XUNIF_DICE,                  &
                         XUNIF_GROUND_DEPTH, XUNIF_ROOT_DEPTH, XUNIF_ROOT_EXTINCTION,               &
                         XUNIF_ROOT_LIN, XUNIF_LAI, XUNIF_VEG, XUNIF_Z0, XUNIF_EMIS,                &
                         XUNIF_RSMIN, XUNIF_GAMMA, XUNIF_WRMAX_CF, XUNIF_RGL,                       &
                         XUNIF_CV, XUNIF_Z0_O_Z0H,                                                  &
                         XUNIF_ALBNIR_VEG, XUNIF_ALBVIS_VEG, XUNIF_ALBUV_VEG,                       &
                         XUNIF_ALBNIR_SOIL, XUNIF_ALBVIS_SOIL, XUNIF_ALBUV_SOIL,                    &
                         XUNIF_GMES, XUNIF_BSLAI, XUNIF_LAIMIN, XUNIF_SEFOLD,                       &
                         XUNIF_GC, XUNIF_DMAX, XUNIF_F2I, LUNIF_STRESS, XUNIF_H_TREE, XUNIF_RE25,   &
                         XUNIF_CE_NITRO, XUNIF_CF_NITRO, XUNIF_CNA_NITRO,                           &
                         XUNIF_IRRIGTYPE, XUNIF_WATSUP, XUNIF_IRRIGFRAC, XUNIF_IRRIGFREQ,           &
                         XUNIF_IRRIGTIME, XUNIF_F2THRESHOLD, NUNIF_VEG_IRR_USE,                     &
                         XUNIF_IRRIGTYPE_C, XUNIF_WATSUP_C, XUNIF_IRRIGFRAC_C,                      &
                         XUNIF_IRRIGFREQ_C, XUNIF_IRRIGFREQ_CTYPE, XUNIF_IRRIGTIME_C,               &
                         XUNIF_F2THRESHOLD_C, XUNIF_WATSUP_CTIME , XUNIF_F2THRESHOLD_CTIME,         &
                         XUNIF_SEED_M, XUNIF_SEED_D, XUNIF_REAP_M, XUNIF_REAP_D,                    &
                         XUNIF_SEED_M_C, XUNIF_SEED_D_C, XUNIF_REAP_M_C, XUNIF_REAP_D_C,            &
                         XUNIF_SEED_D_DELTA, XUNIF_REAP_D_DELTA,                                    &
                         XUNIF_SEED_S2_M, XUNIF_SEED_S2_D, XUNIF_REAP_S2_M, XUNIF_REAP_S2_D,        &
                         XUNIF_SEED_S2_M_C, XUNIF_SEED_S2_D_C, XUNIF_REAP_S2_M_C, XUNIF_REAP_S2_D_C,&
                         XUNIF_SEED_S2_D_DELTA, XUNIF_REAP_S2_D_DELTA,                              &
                         XUNIF_SEED_S3_M, XUNIF_SEED_S3_D, XUNIF_REAP_S3_M, XUNIF_REAP_S3_D,        &
                         XUNIF_SEED_S3_M_C, XUNIF_SEED_S3_D_C, XUNIF_REAP_S3_M_C, XUNIF_REAP_S3_D_C,&
                         XUNIF_SEED_S3_D_DELTA, XUNIF_REAP_S3_D_DELTA,                              &
                         CFNAM_VEG,CFNAM_LAI,CFNAM_RSMIN,CFNAM_GAMMA,CFNAM_WRMAX_CF,                &
                         CFNAM_RGL,CFNAM_CV,CFNAM_DG,CFNAM_DICE,CFNAM_Z0,CFNAM_Z0_O_Z0H,            &
                         CFNAM_ALBNIR_VEG,CFNAM_ALBVIS_VEG,CFNAM_ALBUV_VEG,                         &
                         CFNAM_ALBNIR_SOIL,CFNAM_ALBVIS_SOIL,CFNAM_ALBUV_SOIL,                      &
                         CFNAM_EMIS,                                                                &
                         CFNAM_VEGTYPE,CFNAM_ROOTFRAC,                                              &
                         CFNAM_GROUND_DEPTH,CFNAM_ROOT_DEPTH,CFNAM_ROOT_EXTINCTION,CFNAM_ROOT_LIN,  &
                         CFNAM_GMES,CFNAM_BSLAI,CFNAM_LAIMIN,CFNAM_SEFOLD,CFNAM_GC,                 &
                         CFNAM_DMAX,CFNAM_F2I, CFNAM_H_TREE,CFNAM_RE25,                             &
                         CFNAM_CE_NITRO,CFNAM_CF_NITRO,CFNAM_CNA_NITRO,                             &
                         CFNAM_IRRIGTYPE, CFNAM_IRRIGFRAC, CFNAM_WATSUP, CFNAM_IRRIGFREQ,           &
                         CFNAM_IRRIGTIME, CFNAM_F2THRESHOLD,                                        &
                         CFNAM_SEED_M, CFNAM_SEED_D, CFNAM_REAP_M, CFNAM_REAP_D,                    &
                         CFNAM_SEED_S2_M, CFNAM_SEED_S2_D, CFNAM_REAP_S2_M, CFNAM_REAP_S2_D,        &
                         CFNAM_SEED_S3_M, CFNAM_SEED_S3_D, CFNAM_REAP_S3_M, CFNAM_REAP_S3_D,        &
                         CFTYP_VEG,CFTYP_LAI,CFTYP_RSMIN,CFTYP_GAMMA,CFTYP_WRMAX_CF,                &
                         CFTYP_RGL,CFTYP_CV,CFTYP_DG,CFTYP_DICE,CFTYP_Z0,CFTYP_Z0_O_Z0H,            &
                         CFTYP_ALBNIR_VEG,CFTYP_ALBVIS_VEG,CFTYP_ALBUV_VEG,                         &
                         CFTYP_ALBNIR_SOIL,CFTYP_ALBVIS_SOIL,CFTYP_ALBUV_SOIL,                      &
                         CFTYP_EMIS,                                                                &
                         CFTYP_VEGTYPE,CFTYP_ROOTFRAC,                                              &
                         CFTYP_GROUND_DEPTH,CFTYP_ROOT_DEPTH,CFTYP_ROOT_EXTINCTION,CFTYP_ROOT_LIN,  &
                         CFTYP_GMES,CFTYP_BSLAI,CFTYP_LAIMIN,CFTYP_SEFOLD,CFTYP_GC,                 &
                         CFTYP_DMAX,CFTYP_F2I, CFTYP_H_TREE,CFTYP_RE25,                             &
                         CFTYP_CE_NITRO,CFTYP_CF_NITRO,CFTYP_CNA_NITRO,                             &
                         CFTYP_IRRIGTYPE, CFTYP_IRRIGFRAC, CFTYP_WATSUP, CFTYP_IRRIGFREQ,           &
                         CFTYP_IRRIGTIME, CFTYP_F2THRESHOLD,                                        &
                         CFTYP_SEED_M, CFTYP_SEED_D, CFTYP_REAP_M, CFTYP_REAP_D,                    &
                         CFTYP_SEED_S2_M, CFTYP_SEED_S2_D, CFTYP_REAP_S2_M, CFTYP_REAP_S2_D,        &
                         CFTYP_SEED_S3_M, CFTYP_SEED_S3_D, CFTYP_REAP_S3_M, CFTYP_REAP_S3_D,        &
                         XUNIF_Z0LITTER, XUNIF_GNDLITTER, XUNIF_H_VEG, CFNAM_Z0LITTER,              &
                         CFNAM_GNDLITTER, CFNAM_H_VEG, CFTYP_Z0LITTER, CFTYP_GNDLITTER, CFTYP_H_VEG, &
                         XUNIF_CONDSAT, CFNAM_CONDSAT, CFTYP_CONDSAT, XUNIF_MPOTSAT, CFNAM_MPOTSAT, &
                         CFTYP_MPOTSAT, XUNIF_BCOEF, CFNAM_BCOEF, CFTYP_BCOEF, XUNIF_WWILT, CFNAM_WWILT, &
                         CFTYP_WWILT, XUNIF_WFC, CFNAM_WFC, CFTYP_WFC, XUNIF_WSAT, CFNAM_WSAT, CFTYP_WSAT

!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_ISBA_PAR',0,ZHOOK_HANDLE)
!
IF (U%LECOSG) THEN
             !4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
  XSTRESS = (/1.,1.,1.,0.,0.,0.,0.,0.,1.,1.,1.,0.,0.,0.,0.,1.,1.,0.,0.,0./)
ELSE
             !1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
  XSTRESS = (/1.,1.,1.,0.,1.,0.,1.,0.,1.,0.,0.,0.,0.,0.,1.,0.,1.,0.,0.,0./)
ENDIF
!
DTV%NVEGTYPE = NVEGTYPE
!
NTIME                 = 36
XUNIF_VEG             = XUNDEF ! vegetation fraction
XUNIF_LAI             = XUNDEF ! LAI
XUNIF_RSMIN           = XUNDEF ! minimal stomatal resistance
XUNIF_GAMMA           = XUNDEF ! gamma parameter
XUNIF_WRMAX_CF        = XUNDEF ! coefficient for interception
XUNIF_RGL             = XUNDEF ! Rgl
XUNIF_CV              = XUNDEF ! Cv
XUNIF_DG              = XUNDEF ! soil depths
XUNIF_DICE            = XUNDEF ! soil ice depths for runoff
XUNIF_Z0              = XUNDEF ! roughness length of vegetation
XUNIF_Z0_O_Z0H        = XUNDEF ! ratio of roughness lengths
XUNIF_ALBNIR_VEG      = XUNDEF ! albedo of vegetation (near-infra-red)
XUNIF_ALBVIS_VEG      = XUNDEF ! albedo of vegetation (visible)
XUNIF_ALBUV_VEG       = XUNDEF ! albedo of vegetation (UV)
XUNIF_ALBNIR_SOIL     = XUNDEF ! albedo of soil (near-infra-red)
XUNIF_ALBVIS_SOIL     = XUNDEF ! albedo of soil (visible)
XUNIF_ALBUV_SOIL      = XUNDEF ! albedo of soil (UV)
XUNIF_EMIS            = XUNDEF ! emissivity of vegetation
XUNIF_VEGTYPE         = XUNDEF ! fractions of each vegtypes
XUNIF_ROOTFRAC        = XUNDEF ! root fraction profiles
XUNIF_GROUND_DEPTH    = XUNDEF ! ground depth
XUNIF_ROOT_DEPTH      = XUNDEF ! root depth
XUNIF_ROOT_EXTINCTION = XUNDEF ! root extinction parameter
XUNIF_ROOT_LIN        = XUNDEF ! root linear parameter
XUNIF_GMES            = XUNDEF ! Gmes
XUNIF_BSLAI           = XUNDEF ! Biomass over LAI
XUNIF_LAIMIN          = XUNDEF ! minimum LAI
XUNIF_SEFOLD          = XUNDEF ! Sefold
XUNIF_GC              = XUNDEF ! Gc
XUNIF_DMAX            = XUNDEF ! Dmax
XUNIF_F2I             = XUNDEF ! F2I
LUNIF_STRESS          = .TRUE. ! stress type
XUNIF_H_TREE          = XUNDEF ! height of trees
XUNIF_RE25            = XUNDEF ! soil respiration parameter
XUNIF_CE_NITRO        = XUNDEF ! CE for nitrogen
XUNIF_CF_NITRO        = XUNDEF ! CF for nitrogen
XUNIF_CNA_NITRO       = XUNDEF ! CNA for nitrogen
!
XUNIF_IRRIGTYPE       = XUNDEF ! irrigation type (0,1,2,3,4)
XUNIF_WATSUP          = XUNDEF ! irrigation amount (mm)
XUNIF_IRRIGFRAC       = XUNDEF ! fraction of irrigation field (for each vegtype) (0-1)
XUNIF_IRRIGFREQ       = XUNDEF ! max frequency bvetween two irrigation (s)
XUNIF_IRRIGTIME       = XUNDEF ! time of a irrigation (s)
XUNIF_F2THRESHOLD     = XUNDEF ! soil moisture threshold for start irrigation
NUNIF_VEG_IRR_USE     = NUNDEF ! Correspondance of NVEG_IRR to NVEGTYPE: which VEGTYPE is duplicated to be irrigated
XUNIF_IRRIGTYPE_C     = XUNDEF ! Constants for all different vegetation cover and all period
XUNIF_WATSUP_C        = XUNDEF
XUNIF_IRRIGFRAC_C     = XUNDEF
XUNIF_IRRIGFREQ_C     = XUNDEF
XUNIF_IRRIGTIME_C     = XUNDEF
XUNIF_F2THRESHOLD_C   = XUNDEF
XUNIF_IRRIGFREQ_CTYPE = XUNDEF ! Constant by irrigtype !
XUNIF_WATSUP_CTIME    = XUNDEF ! Constant by time (for all different vegetation cover)
XUNIF_F2THRESHOLD_CTIME= XUNDEF
XUNIF_SEED_M          = XUNDEF ! Date of germination for irrigation start - month
XUNIF_SEED_D          = XUNDEF ! Date of germination for irrigation start - day
XUNIF_REAP_M          = XUNDEF ! Date of reap for irrigation end - month
XUNIF_REAP_D          = XUNDEF ! Date of reap for irrigation end - day
XUNIF_SEED_S2_M       = XUNDEF ! Date of germination for irrigation saison 2 - month
XUNIF_SEED_S2_D       = XUNDEF ! Date of germination for irrigation saison 2 - day
XUNIF_REAP_S2_M       = XUNDEF ! Date of reap for irrigation saison 2 - month
XUNIF_REAP_S2_D       = XUNDEF ! Date of reap for irrigation saison 2 - day
XUNIF_SEED_S3_M       = XUNDEF ! Date of germination for irrigation saison 3 - month
XUNIF_SEED_S3_D       = XUNDEF ! Date of germination for irrigation saison 3 - day
XUNIF_REAP_S3_M       = XUNDEF ! Date of reap for irrigation saison 3 - month
XUNIF_REAP_S3_D       = XUNDEF ! Date of reap for irrigation saison 3 - day
XUNIF_SEED_M_C        = XUNDEF
XUNIF_SEED_D_C        = XUNDEF
XUNIF_REAP_M_C        = XUNDEF
XUNIF_REAP_D_C        = XUNDEF
XUNIF_SEED_S2_M_C     = XUNDEF
XUNIF_SEED_S2_D_C     = XUNDEF
XUNIF_REAP_S2_M_C     = XUNDEF
XUNIF_REAP_S2_D_C     = XUNDEF
XUNIF_SEED_S3_M_C     = XUNDEF
XUNIF_SEED_S3_D_C     = XUNDEF
XUNIF_REAP_S3_M_C     = XUNDEF
XUNIF_REAP_S3_D_C     = XUNDEF
XUNIF_SEED_D_DELTA    = XUNDEF
XUNIF_REAP_D_DELTA    = XUNDEF
XUNIF_SEED_S2_D_DELTA = XUNDEF
XUNIF_REAP_S2_D_DELTA = XUNDEF
XUNIF_SEED_S3_D_DELTA = XUNDEF
XUNIF_REAP_S3_D_DELTA = XUNDEF
!
XUNIF_Z0LITTER        = XUNDEF
XUNIF_GNDLITTER       = XUNDEF
XUNIF_H_VEG           = XUNDEF
!
XUNIF_CONDSAT         = XUNDEF
XUNIF_MPOTSAT         = XUNDEF
XUNIF_BCOEF           = XUNDEF
XUNIF_WWILT           = XUNDEF
XUNIF_WFC             = XUNDEF
XUNIF_WSAT            = XUNDEF
!
CFNAM_VEGTYPE (:)     = '                            '
!
CFNAM_VEG  (:,:)      = '                            '
CFNAM_LAI  (:,:)      = '                            '
CFNAM_Z0   (:,:)      = '                            '
CFNAM_EMIS (:,:)      = '                            '
!
CFNAM_DG       (:,:)  = '                            '
CFNAM_ROOTFRAC (:,:)  = '                            '
CFNAM_DICE     (:)    = '                            '
!
CFNAM_GROUND_DEPTH    (:) = '                            '
CFNAM_ROOT_DEPTH      (:) = '                            '
CFNAM_ROOT_EXTINCTION (:) = '                            '
CFNAM_ROOT_LIN        (:) = '                            '
!
CFNAM_RSMIN       (:) = '                            '
CFNAM_GAMMA       (:) = '                            '
CFNAM_WRMAX_CF    (:) = '                            '
CFNAM_RGL         (:) = '                            '
CFNAM_CV          (:) = '                            '
CFNAM_Z0_O_Z0H    (:) = '                            '
CFNAM_ALBNIR_VEG  (:,:) = '                            '
CFNAM_ALBVIS_VEG  (:,:) = '                            '
CFNAM_ALBUV_VEG   (:,:) = '                            '
CFNAM_ALBNIR_SOIL (:,:) = '                            '
CFNAM_ALBVIS_SOIL (:,:) = '                            '
CFNAM_ALBUV_SOIL  (:,:) = '                            '
CFNAM_GMES        (:) = '                            '
CFNAM_BSLAI       (:) = '                            '
CFNAM_LAIMIN      (:) = '                            '
CFNAM_SEFOLD      (:) = '                            '
CFNAM_GC          (:) = '                            '
CFNAM_DMAX        (:) = '                            '
CFNAM_F2I         (:) = '                            '
CFNAM_H_TREE      (:) = '                            '
CFNAM_RE25        (:) = '                            '
CFNAM_CE_NITRO    (:) = '                            '
CFNAM_CF_NITRO    (:) = '                            '
CFNAM_CNA_NITRO   (:) = '                            '
!
CFNAM_IRRIGTYPE    (:) = '                            '
CFNAM_IRRIGFRAC    (:) = '                            '
CFNAM_WATSUP     (:,:) = '                            '
CFNAM_IRRIGFREQ    (:) = '                            '
CFNAM_IRRIGTIME    (:) = '                            '
CFNAM_F2THRESHOLD(:,:) = '                            '
CFNAM_SEED_M       (:) = '                            '
CFNAM_SEED_D       (:) = '                            '
CFNAM_REAP_M       (:) = '                            '
CFNAM_REAP_D       (:) = '                            '
CFNAM_SEED_S2_M    (:) = '                            '
CFNAM_SEED_S2_D    (:) = '                            '
CFNAM_REAP_S2_M    (:) = '                            '
CFNAM_REAP_S2_D    (:) = '                            '
CFNAM_SEED_S3_M    (:) = '                            '
CFNAM_SEED_S3_D    (:) = '                            '
CFNAM_REAP_S3_M    (:) = '                            '
CFNAM_REAP_S3_D    (:) = '                            '
!
CFNAM_Z0LITTER    (:,:) = '                            '
CFNAM_GNDLITTER   (:,:) = '                            '
CFNAM_H_VEG       (:,:) = '                            '
!
CFNAM_CONDSAT     (:) = '                            '
CFNAM_MPOTSAT     (:) = '                            '
CFNAM_BCOEF       (:) = '                            '
CFNAM_WWILT       (:) = '                            '
CFNAM_WFC         (:) = '                            '
CFNAM_WSAT        (:) = '                            '
!
CFTYP_VEGTYPE (:)     = '      '
!
CFTYP_VEG  (:,:)      = '      '
CFTYP_LAI  (:,:)      = '      '
CFTYP_Z0   (:,:)      = '      '
CFTYP_EMIS (:,:)      = '      '
!
CFTYP_DG       (:,:)  = '      '
CFTYP_ROOTFRAC (:,:)  = '      '
CFTYP_DICE     (:)    = '      '
!
CFTYP_GROUND_DEPTH    (:) = '      '
CFTYP_ROOT_DEPTH      (:) = '      '
CFTYP_ROOT_EXTINCTION (:) = '      '
CFTYP_ROOT_LIN        (:) = '      '
!
CFTYP_RSMIN       (:) = '      '
CFTYP_GAMMA       (:) = '      '
CFTYP_WRMAX_CF    (:) = '      '
CFTYP_RGL         (:) = '      '
CFTYP_CV          (:) = '      '
CFTYP_Z0_O_Z0H    (:) = '      '
CFTYP_ALBNIR_VEG  (:,:) = '      '
CFTYP_ALBVIS_VEG  (:,:) = '      '
CFTYP_ALBUV_VEG   (:,:) = '      '
CFTYP_ALBNIR_SOIL (:,:) = '      '
CFTYP_ALBVIS_SOIL (:,:) = '      '
CFTYP_ALBUV_SOIL  (:,:) = '      '
CFTYP_GMES        (:) = '      '
CFTYP_BSLAI       (:) = '      '
CFTYP_LAIMIN      (:) = '      '
CFTYP_SEFOLD      (:) = '      '
CFTYP_GC          (:) = '      '
CFTYP_DMAX        (:) = '      '
CFTYP_F2I         (:) = '      '
CFTYP_H_TREE      (:) = '      '
CFTYP_RE25        (:) = '      '
CFTYP_CE_NITRO    (:) = '      '
CFTYP_CF_NITRO    (:) = '      '
CFTYP_CNA_NITRO   (:) = '      '
!
CFTYP_IRRIGTYPE    (:) = '      '
CFTYP_IRRIGFRAC    (:) = '      '
CFTYP_WATSUP     (:,:) = '      '
CFTYP_IRRIGFREQ    (:) = '      '
CFTYP_IRRIGTIME    (:) = '      '
CFTYP_F2THRESHOLD(:,:) = '      '
CFTYP_SEED_M       (:) = '      '
CFTYP_SEED_D       (:) = '      '
CFTYP_REAP_M       (:) = '      '
CFTYP_REAP_D       (:) = '      '
CFTYP_SEED_S2_M    (:) = '      '
CFTYP_SEED_S2_D    (:) = '      '
CFTYP_REAP_S2_M    (:) = '      '
CFTYP_REAP_S2_D    (:) = '      '
CFTYP_SEED_S3_M    (:) = '      '
CFTYP_SEED_S3_D    (:) = '      '
CFTYP_REAP_S3_M    (:) = '      '
CFTYP_REAP_S3_D    (:) = '      '
!
CFTYP_Z0LITTER    (:,:) = '      '
CFTYP_GNDLITTER   (:,:) = '      '
CFTYP_H_VEG       (:,:) = '      '
!
CFTYP_CONDSAT     (:) = '      '
CFTYP_MPOTSAT     (:) = '      '
CFTYP_BCOEF       (:) = '      '
CFTYP_WWILT       (:) = '      '
CFTYP_WFC         (:) = '      '
CFTYP_WSAT        (:) = '      '
!
ISIZE_LMEB_PATCH=COUNT(IO%LMEB_PATCH(:))
!-------------------------------------------------------------------------------
!
!*    2.      Input file for cover types
!             --------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_DATA_ISBA',GFOUND,ILUOUT)
!
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_DATA_ISBA)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
DTV%NTIME = NTIME
!
!-------------------------------------------------------------------------------
IF (NVEGTYPE_MAX < NVEGTYPE) THEN
  WRITE(ILUOUT,*) '------------------------------------'
  WRITE(ILUOUT,*) 'Please update pgd_isba_par.f90 routine : '
  WRITE(ILUOUT,*) 'The maximum number of VEGTYPE  '
  WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
  WRITE(ILUOUT,*) 'must be increased to : ', NVEGTYPE
  WRITE(ILUOUT,*) '------------------------------------'
  CALL ABOR1_SFX('PGD_ISBA_PAR: MAXIMUM NUMBER OF VEGTYPE MUST BE INCREASED IN NAMELIST DECLARATION')
END IF
!-------------------------------------------------------------------------------
IF (NGROUND_MAX < IO%NGROUND_LAYER) THEN
  WRITE(ILUOUT,*) '------------------------------------'
  WRITE(ILUOUT,*) 'Please update pgd_isba_par.f90 routine : '
  WRITE(ILUOUT,*) 'The maximum number of soil layers  '
  WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
  WRITE(ILUOUT,*) 'must be increased to : ', IO%NGROUND_LAYER
  WRITE(ILUOUT,*) '------------------------------------'
  CALL ABOR1_SFX('PGD_ISBA_PAR: MAXIMUM NUMBER OF SOIL LAYERS MUST BE INCREASED IN NAMELIST DECLARATION')
END IF
!-------------------------------------------------------------------------------
!
IF (NTIME/=36 .AND. NTIME/=12 .AND. NTIME/=2 .AND. NTIME/=1) &
   CALL ABOR1_SFX('PGD_ISBA_PAR: WRONG VALUE FOR NTIME (POSSIBLE VALUES ARE 1, 12 OR 36')
!
!-------------------------------------------------------------------------------
!
!*    3.      Uniform fields are prescribed
!             -----------------------------
!
!-----------------------If irrigation or agricultural practice activated,------------------------
!-----------------------Indicate which vegetype are concerned------------------------------------
!
IF (LAGRIP .OR. LIRRIGMODE) THEN
  IF ( NVEG_IRR==0 .AND. U%LECOSG ) THEN
    !
    NVEG_IRR=NVEG_IRR_DEFAULT
    ALLOCATE(DTV%NPAR_VEG_IRR_USE(NVEG_IRR_DEFAULT))
    DTV%NPAR_VEG_IRR_USE=NVEG_IRR_USE_DEFAULT
    !
  ELSEIF ( NVEG_IRR==0 ) THEN
    IF ( NVT_IRR == 0 ) THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '*****************************************************'
      WRITE(ILUOUT,*) '* Without ECOCLIMAP-SG and if there is not NVT_IRR  *'
      WRITE(ILUOUT,*) '* vegetation type in you vegetation description, you*'
      WRITE(ILUOUT,*) '* have to define NVEG_IRR AND NUNIF_VEG_IRR_USE for *'
      WRITE(ILUOUT,*) '* irrigation and  agricultural practices !!         *'
      WRITE(ILUOUT,*) '*****************************************************'
      CALL ABOR1_SFX('PGD_ISBA_PAR: WITHOUT ECOCLIMAP-SG AND NVT_IRR YOU HAVE TO DEFINE NVEG_IRR AND NUNIF_VEG_IRR_USE')
    ELSE
      NVEG_IRR = 1
      ALLOCATE(DTV%NPAR_VEG_IRR_USE(NVEG_IRR))
      DTV%NPAR_VEG_IRR_USE(1)=NVT_IRR
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '********************************************************************'
      WRITE(ILUOUT,*) '* Without ECOCLIMAP-SG, by default only the NVT_IRR (here = ', NVT_IRR, ')*'
      WRITE(ILUOUT,*) '* vegetation type is irrigated and use with agricultural practices.*'
      WRITE(ILUOUT,*) '********************************************************************'
    ENDIF
  ELSE
    ALLOCATE(DTV%NPAR_VEG_IRR_USE(NVEG_IRR))
    DTV%NPAR_VEG_IRR_USE=NUNIF_VEG_IRR_USE
    !
  ENDIF
  !
  IF ( NVEG_IRR==0 .OR. ( NVEG_IRR /= SIZE(DTV%NPAR_VEG_IRR_USE) ) .OR.                       &
      (NVEG_IRR>0 .AND. (ANY(DTV%NPAR_VEG_IRR_USE==NUNDEF) .OR. ANY(DTV%NPAR_VEG_IRR_USE<=0)  &
                         .OR. ANY(DTV%NPAR_VEG_IRR_USE>NVEGTYPE) ) ) ) THEN
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) '* Error in PGD field preparation of irrigation parameters *'
    WRITE(ILUOUT,*) '* There are irrigation or/and the agricultural practices  *'
    WRITE(ILUOUT,*) '* activated but inconsitency between default and          *'
    WRITE(ILUOUT,*) '* prescribed data for NVEG_IRR or NVEG_IRR_USE.           *'
    IF ( NVEG_IRR==0 ) THEN
      WRITE(ILUOUT,*) '* ==> NVEG_IRR have to be > 0                             *'
    ELSEIF ( NVEG_IRR /= SIZE(DTV%NPAR_VEG_IRR_USE)) THEN
      WRITE(ILUOUT,*) '* ==> NVEG_IRR and size(NPAR_VEG_IRR_USE) have to be equal*'
    ELSE
      WRITE(ILUOUT,*) '* ==> When NVEG_IRR>0, all NVEG_IRR_USE have to be al def.*'
    ENDIF
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_ISBA_PAR: MISSING (OR WRONG) PRESCRIBED VALUE OR INPUT FILE FOR NVEG_IRR/NVEG_IRR_USE')
  !
  ELSEIF (NVEG_IRR > 0) THEN
    DO JL = 1, NVEG_IRR
      DO JVEG = 1, NVEG_IRR
        IF ( JL/=JVEG .AND. DTV%NPAR_VEG_IRR_USE(JL) == DTV%NPAR_VEG_IRR_USE(JVEG) ) THEN
          WRITE(ILUOUT,*) ' '
          WRITE(ILUOUT,*) '***********************************************************'
          WRITE(ILUOUT,*) '* Error in PGD field preparation parameters !             *'
          WRITE(ILUOUT,*) '* There are the irrigation or/and the agricultural        *'
          WRITE(ILUOUT,*) '* practices activated but some values in NUNIF_VEG_IRR_USE*'
          WRITE(ILUOUT,*) '* (given in OPTION.nam) are identical.                    *'
          WRITE(ILUOUT,*) '***********************************************************'
          WRITE(ILUOUT,*) ' '
          CALL ABOR1_SFX('PGD_ISBA_PAR: INDENTICAL PRESCRIBED VALUE IN NUNIF_VEG_IRR_USE (NOT ALLOWED)')
        ENDIF
      ENDDO
    ENDDO
  ENDIF
  !
ELSE
  !
  IF (NVEG_IRR/=0) THEN
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) '* Without LAGRIP .OR. LIRRIGMODE, the parameter NVEG_IRR  *'
    WRITE(ILUOUT,*) '* is not used and so have to do not initialised.          *'
    WRITE(ILUOUT,*) '* The value given is forced (NVEG_IRR=0)                  *'
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) ' '
    NVEG_IRR=0
  ENDIF
  ALLOCATE(DTV%NPAR_VEG_IRR_USE(1))
  DTV%NPAR_VEG_IRR_USE=(/0./)
  !
ENDIF
!
IF ( LAGRIP .AND. .NOT.LIRRIGMODE .AND. NVEG_IRR/=0 ) THEN
  IF ( ANY(DTV%NPAR_VEG_IRR_USE(:)==NVT_TEBD) .OR. ANY(DTV%NPAR_VEG_IRR_USE(:)==NVT_TENE) .OR. &
       ANY(DTV%NPAR_VEG_IRR_USE(:)==NVT_TEBE) .OR. ANY(DTV%NPAR_VEG_IRR_USE(:)==NVT_BOBD) .OR. &
       ANY(DTV%NPAR_VEG_IRR_USE(:)==NVT_BONE) .OR. ANY(DTV%NPAR_VEG_IRR_USE(:)==NVT_BOND) .OR. &
       ANY(DTV%NPAR_VEG_IRR_USE(:)==NVT_TRBD) .OR. ANY(DTV%NPAR_VEG_IRR_USE(:)==NVT_TRBE) .OR. &
       ANY(DTV%NPAR_VEG_IRR_USE(:)==NVT_SHRB) .OR. ANY(DTV%NPAR_VEG_IRR_USE(:)==NVT_FLTR) .OR. &
       ANY(DTV%NPAR_VEG_IRR_USE(:)==NVT_SNOW) .OR. ANY(DTV%NPAR_VEG_IRR_USE(:)==NVT_ROCK) .OR. &
       ANY(DTV%NPAR_VEG_IRR_USE(:)==NVT_NO) ) THEN
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '*******************************************************'
    WRITE(ILUOUT,*) '* Error in PGD field preparation parameters !         *'
    WRITE(ILUOUT,*) '* There are an arborescent vegetation type associated *'
    WRITE(ILUOUT,*) '* with agricultural practices (without irrigation).   *'
    WRITE(ILUOUT,*) '* LAGRIP is apply only for herbaceous !!              *'
    WRITE(ILUOUT,*) '*******************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_ISBA_PAR: LAGRIP (WITHOUT LIRRIGMODE) DEFINE (WITH NPAR_VEG_IRR_USE) FOR A NON HERBACEOUS VEGETATION TYPE.')
  ENDIF
ENDIF
!
!
!-------------------------check NPATCH and give the NAME CORRESPONDANCE---------------------------
!--------------------------(num of irrigated vegtype and correspndance)--------------------------- 
!
NPATCH_LIST(:) = 0
ISIZE_PATCH_ORI = 0
ALLOCATE(DTV%CPATCH_NAME(IO%NPATCH,2))
DTV%CPATCH_NAME(:,:) = '-'
IF ((LAGRIP .OR. LIRRIGMODE) .AND. U%LECOSG) THEN
  !
  IF (IO%NPATCH>NVEGTYPE+NVEG_IRR) THEN
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '*****************************************'
    WRITE(ILUOUT,*) '* Number of patch must be superior to', NVEGTYPE+NVEG_IRR
    WRITE(ILUOUT,*) '* You have chosen NPATCH = ', IO%NPATCH
    WRITE(ILUOUT,*) '*****************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_ISBA_PAR: NPATCH MUST LESS OR EQUAL TO NVEGTYPE + NVEG_IRR')
  ENDIF
  !
  DO JVEG = 1, NVEGTYPE+NVEG_IRR
    CALL  VEGTYPE_TO_PATCH_IRRIG(JVEG,IO%NPATCH,DTV%NPAR_VEG_IRR_USE,JL,PATCH_NAME_TMP)
    IF ( JL > IO%NPATCH ) CALL ABOR1_SFX('PGD_ISBA_PAR: NPATCH_TREE NOT COHERENT WITH NPATCH (AND NVEG_IRR + NPAR_VEG_IRR_USE)')
    NPATCH_LIST(JL)=JL
    IF ( DTV%CPATCH_NAME(JL,1)=='-' ) THEN
      DTV%CPATCH_NAME(JL,1) = PATCH_NAME_TMP(1)
      ISIZE_PATCH_ORI = ISIZE_PATCH_ORI + 1
      IF ( JVEG > NVEGTYPE ) DTV%CPATCH_NAME(JL,1) = 'IRRIGATED '//DTV%CPATCH_NAME(JL,1)
      DTV%CPATCH_NAME(JL,2) = PATCH_NAME_TMP(2)
    ELSE
      DTV%CPATCH_NAME(JL,2) = TRIM(DTV%CPATCH_NAME(JL,2))//'+'//PATCH_NAME_TMP(2)
    ENDIF
  ENDDO
!  
!ELSEIF ( .NOT.U%LECOSG .AND. NVEG_IRR/=0 ) THEN
!  WRITE(ILUOUT,*) ' '
!  WRITE(ILUOUT,*) '***********************************************************'
!  WRITE(ILUOUT,*) '* !! TAKE CARE !!                                         *'
!  WRITE(ILUOUT,*) '* Without ECOCLIMAP-SG, the possibility to duplicate some *'
!  WRITE(ILUOUT,*) '* vegtype into other pathes has never been tested yet !!! *'
!  WRITE(ILUOUT,*) '* Changes in the code are needeed (or use ECOCLIMAP-SG)   *'
 ! WRITE(ILUOUT,*) '***********************************************************'
!  WRITE(ILUOUT,*) ' '
!  CALL ABOR1_SFX('PGD_ISBA_PAR: NVEG_IRR/=0 WITHOUT ECOCLIMAP-SG, TO USE THAT CHANGES ARE NEEDED')
ELSE
  !
  IF (IO%NPATCH>NVEGTYPE) THEN
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '***************************************************'
    WRITE(ILUOUT,*) '* Number of patch must be less or equal to ', NVEGTYPE, '*'
    WRITE(ILUOUT,*) '* You have chosen NPATCH = ', IO%NPATCH, '       *'
    WRITE(ILUOUT,*) '**************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_ISBA_PAR: NPATCH MUST LESS OR EQUAL TO NVEGTYPE')
  ENDIF
  !
  DO JVEG = 1, NVEGTYPE
    CALL  VEGTYPE_TO_PATCH(JVEG,IO%NPATCH,JL,PATCH_NAME_TMP)
    NPATCH_LIST(JL)=JL
    NVEG_LIST(JVEG)=JL
    IF ( DTV%CPATCH_NAME(JL,1)=='-' ) THEN
      ISIZE_PATCH_ORI = ISIZE_PATCH_ORI + 1
      DTV%CPATCH_NAME(JL,1) = PATCH_NAME_TMP(1)
      DTV%CPATCH_NAME(JL,2) = PATCH_NAME_TMP(2)
    ELSE
      DTV%CPATCH_NAME(JL,2) = TRIM(DTV%CPATCH_NAME(JL,2))//'+'//PATCH_NAME_TMP(2)
    ENDIF
  ENDDO
ENDIF
!
IF ( IO%CPHOTO/='NON' .AND. ISIZE_PATCH_ORI < 12 ) THEN
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '*****************************************'
  WRITE(ILUOUT,*) '* With option CPHOTO = ', IO%CPHOTO
  WRITE(ILUOUT,*) '* Number of patch must be at least 12'
  WRITE(ILUOUT,*) '* But you have chosen NPATCH (OR NPATCH_TREE)= ', ISIZE_PATCH_ORI
  WRITE(ILUOUT,*) '*****************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('PGD_ISBA_PAR: CPHOTO='//IO%CPHOTO//' REQUIRES NPATCH (OR NPATCH_TREE)>12')
END IF
!
IF ( ANY( NPATCH_LIST(:) == 0 ) ) CALL ABOR1_SFX('PGD_ISBA_PAR: NPATCH NOT COHERENT WITH NPATCH_TREE (+ NVEG_IRR+NPAR_VEG_IRR_USE)')
!
IF ( .NOT.U%LECOSG .AND. NVEG_IRR/=0 ) THEN
  DO JL = 1, SIZE(DTV%NPAR_VEG_IRR_USE)
    JN=COUNT(NVEG_LIST==NVEG_LIST(DTV%NPAR_VEG_IRR_USE(JL)))
    DO JK = 1, SIZE(DTV%NPAR_VEG_IRR_USE) 
      IF ( JL/=JK .AND. NVEG_LIST(DTV%NPAR_VEG_IRR_USE(JL))==NVEG_LIST(DTV%NPAR_VEG_IRR_USE(JK)) ) JN=JN-1 
    ENDDO
    IF ( JN > 1 ) THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '*****************************************************************'
      WRITE(ILUOUT,*) '* TAKE CARE:                                                    *'
      WRITE(ILUOUT,*) '* Without ECOCLIMAP-SG, you chose to merge (via the patch tree) *'
      WRITE(ILUOUT,*) '* irrigated vegetation type and/or associated agricultural      *'
      WRITE(ILUOUT,*) '* practice with other vegetation type. That could imply strange *'
      WRITE(ILUOUT,*) '* irrigated value / practices... Be carefull with that !!!!     *'
      WRITE(ILUOUT,*) '*****************************************************************'
      WRITE(ILUOUT,*) ' '
    ENDIF
  ENDDO
ENDIF
!
!
!-------------------------------------vegtypes-----------------------------------------
!
ALLOCATE(DTV%XPAR_VEGTYPE(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS,  &
                       HPROGRAM,'ARI','VEGTYPE: vegetation type','NAT',CFNAM_VEGTYPE,   &
                       CFTYP_VEGTYPE,XUNIF_VEGTYPE,DTV%XPAR_VEGTYPE,DTV%LDATA_VEGTYPE)
IF (.NOT. DTV%LDATA_VEGTYPE ) THEN
  !
  ! #rustine START
  ! IF LECOSG AND NOT LTOWN_TO_ROCK, it's needed to attribute town fraction n°X (TOWN_FRAC_X) to vegetype fraction n°Y (VEG_FRAC_Y).
  ! But here, we cheek if vegetation VEG_FRAC_Y > 0 before the attribution, because else LAI, albedo, ... don't exist.
  !
  IF (U%LECOSG .AND. .NOT. U%LTOWN_TO_ROCK ) THEN
    !
    ! If everything goes well, we have DTCO%XDATA_VEGTYPE with correspondance for surface fraction of vegetation to vegtype
    ! fraction with XDATA_VEGTYPE(SUM(NTYPE(1:2))+JTYPE,JTYPE) from init_types_param.F90
    ! 
    ! Define the corresponding vegetype list order for town fraction (initily in init_types_param.F90: NVT_GRAS, NVT_NO AND NVT_TEBD
    LIST_NVT_GRAS = (/NVT_GRAS, NVT_C4  , NVT_C3S , NVT_C3W , NVT_BOGR, NVT_TROG, NVT_FLGR, &
                      NVT_SHRB, NVT_FLTR, NVT_ROCK, NVT_NO  , NVT_SNOW, NVT_TEBD, NVT_TENE, &
                      NVT_TEBE, NVT_BOBD, NVT_BONE, NVT_BOND, NVT_TRBD, NVT_TRBE/)
    LIST_NVT_NO   = (/NVT_NO  , NVT_ROCK, NVT_SNOW, NVT_GRAS, NVT_C4  , NVT_C3S , NVT_C3W , &
                      NVT_BOGR, NVT_TROG, NVT_FLGR, NVT_SHRB, NVT_FLTR, NVT_TEBD, NVT_TENE, &
                      NVT_TEBE, NVT_BOBD, NVT_BONE, NVT_BOND, NVT_TRBD, NVT_TRBE/)
    LIST_NVT_TEBD = (/NVT_TEBD, NVT_TENE, NVT_TEBE, NVT_BOBD, NVT_BONE, NVT_BOND, NVT_TRBD, &
                      NVT_TRBE, NVT_SHRB, NVT_FLTR, NVT_FLGR, NVT_GRAS, NVT_C4  , NVT_C3S , &
                      NVT_C3W , NVT_BOGR, NVT_TROG, NVT_NO  , NVT_ROCK, NVT_SNOW/)
    !
    ! Choose the present vegtype for the three list
    CHOICE_GRAS = 0
    CHOICE_NO   = 0
    CHOICE_TEBD = 0
    DO JL = 1, size(LIST_NVT_GRAS,1)
      IF ( CHOICE_GRAS == 0 .AND. S%LCOVER(SUM(NTYPE(1:2)) + LIST_NVT_GRAS(JL)) ) CHOICE_GRAS = LIST_NVT_GRAS(JL)
      IF ( CHOICE_NO   == 0 .AND. S%LCOVER(SUM(NTYPE(1:2)) + LIST_NVT_NO  (JL)) ) CHOICE_NO   = LIST_NVT_NO  (JL)
      IF ( CHOICE_TEBD == 0 .AND. S%LCOVER(SUM(NTYPE(1:2)) + LIST_NVT_TEBD(JL)) ) CHOICE_TEBD = LIST_NVT_TEBD(JL) 
    ENDDO
    !
    ! IF any CHOICE_XXXX = 0, that mean there is no vegetation (nature) surface. In this case, for all town give NVT_NO with some
    ! modif... (from arrange_cover.F90 with TOWN_TO_ROCK)
    IF ( CHOICE_GRAS == 0 ) THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '**************************************************************'
      WRITE(ILUOUT,*) '**************************************************************'
      WRITE(ILUOUT,*) '*                          BIG CARE                          *'
      WRITE(ILUOUT,*) '**************************************************************'
      WRITE(ILUOUT,*) '* CARE ! YOU ARE IN THE SITUATION WHERE THERE IS NO NATURE ! *'
      WRITE(ILUOUT,*) '* You need no have (with ECOSG) at least one point with one  *'
      WRITE(ILUOUT,*) '* fraction. Else this #rustine do not take into account the  *'
      WRITE(ILUOUT,*) '* fraction of town "forgeted".                               *'
      WRITE(ILUOUT,*) '* An alternative is to run the model without input map file  *'
      WRITE(ILUOUT,*) '* (as LAI, Albedo, ... but constants ! Good luck ;)          *'
      WRITE(ILUOUT,*) '**************************************************************'
      WRITE(ILUOUT,*) '**************************************************************'
      WRITE(ILUOUT,*) ' '
      IF ( CHOICE_NO/=0 .OR. CHOICE_TEBD/=0 ) CALL ABOR1_SFX('PGD_ISBA_PAR: ERROR WITH CONCEPT OF TOWN FRAC TO VEG FRAC')
    !
    ! Il y a dans la région étudiée de la nature, alors:
    ! On transmet les TOWN_FRAC_X aux VEG_FRAC_Y associés selon les régles initialement définies dans 
    ELSE
      DTCO%XDATA_VEGTYPE(NUT_CPHR,CHOICE_GRAS) = 0.0
      DTCO%XDATA_VEGTYPE(NUT_CPMR,CHOICE_GRAS) = 0.0
      DTCO%XDATA_VEGTYPE(NUT_CPLR,CHOICE_GRAS) = 0.0 
      DTCO%XDATA_VEGTYPE(NUT_OPHR,CHOICE_GRAS) = 0.4  
      DTCO%XDATA_VEGTYPE(NUT_OPMR,CHOICE_GRAS) = 0.4
      DTCO%XDATA_VEGTYPE(NUT_OPLR,CHOICE_GRAS) = 0.4
      DTCO%XDATA_VEGTYPE(NUT_LWLR,CHOICE_GRAS) = 0.5
      DTCO%XDATA_VEGTYPE(NUT_LALR,CHOICE_GRAS) = 0.5
      DTCO%XDATA_VEGTYPE(NUT_SPAR,CHOICE_GRAS) = 0.5
      DTCO%XDATA_VEGTYPE(NUT_INDU,CHOICE_GRAS) = 0.4
      !
      DTCO%XDATA_VEGTYPE(NUT_CPHR,CHOICE_NO) = 0.0
      DTCO%XDATA_VEGTYPE(NUT_CPMR,CHOICE_NO) = 0.0
      DTCO%XDATA_VEGTYPE(NUT_CPLR,CHOICE_NO) = 0.0 
      DTCO%XDATA_VEGTYPE(NUT_OPHR,CHOICE_NO) = 0.2  
      DTCO%XDATA_VEGTYPE(NUT_OPMR,CHOICE_NO) = 0.2
      DTCO%XDATA_VEGTYPE(NUT_OPLR,CHOICE_NO) = 0.2
      DTCO%XDATA_VEGTYPE(NUT_LWLR,CHOICE_NO) = 0.5
      DTCO%XDATA_VEGTYPE(NUT_LALR,CHOICE_NO) = 0.5
      DTCO%XDATA_VEGTYPE(NUT_SPAR,CHOICE_NO) = 0.2
      DTCO%XDATA_VEGTYPE(NUT_INDU,CHOICE_NO) = 0.6
      !
      DTCO%XDATA_VEGTYPE(NUT_CPHR,CHOICE_TEBD) = 1.0 
      DTCO%XDATA_VEGTYPE(NUT_CPMR,CHOICE_TEBD) = 1.0
      DTCO%XDATA_VEGTYPE(NUT_CPLR,CHOICE_TEBD) = 1.0
      DTCO%XDATA_VEGTYPE(NUT_OPHR,CHOICE_TEBD) = 0.4  
      DTCO%XDATA_VEGTYPE(NUT_OPMR,CHOICE_TEBD) = 0.4
      DTCO%XDATA_VEGTYPE(NUT_OPLR,CHOICE_TEBD) = 0.4
      DTCO%XDATA_VEGTYPE(NUT_LWLR,CHOICE_TEBD) = 0.0
      DTCO%XDATA_VEGTYPE(NUT_LALR,CHOICE_TEBD) = 0.0
      DTCO%XDATA_VEGTYPE(NUT_SPAR,CHOICE_TEBD) = 0.3
      DTCO%XDATA_VEGTYPE(NUT_INDU,CHOICE_TEBD) = 0.0
      !
    ENDIF
    !
!    ! WITH ECOSG AND TOWN_TO_ROCK, check if there is NTV ROCK OR GIVE ANOTHER VEGTYPE...
!  ELSEIF ( U%LECOSG .AND. U%LTOWN_TO_ROCK .AND. .NOT.S%LCOVER(SUM(NTYPE(1:2)) + NVT_ROCK) ) THEN
!    !
!    ! Reset XDATA_VEGTYPE for NVT_ROCK
!    DTCO%XDATA_VEGTYPE(:,NVT_ROCK) = 0.
!    DTCO%XDATA_VEGTYPE(SUM(NTYPE(1:2))+NVT_ROCK,NVT_ROCK) = 1.
!    !
!    ! Define the corresponding vegetype list order for town fraction
!    LIST_NVT_NO   = (/NVT_NO  , NVT_ROCK, NVT_SNOW, NVT_GRAS, NVT_C4  , NVT_C3S , NVT_C3W , &
!                      NVT_BOGR, NVT_TROG, NVT_FLGR, NVT_SHRB, NVT_FLTR, NVT_TEBD, NVT_TENE, &
!                      NVT_TEBE, NVT_BOBD, NVT_BONE, NVT_BOND, NVT_TRBD, NVT_TRBE/)
!    CHOICE_NO   = 0
!    DO JL = 1, size(LIST_NVT_GRAS,1)
!      IF ( CHOICE_NO == 0 .AND. S%LCOVER(SUM(NTYPE(1:2)) + LIST_NVT_NO  (JL)) ) CHOICE_NO   = LIST_NVT_NO  (JL)
!    ENDDO
!    !
!    IF ( CHOICE_NO == 0 ) THEN
!      WRITE(ILUOUT,*) '**************************************************************'
!      WRITE(ILUOUT,*) '**************************************************************'
!      WRITE(ILUOUT,*) '*                          BIG CARE                          *'
!      WRITE(ILUOUT,*) '**************************************************************'
!      WRITE(ILUOUT,*) '* CARE ! YOU ARE IN THE SITUATION WHERE THERE IS NO NATURE ! *'
!      WRITE(ILUOUT,*) '* You need no have (with ECOSG) at least one point with one  *'
!      WRITE(ILUOUT,*) '* fraction. Else this #rustine do not take into account the  *'
!      WRITE(ILUOUT,*) '* fraction of town "forgeted".                               *'
!      WRITE(ILUOUT,*) '* An alternative is to run the model without input map file  *'
!      WRITE(ILUOUT,*) '* (as LAI, Albedo, ... but constants ! Good luck ;)          *'
!      WRITE(ILUOUT,*) '**************************************************************'
!      WRITE(ILUOUT,*) '**************************************************************'
!    !
!    ! Il y a dans la région étudiée de la nature, alors:
!    ! On transmet les TOWN_FRAC_X au VEG_FRAC_Y associé
!    ELSE
!      DTCO%XDATA_VEGTYPE(SUM(NTYPE(1:3))+1:SUM(NTYPE(1:3))+10,CHOICE_NO) = 1.
!    ENDIF
  !
  ENDIF
  ! #rustine END
  !
  DO JVEG = 1,NVEGTYPE
    CALL AV_PGD(DTCO, DTV%XPAR_VEGTYPE(:,JVEG),S%XCOVER,DTCO%XDATA_VEGTYPE(:,JVEG),'NAT','ARI',S%LCOVER)
  ENDDO
ENDIF
!
!
IF (.NOT.IO%LECOCLIMAP .AND. .NOT.DTV%LDATA_VEGTYPE) THEN
  !
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in PGD field preparation of field VEGTYPE         *'
  WRITE(ILUOUT,*) '* There is no prescribed value and no input file          *'
  WRITE(ILUOUT,*) '* Without ECOCLIMAP, this field must be prescribed        *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR VEGTYPE')
  !
ELSEIF (DTV%LDATA_VEGTYPE) THEN
  !
  WHERE (DTV%XPAR_VEGTYPE(:,:)==XUNDEF) DTV%XPAR_VEGTYPE(:,:)=0.
  WHERE (DTV%XPAR_VEGTYPE(:,:)/=0.) DTV%XPAR_VEGTYPE(:,:) = DTV%XPAR_VEGTYPE(:,:) / &
                                                    SPREAD(SUM(DTV%XPAR_VEGTYPE(:,:),2),2,NVEGTYPE)
  !  
ENDIF
!
!--------------------------------temporal fields-----------------------------------
!
ALLOCATE(DTV%LDATA_VEG(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_VEG(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                        HPROGRAM,'ARI','VEG: vegetation fraction','NAT',CFNAM_VEG,CFTYP_VEG,XUNIF_VEG, &
                        DTV%XPAR_VEG,DTV%LDATA_VEG)
IF (ALL(.NOT.DTV%LDATA_VEG)) DEALLOCATE(DTV%XPAR_VEG)
!
ALLOCATE(DTV%LDATA_LAI(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_LAI(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                        HPROGRAM,'ARI','LAI: leaf area index','NAT',CFNAM_LAI,CFTYP_LAI,XUNIF_LAI, &
                        DTV%XPAR_LAI,DTV%LDATA_LAI)
IF (ALL(.NOT.DTV%LDATA_LAI)) DEALLOCATE(DTV%XPAR_LAI)
!
ALLOCATE(DTV%LDATA_H_VEG(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_H_VEG(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                        HPROGRAM,'ARI','H_VEG: vegetation height','NAT',CFNAM_H_VEG,CFTYP_H_VEG,XUNIF_H_VEG, &
                        DTV%XPAR_H_VEG,DTV%LDATA_H_VEG) 
IF (ALL(.NOT.DTV%LDATA_H_VEG)) DEALLOCATE(DTV%XPAR_H_VEG)
!
ALLOCATE(DTV%LDATA_Z0(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_Z0(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                        HPROGRAM,'CDN','Z0: roughness length','NAT',CFNAM_Z0,CFTYP_Z0,XUNIF_Z0, &
                        DTV%XPAR_Z0,DTV%LDATA_Z0)
IF (ALL(.NOT.DTV%LDATA_Z0)) DEALLOCATE(DTV%XPAR_Z0)
!
ALLOCATE(DTV%LDATA_EMIS(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_EMIS(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                        HPROGRAM,'ARI','EMIS: emissivity','NAT',CFNAM_EMIS,CFTYP_EMIS,XUNIF_EMIS, &
                        DTV%XPAR_EMIS,DTV%LDATA_EMIS)
IF (ALL(.NOT.DTV%LDATA_EMIS)) DEALLOCATE(DTV%XPAR_EMIS)
!
IF (.NOT.IO%LECOCLIMAP .AND. .NOT.(ANY(DTV%LDATA_VEG) .AND. ANY(DTV%LDATA_LAI) .AND. &
                                   ANY(DTV%LDATA_Z0) .AND. ANY(DTV%LDATA_EMIS))) THEN
  !
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in PGD field preparation of temporal fields       *'
  WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
  IF (ALL(.NOT.DTV%LDATA_VEG )) WRITE(ILUOUT,*) '* for VEG                            *'
  IF (ALL(.NOT.DTV%LDATA_LAI )) WRITE(ILUOUT,*) '* for LAI                            *'
  IF (ALL(.NOT.DTV%LDATA_Z0  )) WRITE(ILUOUT,*) '* for Z0                             *'
  IF (ALL(.NOT.DTV%LDATA_EMIS)) WRITE(ILUOUT,*) '* for EMIS                           *'
  WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR TEMPORAL PARAMETERS')
  !
ENDIF
!
ALLOCATE(DTV%LDATA_ALBNIR_VEG(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_ALBNIR_VEG(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','ALBNIR_VEG: NIR albedo of vegetation','NAT',CFNAM_ALBNIR_VEG,   &
                       CFTYP_ALBNIR_VEG,XUNIF_ALBNIR_VEG,DTV%XPAR_ALBNIR_VEG,DTV%LDATA_ALBNIR_VEG)
IF (ALL(.NOT.DTV%LDATA_ALBNIR_VEG)) DEALLOCATE(DTV%XPAR_ALBNIR_VEG)
!
ALLOCATE(DTV%LDATA_ALBVIS_VEG(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_ALBVIS_VEG(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','ALBVIS_VEG: VIS albedo of vegetation','NAT',CFNAM_ALBVIS_VEG,   &
                       CFTYP_ALBVIS_VEG,XUNIF_ALBVIS_VEG,DTV%XPAR_ALBVIS_VEG,DTV%LDATA_ALBVIS_VEG)  
IF (ALL(.NOT.DTV%LDATA_ALBVIS_VEG)) DEALLOCATE(DTV%XPAR_ALBVIS_VEG)
!
ALLOCATE(DTV%LDATA_ALBUV_VEG(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_ALBUV_VEG(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','ALBUV_VEG: UV albedo of vegetation','NAT',CFNAM_ALBUV_VEG,   &
                       CFTYP_ALBUV_VEG,XUNIF_ALBUV_VEG,DTV%XPAR_ALBUV_VEG,DTV%LDATA_ALBUV_VEG)
IF (ALL(.NOT.DTV%LDATA_ALBUV_VEG)) DEALLOCATE(DTV%XPAR_ALBUV_VEG)
!
ALLOCATE(DTV%LDATA_ALBNIR_SOIL(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_ALBNIR_SOIL(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','ALBNIR_SOIL: NIR albedo of SOIL','NAT',CFNAM_ALBNIR_SOIL,   &
                       CFTYP_ALBNIR_SOIL,XUNIF_ALBNIR_SOIL,DTV%XPAR_ALBNIR_SOIL,DTV%LDATA_ALBNIR_SOIL)  
IF (ALL(.NOT.DTV%LDATA_ALBNIR_SOIL)) DEALLOCATE(DTV%XPAR_ALBNIR_SOIL)
!
ALLOCATE(DTV%LDATA_ALBVIS_SOIL(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_ALBVIS_SOIL(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','ALBVIS_SOIL: VIS albedo of SOIL','NAT',CFNAM_ALBVIS_SOIL,   &
                       CFTYP_ALBVIS_SOIL,XUNIF_ALBVIS_SOIL,DTV%XPAR_ALBVIS_SOIL,DTV%LDATA_ALBVIS_SOIL)  
IF (ALL(.NOT.DTV%LDATA_ALBVIS_SOIL)) DEALLOCATE(DTV%XPAR_ALBVIS_SOIL)
!
ALLOCATE(DTV%LDATA_ALBUV_SOIL(NTIME*NVEGTYPE))
ALLOCATE(DTV%XPAR_ALBUV_SOIL(KDIM,NTIME,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','ALBUV_SOIL: UV albedo of SOIL','NAT',CFNAM_ALBUV_SOIL,   &
                       CFTYP_ALBUV_SOIL,XUNIF_ALBUV_SOIL,DTV%XPAR_ALBUV_SOIL,DTV%LDATA_ALBUV_SOIL)  
IF (ALL(.NOT.DTV%LDATA_ALBUV_SOIL)) DEALLOCATE(DTV%XPAR_ALBUV_SOIL)
!
!
! ------------ Begin MEB parameters ---------------------
!
ALLOCATE(DTV%LDATA_GNDLITTER(NTIME*NVEGTYPE))
ALLOCATE(DTV%LDATA_Z0LITTER (NTIME*NVEGTYPE))
DTV%LDATA_GNDLITTER(:) = .FALSE.
DTV%LDATA_Z0LITTER (:) = .FALSE.
!
IF(ISIZE_LMEB_PATCH>0) THEN
  !
  ALLOCATE(DTV%XPAR_GNDLITTER(KDIM,NTIME,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','GNDLITTER: ground litter fraction','NAT',&
                         CFNAM_GNDLITTER,CFTYP_GNDLITTER,XUNIF_GNDLITTER,DTV%XPAR_GNDLITTER,DTV%LDATA_GNDLITTER)
  IF (ALL(.NOT.DTV%LDATA_GNDLITTER)) DEALLOCATE(DTV%XPAR_GNDLITTER)
  !
  ALLOCATE(DTV%XPAR_Z0LITTER(KDIM,NTIME,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'CDN','Z0LITTER: ground litter roughness length','NAT', &
                         CFNAM_Z0LITTER,CFTYP_Z0LITTER,XUNIF_Z0LITTER,DTV%XPAR_Z0LITTER,DTV%LDATA_Z0LITTER)
  IF (ALL(.NOT.DTV%LDATA_Z0LITTER)) DEALLOCATE(DTV%XPAR_Z0LITTER)
  !
ENDIF
! ------------ End MEB parameters ---------------------
!
!--------------------------------depths fields-----------------------------------
!
ALLOCATE(DTV%XPAR_DG(KDIM,IO%NGROUND_LAYER,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','DG: ground depth','NAT',CFNAM_DG,CFTYP_DG,&
                         XUNIF_DG,DTV%XPAR_DG,DTV%LDATA_DG(1:NVEGTYPE))
IF (ALL(.NOT.DTV%LDATA_DG)) DEALLOCATE(DTV%XPAR_DG)
!  
ALLOCATE(DTV%XPAR_ROOT_DEPTH(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','ROOT_DEPTH: root depth','NAT',CFNAM_ROOT_DEPTH,CFTYP_ROOT_DEPTH,&
                       XUNIF_ROOT_DEPTH,DTV%XPAR_ROOT_DEPTH,DTV%LDATA_ROOT_DEPTH)
IF (ALL(.NOT.DTV%LDATA_ROOT_DEPTH)) DEALLOCATE(DTV%XPAR_ROOT_DEPTH)
ALLOCATE(DTV%XPAR_GROUND_DEPTH(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                        HPROGRAM,'ARI','GROUND_DEPTH: ground depth','NAT',CFNAM_GROUND_DEPTH,CFTYP_GROUND_DEPTH,&
      XUNIF_GROUND_DEPTH,DTV%XPAR_GROUND_DEPTH,DTV%LDATA_GROUND_DEPTH)
IF (ALL(.NOT.DTV%LDATA_GROUND_DEPTH)) DEALLOCATE(DTV%XPAR_GROUND_DEPTH)
!
IF(IO%CISBA=='DIF')THEN 
  ! 
  ALLOCATE(DTV%XPAR_ROOTFRAC(KDIM,IO%NGROUND_LAYER,NVEGTYPE))  
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                        HPROGRAM,'ARI','ROOTFRAC: root fraction','NAT',&
                        CFNAM_ROOTFRAC,CFTYP_ROOTFRAC,XUNIF_ROOTFRAC,&
                        DTV%XPAR_ROOTFRAC,DTV%LDATA_ROOTFRAC(1:NVEGTYPE))
  IF (ALL(.NOT.DTV%LDATA_ROOTFRAC)) DEALLOCATE(DTV%XPAR_ROOTFRAC)
  !
  ALLOCATE(DTV%XPAR_ROOT_EXTINCTION(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                        HPROGRAM,'ARI','ROOT_EXTINCTION: root extinction','NAT',CFNAM_ROOT_EXTINCTION,CFTYP_ROOT_EXTINCTION,&
        XUNIF_ROOT_EXTINCTION,DTV%XPAR_ROOT_EXTINCTION,DTV%LDATA_ROOT_EXTINCTION)
  IF (ALL(.NOT.DTV%LDATA_ROOT_EXTINCTION)) DEALLOCATE(DTV%XPAR_ROOT_EXTINCTION)
  !      
  ALLOCATE(DTV%XPAR_ROOT_LIN(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                        HPROGRAM,'ARI','ROOT_LIN: root linear','NAT',CFNAM_ROOT_LIN,CFTYP_ROOT_LIN,&
        XUNIF_ROOT_LIN,DTV%XPAR_ROOT_LIN,DTV%LDATA_ROOT_LIN)
  IF (ALL(.NOT.DTV%LDATA_ROOT_LIN)) DEALLOCATE(DTV%XPAR_ROOT_LIN)
  !
  IF (.NOT.IO%LECOCLIMAP) THEN
    IF(ALL(.NOT.DTV%LDATA_DG) .AND. ALL(.NOT.DTV%LDATA_ROOTFRAC) .AND. &
       (ALL(.NOT.DTV%LDATA_ROOT_DEPTH).OR.ALL(.NOT.DTV%LDATA_ROOT_EXTINCTION).OR.ALL(.NOT.DTV%LDATA_ROOT_LIN))) THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '****************************************************************************'
      WRITE(ILUOUT,*) '* Without ECOCLIMAP, Error in PGD field preparation for ISBA-DIF           *'
      WRITE(ILUOUT,*) '* There is no prescribed value and no input file                           *'
      WRITE(ILUOUT,*) '*  (1) XUNIF_ROOTFRAC must be given.                                       *'
      WRITE(ILUOUT,*) '*  (2) Other solution, give all these fields:                              *'
      WRITE(ILUOUT,*) '*     - XUNIF_ROOT_DEPTH      (soil root depth)                            *'
      WRITE(ILUOUT,*) '*     - XUNIF_ROOT_EXTINCTION (root extinction parameter [Jackson 1996])   *'
      WRITE(ILUOUT,*) '*     - XUNIF_ROOT_LIN        (0.05 usually; 1=uniform root distribution!!)*'
      WRITE(ILUOUT,*) '****************************************************************************'
      WRITE(ILUOUT,*) ' '
      CALL ABOR1_SFX("PGD_ISBA_PAR: PROBLEM IN SOIL GRID COMPUTATION") 
    ELSEIF( .NOT.ALL(IO%XSOILGRID(:)==XUNDEF) .AND. &
           (ALL(.NOT.DTV%LDATA_GROUND_DEPTH)   .OR.ALL(.NOT.DTV%LDATA_ROOT_DEPTH).OR. &
            ALL(.NOT.DTV%LDATA_ROOT_EXTINCTION).OR.ALL(.NOT.DTV%LDATA_ROOT_LIN)  )) THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '****************************************************************************'
      WRITE(ILUOUT,*) '* Without ECOCLIMAP, Error in PGD field preparation for ISBA-DIF           *'
      WRITE(ILUOUT,*) '* There is no prescribed value and no input file.                          *'
      WRITE(ILUOUT,*) '* When XSOILGRID is given, other field are needed :                        *'
      WRITE(ILUOUT,*) '*     - XUNIF_GROUND_DEPTH    (soil ground depth for moisture)             *'
      WRITE(ILUOUT,*) '*     - XUNIF_ROOT_DEPTH      (soil root depth)                            *'
      WRITE(ILUOUT,*) '*     - XUNIF_ROOT_LIN        (0.05 usually; 1=uniform root distribution!!)*'
      WRITE(ILUOUT,*) '*     - XUNIF_ROOT_EXTINCTION (root extinction parameter [Jackson 1996])   *'
      WRITE(ILUOUT,*) '****************************************************************************'
      WRITE(ILUOUT,*) ' '
      CALL ABOR1_SFX("PGD_ISBA_PAR: PROBLEM IN SOIL GRID COMPUTATION") 
    ENDIF
    IF(ALL(.NOT.DTV%LDATA_DG) .AND.ALL(IO%XSOILGRID(:)==XUNDEF))THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '****************************************************************************'
      WRITE(ILUOUT,*) '* Without ECOCLIMAP, Error in PGD field preparation for ISBA-DIF           *'
      WRITE(ILUOUT,*) '* There is no prescribed value to compute vertical soil grid.              *'
      WRITE(ILUOUT,*) '* 2 solutions:                                                             *'
      WRITE(ILUOUT,*) '* (1) Give XUNIF_DG in NAM_DATA_ISBA.                                      *'
      WRITE(ILUOUT,*) '*  OR                                                                      *'
      WRITE(ILUOUT,*) '* (2) Give XSOILGRID in NAM_ISBA                                           *'
      WRITE(ILUOUT,*) '****************************************************************************'
      WRITE(ILUOUT,*) ' '       
      CALL ABOR1_SFX("PGD_ISBA_PAR: PROBLEM IN SOIL GRID COMPUTATION") 
    ENDIF
  ENDIF
  !
ELSE   
  !
  IF ( .NOT.IO%LECOCLIMAP .AND. ALL(.NOT.DTV%LDATA_DG) .AND. &
      (ALL(.NOT.DTV%LDATA_GROUND_DEPTH).OR.ALL(.NOT.DTV%LDATA_ROOT_DEPTH)) ) THEN
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '****************************************************************************'
    WRITE(ILUOUT,*) '* Without ECOCLIMAP, Error in PGD field preparation                        *'
    WRITE(ILUOUT,*) '* There is no prescribed value and no input file                           *'
    WRITE(ILUOUT,*) '* XUNIF_DG or both XUNIF_GROUND_DEPTH and XUNIF_ROOT_DEPTH must be given.  *'
    WRITE(ILUOUT,*) '****************************************************************************'
    WRITE(ILUOUT,*) ' '       
    CALL ABOR1_SFX("PGD_ISBA_PAR: PROBLEM IN SOIL GRID COMPUTATION") 
  ENDIF
  !
ENDIF
!
ALLOCATE(DTV%XPAR_DICE(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','DICE: ice depth for runoff','NAT',CFNAM_DICE,CFTYP_DICE,&
                       XUNIF_DICE,DTV%XPAR_DICE,DTV%LDATA_DICE)
!
IF (.NOT.IO%LECOCLIMAP.AND.ALL(.NOT.DTV%LDATA_DICE)) THEN
  DTV%LDATA_DICE(:) = .FALSE.
  IF(IO%CISBA/='DIF' .AND. (ANY(DTV%LDATA_DG).OR.ANY(DTV%LDATA_ROOT_DEPTH))) THEN
    DO JVEG = 1,NVEGTYPE
      IF(DTV%LDATA_DG(JVEG))THEN
        DTV%LDATA_DICE(JVEG)=.TRUE.
        WHERE(DTV%XPAR_DG(:,2,JVEG)/=XUNDEF) DTV%XPAR_DICE(:,JVEG) = MAX(0.2,0.8*DTV%XPAR_DG(:,2,JVEG))
      ELSEIF(DTV%LDATA_ROOT_DEPTH(JVEG))THEN
        DTV%LDATA_DICE(JVEG)=.TRUE.
        WHERE(DTV%XPAR_ROOT_DEPTH(:,JVEG)/=XUNDEF) DTV%XPAR_DICE(:,JVEG) = MAX(0.2,0.8*DTV%XPAR_ROOT_DEPTH(:,JVEG))
      ENDIF
    ENDDO
  ELSEIF (IO%CISBA=='DIF') THEN
    DTV%XPAR_DICE(:,:) = 0.0
    DTV%LDATA_DICE(:)  =.TRUE.
  ELSE
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) '* Error in PGD field preparation of field DICE            *'
    WRITE(ILUOUT,*) '* There is no prescribed value and no input file          *'
    WRITE(ILUOUT,*) '* Without ECOCLIMAP, this field must be prescribed        *'
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR DICE')
  ENDIF
ENDIF  
IF (ALL(.NOT.DTV%LDATA_DICE)) DEALLOCATE(DTV%XPAR_DICE)
!
!---------------------classical fields---------------------------------------------
!
ALLOCATE(DTV%XPAR_RSMIN(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'INV','RSMIN: minimal stomatal resistance','NAT',CFNAM_RSMIN,   &
                       CFTYP_RSMIN,XUNIF_RSMIN,DTV%XPAR_RSMIN,DTV%LDATA_RSMIN)
IF (ALL(.NOT.DTV%LDATA_RSMIN)) DEALLOCATE(DTV%XPAR_RSMIN)
!
ALLOCATE(DTV%XPAR_GAMMA(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','GAMMA: gamma coefficient','NAT',CFNAM_GAMMA,   &
                       CFTYP_GAMMA,XUNIF_GAMMA,DTV%XPAR_GAMMA,DTV%LDATA_GAMMA)
IF (ALL(.NOT.DTV%LDATA_GAMMA)) DEALLOCATE(DTV%XPAR_GAMMA)
!
ALLOCATE(DTV%XPAR_WRMAX_CF(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','WRMAX_CF: coeff. for max WR','NAT',CFNAM_WRMAX_CF,   &
                       CFTYP_WRMAX_CF,XUNIF_WRMAX_CF,DTV%XPAR_WRMAX_CF,DTV%LDATA_WRMAX_CF)
IF (ALL(.NOT.DTV%LDATA_WRMAX_CF)) DEALLOCATE(DTV%XPAR_WRMAX_CF)
!
ALLOCATE(DTV%XPAR_RGL(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','RGL: max SW rad. for photosynthesis','NAT',CFNAM_RGL,   &
                       CFTYP_RGL,XUNIF_RGL,DTV%XPAR_RGL,DTV%LDATA_RGL)  
IF (ALL(.NOT.DTV%LDATA_RGL)) DEALLOCATE(DTV%XPAR_RGL)
!
ALLOCATE(DTV%XPAR_CV(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'INV','CV: thermal inertia for vegetation','NAT',CFNAM_CV,   &
                       CFTYP_CV,XUNIF_CV,DTV%XPAR_CV,DTV%LDATA_CV)  
IF (ALL(.NOT.DTV%LDATA_CV)) DEALLOCATE(DTV%XPAR_CV)
!
ALLOCATE(DTV%XPAR_Z0_O_Z0H(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','Z0_O_Z0H: ratio of roughness lengths','NAT',CFNAM_Z0_O_Z0H,   &
                       CFTYP_Z0_O_Z0H,XUNIF_Z0_O_Z0H,DTV%XPAR_Z0_O_Z0H,DTV%LDATA_Z0_O_Z0H)  
IF (ALL(.NOT.DTV%LDATA_Z0_O_Z0H)) DEALLOCATE(DTV%XPAR_Z0_O_Z0H)
!
IF (.NOT.IO%LECOCLIMAP .AND. .NOT.(ANY(DTV%LDATA_RSMIN).AND.ANY(DTV%LDATA_GAMMA).AND.ANY(DTV%LDATA_WRMAX_CF).AND.&
         ANY(DTV%LDATA_RGL).AND.ANY(DTV%LDATA_CV).AND.ANY(DTV%LDATA_Z0_O_Z0H).AND.ANY(DTV%LDATA_ALBNIR_VEG).AND. &
         ANY(DTV%LDATA_ALBVIS_VEG).AND.ANY(DTV%LDATA_ALBUV_VEG).AND.ANY(DTV%LDATA_ALBNIR_SOIL).AND.&
         ANY(DTV%LDATA_ALBVIS_SOIL).AND.ANY(DTV%LDATA_ALBUV_SOIL))) THEN
  !
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in PGD field preparation of classical fields      *'
  WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
  IF (ALL(.NOT.DTV%LDATA_RSMIN       )) WRITE(ILUOUT,*) '* for RSMIN                  *'
  IF (ALL(.NOT.DTV%LDATA_GAMMA       )) WRITE(ILUOUT,*) '* for GAMMA                  *'
  IF (ALL(.NOT.DTV%LDATA_WRMAX_CF    )) WRITE(ILUOUT,*) '* for WRMAX_CF               *'
  IF (ALL(.NOT.DTV%LDATA_RGL         )) WRITE(ILUOUT,*) '* for RGL                    *'
  IF (ALL(.NOT.DTV%LDATA_CV          )) WRITE(ILUOUT,*) '* for CV                     *'
  IF (ALL(.NOT.DTV%LDATA_Z0_O_Z0H    )) WRITE(ILUOUT,*) '* for Z0_O_Z0H               *'
  IF (ALL(.NOT.DTV%LDATA_ALBNIR_VEG  )) WRITE(ILUOUT,*) '* for ALBNIR_VEG             *'
  IF (ALL(.NOT.DTV%LDATA_ALBVIS_VEG  )) WRITE(ILUOUT,*) '* for ALBVIS_VEG             *'
  IF (ALL(.NOT.DTV%LDATA_ALBUV_VEG   )) WRITE(ILUOUT,*) '* for ALBUV_VEG              *'
  IF (ALL(.NOT.DTV%LDATA_ALBNIR_SOIL )) WRITE(ILUOUT,*) '* for ALBNIR_SOIL            *'
  IF (ALL(.NOT.DTV%LDATA_ALBVIS_SOIL )) WRITE(ILUOUT,*) '* for ALBVIS_SOIL            *'
  IF (ALL(.NOT.DTV%LDATA_ALBUV_SOIL  )) WRITE(ILUOUT,*) '* for ALBUV_SOIL             *'
  WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR CLASSICAL PARAMETERS')
  !
ENDIF
!
!--------------------------------------AGS parameters----------------------------
!
IF (IO%CPHOTO/='NON' .OR. &
    (ALL(.NOT.DTV%LDATA_Z0).AND.(ANY(DTV%LDATA_LAI).OR.DTV%LDATA_VEGTYPE)) .OR. ISIZE_LMEB_PATCH>0) THEN
  !
  ALLOCATE(DTV%XPAR_H_TREE(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                        HPROGRAM,'ARI','H_TREE: height of trees','NAT',CFNAM_H_TREE,   &
                        CFTYP_H_TREE,XUNIF_H_TREE,DTV%XPAR_H_TREE,DTV%LDATA_H_TREE)  
  IF (ALL(.NOT.DTV%LDATA_H_TREE)) THEN
    DEALLOCATE(DTV%XPAR_H_TREE)
  ELSE
    IF (U%LECOSG) THEN
      DTV%LDATA_H_TREE(1:3)   = .FALSE.
      DTV%LDATA_H_TREE(13:18) = .FALSE.
      DTV%LDATA_H_TREE(20)    = .FALSE.
    ELSE
      DTV%LDATA_H_TREE (1:3)  = .FALSE.
      DTV%LDATA_H_TREE (7:12) = .FALSE.
      DTV%LDATA_H_TREE(18:19) = .FALSE.
    ENDIF
  ENDIF
  !
ENDIF

IF (IO%CPHOTO/='NON' .OR. ISIZE_LMEB_PATCH>0) THEN
  ALLOCATE(DTV%XPAR_BSLAI(KDIM,NVEGTYPE))
CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                       HPROGRAM,'ARI','BSLAI: biomass over LAI','NAT',CFNAM_BSLAI,   &
                       CFTYP_BSLAI,XUNIF_BSLAI,DTV%XPAR_BSLAI,DTV%LDATA_BSLAI)  
  IF (ALL(.NOT.DTV%LDATA_BSLAI)) DEALLOCATE(DTV%XPAR_BSLAI)
ENDIF
!
IF (.NOT.IO%LECOCLIMAP .AND. ISIZE_LMEB_PATCH>0 ) THEN
  IF (.NOT.(ANY(DTV%LDATA_H_TREE).AND.ANY(DTV%LDATA_GNDLITTER).AND.ANY(DTV%LDATA_Z0LITTER)  &
      .AND. ANY(DTV%LDATA_BSLAI))) THEN
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) '* Error in PGD field preparation of MEB fields            *'
    WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
    IF (ALL(.NOT.DTV%LDATA_GNDLITTER   )) WRITE(ILUOUT,*) '* for GNDLITTER              *'
    IF (ALL(.NOT.DTV%LDATA_Z0LITTER    )) WRITE(ILUOUT,*) '* for Z0LITTER               *'
    IF (ALL(.NOT.DTV%LDATA_H_TREE      )) WRITE(ILUOUT,*) '* for H_TREE                 *'
    IF (ALL(.NOT.DTV%LDATA_BSLAI       )) WRITE(ILUOUT,*) '* for BSLAI                  *'
    WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR MEB PARAMETERS')
  ENDIF
  !
ENDIF
!
ALLOCATE(DTV%LDATA_WATSUP(NTIME*NVEGTYPE))
DTV%LDATA_WATSUP(:) = .FALSE.
ALLOCATE(DTV%LDATA_F2THRESHOLD(NTIME*NVEGTYPE))
DTV%LDATA_F2THRESHOLD(:) = .FALSE.
!
!
IF (U%LECOSG .AND. (LAGRIP .OR. LIRRIGMODE)) THEN
  !
  ! * Irrigfrac (only with ECOCLIMAP-SG, to duplicate patch)
  ! -----------
  !
  ALLOCATE(DTV%XPAR_IRRIGFRAC(KDIM,NVEGTYPE))
  IF ( XUNIF_IRRIGFRAC_C/=XUNDEF )  WHERE ( XUNIF_IRRIGFRAC(:)==XUNDEF ) XUNIF_IRRIGFRAC(:)=XUNIF_IRRIGFRAC_C
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARV','IRRIGFRAC: irrigation fraction','NAT',&
                         CFNAM_IRRIGFRAC,CFTYP_IRRIGFRAC,XUNIF_IRRIGFRAC,DTV%XPAR_IRRIGFRAC,DTV%LDATA_IRRIGFRAC) ! Note: The same map can be use for IRRIGFRAC and IRRIGTYPE
  IF (ALL(.NOT.DTV%LDATA_IRRIGFRAC)) DEALLOCATE(DTV%XPAR_IRRIGFRAC)
  !
!ELSEIF (LAGRIP .OR. LIRRIGMODE) THEN
!  !
!  ALLOCATE(DTV%XPAR_IRRIGFRAC(KDIM,NVEGTYPE))
!  IF ( XUNIF_IRRIGFRAC_C/=XUNDEF )  WHERE ( XUNIF_IRRIGFRAC(:)==XUNDEF ) XUNIF_IRRIGFRAC(:)=XUNIF_IRRIGFRAC_C
!  DTV%LDATA_IRRIGFRAC(:) = .FALSE.
!  DTV%XPAR_IRRIGFRAC(:,:) = XUNDEF
!  DO JVEG = 1, NVEGTYPE
!    IF ( XUNIF_IRRIGFRAC(JVEG) /= XUNDEF ) THEN
!      DTV%XPAR_IRRIGFRAC(:,JVEG) = XUNIF_IRRIGFRAC(JVEG)
!      DTV%LDATA_IRRIGFRAC(JVEG) = .TRUE.
!    ELSEIF ( JVEG == NVT_IRR ) THEN
!      DTV%XPAR_IRRIGFRAC(:,JVEG) = 1
!      DTV%LDATA_IRRIGFRAC(JVEG) = .TRUE.
!    ENDIF
!  ENDDO
!  IF (ANY(DTV%LDATA_IRRIGFRAC)) THEN
!    WRITE(ILUOUT,*) ' '
!    WRITE(ILUOUT,*) '***********************************************************'
!    WRITE(ILUOUT,*) '* !! TAKE CARE !!                                         *'
!    WRITE(ILUOUT,*) '* Without ECOCLIMAP-SG, change of irrigated  vegtype has  *'
!    WRITE(ILUOUT,*) '* never been tested yet !!! Remove IRRIGFRAC input or test*'
!    WRITE(ILUOUT,*) '***********************************************************'
!    WRITE(ILUOUT,*) ' '
!    CALL ABOR1_SFX('PGD_ISBA_PAR: YOU HAVE TO TEST THIS POSSIBILITY ;)')
!  ENDIF
!  IF (ALL(.NOT.DTV%LDATA_IRRIGFRAC)) DEALLOCATE(DTV%XPAR_IRRIGFRAC)
ELSE
  DTV%LDATA_IRRIGFRAC(:) = .FALSE.
ENDIF
!
IF (LIRRIGMODE) THEN
  !
  ! Check if some constant are given for a vegetation type not used in this simulation (mainly if there not ECOCLIMAP-SG actvivated)
  DO JVEG = 1, NVEGTYPE
    IF ( .NOT.ANY(DTV%NPAR_VEG_IRR_USE(:)==JVEG)                                       &
         .AND. ( XUNIF_IRRIGTYPE(JVEG)/=XUNDEF .OR. XUNIF_IRRIGFRAC(JVEG)/=XUNDEF .OR. &
                 XUNIF_IRRIGFREQ(JVEG)/=XUNDEF .OR. XUNIF_IRRIGTIME(JVEG)/=XUNDEF .OR. &
                 ANY(XUNIF_WATSUP(JVEG,:)/=XUNDEF) .OR. ANY(XUNIF_F2THRESHOLD(JVEG,:)/=XUNDEF) ) ) THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '**************************************************************'
      WRITE(ILUOUT,*) '* You have indicate with NUNIF_VEG_IRR_USE (or using default *'
      WRITE(ILUOUT,*) '* values) a list of vegetation type for irrigation or        *'
      WRITE(ILUOUT,*) '* agricultural practices. But you give value for at least one*'
      WRITE(ILUOUT,*) '* ohter vegetation type: ', JVEG,'*'
      WRITE(ILUOUT,*) '**************************************************************'
      CALL ABOR1_SFX('PGD_ISBA_PAR: A VALUE ABOUT IRRIGATION IS PROVIDE FOR A NON IRRIGATE VEGETATION TYPE (DEF IN NVEG_IRR_USE)')
    ENDIF
  ENDDO
  !
  ! * Irrigtype
  ! -----------
  !
  IF ( XUNIF_IRRIGTYPE_C/=XUNDEF )  WHERE ( XUNIF_IRRIGTYPE(:)==XUNDEF ) XUNIF_IRRIGTYPE(:)=XUNIF_IRRIGTYPE_C
  DO JVEG = 1, NVEGTYPE ! By default, an irrigated vegtype is with sprinkler
    IF ( ANY(DTV%NPAR_VEG_IRR_USE(:)==JVEG) .AND. XUNIF_IRRIGTYPE(JVEG)==XUNDEF ) XUNIF_IRRIGTYPE(JVEG)=1
  ENDDO
  ALLOCATE(DTV%XPAR_IRRIGTYPE(KDIM,NVEGTYPE))
  IF ( U%LECOSG ) THEN ! Take care: MAJ or  MA1 are valable only from 0 to 31 (cf ZCOUNT in av_pgd_param.F90).
    CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'MA1','IRRIGTYPE: irrigation type','NAT',&
                         CFNAM_IRRIGTYPE,CFTYP_IRRIGTYPE,XUNIF_IRRIGTYPE,DTV%XPAR_IRRIGTYPE,DTV%LDATA_IRRIGTYPE)
  ELSE
    CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'MAJ','IRRIGTYPE: irrigation type','NAT',&
                         CFNAM_IRRIGTYPE,CFTYP_IRRIGTYPE,XUNIF_IRRIGTYPE,DTV%XPAR_IRRIGTYPE,DTV%LDATA_IRRIGTYPE)
  ENDIF
  IF (ALL(.NOT.DTV%LDATA_IRRIGTYPE)) DEALLOCATE(DTV%XPAR_IRRIGTYPE)
  !
  ALLOCATE(ZWORK(NIRR_TYPE+2))
  DO JL=1,NIRR_TYPE
    ZWORK(JL)=JL
  ENDDO
  ZWORK(NIRR_TYPE+1)=XUNDEF
  ZWORK(NIRR_TYPE+2)=0.
  DO JVEG = 1, NVEGTYPE
    IF ( DTV%LDATA_IRRIGTYPE(JVEG) ) THEN
      DO JL = 1,KDIM
        IF ( .NOT.ANY(ZWORK(:)==DTV%XPAR_IRRIGTYPE(JL,JVEG)) ) THEN
          WRITE(ILUOUT,*) ' '
          WRITE(ILUOUT,*) '***********************************************************'
          WRITE(ILUOUT,*) '* Error in PGD field preparation of irrigation types map  *'
          WRITE(ILUOUT,*) '* A value is not a valid irrigation type (0, 1, 2, 3):    *'
          WRITE(ILUOUT,*) '* It is:', DTV%XPAR_IRRIGTYPE(JL,JVEG), ' at the point = ', JL, ' for the vegtype = ', JVEG
          WRITE(ILUOUT,*) '*************************************************************'
          WRITE(ILUOUT,*) ' '
          CALL ABOR1_SFX('PGD_ISBA_PAR: WRONG VALUE IN IRRIGATION TYPE!')
        ENDIF
      ENDDO
    ENDIF
  ENDDO
  DEALLOCATE(ZWORK)
  !
  ! * Watsup
  ! --------
  !
  IF ( XUNIF_WATSUP_C/=XUNDEF )  WHERE ( XUNIF_WATSUP_CTIME(:)==XUNDEF ) XUNIF_WATSUP_CTIME(:)=XUNIF_WATSUP_C
  DO JL = 1,NTIME_MAX
    IF ( XUNIF_WATSUP_CTIME(JL)/=XUNDEF )  WHERE ( XUNIF_WATSUP(:,JL)==XUNDEF ) XUNIF_WATSUP(:,JL)=XUNIF_WATSUP_CTIME(JL)
  ENDDO
  ALLOCATE(DTV%XPAR_WATSUP(KDIM,DTV%NTIME,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','WATSUP: water supply during irr. (mm)','NAT',&
                         CFNAM_WATSUP,CFTYP_WATSUP,XUNIF_WATSUP,DTV%XPAR_WATSUP,DTV%LDATA_WATSUP)
  IF (ALL(.NOT.DTV%LDATA_WATSUP)) DEALLOCATE(DTV%XPAR_WATSUP)
  !
  ! * Irrigfreq
  ! -----------
  !
  ALLOCATE(DTV%XPAR_IRRIGFREQ(KDIM,NVEGTYPE))
  IF ( ALL(XUNIF_IRRIGFREQ_CTYPE(:)==XUNDEF) .AND. ALL(XUNIF_IRRIGFREQ(:)==XUNDEF) .AND. XUNIF_IRRIGFREQ_C==XUNDEF .AND. &
       ALL(CFTYP_IRRIGFREQ=='      ' ) .AND. ANY(DTV%LDATA_IRRIGTYPE) )  XUNIF_IRRIGFREQ_CTYPE(:)=IRRIGFREQ_CTYPE_DEFAULT(:)
  !
  IF ( ( ANY(XUNIF_IRRIGFREQ_CTYPE(:)/=XUNDEF) .AND. ANY(XUNIF_IRRIGFREQ(:) /=XUNDEF) ) .OR.  &
       ( ANY(XUNIF_IRRIGFREQ_CTYPE(:)==XUNDEF) .AND. XUNIF_IRRIGFREQ_C==XUNDEF ) ) THEN
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '*************************************************************'
    WRITE(ILUOUT,*) '* Error in PGD field preparation of irrigation parameters   *'
    WRITE(ILUOUT,*) '* If one IRRIGFREQ is define by type (XUNIF_IRRIGFREQ_CTYPE)*'
    IF ( ANY(XUNIF_IRRIGFREQ_CTYPE(:)/=XUNDEF) .AND. ANY(XUNIF_IRRIGFREQ(:) /=XUNDEF) ) THEN
      WRITE(ILUOUT,*) '* and one IRRIGFREQ s define by vegtype (XUNIF_IRRIGFREQ_C),*'
      WRITE(ILUOUT,*) '* there is a conflict for the IRRIGFREC computation method. *'
    ELSE
      WRITE(ILUOUT,*) '* all others IRRIGFREQ by type (or a default value for all  *'
      WRITE(ILUOUT,*) '* others with XUNIF_IRRIGFREQ_C) have to be define.         *'
    ENDIF
    WRITE(ILUOUT,*) '*************************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_ISBA_PAR: MISSING PRESCRIBED VALUE FOR XUNIF_IRRIGFREQ_C (TYPE)')
    !
  ELSEIF ( ANY(XUNIF_IRRIGFREQ_CTYPE(:)/=XUNDEF) .AND. ANY(DTV%LDATA_IRRIGTYPE) ) THEN
    WHERE ( XUNIF_IRRIGFREQ_CTYPE(:)==XUNDEF ) XUNIF_IRRIGFREQ_CTYPE(:)=XUNIF_IRRIGFREQ_C
    DO JVEG = 1,NVEGTYPE_MAX
      IF ( DTV%LDATA_IRRIGTYPE(JVEG) ) THEN
        DO JL=1,NIRR_TYPE
          WHERE ( DTV%XPAR_IRRIGTYPE(:,JVEG) == JL ) DTV%XPAR_IRRIGFREQ(:,JVEG) = XUNIF_IRRIGFREQ_CTYPE(JL)
        ENDDO
      ENDIF
    ENDDO
    DTV%LDATA_IRRIGFREQ(:) = DTV%LDATA_IRRIGTYPE(:)
    !
  ELSE
    IF ( XUNIF_IRRIGFREQ_C/=XUNDEF )  WHERE ( XUNIF_IRRIGFREQ(:)==XUNDEF ) XUNIF_IRRIGFREQ(:)=XUNIF_IRRIGFREQ_C
    ALLOCATE(DTV%XPAR_IRRIGFREQ(KDIM,NVEGTYPE))
    CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','IRRIGFREQ: minimum time between two irrigations. (s)','NAT',&
                         CFNAM_IRRIGFREQ,CFTYP_IRRIGFREQ,XUNIF_IRRIGFREQ,DTV%XPAR_IRRIGFREQ,DTV%LDATA_IRRIGFREQ)
  ENDIF
  IF (ALL(.NOT.DTV%LDATA_IRRIGFREQ)) DEALLOCATE(DTV%XPAR_IRRIGFREQ)
  !
  ! * Irrigtime
  ! -----------
  !
  IF ( XUNIF_IRRIGTIME_C/=XUNDEF )  WHERE ( XUNIF_IRRIGTIME(:)==XUNDEF ) XUNIF_IRRIGTIME(:)=XUNIF_IRRIGTIME_C
  ALLOCATE(DTV%XPAR_IRRIGTIME(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','IRRIGTIME: irrigation application time. (s)','NAT',&
                         CFNAM_IRRIGTIME,CFTYP_IRRIGTIME,XUNIF_IRRIGTIME,DTV%XPAR_IRRIGTIME,DTV%LDATA_IRRIGTIME)
  IF (ALL(.NOT.DTV%LDATA_IRRIGTIME)) DEALLOCATE(DTV%XPAR_IRRIGTIME)
  !
  ! * Threshold for irrigation
  ! --------------------------
  !
  IF ( ALL(XTHRESHOLD(:)<0.01) ) THEN
    XTHRESHOLD(:)=XTHRESHOLD_DEFAULT(:)
  ELSEIF ( ANY(XTHRESHOLD(:)<0.01) .OR. ANY(XTHRESHOLD(:)>1) ) THEN
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '******************************************************'
    WRITE(ILUOUT,*) '* Error in PGD field preparation for XTHRESHOLD:     *'
    WRITE(ILUOUT,*) '*   Some value are wrong (<0.01 or >1) in OPTION.NAM *'
    WRITE(ILUOUT,*) '******************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_ISBA_PAR: WRONG XTHRESHOLD IN OPTION.NAM')
  ENDIF
  !
  IF ( XUNIF_F2THRESHOLD_C/=XUNDEF )  WHERE ( XUNIF_F2THRESHOLD_CTIME(:)==XUNDEF ) XUNIF_F2THRESHOLD_CTIME(:)=XUNIF_F2THRESHOLD_C
  DO JL = 1,NTIME_MAX
    IF ( XUNIF_F2THRESHOLD_CTIME(JL)/=XUNDEF ) &
                                      WHERE ( XUNIF_F2THRESHOLD(:,JL)==XUNDEF ) XUNIF_F2THRESHOLD(:,JL)=XUNIF_F2THRESHOLD_CTIME(JL)
  ENDDO
  ALLOCATE(DTV%XPAR_F2THRESHOLD(KDIM,DTV%NTIME,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','F2THRESHOLD: humidity threshold for irrigation triggering. ','NAT',&
                         CFNAM_F2THRESHOLD,CFTYP_F2THRESHOLD,XUNIF_F2THRESHOLD,DTV%XPAR_F2THRESHOLD,DTV%LDATA_F2THRESHOLD)
  IF (ALL(.NOT.DTV%LDATA_F2THRESHOLD)) DEALLOCATE(DTV%XPAR_F2THRESHOLD)
  !
ELSE
  DTV%LDATA_IRRIGTYPE (:)  = .FALSE.
  DTV%LDATA_WATSUP(:)      = .FALSE.
  DTV%LDATA_IRRIGFREQ(:)   = .FALSE.
  DTV%LDATA_IRRIGTIME(:)   = .FALSE.
  DTV%LDATA_F2THRESHOLD(:) = .FALSE.
ENDIF
!
!
LMULTI_SEASON = .FALSE.
IF (LAGRIP .OR. LIRRIGMODE) THEN
  ! Check if some constant are given for a vegetation type not used in this simulation (mainly if there not ECOCLIMAP-SG actvivated)
  DO JVEG = 1, NVEGTYPE
    IF ( .NOT.ANY(DTV%NPAR_VEG_IRR_USE(:)==JVEG)                                 &
         .AND. ( XUNIF_SEED_M(JVEG)/=XUNDEF .OR. XUNIF_REAP_M(JVEG)/=XUNDEF .OR. &
                 XUNIF_SEED_D(JVEG)/=XUNDEF .OR. XUNIF_REAP_D(JVEG)/=XUNDEF ) ) THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '**************************************************************'
      WRITE(ILUOUT,*) '* You have indicate with NUNIF_VEG_IRR_USE (or using default *'
      WRITE(ILUOUT,*) '* values) a list of vegetation type for irrigation or        *'
      WRITE(ILUOUT,*) '* agricultural practices. But you give value for at least one*'
      WRITE(ILUOUT,*) '* ohter vegetation type: ', JVEG,'*'
      WRITE(ILUOUT,*) '**************************************************************'
      CALL ABOR1_SFX('PGD_ISBA_PAR: A VALUE ABOUT IRRIGATION OR AGRICULTURAL PRACTICES IS PROVIDE FOR A NON IRRIGATE VEGTYPE')
    ENDIF
  ENDDO
  !
  ! * Tseed
  ! -------
  !
  IF ( XUNIF_SEED_M_C/=XUNDEF )  WHERE ( XUNIF_SEED_M(:)==XUNDEF ) XUNIF_SEED_M(:)=XUNIF_SEED_M_C
  DO JVEG=1,NVEGTYPE
    IF (  XUNIF_SEED_M(JVEG) /= XUNDEF .AND. ( XUNIF_SEED_M(JVEG)<1 .OR. XUNIF_SEED_M(JVEG)>12 ) ) THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '*************************************************'
      WRITE(ILUOUT,*) '* Error in PGD field preparation for T_SEED_M:  *'
      WRITE(ILUOUT,*) '*   The input value do not exist (XUNIF_SEED_M) *'
      WRITE(ILUOUT,*) '*************************************************'
      WRITE(ILUOUT,*) ' '
      CALL ABOR1_SFX('PGD_ISBA_PAR: WRONG INPUT TSEED MONTH (XUNIF_SEED_M)')
    ENDIF
  ENDDO
  ALLOCATE(DTV%XPAR_SEED_M(KDIM,NVEGTYPE))
  ! Take care: MAJ or  MA1 are valable only from 0 to 31 (cf ZCOUNT in av_pgd_param.F90).
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'MAJ','SEED_M: month of seeding','NAT',&
                         CFNAM_SEED_M,CFTYP_SEED_M,XUNIF_SEED_M,DTV%XPAR_SEED_M,DTV%LDATA_SEED_M)
  !
  IF ( XUNIF_SEED_D_C/=XUNDEF )  WHERE ( XUNIF_SEED_D(:)==XUNDEF ) XUNIF_SEED_D(:)=XUNIF_SEED_D_C
  DO JVEG=1,NVEGTYPE
    IF ( XUNIF_SEED_M(JVEG) /= XUNDEF .AND. XUNIF_SEED_D(JVEG) /= XUNDEF) THEN 
      IF ( XUNIF_SEED_D(JVEG)<1 .OR. XUNIF_SEED_D(JVEG)>INB_DM(INT(XUNIF_SEED_M(JVEG))) ) THEN
        WRITE(ILUOUT,*) ' '
        WRITE(ILUOUT,*) '**************************************************************'
        WRITE(ILUOUT,*) '* Error in PGD field preparation for T_SEED_D (XUNIF_SEED_D):*'
        WRITE(ILUOUT,*) '*   The input value do not exist: <1 or >[monthDays].        *'
        WRITE(ILUOUT,*) '**************************************************************'
        WRITE(ILUOUT,*) ' '
        CALL ABOR1_SFX('PGD_ISBA_PAR: WRONG INPUT TSEED DAY (XUNIF_SEED_D)')
      ENDIF
    ELSEIF ( XUNIF_SEED_M(JVEG) /= XUNDEF .OR. XUNIF_SEED_D(JVEG) /= XUNDEF) THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '**************************************************************'
      WRITE(ILUOUT,*) '* Error in PGD field preparation for T_SEED_D or T_SEED_M    *'
      WRITE(ILUOUT,*) '*   The input value have to be define (or undef) for both    *'
      WRITE(ILUOUT,*) '*   together (XUNIF_SEED_D and XUNIF_SEED_M).                *'
      WRITE(ILUOUT,*) '**************************************************************'
      WRITE(ILUOUT,*) ' '
      CALL ABOR1_SFX('PGD_ISBA_PAR: NOT CONSISTENT INPUT BETWEEN TSEED DAY AND  MONTH)')
    ENDIF
  ENDDO
  !
  ALLOCATE(DTV%XPAR_SEED_D(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'MAJ','SEED_D: day of seeding','NAT',&
                         CFNAM_SEED_D,CFTYP_SEED_D,XUNIF_SEED_D,DTV%XPAR_SEED_D,DTV%LDATA_SEED_D)
  !
  ! Stard random generator (if needeed)
  IF ( ( XUNIF_SEED_D_DELTA/=XUNDEF .AND. XUNIF_SEED_D_DELTA/=0. ) .OR. &
       ( XUNIF_REAP_D_DELTA/=XUNDEF .AND. XUNIF_REAP_D_DELTA/=0. ) ) THEN
    !
    ! To know the size needid fot the put
    CALL RANDOM_SEED(SIZE=JL)
    ALLOCATE(SEED(1:JL))
    !
    ! First try if the OS provides a random number generator
    OPEN(newunit=JK, file="/dev/urandom", access="stream", &
         form="unformatted", action="read", status="old", iostat=JN)
    !
    IF (JN == 0) THEN
      READ(JK) SEED
      CLOSE(JK)
    !
    ! else compute a number with clock (milliseconde or more) and ID of the task
    ELSE
      ! Start with system clock (more precisely)
      CALL SYSTEM_CLOCK(VALUES(1))
      !
      IF (VALUES(1) /= 0) THEN
        TM = TRANSFER(VALUES(1),TM)
      !
      ! else, use the fortran time
      ELSE
        CALL DATE_AND_TIME(VALUES=VALUES)
        JK = (VALUES(1) - 1970) * 365_8 * 24 * 60 * 60 * 1000  +  VALUES(2) * 31_8 * 24 * 60 * 60 * 1000 &
             + VALUES(3) * 24 * 60 * 60 * 60 * 1000            +  VALUES(5) * 60 * 60 * 1000             &
             + VALUES(6) * 60 * 1000                           +  VALUES(7) * 1000        +       VALUES(8)
        TM = TRANSFER(JK,TM)
      ENDIF
      !
      ! MIXT
      JN = IEOR(TM(1), TM(2))
      JN = IEOR(JN   , NRANK + 1099279)
      !
      ! Assign SEED
      ! In case of...
      IF (JL >= 3) THEN
        SEED(1) = TM(1) + 36269
        SEED(2) = TM(2) + 72551
        IF (JL > 3) THEN
          SEED(4:) = JN + 37 * (/ (JK, JK = 0, JL - 4 ) /)
        ENDIF
        !
      ELSE
        SEED = JN + 37 * (/ (JK, JK = 0, JL - 1 ) /)
      ENDIF
    ENDIF
    !
    CALL RANDOM_SEED(PUT=SEED)
    !
  ENDIF
  !
  IF ( ANY(XUNIF_SEED_D/=XUNDEF) .AND. XUNIF_SEED_D_DELTA/=XUNDEF .AND. XUNIF_SEED_D_DELTA/=0. & 
       .AND. ALL(CFTYP_SEED_D=='      ') .AND. ALL(CFTYP_SEED_M=='      ') ) THEN
    !
    DO JVEG=1,NVEGTYPE
      IF ( XUNIF_SEED_D(JVEG)/=XUNDEF .AND. DTV%LDATA_SEED_D(JVEG) ) THEN ! Note: before we stop if we don't have XUNIF_SEED_M(JVEG)/=XUNDEF
        DO  JL= 1, SIZE(DTV%XPAR_SEED_D,1)
          CALL RANDOM_NUMBER(RAND)
          !
          NRAND = INT( RAND * (XUNIF_SEED_D_DELTA*2+1) - XUNIF_SEED_D_DELTA )
          CALL ADD_DAYS( 1987 , INT(DTV%XPAR_SEED_M(JL,JVEG)), INT(DTV%XPAR_SEED_D(JL,JVEG)), NRAND, IMONTH, IDAY)
          DTV%XPAR_SEED_M(JL,JVEG) = IMONTH
          DTV%XPAR_SEED_D(JL,JVEG) = IDAY
          !
        ENDDO
      ENDIF
    ENDDO
  !
  ELSEIF ( XUNIF_SEED_D_DELTA/=XUNDEF .AND. XUNIF_SEED_D_DELTA/=0 .AND. ( ALL(XUNIF_SEED_D==XUNDEF) .OR.       & 
           ANY(CFTYP_SEED_D/='      ') .OR. ANY(CFTYP_SEED_M/='      ')) ) THEN
    CALL ABOR1_SFX('PGD_ISBA_PAR: IF XUNIF_SEED_D_DELTA IS DEFINE, XUNIF_SEED_D/M(_C) HAVE TO BE DEFINE (AND NO MAP LINK)')
  ENDIF
  !
  ! * Treap
  ! -------
  !
  IF ( XUNIF_REAP_M_C/=XUNDEF )  WHERE ( XUNIF_REAP_M(:)==XUNDEF ) XUNIF_REAP_M(:)=XUNIF_REAP_M_C
  DO JVEG=1,NVEGTYPE
    IF (  XUNIF_REAP_M(JVEG) /= XUNDEF .AND. ( XUNIF_REAP_M(JVEG)<1 .OR. XUNIF_REAP_M(JVEG)>12 ) ) THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '*************************************************'
      WRITE(ILUOUT,*) '* Error in PGD field preparation for T_REAP_M:  *'
      WRITE(ILUOUT,*) '*   The input value do not exist (XUNIF_REAP_M) *'
      WRITE(ILUOUT,*) '*************************************************'
      WRITE(ILUOUT,*) ' '
      CALL ABOR1_SFX('PGD_ISBA_PAR: WRONG INPUT REAP MONTH (XUNIF_REAP_M)')
    ENDIF
  ENDDO
  !
  ALLOCATE(DTV%XPAR_REAP_M(KDIM,NVEGTYPE))
  ! Take care: MAJ or  MA1 are valable only from 0 to 31 (cf ZCOUNT in av_pgd_param.F90).
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'MAJ','REAP_M: month of reaping','NAT',&
                         CFNAM_REAP_M,CFTYP_REAP_M,XUNIF_REAP_M,DTV%XPAR_REAP_M,DTV%LDATA_REAP_M)
  !
  IF ( XUNIF_REAP_D_C/=XUNDEF )  WHERE ( XUNIF_REAP_D(:)==XUNDEF ) XUNIF_REAP_D(:)=XUNIF_REAP_D_C
  DO JVEG=1,NVEGTYPE
    IF ( XUNIF_REAP_M(JVEG) /= XUNDEF .AND. XUNIF_REAP_D(JVEG) /= XUNDEF) THEN
      IF ( XUNIF_REAP_D(JVEG)<1 .OR. XUNIF_REAP_D(JVEG)>INB_DM(INT(XUNIF_REAP_M(JVEG))) ) THEN
        WRITE(ILUOUT,*) ' '
        WRITE(ILUOUT,*) '**************************************************************'
        WRITE(ILUOUT,*) '* Error in PGD field preparation for T_REAP_D (XUNIF_REAP_D):*'
        WRITE(ILUOUT,*) '*   The input value do not exist: <1 or >[monthDays].        *'
        WRITE(ILUOUT,*) '**************************************************************'
        WRITE(ILUOUT,*) ' '
        CALL ABOR1_SFX('PGD_ISBA_PAR: WRONG INPUT TREAP DAY (XUNIF_REAP_D)')
      ENDIF
    ELSEIF ( XUNIF_REAP_M(JVEG) /= XUNDEF .OR. XUNIF_REAP_D(JVEG) /= XUNDEF) THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '**************************************************************'
      WRITE(ILUOUT,*) '* Error in PGD field preparation for T_REAP_D or T_REAP_M    *'
      WRITE(ILUOUT,*) '*   The input value have to be define (or undef) for both    *'
      WRITE(ILUOUT,*) '*   together (XUNIF_REAP_D and XUNIF_REAP_M).                *'
      WRITE(ILUOUT,*) '**************************************************************'
      WRITE(ILUOUT,*) ' '
      CALL ABOR1_SFX('PGD_ISBA_PAR: NOT CONSISTENT INPUT BETWEEN TREAP DAY AND  MONTH)')
    ENDIF
  ENDDO
  ALLOCATE(DTV%XPAR_REAP_D(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'MAJ','REAP_D: day of reaping','NAT',&
                         CFNAM_REAP_D,CFTYP_REAP_D,XUNIF_REAP_D,DTV%XPAR_REAP_D,DTV%LDATA_REAP_D)
  !
  IF ( ANY(XUNIF_REAP_D/=XUNDEF) .AND. XUNIF_REAP_D_DELTA/=XUNDEF .AND. XUNIF_REAP_D_DELTA/=0. &
       .AND. ALL(CFTYP_REAP_D=='      ') .AND. ALL(CFTYP_REAP_M=='      ') ) THEN
    !
    DO JVEG=1,NVEGTYPE
      IF ( XUNIF_REAP_D(JVEG)/=XUNDEF .AND. DTV%LDATA_REAP_D(JVEG) ) THEN ! Note: before we stop if we don't have XUNIF_REAP_M(JVEG)/=XUNDEF
        DO  JL= 1, SIZE(DTV%XPAR_REAP_D,1)
          CALL RANDOM_NUMBER(RAND)
          !
          NRAND = INT( RAND * (XUNIF_REAP_D_DELTA*2+1) - XUNIF_REAP_D_DELTA )
          CALL ADD_DAYS( 1987 , INT(DTV%XPAR_REAP_M(JL,JVEG)), INT(DTV%XPAR_REAP_D(JL,JVEG)), NRAND, IMONTH, IDAY)
          DTV%XPAR_REAP_M(JL,JVEG) = IMONTH
          DTV%XPAR_REAP_D(JL,JVEG) = IDAY
          !
        ENDDO
      ENDIF
    ENDDO
  !
  ELSEIF ( XUNIF_REAP_D_DELTA/=XUNDEF .AND. XUNIF_REAP_D_DELTA/=0 .AND. (ALL(XUNIF_REAP_D==XUNDEF) .OR.       & 
           ANY(CFTYP_REAP_D/='      ') .OR. ANY(CFTYP_REAP_M/='      ')) ) THEN
    CALL ABOR1_SFX('PGD_ISBA_PAR: IF XUNIF_REAP_D_DELTA IS DEFINE, XUNIF_REAP_D/M(_C) HAVE TO BE DEFINE (AND NO MAP LINK)')
  ENDIF
  !
  IF ( U%LECOSG .AND. (LIRRIGMODE .OR. LAGRIP) ) THEN
    DO JL = 1,NVEG_IRR
      IF ( .NOT.DTV%LDATA_SEED_M(DTV%NPAR_VEG_IRR_USE(JL)) ) DTV%XPAR_SEED_M(:,DTV%NPAR_VEG_IRR_USE(JL)) = DATES_IRRIG_DEFAULT(1)
      IF ( .NOT.DTV%LDATA_SEED_M(DTV%NPAR_VEG_IRR_USE(JL)) ) DTV%LDATA_SEED_M(DTV%NPAR_VEG_IRR_USE(JL)) = .TRUE.
      IF ( .NOT.DTV%LDATA_SEED_D(DTV%NPAR_VEG_IRR_USE(JL)) ) DTV%XPAR_SEED_D(:,DTV%NPAR_VEG_IRR_USE(JL)) = DATES_IRRIG_DEFAULT(2)
      IF ( .NOT.DTV%LDATA_SEED_D(DTV%NPAR_VEG_IRR_USE(JL)) ) DTV%LDATA_SEED_D(DTV%NPAR_VEG_IRR_USE(JL)) = .TRUE.
      IF ( .NOT.DTV%LDATA_REAP_M(DTV%NPAR_VEG_IRR_USE(JL)) ) DTV%XPAR_REAP_M(:,DTV%NPAR_VEG_IRR_USE(JL)) = DATES_IRRIG_DEFAULT(3)
      IF ( .NOT.DTV%LDATA_REAP_M(DTV%NPAR_VEG_IRR_USE(JL)) ) DTV%LDATA_REAP_M(DTV%NPAR_VEG_IRR_USE(JL)) = .TRUE.
      IF ( .NOT.DTV%LDATA_REAP_D(DTV%NPAR_VEG_IRR_USE(JL)) ) DTV%XPAR_REAP_D(:,DTV%NPAR_VEG_IRR_USE(JL)) = DATES_IRRIG_DEFAULT(4)
      IF ( .NOT.DTV%LDATA_REAP_D(DTV%NPAR_VEG_IRR_USE(JL)) ) DTV%LDATA_REAP_D(DTV%NPAR_VEG_IRR_USE(JL)) = .TRUE.
    ENDDO
  ENDIF
  IF (ALL(.NOT.DTV%LDATA_SEED_M)) DEALLOCATE(DTV%XPAR_SEED_M)
  IF (ALL(.NOT.DTV%LDATA_SEED_D)) DEALLOCATE(DTV%XPAR_SEED_D)
  IF (ALL(.NOT.DTV%LDATA_REAP_M)) DEALLOCATE(DTV%XPAR_REAP_M)
  IF (ALL(.NOT.DTV%LDATA_REAP_D)) DEALLOCATE(DTV%XPAR_REAP_D)
  !
  ! Test is there is some obvious missing value
  !
  DO JVEG = 1,NVEGTYPE
    IF ( ( DTV%LDATA_SEED_M(JVEG) .AND. .NOT.DTV%LDATA_SEED_D(JVEG) ) .OR. &
         ( DTV%LDATA_REAP_M(JVEG) .AND. .NOT.DTV%LDATA_REAP_D(JVEG) ) .OR. &
         ( DTV%LDATA_SEED_M(JVEG) .AND. .NOT.DTV%LDATA_REAP_M(JVEG) ) ) THEN
      CALL ABOR1_SFX('PGD_ISBA_PAR: FOR A VEGTYPE, IF THERE IS VALUES FOR ONE DATE, ALL ASSOCIATED DATE HAVE TO BE GIVEN')
    ENDIF
  ENDDO
  !
  !
  ! Case of multiannual season for crops (agricultural practices + irrigations)
  ! ------------------------------------
  !
  ! Only in case of any TSEED or TREAP is define for the first season
  !
  IF ( ANY(DTV%LDATA_SEED_M) ) THEN
    !
    ! # if some constant are given for a vegetation type not used (irrigation /  agricultural practices) in this simulation    
    DO JVEG = 1, NVEGTYPE
      IF ( .NOT.ANY(DTV%NPAR_VEG_IRR_USE(:)==JVEG)                                       &
           .AND. ( XUNIF_SEED_S2_M(JVEG)/=XUNDEF .OR. XUNIF_REAP_S2_M(JVEG)/=XUNDEF .OR. &
                   XUNIF_SEED_S2_D(JVEG)/=XUNDEF .OR. XUNIF_REAP_S2_D(JVEG)/=XUNDEF ) ) THEN
        WRITE(ILUOUT,*) ' '
        WRITE(ILUOUT,*) '**************************************************************'
        WRITE(ILUOUT,*) '* You have indicate with NUNIF_VEG_IRR_USE (or using default *'
        WRITE(ILUOUT,*) '* values) a list of vegetation type for irrigation or        *'
        WRITE(ILUOUT,*) '* agricultural practices. But you give value for at least one*'
        WRITE(ILUOUT,*) '* ohter vegetation type (in saison 2): ', JVEG,'*'
        WRITE(ILUOUT,*) '**************************************************************'
        WRITE(ILUOUT,*) ' '
        CALL ABOR1_SFX('PGD_ISBA_PAR: A DATE OF AGRICULTURAL PRACTICES IS PROVIDE FOR A NON IRRIGATE VEGTYPE (SAISON 2)')
      ENDIF
    ENDDO
    !
    ! * Tseed
    ! -------
    !
    IF ( XUNIF_SEED_S2_M_C/=XUNDEF )  WHERE ( XUNIF_SEED_S2_M(:)==XUNDEF ) XUNIF_SEED_S2_M(:)=XUNIF_SEED_S2_M_C
    DO JVEG=1,NVEGTYPE
      IF (  XUNIF_SEED_S2_M(JVEG) /= XUNDEF .AND. ( XUNIF_SEED_S2_M(JVEG)<1 .OR. XUNIF_SEED_S2_M(JVEG)>12 ) ) THEN
        WRITE(ILUOUT,*) ' '
        WRITE(ILUOUT,*) '**************************************************************'
        WRITE(ILUOUT,*) '* Error in PGD field preparation for T_SEED_S2_M (saison 2): *'
        WRITE(ILUOUT,*) '*   The input value do not exist (XUNIF_SEED_S2_M)           *'
        WRITE(ILUOUT,*) '**************************************************************'
        WRITE(ILUOUT,*) ' '
        CALL ABOR1_SFX('PGD_ISBA_PAR: WRONG INPUT TSEED MONTH (XUNIF_SEED_2_M) IN SAISON 2')
      ENDIF
    ENDDO
    ALLOCATE(DTV%XPAR_SEED_S2_M(KDIM,NVEGTYPE))
    ! Take care: MAJ or  MA1 are valable only from 0 to 31 (cf ZCOUNT in av_pgd_param.F90).
    CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                           HPROGRAM,'MAJ','SEED_2_M: month of seeding','NAT',&
                           CFNAM_SEED_S2_M,CFTYP_SEED_S2_M,XUNIF_SEED_S2_M,DTV%XPAR_SEED_S2_M,DTV%LDATA_SEED_S2_M)
    !
    IF ( XUNIF_SEED_S2_D_C/=XUNDEF )  WHERE ( XUNIF_SEED_S2_D(:)==XUNDEF ) XUNIF_SEED_S2_D(:)=XUNIF_SEED_S2_D_C
    DO JVEG=1,NVEGTYPE
      IF ( XUNIF_SEED_S2_M(JVEG) /= XUNDEF .AND. XUNIF_SEED_S2_D(JVEG) /= XUNDEF) THEN
        IF ( XUNIF_SEED_S2_D(JVEG)<1 .OR. XUNIF_SEED_S2_D(JVEG)>INB_DM(INT(XUNIF_SEED_S2_M(JVEG))) ) THEN
          WRITE(ILUOUT,*) ' '
          WRITE(ILUOUT,*) '*************************************************************'
          WRITE(ILUOUT,*) '* Error in PGD field preparation for T_SEED_S2_D (saison 2):*'
          WRITE(ILUOUT,*) '*   The input value do not exist: <1 or >[monthDays].       *'
          WRITE(ILUOUT,*) '**************************************************************'
          WRITE(ILUOUT,*) ' '
          CALL ABOR1_SFX('PGD_ISBA_PAR: WRONG INPUT TSEED_S2 (XUNIF_SEED_S2_D)')
        ENDIF
      ELSEIF ( XUNIF_SEED_S2_M(JVEG) /= XUNDEF .OR. XUNIF_SEED_S2_D(JVEG) /= XUNDEF) THEN
        WRITE(ILUOUT,*) ' '
        WRITE(ILUOUT,*) '*****************************************************************'
        WRITE(ILUOUT,*) '* Error in PGD field preparation for T_SEED_S2_D or T_SEED_S2_M *'
        WRITE(ILUOUT,*) '*   The input value have to be define (or undef) for both       *'
        WRITE(ILUOUT,*) '*   together (XUNIF_SEED_S2_D and XUNIF_SEED_S2_M).             *'
        WRITE(ILUOUT,*) '*****************************************************************'
        WRITE(ILUOUT,*) ' '
        CALL ABOR1_SFX('PGD_ISBA_PAR: NOT CONSISTENT INPUT BETWEEN TSEED_S2 DAY AND  MONTH)')
      ENDIF
    ENDDO
    ALLOCATE(DTV%XPAR_SEED_S2_D(KDIM,NVEGTYPE))
    CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                           HPROGRAM,'MAJ','SEED_S2_D: day of seeding','NAT',&
                           CFNAM_SEED_S2_D,CFTYP_SEED_S2_D,XUNIF_SEED_S2_D,DTV%XPAR_SEED_S2_D,DTV%LDATA_SEED_S2_D)
    !
    ! Add random if a DELTA is imposed
    IF ( ANY(XUNIF_SEED_S2_D/=XUNDEF) .AND. XUNIF_SEED_S2_D_DELTA/=XUNDEF .AND. XUNIF_SEED_S2_D_DELTA/=0. &
         .AND. ALL(CFTYP_SEED_S2_D=='      ') .AND. ALL(CFTYP_SEED_S2_M=='      ') ) THEN
      !
      DO JVEG=1,NVEGTYPE
        IF ( XUNIF_SEED_S2_D(JVEG)/=XUNDEF .AND. DTV%LDATA_SEED_S2_D(JVEG) ) THEN ! Note: before we stop if we don't have XUNIF_SEED_M(JVEG)/=XUNDEF
          DO  JL= 1, SIZE(DTV%XPAR_SEED_S2_D,1)
            CALL RANDOM_NUMBER(RAND)
            !
            NRAND = INT( RAND * (XUNIF_SEED_S2_D_DELTA*2+1) - XUNIF_SEED_S2_D_DELTA )
            CALL ADD_DAYS( 1987 , INT(DTV%XPAR_SEED_S2_M(JL,JVEG)), INT(DTV%XPAR_SEED_S2_D(JL,JVEG)), NRAND, IMONTH, IDAY)
            DTV%XPAR_SEED_S2_M(JL,JVEG) = IMONTH
            DTV%XPAR_SEED_S2_D(JL,JVEG) = IDAY
            !
          ENDDO
        ENDIF
      ENDDO
      !
    ELSEIF ( XUNIF_SEED_S2_D_DELTA/=XUNDEF .AND. XUNIF_SEED_S2_D_DELTA/=0 .AND. ( ALL(XUNIF_SEED_S2_D==XUNDEF) .OR.       &
             ANY(CFTYP_SEED_S2_D/='      ') .OR. ANY(CFTYP_SEED_S2_M/='      ')) ) THEN
      CALL ABOR1_SFX('PGD_ISBA_PAR: IF XUNIF_SEED_S2_D_DELTA IS DEFINE, XUNIF_SEED_S2_D/M(_C) HAVE TO BE DEFINE (AND NO MAP LINK)')
    ENDIF
    !
    ! * Treap
    ! -------
    !
    IF ( XUNIF_REAP_S2_M_C/=XUNDEF )  WHERE ( XUNIF_REAP_S2_M(:)==XUNDEF ) XUNIF_REAP_S2_M(:)=XUNIF_REAP_S2_M_C
    DO JVEG=1,NVEGTYPE
      IF (  XUNIF_REAP_S2_M(JVEG) /= XUNDEF .AND. ( XUNIF_REAP_S2_M(JVEG)<1 .OR. XUNIF_REAP_S2_M(JVEG)>12 ) ) THEN
        WRITE(ILUOUT,*) ' '
        WRITE(ILUOUT,*) '**************************************************************'
        WRITE(ILUOUT,*) '* Error in PGD field preparation for T_REAP_S2_M (saison 2): *'
        WRITE(ILUOUT,*) '*   The input value do not exist (XUNIF_REAP_S2_M)           *'
        WRITE(ILUOUT,*) '**************************************************************'
        WRITE(ILUOUT,*) ' '
        CALL ABOR1_SFX('PGD_ISBA_PAR: WRONG INPUT REAP MONTH FOR SAISON 2 (XUNIF_REAP_S2_M)')
      ENDIF
    ENDDO
    !
    ALLOCATE(DTV%XPAR_REAP_S2_M(KDIM,NVEGTYPE))
    ! Take care: MAJ or  MA1 are valable only from 0 to 31 (cf ZCOUNT in av_pgd_param.F90).
    CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                           HPROGRAM,'MAJ','REAP_S2_M: month of reaping','NAT',&
                           CFNAM_REAP_S2_M,CFTYP_REAP_S2_M,XUNIF_REAP_S2_M,DTV%XPAR_REAP_S2_M,DTV%LDATA_REAP_S2_M)
    !
    IF ( XUNIF_REAP_S2_D_C/=XUNDEF )  WHERE ( XUNIF_REAP_S2_D(:)==XUNDEF ) XUNIF_REAP_S2_D(:)=XUNIF_REAP_S2_D_C
    DO JVEG=1,NVEGTYPE
      IF ( XUNIF_REAP_S2_M(JVEG) /= XUNDEF .AND. XUNIF_REAP_S2_D(JVEG) /= XUNDEF) THEN
        IF ( XUNIF_REAP_S2_D(JVEG)<1 .OR. XUNIF_REAP_S2_D(JVEG)>INB_DM(INT(XUNIF_REAP_S2_M(JVEG))) ) THEN
          WRITE(ILUOUT,*) ' '
          WRITE(ILUOUT,*) '*************************************************************'
          WRITE(ILUOUT,*) '* Error in PGD field preparation for T_REAP_S2_D (saison 2):*'
          WRITE(ILUOUT,*) '*   The input value do not exist: <1 or >[monthDays].       *'
          WRITE(ILUOUT,*) '**************************************************************'
          WRITE(ILUOUT,*) ' '
          CALL ABOR1_SFX('PGD_ISBA_PAR: WRONG INPUT TREAP_S2 (XUNIF_REAP_S2_D)')
        ENDIF
      ELSEIF ( XUNIF_REAP_S2_M(JVEG) /= XUNDEF .OR. XUNIF_REAP_S2_D(JVEG) /= XUNDEF) THEN
        WRITE(ILUOUT,*) ' '
        WRITE(ILUOUT,*) '*****************************************************************'
        WRITE(ILUOUT,*) '* Error in PGD field preparation for T_REAP_S2_D or T_REAP_S2_M *'
        WRITE(ILUOUT,*) '*   The input value have to be define (or undef) for both       *'
        WRITE(ILUOUT,*) '*   together (XUNIF_REAP_S2_D and XUNIF_REAP_S2_M).             *'
        WRITE(ILUOUT,*) '*****************************************************************'
        WRITE(ILUOUT,*) ' '
        CALL ABOR1_SFX('PGD_ISBA_PAR: NOT CONSISTENT INPUT BETWEEN TREAP_S2 DAY AND  MONTH)')
      ENDIF
    ENDDO
    ALLOCATE(DTV%XPAR_REAP_S2_D(KDIM,NVEGTYPE))
    CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                           HPROGRAM,'MAJ','REAP_S2_D: day of reaping','NAT',&
                           CFNAM_REAP_S2_D,CFTYP_REAP_S2_D,XUNIF_REAP_S2_D,DTV%XPAR_REAP_S2_D,DTV%LDATA_REAP_S2_D)
    !
    IF ( ANY(XUNIF_REAP_S2_D/=XUNDEF) .AND. XUNIF_REAP_S2_D_DELTA/=XUNDEF .AND. XUNIF_REAP_S2_D_DELTA/=0. &
         .AND. ALL(CFTYP_REAP_S2_D=='      ') .AND. ALL(CFTYP_REAP_S2_M=='      ') ) THEN
      !
      DO JVEG=1,NVEGTYPE
        IF ( XUNIF_REAP_S2_D(JVEG)/=XUNDEF .AND. DTV%LDATA_REAP_S2_D(JVEG) ) THEN ! Note: before we stop if we don't have XUNIF_REAP_M(JVEG)/=XUNDEF
          DO  JL= 1, SIZE(DTV%XPAR_REAP_S2_D,1)
            CALL RANDOM_NUMBER(RAND)
            !
            NRAND = INT( RAND * (XUNIF_REAP_S2_D_DELTA*2+1) - XUNIF_REAP_S2_D_DELTA )
            CALL ADD_DAYS( 1987 , INT(DTV%XPAR_REAP_S2_M(JL,JVEG)), INT(DTV%XPAR_REAP_S2_D(JL,JVEG)), NRAND, IMONTH, IDAY)
            DTV%XPAR_REAP_S2_M(JL,JVEG) = IMONTH
            DTV%XPAR_REAP_S2_D(JL,JVEG) = IDAY
            !
          ENDDO
        ENDIF
      ENDDO
    !
    ELSEIF ( XUNIF_REAP_S2_D_DELTA/=XUNDEF .AND. XUNIF_REAP_S2_D_DELTA/=0 .AND. (ALL(XUNIF_REAP_S2_D==XUNDEF) .OR.       &
             ANY(CFTYP_REAP_S2_D/='      ') .OR. ANY(CFTYP_REAP_S2_M/='      ')) ) THEN
      CALL ABOR1_SFX('PGD_ISBA_PAR: IF XUNIF_REAP_S2_D_DELTA IS DEFINE, XUNIF_REAP_S2_D/M(_C) HAVE TO BE DEFINE (AND NO MAP LINK)')
    ENDIF
    !
    IF (ALL(.NOT.DTV%LDATA_SEED_S2_M)) DEALLOCATE(DTV%XPAR_SEED_S2_M)
    IF (ALL(.NOT.DTV%LDATA_SEED_S2_D)) DEALLOCATE(DTV%XPAR_SEED_S2_D)
    IF (ALL(.NOT.DTV%LDATA_REAP_S2_M)) DEALLOCATE(DTV%XPAR_REAP_S2_M)
    IF (ALL(.NOT.DTV%LDATA_REAP_S2_D)) DEALLOCATE(DTV%XPAR_REAP_S2_D)
    !
    ! Test is there is some obvious missing value
    !
    DO JVEG = 1,NVEGTYPE
      IF ( ( DTV%LDATA_SEED_S2_M(JVEG) .AND. .NOT.DTV%LDATA_SEED_S2_D(JVEG) ) .OR. &
           ( DTV%LDATA_REAP_S2_M(JVEG) .AND. .NOT.DTV%LDATA_REAP_S2_D(JVEG) ) .OR. &
           ( DTV%LDATA_SEED_S2_M(JVEG) .AND. .NOT.DTV%LDATA_REAP_S2_M(JVEG) ) ) THEN
        CALL ABOR1_SFX('PGD_ISBA_PAR: FOR A VEGTYPE, IF THERE IS VALUES FOR ONE DATE, ALL ASSOCIATED DATE HAVE TO BE GIVEN (S-2)')
      ENDIF
    ENDDO
    !
    IF (ANY(DTV%LDATA_SEED_S2_M)) LMULTI_SEASON = .TRUE.
    !
    IF ( LMULTI_SEASON ) THEN
      !
      ! # if some constant are given for a vegetation type not used (irrigation /  agricultural practices) in this simulation
      DO JVEG = 1, NVEGTYPE
        IF ( .NOT.ANY(DTV%NPAR_VEG_IRR_USE(:)==JVEG)                                       &
             .AND. ( XUNIF_SEED_S3_M(JVEG)/=XUNDEF .OR. XUNIF_REAP_S3_M(JVEG)/=XUNDEF .OR. &
                     XUNIF_SEED_S3_D(JVEG)/=XUNDEF .OR. XUNIF_REAP_S3_D(JVEG)/=XUNDEF ) ) THEN
          WRITE(ILUOUT,*) ' '
          WRITE(ILUOUT,*) '**************************************************************'
          WRITE(ILUOUT,*) '* You have indicate with NUNIF_VEG_IRR_USE (or using default *'
          WRITE(ILUOUT,*) '* values) a list of vegetation type for irrigation or        *'
          WRITE(ILUOUT,*) '* agricultural practices. But you give value for at least one*'
          WRITE(ILUOUT,*) '* ohter vegetation type (in saison 3): ', JVEG,'*'
          WRITE(ILUOUT,*) '**************************************************************'
          WRITE(ILUOUT,*) ' '
          CALL ABOR1_SFX('PGD_ISBA_PAR: A DATE OF AGRICULTURAL PRACTICES IS PROVIDE FOR A NON IRRIGATE VEGTYPE (SAISON 3)')
        ENDIF
      ENDDO
      !
      ! * Tseed
      ! -------
      !
      IF ( XUNIF_SEED_S3_M_C/=XUNDEF )  WHERE ( XUNIF_SEED_S3_M(:)==XUNDEF ) XUNIF_SEED_S3_M(:)=XUNIF_SEED_S3_M_C
      DO JVEG=1,NVEGTYPE
        IF (  XUNIF_SEED_S3_M(JVEG) /= XUNDEF .AND. ( XUNIF_SEED_S3_M(JVEG)<1 .OR. XUNIF_SEED_S3_M(JVEG)>12 ) ) THEN
          WRITE(ILUOUT,*) ' '
          WRITE(ILUOUT,*) '**************************************************************'
          WRITE(ILUOUT,*) '* Error in PGD field preparation for T_SEED_S3_M (saison 3): *'
          WRITE(ILUOUT,*) '*   The input value do not exist (XUNIF_SEED_S3_M)           *'
          WRITE(ILUOUT,*) '**************************************************************'
          WRITE(ILUOUT,*) ' '
          CALL ABOR1_SFX('PGD_ISBA_PAR: WRONG INPUT TSEED MONTH (XUNIF_SEED_S3_M) IN SAISON 3')
        ENDIF
      ENDDO
      ALLOCATE(DTV%XPAR_SEED_S3_M(KDIM,NVEGTYPE))
      ! Take care: MAJ or  MA1 are valable only from 0 to 31 (cf ZCOUNT in av_pgd_param.F90).
      CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                             HPROGRAM,'MAJ','SEED_3_M: month of seeding','NAT',&
                             CFNAM_SEED_S3_M,CFTYP_SEED_S3_M,XUNIF_SEED_S3_M,DTV%XPAR_SEED_S3_M,DTV%LDATA_SEED_S3_M)
      !
      IF ( XUNIF_SEED_S3_D_C/=XUNDEF )  WHERE ( XUNIF_SEED_S3_D(:)==XUNDEF ) XUNIF_SEED_S3_D(:)=XUNIF_SEED_S3_D_C
      DO JVEG=1,NVEGTYPE
        IF ( XUNIF_SEED_S3_M(JVEG) /= XUNDEF .AND. XUNIF_SEED_S3_D(JVEG) /= XUNDEF) THEN
          IF ( XUNIF_SEED_S3_D(JVEG)<1 .OR. XUNIF_SEED_S3_D(JVEG)>INB_DM(INT(XUNIF_SEED_S3_M(JVEG))) ) THEN
            WRITE(ILUOUT,*) ' '
            WRITE(ILUOUT,*) '*************************************************************'
            WRITE(ILUOUT,*) '* Error in PGD field preparation for T_SEED_S3_D (saison 3):*'
            WRITE(ILUOUT,*) '*   The input value do not exist: <1 or >[monthDays].       *'
            WRITE(ILUOUT,*) '**************************************************************'
            WRITE(ILUOUT,*) ' '
            CALL ABOR1_SFX('PGD_ISBA_PAR: WRONG INPUT TSEED_S3 (XUNIF_SEED_S3_D)')
          ENDIF
        ELSEIF ( XUNIF_SEED_S3_M(JVEG) /= XUNDEF .OR. XUNIF_SEED_S3_D(JVEG) /= XUNDEF) THEN
          WRITE(ILUOUT,*) ' '
          WRITE(ILUOUT,*) '*****************************************************************'
          WRITE(ILUOUT,*) '* Error in PGD field preparation for T_SEED_S3_D or T_SEED_S3_M *'
          WRITE(ILUOUT,*) '*   The input value have to be define (or undef) for both       *'
          WRITE(ILUOUT,*) '*   together (XUNIF_SEED_S3_D and XUNIF_SEED_S3_M).             *'
          WRITE(ILUOUT,*) '*****************************************************************'
          WRITE(ILUOUT,*) ' '
          CALL ABOR1_SFX('PGD_ISBA_PAR: NOT CONSISTENT INPUT BETWEEN TSEED_S3 DAY AND  MONTH)')
        ENDIF
      ENDDO
      ALLOCATE(DTV%XPAR_SEED_S3_D(KDIM,NVEGTYPE))
      CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                             HPROGRAM,'MAJ','SEED_S3_D: day of seeding','NAT',&
                             CFNAM_SEED_S3_D,CFTYP_SEED_S3_D,XUNIF_SEED_S3_D,DTV%XPAR_SEED_S3_D,DTV%LDATA_SEED_S3_D)
      !
      ! Add random if a DELTA is imposed
      IF ( ANY(XUNIF_SEED_S3_D/=XUNDEF) .AND. XUNIF_SEED_S3_D_DELTA/=XUNDEF .AND. XUNIF_SEED_S3_D_DELTA/=0. &
           .AND. ALL(CFTYP_SEED_S3_D=='      ') .AND. ALL(CFTYP_SEED_S3_M=='      ') ) THEN
        !
        DO JVEG=1,NVEGTYPE
          IF ( XUNIF_SEED_S3_D(JVEG)/=XUNDEF .AND. DTV%LDATA_SEED_S3_D(JVEG) ) THEN ! Note: before we stop if we don't have XUNIF_SEED_M(JVEG)/=XUNDEF
            DO  JL= 1, SIZE(DTV%XPAR_SEED_S3_D,1)
              CALL RANDOM_NUMBER(RAND)
              !
              NRAND = INT( RAND * (XUNIF_SEED_S3_D_DELTA*2+1) - XUNIF_SEED_S3_D_DELTA )
              CALL ADD_DAYS( 1987 , INT(DTV%XPAR_SEED_S3_M(JL,JVEG)), INT(DTV%XPAR_SEED_S3_D(JL,JVEG)), NRAND, IMONTH, IDAY)
              DTV%XPAR_SEED_S3_M(JL,JVEG) = IMONTH
              DTV%XPAR_SEED_S3_D(JL,JVEG) = IDAY
              !
            ENDDO
          ENDIF
        ENDDO
        !
      ELSEIF ( XUNIF_SEED_S3_D_DELTA/=XUNDEF .AND. XUNIF_SEED_S3_D_DELTA/=0 .AND. ( ALL(XUNIF_SEED_S3_D==XUNDEF) .OR.       &
               ANY(CFTYP_SEED_S3_D/='      ') .OR. ANY(CFTYP_SEED_S3_M/='      ')) ) THEN
        CALL ABOR1_SFX('PGD_ISBA_PAR: IF XUNIF_SEED_S3_D_DELTA IS DEFINE XUNIF_SEED_S3_D/M(_C) HAVE TO BE DEFINE (AND NO MAP LINK)')
      ENDIF
      !
      ! * Treap
      ! -------
      !
      IF ( XUNIF_REAP_S3_M_C/=XUNDEF )  WHERE ( XUNIF_REAP_S3_M(:)==XUNDEF ) XUNIF_REAP_S3_M(:)=XUNIF_REAP_S3_M_C
      DO JVEG=1,NVEGTYPE
        IF (  XUNIF_REAP_S3_M(JVEG) /= XUNDEF .AND. ( XUNIF_REAP_S3_M(JVEG)<1 .OR. XUNIF_REAP_S3_M(JVEG)>12 ) ) THEN
          WRITE(ILUOUT,*) ' '
          WRITE(ILUOUT,*) '**************************************************************'
          WRITE(ILUOUT,*) '* Error in PGD field preparation for T_REAP_S3_M (saison 3): *'
          WRITE(ILUOUT,*) '*   The input value do not exist (XUNIF_REAP_S3_M)           *'
          WRITE(ILUOUT,*) '**************************************************************'
          WRITE(ILUOUT,*) ' '
          CALL ABOR1_SFX('PGD_ISBA_PAR: WRONG INPUT REAP MONTH FOR SAISON 3 (XUNIF_REAP_S3_M)')
        ENDIF
      ENDDO
      !
      ALLOCATE(DTV%XPAR_REAP_S3_M(KDIM,NVEGTYPE))
      ! Take care: MAJ or  MA1 are valable only from 0 to 31 (cf ZCOUNT in av_pgd_param.F90).
      CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                             HPROGRAM,'MAJ','REAP_S3_M: month of reaping','NAT',&
                             CFNAM_REAP_S3_M,CFTYP_REAP_S3_M,XUNIF_REAP_S3_M,DTV%XPAR_REAP_S3_M,DTV%LDATA_REAP_S3_M)
      !
      IF ( XUNIF_REAP_S3_D_C/=XUNDEF )  WHERE ( XUNIF_REAP_S3_D(:)==XUNDEF ) XUNIF_REAP_S3_D(:)=XUNIF_REAP_S3_D_C
      DO JVEG=1,NVEGTYPE
        IF ( XUNIF_REAP_S3_M(JVEG) /= XUNDEF .AND. XUNIF_REAP_S3_D(JVEG) /= XUNDEF) THEN
          IF ( XUNIF_REAP_S3_D(JVEG)<1 .OR. XUNIF_REAP_S3_D(JVEG)>INB_DM(INT(XUNIF_REAP_S3_M(JVEG))) ) THEN
            WRITE(ILUOUT,*) ' '
            WRITE(ILUOUT,*) '*************************************************************'
            WRITE(ILUOUT,*) '* Error in PGD field preparation for T_REAP_S3_D (saison 3):*'
            WRITE(ILUOUT,*) '*   The input value do not exist: <1 or >[monthDays].       *'
            WRITE(ILUOUT,*) '*************************************************************'
            WRITE(ILUOUT,*) ' '
            CALL ABOR1_SFX('PGD_ISBA_PAR: WRONG INPUT TREAP_S3 (XUNIF_REAP_S3_D)')
          ENDIF
        ELSEIF ( XUNIF_REAP_S3_M(JVEG) /= XUNDEF .OR. XUNIF_REAP_S3_D(JVEG) /= XUNDEF) THEN
          WRITE(ILUOUT,*) ' '
          WRITE(ILUOUT,*) '*****************************************************************'
          WRITE(ILUOUT,*) '* Error in PGD field preparation for T_REAP_S3_D or T_REAP_S3_M *'
          WRITE(ILUOUT,*) '*   The input value have to be define (or undef) for both       *'
          WRITE(ILUOUT,*) '*   together (XUNIF_REAP_S3_D and XUNIF_REAP_S3_M).             *'
          WRITE(ILUOUT,*) '*****************************************************************'
          WRITE(ILUOUT,*) ' '
          CALL ABOR1_SFX('PGD_ISBA_PAR: NOT CONSISTENT INPUT BETWEEN TREAP_S3 DAY AND  MONTH)')
        ENDIF
      ENDDO
      ALLOCATE(DTV%XPAR_REAP_S3_D(KDIM,NVEGTYPE))
      CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                             HPROGRAM,'MAJ','REAP_S3_D: day of reaping','NAT',&
                             CFNAM_REAP_S3_D,CFTYP_REAP_S3_D,XUNIF_REAP_S3_D,DTV%XPAR_REAP_S3_D,DTV%LDATA_REAP_S3_D)
      !
      IF ( ANY(XUNIF_REAP_S3_D/=XUNDEF) .AND. XUNIF_REAP_S3_D_DELTA/=XUNDEF .AND. XUNIF_REAP_S3_D_DELTA/=0. &
           .AND. ALL(CFTYP_REAP_S3_D=='      ') .AND. ALL(CFTYP_REAP_S3_M=='      ') ) THEN
        !
        DO JVEG=1,NVEGTYPE
          IF ( XUNIF_REAP_S3_D(JVEG)/=XUNDEF .AND. DTV%LDATA_REAP_S3_D(JVEG) ) THEN ! Note: before we stop if we don't have XUNIF_REAP_M(JVEG)/=XUNDEF
            DO  JL= 1, SIZE(DTV%XPAR_REAP_S3_D,1)
              CALL RANDOM_NUMBER(RAND)
              !
              NRAND = INT( RAND * (XUNIF_REAP_S3_D_DELTA*2+1) - XUNIF_REAP_S3_D_DELTA )
              CALL ADD_DAYS( 1987 , INT(DTV%XPAR_REAP_S3_M(JL,JVEG)), INT(DTV%XPAR_REAP_S3_D(JL,JVEG)), NRAND, IMONTH, IDAY)
              DTV%XPAR_REAP_S3_M(JL,JVEG) = IMONTH
              DTV%XPAR_REAP_S3_D(JL,JVEG) = IDAY
              !
            ENDDO
          ENDIF
        ENDDO
        !
      ELSEIF ( XUNIF_REAP_S3_D_DELTA/=XUNDEF .AND. XUNIF_REAP_S3_D_DELTA/=0 .AND. (ALL(XUNIF_REAP_S3_D==XUNDEF) .OR.       &
               ANY(CFTYP_REAP_S3_D/='      ') .OR. ANY(CFTYP_REAP_S3_M/='      ')) ) THEN
        CALL ABOR1_SFX('PGD_ISBA_PAR: IF XUNIF_REAP_S3_D_DELTA IS DEFINE XUNIF_REAP_S3_D/M(_C) HAVE TO BE DEFINE (AND NO MAP LINK)')
      ENDIF
      !
      IF (ALL(.NOT.DTV%LDATA_SEED_S3_M)) DEALLOCATE(DTV%XPAR_SEED_S3_M)
      IF (ALL(.NOT.DTV%LDATA_SEED_S3_D)) DEALLOCATE(DTV%XPAR_SEED_S3_D)
      IF (ALL(.NOT.DTV%LDATA_REAP_S3_M)) DEALLOCATE(DTV%XPAR_REAP_S3_M)
      IF (ALL(.NOT.DTV%LDATA_REAP_S3_D)) DEALLOCATE(DTV%XPAR_REAP_S3_D)
      !
      ! Test is there is some obvious missing value
      !
      DO JVEG = 1,NVEGTYPE
        IF ( ( DTV%LDATA_SEED_S3_M(JVEG) .AND. .NOT.DTV%LDATA_SEED_S3_D(JVEG) ) .OR. &
             ( DTV%LDATA_REAP_S3_M(JVEG) .AND. .NOT.DTV%LDATA_REAP_S3_D(JVEG) ) .OR. &
             ( DTV%LDATA_SEED_S3_M(JVEG) .AND. .NOT.DTV%LDATA_REAP_S3_M(JVEG) ) ) THEN
          CALL ABOR1_SFX('PGD_ISBA_PAR: FOR A VEGTYPE, IF THERE IS VALUES FOR ONE DATE, ALL ASSOCIATED DATE HAVE TO BE GIVEN (S-3)')
        ENDIF
      ENDDO
      !
    ELSE
      DTV%LDATA_SEED_S3_M(:) = .FALSE.
      DTV%LDATA_SEED_S3_D(:) = .FALSE.
      DTV%LDATA_REAP_S3_M(:) = .FALSE.
      DTV%LDATA_REAP_S3_D(:) = .FALSE.
    ENDIF
  ELSE
    DTV%LDATA_SEED_S2_M(:) = .FALSE.
    DTV%LDATA_SEED_S2_D(:) = .FALSE.
    DTV%LDATA_REAP_S2_M(:) = .FALSE.
    DTV%LDATA_REAP_S2_D(:) = .FALSE.
    DTV%LDATA_SEED_S3_M(:) = .FALSE.
    DTV%LDATA_SEED_S3_D(:) = .FALSE.
    DTV%LDATA_REAP_S3_M(:) = .FALSE.
    DTV%LDATA_REAP_S3_D(:) = .FALSE.
    !
  ENDIF
  !
  ! If LMULTI_SEASON we have to check that is not overlaping between seasons
  IF ( LMULTI_SEASON ) THEN
    !
    IF ( (XUNIF_SEED_D_DELTA   /=XUNDEF .AND. XUNIF_SEED_D_DELTA   /=0.) .OR. & 
         (XUNIF_REAP_D_DELTA   /=XUNDEF .AND. XUNIF_REAP_D_DELTA   /=0.) .OR. &
         (XUNIF_SEED_S2_D_DELTA/=XUNDEF .AND. XUNIF_SEED_S2_D_DELTA/=0.) .OR. &
         (XUNIF_REAP_S2_D_DELTA/=XUNDEF .AND. XUNIF_REAP_S2_D_DELTA/=0.) .OR. &
         (XUNIF_SEED_S3_D_DELTA/=XUNDEF .AND. XUNIF_SEED_S3_D_DELTA/=0.) .OR. &    
         (XUNIF_REAP_S3_D_DELTA/=XUNDEF .AND. XUNIF_REAP_S3_D_DELTA/=0.) ) THEN
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '*************************************************************'
      WRITE(ILUOUT,*) '* CAUTION: If you activated a random value for agricultural *'
      WRITE(ILUOUT,*) '* practice dates with multiple season, you have to be sure  *'
      WRITE(ILUOUT,*) '* that will not induce overloaping for different season!    *'
      WRITE(ILUOUT,*) '*************************************************************'
      WRITE(ILUOUT,*) ' '
    ENDIF
    !
    DO JVEG = 1, NVEGTYPE
      IF ( DTV%LDATA_SEED_M(JVEG) .AND. DTV%LDATA_SEED_S2_M(JVEG) ) THEN                                                             ! If there is a least two season define on one point
      DO JL = 1, KDIM
        IF ( DTV%XPAR_SEED_S2_M(JL,JVEG) /= 0. .OR. DTV%XPAR_SEED_S2_M(JL,JVEG) /= XUNDEF ) THEN                                     ! If there is a second season in this point
          IF ( DTV%XPAR_REAP_M(JL,JVEG) <  DTV%XPAR_SEED_M(JL,JVEG) .OR.                                                           & 
              (DTV%XPAR_REAP_M(JL,JVEG) == DTV%XPAR_SEED_M(JL,JVEG) .AND. DTV%XPAR_REAP_D(JL,JVEG) <=DTV%XPAR_SEED_D(JL,JVEG))) THEN ! T reap of the first season have to be after the t seed (of the first seson)
            CALL ABOR1_SFX('PGD_ISBA_PAR: WITH AT LEAST 2 SEASONS, TREAP HAVE TO BE LATER THAN TSEED (SEASON 1)')
          ELSEIF ( DTV%XPAR_SEED_S2_M(JL,JVEG) <  DTV%XPAR_REAP_M(JL,JVEG) .OR.                                                    &
                  (DTV%XPAR_SEED_S2_M(JL,JVEG) == DTV%XPAR_REAP_M(JL,JVEG)                                                         &
                                                                 .AND.DTV%XPAR_SEED_S2_D(JL,JVEG) < DTV%XPAR_REAP_D(JL,JVEG)) ) THEN ! T seed of the season 2 have to be later than T reap (of the season 1)
            CALL ABOR1_SFX('PGD_ISBA_PAR: IF IS DEFINE, TSEED_S2 OF THE SAISON 2 HAVE TO BE LATER THAN TSEED OF THE SEASON 1')
          ENDIF
          IF ( DTV%LDATA_SEED_S3_M(JVEG) .AND. ( DTV%XPAR_SEED_S3_M(JL,JVEG)/=0. .OR. DTV%XPAR_SEED_S3_M(JL,JVEG)/=XUNDEF ) ) THEN   ! There is a least one point with 3 seasons and that we are on one of these point
            IF ( DTV%XPAR_REAP_S2_M(JL,JVEG) <  DTV%XPAR_SEED_S2_M(JL,JVEG) .OR.                                                   &
                (DTV%XPAR_REAP_S2_M(JL,JVEG) == DTV%XPAR_SEED_S2_M(JL,JVEG)                                                        &
                                                            .AND. DTV%XPAR_REAP_S2_D(JL,JVEG) <= DTV%XPAR_SEED_S2_D(JL,JVEG)) ) THEN ! T reap of the saison 2 have to be later than T seed of the season 2
              CALL ABOR1_SFX('PGD_ISBA_PAR: WITH AT LEAST 3 SEASONS, TREAP_S2 HAVE TO BE LATER THAN TSEED_S2 (SEASON 2)')
            ELSEIF ( DTV%XPAR_SEED_S3_M(JL,JVEG) <  DTV%XPAR_REAP_S2_M(JL,JVEG) .OR.                                               &
                    (DTV%XPAR_SEED_S3_M(JL,JVEG) == DTV%XPAR_REAP_S2_M(JL,JVEG)                                                    &
                                                             .AND. DTV%XPAR_SEED_S3_D(JL,JVEG) < DTV%XPAR_REAP_S2_D(JL,JVEG)) ) THEN ! T seed of the season 3 have to be later than T reap of the season 2
              CALL ABOR1_SFX('PGD_ISBA_PAR: IF IS DEFINE, TSEED_S3 OF THE SAISON 3 HAVE TO BE LATER THAN TREAP_S2 OF THE SEASON 2')
            ELSEIF ( ( DTV%XPAR_REAP_S3_M(JL,JVEG) <  DTV%XPAR_SEED_S3_M(JL,JVEG) .OR.                                             &
                      (DTV%XPAR_REAP_S3_M(JL,JVEG) == DTV%XPAR_SEED_S3_M(JL,JVEG)                                                  &
                                                         .AND. DTV%XPAR_REAP_S3_D(JL,JVEG) <=DTV%XPAR_SEED_S3_D(JL,JVEG)) ) .AND.  &
                     ( DTV%XPAR_REAP_S3_M(JL,JVEG) >  DTV%XPAR_SEED_M(JL,JVEG) .OR.                                                &
                      (DTV%XPAR_REAP_S3_M(JL,JVEG) == DTV%XPAR_SEED_M(JL,JVEG)                                                     &
                                                              .AND. DTV%XPAR_REAP_S3_D(JL,JVEG) > DTV%XPAR_SEED_D(JL,JVEG)) ) ) THEN ! If there is a "winter crop" in the last season, T seed of the season 1 have to be later than T reap of the season 3
              CALL ABOR1_SFX('PGD_ISBA_PAR: IF THERE IS A "WINTER" SAISON 3, TREAP_S3 (S-3) HAVE TO BE BEFORE THAN TSEED (S-1)')
            ENDIF
          ELSEIF ( ( DTV%XPAR_REAP_S2_M(JL,JVEG) <  DTV%XPAR_SEED_S2_M(JL,JVEG) .OR.                                               & ! If there is only two season
                    (DTV%XPAR_REAP_S2_M(JL,JVEG) == DTV%XPAR_SEED_S2_M(JL,JVEG)                                                    &
                                                         .AND. DTV%XPAR_REAP_S2_D(JL,JVEG) <= DTV%XPAR_SEED_S2_D(JL,JVEG)) ) .AND. &
                   ( DTV%XPAR_REAP_S2_M(JL,JVEG) >  DTV%XPAR_SEED_M(JL,JVEG) .OR.                                                  &
                    (DTV%XPAR_REAP_S2_M(JL,JVEG) == DTV%XPAR_SEED_M(JL,JVEG)                                                       &
                                                              .AND. DTV%XPAR_REAP_S2_D(JL,JVEG) > DTV%XPAR_SEED_D(JL,JVEG)) ) ) THEN  ! If there is a "winter crop" in the last season, T seed of the season 1 have to be later than T reap of the season s2
              CALL ABOR1_SFX('PGD_ISBA_PAR: IF THERE IS A "WINTER" SAISON 2, TREAP_S2 (S-2) HAVE TO BE BEFORE THAN TSEED (S-1)')
          ENDIF
        ENDIF
      ENDDO
      ENDIF
    ENDDO
    !
  ENDIF
  !
ELSE
  DTV%LDATA_SEED_M(:) = .FALSE.
  DTV%LDATA_SEED_D(:) = .FALSE.
  DTV%LDATA_REAP_M(:) = .FALSE.
  DTV%LDATA_REAP_D(:) = .FALSE.
  DTV%LDATA_SEED_S2_M(:) = .FALSE.
  DTV%LDATA_SEED_S2_D(:) = .FALSE.
  DTV%LDATA_REAP_S2_M(:) = .FALSE.
  DTV%LDATA_REAP_S2_D(:) = .FALSE.
  DTV%LDATA_SEED_S3_M(:) = .FALSE.
  DTV%LDATA_SEED_S3_D(:) = .FALSE.
  DTV%LDATA_REAP_S3_M(:) = .FALSE.
  DTV%LDATA_REAP_S3_D(:) = .FALSE.
ENDIF
!
! Without ECOCLIMAP-SG, there is no duplication of vegetation type. So:
!      - the vegetation type non irrigated or without agricultural practices are clean for associate parameters
!      - the associated key variables are clean
IF ( .NOT.U%LECOSG .AND. (LIRRIGMODE .OR. LAGRIP) ) THEN
  !
  DO JVEG = 1, NVEGTYPE
    IF ( .NOT.ANY(DTV%NPAR_VEG_IRR_USE(:)==JVEG) ) THEN
      DTV%LDATA_IRRIGFRAC(JVEG)     = .FALSE.
      IF (LIRRIGMODE) THEN
        DTV%LDATA_IRRIGTYPE(JVEG)   = .FALSE.
        DTV%LDATA_IRRIGFREQ(JVEG)   = .FALSE.
        DTV%LDATA_IRRIGTIME(JVEG)   = .FALSE.
        DO JL=1,DTV%NTIME
          DTV%LDATA_WATSUP     ((JL-1)*NVEGTYPE+JVEG) = .FALSE.
          DTV%LDATA_F2THRESHOLD((JL-1)*NVEGTYPE+JVEG) = .FALSE.
        ENDDO
      ENDIF
      DTV%LDATA_SEED_M(JVEG) = .FALSE.
      DTV%LDATA_SEED_D(JVEG) = .FALSE.
      DTV%LDATA_REAP_M(JVEG) = .FALSE.
      DTV%LDATA_REAP_D(JVEG) = .FALSE.
    ENDIF
  ENDDO
  !
  NVEG_IRR=0
  DEALLOCATE(DTV%NPAR_VEG_IRR_USE)
  ALLOCATE(DTV%NPAR_VEG_IRR_USE(1))
  DTV%NPAR_VEG_IRR_USE=(/0./)
  !
ENDIF
!
IF (IO%CPHOTO/='NON') THEN
  !
  ALLOCATE(DTV%XPAR_RE25 (KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','RE25: ecosystem respiration','NAT',CFNAM_RE25,   &
                         CFTYP_RE25,XUNIF_RE25,DTV%XPAR_RE25,DTV%LDATA_RE25)  
  IF (ALL(.NOT.DTV%LDATA_RE25)) DEALLOCATE(DTV%XPAR_RE25)  
  !
  ALLOCATE(DTV%XPAR_LAIMIN(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','LAIMIN: minimum LAI','NAT',CFNAM_LAIMIN,   &
                         CFTYP_LAIMIN,XUNIF_LAIMIN,DTV%XPAR_LAIMIN,DTV%LDATA_LAIMIN)  
  IF (ALL(.NOT.DTV%LDATA_LAIMIN)) DEALLOCATE(DTV%XPAR_LAIMIN)          
  !
  ALLOCATE(DTV%XPAR_SEFOLD(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','SEFOLD: e-folding time for senescence','NAT',CFNAM_SEFOLD,   &
                         CFTYP_SEFOLD,XUNIF_SEFOLD,DTV%XPAR_SEFOLD,DTV%LDATA_SEFOLD)  
  IF (ALL(.NOT.DTV%LDATA_SEFOLD)) DEALLOCATE(DTV%XPAR_SEFOLD)
  !  
  ALLOCATE(DTV%XPAR_GMES(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','GMES: mesophyl conductance','NAT',CFNAM_GMES,   &
                         CFTYP_GMES,XUNIF_GMES,DTV%XPAR_GMES,DTV%LDATA_GMES)
  IF (ALL(.NOT.DTV%LDATA_GMES)) DEALLOCATE(DTV%XPAR_GMES)
  !
  ALLOCATE(DTV%XPAR_GC(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','GC: cuticular conductance','NAT',CFNAM_GC,   &
                         CFTYP_GC,XUNIF_GC,DTV%XPAR_GC,DTV%LDATA_GC)  
  IF (ALL(.NOT.DTV%LDATA_GC)) DEALLOCATE(DTV%XPAR_GC)
  !
  IF (.NOT.IO%LECOCLIMAP .AND. .NOT.(ANY(DTV%LDATA_H_TREE).AND.ANY(DTV%LDATA_RE25).AND.&
          ANY(DTV%LDATA_LAIMIN).AND.ANY(DTV%LDATA_BSLAI).AND.ANY(DTV%LDATA_SEFOLD).AND.&
          ANY(DTV%LDATA_GMES).AND.ANY(DTV%LDATA_GC))) THEN
    !
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) '* Error in PGD field preparation of AGS fields            *'
    WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
    IF (ALL(.NOT.DTV%LDATA_H_TREE )) WRITE(ILUOUT,*) '* for H_TREE                      *'
    IF (ALL(.NOT.DTV%LDATA_RE25   )) WRITE(ILUOUT,*) '* for RE25                        *'    
    IF (ALL(.NOT.DTV%LDATA_LAIMIN )) WRITE(ILUOUT,*) '* for LAIMIN                      *'    
    IF (ALL(.NOT.DTV%LDATA_BSLAI  )) WRITE(ILUOUT,*) '* for BSLAI                       *'
    IF (ALL(.NOT.DTV%LDATA_SEFOLD )) WRITE(ILUOUT,*) '* for SEFOLD                      *'
    IF (ALL(.NOT.DTV%LDATA_GMES   )) WRITE(ILUOUT,*) '* for GMES                        *'
    IF (ALL(.NOT.DTV%LDATA_GC     )) WRITE(ILUOUT,*) '* for GC                          *'
    WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR AGS PARAMETERS')
    !
  ENDIF
  !  
  !--------------------------------------AGS Stress parameters----------------------------
  !  
  ALLOCATE(DTV%XPAR_F2I(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','F2I: critical normalized soil water content (stress)','NAT',CFNAM_F2I,   &
                         CFTYP_F2I,XUNIF_F2I,DTV%XPAR_F2I,DTV%LDATA_F2I)
  IF (ALL(.NOT.DTV%LDATA_F2I)) DEALLOCATE(DTV%XPAR_F2I)
  !
  ALLOCATE(DTV%XPAR_DMAX(KDIM,NVEGTYPE))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                         HPROGRAM,'ARI','DMAX: maximum air saturation deficit','NAT',CFNAM_DMAX,   &
                         CFTYP_DMAX,XUNIF_DMAX,DTV%XPAR_DMAX,DTV%LDATA_DMAX)  
  IF (ALL(.NOT.DTV%LDATA_DMAX)) DEALLOCATE(DTV%XPAR_DMAX)
  !
  ALLOCATE(DTV%LPAR_STRESS(KDIM,NVEGTYPE))
  DO JVEG=1,NVEGTYPE
    GPAR_STRESS = LUNIF_STRESS(JVEG)
    IF (XSTRESS(JVEG)<1.) GPAR_STRESS = .FALSE.
    IF (XSTRESS(JVEG)==1. .AND. .NOT.GPAR_STRESS) DTV%LDATA_STRESS=.TRUE.
    DTV%LPAR_STRESS(:,JVEG) = GPAR_STRESS
  ENDDO
  IF (ALL(.NOT.DTV%LDATA_STRESS)) DEALLOCATE(DTV%LPAR_STRESS)
  !
  IF (.NOT.IO%LECOCLIMAP .AND. .NOT.(ANY(DTV%LDATA_F2I).AND.ANY(DTV%LDATA_DMAX))) THEN
    !
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) '* Error in PGD field preparation of AGS Stress fields     *'
    WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
    IF (ALL(.NOT.DTV%LDATA_F2I  )) WRITE(ILUOUT,*) '* for F2I                           *'
    IF (ALL(.NOT.DTV%LDATA_DMAX )) WRITE(ILUOUT,*) '* for DMAX                          *'
    WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR AGS STRESS PARAMETERS')
    !
  ENDIF
  !    
  !--------------------------------------AGS Nitrogen parameters----------------------------
  !  
  IF (IO%CPHOTO=='NIT' .OR. IO%CPHOTO=='NCB') THEN
    !
    ALLOCATE(DTV%XPAR_CE_NITRO(KDIM,NVEGTYPE))
    CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                           HPROGRAM,'ARI','CE_NITRO: leaf area ratio sensitivity to nitrogen ccion','NAT',&
                           CFNAM_CE_NITRO, CFTYP_CE_NITRO,XUNIF_CE_NITRO,DTV%XPAR_CE_NITRO,DTV%LDATA_CE_NITRO)  
    IF (ALL(.NOT.DTV%LDATA_CE_NITRO)) DEALLOCATE(DTV%XPAR_CE_NITRO)
    !
    ALLOCATE(DTV%XPAR_CF_NITRO(KDIM,NVEGTYPE))
    CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                           HPROGRAM,'ARI','CF_NITRO: lethal minimum value of leaf area ratio','NAT',&
                           CFNAM_CF_NITRO,CFTYP_CF_NITRO,XUNIF_CF_NITRO,DTV%XPAR_CF_NITRO,DTV%LDATA_CF_NITRO)
    IF (ALL(.NOT.DTV%LDATA_CF_NITRO)) DEALLOCATE(DTV%XPAR_CF_NITRO)
    !
    ALLOCATE(DTV%XPAR_CNA_NITRO(KDIM,NVEGTYPE))
    CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS, DTV%XPAR_VEGTYPE, &
                           HPROGRAM,'ARI','CNA_NITRO: nitrogen ccion of active biomass','NAT',&
                           CFNAM_CNA_NITRO,CFTYP_CNA_NITRO,XUNIF_CNA_NITRO,DTV%XPAR_CNA_NITRO,DTV%LDATA_CNA_NITRO)
    IF (ALL(.NOT.DTV%LDATA_CNA_NITRO)) DEALLOCATE(DTV%XPAR_CNA_NITRO)
    !
    IF (.NOT.IO%LECOCLIMAP .AND. &
         .NOT.(ANY(DTV%LDATA_CE_NITRO).AND.ANY(DTV%LDATA_CF_NITRO).AND.ANY(DTV%LDATA_CNA_NITRO))) THEN
      !
      WRITE(ILUOUT,*) ' '
      WRITE(ILUOUT,*) '***********************************************************'
      WRITE(ILUOUT,*) '* Error in PGD field preparation of AGS Nitrogen fields   *'
      WRITE(ILUOUT,*) '* There is no prescribed value and no input file :        *'
      IF (ALL(.NOT.DTV%LDATA_CE_NITRO  )) WRITE(ILUOUT,*) '* for CE_NITRO                 *'
      IF (ALL(.NOT.DTV%LDATA_CF_NITRO  )) WRITE(ILUOUT,*) '* for CF_NITRO                 *'
      IF (ALL(.NOT.DTV%LDATA_CNA_NITRO )) WRITE(ILUOUT,*) '* for CNA_NITRO                *'
      WRITE(ILUOUT,*) '* Without ECOCLIMAP, these fields must be prescribed      *'
      WRITE(ILUOUT,*) '***********************************************************'
      WRITE(ILUOUT,*) ' '
      CALL ABOR1_SFX('PGD_ISBA_PAR: NO PRESCRIBED VALUE NOR INPUT FILE FOR AGS NITROGEN PARAMETERS')
      !
    ENDIF
    !    
  ENDIF
  !
ENDIF
!
!--------------------------------hydrological fields-----------------------------------
!
ALLOCATE(DTV%XPAR_CONDSAT(KDIM,IO%NGROUND_LAYER))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS,  &
                         HPROGRAM,'ARI','CONDSAT: ','NAT',CFNAM_CONDSAT,CFTYP_CONDSAT,&
                         XUNIF_CONDSAT,DTV%XPAR_CONDSAT,DTV%LDATA_CONDSAT)
IF (.NOT.DTV%LDATA_CONDSAT) DEALLOCATE(DTV%XPAR_CONDSAT)
!
ALLOCATE(DTV%XPAR_MPOTSAT(KDIM,IO%NGROUND_LAYER))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS,  &
                         HPROGRAM,'ARI','MPOTSAT: ','NAT',CFNAM_MPOTSAT,CFTYP_MPOTSAT,&
                         XUNIF_MPOTSAT,DTV%XPAR_MPOTSAT,DTV%LDATA_MPOTSAT)
IF (.NOT.DTV%LDATA_MPOTSAT) DEALLOCATE(DTV%XPAR_MPOTSAT)
!
ALLOCATE(DTV%XPAR_BCOEF(KDIM,IO%NGROUND_LAYER))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS,  &
                         HPROGRAM,'ARI','BCOEF: ','NAT',CFNAM_BCOEF,CFTYP_BCOEF,&
                         XUNIF_BCOEF,DTV%XPAR_BCOEF,DTV%LDATA_BCOEF)
IF (.NOT.DTV%LDATA_BCOEF) DEALLOCATE(DTV%XPAR_BCOEF)
!
ALLOCATE(DTV%XPAR_WWILT(KDIM,IO%NGROUND_LAYER))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS,  &
                         HPROGRAM,'ARI','WWILT: ','NAT',CFNAM_WWILT,CFTYP_WWILT,&
                         XUNIF_WWILT,DTV%XPAR_WWILT,DTV%LDATA_WWILT)
IF (.NOT.DTV%LDATA_WWILT) DEALLOCATE(DTV%XPAR_WWILT)
!
ALLOCATE(DTV%XPAR_WFC(KDIM,IO%NGROUND_LAYER))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS,  &
                         HPROGRAM,'ARI','WFC: ','NAT',CFNAM_WFC,CFTYP_WFC,&
                         XUNIF_WFC,DTV%XPAR_WFC,DTV%LDATA_WFC)
IF (.NOT.DTV%LDATA_WFC) DEALLOCATE(DTV%XPAR_WFC)
!
ALLOCATE(DTV%XPAR_WSAT(KDIM,IO%NGROUND_LAYER))
  CALL INI_VAR_FROM_DATA(DTCO, UG, U, USS,  &
                         HPROGRAM,'ARI','WSAT: ','NAT',CFNAM_WSAT,CFTYP_WSAT,&
                         XUNIF_WSAT,DTV%XPAR_WSAT,DTV%LDATA_WSAT)
IF (.NOT.DTV%LDATA_WSAT) DEALLOCATE(DTV%XPAR_WSAT)
!
!----------------------------------------------------------------------------------------
!
IF (IO%LECOCLIMAP .AND. DTV%LDATA_VEGTYPE) THEN
  !  
  CALL EXTRAPOL_FIELDS(DTCO, DTV, KDIM, IO, S, UG, U, HPROGRAM,ILUOUT)
  !
ENDIF
!

!----------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_ISBA_PAR',1,ZHOOK_HANDLE)
!
END SUBROUTINE PGD_ISBA_PAR
