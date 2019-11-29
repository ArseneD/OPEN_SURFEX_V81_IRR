!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############
      MODULE MODD_AGRI
!     ###############
!
!!****  *MODD_AGRI* - declaration of agricultural practices constants

!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare  the 
!     typical constants for agricultural practices
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!          
!!    AUTHOR
!!    ------
!!      P. LE MOIGNE *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2006
!!      J.Etchanchu 01/2018 Adding the key for the irrigation module choice
!!      A. Druel    02/2019 Adding new key for the irrigation (new version with ECOCLIMAP-SG)
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
IMPLICIT NONE 
!
LOGICAL                             :: LAGRIP  
                                        ! General switch for agricultural practices
                                        ! (seeding and irrigation)
!
LOGICAL                             :: LIRRIGMODE
                                        ! General switch for the irrigation module choicie
LOGICAL                             :: LMULTI_SEASON
                                        ! Multi season for agricultural pratices (OAGRIP OR IRRIGATION)
INTEGER, PARAMETER                  :: JPSTAGE = 4   
                                        ! Number of stages for Irrigation
!
REAL, DIMENSION(JPSTAGE)            :: XTHRESHOLD
                                        ! Threshold on f2 for irrigation
!
REAL, PARAMETER, DIMENSION(JPSTAGE) :: XTHRESHOLD_DEFAULT=(/0.70,0.55,0.40,0.25/)
                                        ! Threshold on f2 for irrigation with PM06i (default)
!
INTEGER                             :: NVEG_IRR
                                        ! Number of vegtype irrigated
INTEGER, PARAMETER                  :: NVEG_IRR_DEFAULT = 6 
                                        ! Number of irrigtype: DEFAULT
INTEGER, PARAMETER, DIMENSION(NVEG_IRR_DEFAULT) :: NVEG_IRR_USE_DEFAULT=(/5,7,12,16,17,18/)
                                        ! Correspondance of NVEG_IRR to NVEGTYPE: which VEGTYPE is duplicated to be irrigated: DEFAULT
INTEGER                             :: NPATCH_TREE = 0
                                        ! number of the tree patch use (corresponding patch without irrigation)
INTEGER, PARAMETER                  :: NIRR_TYPE = 3
                                        ! Number of irrigtype (with 3, 1: spraying, 2: dripping, 3: flooding)
INTEGER, PARAMETER, DIMENSION(NIRR_TYPE)        :: IRRIGFREQ_CTYPE_DEFAULT=(/604800,0,604800/)
                                        ! irrig freq max in s for 1: spraying, 2: dripping, 3: flooding
INTEGER, PARAMETER, DIMENSION(4)                :: DATES_IRRIG_DEFAULT=(/3,15,8,31/)
                                        ! Defaults irrigated date: (/ seeding month, seeding day, reaping month, reaping day /)
INTEGER                             :: NIRR_STOP_BTR = 14
                                        ! Number of days that the irrigation stop before the reap (TREAP)
!
END MODULE MODD_AGRI
