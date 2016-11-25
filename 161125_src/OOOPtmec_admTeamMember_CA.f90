! This file is part of:
! MPMD with Coarray Fortran Load Balancing – Example Program
! copyright 2016 by Michael Siehl
! www.mpmd-with-coarray-fortran.de
! http://www.mpmd-with-coarray-fortran.de/MPMD_Load_Balancing_example.pdf

module OOOPtmec_admTeamMember_CA
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPtmec
!********************************************************
! Abstract Data Type (ADT):         OOOPtmec_adtTeamMember_CA
! Abstract Data Type Module (adm):  OOOPtmec_admTeamMember_CA.f90
!********************************************************
! Purpose:                    TeamMember_CA-Object
! Language:                   mainly Fortran 95 with Fortran 2008 coarrays
! Programmer:                 Michael Siehl
! Date:                       January 2016
!********************************************************
! Naming Conventions:
!
!  for scalar members:
!                             m: ADT member
!                             S: property set, G: property get,
!                             CopyImgToImg: copy an ADT member image to image
!  for array members:
!                             A: array
!                             mA: ADT array member
!                             SA: set array property, GA: get array property,
!                             CopyAImgToImg: copy an ADT array member image to image
!
!  for elements of array members:
!                             SAElement: set only one array element property
!                             GAElement: get only one array element property
!                             CopyAElementImgToImg: copy only one element of an ADT array member image to image
!
!                             99: signals a static array member which has an upper array bound
!                                 larger than necessary; the upper bound is given by a global parameter
!
!  other naming conventions:
!                             _CA: coarray routine / coarray declaration
!                             _SYNC_: synchronization routine
!                             CopyCoarrayObjImgToImg: copy a coarray ADT object image to image
!
!                             DC: deep copy routine
!                             Enum: enumeration
!
!                             OO: public (outer) scope (the two leading namespace letters)
!                             II: private (inner) scope
!                             UU: sub-object
!********************************************************
!___________________________________________________________

use OOOGglob_Globals
use OOOEerro_admError
!___________________________________________________________

implicit none
!___________________________________________________________

private
!___________________________________________________________
!
!*******************************
!****  Local ADT Routines:  ****
!*******************************
!***
! access routines for scalar
! and static array members:
!***
! access routines for
! dynamic array members:

!***
! local ADT management:
public :: OOOPtmec_StructureConstructor
!__________________________________________________________
!
!*********************************
!****  Coarray ADT Routines:  ****
!*********************************
!***
! access routines for scalar
! and static array members:

!***
! access routines for
! dynamic array members:

!***
! coarray ADT management:

!___________________________________________________________
!
!***************************
!****  Error Handling:  ****
!***************************
!***
! local ADT:
private :: IItmec_ErrorHandler
!***
! coarray ADT:
public :: IItmec_ImageNumberBoundError_CA
!___________________________________________________________
!
!************************
!****  Enumerations:  ***
!************************
!
!___________________________________________________________
!
!********************************************************
!***  Abstract Data Type Specification: *****************
!********************************************************
type, public :: OOOPtmec_adtTeamMember_CA
  private
  !*****
  type (OOOEerroc_colError) :: m_UUerrocError ! error collection
  !
end type OOOPtmec_adtTeamMember_CA
!___________________________________________________________
!
!****************************************************
!***  Corresponding Coarray Declarations:  **********
!****************************************************
!***
type (OOOPtmec_adtTeamMember_CA), public, codimension[*], volatile, save :: OOOPtmecTeamMember_CA_1
!___________________________________________________________





contains


!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Local ADT Routines: ***********************
!**********************************************************

!*******************************
! access routines for scalar   *
! and static array members:    *
!*******************************
!__________________________________________________________




!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Local ADT Routines: ***********************
!**********************************************************

!***************************
! access routines for      *
! dynamic array members:   *
!***************************

!___________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Local ADT Routines: ***********************
!**********************************************************

!*************************
! local ADT management:  *
!*************************



!
subroutine OOOPtmec_StructureConstructor (Object)
  ! structure constructor
  type (OOOPtmec_adtTeamMember_CA), intent (inout) :: Object
  !
                                                                call OOOGglob_subSetProcedures ("OOOPtmec_StructureConstructor")
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPtmec_StructureConstructor
!___________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Coarray ADT Routines: *********************
!**********************************************************

!*******************************
! access routines for scalar   *
! and static array members:    *
!*******************************
!__________________________________________________________






!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Coarray ADT Routines:  ********************
!**********************************************************

!***************************
! access routines for      *
! dynamic array members:   *
!***************************

!___________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Coarray ADT Routines:  ********************
!**********************************************************

!**************************
! coarray ADT management: *
!**************************
!___________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Error Handling:   *************************
!**********************************************************
!__________________________________________________________
!
!Private
subroutine IItmec_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT and CA routines
  type(OOOPtmec_adtTeamMember_CA), intent(inout) :: Object
  character(kind=1, len=*), intent(in) :: chrErrorDescription
  integer(OOOGglob_kint), intent(in) :: intErrorType ! 1=warning, 2=Severe System error
  integer(OOOGglob_kint), intent(in) :: intErrorNumber ! Run Time error Number (e.g. status)
  call OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
end subroutine IItmec_ErrorHandler
!__________________________________________________________
!
!Private
logical(OOOGglob_klog) function IItmec_ImageNumberBoundError_CA (Object_CA, intImageNumber)
  ! error handling routine
  ! checks if the image number does exist
  type(OOOPtmec_adtTeamMember_CA), codimension[*], volatile, intent(inout) :: Object_CA
  integer(OOOGglob_kint), intent(in) :: intImageNumber
  !
  IItmec_ImageNumberBoundError_CA = .false.
  !
  if (intImageNumber .lt. 1) then ! image number is too small
    IItmec_ImageNumberBoundError_CA = .true.
  end if
  !
  if (intImageNumber .gt. num_images()) then ! image number is too large
    IItmec_ImageNumberBoundError_CA = .true.
  end if
  !
end function IItmec_ImageNumberBoundError_CA
!__________________________________________________________

end module OOOPtmec_admTeamMember_CA
