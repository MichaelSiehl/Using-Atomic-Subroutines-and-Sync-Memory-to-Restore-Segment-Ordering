! This file is part of:
! MPMD with Coarray Fortran Load Balancing – Example Program
! copyright 2016 by Michael Siehl
! www.mpmd-with-coarray-fortran.de
! http://www.mpmd-with-coarray-fortran.de/MPMD_Load_Balancing_example.pdf

module OOOPtemc_admTeamManager_CA
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPtemc
!********************************************************
! Abstract Data Type (ADT):         OOOPtemc_adtTeamManager_CA
! Abstract Data Type Module (adm):  OOOPtemc_admTeamManager_CA.f90
!********************************************************
! Purpose:                    TeamManager_CA-Object
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
public :: OOOPtemc_StructureConstructor
!__________________________________________________________
!
!*********************************
!****  Coarray ADT Routines:  ****
!*********************************
!***
! access routines for scalar
! and static array members:
public :: OOOPtemcS_chrTeamMembersFileName_CA, OOOPtemcG_chrTeamMembersFileName_CA
public :: OOOPtemcS_intNumberOfTeamMembers_CA, OOOPtemcG_intNumberOfTeamMembers_CA
public :: OOOPtemcSA_intTeamMemberImages99_CA, OOOPtemcGA_intTeamMemberImages99_CA
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
private :: IItemc_ErrorHandler
!***
! coarray ADT:
public :: IItemc_ImageNumberBoundError_CA
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
type, public :: OOOPtemc_adtTeamManager_CA
  private
  !*****
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: m_chrTeamMembersFileName
  !*****
  integer(OOOGglob_kint) :: m_intNumberOfTeamMembers = 0
  !*****
  integer(OOOGglob_kint), dimension (1:OOOGglob_TeamMembers_UpperBound) :: mA_intTeamMemberImages99
  !*****
  type (OOOEerroc_colError) :: m_UUerrocError ! error collection
  !
end type OOOPtemc_adtTeamManager_CA
!___________________________________________________________
!
!****************************************************
!***  Corresponding Coarray Declarations:  **********
!****************************************************
!***
type (OOOPtemc_adtTeamManager_CA), public, codimension[*], volatile, save :: OOOPtemcTeamManager_CA_1
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
subroutine OOOPtemc_StructureConstructor (Object)
  ! structure constructor
  type (OOOPtemc_adtTeamManager_CA), intent (inout) :: Object
  !
                                                                call OOOGglob_subSetProcedures ("OOOPtemc_StructureConstructor")
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPtemc_StructureConstructor
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
!
subroutine OOOPtemcS_chrTeamMembersFileName_CA (Object_CA, chrTeamMembersFileName, intImageNumber)
  type (OOOPtemc_adtTeamManager_CA), codimension[*], volatile, intent (inout) :: Object_CA
  character(kind=OOOGglob_kcha,len=*), intent (in) :: chrTeamMembersFileName
  integer(OOOGglob_kint), intent (in) :: intImageNumber
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                  ("OOOPtemcS_chrTeamMembersFileName_CA")
                                                                !
                                                                if (IItemc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) then
                                                                  call IItemc_ErrorHandler (Object_CA, &
                                                                   "no valid image number", &
                                                                     OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
  Object_CA[intImageNumber] % m_chrTeamMembersFileName = chrTeamMembersFileName
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPtemcS_chrTeamMembersFileName_CA
!**********
subroutine OOOPtemcG_chrTeamMembersFileName_CA (Object_CA, chrTeamMembersFileName, intImageNumber)
  type (OOOPtemc_adtTeamManager_CA), codimension[*], volatile, intent (inout) :: Object_CA
  character(kind=OOOGglob_kcha,len=*), intent (out) :: chrTeamMembersFileName
  integer(OOOGglob_kint), intent (in) :: intImageNumber
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                  ("OOOPtemcG_chrTeamMembersFileName_CA")
                                                                !
                                                                if (IItemc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) then
                                                                  call IItemc_ErrorHandler (Object_CA, &
                                                                   "no valid image number", &
                                                                     OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
  chrTeamMembersFileName = Object_CA[intImageNumber] % m_chrTeamMembersFileName
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPtemcG_chrTeamMembersFileName_CA
!**********
!__________________________________________________________
!
subroutine OOOPtemcS_intNumberOfTeamMembers_CA (Object_CA, intNumberOfTeamMembers, &
                                                   intImageNumber)
  type (OOOPtemc_adtTeamManager_CA), codimension[*], volatile, intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intNumberOfTeamMembers
  integer(OOOGglob_kint), intent (in) :: intImageNumber
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                  ("OOOPtemcS_intNumberOfTeamMembers_CA")
                                                                !
                                                                if (IItemc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) then
                                                                  call IItemc_ErrorHandler (Object_CA, &
                                                                   "no valid image number", &
                                                                     OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
                                                                if (intNumberOfTeamMembers > OOOGglob_TeamMembers_UpperBound) then
                                                                  call IItemc_ErrorHandler (Object_CA, "to many elements", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
  !
  Object_CA[intImageNumber] % m_intNumberOfTeamMembers = intNumberOfTeamMembers
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPtemcS_intNumberOfTeamMembers_CA
!**********
subroutine OOOPtemcG_intNumberOfTeamMembers_CA (Object_CA, intNumberOfTeamMembers, &
                                                intImageNumber)
  type (OOOPtemc_adtTeamManager_CA), codimension[*], volatile, intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (out) :: intNumberOfTeamMembers
  integer(OOOGglob_kint), intent (in) :: intImageNumber
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPtemcG_intNumberOfTeamMembers_CA")
                                                                !
                                                                if (IItemc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) then
                                                                  call IItemc_ErrorHandler (Object_CA, &
                                                                   "no valid image number", &
                                                                     OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
  intNumberOfTeamMembers = Object_CA[intImageNumber] % m_intNumberOfTeamMembers
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPtemcG_intNumberOfTeamMembers_CA
!**********
!___________
!
subroutine OOOPtemcSA_intTeamMemberImages99_CA (Object_CA, intTeamMemberImages99)
  type (OOOPtemc_adtTeamManager_CA), codimension[*], volatile, intent (inout) :: Object_CA
  integer(OOOGglob_kint), dimension(1:OOOGglob_TeamMembers_UpperBound), intent (in) :: intTeamMemberImages99
                                                                call OOOGglob_subSetProcedures &
                                                                  ("OOOPtemcSA_intTeamMemberImages99_CA")
  Object_CA % mA_intTeamMemberImages99 = intTeamMemberImages99
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPtemcSA_intTeamMemberImages99_CA
!**********
subroutine OOOPtemcGA_intTeamMemberImages99_CA (Object_CA, intTeamMemberImages99)
  type (OOOPtemc_adtTeamManager_CA), codimension[*], volatile, intent (inout) :: Object_CA
  integer(OOOGglob_kint), dimension(1:OOOGglob_TeamMembers_UpperBound), intent (out) :: intTeamMemberImages99
                                                                call OOOGglob_subSetProcedures &
                                                                ("OOOPtemcGA_intTeamMemberImages99_CA")
  intTeamMemberImages99 = Object_CA % mA_intTeamMemberImages99
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPtemcGA_intTeamMemberImages99_CA
!**********
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
!





!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Error Handling:   *************************
!**********************************************************
!__________________________________________________________
!
!Private
subroutine IItemc_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT and CA routines
  type(OOOPtemc_adtTeamManager_CA), intent(inout) :: Object
  character(kind=1, len=*), intent(in) :: chrErrorDescription
  integer(OOOGglob_kint), intent(in) :: intErrorType ! 1=warning, 2=Severe System error
  integer(OOOGglob_kint), intent(in) :: intErrorNumber ! Run Time error Number (e.g. status)
  call OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
end subroutine IItemc_ErrorHandler
!__________________________________________________________
!
!Private
logical(OOOGglob_klog) function IItemc_ImageNumberBoundError_CA (Object_CA, intImageNumber)
  ! error handling routine
  ! checks if the image number does exist
  type(OOOPtemc_adtTeamManager_CA), codimension[*], volatile, intent(inout) :: Object_CA
  integer(OOOGglob_kint), intent(in) :: intImageNumber
  !
  IItemc_ImageNumberBoundError_CA = .false.
  !
  if (intImageNumber .lt. 1) then ! image number is too small
    IItemc_ImageNumberBoundError_CA = .true.
  end if
  !
  if (intImageNumber .gt. num_images()) then ! image number is too large
    IItemc_ImageNumberBoundError_CA = .true.
  end if
  !
end function IItemc_ImageNumberBoundError_CA
!__________________________________________________________

end module OOOPtemc_admTeamManager_CA
