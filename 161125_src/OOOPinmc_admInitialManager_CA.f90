! This file is part of:
! MPMD with Coarray Fortran Load Balancing – Example Program
! copyright 2016 by Michael Siehl
! www.mpmd-with-coarray-fortran.de
! http://www.mpmd-with-coarray-fortran.de/MPMD_Load_Balancing_example.pdf

module OOOPinmc_admInitialManager_CA
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPinmc
!********************************************************
! Abstract Data Type (ADT):         OOOPinmc_adtInitialManager_CA
! Abstract Data Type Module (adm):  OOOPinmc_admInitialManager_CA.f90
!********************************************************
! Purpose:                    InitialManager_CA-Object
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
!
!***
! local ADT management:
public :: OOOPinmc_StructureConstructor
!__________________________________________________________
!
!*********************************
!****  Coarray ADT Routines:  ****
!*********************************
!***
! access routines for scalar
! and static array members:
public :: OOOPinmcS_intNumberOfTeamManagers_CA, OOOPinmcG_intNumberOfTeamManagers_CA
public :: OOOPinmcSA_intTeamManagerImages99_CA, OOOPinmcGA_intTeamManagerImages99_CA
public :: OOOPinmcSA_chrTeamMembersFiles99_CA, OOOPinmcGA_chrTeamMembersFiles99_CA
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
private :: IIinmc_ErrorHandler
!***
! coarray ADT:
private :: IIinmc_ImageNumberBoundError_CA
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
type, public :: OOOPinmc_adtInitialManager_CA
  private
  !*****
  integer(OOOGglob_kint) :: m_intNumberOfTeamManagers = 0
  !*****
  integer(OOOGglob_kint), dimension (1:OOOGglob_TeamManagers_UpperBound) :: mA_intTeamManagerImages99
  !*****
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40), dimension (1:OOOGglob_TeamManagers_UpperBound) :: mA_chrTeamMembersFiles99
  !*****
  type (OOOEerroc_colError) :: m_UUerrocError ! error collection
  !
end type OOOPinmc_adtInitialManager_CA
!__________________________________________________________
!
!****************************************************
!***  Corresponding Coarray Declarations:  **********
!****************************************************
!***
type (OOOPinmc_adtInitialManager_CA), public, codimension[*], volatile, save :: OOOPinmcInitialManager_CA_1
!
!__________________________________________________________





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

!___________________________________________________________


!
subroutine OOOPinmc_StructureConstructor (Object)
  ! structure constructor
  type (OOOPinmc_adtInitialManager_CA), intent (inout) :: Object
  !
                                                                call OOOGglob_subSetProcedures ("OOOPinmc_StructureConstructor")
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPinmc_StructureConstructor
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
!___________________________________________________________
!
subroutine OOOPinmcS_intNumberOfTeamManagers_CA (Object_CA, intNumberOfTeamManagers, &
                                                   intImageNumber)
  type (OOOPinmc_adtInitialManager_CA), codimension[*], volatile, intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intNumberOfTeamManagers
  integer(OOOGglob_kint), intent (in) :: intImageNumber
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                  ("OOOPinmcS_intNumberOfTeamManagers_CA")
                                                                !
                                                                if (IIinmc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) then
                                                                  call IIinmc_ErrorHandler (Object_CA, &
                                                                       "no valid image number", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
                                                                if (intNumberOfTeamManagers > OOOGglob_TeamManagers_UpperBound) &
                                                                then
                                                                  call IIinmc_ErrorHandler (Object_CA, "to many elements", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
  !
  Object_CA[intImageNumber] % m_intNumberOfTeamManagers = intNumberOfTeamManagers
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPinmcS_intNumberOfTeamManagers_CA
!**********
subroutine OOOPinmcG_intNumberOfTeamManagers_CA (Object_CA, intNumberOfTeamManagers, &
                                                intImageNumber)
  type (OOOPinmc_adtInitialManager_CA), codimension[*], volatile, intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (out) :: intNumberOfTeamManagers
  integer(OOOGglob_kint), intent (in) :: intImageNumber
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPinmcG_intNumberOfTeamManagers_CA")
                                                                !
                                                                if (IIinmc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) then
                                                                  call IIinmc_ErrorHandler (Object_CA, &
                                                                       "no valid image number", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
  intNumberOfTeamManagers = Object_CA[intImageNumber] % m_intNumberOfTeamManagers
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPinmcG_intNumberOfTeamManagers_CA
!**********
!_______
!
subroutine OOOPinmcSA_intTeamManagerImages99_CA (Object_CA, intTeamManagerImages99)
  type (OOOPinmc_adtInitialManager_CA), codimension[*], volatile, intent (inout) :: Object_CA
  integer(OOOGglob_kint), dimension(1:OOOGglob_TeamManagers_UpperBound), intent (in) :: intTeamManagerImages99
                                                                call OOOGglob_subSetProcedures &
                                                                  ("OOOPinmcSA_intTeamManagerImages99_CA")
  Object_CA % mA_intTeamManagerImages99 = intTeamManagerImages99
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPinmcSA_intTeamManagerImages99_CA
!**********
subroutine OOOPinmcGA_intTeamManagerImages99_CA (Object_CA, intTeamManagerImages99)
  type (OOOPinmc_adtInitialManager_CA), codimension[*], volatile, intent (inout) :: Object_CA
  integer(OOOGglob_kint), dimension(1:OOOGglob_TeamManagers_UpperBound), intent (out) :: intTeamManagerImages99
                                                                call OOOGglob_subSetProcedures &
                                                                  ("OOOPinmcGA_intTeamManagerImages99_CA")
  intTeamManagerImages99 = Object_CA % mA_intTeamManagerImages99
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPinmcGA_intTeamManagerImages99_CA
!___________________________________________________________
!
subroutine OOOPinmcSA_chrTeamMembersFiles99_CA (Object_CA, chrTeamMembersFiles99)
  type (OOOPinmc_adtInitialManager_CA), codimension[*], volatile, intent (inout) :: Object_CA
  character(kind=OOOGglob_kcha,len=*), dimension(1:OOOGglob_TeamManagers_UpperBound), intent (in) :: chrTeamMembersFiles99
                                                                call OOOGglob_subSetProcedures &
                                                                  ("OOOPinmcSA_chrTeamMembersFiles99_CA")
  Object_CA % mA_chrTeamMembersFiles99 = chrTeamMembersFiles99
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPinmcSA_chrTeamMembersFiles99_CA
!**********
subroutine OOOPinmcGA_chrTeamMembersFiles99_CA (Object_CA, chrTeamMembersFiles99)
  type (OOOPinmc_adtInitialManager_CA), codimension[*], volatile, intent (inout) :: Object_CA
  character(kind=OOOGglob_kcha,len=*), dimension(1:OOOGglob_TeamManagers_UpperBound), intent (out) :: chrTeamMembersFiles99
                                                                call OOOGglob_subSetProcedures &
                                                                  ("OOOPinmcGA_chrTeamMembersFiles99_CA")
  chrTeamMembersFiles99 = Object_CA % mA_chrTeamMembersFiles99
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPinmcGA_chrTeamMembersFiles99_CA
!**********
!___________________________________________________________





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
subroutine IIinmc_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT and CA routines
  type(OOOPinmc_adtInitialManager_CA), intent(inout) :: Object
  character(kind=1, len=*), intent(in) :: chrErrorDescription
  integer(OOOGglob_kint), intent(in) :: intErrorType ! 1=warning, 2=Severe System error
  integer(OOOGglob_kint), intent(in) :: intErrorNumber ! Run Time error Number (e.g. status)
  call OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
end subroutine IIinmc_ErrorHandler
!__________________________________________________________
!
!Private
logical(OOOGglob_klog) function IIinmc_ImageNumberBoundError_CA (Object_CA, intImageNumber)
  ! error handling routine
  ! checks if the image number does exist
  type(OOOPinmc_adtInitialManager_CA), codimension[*], volatile, intent(inout) :: Object_CA
  integer(OOOGglob_kint), intent(in) :: intImageNumber
  !
  IIinmc_ImageNumberBoundError_CA = .false.
  !
  if (intImageNumber .lt. 1) then ! image number is too small
    IIinmc_ImageNumberBoundError_CA = .true.
  end if
  !
  if (intImageNumber .gt. num_images()) then ! image number is too large
    IIinmc_ImageNumberBoundError_CA = .true.
  end if
  !
end function IIinmc_ImageNumberBoundError_CA
!__________________________________________________________

end module OOOPinmc_admInitialManager_CA
