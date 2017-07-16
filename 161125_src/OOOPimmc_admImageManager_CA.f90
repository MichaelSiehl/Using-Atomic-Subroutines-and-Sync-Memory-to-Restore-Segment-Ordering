! https://github.com/MichaelSiehl/Using-Atomic-Subroutines-and-Sync-Memory-to-Restore-Segment-Ordering
! src_161123

module OOOPimmc_admImageManager_CA
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPimmc
!********************************************************
! Abstract Data Type (ADT):         OOOPimmc_adtImageManager_CA
! Abstract Data Type Module (adm):  OOOPimmc_admImageManager_CA.f90
!********************************************************
! Purpose:                    ImageManager_CA-Object
! Language:                   mainly Fortran 95 with Fortran 2008 coarrays
! Programmer:                 Michael Siehl
! Date:                       November 2016
!********************************************************
! Naming Conventions:
!
!  for scalar members:
!                             m: object member
!                             S: setter, G: getter,
!                             S_atomic: the setter operates on atomic values using atomic_define and SYNC MEMORY
!                             G_check_atomic: the getter only checks local PGAS memory for specific values atomically
!
!  for array members:
!                             A: array
!                             mA: array member
!                             SA: set array, GA: get array,
!
!  for elements of array members:
!                             SAElement: set only one array element
!                             GAElement: get only one array element
!
!                             99: signals a static array member which has an upper array bound
!                                 larger than necessary; the upper bound is given by a global parameter
!
!  other naming conventions:
!                             _CA: coarray routine / coarray declaration
!                             _SYNC_: synchronization routine
!
!                             Enum: enumeration
!
!                             OO: public (outer) scope (the two leading namespace letters)
!                             II: private (inner) scope
!                             UU: sub-object
!********************************************************
!___________________________________________________________
!
use OOOGglob_Globals
use OOOEerro_admError
!
!___________________________________________________________

implicit none
!___________________________________________________________

private
!__________________________________________________________
!
! access routines for scalar
! and static array members:
public :: OOOPimmcS_chrTeamMembersFileName_CA, OOOPimmcG_chrTeamMembersFileName_CA
!___________________________________________________________
!
!***************************
!****  Error Handling:  ****
!***************************
!***
! local ADT:
private :: IIimmc_ErrorHandler
!***
! coarray ADT:
public :: IIimmc_ImageNumberBoundError_CA
!___________________________________________________________
!
!***************************************
!***  Type Definition: *****************
!***************************************
type, public :: OOOPimmc_adtImageManager_CA
  private
  !*****
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: m_chrTeamMembersFileName
  !*****
  type (OOOEerroc_colError) :: m_UUerrocError ! error collection
  !
end type OOOPimmc_adtImageManager_CA
!___________________________________________________________
!
!****************************************************
!***  Corresponding Coarray Declarations:  **********
!****************************************************
!***
type (OOOPimmc_adtImageManager_CA), public, codimension[*], save :: OOOPimmcImageManager_CA_1
!
!___________________________________________________________




contains



!##################################################################################################
!##################################################################################################
!##################################################################################################


!*******************************
! access routines for scalar   *
! and static array members:    *
!*******************************
!
subroutine OOOPimmcS_chrTeamMembersFileName_CA (Object_CA, chr_TeamMembersFileName, intImageNumber)
  type (OOOPimmc_adtImageManager_CA), codimension[*], intent (inout) :: Object_CA
  character(kind=OOOGglob_kcha,len=*), intent (in) :: chr_TeamMembersFileName
  integer(OOOGglob_kint), intent (in) :: intImageNumber
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                  ("OOOPimmcS_chrTeamMembersFileName_CA")
                                                                !
                                                                if (IIimmc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) then
                                                                  call IIimmc_ErrorHandler (Object_CA, &
                                                                   "no valid image number", &
                                                                     OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
  Object_CA[intImageNumber] % m_chrTeamMembersFileName = chr_TeamMembersFileName
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimmcS_chrTeamMembersFileName_CA
!**********
subroutine OOOPimmcG_chrTeamMembersFileName_CA (Object_CA, chr_TeamMembersFileName, intImageNumber)
  type (OOOPimmc_adtImageManager_CA), codimension[*], intent (inout) :: Object_CA
  character(kind=OOOGglob_kcha,len=*), dimension(1), intent (out) :: chr_TeamMembersFileName
  integer(OOOGglob_kint), intent (in) :: intImageNumber
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                  ("OOOPimmcG_chrTeamMembersFileName_CA")
                                                                !
                                                                if (IIimmc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) then
                                                                  call IIimmc_ErrorHandler (Object_CA, &
                                                                   "no valid image number", &
                                                                     OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
  chr_TeamMembersFileName = Object_CA[intImageNumber] % m_chrTeamMembersFileName
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimmcG_chrTeamMembersFileName_CA
!**********
!__________________________________________________________

!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Error Handling:   *************************
!**********************************************************
!__________________________________________________________
!
!Private
subroutine IIimmc_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT and CA routines
  type(OOOPimmc_adtImageManager_CA), intent(inout) :: Object
  character(kind=1, len=*), intent(in) :: chrErrorDescription
  integer(OOOGglob_kint), intent(in) :: intErrorType ! 1=warning, 2=Severe System error
  integer(OOOGglob_kint), intent(in) :: intErrorNumber ! Run Time error Number (e.g. status)
  call OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
end subroutine IIimmc_ErrorHandler
!__________________________________________________________
!
!Private
logical(OOOGglob_klog) function IIimmc_ImageNumberBoundError_CA (Object_CA, intImageNumber)
  ! error handling routine
  ! checks if the image number does exist
  type(OOOPimmc_adtImageManager_CA), codimension[*], intent(inout) :: Object_CA
  integer(OOOGglob_kint), intent(in) :: intImageNumber
  !
  IIimmc_ImageNumberBoundError_CA = .false.
  !
  if (intImageNumber .lt. 1) then ! image number is too small
    IIimmc_ImageNumberBoundError_CA = .true.
  end if
  !
  !********
  !
  if (intImageNumber .gt. num_images()) then ! image number is too large
    IIimmc_ImageNumberBoundError_CA = .true.
  end if
  !
end function IIimmc_ImageNumberBoundError_CA
!__________________________________________________________

end module OOOPimmc_admImageManager_CA
