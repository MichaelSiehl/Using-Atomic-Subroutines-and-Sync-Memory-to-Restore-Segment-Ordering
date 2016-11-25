! https://github.com/MichaelSiehl/Using-Atomic-Subroutines-and-Sync-Memory-to-Restore-Segment-Ordering
! src_161123

module OOOPtmem_admTeamMember
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPtmem
!********************************************************
! Abstract Data Type (ADT):         OOOPtmem_adtTeamMember
! Abstract Data Type Module (adm):  OOOPtmem_admTeamMember.f90
!********************************************************
! Purpose:                    TeamMember-Object
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

use OOOGglob_Globals ! kind-Values
use OOOEerro_admError ! error-Collection
!
!___________________________________________________________

implicit none
!___________________________________________________________

private
!___________________________________________________________
!
!*******************
! ADT-Management: **
!*******************
public :: OOOPtmem_StructureConstructor
!___________________________________________________________
!
!********************
!** Program Logic: **
!********************
public :: OOOPtmem_Start
!___________________________________________________________
!
!*********************
!** Error Handling: **
!*********************
private :: IItmem_ErrorHandler
!___________________________________________________________
!
!*********************
!**  Enumerations:  **
!*********************
!

!___________________________________________________________
!
!***************************************
!***  Type Definition: *****************
!***************************************
type, public :: OOOPtmem_adtTeamMember
  private
  !*****
  type (OOOEerroc_colError) :: m_UUerrocError ! error-Collection
  !
end type OOOPtmem_adtTeamMember
!___________________________________________________________
!
!****************************************************
!***  Corresponding Local Object Declaration:  ******
!****************************************************
!***
type (OOOPtmem_adtTeamMember), public, save :: OOOPtmemTeamMember_1
!___________________________________________________________




contains


!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************
! ADT-Management:  *
!*******************
!___________________________________________________________
!

subroutine OOOPtmem_StructureConstructor (Object)
  ! structure constructor
  type (OOOPtmem_adtTeamMember), intent (inout) :: Object
  !
                                                                call OOOGglob_subSetProcedures ("OOOPtmem_StructureConstructor")
  ! initialize something here
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPtmem_StructureConstructor
!___________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************
! Program Logic:   *
!*******************
!___________________________________________________________

subroutine OOOPtmem_Start (Object)
  use OOOPimsc_admImageStatus_CA ! access corresponding coarrays to
                                 ! communicate with remote or local PGAS memory
  type (OOOPtmem_adtTeamMember), intent (inout) :: Object
  !
                                                                call OOOGglob_subSetProcedures ("OOOPtmem_Start")
  !
  call OOOPtmem_StructureConstructor (Object)
  !
!write(*,*) 'TeamMember started on Image: ', this_image()
  !
  ! communicate with local PGAS memory to finish image execution:
  call OOOPimscS_atomic_intImageActivityFlag_CA (OOOPimscImageStatus_CA_1, OOOPimscEnum_ImageActivityFlag % &
                                                                      ExecutionFinished, this_image())
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPtmem_Start
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
subroutine IItmem_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT-Routines
  type(OOOPtmem_adtTeamMember), intent(inout) :: Object
  character(kind=1, len=*), intent(in) :: chrErrorDescription
  integer(OOOGglob_kint), intent(in) :: intErrorType ! 1=warning, 2=Severe System error
  integer(OOOGglob_kint), intent(in) :: intErrorNumber ! Run Time error Number (e.g. status)
  call OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
end subroutine IItmem_ErrorHandler
!__________________________________________________________





end module OOOPtmem_admTeamMember
