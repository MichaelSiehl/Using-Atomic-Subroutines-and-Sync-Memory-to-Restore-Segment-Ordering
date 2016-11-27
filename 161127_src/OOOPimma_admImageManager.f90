! https://github.com/MichaelSiehl/Using-Atomic-Subroutines-and-Sync-Memory-to-Restore-Segment-Ordering
! src_161123

module OOOPimma_admImageManager
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPimma
!********************************************************
! Abstract Data Type (ADT):         OOOPimma_adtImageManager
! Abstract Data Type Module (adm):  OOOPimma_admImageManager.f90
!********************************************************
! Purpose:                    ImageManager-Object
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

use OOOGglob_Globals
use OOOEerro_admError
!
use OOOPimmc_admImageManager_CA
use OOOPimsc_admImageStatus_CA ! access corresponding coarrays to
                               ! communicate with remote or local PGAS memory
use OOOPtema_admTeamManager
use OOOPtmem_admTeamMember
use OOOPinma_admInitialManager
!___________________________________________________________

implicit none
!___________________________________________________________

private
!___________________________________________________________
!
!
!*******************
! ADT-Management: **
!*******************
public :: OOOPimma_StructureConstructor
!___________________________________________________________
!
!********************
!** Program Logic: **
!********************
public :: OOOPimma_Start
private :: IIimma_SYNC_CheckActivityFlag ! synchronization routine
!___________________________________________________________
!
!********************
!** Error Handling: *
!********************
private :: IIimma_ErrorHandler
!___________________________________________________________
!
!*********************
!**  Enumerations:  **
!*********************

!___________________________________________________________
!
!***************************************
!***  Type Definition: *****************
!***************************************
type, public :: OOOPimma_adtImageManager
  private
  !*****
  type (OOOEerroc_colError) :: m_UUerrocError ! error-Collection
  !
end type OOOPimma_adtImageManager
!__________________________________________________________
!
!****************************************************
!***  Corresponding Local Object Declaration:  ******
!****************************************************
!***
type (OOOPimma_adtImageManager), public, save :: OOOPimmaImageManager_1
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






!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************
! ADT-Management:  *
!*******************
!___________________________________________________________

subroutine OOOPimma_StructureConstructor (Object)
  ! structure constructor
  type (OOOPimma_adtImageManager), intent (inout) :: Object
  !
                                                                call OOOGglob_subSetProcedures ("OOOPimma_StructureConstructor")
  !
  ! initialize something here
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimma_StructureConstructor
!___________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************
! Program Logic:   *
!*******************
!___________________________________________________________
!
subroutine OOOPimma_Start (Object)
  type (OOOPimma_adtImageManager), intent (inout) :: Object
  !
                                                                call OOOGglob_subSetProcedures ("OOOPimma_Start")
  !
  call OOOPimma_StructureConstructor (Object)
  sync all
  !
  if (this_image() == 1) then
    ! on image 1 only: check if the number of images is not less 4,
    ! as a minimum requirement:
    if (num_images() .lt. 4) then
      write(*,*) ' **************************************'
      write(*,*) ' ** Total number of images is to small: ', num_images(), '. '
      write(*,*) ' ** Total number of images must be greater'
      write(*,*) ' ** than 3 for the program to execute !'
      write(*,*) ' **** program execution stopped *******'
      write(*,*) ' **************************************'
      error stop ! STOP only, would crash the Linux terminal window
    end if
    !
    sync images(*) ! all other images will wait for the executing image (image 1)
                    ! to reach this (but do not wait for each other)
  else
    sync images(1) ! image 1 will wait for each of the other (executing) images to reach this
  end if
  !********
  if (this_image() .gt. 1) then ! on all images except on image 1
    call IIimma_SYNC_CheckActivityFlag (Object) ! synchronization routine
  else
    call OOOPinma_Start (OOOPinmaInitialManager_1) ! start the InitialManager on image 1
    !
    call IIimma_SYNC_CheckActivityFlag (Object) ! synchronization routine, will finish execution on image 1
                                                ! due to the call of subroutine IIinma_FinishExecution earlier
                                                ! in subroutine OOOPinma_Start
  end if
    !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimma_Start
!___________________________________________________________
!
subroutine IIimma_SYNC_CheckActivityFlag (Object)
  !!! synchronization routine !!!!!
  !!! synchronization counterpart routines are:  !!!!
  !!! IIinma_ActivateTeamManagerImage  !!!!
  !!! IItema_ActivateTeamMemberImage   !!!!
  !
  use OOOPimsc_admImageStatus_CA ! access corresponding coarrays to
                                 ! communicate with remote or local PGAS memory
  type (OOOPimma_adtImageManager), intent (in) :: Object
  integer(OOOGglob_kint) :: intImageActivityFlag
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: chrTeamMembersFileName
  !
                                                                call OOOGglob_subSetProcedures ("IIimma_SYNC_CheckActivityFlag")
  !
  do ! check the ImageActivityFlag in local PGAS memory permanently until it has
     !         value OOOPimscEnum_ImageActivityFlag % ExecutionFinished
    !write(*,*) 'being within the spin-(wait) loop'
    !
    if (OOOPimscG_check_atomic_intImageActivityFlag_CA (OOOPimscImageStatus_CA_1, &
                                               OOOPimscEnum_ImageActivityFlag % TeamManager)) then
      call OOOPimmcG_chrTeamMembersFileName_CA (OOOPimmcImageManager_CA_1, chrTeamMembersFileName, this_image())
      call OOOPtema_Start (OOOPtemaTeamManager_1, chrTeamMembersFileName) ! start a TeamManager on the executing image
    end if
    !
    if (OOOPimscG_check_atomic_intImageActivityFlag_CA (OOOPimscImageStatus_CA_1, &
                                               OOOPimscEnum_ImageActivityFlag % TeamMember)) then
      call OOOPtmem_Start (OOOPtmemTeamMember_1) ! start a TeamMember on the executing image
    end if
    !
    ! finish image execution:
    if (OOOPimscG_check_atomic_intImageActivityFlag_CA (OOOPimscImageStatus_CA_1, &
                                               OOOPimscEnum_ImageActivityFlag % ExecutionFinished)) then
!      write(*,*) 'Execution finished on image', this_image()
      exit
    end if
    !
  end do
!
                                                                call OOOGglob_subResetProcedures
end subroutine IIimma_SYNC_CheckActivityFlag
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
subroutine IIimma_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT-Routines
  type(OOOPimma_adtImageManager), intent(inout) :: Object
  character(kind=1, len=*), intent(in) :: chrErrorDescription
  integer(OOOGglob_kint), intent(in) :: intErrorType ! 1=warning, 2=Severe System error
  integer(OOOGglob_kint), intent(in) :: intErrorNumber ! Run Time error Number (e.g. status)
  call OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
end subroutine IIimma_ErrorHandler
!__________________________________________________________






end module OOOPimma_admImageManager
