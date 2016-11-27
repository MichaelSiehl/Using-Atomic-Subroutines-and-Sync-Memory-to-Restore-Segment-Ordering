! https://github.com/MichaelSiehl/Using-Atomic-Subroutines-and-Sync-Memory-to-Restore-Segment-Ordering
! src_161123

module OOOPtema_admTeamManager
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPtema
!********************************************************
! Abstract Data Type (ADT):         OOOPtema_adtTeamManager
! Abstract Data Type Module (adm):  OOOPtema_admTeamManager.f90
!********************************************************
! Purpose:                    TeamManager-Object
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
use OOOPstpa_admStartPath ! load the start path from file
!
use OOOPimsc_admImageStatus_CA ! communicate with remote or local PGAS memory
!___________________________________________________________

implicit none
!___________________________________________________________

private
!___________________________________________________________
!
!*****************************
! access routines for scalar *
! and static array members:  *
!*****************************
public :: OOOPtemaS_chrTeamMembersFileName, OOOPtemaG_chrTeamMembersFileName
public :: OOOPtemaS_intNumberOfTeamMembers, OOOPtemaG_intNumberOfTeamMembers
!
!*******************
! ADT-Management: **
!*******************
public :: OOOPtema_StructureConstructor
!___________________________________________________________
!
!********************
!** Program Logic: **
!********************
public :: OOOPtema_Start
private :: IItema_LoadTeamMembers
private :: IItema_ActivateTeamMemberImage
!___________________________________________________________
!
!*********************
!** Error Handling: **
!*********************
private :: IItema_ErrorHandler
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
type, public :: OOOPtema_adtTeamManager
  private
  !*****
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: m_chrTeamMembersFileName = ""
  !****
  integer(OOOGglob_kint) :: m_intNumberOfTeamMembers = 0
  !*****
  integer(OOOGglob_kint), dimension (1:OOOGglob_TeamMembers_UpperBound) :: mA_intTeamMemberImages99
  !*****
  type (OOOEerroc_colError) :: m_UUerrocError ! error-Collection
  !
end type OOOPtema_adtTeamManager
!___________________________________________________________
!
!****************************************************
!***  Corresponding Local Object Declaration:  ******
!****************************************************
!***
type (OOOPtema_adtTeamManager), public, save :: OOOPtemaTeamManager_1
!___________________________________________________________








contains


!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************************
! access routines for scalar   *
! and static array members:    *
!*******************************
!___________________________________________________________
!
subroutine OOOPtemaS_chrTeamMembersFileName (Object, chrTeamMembersFileName)
  type (OOOPtema_adtTeamManager), intent (inout) :: Object
  character(kind=OOOGglob_kcha,len=*), intent (in) :: chrTeamMembersFileName
                                                                call OOOGglob_subSetProcedures ("OOOPtemaS_chrTeamMembersFileName")
  Object % m_chrTeamMembersFileName = chrTeamMembersFileName
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPtemaS_chrTeamMembersFileName
!**********
subroutine OOOPtemaG_chrTeamMembersFileName (Object, chrTeamMembersFileName)
  type (OOOPtema_adtTeamManager), intent (in) :: Object
  character(kind=OOOGglob_kcha,len=*), intent (out) :: chrTeamMembersFileName
                                                                call OOOGglob_subSetProcedures ("OOOPtemaG_chrTeamMembersFileName")
  chrTeamMembersFileName = Object % m_chrTeamMembersFileName
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPtemaG_chrTeamMembersFileName
!__________________________________________________________
!
subroutine OOOPtemaS_intNumberOfTeamMembers (Object, intNumberOfTeamMembers)
  type (OOOPtema_adtTeamManager), intent (inout) :: Object
  integer(OOOGglob_kint), intent (in) :: intNumberOfTeamMembers
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures ("OOOPtemaS_intNumberOfTeamMembers")
                                                                !
                                                                if (intNumberOfTeamMembers > OOOGglob_TeamMembers_UpperBound) then
                                                                  call IItema_ErrorHandler (Object, "to many elements", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
  !
  Object % m_intNumberOfTeamMembers = intNumberOfTeamMembers
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPtemaS_intNumberOfTeamMembers
!**********
subroutine OOOPtemaG_intNumberOfTeamMembers (Object, intNumberOfTeamMembers)
  type (OOOPtema_adtTeamManager), intent (in) :: Object
  integer(OOOGglob_kint), intent (out) :: intNumberOfTeamMembers
                                                                call OOOGglob_subSetProcedures ("OOOPtemaG_intNumberOfTeamMembers")
  intNumberOfTeamMembers = Object % m_intNumberOfTeamMembers
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPtemaG_intNumberOfTeamMembers
!__________________________________________________________




!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************
! ADT-Management:  *
!*******************
!___________________________________________________________
!
subroutine OOOPtema_StructureConstructor (Object)
  ! structure constructor
  type (OOOPtema_adtTeamManager), intent (inout) :: Object
  !
                                                                call OOOGglob_subSetProcedures ("OOOPtema_StructureConstructor")
  ! initialize something here
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPtema_StructureConstructor
!___________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************
! Program Logic:   *
!*******************
!___________________________________________________________

subroutine OOOPtema_Start (Object, chrTeamMembersFileName)
  use OOOPimsc_admImageStatus_CA ! access corresponding coarrays to
                                 ! communicate with remote or local PGAS memory
  type (OOOPtema_adtTeamManager), intent (inout) :: Object
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40), intent (in) :: chrTeamMembersFileName
  integer(OOOGglob_kint) :: intTeamMemberImageNumber
  integer(OOOGglob_kint) :: intCounter = 0
  !
                                                                call OOOGglob_subSetProcedures ("OOOPtema_Start")
  !
  call OOOPtema_StructureConstructor (Object)
  !
! write(*,*) 'TeamManager started on Image: ', this_image()
  !
  ! we do an early setting of the ImageActivityFlag at this place
  ! to allow supersession of the ExecutionFinished value later on:
  call OOOPimscS_atomic_intImageActivityFlag_CA (OOOPimscImageStatus_CA_1, OOOPimscEnum_ImageActivityFlag % &
                                                                      ExecutionFinished, this_image())
  !
  call OOOPtemaS_chrTeamMembersFileName (Object, chrTeamMembersFileName)
  call IItema_LoadTeamMembers (Object) ! from the belonging TeamMembers.txt file
  !
  ! activate the TeamMembers on their images as given by the belonging TeamMembers_x.txt file:
  do intCounter = 1, Object % m_intNumberOfTeamMembers
    ! (mA_intTeamMemberImages99(intCounter) gives the (remote) image number of the TeamMember):
    intTeamMemberImageNumber = Object % mA_intTeamMemberImages99(intCounter)
    call IItema_ActivateTeamMemberImage (Object, intTeamMemberImageNumber)
  end do
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPtema_Start
!___________________________________________________________
!
subroutine IItema_LoadTeamMembers (Object)
  ! method, loads the data from the belonging TeamMembers_x.txt file
  type (OOOPtema_adtTeamManager), intent (inout) :: Object
  !
  type (OOOPstpa_adtStartPath) :: UUStartPath1
  !
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: OpenStatus = 'OLD'
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: OpenAccess  = 'SEQUENTIAL'
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: OpenForm = 'FORMATTED'
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: OpenPosition = 'REWIND'
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: OpenAction = 'read'
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: OpenBlank = 'NULL'
  !
  character(kind=OOOGglob_kcha,len=OOOGglob_Len200) :: chrStartPath
  !
  ! for inquire:
  logical(kind=OOOGglob_klog) :: logExist
  integer(OOOGglob_kint) :: intRecl = 0
  !
  integer(OOOGglob_kint) :: FileUnit = 0
  integer(OOOGglob_kint) :: FileStatus = 0 ! File-error-status
  character(kind=OOOGglob_kcha,len=OOOGglob_Len200) :: chrPathAndFileName = ""
  integer(OOOGglob_kint) :: intNumberOfTeamMembers
  !
                                                                call OOOGglob_subSetProcedures ("IItema_LoadTeamMembers")
  !
  FileUnit = OOOGglob_FileUnitA
  !
  ! get the path to the files directory:
  call OOOPstpa_LoadPath (UUStartPath1)
  call OOOPstpaG_chrPath (UUStartPath1, chrStartPath)
  !
  chrPathAndFileName = trim(chrStartPath) // Object % m_chrTeamMembersFileName
  !
  open (unit=FileUnit, iostat=FileStatus, file=trim(chrPathAndFileName), &
      status=trim(OpenStatus), access=trim(OpenAccess), form=trim(OpenForm), &
      position=trim(OpenPosition), action=trim(OpenAction), &
      blank=trim(OpenBlank), delim='APOSTROPHE')
                                                                !
                                                                if (FileStatus /= 0) then
                                                                  call IItema_ErrorHandler (Object, "File-Open-error", &
                                                                    OOOGglob_error, FileStatus)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
  !
  read (unit=FileUnit, fmt=*, iostat=FileStatus) intNumberOfTeamMembers
                                                                !
                                                                if (FileStatus /= 0) then
                                                                  call IItema_ErrorHandler (Object, "File read-Error 1", &
                                                                    OOOGglob_error, FileStatus)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
  !
  call OOOPtemaS_intNumberOfTeamMembers (Object, intNumberOfTeamMembers) ! contains error handling
  !
  ! read the TeamMember image numbers from file:
  read (unit=FileUnit, fmt=*, iostat=FileStatus) Object % mA_intTeamMemberImages99 (1 : Object % m_intNumberOfTeamMembers)
  !
                                                                if (FileStatus /= 0) then
                                                                  ! FileStatus error
                                                                  call IItema_ErrorHandler (Object, "File read-Error 2", &
                                                                    OOOGglob_error, FileStatus)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
  !
  close (unit=FileUnit, iostat=FileStatus, status='KEEP')
                                                                if (FileStatus /= 0) then
                                                                  call IItema_ErrorHandler (Object, "File-Close-error", &
                                                                    OOOGglob_error, FileStatus)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
  !
                                                                call OOOGglob_subResetProcedures
  !
end subroutine IItema_LoadTeamMembers
!___________________________________________________________
!
subroutine IItema_ActivateTeamMemberImage (Object, intTeamMemberImageNumber)
  !!!  synchronization counterpart routine  !!!!
  !!!  for IIimma_SYNC_CheckActivityFlag    !!!!
  ! starts a single TeamMember on its image
  !
  use OOOPimsc_admImageStatus_CA ! access corresponding coarrays to
                                 ! communicate with remote or local PGAS memory
  type (OOOPtema_adtTeamManager), intent (inout) :: Object
  integer(OOOGglob_kint), intent(in) :: intTeamMemberImageNumber
  !
                                                                call OOOGglob_subSetProcedures ("IItema_ActivateTeamMemberImage")
  ! communicate with remote or local PGAS memory to activate a TeamMember image:
  call OOOPimscS_atomic_intImageActivityFlag_CA (OOOPimscImageStatus_CA_1, OOOPimscEnum_ImageActivityFlag % TeamMember, &
                                                         intTeamMemberImageNumber)
  !
                                                                call OOOGglob_subResetProcedures
end subroutine IItema_ActivateTeamMemberImage
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
subroutine IItema_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT-Routines
  type(OOOPtema_adtTeamManager), intent(inout) :: Object
  character(kind=1, len=*), intent(in) :: chrErrorDescription
  integer(OOOGglob_kint), intent(in) :: intErrorType ! 1=warning, 2=Severe System error
  integer(OOOGglob_kint), intent(in) :: intErrorNumber ! Run Time error Number (e.g. status)
  call OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
end subroutine IItema_ErrorHandler
!__________________________________________________________





end module OOOPtema_admTeamManager
