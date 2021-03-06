! https://github.com/MichaelSiehl/Using-Atomic-Subroutines-and-Sync-Memory-to-Restore-Segment-Ordering
! src_161123

module OOOPimsc_admImageStatus_CA
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPimsc
!********************************************************
! Abstract Data Type (ADT):         OOOPimsc_adtImageStatus_CA
! Abstract Data Type Module (adm):  OOOPimsc_admImageStatus_CA.f90
!********************************************************
! Purpose:                    ImageStatus_CA-Object
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
use, intrinsic :: iso_fortran_env
!___________________________________________________________

implicit none
!___________________________________________________________

private
!__________________________________________________________
!
! access routines for scalar
! and static array members:
public :: OOOPimscS_atomic_intImageActivityFlag_CA, OOOPimscG_check_atomic_intImageActivityFlag_CA
!***
public :: OOOPimsc_subSyncMemory ! 161124: to track the segment execution on every image
private :: OOOPimscS_atomic_increment_intImageSyncMemoryCount_CA ! 161124: track the segment execution on every image
!***
! coarray ADT management:
public :: OOOPimsc_StructureConstructor_CA
!___________________________________________________________
!
!***************************
!****  Error Handling:  ****
!***************************
!***
! local ADT:
private :: IIimsc_ErrorHandler
!***
! coarray ADT:
private :: IIimsc_ImageNumberBoundError_CA
!___________________________________________________________
!
!************************
!****  Enumerations:  ***
!************************
!***  ImageActivityFlag:
type, private :: OOOPimsc_DontUse1
  integer(kind=OOOGglob_kint) :: InitialWaiting ! = 1
  integer(kind=OOOGglob_kint) :: TeamManager ! = 2
  integer(kind=OOOGglob_kint) :: TeamMember ! = 3
  integer(kind=OOOGglob_kint) :: ExecutionFinished ! = 4
end type OOOPimsc_DontUse1
!
type (OOOPimsc_DontUse1), public, parameter :: OOOPimscEnum_ImageActivityFlag &
                                           = OOOPimsc_DontUse1 (1,2,3,4)
!
!___________________________________________________________
!
!***************************************
!***  Type Definition: *****************
!***************************************
type, public :: OOOPimsc_adtImageStatus_CA
  private
  !*****
  integer(atomic_int_kind) :: m_atomic_intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % InitialWaiting
  !*****
  integer(atomic_int_kind) :: m_atomic_intImageSyncMemoryCount = 0 ! 161124
  !*****
  type (OOOEerroc_colError) :: m_UUerrocError ! error collection
  !
end type OOOPimsc_adtImageStatus_CA
!___________________________________________________________
!
!****************************************************
!***  Corresponding Coarray Declarations:  **********
!****************************************************
!***
type (OOOPimsc_adtImageStatus_CA), public, codimension[*], save :: OOOPimscImageStatus_CA_1
!___________________________________________________________





contains


!##################################################################################################
!##################################################################################################
!##################################################################################################


!*******************************
! access routines for scalar   *
! and static array members:    *
!*******************************
!__________________________________________________________
!
!**********
subroutine OOOPimscS_atomic_intImageActivityFlag_CA (Object_CA, intImageActivityFlag, &
                                                   intImageNumber)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intImageActivityFlag
  integer(OOOGglob_kint), intent (in) :: intImageNumber
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPimscS_atomic_intImageActivityFlag_CA")
                                                                !
                                                                if (IIimsc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "no valid image number", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
  call OOOPimsc_subSyncMemory (Object_CA) ! calling sync memory
  call atomic_define(Object_CA[intImageNumber] % m_atomic_intImageActivityFlag, intImageActivityFlag)
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimscS_atomic_intImageActivityFlag_CA
!**********
logical(OOOGglob_klog) function OOOPimscG_check_atomic_intImageActivityFlag_CA (Object_CA, intCheckImageActivityFlag)
  ! in order to hide the sync memory statement herein, this Getter does not allow
  ! to access the member directly, but instead does only allow to check the atomic member
  ! for specific values (this Getter is intented for synchronizations)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intCheckImageActivityFlag
  integer(OOOGglob_kint) :: intImageActivityFlag
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPimscG_check_atomic_intImageActivityFlag_CA")
  !
  OOOPimscG_check_atomic_intImageActivityFlag_CA = .false.
  !
  call atomic_ref(intImageActivityFlag, Object_CA % m_atomic_intImageActivityFlag)
  if (intCheckImageActivityFlag == intImageActivityFlag) then
    call OOOPimsc_subSyncMemory (Object_CA) ! calling sync memory
    OOOPimscG_check_atomic_intImageActivityFlag_CA = .true.
  end if
  !
                                                                call OOOGglob_subResetProcedures
end function OOOPimscG_check_atomic_intImageActivityFlag_CA
!**********
!____________________________________________________________
!
!********** 161124: to track the segment execution on every image:
subroutine OOOPimsc_subSyncMemory (Object_CA)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA

  !
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPimsc_subSyncMemory")
  ! encapsulate access to the F2008 sync memory statement because
  sync memory ! this coarray fortran statement forms the unordered execution segments
  !
  call OOOPimscS_atomic_increment_intImageSyncMemoryCount_CA (Object_CA) ! increment the ImageSyncMemoryCount
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_subSyncMemory
!
!********** 161124: track the segment execution on every image:
! private:
subroutine OOOPimscS_atomic_increment_intImageSyncMemoryCount_CA (Object_CA)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint) :: status = 0 ! error status
  integer(OOOGglob_kint) :: intSegment ! only to produce a test-output
  !
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPimscS_atomic_increment_intImageSyncMemoryCount_CA")
  !
  ! increment the ImageSyncMemoryCount member atomically on the executing image only:
  !
  !call atomic_add(Object_CA % m_atomic_intImageSyncMemoryCount, 1) ! atomic_add is Fortran 2015 syntax
  !
  ! Fortran 2008 syntax:
  call atomic_ref(intTemp, Object_CA % m_atomic_intImageSyncMemoryCount)
  intTemp = intTemp + 1
  ! don't execute sync memory for local atomic_define:
  call atomic_define(Object_CA % m_atomic_intImageSyncMemoryCount, intTemp)
  !
  ! test:
  call atomic_ref(intSegment, Object_CA % m_atomic_intImageSyncMemoryCount) ! only to produce a test-output:
  write(*,*) 'entering segment', intSegment, 'on image', this_image()
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimscS_atomic_increment_intImageSyncMemoryCount_CA
!____________________________________________________________

!##################################################################################################
!##################################################################################################
!##################################################################################################


!**************************
! coarray ADT management: *
!**************************
!___________________________________________________________
!
!
subroutine OOOPimsc_StructureConstructor_CA (Object_CA)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  !
                                                                call OOOGglob_subSetProcedures ("OOOPimsc_StructureConstructor_CA")

  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_StructureConstructor_CA
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
subroutine IIimsc_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT and CA routines
  type(OOOPimsc_adtImageStatus_CA), intent(inout) :: Object
  character(kind=1, len=*), intent(in) :: chrErrorDescription
  integer(OOOGglob_kint), intent(in) :: intErrorType ! 1=warning, 2=Severe System error
  integer(OOOGglob_kint), intent(in) :: intErrorNumber ! Run Time error Number (e.g. status)
  call OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
end subroutine IIimsc_ErrorHandler
!__________________________________________________________
!
!Private
logical(OOOGglob_klog) function IIimsc_ImageNumberBoundError_CA (Object_CA, intImageNumber)
  ! error handling routine
  ! checks if the image number does exist
  type(OOOPimsc_adtImageStatus_CA), codimension[*], intent(inout) :: Object_CA
  integer(OOOGglob_kint), intent(in) :: intImageNumber
  !
  IIimsc_ImageNumberBoundError_CA = .false.
  !
  if (intImageNumber .lt. 1) then ! image number is too small
    IIimsc_ImageNumberBoundError_CA = .true.
  end if
  !
  if (intImageNumber .gt. num_images()) then ! image number is too large
    IIimsc_ImageNumberBoundError_CA = .true.
  end if
  !
end function IIimsc_ImageNumberBoundError_CA
!__________________________________________________________

end module OOOPimsc_admImageStatus_CA
