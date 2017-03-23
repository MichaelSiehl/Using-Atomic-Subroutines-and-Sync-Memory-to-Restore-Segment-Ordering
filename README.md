# Using-Atomic-Subroutines-and-Sync-Memory-to-Restore-Segment-Ordering
The content of this repository is currently in experimental state

# STEP 1 (161125_src): - track the execution segments on every image
######################################################################################<br />
SOURCE CODE (see the OOOPimsc_admImageStatus_CA.f90 file):<br />
```fortran
!==========================================================
!____________________________________________________________
!
!********** 161124: to track the execution segments on every image:
subroutine OOOPimsc_subSyncMemory (Object_CA)
  type (OOOPimsc_adtImageStatus_CA), codimension[ * ], volatile, intent (inout) :: Object_CA
  !
  ! encapsulate access to the sync memory statement herein
  sync memory ! to allow tracking of the execution segments on every image
  !
  call OOOPimscS_atomic_increment_intImageSyncMemoryCount_CA (Object_CA) ! increment the ImageSyncMemoryCount
  !
end subroutine OOOPimsc_subSyncMemory
```
```fortran
!____________________________________________________________
!********** 161124: track the execution segments on every image:
! private:
subroutine OOOPimscS_atomic_increment_intImageSyncMemoryCount_CA (Object_CA)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], volatile, intent (inout) :: Object_CA
  !
  ! increment the ImageSyncMemoryCount member atomically on the executing image only:
  call atomic_add(Object_CA % m_atomic_intImageSyncMemoryCount, 1) ! atomic_add is Fortran 2015 syntax
  !
end subroutine OOOPimscS_atomic_increment_intImageSyncMemoryCount_CA
!____________________________________________________________
!######################################################################################
```
OUTPUT (from programm execution with 11 coarray images):<br />
 mpirun -np 11 ./a_gfortran.out<br />
```fortran
==========================================================
 entering segment           1 on image           1
 entering segment           2 on image           1
 entering segment           3 on image           1
 entering segment           4 on image           1
 entering segment           5 on image           1
 entering segment           1 on image           9
 entering segment           2 on image           9
 entering segment           3 on image           9
 entering segment           1 on image           2
 entering segment           2 on image           2
 entering segment           3 on image           2
 entering segment           4 on image           9
 entering segment           5 on image           9
 entering segment           1 on image          10
 entering segment           6 on image           9
 entering segment           7 on image           9
 entering segment           8 on image           9
 entering segment           4 on image           2
 entering segment           1 on image           3
 entering segment           2 on image           3
 entering segment           3 on image           3
 entering segment           2 on image          10
 entering segment           3 on image          10
 entering segment           1 on image           6
 entering segment           2 on image           6
 entering segment           3 on image           6
 entering segment           4 on image           6
 entering segment           5 on image           6
 entering segment           1 on image           7
 entering segment           2 on image           7
 entering segment           3 on image           7
 entering segment           6 on image           6
 entering segment           7 on image           6
 entering segment           8 on image           6
 entering segment           1 on image           8
 entering segment           2 on image           8
 entering segment           3 on image           8
 entering segment           5 on image           2
 entering segment           6 on image           2
 entering segment           1 on image           5
 entering segment           2 on image           5
 entering segment           3 on image           5
 entering segment           1 on image           4
 entering segment           2 on image           4
 entering segment           3 on image           4
 entering segment           1 on image          11
 entering segment           2 on image          11
 entering segment           3 on image          11
 ```
######################################################################################
# STEP 2 (161127_src): - instead of the scalar integer, we now use an integer array together with atomic subroutines, to allow a simple syntax for the segment ordering synchronization later on<br />
######################################################################################<br />
SOURCE CODE (see the OOOPimsc_admImageStatus_CA.f90 file):
```fortran
!==========================================================
!***************************************
!***  Type Definition: *****************
!***************************************
type, public :: OOOPimsc_adtImageStatus_CA
  private
  !*****
  integer(atomic_int_kind) :: m_atomic_intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % InitialWaiting
  !*****
  integer(atomic_int_kind), dimension (1:OOOGglob_NumImages_UpperBound) :: mA_atomic_intImageSyncMemoryCount = 0 ! 161126
  !*****
  type (OOOEerroc_colError) :: m_UUerrocError ! error collection
  !
end type OOOPimsc_adtImageStatus_CA
!____________________________________________________________
```
```fortran
!____________________________________________________________
!
!********** 161124: to track the execution segments on every image:
subroutine OOOPimsc_subSyncMemory (Object_CA)
  type (OOOPimsc_adtImageStatus_CA), codimension[ * ], volatile, intent (inout) :: Object_CA
  !
  ! encapsulate access to the sync memory statement herein
  sync memory ! to allow tracking of the execution segments on every image
  !
  call OOOPimscSAElement_atomic_increment_intImageSyncMemoryCount_CA (Object_CA) ! increment the ImageSyncMemoryCount
  !
end subroutine OOOPimsc_subSyncMemory
```
```fortran
!____________________________________________________________
!********** 161124: track the execution segments on every image:
! private:
subroutine OOOPimscSAElement_atomic_increment_intImageSyncMemoryCount_CA (Object_CA)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], volatile, intent (inout) :: Object_CA
  !
  ! increment (by 1) the ImageSyncMemoryCount member atomically on the executing image only:
  ! 161126: every image uses its own array index (this_image()):
  call atomic_add(Object_CA % mA_atomic_intImageSyncMemoryCount(this_image()), 1) ! atomic_add is Fortran 2015 syntax
  !
end subroutine OOOPimscSAElement_atomic_increment_intImageSyncMemoryCount_CA
!____________________________________________________________
!######################################################################################
```
