# Using-Atomic-Subroutines-and-Sync-Memory-to-Restore-Segment-Ordering
The content of this repository is currently in experimental state

# 161125_src: STEP 1 - track the segment execution on every image
source code:

!____________________________________________________________
!
!********** 161124: to track the segment execution on every image:
subroutine OOOPimsc_subSyncMemory (Object_CA)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], volatile, intent (inout) :: Object_CA
  !
              ! encapsulate access to the F2008 sync memory statement because
  sync memory ! this coarray fortran statement forms the unordered execution segments
  !
  call OOOPimscS_atomic_increment_intImageSyncMemoryCount_CA (Object_CA) ! increment the ImageSyncMemoryCount
  !
end subroutine OOOPimsc_subSyncMemory
!
!********** 161124: track the segment execution on every image:
! private:
subroutine OOOPimscS_atomic_increment_intImageSyncMemoryCount_CA (Object_CA)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], volatile, intent (inout) :: Object_CA
  integer(OOOGglob_kint) :: status = 0 ! error status
  !
  ! increment the ImageSyncMemoryCount member atomically on the executing image only:
  call atomic_add(Object_CA % m_atomic_intImageSyncMemoryCount, 1) ! atomic_add is Fortran 2015 syntax
  !
end subroutine OOOPimscS_atomic_increment_intImageSyncMemoryCount_CA
!____________________________________________________________
