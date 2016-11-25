# Using-Atomic-Subroutines-and-Sync-Memory-to-Restore-Segment-Ordering
The content of this repository is currently in experimental state

# 161125_src: STEP 1 - track the segment execution on every image
###############################<br />
source code: (see the OOOPimsc_admImageStatus_CA.f90 file)<br />
!____________________________________________________________<br />
!<br />
!********** 161124: to track the segment execution on every image:<br />
subroutine OOOPimsc_subSyncMemory (Object_CA)<br />
&nbsp;&nbsp;type (OOOPimsc_adtImageStatus_CA), codimension[*], volatile, intent (inout) :: Object_CA<br />
&nbsp;&nbsp;!<br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;! encapsulate access to the F2008 sync memory statement because<br />
&nbsp;&nbsp;sync memory ! this coarray fortran statement forms the unordered execution segments<br />
&nbsp;&nbsp;!<br />
&nbsp;&nbsp;call OOOPimscS_atomic_increment_intImageSyncMemoryCount_CA (Object_CA) ! increment the ImageSyncMemoryCount<br />
&nbsp;&nbsp;!<br />
end subroutine OOOPimsc_subSyncMemory<br />
!<br />
!********** 161124: track the segment execution on every image:<br />
! private:<br />
subroutine OOOPimscS_atomic_increment_intImageSyncMemoryCount_CA (Object_CA)<br />
&nbsp;&nbsp;type (OOOPimsc_adtImageStatus_CA), codimension[*], volatile, intent (inout) :: Object_CA<br />
&nbsp;&nbsp;integer(OOOGglob_kint) :: status = 0 ! error status<br />
&nbsp;&nbsp;!<br />
&nbsp;&nbsp;! increment the ImageSyncMemoryCount member atomically on the executing image only:<br />
&nbsp;&nbsp;call atomic_add(Object_CA % m_atomic_intImageSyncMemoryCount, 1) ! atomic_add is Fortran 2015 syntax<br />
&nbsp;&nbsp;!<br />
end subroutine OOOPimscS_atomic_increment_intImageSyncMemoryCount_CA<br />
!____________________________________________________________<br />
###############################<br /> 
