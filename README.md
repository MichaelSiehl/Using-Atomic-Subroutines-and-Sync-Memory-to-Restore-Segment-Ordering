# Using-Atomic-Subroutines-and-Sync-Memory-to-Restore-Segment-Ordering
The content of this repository is currently in experimental state

# STEP 1 (161125_src): - track the execution segments on every image
######################################################################################<br />
SOURCE CODE (see the OOOPimsc_admImageStatus_CA.f90 file):<br />
==========================================================<br />
!____________________________________________________________<br />
!<br />
!********** 161124: to track the execution segments on every image:<br />
subroutine OOOPimsc_subSyncMemory (Object_CA)<br />
&nbsp;&nbsp;type (OOOPimsc_adtImageStatus_CA), codimension[ * ], volatile, intent (inout) :: Object_CA<br />
&nbsp;&nbsp;!<br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;! encapsulate access to the sync memory statement herein<br />
&nbsp;&nbsp;sync memory ! to allow tracking of the execution segments on every image<br />
&nbsp;&nbsp;!<br />
&nbsp;&nbsp;call OOOPimscS_atomic_increment_intImageSyncMemoryCount_CA (Object_CA) ! increment the ImageSyncMemoryCount<br />
&nbsp;&nbsp;!<br />
end subroutine OOOPimsc_subSyncMemory<br />
!____________________________________________________________<br />
!********** 161124: track the execution segments on every image:<br />
! private:<br />
subroutine OOOPimscS_atomic_increment_intImageSyncMemoryCount_CA (Object_CA)<br />
&nbsp;&nbsp;type (OOOPimsc_adtImageStatus_CA), codimension[*], volatile, intent (inout) :: Object_CA<br />
&nbsp;&nbsp;!<br />
&nbsp;&nbsp;! increment the ImageSyncMemoryCount member atomically on the executing image only:<br />
&nbsp;&nbsp;call atomic_add(Object_CA % m_atomic_intImageSyncMemoryCount, 1) ! atomic_add is Fortran 2015 syntax<br />
&nbsp;&nbsp;!<br />
end subroutine OOOPimscS_atomic_increment_intImageSyncMemoryCount_CA<br />
!____________________________________________________________<br />
######################################################################################<br />

OUTPUT (from programm execution with 11 coarray images):<br />
 mpirun -np 11 ./a_gfortran.out<br />
==========================================================<br />
 entering segment           1 on image           1<br />
 entering segment           2 on image           1<br />
 entering segment           3 on image           1<br />
 entering segment           4 on image           1<br />
 entering segment           5 on image           1<br />
 entering segment           1 on image           9<br />
 entering segment           2 on image           9<br />
 entering segment           3 on image           9<br />
 entering segment           1 on image           2<br />
 entering segment           2 on image           2<br />
 entering segment           3 on image           2<br />
 entering segment           4 on image           9<br />
 entering segment           5 on image           9<br />
 entering segment           1 on image          10<br />
 entering segment           6 on image           9<br />
 entering segment           7 on image           9<br />
 entering segment           8 on image           9<br />
 entering segment           4 on image           2<br />
 entering segment           1 on image           3<br />
 entering segment           2 on image           3<br />
 entering segment           3 on image           3<br />
 entering segment           2 on image          10<br />
 entering segment           3 on image          10<br />
 entering segment           1 on image           6<br />
 entering segment           2 on image           6<br />
 entering segment           3 on image           6<br />
 entering segment           4 on image           6<br />
 entering segment           5 on image           6<br />
 entering segment           1 on image           7<br />
 entering segment           2 on image           7<br />
 entering segment           3 on image           7<br />
 entering segment           6 on image           6<br />
 entering segment           7 on image           6<br />
 entering segment           8 on image           6<br />
 entering segment           1 on image           8<br />
 entering segment           2 on image           8<br />
 entering segment           3 on image           8<br />
 entering segment           5 on image           2<br />
 entering segment           6 on image           2<br />
 entering segment           1 on image           5<br />
 entering segment           2 on image           5<br />
 entering segment           3 on image           5<br />
 entering segment           1 on image           4<br />
 entering segment           2 on image           4<br />
 entering segment           3 on image           4<br />
 entering segment           1 on image          11<br />
 entering segment           2 on image          11<br />
 entering segment           3 on image          11<br />
######################################################################################<br />
