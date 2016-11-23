! https://github.com/MichaelSiehl/Using-Atomic-Subroutines-and-Sync-Memory-to-Restore-Segment-Ordering
! src_161123

module Main_Sub
!
contains
!
!**********
!
subroutine Entry_Main_Sub
  !
  use OOOPimma_admImageManager
  !
  implicit none
  !
  call OOOPimma_Start (OOOPimmaImageManager_1) ! start the ImageManager on all images
  !
end subroutine Entry_Main_Sub
!
!**********
!
end module Main_Sub
