      program openmoyt

      implicit none
      
      
      integer,parameter :: nx=81,ny=61,nz=16
      real(4) :: ntime
      real*8 ,dimension(nx,ny,nz) :: umean
      real(4),dimension(nx,ny) :: uxux,uyuy,uxuy
      integer :: i,j

      ntime=45000.

      open(10,file='../umean.dat0120000', FORM='UNFORMATTED',&
          access='stream')
      read(10) umean
      close(10)

      open(11,file='umean',form='unformatted',status='unknown')
      write(11) umean
      close(11)

      print *, umean(1:10,1:10,8)
      
!      ul2=ul2/(ntime*nz)
!      vl2=vl2/(ntime*nz)
!      ulul=ulul/(ntime*nz)
!      vlvl=vlvl/(ntime*nz)
!      ulvl=ulvl/(ntime*nz)     
!      
!      uxux(:,:)=ulul(:,:) - ul2(:,:)*ul2(:,:)
!      uyuy(:,:)=vlvl(:,:) - vl2(:,:)*vl2(:,:)
!      uxuy(:,:)=ulvl(:,:) - ul2(:,:)*vl2(:,:)
!
!
! 
!      do j=1,(ny-1)/2
!        do i=1,nx
!           uxux(i,j) = 0.5*(uxux(i,j)+uxux(i,ny-j+1))
!           uyuy(i,j) = 0.5*(uyuy(i,j)+uyuy(i,ny-j+1))
!           uxuy(i,j) = 0.5*(uxuy(i,j)-uxuy(i,ny-j+1))
!        enddo
!      enddo
!      
!      do j=(ny-1)/2+2,ny
!        do i=1,nx
!           uxux(i,j) = uxux(i,ny-j+1)
!           uyuy(i,j) = uyuy(i,ny-j+1)
!           uxuy(i,j) = -uxuy(i,ny-j+1)
!        enddo
!      enddo
! 
!      do i=1,nx
!         j=(ny-1)/2+1
!         uxux(i,j) = 0.5*(uxux(i,j-1)+uxux(i,j+1))
!         uyuy(i,j) = 0.5*(uyuy(i,j-1)+uyuy(i,j+1))
!         uxuy(i,j) = 0.5*(uxuy(i,j-1)+uxuy(i,j+1))
!      enddo
!
!
!            
!       print *,'ululmax,vlvlmax,ulvlmax',maxval(uxux), maxval(uyuy), maxval(uxuy)
!      
!      open(11,file='uxux',form='unformatted',status='unknown')
!      write(11) uxux
!      close(11)
!      
!      open(12,file='uyuy',form='unformatted',status='unknown')
!      write(12) uyuy
!      close(12)
! 
!      open(13,file='uxuy',form='unformatted',status='unknown')
!      write(13) uxuy
!      close(13)
     
          
      end program openmoyt
