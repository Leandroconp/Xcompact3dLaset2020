      program openmoyt

      implicit none
      ! When big data compile as: gfortran -mcmodel=medium openmoyt.f90      
      
      integer,parameter :: nx=81,ny=61,nz=16
      real(4) :: ntime
      real*8 ,dimension(nx,ny,nz) :: umean, vmean, uumean, vvmean,&
                                     uvmean
      real*8 ,dimension(nx,ny,nz) :: ul2mean, vl2mean, ulvlmean
      real*8 ,dimension(nx,ny) :: ul2mz, vl2mz, ulvlmz
      integer :: i,j,k
      character(6) :: chits

      write (*,*) 'file number - Xcompact3D to ParaView'
      read (*,*) chits
      write (*,*) 'Time interval'
      read (*,*) ntime


      open(10,file='../umean.dat0'//chits, FORM='UNFORMATTED',&
          access='stream')
      read(10) umean
      close(10)
      open(10,file='../vmean.dat0'//chits, FORM='UNFORMATTED',&
          access='stream')
      read(10) vmean
      close(10)
      open(10,file='../uumean.dat0'//chits, FORM='UNFORMATTED',&
          access='stream')
      read(10) uumean
      close(10)
      open(10,file='../vvmean.dat0'//chits, FORM='UNFORMATTED',&
          access='stream')
      read(10) vvmean
      close(10)

      umean = umean/ntime
      vmean = vmean/ntime
      uumean = uumean/ntime
      vvmean = vvmean/ntime
      ul2mean = uumean - umean*umean
      vl2mean = vvmean - vmean*vmean
      ulvlmean = uvmean - umean*vmean

      do k=1,nz
        do j=1,(ny-1)/2
          do i=1,nx
           ul2mean(i,j,k) = 0.5*(ul2mean(i,j,k)+ul2mean(i,ny-j+1,k))
           vl2mean(i,j,k) = 0.5*(vl2mean(i,j,k)+vl2mean(i,ny-j+1,k))
           ulvlmean(i,j,k) = 0.5*(ulvlmean(i,j,k)-ulvlmean(i,ny-j+1,k))
          enddo
        enddo
      enddo

      do k=1,nz
        do j=(ny-1)/2+2,ny
          do i=1,nx
             ul2mean(i,j,k) = ul2mean(i,ny-j+1,k)
             vl2mean(i,j,k) = vl2mean(i,ny-j+1,k)
             ulvlmean(i,j,k) = -ulvlmean(i,ny-j+1,k)
          enddo
        enddo
      enddo

      do k=1,nz
        do i=1,nx
           j=(ny-1)/2+1
           ul2mean(i,j,k) = 0.5*(ul2mean(i,j-1,k)+ul2mean(i,j+1,k))
           vl2mean(i,j,k) = 0.5*(vl2mean(i,j-1,k)+vl2mean(i,j+1,k))
           ulvlmean(i,j,k) = 0.5*(vl2mean(i,j-1,k)+vl2mean(i,j+1,k))
        enddo
      enddo

      do i=1,nx
        do j=1,ny
          ul2mz(i,j) = sum(ul2mean(i,j,:))/nz
          vl2mz(i,j) = sum(vl2mean(i,j,:))/nz
          ulvlmz(i,j) = sum(ulvlmean(i,j,:))/nz
        enddo
      enddo  

      open(11,file='ul2mz',form='unformatted',status='unknown')
      write(11) ul2mz
      close(11)
      open(11,file='vl2mz',form='unformatted',status='unknown')
      write(11) vl2mz
      close(11)
      open(11,file='ulvlmz',form='unformatted',status='unknown')
      write(11) ulvlmz
      close(11)


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
