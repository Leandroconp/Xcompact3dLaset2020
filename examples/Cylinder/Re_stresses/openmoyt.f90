      program openmoyt

      implicit none
      
      
      integer,parameter :: nx=201,ny=193,nz=32
      real(8),parameter :: xlx=20.,yly=18.,zlz=6.
      real(8) :: ntime, maxlengthul2, maxlengthvl2, maxlengthulvl
      real(8) :: loc1ul2, loc2ul2, loc1vl2, loc2vl2, loc1ulvl, loc2ulvl, dx, dy
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

      dx = xlx/real((nx-1),8)
      dy = yly/real((ny-1),8)

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
      open(10,file='../uvmean.dat0'//chits, FORM='UNFORMATTED',&
          access='stream')
      read(10) uvmean
      close(10)

      umean = umean/ntime
      vmean = vmean/ntime
      uumean = uumean/ntime
      vvmean = vvmean/ntime
      uvmean = uvmean/ntime

      ul2mean(:,:,:) = uumean(:,:,:) - umean(:,:,:)*umean(:,:,:)
      vl2mean(:,:,:) = vvmean(:,:,:) - vmean(:,:,:)*vmean(:,:,:)
      ulvlmean(:,:,:) = uvmean(:,:,:) - umean(:,:,:)*vmean(:,:,:)

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
           ulvlmean(i,j,k) = 0.5*(ulvlmean(i,j-1,k)+ulvlmean(i,j+1,k))
        enddo
      enddo
      
      !Calculate the z average and the length formation
      maxlengthul2=0.0D0; maxlengthvl2=0.0D0; maxlengthulvl=0.0D0
      do i=1,nx
        do j=1,ny
          ul2mz(i,j) = sum(ul2mean(i,j,:))/nz
          vl2mz(i,j) = sum(vl2mean(i,j,:))/nz
          ulvlmz(i,j) = sum(ulvlmean(i,j,:))/nz
          if (ul2mz(i,j).gt.maxlengthul2) then   ! Length formation vortex
            maxlengthul2=ul2mz(i,j)
            loc1ul2=(i-1)*dx
            loc2ul2=(j-1)*dy
          endif
          if (vl2mz(i,j).gt.maxlengthvl2) then   ! Length formation vortex
            maxlengthvl2=vl2mz(i,j)
            loc1vl2=(i-1)*dx
            loc2vl2=(j-1)*dy
          endif
          if (ulvlmz(i,j).gt.maxlengthulvl) then   ! Length formation vortex
            maxlengthulvl=ulvlmz(i,j)
            loc1ulvl=(i-1)*dx
            loc2ulvl=(j-1)*dy
          endif
        enddo
      enddo 


      open(3,file='Lful2-'//chits//'.txt') 
      write(3,4) loc1ul2, loc2ul2, maxlengthul2
      close(3)
      open(3,file='Lfvl2-'//chits//'.txt') 
      write(3,4) loc1vl2, loc2vl2, maxlengthvl2
      close(3)
      open(3,file='Lfulvl-'//chits//'.txt') 
      write(3,4) loc1ulvl, loc2ulvl, maxlengthulvl
      close(3)
      open(11,file='ul2mz',form='unformatted',status='unknown')
      write(11) ul2mz
      close(11)
      open(11,file='vl2mz',form='unformatted',status='unknown')
      write(11) vl2mz
      close(11)
      open(11,file='ulvlmz',form='unformatted',status='unknown')
      write(11) ulvlmz
      close(11)
      open(11,file='ul2mz2',form='unformatted',status='unknown')
      write(11) ul2mz
      close(11)
      open(11,file='vl2mz2',form='unformatted',status='unknown')
      write(11) vl2mz
      close(11)
      open(11,file='ulvlmz2',form='unformatted',status='unknown')
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
      4 format (3F8.3) 
          
      end program openmoyt
