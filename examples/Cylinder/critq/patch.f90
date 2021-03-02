!********************************************************************
!
subroutine patch(cx,cy,cz,ux,uy,uz,tx,ty,tz,nx,ny,nzb,xcil,ycil)
! 
!********************************************************************
!
USE param
USE mpimod
USE swap
USE IBM
!$ use omp_lib

implicit none

real(8),dimension(nx,ny,nzb) :: cx,cy,cz,ux,uy,uz,tx,ty,tz
!real(8),dimension(ny) :: yp
real(8) :: hs,etas,gr,xg,dum,zero,halfpi,xm

integer(4) :: nx,ny,nzb,i,j,k,ig,js,imin,imax,jmin,jmax,idelta
real(8) :: a,b,c,d,xmod,xcil,ycil

real(8) , external :: epmach

integer :: longueur,num,nxcil,nycil
character(len=3) suffix
character(len=20) nfichier




idelta=int(4.*(ra*nx/xlx))
nxcil=int((cex+xcil)*nx/xlx)
nycil=int((cey+ycil)*ny/yly)
print *,'cex,cey,xcil,ycil,xlx,yly=',cex,cey,xcil,ycil,xlx,yly
print *,'idelta,nxcil,nycil=',idelta,nxcil,nycil
!stop

!dum=0.
!halfpi=0.5*pi
!zero=epmach(dum)*100
!if (nrang==0) print *,'zero = ',zero

!hs=1.
!xg=8.*hs+dx/2.
!js=1
!do while (yp(js)-hs<zero)
!   js=js+1
!enddo
!js=js-1
!ig=(xg+zero)/dx+1+1

!if (nrang==0) print *,'ig,js =',ig,js
!ig=642
!js=84




!idelta=js-6

imin=nxcil-idelta
imax=nxcil+idelta
jmin=nycil-idelta
jmax=nycil+idelta
print *,'imin,imax,jmin,jmax=',imin,imax,jmin,jmax
!stop
!if (nrang==0) then
!   do j=1,ny
!   do i=1,nx
!      xm=(i-1)*dx
!      if (i.ge.imin.and.i.le.imax.and.j.ge.jmin.and.j.le.jmax) then
!         write (101,*) i,j,xm,yp(j)
!      endif
!   enddo
!   enddo
!endif

!ordre 2 
!do k=1,nz
!do j=jmin,jmax
!do i=imin,imax
!   cx(i,j,k)=0.125*(ux(i-1,j,k)+ux(i+1,j,k)&
!                  + ux(i,j-1,k)+ux(i,j+1,k)&
!                  + ux(i,j  ,k)+ux(i,j  ,k)&
!                  + ux(i,j  ,k)+ux(i,j  ,k))
!   cy(i,j,k)=0.125*(uy(i-1,j,k)+uy(i+1,j,k)&
!                  + uy(i,j-1,k)+uy(i,j+1,k)&
!                  + uy(i,j  ,k)+uy(i,j  ,k)&
!                  + uy(i,j  ,k)+uy(i,j  ,k))
!enddo
!enddo
!enddo

!ordre 6 with alpha and beta equal zero
!a=11./16.
!b=15./32.
!b=b/2.
!c=-3./16.
!c=c/2.
!d=1./32.
!d=d/2.
a=1./2.
b=9./16.
b=b/2.
c=0.
c=c/2.
d=-1./16.
d=d/2.
!ordre 2
!a=1./2.
!b=1./2.
!b=b/2.
!c=0.
!d=0.
!$omp parallel default(shared) private(i,j,k)
!$omp do
do k=1,nzb
do j=jmin-3,jmax+3
do i=imin,imax
   tx(i,j,k)=a*ux(i,j,k)+b*(ux(i-1,j,k)+ux(i+1,j,k))&
                        +c*(ux(i-2,j,k)+ux(i+2,j,k))&
                        +d*(ux(i-3,j,k)+ux(i+3,j,k))
!   ty(i,j,k)=a*uy(i,j,k)+b*(uy(i-1,j,k)+uy(i+1,j,k))&
!                        +c*(uy(i-2,j,k)+uy(i+2,j,k))&
!                        +d*(uy(i-3,j,k)+uy(i+3,j,k))
!   tz(i,j,k)=a*uz(i,j,k)+b*(uz(i-1,j,k)+uz(i+1,j,k))&
!                        +c*(uz(i-2,j,k)+uz(i+2,j,k))&
!                        +d*(uz(i-3,j,k)+uz(i+3,j,k))
enddo
enddo
enddo
!$omp end do
!$omp end parallel
!$omp parallel default(shared) private(i,j,k)
!$omp do
do k=1,nzb
do j=jmin,jmax
do i=imin,imax
   cx(i,j,k)=a*tx(i,j,k)+b*(tx(i,j-1,k)+tx(i,j+1,k))&
                        +c*(tx(i,j-2,k)+tx(i,j+2,k))&
                        +d*(tx(i,j-3,k)+tx(i,j+3,k))
!   cy(i,j,k)=a*ty(i,j,k)+b*(ty(i,j-1,k)+ty(i,j+1,k))&
!                        +c*(ty(i,j-2,k)+ty(i,j+2,k))&
!                        +d*(ty(i,j-3,k)+ty(i,j+3,k))
!   cz(i,j,k)=a*tz(i,j,k)+b*(tz(i,j-1,k)+tz(i,j+1,k))&
!                        +c*(tz(i,j-2,k)+tz(i,j+2,k))&
!                        +d*(tz(i,j-3,k)+tz(i,j+3,k))
enddo
enddo
enddo
!$omp end do
!$omp end parallel
do k=1,nzb
do j=jmin,jmax
do i=imin,imax
   xmod=0.5*(tanh(0.5*(idelta-10-sqrt(float((i-nxcil)*(i-nxcil)+(j-nycil)*(j-nycil)))))+1.)
   !xmod=1.
   ux(i,j,k)=xmod*cx(i,j,k)+(1.-xmod)*ux(i,j,k)
!   uy(i,j,k)=xmod*cy(i,j,k)+(1.-xmod)*uy(i,j,k)
!   uz(i,j,k)=xmod*cz(i,j,k)+(1.-xmod)*uz(i,j,k)
!   uz(i,j,k)=xmod
enddo
enddo
enddo
!
!nfichier='fmod'
!num=nrang+1
!call numcar (num,suffix)
!longueur=index(nfichier,' ')-1
!nfichier=nfichier(1:longueur)//suffix
!longueur=index(nfichier,' ')-1
!open(74,file=nfichier(1:longueur),form='unformatted')
!write(74) uz
!close(74)
!
!open(75,file='tampon.avs',form='unformatted')
!write(75) uz
!close(75)
!endif


return
end subroutine patch
