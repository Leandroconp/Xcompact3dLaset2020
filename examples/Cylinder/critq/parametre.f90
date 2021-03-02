!
!********************************************************************
!
subroutine parametre(nx,ny,nz) 
!
!********************************************************************
  
USE param
USE IBM 
USE mpimod

implicit none

integer :: nx,ny,nz,longueur 
real(8) :: re, sc, theta, dtv, cfl,cf2 
character :: a*80
 
pi=acos(-1.) 
twopi=2.*acos(-1.)

1000 format(a,80x) 
1003 format(a,80x)
open(10,file='incompact3d.prm',status='unknown',form='formatted') 
read (10,1000) a 
read (10,1000) a 
read (10,1000) a 
read (10,*) xlx,a
read (10,*) yly 
read (10,*) zlz 
read (10,*) re 
read (10,*) u1 
read (10,*) u2 
read (10,*) bruit 
read (10,*) dt
read (10,1000) a 
read (10,1000) a 
read (10,1000) a 
read (10,*) nclx 
read (10,*) ncly 
read (10,*) nclz 
read (10,*) iecoule 
read (10,*) ientree
read (10,*) idebut
read (10,*) ifin
read (10,*) nschema
read (10,*) istret
read (10,*) beta
read (10,*) iswap
read (10,*) iskew
read (10,*) ifiltre
read (10,1000) a 
read (10,1000) a 
read (10,1000) a 
read (10,1003) filepath
read (10,*) ilit 
read (10,1003) filecharge 
read (10,*) isave
read (10,*) imodulo
read (10,1003) nchamp 
read (10,1003) filebruit 
read (10,*) icommence
read (10,1003) fileturb
read (10,1000) a 
read (10,1000) a 
read (10,1000) a 
read (10,*) ivirtuel
read (10,*) cex 
read (10,*) cey 
read (10,*) cez 
read (10,*) ra
read (10,1000) a
read (10,1000) a
read (10,1000) a
read (10,*) movel
read (10,*) xmovi
read (10,*) ymovi
read (10,*) freqmovi
close(10) 

if (nrang==0) then
   if (iecoule.eq.1) print *,'Constant flow field'
   if (iecoule.eq.2) print *,'Mixing layer hyperbolic tangent profile'
   if (iecoule.eq.3) print *,'Wake flow'
   if (iecoule.eq.4) print *,'Mixing layer with splitter plate'
   if (iecoule.eq.5) print *,'Channel flow'
   if (iecoule.eq.6) print *,'Taylor Green vortices'
   if (iecoule.eq.7) print *,'Cavity flow'
   if (iecoule.eq.8) print *,'Flat plate Boundary layer'
   if (iecoule.eq.9) print *,'Water tank'
   write(*,1101) nx,ny,nz
   write(*,1103) xlx,yly,zlz 
   write(*,1102) nclx,ncly,nclz 
   write(*,1104) u1,u2 
   write(*,1105) re
   write(*,1111) bruit 
   write(*,1106) dt
   if (nschema.eq.1) print *,'Temporal scheme   : Adams-bashforth 2'
   if (nschema.eq.2) print *,'Temporal scheme   : Runge-Kutta 3'
   if (nschema.eq.3) print *,'Temporal scheme   : Runge-Kutta 4'
   if (ivirtuel.eq.0) print *,'Immersed boundary : off'
   if (ivirtuel.eq.1) then
      print *,'Immersed boundary : on'
      write(*,1107) cex,cey,cez
!      write(*,1108) ra
   endif
   if (ifiltre.eq.1) print *,'Filter            : on'
   if (ifiltre.eq.0) print *,'Filter            : off'

 1101 format(' Spatial Resolution: (nx,ny,nz)=(',I4,',',I4,',',I4,')')
 1102 format(' Boundary condition: (nclx,ncly,nclz)=(',I1,',',I1,',',I1,')')
 1103 format(' Domain dimension  : (lx,ly,lz)=(',F6.1,',',F6.1,',',F6.1,')')
 1104 format(' High and low speed: u1=',F6.2,' and u2=',F6.2)
 1105 format(' Reynolds number Re: ',F8.0)
 1106 format(' Time step dt      : ',F10.4)
 1107 format(' Object centred at : (',F6.2,',',F6.2,',',F6.2,')')
 1111 format(' Turbulence intensity: ',F10.4)
 1110 format(' Object length     : ',F6.2)
endif

xnu=1./re 
   
if (nclx==0) dx=xlx/nx 
if (nclx==1 .or. nclx==2) dx=xlx/(nx-1.) 
if (ncly==0) dy=yly/ny 
if (ncly==1.or.ncly==2) dy   =yly/(ny-1.) 
if (nz>1) then 
   if (nclz==0) dz=zlz/nz 
   if (nclz==1.or.nclz==2) dz=zlz/(nz-1.) 
else 
   dz=zlz 
endif

dx2=dx*dx 
dy2=dy*dy 
dz2=dz*dz 
cfl=u1*dt/dx 
cf2=dt/(dx*dx*re)
if (nrang==0) then
   write(*,1108)cfl
   write(*,1109)cf2
 1108 format(' Convective criteria (CFL)  :',F10.6)
 1109 format(' Diffusion criteria         :',F10.6)
   if (nschema.eq.1) then
      if ((cfl.gt.sqrt(2.)/1.989).or.cf2.gt.2./6.857) then
         print *,'Stability condition not respected'
         stop
      endif
   endif
   if (nschema.eq.2) then
      if ((cfl.gt.sqrt(3.)/1.989).or.cf2.gt.2.5/6.857) then
         print *,'Stability condition not respected'
         stop
      endif
   endif
   if (nschema.eq.3) then
      if ((cfl.gt.2.85/1.989).or.cf2.gt.2.9/6.857) then
         print *,'Stability condition not respected'
         stop
      endif
   endif
endif
longueur=index(filebruit,' ')-1 
if (ientree==2) open(40,file=filebruit(1:longueur),form='unformatted',status='unknown') 
longueur=index(filesauve,' ')-1 
if (nxboite/=0 .or. ientree==3) open(50,file=filesauve(1:longueur),form='unformatted',status='unknown') 

!******************************************************************
!
!**avancement en temps***1=AB2***2=RK3***3=RK4C&K****************** 
!
!******************************************************************

adt(:)=0. ; bdt(:)=0. ; gdt(:)=0.
if (nschema==1) then!AB2
   iavance_temps=1 
   adt(1)=1.5*dt
   bdt(1)=-0.5*dt
   gdt(1)=adt(1)+bdt(1)
endif
if (nschema==2) then !RK3
   iavance_temps=3 
   adt(1)=(8./15.)*dt
   bdt(1)=0.
   gdt(1)=adt(1)
   adt(2)=(5./12.)*dt
   bdt(2)=(-17./60.)*dt
   gdt(2)=adt(2)+bdt(2)
   adt(3)=(3./4.)*dt
   bdt(3)=(-5./12.)*dt
   gdt(3)=adt(3)+bdt(3)
endif
if (nschema==3) then !RK4 Carpenter and Kennedy  
   iavance_temps=5 
   adt(1)=0.
   adt(2)=-0.4178904745
   adt(3)=-1.192151694643
   adt(4)=-1.697784692471
   adt(5)=-1.514183444257
   bdt(1)=0.1496590219993
   bdt(2)=0.3792103129999
   bdt(3)=0.8229550293869
   bdt(4)=0.6994504559488
   bdt(5)=0.1530572479681
   gdt(1)=0.1496590219993*dt
   gdt(2)=0.220741935365*dt
   gdt(3)=0.25185480577*dt
   gdt(4)=0.33602636754*dt
   gdt(5)=0.041717869325*dt
endif

return  
end subroutine parametre
