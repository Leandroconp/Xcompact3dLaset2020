!
! LEANDRO PINTO
! MARCH 2021
!
!***********************************************************
!
 program dissipation
!
!***********************************************************


USE param
USE parfiX;USE parfiY;USE parfiZ
USE derivX;USE derivY;USE derivZ
USE IBM

implicit none

integer,parameter :: nx=801,ny=769,nz=128
integer,parameter :: nxm=nx-1,nym=ny-1,nzm=nz
integer,parameter :: interval=5000, pvirt=100
real(8),dimension(nx,ny,nz) :: uxf,uyf,uzf,diss1
real(8),dimension(nx,ny,nz) :: sy1,di1,di2,sz1,sy2,sy3,sy4,sy5,sy6,sy7,sy8,sy9,di11
real(8),dimension(nx,nz) :: sy
real(8),dimension(nx,ny) :: sz, uxm,uym,uzm
real(8),dimension(ny,nz):: sx
integer :: nxyz1,ijk,i,nfil,j,k,l,m,ii1,jj1,ii2,jj2,ii3,jj3,ii4,jj4
integer(4) :: nfiles, icrfile, file1, filen, ifile, dig1, dig2, dig3, dig4, dig5, dig6, dig7, dig8, dig9
character(9) :: chits
real(8), dimension(3,3,nx,ny,nz) :: A
real(8) :: zeta, zcoord, etax, etay
real(8), dimension(pvirt) :: zetax, zetay, ediss, efrontx, efronty
real(8), dimension(4) :: weight

!###1D###################################################################################
real(8),allocatable,dimension(:) :: zkz,zk2,ezs
real(8),dimension(nx) :: ffx,fcx,fbx,sfx,scx,sbx,fsx,fwx,ssx,swx,ffxp,fsxp,fwxp,sfxp,ssxp,swxp
real(8),dimension(nx) :: fifx,ficx,fibx,fiffx,fibbx,fiz1x,fiz2x
real(8),dimension(nx,2) ::filax,filaxp
real(8),dimension(nx) :: fifxp,ficxp,fibxp,fiffxp,fibbxp
real(8),dimension(ny) :: ffy,fcy,fby,sfy,scy,sby,fsy,fwy,ssy,swy,ffyp,fsyp,fwyp,sfyp,ssyp,swyp
real(8),dimension(ny) :: fify,ficy,fiby,fiffy,fibby,fiz1y,fiz2y
real(8),dimension(ny,2) ::filay,filayp
real(8),dimension(ny) :: fifyp,ficyp,fibyp,fiffyp,fibbyp
real(8),dimension(nz) :: ffz,fcz,fbz,sfz,scz,sbz,fsz,fwz,ssz,swz,ffzp,fszp,fwzp,sfzp,sszp,swzp
real(8),dimension(nz) :: fifz,ficz,fibz,fiffz,fibbz,fiz1z,fiz2z
real(8),dimension(nz,2) ::filaz,filazp
real(8),dimension(nz) :: fifzp,ficzp,fibzp,fiffzp,fibbzp
real(8),dimension(nx) :: cfx6,ccx6,cbx6,cfxp6,ciwxp6,csxp6,cwxp6,csx6,cwx6,cifx6,cicx6,cisx6
real(8),dimension(nx) :: cibx6,cifxp6,cisxp6,ciwx6,cisip6,ciwip6,cisi6,ciwi6
real(8),dimension(nx) :: cfi6,cci6,cbi6,cfip6,csip6,cwip6,csi6,cwi6,cifi6,cici6,cibi6,cifip6
real(8),dimension(ny) :: cfy6,ccy6,cby6,cfyp6,csyp6,cwyp6,csy6
real(8),dimension(ny) :: cwy6,cify6,cicy6,ciby6,cifyp6,cisyp6,ciwyp6,cisy6,ciwy6
real(8),dimension(ny) :: cfi6y,cci6y,cbi6y,cfip6y,csip6y,cwip6y,csi6y,cwi6y,cifi6y,cici6y
real(8),dimension(ny) :: cibi6y,cifip6y,cisip6y,ciwip6y,cisi6y,ciwi6y
real(8),dimension(nz) :: cfz6,ccz6,cbz6,cfzp6,cszp6,cwzp6,csz6
real(8),dimension(nz) :: cwz6,cifz6,cicz6,cibz6,cifzp6,ciszp6,ciwzp6,cisz6,ciwz6
real(8),dimension(nz) :: cfi6z,cci6z,cbi6z,cfip6z,csip6z,cwip6z,csi6z,cwi6z,cifi6z,cici6z
real(8),dimension(nz) :: cibi6z,cifip6z,cisip6z,ciwip6z,cisi6z,ciwi6z
real(8),dimension(ny) :: ppy,pp2y,pp4y
real(8),dimension(ny) :: ppyi,pp2yi,pp4yi
real(8),dimension(ny) :: yp,ypi
real(8),dimension(2) :: ja,jb
!###VARAIBLES#############################################################################


call parametre(nx,ny,nz)

call schemas(ffx,fcx,fbx,ffy,fcy,fby,ffz,fcz,fbz,sfx,scx,sbx,sfy,&
     scy,sby,sfz,scz,sbz,fsx,fwx,fsy,fwy,fsz,fwz,ssx,swx,ssy,swy,ssz,swz,&
     ffxp,fsxp,fwxp,ffyp,fsyp,fwyp,ffzp,fszp,fwzp,sfxp,ssxp,swxp,sfyp,ssyp,&
     swyp,sfzp,sszp,swzp,nx,nxm,ny,nym,nz,nzm,cfx6,ccx6,cbx6,cfxp6,&
     csxp6,cwxp6,csx6,cwx6,cifx6,cicx6,cibx6,cifxp6,cisxp6,&
     ciwxp6,cisx6,ciwx6,cfi6,cci6,cbi6,cfip6,csip6,cwip6,csi6,&
     cwi6,cifi6,cici6,cibi6,cifip6,cisip6,ciwip6,cisi6,ciwi6,&
     cfy6,ccy6,cby6,cfyp6,csyp6,cwyp6,csy6,&
     cwy6,cify6,cicy6,ciby6,cifyp6,cisyp6,ciwyp6,cisy6,ciwy6,&
     cfi6y,cci6y,cbi6y,cfip6y,csip6y,cwip6y,csi6y,cwi6y,&
     cifi6y,cici6y,cibi6y,cifip6y,cisip6y,ciwip6y,cisi6y,ciwi6y,&
     cfz6,ccz6,cbz6,cfzp6,cszp6,cwzp6,csz6,&
     cwz6,cifz6,cicz6,cibz6,cifzp6,ciszp6,ciwzp6,cisz6,ciwz6,&
     cfi6z,cci6z,cbi6z,cfip6z,csip6z,cwip6z,csi6z,cwi6z,&
     cifi6z,cici6z,cibi6z,cifip6z,cisip6z,ciwip6z,cisi6z,ciwi6z)


write (*,*) 'first file, last file'
read (*,*) file1, filen
nfiles=(int(ifin/interval))
nxyz1=nx*ny*nz


      nfil=67
      open(nfil,file='ediss.xdmf')

      write(nfil,'(A22)')'<?xml version="1.0" ?>'
      write(nfil,*)'<!DOCTYPE Xdmf SYSTEM "Xdmf.dtd" []>'
      write(nfil,*)'<Xdmf xmlns:xi="http://www.w3.org/2001/XInclude" Version="2.0">'
      write(nfil,*)'<Domain>'
      write(nfil,*)'    <Topology name="topo" TopologyType="3DRectMesh"'
      write(nfil,*)'        Dimensions="',nz,ny,nx,'">'
      write(nfil,*)'    </Topology>'
      write(nfil,*)'    <Geometry name="geo" Type="ORIGIN_DXDYDZ">'
      write(nfil,*)'        <!-- Origin -->'
      write(nfil,*)'        <DataItem Format="XML" Dimensions="3">'
      write(nfil,*)'        0.0 0.0 0.0'
      write(nfil,*)'        </DataItem>'
      write(nfil,*)'        <!-- DxDyDz -->'
      write(nfil,*)'        <DataItem Format="XML" Dimensions="3">'
      write(nfil,*)'        ',dx,dy,dz
      write(nfil,*)'        </DataItem>'
      write(nfil,*)'    </Geometry>'
      write(nfil,'(/)')
      write(nfil,*)'    <Grid Name="TimeSeries" GridType="Collection" CollectionType="Temporal">'
      write(nfil,*)'        <Time TimeType="HyperSlab">'
      write(nfil,*)'            <DataItem Format="XML" NumberType="Float" Dimensions="3">'
      write(nfil,*)'           <!--Start, Stride, Count-->'
      write(nfil,*)'            0.0',dt
      write(nfil,*)'            </DataItem>'
      write(nfil,*)'        </Time>'



do ifile = file1, filen, interval

     !IF THE DATA ARE STORED WITH 5 DIGITS, IE UX00001,UX00002,ETC.
     dig1 =   ifile/1000000 + 48
     dig2 = ( ifile - 100000000*( ifile/100000000 ) )/10000000 + 48
     dig3 = ( ifile - 10000000*( ifile/10000000 ) )/1000000 + 48
     dig4 = ( ifile - 1000000*( ifile/1000000 ) )/100000 + 48
     dig5 = ( ifile - 100000*( ifile/100000 ) )/10000 + 48
     dig6 = ( ifile - 10000*( ifile/10000 ) )/1000 + 48
     dig7 = ( ifile - 1000*( ifile/1000 ) )/100 + 48
     dig8 = ( ifile - 100*( ifile/100 ) )/10 + 48
     dig9 = ( ifile - 10*( ifile/10 ) )/1 + 48
     chits(1:9) = char(dig1)//char(dig2)//char(dig3)//char(dig4)//char(dig5)//char(dig6)//char(dig7)//char(dig8)//char(dig9)

     print *,'opening ux-'//chits//'.bin'
     open(10,file='../ux-'//chits//'.bin', FORM='UNFORMATTED',&
          access='stream')
     read(10) uxf
     close(10)
     print *,'opening uy-'//chits//'.bin'
     open(10,file='../uy-'//chits//'.bin', FORM='UNFORMATTED',&
          access='stream')
     read(10) uyf
     close(10)
     print *,'opening uz-'//chits//'.bin'
     open(10,file='../uz-'//chits//'.bin', FORM='UNFORMATTED',&
          access='stream')
     read(10) uzf
     close(10)


     print *,'Calculating fluctuating velocities' 
     ! Flutuating velocities
     do i=1,nx
        do j=1,ny
             uxm(i,j) = sum(uxf(i,j,:))/nz
             uym(i,j) = sum(uyf(i,j,:))/nz    
             uzm(i,j) = sum(uzf(i,j,:))/nz
        enddo
     enddo
     
     do i=1,nx
        do j=1,ny
           do k=1,nz
              uxf(i,j,k) = uxf(i,j,k) - uxm(i,j)
              uyf(i,j,k) = uxf(i,j,k) - uxm(i,j)
              uzf(i,j,k) = uxf(i,j,k) - uxm(i,j)
           enddo
        enddo
     enddo


     ! Derivatives
     print *, 'Calculate derivatives'
     call derx (sy1,uxf,di1,sx,ffxp,fsxp,fwxp,nx,ny,nz,1) !dudx
     call derx (sy2,uyf,di1,sx,ffxp,fsxp,fwxp,nx,ny,nz,1) !dvdx
     call derx (sy3,uzf,di1,sx,ffxp,fsxp,fwxp,nx,ny,nz,1) !dwdx
     call dery (sy4,uxf,di1,di2,sy,ffyp,fsyp,fwyp,ppy,nx,ny,nz,1) !dudy
     call dery (sy5,uyf,di1,di2,sy,ffyp,fsyp,fwyp,ppy,nx,ny,nz,1) !dvdy
     call dery (sy6,uzf,di1,di2,sy,ffyp,fsyp,fwyp,ppy,nx,ny,nz,1) !dwdy
     call derz (sy7,uxf,di11,sz,ffz,fsz,fwz,nx,ny,nz,0) !dudz
     call derz (sy8,uyf,di11,sz,ffz,fsz,fwz,nx,ny,nz,0) !dvdz
     call derz (sy9,uzf,di11,sz,ffz,fsz,fwz,nx,ny,nz,0) !dwdz

    !INSTANTANEOUS DISSIPATION RATE
    diss1=0.0D0
    A(:,:,:,:,:)=0.0D0
    A(1,1,:,:,:)=sy1(:,:,:) !du/dx=sy1
    A(2,1,:,:,:)=sy2(:,:,:) !dv/dx=sy2
    A(3,1,:,:,:)=sy3(:,:,:) !dw/dx=tc1
    A(1,2,:,:,:)=sy4(:,:,:) !du/dy=td1
    A(2,2,:,:,:)=sy5(:,:,:) !dv/dy=te1
    A(3,2,:,:,:)=sy6(:,:,:) !dw/dy=tf1
    A(1,3,:,:,:)=sy7(:,:,:) !du/dz=tg1
    A(2,3,:,:,:)=sy8(:,:,:) !dv/dz=th1
    A(3,3,:,:,:)=sy9(:,:,:) !dw/dz=ti1
     
   !  do k=1,nz
   !    do j=1,ny
   !       do i=1,nx
   !          diss1(i,j,k)=uxf(i,j,k)
   !       enddo
   !    enddo
   !  enddo



    ! Local dissipation rate of kinetic energy for three-dimensional case
  !  do k=1,nz
  !     do j=1,ny
  !        do i=1,nx
  !           do m=1,3
  !              do l=1,3
  !                      diss1(i,j,k)=diss1(i,j,k)+2.*xnu*0.5*0.5*(A(l,m,i,j,k)+A(m,l,i,j,k))**2.0
  !              enddo
  !           enddo
  !        enddo
  !     enddo
  !  enddo

   ! Local dissipation rate of kinetic energy for homogeneous turbulence
   do k=1,nz
      do j=1,ny
         do i=1,nx
            do m=1,3
               do l=1,3
                       diss1(i,j,k)=diss1(i,j,k)+xnu*A(l,m,i,j,k)**2.0D0
               enddo
            enddo
         enddo
      enddo
   enddo
     
     print *, 'Saving dissipation rate energy '
     open(11,file='ediss'//chits,form='unformatted',status='unknown')
     write(11) diss1
     close(11)

     write(nfil,'(/)')
     write(nfil,*)'        <Grid Name="'//chits//'" GridType="Uniform">'
     write(nfil,*)'            <Topology Reference="/Xdmf/Domain/Topology[1]"/>'
     write(nfil,*)'            <Geometry Reference="/Xdmf/Domain/Geometry[1]"/>'
!SINGLE PRECISION-->Precision=4
!DOUBLE PRECISION-->Precision=8
     write(nfil,*)'            <Attribute Name="ediss" Center="Node">'
     write(nfil,*)'               <DataItem Format="Binary" '
     write(nfil,*)'                DataType="Float" Precision="8" Endian="little" Seek="4"'
     write(nfil,*)'                Dimensions="',nz,ny,nx,'">'
     write(nfil,*)'                  ediss'//chits
     write(nfil,*)'               </DataItem>'
     write(nfil,*)'            </Attribute>'

     write(nfil,*)'        </Grid>'


     ! Interpolation of dissipation rate energy to the cylinder surface
     ediss(:) = 0.0D0
     do k=1,nz
        zcoord=(k)*dz
        do i=1,pvirt
           zeta = 2.0D0*i*acos(-1.0)/pvirt
           zetax(i) = cex + 0.5D0*cos(zeta)
           zetay(i) = cey + 0.5D0*sin(zeta)

           etax=zetax(i)/dx - floor(zetax(i)/dx)
           etay=zetay(i)/dy - floor(zetay(i)/dy)

           ! Coordinate system
           ii1=floor(zetax(i)/dx)+1
           jj1=floor(zetay(i)/dy)+1
           ii2=ii1+1
           jj2=jj1
           ii3=ii1
           jj3=jj1+1
           ii4=ii1+1
           jj4=jj1+1

           ! Weights to compose the dissipation rate at the surface cylinder
           weight(1) = (1-etax)*(1-etay)
           weight(2) = etax*(1-etay)
           weight(3) = (1-etax)*etay
           weight(4) = etax*etay

           ! Interpolation of Ediss to the surface cylinder
           ediss(i) = ediss(i) + diss1(ii1,jj1,k)*weight(1) + diss1(ii2,jj2,k)*weight(2) + diss1(ii3,jj3,k)*weight(3)+&
                   diss1(ii4,jj4,k)*weight(4)
           
        enddo
     enddo
     
     
     open(1,file='output'//chits//'.txt')
     do i=1,pvirt
        ediss(i)=ediss(i)/nz
        zeta = 2.0D0*i*acos(-1.0D0)/pvirt
        zetax(i) = cex + 0.5D0*cos(zeta)
        zetay(i) = cey + 0.5D0*sin(zeta)
        ! Matlab parameters for use quiver function
        efrontx(i) = ediss(i)*cos(zeta)
        efronty(i) = ediss(i)*sin(zeta)
        write(1,2) zetax(i),zetay(i),efrontx(i),efronty(i)
     enddo
     close(1)
     2 format (4F8.3)


enddo
write(nfil,'(/)')
write(nfil,*)'    </Grid>'
write(nfil,*)'</Domain>'
write(nfil,'(A7)')'</Xdmf>'
close(nfil)

end program dissipation
