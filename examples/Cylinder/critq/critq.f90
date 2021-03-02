!
! LEANDRO PINTO
! MARCH 2021
!
!***********************************************************
!
 program critq
!
!***********************************************************


USE param
USE parfiX;USE parfiY;USE parfiZ
USE derivX;USE derivY;USE derivZ

implicit none

integer,parameter :: nx=801,ny=769,nz=128
integer,parameter :: nxm=nx-1,nym=ny-1,nzm=nz
integer,parameter :: interval=5000.
real(8),dimension(nx,ny,nz) :: uxf,uyf,uzf
real(8),dimension(nx,ny,nz) :: sy1,di1,di2,sz1,sy2,sy3,sy4,sy5,sy6,sy7,sy8,sy9,di11
real(8),dimension(nx,nz) :: sy
real(8),dimension(nx,ny) :: sz
real(8),dimension(ny,nz):: sx
integer :: nxyz1,ijk,i,nfil
integer(4) :: nfiles, icrfile, file1, filen, ifile, dig1, dig2, dig3, dig4, dig5, dig6, dig7, dig8, dig9
character(9) :: chits

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
      open(nfil,file='qc.xdmf')

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
     
  

     !call patch(sy1,sy2,sy3,uxf,uyf,uzf,sy4,sz1,di1,nx,ny,nz,xcil,ycil)

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

     
     print *, 'Calculate Q-criterium'
     do ijk=1,nxyz1
        di1(ijk,1,1)=-0.5*(sy1(ijk,1,1)**2+sy5(ijk,1,1)**2+sy9(ijk,1,1)**2)-&
             sy4(ijk,1,1)*sy2(ijk,1,1)-sy7(ijk,1,1)*sy3(ijk,1,1)-sy8(ijk,1,1)*sy6(ijk,1,1)
     enddo

     print *, 'Saving Q-criterium'
     open(11,file='qc'//chits,form='unformatted',status='unknown')
     write(11) di1
     close(11)

     write(nfil,'(/)')
     write(nfil,*)'        <Grid Name="'//chits//'" GridType="Uniform">'
     write(nfil,*)'            <Topology Reference="/Xdmf/Domain/Topology[1]"/>'
     write(nfil,*)'            <Geometry Reference="/Xdmf/Domain/Geometry[1]"/>'
!SINGLE PRECISION-->Precision=4
!DOUBLE PRECISION-->Precision=8
     write(nfil,*)'            <Attribute Name="qc" Center="Node">'
     write(nfil,*)'               <DataItem Format="Binary" '
     write(nfil,*)'                DataType="Float" Precision="8" Endian="little" Seek="4"'
     write(nfil,*)'                Dimensions="',nz,ny,nx,'">'
     write(nfil,*)'                  qc'//chits
     write(nfil,*)'               </DataItem>'
     write(nfil,*)'            </Attribute>'

     write(nfil,*)'        </Grid>'

enddo
write(nfil,'(/)')
write(nfil,*)'    </Grid>'
write(nfil,*)'</Domain>'
write(nfil,'(A7)')'</Xdmf>'
close(nfil)

end program critq
