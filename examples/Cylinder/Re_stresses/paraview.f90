  program paraview_raw

  implicit none

  integer(4) :: nx , ny, nz
  real(4) :: dx, dy, dz, dt
  real(4) :: lx, ly, lz
  integer(4) :: nfiles, icrfile, file1, filen, ifile, dig1, dig2, dig3
  integer(4) :: i, j, k, nfil
  character(3) :: chits

  write (*,*) 'nx, ny, nz   - Incompact3D'
  read (*,*) nx, ny, nz
  write (*,*) 'lx, ly, lz   - Incompact3D'
  read (*,*) lx, ly, lz

  ! mesh generation

  dx = lx/real ( ( nx - 1 ), 4 )
  dy = ly/real ( ( ny - 1 ), 4 )
  dz = lz/real ( ( nz - 1 ), 4 )
  if ( nz .eq. 1 ) dz = 0.0

  nfil=67
  open(nfil,file='Restress.xdmf')

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

 ! do ifile = file1, filen, icrfile

 !   dig1 =   ifile/100 + 48
 !   dig2 = ( ifile - 100*( ifile/100 ) )/10 + 48
 !   dig3 = ( ifile - 10*( ifile/10 ) )/1 + 48
 !   chits(1:3) = char(dig1)//char(dig2)//char(dig3)
 !   write(*,*) ifile, 'file'//chits

    write(nfil,'(/)')
    write(nfil,*)'        <Grid Name="Re_stresses" GridType="Uniform">'
    write(nfil,*)'            <Topology Reference="/Xdmf/Domain/Topology[1]"/>'
    write(nfil,*)'            <Geometry Reference="/Xdmf/Domain/Geometry[1]"/>'

    write(nfil,*)'            <Attribute Name="ul2mean" Center="Node">'
    write(nfil,*)'               <DataItem Format="Binary" '
    write(nfil,*)'                DataType="Float" Precision="8" Endian="little" Seek="4"'
    write(nfil,*)'                Dimensions="',nz,ny,nx,'">'
    write(nfil,*)'                  ul2mz'
    write(nfil,*)'               </DataItem>'
    write(nfil,*)'            </Attribute>'

    write(nfil,*)'            <Attribute Name="vl2mean" Center="Node">'
    write(nfil,*)'               <DataItem Format="Binary" '
    write(nfil,*)'                DataType="Float" Precision="8" Endian="little" Seek="4"'
    write(nfil,*)'                Dimensions="',nz,ny,nx,'">'
    write(nfil,*)'                  vl2mz'
    write(nfil,*)'               </DataItem>'
    write(nfil,*)'            </Attribute>'

    write(nfil,*)'            <Attribute Name="ulvlmean" Center="Node">'
    write(nfil,*)'               <DataItem Format="Binary" '
    write(nfil,*)'                DataType="Float" Precision="8" Endian="little" Seek="4"'
    write(nfil,*)'                Dimensions="',nz,ny,nx,'">'
    write(nfil,*)'                  ulvlmz'
    write(nfil,*)'               </DataItem>'
    write(nfil,*)'            </Attribute>'

    write(nfil,*)'        </Grid>'

 ! enddo
  write(nfil,'(/)')
  write(nfil,*)'    </Grid>'
  write(nfil,*)'</Domain>'
  write(nfil,'(A7)')'</Xdmf>'
  close(nfil)

  end program paraview_raw
