module param
  integer :: nclx,ncly,nclz
  integer :: iswap,ifiltre, ivirtuel,istret,iforc_entree,iturb,movel
  integer :: iecoule, iskew, ientree, nschema, idebut, ifin, iles
  integer :: isave,ilit,idebmod, imodulo, idemarre, icommence, irecord
  integer :: nxboite, istat,iread,iavance_temps
  integer :: cont1,contplan,contsauve,contmoyt 
  real(8) :: xlx,yly,zlz,dx,dy,dz,dx2,dy2,dz2
  real(8) :: dt,xnu,bruit,pi,twopi,u1,u2,beta,v
  character :: filecharge*80, filesauve*80, filebruit*80, nchamp*80,filepath*80, fileturb*80 
  real(8),dimension(5) :: adt,bdt,gdt
end module param

module IBM
  real(8) :: cex,cey,cez,ra,xmovi,ymovi,freqmovi
  integer :: icvlf,icvrt,jcvlw,jcvup
  integer :: i5,jj,j1,j2,i55
end module IBM

module derivX
  real(8) :: alcaix6,acix6,bcix6
  real(8) :: ailcaix6, aicix6, bicix6 
  real(8) :: alfa1x,af1x,bf1x,cf1x,df1x,alfa2x,af2x,alfanx,afnx,bfnx
  real(8) :: cfnx,dfnx,alfamx,afmx,alfaix,afix,bfix,alsa1x,as1x,bs1x
  real(8) :: cs1x,ds1x,alsa2x,as2x,alsanx,asnx,bsnx,csnx,dsnx,alsamx
  real(8) :: asmx,alsaix,asix,bsix,csix,as3x,bs3x,astx,bstx
end module derivX

module derivY 
  real(8) :: alcaiy6,aciy6,bciy6
  real(8) :: ailcaiy6, aiciy6, biciy6 
  real(8) :: alfa1y,af1y,bf1y,cf1y,df1y,alfa2y,af2y,alfany,afny,bfny
  real(8) :: cfny,dfny,alfamy,afmy,alfajy,afjy,bfjy,alsa1y,as1y,bs1y
  real(8) :: cs1y,ds1y,alsa2y,as2y,alsany,asny,bsny,csny,dsny,alsamy
  real(8) :: asmy,alsajy,asjy,bsjy,csjy,as3y,bs3y,asty,bsty 
end module derivY

module derivZ
  real(8) :: alcaiz6,aciz6,bciz6 
  real(8) :: ailcaiz6, aiciz6, biciz6 
  real(8) :: alfa1z,af1z,bf1z,cf1z,df1z,alfa2z,af2z,alfanz,afnz,bfnz
  real(8) :: cfnz,dfnz,alfamz,afmz,alfakz,afkz,bfkz,alsa1z,as1z,bs1z
  real(8) :: cs1z,ds1z,alsa2z,as2z,alsanz,asnz,bsnz,csnz,dsnz,alsamz
  real(8) :: asmz,alsakz,askz,bskz,cskz,as3z,bs3z,astz,bstz 
end module derivZ

module parfiX
  real(8) :: fia1x, fib1x, fic1x, fid1x, fie1x, fia2x, fib2x, fic2x, fid2x
  real(8) :: fie2x, fia3x, fib3x, fic3x, fid3x, fie3x, fianx, fibnx, ficnx, fidnx
  real(8) :: fienx, fiamx, fibmx, ficmx, fidmx, fiemx, fiapx, fibpx, ficpx, fidpx
  real(8) :: fiepx, fiaix, fibix, ficix, fidix, fialx, fibex, fih1x, fih2x, fih3x,fih4x 
end module parfiX
!
module parfiY
  real(8) :: fia1y, fib1y, fic1y, fid1y, fie1y, fia2y, fib2y, fic2y, fid2y
  real(8) :: fie2y, fia3y, fib3y, fic3y, fid3y, fie3y, fiany, fibny, ficny, fidny
  real(8) :: fieny, fiamy, fibmy, ficmy, fidmy, fiemy, fiapy, fibpy, ficpy, fidpy
  real(8) :: fiepy, fiaiy, fibiy, ficiy, fidiy, fialy, fibey, fih1y, fih2y, fih3y,fih4y 
end module parfiY

module parfiZ
  real(8) :: fia1z, fib1z, fic1z, fid1z, fie1z, fia2z, fib2z, fic2z, fid2z
  real(8) :: fie2z, fia3z, fib3z, fic3z, fid3z, fie3z, fianz, fibnz, ficnz, fidnz
  real(8) :: fienz, fiamz, fibmz, ficmz, fidmz, fiemz, fiapz, fibpz, ficpz, fidpz
  real(8) :: fiepz, fiaiz, fibiz, ficiz, fidiz, fialz, fibez, fih1z, fih2z, fih3z,fih4z 
end module parfiZ

module swap
  integer :: swap_xz_to_sendtype, swap_xz_to_recvtype
  integer :: swap_to_xz_sendtype, swap_to_xz_recvtype
end module swap

module mpimod
  integer :: nrang,nproc
  integer, allocatable :: jbegin(:),kbegin(:)
end module mpimod
