function envoltoria

clc
clear

ediss=load('output.txt');

quiver(ediss(:,1),ediss(:,2),ediss(:,3),ediss(:,4),'k','MaxHeadSize',0.1...
       'LineWidth', 1.0)
axis([7 9 8 10]);
axis equal tight;
axis([7 9 8 10]);

N = length(dir)

nleft=3;
nright=3;
ndown=3;
nup=3;
dti=0.002
dt=dti*500;
AD = ('../../dados/sauve/');
DA = ('../../press_contour/envoltoria');
cd(AD)
file = dir;
N = length(file)
nx=801; ny=769; nz=2; np=1; x0=0; xf=20; y0=0; yf=18;cex=0.;cey=0.;xc=0.;yc=0.;cexx=8.;ceyy=9.;
xlx=20; yly=18; dx=xlx/(nx-1); dy=yly/(ny-1);pz=2;nxm=nx-1;nym=ny-1;npz=2;
nxm=nx-1; nym=ny-1;nzm=nz;
B=0.0;C=0.;fo=0.;
%p=zeros(nxm,nym);
p=zeros(nx,ny);
for a = 122:150%N-2
if a > 1
cd(AD);
end
fname=file(a+2).name; 

fid = fopen(fname,'r','native');      % open the binary file to read
[u, count2] = fread(fid,Inf,'real*4');   % read the binary file
fclose(fid);                             % close the binary file
cd(DA);
%  var1 = u(2:(nx*ny*nz)+1);             % exclude the first and last point
%  var2 = u((nx*ny*nz)+2:2*(nx*ny*nz)+1);   % exclude the first and last point
%  var3 = u(2*(nx*ny*nz)+2:3*(nx*ny*nz)+1);
var4 = u(3*(nx*ny*nz)+nxm*nym*nzm+2:4*(nx*ny*nz)+nxm*nym*nzm+1);
P1 =  reshape(var4,nx,ny,nz);
%for i=1:nz
    
p =P1(:,:,npz);
P = p;

z=linspace(-9.,yly-9.,ny);             %define os valores do eixo Z dos graficos
x=linspace(-8.,xlx-8.,nx);                 %define os valores do eixo X dos graficos

p=P./dti;
pmax = max(max(p));
pmin = min(min(p));
   

pvirt=100;
%xc=B*cos(4*pi*fo*dt*a);
%yc=C*sin(2*pi*fo*dt*a);
xc=xcil(cont,1);
yc=ycil(cont,1);

for ii=1:pvirt
  zeta=(2*ii*pi)/pvirt;
  zetax(ii)=cexx+0.5*cos(zeta)+xc;
  zetay(ii)=ceyy+0.5*sin(zeta)+yc;
end


for k=1:pvirt
    zeta=(2*k*pi)/pvirt;
    angulo(k)=zeta*180/pi;
    etax=zetax(k)/dx - floor(zetax(k)/dx);
    etay=zetay(k)/dy - floor(zetay(k)/dy);

% Coordenadas do sistema

         i1=floor(zetax(k)/dx)+1;
         j1=floor(zetay(k)/dy)+1;
         i2=i1+1;
         j2=j1;
         i3=i1;
         j3=j1+1;
         i4=i1+1;
         j4=j1+1;
         
% Pesos para compor a pressão na fronteira

         weight(1) = (1-etax)*(1-etay);
         weight(2) = etax*(1-etay);
         weight(3) = (1-etax)*etay;
         weight(4) = etax*etay;
         
% Cálculo da pressão no ponto de fronteira

         pp1(k)=p(i1,j1);
         pfront(k)=weight(1)*p(i1,j1)+weight(2)*p(i2,j2)+weight(3)*p(i3,j3)+weight(4)*p(i4,j4);
         pfrontx(k)=-pfront(k)*cos(zeta);
         pfronty(k)=-pfront(k)*sin(zeta);
         
         
end
for ii=1:pvirt
  zeta=(2*ii*pi)/pvirt;
  zetax(ii)=cex+0.5*cos(zeta)+xc;
  zetay(ii)=cey+0.5*sin(zeta)+yc;
end


set(gcf,'visible','off');
%quiver(zetax,zetay,pfrontx,pfronty,'k') %plota o campo de pressão na superficie do cilindro
quiver(zetax,zetay,.2*pfrontx,.2*pfronty,'k','MaxHeadSize',0.05...
       ,'AutoScale','off') %plota o campo de pressão na superficie do cilindro
hold on
drag(cont,1)
lift(cont,1)
quiver(xcil(cont,1),ycil(cont,1),0.1*drag(cont,1),0.,'k','MaxHeadSize',0.5...
       ,'AutoScale','off')
quiver(xcil(cont,1),ycil(cont,1),0.,0.1*lift(cont,1),'k','MaxHeadSize',0.5...
       ,'AutoScale','off')
%quiver(xcil(cont,1),ycil(cont,1),0.1*drag(cont,1),0.1*lift(cont,1),'k','AutoScale','off')
%pause
pvirt=1000;
for ii=1:pvirt
  zeta=(2*ii*pi)/pvirt;
  zetax(ii)=cex+0.5*cos(zeta)+xc;
  zetay(ii)=cey+0.5*sin(zeta)+yc;
end

plot(zetax,zetay,'k')
plot(xcil(initial:cont,1),ycil(initial:cont,1),'k');
plot(xcil(cont,1),ycil(cont,1),'MarkerFaceColor',[0 0 0],'MarkerSize',6,...
    'Marker','o',...
    'Color',[0 0 0]);

    f='t=';
    g=num2str(temp);
    time=[f g];
    annotation('textbox','String',{time},'FontSize',12,...
    'FontName','Times',...
    'FitBoxToText','off',...
    'LineStyle','none',...
    'Position',[0.17 0.94 0.954898809523809,...
                0.05714]);
    temp=temp+dt;

      
cont=cont+step;
clear zetax zetay

P=p';
%v1 = linspace(-0.5,0.5,15);                      % determina o numero de isolinhas positivas
%[C,h] = contour(x,z,P,[-5:.2:5]);                             %traca as isolinhas positivas da pressão
C = contour(x,z,P,[-5:.4:5]);

%colorbar('vert','YTickLabel',{'-0.5','-.25','0','.25','.5'})                               %orientacao da paleta de cores
%cmpdir=('/media/disk4/simulacoes3d/viv2D/Re1250/XXI/folder_analise3/vorticity_xy/');
%cmp=load([cmpdir 'cmp128' ]);                  %carrega a paleta de cores
%colormap(cmp);
%colorbar('vert')     
%pause

%text_handle = clabel(C,h);
%clabel(C,h)
%clabel(C,h,'LabelSpacing',72)
%clabel(C,h,'FontSize',7,'Color','k','Rotation',90)
clabel(C,'FontSize',7)

axis equal tight; 
axis([-1 3. -2 2]);                           %define a area de plotagem
xlabel('x','FontSize',22)                     %define o eixo X
ylabel('y','FontSize',22)
grid on
%pause
%h = colorbar;
%set(h, 'ylim', [-0.5 0.5])
%pause
h=gcf;
print(h,'-djpeg','-r500',fname);                    
close                  
end
end    
