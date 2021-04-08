function ekdiss

clc
clear

ediss=load('output.txt');

quiver(ediss(:,1),ediss(:,2),ediss(:,3),ediss(:,4),'k','MaxHeadSize',0.1,'LineWidth', 1.0)
axis([7 9 8 10]);
axis equal tight;
axis([7 9 8 10]);

end