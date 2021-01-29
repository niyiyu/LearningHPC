log = load('./log.dat');
A = [log(1,:);log(2,:)];
x_real = log(3,:);
b = log(4,:) ;
xy =log(5:end,:);
x = linspace(1,100,100);
y = x;
for i =1:100
    for j =1:100
        M(j,i) = 0.5 * [x(i),y(j)]*A*[x(i),y(j)]' - b*[x(i),y(j)]' ;
    end
end
contour(M,100);hold on
for i =1:length(xy)-1
    plot(xy(i,1),xy(i,2),'ro','MarkerSize',10*(length(xy)-i+1)/length(xy));hold on;
    line([xy(i,1),xy(i+1,1)],[xy(i,2),xy(i+1,2)],'Color','blue','LineStyle','--','LineWidth',2)
end
plot(x_real(1),x_real(2),'ks','MarkerSize',10);hold on;

%%
misfit = load('./misfit.dat');
figure()
plot(misfit)