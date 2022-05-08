load('amazon_papa.mat')
load('coast')
figure;set(gcf,'position',[10 10 1400 600])
aa=0;
main_labels={'1916','1926*','1963','1983*','1998*','2005','2010','2016*'};
for year=[1916,1926,1963,1983,1998,2005,2010,2016]
    aa=aa+1;
    %subplot(2,4,aa)
    load(['bivariate 0825nomask ',num2str(year),'.mat'])
    grid_pixel_rgb=nan(180,360,3);
    for rows=1:180
        for cols=1:360
            if ~isnan(grid_pixel_ybin(rows,cols)) && ~isnan(grid_pixel_xbin(rows,cols))
                xb=grid_pixel_xbin(rows,cols);
                yb=grid_pixel_ybin(rows,cols);
                grid_pixel_rgb(rows,cols,:)=colos(yb,xb,:);
            end
        end
    end
    draw=grid_pixel_rgb/255;
    % 
    if aa<=4
        axes('position',[0.04+(aa-1)*0.2 0.53 0.17 0.36]);
    else
        axes('position',[0.04+(aa-5)*0.2 0.05 0.17 0.36]);
    end
    hh=imagesc([-180 180],[90 -90],draw);hold on;
    set(hh,'alphadata',~isnan(draw(:,:,1)));
    xlim([-81 -45])
    ylim([-23 15])
    line([-81 -81],[-23 15],'color','k')
    %line([70 140],[55 55],'color','k')

    axis xy;
    title(main_labels{aa},'fontsize',14)
    hold on;
    plot(long,lat,'k-'); hold on;
    line(amazon_lon,amazon_lat,'color','k','linewidth',2);
    set(gca,'fontsize',14)
    box on;
end
axes('position',[0.86 0.45 0.1 0.2]);
imagesc(colos/255)
% xlim([-0.015 0.165])
% ylim([-1.2 1.2])
set(gca,'xtick',0.5:1:5.5)
set(gca,'ytick',0.5:1:5.5)
set(gca,'fontsize',14)
set(gca,'xticklabel',{'0','','0.06','','0.12',''})
set(gca,'yticklabel',{'1','0.6','0.2','-0.2','-0.6','-1'})
xlabel('Seasonality index','fontsize',14)
ylabel('Drought appearance index','fontsize',14)

set(gcf,'PaperUnits','inches','PaperPosition',[0 0 14 6])
print(gcf,'-dtiff','-r300','D:\LSCE\ORCHIDEECAN\bivariate plot zscore nomask0825.tif')
close all
