clear all
close all

format long
load HV_HH_std_mipers.mat % STD MIPERS en dB

% Parameters from the Water-Cloud Model (in dB) for Volume (YOU NEED TO PUT YOUR VALUES HERE)
a_HV=-27;b_HV=-14.78;c_HV=0.0001876;
a_HH=-19;b_HH=-7;c_HH=0.0002; %

AGB_max=100;
Vol_max=3*10^4; % Maximum Volume in the ecosystem. Might need adjustments.
AGB_range=1:AGB_max;
Vol_range=200:200:Vol_max;

HH_list=-20:0.02:-6; % Range of HH and HV values. Check if it represents the values that you have in your sites, otherwise adjust.
HV_list=-28:0.02:-12;

HV=a_HV.*exp(-c_HV.*AGB_range)+b_HV.*(1-exp(-c_HV.*AGB_range));
HH=a_HH.*exp(-c_HH.*AGB_range)+b_HH.*(1-exp(-c_HH.*AGB_range));

%%%%%%%%%%%%%%%%%%%%%%
% Calculation of LUT %
%%%%%%%%%%%%%%%%%%%%%%
HV_range_db = a_HV.*exp(-c_HV.*Vol_range)+b_HV.*(1-exp(-c_HV.*Vol_range));
% HV_range_db = 10.*log10(HV_range_lin);
HH_range_db = a_HH.*exp(-c_HH.*Vol_range)+b_HH.*(1-exp(-c_HH.*Vol_range));
% HH_range_db = 10.*log10(HH_range_lin);

LUT=zeros(length(HH_list),length(HV_list));

p_HH=[];
for h=1:length(HH_list)
    HH_db=HH_list(h);
    p_gobs_B2=[];
    
    for v=1:length(Vol_range)
        gthdB=HH_range_db(v);
        b=round(1+(Vol_range(v)-Vol_range(1))*(AGB_range(end)-AGB_range(1))/(Vol_range(end)-Vol_range(1))); % Approximate conversion from Volume to Biomass to use the AGB standard deviation
        p_gobs_B2=[p_gobs_B2 normpdf(HH_db,gthdB,HV_HH_std_mipers(b,3))];
    end
    p_HH=[p_HH; p_gobs_B2];
end

p_HV=[];
for v=1:length(HV_list)
    HV_db=HV_list(v);
    p_gobs_B=[];
    
    for v=1:length(Vol_range)
        gthdB=HV_range_db(v);
        b=round(1+(Vol_range(v)-Vol_range(1))*(AGB_range(end)-AGB_range(1))/(Vol_range(end)-Vol_range(1))); % Approximate conversion from Volume to Biomass to use the AGB standard deviation
        p_gobs_B=[p_gobs_B normpdf(HV_db,gthdB,HV_HH_std_mipers(b,2))];
    end
    p_HV=[p_HV; p_gobs_B];
end

disp('LUT done')

for h=1:length(HH_list)
    p_gobs_B2=p_HH(h,:);
    for v=1:length(HV_list)
        p_gobs_B=p_HV(v,:);
        p_gobs_B_HHHV=p_gobs_B.*p_gobs_B2./trapz(Vol_range,p_gobs_B.*p_gobs_B2);
        LUT(h,v)=trapz(Vol_range,Vol_range.*p_gobs_B_HHHV);
    end
end

save('Bayes_LUT_vol.mat','LUT','HH_list','HV_list');

res=0.02;
figure,
imagesc(HH_list,HV_list,LUT')
axis equal xy
xlim([HH_list(1)-res/2 HH_list(end)+res/2])
ylim([HV_list(1)-res/2 HV_list(end)+res/2])
xlabel('HH (dB)')
ylabel('HV (dB)')
set(gca,'CLim',[0 Vol_max])
colorbar
title(['AGB_{max}=',num2str(max(max(LUT_N)),'%2.1f'),'Mg.ha^{-1}'])