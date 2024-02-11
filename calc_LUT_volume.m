clear all
close all
format long
load HV_HH_std_mipers.mat % STD MIPERS en dB

%dir
suf = 'SAR_2018';
suf_hv = strcat(suf,'_HV7.tif');
suf_hh = strcat(suf,'_HH7.tif');
filename_hv = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Original',suf_hv);
filename_hh = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Original',suf_hh);



%2017
Vol_max=3856.632254;
HV_list=-29.78070068:0.02:-7.07130003;
HH_list=-22.54039955:0.02:-1.935899973;
a_HV=-23.1288959460132;
b_HV=-14.4916492224374;
c_HV=0.00557735928389455;
a_HH=-15.305038464761;
b_HH=-9.02230612263654;
c_HH=0.00510870013870338;

 

Vol_range=0:100:Vol_max;
AGB_max=100;
AGB_range=1:AGB_max;

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

%%%%%%%%%%%%%%%%%%%%%%
% Prediction %
%%%%%%%%%%%%%%%%%%%%%%
data_hv=importdata(filename_hv);
data_hh=importdata(filename_hh);
[r,c] = size(data_hv);

for i=1:r
    for j=1:c
        HHt=abs(data_hv(i,j)-HH_list);
        HVt=abs(data_hh(i,j)-HV_list);
        HHi=find(HHt==min(HHt),1);
        HVi=find(HVt==min(HVt),1);
        data_hv(i,j)=LUT(HHi,HVi);
    end
end

disp('pred done')

[geotiff1, R] = geotiffread(filename_hv);
t = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR_pred\Prediction_wcm\volume\',suf_hv);
%figure;
%imagesc(geotiff1);
geotiffwrite(t,data_hv,R)
%b = geotiffread(t);
%figure;
%imagesc(b);
