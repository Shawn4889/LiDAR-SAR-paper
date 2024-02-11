%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%2018
clear all
close all
format long
load HV_HH_std_mipers.mat % STD MIPERS en dB

%adjust
AGB_max=100;

out_dir = 'E:\ChangMap\CHM\DB_20210926\DB_SAR_pred';

suf = 'SAR_2018';
suf_hv = strcat(suf,'_HV_5.tif');
suf_hh = strcat(suf,'_HH_5.tif');
suf_out = strcat(suf,'_5_v.tif');
filename_hv = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal',suf_hv);
filename_hh = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal',suf_hh);

t = fullfile(out_dir,suf_out);

%2018
Vol_max=34752.69922;
HV_list=-29.09359932:0.02:-7.611569881;
HH_list=-21.58180046:0.02:-2.601819992;
a_HV=-25.3631449991811;
b_HV=-12.958336157691;
c_HV=0.000183593761291406;
a_HH=-17.286451731245;
b_HH=-7.65827566697082;
c_HH=0.000165156919390662;


Vol_range=0:200:Vol_max;

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
geotiffwrite(t,data_hv,R)


%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%2017

%dir
suf = 'SAR_2018';
suf_hv = strcat(suf,'_HV7_5.tif');
suf_hh = strcat(suf,'_HH7_5.tif');
suf_out = strcat(suf,'_7_5_v.tif');
filename_hv = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal',suf_hv);
filename_hh = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal',suf_hh);
t = fullfile(out_dir,suf_out);

%2017
Vol_max=34752.69922;
HV_list=-29.78070068:0.02:-7.07130003;
HH_list=-22.54039955:0.02:-1.935899973;
a_HV=-26.0240315844668;
b_HV=-12.8528101029758;
c_HV=0.000179887473200757;
a_HH=-17.3131452831718;
b_HH=-7.70866112155938;
c_HH=0.000164654050158506;



Vol_range=0:200:Vol_max;

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
geotiffwrite(t,data_hv,R)


%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%2010
%dir
suf = 'SAR_2010';
suf_hv = strcat(suf,'_HV_5.tif');
suf_hh = strcat(suf,'_HH_5.tif');
suf_out = strcat(suf,'_5_v.tif');
filename_hv = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal',suf_hv);
filename_hh = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal',suf_hh);
t = fullfile(out_dir,suf_out);

%2010
Vol_max=34646.30078;
HV_list=-27.40769958:0.02:-11.18379974;
HH_list=-22.15399933:0.02:-5.508769989;
a_HV=-23.2230161847269;
b_HV=-13.2778105887599;
c_HV=0.000139589567201766;
a_HH=-16.2992501877565;
b_HH=-8.36451532852766;
c_HH=0.000152557905763919;


Vol_range=0:200:Vol_max;

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
geotiffwrite(t,data_hv,R)


%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%2008
%dir
suf = 'SAR_2008';
suf_hv = strcat(suf,'_HV_5.tif');
suf_hh = strcat(suf,'_HH_5.tif');
suf_out = strcat(suf,'_5_v.tif');
filename_hv = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal',suf_hv);
filename_hh = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal',suf_hh);
t = fullfile(out_dir,suf_out);

%2008
Vol_max=33840.10156;
HV_list=-27.05690002:0.02:-7.599199772;
HH_list=-21.22780037:0.02:-2.703900099;
a_HV=-22.5710795631535;
b_HV=-11.2426984942972;
c_HV=0.00011330776993692;
a_HH=-15.8981030519944;
b_HH=-6.60858053093474;
c_HH=0.000119713814968512;


Vol_range=0:200:Vol_max;

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
geotiffwrite(t,data_hv,R)


%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%2007
%dir
suf = 'SAR_2008';
suf_hv = strcat(suf,'_HV7_5.tif');
suf_hh = strcat(suf,'_HH7_5.tif');
suf_out = strcat(suf,'_7_5_v.tif');
filename_hv = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal',suf_hv);
filename_hh = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal',suf_hh);
t = fullfile(out_dir,suf_out);

%2007
Vol_max=33840.10156;
HV_list=-26.71699905:0.02:-6.553770065;
HH_list=-20.46699905:0.02:-2.370270014;
a_HV=-22.3122676389589;
b_HV=-11.0281847957477;
c_HV=0.000110865735997429;
a_HH=-15.7632526017922;
b_HH=-6.40691893857123;
c_HH=0.000115922798377475;

Vol_range=0:200:Vol_max;

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
geotiffwrite(t,data_hv,R)