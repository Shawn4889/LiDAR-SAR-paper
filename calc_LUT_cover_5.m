%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%2018
clear all
close all
format long
load HV_HH_std_mipers.mat % STD MIPERS en dB

%adjust
AGB_max=50;
Vol_max=1.4;
out_dir = 'E:\ChangMap\CHM\DB_20210923\DB_SAR_pred_1_4';

suf = 'SAR_2018';
suf_hv = strcat(suf,'_HV_5.tif');
suf_hh = strcat(suf,'_HH_5.tif');
suf_out = strcat(suf,'_5_c.tif');
filename_hv = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal',suf_hv);
filename_hh = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal',suf_hh);

t = fullfile(out_dir,suf_out);

% 2018 
HV_list=-29.09359932:0.02:-7.611569881;
HH_list=-21.58180046:0.02:-2.601819992;
a_HV=-25.0822974189217;
b_HV=-13.0159122731819;
c_HV=3.78129021708311;
a_HH=-17.1258829672765;
b_HH=-7.71778912603854;
c_HH=3.43434403407925;


Vol_range=0:0.02:Vol_max;

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
suf_out = strcat(suf,'_7_5_c.tif');
filename_hv = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal',suf_hv);
filename_hh = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal',suf_hh);
t = fullfile(out_dir,suf_out);

% 2017
HV_list=-29.78070068:0.02:-7.07130003;
HH_list=-22.54039955:0.02:-1.935899973;
a_HV=-25.7396998002218;
b_HV=-12.9972313755677;
c_HV=3.7617200251029;
a_HH=-17.2088503441387;
b_HH=-8.01631696662649;
c_HH=3.64938791030274;

Vol_range=0:0.02:Vol_max;

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
suf_out = strcat(suf,'_5_c.tif');
filename_hv = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal',suf_hv);
filename_hh = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal',suf_hh);
t = fullfile(out_dir,suf_out);

% 2010
HV_list=-27.40769958:0.02:-11.18379974;
HH_list=-22.15399933:0.02:-5.508769989;
a_HV=-23.1827600276435;
b_HV=-13.7668510890168;
c_HV=3.4793978305592;
a_HH=-16.2895656061686;
b_HH=-8.8920658615135;
c_HH=4.00587966920961;


Vol_range=0:0.02:Vol_max;

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
suf_out = strcat(suf,'_5_c.tif');
filename_hv = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal',suf_hv);
filename_hh = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal',suf_hh);
t = fullfile(out_dir,suf_out);

% 2008
HV_list=-27.05690002:0.02:-7.599199772;
HH_list=-21.22780037:0.02:-2.703900099;
a_HV=-22.5197946436362;
b_HV=-10.6985096513323;
c_HV=2.48854911346198;
a_HH=-15.8234809441426;
b_HH=-6.28767563264565;
c_HH=2.65856285954772;

Vol_range=0:0.02:Vol_max;

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
suf_out = strcat(suf,'_7_5_c.tif');
filename_hv = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal',suf_hv);
filename_hh = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal',suf_hh);
t = fullfile(out_dir,suf_out);

% 2007
HV_list=-26.71699905:0.02:-6.553770065;
HH_list=-20.46699905:0.02:-2.370270014;
a_HV=-22.2720237855968;
b_HV=-10.5952161362341;
c_HV=2.47595619073709;
a_HH=-15.6916930329133;
b_HH=-6.0616003323837;
c_HH=2.56757142491669;

Vol_range=0:0.02:Vol_max;

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