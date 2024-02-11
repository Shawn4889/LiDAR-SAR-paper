suf = 'SAR_2018';
suf_out = strcat(suf,'_c.tif');
suf_hv = strcat(suf,'_HV_5.tif');
suf_hh = strcat(suf,'_HH_5.tif');
filename_hv = fullfile('E:\ChangMap\CHM\DB_20210819\DB_SAR_pred\Focal',suf_hv);
filename_hh = fullfile('E:\ChangMap\CHM\DB_20210819\DB_SAR_pred\Focal',suf_hh);

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
t = fullfile('E:\ChangMap\CHM\DB_20210905\DB_SAR_pred\Prediction_wcm\',suf_out);
%figure;
%imagesc(geotiff1);
geotiffwrite(t,data_hv,R)
%b = geotiffread(t);
%figure;
%imagesc(b);