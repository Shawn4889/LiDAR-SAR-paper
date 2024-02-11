df = readtable('E:\ChangMap\CHM\DB_20210926\DB_csv\Ind_2018.csv');

SAR = df(:,4);
ALS = df(:,7);
SAR = SAR{:,:};
ALS = ALS{:,:};

lm_linear = fitlm(ALS,SAR);