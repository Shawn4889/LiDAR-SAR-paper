import os
import arcpy
import numpy as np
import pandas as pd
from scipy.optimize import curve_fit
import math
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score
arcpy.env.overwriteOutput = True



'''
DB_dir = r"E:\ChangMap\CHM\DB_archive/"
DB_ch_cover_dir = DB_dir + r"Change\Cover/"
DB_ch_vol_dir = DB_dir + r"Change\Volume/"
DB_ind_cover_dir = DB_dir + r"Individual\Cover/"
DB_ind_vol_dir = DB_dir + r"Individual\Volume/"

polars = ["HV","HV7","HH","HH7"]
#years = ["2018_2008","2018_2010","2010_2008"]
years = ["2008","2010","2018"]

def match(year,polar,dir):
    suf1 = "Merged_ALOS_" + year + "_" + polar + "_w_result.csv"
    if "7" in polar:
        suf2 = "Merged_ALOS_" + year + "_HH7_w_result.csv"
    else:
        suf2 = "Merged_ALOS_" + year + "_HH_w_result.csv"
    out = dir + "Match_ALOS_" + year + "_"+ polar + "_w_result.csv"
    print("Output merged file..." + out)
    df1 = pd.read_csv(dir + suf1, header=0)
    df2 = pd.read_csv(dir + suf2, header=0)
    df1 = df1.loc[:, ['C_s_sum5', 'CA_o_mean5']]
    df2 = df2.loc[:, ['C_s_sum5', 'CA_o_mean5']]
    df1 = df1.drop_duplicates(subset=['C_s_sum5'])
    df2 = df2.drop_duplicates(subset=['C_s_sum5'])
    df_merge = pd.merge(df1, df2, left_on='C_s_sum5', right_on='C_s_sum5', how='inner')
    df_merge.drop_duplicates(keep='first', inplace=True)
    df_merge.to_csv(out, index=None)
def match_cover(year,polar,dir):
    dir2 = DB_ind_cover_dir
    suf1 = "Merged_ALOS_" + year + "_" + polar + "_w_result_cover.csv"
    if "7" in polar:
        suf2 = "Merged_ALOS_" + year + "_HH7_w_result"
    else:
        suf2 = "Merged_ALOS_" + year + "_HH_w_result"
    out = dir2 + "Match_ALOS_" + year + "_"+ polar + "_w_result_cover.csv"
    print("Output merged file..." + out)
    df1 = pd.read_csv(dir2 + suf1, header=0)
    df2 = pd.read_csv(dir2 + suf2 + "_cover.csv", header=0)
    df3 = pd.read_csv(dir + suf2 + ".csv", header=0)

    df2 = df2.loc[:, ['C_c_sum5', 'OBJECTID']]
    df3 = df3.loc[:, ['OBJECTID', 'CA_o_mean5']]

    df2 = df2.drop_duplicates(subset=['OBJECTID'])
    df3 = df3.drop_duplicates(subset=['OBJECTID'])
    df2 = pd.merge(df2, df3, left_on='OBJECTID', right_on='OBJECTID', how='inner')

    df1 = df1.loc[:, ['C_c_sum5', 'CA_o_mean5']]
    df2 = df2.loc[:, ['C_c_sum5', 'CA_o_mean5']]
    df1 = df1.drop_duplicates(subset=['C_c_sum5'])
    df2 = df2.drop_duplicates(subset=['C_c_sum5'])
    df_merge = pd.merge(df1, df2, left_on='C_c_sum5', right_on='C_c_sum5', how='inner')
    df_merge.drop_duplicates(keep='first', inplace=True)
    df_merge.to_csv(out, index=None)
def shp_cover_csv(year,polar):
    matches = []
    arcpy.env.workspace = r"E:\ChangMap\CHM\DB_shp"
    fc_list = arcpy.ListFeatureClasses()
    for fc in fc_list:
        if "2018_2008" not in fc and "2018_2010" not in fc and "2010_2008" not in fc:
            if year in fc and polar in fc and "cover" in fc:
                print("Identify..." + fc)
                match = (os.path.join(r"E:\ChangMap\CHM\DB_shp/", fc))
                matches.append(match)
    output = r"E:\ChangMap\CHM\DB_shp/"+ "merge_" + year + polar + "cover.shp"
    print("Merging..." + output)
    arcpy.Merge_management(matches, output)
    arcpy.MakeFeatureLayer_management(output, 'lyr')

    DB_csv = r"E:\ChangMap\CHM\DB_csv/"
    xls = DB_csv + "merge" + polar + year + "_cover" + ".xlsx"
    csv = DB_csv + "merge" + polar + year + "_cover" + ".csv"
    arcpy.TableToExcel_conversion('lyr', xls)
    #clean data
    data_xls = pd.read_excel(xls,header=0)
    print("Dropping columns...")
    data_xls.drop(['Join_Count', 'TARGET_FID', 'Id', "grid_code", 'gridcode','OBJECTID_1', 'ID_1',
                   'CA_o','length', 'area', 'pointid', "grid_code", "Shape_Leng", "Shape_Area"],
                  axis=1, inplace=True)
    data_xls["C_c_sum3"] = data_xls["C_c_sum3"]/2025
    data_xls["C_c_sum5"] = data_xls["C_c_sum5"]/5625
    data_xls["C_c_sum9"] = data_xls["C_c_sum9"]/18225
    data_xls["CA_o_mean3"] = data_xls["CA_o_mean3"]/1000000
    data_xls["CA_o_mean5"] = data_xls["CA_o_mean5"]/1000000
    data_xls["CA_o_mean9"] = data_xls["CA_o_mean9"]/1000000
    print("Output csv...")
    data_xls.to_csv(csv, index=None)
    os.remove(xls) 
    
def shp_volume_csv(year,polar):
    matches = []
    arcpy.env.workspace = r"E:\ChangMap\CHM\DB_shp"
    fc_list = arcpy.ListFeatureClasses()
    for fc in fc_list:
        if "2018_2008" not in fc and "2018_2010" not in fc and "2010_2008" not in fc:
            if year in fc and polar in fc and "cover" not in fc:
                print("Identify..." + fc)
                match = (os.path.join(r"E:\ChangMap\CHM\DB_shp/", fc))
                matches.append(match)
    output = r"E:\ChangMap\CHM\DB_shp/"+ "merge_" + year + polar + "volume.shp"
    print("Merging..." + output)
    arcpy.Merge_management(matches, output)
    arcpy.MakeFeatureLayer_management(output, 'lyr')
    DB_csv = r"E:\ChangMap\CHM\DB_csv/"
    xls = DB_csv + "merge" + polar + year + "_volume" + ".xlsx"
    csv = DB_csv + "merge" + polar + year + "_volume" + ".csv"
    arcpy.TableToExcel_conversion('lyr', xls)
    #clean data
    data_xls = pd.read_excel(xls,header=0)
    print("Dropping columns...")
    data_xls.drop(['Join_Count', 'TARGET_FID', 'Id', "grid_code", 'gridcode','OBJECTID_1', 'ID_1',
                   'CA_o','length', 'area', 'pointid', "grid_code", "Shape_Leng", "Shape_Area"],
                  axis=1, inplace=True)
    data_xls["CA_o_mean3"] = data_xls["CA_o_mean3"]/1000000
    data_xls["CA_o_mean5"] = data_xls["CA_o_mean5"]/1000000
    data_xls["CA_o_mean9"] = data_xls["CA_o_mean9"]/1000000
    print("Output csv...")
    data_xls.to_csv(csv, index=None)
    os.remove(xls)
'''

def volume_pred():
    arcpy.env.workspace = r"E:\ChangMap\CHM\DB_shp"
    fc_list = arcpy.ListFeatureClasses()
    arcpy.env.workspace = r"D:\temp\ArcGIS_project\Test\Test.gdb"
    abc_dir = r"E:\ChangMap\CHM\ABC.csv"
    df_abc = pd.read_csv(abc_dir, header=0)
    df_abc = df_abc[df_abc['Type'] == 'Volume']
    for fc in fc_list:
        #change HV to HH, optional
        if "volume_j" in fc and fc == "merge_2018_HV_volume_j.shp":
            Year = fc.split("_")[1]
            Polarization = fc.split("_")[2]
            if "7" in Polarization:
                Year = str(int(Year)-1)
                Polarization = Polarization[0:2]
            print(Year)
            print(Polarization)
            df_abc_sub = df_abc[(df_abc['Year'] == int(Year)) & (df_abc['Polarization'] == Polarization)]
            A = df_abc_sub.iloc[0]['A']
            B = df_abc_sub.iloc[0]['B']
            C = df_abc_sub.iloc[0]['C']
            print("Processing..." + fc)
            print("Year: " + Year + " Polarization: " + Polarization + " A: " + str(A) + " B: " + str(B) + " C: " + str(C))
            inFeatures = r"E:\ChangMap\CHM\DB_shp/" + fc
            fieldName = "Vol_ALS"
            fieldName2 = "Vol_ALOS_w"
            fieldName3 = "Vol_ALOS_l"
            arr = arcpy.da.TableToNumPyArray(inFeatures, ("CA_o_mean5", "C_s_sum5"))
            df = pd.DataFrame(arr)
            df = df.astype(int)
            X = df.iloc[:, 1].values.reshape(-1, 1)
            Y = (round(df.iloc[:, 0],0)/1000000).values.reshape(-1, 1)
            linear_regressor = LinearRegression()
            linear_regressor.fit(X, Y)
            Y_pred = linear_regressor.predict(X)
            R2 = r2_score(Y, Y_pred)
            print(R2)
            print(linear_regressor.intercept_[0])
            print(linear_regressor.coef_[0][0])
            X_new_linear = (Y-linear_regressor.intercept_[0])/linear_regressor.coef_[0][0]
            X_new_wcm = -(1/C)*np.log((Y-B)/(A-B))
            arcpy.DeleteField_management(inFeatures, "Vol_ALS")
            arcpy.DeleteField_management(inFeatures, "Vol_ALOS")
            arcpy.AddField_management(inFeatures, fieldName, "DOUBLE")
            arcpy.CalculateField_management(inFeatures, fieldName, "!C_s_sum5!")
            arcpy.AddField_management(inFeatures, fieldName2, "DOUBLE")
            arcpy.AddField_management(inFeatures, fieldName3, "DOUBLE")
            cursor1 = arcpy.UpdateCursor(inFeatures)
            i = 0
            for row in cursor1:
                #print(X_new[i][0])
                row.setValue(fieldName2,X_new_wcm[i][0])
                cursor1.updateRow(row)
                i = i + 1

            cursor2 = arcpy.UpdateCursor(inFeatures)
            i = 0
            for row in cursor2:
                #print(X_new[i][0])
                row.setValue(fieldName3,X_new_linear[i][0])
                cursor2.updateRow(row)
                i = i + 1



def cover_pred():
    arcpy.env.workspace = r"E:\ChangMap\CHM\DB_shp"
    fc_list = arcpy.ListFeatureClasses()
    arcpy.env.workspace = r"D:\temp\ArcGIS_project\Test\Test.gdb"
    abc_dir = r"E:\ChangMap\CHM\ABC.csv"
    df_abc = pd.read_csv(abc_dir, header=0)
    df_abc = df_abc[df_abc['Type'] == 'Cover']
    for fc in fc_list:
        #change HV to HH, optional
        if "cover_j" in fc:
            Year = fc.split("_")[1]
            Polarization = fc.split("_")[2]
            if "7" in Polarization:
                Year = str(int(Year) - 1)
                Polarization = Polarization[0:2]
            print(str(Year))
            print(Polarization)
            df_abc_sub = df_abc[(df_abc['Year'] == int(Year)) & (df_abc['Polarization'] == Polarization)]
            print(df_abc_sub)
            A = df_abc_sub.iloc[0]['A']
            B = df_abc_sub.iloc[0]['B']
            C = df_abc_sub.iloc[0]['C']
            print("Processing..." + fc)
            print("Year: " + Year + " Polarization: " + Polarization + " A: " + str(A) + " B: " + str(B) + " C: " + str(C))
            inFeatures = r"E:\ChangMap\CHM\DB_shp/" + fc
            fieldName = "CC_ALS"
            fieldName2 = "CC_ALOS_w"
            fieldName3 = "CC_ALOS_l"
            arr = arcpy.da.TableToNumPyArray(inFeatures, ("CA_o_mean5", "C_c_sum5"))
            df = pd.DataFrame(arr)
            df = df.astype(int)
            X = (round(df.iloc[:, 1],0)/5625).values.reshape(-1, 1)
            Y = (round(df.iloc[:, 0],0)/1000000).values.reshape(-1, 1)
            linear_regressor = LinearRegression()
            linear_regressor.fit(X, Y)
            Y_pred = linear_regressor.predict(X)
            R2 = r2_score(Y, Y_pred)
            print(R2)
            print(linear_regressor.intercept_[0])
            print(linear_regressor.coef_[0][0])
            X_new_linear = (Y - linear_regressor.intercept_[0]) / linear_regressor.coef_[0][0]
            X_new_wcm = -(1 / C) * np.log((Y - B) / (A - B))
            arcpy.DeleteField_management(inFeatures, "CC_ALS")
            arcpy.DeleteField_management(inFeatures, "Cover_ALS")
            arcpy.DeleteField_management(inFeatures, "Cover_ALOS")
            arcpy.DeleteField_management(inFeatures, "CC_ALOS_l")
            arcpy.DeleteField_management(inFeatures, "CC_ALOS_w")
            arcpy.AddField_management(inFeatures, fieldName, "DOUBLE")
            arcpy.CalculateField_management(inFeatures, fieldName, "float(!C_c_sum5!)/5625")
            arcpy.AddField_management(inFeatures, fieldName2, "DOUBLE")
            arcpy.AddField_management(inFeatures, fieldName3, "DOUBLE")
            cursor1 = arcpy.UpdateCursor(inFeatures)
            i = 0
            for row in cursor1:
                # print(X_new[i][0])
                row.setValue(fieldName2, X_new_wcm[i][0])
                cursor1.updateRow(row)
                i = i + 1

            cursor2 = arcpy.UpdateCursor(inFeatures)
            i = 0
            for row in cursor2:
                # print(X_new[i][0])
                row.setValue(fieldName3, X_new_linear[i][0])
                cursor2.updateRow(row)
                i = i + 1



def shp_cover_csv_new(year,polar):
    arcpy.env.workspace = r"E:\ChangMap\CHM\DB_shp"
    output = r"E:\ChangMap\CHM\DB_shp/"+ "merge_" + year + "_"+ polar + "_cover_j.shp"
    print("Output csv..." + output)
    arcpy.MakeFeatureLayer_management(output, 'lyr')
    DB_csv = r"E:\ChangMap\CHM\DB_csv\Individual/"
    xls = DB_csv + "merge_" + year + "_"+ polar + "_cover" + ".xlsx"
    csv = DB_csv + "merge_" + year + "_"+ polar + "_cover" + ".csv"
    arcpy.TableToExcel_conversion('lyr', xls)
    #clean data
    data_xls = pd.read_excel(xls,header=0)
    print("Dropping columns...")
    data_xls.drop(['Join_Count','Join_Cou_1','TARGET_F_1', 'C_c',
                   'gridcode','OBJECTID_1', 'ID_1','TARGET_FID', 'Id', "grid_code",
                   'CA_o','length', 'area', 'pointid', "grid_code", "Shape_Leng", "Shape_Area",
                   "C_c_sum3","C_c_sum9","CA_o_mean3","CA_o_mean9"],
                  axis=1, inplace=True)

    data_xls["C_c_sum5"] = data_xls["C_c_sum5"]/5625
    data_xls["CA_o_mean5"] = data_xls["CA_o_mean5"]/1000000

    print("Output csv...")
    data_xls.to_csv(csv, index=None)
    os.remove(xls)


def shp_volume_csv_new(year,polar):
    arcpy.env.workspace = r"E:\ChangMap\CHM\DB_shp"
    output = r"E:\ChangMap\CHM\DB_shp/"+ "merge_" + year + "_"+ polar + "_volume_j.shp"
    print("Merging..." + output)
    arcpy.MakeFeatureLayer_management(output, 'lyr')
    DB_csv = r"E:\ChangMap\CHM\DB_csv\Individual/"
    xls = DB_csv + "merge_" + year + "_"+ polar + "_volume" + ".xlsx"
    csv = DB_csv + "merge_" + year + "_"+ polar + "_volume" + ".csv"
    arcpy.TableToExcel_conversion('lyr', xls)
    #clean data
    data_xls = pd.read_excel(xls,header=0)
    print("Dropping columns...")
    data_xls.drop(['Join_Count','Join_Cou_1','TARGET_F_1', 'C_c','C_s',
                   'gridcode','OBJECTID_1', 'ID_1','TARGET_FID', 'Id', "grid_code",
                   'CA_o','length', 'area', 'pointid', "grid_code", "Shape_Leng", "Shape_Area",
                   "C_c_sum3","C_c_sum9","C_s_sum3","C_s_sum9","CA_o_mean3","CA_o_mean9"],
                  axis=1, inplace=True)

    data_xls["CA_o_mean5"] = data_xls["CA_o_mean5"]/1000000

    print("Output csv...")
    data_xls.to_csv(csv, index=None)
    os.remove(xls)




def change_(year1,year2,polar,type):
    dir = r"E:\ChangMap\CHM\DB_csv\Individual/"
    if year1 == "2010":
        dir_1 = dir + "merge_" + year1 + "_" + polar[0:2] + "_" + type + ".csv"
    else:
        dir_1 = dir + "merge_" + year1 + "_" + polar + "_" + type + ".csv"
    if year2 == "2010":
        dir_2 = dir + "merge_" + year2 + "_" + polar[0:2] + "_" + type + ".csv"
    else:
        dir_2 = dir + "merge_" + year2 + "_" + polar + "_" + type + ".csv"
    df_csv1 = pd.read_csv(dir_1)
    df_csv2 = pd.read_csv(dir_2)
    print(dir_1)
    print(dir_2)
    df_merge = pd.merge(df_csv1, df_csv2, left_on='ORIG_FID', right_on='ORIG_FID', how='left')

    df_merge["CA_o_mean5"] = df_merge['CA_o_mean5_y'] - df_merge['CA_o_mean5_x']
    if type == "volume":
        df_merge["C_s_sum5"] = df_merge['C_s_sum5_y'] - df_merge['C_s_sum5_x']
        df_merge = df_merge.filter(['ORIG_FID','CA_o_mean5', 'C_s_sum5'])
    elif type == "cover":
        df_merge["C_c_sum5"] = df_merge['C_c_sum5_y'] - df_merge['C_c_sum5_x']
        df_merge = df_merge.filter(['ORIG_FID','CA_o_mean5', 'C_c_sum5'])

    suf = "change_" + year2 + "_" + year1 + "_" + polar + "_" + type  + ".csv"
    out = r"E:\ChangMap\CHM\DB_csv\Change/" + suf
    print("Output csv..." + out)
    df_merge = df_merge.dropna()
    df_merge.to_csv(out, index=None)


def pred():
    dir = r"E:\ChangMap\CHM\DB_csv\Change/"
    files = os.listdir(dir)
    for i in files:
        df = pd.read_csv(dir+i)
        df = df.dropna()
        X = (round(df.iloc[:, 2], 2)).values.reshape(-1, 1)#ALS Sum5
        Y = (round(df.iloc[:, 1], 2)).values.reshape(-1, 1)#SAR CA
        linear_regressor = LinearRegression()
        linear_regressor.fit(X, Y)
        X_new_linear = (Y - linear_regressor.intercept_[0]) / linear_regressor.coef_[0][0]
        df["Linear"] = X_new_linear
        print("output..." + i)
        out = r"E:\ChangMap\CHM\DB_csv\Change/" + i.split(".")[0] + "_pre.csv"
        df.to_csv(out, index=None)


def clean_cover():
    arcpy.env.workspace = r"E:\ChangMap\CHM\DB_shp"
    fc_list = arcpy.ListFeatureClasses()
    arcpy.env.workspace = r"D:\temp\ArcGIS_project\Test\Test.gdb"
    for fc in fc_list:
        #change HV to HH, optional
        if "merge" in fc and "cover" in fc:
            print("Processing..." + fc)
            inFeatures = r"E:\ChangMap\CHM\DB_shp/" + fc
            arcpy.DeleteField_management(inFeatures, "CC_ALS")
            arcpy.DeleteField_management(inFeatures, "Cover_ALS")
            arcpy.DeleteField_management(inFeatures, "Cover_ALOS")
            arcpy.DeleteField_management(inFeatures, "CC_ALOS_l")
            arcpy.DeleteField_management(inFeatures, "CC_ALOS_w")
            arcpy.DeleteField_management(inFeatures, "Diff")
            arcpy.DeleteField_management(inFeatures, "ALOS_5")
            arcpy.DeleteField_management(inFeatures, "ALS_5")


        elif "merge" in fc and "volume" in fc:
            print("Processing..." + fc)
            inFeatures = r"E:\ChangMap\CHM\DB_shp/" + fc
            arcpy.DeleteField_management(inFeatures, "CC_ALS")
            arcpy.DeleteField_management(inFeatures, "Vol_ALS")
            arcpy.DeleteField_management(inFeatures, "Vol_ALOS")
            arcpy.DeleteField_management(inFeatures, "Vol_ALOS_l")
            arcpy.DeleteField_management(inFeatures, "Vol_ALOS_w")
            arcpy.DeleteField_management(inFeatures, "Diff")


def index_join():
    dir = r"E:\ChangMap\CHM\DB_shp/"
    arcpy.env.workspace = dir
    fc_list = arcpy.ListFeatureClasses()
    for fc in fc_list:
        if "merge" in fc:
            target_features = dir + fc
            index_file = dir + "index.shp"
            out_feature_class = dir + fc.split(".")[0] + "_j.shp"
            print("output..." + fc)
            arcpy.SpatialJoin_analysis(target_features, index_file, out_feature_class)


'''
#HV
shp_volume_csv_new("2018","HV")
shp_volume_csv_new("2018","HV7")
shp_volume_csv_new("2010","HV")
shp_volume_csv_new("2008","HV")
shp_volume_csv_new("2008","HV7")

shp_cover_csv_new("2018","HV")
shp_cover_csv_new("2018","HV7")
shp_cover_csv_new("2010","HV")
shp_cover_csv_new("2008","HV")
shp_cover_csv_new("2008","HV7")

#HH
shp_volume_csv_new("2018","HH")
shp_volume_csv_new("2018","HH7")
shp_volume_csv_new("2010","HH")
shp_volume_csv_new("2008","HH")
shp_volume_csv_new("2008","HH7")

shp_cover_csv_new("2018","HH")
shp_cover_csv_new("2018","HH7")
shp_cover_csv_new("2010","HH")
shp_cover_csv_new("2008","HH")
shp_cover_csv_new("2008","HH7")

'''
change_("2008","2018","HV","volume")
change_("2008","2018","HV7","volume")
change_("2008","2010","HV","volume")
change_("2010","2018","HV","volume")
change_("2010","2018","HV7","volume")

change_("2008","2018","HV","cover")
change_("2008","2018","HV7","cover")
change_("2008","2010","HV","cover")
change_("2010","2018","HV","cover")
change_("2010","2018","HV7","cover")

change_("2008","2018","HH","volume")
change_("2008","2018","HH7","volume")
change_("2008","2010","HH","volume")
change_("2010","2018","HH","volume")
change_("2010","2018","HH7","volume")

change_("2008","2018","HH","cover")
change_("2008","2018","HH7","cover")
change_("2008","2010","HH","cover")
change_("2010","2018","HH","cover")
change_("2010","2018","HH7","cover")

change_("2008","2010","HH7","volume")
change_("2008","2010","HH7","cover")
change_("2008","2010","HV7","volume")
change_("2008","2010","HV7","cover")
