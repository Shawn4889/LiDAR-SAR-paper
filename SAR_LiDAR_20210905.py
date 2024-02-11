# Author: Xiaoxuan Li
import arcpy
import pandas as pd
import os
import arcpy
from arcpy.sa import *
from datetime import date
import shutil
arcpy.env.overwriteOutput = True
import subprocess


def focal_null():
    dir = r"E:\ChangMap\CHM\DB_20210830\DB_cover\Original/"
    out = r"E:\ChangMap\CHM\DB_20210830\DB_cover\Pitfill/"
    arcpy.env.workspace = dir
    chms = arcpy.ListRasters('*.tif*')
    for chm in chms:
        print(str(chm))
        chm_out = Con(IsNull(Raster(chm)),
                     FocalStatistics(Raster(chm),
                                     NbrRectangle(3,3),"MEAN"),
                     Raster(chm))
        chm_out.save(out + str(chm))


def clip_ras():
    dir_shp = r"E:\ChangMap\Boundingbox\Clip_new.shp"
    dir_ras = r"E:\ChangMap\CHM\DB_20210905\DB_chm\Pitfill/"
    dir_out = r"E:\ChangMap\CHM\DB_20210905\DB_chm\Subset/"
    arcpy.env.workspace = dir_ras
    arcpy.MakeFeatureLayer_management(dir_shp, "fLayer")
    chms = arcpy.ListRasters('*.tif*')
    for chm in chms:
        print(str(chm))
        outraster = dir_out + chm
        desc = arcpy.Describe("fLayer")
        extent = str(desc.extent.XMin) + " " + \
                 str(desc.extent.YMin) + " " + \
                 str(desc.extent.XMax) + " " + \
                 str(desc.extent.YMax)
        arcpy.Clip_management(chm, extent, outraster, "fLayer", -999, "ClippingGeometry", "NO_MAINTAIN_EXTENT")


def merge_ras(year):
    dir_ras = r"E:\ChangMap\CHM\DB_20210905\DB_chm\Subset/"
    dir_out = r"E:\ChangMap\CHM\DB_20210905\DB_chm\Mosaic/"
    projection = r"E:\GEDI\Boundingbox\UTM36S.prj"
    arcpy.env.workspace = dir_ras
    suf = "*" + year + ".tif*"
    chms = arcpy.ListRasters(suf)
    print(str(chms))
    print("Mosaicking..." + year)
    arcpy.MosaicToNewRaster_management(chms, dir_out, "CHM_" + year + ".tif",
                                       projection, "32_BIT_FLOAT", "1", "1", "LAST", "FIRST")


def zonalstats_volume():
    arc_dir = r"D:\temp\ArcGIS_project\Test\ALOS_LiDAR.gdb/"
    inpolygon = r"E:\ChangMap\CHM\DB_20210819\DB_shp\SAR_poly.shp"
    arcpy.env.workspace = r"E:\ChangMap\CHM\DB_20210905\DB_chm\Mosaic/"
    chms = arcpy.ListRasters('*.tif*')
    for chm in chms:
        print("Input ras: " + chm)
        table = arc_dir + "table_volume_" + chm.split(".")[0]
        print("Output zonal stats..." + table)
        ZonalStatisticsAsTable(inpolygon, "Merge_ID", chm, table)


def zonalstats_cover():
    arc_dir = r"D:\temp\ArcGIS_project\Test\ALOS_LiDAR.gdb/"
    inpolygon = r"E:\ChangMap\CHM\DB_20210819\DB_shp\SAR_poly.shp"
    arcpy.env.workspace = r"E:\ChangMap\CHM\DB_20210905\DB_cover/"
    chms = arcpy.ListRasters('*.tif*')
    for chm in chms:
        print("Input ras: " + chm)
        table = arc_dir + "table_cover_" + chm.split(".")[0]
        print("Output zonal stats..." + table)
        ZonalStatisticsAsTable(inpolygon, "Merge_ID", chm, table)
        arcpy.AddField_management(table, "Cover", "DOUBLE")
        expression = "!SUM! / !COUNT!"
        arcpy.CalculateField_management(table, "Cover", expression, "PYTHON_9.3")


#5*5 **************************************************************************************************************
def join_ras():
    dir = r"D:\temp\ArcGIS_project\Test\ALOS_LiDAR.gdb/"
    arcpy.env.workspace = dir
    suf = ["table_cover_CHM_2008", "table_cover_CHM_2010", "table_cover_CHM_2018",
           "table_volume_CHM_2008", "table_volume_CHM_2010", "table_volume_CHM_2018"]
    for s in suf:
        inpolygon = r"E:\ChangMap\CHM\DB_20210819\DB_shp\SAR_poly.shp"
        print("Shp..." + inpolygon)
        print("Adding join..." + s)
        table = dir + s
        arcpy.MakeFeatureLayer_management(inpolygon, "temp_layer")
        arcpy.AddJoin_management("temp_layer", "Merge_ID", table, "Merge_ID", "KEEP_COMMON")
        output = "Ras_" + s.split("_")[1] + "_" + s.split("_")[3]
        print("Poly to ras..." + output)
        if "volume" in s:
            arcpy.PolygonToRaster_conversion("temp_layer", "SUM", output,"", "", 0.00013475)
        elif "cover" in s:
            arcpy.PolygonToRaster_conversion("temp_layer", "COUNT", output + "_c", "", "", 0.00013475)
            arcpy.PolygonToRaster_conversion("temp_layer", "SUM", output + "_s", "", "", 0.00013475)
            arcpy.PolygonToRaster_conversion("temp_layer", "Cover", output, "", "", 0.00013475)
        arcpy.Delete_management("temp_layer")


def focal_analysis_ALS():
    print("Focal sum analysis")
    ArcGIS_dir = r"D:\temp\ArcGIS_project\Test\ALOS_LiDAR.gdb/"
    arcpy.env.workspace = ArcGIS_dir
    chms = arcpy.ListRasters()
    for chm in chms:
        print("Focal ..." + str(chm))
        outFocalStat5 = FocalStatistics(chm, NbrRectangle(5, 5, 'CELL'), "sum", "")
        outFocalStat5.save(ArcGIS_dir + chm + "_5")


def cal_CC_5():
    ArcGIS_dir = r"D:\temp\ArcGIS_project\Test\ALOS_LiDAR.gdb/"
    years = ["2008", "2010", "2018"]
    for year in years:
        print(year)
        ras_sum = Raster(ArcGIS_dir + "Ras_cover_" + year + "_s_5")
        ras_count = Raster(ArcGIS_dir + "Ras_cover_" + year + "_c_5")
        ras_CC = ras_sum/(ras_count)
        ras_CC.save(ArcGIS_dir + "Ras_cover_" + year + "_5")
        arcpy.Delete_management(ras_sum)
        arcpy.Delete_management(ras_count)

#stats
def extract():
    SAR_dir = r"D:\temp\ArcGIS_project\Test\ALOS_LiDAR.gdb/"
    arcpy.env.workspace = SAR_dir
    years = ["2008", "2010", "2018"]
    for year in years:
        print(year)
        chms = arcpy.ListRasters()
        for ras in chms:
            if str(ras) == "Ras_cover_" + year or str(ras) == "Ras_cover_" + year + "_5"\
                    or str(ras) == "Ras_volume_" + year or str(ras) == "Ras_volume_" + year + "_5":
                point = r"E:\ChangMap\CHM\DB_20210905\DB_shp\Point_" + year + ".shp"
                print("Extracting values to point..." + ras + " " + point)
                ExtractMultiValuesToPoints(point, SAR_dir + ras + ' ' + "RASTERVALU", "NONE")


def output_point():
    dir_shp= r"E:\ChangMap\CHM\DB_20210905\DB_shp/"
    out = r"E:\ChangMap\CHM\DB_20210905\DB_csv/"
    suf = ["2008", "2010", "2018"]
    for s in suf:
        #output SAR tables
        xls = out + "ALS_" + s + ".xlsx"
        fc = dir_shp + "Point_" + s + ".shp"
        print(xls)
        arcpy.TableToExcel_conversion(fc, xls)


def join_tables():
    dir = r"E:\ChangMap\CHM\DB_20210905\DB_csv/"
    suf = ["2008", "2010", "2018"]
    for s in suf:
        table1_dir = dir + "SAR_" + s + ".csv"
        table2_dir = dir + "ALS_" + s + ".xlsx"
        df1 = pd.read_csv(table1_dir)
        df2 = pd.read_excel(table2_dir)
        df2 = df2.iloc[:, 1:]
        df1 = pd.merge(df1, df2, left_on='Merge_ID', right_on='Merge_ID', how='left')
        print("Merging..." + s)
        output = dir + "Merge_" + s + ".csv"
        if s == "2008":
            df1.columns = ['Merge_ID', 'HH', 'HH7', 'HV', 'HV7', 'V1', 'C1', 'V2', 'C2']
        elif s == "2018":
            df1.columns = ['Merge_ID', 'HH', 'HH7', 'HV', 'HV7', 'C1', 'V1', 'V2', 'C2']
        elif s == "2010":
            df1.columns = ['Merge_ID','HH','HV','C1','V1','V2','C2']
        df1.drop_duplicates(keep='first', inplace=True)
        df1.dropna(inplace=True)
        df1.to_csv(output,index=False)


#prediction
def extract_pred():
    SAR_dir = r"E:\ChangMap\CHM\DB_20210905\DB_SAR_pred/"
    arcpy.env.workspace = SAR_dir
    years = ["2008", "2010", "2018"]
    for year in years:
        print(year)
        chms = arcpy.ListRasters()
        for ras in chms:
            if year in ras:
                point = r"E:\ChangMap\CHM\DB_20210905\DB_shp\Point_" + year + ".shp"
                print("Extracting values to point..." + ras + " " + point)
                #ExtractMultiValuesToPoints(point, SAR_dir + ras + ' ' + "RASTERVALU", "NONE")


def output_point_pred():
    dir_shp= r"E:\ChangMap\CHM\DB_20210905\DB_shp/"
    out = r"E:\ChangMap\CHM\DB_20210905\DB_csv/"
    suf = ["2008", "2010", "2018"]
    for s in suf:
        #output SAR tables
        xls = out + "Pred_" + s + ".xlsx"
        fc = dir_shp + "Point_" + s + ".shp"
        print(xls)
        arcpy.TableToExcel_conversion(fc, xls)


def rename_pred():
    suf = ["2008", "2010", "2018"]
    for s in suf:
        print(s)
        dir = "E:\ChangMap\CHM\DB_20210905\DB_csv/"
        output = "E:\ChangMap\CHM\DB_20210905\DB_csv/Pred_" + s + ".csv"
        table = dir + "Pred_" + s + ".xlsx"
        df = pd.read_excel(table)
        df = df.iloc[:, 1:]
        if s == "2008" :
            df.columns = ['Merge_ID', 'V1', 'C1', 'V2', 'C2', 'P_C_5', 'P_V_5', 'P_7_C_5', 'P_7_V_5', 'P_7_C', 'P_7_V',
                          'P_C', 'P_V']
        elif s == "2018":
            df.columns = ['Merge_ID', 'C1', 'V1', 'V2', 'C2', 'P_C_5', 'P_V_5', 'P_7_C_5', 'P_7_V_5', 'P_7_C', 'P_7_V',
                          'P_C', 'P_V']
        elif s == "2010":
            df.columns = ['Merge_ID','C1','V1','V2','C2','P_C_5','P_V_5','P_C','P_V']
        df.drop_duplicates(keep='first', inplace=True)
        df.dropna(inplace=True)
        df = df.drop(df[df.V1 == -9999].index)
        df.to_csv(output, index=False)
        os.remove(table)



def SAR_diff():
    dir = "E:\ChangMap\CHM\DB_20210905\DB_SAR_pred/"
    out = "E:\ChangMap\CHM\DB_20210905\DB_change/"
    list1 = ['SAR_2018_v.tif','SAR_2010_v.tif','SAR_2008_v.tif']
    list2 = ['SAR_2018_c.tif','SAR_2010_c.tif','SAR_2008_c.tif']
    list3 = ['SAR_2018_5_v.tif','SAR_2010_5_v.tif','SAR_2008_5_v.tif']
    list4 = [ 'SAR_2018_5_c.tif','SAR_2010_5_c.tif','SAR_2008_5_c.tif']
    #volume
    print("volume")
    output1 = out + 'SAR_volume_2018_2008.tif'
    output2 = out + 'SAR_volume_2018_2010.tif'
    output3 = out + 'SAR_volume_2010_2008.tif'
    ras_2018_2008 = Raster(dir + list1[0]) - Raster(dir + list1[2])
    ras_2018_2010 = Raster(dir + list1[0]) - Raster(dir + list1[1])
    ras_2010_2008 = Raster(dir + list1[1]) - Raster(dir + list1[2])
    ras_2018_2008.save(output1)
    ras_2018_2010.save(output2)
    ras_2010_2008.save(output3)
    #cover
    print("cover")
    output1 = out + 'SAR_cover_2018_2008.tif'
    output2 = out + 'SAR_cover_2018_2010.tif'
    output3 = out + 'SAR_cover_2010_2008.tif'
    ras_2018_2008 = Raster(dir + list2[0]) - Raster(dir + list2[2])
    ras_2018_2010 = Raster(dir + list2[0]) - Raster(dir + list2[1])
    ras_2010_2008 = Raster(dir + list2[1]) - Raster(dir + list2[2])
    ras_2018_2008.save(output1)
    ras_2018_2010.save(output2)
    ras_2010_2008.save(output3)

    #volume 5*5
    print("volume_5_5")
    output1 = out + 'SAR_volume_2018_2008_5.tif'
    output2 = out + 'SAR_volume_2018_2010_5.tif'
    output3 = out + 'SAR_volume_2010_2008_5.tif'
    ras_2018_2008 = Raster(dir + list3[0]) - Raster(dir + list3[2])
    ras_2018_2010 = Raster(dir + list3[0]) - Raster(dir + list3[1])
    ras_2010_2008 = Raster(dir + list3[1]) - Raster(dir + list3[2])
    ras_2018_2008.save(output1)
    ras_2018_2010.save(output2)
    ras_2010_2008.save(output3)
    #cover 5*5
    print("cover_5_5")
    output1 = out + 'SAR_cover_2018_2008_5.tif'
    output2 = out + 'SAR_cover_2018_2010_5.tif'
    output3 = out + 'SAR_cover_2010_2008_5.tif'
    ras_2018_2008 = Raster(dir + list4[0]) - Raster(dir + list4[2])
    ras_2018_2010 = Raster(dir + list4[0]) - Raster(dir + list4[1])
    ras_2010_2008 = Raster(dir + list4[1]) - Raster(dir + list4[2])
    ras_2018_2008.save(output1)
    ras_2018_2010.save(output2)
    ras_2010_2008.save(output3)



def ALS_diff():
    dir = r"D:\temp\ArcGIS_project\Test\ALOS_LiDAR.gdb/"
    out = "E:\ChangMap\CHM\DB_20210905\DB_change/"
    list1 = ['Ras_volume_2018.tif', 'Ras_volume_2010.tif', 'Ras_volume_2008.tif']
    list2 = ['Ras_cover_2018.tif','Ras_cover_2010.tif','Ras_cover_2008.tif']
    list3 = ['Ras_volume_2018_5.tif', 'Ras_volume_2010_5.tif', 'Ras_volume_2008_5.tif']
    list4 = ['Ras_cover_2018_5.tif','Ras_cover_2010_5.tif','Ras_cover_2008_5.tif']
    #volume
    print("volume")
    output1 = out + 'ALS_volume_2018_2008.tif'
    output2 = out + 'ALS_volume_2018_2010.tif'
    output3 = out + 'ALS_volume_2010_2008.tif'
    ras_2018_2008 = Raster(dir + list1[0]) - Raster(dir + list1[2])
    ras_2018_2010 = Raster(dir + list1[0]) - Raster(dir + list1[1])
    ras_2010_2008 = Raster(dir + list1[1]) - Raster(dir + list1[2])
    ras_2018_2008.save(output1)
    ras_2018_2010.save(output2)
    ras_2010_2008.save(output3)
    #cover
    print("cover")
    output1 = out + 'ALS_cover_2018_2008.tif'
    output2 = out + 'ALS_cover_2018_2010.tif'
    output3 = out + 'ALS_cover_2010_2008.tif'
    ras_2018_2008 = Raster(dir + list2[0]) - Raster(dir + list2[2])
    ras_2018_2010 = Raster(dir + list2[0]) - Raster(dir + list2[1])
    ras_2010_2008 = Raster(dir + list2[1]) - Raster(dir + list2[2])
    ras_2018_2008.save(output1)
    ras_2018_2010.save(output2)
    ras_2010_2008.save(output3)

    #volume 5*5
    print("volume_5_5")
    output1 = out + 'ALS_volume_2018_2008_5.tif'
    output2 = out + 'ALS_volume_2018_2010_5.tif'
    output3 = out + 'ALS_volume_2010_2008_5.tif'
    ras_2018_2008 = Raster(dir + list3[0]) - Raster(dir + list3[2])
    ras_2018_2010 = Raster(dir + list3[0]) - Raster(dir + list3[1])
    ras_2010_2008 = Raster(dir + list3[1]) - Raster(dir + list3[2])
    ras_2018_2008.save(output1)
    ras_2018_2010.save(output2)
    ras_2010_2008.save(output3)
    #cover 5*5
    print("cover_5_5")
    output1 = out + 'ALS_cover_2018_2008_5.tif'
    output2 = out + 'ALS_cover_2018_2010_5.tif'
    output3 = out + 'ALS_cover_2010_2008_5.tif'
    ras_2018_2008 = Raster(dir + list4[0]) - Raster(dir + list4[2])
    ras_2018_2010 = Raster(dir + list4[0]) - Raster(dir + list4[1])
    ras_2010_2008 = Raster(dir + list4[1]) - Raster(dir + list4[2])
    ras_2018_2008.save(output1)
    ras_2018_2010.save(output2)
    ras_2010_2008.save(output3)


################################################################################################test 50 biomass
#prediction
def extract_pred_50():
    SAR_dir = r"E:\ChangMap\CHM\DB_20210921\DB_SAR_pred_50/"
    shp_dir = r"E:\ChangMap\CHM\DB_20210921\DB_shp_50/"
    arcpy.env.workspace = SAR_dir
    years = ["2008", "2010", "2018"]
    for year in years:
        print(year)
        chms = arcpy.ListRasters()
        for ras in chms:
            if year in ras:
                point = shp_dir + "Point_" + year + ".shp"
                print("Extracting values to point..." + ras + " " + point)
                ExtractMultiValuesToPoints(point, SAR_dir + ras + ' ' + "RASTERVALU", "NONE")

def output_point_pred_50():
    dir_shp= r"E:\ChangMap\CHM\DB_20210921\DB_shp_50/"
    out = r"E:\ChangMap\CHM\DB_20210921\DB_csv_50/"
    suf = ["2008", "2010", "2018"]
    for s in suf:
        #output SAR tables
        xls = out + "Pred_" + s + ".xlsx"
        fc = dir_shp + "Point_" + s + ".shp"
        print(xls)
        arcpy.TableToExcel_conversion(fc, xls)


def rename_pred_50():
    suf = ["2008", "2010", "2018"]
    for s in suf:
        print(s)
        dir = "E:\ChangMap\CHM\DB_20210921\DB_csv_50/"
        output = "E:\ChangMap\CHM\DB_20210921\DB_csv_50/Pred_" + s + ".csv"
        table = dir + "Pred_" + s + ".xlsx"
        df = pd.read_excel(table)
        df = df.iloc[:, 1:]
        if s == "2008" or s == "2018":
            df.columns = ['Merge_ID', 'P_C_5', 'P_V_5', 'P_7_C_5', 'P_7_V_5']
        elif s == "2010":
            df.columns = ['Merge_ID', 'P_C_5', 'P_V_5']
        df.drop_duplicates(keep='first', inplace=True)
        df.dropna(inplace=True)
        df.to_csv(output, index=False)
        os.remove(table)


def merge_50():
    SAR_dir = "E:\ChangMap\CHM\DB_20210921\DB_csv_50/"
    ALS_dir = "E:\ChangMap\CHM\DB_20210905\DB_csv/"
    suf = ["2008", "2010", "2018"]
    for s in suf:
        print(s)
        SAR = SAR_dir + "Pred_" + s + ".csv"
        ALS = ALS_dir + "ALS_" + s + ".xlsx"
        output = SAR_dir + "Merge_" + s + ".csv"
        SAR_df = pd.read_csv(SAR)
        ALS_df = pd.read_excel(ALS)
        ALS_df = ALS_df.iloc[:, [1,4,5]]
        ALS_df.columns = ['Merge_ID', 'V2', 'C2']
        df = pd.merge(SAR_df, ALS_df, left_on='Merge_ID', right_on='Merge_ID', how='left')
        df = df.drop(df[df.V2 == -9999].index)
        df.to_csv(output, index=False)


################################################################################################change map for 50

def SAR_diff_5075100():
    #50
    dir = "E:\ChangMap\CHM\DB_20210921\DB_SAR_pred_50/"
    out = "E:\ChangMap\CHM\DB_20210921\DB_Change/"
    list4 = [ 'SAR_2018_5_c.tif','SAR_2010_5_c.tif','SAR_2008_5_c.tif']
    print("cover_5_5 50")
    output1 = out + 'SAR_cover_2018_2008_50.tif'
    output2 = out + 'SAR_cover_2018_2010_50.tif'
    output3 = out + 'SAR_cover_2010_2008_50.tif'
    ras_2018_2008 = Raster(dir + list4[0]) - Raster(dir + list4[2])
    ras_2018_2010 = Raster(dir + list4[0]) - Raster(dir + list4[1])
    ras_2010_2008 = Raster(dir + list4[1]) - Raster(dir + list4[2])
    ras_2018_2008.save(output1)
    ras_2018_2010.save(output2)
    ras_2010_2008.save(output3)


#############################################################################################max cover 1, max biomass 50
#prediction
SAR_dir = r"E:\ChangMap\CHM\DB_20210926\DB_SAR_pred/"
shp_dir = r"E:\ChangMap\CHM\DB_20210926\DB_shp/"
csv_dir = r"E:\ChangMap\CHM\DB_20210926\DB_csv/"


def extract_pred_1():
    arcpy.env.workspace = SAR_dir
    years = ["2008", "2010", "2018"]
    for year in years:
        print(year)
        chms = arcpy.ListRasters()
        for ras in chms:
            if year in ras:
                point = shp_dir + "Point_" + year + ".shp"
                print("Extracting values to point..." + ras + " " + point)
                ExtractMultiValuesToPoints(point, SAR_dir + ras + ' ' + "RASTERVALU", "NONE")


def output_point_pred_1():
    suf = ["2008", "2010", "2018"]
    for s in suf:
        #output SAR tables
        xls = csv_dir + "Pred_" + s + ".xlsx"
        fc = shp_dir + "Point_" + s + ".shp"
        print(xls)
        arcpy.TableToExcel_conversion(fc, xls)


def rename_pred_1():
    suf = ["2008", "2010", "2018"]
    for s in suf:
        print(s)
        table = csv_dir + "Pred_" + s + ".xlsx"
        output = csv_dir + "Pred_" + s + ".csv"
        df = pd.read_excel(table)
        df = df.iloc[:, 1:]
        if s == "2008" or s == "2018":
            df.columns = ['Merge_ID', 'P_C_5', 'P_V_5', 'P_7_C_5', 'P_7_V_5']
        elif s == "2010":
            df.columns = ['Merge_ID', 'P_C_5', 'P_V_5']
        df.drop_duplicates(keep='first', inplace=True)
        df.dropna(inplace=True)
        df.to_csv(output, index=False)
        os.remove(table)


def merge_1():
    ALS_dir = "E:\ChangMap\CHM\DB_20210905\DB_csv/"
    suf = ["2008", "2010", "2018"]
    for s in suf:
        print(s)
        SAR = csv_dir + "Pred_" + s + ".csv"
        ALS = ALS_dir + "ALS_" + s + ".xlsx"
        output = csv_dir + "Merge_" + s + ".csv"
        SAR_df = pd.read_csv(SAR)
        ALS_df = pd.read_excel(ALS)
        ALS_df = ALS_df.iloc[:, [1,4,5]]
        ALS_df.columns = ['Merge_ID', 'V2', 'C2']
        df = pd.merge(SAR_df, ALS_df, left_on='Merge_ID', right_on='Merge_ID', how='left')
        df = df.drop(df[df.C2 == -9999].index)
        df.to_csv(output, index=False)


def SAR_diff_v_1():
    dir = "E:\ChangMap\CHM\DB_20210926\DB_SAR_pred_wcm/"
    out = "E:\ChangMap\CHM\DB_20210926\DB_Change/"
    list4 = ['SAR_2018_5_v.tif', 'SAR_2018_7_5_v.tif', 'SAR_2010_5_v.tif', 'SAR_2008_5_v.tif', 'SAR_2008_7_5_v.tif']
    output1 = out + 'SAR_volume_2018_2008_50_1_wcm.tif'
    output2 = out + 'SAR_volume_2017_2007_50_1_wcm.tif'
    output3 = out + 'SAR_volume_2018_2010_50_1_wcm.tif'
    output4 = out + 'SAR_volume_2010_2008_50_1_wcm.tif'
    ras_2018_2008 = Raster(dir + list4[0]) - Raster(dir + list4[3])
    ras_2017_2007 = Raster(dir + list4[1]) - Raster(dir + list4[4])
    ras_2018_2010 = Raster(dir + list4[0]) - Raster(dir + list4[2])
    ras_2010_2008 = Raster(dir + list4[2]) - Raster(dir + list4[3])
    ras_2018_2008.save(output1)
    ras_2017_2007.save(output2)
    ras_2018_2010.save(output3)
    ras_2010_2008.save(output4)


def SAR_diff_c_1():
    dir = "E:\ChangMap\CHM\DB_20210926\DB_SAR_pred_wcm/"
    out = "E:\ChangMap\CHM\DB_20210926\DB_Change/"
    list4 = ['SAR_2018_5_c.tif','SAR_2018_7_5_c.tif','SAR_2010_5_c.tif','SAR_2008_5_c.tif','SAR_2008_7_5_c.tif']
    output1 = out + 'SAR_cover_2018_2008_50_1_wcm.tif'
    output2 = out + 'SAR_cover_2017_2007_50_1_wcm.tif'
    output3 = out + 'SAR_cover_2018_2010_50_1_wcm.tif'
    output4 = out + 'SAR_cover_2010_2008_50_1_wcm.tif'
    ras_2018_2008 = Raster(dir + list4[0]) - Raster(dir + list4[3])
    ras_2017_2007 = Raster(dir + list4[1]) - Raster(dir + list4[4])
    ras_2018_2010 = Raster(dir + list4[0]) - Raster(dir + list4[2])
    ras_2010_2008 = Raster(dir + list4[2]) - Raster(dir + list4[3])
    ras_2018_2008.save(output1)
    ras_2017_2007.save(output2)
    ras_2018_2010.save(output3)
    ras_2010_2008.save(output4)


def pred_linear():
    dir = "E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal/"
    out = "E:\ChangMap\CHM\DB_20210926\DB_SAR_pred_linear/"
    list = ['SAR_2018_HV_5.tif', 'SAR_2010_HV_5.tif', 'SAR_2008_HV_5.tif']
    output1 = out + 'SAR_cover_2018_linear.tif'
    output2 = out + 'SAR_cover_2010_linear.tif'
    output3 = out + 'SAR_cover_2008_linear.tif'
    ras_2018 = Raster(dir + list[0])*0.04549 + 1.12358
    ras_2010 = Raster(dir + list[1])*0.06313 + 1.42622
    ras_2008 = Raster(dir + list[2])*0.05129 + 1.15105
    ras_2018.save(output1)
    ras_2010.save(output2)
    ras_2008.save(output3)

def SAR_diff_c_1_linear():
    dir = "E:\ChangMap\CHM\DB_20210926\DB_SAR_pred_linear/"
    out = "E:\ChangMap\CHM\DB_20210926\DB_Change/"
    list4 = ['SAR_cover_2018_linear.tif','SAR_cover_2010_linear.tif','SAR_cover_2008_linear.tif']
    output1 = out + 'SAR_cover_2018_2008_50_1_linear.tif'
    output2 = out + 'SAR_cover_2018_2010_50_1_linear.tif'
    output3 = out + 'SAR_cover_2010_2008_50_1_linear.tif'
    ras_2018_2008 = Raster(dir + list4[0]) - Raster(dir + list4[2])
    ras_2018_2010 = Raster(dir + list4[0]) - Raster(dir + list4[1])
    ras_2010_2008 = Raster(dir + list4[1]) - Raster(dir + list4[2])
    ras_2018_2008.save(output1)
    ras_2018_2010.save(output2)
    ras_2010_2008.save(output3)


def pred_log():
    dir = "E:\ChangMap\CHM\DB_20210905\DB_SAR\Focal/"
    out = "E:\ChangMap\CHM\DB_20210926\DB_SAR_pred_log/"
    list = ['SAR_2018_HV_5.tif', 'SAR_2010_HV_5.tif', 'SAR_2008_HV_5.tif']
    output1 = out + 'SAR_cover_2018_log.tif'
    output2 = out + 'SAR_cover_2010_log.tif'
    output3 = out + 'SAR_cover_2008_log.tif'
    ras_2018 = 3.5065*Exp(0.1406*Raster(dir + list[0]))
    ras_2010 = 19.2171*Exp(0.2443*Raster(dir + list[1]))
    ras_2008 = 6.2034*Exp(0.1917*Raster(dir + list[2]))
    ras_2018.save(output1)
    ras_2010.save(output2)
    ras_2008.save(output3)


def SAR_diff_c_1_log():
    dir = "E:\ChangMap\CHM\DB_20210926\DB_SAR_pred_log/"
    out = "E:\ChangMap\CHM\DB_20210926\DB_Change/"
    list4 = ['SAR_cover_2018_log.tif', 'SAR_cover_2010_log.tif', 'SAR_cover_2008_log.tif']
    output1 = out + 'SAR_cover_2018_2008_50_1_log.tif'
    output2 = out + 'SAR_cover_2018_2010_50_1_log.tif'
    output3 = out + 'SAR_cover_2010_2008_50_1_log.tif'
    ras_2018_2008 = Raster(dir + list4[0]) - Raster(dir + list4[2])
    ras_2018_2010 = Raster(dir + list4[0]) - Raster(dir + list4[1])
    ras_2010_2008 = Raster(dir + list4[1]) - Raster(dir + list4[2])
    ras_2018_2008.save(output1)
    ras_2018_2010.save(output2)
    ras_2010_2008.save(output3)
