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


def zonalstats_volume():
    arc_dir = r"D:\temp\ArcGIS_project\Test\ALOS_LiDAR.gdb/"
    inpolygon = r"E:\ChangMap\CHM\DB_20220221\DB_shp\SAR_poly.shp"
    arcpy.env.workspace = r"E:\ChangMap\CHM\DB_20220221\DB_chm\Noshurb/"
    chms = arcpy.ListRasters('*.tif*')
    for chm in chms:
        print("Input ras: " + chm)
        table = arc_dir + "table_height_" + chm.split(".")[0]
        print("Output zonal stats..." + table)
        ZonalStatisticsAsTable(inpolygon, "Merge_ID", chm, table)


def join_ras():
    dir = r"D:\temp\ArcGIS_project\Test\ALOS_LiDAR.gdb/"
    arcpy.env.workspace = dir
    suf = ["table_height_CHM_2008", "table_height_CHM_2010", "table_height_CHM_2018"]

    for s in suf:
        if "height" in s:
            inpolygon = r"E:\ChangMap\CHM\DB_20220221\DB_shp\SAR_poly.shp"
            print("Shp..." + inpolygon)
            print("Adding join..." + s)
            table = dir + s
            arcpy.MakeFeatureLayer_management(inpolygon, "temp_layer")
            arcpy.AddJoin_management("temp_layer", "Merge_ID", table, "Merge_ID", "KEEP_COMMON")
            output = "Ras_" + s.split("_")[1] + "_" + s.split("_")[3]
            print("Poly to ras..." + output)
            arcpy.PolygonToRaster_conversion("temp_layer", "SUM", output, "", "", 0.00013475)
        arcpy.Delete_management("temp_layer")


def focal_analysis_height():
    print("Focal sum analysis")
    ArcGIS_dir = r"D:\temp\ArcGIS_project\Test\ALOS_LiDAR.gdb/"
    arcpy.env.workspace = ArcGIS_dir
    chms = arcpy.ListRasters()
    for chm in chms:
        if "height" in chm:
            print("Focal ..." + str(chm))
            outFocalStat5 = FocalStatistics(chm, NbrRectangle(5, 5, 'CELL'), "sum", "")
            outFocalStat5.save(ArcGIS_dir + chm + "_5")


def focal_analysis_height_c():
    print("Focal sum analysis")
    ArcGIS_dir = r"D:\temp\ArcGIS_project\Test\ALOS_LiDAR.gdb/"
    arcpy.env.workspace = ArcGIS_dir
    chms = ["Ras_height_2008", "Ras_height_2010", "Ras_height_2018"]

    for chm in chms:
        print("Focal ..." + str(chm))
        Ras_height_count = "Ras_cover_" + chm.split("_")[2] + "_s"
        outFocalStat5 = FocalStatistics(Ras_height_count, NbrRectangle(5, 5, 'CELL'), "sum", "")
        outFocalStat5.save(ArcGIS_dir + chm + "_c")



def cal_mean_height_5():
    ArcGIS_dir = r"D:\temp\ArcGIS_project\Test\ALOS_LiDAR.gdb/"
    years = ["2008", "2010", "2018"]
    for year in years:
        print(year)
        ras_sum = Raster(ArcGIS_dir + "Ras_height_" + year + "_5")
        ras_count = Raster(ArcGIS_dir + "Ras_height_" + year + "_c")
        ras_mh = ras_sum/(ras_count)
        ras_mh.save(ArcGIS_dir + "Ras_mheight_" + year + "_5")


def extract():
    dir = r"D:\temp\ArcGIS_project\Test\ALOS_LiDAR.gdb/"
    arcpy.env.workspace = dir
    years = ["2008", "2010", "2018"]
    for year in years:
        print(year)
        ras = dir + "Ras_mheight_" + year + "_5"
        ras = dir + "Ras_height_" + year + "_5"
        point = r"E:\ChangMap\CHM\DB_20220221\DB_shp\Point_" + year + ".shp"
        print("Extracting values to point..." + ras + " " + point)
        ExtractMultiValuesToPoints(point, ras + ' ' + "RASTERVALU", "NONE")
        print("Converting point to excel..." + ras + " " + point)
        out = r"E:\ChangMap\CHM\DB_20220221\DB_csv/"
        xlsx = out + "ALS_" + year + ".xlsx"
        arcpy.TableToExcel_conversion(point, xlsx)


def join_tables():
    dir = r"E:\ChangMap\CHM\DB_20220221\DB_csv/"
    suf = ["2008", "2010", "2018"]
    suf_2 = "Ind_"
    for s in suf:
        table1_dir = "E:\ChangMap\CHM\DB_20210926\DB_csv/" + suf_2 + s + ".csv"
        table2_dir = dir + "ALS_" + s + ".xlsx"
        df1 = pd.read_csv(table1_dir)
        df2 = pd.read_excel(table2_dir)
        df2.columns = ['FID', 'Merge_ID', 'H2']
        df2 = df2.iloc[:, 1:]
        df1 = pd.merge(df1, df2, left_on='Merge_ID', right_on='Merge_ID', how='left')
        print("Merging..." + s)
        output = dir + suf_2 + s + ".csv"
        df1 = df1[df1.H2 != -9999]
        df1.drop_duplicates(keep='first', inplace=True)
        df1.dropna(inplace=True)
        df1.to_csv(output,index=False)


