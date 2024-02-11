# Author: Xiaoxuan Li
import arcpy
import pandas as pd
import os
import arcpy
from arcpy.sa import *
from datetime import date
import numpy as np
import shutil
arcpy.env.overwriteOutput = True
import subprocess

csv_dir = "E:\ChangMap\CHM\DB_20220306\DB_csv/"
shp_dir = "E:\ChangMap\CHM\DB_20220306\DB_shp/"
dir = "E:\ChangMap\CHM\DB_20220306\DB_SAR/"
ori_dir = dir + "Original/"
power_dir = dir + "Power/"
focal_dir = dir + "Focal/"
indb_dir = dir + "DB/"

def SAR_power():
    arcpy.env.workspace = ori_dir
    chms = arcpy.ListRasters('*.tif*')
    for chm in chms:
        print("Power scale to db scale: " + chm)
        ras = Raster(ori_dir + chm)
        rasterPwr = np.power(10., ras / 10.)
        rasterPwr.save(power_dir + chm)


def SAR_aggregate():
    arcpy.env.workspace = power_dir
    chms = arcpy.ListRasters()
    for chm in chms:
        print("Focal ..." + str(chm))
        ras = Raster(power_dir + chm)
        outFocalStat5 = FocalStatistics(ras, NbrRectangle(5, 5, 'CELL'), "mean", "")
        outFocalStat5.save(focal_dir + chm.split(".")[0] + "_5.tif")


def SAR_db():
    arcpy.env.workspace = focal_dir
    chms = arcpy.ListRasters('*.tif*')
    for chm in chms:
        print("Inverting power scale to db scale: " + chm)
        ras = Raster(focal_dir + chm)
        rasterdB = 10 * Log10(ras)
        rasterdB.save(indb_dir + chm)


def extract_indb():
    arcpy.env.workspace = indb_dir
    chms = arcpy.ListRasters('*.tif*')
    years = ["2008", "2010", "2018"]
    for year in years:
        print(year)
        point = shp_dir + "Point_" + year + ".shp"
        for chm in chms:
            if year in chm:
                print("Extract raster values to point: " + chm)
                ExtractMultiValuesToPoints(point, indb_dir + chm + ' ' + "RASTERVALU", "NONE")



def output_csv():
    suf = ["2008", "2010", "2018"]
    for s in suf:
        # output SAR tables
        xls = csv_dir + "ALS_" + s + ".xlsx"
        fc = shp_dir + "Point_" + s + ".shp"
        output = csv_dir + "ALS_" + s + ".csv"
        print(xls)
        arcpy.TableToExcel_conversion(fc, xls)
        df = pd.read_excel(xls)
        df = df.iloc[:, 1:]
        if s == "2008" or s == "2018":
            df.columns = ['Merge_ID', 'HH7_indb', 'HH_indb', 'HV7_indb']
        elif s == "2010":
            df.columns = ['Merge_ID', 'HH_indb', 'HV_indb']
        df.drop_duplicates(keep='first', inplace=True)
        df.dropna(inplace=True)
        df.to_csv(output, index=False)


def join_tables():
    suf = ["2008", "2010", "2018"]
    for s in suf:
        table1_dir = csv_dir + 'Ind_' + s + ".csv"
        table2_dir = csv_dir + "ALS_" + s + ".csv"
        df1 = pd.read_csv(table1_dir)
        df2 = pd.read_csv(table2_dir)
        df1 = pd.merge(df1, df2, left_on='Merge_ID', right_on='Merge_ID', how='left')
        print("Merging..." + s)
        output = csv_dir + "Merge_" + s + ".csv"
        df1 = df1[df1.V2 != -9999]
        df1.drop_duplicates(keep='first', inplace=True)
        df1.dropna(inplace=True)
        df1.to_csv(output,index=False)




def poly_join_ras():
    suf = ["2010_2008", "2018_2008", "2018_2010"]
    for s in suf:
        inpolygon = "E:\ChangMap\CHM\DB_20220317\DB_shp\Poly_"+ s +".shp"
        table = "E:\ChangMap\CHM\DB_20220317\DB_csv\DB_"+ s +".csv"
        print("Join..." + s)
        arcpy.MakeFeatureLayer_management(inpolygon, "temp_layer")
        arcpy.AddJoin_management("temp_layer", "Merge_ID", table, "Merge_ID", "KEEP_COMMON")
        output = "E:\ChangMap\CHM\DB_20220317\Change/" + "Ras_" + s + ".tif"
        print("Select by attribute... "+ s)
        arcpy.SelectLayerByAttribute_management('temp_layer', 'NEW_SELECTION', '"pred" >= -1')
        print("Poly to ras..." + s)
        arcpy.PolygonToRaster_conversion("temp_layer", "pred", output, "", "", 0.00013475)
        arcpy.Delete_management("temp_layer")

