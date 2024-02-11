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


def zonalstats_SAR_100m():
    arc_dir = r"D:\temp\ArcGIS_project\ScanSAR\ScanSAR.gdb/"
    inpolygon = r"E:\ScanSAR\ScanSAR\Grid\Grid_100m.shp"
    arcpy.env.workspace = r"E:\ScanSAR\ScanSAR\single/"
    chms = arcpy.ListRasters('*.tif*')
    for chm in chms:
        print("Input ras: " + chm)
        table = arc_dir + "ScanSAR_" + \
                chm.split(".")[0].split("_")[6].split("-")[0] +\
                chm.split(".")[0].split("_")[6].split("-")[1] +\
                chm.split(".")[0].split("_")[6].split("-")[2] +\
                "_100m"
        print("Output zonal stats..." + table)
        ZonalStatisticsAsTable(inpolygon, "FID", chm, table, "", "MEAN")


def table_xlsx_100m():
    arcpy.env.workspace = r"D:\temp\ArcGIS_project\ScanSAR\ScanSAR.gdb/"
    tables = arcpy.ListTables()
    for table in tables:
        if "_100m" in table:
            print(table)
            xlsx = r"E:\ScanSAR\ScanSAR\table_100m/" + table + ".xlsx"
            arcpy.TableToExcel_conversion(table, xlsx)


def zonalstats_SA_100m():
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        arc_dir = r"D:\temp\ArcGIS_project\ScanSAR\ScanSAR.gdb/"
        inpolygon = r"E:\ScanSAR\ScanSAR\Grid/" + site + "_100m.shp"
        arcpy.env.workspace = r"E:\ScanSAR\ScanSAR\single/"
        chms = arcpy.ListRasters('*.tif*')
        for chm in chms:
            print("Input ras: " + chm)
            table = arc_dir + "ScanSAR_" + site + "_" +\
                    chm.split(".")[0].split("_")[6].split("-")[0] +\
                    chm.split(".")[0].split("_")[6].split("-")[1] +\
                    chm.split(".")[0].split("_")[6].split("-")[2] +\
                    "_100m"
            print("Output zonal stats..." + table)
            ZonalStatisticsAsTable(inpolygon, "FID", chm, table, "", "MEAN")


def table_SA_xlsx_100m():
    arcpy.env.workspace = r"D:\temp\ArcGIS_project\ScanSAR\ScanSAR.gdb/"
    tables = arcpy.ListTables()
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        for table in tables:
            if "ScanSAR_" + site in table:
                print(table)
                xlsx = r"E:\ScanSAR\ScanSAR\site_100m/" + site + "/" + table + ".xlsx"
                arcpy.TableToExcel_conversion(table, xlsx)


def zonalstats_CHM_100m():
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        arc_dir = r"D:\temp\ArcGIS_project\ScanSAR\ScanSAR.gdb/"
        inpolygon = r"E:\ScanSAR\ScanSAR\Grid/" + site + "_100m.shp"
        chm = r"E:\ALS_archive\LiDAR_CHM_Mosaic/" + site + ".tif"
        print("Input ras: " + chm)
        table = arc_dir + "CHM_" + site + "_100m"
        print("Output zonal stats..." + table)
        ZonalStatisticsAsTable(inpolygon, "FID", chm, table, "", "MEAN")


def table_CHM_xlsx_100m():
    arcpy.env.workspace = r"D:\temp\ArcGIS_project\ScanSAR\ScanSAR.gdb/"
    tables = arcpy.ListTables()
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        for table in tables:
            if "CHM_" + site in table:
                print(table)
                xlsx = r"E:\ScanSAR\ScanSAR\site_100m/" + site + "/" + table + ".xlsx"
                arcpy.TableToExcel_conversion(table, xlsx)


def resample_75m():
    arcpy.env.workspace = r"E:\ScanSAR\ScanSAR\single/"
    output = r"E:\ScanSAR\ScanSAR\Resample_75m/"
    chms = arcpy.ListRasters('*.tif*')
    for chm in chms:
        if int(chm.split("_")[6].split("-")[1]) == 9:
            print("Input ras: " + chm)
            arcpy.Resample_management(chm, output + chm, "75", "BILINEAR")


def output_table():
    arcpy.env.workspace = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
    output_dir = r"E:\ScanSAR\ScanSAR\Result_0517/"
    tables = arcpy.ListTables()
    for table in tables:
        if "Mean_AGBD" in table:
            print(table)
            outTable = output_dir + table + ".csv"
            arcpy.conversion.ExportTable(table, outTable)


def clean_table():
    dir = r"E:\ScanSAR\ScanSAR\Result_0517/"
    output = r"E:\ScanSAR\ScanSAR\Result_0517\Result_AGBD.csv"
    df_index = pd.read_csv(dir+"Index.csv")
    files = os.litsir(dir)
    for file in files:
        if "Mean_AGBD" in file:
            print(file.split(".")[0])
            df = pd.read_csv(dir+file)
            df_sel = df.iloc[:, -1]
            df_sel.columns = file.split(".")[0]
            df_index = pd.concat([df_index,df_sel],axis=1)
    df_index.to_csv(output, index=None)




def zonalstats_KNP_25m():
    arc_dir = r"D:\temp\ArcGIS_project\ScanSAR\ScanSAR.gdb/"
    inpolygon = r"E:\ScanSAR\GEDI\GEDI_adjusted_25.shp"
    arcpy.env.workspace = r"E:\ScanSAR\ScanSAR\single/"
    chms = arcpy.ListRasters('*.tif*')
    for chm in chms:
        print("Input ras: " + chm)
        table = arc_dir + "ScanSAR_KNP_" +\
                chm.split(".")[0].split("_")[6].split("-")[0] +\
                chm.split(".")[0].split("_")[6].split("-")[1] +\
                chm.split(".")[0].split("_")[6].split("-")[2] +\
                "_25m"
        print("Output zonal stats..." + table)
        ZonalStatisticsAsTable(inpolygon, "ORIG_FID", chm, table, "", "MEAN")


def output_KNP_25m_table():
    arcpy.env.workspace = r"D:\temp\ArcGIS_project\ScanSAR\ScanSAR.gdb/"
    output_dir = r"E:\ScanSAR\ScanSAR\Result_0531/"
    tables = arcpy.ListTables()
    for table in tables:
        if "ScanSAR_KNP_" in table:
            print(table)
            outTable = output_dir + table + ".csv"
            arcpy.conversion.ExportTable(table, outTable)



def zonalstats_KNP_ori_25m():
    arc_dir = r"D:\temp\ArcGIS_project\ScanSAR\ScanSAR.gdb/"
    inpolygon = r"E:\ScanSAR\GEDI\GEDI_ori_25.shp"
    arcpy.env.workspace = r"E:\ScanSAR\ScanSAR\single/"
    chms = arcpy.ListRasters('*.tif*')
    for chm in chms:
        print("Input ras: " + chm)
        table = arc_dir + "ScanSAR_KNP_ori_" +\
                chm.split(".")[0].split("_")[6].split("-")[0] +\
                chm.split(".")[0].split("_")[6].split("-")[1] +\
                chm.split(".")[0].split("_")[6].split("-")[2] +\
                "_25m"
        print("Output zonal stats..." + table)
        ZonalStatisticsAsTable(inpolygon, "ORIG_FID", chm, table, "", "MEAN")


def output_KNP_ori_25m_table():
    arcpy.env.workspace = r"D:\temp\ArcGIS_project\ScanSAR\ScanSAR.gdb/"
    output_dir = r"E:\ScanSAR\ScanSAR\Result_0531/"
    tables = arcpy.ListTables()
    for table in tables:
        if "ScanSAR_KNP_ori_" in table:
            print(table)
            outTable = output_dir + table + ".csv"
            arcpy.conversion.ExportTable(table, outTable)



def cover_test_1_31():
    shp_dir = r"E:\ScanSAR\archive\selected_area\Protectedareasselect_P.shp"
    arcpy.MakeFeatureLayer_management(shp_dir, 'Protected_shp')
    arcpy.SelectLayerByAttribute_management('Protected_shp', 'NEW_SELECTION', 'FID = 31')
    desc = arcpy.Describe("Protected_shp")
    extent = str(desc.extent.XMin) + " " + \
             str(desc.extent.YMin) + " " + \
             str(desc.extent.XMax) + " " + \
             str(desc.extent.YMax)
    print(extent)
    out_dir = r"E:\ScanSAR\ScanSAR\Result_0607\Sandringham Private Nature Reserve/"
    arcpy.env.workspace = r"E:\ScanSAR\ScanSAR\Map\Cover/"
    chms = arcpy.ListRasters('*.tif*')
    for chm in chms:
        print("Output..." + chm)
        # print("Output..." + outraster_i)
        arcpy.Clip_management(chm, extent, out_dir+chm,'Protected_shp', -999, "ClippingGeometry", "")




def sar_time_series():
    arc_dir = r"D:\temp\ArcGIS_project\ScanSAR\ScanSAR.gdb/"
    inpolygon = r"E:\ScanSAR\archive\selected_area\Protected areas selected 2.shp"
    arcpy.env.workspace = r"E:\ScanSAR\ScanSAR\single/"
    sars = arcpy.ListRasters('*.tif*')
    for sar in sars:
        table = arc_dir + "ScanSAR_ts_" +\
                sar.split(".")[0].split("_")[6].split("-")[0] +\
                sar.split(".")[0].split("_")[6].split("-")[1] +\
                sar.split(".")[0].split("_")[6].split("-")[2] +\
                "_25m"
        print("Output zonal stats..." + table)
        power_scale = 10**(Raster(sar)/10)
        ZonalStatisticsAsTable(inpolygon, "FID", power_scale, table, "", "MEAN")


def sar_time_series_table():
    arcpy.env.workspace = r"D:\temp\ArcGIS_project\ScanSAR\ScanSAR.gdb/"
    output_dir = r"E:\ScanSAR\ScanSAR\Result_0612/"
    tables = arcpy.ListTables()
    for table in tables:
        if "ScanSAR_ts_" in table:
            print(table)
            outTable = output_dir + table + ".csv"
            arcpy.conversion.ExportTable(table, outTable)


def GEDI_Reserve():
    years = ['2019', '2020', '2021', '2022']
    for year in years:
        dir_GEDI_shp = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/" + "GEDI_ori_25_" + year
        arcpy.MakeFeatureLayer_management(dir_GEDI_shp, 'GEDI_SHP')
        dir_reserve_shp = r"E:\ScanSAR\archive\selected_area\Protected areas selected 2.shp"
        arcpy.MakeFeatureLayer_management(dir_reserve_shp, 'Reserve_SHP')
        with arcpy.da.SearchCursor('Reserve_SHP', ['FID']) as cursor:
            for row in cursor:
                print(row[0])
                query = '"FID" = {0}'.format(row[0])
                arcpy.SelectLayerByAttribute_management("Reserve_SHP", 'NEW_SELECTION', query)
                arcpy.SelectLayerByLocation_management('GEDI_SHP', 'INTERSECT', 'Reserve_SHP')
                outTable = r"E:\ScanSAR\ScanSAR\Result_0614/" + str(year) + "/" + str(row[0]) + ".csv"
                arcpy.conversion.ExportTable('GEDI_SHP', outTable)


def Cover_ALS_site():
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        dir_boundary = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/" + site + "_clip"
        arcpy.MakeFeatureLayer_management(dir_boundary, 'Boundary_shp')
        desc = arcpy.Describe("Boundary_shp")
        extent = str(desc.extent.XMin) + " " + \
                 str(desc.extent.YMin) + " " + \
                 str(desc.extent.XMax) + " " + \
                 str(desc.extent.YMax)
        print(extent)
        out_dir = r"E:\ScanSAR\ScanSAR\Result_0614\Cover_ALS_site/" + site + "/"
        arcpy.env.workspace = r"E:\ScanSAR\ScanSAR\Map\Cover/"
        chms = arcpy.ListRasters('*.tif*')
        for chm in chms:
            print("Output..." + chm)
            # print("Output..." + outraster_i)
            arcpy.Clip_management(chm, extent, out_dir + chm, 'Boundary_shp', -999, "ClippingGeometry", "")





def zonalstats_CHM_75m():
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        arc_dir = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
        inpolygon = r"E:\ScanSAR\ScanSAR\Grid/" + site + "_75m.shp"
        chm = r"E:\ALS_archive\LiDAR_CHM_Mosaic_05/" + site + "_05.tif"
        print("Input ras: " + chm)
        table = arc_dir + "CHM_" + site + "_75m"
        print("Output zonal stats..." + table)
        ZonalStatisticsAsTable(inpolygon, "FID", chm, table, "", "MEAN")


def zonalstats_Cover_75m():
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        arc_dir = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
        inpolygon = r"E:\ScanSAR\ScanSAR\Grid/" + site + "_75m.shp"
        chm = r"E:\ALS_archive\LiDAR_CHM_CC15/" + site + ".tif"
        print("Input ras: " + chm)
        table = arc_dir + "Cover_" + site + "_75m"
        print("Output zonal stats..." + table)
        ZonalStatisticsAsTable(inpolygon, "FID", chm, table, "", "SUM")


def table_CHM_xlsx_75m():
    arcpy.env.workspace = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
    tables = arcpy.ListTables()
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        for table in tables:
            if "CHM_" + site + "_75m" in table:
                print(table)
                xlsx = r"E:\ScanSAR\ScanSAR\Result_0625\site_75m/" + site + "/" + table + ".xlsx"
                arcpy.TableToExcel_conversion(table, xlsx)


def table_Cover_xlsx_75m():
    arcpy.env.workspace = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
    tables = arcpy.ListTables()
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        for table in tables:
            if "Cover_" + site + "_75m" in table:
                print(table)
                xlsx = r"E:\ScanSAR\ScanSAR\Result_0625\site_75m/" + site + "/" + table + ".xlsx"
                arcpy.TableToExcel_conversion(table, xlsx)


def zonalstats_SA_75m():
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        arc_dir = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
        inpolygon = r"E:\ScanSAR\ScanSAR\Grid/" + site + "_75m.shp"
        arcpy.env.workspace = r"E:\ScanSAR\ScanSAR\single/"
        chms = arcpy.ListRasters('*.tif*')
        for chm in chms:
            print("Input ras: " + chm)
            table = arc_dir + "ScanSAR_" + site + "_" +\
                    chm.split(".")[0].split("_")[6].split("-")[0] +\
                    chm.split(".")[0].split("_")[6].split("-")[1] +\
                    chm.split(".")[0].split("_")[6].split("-")[2] +\
                    "_75m"
            print("Output zonal stats..." + table)
            ZonalStatisticsAsTable(inpolygon, "FID", chm, table, "", "MEAN")


def table_SA_xlsx_75m():
    arcpy.env.workspace = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
    tables = arcpy.ListTables()
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        for table in tables:
            if "ScanSAR_" + site in table:
                print(table)
                xlsx = r"E:\ScanSAR\ScanSAR\Result_0625\site_75m/" + site + "/" + table + ".xlsx"
                arcpy.TableToExcel_conversion(table, xlsx)


def map_Volume_Cover_AGB_75m():
    equation = r"E:\ScanSAR\ScanSAR\Result_0625\site_75m/equation.csv"
    df = pd.read_csv(equation)
    output_volume_dir = r"E:\ScanSAR\ScanSAR\Result_0625\Map\Volume/"
    output_cover_dir = r"E:\ScanSAR\ScanSAR\Result_0625\Map\Cover/"
    output_agbd_dir =  r"E:\ScanSAR\ScanSAR\Result_0625\Map\AGBD/"
    arcpy.env.workspace = r"E:\ScanSAR\ScanSAR\Resample_75m/"
    chms = arcpy.ListRasters('*.tif*')
    for chm in chms:
        suf = chm.split("_")[6].split("-")[0] + chm.split("_")[6].split("-")[1] + chm.split("_")[6].split("-")[2].split(".")[0]
        print(suf)
        a_volume = df.loc[df['Date'] == int(suf), 'a'].values[0]
        b_volume = df.loc[df['Date'] == int(suf), 'b'].values[0]
        a_cover = df.loc[df['Date'] == int(suf), 'a'].values[1]
        b_cover = df.loc[df['Date'] == int(suf), 'b'].values[1]
        a_AGBD = df.loc[df['Date'] == int(suf), 'a'].values[2]
        b_AGBD = df.loc[df['Date'] == int(suf), 'b'].values[2]
        ras_volume = Exp(Raster(chm)*a_volume + b_volume)
        ras_volume.save(output_volume_dir + "R" + suf + "_volume.tif")
        ras_cover = Exp(Raster(chm)*a_cover + b_cover)
        ras_cover.save(output_cover_dir + "R" + suf + "_cover.tif")
        ras_AGBD = Exp(Raster(chm)*a_AGBD + b_AGBD)
        ras_AGBD.save(output_agbd_dir + "R" + suf + "_agb.tif")


def zonalstats_AGBD_sum():
    inpolygon = r"E:\ScanSAR\archive\selected_area\Protected areas selected 2.shp"
    arc_dir = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
    arcpy.env.workspace = r"E:\ScanSAR\ScanSAR\Result_0625\Map\AGBD/"
    chms = arcpy.ListRasters('*.tif*')
    for chm in chms:
        print("Input ras: " + chm)
        table = arc_dir + "ScanSAR_0630_" + chm.split(".")[0]
        print("Output zonal AGBD stats..." + table)
        ZonalStatisticsAsTable(inpolygon, "FID", chm, table, "", "SUM")


def zonalstats_cover_mean():
    inpolygon = r"E:\ScanSAR\archive\selected_area\Protected areas selected 2.shp"
    arc_dir = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
    arcpy.env.workspace = r"E:\ScanSAR\ScanSAR\Result_0625\Map\Cover/"
    chms = arcpy.ListRasters('*.tif*')
    for chm in chms:
        print("Input ras: " + chm)
        table = arc_dir + "ScanSAR_0630_" + chm.split(".")[0]
        print("Output zonal AGBD stats..." + table)
        ZonalStatisticsAsTable(inpolygon, "FID", chm, table, "", "MEAN")


def table_AGBD_cover_mean_xlsx():
    arcpy.env.workspace = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
    tables = arcpy.ListTables()
    for table in tables:
            if "ScanSAR_0630_" in table:
                print(table)
                xlsx = r"E:\ScanSAR\ScanSAR\Result_0630/" + table + ".xlsx"
                arcpy.TableToExcel_conversion(table, xlsx)



def map_diff_0809():
    #type = "Cover"
    #suf = "_cover.tif"
    type = "AGBD"
    suf = "_agb.tif"
    #type = "Volume"
    #suf = "_volume.tif"

    dir = r"E:\ScanSAR\ScanSAR\Result_0625\Map/" + type + "/"
    output_dir = r"E:\ScanSAR\ScanSAR\Result_0809/"
    arcpy.env.workspace = dir

    chm_2022 = dir + "R20220924" + suf
    chm_2021 = dir + "R20210925" + suf
    chm_2018 = dir + "R20180929" + suf
    chm_2016 = dir + "R20160903" + suf
    chm_2014 = dir + "R20140906" + suf

    R_2022_2018 = Raster(chm_2022) - Raster(chm_2018)
    R_2021_2018 = Raster(chm_2021) - Raster(chm_2018)
    R_2018_2016 = Raster(chm_2018) - Raster(chm_2016)
    R_2018_2014 = Raster(chm_2018) - Raster(chm_2014)
    R_2022_2014 = Raster(chm_2022) - Raster(chm_2014)

    #R_2022_2018.save(output_dir + "Diff_2022_2018" + suf)
    #R_2021_2018.save(output_dir + "Diff_2021_2018" + suf)
    #R_2018_2016.save(output_dir + "Diff_2018_2016" + suf)
    #R_2018_2014.save(output_dir + "Diff_2018_2014" + suf)
    #R_2022_2014.save(output_dir + "Diff_2022_2014" + suf)




#20231004
def zonalstats_hcc_sar_25():
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        arc_dir = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
        inpolygon = r"E:\ScanSAR\ScanSAR\Result_1004\shp_25m/" + site + "_25m_SJ.shp"
        chm = r"E:\ALS_archive\LiDAR_CHM_Mosaic_05/" + site + "_05.tif"
        cc = r"E:\ALS_archive\LiDAR_CHM_CC15/" + site + ".tif"
        sar = r"E:\ScanSAR\ScanSAR\single/PALSAR2_ScanSAR_HV_mtf_5_db_2018-09-29.tif"
        out_dir = r"E:\ScanSAR\ScanSAR\Result_1004\table/"

        table_chm = arc_dir + site + "_chm_25m"
        table_cc = arc_dir + site + "_cc_25m"
        table_sar = arc_dir + site + "_sar_25m"
        print("Input CHM: " + table_chm)
        print("Input CC: " + table_cc)
        print("Input SAR: " + table_sar)
        ZonalStatisticsAsTable(inpolygon, "FID", chm, table_chm, "", "MEAN")
        ZonalStatisticsAsTable(inpolygon, "FID", cc, table_cc, "", "SUM")
        ZonalStatisticsAsTable(inpolygon, "FID", sar, table_sar, "", "MEAN")

        outTable_chm = site + "_chm_25m.csv"
        outTable_cc = site + "_cc_25m.csv"
        outTable_sar = site + "_sar_25m.csv"
        print("Output CHM table: " + outTable_chm)
        print("Output CC table: " + outTable_cc)
        print("Output SAR table: " + outTable_sar)
        arcpy.conversion.TableToTable(table_chm, out_dir, outTable_chm)
        arcpy.conversion.TableToTable(table_cc, out_dir, outTable_cc)
        arcpy.conversion.TableToTable(table_sar, out_dir, outTable_sar)
        #OutRas_hcc = OutRas_chm*OutRas_cc/625*9.0665

def table_25():
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        dir = r"E:\ScanSAR\ScanSAR\Result_1004\shp_25m/"
        out_dir = r"E:\ScanSAR\ScanSAR\Result_1004\table/"
        arcpy.env.workspace = dir
        files = arcpy.ListFeatureClasses()
        for file in files:
            if "SJ" in file:
                print("SJ output table..." + file)
                table_chm = dir + file
                outTable_chm = out_dir + site + "_index_25m.csv"
                arcpy.TableToTable_conversion(table_chm, out_dir, outTable_chm)

#20231004
def zonalstats_hcc_sar_100():
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        arc_dir = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
        inpolygon = r"E:\ScanSAR\ScanSAR\Result_1004\shp_1ha/" + site + "_1ha.shp"
        chm = r"E:\ALS_archive\LiDAR_CHM_Mosaic_05/" + site + "_05.tif"
        cc = r"E:\ALS_archive\LiDAR_CHM_CC15/" + site + ".tif"
        sar = r"E:\ScanSAR\ScanSAR\single/PALSAR2_ScanSAR_HV_mtf_5_db_2018-09-29.tif"
        out_dir = r"E:\ScanSAR\ScanSAR\Result_1004\table/"

        table_chm = arc_dir + site + "_chm_1ha"
        table_cc = arc_dir + site + "_cc_1ha"
        table_sar = arc_dir + site + "_sar_1ha"
        print("Input CHM: " + table_chm)
        print("Input CC: " + table_cc)
        print("Input SAR: " + table_sar)
        ZonalStatisticsAsTable(inpolygon, "FID", chm, table_chm, "", "MEAN")
        ZonalStatisticsAsTable(inpolygon, "FID", cc, table_cc, "", "SUM")
        ZonalStatisticsAsTable(inpolygon, "FID", sar, table_sar, "", "MEAN")

        outTable_chm = site + "_chm_1ha.csv"
        outTable_cc = site + "_cc_1ha.csv"
        outTable_sar = site + "_sar_1ha.csv"
        print("Output CHM table: " + outTable_chm)
        print("Output CC table: " + outTable_cc)
        print("Output SAR table: " + outTable_sar)
        arcpy.conversion.TableToTable(table_chm, out_dir, outTable_chm)
        arcpy.conversion.TableToTable(table_cc, out_dir, outTable_cc)
        arcpy.conversion.TableToTable(table_sar, out_dir, outTable_sar)



def table_100():
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        dir = r"E:\ScanSAR\ScanSAR\Result_1004\shp_1ha/"
        out_dir = r"E:\ScanSAR\ScanSAR\Result_1004\table/"
        arcpy.env.workspace = dir
        files = arcpy.ListFeatureClasses()
        for file in files:
            if "_1ha" in file:
                print("1ha output table..." + file)
                table = dir + file
                outTable = out_dir + site + "_index_1ha.csv"
                arcpy.TableToTable_conversion(table, out_dir, outTable)


def merge_chunk():
    dir = r"E:\ScanSAR\ScanSAR\Result_1004\MC_100\Field_ALS_SAR/"
    #dir = r"E:\ScanSAR\ScanSAR\Result_1004\MC_100\Field_ALS_GEDI_SAR_glm/"
    #dir = r"E:\ScanSAR\ScanSAR\Result_1004\MC_100\Field_ALS_GEDI_SAR_rf/"

    list1 = []
    list2 = []
    outraster_agbd = "SAR_AGBD_2018_1ha.tif"
    outraster_uncertainty = "SAR_uncertainty_2018_1ha.tif"

    for i in range(1,5):
        print(i)
        SAR_AGBD_agbd = dir + "SAR_AGBD_" + str(i) + ".tif"
        SAR_AGBD_uncertainty = dir + "SAR_Uncertainty_" + str(i) + ".tif"
        list1.append(SAR_AGBD_agbd)
        list2.append(SAR_AGBD_uncertainty)

    arcpy.MosaicToNewRaster_management(list1, dir, outraster_agbd, "", "32_BIT_FLOAT", 100, 1, "", "FIRST")
    arcpy.MosaicToNewRaster_management(list2, dir, outraster_uncertainty, "", "32_BIT_FLOAT", 100, 1, "", "FIRST")


def zonalstats_hcc_gedi_1ha():
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        arc_dir = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
        inpolygon = r"E:\ScanSAR\ScanSAR\Result_1004\shp_1ha/" + site + "_1ha_SJ.shp"
        chm = r"E:\ALS_archive\LiDAR_CHM_Mosaic_05/" + site + "_05.tif"
        cc = r"E:\ALS_archive\LiDAR_CHM_CC15/" + site + ".tif"
        out_dir = r"E:\ScanSAR\ScanSAR\Result_1004\table/"

        table_chm = arc_dir + site + "_GEDI_chm_1ha"
        table_cc = arc_dir + site + "_GEDI_cc_1ha"

        print("Input CHM: " + table_chm)
        print("Input CC: " + table_cc)

        ZonalStatisticsAsTable(inpolygon, "FID", chm, table_chm, "", "MEAN")
        ZonalStatisticsAsTable(inpolygon, "FID", cc, table_cc, "", "SUM")

        outTable_chm = site + "_GEDI_chm_1ha.csv"
        outTable_cc = site + "_GEDI_cc_1ha.csv"

        print("Output CHM table: " + outTable_chm)
        print("Output CC table: " + outTable_cc)

        arcpy.conversion.TableToTable(table_chm, out_dir, outTable_chm)
        arcpy.conversion.TableToTable(table_cc, out_dir, outTable_cc)


def table_sj_100():
    dir = r"E:\ScanSAR\ScanSAR\Result_1004\shp_1ha/"
    out_dir = r"E:\ScanSAR\ScanSAR\Result_1004\table/"
    arcpy.env.workspace = dir
    files = arcpy.ListFeatureClasses()
    for file in files:
        if "_1ha_SJ" in file:
            table = dir + file
            outTable = file.split("_")[0] + "_GEDI_1ha.csv"
            print("1ha output table..." + outTable)
            arcpy.TableToTable_conversion(table, out_dir, outTable)



def sar_knp_als_zonal():
    arc_dir = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
    inpolygon = r"E:\ScanSAR\ScanSAR\Result_1004\shp_1ha\Merge_1ha_ALS.shp"
    ras_dir = r"E:\ScanSAR\ScanSAR\single/"
    chms = ['2014-09-06','2016-09-03','2017-09-02','2018-09-29','2019-09-28','2020-09-26','2021-09-25','2022-09-24']
    chms = ['2023-09-23']

    for chm in chms:
        print("Input ras: " + chm)
        chm_full = ras_dir + "PALSAR2_ScanSAR_HV_mtf_5_db_" + chm + ".tif"
        table = arc_dir + "ScanSAR_ALS_1ha_" + chm.split("-")[0]
        ZonalStatisticsAsTable(inpolygon, "FID", chm_full, table, "", "MEAN", "")
        #output zonal stats
        out_dir = r"E:\ScanSAR\ScanSAR\Result_1004\table_stats/"
        outTable = "ALS_1ha_" + chm.split("-")[0] + ".csv"
        arcpy.TableToTable_conversion(table, out_dir, outTable)
        print("Output shp..." + outTable)



def finebeam_knp_als_zonal():
    arc_dir = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
    inpolygon = r"E:\ScanSAR\ScanSAR\Result_1004\shp_1ha\Merge_1ha_ALS.shp"
    chm_full = r"E:\ChangMap\ALOS\east\Individual_dates/20170702_HV_compr.tif"
    table = arc_dir + "Finebeam_ALS_1ha_"
    ZonalStatisticsAsTable(inpolygon, "FID", chm_full, table, "", "MEAN", "")
    #output zonal stats
    out_dir = r"E:\ScanSAR\ScanSAR\Result_1004\table_stats/"
    outTable = "Finebeam_1ha.csv"
    arcpy.TableToTable_conversion(table, out_dir, outTable)
    print("Output shp..." + outTable)


def chm_cc_knp_als_zonal():
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        arc_dir = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
        inpolygon = r"E:\ScanSAR\ScanSAR\Result_1004\shp_1ha\Merge_1ha_ALS.shp"
        chm_dir = r"E:\ALS_archive\LiDAR_CHM_Mosaic_05/"
        cc_dir = r"E:\ALS_archive\LiDAR_CHM_CC15/"
        print("Input ras: " + site)
        chm = chm_dir + site + "_05.tif"
        cc = cc_dir + site + ".tif"
        table_chm = arc_dir + "CHM_" + site + "_1ha"
        table_cc = arc_dir + "CC_" + site + "_1ha"
        print("Output zonal stats..." + table_chm)
        ZonalStatisticsAsTable(inpolygon, "FID", chm, table_chm, "", "MEAN")
        ZonalStatisticsAsTable(inpolygon, "FID", cc, table_cc, "", "SUM")
        #output zonal stats
        out_dir = r"E:\ScanSAR\ScanSAR\Result_1004\table_stats/"
        outTable_chm = "ALS_1ha_" + site + "_CHM.csv"
        outTable_cc = "ALS_1ha_" + site + "_CC.csv"
        arcpy.TableToTable_conversion(table_chm, out_dir, outTable_chm)
        arcpy.TableToTable_conversion(table_cc, out_dir, outTable_cc)
        print("Output shp..." + outTable_chm)
        print("Output shp..." + outTable_cc)


def sar_knp_gedi_zonal():
    arc_dir = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
    inpolygon = r"E:\ScanSAR\ScanSAR\Result_1004\shp_1ha\Merge_1ha_GEDI.shp"
    ras_dir = r"E:\ScanSAR\ScanSAR\single/"
    chms = ['2014-09-06','2016-09-03','2017-09-02','2018-09-29','2019-09-28','2020-09-26','2021-09-25','2022-09-24']
    for chm in chms:
        print("Input ras: " + chm)
        chm_full = ras_dir + "PALSAR2_ScanSAR_HV_mtf_5_db_" + chm + ".tif"
        table = arc_dir + "ScanSAR_GEDI_1ha_" + chm.split("-")[0]
        ZonalStatisticsAsTable(inpolygon, "shot_numbe", chm_full, table, "", "MEAN", "")
        #output zonal stats
        out_dir = r"E:\ScanSAR\ScanSAR\Result_1004\table_stats/"
        outTable = "GEDI_1ha_" + chm.split("-")[0] + ".csv"
        arcpy.TableToTable_conversion(table, out_dir, outTable)
        print("Output shp..." + outTable)


def chm_cc_knp_gedi_zonal():
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        arc_dir = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
        inpolygon = r"E:\ScanSAR\ScanSAR\Result_1004\shp_1ha\Merge_1ha_GEDI.shp"
        chm_dir = r"E:\ALS_archive\LiDAR_CHM_Mosaic_05/"
        cc_dir = r"E:\ALS_archive\LiDAR_CHM_CC15/"
        print("Input ras: " + site)
        chm = chm_dir + site + "_05.tif"
        cc = cc_dir + site + ".tif"
        table_chm = arc_dir + "CHM_" + site + "_1ha"
        table_cc = arc_dir + "CC_" + site + "_1ha"
        print("Output zonal stats..." + table_chm)
        ZonalStatisticsAsTable(inpolygon, "FID", chm, table_chm, "", "MEAN")
        ZonalStatisticsAsTable(inpolygon, "FID", cc, table_cc, "", "SUM")
        #output zonal stats
        out_dir = r"E:\ScanSAR\ScanSAR\Result_1004\table_stats/"
        outTable_chm = "GEDI_1ha_" + site + "_CHM.csv"
        outTable_cc = "GEDI_1ha_" + site + "_CC.csv"
        arcpy.TableToTable_conversion(table_chm, out_dir, outTable_chm)
        arcpy.TableToTable_conversion(table_cc, out_dir, outTable_cc)
        print("Output shp..." + outTable_chm)
        print("Output shp..." + outTable_cc)


def finebeam_knp_gedi_zonal():
    arc_dir = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
    inpolygon = r"E:\ScanSAR\ScanSAR\Result_1004\shp_1ha\Merge_1ha_GEDI.shp"
    chm_full = r"E:\ChangMap\ALOS\east\Individual_dates/20170702_HV_compr.tif"
    table = arc_dir + "Finebeam_ALS_1ha_"
    ZonalStatisticsAsTable(inpolygon, "FID", chm_full, table, "", "MEAN", "")
    #output zonal stats
    out_dir = r"E:\ScanSAR\ScanSAR\Result_1004\table_stats/"
    outTable = "Finebeam_1ha_gedi.csv"
    arcpy.TableToTable_conversion(table, out_dir, outTable)
    print("Output shp..." + outTable)


def ScanSAR_25():
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        print(site)
        arc_dir = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
        inpolygon = arc_dir + site + "_25m_SJ"
        SAR = r"E:\ScanSAR\ScanSAR\single\PALSAR2_ScanSAR_HV_mtf_5_db_2018-09-29.tif"
        table = arc_dir + site + "_25m_SJ_t"
        ZonalStatisticsAsTable(inpolygon, "TARGET_FID", SAR, table, "", "MEAN", "")
        #output zonal stats
        out_dir = r"E:\ScanSAR\ScanSAR\Result_20140111\SAR_25m/"
        outTable = site + "_SAR_25m.csv"
        arcpy.TableToTable_conversion(table, out_dir, outTable)
        print("Output shp..." + outTable)


def ALS_25m_H():
    arc_dir = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        print(site)
        shp = arc_dir + site + "_25m_SJ"
        chm = r"E:\ALS_archive\LiDAR_CHM_Mosaic_05/" + site + "_05.tif"
        table = arc_dir + site + "_25m_H_t"
        ZonalStatisticsAsTable(shp, "TARGET_FID", chm, table, "", "MEAN", "")
        #OUTPUT
        out_dir = r"E:\ScanSAR\ScanSAR\Result_20140111\ALS_25m/"
        out_table = site + "_H_25m.csv"
        arcpy.TableToTable_conversion(table, out_dir, out_table)
        print("Output shp..." + out_table)


def ALS_25m_CC():
    arc_dir = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        print(site)
        shp = arc_dir + site + "_25m_SJ"
        chm = r"E:\ALS_archive\LiDAR_CHM_CC15/" + site + ".tif"
        table = arc_dir + site + "_25m_cc_t"
        ZonalStatisticsAsTable(shp, "TARGET_FID", chm, table, "", "SUM", "")
        #OUTPUT
        out_dir = r"E:\ScanSAR\ScanSAR\Result_20140111\ALS_25m/"
        out_table = site + "_CC_25m.csv"
        arcpy.TableToTable_conversion(table, out_dir, out_table)
        print("Output shp..." + out_table)


def ALS_25m_AGBD_map():
    arc_dir = r"D:\temp\ArcGIS_project\ScanSAR_New\ScanSAR_New.gdb/"
    studysite = ['Agincourt', 'Welverdiendt', 'Justicia', 'Ireagh', 'Limpopo1', 'Limpopo2', 'Limpopo3']
    for site in studysite:
        print(site)
        H = r"E:\ALS_archive\LiDAR_CHM_Mosaic_05/" + site + "_05.tif"
        CC = r"E:\ALS_archive\LiDAR_CHM_CC15/" + site + ".tif"
        AGB = 9.0665*Raster(H)*Raster(CC)
        #OUTPUT
        out_dir = r"E:\ScanSAR\ScanSAR\Result_20140111\ALS_25m_map/"
        out_map = out_dir + site + "_AGBD_25m.tif"
        print("Output shp..." + out_map)
        AGB.save(out_map)

