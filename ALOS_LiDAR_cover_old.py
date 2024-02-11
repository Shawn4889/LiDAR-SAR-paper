import arcpy
from arcpy.sa import *
import pandas as pd
import os

arcpy.env.overwriteOutput = True
DB_dir = r"E:\ChangMap\CHM\DB/"
DB_shp = r"E:\ChangMap\CHM\DB_shp/"
DB_csv = r"E:\ChangMap\CHM\DB_csv/"
DB_cover = r"E:\ChangMap\CHM\DB_cover/"
CHM_dir = r"E:\ChangMap\CHM\CHM\CHM_cover/"
ArcGIS_dir = r"D:\temp\ArcGIS_project\Test\ALOS_LiDAR.gdb/"
#ALOS_dir = r"E:\ChangMap\ALOS\Individual_dates-20210129T000312Z-001\Individual_dates/"
ALOS_dir = r"E:\ChangMap\ALOS\Change_SouthAfrica-20201216T102802Z-001\Change_SouthAfrica/"
clip_polygon = r"D:\temp\ArcGIS_project\Test\Test.gdb\Clip"
mask_dir = r"E:\ChangMap\site masks/"
ras_resolution = 0.00013475


def loop(site,suf):
    Location = site
    ALOS_suf = suf
    result_suf = "_ALOS_" + ALOS_suf.split("_")[0][0:4] + "_" + \
                 ALOS_suf.split("_")[1] + "_" + \
                 ALOS_suf.split("_")[2][0]
    initilization()
    alos_clip(Location,ALOS_suf,result_suf)
    float_to_int(Location,result_suf)
    zonalstats(Location,ALOS_suf,result_suf)
    join(Location,result_suf)
    refield(Location,result_suf)
    PolyRas(Location,result_suf)
    focal_analysis(Location,result_suf)
    dataclean(Location,result_suf)
    mergecsv()
    spatialjoin(Location,result_suf)
    output(Location,result_suf)


def initilization():
    print("Deleting previous files...")
    arcpy.env.workspace = ArcGIS_dir
    fc_list = arcpy.ListFeatureClasses()
    tables = arcpy.ListTables()
    ds_list = arcpy.ListDatasets()
    for fc in fc_list:
        if "result" not in fc:
            arcpy.Delete_management(fc)
    for table in tables:
        if "result" not in table:
            arcpy.Delete_management(table)
    for ds in ds_list:
        if "result" not in ds:
            arcpy.Delete_management(ds)
    DBs = os.listdir(DB_dir)
    for DB in DBs:
        if "result" not in DB:
            os.remove(DB_dir + DB)


#Clip ALOS using clip (geodatabase)
def alos_clip(Location,ALOS_suf,result_suf):
    print(result_suf)
    print("Clip ALOS images using site based shapefile")
    arcpy.MakeFeatureLayer_management(clip_polygon, "fLayer")
    query = """ "Name" = '%s'""" % Location
    arcpy.SelectLayerByAttribute_management("fLayer", 'NEW_SELECTION', query)
    inraster = ALOS_dir + ALOS_suf
    outraster = ArcGIS_dir + Location + result_suf
    print("Clipping: " + inraster + " using " + clip_polygon)
    desc = arcpy.Describe("fLayer")
    extent = str(desc.extent.XMin) + " " + str(desc.extent.YMin) + " " + str(desc.extent.XMax) + " " + str(
        desc.extent.YMax)
    print("Clipping raster")
    arcpy.Clip_management(inraster, extent, outraster, "fLayer", "", "ClippingGeometry", "NO_MAINTAIN_EXTENT")


#float ALOS to int
def float_to_int(Location,result_suf):
    print("ALOS float to int")
    inraster = ArcGIS_dir + Location + result_suf
    outraster = Int(Raster(inraster) * 1000000)
    outPolygon = inraster + "_i"
    arcpy.RasterToPolygon_conversion(outraster, outPolygon, "", "VALUE", "", "")


#zonal statistical as table: ALOS polygon ~ CHM change/difference rasters
def zonalstats(Location,ALOS_suf,result_suf):
    print("Zonal stats: ALOS polygon and CHM derived difference map stack as inputs")
    inpolygon = ArcGIS_dir + Location + result_suf + "_i"
    #inraster = CHM_dir + Location + ALOS_suf.split("_")[0][0:4] + ".tif"
    inraster = CHM_dir + Location + "_Diff_" + ALOS_suf.split("_")[2] + "_" + ALOS_suf.split("_")[1] + ".tif"
    print("Input polygon: " + inpolygon)
    table = inpolygon + "_z_1"
    ZonalStatisticsAsTable(inpolygon, "id", inraster, table)


def join(Location,result_suf):
    arcpy.CopyFeatures_management(ArcGIS_dir + Location + result_suf + "_i",
                                  ArcGIS_dir + Location + result_suf + "_i_w")
    inpolygon = ArcGIS_dir + Location + result_suf + "_i"
    arcpy.JoinField_management(inpolygon, "id", inpolygon + "_z_1", "id", ["COUNT"])


def refield(Location,result_suf):
    inpolygon = ArcGIS_dir + Location + result_suf + "_i"
    xls = DB_dir + Location + result_suf + "_i" + ".xlsx"
    csv = DB_dir + Location + result_suf + "_i" + ".csv"
    print("Output xlsx: " + xls)
    arcpy.TableToExcel_conversion(inpolygon, xls)
    #clean data
    data_xls = pd.read_excel(xls,header=0)
    print("Dropping columns...")
    #data_xls.drop(['Join_Count', 'TARGET_FID', 'Id', "OBJECTID_1", "OBJECTID_12",
    #               'pointid', "pointid_1", "pointid_12", "Shape_Length", "Shape_Area"], axis=1, inplace=True)
    print("Renaming columns...")
    data_xls.columns = ["OBJECTID", "ID", "CA_o", "length", "area", "C_c"]
    data_xls.to_csv(csv, index=None)
    os.remove(xls)
    print("Joining csv to shp...")
    inpolygon = ArcGIS_dir + Location + result_suf + "_i_w"
    arcpy.TableToTable_conversion(csv, ArcGIS_dir, "result_csv")
    arcpy.JoinField_management(inpolygon, "OBJECTID", ArcGIS_dir + "result_csv", "OBJECTID")


#convert polygon back to raster based on different attributes
def PolyRas(Location,result_suf):
    fields = ["CA_o", "C_c"]
    for field in fields:
        inFeatures = ArcGIS_dir + Location + result_suf + "_i_w"
        outRaster = ArcGIS_dir + Location + result_suf + "_i_w_" + field
        print("Polygon to raster..." + outRaster)
        arcpy.PolygonToRaster_conversion(inFeatures, field, outRaster,"", "", ras_resolution)

#focal analysis (mean,max,sum)
def focal_analysis(Location,result_suf):
    print("ALOS Focal analysis, including three: mean, max, sum")
    arcpy.env.workspace = ArcGIS_dir
    codes = arcpy.ListRasters("*")
    subdir = Location + result_suf + "_i_w_C"
    for inraster in codes:
        if subdir in inraster:
            print("Input raster: " + inraster)
            outFocalStat3 = FocalStatistics(inraster, NbrRectangle('', '', ''), "SUM", "")
            outraster = ArcGIS_dir + str(inraster) + "_sum3"
            outFocalStat3.save(outraster)
            outFocalStat3 = FocalStatistics(inraster, NbrRectangle('5', '5', ''), "SUM", "")
            outraster = ArcGIS_dir + str(inraster) + "_sum5"
            outFocalStat3.save(outraster)
            outFocalStat3 = FocalStatistics(inraster, NbrRectangle('9', '9', ''), "SUM", "")
            outraster = ArcGIS_dir + str(inraster) + "_sum9"
            outFocalStat3.save(outraster)
            #arcpy.RasterToPolygon_conversion(outraster, outraster + "_p", "", "VALUE", "", "")
    '''
    tempfc = Location + result_suf + "_temp"
    outfeature = Location + result_suf + "_result"
    infeature = subdir + "A_o_sum3_p"
    joinfeature = subdir + "_c_sum3_p"
    print("performing join and output to DB folder")
    arcpy.SpatialJoin_analysis(infeature, joinfeature, tempfc)
    mask = mask_dir + Location + " mask.shp"
    arcpy.MakeFeatureLayer_management(tempfc, 'lyr')
    if os.path.isfile(mask):
        arcpy.SelectLayerByLocation_management('lyr', 'intersect', mask, '', '', 'INVERT')
    arcpy.FeatureClassToFeatureClass_conversion('lyr', ArcGIS_dir, outfeature)
    '''



def dataclean(Location,result_suf):
    arcpy.env.workspace = ArcGIS_dir
    fcs = arcpy.ListFeatureClasses()
    for fc in fcs:
        if Location + result_suf + "_i_w_C" in fc and not os.path.isfile(DB_dir + fc + ".csv"):
            # Spatial join
            xls = DB_dir + fc + ".xlsx"
            csv = DB_dir + fc + ".csv"
            print("Output xlsx: " + xls)
            arcpy.TableToExcel_conversion(fc, xls)
            # clean data
            data_xls = pd.read_excel(xls, header=0)
            print("Dropping columns...")
            data_xls.drop(['OBJECTID','pointid'], axis=1, inplace=True)
            outfield = fc.split("_")[7] + "_" + fc.split("_")[8] + "_" + fc.split("_")[9]
            data_xls.columns = [outfield]
            data_xls.to_csv(csv, index=None)
            os.remove(xls)


def mergecsv():
    try:
        os.remove(DB_dir + "merge3.csv")
        arcpy.Delete_management(ArcGIS_dir + "merge3")
    except:
        print("Start merging all aggregated files...")
    csv_list2 = []
    df_initial2 = pd.DataFrame(csv_list2)
    csv_list3 = []
    df_initial3 = pd.DataFrame(csv_list3)
    out2 = DB_dir + "merge2.csv"
    out3 = DB_dir + "merge3.csv"
    csvs = os.listdir(DB_dir)
    for csv in csvs:
        if "csv" in csv:
            if csv.split(".")[0][-1] == "2":
                print("Merging..." + csv)
                df_csv = pd.read_csv(DB_dir + csv, header=0)
                df_initial2 = pd.concat([df_initial2, df_csv], axis=1)
            elif csv.split(".")[0][-1] == "3":
                print("Merging..." + csv)
                df_csv = pd.read_csv(DB_dir + csv, header=0)
                df_initial3 = pd.concat([df_initial3, df_csv], axis=1)
    #df_initial2.index += 1
    df_initial2.to_csv(out2, index=None)
    #df_initial3.index += 1
    df_initial3.to_csv(out3, index=None)

    print("Output resulting merged file to ArcGIS database...")
    arcpy.TableToDBASE_conversion(out2, ArcGIS_dir)
    arcpy.TableToDBASE_conversion(out3, ArcGIS_dir)


def spatialjoin(Location,result_suf):
    #Add join
    arcpy.CopyFeatures_management(ArcGIS_dir + Location + result_suf + "_i_w_C_c_sum3",
                                  ArcGIS_dir + Location + result_suf + "_i_w_mask3")
    inpolygon3 = ArcGIS_dir + Location + result_suf + "_i_w_mask3"
    join_table2 = ArcGIS_dir + "merge2"
    join_table3 = ArcGIS_dir + "merge3"
    print("Joining all attributes to mask base polypoint")
    arcpy.JoinField_management(inpolygon3, "OBJECTID", join_table3, "OBJECTID")
    target_features = ArcGIS_dir + Location + result_suf + "_i_w"
    out_feature_class3 = ArcGIS_dir + Location + result_suf + "_result3"
    print("Spatial joining mask points to target result polygon")
    DB_shp = DB_dir + Location + result_suf + "_result3.shp"
    if os.path.isfile(DB_shp):
        arcpy.Delete_management(DB_shp)
    arcpy.FeatureClassToShapefile_conversion(out_feature_class3, DB_dir)


def output(Location,result_suf):
    inpolygon = DB_dir + Location + result_suf + "_result3.shp"
    mask = mask_dir + Location + " mask.shp"
    arcpy.MakeFeatureLayer_management(inpolygon, 'lyr')
    if os.path.isfile(mask):
        arcpy.SelectLayerByLocation_management('lyr', 'intersect', mask,'','','INVERT')
    xls = DB_csv + Location + result_suf + "_result_c" + ".xlsx"
    csv = DB_csv + Location + result_suf + "_result_c" + ".csv"
    print("Output shp and xlsx: " + xls)
    shp_out = DB_shp + Location + result_suf + "_result_c.shp"
    if not os.path.isfile(shp_out):
        arcpy.FeatureClassToShapefile_conversion('lyr', DB_shp)
        arcpy.Rename_management(DB_shp + 'lyr.shp', shp_out)
    arcpy.TableToExcel_conversion('lyr', xls)
    #clean data
    data_xls = pd.read_excel(xls,header=0)
    print("Dropping columns...")
    data_xls.drop(['Join_Count', 'TARGET_FID', 'Join_Cou_1', 'TARGET_F_1', "grid_code", 'Id', 'OBJECTID_1', 'ID_1',
                   'pointid', "grid_code", "pointid_1", "grid_code_", "Shape_Leng", "Shape_Area"],
                  axis=1, inplace=True)
    print("Output csv...")
    data_xls.to_csv(csv, index=None)
    os.remove(xls)
    arcpy.Delete_management('lyr')




'''
#2007
loop("Agincourt","20080823_HV7_west_compr.tif")
loop("Ireagh","20080823_HV7_west_compr.tif")
loop("Justicia","20080823_HV7_west_compr.tif")

#2008
loop("Agincourt","20080825_HV_west_compr.tif")
loop("Ireagh","20080825_HV_west_compr.tif")
loop("Justicia","20080825_HV_west_compr.tif")

#2010
loop("Justicia","20100831_HV_west_compr.tif")
loop("WelAndover","20100831_HV_west_compr.tif")
loop("Welverdiendt","20100831_HV_west_compr.tif")

#2017
loop("Agincourt","20180702_HV7_west_compr.tif")
loop("Ireagh","20180702_HV7_west_compr.tif")
loop("Justicia","20180702_HV7_west_compr.tif")
loop("WelAndover","20180702_HV7_west_compr.tif")
loop("Welverdiendt","20180702_HV7_west_compr.tif")

#2018
loop("Agincourt","20180506_HV_west_compr.tif")
loop("Ireagh","20180506_HV_west_compr.tif")
loop("Justicia","20180506_HV_west_compr.tif")
loop("WelAndover","20180506_HV_west_compr.tif")
loop("Welverdiendt","20180506_HV_west_compr.tif")
'''

#2007
loop("Agincourt","20080823_HH7_west_compr.tif")
loop("Ireagh","20080823_HH7_west_compr.tif")
loop("Justicia","20080823_HH7_west_compr.tif")

#2008
loop("Agincourt","20080825_HH_west_compr.tif")
loop("Ireagh","20080825_HH_west_compr.tif")
loop("Justicia","20080825_HH_west_compr.tif")

#2010
loop("Justicia","20100831_HH_west_compr.tif")
loop("WelAndover","20100831_HH_west_compr.tif")
loop("Welverdiendt","20100831_HH_west_compr.tif")

#2017
loop("Agincourt","20180702_HH7_west_compr.tif")
loop("Ireagh","20180702_HH7_west_compr.tif")
loop("Justicia","20180702_HH7_west_compr.tif")
loop("WelAndover","20180702_HH7_west_compr.tif")
loop("Welverdiendt","20180702_HH7_west_compr.tif")

#2018
loop("Agincourt","20180506_HH_west_compr.tif")
loop("Ireagh","20180506_HH_west_compr.tif")
loop("Justicia","20180506_HH_west_compr.tif")
loop("WelAndover","20180506_HH_west_compr.tif")
loop("Welverdiendt","20180506_HH_west_compr.tif")