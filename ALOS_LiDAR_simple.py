import arcpy
from arcpy.sa import *
import pandas as pd
import os

arcpy.env.overwriteOutput = True
DB_dir = r"E:\ChangMap\CHM\DB/"
DB_shp = r"E:\ChangMap\CHM\DB_shp/"
DB_csv = r"E:\ChangMap\CHM\DB_csv/"
CHM_dir = r"E:\ChangMap\CHM\CHM\CHM_result\allin/"
ArcGIS_dir = r"D:\temp\ArcGIS_project\Test\ALOS_LiDAR.gdb/"
ALOS_dir = r"E:\ChangMap\ALOS\Change_SouthAfrica-20201216T102802Z-001\Change_SouthAfrica/"
clip_polygon = r"D:\temp\ArcGIS_project\Test\Test.gdb\Clip"
mask_dir = r"E:\ChangMap\site masks/"
ras_resolution = 0.00013475


def loop(site,suf):
    Location = site
    ALOS_suf = suf
    result_suf = "_ALOS_" + ALOS_suf.split("_")[2] + "_" + \
                 ALOS_suf.split("_")[1] + "_" + \
                 ALOS_suf.split("_")[3] + "_" + \
                 ALOS_suf.split("_")[4][0]
    initilization()
    alos_clip(Location,ALOS_suf,result_suf)
    float_to_int(Location,result_suf)
    zonalstats(Location,ALOS_suf,result_suf)
    join(Location,result_suf)
    refield(Location,result_suf)
    PolyRas(Location,result_suf)
    focal_analysis(Location,result_suf)
    extractpoint(Location,result_suf)
    mergeresult(Location,result_suf)
    output(Location, result_suf)
    #dataclean(Location,result_suf)
    #mergecsv(Location,result_suf)
    #spatialjoin(Location,result_suf)



def initilization():
    print("Deleting previous files...")
    arcpy.env.workspace = ArcGIS_dir
    fc_list = arcpy.ListFeatureClasses()
    tables = arcpy.ListTables()
    ds_list = arcpy.ListDatasets()
    for fc in fc_list:
        arcpy.Delete_management(fc)
    for table in tables:
        arcpy.Delete_management(table)
    for ds in ds_list:
        arcpy.Delete_management(ds)
    DBs = os.listdir(DB_dir)
    for DB in DBs:
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
    inraster = CHM_dir + Location + "_Diff_" + ALOS_suf.split("_")[2] + "_" + ALOS_suf.split("_")[1] + ".tif"
    print("Input polygon: " + inpolygon)
    table = inpolygon + "_z_1"
    ZonalStatisticsAsTable(inpolygon, "id", inraster, table)


def join(Location,result_suf):
    arcpy.CopyFeatures_management(ArcGIS_dir + Location + result_suf + "_i",
                                  ArcGIS_dir + Location + result_suf + "_i_w")
    inpolygon = ArcGIS_dir + Location + result_suf + "_i"
    arcpy.JoinField_management(inpolygon, "id", inpolygon + "_z_1", "id", ["COUNT", "Sum"])


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
    data_xls.columns = ["OBJECTID", "ID", "CA_o", "length", "area", "C_c", "C_s"]
    data_xls.to_csv(csv, index=None)
    os.remove(xls)
    print("Joining csv to shp...")
    inpolygon = ArcGIS_dir + Location + result_suf + "_i_w"
    arcpy.TableToTable_conversion(csv, ArcGIS_dir, "result_csv")
    arcpy.JoinField_management(inpolygon, "OBJECTID", ArcGIS_dir + "result_csv", "OBJECTID")


#convert polygon back to raster based on different attributes
def PolyRas(Location,result_suf):
    fields = ["CA_o","C_c","C_s"]
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
    for inraster in codes:
        if Location + result_suf + "_i_w_C" in inraster:
            if "_o" in inraster:
                print("ALOS mean raster: " + inraster)
                outFocalStat1 = FocalStatistics(inraster, NbrRectangle('', '', 'CELL'), "MEAN", "")
                outFocalStat1.save(inraster + "_mean3")
                outFocalStat2 = FocalStatistics(inraster, NbrRectangle(5, 5, 'CELL'), "MEAN", "")
                outFocalStat2.save(inraster + "_mean5")
                outFocalStat3 = FocalStatistics(inraster, NbrRectangle(9, 9, 'CELL'), "MEAN", "")
                outFocalStat3.save(inraster + "_mean9")
            else:
                print("Focal output: " + inraster)
                outFocalStat4 = FocalStatistics(inraster, NbrRectangle('', '', 'CELL'), "SUM", "")
                outFocalStat4.save(inraster + "_sum3")
                outFocalStat5 = FocalStatistics(inraster, NbrRectangle(5, 5, 'CELL'), "SUM", "")
                outFocalStat5.save(inraster + "_sum5")
                outFocalStat6 = FocalStatistics(inraster, NbrRectangle(9, 9, 'CELL'), "SUM", "")
                outFocalStat6.save(inraster + "_sum9")

def extractpoint(Location,result_suf):
    field_list = ["ID","Shape","PID","Value"]
    inraster = ArcGIS_dir + Location + result_suf
    point = inraster + "_p"
    arcpy.RasterToPoint_conversion(inraster, point, "VALUE")
    rasters = arcpy.ListRasters("*")
    for ras in rasters:
        if "mean" in ras or "sum" in ras:
            print("Extracting values to point..." + ras)
            outfield = ras.split("_")[8] + "_" + ras.split("_")[9] + "_" + ras.split("_")[10]
            field_list.append(outfield)
            ExtractMultiValuesToPoints(point, ras + ' ' + "RASTERVALU", "NONE")
    print(field_list)


def mergeresult(Location,result_suf):
    target_features = ArcGIS_dir + Location + result_suf + "_i_w"
    inpolygon = ArcGIS_dir + Location + result_suf + "_p"
    out_feature_class = ArcGIS_dir + Location + result_suf + "_s"
    print("Spatial join..." + out_feature_class)
    arcpy.SpatialJoin_analysis(target_features, inpolygon, out_feature_class)
    print("Altering fields...")
    try:
        arcpy.AlterField_management(out_feature_class, "RASTERVALU", "CA_o_mean3")
        arcpy.AlterField_management(out_feature_class, "RASTERVALU_1", "CA_o_mean5")
        arcpy.AlterField_management(out_feature_class, "RASTERVALU_12", "CA_o_mean9")
        arcpy.AlterField_management(out_feature_class, "RASTERVALU_12_13", "C_c_sum3")
        arcpy.AlterField_management(out_feature_class, "RASTERVALU_12_13_14", "C_c_sum5")
        arcpy.AlterField_management(out_feature_class, "RASTERVALU_12_13_14_15", "C_c_sum9")
        arcpy.AlterField_management(out_feature_class, "RASTERVALU_12_13_14_15_16", "C_s_sum3")
        arcpy.AlterField_management(out_feature_class, "RASTERVALU_12_13_14_15_16_17", "C_s_sum5")
        arcpy.AlterField_management(out_feature_class, "RASTERVALU_12_13_14_15_16_17_18", "C_s_sum9")
    except:
        print("Failed to alter fields...")

def output(Location,result_suf):
    inpolygon = ArcGIS_dir + Location + result_suf + "_s"
    mask = mask_dir + Location + " mask.shp"
    arcpy.MakeFeatureLayer_management(inpolygon, 'lyr')
    if os.path.isfile(mask):
        arcpy.SelectLayerByLocation_management('lyr', 'intersect', mask,'','','INVERT')
    xls = DB_csv + Location + result_suf + "_result" + ".xlsx"
    csv = DB_csv + Location + result_suf + "_result" + ".csv"
    print("Output shp and xlsx: " + xls)
    arcpy.FeatureClassToShapefile_conversion('lyr', DB_shp)
    arcpy.Rename_management(DB_shp + 'lyr.shp', DB_shp + Location + result_suf + "_result.shp")
    arcpy.TableToExcel_conversion('lyr', xls)
    #clean data
    data_xls = pd.read_excel(xls,header=0)
    print("Dropping columns...")
    data_xls.drop(['Join_Count', 'TARGET_FID', 'Id', "grid_code", 'OBJECTID_1', 'ID_1',
                   'length', 'area', 'pointid', "grid_code", "Shape_Length", "Shape_Area"],
                  axis=1, inplace=True)
    print("Output csv...")
    data_xls.to_csv(csv, index=None)
    os.remove(xls)
    arcpy.Delete_management('lyr')


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
            outfield = fc.split("_")[8] + "_" + fc.split("_")[9] + "_" + fc.split("_")[10]
            data_xls.columns = [outfield]
            data_xls.to_csv(csv, index=None)
            os.remove(xls)
def mergecsv(Location,result_suf):
    try:
        os.remove(DB_dir + "merge.csv")
        arcpy.Delete_management(ArcGIS_dir + "merge")
    except:
        print("Start merging all aggregated files...")
    os.remove(DB_dir + Location + result_suf + "_i.csv")
    csv_list3 = []
    df_initial3 = pd.DataFrame(csv_list3)
    out3 = DB_dir + "merge.csv"
    csvs = os.listdir(DB_dir)
    for csv in csvs:
        if "csv" in csv:
            print("Merging..." + csv)
            df_csv = pd.read_csv(DB_dir + csv, header=0)
            df_initial3 = pd.concat([df_initial3, df_csv], axis=1)
    #df_initial3.index += 1
    df_initial3.to_csv(out3, index=None)
    print("Output resulting merged file to ArcGIS database...")
    arcpy.TableToDBASE_conversion(out3, ArcGIS_dir)
def spatialjoin(Location,result_suf):
    #Add join
    arcpy.CopyFeatures_management(ArcGIS_dir + Location + result_suf + "_i_w_C_c_sum3",
                                  ArcGIS_dir + Location + result_suf + "_i_w_mask3")
    inpolygon3 = ArcGIS_dir + Location + result_suf + "_i_w_mask3"
    join_table3 = ArcGIS_dir + "merge"
    print("Joining all attributes to mask base polypoint")
    arcpy.JoinField_management(inpolygon3, "OBJECTID", join_table3, "OBJECTID")
    target_features = ArcGIS_dir + Location + result_suf + "_i_w"
    out_feature_class3 = ArcGIS_dir + Location + result_suf + "_result3"
    print("Spatial joining mask points to target result polygon")
    arcpy.SpatialJoin_analysis(target_features, inpolygon3, out_feature_class3)
    DB_shp = DB_dir + Location + result_suf + "_result3.shp"
    if os.path.isfile(DB_shp):
        arcpy.Delete_management(DB_shp)
    arcpy.FeatureClassToShapefile_conversion(out_feature_class3, DB_dir)

'''



'''
#2007-2017
loop("Agincourt","change_2008_2018_HV7_west_compr.tif")
#loop("Ireagh","change_2008_2018_HV7_west_compr.tif")
#loop("Justicia","change_2008_2018_HV7_west_compr.tif")

#2008-2018
loop("Agincourt","change_2008_2018_HV_west_compr.tif")
loop("Ireagh","change_2008_2018_HV_west_compr.tif")
loop("Justicia","change_2008_2018_HV_west_compr.tif")

#2008-2010
loop("Justicia","change_2008_2010_HV_west_compr.tif")

#2010-2018
loop("Justicia","change_2010_2018_HV_west_compr.tif")
loop("WelAndover","change_2010_2018_HV_west_compr.tif")
loop("Welverdiendt","change_2010_2018_HV_west_compr.tif")
'''

#2007-2017
loop("Agincourt","change_2008_2018_HH7_west_compr.tif")
loop("Ireagh","change_2008_2018_HH7_west_compr.tif")
loop("Justicia","change_2008_2018_HH7_west_compr.tif")

#2008-2018
loop("Agincourt","change_2008_2018_HH_west_compr.tif")
loop("Ireagh","change_2008_2018_HH_west_compr.tif")
loop("Justicia","change_2008_2018_HH_west_compr.tif")

#2008-2010
loop("Justicia","change_2008_2010_HH_west_compr.tif")

#2010-2018
loop("Justicia","change_2010_2018_HH_west_compr.tif")
loop("WelAndover","change_2010_2018_HH_west_compr.tif")
loop("Welverdiendt","change_2010_2018_HH_west_compr.tif")