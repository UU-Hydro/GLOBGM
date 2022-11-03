#! /usr/bin/python

import os
import sys

input_folder = "/scratch-shared/edwinhs/pcrglobwb_for_land-subsidence-model_runs_version_20200328/ssp2_rcp4p5_hadgem_for_egu_2020/begin_from_2011/"

start_year = 2011 
final_year = 2050

years = range(start_year, final_year + 1)

outp_folder = "/scratch-shared/edwinsu/pcrglobwb_for_land-subsidence-model_runs_version_20200328/ssp2_rcp4p5_hadgem_for_egu_2020/begin_from_2011/global/netcdf/"

#~ cmd = "rm -rf " + outp_folder
#~ print(cmd)
#~ os.system(cmd)

cmd = "mkdir -p " + outp_folder
print(cmd)

os.system(cmd)

cmd = "cp merge_netcdf.py " + outp_folder
print(cmd)
os.system(cmd)

os.chdir(outp_folder)

for year in years:
    
    cmd = ""
    
    # monthly total
    cmd += "python merge_netcdf.py " + \
          input_folder + " " + \
          outp_folder + " " + \
          "outMonthTotNC " + \
          str(year)+"-01-31" + " " + str(year)+"-12-31" + " " + \
          "gwRecharge,totalGroundwaterAbstraction,fossilGroundwaterAbstraction NETCDF4 True 8 Global & "

    # monthly average
    cmd += "python merge_netcdf.py " + \
          input_folder + " " + \
          outp_folder + " " + \
          "outMonthAvgNC " + \
          str(year)+"-01-31" + " " + str(year)+"-12-31" + " " + \
          "discharge,channelStorage NETCDF4 True 8 Global & "

    cmd += "wait"

    print(cmd)
    os.system(cmd)
