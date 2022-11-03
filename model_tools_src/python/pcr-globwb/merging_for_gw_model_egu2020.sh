
#~ dischargeInputNC               = /scratch-shared/edwinhs/pcrglobwb_for_land-subsidence-model_runs_version_20200328/ssp2_rcp4p5_hadgem_for_egu_2020/begin_from_2011_test_java/netcdf/discharge_monthAvg_output.nc
#~ groundwaterRechargeInputNC     = /scratch-shared/edwinhs/pcrglobwb_for_land-subsidence-model_runs_version_20200328/ssp2_rcp4p5_hadgem_for_egu_2020/begin_from_2011_test_java/netcdf/gwRecharge_monthTot_output.nc
#~ groundwaterAbstractionInputNC  = /scratch-shared/edwinhs/pcrglobwb_for_land-subsidence-model_runs_version_20200328/ssp2_rcp4p5_hadgem_for_egu_2020/begin_from_2011_test_java/netcdf/totalGroundwaterAbstraction_monthTot_output.nc
#~ channelStorageInputNC          = /scratch-shared/edwinhs/pcrglobwb_for_land-subsidence-model_runs_version_20200328/ssp2_rcp4p5_hadgem_for_egu_2020/begin_from_2011_test_java/netcdf/channelStorage_monthAvg_output.nc

            #~ cmd = 'python '+ self.configuration.path_of_this_module + "/merge_netcdf.py " + str(self.configuration.main_output_directory) + " " +\
                                                                                            #~ str(self.configuration.main_output_directory) + "/global/netcdf/ "+\
                                                                                            #~ str(nc_report_type)  + " " +\
                                                                                            #~ str(start_date) + " " +\
                                                                                            #~ str(end_date)   + " " +\
                                                                                            #~ str(netcdf_files_that_will_be_merged) + " " +\
                                                                                            #~ str(self.netcdf_format)  + " "  +\
                                                                                            #~ str(self.zlib_option  )  + " "  +\
                                                                                            #~ str(max_number_of_cores) + " "  +\
                                                                                            #~ str(self.configuration.globalOptions['cloneAreas'])  + " "

#~ python merge_netcdf.py /scratch-shared/edwinhs/pcrglobwb_for_land-subsidence-model_runs_version_20200328/ssp2_rcp4p5_hadgem_for_egu_2020/begin_from_2011/ /scratch-shared/edwinsu/pcrglobwb_for_land-subsidence-model_runs_version_20200328/ssp2_rcp4p5_hadgem_for_egu_2020/begin_from_2011/global/netcdf/ outMonthTotNC 2011-01-31 2050-12-31 precipitation,gwRecharge,surfaceWaterInf,totalRunoff,runoff,totalGroundwaterAbstraction,fossilGroundwaterAbstraction,totalEvaporation NETCDF4 False 16 Global &
#~ 
#~ python merge_netcdf.py /scratch-shared/edwinhs/pcrglobwb_for_land-subsidence-model_runs_version_20200328/ssp2_rcp4p5_hadgem_for_egu_2020/begin_from_2011/ /scratch-shared/edwinsu/pcrglobwb_for_land-subsidence-model_runs_version_20200328/ssp2_rcp4p5_hadgem_for_egu_2020/begin_from_2011/global/netcdf/ outMonthAvgNC 2011-01-31 2050-12-31 discharge,channelStorage,temperature NETCDF4 False 16 Global &
#~ 
#~ wait

python merge_netcdf.py /scratch-shared/edwinhs/pcrglobwb_for_land-subsidence-model_runs_version_20200328/ssp2_rcp4p5_hadgem_for_egu_2020/begin_from_2011/ /scratch-shared/edwinsu/pcrglobwb_for_land-subsidence-model_runs_version_20200328/ssp2_rcp4p5_hadgem_for_egu_2020/begin_from_2011/global/netcdf/ outMonthTotNC 2011-01-31 2050-12-31 gwRecharge,totalGroundwaterAbstraction,fossilGroundwaterAbstraction NETCDF4 False 8 Global &

python merge_netcdf.py /scratch-shared/edwinhs/pcrglobwb_for_land-subsidence-model_runs_version_20200328/ssp2_rcp4p5_hadgem_for_egu_2020/begin_from_2011/ /scratch-shared/edwinsu/pcrglobwb_for_land-subsidence-model_runs_version_20200328/ssp2_rcp4p5_hadgem_for_egu_2020/begin_from_2011/global/netcdf/ outMonthAvgNC 2011-01-31 2050-12-31 discharge,channelStorage NETCDF4 False 8 Global &

wait
