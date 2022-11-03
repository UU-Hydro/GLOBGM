#!/usr/bin/env python
# -*- coding: utf-8 -*-
#

import os
import shutil
import sys
import datetime

import pcraster as pcr

from pcraster.framework import DynamicModel
from pcraster.framework import DynamicFramework

from currTimeStep import ModelTime

import ncConverter_for_discharge_30sec as netcdf_writer
import virtualOS as vos

import logging
logger = logging.getLogger(__name__)


class DeterministicRunner(DynamicModel):

    def __init__(self, modelTime, model_setup):
        DynamicModel.__init__(self)

        # ~ Please also check the previous script: https://github.com/edwinkost/estimate_discharge_from_local_runoff/blob/develop/python_estimate_flow/estimate_discharge_from_local_runoff.py

        # initiate model time
        self.modelTime = modelTime        


        # get the model setup
        self.model_setup = model_setup
        
        # set clone
        self.clone = self.model_setup["clone_file"]
        pcr.setclone(self.clone)
        
        # output and tmp folders
        self.output_folder = model_setup['output_dir']
        self.tmp_folder    = model_setup['tmp_dir']

        # read ldd
        self.ldd = vos.readPCRmapClone(v                = self.model_setup["ldd_file"], \
                                       cloneMapFileName = self.clone, \
                                       tmpDir           = self.tmp_folder, \
                                       absolutePath     = None, \
                                       isLddMap         = True, \
                                       cover            = None, \
                                       isNomMap         = False)
        self.ldd = pcr.lddrepair(pcr.lddrepair(pcr.ldd(self.ldd)))                               

        # read cell area (m2)
        self.cell_area = vos.readPCRmapClone(v                = self.model_setup["cell_area_file"], \
                                             cloneMapFileName = self.clone, \
                                             tmpDir           = self.tmp_folder, \
                                             absolutePath     = None, \
                                             isLddMap         = False, \
                                             cover            = None, \
                                             isNomMap         = False)

        
        # initiate a netcdf writer
        self.netcdf_report = netcdf_writer.PCR2netCDF(self.model_setup["clone_file"])
        self.netcdf_report.createNetCDF(self.model_setup["discharge_output_file"],\
                                        "discharge",\
                                        "m3/s")
        
    def initial(self): 
        
        
        # read cell area (m2)
        self.cell_area 

    def dynamic(self):

        # re-calculate current model time using current pcraster timestep value
        self.modelTime.update(self.currentTimeStep())

        # update water bodies at the beginning of model simulation and at every first day of the year
        if self.modelTime.timeStepPCR == 1 or (self.modelTime.doy == 1):

            logger.info(" \n\n Updating lakes and reservoirs %s \n\n", self.modelTime.currTime)

            # read lake and reservoir ids
            date_used = self.modelTime.fulldate
            lake_and_reservoir_ids = vos.netcdf2PCRobjClone(self.model_setup["lake_and_reservoir_file"],\
                                                            'waterBodyIds',\
                                                            date_used, \
                                                            useDoy = 'yearly',\
                                                            cloneMapFileName = self.clone)
            self.lake_and_reservoir_ids = pcr.nominal(lake_and_reservoir_ids)
            
        # calculating discharge only at the last day of every month
        if self.modelTime.isLastDayOfMonth():
            
            logger.info(" \n\n Calculating for time %s \n\n", self.modelTime.currTime)

            # read monthly runoff (m.month-1)
            monthly_total_local_runoff    = vos.netcdf2PCRobjClone(self.model_setup["monthly_runoff_file"], \
                                                                   "total_runoff", \
                                                                   str(self.modelTime.fulldate), \
                                                                   None, \
                                                                   self.clone)
            monthly_total_local_runoff    = pcr.cover(\
                                            monthly_total_local_runoff, 0.0)
            
            # daily runoff (m3.day-1)
            self.daily_total_local_runoff = monthly_total_local_runoff * self.cell_area / self.modelTime.day 
            
            
            # calculate discharge at river cells (m3.s-1)
            self.river_discharge = pcr.catchmenttotal(self.daily_total_local_runoff, self.ldd) / (24. * 3600.)
            self.river_discharge = pcr.max(0.0, pcr.cover(self.river_discharge, 0.0))
            
            # calculate discharge at lakes and reservoirs
            self.lake_and_reservoir_discharge = pcr.areamaximum(self.river_discharge, self.lake_and_reservoir_ids)
            self.lake_and_reservoir_discharge = pcr.ifthen(pcr.scalar(self.lake_and_reservoir_ids) > 0.0, self.lake_and_reservoir_discharge)
            
            # merge all discharge values
            self.discharge = pcr.cover(self.lake_and_reservoir_discharge, self.river_discharge)
            

            # reporting 
            # - time stamp for reporting
            timeStamp = datetime.datetime(self.modelTime.year,\
                                          self.modelTime.month,\
                                          self.modelTime.day,\
                                          0)
            logger.info("Reporting for time %s", self.modelTime.currTime)
            self.netcdf_report.data2NetCDF(self.model_setup["discharge_output_file"], \
                                           "discharge", \
                                           pcr.pcr2numpy(self.discharge, vos.MV), \
                                           timeStamp)


def main():

    model_setup = {}

    model_setup["clone_file"]              = "/scratch/depfg/sutan101/data/pcrglobwb_gmglob_input/develop/global_30sec/cloneMaps/global_30sec_clone.map"
    model_setup["ldd_file"]                = "/scratch/depfg/sutan101/data/pcrglobwb_gmglob_input/develop/global_30sec/routing/surface_water_bodies/version_2020-05-XX//lddsound_30sec_version_202005XX_correct_lat.nc"
    
    model_setup["cell_area_file"]          = "/scratch/depfg/sutan101/data/pcrglobwb_gmglob_input/develop/global_30sec/others/estimate_cell_dimension/30sec/cdo_grid_area_30sec_map_correct_lat.nc"

    model_setup["lake_and_reservoir_file"] = "/scratch/depfg/sutan101/data/pcrglobwb_gmglob_input/develop/global_30sec/routing/surface_water_bodies/version_2020-05-XX/lakes_and_reservoirs_30sec_global_2019_version_202005XX.nc"

    model_setup["monthly_runoff_file"] = "/scratch/depfg/sutan101/data/pcrglobwb_gmglob_input/develop/example_output/pcrglobwb/global_05min_gmd_paper_output/totalRunoff_monthTot_output_1958-01-31_to_2015-12-31.zip.nc"
    
    model_setup["start_date"] = "1958-01-31"
    model_setup["end_date"]   = "2015-12-31"

    model_setup["output_dir"] = "/scratch/depfg/sutan101/discharge_30sec_gmd_paper/monthly_1958-2015_splitted/" + model_setup["start_date"] + "_to_" + model_setup["end_date"] + "/"

    model_setup["discharge_output_file"] = model_setup["output_dir"] + "/" + "discharge_30sec_monthAvg_" + model_setup["start_date"] + "_to_" + model_setup["end_date"] + ".nc"


    print(model_setup["output_dir"])
    
    # make output and temporary folders
    if os.path.exists(model_setup["output_dir"]): shutil.rmtree(model_setup["output_dir"])
    os.makedirs(model_setup["output_dir"])
    # - make temporary folder
    model_setup["tmp_dir"] = model_setup["output_dir"] +  "/tmp/"
    os.makedirs(model_setup["tmp_dir"])


    # logger
    # - making a log directory
    log_file_directory = model_setup["output_dir"] + "/" + "log/"
    os.makedirs(log_file_directory)
    # - initialize logging
    vos.initialize_logging(log_file_directory)
    

    # timeStep info: year, month, day, doy, hour, etc
    currTimeStep = ModelTime() 
    currTimeStep.getStartEndTimeSteps(model_setup["start_date"], model_setup["end_date"])
    

    # Running the deterministic_runner
    logger.info('Starting the calculation.')
    deterministic_runner = DeterministicRunner(currTimeStep, model_setup)
    dynamic_framework = DynamicFramework(deterministic_runner,currTimeStep.nrOfTimeSteps)
    dynamic_framework.setQuiet(True)
    dynamic_framework.run()  
    
        
if __name__ == '__main__':
    sys.exit(main())
