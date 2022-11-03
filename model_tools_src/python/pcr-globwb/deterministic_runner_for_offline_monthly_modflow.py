#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# PCR-GLOBWB (PCRaster Global Water Balance) Global Hydrological Model
#
# Copyright (C) 2016, Edwin H. Sutanudjaja, Rens van Beek, Niko Wanders, Yoshihide Wada,
# Joyce H. C. Bosmans, Niels Drost, Ruud J. van der Ent, Inge E. M. de Graaf, Jannis M. Hoch,
# Kor de Jong, Derek Karssenberg, Patricia López López, Stefanie Peßenteiner, Oliver Schmitz,
# Menno W. Straatsma, Ekkamol Vannametee, Dominik Wisser, and Marc F. P. Bierkens
# Faculty of Geosciences, Utrecht University, Utrecht, The Netherlands
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import os
import sys
import datetime
import glob

import pcraster as pcr
from pcraster.framework import DynamicModel
from pcraster.framework import DynamicFramework

from configuration_for_modflow import Configuration
from currTimeStep import ModelTime
from reporting_for_modflow import Reporting

import modflow
#~ try:
    #~ import modflow
#~ except:
    #~ pass

import virtualOS as vos

import logging
logger = logging.getLogger(__name__)

import disclaimer

class DeterministicRunner(DynamicModel):

    def __init__(self, configuration, modelTime, system_arguments):
        DynamicModel.__init__(self)

        # model time object
        self.modelTime = modelTime

        # make the configuration available for the other method/function
        self.configuration = configuration

        # model and reporting objects
        self.model     = modflow.ModflowCoupling(configuration, modelTime)
        self.reporting = Reporting(configuration, self.model, modelTime)

        # set the clone map
        pcr.setclone(configuration.cloneMap)

        # TODO: pre-factors based on the system arguments


    def initial(self):

        # get or prepare the initial condition for groundwater head
        self.model.get_initial_heads()

    def dynamic(self):

        # re-calculate current model time using current pcraster timestep value
        self.modelTime.update(self.currentTimeStep())

        # update/calculate model and daily merging, and report ONLY at the last day of the month
        if self.modelTime.isLastDayOfMonth():

            # update MODFLOW model (It will pick up current model time from the modelTime object)
            self.model.update()
            # reporting is only done at the end of the month
            # self.reporting.report() #JV

def main():

    # print disclaimer
    disclaimer.print_disclaimer()

    # get the full path of configuration/ini file given in the system argument
    iniFileName   = os.path.abspath(sys.argv[1])

    # debug option
    debug_mode = False
    if len(sys.argv) > 2:
        if sys.argv[2] == "debug": debug_mode = True

    # options to perform steady state calculation
    steady_state_only = False
    if len(sys.argv) > 3:
        if sys.argv[3] == "steady-state-only": steady_state_only = True

    if len(sys.argv) > 4: #JV
        tile = sys.argv[4]
        print('Writing ini-file for tile %s'%tile)
        iniFileName_new = os.path.join(os.path.dirname(iniFileName),'%s.ini'%tile)
        f = open(iniFileName,'r'); s = f.read(); f.close()
        iniFileName = iniFileName_new
        s = s.replace('$tile$',tile)
        f = open(iniFileName,'w'); f.write(s); f.close()

    # object to handle configuration/ini file
    configuration = Configuration(iniFileName = iniFileName, \
                                  debug_mode = debug_mode, \
                                  steady_state_only = steady_state_only)

    # if steady_state_only startTime = endTime
    if steady_state_only:
       configuration.globalOptions['endTime'] = configuration.globalOptions['startTime']

    # timeStep info: year, month, day, doy, hour, etc
    currTimeStep = ModelTime()

    # Running the deterministic_runner
    currTimeStep.getStartEndTimeSteps(configuration.globalOptions['startTime'],
                                      configuration.globalOptions['endTime'])
    logger.info('Model run starts.')
    deterministic_runner = DeterministicRunner(configuration, currTimeStep, sys.argv)

    dynamic_framework = DynamicFramework(deterministic_runner, currTimeStep.nrOfTimeSteps)
    dynamic_framework.setQuiet(True)
    dynamic_framework.run()

if __name__ == '__main__':
    # print disclaimer
    disclaimer.print_disclaimer(with_logger = True)
    sys.exit(main())


