# GLOBGM

The GLOBGM is the PCR-*GLOB*WB MODFLOW global-scale MODFLOW *G*roundwater *M*odel at 30 arcsec (~ 1km at the equator) spatial resolution. The current version 1.0 uses a distributed memory parallel prototype of MODFLOW 6, developed together with the USGS (https://github.com/verkaik/modflow6-parallel.git, https://doi.org/10.5281/zenodo.5778658).

Contact: Jarno Verkaik (globgm.info@gmail.com)

GLOBGM main reference/paper: Verkaik, J., Sutanudjaja, E. H., Oude Essink, G.H.P., Lin, H.X., and Bierkens, M. F. P.: GLOBGM v1.0: a parallel implementation of a 30 arcsec PCRGLOBWB-MODFLOW global-scale groundwater model, Submitted to Geosci. Model Dev., September 2022.

PCR-GLOBWB main reference/paper: Sutanudjaja, E. H., van Beek, R., Wanders, N., Wada, Y., Bosmans, J. H. C., Drost, N., van der Ent, R. J., de Graaf, I. E. M., Hoch, J. M., de Jong, K., Karssenberg, D., López López, P., Peßenteiner, S., Schmitz, O., Straatsma, M. W., Vannametee, E., Wisser, D., and Bierkens, M. F. P.: PCR-GLOBWB 2: a 5 arcmin global hydrological and water resources model, Geosci. Model Dev., 11, 2429-2453, https://doi.org/10.5194/gmd-11-2429-2018, 2018.


## Input and output files (including Yoda access)

The main GLOBGM input and output files are available through the Yoda research data management service (https://www.uu.nl/en/research/yoda):

- URL: https://geo.data.uu.nl/research-globgm/
- Username: globgm.user@gmail.com
- Password: globgm.user.2022

Note that among the available raster files provided by Yoda, there are files with the IDF (iMOD Data File) extension. These binary files can be viewed and processed using iMOD (https://oss.deltares.nl/web/imod/download-imod5).

## How to install

For the 'Write Tiled Parameter Data' pre-processing, PCR-Raster Python should be installed, see steps 1-3 in 'How to install' section at GitHub repository https://github.com/UU-Hydro/PCR-GLOBWB_model and https://pcraster.geo.uu.nl/. For this pre-processing, the modified PCR-GLOBWB Python model files are located in ./model_tools_src/python/pcr-globwb.

The GLOBGM pre-processing stept 'Prepare Model Partitioning' and 'Partition and Write Model Input' require the Fortran compilation of the tools located in ./model_tools_src/fortran, repectively. Note that for some of these tools (i.e. *catchcreatemetis*, *mf6ggm*) the code should be linked with the METIS library (http://glaros.dtc.umn.edu/gkhome/metis/metis/download). For this, METIS should be compiled at 64-bit precision (see ./model_tools_src/c/metis/metis.h). 

Furthermore, for running the model ('Run Model') the MODFLOW 6 computational kernel (https://github.com/verkaik/modflow6-parallel.git, https://doi.org/10.5281/zenodo.5778658) should be compiled with a Fortran compiler and linked with the Message Passing Interface library (see e.g. the template ./model_tools_src/fortran/modflow6/makefile).

## How to run

Template job scripts are given in ./model_job_scripts/, corresponding to steps of the workflow as mentioned in Section 2.3 of the GLOBGM paper (Verkaik et al, 2022).

## Disclaimer

The GLOBGM is released under the GPL license v3.0 (see LICENSE). This program comes with ABSOLUTELY NO WARRANTY.
