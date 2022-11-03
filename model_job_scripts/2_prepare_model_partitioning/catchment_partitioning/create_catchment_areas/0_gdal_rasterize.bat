setlocal EnableDelayedExpansion
set gdal=c:\OSGeo4W64\bin\gdal_rasterize.exe
set d=.\hydrobasins_download\lev{pft_lev}\

set shp=hybas_lake_af_lev{pft_lev}_v1c
%gdal% -l %shp% -a SORT -ts 43200.0 21600.0 -a_nodata 0.0 -te -180.0 -90.0 180.0 90.0 -ot Int32 -of Ehdr %d%%shp%.shp %shp%.flt

set shp=hybas_lake_ar_lev{pft_lev}_v1c
%gdal% -l %shp% -a SORT -ts 43200.0 21600.0 -a_nodata 0.0 -te -180.0 -90.0 180.0 90.0 -ot Int32 -of Ehdr %d%%shp%.shp %shp%.flt

set shp=hybas_lake_as_lev{pft_lev}_v1c
%gdal% -l %shp% -a SORT -ts 43200.0 21600.0 -a_nodata 0.0 -te -180.0 -90.0 180.0 90.0 -ot Int32 -of Ehdr %d%%shp%.shp %shp%.flt

set shp=hybas_lake_au_lev{pft_lev}_v1c
%gdal% -l %shp% -a SORT -ts 43200.0 21600.0 -a_nodata 0.0 -te -180.0 -90.0 180.0 90.0 -ot Int32 -of Ehdr %d%%shp%.shp %shp%.flt

set shp=hybas_lake_eu_lev{pft_lev}_v1c
%gdal% -l %shp% -a SORT -ts 43200.0 21600.0 -a_nodata 0.0 -te -180.0 -90.0 180.0 90.0 -ot Int32 -of Ehdr %d%%shp%.shp %shp%.flt

set shp=hybas_lake_na_lev{pft_lev}_v1c
%gdal% -l %shp% -a SORT -ts 43200.0 21600.0 -a_nodata 0.0 -te -180.0 -90.0 180.0 90.0 -ot Int32 -of Ehdr %d%%shp%.shp %shp%.flt

set shp=hybas_lake_sa_lev{pft_lev}_v1c
%gdal% -l %shp% -a SORT -ts 43200.0 21600.0 -a_nodata 0.0 -te -180.0 -90.0 180.0 90.0 -ot Int32 -of Ehdr %d%%shp%.shp %shp%.flt

set shp=hybas_lake_si_lev{pft_lev}_v1c
%gdal% -l %shp% -a SORT -ts 43200.0 21600.0 -a_nodata 0.0 -te -180.0 -90.0 180.0 90.0 -ot Int32 -of Ehdr %d%%shp%.shp %shp%.flt

pause
