set exe={bin_dir}\filter_hydbas.exe
set flm={yoda_input}\landmask
set fhb=hybas_lake_lev{pft_lev}_v1c
set fo=hybas_lake_lev{pft_lev}_v1c_filt

%exe% %flm% %fhb% %fo% > log.txt

pause
