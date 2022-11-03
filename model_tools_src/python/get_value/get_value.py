import netCDF4 as nc
import numpy as np
import datetime as dt
import sys
import os

print("Current working dir.: %s"%(os.getcwd()))

txtModelFile = "tmp.txt"

lat          = np.float(sys.argv[1])
lon          = np.float(sys.argv[2])
ncFileName   = sys.argv[3]
varName      = sys.argv[4]

print("lat, long: %f, %f"%(lat,lon))
print("nc file: %s"%(ncFileName))
print("var: %s"%(varName))

# open netcdf file
ds = nc.Dataset(ncFileName)

# the the time object, and determine dataset begin/end time
nctime = ds.variables['time'] # A netCDF time variable object.
ts = nc.num2date(nctime[:][0],  units=nctime.units, calendar=nctime.calendar)
te = nc.num2date(nctime[:][-1], units=nctime.units, calendar=nctime.calendar)
startDate = "%.4i-%.2i-%.2i"%(ts.year, ts.month, ts.day)
endDate   = "%.4i-%.2i-%.2i"%(te.year, te.month, te.day)
dts = dt.datetime.strptime(str(startDate),'%Y-%m-%d')
dte = dt.datetime.strptime(str(  endDate),'%Y-%m-%d')

print("Date range: %s - %s"%(startDate,endDate))

# identify row and column indexes:
dx = abs(ds.variables['lon'][:] - lon)
dy = abs(ds.variables['lat'][:] - lat)
minX = min(dx); minY = min(dy)

#with np.printoptions(threshold=np.inf):
#    print(dx)

xStationIndex = int(np.where(dx == minX)[0][0])
yStationIndex = int(np.where(dy == minY)[0][0])

print("x, y index: %i, %i"%(xStationIndex,yStationIndex))

# cropping the data:
cropData = ds.variables[varName][:,yStationIndex,xStationIndex]

# cropping the time:
cropTime = nctime[:]

idx_start = nc.date2index(dts, nctime, calendar = nctime.calendar, select = 'exact')
idx_end   = nc.date2index(dte, nctime, calendar = nctime.calendar, select = 'exact')
cropData = cropData[int(idx_start):int(idx_end+1)]
cropTime = cropTime[int(idx_start):int(idx_end+1)]

# convert numeric dates to yyyy-mm-dd
cropTime = ["%.4i-%.2i-%.2i"%(x.year, x.month, x.day) for x in nc.num2date(cropTime, units=nctime.units, calendar=nctime.calendar)]

# convert to 2 column format
cropData = np.column_stack((cropTime,cropData))

# save cropData to a txt file:
txtModel = open(txtModelFile,"w")
np.savetxt(txtModelFile,cropData,delimiter=";",fmt='%s')
txtModel.close()

