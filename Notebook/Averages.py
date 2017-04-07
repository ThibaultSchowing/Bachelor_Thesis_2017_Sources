# -*- coding: utf-8 -*-

import numpy as np
import math
from PIL import Image
from scipy.misc import toimage
from matplotlib.pyplot import imshow




class ClimatAverages:
    
    def __init__(self, year="2011", month="1", measure="tmin"
                 ,path = "/mnt/hgfs/sharedKali/TB_Data_Meteo/outputs_yearly_v2_2011_2016/average/",):
        
        self.path = str(path)
        self.measure = str(measure)
        self.year = str(year)
        self.month = str(month)
        
        
        records_matrix = open(self.path  + measure + '_' + year + '_' + month + '.asc')
        # Reading the data 
        tmpMatriceData =  [l.strip().split(' ') for l in records_matrix.readlines()[6::]]
        records_matrix.seek(0)
        tmpMatriceInfo =  [l.strip().split(' ') for l in records_matrix.readlines()[:5]]
        self.matriceData = np.array(tmpMatriceData)
        self.matriceInfo = np.array(tmpMatriceInfo)
        records_matrix.close()
        
        self.NCOLS = self.matriceInfo[0,1]
        self.NROW = self.matriceInfo[1,1]
        self.XLLCORNER = self.matriceInfo[2,1]
        self.YLLCORNER = self.matriceInfo[3,1]
        self.CELLSIZE = self.matriceInfo[4,1]
        
        
        self.lons = []
        self.lats = []
        
        basex = float(self.XLLCORNER)
        basey = float(self.YLLCORNER)
        
        # Coordonnées sur x et y (décimal)
        for i in range(0, 720):
            self.lons.append(basex)
            self.lats.append(basey)
            basex += float(self.CELLSIZE)
            basey += float(self.CELLSIZE)
        
        
        self.lons = np.array(self.lons)
        
        self.lats = np.array(self.lats)
        
        # Minimums and maximums
        self.minLon = self.lons[0]
        self.maxLon = self.lons[-1]
        self.minLat = self.lats[0]
        self.maxLat = self.lats[-1]
        
    # The table is a 720 x 720 table
    # TODO: check the coordinate are in the range (in risaralda) and then convert it (lon and lat) in the range 0-720
    
    
    
    def isOnMap(self, lon, lat):
        if(lon < self.minLon or lon > self.maxLon):
            return False
        elif(lat < self.minLat or lat > self.maxLat):
            return False
        else:
            return True
    
    
    # Points: idx of the table you want to be red
    def displayMap(self, points = [None]):
        
        
        NewMax = 255
        NewMin = 0
        
        array2 = np.array(self.matriceData) 
        array2range = []
        
        OldMax = float(-8000)
        OldMin = float(8000)
        
        # Find min and max to then convert the values in another range
        
        for row in self.matriceData:
            for val in row:
                if (float(val) != -9999) and (float(val) < OldMin):
                    OldMin = float(val)
                if (float(val) != -9999) and (float(val) > OldMax):
                    OldMax = float(val)
        
        NewRange = float(NewMax - NewMin)
        NewRange = float(NewRange)
        
        NewMax = float(NewMax)
        NewMin = float(NewMin)
        
        OldRange = float(OldMax) - float(OldMin)
        OldRange = float(OldRange)
        
        # Convert values range (temperature, rain, etc in 0-255 range)
        for row in array2:
            tempLine = []
            for OldValue in row:
                NewValue = 0
                if(float(OldValue) == -9999):
                    color = [20,70,235]
                else:
                    NewValue = float(((float(OldValue) - OldMin) * NewRange) / OldRange) + NewMin
                    color = [int(NewValue)/2, int(NewValue), 150]
                
                tempLine.append(color)
            array2range.append(tempLine)
        array2range = np.array(array2range)
        data = array2range
        
        if(len(points) > 0):
            for point in points:
                data[point[0],point[1]] = [255, 0, 0]
                if(point[0] < 719 and point[0] > 0 and point[1] > 0 and point[1] < 719):
                    data[point[0],point[1]+1] = data[point[0],point[1]-1] = data[point[0]-1,point[1]] = data[point[0]+1,point[1]] = [255, 0, 0]
                    
                
        
        img = toimage(data)
        img.show()
        imshow(img, aspect='auto')
    
    
    # Returns the two table indices corresponding to the nearest coordinates given
    #
    # Params:
    #         - Coordinates [Longitude, Latitude]
    #
    def getNearestCoordIdx(self, coord):
        
        lon = coord[0]
        lat = coord[1]
        idxLon = (np.abs(self.lons-lon)).argmin()
        idxLat = (np.abs(self.lats-lat)).argmin()
        idxLat = np.abs(idxLat - 719)
        
        return [idxLat, idxLon]
    
    
    # Allows to get the nearest value, according to the given coordinates
    #
    # Params: 
    #         - Coordinates [Longitude, Latitude]
    #
    # 
    def getNearestCoordValue(self, coord):
        indices = self.getNearestCoordIdx(coord)
        return self.matriceData[indices[0], indices[1]]

class ClimaticDataWraper:
    
    def __init__(self, years):
        
        months = ["1","2","3","4","5","6","7","8","9","10","11","12"]
        
        # All datas
        self.globalTmaxAverages = []
        self.globalTminAverages = []
        self.globalTmeanAverages = []
        self.globalDtrAverages = []
        self.globalPrecAverages = []


        for year in years: 
            print("Loading data for {}".format(year))
            for month in months:
                self.globalTmaxAverages.append(ClimatAverages(year, month, measure="tmax"))
                self.globalTminAverages.append(ClimatAverages(year, month, measure="tmin"))
                self.globalTmeanAverages.append(ClimatAverages(year, month, measure="tmean"))
                self.globalDtrAverages.append(ClimatAverages(year, month, measure="dtr"))
                self.globalPrecAverages.append(ClimatAverages(year, month, measure="prec"))
            print("Done !")
    
    # Coord = [lon, lat]
    def getTminAverages(self, coord):
        dataVector = []
        for tmin in self.globalTminAverages:
            dataVector.append(tmin.getNearestCoordValue([coord[0], coord[1]]))
        return np.array(dataVector)
    
    # Coord = [lon, lat]
    def getTmaxAverages(self, coord):
        dataVector = []
        for tmax in self.globalTmaxAverages:
            dataVector.append(tmax.getNearestCoordValue([coord[0], coord[1]]))
        return np.array(dataVector)
    
    # Coord = [lon, lat]
    def getTmeanAverages(self, coord):
        dataVector = []
        for tmean in self.globalTmeanAverages:
            dataVector.append(tmean.getNearestCoordValue([coord[0], coord[1]]))
        return np.array(dataVector)
    
    # Coord = [lon, lat]
    def getDtrAverages(self, coord):
        dataVector = []
        for dtr in self.globalDtrAverages:
            dataVector.append(dtr.getNearestCoordValue([coord[0], coord[1]]))
        return np.array(dataVector)
    
    # Coord = [lon, lat]
    def getPrecAverages(self, coord):
        dataVector = []
        for prec in self.globalPrecAverages:
            dataVector.append(prec.getNearestCoordValue([coord[0], coord[1]]))
        return np.array(dataVector)



