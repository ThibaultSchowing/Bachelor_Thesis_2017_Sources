# -*- coding: utf-8 -*-
import numpy as np


class Soils:
    
    def __init__(self, path = "./Data/soils/suelos_risaralda_v2.csv"):
        
        
        self.path = path
        
        # Reading file
        records_matrix = open(self.path)
        # Reading the data 
        tmpMatriceData =  [l.strip().split(';') for l in records_matrix.readlines()[1::]]
        records_matrix.seek(0)
        tmpMatriceInfo =  [l.strip().split(';') for l in records_matrix.readlines()[:1]]
        self.matriceData = np.array(tmpMatriceData)
        self.matriceInfo = np.array(tmpMatriceInfo)
    
    # id   code      profile    layer   layer code
    #     _________ _________ _________ _________
    #  1 |  abc    |  abcP1  |        1|     abc1|
    #  2 |  abc    |  abcP1  |        2|     abc2|
    #  3 |  abc    |  abcP1  |        3|     abc3|
    #    |         |_________|_________|_________|
    #  4 |  abc    |  abcP2  |        1|     abc1|
    #  5 |  abc    |  abcP2  |        2|     abc2|
    #  6 |  abc    |  abcP2  |        3|     abc3|
    #  
    #  To get an unique identifier we need profile + layer code
    #  As we do to get pH or Organic 
    #
    
    
    def isNA(self, value):
        return (value == "NA" or value == "N.A." or value == "na" or value == "n.a.")
    
    # returns all ID's
    def getIds(self):
        array = [int(stridx)-1 for stridx in self.matriceData[:,0]]
        return array
    
    # returns all codes
    def getCodes(self):
        return self.matriceData[:,1]
    
    def getProfilCodes(self):
        return self.matriceData[:,2]
    
    # simply count the number of occurence of "profile" ex test value: 66MALP02
    def getNumberOfLayers(self,profile):
        count = 0
        for i in range(0,len(self.matriceData)):
            if self.matriceData[i,2] == profile:
                count += 1
        return count
    
    # return table with layers codes
    def getLayers(self, profile):
        layers = []
        
        for i in range(0, len(self.matriceData)):
            if self.matriceData[i,2] == profile:
                layers.append(self.matriceData[i,4])
        
        return layers
        
    
    # return id (1-70) => (0-69)
    # params: 
    def getLayerID(self, profile, layerCode):
        profileData = []
        
        for i in range(0, len(self.matriceData)):
            if self.matriceData[i,2] == profile:
                profileData.append(self.matriceData[i,:])
        profileData = np.array(profileData)
        
        for j in range(0, len(profileData)):
            if profileData[j, 4] == layerCode:
                return (int(profileData[j,0])) -1
        return None
    
    
    
    # returns the Horizon Symbol
    def getHorizonSymbol(self, layerID):
        
        return self.matriceData[layerID, 5]
    
    
    # returns the horizon depth informations (Start depth, Finish depth, depth)
    def getHorizonDepthInfos(self, layerID):
        
        return (self.matriceData[layerID, 6],self.matriceData[layerID, 7],self.matriceData[layerID, 8])

    
    # returns the textures table (5 binairy variable table)
    # Franco|Acrilloso|Limoso|Arenoso|Cascajoso
    def getTexture(self, layerID):
        
        textureValues = ["franco","arcilloso","limoso","arenoso","cascajoso"]
        
        
        textureTable = np.zeros(len(textureValues))
        
        data = self.matriceData[layerID, 9].lower()

        #print data
        #print layerID
        if self.isNA(data):
            #print "No value - NA"
            return None
        
        
        for i in range(0, len(textureValues)):
            if textureValues[i] in data:
                textureTable[i] = 1
            if i == 1:
                # if contains acrillo 
                if "arcillo" in data:
                    textureTable[i] = 1
        
        
        return textureTable
    
    # returns the pH of the layer passed in parameter (float)
    def getPH(self, layerID):
        value = self.matriceData[layerID, 12]
        if(not self.isNA(value)):
            return float(value)
        else: 
            return None
    
    # returns the % of organic mater (float, percentage -> 0.8 means 0.8%)
    def getOrganic(self, layerID):
        value = self.matriceData[layerID, 13].replace('%','')
        if(not self.isNA(value)):
            return float(value)
        else:
            return None
    
    
    def getGravimetricHumidity1(self, layerID):
        value = self.matriceData[layerID, 14]
        if(not self.isNA(value)):
            return float(value)
        else:
            return None
    
    
    def getGravimetricHumidity2(self, layerID):
        value = self.matriceData[layerID, 15]
        if (not self.isNA(value)):
            return float(value)
        else: 
            return None
    
    
    def getUsableHumidity(self, layerID):
        
        value = self.matriceData[layerID, 16]
        if (not self.isNA(value)):
            return float(value)
        else: 
            return None
    
    
    def getRealDensity(self, layerID):
        
        value = self.matriceData[layerID, 17]
        if (not self.isNA(value)):
            return float(value)
        else: 
            return None
    
    
    def getApparentDensity(self, layerID):
        
        value = self.matriceData[layerID, 18]
        if (not self.isNA(value)):
            return float(value)
        else: 
            return None
    
    
    def getTotalPorosity(self, layerID):
        
        value = self.matriceData[layerID, 19]
        if (not self.isNA(value)):
            return float(value)
        else: 
            return None
    
    
    def layerSummary(self, layerID):
        summary = []
        summary.append(self.getPH(layerID))
        summary.append(self.getOrganic(layerID))
        summary.append(self.getHorizonSymbol(layerID))
        summary.append(self.getHorizonDepthInfos(layerID))
        summary.append(self.getGravimetricHumidity1(layerID))
        summary.append(self.getGravimetricHumidity2(layerID))
        summary.append(self.getUsableHumidity(layerID))
        summary.append(self.getRealDensity(layerID))
        summary.append(self.getApparentDensity(layerID))
        summary.append(self.getTotalPorosity(layerID))
        
        summary = np.array(summary)
        return summary
    
    
class Soil:
    
    def __init__(self, profileCode):
        ss = Soils()
        
        # Features for each layer [[l1],[l2],[l3]...]
        
        self.layerIdxs = []
        
        self.textures = []
        self.phs = []
        self.organics = []
        
        # Initial, final, thickness 
        self.horizonDepthInfos = []
        self.gravimetricHumidities1 = []
        self.gravimetricHumidities2 = []
        self.usableHumidities = []
        self.realDensities = []
        self.apparentDensities = []
        self.totalPorosities = []
        
        
        layers = ss.getLayers(profileCode)
        self.nbLayers = len(layers)
        
        # Foreach layer, append the values to the class attributes
        for layerCode in layers:
            layerID = ss.getLayerID(profileCode, layerCode)
            
            self.layerIdxs.append(layerID)
            
            self.textures.append(ss.getTexture(layerID))
            self.phs.append(ss.getPH(layerID))
            self.organics.append(ss.getOrganic(layerID))
            
            self.horizonDepthInfos.append(ss.getHorizonDepthInfos(layerID))
            self.gravimetricHumidities1.append(ss.getGravimetricHumidity1(layerID))
            self.gravimetricHumidities2.append(ss.getGravimetricHumidity2(layerID))
            self.usableHumidities.append(ss.getUsableHumidity(layerID))
            self.realDensities.append(ss.getRealDensity(layerID))
            self.apparentDensities.append(ss.getApparentDensity(layerID))
            self.totalPorosities.append(ss.getTotalPorosity(layerID))
            
            
            
           
