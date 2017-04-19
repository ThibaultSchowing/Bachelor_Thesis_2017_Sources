# -*- coding: utf-8 -*-
import numpy as np
import pandas as pd
import Soils

class CoffeeCups:
    
    def __init__(self,climatWrapper, path = "./Data/Coffee/BD_MapaPerfilesTazaRisaralda_ciat_utf-8.csv"):
        
        
        
        self.path = path
        self.climatWrapper = climatWrapper
        
        #"2011","2012","2013","2014","2015",
        #self.climatWrapper = avgs.ClimaticDataWraper(["2016"])
        
        
        # Reading file
        records_matrix = open(self.path)
        # Reading the data 
        tmpMatriceData =  [l.strip().split(';') for l in records_matrix.readlines()[2::]]
        records_matrix.seek(0)
        tmpMatriceInfo =  [l.strip().split(';') for l in records_matrix.readlines()[:1]]
        self.matriceData = np.array(tmpMatriceData)
        self.matriceInfo = np.array(tmpMatriceInfo)
        
        self.codes = {}
        for i in range(len(self.matriceData)):
            tmp = {str(self.matriceData[i,0]) : i}
            self.codes.update(tmp)
        
        self.infoTastePoints = ["Fragrance-Aroma","Savor","Savor residual","Body","Acidity","Balance","Sweetness","Clean-cup","Uniformity","Taster points","Total"]
        self.infoTasteNotas = ["Fragrance-Aroma Nota","Savor Nota","Savor residual Nota","Body Nota","Acidity Nota","Balance Nota","Sweetness Nota","Clean-cup Nota","Uniformity Nota"]
    #======================================================================
    # Culture informations: 
    # Return the informations below:
    # - Returns the table ID of the cup (index in matriceData)
    # - Returns the codes of all cups
    # - Returns the date of tasting
    # - Returns the taster name/entity
    # - Returns the Productor name
    # - Returns the Licence number
    # - Returns the SICA ID
    # - Returns the Property name
    # - Returns the Verada name
    # - Returns the Total SCA points
    # - Returns a summary of all those informations
    #
    #======================================================================
        
        
    # returns the index in matriceData of the requested cupCode
    def getTableID(self, cupCode):
        return self.codes[cupCode]
    
    # returns the list of all cup's codes
    def getCodes(self):
        return self.codes.keys()
    
    # Returns Date in np.string_ format
    def getDate(self, cupCode):
        return self.matriceData[self.codes[cupCode], 1]
    
    def getTaster(self, cupCode):
        return self.matriceData[self.codes[cupCode], 2]
    
    def getProductorName(self, cupCode):
        return self.matriceData[self.codes[cupCode], 3]
    
    # Cédula ?
    def getLicence(self, cupCode):
        return self.matriceData[self.codes[cupCode], 4]
    
    def getSICA(self, cupCode):
        return self.matriceData[self.codes[cupCode], 5]
    
    # Finca
    def getProperty(self, cupCode):
        return self.matriceData[self.codes[cupCode], 6]
    
    # Vereda
    def getVereda(self, cupCode):
        return self.matriceData[self.codes[cupCode], 7]
    

    
    
    def getCultureSummary(self, cupCode):
        summary = []
        
        summary.append(self.getDate(cupCode))
        summary.append(self.getTaster(cupCode))
        summary.append(self.getProductorName(cupCode))
        summary.append(self.getLicence(cupCode))
        summary.append(self.getSICA(cupCode))
        summary.append(self.getProperty(cupCode))
        summary.append(self.getVereda(cupCode))
        
        return summary
    
    
    
    #======================================================================
    # Taste informations: 
    # Return the textual values of the tastes
    #
    #======================================================================
    # returns trimed / separated notes (text) of different traits of coffee taste
    # For encoding test others .decode('iso-8859-1') works but bad
    
    def getTasteData(self, cupCode, index):
        tmp = self.matriceData[self.codes[cupCode], index]
        trimed = []
        trimed[:] = [f.strip() for f in tmp.split(",")]
        return trimed
    
    def getFragranceAromaNota(self, cupCode):
        return self.getTasteData(cupCode, 10)
    
    def getSavorNota(self, cupCode):
        return self.getTasteData(cupCode, 12)
    
    def getSavorResidualNota(self, cupCode):
        return self.getTasteData(cupCode, 14)
    
    def getBodyNota(self, cupCode):
        return self.getTasteData(cupCode, 16)
    
    def getAcidityNota(self, cupCode):
        return self.getTasteData(cupCode, 18)
    
    def getBalanceNota(self, cupCode):
        return self.getTasteData(cupCode, 20)
    
    def getSweetnessNota(self, cupCode):
        return self.getTasteData(cupCode, 22)
    
    def getCleancupNota(self, cupCode):
        return self.getTasteData(cupCode, 24)
    
    def getUniformityNota(self, cupCode):
        return self.getTasteData(cupCode, 26)
    
    def getTasteNoteSummary(self, cupCode):
        
        summary = []
        
        summary.append(self.getFragranceAromaNota(cupCode))
        summary.append(self.getSavorNota(cupCode))
        summary.append(self.getSavorResidualNota(cupCode))
        summary.append(self.getBodyNota(cupCode))
        summary.append(self.getAcidityNota(cupCode))
        summary.append(self.getBalanceNota(cupCode))
        summary.append(self.getSweetnessNota(cupCode))
        summary.append(self.getCleancupNota(cupCode))
        summary.append(self.getUniformityNota(cupCode))
        
        return summary
    
    #======================================================================
    # Taste informations: 
    # Return the number of Points of each note
    #
    #======================================================================
    
    def getTastePoints(self, cupCode, index):
        tmp = str(self.matriceData[self.codes[cupCode], index])
        if tmp == "":
            return None
        tmp = tmp.replace(",", ".")
        #print tmp
        try:
            return float(tmp)
        except:
            #[DEBUG] 
            #print("Cup code: ", cupCode,  "Data format problem. Data: ", tmp)
            return None
    
    def getTotalSCAPoints(self, cupCode):
        return self.getTastePoints(cupCode, 8)
    
    def getFragranceAromaPoints(self, cupCode):
        return self.getTastePoints(cupCode, 9)
    
    def getSavorPoints(self, cupCode):
        return self.getTastePoints(cupCode, 11)
    
    def getSavorResidualPoints(self, cupCode):
        return self.getTastePoints(cupCode, 13)
    
    def getBodyPoints(self, cupCode):
        return self.getTastePoints(cupCode, 15)
    
    def getAcidityPoints(self, cupCode):
        return self.getTastePoints(cupCode, 17)
    
    def getBalancePoints(self, cupCode):
        return self.getTastePoints(cupCode, 19)
    
    def getSweetnessPoints(self, cupCode):
        return self.getTastePoints(cupCode, 21)
    
    def getCleancupPoints(self, cupCode):
        return self.getTastePoints(cupCode, 23)
    
    def getUniformityPoints(self, cupCode):
        return self.getTastePoints(cupCode, 25)
    
    def getTasterPoints(self, cupCode):
        return self.getTastePoints(cupCode, 27)
    
    def getTastePointsSummary(self, cupCode):
        
        summary = []
        
        summary.append(self.getFragranceAromaPoints(cupCode))
        summary.append(self.getSavorPoints(cupCode))
        summary.append(self.getSavorResidualPoints(cupCode))
        summary.append(self.getBodyPoints(cupCode))
        summary.append(self.getAcidityPoints(cupCode))
        summary.append(self.getBalancePoints(cupCode))
        summary.append(self.getSweetnessPoints(cupCode))
        summary.append(self.getCleancupPoints(cupCode))
        summary.append(self.getUniformityPoints(cupCode))
        summary.append(self.getTasterPoints(cupCode))
        summary.append(self.getTotalSCAPoints(cupCode))
        
        return summary
    
    
    
    # returns table of uppercase / accentless strings
    def normalize(self, strtab):
        
        clearTab = [w.replace('Á', 'a') for w in strtab]
        clearTab = [w.replace('á', 'a') for w in clearTab]
        clearTab = [w.replace('é', 'e') for w in clearTab]
        clearTab = [w.replace('É', 'e') for w in clearTab]
        clearTab = [w.replace('í', 'i') for w in clearTab]
        clearTab = [w.replace('Í', 'i') for w in clearTab]
        clearTab = [w.replace('ó', 'o') for w in clearTab]
        clearTab = [w.replace('Ó', 'o') for w in clearTab]
        clearTab = [w.replace('ú', 'u') for w in clearTab]
        clearTab = [w.replace('Ú', 'u') for w in clearTab]
        clearTab = [w.replace('ñ', 'n') for w in clearTab]
        clearTab = [w.replace('Ñ', 'n') for w in clearTab]
        clearTab = [chars.upper().strip() for chars in clearTab]
        
        return clearTab
    
    # THE OBJECTIVE OF THE FUNCTIONS BELOW IS TO FORMAT THE DATA SO THEY'RE USABLE FOR STATISTIC USE (CONVERT TO BINARY, ETC)
    
    # TODO - Get taste notes binary tables or binary for residual savour,body, acidity, balance, sweetness, clean-cup, unoformity
    # TODO format the return datas
    # 
    def getSavorTable(self, cupCode):
        savorData = self.getFragranceAromaNota(cupCode)
        savorData = self.normalize(savorData)
        
        
        return savorData
    
    #
    #
    # Attention: missing values in the xls file -> paja
    #
    def getResidualTable(self, cupCode):
        savorTable = ["paja","Jugoso","Redondeado","Seco","Áspero","Persistente","Leve","Suave","Astringente","Sucio","Estructurado","Débil","Rápido","Apagado","Balanceado","elicado","Limpio"]
        savorTable = self.normalize(savorTable)
        
        savorResult = np.zeros(len(savorTable))
        
        # print savorTable
        #['JUGOSO', 'REDONDEADO', 'SECO', 'ASPERO', 'PERSISTENTE', 'LEVE', 'SUAVE', 'ASTRINGENTE', 'SUCIO', 'ESTRUCTURADO', 'DEBIL', 'RAPIDO', 'APAGADO', 'BALANCEADO', 'ELICADO', 'LIMPIO']
        
        savorData = self.getSavorResidualNota(cupCode)
        savorData = self.normalize(savorData)
        
        
        # only one possible value
        
        # TODO return formated value, fitted for analysis
        return savorData
    #
    #
    #
    #
    def getBodyTable(self, cupCode):
        bodyTable = ["Acuoso","Sedoso","Liso","Manchoso","Jugoso","Cremoso","Aterciopelado","Arenoso"]
        bodyTable = self.normalize(bodyTable)
        
        bodyResult = np.zeros(len(bodyTable))
        
        bodyData = self.getBodyNota(cupCode)
        bodyData = self.normalize(bodyData)
        
                
        return bodyData
    #
    #
    #
    #
    def getAcidityTable(self, cupCode):
        acidTable = ["Apagado","DÉbil","Brillante","Delicado","Flojo","Vibrante","Leve","Rápido","Tartárico","Málico","Citrico","Fosforico"]
        
        acidTable = self.normalize(acidTable)
        
        acidData = self.getAcidityNota(cupCode)
        
        acidData = self.normalize(acidData)
        
        return acidData
    #
    #
    #
    #
    def getBalanceTable(self, cupCode):
        balanceTable = ["Denso","Estructurado","Inconsistente","Desequilibrado","Profundo ","Equilibrado","Marcado","Agudo","Complejo","Redondeado","Jugoso","Fresco"]
        balanceTable = self.normalize(balanceTable)
        
        balanceData = self.getBalanceNota(cupCode)
        balanceData = self.normalize(balanceData)
        
        return balanceData
    #
    #
    #
    #
    def getSweetness(self, cupCode):
        sweetnessTable = ["Amargo","Dulce"]
        sweetnessTable = self.normalize(sweetnessTable)
        
        sweetnessData = self.getSweetnessNota(cupCode)
        sweetnessData = self.normalize(sweetnessData)
        
        #TODO - return 0 or 1
        
        return sweetnessData
    #
    #
    #
    #
    #
    def getCleancup(self, cupCode):
        cleancupTable = ["Limpia","Sucia"]
        cleancupTable = self.normalize(cleancupTable)
        
        cleancupData = self.getCleancupNota(cupCode)
        cleancupData = self.normalize(cleancupData)
        
        #TODO - return 0 or 1
        
        return cleancupData
    #
    #
    #
    #
    def getUniformity(self, cupCode):
        uniformityTable = ["consistente","inconsistente"]
        uniformityTable = self.normalize(uniformityTable)
        
        uniformityData = self.getUniformityNota(cupCode)
        uniformityData = self.normalize(uniformityData)
        
        # TODO - return 0 or 1 if there is only two different uniformity
        return uniformityData
    
    # TODO - Get geolocalisation and profile
    def getVeredaLocation(self, cupCode):
        # RETURN ONE LOCATION
        pereiraCoord = [-75.690601,4.8087174]
        return pereiraCoord
    def getSoilProfile(self, cupCode):
        
        # 66BALP02
        # 66CHICHU
        lambdaProfile = "66BALP02"
        return lambdaProfile
    
'''
 summary.append(self.getDate(cupCode))
        summary.append(self.getTaster(cupCode))
        summary.append(self.getProductorName(cupCode))
        summary.append(self.getLicence(cupCode))
        summary.append(self.getSICA(cupCode))
        summary.append(self.getProperty(cupCode))
        summary.append(self.getVereda(cupCode))
'''


class CoffeeCup:
    def __init__(self, code, cc):
        
        # COFFEE INFORMATIONS (OUTPUT)#
        
        #cc = CoffeeCups()
        self.code = code
        
        self.infoTitle = ["Date","Taster","Productor Name","ID number","SICA","Property","Vereda"]
        self.info = cc.getCultureSummary(code)
        
        self.taste = cc.getTasteNoteSummary(code)
        self.points = cc.getTastePointsSummary(code)
        self.infoTastePoints = cc.infoTastePoints
        self.infoTasteNotas = cc.infoTasteNotas
        self.totalPoints = cc.getTotalSCAPoints(code)
        
        self.savor = cc.getSavorTable(code)
        self.residual = cc.getResidualTable(code)
        self.body = cc.getBodyTable(code)
        self.acidity = cc.getAcidityTable(code)
        self.balance = cc.getBalanceTable(code)
        
        self.sweet = cc.getSweetness(code) # binary balue dulce-amargo
        self.cleancup = cc.getCleancup(code) # binary value clean-notclean
        self.uniformity = cc.getUniformity(code) # binary value uniform-not uniform
        
        #TODO IN COFFEECUPS CLASS ABOVE
        self.location = cc.getVeredaLocation(code)
        self.soilProfile = cc.getSoilProfile(code)
        
        # CLIMAT INFORMATIONS (INPUT)#
        
        # Modify the available years in __ini__() in the CoffeeCups class 
        climat = cc.climatWrapper
        
        # Number of years of climatic data requested
        self.nbYears = climat.nbYears
        self.years = climat.years
        
        self.tmax = climat.getTmaxAverages(self.location)
        self.tmin = climat.getTminAverages(self.location)
        self.tmean = climat.getTmeanAverages(self.location)
        self.dtr = climat.getDtrAverages(self.location)
        self.prec = climat.getPrecAverages(self.location)
        
        
        
        # SOILS INFORMATIONS (INPUT)# TODO get the soil code (codigo del perfil)
        soilInfo = Soils.Soil(self.soilProfile)
        self.organics = soilInfo.organics
        self.phs = soilInfo.phs
        self.textures = soilInfo.textures
        self.horizonDepthInfos = soilInfo.horizonDepthInfos
        self.gravimetricHumidities1 = soilInfo.gravimetricHumidities1
        self.gravimetricHumidities2 = soilInfo.gravimetricHumidities2
        self.usableHumidities = soilInfo.usableHumidities
        self.realDensities = soilInfo.realDensities
        self.apparentDensities = soilInfo.apparentDensities
        self.totalPorosities = soilInfo.totalPorosities
        
        # CREATE HERE THE DATAFRAME
        
        ## General information
        rawTitle = ["Code","Date","Taster","Productor Name","ID number","SICA","Property","Vereda"]
        rawData = [self.code, self.info[0], self.info[1], self.info[2], self.info[3], self.info[4], self.info[5], self.info[6]]
        
        ## Climatic datas
        measures = ["tmax","tmin","tmean","dtr","prec"]
        for m in measures:
            for y in self.years:
                for i in range(1,13):
                    title = str(m) + str(y) + "." + str(i)
                    rawTitle.append(title)
        self.rawTitle = rawTitle
        
        for t in self.tmax:
            rawData.append(t)
        for t in self.tmin:
            rawData.append(t)
        for t in self.tmean:
            rawData.append(t)
        for t in self.dtr:
            rawData.append(t)
        for t in self.prec:
            rawData.append(t)
            
        
        
        ## Soil datas
        
        # To make it uniform, each profile is suposed to have 3 layers. 
        soilTitle = ["ph1","ph2","ph3",
                     "organic1","organic2","organic3", 
                     "grav. moist 1", "grav. moist 2", "grav. moist 3",
                     "2nd grav. moist 1", "2nd grav. moist 2", "2nd grav. moist 3",
                     "usableMoist 1", "usableMoist 2", "usableMoist 3",
                    "real density1", "real density2", "real density3", 
                    "apparent density1", "apparent density2", "apparent density3", 
                    "total porosity1", "total porosity2", "total porosity3"]
        for T in soilTitle:
            rawTitle.append(T)
        
        # For each layer add the values if exists or add NA
        MAX_LAYER = 3
        
        #PH
        for i in range(0,MAX_LAYER):
            try:
                rawData.append(self.phs[i])
            except:
                rawData.append(None)
                
        #Organic material
        for i in range(0,MAX_LAYER):
            try:
                rawData.append(self.organics[i])
            except:
                rawData.append(None)
        
        # gravimetric moisture 1
        for i in range(0,MAX_LAYER):
            try:
                rawData.append(self.gravimetricHumidities1[i])
            except:
                rawData.append(None)
        
        # gravimetric moisture 2
        for i in range(0,MAX_LAYER):
            try:
                rawData.append(self.gravimetricHumidities2[i])
            except:
                rawData.append(None)
        
        # usableHumidities
        for i in range(0,MAX_LAYER):
            try:
                rawData.append(self.usableHumidities[i])
            except:
                rawData.append(None)
        
        # Real density
        
        for i in range(0,MAX_LAYER):
            try:
                rawData.append(self.realDensities[i])
            except:
                rawData.append(None)
        
        
        
        # Apparent density
        
        for i in range(0,MAX_LAYER):
            try:
                rawData.append(self.apparentDensities[i])
            except:
                rawData.append(None)
        
        
        
        # Total porosity
        
        for i in range(0,MAX_LAYER):
            try:
                rawData.append(self.totalPorosities[i])
            except:
                rawData.append(None)
        
        
        
        self.rawData = rawData
        
        