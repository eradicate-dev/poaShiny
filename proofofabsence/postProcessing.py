#!/usr/bin/env python

########################################
########################################
# This file is part of Proof of Absence
# Copyright (C) 2016 Dean Anderson and Sam Gillingham
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
########################################
########################################


# IMPORT MODULES

#import matplotlib
#matplotlib.use('Agg')
#import matplotlib.pyplot as P
import pylab as P


import os
import sys
#import argparse
import numpy as np
from scipy.stats.mstats import mquantiles
import prettytable
import pickle
from proofofabsence import params

# for opening files with TuiView
from PyQt5.QtWidgets import QApplication
from tuiview import geolinkedviewers
from tuiview.viewerstretch import ViewerStretch

def quantileFX(a):
    """
    calc 95% CIs
    """
    return mquantiles(a, prob=[0.025, 0.975], axis=1)

def getPickleResults(pklpath, inPKL_Name):
    """
    unpickle results from preProcessing.py and calculation.py
    """
    PKLFName = os.path.join(pklpath, inPKL_Name)
    # unpickle preprocessing
    fileobj = open(PKLFName, 'rb')
    outObject = pickle.load(fileobj)
    fileobj.close()
    return(outObject)

class ResultsProcessing(object):
    def __init__(self, inputDataPath, outputDataPath, preProcessingResults,
            calculationResults, zoneShapeFName, SSePoFResultTableName,
            pofSSeGraphName, zoneSeResultTableName):
        """
        Object to read in results and produce tables, graphs and 2-D images
        """
        # necessary to connect to Qt for TuiView
        self.app = QApplication(sys.argv)

        # save zone shape file for overlaying on rasters
        self.zoneShapeFName = zoneShapeFName

        ##########################
        # Run obligatory functions
        self.unpickleWrapper(inputDataPath, outputDataPath, preProcessingResults,
            calculationResults)

        ## Result functions are called in project file
        # to make tables and figures
#        ##########################


    #########################
    #########################
    #   Class functions
    #
    def unpickleWrapper(self, inputDataPath, outputDataPath, preProcessingResults,
        calculationResults):
        """
        fx to unpickle data from preprocessing and calculation
        """
        self.inputdatapath = inputDataPath
        self.outputdatapath = outputDataPath

        # unpickle the preProcessing spatial data
        #self.predat = getPickleResults(self.outputdatapath, preProcessingResults)
        # unpickle the calculation results              #########   NEEDS TO MATCH UP IN CALCULATION.PY
        self.calcdat = getPickleResults(self.outputdatapath, calculationResults)

    def makeTableFX(self, timeName):
        # SSe 2-D, years by iteration
        self.SSe2D = self.calcdat.sensitivityMatrix
        self.years = np.arange(self.calcdat.params.years[0], (self.calcdat.params.years[-1] + 1)) 
        self.nYears = len(self.years)
        self.yrNames = self.years.astype('str')
        # PoF 2-D, years by iteration
        self.PoF2D = self.calcdat.poFMatrix
        self.prpSearched = self.calcdat.proportionSearchedExtent
        # make empty table to populate    
        resultTable = np.zeros(shape=(7, self.nYears))
        meanPoFAllYr = np.round(np.mean(self.PoF2D, axis=1), 3)
        resultTable[0] = meanPoFAllYr
        PoFQuantiles = np.round(quantileFX(self.PoF2D), 3)
        PoFQuantiles = PoFQuantiles.transpose()
        resultTable[1:3] = PoFQuantiles
        meanSSeAllYr = np.round(np.mean(self.SSe2D, axis=1), 3)
        resultTable[3] = meanSSeAllYr
        SSeQuantiles = np.round(quantileFX(self.SSe2D), 3)
        SSeQuantiles = SSeQuantiles.transpose()
        resultTable[4:6] = SSeQuantiles
        resultTable[6] = np.round(self.prpSearched, 3)
        resultTable = resultTable.transpose()
        aa = prettytable.PrettyTable([timeName, 'Mean PoF', 'Lo CI PoF', 'Hi CI PoF',
                                'Mean SSe', 'Lo CI SSe', 'Hi CI SSe', 'Prop. Searched'])
#        aa = prettytable.PrettyTable(['Years', 'Mean PoF', 'Lo CI PoF', 'Hi CI PoF',
#                                'Mean SSe', 'Lo CI SSe', 'Hi CI SSe', 'Prop. Searched'])
        for i in range(self.nYears):
            name = self.yrNames[i]
            row = [name] + resultTable[i].tolist()
            aa.add_row(row)
        print('##############   PoF and SSe Results Table    ###########')
        print(aa)
        self.summaryTable = resultTable.copy()


    def writeToFileFX(self, SSePoFResultTableName, timeName):
        """
        # Write result table to file
        """
        (m, n) = self.summaryTable.shape
        # create new structured array with columns of different types
        structured = np.empty((m,), dtype=[(timeName, 'U12'), ('Mean PoF', np.float),
                    ('Lo CI PoF', np.float), ('Hi CI PoF', np.float), ('Mean SSe', np.float),
                    ('Lo CI SSe', np.float), ('Hi CI SSe', np.float),
                    ('Prop. Searched', np.float)])
        # copy data over
        structured['Mean PoF'] = self.summaryTable[:, 0]
        structured['Lo CI PoF'] = self.summaryTable[:, 1]
        structured['Hi CI PoF'] = self.summaryTable[:, 2]
        structured['Mean SSe'] = self.summaryTable[:, 3]
        structured['Lo CI SSe'] = self.summaryTable[:, 4]
        structured['Hi CI SSe'] = self.summaryTable[:, 5]
        structured['Prop. Searched'] = self.summaryTable[:, 6]
        structured[timeName] = self.yrNames
        summaryTableFname = os.path.join(self.outputdatapath, SSePoFResultTableName)
        np.savetxt(summaryTableFname, structured, fmt=['%s', '%.4f', '%.4f', '%.4f',
                    '%.4f', '%.4f', '%.4f', '%.4f'], comments = '', delimiter=',',
                    header=timeName + ',' + 'Mean_PoF, Lo_CI_PoF, Hi_CI_PoF, Mean_SSe, Lo_CI_SSe, Hi_CI_SSe, Prop_Searched')


    def makeZoneTableFX(self):
        # zone Se 3-D: (years, iteration and zones)
#        self.zoneSe3D= self.calcdat.zoneSeMatrix
        self.zoneYears = np.arange(self.calcdat.params.years[0], (self.calcdat.params.years[-1] + 1)) 
        self.nYears = len(self.zoneYears)
        self.yrNames = self.zoneYears.astype('str')
#        self.zoneNames = self.calcdat.Name_zone
        print('Name_zone', self.calcdat.Name_zone)

        # make empty table to populate    
        self.zoneResultTable = np.zeros(shape=((self.nYears) * self.calcdat.numberZones, 4))
        self.zoneNameArray = np.empty((self.nYears * self.calcdat.numberZones), dtype = '<U12')
        self.yearArray = np.empty((self.nYears * self.calcdat.numberZones), dtype = '<U12')
        rowCount = 0
        ## LOOP YEARS
        for j in range(self.nYears):
            year_j = self.zoneYears[j] 
            # all iter within year j and zone i
            ## LOOP ZONES
            for i in range(self.calcdat.numberZones):
                name_i = self.calcdat.Name_zone[i]
                self.zoneNameArray[rowCount] = name_i
                self.yearArray[rowCount] = year_j
                sez_ij = self.calcdat.zoneSeMatrix[j, :, i]
                self.zoneResultTable[rowCount, 0] = np.round(np.mean(sez_ij), 3)
                # get quantiles
                quants = np.round(mquantiles(sez_ij, prob=[0.025, 0.975]), 3)
                self.zoneResultTable[rowCount, 1:3] = quants
                self.zoneResultTable[rowCount, 3] = (
                    np.round(self.calcdat.proportionSearchedZone[j, i], 3))
                rowCount += 1
#        print('self.zoneResultTable', self.zoneResultTable)


    def writeZoneTableToFileFX(self, zoneSeResultTableName, timeName):
        """
        # Write result table to file
        """
        # create new structured array with columns of different types
        (m, n) = self.zoneResultTable.shape
        print('shp zone table: ', m, n)
        structured = np.empty((m,), dtype=[(timeName, 'U12'), ('Zone Name', 'U12'), 
                    ('Mean Zone Se', np.float), ('Lo CI Zone Se', np.float), 
                    ('Hi CI Zone Se', np.float), ('Prop. Zone Searched', np.float)])
        # copy data over
        structured['Zone Name'] = self.zoneNameArray
        structured[timeName] = self.yearArray
        structured['Mean Zone Se'] = self.zoneResultTable[:, 0]
        structured['Lo CI Zone Se'] = self.zoneResultTable[:, 1]
        structured['Hi CI Zone Se'] = self.zoneResultTable[:, 2]
        structured['Prop. Zone Searched'] = self.zoneResultTable[:, 3]
        zoneTableFname = os.path.join(self.outputdatapath, zoneSeResultTableName)
        np.savetxt(zoneTableFname, structured, fmt=['%s', '%s', '%.4f', '%.4f',
                    '%.4f', '%.4f'], comments = '', delimiter = ',',
                    header=timeName + ', Zones, Mean_SeZ, Lo_CI_SeZ, Hi_CI_SeZ, Prop_Searched')
        ## PRINT TABLE TO SCREEN
        aa = prettytable.PrettyTable([timeName, 'Zones', 'Mean SeZ', 'Lo CI SeZ', 'Hi CI SeZ', 
            'Prop. Searched'])
        for i in range(m):
            row = structured[i].tolist()
            aa.add_row(row)
        print('#####################################################')
        print('##############   Zone Se Results Table    ###########')
        print(aa)


    def plotFX(self, pofSSeGraphName, probName, timeName, targetPoA, priorTime):
        """
        graph PoF and SSe results
        """
        # graph PoF results.
        P.figure(figsize=(16, 8))
        P.subplot(1, 2, 1)
        priorYear = self.years[0] - 1
        self.yearsArray = np.append(priorYear, self.years)
        meanPrior = np.mean(self.calcdat.priorStore)
        quantsPrior = mquantiles(self.calcdat.priorStore, prob = [0.025, 0.975])
        meanPoF = np.append(meanPrior, self.summaryTable[:, 0])
        lowQuantsPoF = np.append(quantsPrior[0], self.summaryTable[:, 1]) 
        highQuantsPoF = np.append(quantsPrior[1], self.summaryTable[:, 2]) 
        P.plot(self.yearsArray, meanPoF, label='Mean probability', color='k', linewidth=5)
        P.plot(self.yearsArray, lowQuantsPoF, label='95% CI', linewidth=2, color='k')
        P.plot(self.yearsArray, highQuantsPoF, color='k')
        P.axhline(y = targetPoA, color='k', ls='dashed')
        miny = np.min(lowQuantsPoF) - .1
        if miny < 0:
            miny = 0
#        P.ylim([miny, 1])
#        P.xlim([(np.min(self.yearsArray) - 0.1), (np.max(self.yearsArray) + 0.1)])
        ax = P.gca()
        for tick in ax.xaxis.get_major_ticks():
            tick.label.set_fontsize(14)
        for tick in ax.yaxis.get_major_ticks():
            tick.label.set_fontsize(14)
        self.getNTicks(priorTime)
        P.xticks(self.xTicksYr, self.xlabelsYr)
        P.xlabel(timeName, fontsize=17)
        P.ylabel(probName, fontsize=17) # probname because sometimes 'free' or 'prevalence'
        P.legend(loc='lower right')
        # graph SSe results
        P.subplot(1, 2, 2)

        ax2 = P.gca()
        lowCI = self.summaryTable[:,3] - self.summaryTable[:, 4]
        hiCI = self.summaryTable[:,5] - self.summaryTable[:, 3]
        lns1 = ax2.errorbar(self.years, self.summaryTable[:, 3], yerr = [lowCI, hiCI], ecolor = 'black',
            marker = 'o', markerfacecolor = 'black', ms = 3, ls = 'none', mew = 3,
            markeredgecolor = 'black', capsize = 8, label = 'Mean and 95% CI')
        P.legend(handles = [lns1], loc='upper left') 
#        P.plot(self.years, self.summaryTable[:, 3], label='Mean sensitivity', color='k', linewidth=5)
#        P.plot(self.years, self.summaryTable[:, 4], label='95% CI',
#               linewidth=2, color='k')
#        P.plot(self.years, self.summaryTable[:, 5], color='k')
        maxy = np.max(self.summaryTable[:, 5]) + .1
        miny = np.min(self.summaryTable[:, 4]) - .1
        if maxy > 1:
            maxy = 1
        P.ylim([miny, maxy])
        P.xlim([(np.min(self.yearsArray) - 0.1), (np.max(self.yearsArray) + 0.1)])
        P.xlabel(timeName, fontsize=17)
        P.ylabel('System sensitivity', fontsize=17)
#        P.legend(loc='lower left')
        for tick in ax2.xaxis.get_major_ticks():
            tick.label.set_fontsize(14)
        for tick in ax2.yaxis.get_major_ticks():
            tick.label.set_fontsize(14)
        P.xticks(self.xTicksYr, self.xlabelsYr)
        # save plots to project directory
        pofSSeGraphFname = os.path.join(self.outputdatapath, pofSSeGraphName)
        P.savefig(pofSSeGraphFname, format='png')
        P.show()


    def getNTicks(self, priorName):
        """
        get n==5 ticks for plot
        """
        if self.nYears <= 5:
            self.byTicks = 1
        else:
            self.byTicks = np.round(self.nYears / 5.0)
        self.xTicksYr = np.arange(self.yearsArray[0], (self.yearsArray[-1] + 1), self.byTicks)
        self.xTicksYr = self.xTicksYr.astype(int)
        self.xlabelsYr = [str(x) for x in self.xTicksYr]
        self.xlabelsYr[0] = priorName

    def plotMeanSeUAllYears(self):
        """
        plot Mean Seu by year using tuiview
        """
       
        # work out the 'layername' for the shapefile - which is just the filename
        # without the extension for .shp
        layername = os.path.splitext(os.path.basename(self.zoneShapeFName))[0]

        # create a container for geolinked viewers
        viewers = geolinkedviewers.GeolinkedViewers()
        # start a viewer 
        viewer = viewers.newViewer()
        
        # resize
        viewer.resizeForWidgetSize(500, 500)
        
        # stretch
        stretch = ViewerStretch()
        #stretch.setGreyScale()
        stretch.setPseudoColor('Reds')
        stretch.setLinearStretch()
        # band number set in loop below
        
        for band, year in enumerate(self.years, start=1):
            # set the right band
            stretch.setBands((band,))
            
            # open raster layer
            viewer.addRasterInternal(self.calcdat.meanSeuTifPathName, stretch)
            
            # add the zones shapefile
            viewer.addVectorInternal(self.zoneShapeFName, layername)
        
            # save as png
            Fname = os.path.join(self.outputdatapath, 'MeanSeU_From_%d.png' % year)
            viewer.saveCurrentViewInternal(Fname)
            
            # remove the top most layers for next time around the loop
            viewer.removeLayer() # shp
            viewer.removeLayer() # raster
            
        viewers.closeAll()

    def plotRelRisk(self):
        """
        plot of RelRisk with zone boundaries
        """
        # create a container for geolinked viewers
        viewers = geolinkedviewers.GeolinkedViewers()
        # start a viewer 
        viewer = viewers.newViewer()

        # work out the 'layername' for the shapefile - which is just the filename
        # without the extension for .shp
        layername = os.path.splitext(os.path.basename(self.zoneShapeFName))[0]
        
        # resize
        viewer.resizeForWidgetSize(500, 500)
        
        # stretch
        stretch = ViewerStretch()
        stretch.setBands((1,))
        #stretch.setGreyScale()
        stretch.setPseudoColor('Reds')
        stretch.setLinearStretch()
        
        # open file
        viewer.addRasterInternal(self.calcdat.relRiskTifName, stretch)
        
        # add the zones shapefile
        viewer.addVectorInternal(self.zoneShapeFName, layername)
            
        # save as png
        RRFigFname = os.path.join(self.outputdatapath, 'updatedRelRisk.png')
        viewer.saveCurrentViewInternal(RRFigFname)
        
        viewers.closeAll()
        
