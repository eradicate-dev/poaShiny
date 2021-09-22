"""
Contains class that allow setting the numeric parameters that
control the running of the model.
"""

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

from __future__ import print_function, division
import numpy as np
from numba import jit

PERT_SHAPE = 4.0
"default pert distribution shape"

TYPE_POSSUM = 0
TYPE_POSSTRAP = 1
TYPE_CHEWCARD = 2
TYPE_FERRET = 3
TYPE_PIG = 4
TYPE_REDDEER = 5
"""
default types of animals
other types can be added but give them a unique number 
"""

DETECT_DISEASE_CHEWCARD = 0
DETECT_DISEASE_TRAP = 1
DETECT_DISEASE_SENTINEL = 2
DETECT_ANIMAL = 3
"""
Different detection process
"""

ANIMAL_PARAM_DTYPE = [('mean_sig', 'f8'), ('sd_sig', 'f8'),
                      ('mean_CC', 'f8'), ('sd_CC', 'f8'), ('a_CC', 'f8'), ('b_CC', 'f8'),
                      ('mean_capt', 'f8'), ('sd_capt', 'f8'), ('a_capt', 'f8'), ('b_capt', 'f8'),
                      ('mean_test', 'f8'), ('sd_test', 'f8'), ('a_test', 'f8'), ('b_test', 'f8'),
                      ('mean_infect', 'f8'), ('sd_infect', 'f8'), ('a_infect', 'f8'),
                      ('b_infect', 'f8')]
"""
numpy dtype for structured array that contains the parameters for 
each animal
"""

MALE = 0
FEMALE = 1
"sex of the animal"

GRID_SURVEY_PARAM_DTYPE = [('year', 'i4'), ('a', 'f8'), ('b', 'f8'), ('code', 'u8')]
"""
numpy dtype for structured array that contains the parameters each grid
survey.
"""


class Animal(object):
    """
    Class that described an animal/trap and how to process it

    'name' is the what is used in the traps file, More stuff may be added
    later
    'detect' is one of the DETECT_* constants above
    """

    def __init__(self, name, detect):
        self.name = name
        self.detect = detect


class AnimalTypes(object):
    """
    Class that knows about all the types of animals/traps
    that are in the surveillance data.

    New types can be added. By default just the standard ones
    are known about (see TYPE_* constants above).
    """
    functionDict = None

    def __init__(self):
        """
        add the standard animal codes with their functions
        """
        self.functionDict = {TYPE_POSSUM: Animal("possum", DETECT_DISEASE_TRAP),
                             TYPE_POSSTRAP: Animal("posstrap", DETECT_DISEASE_TRAP),
                             TYPE_CHEWCARD: Animal("chewcard", DETECT_DISEASE_CHEWCARD),
                             TYPE_FERRET: Animal("ferret", DETECT_DISEASE_SENTINEL),
                             TYPE_PIG: Animal("pig", DETECT_DISEASE_SENTINEL),
                             TYPE_REDDEER: Animal("reddeer", DETECT_DISEASE_SENTINEL)}

    def addAnimal(self, animal, name, detect):
        """
        Add a new animal/sentinel etc that isn't one of the standard
        types.
        'animal' is the code to use for setting the params below.
        'name' is the name that will appear in the survey data
        'detect' is one of the DETECT_* constants above that specifies
            the detection process.
        """
        if animal in self.functionDict:
            msg = 'animal code already in use'
            raise KeyError(msg)
        self.functionDict[animal] = Animal(name.lower(), detect)


class POAParameters(object):
    """
    Contains the parameters for the POA run. This object is also required
    for the pre-processing.
    """
    startpu = 1.0
    prior_a = None
    prior_b = None
    prior_min = None
    prior_max = None
    intro_a = None
    intro_b = None
    intro_min = None
    intro_max = None
    nIter = 10
    nChewcardTraps = 3
    minRR = 0
    RRTrapDistance = 0.0
    parameterArray = None
    animals = None
    multipleZones = False
    Pz = 1.0  # zone design prevalence - should always be set to 1
    # kBufferAnimals are the types we buffer with higher RR values
    # if they are in areas of < minRR
    #    RRBufferAnimals = {TYPE_POSSUM, TYPE_POSSTRAP, TYPE_CHEWCARD}
    gridSurveyData = None
    gridSurveyParams = None
    # set to greater than one for multi threading
    nthreads = 1

    def __init__(self, animals=None):
        """
        Set animals to an instance of AnimalTypes if the default
        set of animals/traps isn't appropriate.
        """
        if animals is None:
            animals = AnimalTypes()

        # can't do a len() as the codes could be non-contiguous
        # and we want to index it as an array
        nSpecies = max(animals.functionDict.keys()) + 1
        self.parameterArray = np.ones(nSpecies, dtype=ANIMAL_PARAM_DTYPE)
        self.animals = animals
        self.RRBufferAnimals = {TYPE_POSSUM, TYPE_POSSTRAP, TYPE_CHEWCARD}
        print('nSpecies', nSpecies)
        print('parameterArray', self.parameterArray.shape)
        print('animals', self.animals)

    def setNumThreads(self, nthreads):
        """
        Set the number of threads to use for processing. The number of
        iterations will be split into even amounts for each thread.
        """
        if nthreads < 1:
            msg = 'Number of threads must be greater of equal to one'
            raise ValueError(msg)

        self.nthreads = nthreads

    def setMinRR(self, minRR):
        """
        This sets the minimum RR - values below this are ignored
        """
        self.minRR = minRR

    def setRRTrapDistance(self, RRDistance):
        """
        Sets the distance around traps which must have their RR values
        boosted to max(max(RR) / 2, minRR)
        """
        self.RRTrapDistance = RRDistance

    def setYears(self, startYear, endYear):
        """
        Sets a sequence of years that the model is to be run over
        """
        self.years = np.arange(startYear, (endYear + 1), dtype=int)

    def addRRBufferAnimal(self, animalCode):
        """
        Add an animal code to the set of animals that we buffer
        the RR array around (see setRRTrapDistance)
        """
        self.RRBufferAnimals.add(animalCode)

    def setNumIterations(self, nIter):
        """
        Set the number of iterations to do for each year
        """
        self.nIter = nIter

    def setNumChewcardTraps(self, nChewcardTraps):
        """
        Set the number of chewcard traps
        """
        self.nChewcardTraps = nChewcardTraps

    def setPu(self, startpu, puRateIncrease=0.0):
        """
        Set the Pu - cell level design prevalence
        """
        self.pu = startpu
        self.puRate = puRateIncrease

    def setPrior(self, min, mode, max):
        """
        Sets the prior probability
        """
        self.prior_a, self.prior_b = findBetaPert(min, mode, max)
        self.prior_min = min
        self.prior_max = max

    def setIntro(self, min, mode, max):
        """
        Sets the intro probability
        """
        self.intro_a, self.intro_b = findBetaPert(min, mode, max)
        self.intro_min = min
        self.intro_max = max

    def setSigma(self, animal, mean, sd):
        """
        Sets the sigma parameters for the given animal index
        """
        self.parameterArray[animal]['mean_sig'] = mean
        self.parameterArray[animal]['sd_sig'] = sd

    def setChewcard(self, animal, mean, sd):
        """
        Sets the chewcard parameters for the given animal index
        """
        self.parameterArray[animal]['mean_CC'] = mean
        self.parameterArray[animal]['sd_CC'] = sd

        a, b = findBeta(mean, sd)
        self.parameterArray[animal]['a_CC'] = a
        self.parameterArray[animal]['b_CC'] = b

    def setCapture(self, animal, mean, sd):
        """
        Sets the capture parameters for the given animal index
        """
        self.parameterArray[animal]['mean_capt'] = mean
        self.parameterArray[animal]['sd_capt'] = sd

        a, b = findBeta(mean, sd)
        self.parameterArray[animal]['a_capt'] = a
        self.parameterArray[animal]['b_capt'] = b

    def setTest(self, animal, mean, sd):
        """
        Sets the capture parameters for the given animal index
        """
        self.parameterArray[animal]['mean_test'] = mean
        self.parameterArray[animal]['sd_test'] = sd

        a, b = findBeta(mean, sd)
        self.parameterArray[animal]['a_test'] = a
        self.parameterArray[animal]['b_test'] = b

    def setInfect(self, animal, mean, sd):
        """
        Sets the infect parameters for the given animal index
        """
        self.parameterArray[animal]['mean_infect'] = mean
        self.parameterArray[animal]['sd_infect'] = sd

        a, b = findBeta(mean, sd)
        self.parameterArray[animal]['a_infect'] = a
        self.parameterArray[animal]['b_infect'] = b

    def setGridSurvey(self, gridSurveyYears, gridSurveyData,
                      gridSurveyMeans, gridSurveySD, gridSurveyCodes):
        """
        Sets the data and parameters for grid survey
        """
        nGrids = len(gridSurveyYears)
        self.gridSurveyParams = np.empty((nGrids,),
                                         dtype=GRID_SURVEY_PARAM_DTYPE)

        for i in range(nGrids):
            self.gridSurveyParams[i]['year'] = gridSurveyYears[i]
            a, b = findBeta(gridSurveyMeans[i], gridSurveySD[i])
            self.gridSurveyParams[i]['a'] = a
            self.gridSurveyParams[i]['b'] = b
            self.gridSurveyParams[i]['code'] = gridSurveyCodes[i]

        self.gridSurveyData = gridSurveyData

    def setMultipleZones(self, multipleZones):
        """
        Sets whether we expect multiple zones in the shape
        file or not.
        """
        self.multipleZones = multipleZones


def findBeta(mu, sdev):
    """
    Find a and b of a Beta distribution given mean and standard deviation
    """
    sdevsq = sdev * sdev;
    a = mu * ((mu * (1.0 - mu)) / sdevsq - 1.0)
    b = (1.0 - mu) * ((mu * (1.0 - mu)) / sdevsq - 1.0)

    return a, b


def findBetaPert(min, mode, max, shape=PERT_SHAPE):
    """
    Find a and b of a Beta distribution given the information for a Pert distribution 
    (min, mode, max)
    see http://rgm2.lab.nig.ac.jp/RGM2/func.php?rd_id=mc2d:pert 
    """
    mu = (min + max + shape * mode)/(shape + 2.0);
    if np.round(mu,8) == mode:
        shape1 = int(1) + shape / int(2)
    else:
        shape1 = (mu - min) * (int(2) * mode - min - max)/((mode-mu)*(max - min))
    shape2 = shape1 * (max - mu)/(mu - min)
    return shape1, shape2


