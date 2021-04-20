
"""
Contains utility functions for dealing with GPS data.
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

SURVEY_DTYPE = [('Year','u4'), ('Species', 'U10'), ('Easting', 'f8'), 
    ('Northing', 'f8'), ('Age', 'f8'), ('Sex', 'U10'), ('Tnights', 'f8')]

def convertGPSToSurvey(xCol, yCol, outputSurvey, spacing, year, species, age, 
        sex, trapnights):
    """
    Converts a GPS data into a survey csv file. 'xCol' and 'yCol' are input 1d arrays
    with the GPS coords. Each of these points are output (with the specifed
    year, species, age and sex) into the outputSurvey filealong with 
    intermediate points created from a  straight line between them at the 
    given spacing (in metres).

    year is an int. species and sex are assumed to be strings and are inserted into 
    the output file exactly as they are passed in. age and trapnights should be
    floats.

    outputSurvey is written in the standard .csv format that can be read by
    preProcessing.RawData.readSurveyData().
    
    """
    # use a bit of TuiView for this
    from tuiview.viewertoolclasses import bresenhamline

    if spacing < 1:
        msg = "Can't handle spacing values less than 1"

    # bresenhamline has an issue with the spacing, have to do a hack
    # (see below) so we must int spacing
    spacing = int(np.round(spacing))

    lastX = None
    lastY = None
    outData = None
    count = 0
    for x, y in zip(xCol, yCol):
        if lastX is None:
            lastX = x
            lastY = y
            # just put the first point in, we will do the interp next
            outData = np.zeros((1,), dtype=SURVEY_DTYPE)
            outData[0]['Year'] = year
            outData[0]['Species'] = species
            outData[0]['Easting'] = x
            outData[0]['Northing'] = y
            outData[0]['Sex'] = sex
            outData[0]['Age'] = age
            outData[0]['Tnights'] = trapnights
        else:

            # unfortunately bresenhamline doesn't work properly
            # when you set max_pts to anything other than -1.
            # Workaround is to get all points then subsample the result
            coords = bresenhamline(np.array([[lastX, lastY]]),
                        np.array([[x, y]]), -1)
            coords = coords[spacing::spacing]

            #if len(coords) > 0:
            #    print(lastX, lastY, x, y)
            #    print(coords)
            #    raise SystemExit()

            # create an array for this set of points
            nPoints = coords.shape[0] + 1
            tmpData = np.zeros((nPoints,), dtype=SURVEY_DTYPE)

            # set all the constant columns
            tmpData['Year'] = year
            tmpData['Species'] = species
            tmpData['Sex'] = sex
            tmpData['Age'] = age
            tmpData['Tnights'] = trapnights
            
            # set the points we generated
            tmpData[:nPoints-1]['Easting'] = coords[..., 1]
            tmpData[:nPoints-1]['Northing'] = coords[..., 0]

            # and the current points
            tmpData[-1]['Easting'] = x
            tmpData[-1]['Northing'] = y

            # append to the outData
            outData = np.append(outData, tmpData)
            count += 1

    # now write outData as a .csv file
    np.savetxt(outputSurvey, outData, fmt=['%d', '%s', '%f', '%f', '%f', '%s', '%f'],
                delimiter=',', header='Year,Species,Easting,Northing,Age,Sex,Tnights')

