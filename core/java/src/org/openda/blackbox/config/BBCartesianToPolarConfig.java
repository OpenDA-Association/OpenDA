/* MOD_V2.0
* Copyright (c) 2012 OpenDA Association
* All rights reserved.
* 
* This file is part of OpenDA. 
* 
* OpenDA is free software: you can redistribute it and/or modify 
* it under the terms of the GNU Lesser General Public License as 
* published by the Free Software Foundation, either version 3 of 
* the License, or (at your option) any later version. 
* 
* OpenDA is distributed in the hope that it will be useful, 
* but WITHOUT ANY WARRANTY; without even the implied warranty of 
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
* GNU Lesser General Public License for more details. 
* 
* You should have received a copy of the GNU Lesser General Public License
* along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
*/


package org.openda.blackbox.config;

import java.util.List;

/**
 * Configuration for parameter adjustments by specifying the standard deviation
 * on the X- and Y-component of a vector, for cases where the vector itself is
 * expressed in terms of angle and radius.  
 */
public class BBCartesianToPolarConfig {

    private double xScale;
    private double yScale;
    private String xSuffixCaption;
    private String ySuffixCaption;
    private String xCaption;
    private String yCaption;
    private double stdDev;
	private List<BBStochModelVectorConfig> vectorConfigs;

    public BBCartesianToPolarConfig(double xScale, double yScale,
                                    String xSuffixCaption, String ySuffixCaption,
                                    String xCaption, String yCaption,
                                    double stdDev, List<BBStochModelVectorConfig> vectorConfigs) {

        this.xScale = xScale;
        this.yScale = yScale;
        this.xSuffixCaption = xSuffixCaption;
        this.ySuffixCaption = ySuffixCaption;
        this.xCaption = xCaption;
        this.yCaption = yCaption;
        this.stdDev = stdDev;
		this.vectorConfigs = vectorConfigs;
    }

    public double getXScale() {
        return xScale;
    }

    public double getYScale() {
        return yScale;
    }

    public String getXSuffixCaption() {
        return xSuffixCaption;
    }

    public String getYSuffixCaption() {
        return ySuffixCaption;
    }

    public String getXCaption() {
        return xCaption;
    }

    public String getYCaption() {
        return yCaption;
    }

    public double getStdDev() {
        return stdDev;
    }

	public List<BBStochModelVectorConfig> getVectorConfigs() {
        return vectorConfigs;
    }

}
