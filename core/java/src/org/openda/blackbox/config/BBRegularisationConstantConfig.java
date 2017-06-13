/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
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
 * Configuration for regularisation by means of a constant factor
 */
public class BBRegularisationConstantConfig {

    // possible status values
    public static final int OPERATION_ADD = 0;
    public static final int OPERATION_SET = 1;
	public static final int OPERATION_MULTIPLY = 2;

	public static final int TRANSFORMATION_IDENTITY = 0;
	public static final int TRANSFORMATION_LN = 1;
	public static final int TRANSFORMATION_SET =2;

	private double scale;
    private double stdDev;
	private double mean;
	private String uncertainItemId;

	private int operationType;
	private int transformationType;

	private List<BBStochModelVectorConfig> vectorConfigs;
	private String id = null;

	public BBRegularisationConstantConfig(String id,
										  double scale,
										  double stdDev,
										  double mean,
										  String uncertainItemId,
										  int operationType,
										  int transformationType,
										  List<BBStochModelVectorConfig> vectorConfigs) {
		this.id = id;
		this.scale = scale;
		this.stdDev = stdDev;
		this.mean = mean;
		this.uncertainItemId = uncertainItemId;
		this.operationType = operationType;
		this.transformationType = transformationType;
		this.vectorConfigs = vectorConfigs;
	}

	public double getScale() {
        return scale;
    }

    public double getStdDev() {
        return stdDev;
    }

	public double getMean() {
		return mean;
	}

	public boolean useUncertaintyEngine() {
		return uncertainItemId != null;
	}

	public String getUncertainItemId() {
		return uncertainItemId;
	}

	public int getTransformation() {
        return transformationType;
    }

    public List<BBStochModelVectorConfig> getVectorConfigs() {
        return vectorConfigs;
    }

	public String getId() {
		return id;
	}
}
