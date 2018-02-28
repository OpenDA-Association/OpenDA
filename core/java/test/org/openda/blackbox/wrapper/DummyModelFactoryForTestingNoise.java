/* OpenDA v2.4.3 
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
package org.openda.blackbox.wrapper;
import org.openda.blackbox.interfaces.IModelFactory;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IModelInstance;
import org.openda.interfaces.IStochModelFactory;
import org.openda.noiseModels.MapsNoiseModelInstance;
import org.openda.utils.Time;

import java.io.File;
import java.util.ArrayList;

/**
 * Dummy model factory for instantiating Dummy2dFieldModels
 */
public class DummyModelFactoryForTestingNoise implements IModelFactory {

	private int instanceCounter = 0;
	private double[] xAxisSteps = null;
	private double[] yAxisSteps = null;
	private ArrayList<String> exchangeItemIdsTS = new ArrayList<String>();
	private ArrayList<String> exchangeItemIds2D = new ArrayList<String>();
    private Time simulationTime = null;

	public void initialize(File workingDir, String[] args) {

		// parse arguments as key=value pairs
		for (String argument : args) {
			String[] argParts = argument.split("=");
			if (argParts[0].trim().equals("x")) {
				xAxisSteps = org.openda.noiseModels.MapsNoiseModelInstance.parseGridOneDim(argParts[1]);
			} else if (argParts[0].trim().equals("y")) {
				yAxisSteps = MapsNoiseModelInstance.parseGridOneDim(argParts[1]);
			} else if (argParts[0].trim().equals("tsEI")) {
				exchangeItemIdsTS.add(argParts[1]);
			} else if (argParts[0].trim().equals("2dEI")) {
				exchangeItemIds2D.add(argParts[1]);
			} else if (argParts[0].trim().equals("timeHorizon")) {
				String timespanString = argParts[1];
				if(!timespanString.contains("...")){
					throw new RuntimeException("NoiseModel expects a timeHorizon");
				}
				Time[] simulationSequence = Time.Mjds2Times(TimeUtils.dateTimeSequenceString2Mjd(timespanString));
				int nTimes=simulationSequence.length;
				double startTime     = simulationSequence[0].getBeginMJD();
				double endTime       = simulationSequence[nTimes-1].getBeginMJD();
				double deltaT = simulationSequence[1].getBeginMJD()-simulationSequence[0].getBeginMJD();
				simulationTime = new Time(startTime, endTime, deltaT);
			}
		}
		if (exchangeItemIds2D.size() > 0 && xAxisSteps == null) {
			throw new RuntimeException("No x/y axis defined");
		}
	}

	public IModelInstance getInstance(String[] arguments, IStochModelFactory.OutputLevel outputLevel) {
		return new DummyModelForTestingNoise(++instanceCounter,
				xAxisSteps, yAxisSteps,
				exchangeItemIdsTS,
				exchangeItemIds2D,
				simulationTime);
	}

	public void finish() {
		// no action needed (yet)
	}
}
