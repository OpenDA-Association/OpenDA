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
package org.openda.blackbox.wrapper;

import org.openda.exchange.ArrayExchangeItem;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.DoublesExchangeItem;
import org.openda.interfaces.*;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.utils.Array;
import org.openda.utils.Time;
import org.openda.utils.Vector;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Dummy model for testing the stoch model configuration for time dependent noise 
 */
public class DummyModelForTestingNoise implements IModelInstance {

	private HashMap<String, IPrevExchangeItem> exchangeItems = new HashMap<String, IPrevExchangeItem>();
	private Time simulationTime;
	private Time currentTime;
	private ArrayList<String> exchangeItemsIdsTS;
	private ArrayList<String> exchangeItemsIds2D;
	private double[] xAxisSteps=null;
	private double[] yAxisSteps=null;
	private int instanceNumber;

	public DummyModelForTestingNoise(int instanceNumber,
									 double[] xAxisSteps, double[] yAxisSteps,
									 ArrayList<String> exchangeItemsIdsTS,
									 ArrayList<String> exchangeItemsIds2D,
									 Time simulationTime) {
		this.instanceNumber=instanceNumber;
		this.simulationTime = simulationTime;
		this.currentTime = new Time(simulationTime.getBeginTime());
		this.exchangeItemsIds2D=exchangeItemsIds2D;
		this.exchangeItemsIdsTS=exchangeItemsIdsTS;
		if(xAxisSteps!=null){
			this.xAxisSteps=new double[xAxisSteps.length];
			System.arraycopy(xAxisSteps, 0, this.xAxisSteps, 0, xAxisSteps.length);
			this.yAxisSteps=new double[yAxisSteps.length];
			System.arraycopy(yAxisSteps, 0, this.yAxisSteps, 0, yAxisSteps.length);
		}
		
		for (String exchangeItemId : exchangeItemsIdsTS) {
			exchangeItems.put(exchangeItemId,
					new DummmyExchangeItemTS(exchangeItemId, instanceNumber,
							this.currentTime));
		}

		for (String exchangeItem2dId : exchangeItemsIds2D) {
			exchangeItems.put(exchangeItem2dId,
					new DummmyExchangeItem2D(exchangeItem2dId, instanceNumber,
							xAxisSteps, yAxisSteps, this.currentTime));
		}
	    //add a state item
		ArrayExchangeItem state=new ArrayExchangeItem("state",Role.InOut);
		IArray tempArray=new Array("{1,2,3}");
		state.setArray(tempArray);
		exchangeItems.put("state",state);
		
	}

	public String[] getExchangeItemIDs() {
		return exchangeItems.keySet().toArray(new String[exchangeItems.size()]);
	}

	public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
		return exchangeItems.get(exchangeItemID);
	}

	public ITime getTimeHorizon() {
		return simulationTime;
	}

	public ITime getCurrentTime() {
		return currentTime; 
	}

	public void compute(ITime targetTime) {
		System.out.println("DummyModelForTestingNoise.compute until "+targetTime);
		this.currentTime = new Time(targetTime);
		for (String exchangeItemId : exchangeItemsIdsTS) {
			DummmyExchangeItemTS ei = (DummmyExchangeItemTS)exchangeItems.get(exchangeItemId);
			ei.currentTime=new Time(targetTime);
		}

		for (String exchangeItem2dId : exchangeItemsIds2D) {
			DummmyExchangeItem2D ei = (DummmyExchangeItem2D)exchangeItems.get(exchangeItem2dId);
			ei.currentTime=new Time(targetTime);
		}
	}

	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyModelForTestingNoise.getObservedLocalization(): Not implemented yet.");
	}

	public IModelState saveInternalState() {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyModelForTestingNoise.saveInternalState(): Not implemented yet.");
	}

	public void restoreInternalState(IModelState savedInternalState) {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyModelForTestingNoise.restoreInternalState(): Not implemented yet.");
	}

	public void releaseInternalState(IModelState savedInternalState) {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyModelForTestingNoise.releaseInternalState(): Not implemented yet.");
	}

	public IModelState loadPersistentState(File persistentStateFile) {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyModelForTestingNoise.loadPersistentState(): Not implemented yet.");
	}

	public File getModelRunDir() {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyModelForTestingNoise.getModelRunDir(): Not implemented yet.");
	}

	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyModelForTestingNoise.getExchangeItemIDs(): Not implemented yet.");
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyModelForTestingNoise.getDataObjectExchangeItem(): Not implemented yet.");
	}

	public void finish() {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyModelForTestingNoise.finish(): Not implemented yet.");
	}

	public IInstance getParent() {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyModelForTestingNoise.getParent(): Not implemented yet.");
	}

	public void initialize(File workingDir, String[] arguments) {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyModelForTestingNoise.initialize(): Not implemented yet.");
	}

	private class DummmyExchangeItem2D extends DoublesExchangeItem{

		private int modelInstanceNumber;
		private Time currentTime;

		public DummmyExchangeItem2D(String exchangeItem2dId, int modelInstanceNumber,
									double[] xAxisSteps, double[] yAxisSteps,
									Time currentTime) {
			super(exchangeItem2dId, Role.InOut, new double[xAxisSteps.length*yAxisSteps.length]);
			this.modelInstanceNumber = modelInstanceNumber;
			this.currentTime = currentTime;
		}
		
		public double[] getTimes() {
			return new double[]{this.currentTime.getMJD()};
		}
		
		public String toString() {
			return this.getId() + "-" + String.valueOf(modelInstanceNumber) + "-" +
					new Vector(getValuesAsDoubles()).toString();
		}
	}

	private class DummmyExchangeItemTS extends DoubleExchangeItem{

		private int modelInstanceNumber;
		private Time currentTime;

		public DummmyExchangeItemTS(String exchangeItem2dId, int modelInstanceNumber,
									Time currentTime) {
			super(exchangeItem2dId, 0);
			this.modelInstanceNumber = modelInstanceNumber;
			this.currentTime = currentTime;
		}

		public double[] getTimes() {
			return new double[]{this.currentTime.getMJD()};
		}

		public String toString() {
			return this.getId() + "-" + String.valueOf(modelInstanceNumber) + "-" +
					String.valueOf(getValuesAsDoubles()[0]);
		}
	}
}
