/* MOD_V1.0
* Copyright (c) 2015 OpenDA Association
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

package org.openda.models.circularAdvection1000;

/**
 *  A 1D linear advection model of two coupled variables as discribed in
 *  Oke et al(2007) "Impacts of localisation in the EnKF and EnOI: experiments with a small model";
 *
 *  The model simulates a simple advection following
 *
 *  da/dt + u da/dx = 0
 *
 *  with 	a ... model variable
 * 			u ... advection speed (default u=1)
 * 			t ... time
 * 			x ... space	(in a grid of 1<=x<=1000)
 *
 * 	The evolution of a second variable b is coupled to the change in a:
 *
 * 	b = 0.5 + 10 * da/dx
 *
 * 	The model is motivated by the general relations between variables (e.g. velocity and pressure) in oceanographic and
 * 	implementations. Oke et al. used the model to depict the effects of localisation in assimilation of variable a and the
 * 	influence on the evolution in b.
 *
 *
 * @Author: C.Strube
 **/


import org.openda.interfaces.*;
import org.openda.observationOperators.ObservationOperatorDeprecatedModel;
import org.openda.observers.TimeSeriesObservationDescriptions;
import org.openda.utils.*;
import sun.reflect.generics.tree.Tree;

import java.io.*;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Random;

import static java.lang.Math.abs;
import static java.lang.Math.pow;
import static java.lang.Math.sqrt;

/**
 * Created by strube on 8-6-15.
 */
public class CircularAdvection1000Instance implements IStochModelInstance, IStochModelInstanceDeprecated {

	static int nEnsembleFromFile=0;
	static double [][] initialEnsembleValues;


	static int nEns = 0;
	static TreeVector[] stateNoFix = new TreeVector[1000];
	boolean notInit = true;
	int ikBenMember = -999;
	int mynum;

	// Time parameters
	double timeNow;
	double endTime;
	double startTime;
	double timeStep;
	//int timeStepCounter;

	// State parameter
	TreeVector state;
	int varSize = 0;

	//initial state parameters
	double[] amplitude;
	double[] phase;
	int N;            // number of joint sines

	// reference field
	IVector refFieldA;            // truth field for setup of ensemble (only variable a given)
	IVector refFieldB;

	boolean autoNoise = false;
	boolean periodic = false;        // Use adapted localisation matrix for periodic relations

	private static Random generator;


	public void readEnsemble(File workingDir, String filename) {

		try {

			File file = new File(workingDir, filename);
			if (!file.exists()) {
				throw new FileNotFoundException("File does not exist: " + file);
			}
			if (!file.isFile()) {
				throw new IllegalArgumentException("Should be a file: " + file);
			}
			FileReader fileReader = new FileReader(file);
			BufferedReader buff = new BufferedReader(fileReader);

			// Read first line to figure out number of ensembles
			for (int i = 0; i < 2000; i++) {
				String line = buff.readLine();
				String[] lineWithValues = line.split(",");

				if (i == 0) {
					this.nEnsembleFromFile = lineWithValues.length;
					this.initialEnsembleValues = new double[this.nEnsembleFromFile][2000];
				}
				for (int iEns = 0; iEns < this.nEnsembleFromFile; iEns++) {
					//NumberFormat format = NumberFormat.getInstance(Locale.US);
					//Number number = format.parse(lineWithValues[iEns]);
					//this.initialEnsembleValues[iEns][i] = number.doubleValue();
					this.initialEnsembleValues[iEns][i] = Double.parseDouble(lineWithValues[iEns]);
				}
			}
		//} catch (ParseException e) {
		//	e.printStackTrace();
		} catch (FileNotFoundException e) {
			throw new RuntimeException("Cannot read the file with initial ensembles");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public IVector getState() {
		return this.state.clone();
	}

	/**
	 * Get the full set of state variables of a single domain from a model.
	 *
	 * @param iDomain The index of the localication domain
	 * @return A Vector containing the values of all state variables in the model.
	 */
	public IVector getState(int iDomain) {
		throw new RuntimeException("not implemented");
	}


	public void axpyOnState(double alpha, IVector vector) {
		this.state.axpy(alpha, vector);
	}

	/**
	 * Peform a <c>state variable += alpha * vector</c> operation on each state variable in the model.
	 *
	 * @param alpha   The <c>alpha</c> in <c>state variable += alpha * vector</c>.
	 * @param vector  A Vector containing the values for the axpy-operation on all state variables in the model.
	 * @param iDomain An integer denoting the domain on which the axpy-operation should be performed.
	 */
	public void axpyOnState(double alpha, IVector vector, int iDomain) {
		throw new RuntimeException("not implemented");
	}


	public IVector getParameters() {
		throw new RuntimeException("not implemented");
		//return null;
	}


	public void setParameters(IVector parameters) {
		throw new RuntimeException("not implemented");
	}


	public void axpyOnParameters(double alpha, IVector vector) {
		throw new RuntimeException("not implemented");
	}


	public IStochVector getStateUncertainty() {
		throw new RuntimeException("not implemented");
		//return null;
	}


	public IStochVector getParameterUncertainty() {
		throw new RuntimeException("not implemented");
		//return null;
	}


	public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
		throw new RuntimeException("not implemented");
		//return new IStochVector[0];
	}


	public boolean isWhiteNoiseStationary() {
		throw new RuntimeException("not implemented");
		//return false;
	}


	public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
		throw new RuntimeException("not implemented");
		//return new ITime[0];
	}


	public IVector[] getWhiteNoise(ITime timeSpan) {
		throw new RuntimeException("not implemented");
		//return new IVector[0];
	}


	public void setWhiteNoise(IVector[] whiteNoise) {
		throw new RuntimeException("not implemented");
	}


	public void axpyOnWhiteNoise(double alpha, IVector[] vector) {
		throw new RuntimeException("not implemented");
	}


	public void setAutomaticNoiseGeneration(boolean value) {
		this.autoNoise = value;
		//throw new RuntimeException("not implemented");
	}

	/**
	 * Get operator H to get model values corresponding to a number of observations
	 *
	 * @return vector with the model values corresponding to each observation given in the descriptions
	 */
	public IObservationOperator getObservationOperator() {
		return new ObservationOperatorDeprecatedModel((IStochModelInstanceDeprecated) this);
	}

	/**
	 * Get model values corresponding to a number of observations
	 *
	 * @param observationDescriptions An ObservationDescriptions object with meta data for the observations
	 * @return vector with the model values corresponding to each observation given in the descriptions
	 */


	public IVector getObservedValues(IObservationDescriptions observationDescriptions) {

		int nObs = observationDescriptions.getObservationCount();
		IVector observedValues = new Vector(nObs);
		IVector obsLoc = observationDescriptions.getValueProperties("location");
		IVector obsType = observationDescriptions.getValueProperties("type");
		if (obsLoc == null) {
			throw new RuntimeException("The observations file has to be setup with the keys 'time', 'location', 'value','std','type'. ");
		}

		/* For now, implement only the use of csv files:
		 * - file with descriptors 	"time"	=	time step associated
		 * 							"location" =	observation location
		 * 							"value"	=	observed value
		 *							"std"	=	estimated standard deviation of observations
		 *							"type"	=	input whether obs is 'a' or 'b' variable
		 */

		int thisLoc = 0;
		double thisValue = 0.0;

		// Only the current state is available
		for (int i = 0; i < nObs; i++) {
			if (obsType.getValue(i) < 1.5) {    // Type a (in csv file referred to as 1.0)
				thisLoc = (int) obsLoc.getValue(i);

				thisValue = this.state.getValue(thisLoc - 1);    // Vector indices from 0 to (varSize-1)
				observedValues.setValue(i, thisValue);
			} else if (obsType.getValue(i) > 1.5) {        // Type b=0.5 + 10*da/dx (in csv file referred to as 2.0)
				thisLoc = (int) obsLoc.getValue(i);

				thisValue = this.state.getValue(varSize + thisLoc - 1);
				observedValues.setValue(i, thisValue);
			}

		}

		return observedValues;
//		throw new RuntimeException("not implemented");
	}


	public void announceObservedValues(IObservationDescriptions observationDescriptions) {

		//throw new RuntimeException("not implemented");
	}


	public IVector getStateScaling() {
		throw new RuntimeException("not implemented");
		//return null;
	}


	public IVector[] getStateScaling(IObservationDescriptions observationDescriptions) {
		throw new RuntimeException("not implemented");
		//return new IVector[0];
	}

	
	public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
		throw new RuntimeException("not implemented");
		//return null;
	}

	/**
	 * Get the computational time horizon of the model (begin and end time).
	 *
	 * @return The time horizon (containing begin and end time).
	 */
	@Override
	public ITime getTimeHorizon() {
		return new Time(this.startTime, this.endTime, this.timeStep);
	}

	@Override
	public ITime getCurrentTime() {
		return new Time(this.timeNow);
	}

	/**
	 * Let the stochastic model instance compute to the requested target time stamp.
	 * This function can not be used to go back in time. Use saveState and restoreState instead.
	 * Use Infinity if the targetTime is unknown
	 *
	 * @param targetTime Time stamp to compute to.
	 */
	public void compute(ITime targetTime) {

		double valuesNewA[] = new double[this.varSize];    // two variables of each 1000 grid values
		double valuesNewB[] = new double[this.varSize];
		double valuesOldA[]; 	// working variable for previous values
		double valuesOldB[];

		valuesOldA = this.state.getSubTreeVector("a").getValues();
		valuesOldB = this.state.getSubTreeVector("b").getValues();

		int nStep = (int) Math.floor(targetTime.getMJD() - this.timeNow + 0.5);
		for (int i=0; i<1000; i++){
			int inew=(i+nStep)%1000;
			valuesNewA[inew]=valuesOldA[i];
			valuesNewB[inew]=valuesOldB[i];

		}

		state.getSubTreeVector("a").setValues(valuesNewA);
		state.getSubTreeVector("b").setValues(valuesNewB);

		this.timeNow = targetTime.getMJD();
	}

	/**
	 * Returns the localization domain based on the observation locations.
	 *
	 * @return Localization domain.
	 */
	public ILocalizationDomains getLocalizationDomains() {
		return null;
	}

	@Override
	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {

		IVector[] observedLocalization = new IVector[observationDescriptions.getObservationCount()];
		IVector obsLoc = observationDescriptions.getValueProperties("location"); //momentan noch die Observationslocation
		double a = Math.sqrt(10 / 3) * distance;        // characteristic length scale for model
		int middle = Math.round(this.varSize / 2);

		for (int k = 0; k < obsLoc.getSize(); k++) {

			double[] mask = new double[this.varSize];
			for (int idx = 0; idx < this.varSize; idx++) {
				// for circular situation a connection between left and right border is necessary --> Build mask always around loc=50 (middle of domain) and shift it afterwards to the genuine position
				double dist;
				if (periodic) {
					dist = Math.abs(middle - idx - 1);    // start at idx 0, while grid starts at 1
				} else {
					dist = Math.abs(obsLoc.getValue(k) - idx - 1);    // start at idx 0, while grid starts at 1
				}
				if (0.0 <= dist && dist <= a) {
					mask[idx] = -0.25 * Math.pow((dist / a), 5) + 0.5 * Math.pow((dist / a), 4) + 5.0 / 8.0 * Math.pow((dist / a), 3) - 5.0 / 3.0 * Math.pow((dist / a), 2) + 1.0;
				} else if (a < dist && dist <= 2.0 * a) {
					mask[idx] = 1 / 12.0 * Math.pow((dist / a), 5) - 0.5 * Math.pow((dist / a), 4) + 5.0 / 8.0 * Math.pow((dist / a), 3) + 5.0 / 3.0 * Math.pow((dist / a), 2) - 5.0 * (dist / a) + 4.0 - 2.0 / 3.0 * (a / dist);
				} else if (2.0 * a < dist) {
					mask[idx] = 0.0;
				} else {
					throw new RuntimeException("There is a problem in the determination of the localisation weights.");
				}

			}

			if (periodic) {
				// Shift to proper position (only works if observation on discretisation grid)
				double[] maskShift = mask.clone();
				for (int i = 0; i < mask.length; i++) {
					int idx = ((int) obsLoc.getValue(k) - middle + i);
					if (idx < 0.0) {
						idx += this.varSize;
					} else if (idx > this.varSize - 1) {
						idx -= this.varSize;
					}
					mask[idx] = maskShift[i];
				}
			}

			TreeVector subvector = new TreeVector("localisation");
			subvector.addChild("a", mask);
			subvector.addChild("b", mask);

			observedLocalization[k] = subvector;
		}

		return observedLocalization;
	}

	/**
	 * Returns the localization weights for each observation location.
	 * This method assumes that there is only one state vector.
	 *
	 * @param observationDescriptions observation description
	 * @param distance                characteristic distance for Cohn's formula
	 * @param iDomain                 Selection of sub-domain
	 * @return weight vector for each observation location.
	 * The size of the returned array must equal the number of observation locations in the given observationDescriptions.
	 * The size of each vector in the returned array must equal the size of the state vector of the implementing modelInstance.
	 */
	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance, int iDomain) {
		throw new RuntimeException("not implemented");
	}

	@Override
	public IModelState saveInternalState() {
		throw new RuntimeException("not implemented");
	}

	@Override
	public void restoreInternalState(IModelState savedInternalState) {
		throw new RuntimeException("not implemented");
	}

	@Override
	public void releaseInternalState(IModelState savedInternalState) {
		throw new RuntimeException("not implemented");
	}

	@Override
	public IModelState loadPersistentState(File persistentStateFile) {
		throw new RuntimeException("not implemented");
	}

	@Override
	public File getModelRunDir() {
		throw new RuntimeException("not implemented");
	}

	@Override
	public String[] getExchangeItemIDs() {
		throw new RuntimeException("not implemented");
	}

	@Override
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		throw new RuntimeException("not implemented");
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		throw new RuntimeException("not implemented");
	}

	@Override
	public void finish() {
		//throw new RuntimeException("not implemented");
	}

	@Override
	public void initialize(File workingDir, String[] arguments) {

		if (arguments.length < 2) {
			throw new RuntimeException("The initialize function needs two arguments (ensemble number and filename to initial ensemble)");
		}

		if (nEnsembleFromFile == 0) {
			readEnsemble(workingDir, arguments[1]);
		}

		// Which ensemble member? (Make sure to use a different set of random numbers for each member, but the same random set for one member in each run)
		this.mynum = Integer.parseInt(arguments[0]);
		this.startTime = 0.0;
		this.endTime = 300.0;
		this.timeStep = 1.0;

		this.timeNow = 0.0;

		this.varSize = 1000;
		this.state = new TreeVector("state");

		double[] valuesA = new double[this.varSize];    // two variables of each 100(0) grid values (first a, then b)
		double[] valuesB = new double[this.varSize];

		for (int i=0; i<this.varSize; i++){
			valuesA[i]=initialEnsembleValues[this.mynum][i];
			valuesB[i]=initialEnsembleValues[this.mynum][i+this.varSize];
		}
		state.addChild("a", valuesA);
		state.addChild("b", valuesB);
	}

	@Override
	public IInstance getParent() {
		throw new RuntimeException("not implemented");
	}
}

