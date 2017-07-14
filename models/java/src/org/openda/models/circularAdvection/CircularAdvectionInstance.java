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

package org.openda.models.circularAdvection;

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

import java.io.*;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import static java.lang.Math.abs;
import static java.lang.Math.pow;
import static java.lang.Math.sqrt;

/**
 * Created by strube on 8-6-15.
 */
public class CircularAdvectionInstance implements IStochModelInstance, IStochModelInstanceDeprecated {


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
	boolean twoVars = true;
	boolean periodic = false;        // Use adapted localisation matrix for periodic relations

	private static Random generator;


	public IVector getState() {

		initializeMe();

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
		initializeMe();
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
		//initializeMe();
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
		initializeMe();

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
		initializeMe();
		return new Time(this.startTime, this.endTime, this.timeStep);
	}

	@Override
	public ITime getCurrentTime() {
		initializeMe();
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
		double valuesOld[]; 	// working variable for previous values

		initializeMe();

		valuesOld = this.state.getValues();

		int nStep = (int) Math.floor(targetTime.getMJD() - this.timeNow + 0.5);
		for (int iStep = 0; iStep < nStep; iStep++) {
			valuesNewA[iStep] = valuesOld[this.varSize - nStep + iStep];            // a[0]
			if (twoVars) {
				valuesNewB[iStep] = valuesOld[2 * this.varSize - nStep + iStep];	// b[0]
			}
		}

		for (int i = nStep; i < this.varSize; i++) {
			valuesNewA[i] = valuesOld[i - nStep];
			if (twoVars) {
				valuesNewB[i] = valuesOld[this.varSize + i - nStep];
			}
		}

		TreeVector zwerg = new TreeVector("state");
		zwerg.addChild("a", valuesNewA);
		if (twoVars) {
			zwerg.addChild("b", valuesNewB);
		}

		this.state = zwerg;
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

		initializeMe();

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
			if (twoVars) {
				subvector.addChild("b", mask);
			}

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


		nEns += 1;		// actually nEns+1 at the end, since main model also in the counting
		ikBenMember = nEns - 1;

		// Read the reference field

		if (arguments.length < 2) {
			throw new RuntimeException("Circular advection needs reference field as base for the ensemble setup.");
		}

		try {
			File file = new File(workingDir, arguments[1]);
			if (!file.exists()) {
				throw new FileNotFoundException("File does not exist: " + file);
			}
			if (!file.isFile()) {
				throw new IllegalArgumentException("Should be a file: " + file);
			}
			FileReader fileReader = new FileReader(file);
			BufferedReader buff = new BufferedReader(fileReader);

			// read header
			String line = buff.readLine();
			// Split first line
			String[] labelStrings = line.split(",");
			int noKeys = labelStrings.length;
			String[] keys = labelStrings;

			// extract labels
			java.util.Vector<String> labelVector = new java.util.Vector<String>();
			for (int i = 0; i < noKeys; i++) {
				labelStrings[i] = labelStrings[i].trim();
				labelVector.add(labelStrings[i]);
			}

			//
			// read content
			//
			java.util.Vector<Double[]> tempValues = new java.util.Vector<Double[]>();
			boolean eof = false;
			while (!eof) {
				line = buff.readLine();
				if (line == null)
					eof = true;
				else {
					//System.out.println(line);
					// parse values
					String[] valueStrings = line.split(",");
					Double[] tempRow = new Double[noKeys];
					for (int j = 0; j < noKeys; j++) {
						tempRow[j] = Double.parseDouble(valueStrings[j]);
					}
					tempValues.add(tempRow);
				}
			}

			//
			// copy data to this.values
			//
			int noRefValues = tempValues.size();
			Vector[] refValues = new Vector[noKeys];
			for (int i = 0; i < noKeys; i++) {
				refValues[i] = new Vector(noRefValues);
			}
			for (int i = 0; i < noRefValues; i++) {
				Double[] tempRow = tempValues.get(i);
				for (int j = 0; j < noKeys; j++) {
					refValues[j].setValue(i, tempRow[j]);
				}
			}
			this.refFieldA = refValues[1];
			this.refFieldB = refValues[2];
			buff.close();


		} catch (IOException e) {
			System.out.println("Error -- " + e.toString());
			throw new RuntimeException("Cannot read reference field in CircularAdvectionInstance from file");
		}

		// Which ensemble member? (Make sure to use a different set of random numbers for each member, but the same random set for one member in each run)
		this.mynum = Integer.parseInt(arguments[0]);
		generator = new Random(20150624 + mynum);

		this.startTime = 0.0;
		this.endTime = 200.0;
		this.timeStep = 1.0;

		this.timeNow = 0.0;
		//this.timeStepCounter = 0;

		this.varSize = 100;
		this.state = new TreeVector("state");

		this.N = 6;    // sines k=0,1,...5

		this.amplitude = new double[this.N];        // sine amplitudes as in paper
		this.phase = new double[this.N];            // sine phases as in paper

		double[] valuesA = new double[this.varSize];    // two variables of each 100(0) grid values (first a, then b)
		double[] valuesB = new double[this.varSize];

		// parameters for variable a
		for (int k = 0; k < N; k++) {
			double ampl = generator.nextDouble();                // 0 <= A_k < 1
			this.amplitude[k] = ampl;
			double ph = generator.nextDouble();    // 0 <= p_k < 2*pi
			this.phase[k] = 2 * Math.PI * ph;
		}

		// initialisation for variable a (as superposition of sines with reference field as background)
		double sumA = 0.0;
		for (int i = 0; i < this.varSize; i++) {
			double help = 0.0;
			for (int k = 0; k < N; k++) {
				help += this.amplitude[k] * Math.sin((2 * Math.PI * k * i / this.varSize) + this.phase[k]);
			}
			valuesA[i] = help;
			sumA += valuesA[i];
		}

		// Remove mean and set variance to 1 --> Normalise a
		double meanA = sumA / varSize;
		double varA = 0.0;
		for (int i = 0; i < this.varSize; i++) {
			valuesA[i] -= meanA;
			varA += (valuesA[i] * valuesA[i]);
		}
		varA /= (this.varSize - 1);

		for (int i = 0; i < this.varSize; i++) {
			valuesA[i] /= sqrt(varA);
		}


		if (twoVars) {
			for (int i = 1; i < this.varSize - 1; i++) {
				valuesB[i] = 0.5 * (valuesA[i + 1] - valuesA[i - 1]);
			}

			valuesB[0] = 0.5 * (valuesA[1] - valuesA[this.varSize - 1]);
			valuesB[varSize - 1] = 0.5 * (valuesA[0] - valuesA[varSize - 2]);
		}

		state.addChild("a", valuesA);
		if (twoVars) {
			state.addChild("b", valuesB);
		}

		stateNoFix[ikBenMember] = state.clone();

	}

	@Override
	public IInstance getParent() {
		throw new RuntimeException("not implemented");
	}


	private void initializeMe() {

		if (notInit) {
			notInit = false;
			if (mynum > 0.5) {        // For ensembles, ensemble mean is removed ... (leaving main model declaration out)
				double[] sumA = new double[varSize];
				double[] sumB = new double[varSize];

				//Calculate ensemble means for variable A and B (at each location)

				for (int i = 0; i < varSize; i++) {
					sumA[i] = 0.0;
					sumB[i] = 0.0;
					for (int q = 1; q < nEns; q++) {			// start counting with the first ensemble member; member 0 is the main model
						sumA[i] += stateNoFix[q].getValue(i);
						sumB[i] += stateNoFix[q].getValue(varSize + i);
					}
				}

				double[] meanA = new double[varSize];
				double[] meanB = new double[varSize];
				for (int i = 0; i < sumA.length; i++) {
					meanA[i] = sumA[i] / (nEns-1);
					meanB[i] = sumB[i] / (nEns-1);
				}
				//Remove ensemble means
				this.state.getSubTreeVector("a").axpy(-1.0, new Vector(meanA));
				this.state.getSubTreeVector("b").axpy(-1.0, new Vector(meanB));
			}

			//Add reference field

			this.state.getSubTreeVector("a").axpy(1.0, refFieldA);

			if (twoVars) {
				this.state.getSubTreeVector("b").axpy(1.0, refFieldB);
			}
		}
	}
}

