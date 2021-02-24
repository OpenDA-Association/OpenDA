/*
* Copyright (c) 2021 OpenDA Association 
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
package org.openda.externalsocket;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IInstance;
import org.openda.interfaces.ILocalizationDomains;
import org.openda.interfaces.IModelState;
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IObservationOperator;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochModelInstanceDeprecated;
import org.openda.interfaces.IStochVector;
import org.openda.interfaces.ITime;
import org.openda.interfaces.IVector;
import org.openda.observationOperators.ObservationOperatorDeprecatedModel;
import org.openda.utils.StochVector;
import org.openda.utils.Time;
import org.openda.utils.Vector;

import java.io.File;
import java.util.LinkedHashMap;

public class ExternalSocketModelInstance implements IStochModelInstance, IStochModelInstanceDeprecated, Cloneable {

	private static final String EXTERNAL_SOCKET_PARAMETER = "ExternalSocketParameter";
	private int portNumber;
	private Vector parameterVector;
	private Vector modelResults;
	private final double[] lowerBounds;
	private final double[] upperBounds;
	private File dummyModelDir;
	private final StochVector parameterUncertainties;
	private LinkedHashMap<String, DoubleExchangeItem> exchangeItems = new LinkedHashMap<>();
	private Time fakeTime = new Time(58119,58120,1d/24d);

	public ExternalSocketModelInstance(int portNumber, double[] parameterValues, double[] standardDeviations, double[] lowerBounds, double[] upperBounds, File dummyModelDir) {
		this.portNumber = portNumber;
		this.parameterVector = new Vector(parameterValues);
		this.lowerBounds = lowerBounds;
		this.upperBounds = upperBounds;
		this.dummyModelDir = dummyModelDir;
		this.parameterUncertainties = new StochVector(parameterValues, standardDeviations);
		for (int i = 0; i < parameterValues.length; i++) {
			String paramId = EXTERNAL_SOCKET_PARAMETER + "_" + i;
			DoubleExchangeItem paramEI = new DoubleExchangeItem(paramId, parameterValues[i]);
			exchangeItems.put(paramId, paramEI);
		}
	}

	public IVector getState() {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getState() not implemented yet");

	}

	public IVector getState(int iDomain) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getState() not implemented yet");

	}

	public void axpyOnState(double alpha, IVector vector) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.axpyOnState() not implemented yet");

	}

	public void axpyOnState(double alpha, IVector vector, int iDomain) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.axpyOnState() not implemented yet");

	}

	public IVector getParameters() {
		if (parameterVector == null) {
			throw new RuntimeException("CalLibStochModelInstance.getParameters(): initialParameterVector == null");
		}
		return parameterVector;
	}

	public void setParameters(IVector parameters) {
		parameterVector = new Vector(parameters.getValues());
	}

	public void axpyOnParameters(double alpha, IVector vector) {
		Vector parameterVector = new Vector(this.parameterVector.getValues());
		parameterVector.axpy(alpha, vector);
		this.parameterVector = parameterVector.clone();
	}

	public IStochVector getStateUncertainty() {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getStateUncertainty() not implemented yet");

	}

	public IStochVector getParameterUncertainty() {
		return parameterUncertainties;
	}

	public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getWhiteNoiseUncertainty() not implemented yet");

	}

	public boolean isWhiteNoiseStationary() {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.isWhiteNoiseStationary() not implemented yet");

	}

	public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getWhiteNoiseTimes() not implemented yet");

	}

	public IVector[] getWhiteNoise(ITime timeSpan) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getWhiteNoise() not implemented yet");

	}

	public void setWhiteNoise(IVector[] whiteNoise) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.setWhiteNoise() not implemented yet");

	}

	public void axpyOnWhiteNoise(double alpha, IVector[] vector) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.axpyOnWhiteNoise() not implemented yet");

	}

	public void setAutomaticNoiseGeneration(boolean value) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.setAutomaticNoiseGeneration() not implemented yet");

	}

	public IObservationOperator getObservationOperator() {
		return new ObservationOperatorDeprecatedModel(this);

	}

	public void announceObservedValues(IObservationDescriptions observationDescriptions) {
		// Not used
	}

	public IVector getStateScaling() {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getStateScaling() not implemented yet");

	}

	public IVector[] getStateScaling(IObservationDescriptions observationDescriptions) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getStateScaling() not implemented yet");

	}

	public IExchangeItem getExchangeItem(String exchangeItemID) {
		return exchangeItems.get(exchangeItemID);
	}

	public ITime getTimeHorizon() {
		return fakeTime;
	}

	public ITime getCurrentTime() {
		return fakeTime;
	}

	public void compute(ITime targetTime) {

/*		Runnable socketServerRunnable = new Runnable() {
			@Override
			public void run() {
				SocketServer socketServer = new SocketServer(portNumber);
				socketServer.runAndWaitForMessage();
			}
		};
		Thread thread = new Thread(socketServerRunnable);
		thread.start();*/

		SocketClient socketClient = new SocketClient(portNumber);
		int size = parameterVector.getSize();
		StringBuilder stringBuilder = new StringBuilder(10);
		stringBuilder.append("X:");
		for (int i = 0; i < size; i++) {
			double value = parameterVector.getValue(i);
			// When comparing with Double.NaN < and > always return false
			if (value < lowerBounds[i]) value = lowerBounds[i];
			if (value > upperBounds[i]) value = upperBounds[i];
			stringBuilder.append(value);
			stringBuilder.append(';');
		}
		String messageIn = stringBuilder.toString();
		System.out.println("Sending message: " + messageIn);
		String received = socketClient.sendAndReceive(messageIn);
		System.out.println("Received" + received);
		String[] split = received.trim().split(";");
		double[] receivedValues = new double[split.length];
		for (int i = 0; i < split.length; i++) {
			double parsedModelResult = Double.parseDouble(split[i]);
			receivedValues[i] = parsedModelResult;
		}
		modelResults = new Vector(receivedValues);
	}

	public ILocalizationDomains getLocalizationDomains() {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getLocalizationDomains() not implemented yet");

	}

	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getObservedLocalization() not implemented yet");

	}

	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance, int iDomain) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getObservedLocalization() not implemented yet");

	}

	public IModelState saveInternalState() {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.saveInternalState() not implemented yet");

	}

	public void restoreInternalState(IModelState savedInternalState) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.restoreInternalState() not implemented yet");

	}

	public void releaseInternalState(IModelState savedInternalState) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.releaseInternalState() not implemented yet");

	}

	public IModelState loadPersistentState(File persistentStateFile) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.loadPersistentState() not implemented yet");

	}

	public File getModelRunDir() {
		return dummyModelDir;
	}

	public String[] getExchangeItemIDs() {
		return exchangeItems.keySet().toArray(new String[0]);
	}

	public String[] getExchangeItemIDs(IExchangeItem.Role role) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getExchangeItemIDs() not implemented yet");

	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return exchangeItems.get(exchangeItemID);

	}

	public void finish() {
		// Not used, sendFinalParameters is used instead
	}

	public void initialize(File workingDir, String[] arguments) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.initialize() not implemented yet");

	}

	public IInstance getParent() {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getParent() not implemented yet");

	}

	public IVector getObservedValues(IObservationDescriptions observationDescriptions) {
		return modelResults;
	}

	void sendFinalParameters(double[] stdValues) {
		// Used for testing
/*		Runnable socketServerRunnable = new Runnable() {
			@Override
			public void run() {
				SocketServer socketServer = new SocketServer(portNumber);
				socketServer.runAndWaitForMessage();
			}
		};
		Thread thread = new Thread(socketServerRunnable);
		thread.start();*/

		SocketClient socketClient = new SocketClient(portNumber);
		int size = parameterVector.getSize();
		StringBuilder stringBuilder = new StringBuilder(10);
		stringBuilder.append("C:");
		for (int i = 0; i < size; i++) {
			stringBuilder.append(parameterVector.getValue(i));
			if (i != size - 1) stringBuilder.append(';');
		}
		if (stdValues != null) {
			stringBuilder.append(':');
			for (int i = 0; i < stdValues.length; i++) {
				stringBuilder.append(stdValues[i]);
				if (i != stdValues.length - 1) stringBuilder.append(';');
			}
		}
		String messageIn = stringBuilder.toString();
		System.out.println("Sending message: " + messageIn);
		socketClient.send(messageIn);
		System.out.println("Message sent " + messageIn);

	}
}
