package org.openda.externalsocket;

import org.openda.exchange.DoubleExchangeItem;
import org.openda.interfaces.*;
import org.openda.observationOperators.ObservationOperatorDeprecatedModel;
import org.openda.utils.StochVector;
import org.openda.utils.Time;
import org.openda.utils.Vector;

import java.io.File;
import java.util.LinkedHashMap;

public class ExternalSocketModelInstance implements IStochModelInstance, IStochModelInstanceDeprecated, Cloneable {

	public static final String EXTERNAL_SOCKET_RESULT = "ExternalSocketResult";
	public static final String EXTERNAL_SOCKET_PARAMETER = "ExternalSocketParameter";
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
			String resultId = EXTERNAL_SOCKET_RESULT + "_" + i;
			DoubleExchangeItem resultEI = new DoubleExchangeItem(resultId, i * 100);
			exchangeItems.put(resultId, resultEI);
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

	}

	public IVector getStateScaling() {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getStateScaling() not implemented yet");

	}

	public IVector[] getStateScaling(IObservationDescriptions observationDescriptions) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getStateScaling() not implemented yet");

	}

	public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
		return exchangeItems.get(exchangeItemID);
	}

	public ITime getTimeHorizon() {
		return fakeTime;
	}

	public ITime getCurrentTime() {
		return fakeTime;
	}

	public void compute(ITime targetTime) {

		/*final int port = 8124;
		Runnable socketServerRunnable = new Runnable() {
			@Override
			public void run() {
				SocketServer socketServer = new SocketServer(port);
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
			String paramId = EXTERNAL_SOCKET_PARAMETER + "_" + i;
			DoubleExchangeItem paramEI = exchangeItems.get(paramId);
			stringBuilder.append(paramEI.getValue());
			stringBuilder.append(';');
		}
		String messageIn = stringBuilder.toString();
		System.out.println("Sending message: " + messageIn);
		String received = socketClient.sendAndReceive(messageIn);
		System.out.println("Received" + received);
		String[] split = received.split(";");
		double[] receivedValues = new double[split.length];
		for (int i = 0; i < split.length; i++) {
			double parsedModelResult = Double.parseDouble(split[i]);
			receivedValues[i] = parsedModelResult;
			String resultId = EXTERNAL_SOCKET_RESULT + "_" + i;
			exchangeItems.get(resultId).setValuesAsDoubles(new double[]{parsedModelResult});
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

	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getExchangeItemIDs() not implemented yet");

	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return exchangeItems.get(exchangeItemID);

	}

	public void finish() {
		// TODO EP socket client send final parameters

	}

	public void initialize(File workingDir, String[] arguments) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.initialize() not implemented yet");

	}

	public IInstance getParent() {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getParent() not implemented yet");

	}

	public IVector getObservedValues(IObservationDescriptions observationDescriptions) {
		SocketClient socketClient = new SocketClient(portNumber);
		int size = parameterVector.getSize();
		StringBuilder stringBuilder = new StringBuilder(10);
		stringBuilder.append("Z:");
		for (int i = 0; i < size; i++) {
			stringBuilder.append(parameterVector.getValue(i));
			stringBuilder.append(';');
		}
		String received = socketClient.sendAndReceive(stringBuilder.toString());
		String[] split = received.split(";");
		double[] receivedValues = new double[split.length];
		for (int i = 0; i < split.length; i++) {
			double parsedModelResult = Double.parseDouble(split[i]);
			receivedValues[i] = parsedModelResult;
			String resultId = EXTERNAL_SOCKET_RESULT + "_" + i;
			exchangeItems.get(resultId).setValuesAsDoubles(new double[]{parsedModelResult});
		}
		modelResults = new Vector(receivedValues);
		return modelResults;
	}

	public void sendFinalParameters() {
		// Used for testing
		/*final int port = 8124;
		Runnable socketServerRunnable = new Runnable() {
			@Override
			public void run() {
				SocketServer socketServer = new SocketServer(port);
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
			String paramId = EXTERNAL_SOCKET_PARAMETER + "_" + i;
			DoubleExchangeItem paramEI = exchangeItems.get(paramId);
			stringBuilder.append(paramEI.getValue());
			stringBuilder.append(';');
		}
		String messageIn = stringBuilder.toString();
		System.out.println("Sending message: " + messageIn);
		String received = socketClient.sendAndReceive(messageIn);
		System.out.println("Received" + received);

	}
}
