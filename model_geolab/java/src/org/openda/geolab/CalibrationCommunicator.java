package org.openda.geolab;

import org.openda.interfaces.IVector;
import org.openda.utils.Vector;

class CalibrationCommunicator {

	private Vector modelResults = null;
	private Vector parameterVector = null;
	private ExitStatus exitStatus;

	private final int sleepTimeInMillis = 100;

	private String errorString = "(no errors set)";

	Vector getModelResults() {
		while (modelResults == null) {
			try {
				Thread.sleep(sleepTimeInMillis);
			} catch (InterruptedException e) {
				throw new RuntimeException("Thread that runs the CalLibStochModelInstance has been interrupted");
			}
		}
		return modelResults;
	}

	void setModelResults(Vector vector) {
		modelResults = vector;
	}

	void setParameters(IVector parameters) {
		parameterVector = new Vector(parameters.getValues());
	}

	Vector getParameters() {
		while (parameterVector == null && exitStatus == ExitStatus.RUNNING) {
			try {
				Thread.sleep(sleepTimeInMillis);
			} catch (InterruptedException e) {
				throw new RuntimeException("Thread that runs the CalLibStochModelInstance has been interrupted");
			}
		}
		if (exitStatus == ExitStatus.DONE || exitStatus == ExitStatus.ERROR || parameterVector == null) return null;

		return parameterVector;
	}

	void setExitStatus(ExitStatus exitStatus) {
		this.exitStatus = exitStatus;
	}

	void setErrorString(String errorString) {
		this.errorString = errorString;
	}

	String getErrorString() {
		return errorString;
	}
}
