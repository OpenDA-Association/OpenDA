package org.openda.externalsocket;

import org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType;
import org.openda.model_external_socket.io.castorgenerated.ExternalModelStochModelFactoryConfigXML;
import org.openda.utils.io.CastorUtils;

import java.io.File;

public class ExternalModelStochModelFactoryConfigReader {

	private final int portNumber;
	private final double[] values;
	private final double[] stdDev;
	private final double[] lowerBounds;
	private final double[] upperBounds;

	public ExternalModelStochModelFactoryConfigReader(File configFile) {
		ExternalModelStochModelFactoryConfigXML castor = (ExternalModelStochModelFactoryConfigXML) CastorUtils.parse(configFile,ExternalModelStochModelFactoryConfigXML.class);
		portNumber = castor.getPortNumber();
		int parameterCount = castor.getParameterCount();
		values = new double[parameterCount];
		stdDev = new double[parameterCount];
		lowerBounds = new double[parameterCount];
		upperBounds = new double[parameterCount];
		for (int i = 0; i < parameterCount; i++) {
			ExternalModelParameterComplexType parameter = castor.getParameter(i);
			values[i] = parameter.getValue();
			stdDev[i] = parameter.getStdDev();
			lowerBounds[i] = parameter.hasLowerBound() ? parameter.getLowerBound() : Double.NaN;
			upperBounds[i] = parameter.hasUpperBound() ? parameter.getUpperBound() : Double.NaN;
		}
	}

	public int getPortNumber() {
		return portNumber;
	}

	public double[] getValues() {
		return values;
	}

	public double[] getStdDev() {
		return stdDev;
	}

	public double[] getLowerBounds() {
		return lowerBounds;
	}

	public double[] getUpperBounds() {
		return upperBounds;
	}
}
