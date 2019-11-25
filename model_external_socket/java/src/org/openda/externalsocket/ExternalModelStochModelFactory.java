package org.openda.externalsocket;

import org.openda.blackbox.interfaces.IModelFactory;
import org.openda.blackbox.interfaces.ITimeHorizonConsumer;
import org.openda.interfaces.IModelInstance;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.ITime;
import org.openda.utils.Results;

import java.io.File;

public class ExternalModelStochModelFactory implements IModelFactory, ITimeHorizonConsumer {

	private ExternalModelStochModelFactoryConfigReader configReader;

	@Override
	public IModelInstance getInstance(String[] arguments, IStochModelFactory.OutputLevel outputLevel) {
		return new ExternalModelStochModelInstance(configReader.getPortNumber(), configReader.getValues(), configReader.getStdDev(), configReader.getLowerBounds(), configReader.getUpperBounds());
	}

	@Override
	public void finish() {
		throw new RuntimeException("org.openda.externalsocket.ExternalModelStochModelFactory.finish() not implemented yet");

	}

	@Override
	public void setTimeHorizon(ITime timeHorizon) {
		throw new RuntimeException("org.openda.externalsocket.ExternalModelStochModelFactory.setTimeHorizon() not implemented yet");

	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		if (arguments == null || arguments.length < 1) {
			throw new IllegalArgumentException(getClass().getSimpleName()
				+ ": First argument should be: relative bmiModelFactoryConfig file path.");
		}
		File externalModelFactoryConfigFile = new File(workingDir, arguments[0]);
		if (!externalModelFactoryConfigFile.exists()) {
			throw new IllegalArgumentException(getClass().getSimpleName() + ": Cannot find bmiModelFactoryConfig file "
				+ externalModelFactoryConfigFile.getAbsolutePath());
		}

		// read model factory config.
		Results.putMessage(getClass().getSimpleName() + ": reading bmiModelFactory config file "
			+ externalModelFactoryConfigFile.getAbsolutePath());
		configReader = new ExternalModelStochModelFactoryConfigReader(externalModelFactoryConfigFile);
	}
}
