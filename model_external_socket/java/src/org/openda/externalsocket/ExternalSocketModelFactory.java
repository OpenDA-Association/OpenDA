package org.openda.externalsocket;

import org.openda.blackbox.interfaces.IModelFactory;
import org.openda.blackbox.interfaces.ITimeHorizonConsumer;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelPostProcessor;
import org.openda.interfaces.ITime;

import java.io.File;

public class ExternalSocketModelFactory implements IModelFactory, ITimeHorizonConsumer, IStochModelFactory {

	private ExternalSocketModelFactoryConfigReader configReader;
	private File dummyModelDir;

	@Override
	public ExternalSocketModelInstance getInstance(OutputLevel outputLevel) {
		return new ExternalSocketModelInstance(configReader.getPortNumber(), configReader.getValues(), configReader.getStdDev(), configReader.getLowerBounds(), configReader.getUpperBounds(), dummyModelDir);
	}

	@Override
	public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelFactory.getPostprocessorInstance() not implemented yet");

	}

	@Override
	public ExternalSocketModelInstance getInstance(String[] arguments, IStochModelFactory.OutputLevel outputLevel) {
		return new ExternalSocketModelInstance(configReader.getPortNumber(), configReader.getValues(), configReader.getStdDev(), configReader.getLowerBounds(), configReader.getUpperBounds(), dummyModelDir);
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
		dummyModelDir = new File(workingDir, "dummyModelDir");
		File externalModelFactoryConfigFile = new File(workingDir, arguments[0]);
		if (!externalModelFactoryConfigFile.exists()) {
			throw new IllegalArgumentException(getClass().getSimpleName() + ": Cannot find bmiModelFactoryConfig file "
				+ externalModelFactoryConfigFile.getAbsolutePath());
		}

		configReader = new ExternalSocketModelFactoryConfigReader(externalModelFactoryConfigFile);
	}
}
