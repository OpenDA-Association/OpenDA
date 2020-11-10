package org.openda.externalfile;

import org.openda.blackbox.interfaces.IModelFactory;
import org.openda.blackbox.interfaces.ITimeHorizonConsumer;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochModelPostProcessor;
import org.openda.interfaces.ITime;

import java.io.File;

public class ExternalFileModelFactory implements IModelFactory, ITimeHorizonConsumer, IStochModelFactory {

	private ExternalFileModelFactoryConfigReader configReader;
	private File dummyModelDir;

	public ExternalFileModelFactory() {
		
	}


	//@Override
	public ExternalFileModelInstance getInstance(String[] arguments, IStochModelFactory.OutputLevel outputLevel) {
		return new ExternalFileModelInstance(configReader.getModelParametersFile(), configReader.getModelResultsFile(), dummyModelDir);
	}

	@Override
	public IStochModelInstance getInstance(OutputLevel outputLevel) {
		return new ExternalFileModelInstance(configReader.getModelParametersFile(), configReader.getModelResultsFile(), dummyModelDir);
	}

	@Override
	public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
		throw new RuntimeException("org.openda.fewsworkflow.FewsWorkflowModelFactory.getPostprocessorInstance() not implemented yet");

	}

	//@Override
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
		File fewsWorkflowModelFactoryConfigFile = new File(workingDir, arguments[0]);
		if (!fewsWorkflowModelFactoryConfigFile.exists()) {
			throw new IllegalArgumentException(getClass().getSimpleName() + ": Cannot find fewsWorkflowModelFactoryConfig file "
				+ fewsWorkflowModelFactoryConfigFile.getAbsolutePath());
		}

		configReader = new ExternalFileModelFactoryConfigReader(fewsWorkflowModelFactoryConfigFile);
	}
}
