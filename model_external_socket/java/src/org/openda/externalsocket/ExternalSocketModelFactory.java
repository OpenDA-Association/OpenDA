/*
* Copyright (c) 2023 OpenDA Association 
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
