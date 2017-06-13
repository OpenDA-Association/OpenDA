/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
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
package org.openda.utils;
import org.openda.interfaces.IAlgorithm;
import org.openda.interfaces.IConfigurable;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochObserver;

import java.io.File;

/**
 * Functions for creating / checking object instances
 */
public class ObjectSupport {

    public static void checkCompatibility(Object objectInstance, Class expectedType, String messagePrefix) {
        if (!expectedType.isInstance(objectInstance)) {
            String prefix = messagePrefix != null ? messagePrefix + ": " : "";
            throw new RuntimeException(prefix + "Object instance of type " + objectInstance.getClass().getName()
                    + " is not assignable to " + (expectedType.isInterface() ? "interface" : "class") + expectedType.getName());
        }
    }

    public static Object createNewInstance(String className) {
        // Create instance of class
        final Class javaClass;
        Object object;
        try {
            javaClass = Class.forName(className);
            object = javaClass.newInstance();
        } catch (Exception e) {
            throw new RuntimeException("Could not create instance for " + className + ": " + e.getMessage());
        }
        return object;
    }

    public static Object createNewInstance(String className, Class expectedType) {
        // Create instance of class and check it
        Object object = createNewInstance(className);
        checkCompatibility(object, expectedType, null);
        return object;
    }

    public static IConfigurable createConfigurable(String messagePrefix, OpenDaComponentConfig openDaComponentConfig) {
        return createConfigurable(messagePrefix, openDaComponentConfig.getClassName(),
                openDaComponentConfig.getWorkingDir(), openDaComponentConfig.getArguments());
    }

    public static IConfigurable createConfigurable(String messagePrefix, String className, Class expectedType, File workingDir, String[] arguments) {
        String messageString = messagePrefix + "\n\tclassName: " + className +
                "\n\tdir.: " + workingDir + (arguments != null ? "\n\tconfig.: " + arguments[0] : "");
        Object instance = createNewInstance(className);
        checkCompatibility(instance, expectedType, messageString);
        IConfigurable configurable = (IConfigurable) instance;
        configurable.initialize(workingDir, arguments);
        return configurable;
    }

    public static IConfigurable createConfigurable(String messagePrefix, String className, File workingDir, String[] arguments) {
        String messageString = messagePrefix + "\n\tclassName: " + className +
                "\n\tdir.: " + workingDir + (arguments != null ? "\n\tconfig.: " + arguments[0] : "");
        Object instance = createNewInstance(className);
        checkCompatibility(instance, IConfigurable.class, messageString);
        IConfigurable configurable = (IConfigurable) instance;
        configurable.initialize(workingDir, arguments);
        return configurable;
    }

    public static IAlgorithm createAlgorithm(OpenDaComponentConfig algorithmConfiguration) {
		String algorithmString = "Algorithm: " +
		"\n\tclassName: " + algorithmConfiguration.getClassName() +
		"\n\tdir.: " + algorithmConfiguration.getWorkingDir() +
		"\n\tconfig.: " + algorithmConfiguration.getArguments()[0];
		Results.putProgression("Starting " + algorithmString);
		IAlgorithm algorithm;
		Object runInstance;
		try {
			Class aClass = Class.forName(algorithmConfiguration.getClassName());
			runInstance = aClass.newInstance();
		} catch (ClassNotFoundException e) {
			throw new RuntimeException("ClassNotFoundException creating " + algorithmString);
		} catch (InstantiationException e) {
			throw new RuntimeException("InstantiationException creating " + algorithmString);
		} catch (IllegalAccessException e) {
			throw new RuntimeException("IllegalAccessException creating " + algorithmString);
		}
		if (!(runInstance instanceof IAlgorithm)) {
			throw new RuntimeException(algorithmString + " is not implementing the Algorithm interface");
		}
		algorithm = (IAlgorithm)runInstance;
        algorithm.initialize(algorithmConfiguration.getWorkingDir(), algorithmConfiguration.getArguments());
        return algorithm;
	}
}
