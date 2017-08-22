/* OpenDA v2.4.1 
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

package org.openda.blackbox.config;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;

/**
 * TODO: description
 */
public class BBAction {

    private File configDir;
    private String exePath;
    private String fullExePath;
    private String className;
    private String[] arguments;
    private ArrayList<BBCheckOutput> checkOutputs;
	private BBCheckReturnStatus checkReturnStatus;
    private boolean ignoreReturnStatus;
    private AliasDefinitions aliasDefinitions;
    private Collection<String> aliasesUsedInClassName;
    private Collection<String> aliasesUsedInExeName;
    private Collection<String> aliasesUsedInArguments;
    private String actualWorkingDirectory;
	private Collection<String> aliasesUsedInActualWorkingDirectory;

	public BBAction(File configDir, String exePath, String className, String[] arguments,
                    String actualWorkingDirectory,
                    ArrayList<BBCheckOutput> checkOutputs, BBCheckReturnStatus checkReturnStatus, boolean ignoreReturnStatus,
                    AliasDefinitions aliasDefinitions) {
        this.configDir = configDir;
        this.exePath = exePath;
        this.fullExePath = null;
        this.className = className;
        this.arguments = arguments;
        this.checkOutputs = checkOutputs;
		this.checkReturnStatus = checkReturnStatus;
        this.ignoreReturnStatus = ignoreReturnStatus;
        this.aliasDefinitions = (aliasDefinitions != null) ? aliasDefinitions : new AliasDefinitions();
		aliasesUsedInActualWorkingDirectory = this.aliasDefinitions.getUsedAliasIds(actualWorkingDirectory);
        aliasesUsedInClassName = this.aliasDefinitions.getUsedAliasIds(className);
        aliasesUsedInExeName = this.aliasDefinitions.getUsedAliasIds(exePath);
        aliasesUsedInArguments = this.aliasDefinitions.getUsedAliasIds(arguments);
        this.actualWorkingDirectory = actualWorkingDirectory;
    }

    public String getClassName() {
        return className;
    }

    public void validate() {
        if (exePath!= null) fullExePath = BBUtils.determineExe(configDir, aliasDefinitions.apply(exePath, aliasesUsedInExeName));
        if (className !=null)  BBUtils.validateClass(className) ;
    }

    public String[] getArguments() {
		return aliasDefinitions.applyToArguments(this.arguments, aliasesUsedInArguments);
    }

    public void run(File instanceFileOrDir) {
    	this.run(instanceFileOrDir,this.aliasDefinitions);
    }
    
    public void run(File instanceFileOrDir,AliasDefinitions localAliasDefinitions) {
		Object returnValueAsObject = null;
        if (className != null) {
            try {
            	String[] argumentsStrings=localAliasDefinitions.applyToArguments(this.arguments, aliasesUsedInArguments);
                returnValueAsObject = BBUtils.runJavaClass(localAliasDefinitions.apply(className, aliasesUsedInClassName), instanceFileOrDir, argumentsStrings);
            } catch (Exception e) {
                throw new RuntimeException("Error running class " + className +
                        " on " + instanceFileOrDir.getAbsolutePath() + ": " + e.getMessage(), e);
            }
        } else {
            if (fullExePath == null) {
                // full exe path could not been constructed for config dir (see validate function)
                // check if it can be constructed from instance dir
                fullExePath = BBUtils.determineExe(instanceFileOrDir, localAliasDefinitions.apply(exePath, aliasesUsedInExeName));
            }
            try {
                File actualRunDir = instanceFileOrDir;
                if (actualWorkingDirectory != null) {
                    actualRunDir = new File(instanceFileOrDir,
                    		localAliasDefinitions.apply(actualWorkingDirectory, aliasesUsedInActualWorkingDirectory));
                    if (!actualRunDir.exists()) {
                        throw new RuntimeException("Error running executable, actual run dir. " + actualRunDir +
                                actualRunDir.getAbsolutePath() + " does not exist");
                    }
                }
             	String[] argumentsStrings=localAliasDefinitions.applyToArguments(this.arguments, aliasesUsedInArguments);
                returnValueAsObject = BBUtils.runExecutable(fullExePath, actualRunDir, argumentsStrings);
            } catch (Exception e) {
                throw new RuntimeException("Error running executable " + fullExePath +
                        " on " + instanceFileOrDir.getAbsolutePath() + ": " + e.getMessage(), e);
            }
        }
        for (BBCheckOutput checkOutput : checkOutputs) {
            checkOutput.performCheck(instanceFileOrDir);
        }

        if (checkReturnStatus != null) {
            checkReturnStatus.performCheck(returnValueAsObject);
        }

    }

    public String toString() {
        String string = "\tcommand:" + aliasDefinitions.apply(exePath, aliasesUsedInExeName) + "\targs:";
        for (String argument : arguments) {
            string += " " + aliasDefinitions.apply(argument, aliasesUsedInArguments);
        }
        string += "\n\tchecks:\n";
        for (BBCheckOutput checkOutput : checkOutputs) {
            string += "\t\t" + checkOutput.toString() + "\n";
        }
        string += "\treturn value will be " + (ignoreReturnStatus ? "IGNORED" : "checked");
        return string;
    }

    public void setArgument(int argumentIndex, String argumentString) {
        if (argumentIndex < 0 || argumentIndex >= arguments.length ) {
            throw new RuntimeException("Error setting argument, argument (=" + argumentIndex +
                    ") is out of range (must be between 0 and " + argumentString + ")");
        }
        arguments[argumentIndex] = argumentString;
    }
}
