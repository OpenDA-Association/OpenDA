/* MOD_V2.0 
* Copyright (c) 2012 OpenDA Association
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
import java.util.Collection;
import java.util.List;

/**
 * Config info for a noise model in the stoch. model config
 */
public class BBNoiseModelConfig {

	private AliasDefinitions aliasDefinitions;
	private File workingDir;
	private String configFileName;
	private String className;
	private String[] arguments;
	private List<NoiseModelExchangeItemConfig> exchangeItemConfigs;
	private Collection<String> aliasesUsedInClassName;
	private Collection<String> aliasesUsedInConfigFileName;
	private Collection<String> aliasesUsedInArguments;

	public BBNoiseModelConfig(AliasDefinitions aliasDefinitions,
								   File workingDir, String configFileName,
								   String className,
								   String[] arguments,
								   List<NoiseModelExchangeItemConfig> exchangeItemConfigs) {
		this.aliasDefinitions = aliasDefinitions;
		this.workingDir = workingDir;
		this.configFileName = configFileName;
		this.className = className;
		this.arguments = arguments;
		this.exchangeItemConfigs = exchangeItemConfigs;
		this.aliasesUsedInClassName = this.aliasDefinitions.getUsedAliasIds(className);
		this.aliasesUsedInConfigFileName = this.aliasDefinitions.getUsedAliasIds(configFileName);
		aliasesUsedInArguments = this.aliasDefinitions.getUsedAliasIds(arguments);
	}

	public String getClassName() {
		return aliasDefinitions.apply(className, aliasesUsedInClassName);
	}

	public File getWorkingDir() {
		return workingDir;
	}

	public String getConfigFileName() {
		return aliasDefinitions.apply(configFileName, aliasesUsedInConfigFileName);
	}

	public String[] getArguments() {
		return aliasDefinitions.applyToArguments(this.arguments, aliasesUsedInArguments);
	}

	public List<NoiseModelExchangeItemConfig> getExchangeItemConfigs() {
		return exchangeItemConfigs;
	}

	public String[] getArgumentsIncludingConfigFile() {
		String[] argsInConfig = getArguments();
		String[] actualArgs = new String[1 + argsInConfig.length];
		actualArgs[0] = getConfigFileName();
		System.arraycopy(argsInConfig, 0, actualArgs, 1, argsInConfig.length);
		return actualArgs;
	}

}
