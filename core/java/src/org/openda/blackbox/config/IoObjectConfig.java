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

package org.openda.blackbox.config;

import java.util.Collection;

/**
 * Object for storage of the data needed to initialize or reinitialize an IoObject.
 * Now that aliases have become more dynamic, it has become necessary to pass the aliases as
 * an argument instead of storing them with this config-object. 
 */
public class IoObjectConfig {

    private String id;
    private String className;
    private String fileName;
    private String[] arguments;
    //private AliasDefinitions aliasDefinitions;
    private Collection<String> aliasesUsedInId;
    private Collection<String> aliasesUsedInClassName;
    private Collection<String> aliasesUsedInFileName;
    private Collection<String> aliasesUsedInArguments;

    public IoObjectConfig(String id, String className, String fileName, AliasDefinitions aliasDefinitions, String[] arguments) {
        this.id = id;
        this.className = className;
        this.fileName = fileName;
        this.arguments = arguments;
        //this.aliasDefinitions = aliasDefinitions;
        this.aliasesUsedInId = aliasDefinitions.getUsedAliasIds(id);
        this.aliasesUsedInClassName = aliasDefinitions.getUsedAliasIds(className);
        this.aliasesUsedInFileName = aliasDefinitions.getUsedAliasIds(fileName);
        this.aliasesUsedInArguments = aliasDefinitions.getUsedAliasIds(arguments);
    }

    public String getId(AliasDefinitions aliasDefinitions) {
        return aliasDefinitions.apply(id, aliasesUsedInId);
    }

    public String getClassName(AliasDefinitions aliasDefinitions) {
        return aliasDefinitions.apply(className, aliasesUsedInClassName);
    }

    public String getFileName(AliasDefinitions aliasDefinitions) {
        return aliasDefinitions.apply(fileName, aliasesUsedInFileName);
    }

    public String[] getArguments(AliasDefinitions aliasDefinitions) {
		return aliasDefinitions.applyToArguments(this.arguments, aliasesUsedInArguments);
	}
}
