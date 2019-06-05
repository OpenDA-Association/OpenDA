/*
 * Copyright (c) 2019 OpenDA Association
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
 * Configuration for creating a selector.
 */
public class BBConfigurable {

    private String className;
    private String[] arguments;
    private AliasDefinitions aliasDefinitions;
    private Collection<String> aliasesUsedInArguments;

    public BBConfigurable(String className, String[] arguments, AliasDefinitions aliasDefinitions) {
        this.className = className;
        this.arguments = arguments;
        this.aliasDefinitions = aliasDefinitions;
        aliasesUsedInArguments = aliasDefinitions.getUsedAliasIds(arguments);
    }

    public String getClassName() {
        return className;
    }

    public String[] getArguments() {
		return aliasDefinitions.applyToArguments(this.arguments, aliasesUsedInArguments);
	}
}
