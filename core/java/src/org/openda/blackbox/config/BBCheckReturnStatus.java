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

import java.util.Collection;

/**
 * Checks return value after BBAction has been performed
*/
public class BBCheckReturnStatus {
	private final String expect;
	private final AliasDefinitions aliasDefinitions;
	private Collection<String> aliasesUsedInExpect;

	public BBCheckReturnStatus(String expect, AliasDefinitions aliasDefinitions) {
		this.expect = expect;
		this.aliasDefinitions = aliasDefinitions;
		this.aliasesUsedInExpect = aliasDefinitions.getUsedAliasIds(expect);
	}

	public void performCheck(Object obj) {
		if (expect != null) {
			String expect = this.expect;
			String returnString = aliasDefinitions.apply(expect, aliasesUsedInExpect);
			if (obj == null) {
				throw new IllegalStateException("CheckReturnStatus failed: action has no return value.");
			}
			boolean success = obj.toString().equals(returnString);
			if (!success) {
				throw new IllegalStateException("CheckReturnStatus failed: return value not equal to '"+ returnString + "'" );
			}
		}
	}
}
