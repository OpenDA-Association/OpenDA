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

import org.openda.interfaces.IDimensionIndex;
import org.openda.interfaces.IPrevExchangeItem;

/**
 * Configuration speficying a subvector in the parameter vector, the state vector, or the predictor vector
 */
public class BBModelVectorConfig extends BBStochModelVectorConfig {

    private IoObjectConfig ioObjectConfig;
	private IPrevExchangeItem.Role role;

    public BBModelVectorConfig(String id, IoObjectConfig ioObjectConfig, String elementId, IDimensionIndex[] selectionIndices, BBConfigurable selectorConfig, IPrevExchangeItem.Role role) {
        super(id, elementId, selectionIndices, selectorConfig);
		this.role = role;
        this.ioObjectConfig = ioObjectConfig;
    }

    public IoObjectConfig getIoObjectConfig() {
        return ioObjectConfig;
    }

	public IPrevExchangeItem.Role getRole() {
		return role;
	}

}
