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
import java.util.Map;

/**
 * Created by hummel on 07-Aug-15.
 */
public class BBBoundaryMappingConfig {
	private int operationType;
	private Map<String, String> mappingExchangeItems;

	public BBBoundaryMappingConfig(int operationType, Map<String, String> mappingExchangeItems) {
		this.operationType = operationType;
		this.mappingExchangeItems = mappingExchangeItems;
	}

	public int getOperationType() {
		return this.operationType;
	}

	public Map<String, String> getMappingExchangeItems() {
		return this.mappingExchangeItems;
	}
}
