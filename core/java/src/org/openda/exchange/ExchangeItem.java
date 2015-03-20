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


package org.openda.exchange;

import org.openda.interfaces.*;

/**
 * Base implementation of IPrevExchangeItem
 */
public abstract class ExchangeItem implements IExchangeItem {

    private String id;
    private String description;
    private String quantityId;
    private String unitId;
    private IPrevExchangeItem.Role role= IPrevExchangeItem.Role.InOut;
	private TimeInfo timeInfo = new TimeInfo();

	public ExchangeItem(String id) {
        this.id = id;
    }

    public ExchangeItem(String id, String description) {
        this(id);
        this.description = description;
    }

    public ExchangeItem(String id, String quantityId, String unitId) {
        this.id = id;
        this.quantityId = quantityId;
        this.unitId = unitId;
    }

    public String getId() {
        return id;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public void setQuantityId(String quantityId) {
        this.quantityId = quantityId;
    }

    public void setUnitId(String unitId) {
        this.unitId = unitId;
    }

    public void setRole(IPrevExchangeItem.Role role){
    	this.role = role;
    }

    public IPrevExchangeItem.Role getRole(){
    	return this.role;
    }

    public String getQuantityId() {
        return quantityId;
    }

    public String getUnitId() {
        return unitId;
    }

    public double[] getTimes() {
        return timeInfo.getTimes();
    }

    public void setTimes(double[] times) {
        timeInfo.setTimes(times);
    }

    public void setTimes(ITime[] times) {
		timeInfo.setTimes(times);
    }

	public void copyValuesFromItem(IExchangeItem sourceItem) {
		if (sourceItem.getValueType() != getValueType()) {
			throw new RuntimeException("Incompatible value types in copy action from " + sourceItem.getId() +
			" to " + getId() + "(" + sourceItem.getValueType().toString() + "/=" + getValueType().toString());
		}
		setValues(sourceItem.getValues());
	}


	public ITimeInfo getTimeInfo() {
		return timeInfo;
	}

	public IQuantityInfo getQuantityInfo() {
		return new QuantityInfo(quantityId, unitId);
	}

	public IGeometryInfo getGeometryInfo() {
		return null;
	}
}
