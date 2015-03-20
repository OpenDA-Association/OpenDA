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

package org.openda.uncertainties;


/**
 * Class to store properties of an uncertain model parameter. This class will be used
 * in PDF and Variation.
 */
public class UncertainItem {

    private String id;
    private boolean isActive;
    private String description;

    //if no basic value available, then set to Double.NaN.
    private double basicValue = Double.NaN;

    public UncertainItem(String id, String description, boolean active) {
    	this.id = id;
        this.isActive = active;
        this.description = description;
    }

    /**
     * Clones this object.
     * @return clonedUncertainItem a clone of this UncertainItem object.
     */
    public UncertainItem clone() {
    	UncertainItem clonedUncertainItem = new UncertainItem(new String(this.id),
    			new String(this.description), new Boolean(this.isActive));
    	clonedUncertainItem.setBasicValue(new Double(this.basicValue));

    	return clonedUncertainItem;
    }

    public String getId() {
        return id;
    }

    public boolean isActive() {
        return isActive;
    }

    public void setActive(boolean active) {
        isActive = active;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public void setId(String id) {
        this.id = id;
    }

	public void setBasicValue(double basicValue) {
		this.basicValue = basicValue;
	}

	public double getBasicValue() {
		return this.basicValue;
	}

    /**
     * Returns a string representation of the object. In general, the
     * <code>toString</code> method returns a string that
     * "textually represents" this object. The result should
     * be a concise but informative representation that is easy for a
     * person to read.
     * It is recommended that all subclasses override this method.
     * <p/>
     * The <code>toString</code> method for class <code>Object</code>
     * returns a string consisting of the name of the class of which the
     * object is an instance, the at-sign character `<code>@</code>', and
     * the unsigned hexadecimal representation of the hash code of the
     * object. In other words, this method returns a string equal to the
     * value of:
     * <blockquote>
     * <pre>
     * getClass().getName() + '@' + Integer.toHexString(hashCode())
     * </pre></blockquote>
     *
     * @return a string representation of the object.
     */
    public String toString() {
        return " Uncertainty ID : " + id + " " + (isActive? "Active":" NonActive");
    }

    public boolean compare(UncertainItem uncertainItem) {
        if (!uncertainItem.getId().equals(this.getId())) return false;
        if (!uncertainItem.getDescription().equals(this.getDescription())) return false;
        if (Double.compare(uncertainItem.getBasicValue(), this.getBasicValue())!=0) return false;
        return true;
    }
}
