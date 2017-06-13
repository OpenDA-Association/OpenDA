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

package org.openda.model_delft3d;

import org.openda.interfaces.IPrevExchangeItem;

import java.io.File;

/**
 * Created by IntelliJ IDEA.
 * User: nils
 * Date: May 8, 2009
 * Time: 3:40:50 PM
 *
 * Implementations of of the exangeItems of a SIMONA roughness include file
 *
 * The include file contained lines of the forms:
 *
 *   < R_CODE=[ival] A=[val]  B=[val]  C=[val]  D=[val] >
 *
 * or the new format
 *
 *   < R_CODE=[ival] PARAM=[val] A=[val]  B=[val]  C=[val]  D=[val] >
 *
 * NOTE: in SIMONA it is allowed to specify a roughness parameter over multiple lines
 *       this implementation DOES NOT support a multiple line declaration!
 *
 * The parameters that can be calibrates are A, B, C and D Therefore each line can potentially
 * result in 4 different parameters. (In practical applications only one of the four parameters
 * will be calibrated. But which parameters are to be calibrated is not a part of the exchange
 * items implementation
 *
 */
public class D3dRoughParamsFileExchangeItem implements IPrevExchangeItem {

    private String id;                    // Unique ID of the parameter
    private String quantityId ="-";       // The code A,B,C,D
    private String unitId     ="-";       // Not sure whether these parameters have a unit
    private Class classType=double.class; // The value of the exchange item is always a double
    private double value;                 // The value of the exchange item
    private String code;                  // The parameter code A,B,C or D
    File roughFile;                       // File handle to roughness include file
    int lineNum;                          // Line number that corresponds to the parameter
    private String description;

    public D3dRoughParamsFileExchangeItem(String id, File roughFile, int lineNum, String code){

        this.id         = id;
        this.quantityId ="-";
        this.unitId     ="-";
        this.classType  =double.class;
        this.code       =code;
        this.roughFile  =roughFile;
        this.lineNum    =lineNum;

        // Here we need to read the value from the input file will be stored in this.value
        this.value=D3dRoughParamsUtils.readValueFromFile(this.roughFile, this.lineNum, this.code);
    }

    public D3dRoughParamsFileExchangeItem(String id, String description, File roughFile, int lineNum, String code){
        this(id, roughFile, lineNum, code);
        this.description = description;
    }

    public String getId() {
        return this.id;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public String getQuantityId() {
        return this.quantityId;
    }

    public String getUnitId() {
        return this.unitId;
    }

    public Class getValueType() {
        return this.classType;
    }

    public Role getRole() {
        return IPrevExchangeItem.Role.InOut;
    }

    public Object getValues() {
        return this.value;
    }

    public double[] getValuesAsDoubles() {
        double retval[]=new double[1];
        retval[0]=this.value;
        return retval;
    }

    public void setValues(Object values) {
        if (values instanceof Double){
            this.value=(Double) values;
        } else {
            throw new UnsupportedOperationException("Only Value of type Double can be set");
        }
        D3dRoughParamsUtils.writeValueToFile(this.roughFile,this.lineNum, this.code,  this.value);
    }

    public void axpyOnValues(double alpha, double[] axpyValues) {
        if (axpyValues.length !=1){
            throw new UnsupportedOperationException("Length of axpy value array must be 1");
        }
        this.value += alpha * axpyValues[0];
    }

	public void multiplyValues(double[] multiplicationFactors) {
		if (multiplicationFactors.length !=1){
			throw new UnsupportedOperationException("Length of multiplication factors array must be 1");
		}
		this.value *= multiplicationFactors[0];
	}

	public void setValuesAsDoubles(double[] values) {
        if (values.length !=1){
            throw new UnsupportedOperationException("Length of value array must be 1");
        }
        this.value = values[0];
        D3dRoughParamsUtils.writeValueToFile(this.roughFile,this.lineNum, this.code,  this.value);
    }

    public double[] getTimes() {
        return null;
    }

    public void setTimes(double[] times) {
        throw new RuntimeException(this.getClass().getName() + " does not have timeStamps");
    }
}

