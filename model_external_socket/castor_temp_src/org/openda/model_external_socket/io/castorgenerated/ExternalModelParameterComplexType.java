/*
* Copyright (c) 2023 OpenDA Association 
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

package org.openda.model_external_socket.io.castorgenerated;

  //---------------------------------/
 //- Imported classes and packages -/
//---------------------------------/

import java.io.IOException;
import java.io.Reader;
import java.io.Serializable;
import java.io.Writer;
import org.exolab.castor.xml.MarshalException;
import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;
import org.exolab.castor.xml.ValidationException;
import org.xml.sax.ContentHandler;

/**
 * Class ExternalModelParameterComplexType.
 * 
 * @version $Revision$ $Date$
 */
public class ExternalModelParameterComplexType implements java.io.Serializable {


      //--------------------------/
     //- Class/Member Variables -/
    //--------------------------/

    /**
     * Field _startValue
     */
    private double _startValue;

    /**
     * keeps track of state for field: _startValue
     */
    private boolean _has_startValue;

    /**
     * Field _stdDev
     */
    private double _stdDev;

    /**
     * keeps track of state for field: _stdDev
     */
    private boolean _has_stdDev;

    /**
     * Field _lowerBound
     */
    private double _lowerBound;

    /**
     * keeps track of state for field: _lowerBound
     */
    private boolean _has_lowerBound;

    /**
     * Field _upperBound
     */
    private double _upperBound;

    /**
     * keeps track of state for field: _upperBound
     */
    private boolean _has_upperBound;


      //----------------/
     //- Constructors -/
    //----------------/

    public ExternalModelParameterComplexType() {
        super();
    } //-- org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType()


      //-----------/
     //- Methods -/
    //-----------/

    /**
     * Method deleteLowerBound
     */
    public void deleteLowerBound()
    {
        this._has_lowerBound= false;
    } //-- void deleteLowerBound() 

    /**
     * Method deleteUpperBound
     */
    public void deleteUpperBound()
    {
        this._has_upperBound= false;
    } //-- void deleteUpperBound() 

    /**
     * Returns the value of field 'lowerBound'.
     * 
     * @return the value of field 'lowerBound'.
     */
    public double getLowerBound()
    {
        return this._lowerBound;
    } //-- double getLowerBound() 

    /**
     * Returns the value of field 'startValue'.
     * 
     * @return the value of field 'startValue'.
     */
    public double getStartValue()
    {
        return this._startValue;
    } //-- double getStartValue() 

    /**
     * Returns the value of field 'stdDev'.
     * 
     * @return the value of field 'stdDev'.
     */
    public double getStdDev()
    {
        return this._stdDev;
    } //-- double getStdDev() 

    /**
     * Returns the value of field 'upperBound'.
     * 
     * @return the value of field 'upperBound'.
     */
    public double getUpperBound()
    {
        return this._upperBound;
    } //-- double getUpperBound() 

    /**
     * Method hasLowerBound
     */
    public boolean hasLowerBound()
    {
        return this._has_lowerBound;
    } //-- boolean hasLowerBound() 

    /**
     * Method hasStartValue
     */
    public boolean hasStartValue()
    {
        return this._has_startValue;
    } //-- boolean hasStartValue() 

    /**
     * Method hasStdDev
     */
    public boolean hasStdDev()
    {
        return this._has_stdDev;
    } //-- boolean hasStdDev() 

    /**
     * Method hasUpperBound
     */
    public boolean hasUpperBound()
    {
        return this._has_upperBound;
    } //-- boolean hasUpperBound() 

    /**
     * Method isValid
     */
    public boolean isValid()
    {
        try {
            validate();
        }
        catch (org.exolab.castor.xml.ValidationException vex) {
            return false;
        }
        return true;
    } //-- boolean isValid() 

    /**
     * Method marshal
     * 
     * @param out
     */
    public void marshal(java.io.Writer out)
        throws org.exolab.castor.xml.MarshalException, org.exolab.castor.xml.ValidationException
    {
        
        Marshaller.marshal(this, out);
    } //-- void marshal(java.io.Writer) 

    /**
     * Method marshal
     * 
     * @param handler
     */
    public void marshal(org.xml.sax.ContentHandler handler)
        throws java.io.IOException, org.exolab.castor.xml.MarshalException, org.exolab.castor.xml.ValidationException
    {
        
        Marshaller.marshal(this, handler);
    } //-- void marshal(org.xml.sax.ContentHandler) 

    /**
     * Sets the value of field 'lowerBound'.
     * 
     * @param lowerBound the value of field 'lowerBound'.
     */
    public void setLowerBound(double lowerBound)
    {
        this._lowerBound = lowerBound;
        this._has_lowerBound = true;
    } //-- void setLowerBound(double) 

    /**
     * Sets the value of field 'startValue'.
     * 
     * @param startValue the value of field 'startValue'.
     */
    public void setStartValue(double startValue)
    {
        this._startValue = startValue;
        this._has_startValue = true;
    } //-- void setStartValue(double) 

    /**
     * Sets the value of field 'stdDev'.
     * 
     * @param stdDev the value of field 'stdDev'.
     */
    public void setStdDev(double stdDev)
    {
        this._stdDev = stdDev;
        this._has_stdDev = true;
    } //-- void setStdDev(double) 

    /**
     * Sets the value of field 'upperBound'.
     * 
     * @param upperBound the value of field 'upperBound'.
     */
    public void setUpperBound(double upperBound)
    {
        this._upperBound = upperBound;
        this._has_upperBound = true;
    } //-- void setUpperBound(double) 

    /**
     * Method unmarshalExternalModelParameterComplexType
     * 
     * @param reader
     */
    public static org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType unmarshalExternalModelParameterComplexType(java.io.Reader reader)
        throws org.exolab.castor.xml.MarshalException, org.exolab.castor.xml.ValidationException
    {
        return (org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType) Unmarshaller.unmarshal(org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType.class, reader);
    } //-- org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType unmarshalExternalModelParameterComplexType(java.io.Reader) 

    /**
     * Method validate
     */
    public void validate()
        throws org.exolab.castor.xml.ValidationException
    {
        org.exolab.castor.xml.Validator validator = new org.exolab.castor.xml.Validator();
        validator.validate(this);
    } //-- void validate() 

}
