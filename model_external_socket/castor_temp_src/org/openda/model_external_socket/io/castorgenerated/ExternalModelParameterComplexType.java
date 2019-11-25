/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.4.3</a>, using an XML
 * Schema.
 * $Id$
 */

package org.openda.model_external_socket.io.castorgenerated;

  //---------------------------------/
 //- Imported classes and packages -/
//---------------------------------/

import java.io.Reader;
import java.io.Serializable;
import java.io.Writer;
import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;
import org.exolab.castor.xml.ValidationException;

/**
 * Class ExternalModelParameterComplexType.
 * 
 * @version $Revision$ $Date$
 */
public abstract class ExternalModelParameterComplexType implements java.io.Serializable {


      //--------------------------/
     //- Class/Member Variables -/
    //--------------------------/

    /**
     * Field _value
     */
    private double _value;

    /**
     * keeps track of state for field: _value
     */
    private boolean _has_value;

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
     * Returns the value of field 'value'.
     * 
     * @return the value of field 'value'.
     */
    public double getValue()
    {
        return this._value;
    } //-- double getValue() 

    /**
     * Method hasLowerBound
     */
    public boolean hasLowerBound()
    {
        return this._has_lowerBound;
    } //-- boolean hasLowerBound() 

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
     * Method hasValue
     */
    public boolean hasValue()
    {
        return this._has_value;
    } //-- boolean hasValue() 

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
     * Sets the value of field 'value'.
     * 
     * @param value the value of field 'value'.
     */
    public void setValue(double value)
    {
        this._value = value;
        this._has_value = true;
    } //-- void setValue(double) 

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
