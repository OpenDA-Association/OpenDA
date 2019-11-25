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
import java.util.ArrayList;
import java.util.Enumeration;
import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;
import org.exolab.castor.xml.ValidationException;

/**
 * Class ExternalModelStochModelFactoryConfigXML.
 * 
 * @version $Revision$ $Date$
 */
public abstract class ExternalModelStochModelFactoryConfigXML implements java.io.Serializable {


      //--------------------------/
     //- Class/Member Variables -/
    //--------------------------/

    /**
     * Field _portNumber
     */
    private int _portNumber;

    /**
     * keeps track of state for field: _portNumber
     */
    private boolean _has_portNumber;

    /**
     * Field _parameterList
     */
    private java.util.ArrayList _parameterList;


      //----------------/
     //- Constructors -/
    //----------------/

    public ExternalModelStochModelFactoryConfigXML() {
        super();
        _parameterList = new ArrayList();
    } //-- org.openda.model_external_socket.io.castorgenerated.ExternalModelStochModelFactoryConfigXML()


      //-----------/
     //- Methods -/
    //-----------/

    /**
     * Method addParameter
     * 
     * @param vParameter
     */
    public void addParameter(org.openda.model_external_socket.io.castorgenerated.Parameter vParameter)
        throws java.lang.IndexOutOfBoundsException
    {
        _parameterList.add(vParameter);
    } //-- void addParameter(org.openda.model_external_socket.io.castorgenerated.Parameter) 

    /**
     * Method addParameter
     * 
     * @param index
     * @param vParameter
     */
    public void addParameter(int index, org.openda.model_external_socket.io.castorgenerated.Parameter vParameter)
        throws java.lang.IndexOutOfBoundsException
    {
        _parameterList.add(index, vParameter);
    } //-- void addParameter(int, org.openda.model_external_socket.io.castorgenerated.Parameter) 

    /**
     * Method clearParameter
     */
    public void clearParameter()
    {
        _parameterList.clear();
    } //-- void clearParameter() 

    /**
     * Method enumerateParameter
     */
    public java.util.Enumeration enumerateParameter()
    {
        return new org.exolab.castor.util.IteratorEnumeration(_parameterList.iterator());
    } //-- java.util.Enumeration enumerateParameter() 

    /**
     * Method getParameter
     * 
     * @param index
     */
    public org.openda.model_external_socket.io.castorgenerated.Parameter getParameter(int index)
        throws java.lang.IndexOutOfBoundsException
    {
        //-- check bounds for index
        if ((index < 0) || (index > _parameterList.size())) {
            throw new IndexOutOfBoundsException();
        }
        
        return (org.openda.model_external_socket.io.castorgenerated.Parameter) _parameterList.get(index);
    } //-- org.openda.model_external_socket.io.castorgenerated.Parameter getParameter(int) 

    /**
     * Method getParameter
     */
    public org.openda.model_external_socket.io.castorgenerated.Parameter[] getParameter()
    {
        int size = _parameterList.size();
        org.openda.model_external_socket.io.castorgenerated.Parameter[] mArray = new org.openda.model_external_socket.io.castorgenerated.Parameter[size];
        for (int index = 0; index < size; index++) {
            mArray[index] = (org.openda.model_external_socket.io.castorgenerated.Parameter) _parameterList.get(index);
        }
        return mArray;
    } //-- org.openda.model_external_socket.io.castorgenerated.Parameter[] getParameter() 

    /**
     * Method getParameterCount
     */
    public int getParameterCount()
    {
        return _parameterList.size();
    } //-- int getParameterCount() 

    /**
     * Returns the value of field 'portNumber'.
     * 
     * @return the value of field 'portNumber'.
     */
    public int getPortNumber()
    {
        return this._portNumber;
    } //-- int getPortNumber() 

    /**
     * Method hasPortNumber
     */
    public boolean hasPortNumber()
    {
        return this._has_portNumber;
    } //-- boolean hasPortNumber() 

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
     * Method removeParameter
     * 
     * @param vParameter
     */
    public boolean removeParameter(org.openda.model_external_socket.io.castorgenerated.Parameter vParameter)
    {
        boolean removed = _parameterList.remove(vParameter);
        return removed;
    } //-- boolean removeParameter(org.openda.model_external_socket.io.castorgenerated.Parameter) 

    /**
     * Method setParameter
     * 
     * @param index
     * @param vParameter
     */
    public void setParameter(int index, org.openda.model_external_socket.io.castorgenerated.Parameter vParameter)
        throws java.lang.IndexOutOfBoundsException
    {
        //-- check bounds for index
        if ((index < 0) || (index > _parameterList.size())) {
            throw new IndexOutOfBoundsException();
        }
        _parameterList.set(index, vParameter);
    } //-- void setParameter(int, org.openda.model_external_socket.io.castorgenerated.Parameter) 

    /**
     * Method setParameter
     * 
     * @param parameterArray
     */
    public void setParameter(org.openda.model_external_socket.io.castorgenerated.Parameter[] parameterArray)
    {
        //-- copy array
        _parameterList.clear();
        for (int i = 0; i < parameterArray.length; i++) {
            _parameterList.add(parameterArray[i]);
        }
    } //-- void setParameter(org.openda.model_external_socket.io.castorgenerated.Parameter) 

    /**
     * Sets the value of field 'portNumber'.
     * 
     * @param portNumber the value of field 'portNumber'.
     */
    public void setPortNumber(int portNumber)
    {
        this._portNumber = portNumber;
        this._has_portNumber = true;
    } //-- void setPortNumber(int) 

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
