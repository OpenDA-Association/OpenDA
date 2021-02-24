/*
* Copyright (c) 2021 OpenDA Association 
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
import java.util.ArrayList;
import java.util.Enumeration;
import org.exolab.castor.xml.MarshalException;
import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;
import org.exolab.castor.xml.ValidationException;
import org.xml.sax.ContentHandler;

/**
 * Class ExternalSocketModelFactoryConfigXML.
 * 
 * @version $Revision$ $Date$
 */
public class ExternalSocketModelFactoryConfigXML implements java.io.Serializable {


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

    public ExternalSocketModelFactoryConfigXML() {
        super();
        _parameterList = new ArrayList();
    } //-- org.openda.model_external_socket.io.castorgenerated.ExternalSocketModelFactoryConfigXML()


      //-----------/
     //- Methods -/
    //-----------/

    /**
     * Method addParameter
     * 
     * @param vParameter
     */
    public void addParameter(org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType vParameter)
        throws java.lang.IndexOutOfBoundsException
    {
        _parameterList.add(vParameter);
    } //-- void addParameter(org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType) 

    /**
     * Method addParameter
     * 
     * @param index
     * @param vParameter
     */
    public void addParameter(int index, org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType vParameter)
        throws java.lang.IndexOutOfBoundsException
    {
        _parameterList.add(index, vParameter);
    } //-- void addParameter(int, org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType) 

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
    public org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType getParameter(int index)
        throws java.lang.IndexOutOfBoundsException
    {
        //-- check bounds for index
        if ((index < 0) || (index > _parameterList.size())) {
            throw new IndexOutOfBoundsException();
        }
        
        return (org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType) _parameterList.get(index);
    } //-- org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType getParameter(int) 

    /**
     * Method getParameter
     */
    public org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType[] getParameter()
    {
        int size = _parameterList.size();
        org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType[] mArray = new org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType[size];
        for (int index = 0; index < size; index++) {
            mArray[index] = (org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType) _parameterList.get(index);
        }
        return mArray;
    } //-- org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType[] getParameter() 

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
     * Method removeParameter
     * 
     * @param vParameter
     */
    public boolean removeParameter(org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType vParameter)
    {
        boolean removed = _parameterList.remove(vParameter);
        return removed;
    } //-- boolean removeParameter(org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType) 

    /**
     * Method setParameter
     * 
     * @param index
     * @param vParameter
     */
    public void setParameter(int index, org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType vParameter)
        throws java.lang.IndexOutOfBoundsException
    {
        //-- check bounds for index
        if ((index < 0) || (index > _parameterList.size())) {
            throw new IndexOutOfBoundsException();
        }
        _parameterList.set(index, vParameter);
    } //-- void setParameter(int, org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType) 

    /**
     * Method setParameter
     * 
     * @param parameterArray
     */
    public void setParameter(org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType[] parameterArray)
    {
        //-- copy array
        _parameterList.clear();
        for (int i = 0; i < parameterArray.length; i++) {
            _parameterList.add(parameterArray[i]);
        }
    } //-- void setParameter(org.openda.model_external_socket.io.castorgenerated.ExternalModelParameterComplexType) 

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
     * Method unmarshalExternalSocketModelFactoryConfigXML
     * 
     * @param reader
     */
    public static org.openda.model_external_socket.io.castorgenerated.ExternalSocketModelFactoryConfigXML unmarshalExternalSocketModelFactoryConfigXML(java.io.Reader reader)
        throws org.exolab.castor.xml.MarshalException, org.exolab.castor.xml.ValidationException
    {
        return (org.openda.model_external_socket.io.castorgenerated.ExternalSocketModelFactoryConfigXML) Unmarshaller.unmarshal(org.openda.model_external_socket.io.castorgenerated.ExternalSocketModelFactoryConfigXML.class, reader);
    } //-- org.openda.model_external_socket.io.castorgenerated.ExternalSocketModelFactoryConfigXML unmarshalExternalSocketModelFactoryConfigXML(java.io.Reader) 

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
