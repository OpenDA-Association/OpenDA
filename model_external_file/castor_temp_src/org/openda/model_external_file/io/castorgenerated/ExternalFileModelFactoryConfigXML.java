/*
 * This class was automatically generated with 
 * <a href="http://www.castor.org">Castor 0.9.4.3</a>, using an XML
 * Schema.
 * $Id$
 */

package org.openda.model_external_file.io.castorgenerated;

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
 * Class ExternalFileModelFactoryConfigXML.
 * 
 * @version $Revision$ $Date$
 */
public class ExternalFileModelFactoryConfigXML implements java.io.Serializable {


      //--------------------------/
     //- Class/Member Variables -/
    //--------------------------/

    /**
     * Field _modelParametersFile
     */
    private java.lang.String _modelParametersFile;

    /**
     * Field _modelResultsFile
     */
    private java.lang.String _modelResultsFile;


      //----------------/
     //- Constructors -/
    //----------------/

    public ExternalFileModelFactoryConfigXML() {
        super();
    } //-- org.openda.model_external_file.io.castorgenerated.ExternalFileModelFactoryConfigXML()


      //-----------/
     //- Methods -/
    //-----------/

    /**
     * Returns the value of field 'modelParametersFile'.
     * 
     * @return the value of field 'modelParametersFile'.
     */
    public java.lang.String getModelParametersFile()
    {
        return this._modelParametersFile;
    } //-- java.lang.String getModelParametersFile() 

    /**
     * Returns the value of field 'modelResultsFile'.
     * 
     * @return the value of field 'modelResultsFile'.
     */
    public java.lang.String getModelResultsFile()
    {
        return this._modelResultsFile;
    } //-- java.lang.String getModelResultsFile() 

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
     * Sets the value of field 'modelParametersFile'.
     * 
     * @param modelParametersFile the value of field
     * 'modelParametersFile'.
     */
    public void setModelParametersFile(java.lang.String modelParametersFile)
    {
        this._modelParametersFile = modelParametersFile;
    } //-- void setModelParametersFile(java.lang.String) 

    /**
     * Sets the value of field 'modelResultsFile'.
     * 
     * @param modelResultsFile the value of field 'modelResultsFile'
     */
    public void setModelResultsFile(java.lang.String modelResultsFile)
    {
        this._modelResultsFile = modelResultsFile;
    } //-- void setModelResultsFile(java.lang.String) 

    /**
     * Method unmarshalExternalFileModelFactoryConfigXML
     * 
     * @param reader
     */
    public static org.openda.model_external_file.io.castorgenerated.ExternalFileModelFactoryConfigXML unmarshalExternalFileModelFactoryConfigXML(java.io.Reader reader)
        throws org.exolab.castor.xml.MarshalException, org.exolab.castor.xml.ValidationException
    {
        return (org.openda.model_external_file.io.castorgenerated.ExternalFileModelFactoryConfigXML) Unmarshaller.unmarshal(org.openda.model_external_file.io.castorgenerated.ExternalFileModelFactoryConfigXML.class, reader);
    } //-- org.openda.model_external_file.io.castorgenerated.ExternalFileModelFactoryConfigXML unmarshalExternalFileModelFactoryConfigXML(java.io.Reader) 

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
