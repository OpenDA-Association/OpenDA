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

package org.openda.utils.io;

import org.exolab.castor.xml.Unmarshaller;
import org.exolab.castor.xml.ValidationException;
import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.MarshalException;
import org.xml.sax.InputSource;

import java.io.*;


/**
 * Generic unmarshaller routine
 */
public class CastorUtils {

    /**
     * Parse an XML file.
     *
     * @param xmlFile XML file to be parsed.
     * @param xmlClassType The class of the XML object that is expected in the file.
     * @return The XML object that was parsed from the file.
     */
    public static Object parse(File xmlFile, Class xmlClassType) {

        if (xmlFile == null) {
            throw new IllegalArgumentException("CastorUtils.parse: xmlFile == null");
        }
        if (!xmlFile.exists()) {
            throw new IllegalArgumentException("CastorUtils.parse: file does not exist: " +
                    xmlFile.getAbsolutePath());
        }

        Object parsedObject = null;
        if (xmlFile.exists()) {
            try {
                // unmarshal the config file
                Unmarshaller unmarshaller = new Unmarshaller(xmlClassType);
                Reader reader = new BufferedReader(new FileReader(xmlFile));
                InputSource is = new InputSource(reader);
                parsedObject = unmarshaller.unmarshal(is);
                reader.close();
            } catch (ValidationException e) {
                 throw new RuntimeException(e.getMessage() + " at " + e.getLocation() + " (file: " + xmlFile.getAbsolutePath() + ")");
            } catch (FileNotFoundException e) {
                throw new RuntimeException(e.getMessage() + " (file: " + xmlFile.getAbsolutePath() + ")");
            } catch (IOException e) {
                throw new RuntimeException(e.getMessage() + " (file: " + xmlFile.getAbsolutePath() + ")");
            } catch (org.exolab.castor.xml.MarshalException e) {
                throw new RuntimeException(e.getMessage() + " (file: " + xmlFile.getAbsolutePath() + ")");
            }
        }
        return parsedObject;
    }


    /**
     * Write an XML file.
     *
     * @param object The castor object to be written to file.
     * @param xmlFile XML file to be written.
     * @param rootElement The root element in the XML file.
     * @param schemaLocation Schema location (<code>null</code>: no schema location)
     * @param noNameSpaceLocation specific location of schema file (<code>null</code>: no specific location)
     */
    public static void write(Object object, File xmlFile, String rootElement, String schemaLocation, String noNameSpaceLocation) {

        try {
            BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(xmlFile), "UTF-8"));
            Marshaller marshaller = new Marshaller(writer);
            marshaller.setRootElement(rootElement);

            if (schemaLocation != null) {
                marshaller.setSchemaLocation(schemaLocation + "/" + rootElement + ".xsd");
            } else if (noNameSpaceLocation != null) {
                marshaller.setNoNamespaceSchemaLocation(schemaLocation);
            } else {
                marshaller.setSchemaLocation("http://www.openda.org http://www.openda.org/schemas/" + rootElement + ".xsd");
            }

            marshaller.marshal(object);
            writer.close();
        } catch (IOException e) {
            throw new RuntimeException(e.getMessage() + " (file: " + xmlFile.getAbsolutePath() + ")");
        } catch (ValidationException e) {
            throw new RuntimeException(e.getMessage() + " (file: " + xmlFile.getAbsolutePath() + ")");
        } catch (MarshalException e) {
            throw new RuntimeException(e.getMessage() + " (file: " + xmlFile.getAbsolutePath() + ")");
        }
    }
}
