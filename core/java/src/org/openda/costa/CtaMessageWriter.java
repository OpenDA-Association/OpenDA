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
package org.openda.costa;
import org.openda.utils.Results;

/**
 * Created by IntelliJ IDEA.
 * User: Nils
 * Date: 10-dec-2009
 * Time: 16:32:32
 * To change this template use File | Settings | File Templates.
 */
public class CtaMessageWriter {
    CtaMessageWriter(){};

    public static void Write(String nameOfClass, String method, String message, String type){
    char typeCode=type.toUpperCase().charAt(0);


        switch(typeCode){
            // Message (normal string stream)
            case 'M' :
                System.out.println(message);
                break;
            // Informative message
            case 'I' :
                // Note: the replaceAll action causes the text output in Intellij to be correct
                // Otherwise forward slashes will cause vanishing letters TODO
                Results.putProgression("Info: ("+nameOfClass+"."+method+") "+ message.replaceAll("/","/"));
                break;
            // Warning
            case 'W' :
               Results.putMessage("Warning: ("+nameOfClass+"."+method+") "+ message);
               break;
            // Error
            case 'E' :
                Results.putProgression("CTA MESSAGE WRITER: Error: ("+nameOfClass+"."+method+") "+ message);
                // Commented the following line out since it makes non-fatal errors into fatal errors.
                // throw new RuntimeException("Native Error");
                break;
           // Fatal
           case 'F' :
                Results.putProgression("FATAL ERROR: ("+nameOfClass+"."+method+") "+ message);
                throw new RuntimeException("Native Fatal Error");
           default :
               Results.putMessage("FATAL ERROR: (Unknown priority flag ("+type.toUpperCase()+")");
               Results.putMessage("Message: ("+nameOfClass+"."+method+") "+ message);
               throw new RuntimeException("Illegal priority flag");
        }
    }
}
