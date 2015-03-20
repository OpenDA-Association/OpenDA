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
package org.openda.blackbox.config;
import org.openda.utils.Time;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Date implementation of KeyType
 */
public class KeyTypeDate extends KeyTypeString {
    static String FORMAT_DATE = "yyyy-MM-dd";

    public String getValueAsString(String value) throws NumberFormatException {
        Double valueAsDouble = Double.valueOf(value);
        Date valueAsDate = new Time(valueAsDouble).getDate();
        DateFormat formatter = new SimpleDateFormat(FORMAT_DATE);
        return formatter.format(valueAsDate);
    }


}
