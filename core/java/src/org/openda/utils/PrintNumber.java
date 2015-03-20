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

package org.openda.utils;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;

public class PrintNumber {
    
    public static String printNumber(double value) {
        DecimalFormatSymbols symbols = new DecimalFormatSymbols();
        symbols.setDecimalSeparator('.');
        DecimalFormat formatFloat = new DecimalFormat("0.###", symbols);
        DecimalFormat formatExponent = new DecimalFormat("0.###E0", symbols);

        if (Math.abs(value) > 0.01 && Math.abs(value) < 1000.0 || value == 0.0) {
            return formatFloat.format(value);
        } else {
            return formatExponent.format(value);
        }
    }

    public static String printNumberExtended(double value) {
        DecimalFormatSymbols symbols = new DecimalFormatSymbols();
        symbols.setDecimalSeparator('.');
        DecimalFormat formatFloat = new DecimalFormat("0.#####", symbols);
        DecimalFormat formatExponent = new DecimalFormat("0.#####E0", symbols);

        if (Math.abs(value) > 0.01 && Math.abs(value) < 1000.0 || value == 0.0) {
            return formatFloat.format(value);
        } else {
            return formatExponent.format(value);
        }
    }

}
