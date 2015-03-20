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

import org.openda.blackbox.interfaces.IKeyDateType;
import org.openda.utils.Time;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

/**
 * Class implementation of SwanDateString KeyType
 */
public class KeyTypeSwanStartDateString extends KeyTypeString implements IKeyDateType{
    static String DATE_FORMAT = "yyyyMMdd";
    static String TIME_FORMAT = "HHmm";
    static String USED_TIMEZONE = "UTC";
    Locale DEFAULT_LOCALE = Locale.UK;

    protected double startDate = Double.NaN;
    protected double endDate = Double.NaN;

    @Override
    public void setStartDate(double date) {
        this.startDate = date;

    }

    @Override
    public void setEndDate(double date) {
        this.endDate = date;

    }

    protected String parseDateValue(double value) {
        Locale.setDefault(DEFAULT_LOCALE); //make sure date is written in UK format, independent of local settings
        DateFormat dateFormatter = new SimpleDateFormat(DATE_FORMAT);
        DateFormat timeFormatter = new SimpleDateFormat(TIME_FORMAT);
        dateFormatter.setTimeZone(TimeZone.getTimeZone(USED_TIMEZONE));
        timeFormatter.setTimeZone(TimeZone.getTimeZone(USED_TIMEZONE));
        Date date = new Time(value).getDate();
        return dateFormatter.format(date) + "." + timeFormatter.format(date);
    }

    @Override
    public String calculateValue() {
        if (Double.isNaN(startDate)) {
            throw new RuntimeException(this.getClass().getName() + ".calculateValue(): startDate needs to be set before calling this method.");
        }
        return parseDateValue(startDate);
    }

}
