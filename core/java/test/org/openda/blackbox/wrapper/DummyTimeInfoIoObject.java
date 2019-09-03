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
package org.openda.blackbox.wrapper;

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.Time;

import java.io.File;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Dummy IoObject for time info
 */
public class DummyTimeInfoIoObject implements IoObjectInterface {

    private IExchangeItem[] exchangeItems = null;

    public void initialize(File workingDir, String fileName, String[] arguments) {
        if (arguments.length != 2) {
            throw new RuntimeException(this.getClass().getName() +  ": expecting two arguments: start time end time");
        }
        DateFormat formatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        Date startDate;
        Date endDate;

        try {
            startDate = formatter.parse(arguments[0]);
        } catch (ParseException e) {
            throw new RuntimeException(this.getClass().getName() +
                    ": could not parse start time from argument \"" + arguments[0] + "\"");
        }
        try {
            endDate = formatter.parse(arguments[1]);
        } catch (ParseException e) {
            throw new RuntimeException(this.getClass().getName() +
                    ": could not parse end time from argument \"" + arguments[1] + "\"");
        }
        double startTimeAsMjd = new Time(startDate).getMJD();
        double endTimeAsMjd = new Time(endDate).getMJD();

        exchangeItems = new IExchangeItem[2];
        exchangeItems[0] = new DoubleExchangeItem("start_time", startTimeAsMjd);
        exchangeItems[1] = new DoubleExchangeItem("end_time", endTimeAsMjd);
    }

    public IExchangeItem[] getExchangeItems() {
        return exchangeItems;
    }

    public void finish() {
        // no action needed
    }
}
