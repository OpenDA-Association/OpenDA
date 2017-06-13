/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
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
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.DoublesExchangeItem;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;

import java.io.File;

/**
 * Created by pelgrim on 28-Oct-16.
 */
public class DummyStateDataObject implements IDataObject {
    private static int stateSize;

    @Override
    public String[] getExchangeItemIDs() {
        return new String[]{"state", "StartTime", "StopTime", "TimeStep"};
    }

    @Override
    public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
        throw new RuntimeException("org.openda.blackbox.wrapper.DummyStateDataObject.getExchangeItemIDs() not implemented yet");

    }

    @Override
    public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
        switch (exchangeItemID) {
            case "state" :
                //double[] values = new double[1061];
                double[] values = new double[stateSize];
                fillWithRandomValues(values);
                return new DoublesExchangeItem("state", IPrevExchangeItem.Role.InOut, values);
            case "StartTime" :
                return new DoubleExchangeItem("StartTime", IPrevExchangeItem.Role.InOut, 0);
            case "StopTime" :
                return new DoubleExchangeItem("StopTime", IPrevExchangeItem.Role.InOut, 10);
            case "TimeStep" :
                return new DoubleExchangeItem("TimeStep", IPrevExchangeItem.Role.InOut, 1);
            default:
                return null;
        }
    }

    private void fillWithRandomValues(double[] values) {
        for (int i = 0; i < values.length; i++) {
			values[i] = Math.random();
		}
    }

    @Override
    public void finish() {
    }

    @Override
    public void initialize(File workingDir, String[] arguments) {

    }

    public static void setStateSize(int size) {
        stateSize = size;
    }
}
