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

package org.openda.model_delft3d;

import org.openda.model_delft3d.ods.OdsStore;
import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.IPrevExchangeItem;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * Delft3d result reader for nefis files etc.
 */
public class D3dResults implements IoObjectInterface {

    IPrevExchangeItem[] exchangeItems = new IPrevExchangeItem[0];

    public void initialize(File workingDir, String fileName, String[] arguments) {
        if (arguments.length < 1) {
            throw new IllegalArgumentException("D3dResults.initialize() expecteds at least one argument, " +
            "the bin dir. containing the native DLL's. Additional arguments are interpreted as station selection");
        }
        File trihfile = new File(workingDir, fileName);
		try {
            String nativeBinDir;
			File arguments0AsFile = new File(arguments[0]);
			if (arguments0AsFile.isAbsolute()) {
				nativeBinDir = arguments0AsFile.getAbsolutePath();
			} else {
				nativeBinDir = new File(workingDir, arguments[0]).getAbsolutePath();
			}
            OdsStore odsStore = new OdsStore(trihfile,nativeBinDir);
            String[] parameters = odsStore.getParameters();   // TODO: REACTIVE USING THE VARIOUS PARAMS
            List<IPrevExchangeItem> exchangeItemList = new ArrayList<IPrevExchangeItem>();
//            for (String parameter : new String[]{"water level"}) {
			for (String parameter : parameters) {
                String[] locations = odsStore.getLocations(parameter);
                for (String location : locations) {
                    String seriesId = location + "." + parameter;

                    // if no selected exchange items are specified as argument, all exchange items will be created
                    boolean createExchangeItem = arguments.length == 1;

                    // if selected exchange items arguments are provided, check if it contains seriesId.
                    // If so, create the exchange item
                    for (int i = 1; !createExchangeItem && i < arguments.length; i++) {
                        if (arguments[i].equalsIgnoreCase(seriesId)) {
                            createExchangeItem = true;
                        }
                    }
                    if (createExchangeItem) {
                        double[] times = odsStore.getTimes(parameter);
                        float[] values = odsStore.getLocationTimeSeriesValues(parameter, location, times[0], times[times.length-1],1);
                        double[] valuesAsDouble = new double[values.length];
                        for (int i = 0; i < valuesAsDouble.length; i++) {
                            times[i] -= 2400000.5d;
                            valuesAsDouble[i] = values[i];
                        }
                        exchangeItemList.add(new TimeSeries(times, valuesAsDouble, "computed", parameter, "[ as in file ]", location));
                    }
                }
            }
            this.exchangeItems = exchangeItemList.toArray(new IPrevExchangeItem[exchangeItemList.size()]);
        } catch (Exception e) {
			throw new RuntimeException(e.getMessage());
        }
    }

    public IPrevExchangeItem[] getExchangeItems() {
        return exchangeItems;
    }

    public void finish() {
        // no action
    }
}
