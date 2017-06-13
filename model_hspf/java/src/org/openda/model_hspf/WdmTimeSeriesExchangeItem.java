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

package org.openda.model_hspf;

import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.timeseries.TimeSeries;

/**
 * Exchange item for a time series stored in a wdm file.
 *
 * See http://water.usgs.gov/cgi-bin/man_wrdapp?wdm(1) :
 * A WDM file is a binary, direct-access file used to store
 * hydrologic, hydraulic, meteorologic, water-quality, and
 * physiographic data.  The WDM file is organized into data
 * sets (DSN = Data Set Number).  Each data set contains a specific type of data, such
 * as streamflow at a specific site or air temperature at a
 * weather station.  Each data set contains attributes that
 * describe the data, such as station identification number,
 * time step of data, latitude, and longitude.  A WDM file may
 * contain a single data set or as many as 200,000 data sets.
 * A data set may be described by a few attributes or by
 * hundreds of attributes.  Data can be added, deleted, and
 * modified without restructuring the data in the file.  Space
 * from deleted data sets is reused.
 *
 * To manually open and edit a wdm file use WDMUtil, which
 * is installed as part of the BASINS package, which is available from:
 * http://water.epa.gov/scitech/datait/models/basins/index.cfm
 *
 * @author Arno Kockx
 */
public class WdmTimeSeriesExchangeItem extends TimeSeries {
    private int dataSetNumber = -1;

    public WdmTimeSeriesExchangeItem(String id, Role role) {
        super();

        this.role = role;

        String location = BBUtils.getLocationFromId(id);
        String parameter = BBUtils.getParameterFromId(id);
        setLocation(location);
        setQuantity(parameter);
    }

    /**
     * @return dataSetNumber of this time series in the wdm file. Returns -1 if unknown.
     */
    public int getDataSetNumber() {
        return dataSetNumber;
    }

    /**
     * @param dataSetNumber of this time series in the wdm file.
     */
    public void setDataSetNumber(int dataSetNumber) {
        this.dataSetNumber = dataSetNumber;
    }
}
