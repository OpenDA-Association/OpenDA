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

import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IPrevExchangeItem.Role;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * DataObject for one WDM (Watershed Data Management) file.
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
 * The HSPF model can be installed as part of the BASINS package,
 * which is available from:
 * http://water.epa.gov/scitech/datait/models/basins/index.cfm
 *
 * @author Arno Kockx
 */
public class WdmTimeSeriesDataObject implements IDataObject {
	/**
	 * Wrapped WdmTimeSeriesIoObject.
	 */
	private WdmTimeSeriesIoObject wrappedIoObject = null;

	/**
	 * @param workingDir the working directory.
	 * @param arguments File pathname:
	 *                  The pathname of the file containing the data for this DataObject (relative to the working directory).
	 *                  Other arguments:
	 *                  The first argument should be the pathname of the wdm.dll file (relative to the working directory).
	 *                  The second argument should be the pathname of the message file (relative to working directory).
	 *                  The third argument should be the role of this DataObject. Role can be 'input' or 'output'.
	 *                  The fourth argument should be the timeZone that is used by the model (in hours with respect to GMT, between -12 and 12).
	 *                  For role INPUT the fifth and sixth arguments should be the ids of the startTime and endTime exchangeItems respectively,
	 *                  For role OUTPUT the fifth and sixth arguments should be respectively the startTime and endTime of the model run.
	 *                  The (optional) seventh and further arguments should be the location and parameter ids of the time series for which exchange items should be made,
	 *                  if no seventh and further arguments present then exchange items will be created for all time series in the file.
	 */
	public void initialize(File workingDir, String[] arguments) {
		//initialize role.
		if (arguments == null || arguments.length < 1) {
			throw new IllegalArgumentException(getClass().getSimpleName() + ": File pathname of the wdm file (relative to the working directory) not specified.");
		}

		String wdmTimeSeriesFileName = arguments[0];
		String[] otherArguments = new String[arguments.length - 1];
		System.arraycopy(arguments, 1, otherArguments, 0, otherArguments.length);
		wrappedIoObject = new WdmTimeSeriesIoObject();
		wrappedIoObject.initialize(workingDir, wdmTimeSeriesFileName, otherArguments);
	}

	public String[] getExchangeItemIDs() {
		List<String> exchangeItemIds = new ArrayList<>();
		for (IPrevExchangeItem exchangeItem : wrappedIoObject.getExchangeItems()) {
			exchangeItemIds.add(exchangeItem.getId());
		}
		return exchangeItemIds.toArray(new String[exchangeItemIds.size()]);
	}

	public String[] getExchangeItemIDs(Role role) {
		List<String> exchangeItemIds = new ArrayList<>();
		for (IPrevExchangeItem exchangeItem : wrappedIoObject.getExchangeItems()) {
			if (exchangeItem.getRole().equals(role)) {
				exchangeItemIds.add(exchangeItem.getId());
			}
		}
		return exchangeItemIds.toArray(new String[exchangeItemIds.size()]);
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemId) {
		for (IPrevExchangeItem exchangeItem : wrappedIoObject.getExchangeItems()) {
			//a WdmTimeSeriesIoObject returns WdmTimeSeriesExchangeItems that implement TimeSeries which implements both IPrevExchangeItem and IExchangeItem,
			//so here can cast IPrevExchangeItem to IExchangeItem.
			if (exchangeItem.getId().equals(exchangeItemId)) return (IExchangeItem) exchangeItem;
		}

		//if not found.
		return null;
	}

	public void finish() {
		if (wrappedIoObject != null) wrappedIoObject.finish();
	}
}
