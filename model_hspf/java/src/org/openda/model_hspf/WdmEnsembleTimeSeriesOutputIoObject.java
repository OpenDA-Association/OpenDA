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

package org.openda.model_hspf;

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.utils.Results;

import java.io.File;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * IoObject for multiple WDM (Watershed Data Management) files that together form an ensemble.
 * Each wdm file gets a different ensemble member id.
 *
 * @author Arno Kockx
 */
public class WdmEnsembleTimeSeriesOutputIoObject implements IoObjectInterface {
	/**
	 * Map with wrapped ioObjects. One WdmTimeSeriesIoObject for each ensembleMemberId.
	 */
	private Map<String, IoObjectInterface> wrappedIoObjects = new LinkedHashMap<String, IoObjectInterface>();
	/**
	 * Contains all exchangeItems of all wrapped ioObjects. These exchangeItems are wrapped only to add an ensembleMemberId to their ids.
	 */
	private List<IPrevExchangeItem> wrappedExchangeItems = new ArrayList<IPrevExchangeItem>();

	/**
	 * @param workingDir the working directory.
	 * @param fileName the prefix for the names of the files containing the data for this IoObject (relative to the working directory).
	 *                 The file names are constructed by adding to the given prefix the following postfix: "<ensembleMemberNumber>-out.wdm"  where <ensembleMemberNumber> is an integer starting at 1.
	 *                 Each file should contain the same data for a different ensemble member. The number of ensemble members is determined by the number of files that are present.
	 * @param arguments the first argument should be the path of the wdm.dll file (relative to the working directory),
	 *                  the second argument should be the path of the message file (relative to working directory),
	 *                  the third argument should be the role of this IoObject. Role must be 'output',
	 *                  the fourth argument should be the timeZone that is used by the model (in hours with respect to GMT, between -12 and 12),
	 *                  for role OUTPUT the fifth and sixth arguments should be respectively the startTime and endTime of the model run,
	 *                  the (optional) seventh and further arguments should be the location and parameter ids of the time series for which exchange items should be made,
	 *                  if no seventh and further arguments present then exchange items will be created for all time series in the files.
	 */
	public void initialize(File workingDir, String fileName, String[] arguments) {
		//initialize role.
		if (arguments == null || arguments.length < 1) {
			throw new IllegalArgumentException(getClass().getSimpleName() + ": No arguments specified. The first argument should be the path of the wdm.dll file (relative to working directory).");
		}
		if (arguments.length < 2) {
			throw new IllegalArgumentException(getClass().getSimpleName() + ": No arguments specified. The second argument should be the path of the message file (relative to working directory).");
		}
		if (arguments.length < 3) {
			throw new IllegalArgumentException(getClass().getSimpleName() + ": No role argument specified. The third argument should be the role of this IoObject. Role can be 'input' or 'output'.");
		}
		Role role = WdmUtils.initializeRole(arguments[2]);
		if (role == IPrevExchangeItem.Role.Input) {
			throw new UnsupportedOperationException(getClass().getSimpleName() + " not implemented for role input.");
		}

		createWrappedIoObjects(workingDir, fileName, arguments);
		wrapExchangeItems();
	}

	private void createWrappedIoObjects(File workingDir, String wdmTimeSeriesFilePrefix, String[] arguments) {
		wrappedIoObjects.clear();

		//create WdmTimeSeriesIoObjects.
		int ensembleMemberIndex = 0;
		while (true) {
			String wdmTimeSeriesFileName = wdmTimeSeriesFilePrefix + (ensembleMemberIndex + 1) + "-out.wdm";
			File wdmTimeSeriesFile = new File(workingDir, wdmTimeSeriesFileName);
			if (!wdmTimeSeriesFile.exists()) {
				break;
			}

			WdmTimeSeriesIoObject ioObject = new WdmTimeSeriesIoObject();
			ioObject.initialize(workingDir, wdmTimeSeriesFileName, arguments);
			wrappedIoObjects.put(String.valueOf(ensembleMemberIndex + 1), ioObject);

			ensembleMemberIndex++;
		}
		int ensembleMemberCount = ensembleMemberIndex;
		if (ensembleMemberCount == 0) {
			throw new IllegalArgumentException(this.getClass().getSimpleName() + ": No wdm time series files found with prefix '" + wdmTimeSeriesFilePrefix
					+ "' and postfix <ensembleMemberNumber>-out.wdm in directory " + workingDir.getAbsolutePath());
		}
		Results.putMessage(getClass().getSimpleName() + ": Found wdm time series files for " + ensembleMemberCount + " ensemble members.");
	}

	/**
	 * Wrap all exchangeItems of the wrapped ioObjects.
	 */
	private void wrapExchangeItems() {
		wrappedExchangeItems.clear();

		for (Map.Entry<String, IoObjectInterface> entry : wrappedIoObjects.entrySet()) {
			String ensembleMemberId = entry.getKey();
			IoObjectInterface wrappedIoObject = entry.getValue();
			for (IPrevExchangeItem wrappedExchangeItem : wrappedIoObject.getExchangeItems()) {
				wrappedExchangeItems.add(new EnsembleMemberExchangeItem(wrappedExchangeItem, ensembleMemberId));
			}
		}
	}

	public IPrevExchangeItem[] getExchangeItems() {
		return wrappedExchangeItems.toArray(new IPrevExchangeItem[wrappedExchangeItems.size()]);
	}

	public void finish() {
		for (IoObjectInterface wrappedIoObject : wrappedIoObjects.values()) {
			wrappedIoObject.finish();
		}
	}
}
