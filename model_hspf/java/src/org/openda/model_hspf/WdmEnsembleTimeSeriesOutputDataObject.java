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
import org.openda.interfaces.IEnsembleDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.utils.Results;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * DataObject for multiple WDM (Watershed Data Management) files that together form an ensemble.
 * Each wdm file gets a different ensemble member index.
 *
 * @author Arno Kockx
 */
public class WdmEnsembleTimeSeriesOutputDataObject implements IDataObject, IEnsembleDataObject {
	/**
	 * Wrapped dataObjects. One WdmTimeSeriesDataObject for each ensembleMemberIndex.
	 */
	private ArrayList<WdmTimeSeriesDataObject> wrappedDataObjects = new ArrayList<>();

	/**
	 * @param workingDir the working directory.
	 * @param arguments File pathname prefix:
	 *                  This should be the prefix for the pathnames of the wdm files containing the data for this DataObject (relative to the working directory).
	 *                  The file names are constructed by adding to the given prefix the following postfix: "<ensembleMemberNumber>.wdm"  where <ensembleMemberNumber> is an integer starting at 1.
	 *                  Each file should contain the same data for a different ensemble member. The number of ensemble members is determined by the number of files that are present.
	 *                  Other arguments:
	 *                  The first argument should be the path of the wdm.dll file (relative to the working directory).
	 *                  The second argument should be the path of the message file (relative to working directory).
	 *                  The third argument should be the role of this DataObject. Role must be 'output'.
	 *                  The fourth argument should be the timeZone that is used by the model (in hours with respect to GMT, between -12 and 12).
	 *                  The fifth and sixth arguments should be respectively the startTime and endTime of the model run.
	 *                  The (optional) seventh and further arguments should be the location and parameter ids of the time series for which exchange items should be made,
	 *                  if no seventh and further arguments present then exchange items will be created for all time series in the files.
	 */
	public void initialize(File workingDir, String[] arguments) {
		//initialize role.
		if (arguments == null || arguments.length < 1) {
			throw new IllegalArgumentException(getClass().getSimpleName() + ": File pathname prefix for the names of the wdm files not specified.");
		}
		//the error messages ignore the first argument (fileName), because in config or in DataCopier the fileName is a separate element and not an argument.
		if (arguments.length < 2) {
			throw new IllegalArgumentException(getClass().getSimpleName() + ": No arguments specified. The first argument should be the path of the wdm.dll file (relative to working directory).");
		}
		if (arguments.length < 3) {
			throw new IllegalArgumentException(getClass().getSimpleName() + ": No arguments specified. The second argument should be the path of the message file (relative to working directory).");
		}
		if (arguments.length < 4) {
			throw new IllegalArgumentException(getClass().getSimpleName() + ": No role argument specified. The third argument should be the role of this DataObject. Role must be 'output'.");
		}
		Role role = WdmUtils.initializeRole(arguments[3]);
		if (role == IPrevExchangeItem.Role.Input) {
			throw new UnsupportedOperationException(getClass().getSimpleName() + " not implemented for role input.");
		}

		createWrappedDataObjects(workingDir, arguments);
	}

	private void createWrappedDataObjects(File workingDir, String[] arguments) {
		wrappedDataObjects.clear();

		//create WdmTimeSeriesDataObjects.
		String wdmTimeSeriesFilePrefix = arguments[0];
		int ensembleMemberIndex = 0;
		while (true) {
			String wdmTimeSeriesFileName = wdmTimeSeriesFilePrefix + (ensembleMemberIndex + 1) + ".wdm";
			File wdmTimeSeriesFile = new File(workingDir, wdmTimeSeriesFileName);
			if (!wdmTimeSeriesFile.exists()) {
				break;
			}

			WdmTimeSeriesDataObject dataObject = new WdmTimeSeriesDataObject();
			String[] otherArguments = new String[arguments.length];
			otherArguments[0] = wdmTimeSeriesFileName;
			System.arraycopy(arguments, 1, otherArguments, 1, otherArguments.length - 1);
			dataObject.initialize(workingDir, otherArguments);
			wrappedDataObjects.add(dataObject);

			ensembleMemberIndex++;
		}
		int ensembleMemberCount = ensembleMemberIndex;
		if (ensembleMemberCount == 0) {
			throw new IllegalArgumentException(this.getClass().getSimpleName() + ": No wdm time series files found with prefix '" + wdmTimeSeriesFilePrefix
					+ "' and postfix <ensembleMemberNumber>.wdm in working directory " + workingDir != null ? workingDir.getAbsolutePath() : "");
		}
		Results.putMessage(getClass().getSimpleName() + ": Found wdm time series files for " + ensembleMemberCount + " ensemble members.");
	}

	public String[] getExchangeItemIDs() {
		//ignore ensemble exchange items.
		return new String[0];
	}

	public String[] getExchangeItemIDs(Role role) {
		//ignore ensemble exchange items.
		return new String[0];
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemId) {
		String[] ensembleExchangeItemIds = getEnsembleExchangeItemIds();
		if (Arrays.asList(ensembleExchangeItemIds).contains(exchangeItemId)) {//if ensemble exchange item.
			throw new IllegalStateException(getClass().getSimpleName() + ".getDataObjectExchangeItem: exchange item with id '"
					+ exchangeItemId + "' is an ensemble exchange item. Call method getDataObjectExchangeItem(String exchangeItemId, int ensembleMemberIndex) instead.");
		}
		//this object contains only ensemble exchange items.
		return null;
	}

	/**
	 * Get the ensemble member indices of the ensemble exchange items.
	 * The ensemble member indices must be the same for all ensemble exchange items.
	 * This should ignore any exchange items for which there are no ensemble members available.
	 * Should return int[0] if there are no ensemble members.
	 *
	 * @return array of ensemble member indices.
	 */
	public int[] getEnsembleMemberIndices() {
		int[] indices = new int[wrappedDataObjects.size()];
		for (int n = 0; n < indices.length; n++) {
			//start at 1.
			indices[n] = n + 1;
		}
		return indices;
	}

	/**
	 * Get the identifiers of the ensemble exchange items.
	 * This should ignore any exchange items for which there are no ensemble members available.
	 * Should return String[0] if there are no matching ensemble items.
	 *
	 * @return array of ensemble exchange item identifiers.
	 */
	public String[] getEnsembleExchangeItemIds() {
		if (wrappedDataObjects.isEmpty()) return new String[0];

		//this code assumes that all wrappedDataObjects have exactly the same exchangeItems.
		return wrappedDataObjects.get(0).getExchangeItemIDs();
	}

	/**
	 * Get the ensemble exchange item specified by the given exchangeItemId and ensembleMemberIndex.
	 * If the given ensembleMemberIndex does not exist, then this method should throw an IllegalStateException.
	 * If there are no ensemble members available for the given exchangeItem, then it should throw an
	 * IllegalStateException stating that the equivalent method without the argument "int ensembleMemberIndex" must be called instead.
	 * Returns null if no ensemble exchange item with the given exchangeItemId is found.
	 *
	 * @param exchangeItemId      ensemble exchange item identifier.
	 * @param ensembleMemberIndex ensemble member index.
	 * @return the requested ensemble exchange item.
	 */
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemId, int ensembleMemberIndex) {
		if (wrappedDataObjects.isEmpty()) return null;
		if (ensembleMemberIndex < 1 || ensembleMemberIndex > wrappedDataObjects.size()) {
			throw new IllegalStateException(getClass().getSimpleName() + ".getDataObjectExchangeItem: ensembleMemberIndex " + ensembleMemberIndex + " does not exist.");
		}

		return wrappedDataObjects.get(ensembleMemberIndex - 1).getDataObjectExchangeItem(exchangeItemId);
	}

	public void finish() {
		for (IDataObject wrappedDataObject : wrappedDataObjects) {
			wrappedDataObject.finish();
		}
	}
}
