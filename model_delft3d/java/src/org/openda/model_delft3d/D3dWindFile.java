/* MOD_V2.0
 * Copyright (c) 2016 OpenDA Association
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

import org.openda.exchange.ArrayExchangeItem;
import org.openda.exchange.TimeInfo;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.wrapper_utils.io.SpaceVaryingWindAndPressureFile;
import java.io.*;
import java.util.Arrays;
import java.util.List;

/**
 * Delft3D 2d-field wind files reader/writer.
 *
 * Supported capabilities:
 *    Space varying wind on a separate curvilinear grid. (*.amu + *.amv + *.grd)
 */
public class D3dWindFile implements IDataObject {

	private static List<String> supportedFieldTypes = Arrays.asList(ModelDefinitionFile.WINDGU, ModelDefinitionFile.WINDGV); // TODO: windu, windv
	private ModelDefinitionFile modelDefinitionFile = null;
	private String fieldType = null;
	private ArrayExchangeItem windExchangeItem = null;
	SpaceVaryingWindAndPressureFile svwpFile = null;

	public void initialize(File workingDirectory, String[] arguments) {

		if (arguments.length != 2) {
			throw new RuntimeException("Please specify a mdf-filename and a wind field type as arguments. Supported types are: " + supportedFieldTypes);
		}

		String mdfFileName = arguments[0];

		fieldType = arguments[1];
		if (!(supportedFieldTypes.contains(fieldType))) {
			throw new RuntimeException("Unrecognised wind field type specified as argument, choose from: " + supportedFieldTypes);
		}

		modelDefinitionFile = ModelDefinitionFile.getModelDefinitionFile(workingDirectory, mdfFileName);

		File windFile = modelDefinitionFile.getFieldFile(fieldType, true);

		svwpFile = new SpaceVaryingWindAndPressureFile(windFile);

		if (svwpFile.getNQuantity() != 1) {
			throw new RuntimeException("Space varying wind and pressure " + windFile.getAbsolutePath() + "file should contain one quantity, found: " + svwpFile.getNQuantity());
		}

		String itemName = "";
		if (fieldType.equals(ModelDefinitionFile.WINDGU)) {
			itemName = "windgu";
		} else if (fieldType.equals(ModelDefinitionFile.WINDGV)) {
			itemName = "windgv";
		}
		windExchangeItem = new ArrayExchangeItem(itemName, IPrevExchangeItem.Role.InOut);
		windExchangeItem.setTimeInfo(new TimeInfo(svwpFile.getTimes()));
		windExchangeItem.setValuesAsDoubles(svwpFile.getValues());
	}

	public String[] getExchangeItemIDs() {
		return new String[] {windExchangeItem.getId()};
	}

	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		if (windExchangeItem.getRole().equals(role)) {
			return new String[] {windExchangeItem.getId()};
		}
		return new String[0];
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		if (exchangeItemID.equals(windExchangeItem.getId())) {
			return windExchangeItem;
		}
		return null;
	}

	public void finish() {
		svwpFile.writeFile(windExchangeItem.getValuesAsDoubles());
	}
}
