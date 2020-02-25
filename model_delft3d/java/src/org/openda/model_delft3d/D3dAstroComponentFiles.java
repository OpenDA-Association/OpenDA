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

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * Combination of D3D astro file (bca) and its corresponding correction file (cor)
 */
public class D3dAstroComponentFiles implements IDataObject {

    ModelDefinitionFile modelDefinitionFile = null;
    D3dAstroComponentsFile bcaComponents = null;
    D3dAstroComponentsFile corComponents = null;

    private List<D3dAstroExchangeItem> exchangeItems = new ArrayList<D3dAstroExchangeItem>();
	private String[] ids;

	@Override
	public void initialize(File workingDir, String[] arguments) {
        if (arguments != null && arguments.length > 1) {
            throw new RuntimeException("No  arguments except filename expected");
        }

        modelDefinitionFile = ModelDefinitionFile.getModelDefinitionFile(workingDir, arguments[0]);
        File bcaFile = modelDefinitionFile.getFieldFile(ModelDefinitionFile.BC_ASTRONOMIC, true);
        File corFile = modelDefinitionFile.getFieldFile(ModelDefinitionFile.BC_ASTRO_CORR, false);

        bcaComponents = new D3dAstroComponentsFile(bcaFile);
        if (corFile != null) {
            corComponents = new D3dAstroComponentsFile(corFile);
            bcaComponents.addCorrections(corComponents);
        }
        for (D3dAstroStation astroStation : bcaComponents.getAstroStations()) {
            exchangeItems.add(new D3dAstroExchangeItem(astroStation));
            for (D3dAstroComponent astroComponent : astroStation.getComponents()) {
                exchangeItems.add(new D3dAstroExchangeItem(
                        astroStation.getId(), D3dAstroExchangeItem.amplitudeString, astroComponent));
                exchangeItems.add(new D3dAstroExchangeItem(
                        astroStation.getId(), D3dAstroExchangeItem.phaseString, astroComponent));
            }
        }
		int size = exchangeItems.size();
		ids = new String[size];
		for (int i = 0; i < size; i++) {
			ids[i] = exchangeItems.get(i).getId();
		}
    }

	@Override
	public String[] getExchangeItemIDs() {
		return ids;
	}

	@Override
	public String[] getExchangeItemIDs(IExchangeItem.Role role) {
		return getExchangeItemIDs();
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		for (int i = 0; i < ids.length; i++) {
			if (ids[i].equals(exchangeItemID)) return exchangeItems.get(i);
		}
		return null;
	}

	@Override
	public void finish() {
        bcaComponents.write(modelDefinitionFile.getFieldFile(ModelDefinitionFile.BC_ASTRONOMIC, true));
        if (corComponents != null) {
            corComponents.write(modelDefinitionFile.getFieldFile(ModelDefinitionFile.BC_ASTRO_CORR, true));
        }
        modelDefinitionFile = null;
        bcaComponents = null;
        corComponents = null;
    }
}
