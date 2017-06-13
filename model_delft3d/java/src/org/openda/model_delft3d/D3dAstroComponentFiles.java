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

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.interfaces.IPrevExchangeItem;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * Combination of D3D astro file (bca) and its corresponding correction file (cor)
 */
public class D3dAstroComponentFiles implements IoObjectInterface {

    ModelDefinitionFile modelDefinitionFile = null;
    D3dAstroComponentsFile bcaComponents = null;
    D3dAstroComponentsFile corComponents = null;

    private List<D3dAstroExchangeItem> exchangeItems = new ArrayList<D3dAstroExchangeItem>();

    public void initialize(File workingDir, String mdFileName, String[] arguments) {
        if (arguments != null && arguments.length > 0) {
            throw new RuntimeException("No  arguments expected");
        }

        modelDefinitionFile = ModelDefinitionFile.getModelDefinitionFile(workingDir, mdFileName);
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
    }

    public IPrevExchangeItem[] getExchangeItems() {
        return exchangeItems.toArray(new IPrevExchangeItem[exchangeItems.size()]);
    }

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
