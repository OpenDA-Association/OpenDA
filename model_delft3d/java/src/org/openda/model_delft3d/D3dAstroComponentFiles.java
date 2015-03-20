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
