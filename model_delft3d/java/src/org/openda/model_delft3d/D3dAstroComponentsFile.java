package org.openda.model_delft3d;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

/**
 * D3D astro file (bca) or correction file (cor)
 */
public class D3dAstroComponentsFile {

    private List<D3dAstroStation> astroStations = new ArrayList<D3dAstroStation>();

    public  D3dAstroComponentsFile(File componentsFile) {
        try {
            FileReader fileReader = new FileReader(componentsFile);
            BufferedReader inputFileBufferedReader = new BufferedReader(fileReader);

            String line = inputFileBufferedReader.readLine();
            D3dAstroStation d3dAstroStation = null;
            while (line != null) {
                String[] fields = line.trim().split(" +");
                if (line.trim().toUpperCase().startsWith("A0")) {
                    if (d3dAstroStation == null) {
                        throw new RuntimeException("Invalid astro/corr file (no station found), " + componentsFile.getAbsolutePath());
                    }
                    if (fields.length < 2) {
                        throw new RuntimeException("Invalid astro/corr line:\n" + line + " in " + componentsFile.getAbsolutePath());
                    }
                    d3dAstroStation.setA0(fields[1]);
                } else {
                    if (fields.length == 1) {
                        d3dAstroStation = new D3dAstroStation(fields[0]);
                        astroStations.add(d3dAstroStation);
                    } else if (fields.length == 3) {
                        if (d3dAstroStation == null) {
                            throw new RuntimeException("Invalid astro/corr file (no station found), " + componentsFile.getAbsolutePath());
                        }
                        d3dAstroStation.addComponent(new D3dAstroComponent(fields[0], fields[1],fields[2]));
                    } else {
                        throw new RuntimeException("Invalid astro/corr line:\n" + line + " in " + componentsFile.getAbsolutePath());
                    }
                }
                line = inputFileBufferedReader.readLine();
            }

            inputFileBufferedReader.close();
            fileReader.close();

        } catch (IOException e) {
            throw new RuntimeException("Could not read from " + componentsFile.getAbsolutePath());
        }
    }

    public List<D3dAstroStation> getAstroStations() {
        return astroStations;
    }

    public void write(File outFile) {

        boolean dataChanged = false;
        for (int i = 0; !dataChanged && i < astroStations.size(); i++) {
            dataChanged = astroStations.get(i).getDataChanged();
        }

        if (dataChanged) {
            try {

                FileWriter fileWriter = new FileWriter(outFile);
                BufferedWriter outputFileBufferedWriter = new BufferedWriter(fileWriter);
                for (D3dAstroStation astroStation : this.astroStations) {
                    outputFileBufferedWriter.write(astroStation.getId());
                    outputFileBufferedWriter.newLine();
                    outputFileBufferedWriter.write(astroStation.toString());
                    outputFileBufferedWriter.newLine();
                    for (D3dAstroComponent astroComponent : astroStation.getComponents()) {
                        outputFileBufferedWriter.write(astroComponent.toString());
                        outputFileBufferedWriter.newLine();
                    }
                }
                outputFileBufferedWriter.close();
                fileWriter.close();
            } catch (IOException e) {
                throw new RuntimeException("Error writing file " + outFile.getAbsolutePath());
            }
        }
    }

    public void addCorrections(D3dAstroComponentsFile corComponents) {
        for (D3dAstroStation astroStation : astroStations) {
            for (D3dAstroStation corStation : corComponents.getAstroStations()) {
                if (corStation.getId().equalsIgnoreCase(astroStation.getId())) {
                    for (D3dAstroComponent astroComponent : astroStation.getComponents()) {
                        for (D3dAstroComponent corComponent : corStation.getComponents()) {
                            if (corComponent.getId().equalsIgnoreCase(astroComponent.getId())) {
                                astroComponent.setCorrection(corComponent);
                            }
                        }
                    }
                }
            }
        }
    }
}
