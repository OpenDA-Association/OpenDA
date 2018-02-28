/* OpenDA v2.4.3 
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
import java.io.*;
import java.util.Locale;

/**
 * Delft3D 2d-field files reader/writer
 */
public class D3dField2DFile implements IoObjectInterface {

    private static String[] knownFileTypes = {ModelDefinitionFile.DEPTH, ModelDefinitionFile.ROUGHNESS};
    private ModelDefinitionFile modelDefinitionFile = null;
    private D3dField2DExchangeItem[] exchangeItems = null;
    private String fileKey = null;
    private int lineLength = 12;

    public void initialize(File workingDir, String mdFileName, String[] arguments) {

        if (arguments.length != 1) {
            throw new RuntimeException("Expected one argument, the 2D-field type: " +
                    ModelDefinitionFile.getKnownFileTypesString(knownFileTypes));
        }

        fileKey = arguments[0];
        ModelDefinitionFile.checkD3dFileArguments(fileKey, knownFileTypes);
        modelDefinitionFile = ModelDefinitionFile.getModelDefinitionFile(workingDir, mdFileName);
        File fieldFile = modelDefinitionFile.getFieldFile(fileKey, true);

        try {
            FileReader fileReader = new FileReader(fieldFile);
            BufferedReader inputFileBufferedReader = new BufferedReader(fileReader);

            if (fileKey.equals(ModelDefinitionFile.DEPTH)) {
                exchangeItems = new D3dField2DExchangeItem[1];
                exchangeItems[0] = readExchangeItem2D(inputFileBufferedReader, "depth");
            } else if (fileKey.equals(ModelDefinitionFile.ROUGHNESS)) {
                exchangeItems = new D3dField2DExchangeItem[2];
                exchangeItems[0] = readExchangeItem2D(inputFileBufferedReader, "roughness-u");
                exchangeItems[1] = readExchangeItem2D(inputFileBufferedReader, "roughness-v");
            }
            inputFileBufferedReader.close();
            fileReader.close();

        } catch (IOException e) {
            throw new RuntimeException("Could not read from " + fieldFile.getAbsolutePath());
        }
    }

    public IPrevExchangeItem[] getExchangeItems() {
        return exchangeItems;
    }

    public void finish() {

        boolean dataChanged = false;
        for (int i = 0; !dataChanged && i < exchangeItems.length; i++) {
            dataChanged = exchangeItems[i].getDataChanged();
        }

        if (dataChanged) {
            try {
                FileWriter fileWriter = new FileWriter(this.modelDefinitionFile.getFieldFile(fileKey, true));
                BufferedWriter outputFileBufferedWriter = new BufferedWriter(fileWriter);
                if (fileKey.equals(ModelDefinitionFile.DEPTH)) {
                    writeExchangeItem2D(outputFileBufferedWriter, exchangeItems[0].getValuesAsDoubles());
                } else if (fileKey.equals(ModelDefinitionFile.ROUGHNESS)) {
                    writeExchangeItem2D(outputFileBufferedWriter, exchangeItems[0].getValuesAsDoubles());
                    writeExchangeItem2D(outputFileBufferedWriter, exchangeItems[1].getValuesAsDoubles());
                }
                outputFileBufferedWriter.close();
                fileWriter.close();
            } catch (IOException e) {
                throw new RuntimeException("Error writing file " + modelDefinitionFile.getFieldFile(fileKey, false));
            }
        }
        modelDefinitionFile = null;
        exchangeItems = null;
        fileKey = null;
    }

    private D3dField2DExchangeItem readExchangeItem2D(BufferedReader inputFileBufferedReader, String exchangeItemId) throws IOException {

        int mmax = modelDefinitionFile.getMmax();
        int nmax = modelDefinitionFile.getNmax();
        int nmmax = nmax * mmax;
        double[] values = new double[nmax * mmax];
        int index = 0;
        String line = inputFileBufferedReader.readLine();
        while (line != null && index < nmmax) {
            String[] fields = line.split("[\t ]+");
            for (String field : fields) {
                if (field.length() > 0) {
                    values[index++] = Double.parseDouble(field);
                }
            }
            if (index < nmmax) {
                line = inputFileBufferedReader.readLine();
            }
        }
        D3dField2D d3dField2D = new D3dField2D(mmax, nmax, values);
        return new D3dField2DExchangeItem(exchangeItemId, d3dField2D);
    }

    private void writeExchangeItem2D(BufferedWriter outputFileBufferedWriter, double[] values) throws IOException {

        Locale locale = new Locale("EN");
        String floatValueFormat = "%13.5e";

        int mmax = modelDefinitionFile.getMmax();
        int nmax = modelDefinitionFile.getNmax();
        int index = 0;
        for (int n = 0; n < nmax; n++) {
            for (int m = 0; m < mmax; m++) {
                if (m > 0 && m % lineLength == 0) {
                    outputFileBufferedWriter.newLine();
                }
                outputFileBufferedWriter.write(String.format(locale, floatValueFormat, values[index++]));
            }
            outputFileBufferedWriter.newLine();
        }
    }
}
