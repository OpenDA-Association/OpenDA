/*
* Copyright (c) 2021 OpenDA Association 
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


package org.openda.blackbox.wrapper;

import org.openda.exchange.AbstractDataObject;
import org.openda.exchange.DoublesExchangeItem;
import org.openda.interfaces.IExchangeItem;

import java.io.*;
import java.util.ArrayList;
import java.util.Locale;

/**
 * Dummy Astro DataObject for testing purposes
 */
public class DummyState extends AbstractDataObject {

	public static final String STATE = "state";
	private File stateFile = null;

    @Override
    public void initialize(File workingDir, String[] arguments) {
        stateFile = new File(workingDir, arguments[0]);
        ArrayList<String> lines = new ArrayList<>();
        try {
            BufferedReader bufferedReader = new BufferedReader(new FileReader(stateFile));
            String line = bufferedReader.readLine();
            while (line != null) {
                lines.add(line.trim());
                line = bufferedReader.readLine();
            }
            bufferedReader.close();
        } catch (FileNotFoundException e) {
            throw new RuntimeException(this.getClass().getName() +": file not found: " +
                    stateFile.getAbsolutePath());
        } catch (IOException e) {
            throw new RuntimeException(this.getClass().getName() + "Could not read from file: " +
                    stateFile.getAbsolutePath());
        }
        double[] values = new double[lines.size()];
        for (int i = 0, valuesLength = values.length; i < valuesLength; i++) {
            values[i] = Double.parseDouble(lines.get(i));
        }
		exchangeItems.put(STATE, new DoublesExchangeItem(STATE, IExchangeItem.Role.InOut, values));
    }

    @Override
    public void finish() {
        Locale locale = new Locale("EN");
        try {
            FileWriter fileWriter = new FileWriter(stateFile);
            BufferedWriter outputFileBufferedWriter = new BufferedWriter(fileWriter);
            for (double value : exchangeItems.get(STATE).getValuesAsDoubles()) {
                outputFileBufferedWriter.write(String.format(locale, "%5.2f", value));
                outputFileBufferedWriter.newLine();
            }
            outputFileBufferedWriter.close();
            fileWriter.close();
        } catch (IOException e) {
            throw new RuntimeException("Error writing file " + stateFile.getAbsolutePath());
        }
    }
}
