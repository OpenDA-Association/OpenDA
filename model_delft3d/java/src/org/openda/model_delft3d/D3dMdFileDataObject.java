package org.openda.model_delft3d;

import org.openda.exchange.DoubleExchangeItem;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import static org.openda.model_delft3d.ModelDefinitionFile.fileKeys;

/**
 * Created by Theo on 12.04.2016.
 */
public class D3dMdFileDataObject implements IDataObject {

	protected HashMap<String, IExchangeItem> exchangeItems = new HashMap<>();

	public static final String Stanton = "Stantn";
	public static final String Dalton = "Dalton";
	public static final String D_H = "Dicouv";
	public static final String D_Z = "Dicoww";

	public static final String[] fileKeys = {Stanton, Dalton, D_H, D_Z};

	private File mdFile;
	private String outputFileName = null;
	private File workingDir = null;

	@java.lang.Override
	public String[] getExchangeItemIDs() {

		return exchangeItems.keySet().toArray(new String[exchangeItems.keySet().size()]);
	}

	@java.lang.Override
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {

		List<String> matchingExchangeItemIds = new ArrayList<>();
		for(IExchangeItem exchangeItem : exchangeItems.values())
			if(exchangeItem.getRole() == role) matchingExchangeItemIds.add(exchangeItem.getId());

		return matchingExchangeItemIds.toArray(new String[matchingExchangeItemIds.size()]);

	}

	@java.lang.Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {

		return exchangeItems.get(exchangeItemID);

	}

	@java.lang.Override
	public void initialize(File workingDir, String[] arguments) {

		if(arguments.length !=1 && arguments.length !=2) throw new RuntimeException("D3dMdFile DataObject must be initialised with 1 or 2 arguments: InputFilePath [OutputFilePath]");
		String inputFileName = arguments[0];
		this.outputFileName = inputFileName;
		if (arguments.length == 2) {
			this.outputFileName = arguments[1];
		}

		this.workingDir = workingDir;

		File mdFile = new File(workingDir, inputFileName);
		this.mdFile = mdFile;

		try {
			BufferedReader inputFileBufferedReader = new BufferedReader(new FileReader(mdFile));
			String line = inputFileBufferedReader.readLine();
			while (line != null) {
				String[] fields = line.split("= *");
				if (fields.length == 2) {
					String key = fields[0].trim();
					String value = fields[1].trim();
					for (int i = 0; i < fileKeys.length; i++) {
						if (key.equalsIgnoreCase(fileKeys[i])) {
							double valueAsDouble = Double.parseDouble(value);
//							System.out.println(key + " : " + value);
							exchangeItems.put(key, new DoubleExchangeItem(key, IPrevExchangeItem.Role.InOut, valueAsDouble));
						}
					}
				}
				line = inputFileBufferedReader.readLine();
			}
			inputFileBufferedReader.close();
		} catch (IOException e) {
			throw new RuntimeException("Could not read from " + mdFile.getAbsolutePath());
		}

	}

	@java.lang.Override
	public void finish() {

		String[] exchangeItemIDs = getExchangeItemIDs();

		try {
			FileWriter outputFile = new FileWriter(new File(workingDir, outputFileName));

			BufferedReader inputFileBufferedReader = new BufferedReader(new FileReader(mdFile));
			String line = inputFileBufferedReader.readLine();
			while (line != null) {
				String[] fields = line.split("= *");
					for (int i = 0; i < exchangeItemIDs.length; i++) {
						String key = fields[0].trim();
						if (key.equalsIgnoreCase(exchangeItemIDs[i])) {

							IExchangeItem exchangeItem = getDataObjectExchangeItem(exchangeItemIDs[i]);
							double valueAsdouble = exchangeItem.getValuesAsDoubles()[0];
							String valueAsString = String.format("%.7e",valueAsdouble);

							line = exchangeItemIDs[i] + " =  " + valueAsString;
						}
					}

				outputFile.write(line + "\n");
				line = inputFileBufferedReader.readLine();
			}

			inputFileBufferedReader.close();
			outputFile.close();

		}	catch (FileNotFoundException e1) {
			e1.printStackTrace();
		} catch (IOException e1) {
			e1.printStackTrace();
		}

	}
}
