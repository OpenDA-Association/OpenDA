package org.openda.model_damflow;

import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.*;

import java.io.*;
import java.util.ArrayList;
import java.util.Locale;

/**
 * Created by IntelliJ IDEA.
 * User: Julius Sumihar
 * Date: 3-7-12
 * Time: 10:17
 * To change this template use File | Settings | File Templates.
 */
public class DupuitBHOFile implements IDataObject {

	private File dupuitBHOFile;
	private ArrayList<String> lines = new ArrayList<String>();
	private IExchangeItem[] exchangeItems;
	//TODO: exchange item IDs should be improved, especially for riverwaterlevel. So far it is location0.riverwaterlevel.
	// Also for .O and .H files these should be improved. SOme unique id's should be used for each location,
	// and some logical names should be used. For the moment, the id's are assigned to the locations according
	// to their order of appearance in the respective files. This requires that the order of appearance in both
	// .O and .H files should be consistent.
	private final String[] fileExtensions = new String[]{".B",".H",".O"};
	private final String[] fileHeaders = new String[]{"riverwaterlevel","monitorsample"};
	private final String[] quantities = new String[]{"riverwaterlevel","hydraulichead"};
	private int fileTypeId = Integer.MAX_VALUE;

	public void initialize(File workingDir, String fileName, String[] arguments) {
		this.dupuitBHOFile = new File(workingDir, fileName);
		readDupuitHOFile();
	}

	private void writeDupuitBHOFile(){
		Locale locale = new Locale("EN");
		String dupuitCFormat = "%+6.4e";
		FileWriter fileWriter;
	    try {
			File tempFile = new File(this.dupuitBHOFile.getParent(), "dupuit"+fileExtensions[fileTypeId]+".temp");
			fileWriter = new FileWriter(tempFile);
			BufferedWriter outputFileBufferedWriter = new BufferedWriter(fileWriter);

			outputFileBufferedWriter.write(fileHeaders[fileTypeId]);
			outputFileBufferedWriter.newLine();
			int nStateLines = this.exchangeItems.length;
			if (!this.dupuitBHOFile.getName().contains(fileExtensions[0])){
				outputFileBufferedWriter.write("nline "+nStateLines);
				outputFileBufferedWriter.newLine();
			}
			for (int iStateLine=0; iStateLine<nStateLines; iStateLine++){
				DupuitBHOFileExchangeItem damFlowTimeSeries = (DupuitBHOFileExchangeItem) this.exchangeItems[iStateLine];
				outputFileBufferedWriter.write(damFlowTimeSeries.getNx()+"\t"+ damFlowTimeSeries.getB()+"\t"+ damFlowTimeSeries.getY());
				outputFileBufferedWriter.newLine();
				int nx = damFlowTimeSeries.getNx();
				for (int ix=0; ix<nx; ix++){
					double time = damFlowTimeSeries.getTimes()[ix];
					double phi = damFlowTimeSeries.getValuesAsDoubles()[ix];
					outputFileBufferedWriter.write(String.format(locale,dupuitCFormat,time)+" "+String.format(locale,dupuitCFormat,phi));
					outputFileBufferedWriter.newLine();
				}
			}
			outputFileBufferedWriter.close();
			BBUtils.copyFile(tempFile, this.dupuitBHOFile);
			tempFile.deleteOnExit();
		} catch (IOException e) {
			throw new RuntimeException("Could not write to " + this.dupuitBHOFile.getAbsolutePath());
		}
	}

	private void readDupuitHOFile() {
		String lineFields[];
		String line;
		try {
			BufferedReader dupuitHOFileBufferedReader = new BufferedReader(new FileReader(this.dupuitBHOFile));
			if (this.dupuitBHOFile.getName().contains(fileExtensions[0])){
				fileTypeId = 0;
			} else if (this.dupuitBHOFile.getName().contains(fileExtensions[1]) || this.dupuitBHOFile.getName().contains(fileExtensions[2])){
				fileTypeId = 1;
			} else {
				throw new RuntimeException("Unknown DAMFlow timeseries file extension:"+this.dupuitBHOFile.getAbsolutePath());
			}
			line = dupuitHOFileBufferedReader.readLine();
			if (!line.toLowerCase().contains(fileHeaders[fileTypeId])) {
				throw new RuntimeException("DAMFlow timeseries file does not contain keyword '"+fileHeaders[fileTypeId]+"':"+this.dupuitBHOFile.getAbsolutePath());
			} else {
				line = dupuitHOFileBufferedReader.readLine();
			}
			while (line != null) {
				lines.add(line);
				line = dupuitHOFileBufferedReader.readLine();
			}
			dupuitHOFileBufferedReader.close();
		} catch (IOException e){
			throw new RuntimeException("Could not read DAMFlow timeseries file "+this.dupuitBHOFile.getAbsolutePath());
		}

		int nline = 1;
		int iLine = 0;
		String quantity = quantities[fileTypeId];
		if (fileTypeId !=0){
			line = lines.get(0);
			lineFields = line.trim().split("[ \t]+");
			nline = Integer.parseInt(lineFields[1]);
			iLine = 1;
		}
		exchangeItems = new DupuitBHOFileExchangeItem[nline];
		for (int iStateLine=0; iStateLine<nline; iStateLine++){
			line = lines.get(iLine);
			lineFields = line.trim().split("[ \t]+");
			int nTime = Integer.parseInt(lineFields[0]);
			int b = Integer.parseInt(lineFields[1]);
			int y = Integer.parseInt(lineFields[2]);
			double[] time = new double[nTime];
			double[] phi = new double[nTime];
			iLine++;
			for (int ix=0; ix<nTime; ix++){
				line = lines.get(iLine);
				lineFields = line.trim().split("[ \t]+");
				time[ix] = Double.parseDouble(lineFields[0]);
				phi[ix] = Double.parseDouble(lineFields[1]);
				iLine++;
			}
			String exchangeItemId = "location"+iStateLine+"."+quantity;
			if (fileTypeId ==0){
				exchangeItemId = quantity;
			}
			exchangeItems[iStateLine] = new DupuitBHOFileExchangeItem(nTime,b,y,exchangeItemId,time,phi);
		}
	}

	@Override
	public String[] getExchangeItemIDs() {
		String[] exchangeItemIDs = new String[exchangeItems.length];
		for (int i=0; i<exchangeItemIDs.length; i++){
			exchangeItemIDs[i] = exchangeItems[i].getId();
		}
		return exchangeItemIDs;
	}

	@Override
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitBHOFile - Method Name : getExchangeItemIDs");
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		int i;
		for (i=0; i<this.exchangeItems.length;i++){
			if (exchangeItemID.contentEquals(this.exchangeItems[i].getId())){
				break;
			}
		}
		return this.exchangeItems[i];
	}

	@Override
	public void finish() {
		if  (this.dupuitBHOFile.getName().contains(fileExtensions[0])) {
			writeDupuitBHOFile();
		} else {
			// do nothing, do not rewrite file.
			// TODO: check if it is correct that we don't need to rewrite monitoring files
		}
	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		String fileName = arguments[0];
		String[] remainingArguments = new String[arguments.length-1];
		System.arraycopy(arguments, 1, remainingArguments, 0, remainingArguments.length);
		initialize(workingDir, fileName, remainingArguments);
	}

}
