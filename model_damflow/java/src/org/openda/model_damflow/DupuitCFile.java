package org.openda.model_damflow;

import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.*;

import java.io.*;
import java.util.ArrayList;
import java.util.Locale;

/**
 * Created by IntelliJ IDEA.
 * User: Julius Sumihar
 * Date: 29-6-12
 * Time: 13:48
 * To change this template use File | Settings | File Templates.
 */
public class DupuitCFile implements IDataObject, ITimeInfo {
	private File dupuitCFile;
	private ArrayList<String> lines = new ArrayList<String>();
	private DupuitCFileData dupuitCFileData;
	private IExchangeItem exchangeItem;
	private String[] exchangeItemId = new String[]{"grid.hydraulichead"};
	private int numberOfHydraulicHeads = 0;

	public void initialize(File workingDir, String fileName, String[] arguments) {
		this.dupuitCFile = new File(workingDir, fileName);
		readDupuitCFile();
		exchangeItem = new DupuitCFileExchangeItem(exchangeItemId[0],this);
	}

	private void writeDupuitCFile(){
		Locale locale = new Locale("EN");
		String dupuitCFormat = "%+6.4e";
		FileWriter fileWriter;
	    try {
			File tempFile = new File(this.dupuitCFile.getParent(), "dupuitC.temp");
			fileWriter = new FileWriter(tempFile);
			BufferedWriter outputFileBufferedWriter = new BufferedWriter(fileWriter);

			outputFileBufferedWriter.write("piezolines");
			outputFileBufferedWriter.newLine();
			outputFileBufferedWriter.write("time "+String.format(locale,"%8.6e",this.dupuitCFileData.time)+" nline "+this.dupuitCFileData.nline);
			outputFileBufferedWriter.newLine();
			int nStateLines = this.dupuitCFileData.stateLines.length;
			for (int iStateLine=0; iStateLine<nStateLines; iStateLine++){
				StateLine stateLine = this.dupuitCFileData.stateLines[iStateLine];
				outputFileBufferedWriter.write(stateLine.getNx()+"\t"+stateLine.getB()+"\t"+stateLine.getY());
				outputFileBufferedWriter.newLine();
				int nx = stateLine.getNx();
				for (int ix=0; ix<nx; ix++){
					double x = stateLine.getX()[ix];
					double phi = stateLine.getPhi()[ix];
					outputFileBufferedWriter.write(String.format(locale,dupuitCFormat,x)+" "+String.format(locale,dupuitCFormat,phi));
					outputFileBufferedWriter.newLine();
				}
			}
			outputFileBufferedWriter.close();
			BBUtils.copyFile(tempFile, this.dupuitCFile);
			tempFile.deleteOnExit();
		} catch (IOException e) {
			throw new RuntimeException("Could not write to " + this.dupuitCFile.getAbsolutePath());
		}
	}

	private void readDupuitCFile() {
		String lineFields[];
		String line;
		try {
			BufferedReader dupuitCFileBufferedReader = new BufferedReader(new FileReader(this.dupuitCFile));
			line = dupuitCFileBufferedReader.readLine();
			if (!line.toLowerCase().contains("piezolines")) {
				throw new RuntimeException("DAMFlow state file does not contain keyword 'piezolines':"+this.dupuitCFile.getAbsolutePath());
			} else {
				line = dupuitCFileBufferedReader.readLine();
			}
			while (line != null) {
				lines.add(line);
				line = dupuitCFileBufferedReader.readLine();
			}
			dupuitCFileBufferedReader.close();
		} catch (IOException e){
			throw new RuntimeException("Could not read DAMFlow state file "+this.dupuitCFile.getAbsolutePath());
		}

		line = lines.get(0);
		lineFields = line.trim().split("[ \t]+");
		double time = Double.parseDouble(lineFields[1]);
		int nline = Integer.parseInt(lineFields[3]);


		StateLine[] stateLines = new StateLine[nline];
		int iLine = 1;
		for (int iStateLine=0; iStateLine<nline; iStateLine++){
			line = lines.get(iLine);
			lineFields = line.trim().split("[ \t]+");
			int nx = Integer.parseInt(lineFields[0]);
			int b = Integer.parseInt(lineFields[1]);
			int y = Integer.parseInt(lineFields[2]);
			double[] x = new double[nx];
			double[] phi = new double[nx];
			numberOfHydraulicHeads += nx;
			iLine++;
			for (int ix=0; ix<nx; ix++){
				line = lines.get(iLine);
				lineFields = line.trim().split("[ \t]+");
				x[ix] = Double.parseDouble(lineFields[0]);
				phi[ix] = Double.parseDouble(lineFields[1]);
				iLine++;
			}
			stateLines[iStateLine] = new StateLine(nx,b,y,x,phi);
		}
		dupuitCFileData = new DupuitCFileData(time,nline,stateLines);
	}

	@Override
	public String[] getExchangeItemIDs() {
		return exchangeItemId;
	}

	@Override
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFile - Method Name : getExchangeItemIDs");
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		if (exchangeItemID.equalsIgnoreCase(this.exchangeItemId[0])){
			return this.exchangeItem;
		} else {
			throw new RuntimeException(this.getClass()+": no exchange item with ID "+exchangeItemID+". Available exchange item: "+this.exchangeItemId[0]+".");
		}
	}

	@Override
	public void finish() {
		writeDupuitCFile();
	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		String fileName = arguments[0];
		String[] remainingArguments = new String[arguments.length-1];
		System.arraycopy(arguments, 1, remainingArguments, 0, remainingArguments.length);
		initialize(workingDir, fileName, remainingArguments);
	}

	protected double[] getHydraulicHead() {
		double[] hydraulicHeads = new double[numberOfHydraulicHeads];
		int nStateLines = this.dupuitCFileData.stateLines.length;
		int startElement = 0;
		for (int iStateLine=0; iStateLine<nStateLines; iStateLine++){
			StateLine stateLine = this.dupuitCFileData.stateLines[iStateLine];
			int nx = stateLine.getNx();
			double[] phis = stateLine.getPhi();
			System.arraycopy(phis,0,hydraulicHeads,startElement,nx);
			startElement += nx;
		}
		return hydraulicHeads;
	}

	protected void setHydraulicHead(double[] values) {
		int nStateLines = this.dupuitCFileData.stateLines.length;
		int startElement = 0;
		for (int iStateLine=0; iStateLine<nStateLines; iStateLine++){
			StateLine stateLine = this.dupuitCFileData.stateLines[iStateLine];
			int nx = stateLine.getNx();
			double[] headThisLine = new double[nx];
			System.arraycopy(values,startElement,headThisLine,0,nx);
			this.dupuitCFileData.stateLines[iStateLine].setPhi(headThisLine);
			startElement += nx;
		}
	}

	protected void axpyOnHydraulicHead(double alpha, double[] axpyValues){
		double[] newHydraulicHead = getHydraulicHead();
		for (int i=0;i<newHydraulicHead.length;i++){
			newHydraulicHead[i] = alpha*axpyValues[i] + newHydraulicHead[i];
		}
		setHydraulicHead(newHydraulicHead);
	}

	@Override
	public double[] getTimes() {
		return new double[]{this.dupuitCFileData.time};
	}


	private class StateLine {
		//TODO: find the proper interpretation of nx, b, and y!! also their data type, are they really integer?
		private int nx;
		private int b;
		private int y;
		private double[] x = new double[nx];
		private double[] phi = new double[nx];

		public StateLine(int nx, int b, int y, double[] x, double[] phi) {
			this.nx = nx;
			this.b = b;
			this.y = y;
			this.x = x;
			this.phi = phi;
		}

		public int getNx(){
			return this.nx;
		}

		public int getB(){
			return this.b;
		}

		public int getY(){
			return this.y;
		}

		public double[] getX(){
			return this.x;
		}

		public double[] getPhi(){
			return this.phi;
		}

		public void setPhi(double[] phi){
			this.phi = phi;
		}

	}

	private class DupuitCFileData {

		private StateLine[] stateLines;
		private int nline;
		private double time;

		public DupuitCFileData(double time, int nline, StateLine[] stateLines){
			this.time = time;
			this.nline = nline;
			this.stateLines = stateLines;

		}
	}
}
