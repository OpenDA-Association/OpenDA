package org.openda.model_wflow;

import org.openda.exchange.AbstractDataObject;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.dataobjects.NetcdfUtils;
import ucar.ma2.Array;
import ucar.ma2.InvalidRangeException;
import ucar.nc2.*;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.List;

public class WflowJuliaNetcdfForcingFile extends AbstractDataObject {

	private File slicedFile;
	private File completeFile;
	private double[] allMjdTimes;
	public static final String START_TIME_ID = "startTime";
	public static final String END_TIME_ID = "endTime";
	//private double[] allRawNetcdfTimes;

	@Override
	public void initialize(File workingDir, String[] arguments) {
		String fileName = arguments[0];
		slicedFile = new File(workingDir, fileName);
		completeFile = new File(workingDir, "complete_" + fileName);

		NetcdfFileWriter netcdfFileWriter = null;
		try {
			try {
				Files.copy(slicedFile.toPath(), completeFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
				netcdfFileWriter = NetcdfFileWriter.openExisting(this.completeFile.getAbsolutePath());
				Variable timeVariable = netcdfFileWriter.findVariable("time");
				if (timeVariable == null) throw new RuntimeException("No variable named time found in " + slicedFile);
				//Array timesArray = timeVariable.read();
				//allRawNetcdfTimes = (double[]) timesArray.get1DJavaArray(double.class);
				allMjdTimes = NetcdfUtils.readTimes(timeVariable);
				exchangeItems.put(START_TIME_ID, new DoubleExchangeItem(START_TIME_ID, allMjdTimes[0]));
				exchangeItems.put(END_TIME_ID, new DoubleExchangeItem(END_TIME_ID, allMjdTimes[allMjdTimes.length - 1]));
			} finally {
				if (netcdfFileWriter != null) netcdfFileWriter.close();
			}
		} catch (IOException e) {
			throw new RuntimeException(e);
		}

	}

	@Override
	public void finish() {
		double startTime = exchangeItems.get(START_TIME_ID).getValuesAsDoubles()[0];
		double endTime = exchangeItems.get(END_TIME_ID).getValuesAsDoubles()[0];
		int startTimeIndex = getTimeIndex(startTime);
		int endTimeIndex = getTimeIndex(endTime);
		int timeLength = endTimeIndex - startTimeIndex + 1;
		NetcdfFileWriter completeNetcdfFile = null;
		try {
			try {
				NetcdfFileWriter newSlicedNetcdfFile = NetcdfFileWriter.createNew(NetcdfFileWriter.Version.netcdf4, slicedFile.getAbsolutePath());
				completeNetcdfFile = NetcdfFileWriter.openExisting(this.completeFile.getAbsolutePath());
				NetcdfFile netcdfFile = completeNetcdfFile.getNetcdfFile();
				List<Dimension> dimensions = netcdfFile.getDimensions();
				for (Dimension dimension : dimensions) {
					String fullName = dimension.getFullName();
					if (fullName.equals("time")) {
						newSlicedNetcdfFile.addDimension(null, fullName, timeLength);
						continue;
					}
					newSlicedNetcdfFile.addDimension(null, fullName, dimension.getLength());
				}
				List<Variable> variables = netcdfFile.getVariables();
				for (Variable variable : variables) {
					Variable newVar = newSlicedNetcdfFile.addVariable(null, variable.getShortName(), variable.getDataType(), variable.getDimensionsString());
					copyAttributes(variable, newVar);
				}
				NetcdfUtils.addGlobalAttributes(newSlicedNetcdfFile);
				newSlicedNetcdfFile.create();
				for (Variable variable : variables) {
					Variable timeVariable = NetcdfUtils.findTimeVariableForVariable(variable, netcdfFile);
					Variable newVar = newSlicedNetcdfFile.findVariable(variable.getFullNameEscaped());
					if (timeVariable == null) {
						Array read = variable.read();
						newSlicedNetcdfFile.write(newVar, read);
						continue;
					}
					int[] shapeForRead = variable.getShape();
					shapeForRead[0] = timeLength;
					int[] originForRead = new int[shapeForRead.length];
					originForRead[0] = startTimeIndex;
					Array read = variable.read(originForRead, shapeForRead);
					newSlicedNetcdfFile.write(newVar, read);
				}
				newSlicedNetcdfFile.close();
			} finally {
				if (completeNetcdfFile != null) completeNetcdfFile.close();
			}
		} catch (IOException | InvalidRangeException e) {
			throw new RuntimeException(e);
		}
	}

	private void copyAttributes(Variable variable, Variable newVar) {
		List<Attribute> attributes = variable.getAttributes();
		for (Attribute attribute : attributes) {
			newVar.addAttribute(attribute);
		}
	}

	private int getTimeIndex(double endTime) {
		int endTimeIndex = 0;
		for (int i = 0; i < allMjdTimes.length; i++) {
			double mjdTime = allMjdTimes[i];
			if (mjdTime < endTime) continue;
			endTimeIndex = i;
			break;
		}
		return endTimeIndex;
	}
}
