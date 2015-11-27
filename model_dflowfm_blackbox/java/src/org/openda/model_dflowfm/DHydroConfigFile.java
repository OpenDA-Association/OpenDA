package org.openda.model_dflowfm;

import org.openda.exchange.DoubleExchangeItem;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.ConfigTree;
import org.openda.utils.Time;

import java.io.File;
import java.util.Date;
import java.util.LinkedHashMap;

/**
 * IDataObject for configuration file for d_hydro.exe.
 * Data object is used to modify start/end time
 */
public class DHydroConfigFile implements IDataObject {

	String startTimeName = "startTime";
	String endTimeName = "endTime";

	File workingDir = null;
	String dHydroConfigFileName = null;
	String adjustedDHydroConfigFileName = null;
	LinkedHashMap<String, IExchangeItem> exchangeItems = new LinkedHashMap<>();
	ConfigTree configTree = null;

	public String[] getExchangeItemIDs() {
		return exchangeItems.keySet().toArray(new String[exchangeItems.keySet().size()]);
	}


	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		return getExchangeItemIDs();
	}


	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		if (!exchangeItems.containsKey(exchangeItemID)) {
			throw new RuntimeException("Invalid exchange item id: " + exchangeItemID);
		}
		return exchangeItems.get(exchangeItemID);
	}


	public void initialize(File workingDir, String[] arguments) {
		if (arguments.length != 4) {
			throw new RuntimeException("Expected arguments: dHydroConfigFile adjustedDHydroConfigFile startTime endTime");
		}
		this.workingDir = workingDir;
		dHydroConfigFileName = arguments[0];
		adjustedDHydroConfigFileName = arguments[1];
		Double.parseDouble(arguments[2]);

		readDHydroConfigFile();
	}

	public void finish() {
		writeDHydroConfigFile();
	}

	private void readDHydroConfigFile() {
		configTree = new ConfigTree(workingDir, dHydroConfigFileName);
		String timeField = configTree.getAsString("control/parallel/startGroup/time", "");
		String[] lineParts = timeField.trim().split("\\s", 3);
		double startTimeValue = Double.parseDouble(lineParts[0]);
		double endTimeValue = Double.parseDouble(lineParts[2]);
		IExchangeItem startTimeEI = new DoubleExchangeItem(startTimeName, IPrevExchangeItem.Role.InOut, startTimeValue);
		IExchangeItem endTimeEI = new DoubleExchangeItem(endTimeName, IPrevExchangeItem.Role.InOut, endTimeValue );
		exchangeItems.put(startTimeName, startTimeEI);
		exchangeItems.put(endTimeName, endTimeEI);
	}

	private void writeDHydroConfigFile() {
		configTree.toFile(workingDir, adjustedDHydroConfigFileName);
	}

	private class DHydroStartEndTimeExchangeItem implements IPrevExchangeItem {

		private String id;
		private Date dateTime;

		public DHydroStartEndTimeExchangeItem(String id, Date dateTime) {
			this.id = id;
			this.dateTime = dateTime;
		}

		public String getId() {
			return id;
		}

		public String getDescription() {
			return id;
		}

		public Class getValueType() {
			return Double.class;
		}

		public Role getRole() {
			return IPrevExchangeItem.Role.InOut;
		}

		public Object getValues() {
			long timeInMillis = dateTime.getTime();
			return Time.milliesToMjd(timeInMillis);
		}

		public double[] getValuesAsDoubles() {
			return new double[]{(double) getValues()};
		}

		public void axpyOnValues(double alpha, double[] axpyValues) {
			throw new UnsupportedOperationException(
					"RtcStartEndTimeExchangeItem.axpyOnValues(): Not allowed.");
		}

		public void multiplyValues(double[] multiplicationFactors) {
			throw new UnsupportedOperationException(
					"RtcStartEndTimeExchangeItem.multiplyValues(): Not allowed.");
		}

		public void setValues(Object values) {
			if (!(values instanceof Double)) {
				throw new RuntimeException(this.getClass().getName() + ": unexpected values type: "
						+ values.getClass().getName());
			}
			long timeInMillies = Time.mjdToMillies((double) values);
			dateTime = new Date(timeInMillies);
		}

		public void setValuesAsDoubles(double[] values) {
			setValues(values[0]);
		}

		public double[] getTimes() {
			return null;
		}

		public void setTimes(double[] times) {
			throw new RuntimeException(this.getClass().getName() + " has no time info");
		}
	}
}
