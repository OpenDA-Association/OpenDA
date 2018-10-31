package org.openda.exchange.dataobjects;

import org.openda.exchange.DoublesExchangeItem;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;

import jep.Jep;
import jep.JepConfig;
import jep.JepException;
import jep.python.PyCallable;
import jep.python.PyObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MarkerFactory;


import java.io.File;
import java.util.ArrayList;

/**
 * Created by hegeman on 29-10-18.
 */


public class PythonDataObject implements IDataObject{


	private static final Logger logger = LoggerFactory.getLogger(PythonDataObject.class);

	private Jep jep;
	private PyObject pythonDataObject;
	private String pythonClass;
//	private static String loadClassTemplate = "object_handle = %s.%s('%s')";


	/**
	 * Initialize the IDataObject
	 *
	 * @param workingDir
	 *           Working directory
	 * @param arguments
	 *           First argument should be the file name of the file that will be read.
	 *           Second argument name of the Python class that will be used.
	 *           Additional arguments (may be null zero-length) These arguments are ignored.
	 */
	@Override
	public void initialize(File workingDir, String[] arguments) {
		File dataFile = new File(workingDir.getAbsolutePath(), arguments[0]);
		this.pythonClass = arguments[1];
		JepConfig conf = new JepConfig();

		conf.setInteractive(false);
		conf.setIncludePath("/v3/Stage/Rick/openda/openda_public/core/java/test/org/openda/exchange/dataobjects/testData/python");
		conf.setRedirectOutputStreams(true);
		/*logger.debug(this.jep.eval("import sys") );*/

		try {
			this.jep = conf.createJep();
			this.jep.eval("import sys");
			String pyPath = jep.getValue("sys.executable", String.class);
			logger.debug(MarkerFactory.getMarker("pyPath") ,pyPath);
			this.jep.eval("import " + pythonClass);
			this.jep.eval(loadClass(pythonClass, dataFile));
			this.pythonDataObject = jep.getValue("object_handle", PyObject.class);
			this.jep.eval("del(object_handle)");
			// Create Python coupling


			// Get values from Python code


		}catch (JepException e) {
			// Allowable exceptions
			if (e.getMessage().contains("ModuleNotFoundError"))
				System.out.println("Python module not found, check the configuration");
			// Otherwise retrow exception
			e.printStackTrace();
		}

	}

	/**
	 * Get the identifiers of the exchange items that can be retrieved from and set to the model.
	 * Calls the get_ids() method from the Python class given during initialization.
	 * @return The array of exchange item identifiers, or String[0] if there are no items.
	 */
	@Override
	public String[] getExchangeItemIDs() {
		String[] IDs = new String[0];
		try{
			PyCallable getIds = pythonDataObject.getAttr("get_ids", PyCallable.class);
			ArrayList idList = (ArrayList) getIds.call();
			IDs = (String[]) idList.toArray(new String[0]);
			System.out.println(IDs);

		// return String array with exchange Item ID's
		}catch (JepException e) {
			if (e.getMessage().contains("FileNotFoundError"))
				System.out.println("File not found, check the configuration");
			e.printStackTrace();
		}
		return IDs;
	}

	@Override
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		return this.getExchangeItemIDs();
	}

	@Override
	/**
	 * Get the exchange item specified by <c>exchangeItemID</c>.
 	 * Calls the get_values() method from the Python class given during initialization.
	 * Returns null if no exchangeItem with the given exchangeItemID is found.
	 *
	 * @param exchangeItemID The exchange item identifier.
	 * TODO: @return The required exchange item.
	 */
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		ArrayList<Float> vals = new ArrayList<Float>();
		// Get values from Python code
		IExchangeItem exchangeItem=null;
		// TODO: preserve dimensions from Python data
		try{
			PyCallable getValues = pythonDataObject.getAttr("get_values", PyCallable.class);

			ArrayList<Double> result =  (ArrayList<Double>) getValues.call(exchangeItemID);
			double[] values = new double[result.size()];
			for (int i=0 ; i< values.length; i++) {
				values[i] = result.get(i);
			}
			exchangeItem = new DoublesExchangeItem(exchangeItemID, IPrevExchangeItem.Role.Input,values);
		}catch (JepException e) {
			if (e.getMessage().contains("FileNotFoundError"))
				System.out.println("File not found, check the configuration");
			e.printStackTrace();
		}
		return exchangeItem;
	}

	@Override
	public void finish() {

		// Sent exchange items to Python

		// Python should write Output exchange items

		//Destroy Python coupling

	}
	/**
	 * Creates appopriate string for initializing the Python object.
	 * For internal use only.
	 *
	 * @param pythonClass name of the Python class that will be used.
	 * @param dataFile    full file name of the file that will be read.
	 * @return String that will be used by Jep to initialize the Python object.
	 */
	private String loadClass(String pythonClass, File dataFile){
		// object_handle = %s.%s('%s');
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append("object_handle = ");
		stringBuilder.append(pythonClass);
		stringBuilder.append(".");
		stringBuilder.append(pythonClass);
		stringBuilder.append("('");
		stringBuilder.append(dataFile.getAbsoluteFile());
		stringBuilder.append("')");
	return stringBuilder.toString();
	}

}
