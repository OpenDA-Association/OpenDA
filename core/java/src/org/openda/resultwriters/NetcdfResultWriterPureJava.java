package org.openda.resultwriters;

import org.openda.interfaces.IInstance;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.IVector;
import org.openda.utils.io.NetCDFFile;
import ucar.ma2.InvalidRangeException;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by nils on 13/11/15.
 */
public class NetcdfResultWriterPureJava implements IResultWriter {

	private int defaultMaxSize = Integer.MAX_VALUE;
	private String netcdfnameprefix;
	private Map<String,NetCDFFile> netcdfFiles = new HashMap<String, NetCDFFile>();
	private Map<String,Integer> notSupportedObjects = new HashMap<String, Integer>();
	private Map<String, Integer> timeIndex = new HashMap<String, Integer>();


	private File workingDir;


	public NetcdfResultWriterPureJava(File workingDir, String configString) {
		//Check whether the workingDir exists
		if (workingDir.exists()){
			if (!workingDir.isDirectory()){
				String error="You have specified a working directory for your NETCDF output. The selected directory:"+
						workingDir.getAbsolutePath()+" does exsist but does not appear to be a directory."+
						"Please change you configuration or remove the specified file";
				throw new RuntimeException(error);
			}
		}
		else{
			workingDir.mkdir();
		}
		this.workingDir = workingDir;

        //Check the config string
		if (configString.startsWith("<xml")) {  // TODO: right prefix
			// TODO: read from config file
			throw new RuntimeException("The configuration string should not be the name of an xml-file. I'm expecting a prefix for the generated in the format 'some_prefix_.nc'"
					+" but found "+configString);
		} else {
			if (configString.toLowerCase().endsWith(".nc")) {
				String prefixname = configString.substring(0, configString.length() - 3);
				netcdfnameprefix = new File(workingDir, prefixname).getAbsolutePath();
			} else {
				throw new RuntimeException("The configuration string should must be a prefix for the generated in the format 'some_prefix_.nc'"
						+ " but found " + configString);
			}
		}
	}

	private File idToFile(String id){
		return new File(netcdfnameprefix+id+".nc");
	}

	private NetCDFFile getNetCDFFile(String id){
		//Check whether this is the first write for this id
		if (this.netcdfFiles.containsKey(id)){
			return netcdfFiles.get(id);
		}
		else {
			//First write remove existing file when needed
			File netCDFFile = this.idToFile(id);
			if (netCDFFile.exists()){
				System.out.println("Warning removing (old) file: "+netCDFFile.getAbsolutePath());
				netCDFFile.delete();
			}
			//Create new NetCDF file
			NetCDFFile newFile = new NetCDFFile(netCDFFile);
			netcdfFiles.put(id,newFile);
			return newFile;
		}
	}


	/**
	 * Put a message.
	 *
	 * @param source  the producer of the message (algorithm, model or observer)
	 * @param message message to be written to output
	 */
	@Override
	public void putMessage(Source source, String message) {
		//Not supported for NETCDF
	}

	/**
	 * Put a message.
	 *
	 * @param source  the producer of the message (algorithm, model or observer)
	 * @param message message to be written to output
	 */
	@Override
	public void putMessage(IInstance source, String message) {
		//Not supported for NETCDF
	}

	/**
	 * Put a vector, indicating the algorithm's current iteration.
	 *
	 * @param source      the producer of the message (algorithm, model or observer)
	 * @param id          the output vector's identifier
	 * @param result      the output item
	 * @param outputLevel the level of output group
	 * @param context     context where the value is written
	 * @param iteration   current iteration
	 */
	@Override
	public void putValue(Source source, String id, Object result, OutputLevel outputLevel, String context, int iteration) {
		if (result instanceof IVector){
			double[] values = ((IVector) result).getValues();
			NetCDFFile file = this.getNetCDFFile(id);
			int [] dimensions = new int[1];
			dimensions[0]=values.length;
			try {
				//Time Index of array
				Integer iTime=0;
				if (timeIndex.containsKey(id)) {
					iTime = timeIndex.get(id) + 1;
				}
				timeIndex.put(id,iTime);
				//Write data:
				file.writeArray(values,dimensions,iTime,id);
			} catch (IOException e) {
				throw new RuntimeException("Something went wrong while writing data with ID="+id+" to the NetCDF file"+this.idToFile(id)+"\nWe get an IOException :"+e.getMessage());
			} catch (InvalidRangeException e) {
				throw new RuntimeException("Something went wrong while writing data with ID="+id+" to the NetCDF file"+this.idToFile(id)+"\nWe get an InvalidRangeException :"+e.getMessage());
			}
		}
		else {
			// Object type is not support. Only warn once
			if (! notSupportedObjects.containsKey(id)){
				System.out.println("Warning: Cannot output "+id+" The type of object is not supported (yet) by this resultWriter. ("+result.getClass().getCanonicalName()+")");
				notSupportedObjects.put(id,1);
			}
		}
	}

	/**
	 * Put a report on the iteration.
	 *
	 * @param source     the producer of the message (algorithm, model or observer)
	 * @param iteration  iteration index
	 * @param cost       cost function
	 * @param parameters parameters used in the computation
	 */
	@Override
	public void putIterationReport(IInstance source, int iteration, double cost, IVector parameters) {
		//Not supported for NETCDF
	}

	/**
	 * Give the default maximum size of result item that can be printed to the result file by this result writer.
	 *
	 * @return int defaultMaxSize
	 */
	@Override
	public int getDefaultMaxSize() {
		return this.defaultMaxSize;
	}

	/**
	 * Free the ResultWriter instance.
	 */
	@Override
	public void free() {

	}
}
