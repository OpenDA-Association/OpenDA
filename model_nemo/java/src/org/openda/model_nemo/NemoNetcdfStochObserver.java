/* OpenDA v2.4 
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
package org.openda.model_nemo;
import org.openda.interfaces.*;
import org.openda.utils.Instance;
import org.openda.utils.StochVector;
import org.openda.utils.Time;
import org.openda.utils.Vector;
import ucar.ma2.Array;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.text.ParseException;
import java.util.Arrays;
import java.util.List;

/**
 * Created by nils on 12/05/14.
 */
public class NemoNetcdfStochObserver extends Instance implements IStochObserver, IStochObserverExtra, IObservationDescriptions {

	// Variables from NETCDF files:
	public float[] sshb, tb, Hi, Hj, Hs;

	// Administration to filter bad observations
	int nObsTbOk=0;
	int nObsSshbOk=0;
	int [] tbOkIndex;
	int [] sshbOkIndex;
	boolean [] obsOK;

    boolean only_ssh=true;

    double [] xcoords;
	double [] ycoords;
	double [] zcoords;



	double [] allDatesMJD;
	File [] allDirectories;
	int nDates=0;
	int iStartDate =0;
	int iEndDate =-1;

	double stdev_tb=0.3;
	double stdev_sshb=0.06;
	IStochVector stochVec;

	int offset_sshb = 656668-1;
	int offset_tb   = 225424-1;
	int nz=11, ny=81, nx=121;


	public NemoNetcdfStochObserver(){
		// Empty constructor to keep java happy
	}


	public NemoNetcdfStochObserver(double [] allDatesMJD, File [] allDirectories, int iStartDate, int iEndDate){
	   	this.nDates=iEndDate-iStartDate+1;
		this.allDatesMJD=allDatesMJD;
		this.allDirectories=allDirectories;
		this.iStartDate=iStartDate;
		this.iEndDate=iEndDate;
	}

	private void countValid(){
		boolean [] tbOK= new boolean[this.tb.length];
		boolean [] sshbOK = new boolean[this.sshb.length];

		// We are negative, all observations are wrong
		for (int i=0; i<tbOK.length; i++){tbOK[i]=false;}
		for (int i=0; i<sshbOK.length; i++){sshbOK[i]=false;}

		for (int iNnz=0; iNnz<Hi.length; iNnz++){
			int i=(int) Hi[iNnz]-1;   //Note move to 0-indexing
			if (i<this.sshb.length) {
				if (this.sshb[i]>-9999.0){ sshbOK[i]=true;}
			}
			else {
				if (this.tb[i-this.sshb.length]>-9999.0){ tbOK[i-this.sshb.length]=true;}
			}
		}

		// setup admin for correct observations;
		this.nObsTbOk=0;
		for (int i=0; i<tbOK.length; i++){if (tbOK[i]){this.nObsTbOk++;}}
		this.nObsSshbOk=0;
		for (int i=0; i<sshbOK.length; i++){if (sshbOK[i]){this.nObsSshbOk++;}}

		this.tbOkIndex=new int[this.nObsTbOk];
		this.sshbOkIndex=new int[this.nObsSshbOk];
		this.obsOK = new boolean[this.sshb.length+this.tb.length];


		int nOK=0;
		for (int i=0; i<sshbOK.length; i++){
			this.obsOK[i]=sshbOK[i];
			if (sshbOK[i]){
				this.sshbOkIndex[nOK]=i;nOK++;
			}
		}
		nOK=0;
		for (int i=0; i<tbOK.length; i++){
			this.obsOK[i+sshb.length]=tbOK[i];
			if (tbOK[i]){
			   this.tbOkIndex[nOK]=i;nOK++;
			}
		}
	}

    void setCoordinates(){

		int nObsAll=this.sshb.length+this.tb.length;
		double[] xAll=new double[nObsAll];
		double[] yAll=new double[nObsAll];
		double[] zAll=new double[nObsAll];
		double[] sumAlpha=new double[nObsAll];
		int[]    maxIndex=new int[nObsAll];
		int[]    minIndex=new int[nObsAll];

		for (int i=0;i<nObsAll; i++){xAll[i]=0.0;}
		for (int i=0;i<nObsAll; i++){yAll[i]=0.0;}
		for (int i=0;i<nObsAll; i++){sumAlpha[i]=0.0;}
		for (int i=0;i<nObsAll; i++){maxIndex[i]=0;}
		for (int i=0;i<nObsAll; i++){minIndex[i]=99999;}


		for (int iNnz=0; iNnz<Hi.length; iNnz++){
			int i=(int) Hi[iNnz]-1;   //Note move to 0-indexing
			int j=(int) Hj[iNnz]-1;
			double alpha= (double) Hs[iNnz];
			double x;
			int ix,iy,jCorr,iz;
			if (i<this.sshb.length) {
				jCorr=j-offset_sshb;
			}
			else {
				jCorr=j - offset_tb;
			}
			//array(nx,ny,nz)
			ix=jCorr%(nx);
			iy=(jCorr/nx)%ny;
			iz=((jCorr/nx)/ny)%nz;

			xAll[i]+=alpha*(double) ix;
			yAll[i]+=alpha*(double) iy;
			zAll[i]+=alpha*(double) iz;
			sumAlpha[i]+=alpha;
			if (alpha>0) minIndex[i]=java.lang.Math.min(minIndex[i], jCorr);
			if (alpha>0) maxIndex[i]=java.lang.Math.max(maxIndex[i] ,jCorr);
		}

		// Do not use observations at the border (assimilating boundary condition might be problematic) 	// Does not really help!
//		boolean [] obsOkSelect= new boolean[nObsSshbOk+nObsTbOk];

//		for (int i=0; i<obsOkSelect.length; i++){obsOkSelect[i]=true;}

//		for (int i=0; i<nObsAll; i++){			// one of the conditions is enough to dismiss an observation
//			if (Math.abs(xAll[i]) < 0.5){		// left border, use rather big distance to be sure
//				obsOK[i] = false;
//			} else if (Math.abs(xAll[i] - (nx-1)) < 0.5){	// right border
//				obsOK[i] = false;
//			} else if (Math.abs(yAll[i]) < 0.5){		// bottom border
//				obsOK[i] = false;
//			} else if (Math.abs(yAll[i] - (ny-1)) < 0.5){	// top border
//				obsOK[i] = false;
//			} else if (Math.abs(zAll[i] - (nz-1)) < 0.5) {        // deep see (should not be reached)
//				obsOK[i] = false;
//			}
//		}

		this.nObsTbOk=0;
		this.nObsSshbOk=0;
		for (int i=0; i<obsOK.length; i++){
			if (i<this.sshb.length) {
				if (obsOK[i]){this.nObsSshbOk++;}
			} else {
				if (obsOK[i]){this.nObsTbOk++;}
			}
		}

		int nSshbOK=0;
		int nTbOK=0;
		this.tbOkIndex=new int[this.nObsTbOk];
		this.sshbOkIndex=new int[this.nObsSshbOk];
		for (int i=0; i<obsOK.length; i++){
			if (i<this.sshb.length){
				if (obsOK[i]){
					this.sshbOkIndex[nSshbOK]=i;
					nSshbOK++;
				}
			} else {
				if (obsOK[i]) {
					this.tbOkIndex[nTbOK] = i - this.sshb.length;		// index in tb not in obs=[sshb tb]'
					nTbOK++;
				}
			}
		}

		int [] minOK;
		int [] maxOK;

		int nObsUsed = nSshbOK + nTbOK;
		if(this.only_ssh) { nObsUsed = nSshbOK; }

			this.xcoords = new double[nObsUsed];
			this.ycoords = new double[nObsUsed];
			this.zcoords = new double[nObsUsed];
			for (int i = 0; i < this.xcoords.length; i++) {
				this.xcoords[i] = 0.0;
			}
			for (int i = 0; i < this.ycoords.length; i++) {
				this.ycoords[i] = 0.0;
			}
			for (int i = 0; i < this.zcoords.length; i++) {
				this.zcoords[i] = 0.0;
			}


			minOK = new int[this.xcoords.length];
			maxOK = new int[this.xcoords.length];

			for (int i = 0; i < nSshbOK; i++) {
				this.xcoords[i] = xAll[sshbOkIndex[i]];
				this.ycoords[i] = yAll[sshbOkIndex[i]];
				this.zcoords[i] = zAll[sshbOkIndex[i]];
				minOK[i] = minIndex[sshbOkIndex[i]];
				maxOK[i] = maxIndex[sshbOkIndex[i]];
			}

		if(!only_ssh) {
			for (int i = 0; i < nTbOK; i++) {
				this.xcoords[nSshbOK + i] = xAll[tbOkIndex[i]+this.sshb.length];	// .All vector are of dimension nSsh+nT from the start, while tbOk the indeces in tb only counts
				this.ycoords[nSshbOK + i] = yAll[tbOkIndex[i]+this.sshb.length];
				this.zcoords[nSshbOK + i] = zAll[tbOkIndex[i]+this.sshb.length];
				minOK[nSshbOK + i] = minIndex[tbOkIndex[i]+this.sshb.length];
				maxOK[nSshbOK + i] = maxIndex[tbOkIndex[i]+this.sshb.length];
			}
		}

		//write locations
		if (false) {
			try {
				String fileName = "xyzobs_" + this.iStartDate + ".txt";
				PrintWriter fo = new PrintWriter(new FileOutputStream(new File(fileName)));
				for (int i = 0; i < this.xcoords.length; i++) {
					fo.println(this.xcoords[i] + " " + this.ycoords[i] + " " + this.zcoords[i] + " " + minOK[i] + " " + maxOK[i]);
				}
				fo.close();
			} catch (Exception e) {
				throw new RuntimeException("Error writing xyobs file");
			}

		}
	}

	void initValues(){

		/* Quick return */
		if (this.Hi!=null) return;

		/* First check we have only selected a single day */
		if (this.iEndDate!=this.iStartDate){
			System.out.println("ERROR: used to be error + startData="+this.iStartDate+" endDate="+this.iEndDate );

			//throw new RuntimeException("This observer currently only supports actions on observations from a single date\n"+
			//"If you want to get values/properties from observations from various dates at one time, please extend this implementation");
		}

		/* Read the relevant variables from the input files: */
		/*- step 1: open the input files */
		File yoFile, obsopeFile;
		NetcdfFile yoNetcdf, obsopeNetcdf;
		try{
			System.out.println("Observations from directory:"+this.allDirectories[this.iStartDate]);
			yoFile= new File(this.allDirectories[this.iStartDate],"yo.nc");
			obsopeFile= new File(this.allDirectories[this.iStartDate],"obsope.nc");

			if (!yoFile.exists()){
				throw new RuntimeException("The observtion file"+ yoFile.getAbsolutePath()+" does not exist");
			}
			if (!obsopeFile.exists()){
				throw new RuntimeException("The observtion file"+ yoFile.getAbsolutePath()+" does not exist");
			}

			yoNetcdf = NetcdfFile.open(yoFile.getAbsolutePath(), null);
			obsopeNetcdf = NetcdfFile.open(obsopeFile.getAbsolutePath(), null);
		}catch (Exception e) {
			throw new RuntimeException("Cannot open one of the netCDF input files (yo.nc or obsope.nc):"+e.getMessage());
		}
		try{
			Array sshbArray = yoNetcdf.readSection("sshb");
			Array tbArray = yoNetcdf.readSection("tb");
			Array HArray = obsopeNetcdf.readSection("H");
			List<Variable> vars=obsopeNetcdf.getVariables();
			List<Array> allArrays = obsopeNetcdf.readArrays(vars);
			/* get values (Note reading done a bit difficult be we encountered problems cased by the "." in the variable name*/
			for (int i=0; i<vars.size();i++){
				Variable var = vars.get(i);
				String name  = var.getShortName();
				Array arr    = allArrays.get(i);
				if (name.equalsIgnoreCase("H.i")){this.Hi = (float []) arr.copyTo1DJavaArray();}
				if (name.equalsIgnoreCase("H.j")){this.Hj = (float []) arr.copyTo1DJavaArray();}
				if (name.equalsIgnoreCase("H.s")){this.Hs = (float []) arr.copyTo1DJavaArray();}
			}
			this.sshb = (float []) sshbArray.copyTo1DJavaArray();
			this.tb = (float []) tbArray.copyTo1DJavaArray();


		}catch (Exception e){
			throw new RuntimeException("Cannot read values from the netCDF input files (yo.nc or obsope.nc)"+e.getMessage());
		}

		countValid();
		setCoordinates();

	}


	/**
	 * Get the exchange items describing the measures available in the stoch. observer.
	 *
	 * @return All exchange items in the stoch. observer.
	 */
	public List<IPrevExchangeItem> getExchangeItems() {
		throw new RuntimeException("not yet implemented");
	}

	/**
	 * Get properties (values) that correspond to a given key.
	 *
	 * @param Key key for which the value is asked
	 * @return Properties (column of data from observation descriptions).
	 */
	public IVector getValueProperties(String Key) {
		if (Key.equals("x")){
			return new Vector(this.xcoords);
		}
		else if (Key.equals("y")){
			return new Vector(this.ycoords);
		}
		else if (Key.equals("z")){
			return new Vector(this.zcoords);
		}
		else{
				throw new RuntimeException("Not implemented for Key="+Key);
			}
	}

	/**
	 * Get properties (strings) that correspond to a given key.
	 *
	 * @param Key key for which the value is asked
	 * @return Properties (column of data from observation descriptions).
	 */
	public String[] getStringProperties(String Key) {
		throw new RuntimeException("not yet implemented");
	}

	/**
	 * Get names of all keys
	 *
	 * @return All keys of the observation descriptions.
	 */
	public String[] getPropertyKeys() {
		throw new RuntimeException("not yet implemented");
	}

	/**
	 * Get number of properties/keys.
	 *
	 * @return Number of properties.
	 */
	public int getPropertyCount() {
		throw new RuntimeException("not yet implemented");
	}

	/**
	 * Get number of observations.
	 *
	 * @return Number of observations.
	 */
	public int getObservationCount() {
		this.initValues();
		if (this.only_ssh){
			System.out.println("Calling getObservationCount: return" + nObsSshbOk);
			return nObsSshbOk; // + nObsTbOk;
		}
		else {
			System.out.println("Calling getObservationCount: return" + nObsSshbOk + nObsTbOk);
			return nObsSshbOk + nObsTbOk;
		}
	}

	/**
	 * Create an new Stochastic Observer, containing a selection of the present stochastic observer.<br>
	 * The selection criterium is in the form of an SQLite query.
	 *
	 * @param selection Selection querry
	 * @return Stochastic Observer containing the required selection.
	 */
	public IStochObserver createSelection(String selection) {
		throw new RuntimeException("not yet implemented");
	}

	/**
	 * Create an new Stochastic Observer, containing a selection of the present stochastic observer.<br>
	 * The selection criteria is a timeSpan. The start time of the interval is not included, the end time is
	 * included, i.e. t_start<t<=t_end
	 *
	 * @param selectionTimes timeSpan with selection.
	 * @return Stochastic Observer containing the required selection.
	 */
	public IStochObserver createSelection(ITime selectionTimes) {
		/* Find interval of days */
		System.out.println("Calling createSelection :("+selectionTimes+")");
	    if (this.nDates==0){
		   return this;
		}
		else {
			int iStartDate, iEndDate;
			double begintMJD = selectionTimes.getBeginTime().getMJD();
			double endMJD    = selectionTimes.getEndTime().getMJD();
			/* We are in the order of days, so add eps of 0.1 day */
			begintMJD-=0.1;
			endMJD+=0.1;

			for (iStartDate=0;iStartDate<this.iEndDate+1 && this.allDatesMJD[iStartDate]<begintMJD;iStartDate++);
			for (iEndDate=this.iEndDate;iEndDate>0 && this.allDatesMJD[iEndDate]>endMJD;iEndDate--);
			return new NemoNetcdfStochObserver(this.allDatesMJD, this.allDirectories, iStartDate, iEndDate);
		}
	}


	/**
	 * Create an new Stochastic Observer, containing a selection of the present stochastic observer.<br>
	 * The selection criteria is the type of observations: assimilation or validation
	 *
	 * @param observationType The requested type of observations.
	 * @return Stochastic Observer containing the required selection.
	 */
	public IStochObserver createSelection(Type observationType) {
		throw new RuntimeException("not yet implemented");
	}

	/**
	 * Create an new Selector, containing a selection of the present stochastic observer.<br>
	 * The same selection can used to make selection for the corresponding vectors of observation and prediction.
	 * The selection criteria is the type of observations: assimilation or validation
	 *
	 * @param observationType The requested type of observations.
	 * @return Selector containing the required indices selection.
	 */
	public ISelector createSelector(Type observationType) {
		throw new RuntimeException("not yet implemented");
	}


	/**
	 * Number of observations in the Stochastic Observer.
	 *
	 * @return The number of observations.
	 */
	public int getCount() {
		initValues();
		if (this.only_ssh){
			return this.nObsSshbOk;
		}
		else {
			return this.nObsSshbOk + this.nObsTbOk;
		}
	}

	/**
	 * Get the values for all observations.
	 *
	 * @return The observed values.
	 */
	public IVector getValues() {
		initValues();

		int nObs = this.getCount();
		double[] dValues = new double[nObs];
		for (int i=0; i<this.nObsSshbOk; i++){
			dValues[i]=this.sshb[this.sshbOkIndex[i]];
		}
		if (! this.only_ssh) {
			for (int i = 0; i < this.nObsTbOk; i++) {
				dValues[i + this.nObsSshbOk] = this.tb[this.tbOkIndex[i]];
			}
		}
		Vector values = new Vector(dValues);
		return values;
	}

	/**
	 * Get realization values for all observations, for one ensemble member.
	 *
	 * @return The realizations.
	 */
	public IVector getRealizations() {
		initValues();
		if (stochVec==null) stochVec= new StochVector(this.getExpectations().getValues(),this.getStandardDeviations().getValues());
		return stochVec.createRealization();
	}

	/**
	 * Get expectation values for all stochastic observations.
	 *
	 * @return The expectations.
	 */
	public IVector getExpectations() {
		return this.getValues();
	}

	/**
	 * Evaluate the PDF for stochastic observations, given the values for those observation.
	 *
	 * @param values values for observation's PDF-evaluation.
	 * @return The PDF evaluations.
	 */
	public double evaluatePDF(IVector values) {
		throw new RuntimeException("not yet implemented");
	}

	/**
	 * Evaluate the PDF for stochastic observations, given the values for those observation.
	 *
	 * @param values values for observation's PDF-evaluation.
	 * @return The PDF evaluations.
	 */
	public IVector evaluateMarginalPDFs(IVector values) {
		throw new RuntimeException("not yet implemented");
	}

	/**
	 * Get the covariance matrix (as a vector) for the stochastic observations.
	 *
	 * @return The covariance matrix.
	 */
	public ISqrtCovariance getSqrtCovariance() {
		IStochVector obs = new StochVector(this.getExpectations(),this.getStandardDeviations());
		return obs.getSqrtCovariance();
		//throw new RuntimeException("not yet implemented");
	}

	/**
	 * Get the standard deviation for each each stochastic observation.
	 *
	 * @return The standard deviations.
	 */
	public IVector getStandardDeviations() {
		initValues();
		int nObs=this.getCount();
		double[] dValues = new double[nObs];
		for (int i=0; i<this.nObsSshbOk; i++){
			dValues[i]=this.stdev_sshb;
		}
		if (!this.only_ssh) {
			for (int i = 0; i < this.nObsTbOk; i++) {
				dValues[this.nObsSshbOk + i] = this.stdev_tb;
			}
		}
		Vector values = new Vector(dValues);
		return values;
	}

	/**
	 * Get all different times in increasing order. There is at least one observation for each time.
	 * It is likely that observer.createSelection(time[i]) will be used to walk through the
	 * observations. The implementation of the stochobserver should garantee that al observations are
	 * returned in exactly one batch this way.
	 *
	 * @return array with all uniquely different times.
	 */
	public ITime[] getTimes() {
		Time[] times;
		if (this.iEndDate-this.iStartDate==0){
			initValues();
			times = new Time[1];
			times[0] =  new Time(this.allDatesMJD[this.iStartDate]);
		}
		else {
			/* Not according to formal interface but just return the different days */
			times = new Time[this.nDates];
			for (int iData=0; iData<this.nDates; iData++){
				times[iData]= new Time(this.allDatesMJD[iData]);
			}
		}
		return times;
	}

	/**
	 * free the Stochastic Observer.
	 */
	public void free() {

	}

	/**
	 * Get the observation descriptions.
	 *
	 * @return The Observation Descriptions
	 */
	public IObservationDescriptions getObservationDescriptions() {
		return this;
	}

	/**
	 * Set the instance's parent
	 *
	 * @param parent Parent for the stoch observer (usually the parent is the Algorithm)
	 */
	public void setParent(IInstance parent) {

	}

	/**
	 * Initialize the configurable. Specify what its "working directory" is (usually meaning: the directory
	 * where its configuration file is), and provide its arguments.
	 *
	 * @param workingDir The directory indicating the where the configurable is started (not as 'current
	 *                   working directory', but as the root path for its configuration files etc).
	 * @param arguments  The arguments needed to initialize. Typically the first argument can be a configuration
	 */
	public void initialize(File workingDir, String[] arguments) {
		/* The workingDir must consists of directories of the date containing the observations
		   The format is YYYYMMDD
		   Let us first check out the directory and make a list of all days for which observations
		   are available
		 */
		File[] allFiles=workingDir.listFiles();
		Arrays.sort(allFiles);
		this.allDatesMJD = new double[allFiles.length];
		this.allDirectories = new File[allFiles.length];
		this.nDates = 0;

		/* Loop over all files */
		for (int i=0; i<allFiles.length; i++){
			if (allFiles[i].isDirectory()){
				String dirName=allFiles[i].getName();
				try {
					double dateMJD = org.openda.exchange.timeseries.TimeUtils.date2Mjd(dirName,"yyyyMMdd");
				    /* add data and directory name to list */
					this.allDatesMJD[this.nDates]=dateMJD;
					this.allDirectories[this.nDates]=allFiles[i];
					this.nDates++;

				} catch (ParseException e) {}
			}
		}
		this.iEndDate=this.nDates-1;
		//System.out.println("Debug: NemoNetcdfStochObserver nDates="+this.nDates);
	}

	/**
	 * @return the parent of the current instance
	 */
	public IInstance getParent() {
		return null;
	}

	//Compute H*rho
	public IVector[] getObservedLocalizationAtLocalizationLocations(IObservationDescriptions ObsAssim,  double distance){

		IVector xAssimVector = ObsAssim.getValueProperties("x");	// Position of the current observation to be assimilated, at this point only ONE observation at a time
		IVector yAssimVector = ObsAssim.getValueProperties("y");
		//IVector zAssimVector = ObsAssim.getValueProperties("z");

		int nObsUsed = nObsSshbOk + nObsTbOk;
		if(only_ssh) { nObsUsed = nObsSshbOk; }

		IVector[] retVec = new IVector[xAssimVector.getSize()];
		double[] mask = new double [nObsUsed];
		// Loop through the Hx

		for (int iObs = 0; iObs < xAssimVector.getSize(); iObs++) {
			IVector subtree;

			double xAssim = xAssimVector.getValue(iObs);
			double yAssim = yAssimVector.getValue(iObs);
			//double zAssim = zAssimVector.getValue(iObs);

			for (int ix = 0; ix < this.xcoords.length; ix++) {
				// Apply horizontal localisation in all vertical layers

					double xHx = xcoords[ix];
					double yHx = ycoords[ix];
					//double zHx = zcoords[ix];
					double dist = Math.sqrt((yAssim - yHx) * (yAssim - yHx) + (xAssim - xHx) * (xAssim - xHx));

					// Determine weights (Gaspari-Cohn formula)
					double a = Math.sqrt(10 / 3) * distance;        // characteristic length scale for model

					if (0.0 <= dist && dist <= a) {
						mask[ix] = -0.25 * Math.pow((dist / a), 5) + 0.5 * Math.pow((dist / a), 4) + 5.0 / 8.0 * Math.pow((dist / a), 3) - 5.0 / 3.0 * Math.pow((dist / a), 2) + 1.0;
					} else if (a < dist && dist <= 2.0 * a) {
						mask[ix] = 1 / 12.0 * Math.pow((dist / a), 5) - 0.5 * Math.pow((dist / a), 4) + 5.0 / 8.0 * Math.pow((dist / a), 3) + 5.0 / 3.0 * Math.pow((dist / a), 2) - 5.0 * (dist / a) + 4.0 - 2.0 / 3.0 * (a / dist);
					} else if (2.0 * a < dist) {
						mask[ix] = 0.0;
					} else {
						throw new RuntimeException("There is a problem in the determination of the localisation weights.");
					}
			}

			subtree = new Vector(mask);
			retVec[iObs] = subtree;
		}

		// Vertical Localisation apart from the horizontal (optional), in fact apply extra rho on top for vertical localisation (next step to do!)
		// Vector with correction values rho for each Hx, dimensions: nObs x nObsCurrent (correction of each observation position against the current one)
		return retVec;
	}


}
