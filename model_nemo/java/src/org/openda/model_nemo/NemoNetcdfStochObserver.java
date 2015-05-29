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
public class NemoNetcdfStochObserver extends Instance implements IStochObserver, IObservationDescriptions {

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
	int nz=11, ny=121, nx=81;


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

		// Find observations have a dummy value
		for (int i=0; i<this.tb.length; i++){
			if (this.tb[i]>-9999.0){ tbOK[i]=true;}
		}
		for (int i=0; i<this.sshb.length; i++){
			if (this.sshb[i]>-9999.0){ sshbOK[i]=true;}
		}

		// Find whether model can come up with a corresponding value
		for (int iNnz=0; iNnz<Hi.length; iNnz++){
			int i=(int) Hi[iNnz]-1;   //Note move to 0-indexing
			if (i<this.sshb.length) {
				sshbOK[i]=true;
			}
			else {
				tbOK[i-this.sshb.length]=true;
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
		this.xcoords=new double[sshbOkIndex.length];
		this.ycoords=new double[sshbOkIndex.length];
        for (int i=0;i<this.xcoords.length; i++){this.xcoords[i]=0.0;}
		for (int i=0;i<this.ycoords.length; i++){this.ycoords[i]=0.0;}

		int nObsAll=this.sshb.length+this.tb.length;
		double[] xAll=new double[nObsAll];
		double[] yAll=new double[nObsAll];
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
			int j=(int) Hj[iNnz]-1;   //Note move to 0-indexing
			double alpha= (double) Hs[iNnz];
			double x;
			int ix,iy,jCorr;
			if (i<this.sshb.length) {
				jCorr=j-offset_sshb;
			}
			else {
				jCorr=j - offset_tb;
			}
			iy=jCorr%ny;
			ix=jCorr/ny;
			xAll[i]+=alpha*(double) ix;
			yAll[i]+=alpha*(double) iy;
			sumAlpha[i]+=alpha;
			if (alpha>0) minIndex[i]=java.lang.Math.min(minIndex[i], jCorr);
			if (alpha>0) maxIndex[i]=java.lang.Math.max(maxIndex[i] ,jCorr);
		}

		int nSshOK=this.sshbOkIndex.length;
		int [] minOK = new int[this.xcoords.length];
		int [] maxOK = new int[this.xcoords.length];
		for (int i=0;i<nSshOK;i++){
			this.xcoords[i]=xAll[sshbOkIndex[i]];
			this.ycoords[i]=yAll[sshbOkIndex[i]];
			minOK[i] = minIndex[sshbOkIndex[i]];
			maxOK[i] = maxIndex[sshbOkIndex[i]];
		}
        if (!this.only_ssh){
			for (int i=0;i<this.tbOkIndex.length;i++) {
				this.xcoords[nSshOK + i] = xAll[tbOkIndex[i]];
				this.ycoords[nSshOK + i] = yAll[tbOkIndex[i]];
				minOK[nSshOK + i] = minIndex[tbOkIndex[i]];
				maxOK[nSshOK + i] = maxIndex[tbOkIndex[i]];
			}
		}

		//write locations
		if (false) {
			try {
				String fileName = "xyobs_" + this.iStartDate + ".txt";
				PrintWriter fo = new PrintWriter(new FileOutputStream(new File(fileName)));
				for (int i = 0; i < this.xcoords.length; i++) {
					fo.println(this.xcoords[i] + " " + this.ycoords[i] + " " + minOK[i] + " " + maxOK[i]);
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
				String name  = var.getName();
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


	//	nObsSshbOk=0;
	//	nObsTbOk=0;
	//	for (int i=0;i<sshb.length; i++){if (sshb[i]>-9999.0) nObsSshbOk++;}
	//	for (int i=0;i<tb.length; i++){if (tb[i]>-9999.0) nObsTbOk++;}
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
			return nObsSshbOk + nObsTbOk;
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
		throw new RuntimeException("not yet implemented");
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
		System.out.println("Debug: NemoNetcdfStochObserver nDates="+this.nDates);
	}

	/**
	 * @return the parent of the current instance
	 */
	public IInstance getParent() {
		return null;
	}
}
