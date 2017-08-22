/* OpenDA v2.4.1 
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
package org.openda.dotnet;


import cli.OpenDA.DotNet.Bridge.DoublesExchangeItem;
import cli.OpenDA.DotNet.Bridge.StochObserverFactory;
import cli.OpenDA.DotNet.Bridge.Time;
import cli.OpenDA.DotNet.Interfaces.*;
import org.openda.interfaces.*;
import org.openda.interfaces.IInstance;
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.ISqrtCovariance;
import org.openda.interfaces.IStochObserver;
import org.openda.interfaces.ITime;
import org.openda.interfaces.IVector;
import org.openda.utils.SqrtCovariance;
import org.openda.utils.Vector;

import java.io.File;

/**
 * Author Nils van Velzen
 * Bridge to use C# stochObserver implementations in java
 */
public class StochObserverN2J implements IStochObserver {

	private cli.OpenDA.DotNet.Interfaces.IStochObserver _dotNetStochObs;

	public StochObserverN2J(){
		//Default constructor is needed to dynamically create object!
		_dotNetStochObs=null;
	}

	public StochObserverN2J(cli.OpenDA.DotNet.Interfaces.IStochObserver dotNetStochObs)
	{
		_dotNetStochObs = dotNetStochObs;
	}

	public IStochObserver createSelection(String selection) {
		throw new UnsupportedOperationException("Not implemented yet.");
	}


	public IStochObserver createSelection(ITime selectionTimes) {
		double beginTime=selectionTimes.getBeginTime().getMJD();
		double endTime=selectionTimes.getEndTime().getMJD();
		cli.OpenDA.DotNet.Interfaces.ITime dotNetTime = new Time(beginTime,endTime);
		return new StochObserverN2J(_dotNetStochObs.createSelection(dotNetTime));
	}


	public IStochObserver createSelection(Type observationType) {
		throw new UnsupportedOperationException("Not implemented yet.");
	}


	public ISelector createSelector(Type observationType) {
		throw new UnsupportedOperationException("Not implemented yet.");
	}


	public int getCount() {
        return _dotNetStochObs.getCount();
	}


	public IVector getValues() {
		double[] values = _dotNetStochObs.getValues();
		return new Vector(values);
	}


	public IVector getRealizations() {
		double[] values = _dotNetStochObs.getRealizations();
		return new Vector(values);
	}


	public IVector getExpectations() {
		double[] values = _dotNetStochObs.getExpectations();
		return new Vector(values);
	}


	public double evaluatePDF(IVector values) {
		return _dotNetStochObs.evaluatePDF(values.getValues());
	}


	public IVector evaluateMarginalPDFs(IVector values) {
       double[] returnValues =  _dotNetStochObs.evaluateMarginalPDFs(values.getValues());
		return new Vector(returnValues);
	}


	public ISqrtCovariance getSqrtCovariance()
	{
		String errorMsg=
			"The C# programmer has implemented the method getSqrtCovariance (it does not return null)\n" +
			"The bridging from C# to java does not yet implement converting a C# ISqrtCovariance object"+
			"into the java counterpart \n"+
			"1) your matrix is diagonal -> have C# return null, we will use your method  getStandardDeviations\n"+
			"2) you have correleted observations -> sorry you are the first, you have to implement the converting\n";

		/* Check whether you are thirst one trying to deal with correlated observations */
		cli.OpenDA.DotNet.Interfaces.ISqrtCovariance dotNetSqrtCovariance = _dotNetStochObs.getSqrtCovariance();
		if (dotNetSqrtCovariance!=null){throw new RuntimeException(errorMsg);}

		/* Construct matrix using standard deviations */
		double [] returnValues =  _dotNetStochObs.getStandardDeviations();
		ISqrtCovariance sqrtVariance = new SqrtCovariance(returnValues);
		return sqrtVariance;
	}

	public IVector getStandardDeviations() {
		double[] returnValues =  _dotNetStochObs.getStandardDeviations();
		return new Vector(returnValues);
	}


	public ITime[] getTimes() {
		cli.OpenDA.DotNet.Interfaces.ITime[] dotNetTimes = _dotNetStochObs.get_Times();
		ITime[] times= new ITime[dotNetTimes.length];
		for (int iTime=0; iTime<dotNetTimes.length; iTime++){
			times[iTime] = new TimeN2J(dotNetTimes[iTime]);
		}
		return times;
	}


	public void free() {
		_dotNetStochObs.free();
	}


	public IObservationDescriptions getObservationDescriptions() {
		cli.OpenDA.DotNet.Interfaces.IObservationDescriptions dotNetObsDescr = _dotNetStochObs.getObservationDescriptions();
		return new ObservationDescriptionsJ2N(dotNetObsDescr);
	}


	public void setParent(IInstance parent) {
		//Hmm, do we care? Just write a message
		System.out.println("StochObserverN2J: call to dummy implementation of setParent");

		//throw new UnsupportedOperationException("Not implemented yet.");
		//To change body of implemented methods use File | Settings | File Templates.
	}


	public void initialize(File workingDir, String[] arguments) {
		/* Here we need to dynamically lad the C# stoch observer. */
		/* Arguments:
		  0: name of the DLL
		  1: name of the class
		  2: name of the configuration file
		 */
		String errorArguments="This stochObserver expects three arguments:\n"+
				"0: name of the DLL\n"+
				"1: name of the class\n"+
				"2: name of the configuration file\n"+
				"all separated by ;";
		if (arguments == null || arguments.length == 0) {
			errorArguments=errorArguments+"\n Input arguments are empty";
			throw new RuntimeException(errorArguments);
		}
		String[] subArgs = arguments[0].split(";");
		if (subArgs.length != 3) {
			errorArguments=errorArguments+"\nArguments are:";
			for (int i=0; i<subArgs.length; i++){
				errorArguments=errorArguments+"\n"+subArgs[i];
			}
			throw new RuntimeException(errorArguments);
		}
		/* use C# observer factory method to create an instance */
		cli.OpenDA.DotNet.Bridge.StochObserverFactory factory;
		factory = new StochObserverFactory();
		factory.Initialize(workingDir.getAbsolutePath(),subArgs);
		int outputLevel=0;
		_dotNetStochObs=factory.GetInstance(outputLevel);
	}

	public IInstance getParent() {
		throw new UnsupportedOperationException("Not implemented yet.");
	}
}
