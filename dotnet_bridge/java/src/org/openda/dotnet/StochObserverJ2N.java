/* MOD_V2.0 
* Copyright (c) 2012 OpenDA Association
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
import org.openda.interfaces.*;
import org.openda.utils.ObjectSupport;

import java.io.File;

/**
 * Bridge create .net Stoch Observer from java Stoch Observer
 */
public class StochObserverJ2N implements IStochObserver {

	IStochObserver _javaStochObserver;
	private IInstance _parent = null;

	public StochObserverJ2N() {
	}

	public StochObserverJ2N(IStochObserver javaStochObserver) {
		_javaStochObserver = javaStochObserver;
	}

	@Override
	public IStochObserver createSelection(String selection) {
		return new StochObserverJ2N(_javaStochObserver.createSelection(selection));
	}

	@Override
	public IStochObserver createSelection(ITime selectionTimes) {
		return new StochObserverJ2N(_javaStochObserver.createSelection(selectionTimes));
	}

	@Override
	public IStochObserver createSelection(Type observationType) {
		throw new UnsupportedOperationException("org.openda.dotnet.StochObserverJ2N.createSelection(): Not implemented yet.");
	}

	@Override
	public ISelector createSelector(Type observationType) {
		throw new UnsupportedOperationException("org.openda.dotnet.StochObserverJ2N.createSelector(): Not implemented yet.");
	}

	@Override
	public int getCount() {
		return _javaStochObserver.getCount();
	}

	@Override
	public IVector getValues() {
		return _javaStochObserver.getValues();
	}

	@Override
	public IVector getRealizations() {
		return _javaStochObserver.getRealizations();
	}

	@Override
	public IVector getExpectations() {
		return _javaStochObserver.getExpectations();
	}

	@Override
	public double evaluatePDF(IVector values) {
		return _javaStochObserver.evaluatePDF(values);
	}

	@Override
	public IVector evaluateMarginalPDFs(IVector values) {
		return _javaStochObserver.evaluateMarginalPDFs(values);
	}

	@Override
	public ISqrtCovariance getSqrtCovariance() {
		return _javaStochObserver.getSqrtCovariance();
	}

	@Override
	public IVector getStandardDeviations() {
		return _javaStochObserver.getStandardDeviations();
	}

	@Override
	public ITime[] getTimes() {
		return _javaStochObserver.getTimes();
	}

	@Override
	public void free() {
		_javaStochObserver.free();
	}

	@Override
	public IObservationDescriptions getObservationDescriptions() {
		return new ObservationDescriptionsN2J(_javaStochObserver.getObservationDescriptions());
	}

	@Override
	public void setParent(IInstance parent) {
		_parent = parent;
	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		if (arguments == null || arguments.length == 0) {
			throw new RuntimeException("Arguments expected");
		}
		String[] subArgs = arguments[0].split(";");
		if (subArgs.length != 2) {
			throw new RuntimeException("Arguments expected: java-class-name;config-file-or-string");
		}
		_javaStochObserver = (IStochObserver) ObjectSupport.createNewInstance(subArgs[0], IStochObserver.class); 
		_javaStochObserver.initialize(workingDir, new String[]{subArgs[1]});
	}

	@Override
	public IInstance getParent() {
		return _parent;
	}
}
