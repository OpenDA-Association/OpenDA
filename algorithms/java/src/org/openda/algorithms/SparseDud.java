/* OpenDA v2.4.3 
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


package org.openda.algorithms;

import org.openda.interfaces.*;
import org.openda.utils.Results;
import org.openda.utils.ConfigTree;

import java.util.ArrayList;
import java.util.List;


/**
 * OpenDA Sparse Dud algorithm
 */
public class SparseDud extends BaseDud {

	
	protected BaseDudCoreOptimizer InitializeDudCoreOptimizer() {
		boolean verbose=true;
		return new SparseDudCoreOptimizer(J, findNonZeros(!verbose));
	}

	/**
	 * 
	 * Create a matrix of zeros and ones, to indicate the sparseness pattern of the 
	 * dependencies.
	 *
	 * @param verbose Report detailed messages?
	 * @return The sparseness pattern (zeros/ones) of the dependencies between obs. and parameters.
	 */
	private int[][] findNonZeros( boolean verbose) {
		//Get the parameter names
		ArrayList<String> parameterNames = getParameterNames();
		String[]          observationIds = getObservationIds();
		int npar  = parameterNames.size();
		int npred = stochObserver.getCount();
		Results.putProgression("I have "+npar +" parameters\n");
		Results.putProgression("I have "+npred+" Predictions\n");
		
		int [][] nonZeros = new int[npred][npar];
		
		for (int ipred=0; ipred<npred; ipred++) {
			for (int ipar=0; ipar<npar; ipar++) {
				nonZeros[ipred][ipar] = 0;
			}
		}

		ConfigTree[] subTrees = configtree.getSubTrees("dependencies/obs");
		List<IPrevExchangeItem> exchangeItems = stochObserver.getObservationDescriptions().getExchangeItems();
		checkDependenciesForAllExchangeItems(subTrees, exchangeItems);

		boolean[] allParsFound = new boolean[npar];
		// FOR (all observations for which dependencies are given) DO
		for(ConfigTree dependency : subTrees) {
			String obsnam = dependency.getAsString("@id", "unknown prediction");
			
			// FOR (all dependencies given)
			for(ConfigTree depends_on : dependency.getSubTrees("depends_on/par")) {
				String parnam = depends_on.getAsString("@id", "unknown parameter");
				
				// Find parameter index 
				int parFound = 0;
				for (int jpar=0; jpar<npar; jpar++) {
					if (parameterNames.get(jpar).equals(parnam)) {
						allParsFound[jpar] = true;
						parFound = 1;
						int predFound =  0;
						// Find (non-unique) observation indices
						for (int ipred = 0; ipred < npred; ipred++) {
							if (observationIds[ipred].equals(obsnam)) {
								// Fill in dependencies
								nonZeros[ipred][jpar] = 1;
								predFound = 1;
							}
						}

						if ( predFound==0) {
							String allObservations="";
							if (exchangeItems != null) {
								for (IPrevExchangeItem exchangeItem : exchangeItems) {
									allObservations +=  "\t'" + exchangeItem.getId() + "'\n";
								}
							} else {
								for (int ipred=0; ipred<npred; ipred++) {
									allObservations +=  "\t'" + observationIds[ipred] + "'\n";
								}
							}
							throw new RuntimeException( "The observations '"+obsnam+"' could not be found.\n" + "Available observations are:\n"+allObservations);
						}
						break;
					}
				}
				if ( parFound==0) {
				// Make a pretty string of all parameter names, because the parameter
				// in the input file cannot be found. In that case, the error message will 
				// contain a list of possible parameter names
					String allParameters="";
					for (int jpar=0; jpar<npar; jpar++) {
						allParameters +=  "\t'" + parameterNames.get(jpar) + "'\n";
					}
 
					throw new RuntimeException( "The parameter '"+parnam+"' could not be found.\n"+ "Available parameters are:\n"+allParameters);
				}
			}
		}

		checkDependenciesForAllParameters(parameterNames, allParsFound);

		// print the nonZeros
		if (verbose) {
			for (int ipred=0; ipred<npred; ipred++) {
				String line = "";
				for (int ipar=0; ipar<npar; ipar++) {
					line += nonZeros[ipred][ipar];      	        		 
				}
				Results.putProgression(line + "  "+ observationIds[ipred]);
			}
		}
		return nonZeros;
	}

	private void checkDependenciesForAllParameters(ArrayList<String> parameterNames, boolean[] allParsFound) {
		int parsNotFound = 0;
		for (int i = 0; i < allParsFound.length; i++) {
			if (allParsFound[i]) continue;
			Results.putMessage("No dependency found for parameter " + parameterNames.get(i));
			parsNotFound++;
		}
		if (parsNotFound > 0)
			throw new RuntimeException(parsNotFound + " parameter(s) found without any dependency");
	}

	private void checkDependenciesForAllExchangeItems(ConfigTree[] subTrees, List<IPrevExchangeItem> exchangeItems) {
		boolean anyExchangeItemNotFound = false;
		for (IPrevExchangeItem exchangeItem : exchangeItems) {
			String exchangeItemId = exchangeItem.getId();
			boolean exchangeItemFound = false;
			for (ConfigTree subTree : subTrees) {
				String dependencyId = subTree.getAsString("@id", "unknown prediction");
				if (exchangeItemId.equals(dependencyId)) exchangeItemFound = true;
			}
			if (!exchangeItemFound) {
				Results.putMessage("No dependency found for exchange item " + exchangeItemId);
				anyExchangeItemNotFound = true;
			}
		}
		if (anyExchangeItemNotFound)
			throw new RuntimeException("Exchange items found without any dependency");
	}

}
