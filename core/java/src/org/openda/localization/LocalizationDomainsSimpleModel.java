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

package org.openda.localization;

import org.openda.interfaces.*;
import org.openda.utils.Vector;
import java.util.Arrays;

/**
 * Created with IntelliJ IDEA.
 * User: dirk
 * Date: 1-6-16
 * Time: 15:05
 *
 *
 * Each domain consist of a vector of IVectors. For each model state, there is
 * a vector of observations that can be part of the state.
 *
 */
public class LocalizationDomainsSimpleModel implements ILocalizationDomains {
	private int[][] StateDomains = null;
	private int[][] ObservationDomains = null;

	public LocalizationDomainsSimpleModel() {
	}

	/**
	 * Create the localization based on the observationDescriptions
	 * This returns the localizationDomains.
	 *
	 * @param domains
	 * @return LocalizationDomain
	 */
	public LocalizationDomainsSimpleModel(int[][] domains)
	{
		InitializeDomains(domains);
	}

	public void InitializeDomains(int[][] domains){
		if (StateDomains != null)
		{
			StateDomains = null;
		}

		StateDomains = domains.clone();
		for (int i = 0; i < StateDomains.length; i++) {
			StateDomains[i] = domains[i].clone();
		}
	}

	public int getStateDomainCount()
	{
		if (StateDomains != null) {
			return StateDomains.length;
		}
		return 0;
	}

	public void setDomainObservations(IObservationDescriptions observationDescriptions)
	{
		for (int i = 0; i < StateDomains.length; i++)
		{
			setStateDomainObservations(i, observationDescriptions);
		}
	}

	public void setStateDomainObservations(int iDomain, IObservationDescriptions observationDescriptions)
	{
		int nobs = observationDescriptions.getObservationCount();
		IVector observationIdentityVector = new Vector(nobs);
		observationIdentityVector.setConstant(1.0);

	}


	public void setFullDomain(int nstate)
	{
		int[][] statedomains = new int[1][];
		statedomains[0] = new int[nstate];
		Arrays.fill(statedomains[0], 1);

		InitializeDomains(statedomains);
	}

	/**
	 * Get the selectors for each localization domain
	 * This returns what observations are present in each of the available domains.
	 *
	 * @return Selectors for all domains present.
	 */
	public int[][] getLocalizationStateDomainSelectors()
	{
		return StateDomains;
	}


	/**
	 * Get the selectors for a single localization domain
	 * This returns what observations are present in the seleted domain
	 *
	 * @param i index of the domain
	 * @return Selectors for the specified domain
	 */
	public int[] getLocalizationStateDomainSelector(int i)
	{
		return StateDomains[i];
	}

	/**
	 * Get the selectors for the full domain. That is a vector containing of 1.0 values
	 * with the correct size, based on the size of the observationdescriptions
	 *
	 * @param nstate
	 * @return Selector vector containing only 1.0
	 */
	public int[] getFullStateDomainSelector(int nstate, int nobs)
	{
		if (StateDomains == null || StateDomains[0].length != nobs)
		{
			this.setFullDomain(nstate);
		}
		return StateDomains[0];
	}

	/**
	 * Get the selectors for the full domain (previously defined).
	 * That is a vector containing of 1.0 values with the correct size,
	 * based on the size of the observation descriptor and the state
	 *
	 * @return Selector vector containing only 1.0
	 */
	public int[] getFullStateDomainSelector(){
		return StateDomains[0];
	}

	/**
	 * Get the selectors for the full domain (previously defined).
	 * That is a vector containing of 1.0 values with the correct size,
	 * based on the size of the observation descriptor and the state
	 *
	 * @return Selector vector containing the indices for the selection
	 */
	public int[] getObservationSelector(IObservationDescriptions observationDescriptions, int iDomain){
		int[] selector = new int[observationDescriptions.getObservationCount()];

		// In the simple model, all observations will count for each domain.
		for (int i = 0; i < selector.length; i++)
		{
			selector[i] = i;
		}

		return selector;
	}
}
