/*
 * Copyright (c) 2019 OpenDA Association
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

package org.openda.interfaces;

/**
 * Created with IntelliJ IDEA.
 * User: dirk
 * Date: 1-6-16
 * Time: 15:05
 *
 */
public interface ILocalizationDomains{
	void setStateDomainObservations(int iDomain, IObservationDescriptions observationDescriptions);

	/**
	 * Return the number of domains for local analysis
	 * @return
	 */
	int getStateDomainCount();


	/**
	 * Return the indices of the observations which need to be taken into account for a local analysis
	 * @param observationDescriptions Meta information (descriptions) of the available observations
	 * @param iDomain Domain number
	 * @return indiced of the observations needed for analysis in domain iDomain
	 */
	int[] getObservationSelector(IObservationDescriptions observationDescriptions, int iDomain);
}

