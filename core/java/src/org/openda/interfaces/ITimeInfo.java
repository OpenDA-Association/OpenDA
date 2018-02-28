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

package org.openda.interfaces;

/**
 * Interface to get and set time information. Computations in OpenDA assume time in Modified Julian Days,
 * time since 00:00UTC November 17, 1858.
 * See for instance <a href="http://en.wikipedia.org/wiki/Julian_day">http://en.wikipedia.org/wiki/Julian_day</a>
 */
public interface ITimeInfo {

    /**
     * Get the times from the exchangeItem.
	 * If null is returned, the object does not now about time (because the exchange item has time info),
	 * but it can accept / produce values for any time.
     *
     * @return <code>null</code> if the exchange item knows can handle all times, series of time stamps/spans otherwise
     */
    double[] getTimes();

}
