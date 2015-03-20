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

package org.openda.model_efdc;

import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.IPrevExchangeItem;

/**
 * Exchange item for a time series stored in a .INP (EFDC input) file.
 *
 * @author Arno Kockx
 */
public class EfdcTimeSeriesExchangeItem extends TimeSeries {

    public EfdcTimeSeriesExchangeItem() {
        super();
    }

    @Override
    public Role getRole() {
        return IPrevExchangeItem.Role.Input;
    }
}
