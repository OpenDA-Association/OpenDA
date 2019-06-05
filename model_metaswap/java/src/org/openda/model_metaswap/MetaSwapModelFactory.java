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
package org.openda.model_metaswap;
import org.openda.blackbox.wrapper.BBModelFactory;
import org.openda.blackbox.wrapper.BBModelInstance;
import org.openda.interfaces.IStochModelFactory;

/**
 * Created by hummel on 2018-03-28.
 */
public class MetaSwapModelFactory extends BBModelFactory {

	public BBModelInstance getInstance(String[] arguments, IStochModelFactory.OutputLevel outputLevel) {
		int newInstanceNumber=instanceNumber.val();
		instanceNumber.inc();
		return new MetaSwapModelInstance(this.bbModelConfig, newInstanceNumber, this.timeHorizon);
	}
}
