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

/**
* The oscillator model
*
* simple linear oscilator (e.g. mass-spring system with friction)
* d(x)/d(t) = u
* d(u)/d(t) = - omega^2 * x - (2/t_damp) u
*/

using System;
using OpenDA.DotNet.Bridge;
using OpenDA.DotNet.Interfaces;
using OpenDA.DotNet.SDK;

namespace OpenDA.DotNet.Models
{
	public class SimpleOscillatorModelInstance : AbstractModelInstance
	{
		private const string omegaId = "omega";
		private const string tDampId = "t_damp";

		public const string State0Id = "state[0]";
		public const string State1Id = "state[1]";

		//
		//Most data structures are contained in AbstractModelInstance
		//The abstract SimpleModel is based on the following items. Make
		//sure you put your data in the proper items
		//
		//-	parameters of the model that can be used as control variables with their statistics
		//		java.util.Hashtable<String,Double> parameters=null;
		//-	Internal state of the model
		//		Vector state=null;
		//-	Current time
		//		double t;
		//		int timeStep; //integer value is used for testing equality and counting
		//-	Parameters used for calibration
		//		java.util.Vector<String> ParNames=null;
		//		Vector Pars=null; //present values as a vector
		//-	System noise for Kalman filtering, as standarddeviation per unit time
		//		Vector sysNoiseIntensity=null;
		//

		public SimpleOscillatorModelInstance(string workingDir, string configString)
		{
			//
			//  DEFAULTS
			//

			// parameters of the model (deterministic part)
			parameterValues.Add(tDampId, 8.0); //characteristic time-scale for friction [seconds]
			parameterValues.Add("omega", 0.25*2.0*Math.PI); //oscilation frequency [rad/s]
			// timespan for the simulation
			AddOrSetParameter(StartTimeId, 0.0);
			AddOrSetParameter(StopTimeId, 10.0);
			AddOrSetParameter(TimeStepId, 0.05);

			// Internal state of the model
			state = new VectorJ2N("[0.8,0.0]");

			//
	        // now parse configuration
			//
			Initialize(workingDir, configString);
		}

		protected override void AddExchangeItemsAfterInitialisation()
		{
			DoublesExchangeItem stateExchangeItem0 = new DoublesExchangeItem(State0Id, State0Id,
			                                                    Role.Input, state.GetValue(0));
			DoublesExchangeItem stateExchangeItem1 = new DoublesExchangeItem(State0Id, State0Id,
																Role.Input, state.GetValue(1));
			stateExchangeItem0.Times = new[] { t };
			stateExchangeItem1.Times = new[] { t };
			exchangeItems.Add(State0Id, stateExchangeItem0);
			exchangeItems.Add(State1Id,
					stateExchangeItem1);
		}

		protected override void HandleExchangeItemsBeforeCompute()
		{
			state.SetValue(0, exchangeItems[State0Id].ValuesAsDoubles[0]);
			state.SetValue(1, exchangeItems[State1Id].ValuesAsDoubles[0]);
		}

		protected override void UpdateExchangeItemsAfterCompute()
		{
			exchangeItems[State0Id].Times = new[] {t};
			exchangeItems[State1Id].Times = new[] {t};

			exchangeItems[State0Id].Values = state.GetValue(0);
			exchangeItems[State1Id].Values = state.GetValue(1);
		}

		/**
     * Compute time derivative of state at current time.
     * This is used by the time-integration "mod.compute(t)" as the core of the model.
     * @return vector dt
     */

		protected override IVector Dx(IVector xt, double t)
		{
			IVector result = new VectorJ2N(2);
			// The oscillator model
			//
			// simple linear oscilator (e.g. mass-spring system with friction)
			//% d(x)/d(t) = u
			// d(u)/d(t) = - omega^2 * x - (2/t_damp) u
			//
			double[] x = xt.Values;
			double omega = Math.Max(0.0, parameterValues[omegaId]);
			double tDamp = Math.Max(0.0, parameterValues[tDampId]);
			result.SetValue(0, x[1]);
			result.SetValue(1, -(omega*omega)*x[0] - (2.0/tDamp*x[1]));

			return result;
		}
	}
}