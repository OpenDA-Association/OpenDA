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
 * Abstract class that implements part of the ModelInstance interface based
 * on some assumptions about the data structures used. This class can be extended
 * to construct a toy model with little effort
 *
 * Assumes the following structure for the input file:
 * <?xml version="1.0" encoding="UTF-8"?>
 * <oscillatorConfig>
 *    <simulationTimespan>[0.0,0.05,10.0]</simulationTimespan>
 *    <!-- parameterValues for the model -->
 *    <parameterValues names="t_damp,omega">[8.0,1.5708]</parameterValues>
 *    <!-- uncertain parameterValues (for calibration) and their uncertainty -->
 *    <parameterUncertainty names="t_damp,omega">[1.0,0.1257]</parameterUncertainty>
 *    <!-- system noise for each state variable with bias and std per time-unit -->
 *    <systemNoise>{[0.0,0.0],[0.3,0.3]}</systemNoise>
 *    <!-- state vactor at initial time -->
 *    <initialState>[0.8,0.0]</initialState>
 *    <!-- std of uncertainty of state vector at initial time -->
 *    <initialStateUncertainty>[0.8,0.8]</initialStateUncertainty>
 * </oscillatorConfig>
 */

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using OpenDA.DotNet.Bridge;
using OpenDA.DotNet.Interfaces;
using OpenDA.DotNet.SDK;
using Time=OpenDA.DotNet.Bridge.Time;

namespace OpenDA.DotNet.Models
{
	public abstract class AbstractModelInstance : IModelInstance
	{
		//
		// parameters of the model
		//   that can be used as control variables with their statistics
		//
		protected Dictionary<string, double> parameterValues = new Dictionary<string, double>();

		//
		// parameters in terms of exchange items
		//
		protected Dictionary<string, DoublesExchangeItem> exchangeItems;

		// Internal state of the model
		protected IVector state;
		protected double t; //current time
		protected int timeStep; //integer value is used for testing equality and counting

		// Parameter used for calibration
		protected List<string> parNames = new List<string>();

		// Counter for keeping track of instances
		protected static int nextinstanceNumber = 1;
		protected int thisInstanceNumber;

		// Output storage
		// TODO make protected again when there is another mechanism to organize the output
		public bool storeObs;
		public List<double> tStore = new List<double>();
		public List<int> iStore = new List<int>();
		public List<IVector> xStore = new List<IVector>();
		public OutputLevel outputLevel = OutputLevel.ModelDefault;

		// Configuration
		protected string workingDir;
		protected string configstring;
		protected ConfigTreeJ2N conf;

		// ids for parameter/state/exchangeItems
		public const string StartTimeId = "t_start";
		public const string TimeStepId = "t_step";
		public const string StopTimeId = "t_stop";

		//
		// Abstract methods, to be implemented after initialization
		//
		protected abstract void AddExchangeItemsAfterInitialisation();
		protected abstract void HandleExchangeItemsBeforeCompute();
		protected abstract void UpdateExchangeItemsAfterCompute();

		public void Initialize(string workingDir, string configstring)
		{
			//
			//  GENERIC PART
			//

			// Instance counter
			thisInstanceNumber = nextinstanceNumber;
			nextinstanceNumber++;

			//now parse configuration
			//
			// <?xml version="1.0" encoding="UTF-8"?>
			// <oscillatorConfig>
			//<simulationTimespan>[0.0,0.05,10.0]</simulationTimespan>
			//<parameters names="t_damp,omega">[8.0,1.5708]</parameters>
			//<parameterUncertainty names="t_damp,omega">[1.0,0.1257]</parameterUncertainty>
			//<systemNoise>{[0.0,0.0],[0.3,0.3]}</systemNoise>
			//<initialState>[0.8,0.0]</initialState>
			//<initialStateUncertainty>[0.8,0.8]</initialStateUncertainty>
			//</oscillatorConfig>
			// 
			ResultsJ2N.PutMessage("configstring = " + configstring);
			conf = new ConfigTreeJ2N(workingDir, configstring);

			//
			// parse simulationTimespan
			//
			// <simulationTimespan>[0.0,0.05,10.0]</simulationTimespan>
			string timespanstring = conf.GetContentstring("simulationTimespan");
			if (timespanstring != null)
			{
				IVector timespanVector = new VectorJ2N(timespanstring);
				AddOrSetParameter(StartTimeId, timespanVector.GetValue(0));
				AddOrSetParameter(TimeStepId, timespanVector.GetValue(1));
				AddOrSetParameter(StopTimeId, timespanVector.GetValue(2));
				ResultsJ2N.PutMessage("simulationTimespan =" + timespanVector);
			}
			// start at initial time
			t = parameterValues[StartTimeId];
			timeStep = 0; //counter starts at 0

			//
			// parse parameters
			//
			string namestring = conf.GetAsstring("parameters@names", "");
			string parstring = conf.GetContentstring("parameters");
			if (parstring != null)
			{
				IVector parValues = new VectorJ2N(parstring);
				ResultsJ2N.PutMessage("parameters@names =" + namestring + " parameters=" + parValues);
				string[] names = namestring.Split(new[] {','});
				for (int i = 0; i < names.Length; i++)
				{
					if (!parameterValues.ContainsKey(names[i]))
					{
						parameterValues.Add(names[i], parValues.GetValue(i));
					}
					else
					{
						ResultsJ2N.PutMessage("parameter " + names[i] + " was not recognized. Value was ignored.");
					}
				}
			}

			CreateExchangeItems();
			AddExchangeItemsAfterInitialisation();
		}


		private void CreateExchangeItems()
		{
			exchangeItems = new Dictionary<string, DoublesExchangeItem>();
			foreach (string parameterId in parameterValues.Keys)
			{
				exchangeItems.Add(parameterId,
					new DoublesExchangeItem(parameterId, parameterId,
					Role.Input, parameterValues[parameterId]));
			}
		}

		public IInstance GetParent()
		{
			return null;
		}

		public ITime TimeHorizon
		{
			get
			{
				Time timeHorizon = new Time(parameterValues[StartTimeId], parameterValues[StopTimeId]);
				timeHorizon.StepMJD = parameterValues[TimeStepId];
				return timeHorizon;
			}
		}

		public ITime CurrentTime
		{
			get { return new Time(t); }
		}

		/// <summary>
		/// Compute time derivative of state at current time.
		/// This is used by the time-integration "mod.compute(t)" as the core of the model.
		/// </summary>
		/// <param name="xt">The current state</param>
		/// <param name="t">Current time</param>
		/// <returns>Vector Dx</returns>
		protected abstract IVector Dx(IVector xt, double t);

		public void Compute(ITime targetTime)
		{
			HandleExchangeItemsBeforeCompute();

			double tStep = parameterValues[TimeStepId];

			int nsteps = (int) Math.Round((targetTime.MJD - t)/tStep);
			IVector x = state;
			IVector xn;
			for (int i = 0; i < nsteps; i++)
			{
				// --> Runge-Kutta
				// System.out.print("step :"+i+" ");
				//- dx0 = dx(x,t);
				IVector dxdt0 = Dx(x, t);
				//- dx1 = dx(x+0.5*dx0,t+0.5*dt);
				xn = x.Clone();
				xn.Axpy(0.5*tStep, dxdt0);
				IVector dxdt1 = Dx(xn, t + 0.5*tStep);
				//- dx2 = dx(t+0.5*dt,x+0.5*dx1);
				xn = x.Clone();
				xn.Axpy(0.5*tStep, dxdt1);
				IVector dxdt2 = Dx(xn, t + 0.5*tStep);
				//- dx3 = dx(t+1.0*dt,x+1.0*dx2);
				xn = x.Clone();
				xn.Axpy(1.0*tStep, dxdt2);
				IVector dxdt3 = Dx(xn, t + 0.5*tStep);

				//- x = x + 1/6*dt*(dx0+2*dx1+2*dx2+dx3);
				x.Axpy(1.0/6.0*tStep, dxdt0);
				x.Axpy(2.0/6.0*tStep, dxdt1);
				x.Axpy(2.0/6.0*tStep, dxdt2);
				x.Axpy(1.0/6.0*tStep, dxdt3);

				t += tStep;
				timeStep++;
				if (storeObs)
				{
					// store all states if this is requested
					tStore.Add(t);
					xStore.Add(x.Clone());
					iStore.Add(timeStep);
				}
				if (outputLevel != OutputLevel.Suppress)
				{
					ResultsJ2N.PutValue("model_time", t, MessageType.Step);
					ResultsJ2N.PutValue("x", x, MessageType.Step);
				}
			}
			state.Values = x.Values;
			t += tStep;

			UpdateExchangeItemsAfterCompute();
		}

		public class SavedState : IModelState
		{
			public double time = -1;
			public int timestep = -1;
			public IVector state;

			public void SavePersistentState(string file)
			{
				StreamWriter outStream = new StreamWriter(file);
				// save state vector
				string line = "x=" + state;
				outStream.Write(line + "\n");
				// save time
				line = "t=" + time;
				outStream.Write(line + "\n");
				// save timestep
				line = "timestep=" + timestep;
				outStream.Write(line + "\n");
				outStream.Close();
			}
		}

		public IModelState SaveInternalState()
		{
			SavedState saveState = new SavedState();
			saveState.time = t;
			saveState.timestep = timeStep;
			saveState.state = state.Clone();
			return saveState;
		}

		public void RestoreInternalState(IModelState savedInternalState)
		{
			SavedState saveState = (SavedState) savedInternalState;
			state = saveState.state.Clone();
			timeStep = saveState.timestep;
			t = saveState.time;
		}

		public void ReleaseInternalState(IModelState savedInternalState)
		{
			SavedState saveState = (SavedState) savedInternalState;
			saveState.time = -1;
			saveState.timestep = -1;
			saveState.state = null;
		}

		public IModelState LoadPersistentState(string persistentStateFile)
		{
			VectorJ2N x;
			double persistentTimeStamp;
			int timestep;
			try
			{
				if (!File.Exists(persistentStateFile))
				{
					throw new Exception("Could not find file for saved state:" + persistentStateFile);
				}
				// read state and time from file
				StreamReader buff = new StreamReader(persistentStateFile);
				// Read and parse first line
				string line = buff.ReadLine();
				string[] keyValuePair = line.Split(new[] {'='}); // expect eg x=[1.0,2.0,3.0]
				x = new VectorJ2N(keyValuePair[1]);
				// Read and parse second line
				line = buff.ReadLine();
				keyValuePair = line.Split(new[] {'='}); // expect eg t=1.0
				persistentTimeStamp = Double.Parse(keyValuePair[1]);
				// Read and parse third line
				line = buff.ReadLine();
				keyValuePair = line.Split(new[] {'='}); // expect eg timestep=3
				timestep = Int32.Parse(keyValuePair[1]);
				buff.Close();
			}
			catch (IOException e)
			{
				ResultsJ2N.PutMessage("Exception: " + e.Message);
				throw new Exception("Error reading restart file for model");
			}
			// set state and time
			SavedState saveState = new SavedState();
			saveState.time = persistentTimeStamp;
			saveState.timestep = timestep;
			saveState.state = x;
			return saveState;
		}

		public string ModelRunDirPath
		{
			get { throw new NotImplementedException(); }
		}

		public void Finish()
		{
			// no action needed (yet)
		}


		public void PrintStoredStates()
		{
			Console.Out.WriteLine("--------------");
			for (int i = 0; i < iStore.Count; i++)
			{
				Console.Out.WriteLine("t=" + tStore[i] + " x=" + xStore[i]);
			}
			Console.Out.WriteLine("--------------");
		}


		public IVector GetObservedValues(IObservationDescriptions descr)
		{
			IVector result = new VectorJ2N(descr.ObservationCount);

			// TODO: handle TimeSeriesObservationDescriptions
			IVector obsTimes = descr.GetValueProperties("time");

			IVector obsIndex = descr.GetValueProperties("index") ?? descr.GetValueProperties("xPosition");

			IVector transformIndex;
			try
			{
				transformIndex = descr.GetValueProperties("transform");
			}
			catch (Exception)
			{
				transformIndex = null;
			}

			Time tHor = new Time(parameterValues[StartTimeId],
			                     parameterValues[StopTimeId])
				;
			tHor.StepMJD = parameterValues[TimeStepId];

			if (storeObs)
			{
				//work from stored states
				for (int i = 0; i < descr.ObservationCount; i++)
				{
					// at which timestep is this obs
					long iObs = Utils.GetTimeStep(tHor, (new Time(obsTimes.GetValue(i))));
					// find corresponding storage location
					int thisTIndex = iStore.IndexOf((int) iObs); //time index in storage
					if (thisTIndex < 0)
					{
						throw (new Exception("model.getValues: time out of range for observation nr. " + i));
					}
					int thisXIndex = (int) obsIndex.GetValue(i);
					if ((thisXIndex < 0) | (thisXIndex >= state.Size))
					{
						throw (new Exception("model.getValues: index out of range for "
						                     + " observation nr. " + i
						                     + " index= " + thisXIndex));
					}
					//Console.Out.WriteLine("i="+i+" it="+thisTIndex+" ind= "+thisXIndex);
					double thisValue = xStore[thisTIndex].GetValue(thisXIndex);
					// transform values if needed
					if (transformIndex != null)
					{
						if (transformIndex.GetValue(i) == 2)
						{
							// magic number for quadratic observations
							thisValue = thisValue*thisValue;
						}
					}
					result.SetValue(i, thisValue);
				}
			}
			else
			{
				// only current state is available
				for (int i = 0; i < descr.ObservationCount; i++)
				{
					int thisXIndex = (int) obsIndex.GetValue(i);
					//TODO if(){ //check time
					double thisValue = state.GetValue(thisXIndex);
					// transform values if needed
					if (transformIndex != null)
					{
						if (transformIndex.GetValue(i) == 2)
						{
							// magic number for quadratic observations
							thisValue = thisValue*thisValue;
						}
					}
					result.SetValue(i, thisValue);
				}
			}

			return result;
		}

        public IVector[] GetObservedLocalization(string dummy, IObservationDescriptions observationDescriptions, double distance){
           return null;
        }

		public IVector[] GetObservedLocalization(IObservationDescriptions observationDescriptions, double distance)
		{
			/* Return identity localization */
			// Console.Out.WriteLine("Return identity localization");
			int nObs = observationDescriptions.ObservationCount;
			IVector locvec = new VectorJ2N(state.Size);
			IVector[] result = new IVector[nObs];

			for (int iObs = 0; iObs < nObs; iObs++)
			{
				locvec.SetConstant(1.0);
				result[iObs] = locvec;
			}
			return result;
		}

		public override string ToString()
		{
			string result = GetType().Name + " " + thisInstanceNumber;
			result += "\n   : t= " + CurrentTime.ToString();
			result += "\n   : x= " + state;
			return result;
		}

		//
		//	Exchange items
		//

		public string[] ExchangeItemIDs
		{
			get { return parameterValues.Keys.ToArray(); }
		}

		public string[] GetExchangeItemIDs(int roleAsInt)
		{
			if (Utils.RoleMapJ2N(roleAsInt) == Role.InOut)
			{
				return ExchangeItemIDs;
			}
			throw new InvalidOperationException(
				"OpenDA.DotNet.Models.SimpleOscillatorModelInstance.getExchangeItemIDs(): Role selection not implemented yet.");
		}

		public IExchangeItem GetExchangeItem(string exchangeItemID)
		{
			if (!exchangeItems.Keys.Contains(exchangeItemID))
			{
				throw new ArgumentException("Unknown exchange item id " + exchangeItemID, "exchangeItemID");
			}
			return exchangeItems[exchangeItemID];
		}

		protected void AddOrSetParameter(string parameterId, double value)
		{
			if (parameterValues.ContainsKey(StartTimeId))
			{
				parameterValues[parameterId] = value;
			}
			else
			{
				parameterValues.Add(parameterId, value);
			}
		}

	}
}
