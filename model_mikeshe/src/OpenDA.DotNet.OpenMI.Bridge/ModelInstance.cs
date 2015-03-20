/* MOD_V1.0 
* Copyright (c) 2011 OpenDA Association 
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


using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using OpenDA.DotNet.Bridge;
using OpenDA.DotNet.Interfaces;
using OpenMI.Standard2;
using OpenMI.Standard2.TimeSpace;
using ITime=OpenDA.DotNet.Interfaces.ITime;


//using Oatc.OpenMI.Sdk.Backbone;
//using Oatc.OpenMI.Sdk.Backbone.Generic;
//using Oatc.OpenMI.Sdk.Buffer;

namespace OpenDA.DotNet.OpenMI.Bridge
{
	public class ModelInstance : IModelInstance
	{
		private readonly ModelFactory _openDaModelFactory;
		private readonly ITimeSpaceComponent _openMIComponent;
        private readonly IDictionary<String, ExchangeItem> _inputs = new Dictionary<string, ExchangeItem>();
   private readonly IDictionary<String, ExchangeItem> _outputs = new Dictionary<string, ExchangeItem>();
        private readonly IDictionary<String, ExchangeItem> _inout = new Dictionary<string, ExchangeItem>();

        private int _mSheTstep;
        private double _finalTimeOfSimulation; 
	    private IList<int> _listModelIndicesAtObservations; 

	    private ITimeSpaceComponentExtensions _extendedComponent;


        public ModelInstance(ModelFactory myModelFactory, ITimeSpaceComponent openMIComponent)
		{
            _mSheTstep = 0;
			_openDaModelFactory = myModelFactory;
			_openMIComponent = openMIComponent;
            GetExchangeItemsFromOpenMIComponent();
            _openMIComponent.Update();
        }

		public IInstance GetParent()
		{
			return _openDaModelFactory;
		}

	    public ITimeSpaceComponent OpenMIComponent
	    {
	        get { return _openMIComponent; }
	    }
        /* Note: With New Verson - must remove this method
         * 
	    public bool DoWrite()
		{
			return GetParent().DoWrite();
		}
        */
		public ITime TimeHorizon
		{
			get
			{
			    global::OpenMI.Standard2.TimeSpace.ITime timeHorizon = _openMIComponent.TimeExtent.TimeHorizon;
			    double beginTime = timeHorizon.StampAsModifiedJulianDay;
                double endTime = beginTime + timeHorizon.DurationInDays;
                //We do not have info on the timestepping. Hence we fill in NaN. Noise models will then use their own specified stepping scheme.
                double stepMJD = Double.NaN; 
                DotNet.Bridge.Time openDATimeHorizon = new DotNet.Bridge.Time(beginTime, endTime, stepMJD, true);
                return openDATimeHorizon;
			}
		}

		public ITime CurrentTime
		{
			get
			{
                return DetermineCurrentTime(TimeHorizon.BeginTime);
			}
		}

	    public void Compute(ITime targetTime)
		{
            ITime lastTime = DetermineCurrentTime(null);
            //double FinalSimTime = TimeHorizon.MJD ;
            double FinalSimTime = TimeHorizon.EndTime.MJD;

            // Get the precision for comparing times as specified by the user.
            double timePrecisionMJD = org.openda.utils.performance.OdaGlobSettings.getTimePrecision();


            ITime current = lastTime;
            if (lastTime == null)
		    {
				throw new Exception("Could not find time/space output item to check model's current time");
			}

            // Note: we are absolutely not sure why we have the second part in the while check!!!!
            while (lastTime.EndTime.MJD+timePrecisionMJD < targetTime.MJD && FinalSimTime - timePrecisionMJD > lastTime.EndTime.MJD)
			{
                current = lastTime;
			    try
			    {
			        _openMIComponent.Update();
			    }
			    catch (Exception e)
			    {
			        FileStream fileStream = File.Open("MShe_Exceptions.txt", FileMode.Append);
                    StreamWriter streamWriter = new StreamWriter(fileStream);
                    streamWriter.WriteLine("Exception in Update()" + e);
                    streamWriter.Close();
			    }

                // Get the timeStamp of the Model. This is used in case time information is missing in the exhangeItems of the Model
                ITime timeStampModel = DetermineCurrentTime(null);

			    foreach (KeyValuePair<string, ExchangeItem> output in _outputs)
			    {
                    output.Value.AddNewOpenMIOutputValuesToBuffer(timeStampModel.MJD);
                    output.Value.UpdatetstepCount();
                    
                }
                foreach (KeyValuePair<string, ExchangeItem> inout in _inout)
                {
                    inout.Value.AddNewOpenMIOutputValuesToBuffer(timeStampModel.MJD);
                    inout.Value.UpdatetstepCount();
                }
                foreach (KeyValuePair<string, ExchangeItem> input in _inputs)
                {
                    input.Value.UpdatetstepCount();
                }
                lastTime = DetermineCurrentTime(null);
                _mSheTstep++;
			}
		}

		public string[] ExchangeItemIDs
		{
			get
			{
				string[] exchangeItemIDs = new string[_inputs.Count + _outputs.Count];
				_inputs.Keys.CopyTo(exchangeItemIDs, 0);
				_outputs.Keys.CopyTo(exchangeItemIDs, _inputs.Count);
				return exchangeItemIDs;
			}
		}

		public string[] GetExchangeItemIDs(int roleAsInt)
		{
			Role role = DotNet.Bridge.Utils.RoleMapJ2N(roleAsInt);
			string[] exchangeItemIDs;
			switch (role)
			{
				case Role.Input:
					exchangeItemIDs = new string[_inputs.Count];
					_inputs.Keys.CopyTo(exchangeItemIDs, 0);
					break;
				case Role.Output:
					exchangeItemIDs = new string[_inputs.Count];
					_outputs.Keys.CopyTo(exchangeItemIDs, 0);
					break;
				case Role.InOut:
					exchangeItemIDs = ExchangeItemIDs;
					break;
				default:
					throw new Exception("Invalid Role value: " + role);
			}
			return exchangeItemIDs;
		}

		public IExchangeItem GetExchangeItem(string exchangeItemID)
		{
            // ZZZ Removes the . at the end of the exchangeItemID if there is one
            // Try and Map what's after the . to a location index
            char[] delimiterChars = { '.'};
            string[] idx = exchangeItemID.Split(delimiterChars);
            int idxTemp;
            if (idx.Length == 2)
            {
                if (_inout.ContainsKey(idx[1]))
                {
                    ExchangeItem I = _inout[idx[1]];

                    try
                    {
                        idxTemp = Convert.ToInt32(idx[0]);
                        I.AddSingleIdx(idxTemp);
                    }
                    catch (Exception e)
                    {

                        System.Console.WriteLine("ExchangeItem index invalid (not an integer): " + e.Message);

                    }
                    return I;
                    //return _outputs[exchangeItemID];
                }

                if (_outputs.ContainsKey(idx[1]))
                {
                    ExchangeItem I = _outputs[idx[1]];
                    try
                    {
                        idxTemp = Convert.ToInt32(idx[0]);
                        I.AddSingleIdx(idxTemp);
                    }
                    catch (Exception e)
                    {

                        System.Console.WriteLine("ExchangeItem index invalid (not an integer): " + e.Message);

                    }
                    return I;
                    //return _outputs[exchangeItemID];
                }
                if (!_inputs.ContainsKey(idx[1]))
		        {
		            throw new Exception("Could not find exchange item \"" + exchangeItemID + "\"");
		        }
                return _inputs[idx[1]];
            }
            if (idx.Length == 1)
            {
                if (_inout.ContainsKey(idx[0]))
                {
                    ExchangeItem I = _inout[idx[0]];

                    
                    return I;
                    //return _outputs[exchangeItemID];
                }

                if (_outputs.ContainsKey(idx[0]))
                {
                    ExchangeItem I = _outputs[idx[0]];

                    return I;
                    //return _outputs[exchangeItemID];
                }
                if (!_inputs.ContainsKey(idx[1]))
                {
                    throw new Exception("Could not find exchange item \"" + exchangeItemID + "\"");
                }
                return _inputs[idx[1]];
            }
            return null;
    
		}

		public IModelState SaveInternalState()
		{
			throw new NotImplementedException();
		}

		public void RestoreInternalState(IModelState savedInternalState)
		{
			throw new NotImplementedException();
		}

		public void ReleaseInternalState(IModelState savedInternalState)
		{
			throw new NotImplementedException();
		}

		public IModelState LoadPersistentState(string algorithmStateFilePath)
		{
			throw new NotImplementedException();
		}


        /// <summary>
        /// Get the values from the model corresponding to the observations.
        /// The first this is called, _listModelIndicesAtObservations will be null so a list must be created.
        /// </summary>
        /// <param name="observationDescriptions">The observations.</param>
        /// <returns>Model values corresponding to observation points.</returns>
        public IVector GetObservedValues(OpenDA.DotNet.Interfaces.IObservationDescriptions observationDescriptions)
        {
            if (_extendedComponent == null && _openMIComponent is ITimeSpaceComponentExtensions)
            {
                    _extendedComponent = (ITimeSpaceComponentExtensions) _openMIComponent;
                    _listModelIndicesAtObservations = _extendedComponent.CreateModelIndicesHashTable(observationDescriptions);
                    return new Vector(_extendedComponent.getObservedValues(observationDescriptions));
            }
            else if (_listModelIndicesAtObservations != null)
            {
                double[] modelValuesAtIndices = _extendedComponent.ModelValuesAtProvidedIndices(observationDescriptions,
                                                                                         _listModelIndicesAtObservations);

                return new Vector(modelValuesAtIndices);
            }
            return null;


        }

        public IVector[] GetObservedLocalization(String exchangeItemID, IObservationDescriptions observationDescriptions, double distance)
        {
            if (_openMIComponent is ITimeSpaceComponentExtensions)
            {
                int nObs = observationDescriptions.ObservationCount;
                IVector[] retVectors = new IVector[nObs];

                ITimeSpaceComponentExtensions extendedComponent = (ITimeSpaceComponentExtensions)_openMIComponent;
                double[][] mask = extendedComponent.getLocalization(exchangeItemID, observationDescriptions, distance);
                if (mask != null)
                {
                    for (int iObs = 0; iObs < nObs; iObs++)
                    {
                        retVectors[iObs] = new OpenDA.DotNet.Bridge.Vector(mask[iObs]);
                    }                    
                }
                return retVectors;
            }
            else
            {
                throw new NotImplementedException();
                return null;
            }
        }

        public IVector[] GetObservedLocalization(IObservationDescriptions observationDescriptions, double distance)
		{
            throw new NotImplementedException();

            //NOTE OLD STUFF
            if (_openMIComponent is ITimeSpaceComponentExtensions)
            {
                ITimeSpaceComponentExtensions extendedComponent = (ITimeSpaceComponentExtensions)_openMIComponent;
                //Determine the number of observations (=number of columns)
                int nObs = observationDescriptions.ObservationCount;
                org.openda.utils.TreeVector[] retVectorsTree = new org.openda.utils.TreeVector[nObs];
                for (int iObs = 0; iObs < nObs; iObs++)
                {
                    retVectorsTree[iObs] = new org.openda.utils.TreeVector("localizationMask");
                }

                // Loop over all active echangeItems
                // Note interface of this method must be improved to fix and answer the question which echangeItems are inwhat order in the state vector
                foreach (KeyValuePair<string, ExchangeItem> inout in _inout)
                {
                    string exchangeItemID = inout.Key;
                    double[][] mask = extendedComponent.getLocalization(exchangeItemID, observationDescriptions, distance);
                    if (mask != null)
                    {
                        for (int iObs = 0; iObs < nObs; iObs++)
                        {
                            org.openda.utils.Vector vec = new org.openda.utils.Vector(mask[iObs]);
                            org.openda.utils.TreeVector subVec = new org.openda.utils.TreeVector(exchangeItemID, vec);
                            retVectorsTree[iObs].addChild(subVec);
                        }
                    }
                }
                // Cast fancy treevectors to vectors
                IVector[] retVectors = new IVector[nObs];
                for (int iObs = 0; iObs < nObs; iObs++)
                {
                    double[] values = retVectorsTree[iObs].getValues();
                    retVectors[iObs] = new OpenDA.DotNet.Bridge.Vector(values);
                }
                return retVectors;
            }
            else
            {
                throw new NotImplementedException();
                return null;
            }

        }

        public string ModelRunDirPath
		{
            get { return null; }  // model instance's run dir is not known
		}

		public void Finish()
		{
            _openMIComponent.Finish();
		}

        private ITime DetermineCurrentTime(ITime currentTime)
        {
            /* In case the component implements some extensions, use them */
            if (_openMIComponent is ITimeSpaceComponentExtensions){
                ITimeSpaceComponentExtensions extendedComponent = (ITimeSpaceComponentExtensions) _openMIComponent;
                double now = extendedComponent.currentTime().StampAsModifiedJulianDay;
                return new DotNet.Bridge.Time(now);
            }
            /* We have to fall back on a proper time administration in the exhange items */

                foreach (KeyValuePair<string, ExchangeItem> output in _outputs)
                {
                    if (output.Value.Times != null && output.Value.Times.Length > 0)
                    {
                        double timeAsMJD = output.Value.Times[output.Value.Times.Length - 1];
                        currentTime = new DotNet.Bridge.Time(timeAsMJD);
                        if (currentTime != null)
                        {
                            return currentTime;
                        }

                    }

                }

                foreach (KeyValuePair<string, ExchangeItem> inout in _inout)
                {
                    if (inout.Value.Times != null && inout.Value.Times.Length > 0)
                    {
                        double timeAsMJD = inout.Value.Times[inout.Value.Times.Length - 1];
                        currentTime = new DotNet.Bridge.Time(timeAsMJD);

                        break;
                    }
                    break;
                }

                if (currentTime == null)
                {
                    currentTime = TimeHorizon.BeginTime;
                }
                return currentTime;
           // }
        }



        private ITime DetermineFinalTime(ITime currentTime)
        {
            foreach (KeyValuePair<string, ExchangeItem> output in _outputs)
            {

                if (output.Value.Times != null && output.Value.Times.Length > 0)
                {
                    double timeAsMJD = output.Value.Times[output.Value.Times.Length - 1];

                    currentTime = new DotNet.Bridge.Time(timeAsMJD);
                    break;
                }
            }
            foreach (KeyValuePair<string, ExchangeItem> inout in _inout)
            {
                if (inout.Value.Times != null && inout.Value.Times.Length > 0)
                {
                    double timeAsMJD = inout.Value.Times[inout.Value.Times.Length - 1];
                    currentTime = new DotNet.Bridge.Time(timeAsMJD);
                    break;
                }
            }

            if (currentTime == null)
            {
                currentTime = TimeHorizon.BeginTime;
            }
            return currentTime;
        }



        private void GetExchangeItemsFromOpenMIComponent()
        {

            // ZZZ
            // Match the Input and Output Exchange Items
            foreach (IBaseInput baseInput in _openMIComponent.Inputs)
            {
                foreach (IBaseOutput baseOutput in _openMIComponent.Outputs)
                {
                    if (baseInput.Id.CompareTo(baseOutput.Id) == 0)
                    {
                        ITimeSpaceInput timeSpaceInput = baseInput as ITimeSpaceInput;
                        ITimeSpaceOutput timeSpaceOutput = baseOutput as ITimeSpaceOutput;

                        var IO = new ExchangeItem(timeSpaceOutput);
                        IO.AddIn(timeSpaceInput);

                        if (IO != null && !_inout.ContainsKey(timeSpaceOutput.Id))
                        {
                            _inout.Add(timeSpaceOutput.Id, IO);
                        }
                    }
                }
            }
            
            foreach (IBaseInput baseIn in _openMIComponent.Inputs)
            {
                ITimeSpaceInput timeSpaceIn = baseIn as ITimeSpaceInput;

                if (timeSpaceIn != null && !_inout.ContainsKey(baseIn.Id))
                {
                    _inputs.Add(timeSpaceIn.Id, new ExchangeItem(timeSpaceIn));
                }

            }
            foreach (IBaseOutput baseOut in _openMIComponent.Outputs)
            {
                ITimeSpaceOutput timeSpaceOut = baseOut as ITimeSpaceOutput;
                if (timeSpaceOut != null && !_inout.ContainsKey(baseOut.Id))
                {
                    //ZZZ
                    var I = new ExchangeItem(timeSpaceOut);
                    if (I != null)
                        _outputs.Add(timeSpaceOut.Id, I);
                }
            }
            
        }

	}
}