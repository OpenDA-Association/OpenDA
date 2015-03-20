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


using System;
using System.Collections.Generic;
using OpenDA.DotNet.Interfaces;
using OpenMI.Standard2;
using OpenMI.Standard2.TimeSpace;
using ITime=OpenDA.DotNet.Interfaces.ITime;

namespace OpenDA.DotNet.OpenMI.Bridge
{
	public class ModelInstance : IModelInstance
	{
		private readonly ModelFactory _openDaModelFactory;
		private readonly ITimeSpaceComponent _openMIComponent;
        private readonly IDictionary<String, ExchangeItem> _inputs = new Dictionary<string, ExchangeItem>();
        private readonly IDictionary<String, ExchangeItem> _outputs = new Dictionary<string, ExchangeItem>();

		public ModelInstance(ModelFactory myModelFactory, ITimeSpaceComponent openMIComponent)
		{
			_openDaModelFactory = myModelFactory;
			_openMIComponent = openMIComponent;
            GetExchangeItemsFromOpenMIComponent();
        }

		public IInstance GetParent()
		{
			return _openDaModelFactory;
		}

	    public ITimeSpaceComponent OpenMIComponent
	    {
	        get { return _openMIComponent; }
	    }

		public ITime TimeHorizon
		{
			get
			{
			    global::OpenMI.Standard2.TimeSpace.ITime timeHorizon = _openMIComponent.TimeExtent.TimeHorizon;
			    double beginTime = timeHorizon.StampAsModifiedJulianDay;
                double endTime = beginTime + timeHorizon.DurationInDays;
                DotNet.Bridge.Time openDATimeHorizon = new DotNet.Bridge.Time(beginTime, endTime);
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
            if (lastTime == null)
		    {
				throw new Exception("Could not find time/space output item to check model's current time");
			}

			while (lastTime.EndTime.MJD < targetTime.MJD &&
                _openMIComponent.Status != LinkableComponentStatus.Failed &&
                _openMIComponent.Status != LinkableComponentStatus.Done)
			{
			    try
			    {
			        _openMIComponent.Update();
			    }
			    catch (Exception e)
			    {
                    ModelFactory.AppendLogMessage("Exception in Update()" + e);
                    throw e;
			    }
			    foreach (KeyValuePair<string, ExchangeItem> output in _outputs)
			    {
                    output.Value.AddNewOpenMIOutputValuesToBuffer();
                }
                lastTime = DetermineCurrentTime(null);
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
            if (_inputs.ContainsKey(exchangeItemID))
            {
                return _inputs[exchangeItemID];
            }
		    if (!_outputs.ContainsKey(exchangeItemID))
		    {
		        string availableItems = "";
		        foreach (var exchangeItem in _inputs)
		        {
		            availableItems += "In: " + exchangeItem.Key + "\n";
		        }
                foreach (var exchangeItem in _outputs)
                {
                    availableItems += "Out: " + exchangeItem.Key + "\n";
                }
                throw new Exception("Could not find exchange item \"" + exchangeItemID +
                    "\". Available exchange items:\n" + availableItems);
            }
		    return _outputs[exchangeItemID];
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


        public IVector[] GetObservedLocalization(IObservationDescriptions observationDescriptions, double distance)
        {
            return null;
        }

        public IVector GetObservedValues(IObservationDescriptions observationDescriptions)
        {
            return null;
        }

        public IVector[] GetObservedLocalization(String exchageItemID, IObservationDescriptions observationDescriptions, double distance)
        {
            return null;
        }


		public string ModelRunDirPath
		{
            get { return null; }  // model instance's run dir is not known
		}

		public void Finish()
		{
            if (_openMIComponent != null)
            {
                _openMIComponent.Finish();
            }
		}

        public override string ToString()
        {
            if (_openMIComponent != null)
            {
                return _openMIComponent.Caption;
            }
            return GetType().Name;
        }

        private ITime DetermineCurrentTime(ITime currentTime)
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
            if (currentTime == null)
            {
                currentTime = TimeHorizon.BeginTime;
            }
            return currentTime;
        }

        private void GetExchangeItemsFromOpenMIComponent()
        {
            string message = "";
            foreach (IBaseInput baseInput in _openMIComponent.Inputs)
            {
                ITimeSpaceInput timeSpaceInput = baseInput as ITimeSpaceInput;
                if (timeSpaceInput != null)
                {
                    if (_inputs.ContainsKey(timeSpaceInput.Id))
                    {
                        string mess = "Input item " + timeSpaceInput.Id + " defined more then once";
                        ModelFactory.AppendLogMessage(mess);
                        message += "\n" + mess;
                    }
                    else
                    {
                        _inputs.Add(timeSpaceInput.Id, new ExchangeItem(timeSpaceInput));
                    }
                }
            }
            foreach (IBaseOutput baseOutput in _openMIComponent.Outputs)
            {
                ITimeSpaceOutput timeSpaceOutput = baseOutput as ITimeSpaceOutput;
                if (timeSpaceOutput != null)
                {
                    if (_outputs.ContainsKey(timeSpaceOutput.Id))
                    {
                        string mess = "Output item " + timeSpaceOutput.Id + " defined more then once";
                        ModelFactory.AppendLogMessage(mess);
                        message += "\n" + mess;
                    }
                    else
                    {
                        _outputs.Add(timeSpaceOutput.Id, new ExchangeItem(timeSpaceOutput, true));
                    }
                }
            }
            if (message.Length > 0)
               throw new Exception("Multiple Input and/or Output items:" + message);
        }
	}
}