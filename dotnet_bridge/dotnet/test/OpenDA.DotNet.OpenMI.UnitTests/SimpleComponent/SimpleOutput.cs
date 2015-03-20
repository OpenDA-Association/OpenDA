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
using System.Diagnostics;
using OpenMI.Standard2;
using OpenMI.Standard2.TimeSpace;

namespace OpenDA.DotNet.OpenMI.UnitTests.SimpleComponent
{
	internal class SimpleOutput : SimpleExchangeItem, ITimeSpaceOutput
	{
		private static char _simpleOutputCounter;  // keep track of #outputs

		private readonly IList<IBaseInput> _consumers = new List<IBaseInput>();
		private readonly IList<IBaseAdaptedOutput> _adaptedOutputs = new List<IBaseAdaptedOutput>();
		private readonly double _simpleOutputIndex;
		private readonly ISpatialDefinition _spatialDefinition;

		public SimpleOutput(string elementSetName, string quantityName, SimpleLinkableComponent linkableComponent) :
			base(elementSetName, quantityName, linkableComponent)
		{
			_simpleOutputIndex = ++_simpleOutputCounter;
			_spatialDefinition = new SimpleElementSet(elementSetName);
			TimeSet = new SimpleTimeSet(linkableComponent.TimeExtent.TimeHorizon.StampAsModifiedJulianDay);
		}

		public void AddConsumer(IBaseInput consumer)
		{
			if (_consumers.Contains(consumer))
			{
				throw new Exception("Consumer has already been registered");
			}
			_consumers.Add(consumer);
		}

		public void RemoveConsumer(IBaseInput consumer)
		{
			if (!_consumers.Contains(consumer))
			{
				throw new Exception("Consumer was never registered");
			}
		}

		public void AddAdaptedOutput(IBaseAdaptedOutput adaptedOutput)
		{
			if (_adaptedOutputs.Contains(adaptedOutput))
			{
				throw new Exception("Consumer has already been registered");
			}
			_adaptedOutputs.Add(adaptedOutput);
		}

		public void RemoveAdaptedOutput(IBaseAdaptedOutput adaptedOutput)
		{
			if (!_adaptedOutputs.Contains(adaptedOutput))
			{
				throw new Exception("Consumer was never registered");
			}
		}

		ITimeSpaceValueSet ITimeSpaceOutput.GetValues(IBaseExchangeItem querySpecifier)
		{
			throw new NotImplementedException();
		}

		ITimeSpaceValueSet ITimeSpaceOutput.Values
		{
			get { throw new NotImplementedException(); }
		}

		public IBaseValueSet GetValues(IBaseExchangeItem querySpecifier)
		{
			throw new NotImplementedException();
		}

		public IList<IBaseInput> Consumers
		{
			get { return _consumers; }
		}

		public IList<IBaseAdaptedOutput> AdaptedOutputs
		{
			get { return _adaptedOutputs; }
		}

		public IBaseValueSet Values
		{
			get { return _values; }
		}

		public void ComputeValueForCurrentTime(double currentTime)
		{
			_values.SetValue(new[] { 0, 0 }, currentTime / 100000.0d + 10.0d * _simpleOutputIndex);
			Debug.WriteLine("Computed values for " + Id + ", time:" + currentTime);
		}

		public ITimeSet TimeSet { get; set; }

		public ISpatialDefinition SpatialDefinition
		{
			get { return _spatialDefinition; }
		}
	}
}