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
using System.Linq;
using System.Collections.Generic;
using OpenMI.Standard2;
using OpenMI.Standard2.TimeSpace;

namespace OpenDA.DotNet.OpenMI.UnitTests.SimpleComponent
{
	internal class SimpleLinkableComponent : ITimeSpaceComponent
	{
		private const double Epsilon = 1.0e-9;

		private IList<IArgument> _arguments;

		private readonly string _id;
		private string _caption;
		private string _description;

		private LinkableComponentStatus _status = LinkableComponentStatus.Created;

		private const double StartTimeAsMJD = 55704; // May 23th 2011, 00:00:00
		private const double EndTimeAsMJD = 55705; // May 24th 2011, 00:00:00
		private const double DeltaTasMJD = 1.0d/24.0d;

		private readonly IList<IBaseInput> _inputs = new List<IBaseInput>();
		private readonly IList<IBaseOutput> _outputs = new List<IBaseOutput>();
        private double _currentTimeAsMJD = StartTimeAsMJD;

		private SimpleTimeSet _timeSetForOutputItems;

		private readonly List<IAdaptedOutputFactory> _adaptedOutputFactories = new List<IAdaptedOutputFactory>();
		private readonly ITimeSet _timeExtent;

		public SimpleLinkableComponent(string id)
		{
			_id = id;
			_timeExtent = new SimpleTimeSet(StartTimeAsMJD, EndTimeAsMJD - StartTimeAsMJD);
			_inputs.Add(new SimpleInput("loc.1", "quant.a", this));
			_outputs.Add(new SimpleOutput("loc.2", "quant.b", this));
		}

		public string Caption
		{
			get { return _caption ?? _id; }
			set { _caption = value; }
		}

		public string Description
		{
			get { return _description ?? _id; }
			set { _description = value; }
		}

		public string Id
		{
			get { return _id; }
		}

		public void Initialize()
		{
			Status = LinkableComponentStatus.Initializing;
			// no action needed
			Status = LinkableComponentStatus.Initialized;
		}

		public string[] Validate()
		{
			Status = LinkableComponentStatus.Validating;
			// no checks needed
			Status = LinkableComponentStatus.Valid;
			return new string[0];
		}

		public void Prepare()
		{
			Status = LinkableComponentStatus.Preparing;
			// no action needed
			Status = LinkableComponentStatus.Updated;
		}

		public void Update(params IBaseOutput[] requiredOutputs)
		{
            IBaseOutput[] actualOutputs =
                (requiredOutputs != null && requiredOutputs.Length > 0) ? requiredOutputs :_outputs.ToArray();
			_currentTimeAsMJD += DeltaTasMJD;
			_timeSetForOutputItems = new SimpleTimeSet(_currentTimeAsMJD);
			foreach (SimpleOutput actualOutput in actualOutputs)
			{
				actualOutput.ComputeValueForCurrentTime(_currentTimeAsMJD);
				actualOutput.TimeSet = _timeSetForOutputItems;
			}
			if (_currentTimeAsMJD > EndTimeAsMJD + Epsilon)
			{
				Status = LinkableComponentStatus.Done;
			}
		}

		public void Finish()
		{
			Status = LinkableComponentStatus.Finishing;
			Status = LinkableComponentStatus.Finished;
		}

		public IList<IArgument> Arguments
		{
			get
			{
				if (_arguments == null)
				{
					_arguments = new List<IArgument> {new SimpleArgument("arg1", "<not-set>")};
				}
				return _arguments;
			}
		}

		public LinkableComponentStatus Status
		{
			get { return _status; }
			set { _status = value; }
		}

		public IList<IBaseInput> Inputs
		{
			get { return _inputs; }
		}

		public IList<IBaseOutput> Outputs
		{
			get { return _outputs; }
		}

		public List<IAdaptedOutputFactory> AdaptedOutputFactories
		{
			get { return _adaptedOutputFactories; }
		}

		public event EventHandler<LinkableComponentStatusChangeEventArgs> StatusChanged;

		public ITimeSet TimeExtent
		{
			get { return _timeExtent; }
		}
	}
}