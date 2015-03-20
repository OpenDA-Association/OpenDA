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
using System.Collections;
using System.Collections.Generic;
using OpenDA.DotNet.Interfaces;
using OpenMI.Standard2;
using OpenMI.Standard2.TimeSpace;

namespace OpenDA.DotNet.OpenMI.Bridge
{
    public class ExchangeItem : IExchangeItem
    {
        private readonly ITimeSpaceInput _openMIInputItem;
        private readonly ITimeSpaceOutput _openMIOutputItem;
        private readonly ITimeSpaceExchangeItem _openMIItem;
        private readonly bool _useBufferedValues;
        private readonly List<List<double>> _bufferedValues;
        private readonly List<double> _bufferedTimes;

        private static double _lastOutputTime; // HACK, REMOVE

        public ExchangeItem(IBaseExchangeItem openMIItem) : this(openMIItem, false)
        {
        }

        public ExchangeItem(IBaseExchangeItem openMIItem, bool useBufferedValues)
        {
            if (openMIItem is ITimeSpaceOutput)
            {
                _openMIItem = _openMIOutputItem = (ITimeSpaceOutput) openMIItem;
            }
            else if (openMIItem is ITimeSpaceInput)
            {
                _openMIItem = _openMIInputItem = (ITimeSpaceInput) openMIItem;
            }
            else
            {
                throw new Exception("Unknown exchange item type: " + openMIItem.GetType());
            }
            if (_openMIItem.TimeSet != null && _openMIItem.TimeSet.HasDurations)
            {
                throw new Exception("\"" + Id + ": OpenDA Time Spans not covered by OpenDA Exchange items");
            }
            _useBufferedValues = useBufferedValues;
            if (_useBufferedValues)
            {
                _bufferedValues = new List<List<double>> {new List<double>()};
                _bufferedTimes = new List<double>();
            }
        }

        public string Id
        {
            get { return _openMIItem.Id; }
        }

        public string Description
        {
            get { return _openMIItem.Description; }
        }

        public Type ValueType
        {
            get { return typeof (IList<double>); }
        }

        public Role Role
        {
            get
            {
                if (_openMIInputItem != null)
                {
                    return Role.InOut; // Input Exchange Items can be asked for their current values
                }
                return Role.Output;
            }
        }

        public object Values
        {
            get
            {
                if (_useBufferedValues)
                {
                    return _bufferedValues;
                }
                ITimeSpaceValueSet openMIValueSet = _openMIOutputItem != null
                                                        ? _openMIOutputItem.Values
                                                        : _openMIInputItem.Values;
                // TODO: remove handling only one time step
                // TODO: replace loop by openMIValueSet.GetValue(new[] { 0 })
                List<List<double>> values = new List<List<double>> { new List<double>() };
                int index0CountValues0 = openMIValueSet.GetIndexCount(new[] { 0 });
                for (int i = 0; i < index0CountValues0; i++)
                {
                    values[0].Add((double) openMIValueSet.GetValue(new[] { 0, i }));
                }
                return values;
            }
            set
            {
                if (_openMIInputItem == null)
                {
                    throw new Exception("\"" + Id + "SetValues: Values can not be set for output item");
                }
                if (!(value is double[]))
                {
                    throw new Exception("\"" + Id + "SetValues: Unexpected object type for setting Values: " +
                                        value.GetType());
                }
                double[] doubles = (double[]) value;
                if (doubles.Length != 1)
                {
                    throw new Exception("\"" + Id + "SetValues: Unexpected size for setting Values: " + value.GetType());
                }
                _openMIInputItem.Values = new ValueSet<double>(_openMIItem, doubles[0]);
            }
        }

        public double[] ValuesAsDoubles
        {
            get { return ((List<List<double>>) Values)[0].ToArray(); }
            set
            {
                if (_openMIInputItem == null)
                {
                    throw new Exception("\"" + Id + "SetValues: Values can not be set for output item");
                }
                _openMIInputItem.Values = new ValueSet<double>(_openMIItem, value);
            }
        }

        public void AxpyOnValues(double alpha, double[] axpyValues)
        {
            if (_openMIInputItem == null)
            {
                throw new Exception("\"" + Id + "SetValues: Values can not be set for output item");
            }
            IList timeSeriesValuesForElement = _openMIInputItem.Values.GetElementValuesForTime(0);
            for (int index = 0; index < axpyValues.Length; index++)
            {
                timeSeriesValuesForElement[index] = (double) timeSeriesValuesForElement[index] + alpha * axpyValues[index];
            }
            _openMIInputItem.Values.SetElementValuesForTime(0, timeSeriesValuesForElement);
        }

        public void MultiplyValues(double[] multiplicationFactors)
        {
            throw new NotImplementedException();
        }

        public double[] Times
        {
            get
            {
                if (_useBufferedValues)
                {
                    return _bufferedTimes.ToArray();
                }
                IList<global::OpenMI.Standard2.TimeSpace.ITime> openMITimes = _openMIItem.TimeSet.Times;
                double[] openDATimes = new double[openMITimes.Count];
                for (int i = 0; i < openMITimes.Count; i++)
                {
                    openDATimes[i] = openMITimes[i].StampAsModifiedJulianDay;
                }
                return openDATimes;
            }
            set { throw new Exception("\"" + Id + "Set times not allowed for OpenMI exchange items"); }
        }

        public void AddNewOpenMIOutputValuesToBuffer()
        {
            ITimeSpaceValueSet timeSpaceValueSet = _openMIOutputItem.Values;
            if (_openMIItem.TimeSet != null && _openMIItem.TimeSet.Times.Count > 0)
            {
                double currentOutputTimeAsMJD = _openMIItem.TimeSet.Times[0].StampAsModifiedJulianDay;
                if (!_bufferedTimes.Contains(currentOutputTimeAsMJD))
                {
                    _bufferedValues[0].Add((double)timeSpaceValueSet.Values2D[0][0]);
                    _bufferedTimes.Add(currentOutputTimeAsMJD);
                    _lastOutputTime = currentOutputTimeAsMJD; // HACK, REMOVE
                }
            }
            else
            {
                _bufferedValues[0].Add((double)timeSpaceValueSet.Values2D[0][0]);
                _bufferedTimes.Add(_lastOutputTime); // HACK, REMOVE
            }
        }
    }
}