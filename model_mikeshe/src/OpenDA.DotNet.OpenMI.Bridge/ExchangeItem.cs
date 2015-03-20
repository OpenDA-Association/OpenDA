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
using System.Collections;
using OpenDA.DotNet.Interfaces;
using OpenMI.Standard2;
using OpenMI.Standard2.TimeSpace;
//using DHI.OpenMI2.Sdk.Backbone.Generic;

using Oatc.OpenMI.Sdk.Backbone;
using Oatc.OpenMI.Sdk.Backbone.Generic;
//using Oatc.OpenMI.Sdk.Buffer;

namespace OpenDA.DotNet.OpenMI.Bridge
{
    public class ExchangeItem : IExchangeItem
    {
        private ITimeSpaceInput _openMIInputItem;
        private ITimeSpaceOutput _openMIOutputItem;
        //ZZZ
        private List<ITimeSpaceExchangeItem> _openMIInputOutput = new List<ITimeSpaceExchangeItem>();
        //private List<double> _noise = new List<double>();

        //private List<int> _exchangeINDX = new List<int>(); 
        private IDictionary<String, int> _exchangeINDX = new Dictionary<string, int>();
        private List<int> _exchangeINDX2 = new List<int>();

        private int _tstepCount;
        double[] _Values;

        private ITimeSpaceExchangeItem _openMIItem;
        private bool _useBufferedValues;
        private List<List<double>> _bufferedValues;
        private List<double> _bufferedTimes;

        private static double _lastOutputTime; // HACK, REMOVE
        private double _timeStampBackup;  //In case of missing time just a best try

        public ExchangeItem(IBaseExchangeItem openMIItem)
            : this(openMIItem, false)
        {
        }

        public ExchangeItem(IBaseExchangeItem openMIItem, bool useBufferedValues)
        {
            _tstepCount = 0;
            if (openMIItem is ITimeSpaceOutput)
            {
                _openMIItem = _openMIOutputItem = (ITimeSpaceOutput)openMIItem;
            }
            else if (openMIItem is ITimeSpaceInput)
            {
                _openMIItem = _openMIInputItem = (ITimeSpaceInput)openMIItem;
            }
            else
            {
                throw new Exception("Unknown exchange item type: " + openMIItem.GetType());
            }
            if (_openMIItem.TimeSet != null && _openMIItem.TimeSet.HasDurations)
            {
                //ZZZ throw new Exception("\"" + Id + ": OpenDA Time Spans not covered by OpenDA Exchange items");
            }
            _useBufferedValues = useBufferedValues;
            if (_useBufferedValues)
            {
                _bufferedValues = new List<List<double>> { new List<double>() };
                _bufferedTimes = new List<double>();
            }
        }

        public void AddOut(IBaseExchangeItem openMIo)
        {
            if (openMIo is ITimeSpaceOutput)
            {
                _openMIItem = _openMIOutputItem = (ITimeSpaceOutput)openMIo;
            }
        }

        public void AddSingleIdx(int idx)
        {
            if (!_exchangeINDX.ContainsKey(idx.ToString()))
                _exchangeINDX.Add(idx.ToString(), idx);

            if (!_exchangeINDX2.Contains(idx))
                _exchangeINDX2.Add(idx);

        }



        public void UpdatetstepCount()
        {
            _tstepCount++;
            _Values = null;

        }

        public void AddIn(IBaseExchangeItem openMIo)
        {
            if (openMIo is ITimeSpaceInput)
            {
                _openMIItem = _openMIInputItem = (ITimeSpaceInput)openMIo;
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
            get { return typeof(IList<double>); }
        }

        public Role Role
        {
            get
            {
                if (_openMIInputItem != null)
                {
                    //ZZZ
                    //return Role.InOut; // Input Exchange Items can be asked for their current values
                    return Role.Input; // Input Exchange Items can be asked for their current values
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
                    if (_bufferedValues[_bufferedValues.Count - 1].Count == 0)
                    {
                        return _bufferedValues;
                    }
                    else
                    {
                        //Gets the final list of state values
                        // List<double> temp = new List<Double>(_bufferedValues[_bufferedValues.Count-1]);
                        List<List<double>> ttemp = new List<List<double>>();
                        ttemp.Add(_bufferedValues[_bufferedValues.Count - 1]);
                        return ttemp;
                    }
                }
                //ZZZ Note to self
                // condition ? first_expression : second_expression;
                // If condition is true, first expression is evaluated and becomes the result; if false,
                // the second expression is evaluated and becomes the result. Only one of two expressions is ever evaluated.

                ITimeSpaceValueSet openMIValueSet = _openMIOutputItem != null
                                                        ? _openMIOutputItem.Values
                                                        : _openMIInputItem.Values;

                // TODO: remove handling only one time step
                // TODO: replace loop by openMIValueSet.GetValue(new[] { 0 })
                List<List<double>> values = new List<List<double>> { new List<double>() };
                int index0CountValues0 = openMIValueSet.GetIndexCount(new[] { 0 });
                for (int i = 0; i < index0CountValues0; i++)
                {
                    values[0].Add((double)openMIValueSet.GetValue(new[] { 0, i }));
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
                double[] doubles = (double[])value;
                if (doubles.Length != 1)
                {
                    throw new Exception("\"" + Id + "SetValues: Unexpected size for setting Values: " + value.GetType());
                }
                // _openMIInputItem.Values = new ValueSet<double>(_openMIItem, doubles[0]);
                _openMIInputItem.Values = new ValueSetArray<double>((IList<double>)value);
            }
        }



        public double[] ValuesAsDoubles
        {
            get {
                FillLocalValues();
                double[] retVals = new double[_Values.Length];
                for (int i = 0; i < _Values.Length; i++)
                {
                    retVals[i] = _Values[i];
                }
                return retVals;
                //return ((List<List<double>>)Values)[0].ToArray(); 
            }
            set
            {
                FillLocalValues();
                if (_openMIInputItem == null)
                {
                    throw new Exception("\"" + Id + "SetValues: Values can not be set for output item");
                }
                if (_Values.Length != value.Length)
                {
                    throw new Exception("\"" + Id + "SetValues: dimensions of array ("+value.Length+") does not correspond to dimension of echange item ("+_Values.Length+")");               
                }
                for (int i = 0; i < _Values.Length; i++)
                {
                    _Values[i]=value[i];
                }
                _openMIInputItem.Values = new ValueSet<double>(_openMIItem, value);
            }
        }

        // ZZZ TODO
        // AXPY - Adds a scalar multiple of a vector to this vector. (this += alpha * xx)
        // this.content[i] = this.content[i] + alpha * x.getValue(i);
        // This method is called for two different cases
        // 1. To update the exchange item state variable ( for all the time steps )
        // 2. To add noise to the model using only time axpyValues to the last time step
        //
        // Therefore needs to differentiate between the two by the number of axpyValues
        // being passed. This could be a cause of error if only one time step in the model
        // is run before an observation is available
        public void AxpyOnValuesStef(double alpha, double[] axpyValues)
        {
            if (_openMIInputItem == null)
            {
                throw new Exception("\"" + Id + "SetValues: Values can not be set for output item");
            }
            IList timeSeriesValuesForElement = _openMIInputItem.Values.GetElementValuesForTime(0);
            for (int index = 0; index < axpyValues.Length; index++)
            {
                timeSeriesValuesForElement[index] = (double)timeSeriesValuesForElement[index] + alpha * axpyValues[index];
            }
            _openMIInputItem.Values.SetElementValuesForTime(0, timeSeriesValuesForElement);

            /* tijdelijk */
            for (int i = 0; i < _Values.Length; i++)
            {
                _Values[i] += axpyValues[i] * alpha;
            }
        
        
        
        }


        public void AxpyOnValues(double alpha, double[] axpyValues)
        {
            FillLocalValues();
            if (axpyValues.Length > 0)
            {
                for (int i = 0; i < _Values.Length; i++)
                {
                    _Values[i] += axpyValues[i] * alpha;
                }

                /*_openMIInputItem.Values = new ValueSetArray<double>((IList<double>)dvalues); */
                _openMIInputItem.Values = new ValueSetArray<double>(_Values);
            }



            /*
            ITimeSpaceValueSet temp = _openMIOutputItem.Values;
            double[] axpyValuesArray = new double[temp.Values2D[0].Count];
            List<double> dvalues = new List<double>();

            if (axpyValues.Length > 0)
            {
                for (int i = 0; i < temp.Values2D[0].Count; i++)
                {
                    dvalues.Add((double)temp.GetValue(0, i) + axpyValues[i] * alpha);
                }
                _openMIInputItem.Values = new ValueSetArray<double>((IList<double>)dvalues);


            }
            //throw new NotImplementedException();
             */
        }

        public List<double> ApxyLinearly(ITimeSpaceValueSet data, double[] apxValue)
        {
            int size = data.Values2D[0].Count;
            double incrementV = apxValue[0] / size;
            List<double> dvalues = new List<double>();
            for (int i = 0; i < size; i++)
            {
                dvalues.Add((double)data.GetValue(0, i) + incrementV * i);
            }


            return dvalues;
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

                    double[] t = new double[1];
                    if (_bufferedTimes.Count == 0)
                    {
                        //t[0] = _openMIItem.TimeSet.Times[0].StampAsModifiedJulianDay;
                        return _bufferedTimes.ToArray();
                    }
                    else
                    {
                        t[0] = _bufferedTimes[_bufferedTimes.Count - 1];
                        return t;
                    }//return _bufferedTimes.ToArray();

                }
                IList<global::OpenMI.Standard2.TimeSpace.ITime> openMITimes = _openMIItem.TimeSet.Times;
                double[] openDATimes = new double[openMITimes.Count];
                if (openMITimes.Count > 0)
                {
                    for (int i = 0; i < openMITimes.Count; i++)
                    {
                        openDATimes[i] = openMITimes[i].StampAsModifiedJulianDay;
                    }
                    return openDATimes;
                }
                else
                {
                    double[] t = new double[1];
                    t[0] = _timeStampBackup;
                    return t;
                }
            }
            set { throw new Exception("\"" + Id + "Set times not allowed for OpenMI exchange items"); }
        }

        public void AddNewOpenMIOutputValuesToBuffer_marc()
        {
            //ZZZ
            _bufferedValues = new List<List<Double>>();
            //_bufferedTimes = new List<double>();

            string notimplemented = "seepage";
            string notimplemented2 = "groundwater";

            if (!_openMIOutputItem.Caption.Contains(notimplemented) && !_openMIOutputItem.Caption.Contains(notimplemented2))
            {
                ITimeSpaceValueSet timeSpaceValueSet = _openMIOutputItem.Values;
                int index0CountValues0 = timeSpaceValueSet.GetIndexCount(new[] { 0 });

                //_bufferedValues[0].Add((double)timeSpaceValueSet.Values2D[0][2]);

                List<double> templist = new List<double>();

                // Place the indexed values to buffer
                for (int i = 0; i < index0CountValues0; i++)
                {
                    templist.Add((double)timeSpaceValueSet.Values2D[0][i]);
                }

                //templist.Add((double)timeSpaceValueSet.Values2D[0][2]);
                _bufferedValues.Add(templist);

                if (_openMIItem.TimeSet != null && _openMIItem.TimeSet.Times.Count > 0)
                {
                    _bufferedTimes.Add(_openMIItem.TimeSet.Times[0].StampAsModifiedJulianDay);
                    _lastOutputTime = _openMIItem.TimeSet.Times[0].StampAsModifiedJulianDay; // HACK, REMOVE
                }
                else
                {
                    _bufferedTimes.Add(_lastOutputTime); // HACK, REMOVE
                }
            }





        }

        /// <summary>
        /// Buffer values to the axchangeItems in case of buffering and check/complete time administration when it is missing
        /// </summary>
        /// <param name="timeStampModel">Time Stamp of the model. This timestamp is used in case the exchangeItmem itself does not have a timestemp for some reason</param>
        public void AddNewOpenMIOutputValuesToBuffer(double timeStampModel)
        {

            string notimplemented = "seepage";
            string notimplemented2 = "groundwater";
            string debug1 = "head elevation";

            //Some check for handling time when this is not available for an exchangeItem
            ITimeSpaceValueSet timeSpaceValueSet = _openMIOutputItem.Values;
            if (!(_openMIItem.TimeSet != null && _openMIItem.TimeSet.Times.Count > 0))
            {
                // Set the time of this echangeItem
                _timeStampBackup = timeStampModel;
            }

            // NOTE: for now we have disabled buffering
            return;
            /*           if (_openMIOutputItem.Caption.Contains(debug1))
                       {
                           debug1 = debug1;
                       }

                       if (!_openMIOutputItem.Caption.Contains(notimplemented) && !_openMIOutputItem.Caption.Contains(notimplemented2))
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
                       } */
        }



        private void FillLocalValues()
        {
            if (_Values == null)
            {
                ITimeSpaceValueSet temp = _openMIOutputItem.Values;
                int nVals = temp.Values2D[0].Count;
                _Values = new double[nVals];
                for (int i = 0; i < temp.Values2D[0].Count; i++)
                {
                    _Values[i] = (double)temp.GetValue(0, i);
                }
            }
              
         }
    }
            
}


