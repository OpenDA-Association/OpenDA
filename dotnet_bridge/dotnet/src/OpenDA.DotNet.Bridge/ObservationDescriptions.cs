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

namespace OpenDA.DotNet.Bridge
{
	public class ObservationDescriptions : IObservationDescriptions
	{
		private readonly List<IExchangeItem> _exchangeItems;
        private readonly String[] _keys;
        private readonly IDictionary<String, String[]> _values;
        private readonly int _nKeys=0;
        private readonly int _nObs=0;

		public ObservationDescriptions(IExchangeItem[] exchangeItems)
		{
			_exchangeItems = new List<IExchangeItem>(exchangeItems);
            _nObs = _exchangeItems.Count;
		}

        public ObservationDescriptions(IExchangeItem[] exchangeItems, String [] keys, String[][] values) : this(exchangeItems)
        {
            // Deterimine the number of Keys and nObs
            if (keys!=null ){_nKeys = keys.Length;}
            if (values!=null && values[0] !=null){
                if (_nObs > 0 && values[0].Length != _nObs)
                {
                    throw new Exception("Number of exchangeItems (" + _nObs + ") does not correspond to number of values (" + values[0].Length + ")");
                }
                _nObs = values[0].Length;
            }
 
            // Only when we have things to store
            if (_nKeys > 0 && _nObs > 0){
                _keys = keys;
                _values = new Dictionary<String, String[]>();
                for (int iKey=0; iKey<_nKeys; iKey++){
                     _values.Add(keys[iKey],values[iKey]);
                }
            }
        }

		public List<IExchangeItem> ExchangeItems
		{
			get { return _exchangeItems; }
		}

		public IVector GetValueProperties(string key)
		{
            double[] values=new double[_nObs];
            string[] valueAsString=_values[key];
            for (int iObs=0; iObs<_nObs; iObs++){
                values[iObs] = Convert.ToDouble(valueAsString[iObs], System.Globalization.CultureInfo.InvariantCulture);
            }
            return new Vector(values);
		}

		public string[] GetStringProperties(string key)
		{
			return _values[key];
		}

		public string[] PropertyKeys
		{
            get {
                return _keys;
            }
		}

		public int PropertyCount
		{
            get { return _nKeys; }
		}

		public int ObservationCount
		{
            get { return _nObs; }
		}

		public ITime[] Times
		{
			get { throw new NotImplementedException(); }
		}
	}
}