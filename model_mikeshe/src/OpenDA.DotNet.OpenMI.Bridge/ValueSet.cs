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
using OpenMI.Standard2.TimeSpace;

namespace OpenDA.DotNet.OpenMI.Bridge
{
	public class ValueSet<T> : ITimeSpaceValueSet
	{
		private List<List<T>> _values;

		public ValueSet(ITimeSpaceExchangeItem item, T value)
		{
			if (item.TimeSet != null && item.TimeSet.Times.Count > 1)
			{
				throw new Exception("Can not handle more than one time step");
			}
			List<T> elementValues = new List<T>();
			elementValues.Add(value);
			_values = new List<List<T>>();
			_values.Add(elementValues);
		}

        public ValueSet(ITimeSpaceExchangeItem item, IEnumerable<T> value)
        {
            if (item.TimeSet != null && item.TimeSet.Times.Count > 1)
            {
                throw new Exception("Can not handle more than one time step");
            }
            List<T> elementValues = new List<T>();
            elementValues.AddRange(value);
            _values = new List<List<T>>();
            _values.Add(elementValues);
        }

        public int GetIndexCount(int[] indices)
		{
			if (indices == null || indices.Length == 0)
				return (Values2D.Count);
			if (indices.Length == 1)
				return (Values2D[indices[0]].Count);
			throw new ArgumentException("Indices does not have the correct length, length must be smaller than 2", "indices");
		}

		public object GetValue(int[] indices)
		{
			if (indices.Length != 2)
				throw new ArgumentException("Indices does not have the correct length", "indices");
			return (GetValue(indices[0], indices[1]));
		}

		public void SetValue(int[] indices, object value)
		{
			if (indices.Length != 2)
				throw new ArgumentException("Indices does not have the correct length", "indices");
			SetValue(indices[0], indices[1], value);
		}

		public Type ValueType
		{
			get { return (typeof(T)); }
		}

		public int NumberOfIndices
		{
			get { return (2); }
		}

		public object GetValue(int timeIndex, int elementIndex)
		{
			CheckIndices(timeIndex, elementIndex);
			return Values2D[timeIndex][elementIndex];
		}

		public void SetValue(int timeIndex, int elementIndex, object value)
		{
			if (!(value is T))
				throw new ArgumentException("value is not of the correct type", "value");
			Values2D[timeIndex][elementIndex] = value;
		}

        public IList GetTimeSeriesValuesForElement(int elementIndex)
        {
            throw new NotImplementedException();
            // TODO: loop over time steps, select elementIndex'th value
		}

		public void SetTimeSeriesValuesForElement(int elementIndex, IList values)
		{
            throw new NotImplementedException();
            // TODO: loop over time steps, set elementIndex'th value
        }

		public IList GetElementValuesForTime(int timeIndex)
		{
            IList valuesAsList = new List<T>();
            foreach (var value in _values[timeIndex])
            {
                valuesAsList.Add(value);
            }
            return valuesAsList;
		}

		public void SetElementValuesForTime(int timeIndex, IList values)
		{
            List<T> valuesAsT = new List<T>();
		    foreach (var value in values)
		    {
		        valuesAsT.Add((T) value);
		    }
            _values[timeIndex] = valuesAsT;
		}

		public IList<IList> Values2D
		{
			get { return TypedListToList(_values); }
			set { _values = ListToTypedList(value); }
		}

		private static IList<IList> TypedListToList(IEnumerable<List<T>> typedList2D)
		{
			IList<IList> list2D = new List<IList>();
			foreach (List<T> typedTimeStepValues in typedList2D)
			{
				list2D.Add(typedTimeStepValues);
			}
			return list2D;
		}

		private static List<List<T>> ListToTypedList(IEnumerable<IList> list2D)
		{
			List<List<T>> typedList2D = new List<List<T>>();
			foreach (IList timeStepValues in list2D)
			{
				List<T> typedTimeStepValues = new List<T>();
				foreach (IList value in timeStepValues)
				{
					typedTimeStepValues.Add((T) value);
				}
				typedList2D.Add(typedTimeStepValues);
			}
			return typedList2D;
		}

		private static void CheckIndices(int timeIndex, int elementIndex)
		{
			if (timeIndex != 0)
			{
				throw new Exception("Only one time step supported");
			}
		}

	}
}