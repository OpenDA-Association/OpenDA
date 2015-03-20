using System.Collections;
using System.Collections.Generic;
using Oatc.OpenMI.Sdk.Backbone;
using OpenMI.Standard2;

namespace OpenDA.DotNet.OpenMI.Models
{
	public class OscillInputExchangeItem : ExchangeItem, IBaseInput
	{
		public OscillInputExchangeItem(string id, double exchangeItemValue) : 
			base(id)
		{
			SetSingleValue(exchangeItemValue);
		}

		public void SetSingleValue(double exchangeItemValue)
		{
			IList spaceValuesList = new List<double>();
			spaceValuesList.Add(exchangeItemValue);
			List<IList> timeValuesList = new List<IList>();
			timeValuesList.Add(spaceValuesList);
			Values = new ValueSet(timeValuesList);
		}

		public IBaseOutput Provider { get; set; }

		public IBaseValueSet Values { get; set; }
	}
}