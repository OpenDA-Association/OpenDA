using System;
using OpenDA.DotNet.Interfaces;
using org.openda.interfaces;
using IExchangeItem=OpenDA.DotNet.Interfaces.IExchangeItem;

namespace OpenDA.DotNet.SDK
{
	public class ExchangeItemJ2N : IExchangeItem
	{
		private readonly IPrevExchangeItem _javaExchangeItem;

		public ExchangeItemJ2N(IPrevExchangeItem javaExchangeItem)
		{
			_javaExchangeItem = javaExchangeItem;
		}

		public string Id
		{
			get { return _javaExchangeItem.getId();  }
		}

		public string Description
		{
			get { return _javaExchangeItem.getDescription(); }
		}

		public Type ValueType
		{
			get { return UtilsJ2NAndN2J.ValueTypeMapJ2N(_javaExchangeItem.getValueType()); }
		}

		public Role Role
		{
			get { return UtilsJ2NAndN2J.RoleMapJ2N(_javaExchangeItem.getRole()); }
		}

		public object Values
		{
			get { throw new NotImplementedException(); }
			set { throw new NotImplementedException(); }
		}

		public double[] ValuesAsDoubles
		{
			get { return _javaExchangeItem.getValuesAsDoubles(); }
			set { _javaExchangeItem.setValuesAsDoubles(value); }
		}

		public void AxpyOnValues(double alpha, double[] axpyValues)
		{
			_javaExchangeItem.axpyOnValues(alpha, axpyValues);
		}

		public void MultiplyValues(double[] multiplicationFactors)
		{
			_javaExchangeItem.multiplyValues(multiplicationFactors);
		}

		public double[] Times
		{
			get { return _javaExchangeItem.getTimes(); }
			set { _javaExchangeItem.setTimes(value); }
		}
	}
}