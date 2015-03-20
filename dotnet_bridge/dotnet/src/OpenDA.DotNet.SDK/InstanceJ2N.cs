using OpenDA.DotNet.Interfaces;

namespace OpenDA.DotNet.SDK
{
	public class InstanceJ2N : IInstance
	{
		private readonly org.openda.interfaces.IInstance _instance;

		public InstanceJ2N(org.openda.interfaces.IInstance instance)
		{
			_instance = instance;
		}

		public IInstance GetParent()
		{
			return new InstanceJ2N(_instance.getParent());
		}
	}
}