using java.io;
using OpenDA.DotNet.Interfaces;

namespace OpenDA.DotNet.SDK
{
	public class BBStochModelFactoryJ2N : IStochModelFactory
	{
		private org.openda.blackbox.wrapper.BBStochModelFactory _javaBBStochModelFactory =
			new org.openda.blackbox.wrapper.BBStochModelFactory();

		public void Initialize(string workingDirPath, string[] arguments)
		{
			_javaBBStochModelFactory.initialize(new File(workingDirPath), arguments);
		}

		public IStochModelInstance GetInstance(int outputLevel)
		{
			org.openda.interfaces.IStochModelInstance javaStochModel = _javaBBStochModelFactory.getInstance(UtilsJ2NAndN2J.OutputLevelMapN2J(outputLevel));
			return new BBStochModelInstanceJ2N(javaStochModel);
		}
	}
}
