using System;
using System.IO;
using OpenDA.DotNet.AdditionalInterfaces;
using OpenMI.Standard2.TimeSpace;

namespace OpenDA.DotNet.OpenMI.Models
{
	public class SimpleOscillatorModelFactory : IOpenDaModelProvider
	{
		private string omiFilePath;
		private int instanceCounter;
		private string idPrefix = "idToBeReadFromOmiFile";

		public void Initialize(string workingDirPath, params string[] args)
		{
			if (workingDirPath == null)
			{
				throw new Exception("WorkingDir can not be null");
			}
			if (args == null || args.Length != 1)
			{
				throw new Exception("Expecting one argument, the OMI-file");
			}
			
			omiFilePath = Path.Combine(workingDirPath, args[0]);
		}

		public ITimeSpaceComponent CreateInstance()
		{
			if (omiFilePath == null)
			{
				throw new Exception("Factory not yet initialized");
			}
			// todo: read arguments from OMI file
			instanceCounter++;
			return new SimpleOscillatorModelInstance(idPrefix + instanceCounter, instanceCounter);
		}

		public void SaveInstance(ITimeSpaceComponent timeSpaceComponent)
		{
			throw new NotImplementedException();
		}
	}
}
