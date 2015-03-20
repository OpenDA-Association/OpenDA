using System;
using System.IO;
using OpenDA.DotNet.Bridge;
using OpenDA.DotNet.Interfaces;
using org.openda.algorithms;
using org.openda.algorithms.kalmanFilter;
using org.openda.application;
using org.openda.blackbox.wrapper;
using org.openda.dotnet;
using org.openda.interfaces;
using Console = System.Console;
using IModelInstance = OpenDA.DotNet.Interfaces.IModelInstance;
using IStochModelInstance=org.openda.interfaces.IStochModelInstance;

namespace OpenDA.DotNet.SDK
{
	public class ApplicationRunnerJ2N
	{
		private readonly string odaDirectoryName;
		private readonly string odaFileName;

		public string ModelConfigFilePath { get; private set; }
		public IModelInstance ResultingModelInstance { get; private set; }

		public ApplicationRunnerJ2N(string odaFilePath, bool checkModelConfigFile)
		{
			odaDirectoryName = Path.GetDirectoryName(odaFilePath);
			odaFileName = Path.GetFileName(odaFilePath);
			if (odaDirectoryName == null)
			{
				Console.WriteLine("Could not determine directory name where oda-file is");
				return;
			}

			string logFileName = Path.GetFileNameWithoutExtension(odaFileName) + "-DSlog.txt";
			logFileName = Path.Combine(odaDirectoryName, logFileName);
			File.Delete(logFileName);

			ModelFactory.InsertLogFileName(logFileName);

			if (checkModelConfigFile)
			{
				// check if there is a stochModel/modelConfigFile
				const string stochModelDirName = "stochModel";
				const string modelconfigXml = "modelConfig.xml";
				string modelConfigDir = Path.Combine(odaDirectoryName, stochModelDirName);
				ModelConfigFilePath = Path.Combine(modelConfigDir, modelconfigXml);
				if (!File.Exists(ModelConfigFilePath))
				{
					string message = "File " + ModelConfigFilePath + " expected";
					ModelFactory.AppendLogMessage(message);
					Console.WriteLine(message);
				}
			}
		}

		public void RunApplication()
		{
			RunApplication(null);	
		}

		public void RunApplication(IModelFactory dotNetModelFactory)
		{
			if (dotNetModelFactory != null)
			{
				// Tell OpenDA what model factory to use
				ModelFactory.InsertModelFactory(dotNetModelFactory);
			}
			// run the openda application
			ApplicationRunnerSingleThreaded applicationRunner;
			try
			{
				applicationRunner = new ApplicationRunnerSingleThreaded();
                applicationRunner.runSingleThreaded(new java.io.File(odaDirectoryName), odaFileName);
			}
			catch (Exception e)
			{
				String message = "Error running OpenDA application.\nDetailed error message: " + e.Message;
				ModelFactory.AppendLogMessage(message);
				Console.WriteLine(message);
				return;
			}

			if (true) // TODO: applicationRunner.getStatus()== ApplicationRunnerJ2N.Status.FINISHED // DONE)
			{

				// get a reference to the model instance selected by the algorithm as optimal

				IAlgorithm algorithm = applicationRunner.getAlgorithm();
				IStochModelInstance calibratedStochModel;
				if (algorithm is Dud)
				{
					Dud dud = ((Dud)algorithm);
					calibratedStochModel = dud.getBestEstimate();
				}
				else if (algorithm is SparseDud)
				{
					SparseDud sparseDud = ((SparseDud)algorithm);
					calibratedStochModel = sparseDud.getBestEstimate();
				}
				else if (algorithm is Powell)
				{
					Powell powell = ((Powell)algorithm);
					calibratedStochModel = powell.getBestEstimate();
				}
				else if (algorithm is Simplex)
				{
					Simplex simplex = ((Simplex)algorithm);
					calibratedStochModel = simplex.getBestEstimate();
				}
				else if (algorithm is AbstractSequentialAlgorithm)
				{
					AbstractSequentialAlgorithm asAlgorithm = ((AbstractSequentialAlgorithm)algorithm);
					calibratedStochModel = asAlgorithm.getMainModel();
					if (calibratedStochModel == null)
					{
						Console.WriteLine("No main model set by ensemble algorithm");
						return;
					}
				}
				else
				{
					Console.WriteLine("Unknown Algoritm type: " + algorithm.GetType());
					return;
				}

				// Get the model instance out of the stochModel->java2dotnet->modelInstance layers
				if (calibratedStochModel is BBStochModelInstance)
				{
					org.openda.interfaces.IModelInstance modelInstance = ((BBStochModelInstance)calibratedStochModel).getModel();
					if (modelInstance is ModelInstanceN2J)
					{
						ResultingModelInstance = ((ModelInstanceN2J)modelInstance).getDotNetModelInstance();
					}
					else
					{
						string message = "Unknown java 2 dotnet model instance type: " + modelInstance.GetType();
						ModelFactory.AppendLogMessage(message);
						Console.WriteLine(message);
					}
				}
				else
				{
					string message = "Unknown Stoch model instance type: " + calibratedStochModel.GetType();
					ModelFactory.AppendLogMessage(message);
					Console.WriteLine(message);
				}
			}
		}
	}
}