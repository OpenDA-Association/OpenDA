using System;
using System.Collections.Generic;
using System.Linq;
using OpenMI.Standard2;
using OpenMI.Standard2.TimeSpace;

namespace OpenDA.DotNet.OpenMI.Models
{
	public class SimpleOscillatorModelInstance : ITimeSpaceComponent
	{
		private string _id;
		private int _instanceCounter;

		//
		// parameters of the model
		//   that can be used as control variables with their statistics
		//
		protected Dictionary<string, double> parameterValues = new Dictionary<string, double>();

		//
		// parameters in terms of exchange items
		//
		protected Dictionary<string, IBaseInput> inputs = new Dictionary<string, IBaseInput>();
		protected Dictionary<string, IBaseOutput> outputs = new Dictionary<string, IBaseOutput>();

		// Internal state of the model
		protected double[] state;
		protected double t; //current time
		protected int timeStep; //integer value is used for testing equality and counting

		// Parameter used for calibration
		protected List<string> parNames = new List<string>();

		// Counter for keeping track of instances
		protected static int nextinstanceNumber = 1;
		protected int thisInstanceNumber;

		// Output storage
		// TODO make protected again when there is another mechanism to organize the output
		public bool storeObs;
		public List<double> tStore = new List<double>();
		public List<int> iStore = new List<int>();
		public List<double[]> xStore = new List<double[]>();

		// Configuration
		protected string workingDir;
		protected string configstring;

		// ids for parameter/state/exchangeItems (public, for testing purposes)
		public const string StartTimeId = "t_start";
		public const string TimeStepId = "t_step";
		public const string StopTimeId = "t_stop";
		public const string OmegaId = "omega";
		public const string TDampId = "t_damp";
		public const string State0Id = "state[0]";
		public const string State1Id = "state[1]";

		public SimpleOscillatorModelInstance(string id, int instanceCounter)
		{
			_id = id;
			_instanceCounter = instanceCounter;
		}

		public string Caption { get; set; }

		public string Description { get; set; }

		public string Id
		{
			get { return _id; }
		}

		public void Initialize()
		{
			//
			//  DEFAULTS
			//

			// parameters of the model (deterministic part)
			parameterValues.Add(TDampId, 8.0); //characteristic time-scale for friction [seconds]
			parameterValues.Add("omega", 0.25 * 2.0 * Math.PI); //oscilation frequency [rad/s]
			// timespan for the simulation
			AddOrSetParameter(StartTimeId, 0.0);
			AddOrSetParameter(StopTimeId, 10.0);
			AddOrSetParameter(TimeStepId, 0.05);

			// Internal state of the model
			state = new[]{ 0.8d, 0.0d};

			//
			//  GENERIC PART
			//

			// Instance counter
			thisInstanceNumber = nextinstanceNumber;
			nextinstanceNumber++;

			// now parse configuration for specific parameter and state values
			// (todo)
			// <?xml version="1.0" encoding="UTF-8"?>
			// <oscillatorConfig>
			//<simulationTimespan>[0.0,0.05,10.0]</simulationTimespan>
			//<parameters names="t_damp,omega">[8.0,1.5708]</parameters>
			//<parameterUncertainty names="t_damp,omega">[1.0,0.1257]</parameterUncertainty>
			//<systemNoise>{[0.0,0.0],[0.3,0.3]}</systemNoise>
			//<initialState>[0.8,0.0]</initialState>
			//<initialStateUncertainty>[0.8,0.8]</initialStateUncertainty>
			//</oscillatorConfig>
			// 

			// start at initial time
			t = parameterValues[StartTimeId];
			timeStep = 0; //counter starts at 0

			foreach (string parameterId in parameterValues.Keys)
			{
				inputs.Add(parameterId,
					new OscillInputExchangeItem(parameterId, parameterValues[parameterId]));
			}
			inputs.Add(State0Id, new OscillInputExchangeItem(State0Id, state[0]));
			inputs.Add(State1Id, new OscillInputExchangeItem(State0Id, state[1]));
		}

		public string[] Validate()
		{
			// no action needed
			return new string[0];
		}

		public void Prepare()
		{
			// no action needed
		}

		public void Update(params IBaseOutput[] requiredOutput)
		{
			IBaseOutput[] actualOutputItems = requiredOutput ?? outputs.Values.ToArray();
			foreach (IBaseOutput outputItem in actualOutputItems)
			{
				// todo
			}
		}

		public void Finish()
		{
			// no action needed
		}

		public IList<IArgument> Arguments
		{
			get { throw new NotImplementedException(); }
		}

		public LinkableComponentStatus Status
		{
			get { throw new NotImplementedException(); }
		}

		public IList<IBaseInput> Inputs
		{
			get { return inputs.Values.ToList(); }
		}

		public IList<IBaseOutput> Outputs
		{
			get { return outputs.Values.ToList(); }
		}

		public List<IAdaptedOutputFactory> AdaptedOutputFactories
		{
			get { throw new NotImplementedException(); }
		}

		public event EventHandler<LinkableComponentStatusChangeEventArgs> StatusChanged;
		public ITimeSet TimeExtent
		{
			get { throw new NotImplementedException(); }
		}

		protected void AddOrSetParameter(string parameterId, double value)
		{
			if (parameterValues.ContainsKey(StartTimeId))
			{
				parameterValues[parameterId] = value;
			}
			else
			{
				parameterValues.Add(parameterId, value);
			}
		}
	}
}
