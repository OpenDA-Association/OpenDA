using System.Collections.Generic;
using NUnit.Framework;
using OpenDA.DotNet.Bridge;
using OpenDA.DotNet.Interfaces;
using OpenDA.DotNet.SDK;

namespace OpenDA.DotNet.Models.UnitTests
{
	[TestFixture]
	public class ModelFactoryTest
	{
		[Test]
		public void OscillatorModelTest()
		{
			IModelFactory oscillatorModelFactory = new SimpleOscillatorModelFactory();
			oscillatorModelFactory.Initialize(".", new[] { "../../testData/OpenDaOscillBB/OscillatorModel.xml" });

			string[] modelInstanceArguments = new[] {"this argument is not used"};
			IModelInstance modelInstance = oscillatorModelFactory.GetInstance(modelInstanceArguments,
			                                                                  (int) OutputLevel.ModelDefault);

			IList<string> exchangeItemIDs = modelInstance.ExchangeItemIDs;
			Assert.IsTrue(exchangeItemIDs.Count > 0, "At least one EI");
			IExchangeItem thirdExchangeItem = modelInstance.GetExchangeItem(exchangeItemIDs[2]);
			Assert.AreEqual(AbstractModelInstance.StartTimeId, thirdExchangeItem.Id, "3th exchange item is t_start");

			double startTime = modelInstance.TimeHorizon.BeginTime.MJD;
			double endTime = modelInstance.TimeHorizon.EndTime.MJD;
			double inBetween = startTime + (startTime + endTime) * 3/8;

			modelInstance.Compute(new Time(inBetween));

            IExchangeItem stateExchangeItem0 = modelInstance.GetExchangeItem(SimpleOscillatorModelInstance.State0Id);
			IExchangeItem stateExchangeItem1 = modelInstance.GetExchangeItem(SimpleOscillatorModelInstance.State1Id);

			// check state values
			double[] stateInBetween0 = stateExchangeItem0.ValuesAsDoubles;
            double[] stateInBetween1 = stateExchangeItem1.ValuesAsDoubles;

			Assert.AreEqual(1, stateInBetween0.Length, "#values in state");
            Assert.AreEqual(1, stateInBetween1.Length, "#values in state");
			Assert.AreEqual(-0.0189262285d, stateInBetween0[0], 1e-10, "stateInBetween[0]");
			Assert.AreEqual(0.19278554285d, stateInBetween1[0], 1e-10, "stateInBetween[1]");

			modelInstance.Compute(new Time(endTime));

			// check state values
			double[] stateValuesAtEnd0 = stateExchangeItem0.ValuesAsDoubles;
            double[] stateValuesAtEnd1 = stateExchangeItem1.ValuesAsDoubles;
			Assert.AreEqual(0.00509791929d, stateValuesAtEnd0[0], 1e-10, "stateValuesAtEnd[0]");
			Assert.AreEqual(0.00234214853d, stateValuesAtEnd1[0], 1e-10, "stateValuesAtEnd[1]");

			// assert that previous state values were a copy and are therefore unchanged
			Assert.AreEqual(-0.0189262285d, stateInBetween0[0], 1e-10, "stateValues[0]");
			Assert.AreEqual(0.19278554285d, stateInBetween1[0], 1e-10, "stateValues[1]");

			modelInstance.Finish();
		}

		[Test]
		public void OscillatorBBStochModelTest()
		{
			// force loading assembly
			SimpleOscillatorModelFactory dummyFactory = null;

			IStochModelFactory bbStochModelFactory = new BBStochModelFactoryJ2N();
			bbStochModelFactory.Initialize(".", new[] { "../../testData/OpenDaOscillBB/OscillatorStochBBModel.xml" });

			IStochModelInstance stochModelInstance = bbStochModelFactory.GetInstance((int)OutputLevel.ModelDefault);
			stochModelInstance.SetAutomaticNoiseGeneration(true);

			IList<string> exchangeItemIDs = stochModelInstance.ExchangeItemIDs;
			Assert.IsTrue(exchangeItemIDs.Count > 0, "At least one EI");
			IExchangeItem thirdExchangeItem = stochModelInstance.GetExchangeItem(exchangeItemIDs[2]);
			Assert.AreEqual(AbstractModelInstance.StartTimeId, thirdExchangeItem.Id, "3th exchange item is t_start");

			const double deltaT = 2.0d;

			for (int i = 0; i < 20; i++)
			{
				double targetTime = stochModelInstance.TimeHorizon.BeginTime.MJD + (i+1) * deltaT;
				stochModelInstance.Compute(new Time(targetTime));
			};

			IExchangeItem stateExchangeItem0 = stochModelInstance.GetExchangeItem(SimpleOscillatorModelInstance.State0Id);
			IExchangeItem stateExchangeItem1 = stochModelInstance.GetExchangeItem(SimpleOscillatorModelInstance.State1Id);

			// check state values
			double[] stateValuesAtEnd0 = stateExchangeItem0.ValuesAsDoubles;
			double[] stateValuesAtEnd1 = stateExchangeItem1.ValuesAsDoubles;
			Assert.AreEqual(  8.20208450421d, stateValuesAtEnd0[0], 1e-10, "stateValuesAtEnd[0]");
			Assert.AreEqual(-24.81797019751d, stateValuesAtEnd1[0], 1e-10, "stateValuesAtEnd[1]");

			stochModelInstance.Finish();
		}
	}
}