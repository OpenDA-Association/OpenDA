using OpenDA.DotNet.Models;
using OpenDA.DotNet.SDK;
using NUnit.Framework;

namespace OpenDA.DotNet.OpenDaAppicationTests
{
	[TestFixture]
	public class OpenDaOnOscillatorModelTests
	{
		[Test]
		public void EnkfOnOscillatorModelTest()
		{
			// force loading assembly
			SimpleOscillatorModelFactory dummyFactory = null;

			ApplicationRunnerJ2N applicationRunnerJ2N =
				new ApplicationRunnerJ2N("../../testData/OpenDaOscillBB/oscillatorEnkfOpenDaConfig.oda", false);
			applicationRunnerJ2N.RunApplication();
		}
	}
}
