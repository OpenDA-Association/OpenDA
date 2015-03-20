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


using System.Collections.Generic;
using java.io;
using NUnit.Framework;
using OpenDA.DotNet.AdditionalInterfaces;
using OpenDA.DotNet.Interfaces;
using OpenDA.DotNet.OpenMI.Bridge;
using OpenDA.DotNet.OpenMI.UnitTests.SimpleComponent;
using OpenMI.Standard2;
using OpenMI.Standard2.TimeSpace;
using org.openda.dotnet;
using Console=System.Console;

namespace OpenDA.DotNet.OpenMI.UnitTests
{
	[TestFixture]
	public class ModelFactoryTest
	{
        [Test]
        public void TestDummyModelFactoryGetInstances()
        {
            IOpenDaModelProvider linkableComponentFactory = new SimpleLinkableComponentFactory();
            linkableComponentFactory.Initialize(".", "factory-arg1-value");

            ITimeSpaceComponent modelInstance = linkableComponentFactory.CreateInstance();

            modelInstance.Arguments[0].Value = "instance-arg1-value";
            modelInstance.Initialize();
            modelInstance.Validate();
            modelInstance.Prepare();

            IList<IBaseInput> inputs = modelInstance.Inputs;
            Assert.IsTrue(inputs.Count > 0, "At least one input");
            IBaseInput input = inputs[0];
            Assert.IsTrue(input != null, "Inputs[0] is IBaseInput");

            IList<IBaseOutput> outputs = modelInstance.Outputs;
            Assert.IsTrue(outputs.Count > 0, "At least one output");
            ITimeSpaceOutput output = outputs[0] as ITimeSpaceOutput;
            Assert.IsTrue(output != null, "Outputs[0] is ITimeSpaceOutput");

            while (modelInstance.Status == LinkableComponentStatus.Updated)
            {
                modelInstance.Update();
            }
            Assert.AreEqual(LinkableComponentStatus.Done, modelInstance.Status, "Status == Done");

            modelInstance.Finish();
        }

		[Test]
		public void TestOpenMIModelFactoryGetInstances()
		{
			IOpenDaModelProvider simpleStochModelFactory = new Models.SimpleOscillatorModelFactory();
			simpleStochModelFactory.Initialize(@"..\..\testData\dummyModel\Model", "dummy-omifile.omi");
			ModelFactory.InsertModelFactory(simpleStochModelFactory);
			ModelFactory modelFactoryOpenMI = new ModelFactory();
			modelFactoryOpenMI.Initialize(null, new[] { "OpenDA.DotNet.OpenMI.Models.SimpleOscillatorModelFactory" });
			IModelInstance modelInstance = modelFactoryOpenMI.GetInstance(null, (int) OutputLevel.ModelDefault);
			modelInstance.Finish();
		}
		[Test]
        public void TestOpenDaDotNetModelFactoryGetInstances()
        {
			IOpenDaModelProvider simpleStochModelFactory = new Models.SimpleOscillatorModelFactory();
			simpleStochModelFactory.Initialize(@"..\..\testData\dummyModel\Model", "dummy-omifile.omi");
			ModelFactory.InsertModelFactory(simpleStochModelFactory);
			ModelFactory modelFactoryOpenMI = new ModelFactory();
			DotNet.Bridge.ModelFactory.InsertModelFactory(modelFactoryOpenMI);
			DotNet.Bridge.ModelFactory modelFactoryOpenDaDotNet = new DotNet.Bridge.ModelFactory();
			modelFactoryOpenDaDotNet.Initialize(null, new[] { "OpenDA.DotNet.OpenMI.Bridge.ModelFactory;OpenDA.DotNet.OpenMI.Models.SimpleOscillatorModelFactory" });
			IModelInstance modelInstance = modelFactoryOpenDaDotNet.GetInstance(null, (int) OutputLevel.ModelDefault);
			modelInstance.Finish();
        }
	
		[Test]
		public void TestN2JModelFactoryGetInstances()
		{
			try
			{
				ModelFactoryN2J modelFactoryN2J = new ModelFactoryN2J();
				modelFactoryN2J.initialize(new File(@"..\..\testData\dummyModel\Model"),
				                           new[] { "OpenDA.DotNet.Bridge.ModelFactory;OpenDA.DotNet.OpenMI.Bridge.ModelFactory;OpenDA.DotNet.OpenMI.Models.SimpleOscillatorModelFactory" });
			}
			catch (System.Exception e)
			{
				Assert.IsTrue(e.Message.Contains("is not an IConfigurable"), "TEST NOT OK YET");
			}
		}
	}
}