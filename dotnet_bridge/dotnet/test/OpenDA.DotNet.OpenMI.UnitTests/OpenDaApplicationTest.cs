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


using System;
using java.io;
using NUnit.Framework;
using org.openda.application;
using ModelFactory=OpenDA.DotNet.OpenMI.Bridge.ModelFactory;

namespace OpenDA.DotNet.OpenMI.UnitTests
{
    [TestFixture]
    public class OpenDaApplicationTest
    {
        [Test]
        public void RunOscillApplication1()
        {
            OpenDaApplication.main(new[] { @"..\..\testData\oscill1\oscillEnsrMultiRes_1.oda" });
        }

        [Test]
        public void RunOscillApplication2()
        {
            OpenDaApplication.main(new[] { @"..\..\testData\oscill1\oscillatorSimulationOpenDaConfig.oda" });
        }

        [Test]
        public void RunDummyModelSimulation()
        {
        	try
        	{
        		DotNet.Bridge.ModelFactory.InsertModelFactory(new ModelFactory());
        		ApplicationRunnerSingleThreaded applicationRunner = new ApplicationRunnerSingleThreaded();
                applicationRunner.runSingleThreaded(
                    new File("."), @"..\..\testData\dummyModel\DummyModelSimulation.oda");
        	}
        	catch (Exception e)
        	{
        		// TEST NOT COMPLETE YET
        	}
        }
    }
}
