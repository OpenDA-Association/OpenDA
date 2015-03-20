using NUnit.Framework;
using OpenDA.DotNet.Interfaces;
using OpenDA.DotNet.OpenMI.Bridge;
using org.openda.application;
using java.io;

namespace MikeSheInOpenDA.UnitTests
{
    [TestFixture]
    public class RunMikeSheFromOpenDaTests
    {

        [Test]
        public void RunMikeSheDataAssimilation()
        {
            string modelConfigDir = @"c:\devel_nils\Test_5x5\Distributed\base\";
            string modelconfigOmi = "MIKESHE_WM_Test_5x5.omi";
            
            string odaConfigDir = @"C:\devel_nils\Test_5x5\";
            string odaFileName = "EnKF_test1.oda";
            
            MikeSheOpenMIModelFactory mikeSheOpenMIModelFactory = new MikeSheOpenMIModelFactory();
            mikeSheOpenMIModelFactory.Initialize(modelConfigDir, new[] { modelconfigOmi });

            OpenDA.DotNet.OpenMI.Bridge.ModelFactory.InsertModelFactory(mikeSheOpenMIModelFactory);

            ModelFactory openDaModelFactory = new ModelFactory();
            openDaModelFactory.Initialize(null, null);

            //IModelInstance modelInstance = openDaModelFactory.GetInstance(new string[] { }, outputLevel: 0);

            ApplicationRunnerSingleThreaded applicationRunner = new ApplicationRunnerSingleThreaded();


            applicationRunner.runSingleThreaded(new File(odaConfigDir), odaFileName);
        }

        [Test]
        public void RunMikeSheSimulation()
        {
            string modelConfigDir = @"c:\devel_nils\Test_5x5\Distributed\base\";
            string modelconfigOmi = "MIKESHE_WM_Test_5x5.omi";
            MikeSheOpenMIModelFactory mikeSheOpenMIModelFactory = new MikeSheOpenMIModelFactory();
            mikeSheOpenMIModelFactory.Initialize(modelConfigDir, new[] { modelconfigOmi});

            OpenDA.DotNet.OpenMI.Bridge.ModelFactory.InsertModelFactory(mikeSheOpenMIModelFactory);

            ModelFactory openDaModelFactory = new ModelFactory();
            openDaModelFactory.Initialize(null,null);
            
            IModelInstance modelInstance = openDaModelFactory.GetInstance(new string[] {}, outputLevel: 0);
            modelInstance.Compute(modelInstance.TimeHorizon.EndTime);
        }

        [Test]
        public void RunMikeSheSimulationFromOpenDaConfig()
        {
            MikeSheOpenMIModelFactory mikeSheOpenMIModelFactory = new MikeSheOpenMIModelFactory();

            OpenDA.DotNet.OpenMI.Bridge.ModelFactory.InsertModelFactory(mikeSheOpenMIModelFactory);

            string odaDirectoryPath = "../../testData";
            string odaFileName = "MikeSimulation.oda";

            ApplicationRunnerSingleThreaded applicationRunner = new ApplicationRunnerSingleThreaded();
            applicationRunner.runSingleThreaded(new java.io.File(odaDirectoryPath), odaFileName);

        }
    }
}
