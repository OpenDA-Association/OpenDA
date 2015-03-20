using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NUnit.Framework;
using org.openda.dotnet.DHIStochObserver;
using Oatc.OpenMI.Sdk.Backbone;
using OpenDA.DotNet.Interfaces;
using OpenMI.Standard2.TimeSpace;

namespace org.openda.dotnet.DHIStochObserverTest
{
    
    public class Dfs0ReaderTest
    {
        [Test]
        public void TestInitialize()
        {
            org.openda.dotnet.DHIStochObserver.DHIStochObserver stochObs = new org.openda.dotnet.DHIStochObserver.DHIStochObserver();
            String workingDir = @"..\..\data\"; 
            String[] args = new string[1];
            args[0] = "Test_dfs0.dfs0";

         //   stochObs.Initialize(workingDir,args);

        }


        [Test]
        public void TestGet()
        {
            org.openda.dotnet.DHIStochObserver.DHIStochObserver stochObs = new org.openda.dotnet.DHIStochObserver.DHIStochObserver();
            String workingDir = @"..\..\data\";
            String[] args = new string[1];
            args[0] = "Test_dfs0.dfs0";

    //        stochObs.Initialize(workingDir, args);
            
            // All values (except first set)
            var allValues = stochObs.getValues();

            DateTime start;
            DateTime end;
            start = new DateTime(2014, 2, 27, 5, 0, 0);
            end = new DateTime(2014, 3, 4, 5, 0, 0);

            OpenDA.DotNet.Interfaces.ITime selection;
            //selection = new OpenDA.DotNet.Bridge.Time(start.ToModifiedJulianDay(), end.ToModifiedJulianDay());
            //IStochObserver stochObsSubset;
            //double[] stochObsSubsetValues;
            //stochObsSubset = stochObs.createSelection(selection);
            //stochObsSubsetValues = stochObsSubset.getValues();

            start = new DateTime(2015, 2, 27, 5, 0, 0);
            end = new DateTime(2015, 3, 4, 5, 0, 0);
            //selection = new OpenDA.DotNet.Bridge.Time(start.ToModifiedJulianDay(), end.ToModifiedJulianDay());
            //stochObsSubset = stochObs.createSelection(selection);
            //stochObsSubsetValues = stochObsSubset.getValues();


        }


    }
}
