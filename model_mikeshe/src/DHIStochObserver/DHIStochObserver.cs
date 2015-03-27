using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using DHI.Generic.MikeZero.DFS.dfs123;
using Oatc.OpenMI.Sdk.Backbone;
using OpenDA.DotNet.Bridge;
using OpenDA.DotNet.Interfaces;
using DHI.Generic.MikeZero.DFS;
using MathNet.Numerics.Distributions;
using MathNet.Numerics.Random;


namespace org.openda.dotnet.DHIStochObserver
{

    /// <summary>
    /// A DHI .NET stochastic observer class that reads DHI type .DFS files.
    /// This is used as part of OpenDA.
    /// Supported file types:
    ///     - dfs0, dfs2, dfs3.
    /// 
    /// Written by Nils van Velzen and Marc Ridler
    /// </summary>
    public class DHIStochObserver : IStochObserver, IObservationDescriptions
    {

        // Only two options. Either a dfs0 file, or a dfs2/dfs3 file.
        private IDfsRead _dfsInfo;
        private List<DataPoint> _selectedDataPoints; 
        private ITime _selectionTime;

        /// <summary>
        /// Constructor 
        /// </summary>
        public DHIStochObserver()
        {
        }



        /// <summary>
        /// Construct a DHI stoch observer from input file.
        /// NOTE. Only one dfs file supported. 
        /// - DFS0 files may contain multiple time series of different type.
        ///  
        /// </summary>
        /// <param name="workingDir">Directory containing a .DFS file. (in string format)</param>
        /// <param name="arguments">arguments (the dfs file name).</param>
        
        public void Initialize(String workingDir, String[] arguments)
        {
            string dfsFileName;

            if (!Directory.Exists(workingDir))
            {
                // The workingDir does not exist
                throw new FileNotFoundException("Directory does not exist:" + workingDir);
            }

            if (arguments != null)
            {
                dfsFileName = Path.Combine(workingDir, arguments[0]);
                if(!File.Exists(dfsFileName))
                {
                    throw new FileNotFoundException("File does not exist:" + dfsFileName);
                }
            }
            else
            {
                throw new Exception("Argument containing the dfs file name required to initialize DHIStockObserver.");
            }

            // Determine Type
            string fileExtension = Path.GetExtension(dfsFileName);
            if (String.Compare(fileExtension, ".dfs3", StringComparison.OrdinalIgnoreCase) == 0 ||
                String.Compare(fileExtension, ".dfs2", StringComparison.OrdinalIgnoreCase) == 0 ||
                String.Compare(fileExtension, ".dfs0", StringComparison.OrdinalIgnoreCase) == 0
                )
            {
                fileExtension = Path.GetExtension(dfsFileName);
            }
            else
            {
                throw new Exception("\n ERROR: Observation File Type Incorrect! Expecting dfs2, dfs3 or dfs0. \n  \n");
            }

            if (StringComparer.OrdinalIgnoreCase.Equals(fileExtension, ".dfs3") ||
                    StringComparer.OrdinalIgnoreCase.Equals(fileExtension, ".dfs2"))
            {
                throw new NotImplementedException("not done yet");
            }
            else if (String.Compare(fileExtension, ".dfs0", StringComparison.OrdinalIgnoreCase) == 0)
            {
                _dfsInfo = new Dfs0Reader(dfsFileName);
            }
            else
            {
                throw new Exception("\n ERROR: Observation File Type Incorrect! Expecting dfs2, dfs3 or dfs0. \n  \n");
            }

            // The Selection begins with the entire dfs file. 
            _selectionTime = (ITime)(new OpenDA.DotNet.Bridge.Time(_dfsInfo.StartTime.ToModifiedJulianDay()-1, _dfsInfo.EndTime.ToModifiedJulianDay()));

            // Gets the data ready. Reads all the points and stores to memory.
            _selectedDataPoints = _dfsInfo.GetDataFromTimeRange(new Oatc.OpenMI.Sdk.Backbone.Time(_selectionTime.BeginTime.MJD).ToDateTime(), new Oatc.OpenMI.Sdk.Backbone.Time(_selectionTime.EndTime.MJD).ToDateTime());
        }



        /*
        * Create an new Stochastic Observer, containing a selection of the present stochastic observer.<br>
        * The selection criteria is a timeSpan. The start time of the interval is not included, the end time is
        * included, i.e. t_start<t<=t_end
        * @param selectionTimes    timeSpan with selection.
        * @return                  Stochastic Observer containing the required selection.
        */
        public IStochObserver createSelection(ITime selectionTimes)
        {
            _selectionTime = selectionTimes;
            _selectedDataPoints = _dfsInfo.GetDataFromTimeRange(new Oatc.OpenMI.Sdk.Backbone.Time(_selectionTime.BeginTime.MJD).ToDateTime(), new Oatc.OpenMI.Sdk.Backbone.Time(_selectionTime.EndTime.MJD).ToDateTime());
            return this;
        }

        /**
         * Create an new Stochastic Observer, containing a selection of the present stochastic observer.<br>
         * The selection criteria is the type of observations: assimilation or validation
         *
         * @param observationType The requested type of observations.
         * @return Stochastic Observer containing the required selection.
         */
        public IStochObserver createSelection(Type observationType)
        {
            return null;
        }

        /**
         * Number of observations in the Stochastic Observer.
         * @return The number of observations.
         */
        public int getCount()
        {
            int nObs = _selectedDataPoints.Count;
            return nObs;
        }


        /**
         * Get the values for all observations.
         * @return The observed values.
         */
        public double[] getValues()
        {
            return _selectedDataPoints.Select(p => p.Data).ToArray();
        }

        /**
         * Get realization values for all observations, for one ensemble member.
         * REALIZATION = MEASURED + NOISE
         * @return The realizations.
         */
        public double[] getRealizations()
        {
            var measured = getValues();
            var noise = getStandardDeviations();
            for (int i = 0; i < measured.Count(); i++)
            {
                measured[i] += noise[i];
            }
            return measured;
        }

        /**
         * Get expectation values for all stochastic observations.
         * EXPECTATION = MEASURED VALUE
         * @return The expectations.
         */
        public double[] getExpectations()
        {
            return getValues();
        }

        /**
         * Evaluate the PDF for stochastic observations, given the values for those observation.
         * @param values values for observation's PDF-evaluation.
         * @return The PDF evaluations.
         */
        public double evaluatePDF(double[] values)
        {
            return 0.0;
        }

        /**
         * Evaluate the PDF for stochastic observations, given the values for those observation.
         * @param values values for observation's PDF-evaluation.
         * @return The PDF evaluations.
         */
        public double[] evaluateMarginalPDFs(double[] values)
        {
            return null;
        }

        /**
         * Get the covariance matrix (as a vector) for the stochastic observations.
         * @return The covariance matrix.
         */
        public ISqrtCovariance getSqrtCovariance()
        {
            return null;
        }

        //TODO: Implement return Standard deviation. 
        /**
         * Get the standard deviation for each each stochastic observation.
         * @return The standard deviations.
         */
        public double[] getStandardDeviations()
        {
            const StandardDeviationTypes type = StandardDeviationTypes.Global;

            switch (type)
            {
               case StandardDeviationTypes.Global:

                    //TODO: FIX HARD CODED standard deviation.
                    const double standardDeviation = 0.02;

                    int n = _selectedDataPoints.Count;
                    double[] generatedStd = new double[n];
                    Random mt = new MersenneTwister();
                    Normal.Samples(mt, generatedStd, 0.0, standardDeviation);


                    return generatedStd;
                break;

                default:
                    throw new NotImplementedException("Only global standard deviations supported for now.");
                    break;
            }


        }

        private enum StandardDeviationTypes
        {
            Global,
            TimeVarying,
            BasedOnObservationDesciption
        }


        public ITime[] Times
        {
            get 
            {
                var times = _selectedDataPoints.Select(p => p.Time).ToList().ConvertAll(new Converter<DateTime, ITime>(DateTimeToITime)).ToArray();  
                return times;
                
            }
        }

        /**
         * free the Stochastic Observer.
         */
        public void free()
        {
            
        }

        /**
         * Get the observation descriptions.
         * @return The Observation Descriptions 
         */
        public IObservationDescriptions getObservationDescriptions()
        {
            return (IObservationDescriptions)this;
        }

        /* Methods from the ObservationDescriptions */


        /// <summary>
        /// Get the exchange items describing the measures available in the stoch. observer.
        /// </summary>
        /// <returns>All exchange items in the stoch. observer.</returns>
        public List<IExchangeItem> ExchangeItems
        {
            get { throw new NotImplementedException("ExchangeItems Not implemented."); }
        }


        /// <summary>
        /// Get properties (values) that correspond to a given key.
        /// </summary>
        /// <param name="Key">I  key for which the value is asked</param>
        /// <returns>Properties (column of data from observation descriptions)</returns>
        public IVector GetValueProperties(String Key)
        {
            IVector values;

            //We assume no error so we first collect all coordinate data
            int nObs = this.getCount();
            double[] X = new double[nObs];
            double[] Y = new double[nObs];
            double[] Z = new double[nObs];
            double[] Q = new double[nObs];
            for (int iObs = 0; iObs < nObs; iObs++)
            {
                X[iObs] = _selectedDataPoints[iObs].XYLayerPoint.X;
                Y[iObs] = _selectedDataPoints[iObs].XYLayerPoint.Y;
                Z[iObs] = _selectedDataPoints[iObs].XYLayerPoint.Layer;
                Q[iObs] = 1;  //MEANS HEAD
            }

            //See what we have to return
            Key.ToLower();
            if (System.String.CompareOrdinal(Key, "xposition") == 0)
            {
                values = new Vector(X);
            }
            else if (System.String.CompareOrdinal(Key, "yposition") == 0)
            {
                values = new Vector(Y);
            }
            else if (System.String.CompareOrdinal(Key, "height") == 0)
            {
                values = new Vector(Z);
            }
            else if (System.String.CompareOrdinal(Key, "quantity") == 0)
                values = new Vector(Q);
            else
            {
                throw new Exception("Property " + Key + " is not supported/known by the DHIStockObserver.");
            }
          
            return values;
        }

        /// <summary>
        /// Get properties (strings) that correspond to a given key.
        /// </summary>
        /// <param name="Key">I  key for which the value is asked</param>
        /// <returns>Properties (column of data from observation descriptions)</returns>
        public String[] GetStringProperties(String Key)
        {
            int nObs = this.getCount();
            
            String[] Q = new String[nObs];
            for (int iObs = 0; iObs < nObs; iObs++)
            {
               
                Q[iObs] = "Head";   //TODO not hard coded
            }

            //See what we have to return
            Key.ToLower();

            if (System.String.CompareOrdinal(Key, "quantity") == 0)
                return Q;
            else
            {
                throw new Exception("Property " + Key + " is not supported/known by the DHIStockObserver.");
            }
        }

        /// <summary>
        /// Get names of all keys.
        /// </summary>
        /// <returns>error status: All keys of the observation descriptions</returns>
        public String[] PropertyKeys
        {
            get { return _selectedDataPoints.Select(p => p.VariableID).ToArray(); }
        }

        /// <summary>
        /// Get number of properties/keys.
        /// </summary>
        /// <returns>number of properties </returns>
        public int PropertyCount
        {
            get { return 0; }
        }

        /// <summary>
        /// Get number of observations.
        /// </summary>
        /// <returns>number of observations</returns>
        public int ObservationCount
        {
            get { return this.getCount(); }
        }

        #region Static converts
        private static ITime DateTimeToITime(DateTime dateTime)
        {
            return new OpenDA.DotNet.Bridge.Time(dateTime.ToModifiedJulianDay());
        }

        #endregion

    }
}
