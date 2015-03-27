using System;
using System.Collections.Generic;
using System.Linq;
using DHI.Generic.MikeZero.DFS;
using System.IO;

namespace org.openda.dotnet.DHIStochObserver
{
    /// <summary>
    /// Reads a DFS0 file.
    /// - Assumptions:
    /// 1) Equidistant time series.
    /// 2) All items in the dfs0 are the same variable type.
    /// 3) the variableID (key) is contained in the title.
    /// 4) Each item has a name containing the x,y,z coordinate (where z is an integer layer).
    /// 5) Instantaneous time series only.
    /// </summary>
    public class Dfs0Reader : IDfsRead
    {

        private readonly IDfsFile _dfs0File ;
        private readonly int _numTimeSteps;
        private readonly int _firstTimeStepIndex;
        private readonly double _refdateMJD;
        private readonly int _numItems;
        private readonly List<string> _itemIDs;
        private readonly List<DateTime> _times;
        private readonly double _deleteValueDouble;
        private readonly float _deleteValueFloat;
        private readonly List<IXYLayerPoint> _xyLayerPoints;

        //private readonly List<IExchangeItem> _exchangeItems;
        //private readonly List<TimeSerie> _timeSeries;  

        /// <summary>
        /// DFS0 reader. Gets information from the dfs file, and reads data.
        /// </summary>
        /// <param name="dfsfile">full path string to dfs0 file.</param>
        public Dfs0Reader(string dfsfile)
        {
            // Set ObservationFile
            if (!File.Exists(dfsfile))
                throw new FileNotFoundException("\n ERROR: DFS File Not Found! \n Could not find: {0} \n", dfsfile);

            // Determine Type
            string fileExtension = Path.GetExtension(dfsfile);
            if (System.String.Compare(fileExtension, ".dfs0", System.StringComparison.OrdinalIgnoreCase) == 0)
            {
                fileExtension = Path.GetExtension(dfsfile);
            }
            else
            {
                throw new Exception("\n ERROR: Observation File Type Incorrect! Expecting dfs0. \n  \n");
            }


            // Open the file as a generic dfs file
            _dfs0File = DfsFileFactory.DfsGenericOpen(dfsfile);

            // Header information is contained in the IDfsFileInfo
            IDfsFileInfo fileInfo = _dfs0File.FileInfo;

            // Check for dfs compliance
            CheckDFSCompliance();

            
            // Number of time steps (same for all items)
            _numTimeSteps = fileInfo.TimeAxis.NumberOfTimeSteps;

            // Starting from...
            int _firstTimeStepIndex = fileInfo.TimeAxis.FirstTimeStepIndex;

            // Number of variable items in dfs0
            _numItems = _dfs0File.ItemInfo.Count;

            // Add the IDs to list (Keys)
            _itemIDs = new List<string>();
            _xyLayerPoints = new List<IXYLayerPoint>();
            
            foreach (var itemInfo in _dfs0File.ItemInfo)
            {
                String name = itemInfo.Name;
                _itemIDs.Add(name);

                // Is the coordinate extracted from the name
                // X.Y_Layer
                int iPoint = name.IndexOf(".");
                int iUnderscore = name.IndexOf("_");
                if (iPoint > 0 && iUnderscore > 0 && iUnderscore > iPoint)
                {
                    String sX = name.Substring(0, iPoint);
                    String sY = name.Substring(iPoint + 1, iUnderscore - iPoint - 1);
                    String sL = name.Substring(iUnderscore + 1);
                    Console.WriteLine("Observation location from name: " + sX + " " + sY + " " + sL);
                    double x = Convert.ToDouble(sX);
                    double y = Convert.ToDouble(sY);
                    int zLayer = Convert.ToInt32(sL);
                      
                    _xyLayerPoints.Add(new XYLayerPoint(x, y, zLayer));
                 }
                 // Otherwise we hope it is stored in itemInfo.ReferenceCoordinate...
                 else {

                    double x = Convert.ToDouble(itemInfo.ReferenceCoordinateX);
                    double y = Convert.ToDouble(itemInfo.ReferenceCoordinateY);
                    // Check if an integer value.
                    if(itemInfo.ReferenceCoordinateZ - Math.Round(itemInfo.ReferenceCoordinateZ)>0.001)
                    {
                        Console.WriteLine("WARNING. The Z position in DFS0 file is not an integer. Rounding to nearest integer to retrieve layer integer value.\n");
                    }
                    int zLayer = Convert.ToInt32(Math.Round(itemInfo.ReferenceCoordinateZ));
                    _xyLayerPoints.Add(new XYLayerPoint(x, y,zLayer));
                }
            }


            //Gather all times
            _times = _dfs0File.FileInfo.TimeAxis.GetDateTimes().ToList();
            
           
            
            DateTime firstTime = _times[0];
       
            if (_dfs0File.FileInfo.TimeAxis.TimeAxisType != TimeAxisType.CalendarEquidistant){
                //Handle pseudo irreggular files
                double[] dates = new double[_numTimeSteps]; //just make 1 bigger for easy indexing
                

                for (int iTimeStep = _firstTimeStepIndex; iTimeStep < _numTimeSteps; iTimeStep++){
                    for (int iItem = 1; iItem < _numItems+1; iItem++){
                        IDfsItemData data1 = _dfs0File.ReadItemTimeStep(iItem, iTimeStep);
                        double offsetTime = data1.Time;
                        
                        if (iItem==1){
                            dates[iTimeStep]=offsetTime;
                        }
                        else {
                            if (Math.Abs(offsetTime-dates[iTimeStep])>1.0){
                                 throw new Exception("Non Equidistant Calander is not regular");
                            }
                        }
                    }
                    if (iTimeStep > 0)
                    {
                        _times[iTimeStep] =_times[0].AddSeconds(dates[iTimeStep]);
                    }
                }
            }
            
         

            IList<IDfsDynamicItemInfo> infoAllTimes = _dfs0File.ItemInfo;
            String TimeSeriesName=infoAllTimes[0].Name;

            // Delelte Values
            _deleteValueDouble = _dfs0File.FileInfo.DeleteValueDouble;
            _deleteValueFloat = _dfs0File.FileInfo.DeleteValueFloat;

        }

        /// <summary>
        /// Get a dictionary of datetime,double values with real data.
        /// The order or returned data is by time then time series.
        /// ie. down the column then accross.
        /// </summary>
        /// <param name="startTime">start time (not included)</param>
        /// <param name="endTime">end time (inclusive)</param>
        /// <returns>Dictionary of real values with corresponding DateTime</returns>
        public List<DataPoint> GetDataFromTimeRange(DateTime startTime, DateTime endTime)
        {
            List<int> timeIndicesWithinRange = new List<int>();

            List<DataPoint> valueSet = new List<DataPoint>();

            // Find the start and end indexes
            for (int i = 0; i < _times.Count; i++)
            {
                // if t_start<t<=t_end  then within range. 
                if ( DateTime.Compare( _times[i], startTime ) > 0 &&  DateTime.Compare( _times[i], endTime ) <= 0 )
                {
                    timeIndicesWithinRange.Add(i);       
                }
            }

            for (int i = 0; i < _numItems; i++)
            {
                for (int j = 0; j < timeIndicesWithinRange.Count; j++)
                {
                    double? value = GetDataValue(i, timeIndicesWithinRange[j]);
                    if(value != null)
                    {
                        valueSet.Add(new DataPoint(_times[timeIndicesWithinRange[j]], (double)value, _xyLayerPoints[i], _itemIDs[i]));   
                    }
                }
            }
            return valueSet;
        }



        private double? GetDataValue(int itemNum, int timeIdx)
        {
            if (_dfs0File.ItemInfo[itemNum].DataType == DfsSimpleType.Float)
            {
                var datastruct = (IDfsItemData<float>)_dfs0File.ReadItemTimeStep(itemNum + 1, timeIdx);
                // ReSharper disable CompareOfFloatsByEqualityOperator
                if (datastruct.Data[0] != _deleteValueFloat)
                // ReSharper restore CompareOfFloatsByEqualityOperator
                {
                    return Convert.ToDouble(datastruct.Data[0]);
                }

            }
            else if (_dfs0File.ItemInfo[itemNum].DataType == DfsSimpleType.Double)
            {
                var datastruct = (IDfsItemData<double>)_dfs0File.ReadItemTimeStep(itemNum + 1, timeIdx);
                // ReSharper disable CompareOfFloatsByEqualityOperator
                if (datastruct.Data[0] != _deleteValueDouble)
                // ReSharper restore CompareOfFloatsByEqualityOperator
                {
                    return datastruct.Data[0];
                }
            }
            // if values are not real, return null.
            return null;
        }


        /// <summary>
        /// Return the start date of the DFS file.
        /// </summary>
        public DateTime StartTime { get { return _times[0]; } }

        /// <summary>
        /// Return the end date of the DFS file.
        /// </summary>
        public DateTime EndTime { get { return _times[_times.Count-1]; } }

        /// <summary>
        /// Return number of time steps in dfs file. Does not check for NaN
        /// </summary>
        public int NumberTimeSteps { get { return _numTimeSteps; }}

        /// <summary>
        /// Return number of items in the dfs file. 
        /// </summary>
        public int NumberOfItems { get { return _numItems; } }

        /// <summary>
        /// Return the ID Keys of each item.
        /// </summary>
        public string[] ItemIDs
        {
            get { return _itemIDs.ToArray(); }
        }
        /// <summary>
        /// Checks if the dfs is compliant (calendar, equidistant, instantaneous items...).
        /// </summary>
        private void CheckDFSCompliance()
        {
            // CHECK if Calendar Equidistant (only one supported).
            if (_dfs0File.FileInfo.TimeAxis.TimeAxisType != TimeAxisType.CalendarEquidistant)
            {
                System.Console.WriteLine("Disables test on CalendarEquidistant");
                //throw new Exception("Error in dfs0 file. Only CalendarEquidistant supported");
            }

            // CHECK if not all items are "Instantaneous", then error
            foreach (var iteminfo in _dfs0File.ItemInfo)
            {
                if (iteminfo.ValueType != DataValueType.Instantaneous)
                {
                    throw new Exception("Error in dfs0 file. Only Instantaneous items supported");
                }
            }
        }
    }

}
