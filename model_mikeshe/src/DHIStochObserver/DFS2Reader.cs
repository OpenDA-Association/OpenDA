using System;
using System.Collections.Generic;
using System.Linq;
using DHI.Generic.MikeZero.DFS;
using DHI.Generic.MikeZero.DFS.dfs123;

namespace org.openda.dotnet.DHIStochObserver
{
    /// <summary>
    /// Reads a DFS2 file.
    /// - Assumptions:
    /// 1) Equidistant time series.
    /// 2) All items in the dfs2 are the same variable type.
    /// 3) the variableID (key) is contained in the title.
    /// 4) Each item has a name containing the x,y,z coordinate (where z is an integer layer).
    /// 5) Instantaneous time series only.
    /// </summary>
    public class DFS2Reader : IDfsRead
    {

        private readonly Dfs2File _dfs2File;
        private readonly int _numTimeSteps;
        private readonly int _numItems;
        private readonly List<string> _itemIDs;
        private readonly List<DateTime> _times;
        private readonly double _deleteValueDouble;
        private readonly float _deleteValueFloat;
        private readonly List<IXYLayerPoint> _xyLayerPoints;

        /// <summary>
        /// dfs2 reader. Gets information from the dfs file, and reads data.
        /// </summary>
        /// <param name="dfsfile">full path string to dfs2 file.</param>
        public DFS2Reader(string dfsfile)
        {
            throw new NotImplementedException("ToDo dfs2");

            // Open the file as a generic dfs file
            _dfs2File = DfsFileFactory.Dfs2FileOpen(dfsfile);

            // Header information is contained in the IDfsFileInfo
            IDfsFileInfo fileInfo = _dfs2File.FileInfo;

            // Check for dfs compliance
            CheckDFSCompliance();
            
            // Number of time steps (same for all items)
            _numTimeSteps = fileInfo.TimeAxis.NumberOfTimeSteps;

            // Number of variable items in dfs2
            _numItems = _dfs2File.ItemInfo.Count;

            // Add the IDs to list (Keys)
            _itemIDs = new List<string>();
            foreach (var itemInfo in _dfs2File.ItemInfo)
            {
                _itemIDs.Add(itemInfo.Name);
            }

            _times = _dfs2File.FileInfo.TimeAxis.GetDateTimes().ToList();

            // Delelte Values
            _deleteValueDouble = _dfs2File.FileInfo.DeleteValueDouble;
            _deleteValueFloat = _dfs2File.FileInfo.DeleteValueFloat;

            _xyLayerPoints = new List<IXYLayerPoint>();
            foreach (var itemInfo in _dfs2File.ItemInfo)
            {
                throw new NotImplementedException("ToDo dfs2");
            }
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
            if (_dfs2File.ItemInfo[itemNum].DataType == DfsSimpleType.Float)
            {
                var datastruct = (IDfsItemData<float>)_dfs2File.ReadItemTimeStep(itemNum + 1, timeIdx);
                // ReSharper disable CompareOfFloatsByEqualityOperator
                if (datastruct.Data[0] != _deleteValueFloat)
                // ReSharper restore CompareOfFloatsByEqualityOperator
                {
                    return Convert.ToDouble(datastruct.Data[0]);
                }

            }
            else if (_dfs2File.ItemInfo[itemNum].DataType == DfsSimpleType.Double)
            {
                var datastruct = (IDfsItemData<double>)_dfs2File.ReadItemTimeStep(itemNum + 1, timeIdx);
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
            if (_dfs2File.FileInfo.TimeAxis.TimeAxisType != TimeAxisType.CalendarEquidistant)
            {
                throw new Exception("Error in dfs0 file. Only CalendarEquidistant supported");
            }

            // CHECK if not all items are "Instantaneous", then error
            foreach (var iteminfo in _dfs2File.ItemInfo)
            {
                if (iteminfo.ValueType != DataValueType.Instantaneous)
                {
                    throw new Exception("Error in dfs0 file. Only Instantaneous items supported");
                }
            }
        }
    }

}
