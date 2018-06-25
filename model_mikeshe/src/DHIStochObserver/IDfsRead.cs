using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using OpenDA.DotNet.Interfaces;

namespace org.openda.dotnet.DHIStochObserver
{
    public interface IDfsRead
    {
        /// <summary>
        /// Return number of time steps in dfs file. Does not check for NaN
        /// </summary>
        int NumberTimeSteps { get; }

        /// <summary>
        /// Return number of items in the dfs file. 
        /// </summary>
        int NumberOfItems { get ; }

        /// <summary>
        /// Get a dictionary of datetime,double values with real data.
        /// The order or returned data is by time then time series.
        /// ie. down the column then accross
        /// </summary>
        /// <param name="startTime">start time (not included)</param>
        /// <param name="endTime">end time (inclusive)</param>
        /// <returns>Dictionary of real values with corresponding DateTime</returns>
        List<DataPoint> GetDataFromTimeRange(DateTime startTime, DateTime endTime);

        /// <summary>
        /// Get the start date of the dfs file. Does not check if there is real data.
        /// </summary>
        DateTime StartTime { get; }

        /// <summary>
        /// Get the end date of the dfs file. Does not check if there is real data.
        /// </summary>
        DateTime EndTime { get; }

    }
}
