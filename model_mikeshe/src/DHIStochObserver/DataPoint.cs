using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace org.openda.dotnet.DHIStochObserver
{
    /// <summary>
    /// Class to store one data point. A data point consists of a time stamp (DateTime), 
    /// a double value, and an XYLayer posistion.
    /// </summary>
    public class DataPoint
    {
        private readonly DateTime _time ;
        private readonly double _data;
        private readonly IXYLayerPoint _xyLayerPoint;
        private readonly string _variableID;

        /// <summary>
        /// Constructor to store the information.
        /// </summary>
        /// <param name="time">DateTime time</param>
        /// <param name="data">double data value</param>
        /// <param name="xyLayerPoint">an X,Y,Layer position</param>
        /// <param name="variableID">variable ID</param>
        public DataPoint(DateTime time, double data, IXYLayerPoint xyLayerPoint, string variableID)
        {
            _time = time;
            _data = data;
            _xyLayerPoint = xyLayerPoint;
            _variableID = variableID;
        }

        /// <summary>
        /// Data value (double)
        /// </summary>
        public double Data { get { return _data; } }
        
        /// <summary>
        /// Date stamp (DateTime)
        /// </summary>
        public DateTime Time { get { return _time; } }
        
        /// <summary>
        /// An X,Y,Layer position.
        /// </summary>
        public IXYLayerPoint XYLayerPoint { get { return _xyLayerPoint; } }

        /// <summary>
        /// The Variable ID description
        /// </summary>
        public String VariableID { get { return _variableID; } }
    }
}
