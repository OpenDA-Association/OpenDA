namespace MikeSheInOpenDA.Spatial
{
    public class XYLayerPoint : IXYLayerPoint
    {
        private double _x;
        private double _y;
        private int _layer;


        /// <summary>
        /// Constructor.
        /// </summary>
        /// <returns>None</returns>
        public XYLayerPoint()
        {
            _x = double.NaN;
            _y = double.NaN;
            _layer = -9999;
        }

        /// <summary>
        /// Constructor.
        /// </summary>
        /// <returns>None</returns>
        public XYLayerPoint(double x, double y, int layer)
        {
            _x = x;
            _y = y;
            _layer = layer;
        }


        /// <summary>
        /// Read/Write property describing the x-coordinate of the point.
        /// </summary>
        public double X
        {
            get { return _x; }
            set { _x = value; }
        }

        /// <summary>
        /// Read/Write property describing the y-coordinate of the point.
        /// </summary>
        public double Y
        {
            get { return _y; }
            set { _y = value; }
        }

        /// <summary>
        /// Read/Write property describing the z-coordinate of the point.
        /// </summary>
        public int Layer
        {
            get { return _layer; }
            set { _layer = value; }
        }


        public override string ToString()
        {
            // For debugging purposes, overriding the ToString method.
            return string.Format("XYLayerPoint ({0},{1},layer:{2})", 
                _x.ToString(System.Globalization.CultureInfo.InvariantCulture),
                _y.ToString(System.Globalization.CultureInfo.InvariantCulture),
                _layer.ToString(System.Globalization.CultureInfo.InvariantCulture));
        }
    }
}

