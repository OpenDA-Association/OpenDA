using System;


namespace MikeSheInOpenDA.Spatial
{
    public class SpatialDefine : ISpatialDefine
    {
        
        /// <summary>
        ///  _pt1 is the default point (for Type Point)
        ///  To define a 2D Rectangle or 3D Cuboid use two points on opposite 
        ///  end of the rectangle or cube.
        /// </summary>
        private IXYLayerPoint _pt1;
        private IXYLayerPoint _pt2;
        private GeometryTypes _geometry;  
        
        /// <summary>
        /// Default Constructor
        /// </summary>
        public SpatialDefine()
        {
            _pt1 = new XYLayerPoint(-9999, -9999, -9999);
            _pt2 = new XYLayerPoint(-9999, -9999, -9999);
            _geometry = GeometryTypes.GeometryPoint; 
        }

        /// <summary>
        /// Constructor 1D
        /// </summary>
        public SpatialDefine(IXYLayerPoint pt1)
        {
            _pt1 = pt1;
            _geometry = GeometryTypes.GeometryPoint;
        }


        /// <summary>
        /// Constructor 2D or 3D
        /// </summary>
        public SpatialDefine(IXYLayerPoint pt1, IXYLayerPoint pt2, GeometryTypes gm)
        {
            _pt1 = pt1;
            _pt2 = pt2;
            _geometry = gm;

            if (pt1.Layer != pt2.Layer)
            {
                throw new Exception("Layer number must be the same for the two points");
            }
        }


        /// <summary>
        /// Get Geometry type of this class. Can be either 1D (point), 2D, 3D
        /// </summary>
        public GeometryTypes Geometry
        {
            get
            {
                return _geometry;
            }
        }

        /// <summary>
        /// Get the minimum X coordinate between the two points.
        /// If it's a Geometry type Point, return pt1 x coordinate.
        /// </summary>
        public double XMinCoordinate
        {
            get
            {
                if (_geometry == GeometryTypes.GeometryPoint)
                {
                    return _pt1.X;
                }
                else
                {
                    return _pt1.X  < _pt2.X ? _pt1.X : _pt2.X ;
                }
            }
        }


        /// <summary>
        /// Get the minimum Y coordinate between the two points.
        /// If it's a Geometry type Point, return pt1 y coordinate.
        /// </summary>
        public double YMinCoordinate
        {
            get
            {
                if (_geometry == GeometryTypes.GeometryPoint)
                {
                    return _pt1.Y;
                }
                else
                {
                    return _pt1.Y < _pt2.Y ? _pt1.Y : _pt2.Y;
                }
            }
        }



        /// <summary>
        /// Get the maximum X coordinate between the two points.
        /// If it's a Geometry type Point, return pt1 x coordinate.
        /// </summary>
        public double XMaxCoordinate
        {
            get
            {
                if (_geometry == GeometryTypes.GeometryPoint)
                {
                    return _pt1.X;
                }
                else
                {
                    return _pt1.X > _pt2.X ? _pt1.X : _pt2.X;
                }
            }
        }


        /// <summary>
        /// Get the maximum Y coordinate between the two points.
        /// If it's a Geometry type Point, return pt1 y coordinate.
        /// </summary>
        public double YMaxCoordinate
        {
            get
            {
                if (_geometry == GeometryTypes.GeometryPoint)
                {
                    return _pt1.Y;
                }
                else
                {
                    return _pt1.Y > _pt2.Y ? _pt1.Y : _pt2.Y;
                }
            }
        }


        /// <summary>
        /// Get the Layer number.
        /// Note that the layer numbers must be the same for the two points.
        /// </summary>
        public int Layer
        {
            get
            { 
                return _pt1.Layer;
            }
        }


        /// <summary>
        ///  Returns the spatial Midpoint of the entity
        /// </summary>
        public IXYLayerPoint MidPoint
        {
            get {
                if (_geometry == GeometryTypes.GeometryPoint)
                {
                    return new XYLayerPoint(_pt1.X, _pt1.Y, _pt1.Layer); 
                }
                else
                {
                    return new XYLayerPoint((_pt1.X + _pt2.X) / 2, (_pt1.Y + _pt2.Y) / 2, _pt1.Layer); 
                }
            }
        }

        /// <summary>
        /// Return whether the point is inside this structure.
        /// If this structure is a 1D point, then the (x,y,layer) values must be the same.
        /// </summary>
        /// <param name="pt">The point to check wether it's within this object spatially.</param>
        /// <returns></returns>
        public bool PointInObject(IXYLayerPoint pt)
        {
            if (_geometry == GeometryTypes.GeometryPoint)
            {
                double EPSILON = 0.001;
                if (Math.Abs(pt.X - _pt1.X) < EPSILON && Math.Abs(pt.Y - _pt1.Y) < EPSILON && pt.Layer == _pt1.Layer)
                { return true; }
                else
                { return false; }
            }
            else // 2D or 3D case

            if (pt.X > XMinCoordinate && pt.X < XMaxCoordinate &&
                pt.Y > YMinCoordinate && pt.Y < YMaxCoordinate &&
                pt.Layer == _pt1.Layer)
            {
                return true;
            }
            else
            {
                return false;
            }

        }

    }
}
