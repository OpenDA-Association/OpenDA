namespace MikeSheInOpenDA.Spatial
{
    public interface ISpatialDefine
    {

        /// <summary>
        /// Get Geometry type of this class. Can be either 1D (point), 2D, 3D
        /// </summary>
        GeometryTypes Geometry {get;}

        /// <summary>
        /// Get the minimum X coordinate between the two points.
        /// If it's a Geometry type Point, return pt1 x coordinate.
        /// </summary>
        double XMinCoordinate {get;}

        /// <summary>
        /// Get the minimum Y coordinate between the two points.
        /// If it's a Geometry type Point, return pt1 y coordinate.
        /// </summary>
        double YMinCoordinate {get;}

        /// <summary>
        /// Get the maximum X coordinate between the two points.
        /// If it's a Geometry type Point, return pt1 x coordinate.
        /// </summary>
        double XMaxCoordinate {get;}

        
        /// <summary>
        /// Get the maximum Y coordinate between the two points.
        /// If it's a Geometry type Point, return pt1 y coordinate.
        /// </summary>
        double YMaxCoordinate {get;}

        /// <summary>
        /// Get the Layer number.
        /// Note that the layer numbers must be the same for the two points.
        /// </summary>
        int Layer {get;}

        /// <summary>
        ///  Returns the spatial Midpoint of the entity
        /// </summary>
        IXYLayerPoint MidPoint {get;}

        /// <summary>
        /// Return whether the point is inside this structure.
        /// If this structure is a 1D point, then the (x,y,layer) values must be the same.
        /// </summary>
        /// <param name="pt">The point to check wether it's within this object spatially.</param>
        /// <returns></returns>
        bool PointInObject(IXYLayerPoint pt);

    }
}
