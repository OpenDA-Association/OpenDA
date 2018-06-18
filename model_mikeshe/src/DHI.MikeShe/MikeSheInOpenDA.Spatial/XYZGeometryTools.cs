using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DHI.OpenMI2.Sdk.Backbone;
using DHI.OpenMI2.Sdk.Spatial;

namespace MikeSheInOpenDA.Spatial
{
    public class XYZGeometryTools
    {
        public const double EPSILON = 1e-5;

        /// <summary>
        /// Returns the distance between the two points.
        /// </summary>
        /// <param name="p1">Point</param>
        /// <param name="p2">Point</param>
        /// <returns>Point to point distance</returns>
        public static double CalculatePointToPointDistance2D(IXYLayerPoint p1, IXYLayerPoint p2)
        {
            double dx = p1.X - p2.X;
            double dy = p1.Y - p2.Y;
            return Math.Sqrt(dx * dx + dy * dy);
        }

        /// <summary>
        /// Returns the distance between the two points.
        /// </summary>
        /// <param name="p1">Point</param>
        /// <param name="p2">Point</param>
        /// <returns>Point to point distance</returns>
        public static double CalculatePointToPointDistance2D(IXYLayerPoint p1, ICoordinate p2)
        {
            double dx = p1.X - p2.X;
            double dy = p1.Y - p2.Y;
            return Math.Sqrt(dx * dx + dy * dy);
        }


        public static bool IsPointInPolygon(double x, double y, XYPolygon polygon)
        {

            if (x > polygon.GetX(0) && x < polygon.GetX(1) &&
                y > polygon.GetY(0) && y < polygon.GetY(2))
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        /// <summary>
        /// Finds if the given point is located in the model grid.
        /// Cycles through every grid rectangle.
        /// </summary>
        /// <param name="pt">point to check if it's within the grid.</param>
        /// <param name="modelCoordinates">the model grid.</param>
        /// <param name="layerIndifferent">layer specific</param> 
        /// <returns></returns>
        public static bool IsPointInModelPlain(IXYLayerPoint pt, IDictionary<int, ISpatialDefine> modelCoordinates, bool layerIndifferent = false)
        {
            return modelCoordinates.Values.Any(rec => rec.PointInObject(new XYLayerPoint(pt.X, pt.Y, 0), layerIndifferent));
        }

        /// <summary>
        /// Finds if the given point is located in the model grid.
        /// Cycles through every grid rectangle.
        /// </summary>
        /// <param name="pt">point to check if it's within the grid.</param>
        /// <param name="modelCoordinates">the model grid.</param>
        /// <param name="layerIndifferent">layer specific</param> 
        /// <returns></returns>
        public static bool IsPointInModelPlain(ICoordinate pt, IDictionary<int, ISpatialDefine> modelCoordinates, bool layerIndifferent = false)
        {
            return modelCoordinates.Values.Any(rec => rec.PointInObject(new XYLayerPoint(pt.X, pt.Y, 0), layerIndifferent));
        }
        /// <summary>
        /// Finds if the given point is located in the model grid.
        /// Cycles through every grid rectangle.
        /// </summary>
        /// <param name="pt">point to check if it's within the grid.</param>
        /// <param name="modelCoordinates">the model grid.</param>
        /// <param name="layerIndifferent">layer specific</param>
        /// <returns></returns>
        public static bool IsPointInModelPlain(ICoordinate pt, IList<ISpatialDefine> modelCoordinates, bool layerIndifferent = false)
        {
            return modelCoordinates.Any(rec => rec.PointInObject(new XYLayerPoint(pt.X, pt.Y, 0), layerIndifferent));
        }


        /// <summary>
        /// Finds if the given point is located in the model grid.
        /// Cycles through every grid rectangle.
        /// </summary>
        /// <param name="pt">point to check if it's within the grid.</param>
        /// <param name="modelCoordinates">the model grid.</param>
        /// <param name="layerIndifferent">layer specific</param>
        /// <returns></returns>
        public static bool IsPointInModelPlain(IXYLayerPoint pt, IList<ISpatialDefine> modelCoordinates, bool layerIndifferent = false)
        {
            return modelCoordinates.Any(rec => rec.PointInObject(new XYLayerPoint(pt.X, pt.Y, 0), layerIndifferent));
        }


        /// <summary>
        /// Seaches the model coordinates to find if the given point is locaded within the model. If so, return the model index.
        /// NOTE the point is a Z LAYER!
        /// </summary>
        /// <param name="pt">a point to search for</param>
        /// <param name="modelCoordinates">Dictionary containing the model coordinates as values and the key is the model index for each coordinate.</param>
        /// <returns>Return model index if found, else return -1</returns>
        public static int ModelIndexWherePointIsLocated(IXYLayerPoint pt, IDictionary<int, ISpatialDefine> modelCoordinates)
        {
            foreach (KeyValuePair<int, ISpatialDefine> pair in modelCoordinates)
            {
                if (pair.Value.PointInObject(new XYLayerPoint(pt.X, pt.Y, pt.Layer), false))
                {
                    return pair.Key;
                }
                ;
            }
            return -1;
        }

        /// <summary>
        /// Seaches the model coordinates to find if the given point is locaded within the model. If so, return the model index.
        /// NOTE the point is a Z LAYER!
        /// </summary>
        /// <param name="pt">a point to search for</param>
        /// <param name="modelCoordinates">Dictionary containing the model coordinates as values and the key is the model index for each coordinate.</param>
        /// <returns>Return model index if found, else return -1</returns>
        public static int ModelIndexWherePointIsLocated(ICoordinate pt, IDictionary<int, ISpatialDefine> modelCoordinates)
        {
            foreach (KeyValuePair<int, ISpatialDefine> pair in modelCoordinates)
            {
                if (pair.Value.PointInObject(new XYLayerPoint(pt.X, pt.Y, Convert.ToInt32(pt.Z)), false))
                {
                    return pair.Key;
                }
                ;
            }
            return -1;
        }





    }
}
