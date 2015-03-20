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
        /// <param name="_point">point to check if it's within the grid.</param>
        /// <param name="_grid">the model grid.</param>
        /// <returns></returns>
        public static bool IsPointInModelPlain(ICoordinate pt, IDictionary<int, ISpatialDefine> modelCoordinates)
        {
            return modelCoordinates.Values.Any(rec => rec.PointInObject(new XYLayerPoint(pt.X, pt.Y, 0)));
        }


        /// <summary>
        /// Finds if the given point is located in the model grid.
        /// Cycles through every grid rectangle.
        /// </summary>
        /// <param name="pt">point to check if it's within the grid.</param>
        /// <param name="modelCoordinates">the model grid.</param>
        /// <returns></returns>
        public static bool IsPointInModelPlain(IXYLayerPoint pt, IDictionary<int, ISpatialDefine> modelCoordinates)
        {
            return modelCoordinates.Values.Any(rec => rec.PointInObject(new XYLayerPoint(pt.X, pt.Y, pt.Layer)));
        }

        /// <summary>
        /// Model Index for given point
        /// </summary>
        /// <param name="pt">point to check if it's within the grid.</param>
        /// <param name="modelCoordinates">the model grid.</param>
        /// <returns></returns>
        public static int ModelIndexForPoint(IXYLayerPoint pt, IDictionary<int, ISpatialDefine> modelCoordinates)
        {
            foreach(KeyValuePair<int,ISpatialDefine> modelGrid in modelCoordinates)
            {
                if ( modelGrid.Value.PointInObject( pt ) )
                {
                    return modelGrid.Key;
                }
            }
            return -999;
        }

    }
}
