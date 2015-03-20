using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using MikeSheInOpenDA.Spatial;
using OpenDA.DotNet.OpenMI.Bridge;
using OpenMI.Standard2;
using OpenMI.Standard2.TimeSpace;
using DHI.OpenMI2.Sdk.Spatial;
using DHI.OpenMI2.Sdk.Backbone;

namespace MikeSheInOpenDA
{
    public class MikeSheOpenMITimespaceComponentExtensions : DHI.OpenMI2.MikeShe.WMEngineAccess,ITimeSpaceComponentExtensions  
    {
        public ITime currentTime()
        {
            return this.CurrentTime;
        }

        /// <summary>
        /// For easy observation handling a model can provide some additional exhange items  especially for model predictions at observation locations
        /// </summary>
        /// <returns></returns>
        public IList<OpenDA.DotNet.Interfaces.IExchangeItem> getAdditionalExchangeItem()
        {
            IList<OpenDA.DotNet.Interfaces.IExchangeItem> additional = new List<OpenDA.DotNet.Interfaces.IExchangeItem>();
            // We will hack something here!

            return additional;
        }


        public IList<int> CreateModelIndicesHashTable(OpenDA.DotNet.Interfaces.IObservationDescriptions observationDescriptions)
        {
            IList<int> modelIndices = new List<int>();

            String[] keys = observationDescriptions.PropertyKeys;
            String[] quantity = observationDescriptions.GetStringProperties("quantity");
            double[] xpos = observationDescriptions.GetValueProperties("xposition").Values;
            double[] ypos = observationDescriptions.GetValueProperties("yposition").Values;
            double[] height = observationDescriptions.GetValueProperties("height").Values;

            int observationCount = observationDescriptions.ObservationCount;
            int nObs = quantity.Length; // same as observationCount?


            // The heights should be specified in an array of integers representing the layer. Check if the values are indeed integers or close to integers before converting
            // the array of doubles to an array of integers.
            const double tolerance = 1e-5;
            int[] layer = new int[observationCount];
            for (int i = 0; i < observationCount; i++)
            {
                layer[i] = Convert.ToInt32(height[i]);
                if (Math.Abs(layer[i] - height[i]) > tolerance)
                {
                    throw new Exception("The height specified in the observation was not an integer. Observation \n");
                }
            }

            // An array of model values corresponding to the observation points.
            double[] Hx = new double[nObs];

            List<string> listExchangeIds = ObsIDtoExchangeId(observationDescriptions);

            for (int obsC = 0; obsC < nObs; obsC++)
            {
                IDictionary<int, ISpatialDefine> modelCoord = GetModelCoordinates(listExchangeIds[obsC]);
                IXYLayerPoint obsPoint = new XYLayerPoint(xpos[obsC], ypos[obsC], layer[obsC]);

                int modelVariableIndex = XYZGeometryTools.ModelIndexForPoint(obsPoint, modelCoord);
                modelIndices.Add(modelVariableIndex);

                if (modelVariableIndex < 0)
                {
                    throw new Exception("The observation point was NOT in the model grid! For Point: (" + xpos[obsC].ToString() + "," + ypos[obsC].ToString() + "," + layer[obsC].ToString() + ") \n");
                }
            }
            return modelIndices;
        }


        private List<string> ObsIDtoExchangeId(OpenDA.DotNet.Interfaces.IObservationDescriptions observationDescriptions)
        {
            String[] keys = observationDescriptions.PropertyKeys;
            String[] quantity = observationDescriptions.GetStringProperties("quantity");
            double[] xpos = observationDescriptions.GetValueProperties("xposition").Values;
            double[] ypos = observationDescriptions.GetValueProperties("yposition").Values;
            double[] height = observationDescriptions.GetValueProperties("height").Values;

            int observationCount = observationDescriptions.ObservationCount;
            int nObs = quantity.Length; // same as observationCount?

            List<string> exchangeItemId = new List<string>();

            for (int obsC = 0; obsC < nObs; obsC++)
            {
                if (quantity[obsC].Equals("Head", StringComparison.OrdinalIgnoreCase))
                {
                    exchangeItemId.Add("head elevation in saturated zone,SZ3DGrid");
                }
                else if (quantity[obsC].Equals("SoilMoisture", StringComparison.OrdinalIgnoreCase))
                {
                    exchangeItemId.Add("water content in unsaturated zone,WMUZ3DGrid");
                }
                else if (quantity[obsC].Equals("SurfaceTemperature", StringComparison.OrdinalIgnoreCase))
                {
                    exchangeItemId.Add("Surface temperature (effective),BaseGrid");
                }
                else
                {
                    throw new Exception("Cannot (yet) handle obversvations of quantity (" + quantity[obsC] + ")");
                }
            }
            return exchangeItemId;
        }

        /// <summary>
        /// Hx = Gets the model values at the indices that are passed in the list.
        /// </summary>
        /// <param name="observationDescriptions">The description of the observations.</param>
        /// <param name="indices">List of indices to be retrieved from the model.</param>
        /// <returns>Array of model values at the given indices.</returns>
        public double[] ModelValuesAtProvidedIndices(OpenDA.DotNet.Interfaces.IObservationDescriptions observationDescriptions, IList<int> indices )
        {
            String[] keys = observationDescriptions.PropertyKeys;
            String[] quantity = observationDescriptions.GetStringProperties("quantity");
            double[] xpos = observationDescriptions.GetValueProperties("xposition").Values;
            double[] ypos = observationDescriptions.GetValueProperties("yposition").Values;
            double[] height = observationDescriptions.GetValueProperties("height").Values;

            int observationCount = observationDescriptions.ObservationCount;
            int nObs = quantity.Length; // same as observationCount?


            // The heights should be specified in an array of integers representing the layer. Check if the values are indeed integers or close to integers before converting
            // the array of doubles to an array of integers.
            const double tolerance = 1e-5;
            int[] layer = new int[observationCount];
            for (int i = 0; i < observationCount; i++)
            {
                layer[i] = Convert.ToInt32(height[i]);
                if (Math.Abs(layer[i] - height[i]) > tolerance)
                {
                    throw new Exception("The height specified in the observation was not an integer. Observation \n");
                }
            }

            List<string> obsIds = ObsIDtoExchangeId(observationDescriptions);

            // An array of model values corresponding to the observation points.
            double[] Hx = GetModelValuesDifferentVariables(obsIds, indices);

            return Hx;
        }

        public double[] getObservedValues(OpenDA.DotNet.Interfaces.IObservationDescriptions observationDescriptions)
        {
            String[] keys = observationDescriptions.PropertyKeys;
            String[] quantity = observationDescriptions.GetStringProperties("quantity");
            double[] xpos = observationDescriptions.GetValueProperties("xposition").Values;
            double[] ypos = observationDescriptions.GetValueProperties("yposition").Values;
            double[] height = observationDescriptions.GetValueProperties("height").Values;
            
            int observationCount = observationDescriptions.ObservationCount;
            int nObs = quantity.Length; // same as observationCount?


            // The heights should be specified in an array of integers representing the layer. Check if the values are indeed integers or close to integers before converting
            // the array of doubles to an array of integers.
            for (int i = 0; i < height.Length; i++) { if (double.IsNaN(height[i])) height[i] = 0.0; }
            const double tolerance = 1e-5;
            int[] layer = new int[observationCount];
            for (int i = 0; i < observationCount; i++)
            {
                layer[i] = Convert.ToInt32(height[i]);
                if (Math.Abs(layer[i] - height[i]) > tolerance)
                {
                    throw new Exception("The height specified in the observation was not an integer. Observation \n");
                }
            }

            // An array of model values corresponding to the observation points.
            double[] Hx = new double[nObs];

            for (int obsC = 0; obsC < nObs; obsC++)
            {
                // Set exchangeItem that corresponds to EntityID (no conversion yet)
                String exchangeItemId;
                if (quantity[obsC].Equals("Head", StringComparison.OrdinalIgnoreCase))
                {
                    exchangeItemId = "head elevation in saturated zone,SZ3DGrid";
                }
                else if (quantity[obsC].Equals("SoilMoisture", StringComparison.OrdinalIgnoreCase))
                {
                    exchangeItemId = "water content in unsaturated zone,WMUZ3DGrid";
                }
                else if (quantity[obsC].Equals("SurfaceTemperature", StringComparison.OrdinalIgnoreCase))
                {
                    exchangeItemId = "Surface temperature (effective),BaseGrid";
                }
                else
                {
                    throw new Exception("Cannot (yet) handle obversvations of quantity (" + quantity[obsC] + ")");
                }

                IDictionary<int, ISpatialDefine> modelCoord = GetModelCoordinates(exchangeItemId);
                IXYLayerPoint obsPoint = new XYLayerPoint(xpos[obsC], ypos[obsC], layer[obsC]);


                int modelVariableIndex = XYZGeometryTools.ModelIndexForPoint(obsPoint, modelCoord);


                if(modelVariableIndex >= 0)
                {
                    Hx[obsC] = GetModelValue(exchangeItemId, modelVariableIndex);
                }
                else
                {
                    throw new Exception("The observation point was NOT in the model grid! For Point: (" + xpos[obsC].ToString() + "," + ypos[obsC].ToString()+ "," + layer[obsC].ToString() + ") \n" );
                }
            }
            return Hx;
        }


        /// <summary>
        /// OpenMI does not know about localization. In this method, the user can implement localization for OpenMI models in this method
        /// </summary>
        /// <param name="exchangeItemId">the exchange ID string</param>
        /// <param name="observationDescriptions">OpenDA type of observation description</param>
        /// <param name="locDistance">the distance for gaussian localization</param>
        /// <returns></returns>
        public double[][] getLocalization(string exchangeItemId, OpenDA.DotNet.Interfaces.IObservationDescriptions observationDescriptions, double locDistance)
        {
            //Get the Keys from the observer
            String[] keys = observationDescriptions.PropertyKeys;
            String[] quantity = observationDescriptions.GetStringProperties("quantity");
            double[] xpos = observationDescriptions.GetValueProperties("xposition").Values;
            double[] ypos = observationDescriptions.GetValueProperties("yposition").Values;
            double[] height = observationDescriptions.GetValueProperties("height").Values;
            int observationCount = observationDescriptions.ObservationCount;

            // The heights should be specified in an array of integers representing the layer. Check if the values are indeed integers or close to integers before converting
            // the array of doubles to an array of integers.
            for (int i = 0; i < height.Length; i++) { if (double.IsNaN(height[i])) height[i] = 0.0;}

            const double tolerance = 1e-5;
            int[] layer = new int[observationCount];
            for (int i = 0; i < observationCount; i++)
            {
                layer[i] = Convert.ToInt32(height[i]);
                if ( Math.Abs(layer[i] -  height[i] ) > tolerance )
                {
                    throw new Exception("The height specified in the observation was not an integer. Observation \n");
                }
            }
            

            // Gets the Grid type for the model. Can be a number of possibilities depending on the variable.
            GeometryTypes geometrytype = GetGridType(exchangeItemId);
    

            //BaseGrid
            if (geometrytype == GeometryTypes.Geometry2D)
            {
                return GetLocalized2D(exchangeItemId, observationCount, locDistance, xpos, ypos);
            }
            if (geometrytype == GeometryTypes.Geometry3D)
            {
                return GetLocalized3D(exchangeItemId, observationCount, locDistance, xpos, ypos, layer);
            }
            else
            {
                throw new NotImplementedException("Only 3D SZ and 2D BaseGrid supported so far.");
            }
        }



        #region PrivateMethods

        private double[] GetModelValuesDifferentVariables(List<string> exchangeItemId, IList<int> modelVariableIndices)
        {
            double[] modelValues = new double[modelVariableIndices.Count];
            for (int i = 0; i < modelVariableIndices.Count; i++)
            {
                char[] delimiterChars = { ',' };
                string[] words = exchangeItemId[i].Split(delimiterChars);
                string openMIVariableID = words[0].Trim();


                ITimeSpaceOutput output = FindOutputItem(openMIVariableID);
                var modVars = output.Values.Values2D[0].Cast<double>().ToArray();
                modelValues[i] = modVars[modelVariableIndices[i]];
            }
            return modelValues;
        }

        /// <summary>
        /// Get the values from the model at the given indices
        /// </summary>
        /// <param name="exchangeItemId">The exchange id of the variable to retrived</param>
        /// <param name="modelVariableIndices">List of indices in the model to retrive values from.</param>
        /// <returns>Corresponding model values.</returns>
        private double[] GetModelValuesSameVariable(string exchangeItemId, IList<int> modelVariableIndices)
        {
            char[] delimiterChars = { ',' };
            string[] words = exchangeItemId.Split(delimiterChars);
            string openMIVariableID = words[0].Trim();

            double[] modelValues = new double[modelVariableIndices.Count];

            //Get values from model
            ITimeSpaceOutput output = FindOutputItem(openMIVariableID);
            var modVars = output.Values.Values2D[0].Cast<double>().ToArray();

            for (int i = 0; i < modelVariableIndices.Count; i++)
            {
                modelValues[i] = modVars[modelVariableIndices[i]];
            }
            return modelValues;
        }

        
        private double GetModelValue(string exchangeItemId, int modelVariableIndex)
        {
            char[] delimiterChars = { ',' };
            string[] words = exchangeItemId.Split(delimiterChars);
            string openMIVariableID = words[0].Trim();


            ITimeSpaceOutput output = FindOutputItem(openMIVariableID);
            return output.Values.Values2D[0].Cast<double>().ToArray()[modelVariableIndex]; 
        }

        private ITimeSpaceOutput FindOutputItem(string outputItemId)
        {
            foreach (ITimeSpaceOutput outputItem in base.Outputs)
            {
                if (outputItem.Id.Equals(outputItemId))
                {
                    return outputItem;
                }
            }
            throw new Exception("Output item \"" + outputItemId + "not found.");
        }

        /// <summary>
        /// BASEGRID
        /// Returns a localization hash table.
        /// For each observation point, calculate the gaussian distance to all other points in the model grid.
        /// </summary>
        /// <param name="exchangeItemId">The exchange ID string</param>
        /// <param name="observationCount">The number of observation points</param>
        /// <param name="locDistance">The localization distance (for Gaussian distance)</param>
        /// <param name="xpos">array of observation X coordinates</param>
        /// <param name="ypos">array of observation Y coordinates</param>
        /// <param name="layer">array of observation Z heights (layer in the model)</param>
        /// <returns></returns>
        private double[][] GetLocalized3D(string exchangeItemId, int observationCount, double locDistance, double[] xpos, double[] ypos, int[] layer)
        {
            double[][] localized2D = new double[observationCount][];

            var mshe = base.WMEngine;

            // SZGrid
            int n = mshe.Grid.ElementCount;

            IDictionary<int, ISpatialDefine> modelCoord = GetModelCoordinates(exchangeItemId);

            for (int obsC = 0; obsC < observationCount; obsC++)
            {
                localized2D[obsC] = new double[n];
                IXYLayerPoint obsPoint = new XYLayerPoint(xpos[obsC], ypos[obsC], 0);
                if (XYZGeometryTools.IsPointInModelPlain(obsPoint, modelCoord))
                {
                    for (int i = 0; i < modelCoord.Count; i++)
                    {
                        if (Convert.ToInt32(obsPoint.Layer) == modelCoord[i].Layer)
                        {
                            double distanceC = XYZGeometryTools.CalculatePointToPointDistance2D(modelCoord[i].MidPoint, obsPoint);
                            localized2D[obsC][i] = normalCooefs(distanceC, locDistance);
                        }
                    }
                }
            }
            return localized2D;
        }

        /// <summary>
        /// BASEGRID
        /// Returns a localization hash table.
        /// For each observation point, calculate the gaussian distance to all other points in the model grid.
        /// </summary>
        /// <param name="exchangeItemId">The exchange ID string</param>
        /// <param name="observationCount">The number of observation points</param>
        /// <param name="locDistance">The localization distance (for Gaussian distance)</param>
        /// <param name="xpos">array of observation X coordinates</param>
        /// <param name="ypos">array of observation Y coordinates</param>
        /// <returns></returns>
        private double[][] GetLocalized2D(string exchangeItemId, int observationCount, double locDistance, double[] xpos, double[] ypos)
        {
            double[][] localized2D = new double[observationCount][];

            var mshe = base.WMEngine;
            // BASEGRID
            int n = mshe.Grid.ElementCount;
           
            IDictionary<int, ISpatialDefine> modelCoord = GetModelCoordinates(exchangeItemId);

            for (int obsC = 0; obsC < observationCount; obsC++)
            {
                localized2D[obsC] = new double[n];
                IXYLayerPoint obsPoint = new XYLayerPoint(xpos[obsC], ypos[obsC], 0);
                if (XYZGeometryTools.IsPointInModelPlain(obsPoint, modelCoord))
                {
                    for (int i = 0; i < modelCoord.Count; i++)
                    {
                        if (Convert.ToInt32(obsPoint.Layer) == modelCoord[i].Layer)
                        {
                            double distanceC = XYZGeometryTools.CalculatePointToPointDistance2D(modelCoord[i].MidPoint, obsPoint);
                            localized2D[obsC][i] = normalCooefs(distanceC, locDistance);
                        }
                    }
                }
            }
            return localized2D;
        }

        /// <summary>
        /// Returns the grid type based on the exchangeID. The exchange ID is a string delimiated by a comma.
        /// The second string after the comma contains the information of the Grid.
        /// Options include:
        /// 1) SZ3DGrid used for hydraulic head
        /// 2) BaseGrid used for land surface variabels (surface temperature, evapotranspiration, ...)
        /// 3) WMUZ3DGrid used for soil moisture content (in the UZ)
        /// </summary>
        /// <param name="exchangeItemId">string deliminated by a comma where the string after the comma contains the grid information.</param>
        /// <returns>The geometry type of the variable. This is MikeSHE specific</returns>
        GeometryTypes GetGridType(string exchangeItemId)
        {
            IBaseOutput baseOut = base._outputExchangeItems.First(vID => string.Compare(vID.Id, exchangeItemId) == 0);

            char[] delimiterChars = { ',' };
            string[] words = baseOut.Description.Split(delimiterChars);
            string gridTypewords = words[1].Trim();

            if (string.Compare(gridTypewords, "SZ3DGrid", 0) == 0)
            {
                return GeometryTypes.Geometry3D;
            }
            else if (string.Compare(gridTypewords, "BaseGrid", 0) == 0)
            {
                return GeometryTypes.Geometry2D;
            }
            else if (string.Compare(gridTypewords, "WMUZ3DGrid", 0) == 0)
            {
                return GeometryTypes.Geometry3DUZ;
            }
            else
            {
                throw new Exception("Other types do exisit (UZ...)");
            }
        }


        /// <summary>
        /// 2D BaseGrid !!
        /// Creates a dictionary with key equal to the model state index and the value the spatial information of that state index.
        /// </summary>
        /// <param name="gType">The geometric type of the exchange itme (2d or 3d)</param>
        /// <param name="baseOut">The exchange item base output</param>
        /// <param name="elementID">the string id of the exchange item.</param>
        /// <returns></returns>
        private IDictionary<int, ISpatialDefine> GetModelCoordinates2D(GeometryTypes gType, IBaseOutput baseOut, string elementID)
        {
            IDictionary<int, ISpatialDefine> modelEntities = new Dictionary<int, ISpatialDefine>();
            int elementIDNumber;
            int n;

            try
            {
                elementIDNumber = WMEngine.GetElementCount(elementID);
                n = baseOut.ElementSet().ElementCount;
            }
            catch
            {
                Console.WriteLine("\nElement {0} does not found in the model\n", elementID);
                throw new Exception("\nProblem in Model Instance - unable to find exchange item\n");
            }

            for (int i = 0; i < n; i++)
            {
                XYPolygon modelpolygon = ElementMapper.CreateXYPolygon(baseOut.ElementSet(), i);
                //int zLayer = Convert.ToInt32(i % base.WMEngine.NumberOfSZLayers);

                // Points in Polygon are defined as LL, LR, UR, UL  (l/l = lower/left, u = upper, r = right )
                // Finds the mid x and mid y point in the polygon (assuming rectangular grid)
                IXYLayerPoint min = new XYLayerPoint(modelpolygon.GetX(0), modelpolygon.GetY(0), 0);
                IXYLayerPoint max = new XYLayerPoint(modelpolygon.GetX(1), modelpolygon.GetY(3), 0);

                modelEntities.Add(i, new SpatialDefine(min, max, GeometryTypes.Geometry2D));
            }

            return modelEntities;
        }

        /// <summary>
        /// 3D UZ !!!!
        /// Creates a dictionary with key equal to the model state index and the value the spatial information of that state index.
        /// </summary>
        /// <param name="gType">The geometric type of the exchange itme (2d or 3d)</param>
        /// <param name="baseOut">The exchange item base output</param>
        /// <param name="elementID">the string id of the exchange item.</param>
        /// <returns></returns>
        private IDictionary<int, ISpatialDefine> GetModelCoordinates3DUZ(GeometryTypes gType, IBaseOutput baseOut, string elementID)
        {
            IDictionary<int, ISpatialDefine> modelEntities = new Dictionary<int, ISpatialDefine>();
            int n;

            try
            {
                WMEngine.GetElementCount(elementID);
                n = baseOut.ElementSet().ElementCount;
            }
            catch
            {
                Console.WriteLine("\nElement {0} does not found in the model\n", elementID);
                throw new Exception("\nProblem in Model Instance - unable to find exchange item\n");
            }

            // Determines the number of layers in the UZ Grid.
            int numLayersInGrid = Convert.ToInt32( Math.Round( (double)base.WMEngine.UzGrid.ElementCount/(double)base.WMEngine.UzGrid.BaseGrid.ElementCount ) );

            for (int i = 0; i < n; i++)
            {
                XYPolygon modelpolygon = ElementMapper.CreateXYPolygon(baseOut.ElementSet(), i);
                int zLayer = Convert.ToInt32(i % numLayersInGrid);

                // Points in Polygon are defined as LL, LR, UR, UL  (l/l = lower/left, u = upper, r = right )
                // Finds the mid x and mid y point in the polygon (assuming rectangular grid)
                IXYLayerPoint min = new XYLayerPoint(modelpolygon.GetX(0), modelpolygon.GetY(0), zLayer);
                IXYLayerPoint max = new XYLayerPoint(modelpolygon.GetX(1), modelpolygon.GetY(3), zLayer);

                modelEntities.Add(i, new SpatialDefine(min, max, GeometryTypes.Geometry3D));
            }

            return modelEntities;
        }

        private IDictionary<int, ISpatialDefine> _modelEntities;

        /// <summary>
        /// 3D SZ !!!!
        /// Creates a dictionary with key equal to the model state index and the value the spatial information of that state index.
        /// </summary>
        /// <param name="gType">The geometric type of the exchange itme (2d or 3d)</param>
        /// <param name="baseOut">The exchange item base output</param>
        /// <param name="elementID">the string id of the exchange item.</param>
        /// <returns></returns>
        private IDictionary<int, ISpatialDefine> GetModelCoordinates3DSZ(GeometryTypes gType, IBaseOutput baseOut, string elementID)
        {
            //Run Only once - because it's slow
            if(_modelEntities == null)
            {

                _modelEntities = new Dictionary<int, ISpatialDefine>();
                int n;

                try
                {
                    WMEngine.GetElementCount(elementID);
                    n = baseOut.ElementSet().ElementCount;
                }
                catch
                {
                    Console.WriteLine("\nElement {0} does not found in the model\n", elementID);
                    throw new Exception("\nProblem in Model Instance - unable to find exchange item\n");
                }

                //int numBaseGrid = Convert.ToInt32(Math.Floor((double)n / (double)_mshe.WMEngine.NumberOfSZLayers));

                for (int i = 0; i < n; i++)
                {
                    XYPolygon modelpolygon = ElementMapper.CreateXYPolygon(baseOut.ElementSet(), i);
                    int zLayer = Convert.ToInt32(i % base.WMEngine.NumberOfSZLayers);

                    // Points in Polygon are defined as LL, LR, UR, UL  (l/l = lower/left, u = upper, r = right )
                    // Finds the mid x and mid y point in the polygon (assuming rectangular grid)
                    IXYLayerPoint min = new XYLayerPoint(modelpolygon.GetX(0), modelpolygon.GetY(0), zLayer);
                    IXYLayerPoint max = new XYLayerPoint(modelpolygon.GetX(1), modelpolygon.GetY(3), zLayer);

                    _modelEntities.Add(i, new SpatialDefine(min, max, GeometryTypes.Geometry3D));
                }

            }
            return _modelEntities;
        }

        /// <summary>
        /// Returns a List of ISpatialDefine with assumed 90deg angles. 
        /// The ISpatialDefine is defined by two coordinates and a layer integer.
        /// A ISpatialDefine can represent a cuboid, rectangle or a point. 
        /// </summary>
        /// <param name="elementID"></param>
        /// <returns></returns>
        private IDictionary<int, ISpatialDefine> GetModelCoordinates(string elementIDAndGrid)
        {
            char[] delimiterChars = { ',' };
            string[] words = elementIDAndGrid.Split(delimiterChars);
            string elementID = words[0].Trim();

            IBaseOutput baseOut = base._outputExchangeItems.First(vID => string.Compare(vID.Id, elementID) == 0);

            // Get the Grid Type from the elementID (delimiated by a ',');
            GeometryTypes gType = GetGridType(elementID);

            if (gType == GeometryTypes.Geometry3D)
            {
                return GetModelCoordinates3DSZ(gType, baseOut, elementID);
            }
            if (gType == GeometryTypes.Geometry3DUZ)
            {
                return GetModelCoordinates3DUZ(gType, baseOut, elementID);
            }
            if (gType == GeometryTypes.Geometry2D)
            {
                return GetModelCoordinates2D(gType, baseOut, elementID);
            }
            else
            {
                throw new Exception("The Rest not Implemented");
            }

        }

        /// <summary>
        /// Distance to Normal calculator.
        /// Returns the Gaussian normalized distances.
        /// </summary>
        /// <param name="dist"> array of doubles of distances to each other </param>
        /// <param name="radius"> radius factor </param>
        /// <returns></returns>
        private double normalCooefs(double dist, double radius)
        {
            // Calculated result saved into iteself
            return Math.Exp(-0.5 * Math.Pow((dist / radius), 2));
        }


        #endregion PrivateMethods

    }
}



/*

        /// <summary>
        ///  Calculates Gaussian Localization Mask.
        /// Given a point on a grid, return an array of doubles (of the same size as the grid) with localization values between 0 and 1.
        /// </summary>
        /// <param name="modelInstance">A model instance from WMengine</param>
        /// <param name="elementSetID">Exchange item string id</param>
        /// <param name="point">point around which to calcualte the licalization mask</param>
        /// <param name="locDistance">the distance radius of the Gaussian mask</param>
        /// <returns></returns>
        private double[] GaussianLocalization(DHI.OpenMI2.MikeShe.WMEngineAccess modelInstance, string elementSetID, IXYLayerPoint point, double locDistance)
        {
            IDictionary<int, ISpatialDefine> modelCoord = GetModelCoordinates(elementSetID);

            double[] localized2D = new double[modelCoord.Count];

            if (XYZGeometryTools.IsPointInModelPlain(point, modelCoord))
            {
                for (int i = 0; i < modelCoord.Count; i++)
                {
                    if (Convert.ToInt32(point.Layer) == modelCoord[i].Layer)
                    {
                        double distance = XYZGeometryTools.CalculatePointToPointDistance2D(modelCoord[i].MidPoint, point);
                        localized2D[i] = normalCooefs(distance, locDistance);
                    }
                }
            }
            return localized2D;
        }


*/