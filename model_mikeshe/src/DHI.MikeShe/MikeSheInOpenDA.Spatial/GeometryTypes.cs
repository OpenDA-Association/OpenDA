namespace MikeSheInOpenDA.Spatial
{
    public enum GeometryTypes
    {
        /// <summary>
        /// For dfs3 SZ and UZ
        /// </summary>
        Geometry3D,     

        /// <summary>
        /// For dfs2 BaseGrid
        /// </summary>
        Geometry2D,   

        /// <summary>
        /// For dfs0 - Point based Time series
        /// </summary>
        GeometryPoint,

        /// <summary>
        /// For 3D UZ grid (such as used for soil moisture content)
        /// </summary>
        Geometry3DUZ
    }
}
