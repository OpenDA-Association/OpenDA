package org.openda.model_openfoam;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.openda.interfaces.*;
import org.openda.utils.Array;

/**
 * Represents an unstructured mesh geometry, in a space of multiple dimensions.
 * 
 * The mesh nodes are stored internally as point coordinates from the
 * space in which the mesh is defined.
 * 
 * For example, space = {(x,y,z) element of R^3} 
 * 
 * @author Werner Kramer
 */
public class UnstructuredMeshGeometryInfo implements IGeometryInfo {
    
    private List<IQuantityInfo> coordinates = null;
    private IArray points = null;


	/**
	 * Empty constructor
	 */
    public UnstructuredMeshGeometryInfo()
    {
    }

	/**
	 * Designated constructor
	 * @param coordinates array with dimensions of unstructured mesh
	 * @param points     array with points of unstructured mesh
	 */
	public UnstructuredMeshGeometryInfo(IQuantityInfo[] coordinates, IArray points)
	{
		if (coordinates.length != points.getDimensions()[1] ) {
			throw new RuntimeException(String.format("Number of coordinates %d does not agree with the dimensions of the points array %d.",coordinates.length, points.getNumberOfDimensions() ));
		}
		this.coordinates = new ArrayList<IQuantityInfo>(Arrays.asList(coordinates));
		this.points = new Array(points);
	}

	/**
	 * Convenience constructor
	 * @param coordinates array with dimensions of unstructured mesh
	 */
	public UnstructuredMeshGeometryInfo(IQuantityInfo[] coordinates)
	{
		this(coordinates, new Array( new int[] {0, coordinates.length }) );
	}

	/**
	 * Copy constructor - create a copy of and UnstructeredMeshGeometryInfo object
	 * @param source source UnstructeredMeshGeometryInfo object
	 */
    public UnstructuredMeshGeometryInfo(UnstructuredMeshGeometryInfo source) {
		this.coordinates = source.coordinates;
		this.points = source.points;
	}

    
    public IQuantityInfo[] getCoordinates() {
        return coordinates.toArray(new IQuantityInfo[coordinates.size()]);
    }
    
    public void setCoordinates(IQuantityInfo[] source) {
		coordinates = new ArrayList<IQuantityInfo> (Arrays.asList(source));
    }

    //public int getSize() {
    //    return size();
    //}

    //private int size() {
    //    return meshPoints.getSize() / dimensions.size();
    //}

    //public int dimension() {
    //    return dimensions.size();
    //}

	/**
	 * Getter method for
	 * @return points
	 */
    public IArray getPoints() {
        return points;
    }

	/**
	 * Set the points from an IArray object. The dimensions of the object needs to match the number of coordinates
	 * @param points
	 */
	public void setPoints(IArray points) {
		if (this.coordinates.size() != points.getDimensions()[1]) {
			throw new RuntimeException(String.format("Number of coordinates %d does not agree with the dimensions of the points array %d.", this.coordinates.size(), points.getNumberOfDimensions()));
		}
		this.points = points;
	}

}
