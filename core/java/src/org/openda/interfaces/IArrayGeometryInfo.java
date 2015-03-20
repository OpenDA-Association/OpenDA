package org.openda.interfaces;

public interface IArrayGeometryInfo extends IGeometryInfo{

	/**
	 * Degrees north of the equator. 
	 * Horizontal coordinates are preferably in WGS84.
	 * The latitude grid may form a multidimensional array.
	 * @return latitude
	 */
	public IArray getLatitudeArray();

	/**
	 * Degrees east of Greenwich 0 meridian. 
	 * Horizontal coordinates are preferably in WGS84.
	 * The latitude grid may form a multidimensional array.
	 * @return longitude
	 */
	public IArray getLongitudeArray();

	/**
	 * Height above reference plane. Preferably positive up in meters.
	 * @return height
	 */
	public IArray getHeightArray();

	/**
	 * Pointer or pointers to latitude index in values array.
	 * See IArrayExchangeItem for details
	 * @return pointers to array dimension(s)
	 */
	public int[] getLatitudeValueIndices();

	/**
	 * Pointer or pointers to longitude index in values array.
	 * See IArrayExchangeItem for details
	 * @return pointers to array dimension(s)
	 */
	public int[] getLongitudeValueIndices();

	/**
	 * Pointer or pointers to height index in values array.
	 * See IArrayExchangeItem for details
	 * @return pointers to array dimension(s)
	 */
	public int[] getHeightValueIndices();

	/**
	 * Returns information about the latitude coordinate values.
	 *
	 * @return quantity info
	 */
	public IQuantityInfo getLatitudeQuantityInfo();

	/**
	 * Returns information about the longitude coordinate values.
	 *
	 * @return quantity info
	 */
	public IQuantityInfo getLongitudeQuantityInfo();

	/**
	 * Returns information about the height coordinate values.
	 *
	 * @return quantity info
	 */
	public IQuantityInfo getHeightQuantityInfo();

	/**
	 * Returns information about the active cells.
	 *
	 * @return mask with 1 and 0 values (1 means active).
	 */
	public int[] getActiveCellMask();
}
