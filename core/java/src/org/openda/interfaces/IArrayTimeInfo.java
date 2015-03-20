package org.openda.interfaces;

public interface IArrayTimeInfo extends ITimeInfo{

	/**
	 * Provide times as an IArray. See IArrayExchangeItem for details.
	 * @return latitude
	 */
	public IArray getTimeArray();

	/**
	 * Pointer or pointers to time index in values array.
	 * See IArrayExchangeItem for details. In most cases this method should return "new int[]{0}"
	 * @return pointers to array dimension(s)
	 */
	public int[] getTimeValueIndices();

}
