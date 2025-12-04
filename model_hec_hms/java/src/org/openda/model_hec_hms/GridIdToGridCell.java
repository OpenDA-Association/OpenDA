package org.openda.model_hec_hms;

public class GridIdToGridCell {
	private final String gridId;
	private final GridCell gridCell;

	public GridIdToGridCell(String gridId, GridCell gridCell) {
		this.gridId = gridId;
		this.gridCell = gridCell;
	}

	public String getGridId() {
		return gridId;
	}

	public GridCell getGridCell() {
		return gridCell;
	}
}
