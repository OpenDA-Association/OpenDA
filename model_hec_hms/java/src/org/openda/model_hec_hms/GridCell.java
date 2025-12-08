package org.openda.model_hec_hms;

import java.util.Objects;

public class GridCell {
	private final int x;
	private final int y;

	public GridCell(int x, int y) {
		this.x = x;
		this.y = y;
	}

	public int getX() {
		return x;
	}

	public int getY() {
		return y;
	}

	@Override
	public boolean equals(Object other) {
		if (!(other instanceof GridCell)) {
			return false;
		}

		GridCell otherGridCell = (GridCell) other;

		return x == otherGridCell.x && y == otherGridCell.y;
	}

	@Override
	public int hashCode() {
		return Objects.hash(x, y);
	}
}
