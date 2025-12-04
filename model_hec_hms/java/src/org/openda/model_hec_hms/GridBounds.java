package org.openda.model_hec_hms;

public class GridBounds {
	private int minimumX = Integer.MAX_VALUE;
	private int minimumY = Integer.MAX_VALUE;
	private int maximumX = Integer.MIN_VALUE;
	private int maximumY = Integer.MIN_VALUE;

	public void addCell(int x, int y) {
		minimumX = Math.min(minimumX, x);
		minimumY = Math.min(minimumY, y);
		maximumX = Math.max(maximumX, x);
		maximumY = Math.max(maximumY, y);
	}

	public int getX() {
		return minimumX;
	}

	public int getY() {
		return minimumY;
	}

	public int getWidth() {
		return maximumX + 1 - minimumX;
	}

	public int getHeight() {
		return maximumY + 1 - minimumY;
	}

	public int getSize() {
		return getWidth() * getHeight();
	}

	public int getIndex(GridCell gridCell) {
		int x = gridCell.getX() - minimumX;
		int y = gridCell.getY() - minimumY;

		return x * getHeight() + y;
	}
}
