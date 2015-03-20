package org.openda.model_delft3d;

/**
 * TODO: description
*/
class MnPoint {

    int m;
    int n;

    public MnPoint(String mString, String nString) {
        m = Integer.parseInt(mString);
        n = Integer.parseInt(nString);
    }

    public MnPoint(int m, int n) {
        this.m = m;
        this.n = n;
    }

    public int getM() {
        return m;
    }

    public int getN() {
        return n;
    }
}
