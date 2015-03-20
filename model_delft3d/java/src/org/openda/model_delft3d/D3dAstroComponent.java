package org.openda.model_delft3d;

import java.util.Locale;

/**
 * Astro component, or its correction
 */
public class D3dAstroComponent {

    private String id;
    private double phase;
    private double amplitude;
    private boolean dataChanged;
    private D3dAstroComponent correction;

    public D3dAstroComponent(String id, double amplitude, double phase) {
        this.id = id;
        this.amplitude = amplitude;
        this.phase = phase;
        this.correction = null;
        this.dataChanged = false;
     }

    public D3dAstroComponent(String id, String amplitudeString, String phaseString) {
        this(id, Double.parseDouble(amplitudeString), Double.parseDouble(phaseString));
    }

    public double getPhase() {
        if (correction != null) {
            return phase + correction.getPhase();
        } else {
            return phase;
        }
    }

    public double getAmplitude() {

        if (correction != null) {
            return amplitude * correction.getAmplitude();
        } else {
            return amplitude;
        }
    }

    public void setPhase(double phase) {
        if (correction != null) {
            correction.setPhase(phase - this.phase);
        } else {
            this.phase = phase;
        }
        this.dataChanged = true;
    }

    public void setAmplitude(double amplitude) {
        if (correction != null) {
            correction.setAmplitude(amplitude / this.amplitude);
        } else {
            this.amplitude = amplitude;
        }
        this.dataChanged = true;
    }

    public String getId() {
        return id;
    }

    public boolean getDataChanged() {
        return dataChanged;
    }

    public void setCorrection(D3dAstroComponent correction) {
        this.correction = correction;
    }

    @Override
    public String toString() {
        Locale locale = new Locale("EN");
        String stringFormat = "%-6s";
        String floatValueFormat = "%12.3f";
        return String.format(stringFormat, id) + String.format(locale, floatValueFormat, amplitude)
                + String.format(locale, floatValueFormat, phase);
    }
}
