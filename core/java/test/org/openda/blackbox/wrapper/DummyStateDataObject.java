package org.openda.blackbox.wrapper;

import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.DoublesExchangeItem;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;

import java.io.File;

/**
 * Created by pelgrim on 28-Oct-16.
 */
public class DummyStateDataObject implements IDataObject {
    private static int stateSize;

    @Override
    public String[] getExchangeItemIDs() {
        return new String[]{"state", "StartTime", "StopTime", "TimeStep"};
    }

    @Override
    public String[] getExchangeItemIDs(IExchangeItem.Role role) {
        throw new RuntimeException("org.openda.blackbox.wrapper.DummyStateDataObject.getExchangeItemIDs() not implemented yet");

    }

    @Override
    public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
        switch (exchangeItemID) {
            case "state" :
                //double[] values = new double[1061];
                double[] values = new double[stateSize];
                fillWithRandomValues(values);
                return new DoublesExchangeItem("state", IExchangeItem.Role.InOut, values);
            case "StartTime" :
                return new DoubleExchangeItem("StartTime", IExchangeItem.Role.InOut, 0);
            case "StopTime" :
                return new DoubleExchangeItem("StopTime", IExchangeItem.Role.InOut, 10);
            case "TimeStep" :
                return new DoubleExchangeItem("TimeStep", IExchangeItem.Role.InOut, 1);
            default:
                return null;
        }
    }

    private void fillWithRandomValues(double[] values) {
        for (int i = 0; i < values.length; i++) {
			values[i] = Math.random();
		}
    }

    @Override
    public void finish() {
    }

    @Override
    public void initialize(File workingDir, String[] arguments) {

    }

    public static void setStateSize(int size) {
        stateSize = size;
    }
}
