package org.openda.blackbox.interfaces;

import org.openda.interfaces.ITime;

/**
 * Optional consumer interface that can be implemented by a (stoch)ModelFactory if it
 * needs the timeHorizon to be set by OpenDA. The setTimeHorizon is called
 * after the initialize method of the (stoch)ModelFactory has been called.
 */
public interface ITimeHorizonConsumer {
	public void setTimeHorizon(ITime timeHorizon);
}
