/* MOD_V2.0
* Copyright (c) 2012 OpenDA Association
* All rights reserved.
*
* This file is part of OpenDA.
*
* OpenDA is free software: you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as
* published by the Free Software Foundation, either version 3 of
* the License, or (at your option) any later version.
*
* OpenDA is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public License
* along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
*/

package org.openda.costa;

import org.openda.interfaces.*;
import java.util.List;


public class CtaObservationDescriptions extends CtaObject implements IObservationDescriptions {

    public CtaObservationDescriptions() {
    }

    // create a native observation discription that is actually just an interface to a java
    // observation description instance
    public CtaObservationDescriptions(IObservationDescriptions javaImpl){
       this.ctaHandle = ctaCreateNativeToJavaObserver(javaImpl);
    }


    private CtaObservationDescriptions(int ctaHandle) {
        this.ctaHandle = ctaHandle;
    }

    public List<IPrevExchangeItem> getExchangeItems() {
        throw new UnsupportedOperationException("org.costa.CtaObservationDescriptions.getExchangeItems(): Not implemented yet.");
    }

    public native String[] getPropertyKeys();


    public IObservationDescriptions createSelection(String selection, IRelationTable reltab) {

        if (reltab != null) {
            throw new UnsupportedOperationException("org.costa.ctaObservationDescriptions.createSelection setting of relation table is not implemented yet.");
        }

        int handle = ctaCreateSelection(selection);


        return new CtaObservationDescriptions(handle);

    }

    public IObservationDescriptions createSelection(String selection) {
        throw new UnsupportedOperationException("org.costa.CtaObservationDescriptions.createSelection(): Not implemented yet.");
    }

    public IObservationDescriptions createSelection(ITime timsel) {
        throw new UnsupportedOperationException("org.costa.CtaObservationDescriptions.createDescriptionSelection(): Not implemented yet.");
    }

    private native int ctaCreateSelection(String selection);

    public IObservationDescriptions createSelection(ITime timeSel, IRelationTable reltab) {

        if (reltab != null) {
            throw new UnsupportedOperationException("org.costa.ctaObservationDescriptions.createSelection setting of relation table is not implemented yet.");
        }

        if (timeSel instanceof CtaTime) {
            int ctaTimeSel = ((CtaTime) timeSel).ctaHandle;
            int handle = ctaCreateTimeSelection(ctaTimeSel);
            return new CtaObservationDescriptions(handle);

        } else {
            throw new UnsupportedOperationException("org.costa.ctaObservationDescriptions.createSelection not implemented yet for other types of ITime");
        }
    }

    private native int ctaCreateTimeSelection(int ctaTimeSel);

    public native int getObservationCount();

	public ITime[] getTimes() {
		throw new UnsupportedOperationException("org.openda.costa.CtaObservationDescriptions.getTimes(): Not implemented yet.");
	}

	public native int getPropertyCount();

    public native String[] getStringProperties(String Key);

    public IVector getValueProperties(String Key) {
        int n = this.getObservationCount();
        CtaVector properties = new CtaVector(n);
        ctaGetValueProperties(Key, properties.ctaHandle);
        return properties;
    }

    private  native int ctaCreateNativeToJavaObserver(IObservationDescriptions javaImpl);

    private native void ctaGetValueProperties(String key,
                                              int ctaProperties);

}
