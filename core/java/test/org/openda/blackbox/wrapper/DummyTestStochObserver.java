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
package org.openda.blackbox.wrapper;

import org.openda.interfaces.*;
import org.openda.utils.IdentitySelector;
import org.openda.utils.Instance;
import org.openda.exchange.ExchangeItem;
import java.io.File;
import java.util.List;
import java.util.ArrayList;

/**
 * Dummy observer to test black box wrapper inside openda application
 */
public class DummyTestStochObserver extends Instance implements IStochObserver {

    private String[] arguments = new String[]{};

    public void initialize(File workingDir, String[] arguments) {
        this.arguments = arguments;
    }

    public IObservationDescriptions getObservationDescriptions() {
        return new DummyTestObservationDescriptions(arguments);
    }

    public IStochObserver createSelection(String selection) {
        return this;
    }

    public IStochObserver createSelection(ITime selectionTimes) {
        return this;
    }

	public IStochObserver createSelection(Type observationType) {
		if (observationType == Type.Assimilation) {
			return this;
		}
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.createSelection(): Not implemented yet.");
	}

    public ISelector createSelector(Type observationType) {
        return new IdentitySelector();
    }

    public int getCount() {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.getCount(): Not implemented yet.");
    }

    public IVector getValues() {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.getValues(): Not implemented yet.");
    }

    public IVector getRealizations() {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.getRealizations(): Not implemented yet.");
    }

    public IVector getExpectations() {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.getExpectations(): Not implemented yet.");
    }

    public double evaluatePDF(IVector values) {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.evaluatePDF(): Not implemented yet.");
    }

    public IVector evaluateMarginalPDFs(IVector values) {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.evaluateMarginalPDFs(): Not implemented yet.");
    }

    public ISqrtCovariance getSqrtCovariance() {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.getSqrtCovariance(): Not implemented yet.");
    }

    public IVector getStandardDeviations() {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.getStandardDeviations(): Not implemented yet.");
    }

    public ITime[] getTimes() {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.getTimes(): Not implemented yet.");
    }

    public void free() {
    }

    private class DummyTestObservationDescriptions implements IObservationDescriptions {

        private List<IPrevExchangeItem> exchangeItems = new ArrayList<IPrevExchangeItem>();

        public DummyTestObservationDescriptions(String[] arguments) {
            for (String argument : arguments) {
                String[] ids = argument.trim().split(";");
                for (String id : ids) {
                    exchangeItems.add(new DummyTestExchangeItem(id));
                }
            }
        }

        public List<IPrevExchangeItem> getExchangeItems() {
            return exchangeItems;
        }

        public IVector getValueProperties(String Key) {
            throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.DummyTestObservationDescriptions.getValueProperties(): Not implemented yet.");
        }

        public String[] getStringProperties(String Key) {
            return arguments;
        }

        public String[] getPropertyKeys() {
            throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.DummyTestObservationDescriptions.getPropertyKeys(): Not implemented yet.");
        }

        public int getPropertyCount() {
            throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.DummyTestObservationDescriptions.getPropertyCount(): Not implemented yet.");
        }

        public int getObservationCount() {
            throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.DummyTestObservationDescriptions.getObservationCount(): Not implemented yet.");
        }

		public ITime[] getTimes() {
			return null;
		}

		private class DummyTestExchangeItem extends ExchangeItem implements IPrevExchangeItem {

            public DummyTestExchangeItem(String argument) {
                super(argument);
            }

            public Role getRole() {
                return Role.InOut;
            }

            public Class getValueType() {
                throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.DummyTestObservationDescriptions.DummyTestExchangeItem.getValueType(): Not implemented yet.");
            }

            public ValueType getValuesType() {
                throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.DummyTestObservationDescriptions.DummyTestExchangeItem.getValuesType(): Not implemented yet.");
			}

            public Object getValues() {
                throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.DummyTestObservationDescriptions.DummyTestExchangeItem.getValues(): Not implemented yet.");
            }

            public double[] getValuesAsDoubles() {
                throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.DummyTestObservationDescriptions.DummyTestExchangeItem.getValuesAsDoubles(): Not implemented yet.");
            }

            public void axpyOnValues(double alpha, double[] axpyValues) {
                throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.DummyTestObservationDescriptions.DummyTestExchangeItem.axpyOnValues(): Not implemented yet.");
            }

			public void multiplyValues(double[] multiplicationFactors) {
				throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.DummyTestObservationDescriptions.DummyTestExchangeItem.multiplyValues(): Not implemented yet.");
			}

			public void setValues(Object values) {
                throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.DummyTestObservationDescriptions.DummyTestExchangeItem.setValues(): Not implemented yet.");
            }

            public void setValuesAsDoubles(double[] values) {
                throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestStochObserver.DummyTestObservationDescriptions.DummyTestExchangeItem.setValuesAsDoubles(): Not implemented yet.");
            }

            public double[] getTimes() {
                return null;
            }

            public void setTimes(double[] times) {
                throw new RuntimeException(this.getClass().getName() + " does not have timeStamps");
            }

        }
    }
}
