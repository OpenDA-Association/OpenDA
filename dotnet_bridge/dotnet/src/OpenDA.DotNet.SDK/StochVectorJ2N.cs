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


using System;
using OpenDA.DotNet.Interfaces;
using OpenDA.DotNet.Bridge;

namespace OpenDA.DotNet.SDK
{
    class StochVectorJ2N : IStochVector
    {
        #region IStochVector Members

        private org.openda.interfaces.IStochVector _javaStochVector;

        public StochVectorJ2N(double[] mean, double[] stdDevs)
        {
            _javaStochVector = new org.openda.utils.StochVector(mean, stdDevs);
        }

        public IVector CreateRealization()
        {
            org.openda.interfaces.IVector javaVector = _javaStochVector.createRealization();
            return new VectorJ2N(javaVector.getValues());
        }

        public double EvaluatePdf(IVector vector)
        {
            throw new NotImplementedException();
        }

        public IVector Expectations
        {
            get
            {
                org.openda.interfaces.IVector javaVector = _javaStochVector.getExpectations();
				return new VectorJ2N(javaVector.getValues());
            }
        }

        public ISqrtCovariance SqrtCovariance
        {
            get { throw new NotImplementedException(); }
        }

        public bool HasCorrelatedElements()
        {
            return _javaStochVector.hasCorrelatedElements();
        }

        public IVector StandardDeviations
        {
            get
            {
                org.openda.interfaces.IVector javaVector = _javaStochVector.getStandardDeviations();
				return new VectorJ2N(javaVector.getValues());
            }
        }

        #endregion
    }
}
