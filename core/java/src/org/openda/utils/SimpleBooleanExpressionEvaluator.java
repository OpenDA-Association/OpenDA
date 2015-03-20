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

package org.openda.utils;

import java.util.ArrayList;

/**
 * Simple evaluator for boolean expressions
 */
public class SimpleBooleanExpressionEvaluator {

    private SimpleBooleanExpressionEvaluator[] orChildren = null;
    private SimpleBooleanExpressionEvaluator[] andChildren = null;
    private PrimitiveBoolExpression primitive = null;
    String[] involvedParameters = null;

    /**
     * Constructor for the evaluator.
     * @param selectionString The string to be evaluated.
     */
    public SimpleBooleanExpressionEvaluator(String selectionString) {
        ArrayList<String> parameters = new ArrayList<String>();
        selectionString = selectionString.trim();
        String[] substrings = selectionString.split(" [Oo][Rr] ");
        if (substrings.length > 1) {
            orChildren = new SimpleBooleanExpressionEvaluator[substrings.length];
            for (int i = 0; i < substrings.length; i++) {
                orChildren[i] = new SimpleBooleanExpressionEvaluator(substrings[i]);
            }
        } else {
            substrings = selectionString.split(" [Aa][Nn][Dd] ");
            if (substrings.length > 1) {
                andChildren = new SimpleBooleanExpressionEvaluator[substrings.length];
                for (int i = 0; i < substrings.length; i++) {
                    andChildren[i] = new SimpleBooleanExpressionEvaluator(substrings[i]);
                }
            } else {
                primitive = new PrimitiveBoolExpression(substrings[0]);
                if (!parameters.contains(primitive.parameterID)) parameters.add(primitive.parameterID);
            }
        }
        involvedParameters = parameters.toArray(new String[parameters.size()]);
    }

    /**
     * Get the parameters that are present in the expression to be evaluated.
     * @return The involved parameters.
     */
    public String[] getInvolvedParameters() {
        return involvedParameters;
    }

    /**
     * Set the actual value of one of the expression's parameters.
     *
     * @param parameterID    The parameter to be set.
     * @param parameterValue The parameter's value.
     */
    public void setParameterValue(String parameterID, Object parameterValue) {
        if (primitive != null && primitive.parameterID.equals(parameterID)) {
            primitive.setParameterValue(parameterValue);
        } else if (andChildren != null) {
            for (int i = 0; i < andChildren.length; i++) {
                andChildren[i].setParameterValue(parameterID, parameterValue);
            }
        } else if (orChildren != null) {
            for (int i = 0; i < orChildren.length; i++) {
                orChildren[i].setParameterValue(parameterID, parameterValue);
            }
        }
    }

    /**
     * Evaluate the expression to true or false.
     * Remark: all actual values for the parameters must have been set.
     * @return True or False.
     */
    public boolean evaluate() {
        boolean result;
        if (andChildren != null) {
            result = true;
            for (int i = 0; result && i < andChildren.length; i++) {
                result = result & andChildren[i].evaluate();
            }
        } else if (orChildren != null) {
            result = false;
            for (int i = 0; !result && i < orChildren.length; i++) {
                result = result | orChildren[i].evaluate();
            }
        } else {
            result = primitive.evaluate();
        }
        return result;
    }

    private enum OperatorType {
        GT, GE, LT, LE, EQ      // >, >=, <, <=, ==
    }

    private class PrimitiveBoolExpression {

        private String parameterID;
        private String parameterCompareValueAsString;
        private Object parameterCompareValue = null;
        private Object parameterActualValue = null;
        private OperatorType operatorType;

        public PrimitiveBoolExpression(String expressionString) {
            expressionString = expressionString.trim();
            String[] substrings = expressionString.split("[<>=][==]?");
            if (substrings.length != 2) throw new IllegalArgumentException(
                    "\"" + expressionString + " is not a boolean expression");
            String operatorString = expressionString.substring(substrings[0].length()).
                    substring(0, expressionString.indexOf(substrings[1]) - substrings[0].length()).trim();
            operatorType = determineOperatorType(operatorString);
            parameterID = substrings[0].trim();
            parameterCompareValueAsString = substrings[1].trim();
        }

        public void setParameterValue(Object parameterValue) {
            if (((parameterValue instanceof Boolean) || (parameterValue instanceof String)) &&
                    !(operatorType == OperatorType.EQ)) {
                throw new IllegalArgumentException(SimpleBooleanExpressionEvaluator.class.toString() +
                        ": can not set boolean or string value for \">\", \">=\", \"<\", \"<=\" expressions");
            }
            parameterActualValue = parameterValue;
            try {
                if (parameterValue instanceof Double) {
                    parameterCompareValue = Double.parseDouble(parameterCompareValueAsString);
                } else if (parameterValue instanceof Integer) {
                    parameterCompareValue = Integer.parseInt(parameterCompareValueAsString);
                } else if (parameterValue instanceof Boolean) {
                    parameterCompareValue = Boolean.parseBoolean(parameterCompareValueAsString);
                } else {
                    parameterCompareValue = parameterCompareValueAsString;
                    parameterActualValue = ((String)parameterActualValue).trim();
                }
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException(SimpleBooleanExpressionEvaluator.class.toString() +
                        ": can not compare actual value " + parameterValue + " to \""
                        + parameterCompareValueAsString + "\"");
            }
        }

        public boolean evaluate() {
            if ( parameterActualValue == null ) {
                throw new IllegalStateException(SimpleBooleanExpressionEvaluator.class.toString() +
                        ": actual value for " + parameterID+ " not set");
            }
            boolean result = false;
            if (operatorType == OperatorType.EQ) {
                result = (parameterActualValue.equals(parameterCompareValue));
            } else {
                if (parameterActualValue instanceof Double) {
                    if (operatorType == OperatorType.GT) {
                        result = (Double) parameterActualValue > (Double) parameterCompareValue;
                    } else if (operatorType == OperatorType.GE) {
                        result = (Double) parameterActualValue >= (Double) parameterCompareValue;
                    } else if (operatorType == OperatorType.LT) {
                        result = (Double) parameterActualValue < (Double) parameterCompareValue;
                    } else if (operatorType == OperatorType.LE) {
                        result = (Double) parameterActualValue <= (Double) parameterCompareValue;
                    }
                } else if (parameterActualValue instanceof Integer) {
                    if (operatorType == OperatorType.GT) {
                        result = (Integer) parameterActualValue > (Integer) parameterCompareValue;
                    } else if (operatorType == OperatorType.GE) {
                        result = (Integer) parameterActualValue >= (Integer) parameterCompareValue;
                    } else if (operatorType == OperatorType.LT) {
                        result = (Integer) parameterActualValue < (Integer) parameterCompareValue;
                    } else if (operatorType == OperatorType.LE) {
                        result = (Integer) parameterActualValue <= (Integer) parameterCompareValue;
                    }
                }
            }
            return result;
        }

        private OperatorType determineOperatorType(String operatorString) {
            OperatorType operatorType = OperatorType.EQ;
            if (operatorString.equals(">") ) {
                operatorType = OperatorType.GT;
            } else if (operatorString.equals(">=") ) {
                operatorType = OperatorType.GE;
            } else if (operatorString.equals("<") ) {
                operatorType = OperatorType.LT;
            } else if (operatorString.equals("<=") ) {
                operatorType = OperatorType.LE;
            }
            return operatorType;
        }
    }
}
