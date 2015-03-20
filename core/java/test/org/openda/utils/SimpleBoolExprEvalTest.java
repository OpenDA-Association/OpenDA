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

import junit.framework.TestCase;

/**
 * Tests for SimpleBooleanExpressionEvaluator
 */
public class SimpleBoolExprEvalTest extends TestCase {

    public void testIntegers() {

        String expressionGT = "a>12";
        String expressionGE = "a>=12";
        String expressionEQ = "a=12";
        String expressionEQ2 = "a==12";
        String expressionLE = "a<=12";
        String expressionLT = "a<12";

        SimpleBooleanExpressionEvaluator evaluatorGT = new SimpleBooleanExpressionEvaluator(expressionGT);
        SimpleBooleanExpressionEvaluator evaluatorGE = new SimpleBooleanExpressionEvaluator(expressionGE);
        SimpleBooleanExpressionEvaluator evaluatorEQ = new SimpleBooleanExpressionEvaluator(expressionEQ);
        SimpleBooleanExpressionEvaluator evaluatorEQ2 = new SimpleBooleanExpressionEvaluator(expressionEQ2);
        SimpleBooleanExpressionEvaluator evaluatorLE = new SimpleBooleanExpressionEvaluator(expressionLE);
        SimpleBooleanExpressionEvaluator evaluatorLT = new SimpleBooleanExpressionEvaluator(expressionLT);

        evaluatorGT.setParameterValue("a", 1);
        evaluatorGE.setParameterValue("a", 1);
        evaluatorEQ.setParameterValue("a", 1);
        evaluatorEQ2.setParameterValue("a", 1);
        evaluatorLE.setParameterValue("a", 1);
        evaluatorLT.setParameterValue("a", 1);

        assertFalse(expressionGT + " a:=1", evaluatorGT.evaluate());
        assertFalse(expressionGE + " a:=1", evaluatorGE.evaluate());
        assertFalse(expressionEQ + " a:=1", evaluatorEQ.evaluate());
        assertFalse(expressionEQ2 + " a:=1", evaluatorEQ2.evaluate());
        assertTrue(expressionLE + " a:=1", evaluatorLE.evaluate());
        assertTrue(expressionLT + " a:=1", evaluatorLT.evaluate());

        evaluatorGT.setParameterValue("a", 12);
        evaluatorGE.setParameterValue("a", 12);
        evaluatorEQ.setParameterValue("a", 12);
        evaluatorEQ2.setParameterValue("a", 12);
        evaluatorLE.setParameterValue("a", 12);
        evaluatorLT.setParameterValue("a", 12);

        assertFalse(expressionGT + " a:=12", evaluatorGT.evaluate());
        assertTrue(expressionGE + " a:=12", evaluatorGE.evaluate());
        assertTrue(expressionEQ + " a:=12", evaluatorEQ.evaluate());
        assertTrue(expressionEQ2 + " a:=12", evaluatorEQ2.evaluate());
        assertTrue(expressionLE + " a:=12", evaluatorLE.evaluate());
        assertFalse(expressionLT + " a:=12", evaluatorLT.evaluate());
    }

    public void testDoubles() {

        String expressionGT = "a>2.7";
        String expressionGE = "a>=2.7";
        String expressionEQ = "a=2.7";
        String expressionLE = "a<=2.7";
        String expressionLT = "a<2.7";

        SimpleBooleanExpressionEvaluator evaluatorGT = new SimpleBooleanExpressionEvaluator(expressionGT);
        SimpleBooleanExpressionEvaluator evaluatorGE = new SimpleBooleanExpressionEvaluator(expressionGE);
        SimpleBooleanExpressionEvaluator evaluatorEQ = new SimpleBooleanExpressionEvaluator(expressionEQ);
        SimpleBooleanExpressionEvaluator evaluatorLE = new SimpleBooleanExpressionEvaluator(expressionLE);
        SimpleBooleanExpressionEvaluator evaluatorLT = new SimpleBooleanExpressionEvaluator(expressionLT);

        evaluatorGT.setParameterValue("a", 1.3);
        evaluatorGE.setParameterValue("a", 1.3);
        evaluatorEQ.setParameterValue("a", 1.3);
        evaluatorLE.setParameterValue("a", 1.3);
        evaluatorLT.setParameterValue("a", 1.3);

        assertFalse(expressionGT + " a:=2.7", evaluatorGT.evaluate());
        assertFalse(expressionGE + " a:=2.7", evaluatorGE.evaluate());
        assertFalse(expressionEQ + " a:=2.7", evaluatorEQ.evaluate());
        assertTrue(expressionLE + " a:=2.7", evaluatorLE.evaluate());
        assertTrue(expressionLT + " a:=2.7", evaluatorLT.evaluate());

        evaluatorGT.setParameterValue("a", 2.7);
        evaluatorGE.setParameterValue("a", 2.7);
        evaluatorEQ.setParameterValue("a", 2.7);
        evaluatorLE.setParameterValue("a", 2.7);
        evaluatorLT.setParameterValue("a", 2.7);

        assertFalse(expressionGT + " a:=2.7", evaluatorGT.evaluate());
        assertTrue(expressionGE + " a:=2.7", evaluatorGE.evaluate());
        assertTrue(expressionEQ + " a:=2.7", evaluatorEQ.evaluate());
        assertTrue(expressionLE + " a:=2.7", evaluatorLE.evaluate());
        assertFalse(expressionLT + " a:=2.7", evaluatorLT.evaluate());
    }

    public void testStrings() {

        String expressionEQ = "a==let's run this \"test\" ";
        SimpleBooleanExpressionEvaluator evaluatorEQ = new SimpleBooleanExpressionEvaluator(expressionEQ);

        String equalValue = "let's run this \"test\" ";
        evaluatorEQ.setParameterValue("a", equalValue);
        assertTrue(expressionEQ + " " + equalValue, evaluatorEQ.evaluate());

        String nonEqualValue = "let's run this test ";
        evaluatorEQ.setParameterValue("a", nonEqualValue);
        assertFalse(expressionEQ + " " + nonEqualValue, evaluatorEQ.evaluate());
    }

    public void testAnds() {

        String andExpression_1 = "a>27 and b>12.0";
        SimpleBooleanExpressionEvaluator andEvaluator_1 = new SimpleBooleanExpressionEvaluator(andExpression_1);
        andEvaluator_1.setParameterValue("a", 30);
        andEvaluator_1.setParameterValue("b", 16.0);
        assertTrue(andExpression_1 + " a:=30, b:=16", andEvaluator_1.evaluate());

        String andExpression_2 = "a>27 and b>12.0 and b < 15";
        SimpleBooleanExpressionEvaluator andEvaluator_2 = new SimpleBooleanExpressionEvaluator(andExpression_2);
        andEvaluator_2.setParameterValue("a", 30);
        andEvaluator_2.setParameterValue("b", 16.0);
        assertFalse(andExpression_2 + " a:=30, b:=16", andEvaluator_2.evaluate());

    }

    public void testOrs() {

        String orExpression_1 = "a>27 or b>12.0";
        SimpleBooleanExpressionEvaluator orEvaluator_1 = new SimpleBooleanExpressionEvaluator(orExpression_1);

        orEvaluator_1.setParameterValue("a", 30);
        orEvaluator_1.setParameterValue("b", 10.0);
        assertTrue(orExpression_1 + " a:=30, b:=10", orEvaluator_1.evaluate());

        orEvaluator_1.setParameterValue("a", 15);
        orEvaluator_1.setParameterValue("b", 10.0);
        assertFalse(orExpression_1 + " a:=15, b:=10", orEvaluator_1.evaluate());

        orEvaluator_1.setParameterValue("a", 15);
        orEvaluator_1.setParameterValue("b", 15.0);
        assertTrue(orExpression_1 + " a:=15, b:=15", orEvaluator_1.evaluate());
    }

    public void testAndOrs() {

        String andOrExpression_1 = "a>27 or b>12.0 and b < 15";
        SimpleBooleanExpressionEvaluator andOrEvaluator_1 = new SimpleBooleanExpressionEvaluator(andOrExpression_1);

        andOrEvaluator_1.setParameterValue("a", 10);
        andOrEvaluator_1.setParameterValue("b", 16.0);
        assertFalse(andOrExpression_1 + " a:=10, b:=16", andOrEvaluator_1.evaluate());

        andOrEvaluator_1.setParameterValue("a", 30);
        andOrEvaluator_1.setParameterValue("b", 16.0);
        assertTrue(andOrExpression_1 + " a:=30, b:=16", andOrEvaluator_1.evaluate());

        andOrEvaluator_1.setParameterValue("a", 10);
        andOrEvaluator_1.setParameterValue("b", 14.0);
        assertTrue(andOrExpression_1 + " a:=10, b:=14", andOrEvaluator_1.evaluate());

        String andOrExpression_2 = "a <12 or a> 27 or b>12.0 and b < 15";
        SimpleBooleanExpressionEvaluator andOrEvaluator_2 = new SimpleBooleanExpressionEvaluator(andOrExpression_2);

        andOrEvaluator_2.setParameterValue("a", 15);
        andOrEvaluator_2.setParameterValue("b", 16.0);
        assertFalse(andOrExpression_2 + " a:=15, b:=16", andOrEvaluator_2.evaluate());

        andOrEvaluator_2.setParameterValue("a", 10);
        andOrEvaluator_2.setParameterValue("b", 14.0);
        assertTrue(andOrExpression_2 + " a:=10, b:=14", andOrEvaluator_2.evaluate());

        andOrEvaluator_2.setParameterValue("a", 10);
        andOrEvaluator_2.setParameterValue("b", 16.0);
        assertTrue(andOrExpression_2 + " a:=10, b:=16", andOrEvaluator_2.evaluate());

        String andOrExpression_3 = "a >12 and a< 27 or b>12.0 and b < 15";
        SimpleBooleanExpressionEvaluator andOrEvaluator_3 = new SimpleBooleanExpressionEvaluator(andOrExpression_3);

        andOrEvaluator_3.setParameterValue("a", 15);
        andOrEvaluator_3.setParameterValue("b", 14.0);
        assertTrue(andOrExpression_3 + " a:=15, b:=14", andOrEvaluator_3.evaluate());

        andOrEvaluator_3.setParameterValue("a", 10);
        andOrEvaluator_3.setParameterValue("b", 14.0);
        assertTrue(andOrExpression_3 + " a:=10, b:=14", andOrEvaluator_3.evaluate());

        andOrEvaluator_3.setParameterValue("a", 10);
        andOrEvaluator_3.setParameterValue("b", 16.0);
        assertFalse(andOrExpression_3 + " a:=10, b:=16", andOrEvaluator_3.evaluate());
    }
}
