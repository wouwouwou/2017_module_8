package pp.block2.cc.test;

import org.antlr.v4.runtime.*;
import org.junit.Test;
import pp.block2.cc.antlr.Calculator;
import pp.block2.cc.antlr.ExprLexer;

import java.math.BigInteger;

import static org.junit.Assert.assertEquals;

/**
 * Created by Wouter on 3-5-2017.
 */
public class CalculatorTest {

    @Test
    public void testCalc() {
        Calculator calculator = new Calculator();
        assertEquals(calculator.calculate(new ExprLexer(CharStreams.fromString("1+1"))), new BigInteger("2"));
        assertEquals(calculator.calculate(new ExprLexer(CharStreams.fromString("2+3"))), new BigInteger("5"));
        assertEquals(calculator.calculate(new ExprLexer(CharStreams.fromString("12+67"))), new BigInteger("79"));
        assertEquals(calculator.calculate(new ExprLexer(CharStreams.fromString("-41+99"))), new BigInteger("58"));
        assertEquals(calculator.calculate(new ExprLexer(CharStreams.fromString("----22+--2"))), new BigInteger("24"));
        assertEquals(calculator.calculate(new ExprLexer(CharStreams.fromString("1*1"))), new BigInteger("1"));
        assertEquals(calculator.calculate(new ExprLexer(CharStreams.fromString("2*5"))), new BigInteger("10"));
        assertEquals(calculator.calculate(new ExprLexer(CharStreams.fromString("-7*7"))), new BigInteger("-49"));
        assertEquals(calculator.calculate(new ExprLexer(CharStreams.fromString("2^2"))), new BigInteger("4"));
        assertEquals(calculator.calculate(new ExprLexer(CharStreams.fromString("2^2+1"))), new BigInteger("5"));
        assertEquals(calculator.calculate(new ExprLexer(CharStreams.fromString("3^3*2"))), new BigInteger("54"));
    }
}
