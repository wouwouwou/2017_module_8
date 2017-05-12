package pp.block3.cc.test;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.junit.Test;
import pp.block3.cc.antlr.operators.*;

import static org.junit.Assert.assertEquals;

public class OperTest {

	private final ParseTreeWalker walker = new ParseTreeWalker();
	private final OperCalculator calc = new OperCalculator();

	@Test
	public void operTest() {
		test("5", "3+2");
		test("6", "1+2+3");
		test("27", "(1+2)^3");
		test("10", "2 ^ 3 + 2");
		test("64", "2 ^ 3 ^ 2"); // Power not right associative
		test("\"ababab\"", "\"ab\"^3");
		test("true", "2=2=true");
		test("true", "\"x^2\"^3=\"x^2\"+\"x^2\"+\"x^2\"");
		test("true", "(\"x^2\"^3=\"x^2\"+\"x^2\"+\"x^2\")+false");
		test("false", "false + false");
	}

	private void test(String expected, String expr) {
		assertEquals(expected, parseOperAttr(expr).value);
		ParseTree tree = parseOper(expr);
		calc.init();
		walker.walk(calc, tree);
		assertEquals(expected, calc.value(tree));
	}

	private ParseTree parseOper(String text) {
		CharStream chars = new ANTLRInputStream(text);
		Lexer lexer = new OperLexer(chars);
		TokenStream tokens = new CommonTokenStream(lexer);
		OperParser parser = new OperParser(tokens);
		return parser.t();
	}

	private OperAttrParser.TContext parseOperAttr(String text) {
		CharStream chars = new ANTLRInputStream(text);
		Lexer lexer = new OperAttrLexer(chars);
		TokenStream tokens = new CommonTokenStream(lexer);
		OperAttrParser parser = new OperAttrParser(tokens);
		return parser.t();
	}
}
