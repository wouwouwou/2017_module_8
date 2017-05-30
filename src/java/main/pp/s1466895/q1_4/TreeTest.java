package pp.s1466895.q1_4;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.junit.Test;
import pp.homework.q1_4.TreeLexer;
import pp.homework.q1_4.TreeParser;
import pp.homework.q1_4.TreeParser.TopContext;

import static org.junit.Assert.assertEquals;

/**
 * Created by martijn on 25-5-17.
 */
public class TreeTest {


    private final ParseTreeWalker walker = new ParseTreeWalker();
    private final MyTreeListener calc = new MyTreeListener();


    @Test
    public void test() {
        ordered(true, "(3<5)");
        ordered(true, "((1<2>3)<5)");
        ordered(true, "(3<5>(7<9>10))");
        ordered(false, "((3<4>6)<5)");
        ordered(true, "((((1<2)<3)<4)<5)");
        ordered(true, "(3>(5>(6>(7<9>10))))");
    }


    private void ordered(boolean expected, String tree) {
        ParseTree ptree = parseNumber(tree);
        calc.init();
        walker.walk(calc, ptree);
        assertEquals(expected, calc.getOrdered(ptree));
    }

    private TopContext parseNumber(String text) {
        CharStream chars = new ANTLRInputStream(text);
        Lexer lexer = new TreeLexer(chars);
        TokenStream tokens = new CommonTokenStream(lexer);
        TreeParser parser = new TreeParser(tokens);
        return parser.top();
    }
}
