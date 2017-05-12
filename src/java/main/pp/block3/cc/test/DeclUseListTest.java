package pp.block3.cc.test;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.junit.Assert;
import org.junit.Test;
import pp.block3.cc.symbol.DeclUseLexer;
import pp.block3.cc.symbol.DeclUseList;
import pp.block3.cc.symbol.DeclUseParser;

import java.io.CharArrayReader;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by Wouter on 12-5-2017.
 */
public class DeclUseListTest {

    private DeclUseList declUseList = new DeclUseList();
    private ParseTreeWalker walker = new ParseTreeWalker();

    @Test
    public void fullTest() {
        List<String> errorlist = new ArrayList<>();
        test("(D:aap (U:aap D:noot D:aap (U:noot) (D:noot U:noot )) U:aap)", errorlist);

        errorlist.add("Used identifier at 1::11 is not declared in scope!");
        errorlist.add("The identifier at 1::30 is already declared in scope!");
        errorlist.add("Used identifier at 1::69 is not declared in scope!");
        test("(D:noot (U:aap D:noot D:aap D:noot (U:aap U:noot) (D:noot U:noot)) U:aap)", errorlist);
    }

    private void test(String s, List<String> errorlist) {
        CharStream chars = CharStreams.fromString(s);
        Lexer lexer = new DeclUseLexer(chars);
        TokenStream tokens = new CommonTokenStream(lexer);
        DeclUseParser parser = new DeclUseParser(tokens);
        walker.walk(declUseList, parser.program());
        Assert.assertEquals(errorlist, declUseList.getErrorlist());
    }

}
