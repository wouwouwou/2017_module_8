package pp.block1.cc.test;

import org.junit.Test;
import pp.block1.cc.antlr.cc12;

/**
 * Created by Wouter on 25-4-2017.
 */
public class cc12Test {
    private static LexerTester tester = new LexerTester(cc12.class);

    @Test
    public void succeedingTest() {
        tester.correct("a12345");
        tester.correct("A12345");
        tester.correct("Aabcde");
        tester.correct("ABCDEFABCDEF");
        tester.correct("a2b3c4A54c26 A54c26");
        tester.correct("a2b3c4 A54c26A54c26");
        tester.yields("a12345 O132k3 p2k34I", cc12.IDENTIFIER, cc12.IDENTIFIER, cc12.IDENTIFIER);
        tester.wrong("2DFSAs a45%dfs45");
    }

    @Test
    public void spacesInKeywordTest() {
        tester.wrong("ab df4ss a3s");
    }

    @Test
    public void yieldCountTest() {
        tester.yields("a12345 a132k3 p2k34Ip2k34I", cc12.IDENTIFIER, cc12.IDENTIFIER, cc12.IDENTIFIER, cc12.IDENTIFIER);
    }

    @Test
    public void noSpacesBetweenKeywordsTest() {
        tester.yields("a12345O132k3p2k34Ip2k34Ip2k34I", cc12.IDENTIFIER, cc12.IDENTIFIER, cc12.IDENTIFIER, cc12.IDENTIFIER, cc12.IDENTIFIER);
    }
}