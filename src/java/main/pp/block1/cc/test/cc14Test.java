package pp.block1.cc.test;

import org.junit.Test;
import pp.block1.cc.antlr.cc14;

/**
 * Created by Wouter on 25-4-2017.
 */
public class cc14Test {
    private static LexerTester tester = new LexerTester(cc14.class);

    @Test
    public void succeedingTest() {
        tester.correct("La");
        tester.correct("La La");
        tester.correct("LaLa");
        tester.correct("La La La Li");
        tester.correct("LaLa La Li");
        tester.correct("LaLaLa Li");
        tester.correct("LaLaLaLi");
        tester.correct("La LaLa Li");
        tester.correct("La LaLaLi");
        tester.correct("La La LaLi");
        tester.correct("LaLa LaLi");
        tester.correct("Laaaa");
        tester.correct("Laaaa La");
        tester.correct("La Laaaaaa");
        tester.correct("LaaaaLa");
        tester.correct("LaLaaaaaaa");
        tester.correct("Laa La La Li");
        tester.correct("LaLa Laaa Li");
        tester.correct("LaLaaaaaLa Li");
        tester.correct("LaaLaaaaLaLi");
        tester.correct("Laaa LaLa Li");
        tester.correct("La LaLaLi");
        tester.correct("La Laa LaLi");
        tester.correct("LaLa LaLi");
        tester.correct("La La La Li La La La Li");
        tester.wrong("LaLa LaLiii");
        tester.wrong("LaLaLLaLi");
        tester.wrong("LaLaaaa aaLaLi");
    }

    @Test
    public void spacesInKeywordTest() {
        tester.wrong("L a");
    }

    @Test
    public void yieldCountTest() {
        tester.yields("La La La Li La La La Li", cc14.LALALALI, cc14.LALALALI);
        tester.yields("La Laaaa La La", cc14.LALA, cc14.LALA);
    }

    @Test
    public void noSpacesBetweenKeywordsTest() {
        tester.yields("LaLaaaaLaLa", cc14.LALA, cc14.LALA);
    }
}