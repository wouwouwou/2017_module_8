package pp.block3.cc.test;

import org.junit.Test;
import pp.block1.cc.test.LexerTester;
import pp.block3.cc.tabular.LatexLexer;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * Created by Wouter on 12-5-2017.
 */
public class LatexTest {

    private static LexerTester tester = new LexerTester(LatexLexer.class);

    @Test
    public void tabularTest() throws IOException, URISyntaxException {
        tester.correct(readFile("tabular-0.tex"));
        tester.yields(readFile("tabular-0.tex"),
                LatexLexer.TABLESTART,
                LatexLexer.ENTRY, LatexLexer.SEP, LatexLexer.ENTRY, LatexLexer.SEP, LatexLexer.ENTRY, LatexLexer.ENDROW,
                LatexLexer.ENTRY, LatexLexer.SEP, LatexLexer.ENTRY, LatexLexer.SEP, LatexLexer.ENTRY, LatexLexer.ENDROW,
                LatexLexer.ENTRY, LatexLexer.SEP, LatexLexer.ENTRY, LatexLexer.SEP, LatexLexer.ENTRY, LatexLexer.ENDROW,
                LatexLexer.TABLEEND);

        tester.correct(readFile("tabular-1.tex"));
        tester.correct(readFile("tabular-2.tex"));
        tester.correct(readFile("tabular-3.tex"));
        tester.wrong(readFile("tabular-4.tex"));
        tester.correct(readFile("tabular-5.tex"));
    }

    private String readFile(String file) throws URISyntaxException, IOException {
        URI path = LatexTest.class.getResource(file).toURI();
        byte[] encoded = Files.readAllBytes(Paths.get(path));
        return new String(encoded, Charset.defaultCharset());
    }

}
