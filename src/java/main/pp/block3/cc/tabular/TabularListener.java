package pp.block3.cc.tabular;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeProperty;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import javax.swing.*;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.stream.Collectors;

/**
 * Created by Wouter on 12-5-2017.
 */
public class TabularListener extends LatexBaseListener {

    private static final ParseTreeWalker walker = new ParseTreeWalker();
    private static final TabularListener html = new TabularListener();
    private static final MyErrorListener el = new MyErrorListener();

    private ParseTreeProperty<String> nodes;

    public void init() {
        this.nodes = new ParseTreeProperty<>();
    }

    private void set(ParseTree node, String val) {
        this.nodes.put(node, val);
    }

    private String val(ParseTree node) {
        return this.nodes.get(node);
    }

    @Override
    public void exitTable(LatexParser.TableContext ctx) {
        String rows = ctx.row().stream().map(this::val).collect(Collectors.joining());
        set(ctx, String.format("<table border=\"1\">\n%s</table>", rows));
    }

    @Override
    public void exitRow(LatexParser.RowContext ctx) {
        String entries = ctx.entry().stream().map(this::val).collect(Collectors.joining());
        set(ctx, String.format("<tr>%s</tr>\n", entries));
    }

    @Override
    public void exitEntry(LatexParser.EntryContext ctx) {
        String entry = ctx.ENTRY() == null ? "" : ctx.ENTRY().getText();
        set(ctx, String.format("<td>%s</td>\n", entry));
    }

    public static void main(String[] args) throws IOException, URISyntaxException {
        JFileChooser chooser = new JFileChooser(System.getProperty("user.dir"));
        int returnVal = chooser.showOpenDialog(null);
        FileReader reader = null;
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            reader = new FileReader(chooser.getSelectedFile());
        }
        if (reader == null) {
            throw new NullPointerException();
        }

        ParseTree tree = parse(reader);

        if (el.getErrorlist().size() > 0) throw new RuntimeException("Something went wrong " +
                "with parsing the latex file.");

        html.init();
        walker.walk(html, tree);
        String doc = String.format("<html><body>\n%s\n</body></html>", html.val(tree));
        HTMLWriter.write(doc, chooser.getSelectedFile().getName() + ".html");
    }

    private static ParseTree parse(Reader reader) throws IOException {
        CharStream chars = CharStreams.fromReader(reader);
        Lexer lexer = new LatexLexer(chars);

        lexer.removeErrorListeners();
        lexer.addErrorListener(el);
        TokenStream tokens = new CommonTokenStream(lexer);

        LatexParser parser = new LatexParser(tokens);
        parser.removeErrorListeners();
        parser.addErrorListener(el);

        return parser.table();
    }

    private static String readFile(String file) throws URISyntaxException, IOException {
        URI path = TabularListener.class.getResource(file).toURI();
        byte[] encoded = Files.readAllBytes(Paths.get(path));
        return new String(encoded, Charset.defaultCharset());
    }
}
