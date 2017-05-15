package pp.block3.cc.symbol;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTreeProperty;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import javax.swing.*;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by Wouter on 12-5-2017.
 */
public class DeclUseList extends DeclUseBaseListener {

    /**
     * ParseTreeProperto to link nodes in the parse tree to errors.
     */
    private ParseTreeProperty<String> errors;

    /**
     * List with all errors.
     */
    private List<String> errorlist;

    /**
     * Because we love symbolTables. And it is useful for this exercise. :D
     */
    private MySymbolTable symbolTable;

    /**
     * Initialises the Listener.
     */
    public DeclUseList() {
        this.errors = new ParseTreeProperty<>();
        this. symbolTable = new MySymbolTable();
        this.errorlist = new ArrayList<>();
    }

    @Override
    public void enterSeries(DeclUseParser.SeriesContext ctx) {
        symbolTable.openScope();
    }

    @Override
    public void exitSeries(DeclUseParser.SeriesContext ctx) {
        Token t = ctx.getStop();
        try {
            symbolTable.closeScope();
        } catch (RuntimeException e) {
            String s = "Unable to close root scope level at " + t.getLine() + "::" + t.getCharPositionInLine();
            errors.put(ctx, s);
            errorlist.add(s);
        }
    }

    @Override
    public void exitDecl(DeclUseParser.DeclContext ctx) {
        Token t = ctx.ID().getSymbol();
        if (!symbolTable.add(t.getText())) {
            String s = "The identifier at " + t.getLine() + "::" + t.getCharPositionInLine() +
                    " is already declared in scope!";
            errors.put(ctx, s);
            errorlist.add(s);
        }
    }

    @Override
    public void exitUse(DeclUseParser.UseContext ctx) {
        Token t = ctx.ID().getSymbol();
        if (!symbolTable.contains(t.getText())) {
            String s = "Used identifier at " + t.getLine() + "::" + t.getCharPositionInLine() +
                    " is not declared in scope!";
            errors.put(ctx, s);
            errorlist.add(s);
        }
    }

    private void printErrors() {
        FileReader reader;
        CharStream chars = null;
        try {
            reader = getReader();
            chars = CharStreams.fromReader(reader);
        } catch (java.io.IOException e) {
            e.printStackTrace();
        }
        Lexer lexer = new DeclUseLexer(chars);
        TokenStream tokens = new CommonTokenStream(lexer);
        DeclUseParser parser = new DeclUseParser(tokens);
        ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(this, parser.program());
        errorlist.forEach(System.err::println);
    }

    private FileReader getReader() throws FileNotFoundException, NullPointerException {
        JFileChooser chooser = new JFileChooser(System.getProperty("user.dir"));
        int returnVal = chooser.showOpenDialog(null);
        FileReader reader = null;
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            reader = new FileReader(chooser.getSelectedFile());
        }
        if (reader == null) {
            throw new NullPointerException();
        }
        return reader;
    }

    public static void main(String[] args) {
        DeclUseList list = new DeclUseList();
        list.printErrors();
    }

    public List<String> getErrorlist() {
        return errorlist;
    }
}
