package pp.block4.cc.cfg;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTreeProperty;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import pp.block4.cc.ErrorListener;
import pp.block4.cc.cfg.FragmentParser.ProgramContext;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;

/**
 * Template top-down CFG builder.
 */
public class TopDownCFGBuilder extends FragmentBaseListener {
    /**
     * The CFG being built.
     */
    private Graph graph;

    
    /**
     * 
     */
    private ParseTreeProperty<Node> entrances = new ParseTreeProperty<>();
    private ParseTreeProperty<Node> exits = new ParseTreeProperty<>();

    /**
     * Main method to build and print the CFG of a simple Java program.
     */
    public static void main(String[] args) {
        if (args.length == 0) {
            System.err.println("Usage: [filename]+");
            return;
        }
        TopDownCFGBuilder builder = new TopDownCFGBuilder();
        for (String filename : args) {
            File file = new File(filename);
            System.out.println(filename);
            System.out.println(builder.build(file));
            try {
                builder.graph.writeDOT(filename + "_TD.dot", true);
            } catch (IOException e) {
                //
            }
        }
    }

    /**
     * Builds the CFG for a program contained in a given file.
     */
    private Graph build(File file) {
        Graph result = null;
        ErrorListener listener = new ErrorListener();
        try {
            CharStream chars = new ANTLRInputStream(new FileReader(file));
            Lexer lexer = new FragmentLexer(chars);
            lexer.removeErrorListeners();
            lexer.addErrorListener(listener);
            TokenStream tokens = new CommonTokenStream(lexer);
            FragmentParser parser = new FragmentParser(tokens);
            parser.removeErrorListeners();
            parser.addErrorListener(listener);
            ProgramContext tree = parser.program();
            if (listener.hasErrors()) {
                System.out.printf("Parse errors in %s:%n", file.getPath());
                listener.getErrors().forEach(System.err::println);
            } else {
                result = build(tree);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        try {
			result.writeDOT("Dottopdown", true);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
        return result;
    }

    /**
     * Builds the CFG for a program given as an ANTLR parse tree.
     */
    private Graph build(ProgramContext tree) {
        this.graph = new Graph();
        ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(this, tree);
        return graph;
    }

    @Override
    public void enterProgram(ProgramContext ctx) {
        Node node = new Node(0, "program");
        for (FragmentParser.StatContext stat : ctx.stat()) {
            Node childEntry = addNode(stat, "statement _in_");
            Node childExit = addNode(stat, "statement _out_");
            entrances.put(stat, childEntry);
            exits.put(stat, childExit);
            node.addEdge(childEntry);
            node = childExit;
        }
    }

    @Override
    public void enterDecl(FragmentParser.DeclContext ctx) {
        entrances.get(ctx).addEdge(exits.get(ctx));
    }

    @Override
    public void enterAssignStat(FragmentParser.AssignStatContext ctx) {
        entrances.get(ctx).addEdge(exits.get(ctx));
    }

    @Override
    public void enterIfStat(FragmentParser.IfStatContext ctx) {
        Node entrance = entrances.get(ctx);
        Node exit = exits.get(ctx);
        Node ifEntry = addNode(ctx.stat(0), "if _in_");
        Node ifExit = addNode(ctx.stat(0), "if _out_");
        entrance.addEdge(exit);
        entrances.put(ctx.stat(0), ifEntry);
        exits.put(ctx.stat(0), ifExit);
        if (ctx.stat(1) == null) {
            entrance.addEdge(ifEntry);
            ifExit.addEdge(exit);
        } else {
            Node elseEntry = addNode(ctx.stat(1), "else _in_");
            Node elseExit = addNode(ctx.stat(1), "else _out_");
            entrances.put(ctx.stat(1), elseEntry);
            exits.put(ctx.stat(1), elseExit);
            entrance.addEdge(ifEntry);
            ifExit.addEdge(exit);
            elseExit.addEdge(exit);
        }
    }

    @Override
    public void enterWhileStat(FragmentParser.WhileStatContext ctx) {
        Node entrance = entrances.get(ctx);
        Node exit = exits.get(ctx);
        Node whileEntry = addNode(ctx.stat(),  "while _in_");
        Node whileExit = addNode(ctx.stat(), "while _out_");
        entrances.put(ctx.stat(), whileEntry);
        exits.put(ctx.stat(), whileExit);
        entrance.addEdge(whileEntry);
        entrance.addEdge(exit);
        whileExit.addEdge(entrance);
    }

    @Override
    public void enterBlockStat(FragmentParser.BlockStatContext ctx) {
        Node entrance = entrances.get(ctx);
        Node exit = exits.get(ctx);
        Node node = entrance;
        for (FragmentParser.StatContext stat : ctx.stat()) {
            Node blockEntry = addNode(stat, "block _in_");
            Node blockExit = addNode(stat,"block _out_");
            entrances.put(stat, blockEntry);
            exits.put(stat, blockExit);
            node.addEdge(blockEntry);
            node = blockExit;
        }
        node.addEdge(exit);
    }

    @Override
    public void enterPrintStat(FragmentParser.PrintStatContext ctx) {
        entrances.get(ctx).addEdge(exits.get(ctx));
    }

    /**
     * Adds a node to he CGF, based on a given parse tree node.
     * Gives the CFG node a meaningful ID, consisting of line number and
     * a further indicator.
     */
    private Node addNode(ParserRuleContext node, String text) {
        return this.graph.addNode(node.getStart().getLine() + ": " + text);
    }
}
