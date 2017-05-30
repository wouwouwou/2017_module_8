package pp.block4.cc.cfg;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeProperty;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import pp.block4.cc.ErrorListener;
import pp.block4.cc.cfg.FragmentParser.*;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;

/**
 * Template bottom-up CFG builder.
 */
public class BottomUpCFGBuilder extends FragmentBaseListener {
    /**
     * The CFG being built.
     */
    private Graph graph;

    /**
     * Nodelist parsetreeproperty
     */
    private ParseTreeProperty<Node[]> cfgNodes = new ParseTreeProperty<>();

    /**
     * Main method to build and print the CFG of a simple Java program.
     */
    public static void main(String[] args) {
        if (args.length == 0) {
            System.err.println("Usage: [filename]+");
            return;
        }
        BottomUpCFGBuilder builder = new BottomUpCFGBuilder();
        for (String filename : args) {
            File file = new File(filename);
            System.out.println(filename);
            System.out.println(builder.build(file));
            try {
                builder.graph.writeDOT(filename + "_BU.dot", true);
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
            ParseTree tree = parser.program();
            if (listener.hasErrors()) {
                System.out.printf("Parse errors in %s:%n", file.getPath());
//				listener.getErrors().forEach(System.err::println);
            } else {
                result = build(tree);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return result;
    }

    /**
     * Builds the CFG for a program given as an ANTLR parse tree.
     */
    private Graph build(ParseTree tree) {
        this.graph = new Graph();
        new ParseTreeWalker().walk(this, tree);
        return graph;
    }

    @Override
    public void exitProgram(ProgramContext ctx) {
        for (int i = 0; i < ctx.stat().size() - 1; i++) {
            Node[] stat1 = cfgNodes.get(ctx.stat(i));
            Node[] stat2 = cfgNodes.get(ctx.stat(i + 1));
            stat1[1].addEdge(stat2[0]);

        }
    }

    @Override
    public void exitDecl(DeclContext ctx) {
        Node n = addNode(ctx, "decl");
        cfgNodes.put(ctx, new Node[]{n, n});
    }


    @Override
    public void exitAssignStat(AssignStatContext ctx) {
        Node n = addNode(ctx, "assign");
        cfgNodes.put(ctx, new Node[]{n, n});
    }

    @Override
    public void exitIfStat(IfStatContext ctx) {
        Node exprNode = addNode(ctx.expr(), "ifStat");
        Node ifExit = addNode(ctx, "ifExit");
        for (StatContext s : ctx.stat()) {
            Node thenelse[] = cfgNodes.get(s);
            exprNode.addEdge(thenelse[0]);
            thenelse[1].addEdge(ifExit);
        }
        if (ctx.stat().size() < 2) {
            exprNode.addEdge(ifExit);
        }

        cfgNodes.put(ctx, new Node[]{exprNode, ifExit});
    }


    @Override
    public void exitWhileStat(WhileStatContext ctx) {
        Node exprNode = addNode(ctx.expr(), "whileStat");
        Node[] statNode = cfgNodes.get(ctx.stat());
        exprNode.addEdge(statNode[0]);
        statNode[1].addEdge(exprNode);
        Node exitNode = addNode(ctx.expr(), "whileExit");
        exprNode.addEdge(exitNode);
        cfgNodes.put(ctx, new Node[]{exprNode, exitNode});
    }

    @Override
    public void exitBlockStat(BlockStatContext ctx) {
        for (int i = 0; i < ctx.stat().size() - 1; i++) {
            Node[] stat1 = cfgNodes.get(ctx.stat(i));
            Node[] stat2 = cfgNodes.get(ctx.stat(i + 1));
//			System.out.println(stat1 + " |\n " + ctx.stat(i).getText());
//			System.out.println(stat2 + " " + ctx.stat(i + 1).getText());
            stat1[1].addEdge(stat2[0]);
        }
        cfgNodes.put(ctx, new Node[]{cfgNodes.get(ctx.stat(0))[0], cfgNodes.get(ctx.stat(ctx.stat().size() - 1))[1]});
    }

    @Override
    public void exitPrintStat(PrintStatContext ctx) {
        Node n = addNode(ctx, "print");
        cfgNodes.put(ctx, new Node[]{n, n});
    }

    @Override
    public void exitBreakStat(BreakStatContext ctx) {
        Node n = addNode(ctx, "break");
        ParserRuleContext repctx = ctx;
        while (!(repctx instanceof WhileStatContext)) {
            repctx = repctx.getParent();
        }

        Node exitNode;
        if (cfgNodes.get(((WhileStatContext) repctx).expr()) == null) {

            Node exprNode = addNode(((WhileStatContext) repctx).expr(), "whileStat");
            Node[] whileNode = cfgNodes.get(((WhileStatContext) repctx).stat());
            exprNode.addEdge(whileNode[0]);
            whileNode[1].addEdge(exprNode);
            exitNode = addNode(((WhileStatContext) repctx).expr(), "whileExit");
            exprNode.addEdge(exitNode);
            cfgNodes.put(((WhileStatContext) repctx), new Node[]{exprNode, exitNode});
        } else {
            exitNode = cfgNodes.get(((WhileStatContext) repctx).expr())[1];
        }
        cfgNodes.put(((WhileStatContext) repctx).expr(), new Node[] {exitNode});
        n.addEdge(exitNode);
        cfgNodes.put(ctx, new Node[]{n});
    }

    @Override
    public void exitContStat(ContStatContext ctx) {
        Node n = addNode(ctx, "cont");
        Node exit = cfgNodes.get(ctx.getParent().getParent())[0];
        n.addEdge(exit);
        cfgNodes.put(ctx, new Node[]{n, exit});
    }

//    private void simpleExpr(ParserRuleContext ctx) {
//        Node n = addNode(ctx, "simple");
//        cfgNodes.put(ctx, new Node[]{n, n});
//    }

    /**
     * Adds a node to he CGF, based on a given parse tree node.
     * Gives the CFG node a meaningful ID, consisting of line number and
     * a further indicator.
     */
    private Node addNode(ParserRuleContext node, String text) {
        return this.graph.addNode(node.getStart().getLine() + ": " + text);
    }
}
