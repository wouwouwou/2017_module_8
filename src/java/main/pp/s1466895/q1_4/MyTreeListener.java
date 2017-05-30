package pp.s1466895.q1_4;


import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;
import pp.homework.q1_4.TreeLexer;
import pp.homework.q1_4.TreeListener;
import pp.homework.q1_4.TreeParser;

/**
 * Created by martijn on 25-5-17.
 */
public class MyTreeListener implements TreeListener {

    private static final ParseTreeWalker walker = new ParseTreeWalker();
    private ParseTreeProperty<Boolean> ordered;
    private ParseTreeProperty<Integer> maxValue;
    private ParseTreeProperty<Integer> minValue;


    public void init() {
        ordered = new ParseTreeProperty<>();
        maxValue = new ParseTreeProperty<>();
        minValue = new ParseTreeProperty<>();
    }

    private void setOrdered(ParseTree node, boolean val) {
        this.ordered.put(node, val);
    }

    public boolean getOrdered(ParseTree node) {
        return this.ordered.get(node);
    }

    private void setMaxValue(ParseTree node, int val) {
        this.maxValue.put(node, val);
    }

    private int getMaxValue(ParseTree node) {
        return this.maxValue.get(node);
    }

    private void setMinValue(ParseTree node, int val) {
        this.minValue.put(node, val);
    }

    private int getMinValue(ParseTree node) {
        return this.minValue.get(node);
    }

//    public static boolean ordered(String input) {
//        CharStream stream = new ANTLRInputStream(input);
//        TreeLexer lexer = new TreeLexer(stream);
//
//
//        TokenStream tokens = new CommonTokenStream(lexer);
//
//        TreeParser parser = new TreeParser(tokens);
//
//        TreeListener tl = new MyTreeListener();
//        ParseTree tree = parser.top();
//
//        walker.walk(tl, );
//
//        return ordered.getOrdered(parser.top());
//    }

    @Override
    public void enterTop(TreeParser.TopContext ctx) {
        // unimplemented
    }

    @Override
    public void exitTop(TreeParser.TopContext ctx) {
        ordered.put(ctx, getOrdered(ctx.node()));
    }

    @Override
    public void enterNode(TreeParser.NodeContext ctx) {
        int num = Integer.parseInt(ctx.NUM().getText());
        if (ctx.NUM() != null) {
            setMaxValue(ctx, num);
            setMinValue(ctx, num);
            setOrdered(ctx, true);
        }
    }

    @Override
    public void exitNode(TreeParser.NodeContext ctx) {
        int num = Integer.parseInt(ctx.NUM().getText());

        if (ctx.NUM() != null) {
            setMaxValue(ctx, num);
            setMinValue(ctx, num);
            setOrdered(ctx, true);
        }
        boolean rightOrdered = true;
        boolean leftOrdered = true;

        if (ctx.RIGHT() != null) {
            rightOrdered = getMinValue(ctx.node(ctx.node().size() - 1)) > num && getOrdered(ctx.node(ctx.node().size() - 1));
            setMaxValue(ctx, getMaxValue(ctx.node(ctx.node().size() - 1)));
        }
        if (ctx.LEFT() != null) {
            leftOrdered = getMaxValue(ctx.node(0)) < num && getOrdered(ctx.node(0));
            setMinValue(ctx, getMinValue(ctx.node(0)));
        }

        setOrdered(ctx, leftOrdered && rightOrdered);

    }

    @Override
    public void visitTerminal(TerminalNode node) {
        try {
            int num = Integer.parseInt(node.getText());
            setMaxValue(node, num);
            setMinValue(node, num);
            setOrdered(node, true);

        } catch (NumberFormatException e) {
            //pass
        }

    }

    @Override
    public void visitErrorNode(ErrorNode node) {

    }

    @Override
    public void enterEveryRule(ParserRuleContext ctx) {

    }

    @Override
    public void exitEveryRule(ParserRuleContext ctx) {

    }
}
