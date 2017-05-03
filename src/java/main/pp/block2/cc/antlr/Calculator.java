package pp.block2.cc.antlr;

import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeProperty;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.math.BigInteger;

/**
 * Created by Wouter on 3-5-2017.
 */
public class Calculator extends ExprBaseListener {
    private ParseTreeProperty<BigInteger> treeprop;
    BigInteger result;

    public Calculator() {

    }

    public BigInteger calculate(Lexer lexer) {
        this.treeprop = new ParseTreeProperty<>();
        ExprParser parser = new ExprParser(new CommonTokenStream(lexer));
        ParseTree tree = parser.expr();
        new ParseTreeWalker().walk(this, tree);
        return result;
    }

    @Override
    public void exitGoal(ExprParser.GoalContext ctx) {
        super.exitGoal(ctx);
    }

    @Override
    public void exitExpNum(ExprParser.ExpNumContext ctx) {
        treeprop.put(ctx, treeprop.get(ctx.num()));
        result = treeprop.get(ctx);
    }

    @Override
    public void exitExprExp(ExprParser.ExprExpContext ctx) {
        treeprop.put(ctx, treeprop.get(ctx.expr(0)).pow(treeprop.get(ctx.expr(1)).intValue()));
        result = treeprop.get(ctx);
    }

    @Override
    public void exitExprPar(ExprParser.ExprParContext ctx) {
        treeprop.put(ctx, treeprop.get(ctx.expr()));
        result = treeprop.get(ctx);
    }

    @Override
    public void exitExpPM(ExprParser.ExpPMContext ctx) {
        if (ctx.PLUS() == null) {
            treeprop.put(ctx, treeprop.get(ctx.expr(0)).subtract(treeprop.get(ctx.expr(1))));
        } else {
            treeprop.put(ctx, treeprop.get(ctx.expr(0)).add(treeprop.get(ctx.expr(1))));
        }
        result = treeprop.get(ctx);
    }

    @Override
    public void exitExpMul(ExprParser.ExpMulContext ctx) {
        treeprop.put(ctx, treeprop.get(ctx.expr(0)).multiply(treeprop.get(ctx.expr(1))));
        result = treeprop.get(ctx);
    }

    @Override
    public void exitNumPos(ExprParser.NumPosContext ctx) {
        treeprop.put(ctx, treeprop.get(ctx.DIG()));
    }

    @Override
    public void exitNumNeg(ExprParser.NumNegContext ctx) {
        treeprop.put(ctx, treeprop.get(ctx.num()).negate());
    }

    @Override
    public void visitTerminal(TerminalNode node) {
        if(!"+-*^()".contains(node.getText())) {
            treeprop.put(node, new BigInteger(node.getText()));
        }
    }
}
