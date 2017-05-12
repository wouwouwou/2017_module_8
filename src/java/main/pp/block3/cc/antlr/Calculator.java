package pp.block3.cc.antlr;

import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeProperty;
import pp.block3.cc.antlr.CalcParser.*;

public class Calculator extends CalcBaseListener {
	/**
	 * Map storing the val attribute for all parse tree nodes.
	 */
	private ParseTreeProperty<Integer> vals;

	/**
	 * Initialises the calculator before using it to walk a tree.
	 */
	public void init() {
		vals = new ParseTreeProperty<Integer>();
	}

	@Override
	public void exitPar(ParContext ctx) {
		set(ctx, val(ctx.expr()));
	}

	@Override
	public void exitNumber(NumberContext ctx) {
		set(ctx, Integer.parseInt(ctx.NUMBER().getText()));
	}

	@Override
	public void exitTimes(TimesContext ctx) {
		set(ctx, val(ctx.expr(0)) * val(ctx.expr(1)));
	}

	@Override
	public void exitPlus(PlusContext ctx) {
		set(ctx, val(ctx.expr(0)) + val(ctx.expr(1)));
	}

	@Override
	public void exitNeg(NegContext ctx) {
		set(ctx, -Integer.parseInt(ctx.NUMBER().getText()));
	}

	/**
	 * Sets the val attribute of a given node.
	 */
	private void set(ParseTree node, int val) {
		vals.put(node, val);
	}

	/**
	 * Retrieves the val attribute of a given node.
	 */
	public int val(ParseTree node) {
		return vals.get(node);
	}
}
