package pp.block5.cc.simple;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeProperty;

import pp.block5.cc.pascal.SimplePascalBaseVisitor;
import pp.block5.cc.pascal.SimplePascalParser;
import pp.iloc.Simulator;
import pp.iloc.model.*;

/** Class to generate ILOC code for Simple Pascal. */
public class Generator extends SimplePascalBaseVisitor<Op> {
	/** The representation of the boolean value <code>false</code>. */
	public final static Num FALSE_VALUE = new Num(Simulator.FALSE);
	/** The representation of the boolean value <code>true</code>. */
	public final static Num TRUE_VALUE = new Num(Simulator.TRUE);

	/** The base register. */
	private Reg arp = new Reg("r_arp");
	/** The outcome of the checker phase. */
	private Result checkResult;
	/** Association of statement nodes to labels. */
	private ParseTreeProperty<Label> labels;
	/** The program being built. */
	private Program prog;
	/** Register count, used to generate fresh registers. */
	private int regCount;
	/** Association of expression and target nodes to registers. */
	private ParseTreeProperty<Reg> regs;

	/** Generates ILOC code for a given parse tree,
	 * given a pre-computed checker result.
	 */
	public Program generate(ParseTree tree, Result checkResult) {
		this.prog = new Program();
		this.checkResult = checkResult;
		this.regs = new ParseTreeProperty<>();
		this.labels = new ParseTreeProperty<>();
		this.regCount = 0;
		tree.accept(this);
		return this.prog;
	}

	// Override the visitor methods
	/** Constructs an operation from the parameters 
	 * and adds it to the program under construction. */
	private Op emit(Label label, OpCode opCode, Operand... args) {
		Op result = new Op(label, opCode, args);
		this.prog.addInstr(result);
		return result;
	}

	/** Constructs an operation from the parameters 
	 * and adds it to the program under construction. */
	private Op emit(OpCode opCode, Operand... args) {
		return emit((Label) null, opCode, args);
	}

	/** 
	 * Looks up the label for a given parse tree node,
	 * creating it if none has been created before.
	 * The label is actually constructed from the entry node
	 * in the flow graph, as stored in the checker result.
	 */
	private Label label(ParserRuleContext node) {
		Label result = this.labels.get(node);
		if (result == null) {
			ParserRuleContext entry = this.checkResult.getEntry(node);
			result = createLabel(entry, "n");
			this.labels.put(node, result);
		}
		return result;
	}

	/** Creates a label for a given parse tree node and prefix. */
	private Label createLabel(ParserRuleContext node, String prefix) {
		Token token = node.getStart();
		int line = token.getLine();
		int column = token.getCharPositionInLine();
		String result = prefix + "_" + line + "_" + column;
		return new Label(result);
	}

	/** Retrieves the offset of a variable node from the checker result,
	 * wrapped in a {@link Num} operand. */
	private Num offset(ParseTree node) {
		return new Num(this.checkResult.getOffset(node));
	}

	/** Returns a register for a given parse tree node,
	 * creating a fresh register if there is none for that node. */
	private Reg reg(ParseTree node) {
		Reg result = this.regs.get(node);
		if (result == null) {
			result = new Reg("r_" + this.regCount);
			this.regs.put(node, result);
			this.regCount++;
		}
		return result;
	}

	/** Assigns a register to a given parse tree node. */
	private void setReg(ParseTree node, Reg reg) {
		this.regs.put(node, reg);
	}

    @Override
    public Op visitProgram(SimplePascalParser.ProgramContext ctx) {
        return visit(ctx.body());
    }

    @Override
    public Op visitBody(SimplePascalParser.BodyContext ctx) {
        return visit(ctx.block());
    }

    @Override
    public Op visitBlock(SimplePascalParser.BlockContext ctx) {
        Op result = visit(ctx.stat(0));
        for (int i = 1; i < ctx.stat().size(); i++) {
            result = visit(ctx.stat(i));
        }
        return result;
    }

    @Override
    public Op visitAssStat(SimplePascalParser.AssStatContext ctx) {
        visit(ctx.expr());
        return emit(OpCode.storeAI, reg(ctx.expr()), arp, offset(ctx.target()));
    }

    @Override
    public Op visitIfStat(SimplePascalParser.IfStatContext ctx) {
        Op result = visit(ctx.expr());
        Label lthen = createLabel(ctx, "then");
        Label lend = createLabel(ctx, "end");
        if (ctx.stat().size() > 1) {
            Label lelse = createLabel(ctx, "else");
            emit(OpCode.cbr, reg(ctx.expr()), lthen, lelse);
            emit(lthen, OpCode.nop);
            visit(ctx.stat(0));
            emit(OpCode.jumpI, lend);
            emit(lelse, OpCode.nop);
            visit(ctx.stat(1));
        } else {
            emit(OpCode.cbr, reg(ctx.expr()), lthen, lend);
            emit(lthen, OpCode.nop);
            visit(ctx.stat(0));
        }
        emit(lend, OpCode.nop);
        return result;
    }

    @Override
    public Op visitWhileStat(SimplePascalParser.WhileStatContext ctx) {
        Label lwhile = createLabel(ctx, "while");
        Label ldo = createLabel(ctx, "do");
        Label lend = createLabel(ctx, "end");
        emit(lwhile, OpCode.nop);
        Op result = visit(ctx.expr());
        emit(OpCode.cbr, reg(ctx.expr()), ldo, lend);
        emit(ldo, OpCode.nop);
        visit(ctx.stat());
        emit(OpCode.jumpI, lwhile);
        emit(lend, OpCode.nop);
        return result;
    }

    @Override
    public Op visitInStat(SimplePascalParser.InStatContext ctx) {
        String input = ctx.STR().getText().replaceAll("\"", "");
        Op result = emit(OpCode.in, new Str(input), reg(ctx));
        emit(OpCode.storeAI, reg(ctx), arp, offset(ctx.target()));
        return result;
    }

    @Override
    public Op visitOutStat(SimplePascalParser.OutStatContext ctx) {
        String output = ctx.STR().getText().replaceAll("\"", "");
        visit(ctx.expr());
        return emit(OpCode.out, new Str(output), reg(ctx.expr()));
    }

    @Override
    public Op visitPrfExpr(SimplePascalParser.PrfExprContext ctx) {
        visit(ctx.expr());
        return (ctx.prfOp().MINUS() != null)
                ? emit(OpCode.multI, reg(ctx.expr()), new Num(-1), reg(ctx))
                : emit(OpCode.xorI, reg(ctx.expr()), TRUE_VALUE, reg(ctx));
    }

    @Override
    public Op visitMultExpr(SimplePascalParser.MultExprContext ctx) {
        visit(ctx.expr(0));
        visit(ctx.expr(1));
        return (ctx.multOp().STAR() != null)
                ? emit(OpCode.mult, reg(ctx.expr(0)), reg(ctx.expr(1)), reg(ctx))
                : emit(OpCode.div, reg(ctx.expr(0)), reg(ctx.expr(1)), reg(ctx));
    }

    @Override
    public Op visitPlusExpr(SimplePascalParser.PlusExprContext ctx) {
        visit(ctx.expr(0));
        visit(ctx.expr(1));
        return (ctx.plusOp().PLUS() != null)
                ? emit(OpCode.add, reg(ctx.expr(0)), reg(ctx.expr(1)), reg(ctx))
                : emit(OpCode.sub, reg(ctx.expr(0)), reg(ctx.expr(1)), reg(ctx));
    }

    @Override
    public Op visitCompExpr(SimplePascalParser.CompExprContext ctx) {
        visit(ctx.expr(0));
        visit(ctx.expr(1));
        switch (ctx.compOp().getStart().getType()) {
            case SimplePascalParser.LE:
                return emit(OpCode.cmp_LE, reg(ctx.expr(0)), reg(ctx.expr(1)), reg(ctx));
            case SimplePascalParser.LT:
                return emit(OpCode.cmp_LT, reg(ctx.expr(0)), reg(ctx.expr(1)), reg(ctx));
            case SimplePascalParser.GE:
                return emit(OpCode.cmp_GE, reg(ctx.expr(0)), reg(ctx.expr(1)), reg(ctx));
            case SimplePascalParser.GT:
                return emit(OpCode.cmp_GT, reg(ctx.expr(0)), reg(ctx.expr(1)), reg(ctx));
            case SimplePascalParser.EQ:
                return emit(OpCode.cmp_EQ, reg(ctx.expr(0)), reg(ctx.expr(1)), reg(ctx));
            case SimplePascalParser.NE:
                return emit(OpCode.cmp_NE, reg(ctx.expr(0)), reg(ctx.expr(1)), reg(ctx));
            default:
                return null;
        }
    }

    @Override
    public Op visitBoolExpr(SimplePascalParser.BoolExprContext ctx) {
        visit(ctx.expr(0));
        visit(ctx.expr(1));
        return (ctx.boolOp().AND() != null)
                ? emit(OpCode.and, reg(ctx.expr(0)), reg(ctx.expr(1)), reg(ctx))
                : emit(OpCode.or, reg(ctx.expr(0)), reg(ctx.expr(1)), reg(ctx));
    }

    @Override
    public Op visitParExpr(SimplePascalParser.ParExprContext ctx) {
        setReg(ctx, reg(ctx.expr()));
        return visit(ctx.expr());
    }

    @Override
    public Op visitIdExpr(SimplePascalParser.IdExprContext ctx) {
        return emit(OpCode.loadAI, arp, offset(ctx), reg(ctx));
    }

    @Override
    public Op visitNumExpr(SimplePascalParser.NumExprContext ctx) {
        return emit(OpCode.loadI, new Num(Integer.parseInt(ctx.NUM().getText())), reg(ctx));
    }

    @Override
    public Op visitTrueExpr(SimplePascalParser.TrueExprContext ctx) {
        return emit(OpCode.loadI, TRUE_VALUE, reg(ctx));
    }

    @Override
    public Op visitFalseExpr(SimplePascalParser.FalseExprContext ctx) {
        return emit(OpCode.loadI, FALSE_VALUE, reg(ctx));
    }
}
