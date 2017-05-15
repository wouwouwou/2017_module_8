package pp.block3.cc.antlr.operators;

import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeProperty;
import pp.block3.cc.antlr.Type;
import pp.block3.cc.antlr.operators.OperParser.*;

import java.math.BigInteger;

public class OperCalculator extends OperBaseListener {
    /**
     * s
     * Map storing the val attribute for all parse tree nodes.
     */
    private ParseTreeProperty<String> values;
    private ParseTreeProperty<Type> types;

    /**
     * Initialises the calculator before using it to walk a tree.
     */
    public void init() {
        values = new ParseTreeProperty<String>();
        types = new ParseTreeProperty<Type>();
    }

    @Override
    public void exitNum(NumContext ctx) {
        setValue(ctx, ctx.NUM().getText());
        setType(ctx, Type.NUM);
    }

    @Override
    public void exitBool(BoolContext ctx) {
        setValue(ctx, ctx.BOOL().getText());
        setType(ctx, Type.BOOL);
    }

    @Override
    public void exitStr(StrContext ctx) {
        setValue(ctx, ctx.STR().getText());
        setType(ctx, Type.STR);
    }

    @Override
    public void exitPlus(PlusContext ctx) {
        ParseTree t0 = ctx.t(0);
        ParseTree t1 = ctx.t(1);

        if (type(t0) == Type.NUM && type(t1) == Type.NUM) {
            setValue(ctx, new BigInteger(value(t0)).add(
                    new BigInteger(value(t1))).intValue() + "");
            setType(ctx, Type.NUM);
        } else if (type(t0) == Type.BOOL && type(t1) == Type.BOOL) {
            setValue(ctx, Boolean.toString(
                    Boolean.valueOf(value(t0)) || Boolean.valueOf(value(t1))
            ));
            setType(ctx, Type.BOOL);
        } else if (type(t0) == Type.STR && type(t1) == Type.STR) {
            setValue(ctx, "\"" + strip(value(t0)) + strip(value(t1)) + "\"");
            setType(ctx, Type.STR);
        } else {
//			throw new ParseException();
        }
    }

    @Override
    public void exitHat(HatContext ctx) {
        ParseTree t0 = ctx.t(0);
        ParseTree t1 = ctx.t(1);

        if (type(t0) == Type.NUM && type(t1) == Type.NUM) {
            setValue(ctx, new BigInteger(value(t0)).pow(
                    new BigInteger(value(t1)).intValue()).intValue() + "");
            setType(ctx, Type.NUM);
        } else if (type(t0) == Type.STR && type(t1) == Type.NUM) {
            StringBuilder result = new StringBuilder("\"");
            for (int i = 0; i < Integer.parseInt(value(t1)); i++) {
                result.append(strip(value(t0)));
            }
            setValue(ctx, result + "\"");
            setType(ctx, Type.STR);
        } else {
			System.err.println("Mag niet...");
        }
    }

    @Override
    public void exitEquals(EqualsContext ctx) {
        ParseTree t0 = ctx.t(0);
        ParseTree t1 = ctx.t(1);

        if (type(t0) == Type.NUM && type(t1) == Type.NUM) {
            setValue(ctx, Boolean.toString(Integer.parseInt(value(t0)) == Integer.parseInt(value(t1))));
            setType(ctx, Type.BOOL);
        } else if (type(t0) == Type.BOOL && type(t1) == Type.BOOL) {
            setValue(ctx, Boolean.toString(Boolean.valueOf(value(t0)) == Boolean.valueOf(value(t1))));
            setType(ctx, Type.BOOL);
        } else if (type(t0) == Type.STR && type(t1) == Type.STR) {
            setValue(ctx, Boolean.toString(value(t0).equals(value(t1))));
            setType(ctx, Type.BOOL);
        } else {
//			throw new ParseException();
        }
    }

    @Override
    public void exitParentheses(ParenthesesContext ctx) {
        setValue(ctx, value(ctx.t()));
        setType(ctx, type(ctx.t()));
    }


    private String strip(String str) {
        return str.substring(1, str.length() - 1);
    }

    /**
     * Sets the val attribute of a given node.
     */
    private void setValue(ParseTree node, String value) {
        values.put(node, value);
    }

    /**
     * Sets the val attribute of a given node.
     */
    private void setType(ParseTree node, Type value) {
        types.put(node, value);
    }

    /**
     * Retrieves the val attribute of a given node.
     */
    public String value(ParseTree node) {
        return values.get(node);
    }

    /**
     * Retrieves the val attribute of a given node.
     */
    public Type type(ParseTree node) {
        return types.get(node);
    }
}
