// Generated from SimplePascal6.g4 by ANTLR 4.4
package pp.block6.cc.pascal;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link SimplePascal6Parser}.
 */
public interface SimplePascal6Listener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by the {@code assStat}
	 * labeled alternative in {@link SimplePascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void enterAssStat(@NotNull SimplePascal6Parser.AssStatContext ctx);
	/**
	 * Exit a parse tree produced by the {@code assStat}
	 * labeled alternative in {@link SimplePascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void exitAssStat(@NotNull SimplePascal6Parser.AssStatContext ctx);
	/**
	 * Enter a parse tree produced by the {@code blockStat}
	 * labeled alternative in {@link SimplePascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void enterBlockStat(@NotNull SimplePascal6Parser.BlockStatContext ctx);
	/**
	 * Exit a parse tree produced by the {@code blockStat}
	 * labeled alternative in {@link SimplePascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void exitBlockStat(@NotNull SimplePascal6Parser.BlockStatContext ctx);
	/**
	 * Enter a parse tree produced by the {@code trueExpr}
	 * labeled alternative in {@link SimplePascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterTrueExpr(@NotNull SimplePascal6Parser.TrueExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code trueExpr}
	 * labeled alternative in {@link SimplePascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitTrueExpr(@NotNull SimplePascal6Parser.TrueExprContext ctx);
	/**
	 * Enter a parse tree produced by the {@code outStat}
	 * labeled alternative in {@link SimplePascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void enterOutStat(@NotNull SimplePascal6Parser.OutStatContext ctx);
	/**
	 * Exit a parse tree produced by the {@code outStat}
	 * labeled alternative in {@link SimplePascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void exitOutStat(@NotNull SimplePascal6Parser.OutStatContext ctx);
	/**
	 * Enter a parse tree produced by {@link SimplePascal6Parser#program}.
	 * @param ctx the parse tree
	 */
	void enterProgram(@NotNull SimplePascal6Parser.ProgramContext ctx);
	/**
	 * Exit a parse tree produced by {@link SimplePascal6Parser#program}.
	 * @param ctx the parse tree
	 */
	void exitProgram(@NotNull SimplePascal6Parser.ProgramContext ctx);
	/**
	 * Enter a parse tree produced by {@link SimplePascal6Parser#body}.
	 * @param ctx the parse tree
	 */
	void enterBody(@NotNull SimplePascal6Parser.BodyContext ctx);
	/**
	 * Exit a parse tree produced by {@link SimplePascal6Parser#body}.
	 * @param ctx the parse tree
	 */
	void exitBody(@NotNull SimplePascal6Parser.BodyContext ctx);
	/**
	 * Enter a parse tree produced by the {@code boolType}
	 * labeled alternative in {@link SimplePascal6Parser#type}.
	 * @param ctx the parse tree
	 */
	void enterBoolType(@NotNull SimplePascal6Parser.BoolTypeContext ctx);
	/**
	 * Exit a parse tree produced by the {@code boolType}
	 * labeled alternative in {@link SimplePascal6Parser#type}.
	 * @param ctx the parse tree
	 */
	void exitBoolType(@NotNull SimplePascal6Parser.BoolTypeContext ctx);
	/**
	 * Enter a parse tree produced by the {@code parExpr}
	 * labeled alternative in {@link SimplePascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterParExpr(@NotNull SimplePascal6Parser.ParExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code parExpr}
	 * labeled alternative in {@link SimplePascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitParExpr(@NotNull SimplePascal6Parser.ParExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link SimplePascal6Parser#multOp}.
	 * @param ctx the parse tree
	 */
	void enterMultOp(@NotNull SimplePascal6Parser.MultOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link SimplePascal6Parser#multOp}.
	 * @param ctx the parse tree
	 */
	void exitMultOp(@NotNull SimplePascal6Parser.MultOpContext ctx);
	/**
	 * Enter a parse tree produced by the {@code compExpr}
	 * labeled alternative in {@link SimplePascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterCompExpr(@NotNull SimplePascal6Parser.CompExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code compExpr}
	 * labeled alternative in {@link SimplePascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitCompExpr(@NotNull SimplePascal6Parser.CompExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link SimplePascal6Parser#block}.
	 * @param ctx the parse tree
	 */
	void enterBlock(@NotNull SimplePascal6Parser.BlockContext ctx);
	/**
	 * Exit a parse tree produced by {@link SimplePascal6Parser#block}.
	 * @param ctx the parse tree
	 */
	void exitBlock(@NotNull SimplePascal6Parser.BlockContext ctx);
	/**
	 * Enter a parse tree produced by the {@code falseExpr}
	 * labeled alternative in {@link SimplePascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterFalseExpr(@NotNull SimplePascal6Parser.FalseExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code falseExpr}
	 * labeled alternative in {@link SimplePascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitFalseExpr(@NotNull SimplePascal6Parser.FalseExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link SimplePascal6Parser#plusOp}.
	 * @param ctx the parse tree
	 */
	void enterPlusOp(@NotNull SimplePascal6Parser.PlusOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link SimplePascal6Parser#plusOp}.
	 * @param ctx the parse tree
	 */
	void exitPlusOp(@NotNull SimplePascal6Parser.PlusOpContext ctx);
	/**
	 * Enter a parse tree produced by the {@code whileStat}
	 * labeled alternative in {@link SimplePascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void enterWhileStat(@NotNull SimplePascal6Parser.WhileStatContext ctx);
	/**
	 * Exit a parse tree produced by the {@code whileStat}
	 * labeled alternative in {@link SimplePascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void exitWhileStat(@NotNull SimplePascal6Parser.WhileStatContext ctx);
	/**
	 * Enter a parse tree produced by the {@code ifStat}
	 * labeled alternative in {@link SimplePascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void enterIfStat(@NotNull SimplePascal6Parser.IfStatContext ctx);
	/**
	 * Exit a parse tree produced by the {@code ifStat}
	 * labeled alternative in {@link SimplePascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void exitIfStat(@NotNull SimplePascal6Parser.IfStatContext ctx);
	/**
	 * Enter a parse tree produced by {@link SimplePascal6Parser#boolOp}.
	 * @param ctx the parse tree
	 */
	void enterBoolOp(@NotNull SimplePascal6Parser.BoolOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link SimplePascal6Parser#boolOp}.
	 * @param ctx the parse tree
	 */
	void exitBoolOp(@NotNull SimplePascal6Parser.BoolOpContext ctx);
	/**
	 * Enter a parse tree produced by the {@code idTarget}
	 * labeled alternative in {@link SimplePascal6Parser#target}.
	 * @param ctx the parse tree
	 */
	void enterIdTarget(@NotNull SimplePascal6Parser.IdTargetContext ctx);
	/**
	 * Exit a parse tree produced by the {@code idTarget}
	 * labeled alternative in {@link SimplePascal6Parser#target}.
	 * @param ctx the parse tree
	 */
	void exitIdTarget(@NotNull SimplePascal6Parser.IdTargetContext ctx);
	/**
	 * Enter a parse tree produced by {@link SimplePascal6Parser#var}.
	 * @param ctx the parse tree
	 */
	void enterVar(@NotNull SimplePascal6Parser.VarContext ctx);
	/**
	 * Exit a parse tree produced by {@link SimplePascal6Parser#var}.
	 * @param ctx the parse tree
	 */
	void exitVar(@NotNull SimplePascal6Parser.VarContext ctx);
	/**
	 * Enter a parse tree produced by the {@code intType}
	 * labeled alternative in {@link SimplePascal6Parser#type}.
	 * @param ctx the parse tree
	 */
	void enterIntType(@NotNull SimplePascal6Parser.IntTypeContext ctx);
	/**
	 * Exit a parse tree produced by the {@code intType}
	 * labeled alternative in {@link SimplePascal6Parser#type}.
	 * @param ctx the parse tree
	 */
	void exitIntType(@NotNull SimplePascal6Parser.IntTypeContext ctx);
	/**
	 * Enter a parse tree produced by the {@code multExpr}
	 * labeled alternative in {@link SimplePascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterMultExpr(@NotNull SimplePascal6Parser.MultExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code multExpr}
	 * labeled alternative in {@link SimplePascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitMultExpr(@NotNull SimplePascal6Parser.MultExprContext ctx);
	/**
	 * Enter a parse tree produced by the {@code numExpr}
	 * labeled alternative in {@link SimplePascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterNumExpr(@NotNull SimplePascal6Parser.NumExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code numExpr}
	 * labeled alternative in {@link SimplePascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitNumExpr(@NotNull SimplePascal6Parser.NumExprContext ctx);
	/**
	 * Enter a parse tree produced by the {@code plusExpr}
	 * labeled alternative in {@link SimplePascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterPlusExpr(@NotNull SimplePascal6Parser.PlusExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code plusExpr}
	 * labeled alternative in {@link SimplePascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitPlusExpr(@NotNull SimplePascal6Parser.PlusExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link SimplePascal6Parser#compOp}.
	 * @param ctx the parse tree
	 */
	void enterCompOp(@NotNull SimplePascal6Parser.CompOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link SimplePascal6Parser#compOp}.
	 * @param ctx the parse tree
	 */
	void exitCompOp(@NotNull SimplePascal6Parser.CompOpContext ctx);
	/**
	 * Enter a parse tree produced by the {@code prfExpr}
	 * labeled alternative in {@link SimplePascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterPrfExpr(@NotNull SimplePascal6Parser.PrfExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code prfExpr}
	 * labeled alternative in {@link SimplePascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitPrfExpr(@NotNull SimplePascal6Parser.PrfExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link SimplePascal6Parser#prfOp}.
	 * @param ctx the parse tree
	 */
	void enterPrfOp(@NotNull SimplePascal6Parser.PrfOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link SimplePascal6Parser#prfOp}.
	 * @param ctx the parse tree
	 */
	void exitPrfOp(@NotNull SimplePascal6Parser.PrfOpContext ctx);
	/**
	 * Enter a parse tree produced by the {@code inStat}
	 * labeled alternative in {@link SimplePascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void enterInStat(@NotNull SimplePascal6Parser.InStatContext ctx);
	/**
	 * Exit a parse tree produced by the {@code inStat}
	 * labeled alternative in {@link SimplePascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void exitInStat(@NotNull SimplePascal6Parser.InStatContext ctx);
	/**
	 * Enter a parse tree produced by the {@code boolExpr}
	 * labeled alternative in {@link SimplePascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterBoolExpr(@NotNull SimplePascal6Parser.BoolExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code boolExpr}
	 * labeled alternative in {@link SimplePascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitBoolExpr(@NotNull SimplePascal6Parser.BoolExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link SimplePascal6Parser#varDecl}.
	 * @param ctx the parse tree
	 */
	void enterVarDecl(@NotNull SimplePascal6Parser.VarDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link SimplePascal6Parser#varDecl}.
	 * @param ctx the parse tree
	 */
	void exitVarDecl(@NotNull SimplePascal6Parser.VarDeclContext ctx);
	/**
	 * Enter a parse tree produced by the {@code idExpr}
	 * labeled alternative in {@link SimplePascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterIdExpr(@NotNull SimplePascal6Parser.IdExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code idExpr}
	 * labeled alternative in {@link SimplePascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitIdExpr(@NotNull SimplePascal6Parser.IdExprContext ctx);
}