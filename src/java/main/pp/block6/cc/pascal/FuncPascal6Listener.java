// Generated from FuncPascal6.g4 by ANTLR 4.4
package pp.block6.cc.pascal;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link FuncPascal6Parser}.
 */
public interface FuncPascal6Listener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by the {@code assStat}
	 * labeled alternative in {@link FuncPascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void enterAssStat(@NotNull FuncPascal6Parser.AssStatContext ctx);
	/**
	 * Exit a parse tree produced by the {@code assStat}
	 * labeled alternative in {@link FuncPascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void exitAssStat(@NotNull FuncPascal6Parser.AssStatContext ctx);
	/**
	 * Enter a parse tree produced by the {@code blockStat}
	 * labeled alternative in {@link FuncPascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void enterBlockStat(@NotNull FuncPascal6Parser.BlockStatContext ctx);
	/**
	 * Exit a parse tree produced by the {@code blockStat}
	 * labeled alternative in {@link FuncPascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void exitBlockStat(@NotNull FuncPascal6Parser.BlockStatContext ctx);
	/**
	 * Enter a parse tree produced by the {@code trueExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterTrueExpr(@NotNull FuncPascal6Parser.TrueExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code trueExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitTrueExpr(@NotNull FuncPascal6Parser.TrueExprContext ctx);
	/**
	 * Enter a parse tree produced by the {@code outStat}
	 * labeled alternative in {@link FuncPascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void enterOutStat(@NotNull FuncPascal6Parser.OutStatContext ctx);
	/**
	 * Exit a parse tree produced by the {@code outStat}
	 * labeled alternative in {@link FuncPascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void exitOutStat(@NotNull FuncPascal6Parser.OutStatContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncPascal6Parser#program}.
	 * @param ctx the parse tree
	 */
	void enterProgram(@NotNull FuncPascal6Parser.ProgramContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncPascal6Parser#program}.
	 * @param ctx the parse tree
	 */
	void exitProgram(@NotNull FuncPascal6Parser.ProgramContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncPascal6Parser#body}.
	 * @param ctx the parse tree
	 */
	void enterBody(@NotNull FuncPascal6Parser.BodyContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncPascal6Parser#body}.
	 * @param ctx the parse tree
	 */
	void exitBody(@NotNull FuncPascal6Parser.BodyContext ctx);
	/**
	 * Enter a parse tree produced by the {@code boolType}
	 * labeled alternative in {@link FuncPascal6Parser#type}.
	 * @param ctx the parse tree
	 */
	void enterBoolType(@NotNull FuncPascal6Parser.BoolTypeContext ctx);
	/**
	 * Exit a parse tree produced by the {@code boolType}
	 * labeled alternative in {@link FuncPascal6Parser#type}.
	 * @param ctx the parse tree
	 */
	void exitBoolType(@NotNull FuncPascal6Parser.BoolTypeContext ctx);
	/**
	 * Enter a parse tree produced by the {@code parExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterParExpr(@NotNull FuncPascal6Parser.ParExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code parExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitParExpr(@NotNull FuncPascal6Parser.ParExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncPascal6Parser#multOp}.
	 * @param ctx the parse tree
	 */
	void enterMultOp(@NotNull FuncPascal6Parser.MultOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncPascal6Parser#multOp}.
	 * @param ctx the parse tree
	 */
	void exitMultOp(@NotNull FuncPascal6Parser.MultOpContext ctx);
	/**
	 * Enter a parse tree produced by the {@code compExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterCompExpr(@NotNull FuncPascal6Parser.CompExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code compExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitCompExpr(@NotNull FuncPascal6Parser.CompExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncPascal6Parser#block}.
	 * @param ctx the parse tree
	 */
	void enterBlock(@NotNull FuncPascal6Parser.BlockContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncPascal6Parser#block}.
	 * @param ctx the parse tree
	 */
	void exitBlock(@NotNull FuncPascal6Parser.BlockContext ctx);
	/**
	 * Enter a parse tree produced by the {@code callExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterCallExpr(@NotNull FuncPascal6Parser.CallExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code callExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitCallExpr(@NotNull FuncPascal6Parser.CallExprContext ctx);
	/**
	 * Enter a parse tree produced by the {@code falseExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterFalseExpr(@NotNull FuncPascal6Parser.FalseExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code falseExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitFalseExpr(@NotNull FuncPascal6Parser.FalseExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncPascal6Parser#plusOp}.
	 * @param ctx the parse tree
	 */
	void enterPlusOp(@NotNull FuncPascal6Parser.PlusOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncPascal6Parser#plusOp}.
	 * @param ctx the parse tree
	 */
	void exitPlusOp(@NotNull FuncPascal6Parser.PlusOpContext ctx);
	/**
	 * Enter a parse tree produced by the {@code whileStat}
	 * labeled alternative in {@link FuncPascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void enterWhileStat(@NotNull FuncPascal6Parser.WhileStatContext ctx);
	/**
	 * Exit a parse tree produced by the {@code whileStat}
	 * labeled alternative in {@link FuncPascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void exitWhileStat(@NotNull FuncPascal6Parser.WhileStatContext ctx);
	/**
	 * Enter a parse tree produced by the {@code ifStat}
	 * labeled alternative in {@link FuncPascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void enterIfStat(@NotNull FuncPascal6Parser.IfStatContext ctx);
	/**
	 * Exit a parse tree produced by the {@code ifStat}
	 * labeled alternative in {@link FuncPascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void exitIfStat(@NotNull FuncPascal6Parser.IfStatContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncPascal6Parser#boolOp}.
	 * @param ctx the parse tree
	 */
	void enterBoolOp(@NotNull FuncPascal6Parser.BoolOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncPascal6Parser#boolOp}.
	 * @param ctx the parse tree
	 */
	void exitBoolOp(@NotNull FuncPascal6Parser.BoolOpContext ctx);
	/**
	 * Enter a parse tree produced by the {@code idTarget}
	 * labeled alternative in {@link FuncPascal6Parser#target}.
	 * @param ctx the parse tree
	 */
	void enterIdTarget(@NotNull FuncPascal6Parser.IdTargetContext ctx);
	/**
	 * Exit a parse tree produced by the {@code idTarget}
	 * labeled alternative in {@link FuncPascal6Parser#target}.
	 * @param ctx the parse tree
	 */
	void exitIdTarget(@NotNull FuncPascal6Parser.IdTargetContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncPascal6Parser#var}.
	 * @param ctx the parse tree
	 */
	void enterVar(@NotNull FuncPascal6Parser.VarContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncPascal6Parser#var}.
	 * @param ctx the parse tree
	 */
	void exitVar(@NotNull FuncPascal6Parser.VarContext ctx);
	/**
	 * Enter a parse tree produced by the {@code intType}
	 * labeled alternative in {@link FuncPascal6Parser#type}.
	 * @param ctx the parse tree
	 */
	void enterIntType(@NotNull FuncPascal6Parser.IntTypeContext ctx);
	/**
	 * Exit a parse tree produced by the {@code intType}
	 * labeled alternative in {@link FuncPascal6Parser#type}.
	 * @param ctx the parse tree
	 */
	void exitIntType(@NotNull FuncPascal6Parser.IntTypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncPascal6Parser#params}.
	 * @param ctx the parse tree
	 */
	void enterParams(@NotNull FuncPascal6Parser.ParamsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncPascal6Parser#params}.
	 * @param ctx the parse tree
	 */
	void exitParams(@NotNull FuncPascal6Parser.ParamsContext ctx);
	/**
	 * Enter a parse tree produced by the {@code multExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterMultExpr(@NotNull FuncPascal6Parser.MultExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code multExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitMultExpr(@NotNull FuncPascal6Parser.MultExprContext ctx);
	/**
	 * Enter a parse tree produced by the {@code numExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterNumExpr(@NotNull FuncPascal6Parser.NumExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code numExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitNumExpr(@NotNull FuncPascal6Parser.NumExprContext ctx);
	/**
	 * Enter a parse tree produced by the {@code plusExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterPlusExpr(@NotNull FuncPascal6Parser.PlusExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code plusExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitPlusExpr(@NotNull FuncPascal6Parser.PlusExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncPascal6Parser#compOp}.
	 * @param ctx the parse tree
	 */
	void enterCompOp(@NotNull FuncPascal6Parser.CompOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncPascal6Parser#compOp}.
	 * @param ctx the parse tree
	 */
	void exitCompOp(@NotNull FuncPascal6Parser.CompOpContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncPascal6Parser#args}.
	 * @param ctx the parse tree
	 */
	void enterArgs(@NotNull FuncPascal6Parser.ArgsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncPascal6Parser#args}.
	 * @param ctx the parse tree
	 */
	void exitArgs(@NotNull FuncPascal6Parser.ArgsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncPascal6Parser#funcDecl}.
	 * @param ctx the parse tree
	 */
	void enterFuncDecl(@NotNull FuncPascal6Parser.FuncDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncPascal6Parser#funcDecl}.
	 * @param ctx the parse tree
	 */
	void exitFuncDecl(@NotNull FuncPascal6Parser.FuncDeclContext ctx);
	/**
	 * Enter a parse tree produced by the {@code prfExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterPrfExpr(@NotNull FuncPascal6Parser.PrfExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code prfExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitPrfExpr(@NotNull FuncPascal6Parser.PrfExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncPascal6Parser#prfOp}.
	 * @param ctx the parse tree
	 */
	void enterPrfOp(@NotNull FuncPascal6Parser.PrfOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncPascal6Parser#prfOp}.
	 * @param ctx the parse tree
	 */
	void exitPrfOp(@NotNull FuncPascal6Parser.PrfOpContext ctx);
	/**
	 * Enter a parse tree produced by the {@code inStat}
	 * labeled alternative in {@link FuncPascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void enterInStat(@NotNull FuncPascal6Parser.InStatContext ctx);
	/**
	 * Exit a parse tree produced by the {@code inStat}
	 * labeled alternative in {@link FuncPascal6Parser#stat}.
	 * @param ctx the parse tree
	 */
	void exitInStat(@NotNull FuncPascal6Parser.InStatContext ctx);
	/**
	 * Enter a parse tree produced by the {@code boolExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterBoolExpr(@NotNull FuncPascal6Parser.BoolExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code boolExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitBoolExpr(@NotNull FuncPascal6Parser.BoolExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncPascal6Parser#varDecl}.
	 * @param ctx the parse tree
	 */
	void enterVarDecl(@NotNull FuncPascal6Parser.VarDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncPascal6Parser#varDecl}.
	 * @param ctx the parse tree
	 */
	void exitVarDecl(@NotNull FuncPascal6Parser.VarDeclContext ctx);
	/**
	 * Enter a parse tree produced by the {@code idExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void enterIdExpr(@NotNull FuncPascal6Parser.IdExprContext ctx);
	/**
	 * Exit a parse tree produced by the {@code idExpr}
	 * labeled alternative in {@link FuncPascal6Parser#expr}.
	 * @param ctx the parse tree
	 */
	void exitIdExpr(@NotNull FuncPascal6Parser.IdExprContext ctx);
}