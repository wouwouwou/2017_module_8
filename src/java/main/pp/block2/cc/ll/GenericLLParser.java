package pp.block2.cc.ll;

import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Token;
import pp.block2.cc.*;

import java.util.*;

/**
 * Generic table-driven LL(1)-parser.
 */
public class GenericLLParser implements Parser {
    private final Grammar g;
    private final LLCalc calc;
    private int index;
    private List<? extends Token> tokens;
    /**
     * Map from non-terminals to lists of rules indexed by token type.
     */
    private Map<NonTerm, List<Rule>> ll1Table;

    public GenericLLParser(Grammar g) {
        this.g = g;
        this.calc = new MyLLCalc(g); // here use your own class
    }

    @Override
    public AST parse(Lexer lexer) throws ParseException {
        this.tokens = lexer.getAllTokens();
        this.index = 0;
        return parse(g.getStart());
    }

    /**
     * Parses the start of the token stream according to a given
     * symbol. If it is a terminal, that should be the first token;
     * otherwise, it is a non-terminal that should be expanded
     * according to the next token in the token stream, using the
     * FIRST+-lookup table and recursively calling {@link #parse(Rule)}
     *
     * @param symb the symbol according to which the token stream
     *             should be parsed
     * @return the sub-AST resulting from the parsing of symb;
     * or null if the symbol expands to the empty string
     * @throws ParseException if the symbol cannot be parsed
     *                        because the token stream does not contain the expected symbols
     */
    private AST parse(Symbol symb) throws ParseException {
        AST result;
        if (symb instanceof Term) {
            if (peek().getType() == ((Term) symb).getTokenType()) {
                result = new AST((Term) symb, next());
            } else {
                throw new ParseException();
            }
        } else {
            result = parse(lookup((NonTerm) symb));
        }
        return result;
    }

    /**
     * Parses the start of the token stream according to a given
     * rule, recursively calling {@link #parse(Symbol)} to process
     * the RHS.
     *
     * @return the sub-AST resulting from the parsing of the rule.
     * The top node is the node for the LHS of the rule, its direct
     * children correspond to the symbols of the rule's RHS.
     * @throws ParseException if the symbol cannot be parsed
     *                        because the token stream does not contain the expected symbols
     */
    private AST parse(Rule rule) throws ParseException {
        AST result = new AST(rule.getLHS());
        for (Symbol s : rule.getRHS()) {
            result.addChild(parse(s));
        }
        return result;
    }

    /**
     * Returns the next token, without moving the token index.
     */
    private Token peek() throws ParseException {
        if (index >= tokens.size()) {
            throw new ParseException("Reading beyond end of input");
        }
        return tokens.get(index);
    }

    /**
     * Returns the next token and moves up the token index.
     */
    private Token next() throws ParseException {
        Token result = peek();
        index++;
        return result;
    }

    /**
     * Uses the lookup table to look up the rule to which
     * a given nonterminal should be expanded.
     * The next rule is determined by the next token, using the
     * FIRST+-set of the nonterminal.
     *
     * @throws ParseException if the lookup table does not
     *                        contain a rule for the nonterminal in combination with
     *                        the first token in the token stream.
     */
    private Rule lookup(NonTerm nt) throws ParseException {
        Token next = peek();
        Rule result = getLL1Table().get(nt).get(next.getType());
        if (result == null) {
            throw new ParseException(String.format(
                    "Line %d:%d - no rule for '%s' on token '%s'",
                    next.getLine(), next.getCharPositionInLine(),
                    nt.getName(), next));
        }
        return result;
    }

    /**
     * Lazily builds and then returns the lookup table.
     */
    private Map<NonTerm, List<Rule>> getLL1Table() {
        if (ll1Table == null) {
            ll1Table = calcLL1Table();
        }
        return ll1Table;
    }

    /**
     * Constructs the {@link #ll1Table}.
     */
    private Map<NonTerm, List<Rule>> calcLL1Table() {
        Map<NonTerm, List<Rule>> ll1Table = new HashMap<NonTerm, List<Rule>>();

        for (NonTerm n : g.getNonterminals()) {
            List<Rule> column = new ArrayList<Rule>();
            for (int i = 0; i < g.getTerminals().size() + 1; i++) {
                column.add(null);
            }
            for (Rule rule : g.getRules(n)) {
                Set<Term> firstp = calc.getFirstp().get(rule);

                for (Term term : firstp) {
                    column.set(term.getTokenType(), rule);
                }
            }
            ll1Table.put(n, column);
        }


        for (NonTerm n : ll1Table.keySet()) {
            System.out.println(ll1Table.get(n));
        }

        return ll1Table;
    }
}