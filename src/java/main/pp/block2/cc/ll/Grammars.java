/**
 * 
 */
package pp.block2.cc.ll;

import pp.block2.cc.NonTerm;
import pp.block2.cc.Symbol;
import pp.block2.cc.SymbolFactory;
import pp.block2.cc.Term;



/**
 * Class containing some example grammars.
 * @author Arend Rensink
 *
 */
public class Grammars {
	/** Returns a grammar for simple English sentences. */
	public static Grammar makeSentence() {
		// Define the non-terminals
		NonTerm sent = new NonTerm("Sentence");
		NonTerm subj = new NonTerm("Subject");
		NonTerm obj = new NonTerm("Object");
		NonTerm mod = new NonTerm("Modifier");
		// Define the terminals, using the Sentence.g4 lexer grammar
		// Make sure you take the token constantss from the right class!
		SymbolFactory fact = new SymbolFactory(Sentence.class);
		Term noun = fact.getTerminal(Sentence.NOUN);
		Term verb = fact.getTerminal(Sentence.VERB);
		Term adj = fact.getTerminal(Sentence.ADJECTIVE);
		Term end = fact.getTerminal(Sentence.ENDMARK);
		// Build the context free grammar
		Grammar g = new Grammar(sent);
		g.addRule(sent, subj, verb, obj, end);
		g.addRule(subj, noun);
		g.addRule(subj, mod, subj);
		g.addRule(obj, noun);
		g.addRule(obj, mod, obj);
		g.addRule(mod, adj);
		return g;
	}

    public static Grammar makeIf() {
        // Define the non-terminals
        NonTerm stat = new NonTerm("Stat");
        NonTerm elsepart = new NonTerm("ElsePart");
        // Define the terminals using the If.g4 lexer grammar
        SymbolFactory fact = new SymbolFactory(If.class);
        Term tIf = fact.getTerminal(If.IF);
        Term tThen = fact.getTerminal(If.THEN);
        Term tCond = fact.getTerminal(If.COND);
        Term tElse = fact.getTerminal(If.ELSE);
        Term tAssign = fact.getTerminal(If.ASSIGN);
        Term empty = Symbol.EMPTY;
        // Build the context free grammar
        Grammar g = new Grammar(stat);
        g.addRule(stat, tAssign);
        g.addRule(stat, tIf, tCond, tThen, stat, elsepart);
        g.addRule(elsepart, tElse, stat);
        g.addRule(elsepart, empty);
        return g;
    }

    public static Grammar makeAbc() {
        // Define the non-terminals
        NonTerm l = new NonTerm("L");
        NonTerm r = new NonTerm("R");
        NonTerm q = new NonTerm("Q");
        NonTerm rr = new NonTerm("R'");
        NonTerm qq = new NonTerm("Q'");
        // Define the terminals using the Abc.g4 lexer grammar
        SymbolFactory fact = new SymbolFactory(Abc.class);
        Term a = fact.getTerminal(Abc.A);
        Term b = fact.getTerminal(Abc.B);
        Term c = fact.getTerminal(Abc.C);
        Term empty = Symbol.EMPTY;
        // Build the context free grammar
        Grammar g = new Grammar(l);
        g.addRule(l, r, a);
        g.addRule(l, q, b, a);
        g.addRule(r, a, b, a, rr);
        g.addRule(r, c, a, b, a, rr);
        g.addRule(rr, b, c, rr);
        g.addRule(rr, empty);
        g.addRule(q, b, qq);
        g.addRule(qq, b, c);
        g.addRule(qq, c);
        return g;
    }

    public static Grammar makeClass() {
	    NonTerm c = new NonTerm("C");
        NonTerm p = new NonTerm("P");
        NonTerm x = new NonTerm("X");
        NonTerm y = new NonTerm("Y");
        NonTerm i = new NonTerm("I");

        SymbolFactory fact = new SymbolFactory(Claz.class);
        Term claz = fact.getTerminal(Claz.CLASS);
        Term publik = fact.getTerminal(Claz.PUBLIC);
        Term extendz = fact.getTerminal(Claz.EXTENDS);
        Term implementz = fact.getTerminal(Claz.IMPLEMENTS);
        Term comma = fact.getTerminal(Claz.COMMA);
        Term id = fact.getTerminal(Claz.ID);
        Term empty = Symbol.EMPTY;

        Grammar g = new Grammar(c);
        g.addRule(c, p, claz, id, x, y);
        g.addRule(p, publik);
        g.addRule(p, empty);
        g.addRule(x, extendz, id);
        g.addRule(x, empty);
        g.addRule(y, implementz, i, id);
        g.addRule(y, empty);
        g.addRule(i, id, comma, i);
        g.addRule(i, empty);

        return g;
    }

}
