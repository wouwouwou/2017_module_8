package pp.block2.cc.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.*;

import org.junit.Test;

import pp.block2.cc.NonTerm;
import pp.block2.cc.Symbol;
import pp.block2.cc.Term;
import pp.block2.cc.ll.*;

public class LLCalcTest {
    Grammar sentenceG = Grammars.makeSentence();
    // Define the non-terminals
    NonTerm subj = sentenceG.getNonterminal("Subject");
    NonTerm obj = sentenceG.getNonterminal("Object");
    NonTerm sent = sentenceG.getNonterminal("Sentence");
    NonTerm mod = sentenceG.getNonterminal("Modifier");
    // Define the terminals
    Term adj = sentenceG.getTerminal(Sentence.ADJECTIVE);
    Term noun = sentenceG.getTerminal(Sentence.NOUN);
    Term verb = sentenceG.getTerminal(Sentence.VERB);
    Term end = sentenceG.getTerminal(Sentence.ENDMARK);
    // Now add the last rule, causing the grammar to fail
    Grammar sentenceXG = Grammars.makeSentence();
    private Map<Symbol, Set<Term>> first;

    {
        sentenceXG.addRule(mod, mod, mod);
    }

    LLCalc sentenceXLL = createCalc(sentenceXG);

    /**
     * Tests the LL-calculator for the Sentence grammar.
     */
    @Test
    public void testSentenceOrigLL1() {
        // Without the last (recursive) rule, the grammar is LL-1
        assertTrue(createCalc(sentenceG).isLL1());
    }

    @Test
    public void testSentenceXFirst() {
        Map<Symbol, Set<Term>> first = sentenceXLL.getFirst();
        assertEquals(set(adj, noun), first.get(sent));
        assertEquals(set(adj, noun), first.get(subj));
        assertEquals(set(adj, noun), first.get(obj));
        assertEquals(set(adj), first.get(mod));
    }

    @Test
    public void testSentenceXFollow() {
        // FOLLOW sets
        Map<NonTerm, Set<Term>> follow = sentenceXLL.getFollow();
        assertEquals(set(Symbol.EOF), follow.get(sent));
        assertEquals(set(verb), follow.get(subj));
        assertEquals(set(end), follow.get(obj));
        assertEquals(set(noun, adj), follow.get(mod));
    }

    @Test
    public void testSentenceXFirstPlus() {
        // Test per rule
        Map<Rule, Set<Term>> firstp = sentenceXLL.getFirstp();
        List<Rule> subjRules = sentenceXG.getRules(subj);
        assertEquals(set(noun), firstp.get(subjRules.get(0)));
        assertEquals(set(adj), firstp.get(subjRules.get(1)));
    }

    @Test
    public void testSentenceXLL1() {
        assertFalse(sentenceXLL.isLL1());
    }

    Grammar g = Grammars.makeIf();
    // Define the non-terminals
    NonTerm stat = g.getNonterminal("Stat");
    NonTerm elsepart = g.getNonterminal("ElsePart");
    // Define the terminals
    Term empty = Symbol.EMPTY;
    Term eof = Symbol.EOF;
    Term ifT = g.getTerminal(If.IF);
    Term condT = g.getTerminal(If.COND);
    Term thenT = g.getTerminal(If.THEN);
    Term elseT = g.getTerminal(If.ELSE);
    Term assignT = g.getTerminal(If.ASSIGN);
    LLCalc calc = createCalc(g);

    @Test
    public void testIfFirst() {
        Map<Symbol, Set<Term>> first = calc.getFirst();
        assertEquals(new HashSet<>(Collections.singletonList(assignT)),
                first.get(assignT));
        assertEquals(new HashSet<>(Collections.singletonList(ifT)),
                first.get(ifT));
        assertEquals(new HashSet<>(Collections.singletonList(condT)),
                first.get(condT));
        assertEquals(new HashSet<>(Collections.singletonList(thenT)),
                first.get(thenT));
        assertEquals(new HashSet<>(Collections.singletonList(elseT)),
                first.get(elseT));
        assertEquals(new HashSet<>(Collections.singletonList(eof)),
                first.get(eof));
        assertEquals(new HashSet<>(Collections.singletonList(empty)),
                first.get(empty));
        assertEquals(new HashSet<>(Arrays.asList(assignT, ifT)),
                first.get(stat));
        assertEquals(new HashSet<>(Arrays.asList(elseT, empty)),
                first.get(elsepart));
    }

    @Test
    public void testIfFollow() {
        Map<NonTerm, Set<Term>> follow = calc.getFollow();
        assertEquals(new HashSet<>(Arrays.asList(elseT, eof)), follow.get(stat));
        assertEquals(new HashSet<>(Arrays.asList(elseT, eof)),
                follow.get(elsepart));
    }

    @Test
    public void testIfFirstPlus() {
        Map<Rule, Set<Term>> firstp = calc.getFirstp();
        List<Rule> statRules = g.getRules(stat);
        List<Rule> elsePartRules = g.getRules(elsepart);
        assertEquals(new HashSet<>(Collections.singletonList(assignT)),
                firstp.get(statRules.get(0)));
        assertEquals(new HashSet<>(Collections.singletonList(ifT)),
                firstp.get(statRules.get(1)));
        assertEquals(new HashSet<>(Collections.singletonList(elseT)),
                firstp.get(elsePartRules.get(0)));
        assertEquals(new HashSet<>(Arrays.asList(empty, elseT, eof)),
                firstp.get(elsePartRules.get(1)));
    }

    @Test
    public void testIfLL1() {
        assertFalse(calc.isLL1());
    }

    @Test
    public void testAbc() {
        Grammar g = Grammars.makeAbc();
        // Define the non-terminals
        NonTerm l = g.getNonterminal("L");
        NonTerm r = g.getNonterminal("R");
        NonTerm q = g.getNonterminal("Q");
        NonTerm rr = g.getNonterminal("R'");
        NonTerm qq = g.getNonterminal("Q'");
        // Define the terminals
        Term empty = Symbol.EMPTY;
        Term eof = Symbol.EOF;
        Term a = g.getTerminal(Abc.A);
        Term b = g.getTerminal(Abc.B);
        Term c = g.getTerminal(Abc.C);
        LLCalc calc = createCalc(g);
        // FIRST
        Map<Symbol, Set<Term>> first = calc.getFirst();
        assertEquals(set(empty), first.get(empty));
        assertEquals(set(eof), first.get(eof));
        assertEquals(set(a), first.get(a));
        assertEquals(set(b), first.get(b));
        assertEquals(set(c), first.get(c));
        assertEquals(set(a, b, c), first.get(l));
        assertEquals(set(a, c), first.get(r));
        assertEquals(set(b), first.get(q));
        assertEquals(set(b, empty), first.get(rr));
        assertEquals(set(b, c), first.get(qq));
        // FOLLOW
        Map<NonTerm, Set<Term>> follow = calc.getFollow();
        assertEquals(set(eof), follow.get(l));
        assertEquals(set(a), follow.get(r));
        assertEquals(set(b), follow.get(q));
        assertEquals(set(a), follow.get(rr));
        assertEquals(set(b), follow.get(qq));
        // FIRSTP
        Map<Rule, Set<Term>> firstp = calc.getFirstp();
        List<Rule> rrRules = g.getRules(rr);
        assertEquals(set(a, empty), firstp.get(rrRules.get(1)));
        // LL1
        assertTrue(calc.isLL1());
    }



    /**
     * Creates an LL1-calculator for a given grammar.
     */
    private LLCalc createCalc(Grammar g) {
        return new MyLLCalc(g);
    }

    @SuppressWarnings("unchecked")
    private <T> Set<T> set(T... elements) {
        return new HashSet<>(Arrays.asList(elements));
    }
}
