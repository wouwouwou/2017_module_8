package pp.block2.cc.ll;

import pp.block2.cc.NonTerm;
import pp.block2.cc.Symbol;
import pp.block2.cc.Term;

import java.util.*;

public class TimLLCalc implements LLCalc {

    private final Map<Symbol, Set<Term>> FIRST;
    private final Map<NonTerm, Set<Term>> FOLLOW;
    private final Map<Rule, Set<Term>> FIRSTP;
    private final boolean ISLL1;

    public TimLLCalc(Grammar g) {

        FIRST = new HashMap<>();
        initFirst(g);
        FOLLOW = new HashMap<>();
        initFollow(g);
        FIRSTP = new HashMap<>();
        initFP(g);
        ISLL1 = initLL1(g);
    }

    private boolean initLL1(Grammar g) {
        for (NonTerm nonTerm : g.getNonterminals()) {
            Set<Term> firstAggr = new HashSet<>();
            for (Rule rule : FIRSTP.keySet()) {
                if (rule.getLHS().equals(nonTerm)) {
                    for (Term term : FIRSTP.get(rule)) {
                        if (firstAggr.contains(term)) {
                            return false;
                        }
                    }
                    firstAggr.addAll(FIRSTP.get(rule));
                }
            }
        }
        return true;
    }

    private void initFP(Grammar g) {
        for (Rule rule : g.getRules()) {
            Set<Term> first = firstPerRule(rule);
            if (first.contains(Symbol.EMPTY)) {
                Set<Term> aggr = new HashSet<>(first);
                aggr.addAll(FOLLOW.get(rule.getLHS()));
                FIRSTP.put(rule, aggr);
            } else {
                FIRSTP.put(rule, first);
            }
        }


    }

    private void initFollow(Grammar g) {
        // Add all non-terminals to FIRST, initiating the start element with Symbol.EOF
        for (NonTerm nonTerm : g.getNonterminals()) {
            FOLLOW.put(nonTerm, new HashSet<Term>());
        }
        FOLLOW.get(g.getStart()).add((Symbol.EOF));

        // Determine the contents of FOLLOW
        boolean changing = true;
        while (changing) {
            changing = false;
            for (Rule rule : g.getRules()) {
                List<Symbol> rhs = rule.getRHS();
                NonTerm lhs = rule.getLHS();
                Set<Term> trailer = new HashSet<>();
                trailer.addAll(FOLLOW.get(lhs));
                for (int i = rhs.size() - 1; i >= 0; i--) {
                    if (rhs.get(i) instanceof NonTerm) {
                        if (!FOLLOW.get(rhs.get(i)).containsAll(trailer)) {
                            changing = true;
                            FOLLOW.get(rhs.get(i)).addAll(trailer);
                        }
                        if (FIRST.get(rhs.get(i)).contains(Symbol.EMPTY)) {
                            Set<Term> temp = new HashSet<>(FIRST.get(rhs.get(i)));
                            temp.remove(Symbol.EMPTY);
                            trailer.addAll(temp);
                        } else {
                            trailer = FIRST.get(rhs.get(i));
                        }
                    } else {
                        trailer = FIRST.get(rhs.get(i));
                    }
                }
            }
        }
    }

    private void initFirst(Grammar g) {
        // Add all terminals to FIRST
        FIRST.put(Symbol.EMPTY, new HashSet<>(Collections.singletonList(Symbol.EMPTY)));
        FIRST.put(Symbol.EOF, new HashSet<>(Collections.singletonList(Symbol.EOF)));
        for (Term term : g.getTerminals()) {
            Set<Term> set = new HashSet<>();
            set.add(term);
            FIRST.put(term, set);
        }

        // Add all non-terminals to FIRST with empty sets
        for (NonTerm nonTerm : g.getNonterminals()) {
            FIRST.put(nonTerm, new HashSet<Term>());
        }

        // Determine the contents of FIRST
        boolean changing = true;
        while (changing) {
            changing = false;
            for (Rule rule : g.getRules()) {
                Set<Term> rhs = firstPerRule(rule);

                if (!FIRST.get(rule.getLHS()).containsAll(rhs)) {
                    changing = true;
                    FIRST.get(rule.getLHS()).addAll(rhs);
                }
            }
        }
    }

    public Set<Term> firstPerRule(Rule rule) {
        Set<Term> rhs = new HashSet<>();
        List<Symbol> ruleRhs = rule.getRHS();
        rhs.addAll(FIRST.get(ruleRhs.get(0)));
        rhs.remove(Symbol.EMPTY);
        int i = 1;
        while (i < ruleRhs.size() && FIRST.get(ruleRhs.get(i)).contains(Symbol.EMPTY)) {
            rhs.addAll(FIRST.get(ruleRhs.get(i)));
            rhs.remove(Symbol.EMPTY);
            i++;
        }
        if (i == ruleRhs.size() && FIRST.get(ruleRhs.get(i - 1)).contains(Symbol.EMPTY)) {
            rhs.add(Symbol.EMPTY);
        }
        return rhs;
    }

    /**
     * Returns the FIRST-map for the grammar of this calculator instance.
     */
    @Override
    public Map<Symbol, Set<Term>> getFirst() {
        return FIRST;
    }

    /**
     * Returns the FOLLOW-map for the grammar of this calculator instance.
     */
    @Override
    public Map<NonTerm, Set<Term>> getFollow() {
        return FOLLOW;
    }

    /**
     * Returns the FIRST+-map for the grammar of this calculator instance.
     */
    @Override
    public Map<Rule, Set<Term>> getFirstp() {
        return FIRSTP;
    }

    /**
     * Indicates if the grammar of this calculator instance is LL(1).
     */
    @Override
    public boolean isLL1() {
        return ISLL1;
    }
}