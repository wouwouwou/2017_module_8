package pp.block1.cc.dfa;

/**
 * Created by Wouter on 24-4-2017.
 */
public class MyChecker implements Checker {
    @Override
    public boolean accepts(State start, String word) {
        State s = start;
        boolean errorstate = false;
        for (Character c : word.toCharArray()) {
            if (s.hasNext(c)) {
                s = s.getNext(c);
            } else {
                errorstate = true;
                break;
            }
        }
        return s.isAccepting() && !errorstate;
    }
}
