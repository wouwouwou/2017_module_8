package pp.block1.cc.dfa;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by Wouter on 24-4-2017.
 */
public class MyScanner implements Scanner {
    @Override
    public List<String> scan(State dfa, String text) {
        List<String> res = new ArrayList<>();
        State begin = dfa;
        char[] characters = text.toCharArray();
        int i = 0;
        int beginindex = 0;
        int endindex = 0;
        while (i < characters.length) {
            char c = characters[i];
            if (dfa.hasNext(c)) {
                dfa = dfa.getNext(c);
            } else {
                i = endindex - 1;
                res.add(text.substring(beginindex,endindex));
                beginindex = endindex;
                endindex++;
                dfa = begin;
            }
            if (dfa.isAccepting()) {
                endindex = i + 1;
            }
            i++;
        }
        if (endindex != beginindex) {
            res.add(text.substring(beginindex, endindex));
        }
        return res;
    }
}
