package pp.block3.cc.symbol;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

/**
 * Created by Wouter on 12-5-2017.
 */
public class MySymbolTable implements SymbolTable {

    // Could have been a Stack of Sets. :D
    private Map<Integer, Stack<String>> symboltable;

    public MySymbolTable() {
        this.symboltable = new HashMap<>();
        symboltable.put(0, new Stack<>());
    }

    @Override
    public void openScope() {
        symboltable.put(symboltable.keySet().size(), new Stack<>());
    }

    @Override
    public void closeScope() throws RuntimeException {
        int scope = symboltable.keySet().size() - 1;
        if (scope == 0) {
            throw new RuntimeException();
        }
        symboltable.remove(scope);
    }

    @Override
    public boolean add(String id) {
        int scope = symboltable.keySet().size() - 1;
        Stack<String> s = symboltable.get(scope);
        if (s.contains(id)) {
            return false;
        }
        s.add(id);
        return true;
    }

    @Override
    public boolean contains(String id) {
        for (int i = symboltable.keySet().size() - 1; i >= 0; i--) {
            if (symboltable.get(i).contains(id)) {
                return true;
            }
        }
        return false;
    }
}
