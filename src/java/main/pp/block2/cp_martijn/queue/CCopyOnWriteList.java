package pp.block2.cp_martijn.queue;

import java.util.LinkedList;
import java.util.List;

/**
 * Created by martijn on 23-5-17.
 */
public class CCopyOnWriteList<T> {

    volatile List<T> list = new LinkedList<>(); // is this required?

    public void set(int index, T item) {
        synchronized (this) {
            List<T> newList = new LinkedList<T>();
            newList.addAll(list);
            newList.add(index, item);
            list = newList;
        }
    }

    public void add(T item) {
        synchronized (this) {
            List newList = new LinkedList();
            newList.addAll(list);
            newList.add(item);
            list = newList;
        }
    }

    public T get(int index) {
        return list.get(index);
    }

    public List<T> getList() {
        return list;
    }
}
