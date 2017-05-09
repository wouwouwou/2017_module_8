package pp.block2.cp_martijn.queue;

import java.util.LinkedList;
import java.util.NoSuchElementException;
import java.util.concurrent.ConcurrentLinkedQueue;

/**
 * Created by martijn on 8-5-17.
 */
public class LListQueue<T> implements Queue<T> {

    private LinkedList<T> list = new LinkedList<>();

    private ConcurrentLinkedQueue queue = null;

    @Override
    public void push(T x) {
        synchronized (this) {
            list.addLast(x);
        }
    }

    @Override
    public T pull() throws QueueEmptyException {
        synchronized (this) {
            try {
                return list.pop();
            } catch (NoSuchElementException e) {
                throw new QueueEmptyException();
            }
        }
    }

    @Override
    public int getLength() {
        synchronized (this) {
            return list.size();
        }
    }
}
