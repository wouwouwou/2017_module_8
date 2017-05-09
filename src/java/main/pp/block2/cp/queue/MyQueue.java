package pp.block2.cp.queue;

import java.util.LinkedList;

/**
 * Created by Wouter on 8-5-2017.
 */
public class MyQueue<T> implements Queue<T> {

    private LinkedList<T> queue;

    public MyQueue() {
        queue = new LinkedList<>();
    }

    @Override
    public synchronized void push(T x) {
        queue.push(x);
    }

    @Override
    public synchronized T pull() throws QueueEmptyException {
        return this.queue.pop();
    }

    @Override
    public int getLength() {
        return queue.size();
    }
}
