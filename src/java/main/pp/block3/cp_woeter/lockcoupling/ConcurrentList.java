package pp.block3.cp_woeter.lockcoupling;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Created by Wouter on 15-5-2017.
 */
public class ConcurrentList<T> implements List<T> {

    Node<T> first;

    Node<T> last;

    AtomicInteger size;

    @Override
    public void insert(int pos, T val) {
        assert pos >= 0 && pos <= this.size.get();

        if (pos == this.size.get()) {
            add(val);
        } else {

        }

    }

    /**
     * Returns the (non-null) Node at the specified element index.
     */
    Node<T> node(int index) {
        if (index < (this.size.get() >> 1)) {
            Node<T> x = first;
            for (int i = 0; i < index; i++)
                x = x.next;
            return x;
        } else {
            Node<T> x = last;
            for (int i = this.size.get() - 1; i > index; i--)
                x = x.prev;
            return x;
        }
    }

    @Override
    public void add(T val) {
        final Node<T> l = last;
        final Node<T> newNode = new Node<>(l, val, null);
        last = newNode;
        if (l == null)
            first = newNode;
        else
            l.next = newNode;
        this.size.getAndAdd(1);
    }



    @Override
    public void remove(T item) {
        if (item == null) {
            for (Node<T> x = first; x != null; x = x.next) {
                if (x.item == null) {
                    unlink(x);
                }
            }
        } else {
            for (Node<T> x = first; x != null; x = x.next) {
                if (item.equals(x.item)) {
                    unlink(x);
                }
            }
        }
    }

    private void unlink(Node<T> x) {
        final Node<T> next = x.next;
        final Node<T> prev = x.prev;

        if (prev == null) {
            first = next;
        } else {
            prev.next = next;
            x.prev = null;
        }

        if (next == null) {
            last = prev;
        } else {
            next.prev = prev;
            x.next = null;
        }

        x.item = null;
        this.size.getAndAdd(-1);
    }

    @Override
    public void delete(int pos) {

    }

    @Override
    public int size() {
        return 0;
    }

    @Override
    public String toString() {
        return super.toString();
    }

    private static class Node<E> {
        E item;
        ConcurrentList.Node<E> next;
        ConcurrentList.Node<E> prev;
        ReentrantLock lock;

        Node(ConcurrentList.Node<E> prev, E element, ConcurrentList.Node<E> next) {
            this.item = element;
            this.next = next;
            this.prev = prev;
            this.lock = new ReentrantLock();
        }
    }

}
