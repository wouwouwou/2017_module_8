package pp.block3.cp_martijn.lockcoupling;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Created by martijn on 15-5-17.
 */
public class ConcurrentList<T> implements List<T> {

    private ConcurrentListItem head;

    public ConcurrentList() {
        head = new ConcurrentListItem(null);
    }

    @Override
    public void insert(int pos, T val) {
        ConcurrentListItem rest = head;
        rest.lock();
        ConcurrentListItem next = rest.getNextItem();
        while(pos > 0) {
            if (next == null) {
                throw new IndexOutOfBoundsException();
            }
            next.lock();
            pos--;
            if (pos > 0) {
                rest.unlock();
                rest = next;
            }
        }

        ConcurrentListItem item = new ConcurrentListItem(val);
        item.setNextItem(next);
        rest.setNextItem(item);
        rest.unlock();
        next.unlock();
    }

    @Override
    public void add(T val) {
        ConcurrentListItem rest = head;
        rest.lock();
        ConcurrentListItem next = rest.getNextItem();
        while (next != null) {
            next.lock();
            rest.unlock();
            rest = next;
            next = rest.getNextItem();
        }
        ConcurrentListItem item = new ConcurrentListItem(val);
        rest.setNextItem(item);
        rest.unlock();
    }

    @Override
    public void remove(T item) {
        ConcurrentListItem rest = head;
        rest.lock();
        ConcurrentListItem next = rest.getNextItem();
        while (rest.get() != item) {
            next.lock();
            rest.unlock();
            rest = next;
            next = rest.getNextItem();
        }
        // ConcurrentListItem item = new ConcurrentListItem(val);
        // rest.setNextItem(item);
        rest.unlock();

    }

    @Override
    public void delete(int pos) {

    }

    @Override
    public int size() {
        return 0;
    }

    private class ConcurrentListItem {

        private Lock lock = new ReentrantLock();

        private T value;

        private ConcurrentListItem nextItem;

        ConcurrentListItem(T value) {
            this.value = value;
            nextItem = null;
        }

        T get() {
            return value;
        }

        void set(T newValue) {
            value = newValue;
        }

        void lock() {
            lock.lock();
        }

        void unlock() {
            lock.unlock();
        }

        ConcurrentListItem getNextItem() {
            return nextItem;
        }

        void setNextItem(ConcurrentListItem nextItem) {
            this.nextItem = nextItem;
        }
    }
}
