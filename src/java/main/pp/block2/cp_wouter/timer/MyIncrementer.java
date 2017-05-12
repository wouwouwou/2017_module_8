package pp.block2.cp_wouter.timer;

/**
 * Created by Wouter on 11-5-2017.
 */
public class MyIncrementer {

    private int counter;

    public MyIncrementer() {
        counter = 0;
    }

    public int getCounter() {
        return counter;
    }

    public void setCounter(int counter) {
        this.counter = counter;
    }

    public void increment() {
        this.counter++;
    }
}
