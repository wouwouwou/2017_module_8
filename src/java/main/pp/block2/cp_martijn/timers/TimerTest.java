package pp.block2.cp_martijn.timers;

import nl.utwente.pp.cp.junit.Threaded;
import org.junit.Test;

import java.util.TimerTask;

/**
 * Created by martijn on 9-5-17.
 */
public class TimerTest {

    public static final int NUMBER_OF_THREADS = 10;


    @Test
    @Threaded(count = NUMBER_OF_THREADS)
    public void ConcurrentTimerTest() {

    }

    private class MyTimerTask extends TimerTask {

        Incrementer increment;

        public MyTimerTask(Incrementer increment) {
            this.increment = increment;
        }

        @Override
        public void run() {
            increment.increment();
        }
    }

    private class Incrementer {
        private int inc = 0;

        public void increment() {
            inc++;
        }

        public int value() {
            return inc;
        }
    }
}
