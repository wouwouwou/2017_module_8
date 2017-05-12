package pp.block2.cp_martijn.timers;

import nl.utwente.pp.cp.junit.ConcurrentRunner;
import nl.utwente.pp.cp.junit.ThreadNumber;
import nl.utwente.pp.cp.junit.Threaded;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.util.Calendar;
import java.util.Date;
import java.util.Timer;
import java.util.TimerTask;

/**
 * Created by martijn on 9-5-17.
 */

@RunWith(ConcurrentRunner.class)
public class TimerTest {

    public static final int NUMBER_OF_THREADS = 100;

    public Timer[] timer;
    public long time;

    public Incrementer increment;

    @Before
    public void initialize() {
        timer = new Timer[NUMBER_OF_THREADS];
        increment = new Incrementer();
        time = System.currentTimeMillis() + 1000;
    }

    @Test
    @Threaded(count = NUMBER_OF_THREADS)
    public void SameTimerTest(@ThreadNumber int i) {
        timer[i] = new Timer();
        TimerTask t = new MyTimerTask(increment);
        timer[i].schedule(t, new Date(time));
    }

    @Test
    @Threaded(count = NUMBER_OF_THREADS)
    public void ConcurrentTimerTest(@ThreadNumber int i) {
        timer[i] = new Timer();
        TimerTask t = new MyTimerTask(increment);
        timer[i].schedule(t, new Date(time));
    }

    @After
    public void after() {
            while (System.currentTimeMillis() < time + 100) {
                Thread.yield();
            }
        Assert.assertEquals(NUMBER_OF_THREADS, increment.value());
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
