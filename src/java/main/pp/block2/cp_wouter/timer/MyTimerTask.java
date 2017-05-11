package pp.block2.cp_wouter.timer;

import java.util.TimerTask;

/**
 * Created by Wouter on 11-5-2017.
 */
public class MyTimerTask extends TimerTask {

    private Integer updates;
    private MyIncrementer inc;

    MyTimerTask(Integer updates) {
        this.updates = updates;
    }

    MyTimerTask(MyIncrementer inc) {
        this.inc = inc;
    }

    @Override
    public void run() {
        if (inc != null) {
            this.inc.increment();
        }
        if (updates != null) {
            updates++;
        }
    }
}
