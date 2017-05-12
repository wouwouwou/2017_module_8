package pp.block2.cp_wouter.timer;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Date;
import java.util.Timer;
import java.util.TimerTask;

/**
 * Created by Wouter on 11-5-2017.
 */
public class Program2 {

    private ArrayList<Timer> timerList = new ArrayList<>();

    private Program2(long time, MyIncrementer inc) {
        Date date = new Date(time);
        for (int i = 0; i < 10000; i++) {
            Timer timer = new Timer();
            timer.schedule(new MyTimerTask(inc), date);
            timerList.add(timer);
        }
    }

    public static void main(String[] args) {
        long time = System.currentTimeMillis() + 5000;
        MyIncrementer inc = new MyIncrementer();
        Program2 program = new Program2(time, inc);
        while (System.currentTimeMillis() < time + 100){

        }
        System.out.println(inc.getCounter());
    }
}
