package pp.block2.cp_wouter.timer;

import java.util.Date;
import java.util.Timer;

/**
 * Created by Wouter on 11-5-2017.
 */
public class Program1 {

    private Timer timer;
    private MyIncrementer inc;

    private Program1() {
        this.timer = new Timer();
        this.inc = new MyIncrementer();
    }

    public static void main(String[] args) {
        Program1 program = new Program1();
        long time = System.currentTimeMillis() + 2000;
        Date date = new Date(System.currentTimeMillis() + 2000);
        for (int i = 0; i < 100; i++) {
            program.timer.schedule(new MyTimerTask(program.inc), date);
        }
        while (System.currentTimeMillis() < time + 1000){

        }
        System.out.println(program.inc.getCounter());
    }
}
