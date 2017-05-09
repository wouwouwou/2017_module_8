package pp.block2.cp.test;

import nl.utwente.pp.cp.junit.ConcurrentRunner;
import nl.utwente.pp.cp.junit.Threaded;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import pp.block2.cp.queue.MyQueue;
import pp.block2.cp.queue.QueueEmptyException;

import java.util.NoSuchElementException;

@RunWith(ConcurrentRunner.class)
public class QueueTest {

	/**
	 * The amount of threads used in each test.
	 */
	private static final int AMOUNT_OF_THREADS = 5000;

	/**
	 * int used for tracking the sequence value.
	 */
	//This is already threadsafe. Not thread safe means using a normal int and a ++ operator.
	private MyQueue<Integer> integerQueue;

	/**
	 * If you need to setup some object before the multithreaded tests start, you can do it in a method annotated with
	 * {@link Before}. This methods are always executed single threaded.
	 */
	@Before
	public void before() {
		//Setup an empty queue.
		this.integerQueue = new MyQueue<>();
	}

	/**
	 * Simple multi threaded test which performs reads and writes to a queue from the same threads. There is no
	 * difference between the task of each thread.
	 */
	@Test
	@Threaded(count = AMOUNT_OF_THREADS)
	public void sequenceTest() throws InterruptedException {
        for (int i = 0; i < 100; i++) {
            this.integerQueue.push(i);
        }
        for (int i = 0; i < 100; i++) {
            try {
                this.integerQueue.pull();
            } catch (QueueEmptyException | NoSuchElementException ignored) {
            }
        }
	}

	/**
	 * If you want to assert something about the state of the class after all threads are ended, you can annotate
	 * a method with {@link After} and assert in there. Methods annotated with {@link After} always run single threaded.
	 */
	@After
	public void after() {
		Assert.assertEquals(0, this.integerQueue.getLength());
	}

}
