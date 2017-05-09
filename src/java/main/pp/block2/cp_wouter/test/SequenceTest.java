package pp.block2.cp_wouter.test;

import nl.utwente.pp.cp.junit.ConcurrentRunner;
import nl.utwente.pp.cp.junit.Threaded;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import java.util.concurrent.atomic.AtomicInteger;

@RunWith(ConcurrentRunner.class)
public class SequenceTest {

	/**
	 * The amount of threads used in each test.
	 */
	private static final int AMOUNT_OF_THREADS = 30;

	/**
	 * int used for tracking the sequence value.
	 */
	//This is already threadsafe. Not thread safe means using a normal int and a ++ operator.
	private AtomicInteger sequencevalue;

	/**
	 * If you need to setup some object before the multithreaded tests start, you can do it in a method annotated with
	 * {@link Before}. This methods are always executed single threaded.
	 */
	@Before
	public void before() {
		//Setup an empty queue.
		this.sequencevalue = new AtomicInteger(0);
	}

	/**
	 * Simple multi threaded test which performs reads and writes to a queue from the same threads. There is no
	 * difference between the task of each thread.
	 */
	@Test
	@Threaded(count = AMOUNT_OF_THREADS)
	public void sequenceTest() throws InterruptedException {
		for (int i = 0; i < 100; i++) {
			this.sequencevalue.getAndAdd(1);
		}
	}

	/**
	 * If you want to assert something about the state of the class after all threads are ended, you can annotate
	 * a method with {@link After} and assert in there. Methods annotated with {@link After} always run single threaded.
	 */
	@After
	public void after() {
		Assert.assertEquals(100 * AMOUNT_OF_THREADS, this.sequencevalue.get());
	}

}
