package pp.block2.cp_wouter.test;

import nl.utwente.pp.cp.junit.ConcurrentRunner;
import nl.utwente.pp.cp.junit.ThreadNumber;
import nl.utwente.pp.cp.junit.Threaded;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import pp.block2.cp_wouter.queue.MyQueue;
import pp.block2.cp_wouter.queue.QueueEmptyException;

import java.util.NoSuchElementException;
import java.util.Random;

@RunWith(ConcurrentRunner.class)
public class ImprovedQueueTest {

	/**
	 * The amount of threads used in each test.
	 */
	private static final int AMOUNT_OF_THREADS = 8;

	/**
	 * Simple random object to get random integers from.
	 */
	private final Random random = new Random();

	/**
	 * Queue used for the producer consumer example (advancedMultiThreadedTest).
	 */
	private MyQueue<Integer> producerConsumerQueue;

	/**
	 * If you need to setup some object before the multithreaded tests start, you can do it in a method annotated with
	 * {@link Before}. This methods are always executed single threaded.
	 */
	@Before
	public void before() {
		//Setup an empty queue.
		this.producerConsumerQueue = new MyQueue<>();
	}

	/**
	 * Simple multi threaded test which performs reads and writes to a queue from the same threads. There is no
	 * difference between the task of each thread.
	 */
	@Test
	@Threaded(count = AMOUNT_OF_THREADS)
	public void simpleMultiThreadedTest() throws InterruptedException {
		for (int i = 0; i < 50; i++) {
			int write = this.random.nextInt();
			this.producerConsumerQueue.push(write);
		}
		int i = 50;
		while (i > 0) {
            Integer read = null;
            try {
                read = this.producerConsumerQueue.pull();
            } catch (QueueEmptyException ignored) {
            }
            if (read == null) {
				Thread.sleep(500);
			} else {
				i--;
			}
		}
	}

	/**
	 * Function which reads integers from the queue and therefore performs the task of a consumer.
	 * @param num The number of this consumer.
	 * @throws InterruptedException If the thread got interrupted.
	 */
	private void consumer(int num) throws InterruptedException {
		int i = 50;
		while (i > 0) {
            Integer read = null;
            try {
                read = this.producerConsumerQueue.pull();
            } catch (QueueEmptyException | NoSuchElementException ignored) {
            }
            if (read == null) {
				Thread.sleep(500);
			} else {
				System.out.printf("Consumer %d: Polled %d.%n", num, read);
				i--;
			}
		}
	}

	/**
	 * Function which writes integers to the queue and therefore performs the task of a producer.
	 * @param num The number of this producer.
	 */
	private void producer(int num) {
		for (int i = 0; i < 50; i++) {
			int write = this.random.nextInt();
			System.out.printf("Producer %d: Added %d.%n", num, write);
			this.producerConsumerQueue.push(write);
		}
	}

	/**
	 * Test which shows a simple producer consumer pattern, with different threads performing different tasks.
	 * @param theadNumber The number of the thread executing the code.
	 * @throws InterruptedException If one of the threads got interrupted.
	 */
	@Test
	@Threaded(count = AMOUNT_OF_THREADS * 2)
	public void advancedMultiThreadedTest(@ThreadNumber int theadNumber) throws InterruptedException {
		if (theadNumber < AMOUNT_OF_THREADS) {
			this.producer(theadNumber);
		} else {
			this.consumer(theadNumber - AMOUNT_OF_THREADS);
		}
	}

	/**
	 * This test is missing the {@link Threaded} annotation, so it will run single threaded, like a normal JUnit test
	 * would, which does not run on separate threads at all.
	 */
	@Test
	public void singleThreadedTest() {
		int write = this.random.nextInt();
		this.producerConsumerQueue.push(write);
        try {
            this.producerConsumerQueue.pull();
        } catch (QueueEmptyException ignored) {

        }
    }

	/**
	 * If you want to assert something about the state of the class after all threads are ended, you can annotate
	 * a method with {@link After} and assert in there. Methods annotated with {@link After} always run single threaded.
	 */
	@After
	public void after() {
		//Assert the queue is empty.
        try {
            Assert.assertNull(this.producerConsumerQueue.pull());
        } catch (QueueEmptyException | NoSuchElementException ignored) {

        }
    }

}
