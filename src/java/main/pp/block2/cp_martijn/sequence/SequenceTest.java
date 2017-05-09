package pp.block2.cp_martijn.sequence;

import nl.utwente.pp.cp.junit.ConcurrentRunner;
import nl.utwente.pp.cp.junit.ThreadNumber;
import nl.utwente.pp.cp.junit.Threaded;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import java.util.Queue;
import java.util.Random;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * ExampleTest class showing the usage of the concurrent JUnit runner.
 *
 * Many exercises in concurrent programming ask you to write a 'test' to show something is not thread safe or
 * to gain some confidence (although not proof) that a class is indeed thread safe. Using the concurrent JUnit runner,
 * you can write JUnit 4 test classes that have tests that are run on a certain amount of threads at once.
 *
 * You can create a concurrent JUnit test by annotating your test class with the {@link RunWith} annotation. This
 * annotation expects a specialized test runner class. To use the concurrent JUnit test runner, annotate your test
 * class with {@link ConcurrentRunner} as runner class. It is located in the nl.utwente.pp.cp_martijn package.
 *
 * Once you do have annotated your class with the {@link RunWith} annotation, you can write a JUnit test as usual,
 * with test methods that are annotated using {@link Test}. This however gives you a traditional test that runs single
 * threaded. In order to execute the method concurrently on multiple threads, use the {@link Threaded} annotation on
 * your test method BESIDE the {@link Test} annotation. The {@link Threaded} annotation accepts the count argument on
 * which you can set the amount of threads the test should run on. The {@link ConcurrentRunner} class assures the
 * tests annotated with {@link Threaded} will run concurrently on the specified amount of threads.
 *
 * When a test is run with the {@link Threaded} annotation, the {@link ConcurrentRunner} guarantees that the body of
 * your test method is run on the specified amount of threads, which are all started at the same time. (It uses an
 * {@link java.util.concurrent.CountDownLatch} under the hood to assure this). The test is finished when all threads
 * running the test finish. This means that when your test is not thread safe and some threads deadlock, the test will
 * run forever unless the timeout argument of the JUnit {@link Test} annotation is used.
 *
 * Within the threaded test methods, you can use all functions you expect from JUnit, such as making assertions using
 * the {@link Assert} class. It is also possible to use the 'expected' argument of the JUnit {@link Test} annotation,
 * in that case you expect at least one of the threads to throw the specified exception.
 *
 * Sometimes, it might be the case that some threads in a single test have different roles than other threads. An
 * example of such test might be a test for the producer/consumer pattern. In this case some threads have the role of
 * producer and some threads have the role of consumer. In order to make a difference between the different threads
 * executing the same test, you can get the thread number as parameter of the function. Create a parameter of type
 * 'int' and annotate this parameter with the {@link ThreadNumber} annotation. When the test is executed, the parameter
 * will contain the number of the thread executing the test. This number starts at 0 for the first thread, and is
 * 'count - 1' for the last thread executing the test. This can be used for effectively create different executions
 * for the threads.
 *
 * The following example contains
 */
@RunWith(ConcurrentRunner.class)
public class SequenceTest {

	/**
	 * The amount of threads used in each test.
	 */
	private static final int AMOUNT_OF_THREADS = 10;

	private static final int AMOUNT_OF_INCREMENTS = 300;

	private AtomicInteger counter;

	/**
	 * If you need to setup some object before the multithreaded tests start, you can do it in a method annotated with
	 * {@link Before}. This methods are always executed single threaded.
	 */
	@Before
	public void before() {
		//Setup an empty queue.
		counter = new AtomicInteger(0);
	}

	/**
	 * Simple multi threaded test which performs reads and writes to a queue from the same threads. There is no
	 * difference between the task of each thread.
	 */
	@Test
	@Threaded(count = AMOUNT_OF_THREADS)
	public void simpleMultiThreadedTest() throws InterruptedException {
		for(int i = 0; i < AMOUNT_OF_INCREMENTS; i++){
			counter.getAndAdd(1);
		}
	}

	/**
	 * This test is missing the {@link Threaded} annotation, so it will run single threaded, like a normal JUnit test
	 * would, which does not run on separate threads at all.
	 */
	@Test
	public void singleThreadedTest() {
		for(int i = 0; i < AMOUNT_OF_INCREMENTS*AMOUNT_OF_THREADS; i++){
			counter.getAndAdd(1);
		}
	}

	/**
	 * If you want to assert something about the state of the class after all threads are ended, you can annotate
	 * a method with {@link After} and assert in there. Methods annotated with {@link After} always run single threaded.
	 */
	@After
	public void after() {
		//Assert the queue is empty.
		Assert.assertEquals(AMOUNT_OF_INCREMENTS*AMOUNT_OF_THREADS, counter.get());
	}

}
