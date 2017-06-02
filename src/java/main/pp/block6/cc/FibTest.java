package pp.block6.cc;


import org.junit.Test;
import pp.iloc.Assembler;
import pp.iloc.Simulator;
import pp.iloc.eval.Machine;
import pp.iloc.model.Program;
import pp.iloc.parse.FormatException;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class FibTest {


	@Test
	public void test() {
		testCase(1);
		testCase(2);
		testCase(3);
		testCase(4);
		testCase(5);
		testCase(6);
		testCase(7);
		testCase(8);
		testCase(9);
		testCase(10);
		testCase(11);
		testCase(12);
		testCase(13);
		testCase(14);
		testCase(15);
		testCase(16);
		testCase(17);
		testCase(18);
		testCase(19);
		testCase(20);
		testCase(30);
	}

	private void testCase(int input) {
		Program p = parse("src/pp/block6/cc/fib2016");
		Simulator s = new Simulator(p);
		Machine vm = s.getVM();
		s.setIn(new ByteArrayInputStream(("" + input).getBytes()));
		s.run();
		assertEquals(fib(input), vm.getReg("r_return_val"));
	}

	private Program parse(String filename) {
		File file = new File(filename + ".iloc");
		try {
			Program result = Assembler.instance().assemble(file);
			String print = result.prettyPrint();
			System.out.println("Program " + file + ":");
			System.out.print(print);
			Program other = Assembler.instance().assemble(print);
			assertEquals(result, other);
			return result;
		} catch (FormatException | IOException e) {
			fail(e.getMessage());
			return null;
		}
	}

	private int fib(int n) {
		switch (n) {
			case 0:
			case 1:
				return 1;
			default:
				return fib(n - 2) + fib(n - 1);
		}
	}
}
