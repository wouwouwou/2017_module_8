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
import java.util.stream.IntStream;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class FibTest {


	@Test
	public void test() {
		IntStream.range(1,20).forEach(w -> testCase(w));
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
