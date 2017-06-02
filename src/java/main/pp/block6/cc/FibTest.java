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
import java.net.URISyntaxException;
import java.net.URL;
import java.util.stream.IntStream;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class FibTest {

    private final static String BASE_DIR = "./";
    private final static String EXT = ".iloc";

	@Test
	public void test() {
		IntStream.range(1,20).forEach(w -> testCase(w));
		testCase(30);
	}

	private void testCase(int input) {
		Program p = parse("fib2016");
		Simulator s = new Simulator(p);
		Machine vm = s.getVM();
		s.setIn(new ByteArrayInputStream(("" + input).getBytes()));
		s.run();
		assertEquals(fib(input), vm.getReg("r_return_val"));
	}

	private Program parse(String filename) {
        String path = BASE_DIR + filename + EXT;
        URL url = getClass().getResource(path);
        File file;
        try {
            file = new File(url.toURI());
			Program result = Assembler.instance().assemble(file);
			String print = result.prettyPrint();
			System.out.println("Program " + file + ":");
			System.out.print(print);
			Program other = Assembler.instance().assemble(print);
			assertEquals(result, other);
			return result;
		} catch (FormatException | IOException | URISyntaxException e) {
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
