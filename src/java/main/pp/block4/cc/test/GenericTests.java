package pp.block4.cc.test;

import org.junit.Test;
import pp.iloc.Assembler;
import pp.iloc.Simulator;
import pp.iloc.model.Program;
import pp.iloc.parse.FormatException;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class GenericTests {

    @Test
    public void exercise6Test() {
        Simulator.DEBUG = false;
        Program max = parse("../iloc/max.iloc");
        Simulator s = new Simulator(max);
        s.getVM().init("a", 1, 2, 3, 4, 10, 6, 7, 8, 9);
        s.getVM().setNum("alength", 9);
        s.run();
        assertEquals(s.getVM().getReg("r_max"), 10);
    }

    @Test
    public void fibTest() {
        Program fib = parse("../iloc/fib.iloc");
        Simulator s = new Simulator(fib);
        s.getVM().setNum("n", 45);
        s.run();
    }

    @Test
    public void fibMTest() {
        Program fib = parse("../iloc/fibm.iloc");
        Simulator.DEBUG = false;
        Simulator s = new Simulator(fib);
        s.getVM().setNum("n", 10);
        s.getVM().init("x", 1);
        s.getVM().init("y", 1);
        s.getVM().init("z", 1);
        s.getVM().init("one");
        s.run();
    }

    private Program parse(String path) {
        URL url = getClass().getResource(path);
        try {
            File file = new File(url.toURI());
            Program result = Assembler.instance().assemble(file);
            String print = result.prettyPrint();
            Program other = Assembler.instance().assemble(print);
            assertEquals(result, other);
            return result;
        } catch (FormatException | IOException | URISyntaxException e) {
            fail(e.getMessage());
            return null;
        }
    }
}