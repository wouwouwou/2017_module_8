package pp.block1.cc.test;

import static pp.block1.cc.dfa.State.DFA_LALA;
import static pp.block1.cc.dfa.State.ID6_DFA;

import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import pp.block1.cc.dfa.MyScanner;
import pp.block1.cc.dfa.Scanner;
import pp.block1.cc.dfa.State;

/** Test class for Scanner implementation. */
public class ScannerTest {
	private Scanner myGen = new MyScanner();

	@Test
	public void testID6() {
		this.dfa = ID6_DFA;
		yields("");
		yields("a12345", "a12345");
		yields("a12345AaBbCc", "a12345", "AaBbCc");
	}

	@Test
    public void testLA() {
	    this.dfa = DFA_LALA;
        yields("");
        yields("La", "La");
        yields("LaLa", "LaLa");
        yields("LaLaLaLi", "LaLaLaLi");
        yields("LaLaLaLiLaLaLaLi", "LaLaLaLi", "LaLaLaLi");
        yields("LaLaaaaLaaaaLi", "LaLaaaaLaaaaLi");
        yields("La    Laaaa LaaaaLi", "La    Laaaa LaaaaLi");
    }

	private void yields(String word, String... tokens) {
		List<String> result = this.myGen.scan(this.dfa, word);
		if (result == null) {
			Assert.fail(String.format(
					"Word '%s' is erroneously rejected by %s", word, this.dfa));
		}
		Assert.assertEquals(tokens.length, result.size());
		for (int i = 0; i < tokens.length; i++) {
			Assert.assertEquals(tokens[i], result.get(i));
		}
	}

	private State dfa;
}
