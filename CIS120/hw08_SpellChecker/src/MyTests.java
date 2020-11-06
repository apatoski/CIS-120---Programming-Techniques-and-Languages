import static org.junit.Assert.*;

import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.TreeSet;

import org.junit.*;

/** Put your OWN test cases in this file, for all classes in the assignment. */
public class MyTests {

	private Set<String> makeSet(String[] strings) {
		Set<String> mySet = new TreeSet<String>();
		for (String s : strings) {
			mySet.add(s);
		}
		return mySet;
	}

	///// token tests/////
	@Test
	public void testEmptyFile() throws IOException {
		Reader in = new StringReader("");
		TokenScanner d = new TokenScanner(in);
		try {
			assertFalse("no next", d.hasNext());
			d.next();
			fail();

		} catch (NoSuchElementException e) {
			assertTrue("next raised exception", true);
		} finally {
			in.close();
		}
	}

	@Test
	public void testNullFile() throws IOException {
		Reader in = null;
		try {
			TokenScanner d = new TokenScanner(in);
			d.hasNext();
			d.next();
			fail();
		} catch (IllegalArgumentException e) {
			assertTrue("next raised illegal arg exception", true);
		}
	}

	@Test
	public void testSingleWord() throws IOException {
		Reader in = new StringReader("Lol");
		TokenScanner d = new TokenScanner(in);
		try {
			assertTrue("has next", d.hasNext());
			assertEquals("Lol", d.next());

		} finally {
			in.close();
		}
	}

	@Test
	public void testWordThenNonWord() throws IOException {
		Reader in = new StringReader("...a");
		TokenScanner d = new TokenScanner(in);
		try {
			assertTrue("has next", d.hasNext());
			assertEquals("...", d.next());
			assertEquals("a", d.next());

		} finally {
			in.close();
		}
	}

	@Test
	public void testSingleNonWord() throws IOException {
		Reader in = new StringReader("!@$!#!");
		TokenScanner d = new TokenScanner(in);
		try {
			assertTrue("has next", d.hasNext());
			assertEquals("!@$!#!", d.next());

		} finally {
			in.close();
		}
	}
	// all other tests are already covered in the tokentest class

	//// dictionary tests//////
	@Test(timeout = 500)
	public void d_testNumWOrds() throws IOException {
		Dictionary d = new Dictionary(new TokenScanner(new StringReader("bla...blaa...blaaa")));
		assertEquals(3, d.getNumWords());
	}

	@Test(timeout = 500)
	public void d_testEmptyString() throws IOException {
		Dictionary d = new Dictionary(new TokenScanner(new StringReader("")));
		assertFalse("empty str", d.isWord(""));
	}

	@Test(timeout = 500)
	public void d_testNUll() throws IOException {
		Dictionary d = new Dictionary(new TokenScanner(new StringReader("erger er ger ")));
		assertFalse("null", d.isWord(null));
	}

	@Test(timeout = 500)
	public void d_allCaps() throws IOException {
		Dictionary d = new Dictionary(new TokenScanner(new StringReader("erger er ger ")));
		assertTrue("caps", d.isWord("ER"));
	}

	@Test(timeout = 500)
	public void d_testWhite() throws IOException {
		Dictionary d = new Dictionary(new TokenScanner(new StringReader("erger     er   ger ")));
		assertTrue("whitespace", d.isWord("er"));
	}

	@Test(timeout = 500)
	public void d_testMultiple() throws IOException {
		Dictionary d = new Dictionary(new TokenScanner(new StringReader("erger er eR ")));
		assertEquals("multiple", 2, d.getNumWords());
	}

	///////// filecorrector

	@Test
	public void fc_whiteSpaces() throws IOException, FileCorrector.FormatException {
		Reader r = new StringReader("  meow   , meow!  ");
		Corrector c = new FileCorrector(r);
		assertEquals("working", makeSet(new String[] { "meow!" }), c.getCorrections("meow"));

	}

	@Test
	public void fc_noCorrection() throws IOException, FileCorrector.FormatException {
		Reader r = new StringReader("  meow   , meow!  ");
		Corrector c = new FileCorrector(r);
		assertEquals("working", makeSet(new String[] {}), c.getCorrections("woof"));

	}

	@Test
	public void fc_moreCorrections() throws IOException, FileCorrector.FormatException {
		Reader r = new StringReader("  meow   , meow! " + "\n meow ,  meow!! ");
		Corrector c = new FileCorrector(r);
		assertEquals("working", makeSet(new String[] { "meow!", "meow!!" }), c.getCorrections("meow"));

	}

	@Test
	public void fc_mixedCase() throws IOException, FileCorrector.FormatException {
		Reader r = new StringReader("  meow   , meow!  " + "\n meOw, meoww!" + "\n MEow  , miw");
		Corrector c = new FileCorrector(r);
		assertEquals("working", makeSet(new String[] { "Meow!", "Meoww!", "Miw" }), c.getCorrections("MeOw"));

	}

	@Test
	public void fc_allCaps() throws IOException, FileCorrector.FormatException {
		Reader r = new StringReader("  meow   , meow!  ");
		Corrector c = new FileCorrector(r);
		assertEquals("working", makeSet(new String[] { "Meow!" }), c.getCorrections("MEOW"));

	}

	@Test
	public void fc_allLower() throws IOException, FileCorrector.FormatException {
		Reader r = new StringReader("  meow   , meow!  ");
		Corrector c = new FileCorrector(r);
		assertEquals("working", makeSet(new String[] { "meow!" }), c.getCorrections("meow"));

	}

	@Test
	public void fc_multiCommas() throws IOException, FileCorrector.FormatException {
		Reader r = new StringReader("  meow  ,,,, meow!  ");

		try {
			Corrector c = new FileCorrector(r);
			fail();
		} catch (FileCorrector.FormatException e) {
			assertTrue(true);
		}

	}
	@Test
	public void fc_invalidArg() throws IOException, FileCorrector.FormatException {
		Reader r = new StringReader("  meow  , meow!  ");

		try {
			Corrector c = new FileCorrector(r);
			c.getCorrections(" ");
			
			fail();
		} catch (IllegalArgumentException e) {
			assertTrue(true);
		}

	}
	////// swap corrector

	@Test
	public void sc_testCorrect() throws IOException {
		Reader reader = new FileReader("smallDictionary.txt");
		try {
			Dictionary d = new Dictionary(new TokenScanner(reader));
			SwapCorrector swap = new SwapCorrector(d);
			assertEquals("correct", makeSet(new String[] {}), swap.getCorrections("cay"));

		} finally {
			reader.close();
		}
	}

	@Test
	public void sc_testCase() throws IOException {
		Reader reader = new FileReader("smallDictionary.txt");
		try {
			Dictionary d = new Dictionary(new TokenScanner(reader));
			SwapCorrector swap = new SwapCorrector(d);
			assertEquals("cya -> {cay}", makeSet(new String[] { "Cay" }), swap.getCorrections("CYa"));

		} finally {
			reader.close();
		}
	}

	@Test
	public void sc_testNull() throws IOException {
		Reader reader = new FileReader("smallDictionary.txt");
		try {
			Dictionary d = new Dictionary(new TokenScanner(reader));
			SwapCorrector swap = new SwapCorrector(null);
			fail();
			assertEquals("cya -> {cay}", makeSet(new String[] { "cay" }), swap.getCorrections("cya"));
			assertEquals("oYurs -> {yours}", makeSet(new String[] { "yours" }), swap.getCorrections("oYurs"));
		} catch (Exception e) {
			assertTrue("", true);
		} finally {
			reader.close();
		}
	}
	
	@Test
	public void sc_testInvalidArg() throws IOException {
		Reader reader = new FileReader("smallDictionary.txt");
		try {
			Dictionary d = new Dictionary(new TokenScanner(reader));
			SwapCorrector swap = new SwapCorrector(null);
			swap.getCorrections("erb regre");
			fail();
			assertEquals("cya -> {cay}", makeSet(new String[] { "cay" }), swap.getCorrections("cya"));
			assertEquals("oYurs -> {yours}", makeSet(new String[] { "yours" }), swap.getCorrections("oYurs"));
		} catch (Exception e) {
			assertTrue("", true);
		} finally {
			reader.close();
		}
	}
	
	

}