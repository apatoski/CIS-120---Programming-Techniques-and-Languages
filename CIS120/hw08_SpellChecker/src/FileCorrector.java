
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.io.*;

/**
 * A Corrector whose spelling suggestions are given in a text file.
 * <p>
 * One way to get corrections for a misspelled word is to consult an external
 * resource. This kind of Corrector uses a file that contains pairs of
 * misspelled and corrected words to generate suggestions.
 */
public class FileCorrector extends Corrector {

	/**
	 * A special purpose exception class to indicate errors when reading the
	 * input for the FileCorrector.
	 */

	public static class FormatException extends Exception {
		public FormatException(String msg) {
			super(msg);
		}
	}

	/**
	 * Constructs an instance from the supplied Reader.
	 *
	 * Instead of using the TokenScanner to parse this input, you should read
	 * the input line by line using a BufferedReader. This way you will practice
	 * with an alternative approach to working with text. For methods useful in
	 * parsing the lines of the File, see the String class documentation in
	 * java.lang.String
	 *
	 * <p>
	 * Each line in the input should have a single comma that separates two
	 * parts in the form: misspelled_word,corrected_version
	 *
	 * <p>
	 * For example:<br>
	 * 
	 * <pre>
	 * aligatur,alligator<br>
	 * baloon,balloon<br>
	 * inspite,in spite<br>
	 * who'ev,who've<br>
	 * ther,their<br>
	 * ther,there<br>
	 * </pre>
	 * <p>
	 * The lines are not case-sensitive, so all of the following lines should
	 * function equivalently:<br>
	 * 
	 * <pre>
	 * baloon,balloon<br>
	 * Baloon,balloon<br>
	 * Baloon,Balloon<br>
	 * BALOON,balloon<br>
	 * bAlOon,BALLOON<br>
	 * </pre>
	 * <p>
	 * You should ignore any leading or trailing whitespace around the
	 * misspelled and corrected parts of each line. Thus, the following lines
	 * should all be equivalent:<br>
	 * 
	 * <pre>
	 * inspite,in spite<br>
	 *    inspite,in spite<br>
	 * inspite   ,in spite<br>
	 *  inspite ,   in spite  <br>
	 * </pre>
	 * 
	 * Note that spaces are allowed inside the corrected word. (In general, the
	 * FileCorrector is allowed to suggest strings that are not words according
	 * to TokenScanner.)
	 *
	 * <p>
	 * You should throw a <code>FileCorrector.FormatException</code> if you
	 * encounter input that is invalid. For example, the FileCorrector
	 * constructor should throw an exception if any of these inputs are
	 * encountered:<br>
	 * 
	 * <pre>
	 * ,correct<br>
	 * wrong,<br>
	 * wrong correct<br>
	 * wrong,correct,<br>
	 * </pre>
	 * <p>
	 *
	 * @param r
	 *            The sequence of characters to parse
	 * @throws IOException
	 *             for an io error while reading
	 * @throws FileCorrector.FormatException
	 *             for an invalid line
	 * @throws IllegalArgumentException
	 *             if the provided reader is null
	 */

	Map<String, Set<String>> corrector;
	BufferedReader buffer;

	public FileCorrector(Reader r) throws IOException, FormatException {
		if (r == null) {
			throw new IllegalArgumentException();
		} else {
			buffer = new BufferedReader(r);
			corrector = new TreeMap<String, Set<String>>();
			try {
				String cur, miss, correct;
				while ((cur = buffer.readLine()) != null) {
					miss = getMiss(cur);
					correct = getCorrection(cur);

					if (invalid(cur, miss, correct)) {
						throw new FormatException("invalid format");
					}

					if (!corrector.containsKey(miss)) {
						corrector.put(miss, new TreeSet<String>());
						corrector.get(miss).add(correct);
					} else {
						corrector.get(miss).add(correct);
					}
				}
			} catch (IOException e) {
				throw e;
			}
		}

	}

	private boolean invalid(String cur, String miss, String correct) {
		if (correct == null || miss == null || cur.indexOf(',') != cur.lastIndexOf(',')) {
			return true;
		}
		if (!TokenScanner.isWord(miss)) {
			return true;
		}
		if (cur.replace(',', '!') == cur) {
			return true;
		}
		return false;
	}

	private String getCorrection(String cur) {
		try {
			return cur.substring(cur.indexOf(',') + 1).trim().toLowerCase();
		} catch (Exception e) {
			return null;
		}
	}

	private String getMiss(String cur) {
		try {
			return cur.substring(0, cur.indexOf(',')).trim().toLowerCase();
		} catch (Exception e) {
			return null;
		}
	}

	/**
	 * Construct a FileCorrector from a file.
	 *
	 * @param filename
	 *            of file to read from
	 * @throws IOException
	 *             if error while reading
	 * @throws FileCorrector.FormatException
	 *             for an invalid line
	 * @throws FileNotFoundException
	 *             if file cannot be opened
	 */
	public static FileCorrector make(String filename) throws IOException, FormatException {
		Reader r = new FileReader(filename);
		FileCorrector fc;
		try {
			fc = new FileCorrector(r);
		} finally {
			if (r != null) {
				r.close();
			}
		}
		return fc;
	}

	/**
	 * Returns a set of proposed corrections for an incorrect word. The
	 * corrections should match the case of the input; the matchCase method will
	 * be helpful here.
	 * <p>
	 * See the super class for more information.
	 *
	 * @param wrong
	 *            the misspelled word.
	 * @return a (potentially empty) set of proposed corrections
	 * @throws IllegalArgumentException
	 *             if the input is not a valid word (i.e. only composed of
	 *             letters and/or apostrophes)
	 */
	public Set<String> getCorrections(String wrong) {
        if(!TokenScanner.isWord(wrong)){
        	throw new IllegalArgumentException();
        }
		if (corrector.containsKey(wrong.toLowerCase())) {
			return matchCase(wrong, corrector.get(wrong.toLowerCase()));
		} else
			return new TreeSet<String>();

	}
}
