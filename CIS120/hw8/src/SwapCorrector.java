
import java.util.Set;
import java.util.TreeSet;

/**
 * A Corrector whose spelling suggestions come from "swapped letter" typos.
 * <p>
 * A common misspelling is accidentally swapping two adjacent letters, e.g.
 * "with" -> "wiht". This Corrector is given a Dictionary of valid words and
 * proposes corrections that are precisely one "swap" away from the incorrect
 * word.
 * <p>
 * For example, if the incorrect word is "haet", then this Corrector might
 * suggest "heat" and "hate", provided that both of these words occur in the
 * dictionary.
 * <p>
 * Only swaps between adjacent letters are considered by this corrector.
 */
public class SwapCorrector extends Corrector {

	/**
	 * Constructs a SwapCorrector instance using the given Dictionary.
	 *
	 * @param dict
	 *            the reference dictionary to use to look for corrections
	 *            arising from letter swaps
	 * @throws IllegalArgumentException
	 *             if the provided Dictionary is null
	 */
	Dictionary dictionary;

	public SwapCorrector(Dictionary dict) {
		if (dict == null) {
			throw new IllegalArgumentException();
		}
		dictionary = dict;

	}

	/**
	 * Suggests as corrections any words in the Dictionary (provided when the
	 * object is constructed) that are one swap away from the input word. A swap
	 * is defined as two adjacent letters exchanging positions.
	 * <p>
	 * For example, if the dictionary contains the word "heat" and "hate", then
	 * both should be returned if the input word is "haet".
	 * <p>
	 * If the provided word is already spelled correctly, then the empty set
	 * should be returned.
	 *
	 * See superclass documentation for more information.
	 *
	 * @param wrong
	 *            the input word
	 * @return a (potentially empty) set of proposed corrections
	 * @throws IllegalArgumentException
	 *             if the input is not a valid word (i.e. only composed of
	 *             letters and/or apostrophes)
	 */
	public Set<String> getCorrections(String wrong) {
		if(!TokenScanner.isWord(wrong)){
			throw new IllegalArgumentException();
		}
		char h;
		Set<String> set = new TreeSet<String>();
		for (int i = 0; i < wrong.length() - 1; i++) {
			char w[] = wrong.toCharArray();
			h = w[i];
			w[i] = w[i + 1];
			w[i + 1] = h;
			String mod = String.valueOf(w);

			if (dictionary.isWord(mod.toLowerCase())) {
				set.add(mod);
			}
		}
		return matchCase(wrong, set);
	}
}
