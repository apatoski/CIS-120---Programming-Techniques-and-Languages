import java.util.HashSet;
import java.util.Set;

/**
 *
 * A smart Corrector that uses "edit distance" to generate corrections.
 * <p>
 * NOTE: This corrector is a "challenge" problem and does not count towards your grade.
 * <p>
 * The Levenshtein distance between two words is the smallest number of edits
 * needed to transform one word to the other. An "edit" can be either an:
 * <ul>
 * <li> insertion of a letter
 * <li> deletion of a letter
 * <li> substitution of one letter for one other letter
 * </ul>
 *
 * A "letter" is one of a-z (we do not count apostrophes here).
 * Note that swaps (thsi -> this) do <it>not</it> count as an edit.
 * <p>
 * This Corrector suggests any words in the dictionary that are exactly a
 * distance of one edit away from the given incorrect word.
 */
public class Levenshtein extends Corrector {
  

  /**
   * Constructs a Levenshtein Corrector instance using the given Dictionary.
   * Should throw an <code>IllegalArgumentException</code> if the dictionary
   * provided is null.
   *
   * @param dict
   */
  public Levenshtein(Dictionary dict) {
    throw new UnsupportedOperationException(); // STUB
  }

  /**
   * @param s the input string
   * @return all the words that are one deletion away from s
   */
  Set<String> getDeletions(String s) {
    throw new UnsupportedOperationException(); // STUB
  }

 /**
  * @param s the input string
  * @return all the words that are one substitution away from s
  */
  public Set<String> getSubstitutions(String s) {
    throw new UnsupportedOperationException(); // STUB
  }

  
  /**
   * @param s the input string
   * @return all words that are one insertion away from s
   */
  public Set<String> getInsertions(String s) {
    throw new UnsupportedOperationException(); // STUB
  }

  public Set<String> getCorrections(String wrong) {
    throw new UnsupportedOperationException(); // STUB
  }
}
