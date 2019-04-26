import java.util.Arrays;

class Anagram {
  boolean isAnagram(String word1, String word2) {
    return Arrays.sort(word1) == Arrays.sort(word2);
  }
  String[] Anagrams(String[] words) {
    
  }
}
