def is_pangram(sentence):
  stripped_sent = sorted(set(filter(lambda x: x not in ' "_,.;:0123456789', sentence.lower())))
  return stripped_sent == list("abcdefghijklmnopqrstuvwxyz")