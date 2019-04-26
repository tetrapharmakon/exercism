#!/usr/bin/env bash

@test "An empty string" {
  run bash reverse_string.sh ""
  [ "$output" = "" ]
}

@test "A word" {
  run bash reverse_string.sh "robot"
  [ "$output" = "tobor" ]
}

@test "A capitalised word" {
  run bash reverse_string.sh "Ramen"
  [ "$output" = "nemaR" ]
}

@test "A sentence with punctuation" {
  run bash reverse_string.sh "I'm hungry!"
  [ "$output" = "!yrgnuh m'I" ]
}

@test "A palindrome" {
  run bash reverse_string.sh "racecar"
  [ "$status" -eq 0 ]
  [ "$output" = "racecar" ]
}
