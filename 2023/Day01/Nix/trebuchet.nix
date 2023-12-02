let
  pkgs = import <nixpkgs> {};
in rec {
  inherit (pkgs) lib;

  parse = input: (lib.lists.init (lib.strings.splitString "\n" input));
  parsed = parse (builtins.readFile ../input.txt);
  
  is_digit = char: (lib.lists.findSingle (digit: digit == char) "" "" ["1" "2" "3" "4" "5" "6" "7" "8" "9"]) != "";

  part1' = input: builtins.foldl' builtins.add 0 (
    map (
      line: (
        lib.strings.toInt (
          (lib.lists.findFirst is_digit "" line) + (lib.lists.findFirst is_digit "" (lib.lists.reverseList line))
        )
      )
    ) (map lib.strings.stringToCharacters input)
  );

  part1 = input: (
    lib.fix (
      self: {
        test_result = part1' (parse TEST_1_INPUT);
        result = if self.test_result == TEST_1_ANSWER then part1' input else "Expected '${toString TEST_1_ANSWER}' got '${toString self.test_result}'";
      }
    )
  ).result;

  replace' = a: b: (if a == b then a else replace' (builtins.replaceStrings ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"] ["o1e" "t2o" "t3e" "f4r" "f5e" "s6x" "s7n" "e8t" "n9e"] a) a);
  replace = x: replace' x "";


  part2' = input: part1' (map replace input);
  part2 = input: (
    lib.fix (
      self: {
        test_result = part2' (parse TEST_2_INPUT);
        result = if self.test_result == TEST_2_ANSWER then part2' input else "Expected '${toString TEST_2_ANSWER}' got '${toString self.test_result}'";
      }
    )
  ).result;

  TEST_1_INPUT = "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
";
  TEST_1_ANSWER = 142;
  TEST_2_INPUT = "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
";
  TEST_2_ANSWER = 281;


  result = {part1 = part1 parsed; part2 = part2 parsed;};
}
