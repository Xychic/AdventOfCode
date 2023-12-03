let
  pkgs = import <nixpkgs> {};
in rec {
  inherit (pkgs) lib;

  parse = input: map (
    line: 
      (
        split: { 
          id = lib.strings.toInt(
            builtins.elemAt (
              lib.strings.splitString " " (builtins.elemAt split 0)
            ) 1
          );
          val = {
            red = 0;
            green = 0;
            blue = 0;
          };
        }
      )(lib.strings.splitString ": " line)
  )(
    lib.lists.init (lib.strings.splitString "\n" input)
  );
  parsed = parse (builtins.readFile ../input.txt);
  
  part1' = input: input;
  part1 = input: (
    lib.fix (
      self: {
        test_result = part1' (parse TEST_1_INPUT);
        result = if self.test_result == TEST_1_ANSWER then part1' input else {expected=TEST_1_ANSWER; received=self.test_result;};
      }
    )
  ).result;

  part2' = input: input;
  part2 = input: (
    lib.fix (
      self: {
        test_result = part2' (parse TEST_2_INPUT);
        result = if self.test_result == TEST_2_ANSWER then part2' input else {expected=TEST_2_ANSWER; received=self.test_result;};
      }
    )
  ).result;

  TEST_1_INPUT = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n";
  TEST_1_ANSWER = 0;
  TEST_2_INPUT = TEST_1_INPUT;
  TEST_2_ANSWER = 0;


  result = {part1 = part1 parsed; part2 = part2 parsed;};
}
