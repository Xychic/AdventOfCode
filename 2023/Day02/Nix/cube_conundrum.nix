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
              lib.strings.splitString " " (builtins.head split)
            ) 1
          );
          cubes = builtins.foldl' (acc: x: (data: 
            if data.colour == "red" then 
              {red=lib.max acc.red data.count; green=acc.green; blue=acc.blue;}
            else if data.colour == "green" then
              {red=acc.red; green=lib.max acc.green data.count; blue=acc.blue;}
            else 
              {red=acc.red; green=acc.green; blue=lib.max acc.blue data.count;}
          )(
            {count = lib.strings.toInt(builtins.head x); colour = builtins.elemAt x 1;})) 
            {red=0; green=0; blue=0;} 
            (map (lib.strings.splitString " ") (lib.strings.splitString ", " (lib.strings.replaceStrings [";"] [","] (builtins.elemAt split 1))));
        }
      )(lib.strings.splitString ": " line)
  )(
    lib.lists.init (lib.strings.splitString "\n" input)
  );
  parsed = parse (builtins.readFile ../input.txt);
  
  part1' = input: builtins.foldl' builtins.add 0 (map (x: x.id) (lib.filter (x: x.cubes.red <= 12 && x.cubes.green <= 13 && x.cubes.blue <= 14) input));
  part1 = input: (
    lib.fix (
      self: {
        test_result = part1' (parse TEST_1_INPUT);
        result = if self.test_result == TEST_1_ANSWER then part1' input else {expected=TEST_1_ANSWER; received=self.test_result;};
      }
    )
  ).result;

  part2' = input: builtins.foldl' (acc: x: acc + (x.cubes.red * x.cubes.green * x.cubes.blue)) 0 input;
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
  TEST_1_ANSWER = 8;
  TEST_2_INPUT = TEST_1_INPUT;
  TEST_2_ANSWER = 2286;


  result = {part1 = part1 parsed; part2 = part2 parsed;};
}
