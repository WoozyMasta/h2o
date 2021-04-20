import qualified Data.List as List
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import HelpParser
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Text.ParserCombinators.ReadP

main = defaultMain $ testGroup "Tests" [optNameTests, propertyTests, currentTests]

currentTests =
  testGroup
    "Unit tests"
    [
      test_runner "--help   baba keke" (["--help"], "", "baba keke"),
      test_runner "-h,--help   baba keke" (["-h", "--help"], "", "baba keke"),
      test_runner "-h, --help   baba" (["-h", "--help"], "", "baba"),
      test_runner "-o ARG   baba" (["-o"], "ARG", "baba"),
      test_runner "-o,--out ARG   baba" (["-o", "--out"], "ARG", "baba"),
      test_runner "-o,--out=ARG   baba" (["-o", "--out"], "ARG", "baba"),
      test_runner "-o,--out ARG: baba" (["-o", "--out"], "ARG", "baba"),
      test_runner "-o,--out ARG\n   baba" (["-o", "--out"], "ARG", "baba"),
      test_runner "-o,--out ARG:  baba" (["-o", "--out"], "ARG", "baba"),
      test_runner "-o ARG   baba" (["-o"], "ARG", "baba"),
      test_runner "-o=ARG   baba" (["-o"], "ARG", "baba"),
      test_runnerMult "--out=ARG[,ARG2] baba" [(["--out"], "ARG", "baba"), (["--out"], "ARG,ARG2", "baba")],
      test_runner "--out, -o ARG    baba" (["--out", "-o"], "ARG", "baba"),
      test_runner "-out: baba" (["-out"], "", "baba"),
      test_runner "-out:\n baba" (["-out"], "", "baba"),
      -- examples in the wild
      test_runner
        "  -E, --show-ends          display $ at end of each line"
        (["-E", "--show-ends"], "", "display $ at end of each line"),
      test_runner
        " -h --help       Print this help file and exit"
        (["-h", "--help"], "", "Print this help file and exit"),
      test_runner
        "--min_length    Sets an artificial lower limit"
        (["--min_length"], "", "Sets an artificial lower limit"),
      ---- tar ----
      test_runner
        "-A, --catenate, --concatenate   append tar files to an archive"
        (["-A", "--catenate", "--concatenate"], "", "append tar files to an archive"),
      --
      test_runner
        "-w, --line-width int                  line width"
        (["-w", "--line-width"], "int", "line width"),
      test_runner
        "--stderr=e|a|c           change stderr output mode"
        (["--stderr"], "e|a|c", "change stderr output mode"),
      ---- rsync ----
      test_runner
        "--remote-option=OPT, -M  send OPTION to the remote side only"
        (["--remote-option", "-M"], "OPT", "send OPTION to the remote side only"),
      --
      ---- conda ----
      test_runner
        " -p PATH, --prefix PATH\n           Full path to environment location"
        (["-p", "--prefix"], "PATH", "Full path to environment location"),
      --
      ---- minimap2 ----
      test_runnerMult
        "--cs[=STR]   output the cs tag; STR is 'short' (if absent) or 'long' [none]"
        [ (["--cs"], "", "output the cs tag; STR is 'short' (if absent) or 'long' [none]"),
          (["--cs"], "STR", "output the cs tag; STR is 'short' (if absent) or 'long' [none]")
        ],
      --
      test_runnerMult
        " -O INT[,INT] gap open penalty [4,24]"
        [ (["-O"], "INT", "gap open penalty [4,24]"),
          (["-O"], "INT,INT", "gap open penalty [4,24]")
        ],
      --
      ---- stack ----
      --        (I don't know how to handle such cases properly...)
      test_runnerMult
        "--[no-]dump-logs         Enable/disable dump the build output logs"
        [ (["--dump-logs"], "", "Enable/disable dump the build output logs"),
          (["--no-dump-logs"], "", "Enable/disable dump the build output logs")
        ]
    ]


optNameTests =
  testGroup
    "Test optName"
    [
      testCase "optName (long)" $
        readP_to_S optName "--help" @?= [(OptName "--help" LongType, "")],
      testCase "optName (short)" $
        readP_to_S optName "-o" @?= [(OptName "-o" ShortType, "")],
      testCase "optName (old)" $
        readP_to_S optName "-azvhP" @?= [(OptName "-azvhP" OldType, "")],
      testCase "optName (double dash alone)" $
        readP_to_S optName "-- " @?= [(OptName "--" DoubleDashAlone, "")]
    ]

prevTests =
  testGroup
    "Unit tests"
    [
      test_optItem "--help   baba keke" (["--help"], "", "baba keke"),
      test_optItem "-h,--help   baba keke" (["-h", "--help"], "", "baba keke"),
      test_optItem "-h, --help   baba" (["-h", "--help"], "", "baba"),
      test_optItem "-o ARG   baba" (["-o"], "ARG", "baba"),
      test_optItem "-o,--out ARG   baba" (["-o", "--out"], "ARG", "baba"),
      test_optItem "-o,--out=ARG   baba" (["-o", "--out"], "ARG", "baba"),
      test_optItem "-o,--out ARG: baba" (["-o", "--out"], "ARG", "baba"),
      test_optItem "-o,--out ARG\n   baba" (["-o", "--out"], "ARG", "baba"),
      test_optItem "-o,--out ARG:  baba" (["-o", "--out"], "ARG", "baba"),
      test_optItem "-o ARG   baba" (["-o"], "ARG", "baba"),
      test_optItem "-o=ARG   baba" (["-o"], "ARG", "baba"),
      test_optItem "--out=ARG[,ARG2] baba" (["--out"], "ARG[,ARG2]", "baba"),
      test_optItem "--out, -o ARG    baba" (["--out", "-o"], "ARG", "baba"),
      test_optItem "-out: baba" (["-out"], "", "baba"),
      test_optItem "-out:\n baba" (["-out"], "", "baba"),
      -- examples in the wild
      test_optItem
        "  -E, --show-ends          display $ at end of each line"
        (["-E", "--show-ends"], "", "display $ at end of each line"),
      test_optItem
        " -h --help       Print this help file and exit"
        (["-h", "--help"], "", "Print this help file and exit"),
      test_optItem
        "--min_length    Sets an artificial lower limit"
        (["--min_length"], "", "Sets an artificial lower limit"),
      ---- tar ----
      test_optItem
        "-A, --catenate, --concatenate   append tar files to an archive"
        (["-A", "--catenate", "--concatenate"], "", "append tar files to an archive"),
      --
      test_optItem
        "-w, --line-width int                  line width"
        (["-w", "--line-width"], "int", "line width"),
      test_optItem
        "--stderr=e|a|c           change stderr output mode"
        (["--stderr"], "e|a|c", "change stderr output mode"),
      ---- rsync ----
      test_optItem
        "--remote-option=OPT, -M  send OPTION to the remote side only"
        (["--remote-option", "-M"], "OPT", "send OPTION to the remote side only"),
      --
      ---- conda ----
      test_optItem
        " -p PATH, --prefix PATH\n           Full path to environment location"
        (["-p", "--prefix"], "PATH", "Full path to environment location"),
      --
      ---- minimap2 ----
      test_optItem
        "--cs[=STR]   output the cs tag; STR is 'short' (if absent) or 'long' [none]"
        (["--cs"], "[STR]", "output the cs tag; STR is 'short' (if absent) or 'long' [none]"),
      --
      test_optItem
        " -O INT[,INT] gap open penalty [4,24]"
        (["-O"], "INT[,INT]", "gap open penalty [4,24]"),
      --
      ---- stack ----
      --        (I don't know how to handle such cases properly...)
      test_optItemMult
        "--[no-]dump-logs         Enable/disable dump the build output logs"
        [ (["--dump-logs"], "", "Enable/disable dump the build output logs"),
          (["--no-dump-logs"], "", "Enable/disable dump the build output logs")
        ]
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "Hedgehog tests"
    [ testProperty "optName (long)" prop_longOpt,
      testProperty "optName (short)" prop_shortOpt,
      testProperty "optName (old)" prop_oldOpt
    ]

prop_longOpt :: Property
prop_longOpt =
  property $ do
    s <- forAll (Gen.string (Range.constant 1 10) Gen.alphaNum)
    let ddashed = "--" ++ s
    readP_to_S optName ddashed === [(OptName ddashed LongType, "")]

prop_shortOpt :: Property
prop_shortOpt =
  property $ do
    s <- forAll (Gen.string (Range.singleton 1) Gen.alphaNum)
    let dashed = "-" ++ s
    readP_to_S optName dashed === [(OptName dashed ShortType, "")]

prop_oldOpt :: Property
prop_oldOpt =
  property $ do
    s <- forAll (Gen.string (Range.constant 2 10) Gen.alphaNum)
    let dashed = "-" ++ s
    readP_to_S optName dashed === [(OptName dashed OldType, "")]

makeOpt :: [String] -> String -> String -> Opt
makeOpt names = Opt (map getOptName names)
  where
    getOptName s = case readP_to_S optName s of
      [(optname, _)] -> optname
      _ -> undefined

test_optItem :: String -> ([String], String, String) -> TestTree
test_optItem s (names, args, desc) =
  testCase s $
    actual @?= expected
  where
    actual = readP_to_S optItem s
    opt = makeOpt names args desc
    expected = [(opt, "")]

test_optItemMult :: String -> [([String], String, String)] -> TestTree
test_optItemMult s tuples =
  testCase s $ do
    List.sort actual @?= expected
  where
    actual = readP_to_S optItem s
    opts = List.sort [makeOpt names args desc | (names, args, desc) <- tuples]
    expected = [(opt, "") | opt <- opts]

test_runner :: String -> ([String], String, String) -> TestTree
test_runner s (names, args, desc) =
  testCase s $ do
    List.sort actual @?= expected
  where
    actual = runner s
    expected = List.sort [makeOpt names args desc]

test_runnerMult :: String -> [([String], String, String)] -> TestTree
test_runnerMult s tuples =
  testCase s $ do
    List.sort actual @?= expected
  where
    actual = runner s
    expected = List.sort [makeOpt names args desc | (names, args, desc) <- tuples]
