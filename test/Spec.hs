import qualified Data.List as List
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import HelpParser
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Text.ParserCombinators.ReadP

main = defaultMain $ testGroup "Tests" [optNameTests, propertyTests, currentTests, unsupportedCases]

currentTests =
  testGroup
    "\n ============= unit tests of parse  ============= "
    [ test_parser "--help   baba keke" (["--help"], "", "baba keke"),
      test_parser "-h,--help   baba keke" (["-h", "--help"], "", "baba keke"),
      test_parser "--           baba" (["--"], "", "baba"),
      test_parser "-h, --help   baba" (["-h", "--help"], "", "baba"),
      test_parser "-o ARG   baba" (["-o"], "ARG", "baba"),
      test_parser "-o,--out ARG   baba" (["-o", "--out"], "ARG", "baba"),
      test_parser "-o,--out=ARG   baba" (["-o", "--out"], "ARG", "baba"),
      test_parser "-o,--out ARG: baba" (["-o", "--out"], "ARG", "baba"),
      test_parser "-o,--out ARG\n   baba" (["-o", "--out"], "ARG", "baba"),
      test_parser "-o,--out ARG:  baba" (["-o", "--out"], "ARG", "baba"),
      test_parser "-o ARG   baba" (["-o"], "ARG", "baba"),
      test_parser "-o <ARG1, ARG2>   baba" (["-o"], "<ARG1, ARG2>", "baba"),
      test_parser "-o <ARG1>,<ARG2>   baba" (["-o"], "<ARG1>,<ARG2>", "baba"),
      test_parser "-o=ARG   baba" (["-o"], "ARG", "baba"),
      test_parserMult "--out=ARG[,ARG2] baba" [(["--out"], "ARG", "baba"), (["--out"], "ARG,ARG2", "baba")],
      test_parser "--out, -o ARG    baba" (["--out", "-o"], "ARG", "baba"),
      test_parser "-out: baba" (["-out"], "", "baba"),
      test_parser "-out:\n baba" (["-out"], "", "baba"),
      -- examples in the wild
      test_parser
        "  -E, --show-ends          display $ at end of each line"
        (["-E", "--show-ends"], "", "display $ at end of each line"),
      test_parser
        " -h --help       Print this help file and exit"
        (["-h", "--help"], "", "Print this help file and exit"),
      test_parser
        "--min_length    Sets an artificial lower limit"
        (["--min_length"], "", "Sets an artificial lower limit"),
      ---- tar ----
      test_parser
        "-A, --catenate, --concatenate   append tar files to an archive"
        (["-A", "--catenate", "--concatenate"], "", "append tar files to an archive"),
      --
      test_parser
        "-w, --line-width int                  line width"
        (["-w", "--line-width"], "int", "line width"),
      test_parser
        "--stderr=e|a|c           change stderr output mode"
        (["--stderr"], "e|a|c", "change stderr output mode"),
      ---- rsync ----
      test_parser
        "--remote-option=OPT, -M  send OPTION to the remote side only"
        (["--remote-option", "-M"], "OPT", "send OPTION to the remote side only"),
      --
      ---- conda ----
      test_parser
        " -p PATH, --prefix PATH\n           Full path to environment location"
        (["-p", "--prefix"], "PATH", "Full path to environment location"),
      ---- minimap2 ----
      test_parserMult
        "--cs[=STR]   output the cs tag; STR is 'short' (if absent) or 'long' [none]"
        [ (["--cs"], "", "output the cs tag; STR is 'short' (if absent) or 'long' [none]"),
          (["--cs"], "STR", "output the cs tag; STR is 'short' (if absent) or 'long' [none]")
        ],
      test_parserMult
        " -O INT[,INT] gap open penalty [4,24]"
        [ (["-O"], "INT", "gap open penalty [4,24]"),
          (["-O"], "INT,INT", "gap open penalty [4,24]")
        ],
      ---- stack ----
      test_parserMult
        "--[no-]dump-logs         Enable/disable dump the build output logs"
        [ (["--dump-logs"], "", "Enable/disable dump the build output logs"),
          (["--no-dump-logs"], "", "Enable/disable dump the build output logs")
        ],
      ---- youtube-dl ---
      test_parser
        "    -4, --force-ipv4                     Make all connections via IPv4"
        (["-4", "--force-ipv4"], "", "Make all connections via IPv4"),
      test_parser
        "    -u, --username USERNAME              Login with this account ID"
        (["-u", "--username"], "USERNAME", "Login with this account ID"),
      ---- blastn ----
      test_parser
        " -template_type <String, `coding', `coding_and_optimal', `optimal'>\n    Discontiguous MegaBLAST template type"
        (["-template_type"], "<String, `coding', `coding_and_optimal', `optimal'>", "Discontiguous MegaBLAST template type")
    ]

unsupportedCases =
  testGroup
    "\n ============= Unsupported corner cases against parse ============= "
    [
      test_parser
        " -task <String, Permissible values: 'blastn' 'blastn-short' 'dc-megablast'\n          'megablast' 'rmblastn' >\n         Task to execute"
        (["-task"], "<String, Permissible values: 'blastn' 'blastn-short' 'dc-megablast' ...>", "Task to execute"),
      test_parser
        " -window_size <Integer, >=0>\n      Multiple hits window size, use 0 to specify 1-hit algorithm"
        (["-window_size"], "<Integer, >=0>", "Multiple hits window size, use 0 to specify 1-hit algorithm")
    ]

optNameTests =
  testGroup
    "\n ============= Test optName ============= "
    [ testCase "optName (long)" $
        readP_to_S optName "--help" @?= [(OptName "--help" LongType, "")],
      testCase "optName (short)" $
        readP_to_S optName "-o" @?= [(OptName "-o" ShortType, "")],
      testCase "optName (old)" $
        readP_to_S optName "-azvhP" @?= [(OptName "-azvhP" OldType, "")],
      testCase "optName (double dash alone)" $
        readP_to_S optName "-- " @?= [(OptName "--" DoubleDashAlone, "")]
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

test_parser :: String -> ([String], String, String) -> TestTree
test_parser s (names, args, desc) =
  testCase s $ do
    List.sort actual @?= expected
  where
    actual = parse s
    expected = List.sort [makeOpt names args desc]

test_parserMult :: String -> [([String], String, String)] -> TestTree
test_parserMult s tuples =
  testCase s $ do
    List.sort actual @?= expected
  where
    actual = parse s
    expected = List.sort [makeOpt names args desc | (names, args, desc) <- tuples]
