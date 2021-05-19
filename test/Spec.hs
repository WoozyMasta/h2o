import Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.List as List
import Data.List.Extra (nubSort)
import GenBashCompletions
import GenFishCompletions
import GenZshCompletions
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import HelpParser
import Layout (getOptionLocations, getWidth, makeRanges, mergeRanges, mergeRangesFast)
import Subcommand (firstTwoWordsLoc)
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Text.ParserCombinators.ReadP
import Text.Printf
import Utils (getMostFrequent)

main =
  defaultMain $
    testGroup
      "Tests"
      [optNameTests, propertyTests, currentTests, devTests, unsupportedCases, miscTests, shellCompTests, shellCompGoldenTests, layoutTests]

currentTests =
  testGroup
    "\n ============= unit tests against parse  ============= "
    [ test_parser "--help   baba keke" (["--help"], "", "baba keke"),
      test_parser "-h,--help   baba keke" (["-h", "--help"], "", "baba keke"),
      test_parser "--           baba" (["--"], "", "baba"),
      test_parser "-h, --help   baba" (["-h", "--help"], "", "baba"),
      test_parser "-o ARG   baba" (["-o"], "ARG", "baba"),
      test_parser "-o,--out ARG   baba" (["-o", "--out"], "ARG", "baba"),
      test_parser "-o,--out=ARG   baba" (["-o", "--out"], "ARG", "baba"),
      test_parser "-o,--out ARG\n   baba" (["-o", "--out"], "ARG", "baba"),
      test_parser "-o,--out ARG:\n   baba" (["-o", "--out"], "ARG", "baba"),
      test_parser "-o <ARG1, ARG2>   baba" (["-o"], "<ARG1, ARG2>", "baba"),
      test_parser "-o <ARG1>,<ARG2>   baba" (["-o"], "<ARG1>,<ARG2>", "baba"),
      test_parser "-o<ARG1> <ARG2>   baba" (["-o"], "<ARG1>,<ARG2>", "baba"),
      test_parser "-o arg --output arg    baba" (["-o", "--output"], "arg", "baba"),
      test_parser "-o{arg} --output{arg}    baba" (["-o", "--output"], "{arg}", "baba"),
      test_parser "-o=ARG   baba" (["-o"], "ARG", "baba"),
      test_parserMult
        "--out=ARG[,ARG2] baba"
        [(["--out"], "ARG", "baba"), (["--out"], "ARG,ARG2", "baba")],
      test_parser "--out, -o ARG    baba" (["--out", "-o"], "ARG", "baba"),
      test_parser "--out=ARG1:ARG2\n baba" (["--out"], "ARG1:ARG2", "baba"),
      test_parser
        "-o FILE --out=FILE    without comma, with = sign"
        (["-o", "--out"], "FILE", "without comma, with = sign"),
      test_parser
        "-i <file>, --input <file>   with comma, without = sign"
        (["-i", "--input"], "<file>", "with comma, without = sign"),
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
      ---- gzip ----
      test_parser
        "-S .suf --suffix .suf\n       When compressing, use suffix .suf instead of .gz."
        (["-S", "--suffix"], ".suf", "When compressing, use suffix .suf instead of .gz."),
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
      ---- 7z ----
      test_parser
        "-o{Directory}\n       Set Output directory"
        (["-o"], "{Directory}", "Set Output directory"),
      test_parserMult
        "-si[{name}] : read data from stdin"
        [(["-si"], "", "read data from stdin"), (["-si"], "{name}", "read data from stdin")],
      ---- youtube-dl ---
      test_parser
        "    -4, --force-ipv4                     Make all connections via IPv4"
        (["-4", "--force-ipv4"], "", "Make all connections via IPv4"),
      ---- stack ----
      test_parser
        "--docker*                Run 'stack --docker-help' for details"
        (["--docker*"], "", "Run 'stack --docker-help' for details"),
      test_parser
        "    -u, --username USERNAME              Login with this account ID"
        (["-u", "--username"], "USERNAME", "Login with this account ID"),
      ---- blastn ----
      test_parser
        " -template_type <String, `coding', `coding_and_optimal', `optimal'>\n    Discontiguous MegaBLAST template type"
        (["-template_type"], "<String, `coding', `coding_and_optimal', `optimal'>", "Discontiguous MegaBLAST template type"),
      ---- readseq ----
      test_parserMult
        " -wid[th]=#            sequence line width"
        [(["-wid"], "#", "sequence line width"), (["-width"], "#", "sequence line width")],
      test_parser
        "-extract=1000..9999  * extract all features, sequence from given base range"
        (["-extract"], "1000..9999", "* extract all features, sequence from given base range"),
      test_parserMult
        "-feat[ures]=exon,CDS...   extract sequence of selected features"
        [(["-feat"], "exon,CDS...", "extract sequence of selected features"), (["-features"], "exon,CDS...", "extract sequence of selected features")],
      ---- bowtie2 ----
      test_parser
        "-t/--time          print wall-clock time taken by search phases"
        (["-t", "--time"], "", "print wall-clock time taken by search phases"),
      test_parser
        " -p/--threads <int> number of alignment threads to launch (1)"
        (["-p", "--threads"], "<int>", "number of alignment threads to launch (1)"),
      test_parser
        "-F k:<int>,i:<int> query input files are continuous FASTA where reads"
        (["-F"], "k:<int>,i:<int>", "query input files are continuous FASTA where reads"),
      ---- samtools ----
      test_parser
        "-d STR:STR\n         only include reads with tag STR and associated value STR [null]"
        (["-d"], "STR:STR", "only include reads with tag STR and associated value STR [null]"),
      test_parserMult
        " --input-fmt-option OPT[=VAL]\n               Specify a single input file format option in the form"
        [ (["--input-fmt-option"], "OPT", "Specify a single input file format option in the form"),
          (["--input-fmt-option"], "OPT=VAL", "Specify a single input file format option in the form")
        ],
      test_parser
        "-@, --threads INT\n           Number of additional threads to use [0]"
        (["-@", "--threads"], "INT", "Number of additional threads to use [0]"),
      ---- bcftools ----
      test_parserMult
        "-S, --samples-file [^]<file>   file of samples to annotate (or exclude with \"^\" prefix)"
        [ (["-S", "--samples-file"], "<file>", "file of samples to annotate (or exclude with \"^\" prefix)"),
          (["-S", "--samples-file"], "^<file>", "file of samples to annotate (or exclude with \"^\" prefix)")
        ],
      ---- gridss ----
      test_parser "-o/--output: output VCF." (["-o", "--output"], "", "output VCF."),
      ---- minimap2 ----
      test_parser
        "-w INT\t Minimizer window size [2/3 of k-mer length]."
        (["-w"], "INT", "Minimizer window size [2/3 of k-mer length]."),
      ---- parallel ----
      test_parser
        "--tmpl file=repl         Copy file to repl."
        (["--tmpl"], "file=repl", "Copy file to repl."),
      test_parser
        " --tmux (Long beta testing)       Use tmux for output."
        (["--tmux"], "(Long beta testing)", "Use tmux for output.")
    ]

unsupportedCases :: TestTree
unsupportedCases =
  expectFail $
    testGroup
      "\n ============= Unsupported corner cases parse fail ============= "
      [ -- BAD case illustrated in docopt
        test_parser "--verbose MORE text." (["--verbose"], "MORE", "text."),
        ---- 7z --help ----
        test_parserMult
          " -i[r[-|0]]{@listfile|!wildcard} : Include filenames"
          [(["-i"], "{@listfile|!wildcard}", "Include filenames"), (["-ir"], "{@listfile|!wildcard}", "Include filenames")],
        ---- bcftools ----
        test_parser
          "-s, --samples [^]<list>       comma separated list of samples to include (or exclude with \"^\" prefix)"
          (["-s", "--samples"], "[^]<list>", "comma separated list of samples to include (or exclude with \"^\" prefix)"),
        test_parserMult
          "-h/H, --header-only/--no-header     print the header only/suppress the header in VCF output"
          [(["-h", "--header-only"], "", "print the header only"), (["-H", "--no-header"], "", "suppress the header in VCF output")],
        test_parser
          "--ploidy <assembly>[?]      predefined ploidy, 'list' to print available settings, append '?' for details"
          (["--ploidy"], "<assembly>[?]", "predefined ploidy, 'list' to print available settings, append '?' for details"),
        test_parser
          " -g, --gvcf <int>,[...]          group non-variant sites into gVCF blocks by minimum per-sample DP"
          (["-g", "--gvcf"], "<int>,[...]", "group non-variant sites into gVCF blocks by minimum per-sample DP"),
        ---- parallel ----
        test_parser
          "       --line-buffer\n       --lb\n           Buffer output on line basis. --group will keep the output together for a whole job."
          (["--line-buffer", "--lb"], "", "Buffer output on line basis. --group will keep the output together for a whole job."),
        ---- bwa -----
        test_parserMult
          "-I FLOAT[,FLOAT[,INT[,INT]]]\n     Specify  the  mean"
          [(["-I"], "FLOAT", "Specify  the  mean"), (["-I"], "FLOAT,FLOAT", "Specify  the  mean"), (["-I"], "FLOAT,FLOAT,INT", "Specify  the  mean"), (["-I"], "FLOAT,FLOAT,INT,INT", "Specify  the  mean")],
        ---- blastn ----
        test_parser
          " -task <String, Permissible values: 'blastn' 'blastn-short' 'dc-megablast'\n          'megablast' 'rmblastn' >\n         Task to execute"
          (["-task"], "<String, Permissible values: 'blastn' 'blastn-short' 'dc-megablast' ...>", "Task to execute"),
        test_parser
          " -window_size <Integer, >=0>\n      Multiple hits window size, use 0 to specify 1-hit algorithm"
          (["-window_size"], "<Integer, >=0>", "Multiple hits window size, use 0 to specify 1-hit algorithm"),
        ---- octopus ----
        test_parser
          " --inactive-flank-scoring arg (=1)     Disables additional calculation"
          (["--inactive-flank-scoring"], "arg", "Disables additional calculation"),
        ---- delly ----
        test_parserMult
          "-o [ --outfile ] arg (=\"sv.bcf\")   SV BCF output file"
          [(["-o"], "arg (=\"sv.bcf\")", "SV BCF output file"), (["-o", "--outfile"], "arg (=\"sv.bcf\")", "SV BCF output file")],
        ---- fastqc ----
        test_parserMult
          "    -c              Specifies a non-default file which contains the list of\n\
          \    --contaminants  contaminants to screen overrepresented sequences against.\n\
          \                    The file must contain sets of named contaminants in the\n\
          \                    form name[tab]sequence.  Lines prefixed with a hash will\n\
          \                    be ignored."
          [(["-c", "--contaminants"], "", "Specifies a non-default file which contains the list of contaminants to screen overrepresented sequences against.")]
      ]

devTests =
  testGroup
    "\n ============= tests against parseMany  ============= "
    [ test_parseMany
        "--help   baba\n    -i <file>, --input=<file>   keke"
        [(["--help"], "", "baba"), (["-i", "--input"], "<file>", "keke")],
      test_parseMany
        "--help   baba\n      !!!JUNK LINE!!!\n    -i <file>, --input=<file>   keke"
        [(["--help"], "", "baba"), (["-i", "--input"], "<file>", "keke")],
      test_parseMany
        "--he[lp]   baba\n      !!!JUNK LINE!!!\n    -i <file>, --input=<file>   keke"
        [(["--help"], "", "baba"), (["--he"], "", "baba"), (["-i", "--input"], "<file>", "keke")],
      test_parseMany
        "--he[lp]\n       baba\n      !!!JUNK LINE!!!\n      !!!ANOTHER JUNK!!!\n       -i <file>, --input=<file>   keke"
        [(["--help"], "", "baba"), (["--he"], "", "baba"), (["-i", "--input"], "<file>", "keke")],
      test_parseMany
        "       -w INT\t Minimizer window size [2/3 of k-mer length]. A minimizer is the smallest k-mer in a window of w consecutive  k-"
        [(["-w"], "INT", "Minimizer window size [2/3 of k-mer length]. A minimizer is the smallest k-mer in a window of w consecutive  k-")],
      test_parseMany
        "       -w INT\t Minimizer window size [2/3 of k-mer length]. A minimizer is the smallest k-mer in a window of w consecutive  k-\n\t\t mers.\n\n       -H\t Use  homopolymer-compressed (HPC) minimizers. An HPC sequence is constructed by contracting homopolymer runs to\n\t\t a single base. An HPC minimizer is a minimizer on the HPC sequence.\n"
        [ (["-w"], "INT", "Minimizer window size [2/3 of k-mer length]. A minimizer is the smallest k-mer in a window of w consecutive  k-"),
          (["-H"], "", "Use  homopolymer-compressed (HPC) minimizers. An HPC sequence is constructed by contracting homopolymer runs to")
        ]
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

miscTests =
  testGroup
    "\n ============ misc =============="
    [ testCase "firstTwoWordsLoc: empty" $
        firstTwoWordsLoc
          "  "
          @?= (-1, -1),
      testCase "firstTwoWordsLoc: single word" $
        firstTwoWordsLoc
          " hi"
          @?= (1, -1),
      testCase "firstTwoWordsLoc 0" $
        firstTwoWordsLoc
          "  stop        Stop one or more running containers   "
          @?= (2, 14),
      testCase "firstTwoWordsLoc 1" $
        firstTwoWordsLoc
          "stop        Stop one or more running containers   "
          @?= (0, 12),
      testCase "firstTwoWordsLoc 2" $
        firstTwoWordsLoc
          "   hi               there   "
          @?= (3, 20),
      testCase "getMostFrequent [1, -4, 2, 9, 1, -4, -3, 7, -4, -4, 1] == Just (-4)" $
        getMostFrequent [1, -4, 2, 9, 1, -4, -3, 7, -4, -4, 1] @?= Just (-4)
    ]

shellCompTests =
  testGroup
    "\n ============= Test Fish script generation ============"
    [ testCase "basic fish comp" $
        genFishLineOption cmd opt @?= fishExpected,
      testCase "basic zsh comp" $
        getZshOptStr opt @?= zshArgsExpected,
      testCase "zsh script generation" $
        genZshScript cmd opts @?= zshScriptExpected,
      testCase "bash script generation" $
        genBashScript cmd opts @?= bashScriptExpected
    ]
  where
    cmd = "nanachi"
    names = [OptName "-o" ShortType, OptName "--output" LongType]
    arg = "<file>"
    desc = "Specify the filename to save"
    opt = Opt names arg desc
    fishExpected = "complete -c nanachi -s o -l output -d 'Specify the filename to save' -r"
    zshArgsExpected = "'(-o --output)'{-o,--output}'[Specify the filename to save]'"

    names2 = [OptName "--help" LongType]
    args2 = ""
    desc2 = "Help here."
    opt2 = Opt names2 args2 desc2
    opts = [opt, opt2]
    zshScriptExpected =
      "#compdef nanachi\n\n\
      \args=(\n\
      \    '(-o --output)'{-o,--output}'[Specify the filename to save]'\n\
      \    '--help[Help here.]'\n\
      \)\n\n\
      \_arguments -s $args\n"
    bashScriptExpected =
      "# autogenerated bash script\n\
      \\n\
      \_func()\n\
      \{\n\
      \    local cur prev words cword\n\
      \    _init_completion -n = || return\n\
      \    opts=\"-o --output --help\"\n\n\
      \    if [[ ${cur} == -* ]] || (( COMP_CWORD == 1 )); then\n\
      \        COMPREPLY=( $(compgen -W \"${opts}\" -- \"${cur}\") )\n\
      \        return 0\n\
      \    fi\n\n\
      \    COMPREPLY=( $(compgen -W \"${opts}\" -- \"${cur}\") )\n\
      \    return 0\n\
      \}\n\n\
      \## -o bashdefault and -o default are fallback\n\
      \complete -F _func -o bashdefault -o default nanachi\n"

shellCompGoldenTests :: TestTree
shellCompGoldenTests =
  testGroup
    "Golden Tests of shell completions"
    [ goldenVsString
        "minimap2 fish"
        "golden/minimap2.fish"
        actionFish,
      goldenVsString
        "minimap2 zsh"
        "golden/minimap2.zsh"
        actionZsh,
      goldenVsString
        "minimap2 bash"
        "golden/minimap2.sh"
        actionBash
    ]
  where
    mantext = "golden/minimap2-man.txt"
    actionFish = BLU.fromString . genFishScript "minimap2" . parseMany <$> readFile mantext
    actionZsh = BLU.fromString . genZshScript "minimap2" . parseMany <$> readFile mantext
    actionBash = BLU.fromString . genBashScript "minimap2" . parseMany <$> readFile mantext

propertyTests :: TestTree
propertyTests =
  testGroup
    "Hedgehog tests"
    [ testProperty "optName (long)" prop_longOpt,
      testProperty "optName (short)" prop_shortOpt,
      testProperty "optName (old)" prop_oldOpt,
      testProperty "merge ranges" prop_mergeRanges
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

prop_mergeRanges :: Property
prop_mergeRanges =
  property $ do
    let num = Gen.int (Range.constant 0 200)
    xs <- forAll $ Gen.list (Range.constant 0 300) num
    ys <- forAll $ Gen.list (Range.constant 0 300) num
    let (xRanges, yRanges) = makeRanges (nubSort xs) (nubSort ys)
    mergeRanges xRanges yRanges === mergeRangesFast xRanges yRanges

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
    actual = parseMany s
    expected = List.sort [makeOpt names args desc]

test_parserMult :: String -> [([String], String, String)] -> TestTree
test_parserMult s tuples =
  testCase s $ do
    List.sort actual @?= expected
  where
    actual = parseMany s
    expected = List.sort [makeOpt names args desc | (names, args, desc) <- tuples]

test_parseMany :: String -> [([String], String, String)] -> TestTree
test_parseMany s tuples =
  testCase s $ do
    List.sort actual @?= expected
  where
    actual = parseMany s
    expected = List.sort [makeOpt names args desc | (names, args, desc) <- tuples]

layoutTests :: TestTree
layoutTests =
  testGroup
    "Test layouts"
    [ testCase "tab width" $
        getWidth "   \t          \t\t \t d" @?= 42,
      testCase "tab width 2" $
        getWidth "\t" @?= 8,
      testCase "option location 1" $
        getOptionLocations " \n\n  \t  --option here" @?= [(2, 10)]
    ]
