complete -c minimap2 -s k -d 'Minimizer k-mer length [15]' -x
complete -c minimap2 -s w -d 'Minimizer window size [2/3 of k-mer length]. A minimizer is the smallest k-mer in a window of w con‐' -x
complete -c minimap2 -s H -d 'Use homopolymer-compressed (HPC) minimizers. An HPC sequence is constructed by contracting homopoly‐'
complete -c minimap2 -s I -d 'Load at most NUM target bases into RAM for indexing [4G]. If there are more than NUM bases  in  tar‐' -x
complete -c minimap2 -l idx-no-seq -d 'Don\'t	store  target  sequences in the index. It saves disk space and memory but the index generated'
complete -c minimap2 -s d -d 'Save the minimizer index of target.fa to FILE [no dump]. Minimap2 indexing is fast. It can index the' -r
complete -c minimap2 -l alt -d 'List of ALT contigs [null]' -r
complete -c minimap2 -l alt-drop -d 'Drop ALT hits by FLOAT fraction when ranking and computing mapping quality [0.15]' -x
complete -c minimap2 -s f -d 'If fraction, ignore top FLOAT fraction of most frequent minimizers [0.0002]. If integer, ignore min‐' -x
complete -c minimap2 -s f -d 'If fraction, ignore top FLOAT fraction of most frequent minimizers [0.0002]. If integer, ignore min‐' -x
complete -c minimap2 -l min-occ-floor -d 'Force	minimap2  to always use k-mers occurring INT times or less [0]. In effect, the max occurrence' -x
complete -c minimap2 -s g -d 'Stop chain enlongation if there are no minimizers within INT-bp [10000].' -x
complete -c minimap2 -s r -d 'Bandwidth used in chaining and DP-based alignment [500]. This option approximately controls the max‐' -x
complete -c minimap2 -s n -d 'Discard chains consisting of <INT number of minimizers [3]' -x
complete -c minimap2 -s m -d 'Discard chains with chaining score <INT [40]. Chaining score equals the approximate number of match‐' -x
complete -c minimap2 -s D -d 'If query sequence name/length are identical to the target name/length, ignore diagonal anchors. This'
complete -c minimap2 -s P -d 'Retain  all  chains  and  don\'t attempt to set primary chains. Options -p and -N have no effect when'
complete -c minimap2 -l dual -d 'If no, skip query-target pairs wherein the query name is lexicographically greater than  the  target' -x
complete -c minimap2 -s X -d 'Equivalent to \'-DP --dual=no --no-long-join\'.	Primarily used for all-vs-all read overlapping.'
complete -c minimap2 -s p -d 'Minimal  secondary-to-primary	score  ratio  to output secondary mappings [0.8].  Between two chains' -x
complete -c minimap2 -s N -d 'Output at most INT secondary alignments [5]. This option has no effect when -X is applied.' -x
complete -c minimap2 -s G -d 'Maximum  gap  on  the	reference  (effective  with -xsplice/--splice).  This option also changes the' -x
complete -c minimap2 -s F -d 'Maximum fragment length (aka insert size; effective with -xsr/--frag=yes) [800]' -x
complete -c minimap2 -s M -d 'Mark  as  secondary  a chain that overlaps with a better chain by FLOAT or more of the shorter chain' -x
complete -c minimap2 -l hard-mask-level -d 'Honor option -M and disable a heurstic to save unmapped subsequences and disables --mask-len.'
complete -c minimap2 -l mask-len -d 'Keep an alignment if dropping it leaves an unaligned region on query longer than INT  [inf].  Effec‐' -x
complete -c minimap2 -l max-chain-skip -d 'A  heuristics	that  stops  chaining early [25]. Minimap2 uses dynamic programming for chaining. The' -x
complete -c minimap2 -l max-chain-iter -d 'Check up to INT partial chains during chaining [5000]. This is a heuristic to avoid  quadratic  time' -x
complete -c minimap2 -l chain-gap-scale -d 'Scale of gap cost during chaining [1.0]' -x
complete -c minimap2 -l no-long-join -d 'Disable  the  long gap patching heuristic. When this option is applied, the maximum alignment gap is'
complete -c minimap2 -l lj-min-ratio -d 'Fraction of query sequence length required to bridge a long gap [0.5]. A smaller value helps to  re‐' -x
complete -c minimap2 -l splice -d 'Enable the splice alignment mode.'
complete -c minimap2 -l sr -d 'Enable  short-read  alignment heuristics. In the short-read mode, minimap2 applies a second round of'
complete -c minimap2 -l split-prefix -d 'Prefix to create temporary files. Typically used for a multi-part index.' -x
complete -c minimap2 -l frag -d 'Whether to enable the fragment mode [no]' -x
complete -c minimap2 -l for-only -d 'Only  map  to the forward strand of the reference sequences. For paired-end reads in the forward-re‐'
complete -c minimap2 -l rev-only -d 'Only map to the reverse complement strand of the reference sequences.'
complete -c minimap2 -l heap-sort -d 'If  yes,  sort anchors with heap merge, instead of radix sort. Heap merge is faster for short reads,' -x
complete -c minimap2 -l no-pairing -d 'Treat two reads in a pair as independent reads. The mate related fields in SAM  are  still  properly'
complete -c minimap2 -s A -d 'Matching score [2]' -x
complete -c minimap2 -s B -d 'Mismatching penalty [4]' -x
complete -c minimap2 -s O -d 'Gap open penalty [4,24]. If INT2 is not specified, it is set to INT1.' -x
complete -c minimap2 -s O -d 'Gap open penalty [4,24]. If INT2 is not specified, it is set to INT1.' -x
complete -c minimap2 -s E -d 'Gap  extension penalty [2,1]. A gap of length k costs min{O1+k*E1,O2+k*E2}.  In the splice mode, the' -x
complete -c minimap2 -s E -d 'Gap  extension penalty [2,1]. A gap of length k costs min{O1+k*E1,O2+k*E2}.  In the splice mode, the' -x
complete -c minimap2 -s C -d 'Cost for a non-canonical GT-AG splicing (effective with --splice) [0]' -x
complete -c minimap2 -s z -d 'Truncate an alignment if the running alignment score drops too quickly along the diagonal of the  DP' -x
complete -c minimap2 -s z -d 'Truncate an alignment if the running alignment score drops too quickly along the diagonal of the  DP' -x
complete -c minimap2 -s s -d 'Minimal  peak DP alignment score to output [40]. The peak score is computed from the final CIGAR. It' -x
complete -c minimap2 -s u -d 'How to find canonical splicing sites GT-AG - f: transcript strand; b: both strands; n: no attempt to' -x
complete -c minimap2 -l end-bonus -d 'Score bonus when alignment extends to the end of the query sequence [0].' -x
complete -c minimap2 -l score-N -d 'Score of a mismatch involving ambiguous bases [1].' -x
complete -c minimap2 -l splice-flank -d 'Assume the next base to a GT donor site tends to be A/G (91% in human and 92% in mouse) and the pre‐' -x
complete -c minimap2 -l junc-bed -d 'Gene  annotations in the BED12 format (aka 12-column BED), or intron positions in 5-column BED. With' -r
complete -c minimap2 -l junc-bonus -d 'Score bonus for a splice donor or acceptor found in annotation (effective with --junc-bed) [0].' -x
complete -c minimap2 -l end-seed-pen -d 'Drop a terminal anchor if s<log(g)+INT, where s is the local alignment score around the anchor and g' -x
complete -c minimap2 -l no-end-flt -d 'Don\'t filter seeds towards the ends of chains before performing base-level alignment.'
complete -c minimap2 -l cap-sw-mem -d 'Skip alignment if the DP matrix size is above NUM.  Set 0 to disable [0].' -x
complete -c minimap2 -s a -d 'Generate CIGAR and output alignments in the SAM format. Minimap2 outputs in PAF by default.'
complete -c minimap2 -s o -d 'Output alignments to FILE [stdout].' -r
complete -c minimap2 -s Q -d 'Ignore base quality in the input file.'
complete -c minimap2 -s L -d 'Write	CIGAR  with >65535 operators at the CG tag. Older tools are unable to convert alignments with'
complete -c minimap2 -s R -d 'SAM read group line in a format like @RG\tID:foo\tSM:bar [].' -x
complete -c minimap2 -s y -d 'Copy input FASTA/Q comments to output.'
complete -c minimap2 -s c -d 'Generate CIGAR. In PAF, the CIGAR is written to the `cg\' custom tag.'
complete -c minimap2 -l cs -d 'Output the cs tag.  STR can be either short or long.  If no STR is given, short is assumed. [none]'
complete -c minimap2 -l cs -d 'Output the cs tag.  STR can be either short or long.  If no STR is given, short is assumed. [none]' -x
complete -c minimap2 -l MD -d 'Output the MD tag (see the SAM spec).'
complete -c minimap2 -l eqx -d 'Output =/X CIGAR operators for sequence match/mismatch.'
complete -c minimap2 -s Y -d 'In SAM output, use soft clipping for supplementary alignments.'
complete -c minimap2 -l seed -d 'Integer  seed for randomizing equally best hits. Minimap2 hashes INT and read name when choosing be‐' -x
complete -c minimap2 -s t -d 'Number of threads [3]. Minimap2 uses at most three threads when indexing target sequences, and  uses' -x
complete -c minimap2 -s 2 -d 'Use two I/O threads during mapping. By default, minimap2 uses one I/O	thread.   When	I/O  is  slow'
complete -c minimap2 -s K -d 'Number  of  bases  loaded  into  memory  to  process  in a mini-batch [500M].	Similar to option -I,' -x
complete -c minimap2 -l secondary -d 'Whether to output secondary alignments [yes]' -x
complete -c minimap2 -l max-qlen -d 'Filter out query sequences longer than NUM.' -x
complete -c minimap2 -l paf-no-hit -d 'In  PAF,  output unmapped queries; the strand and the reference name fields are set to `*\'. Warning:'
complete -c minimap2 -l sam-hit-only -d 'In SAM, don\'t output unmapped reads.'
complete -c minimap2 -s x -d 'Preset []. This option applies multiple options at the same time. It should be applied before	other' -x
complete -c minimap2 -o ub -d '--junc-bonus=9 --splice-flank=yes).  In the splice mode, 1) long deletions are taken as'
complete -c minimap2 -l no-kalloc -d 'Use  the libc default allocator instead of the kalloc thread-local allocator.	This debugging option'
complete -c minimap2 -l print-qname -d 'Print query names to stderr, mostly to see which query is crashing minimap2.'
complete -c minimap2 -l print-seeds -d 'Print seed positions to stderr, for debugging only.'
