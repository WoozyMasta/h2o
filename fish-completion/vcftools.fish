complete -c vcftools -l vcf -d 'This option defines the VCF file to be processed.' -r
complete -c vcftools -l gzvcf -d 'This option can be used in place of the --vcf option to read compressed (gzipped) VCF files directly.' -r
complete -c vcftools -l bcf -d 'This option can be used in place of the --vcf option to read BCF2 files directly.' -r
complete -c vcftools -l out -d 'This option defines the output filename prefix for all files generated by vcftools.' -x
complete -c vcftools -l stdout -s c -d 'These options direct the vcftools output to standard out so it can be piped into another program or written directly to a filename of choice.'
complete -c vcftools -l temp -d 'This option can be used to redirect any temporary files that vcftools creates into a specified directory.' -r
complete -c vcftools -l chr -l not-chr -d 'Includes or excludes sites with indentifiers matching <chromosome>.' -x
complete -c vcftools -l from-bp -l to-bp -d 'These options specify a lower bound and upper bound for a range of sites to be processed.' -x
complete -c vcftools -l positions -l exclude-positions -d 'Include or exclude a set of sites on the basis of a list of positions in a file.' -r
complete -c vcftools -l positions-overlap -l exclude-positions-overlap -d 'Include or exclude a set of sites on the basis of the reference allele overlapping with a list of positions in a file.' -r
complete -c vcftools -l bed -l exclude-bed -d 'Include or exclude a set of sites on the basis of a BED file.' -r
complete -c vcftools -l thin -d 'Thin sites so that no two sites are within the specified distance from one another.' -x
complete -c vcftools -l mask -l invert-mask -l mask-min -d 'These options are used to specify a FASTA-like mask file to filter with.' -r
complete -c vcftools -l snp -d 'Include SNP(s) with matching ID (e.g. a dbSNP rsID).' -x
complete -c vcftools -l snps -l exclude -d 'Include or exclude a list of SNPs given in a file.' -r
complete -c vcftools -l keep-only-indels -l remove-indels -d 'Include or exclude sites that contain an indel.'
complete -c vcftools -l remove-filtered-all -d 'Removes all sites with a FILTER flag other than PASS.'
complete -c vcftools -l keep-filtered -l remove-filtered -d 'Includes or excludes all sites marked with a specific FILTER flag.' -x
complete -c vcftools -l keep-INFO -l remove-INFO -d 'Includes or excludes all sites with a specific INFO flag.' -x
complete -c vcftools -l maf -l max-maf -d 'Include only sites with a Minor Allele Frequency greater than or equal to the "--maf" value and less than or equal to the "--max-maf"' -x
complete -c vcftools -l non-ref-af -l max-non-ref-af -d 'Include only sites with all Non-Reference (ALT) Allele Frequencies (af) within the range specified, and including the specified value.' -x
complete -c vcftools -l non-ref-ac -l max-non-ref-ac -d 'Include only sites with all Non-Reference (ALT) Count (ac) within the range specified, and including the specified value.' -x
complete -c vcftools -l non-ref-af-any -l max-non-ref-af-any -d 'Include only sites with all Non-Reference (ALT) Allele Frequencies (af) within the range specified; "any" require only one allele to meet the criteria.' -x
complete -c vcftools -l non-ref-ac-any -l max-non-ref-ac-any -d 'Include only sites with all Non-Reference (ALT) Counts (ac) within the range specified; "any" require only one allele to meet the criteria.' -x
complete -c vcftools -l mac -l max-mac -d 'Include  only  sites  with  Minor  Allele  Count greater than or equal to the "--mac" value and less than or equal to the "--max-mac"' -x
complete -c vcftools -l min-alleles -l max-alleles -d 'Include  only sites with a number of alleles greater than or equal to the "--min-alleles" value and less than or equal to the "--max-' -x
complete -c vcftools -l min-meanDP -l max-meanDP -d 'Includes only sites with mean depth values (over all included individuals) greater than or equal to the "--min-meanDP" value and less' -x
complete -c vcftools -l hwe -d 'Assesses sites for Hardy-Weinberg Equilibrium using an exact test, as defined by Wigginton, Cutler and Abecasis (2005).' -x
complete -c vcftools -l max-missing -d 'Exclude sites on the basis of the proportion of missing data (defined to be between 0 and 1, where 0 allows sites that are completely' -x
complete -c vcftools -l max-missing-count -d 'Exclude sites with more than this number of missing genotypes over all individuals.' -x
complete -c vcftools -l phased -d 'Excludes all sites that contain unphased genotypes.'
complete -c vcftools -l minQ -d 'Includes only sites with Quality value above this threshold.' -x
complete -c vcftools -l indv -l remove-indv -d 'Specify an individual to be kept or removed from the analysis.' -x
complete -c vcftools -l keep -l remove -d 'Provide files containing a list of individuals to either include or exclude in subsequent analysis.' -r
complete -c vcftools -l max-indv -d 'Randomly thins individuals so that only the specified number are retained.' -x
complete -c vcftools -l remove-filtered-geno-all -d 'Excludes all genotypes with a FILTER flag not equal to "." (a missing value) or PASS.'
complete -c vcftools -l remove-filtered-geno -d 'Excludes genotypes with a specific FILTER flag.' -x
complete -c vcftools -l minGQ -d 'Exclude all genotypes with a quality below the threshold specified.' -x
complete -c vcftools -l minDP -l maxDP -d 'Includes only genotypes greater than or equal to the "--minDP" value and less than or equal to the "--maxDP" value.' -x
complete -c vcftools -l freq -l freq2 -d 'Outputs the allele frequency for each site in a file with the suffix ".frq".'
complete -c vcftools -l counts -l counts2 -d 'Outputs the raw allele counts for each site in a file with the suffix ".frq.count".'
complete -c vcftools -l derived -d 'For use with the previous four frequency and count options only.'
complete -c vcftools -l depth -d 'Generates a file containing the mean depth per individual.'
complete -c vcftools -l site-depth -d 'Generates a file containing the depth per site summed across all individuals.'
complete -c vcftools -l site-mean-depth -d 'Generates a file containing the mean depth per site averaged across all individuals.'
complete -c vcftools -l geno-depth -d 'Generates a (possibly very large) file containing the depth for each genotype in the VCF file.'
complete -c vcftools -l hap-r2 -d 'Outputs a file reporting the r2, D, and D\' statistics using phased haplotypes.'
complete -c vcftools -l geno-r2 -d 'Calculates the squared correlation coefficient between genotypes encoded as 0, 1 and 2 to represent the number of non-reference alle‐'
complete -c vcftools -l geno-chisq -d 'If  your  data contains sites with more than two alleles, then this option can be used to test for genotype independence via the chi-'
complete -c vcftools -l hap-r2-positions -l geno-r2-positions -d 'Outputs a file reporting the r2 statistics of the sites contained in the provided file verses all other sites.' -r
complete -c vcftools -l ld-window -d 'This  optional  parameter defines the maximum number of SNPs between the SNPs being tested for LD in the "--hap-r2", "--geno-r2", and' -x
complete -c vcftools -l ld-window-bp -d 'This optional parameter defines the maximum number of physical bases between the SNPs being tested for LD in the "--hap-r2", "--geno-' -x
complete -c vcftools -l ld-window-min -d 'This  optional  parameter defines the minimum number of SNPs between the SNPs being tested for LD in the "--hap-r2", "--geno-r2", and' -x
complete -c vcftools -l ld-window-bp-min -d 'This optional parameter defines the minimum number of physical bases between the SNPs being tested for LD in the "--hap-r2", "--geno-' -x
complete -c vcftools -l min-r2 -d 'This optional parameter sets a minimum value for r2, below which the LD statistic is not reported by the "--hap-r2", "--geno-r2", and' -x
complete -c vcftools -l interchrom-hap-r2 -l interchrom-geno-r2 -d 'Outputs a file reporting the r2 statistics for sites on different chromosomes.'
complete -c vcftools -l TsTv -d 'Calculates the Transition / Transversion ratio in bins of size defined by this option.' -x
complete -c vcftools -l TsTv-summary -d 'Calculates a simple summary of all Transitions and Transversions.'
complete -c vcftools -l TsTv-by-count -d 'Calculates the Transition / Transversion ratio as a function of alternative allele count.'
complete -c vcftools -l TsTv-by-qual -d 'Calculates the Transition / Transversion ratio as a function of SNP quality threshold.'
complete -c vcftools -l FILTER-summary -d 'Generates a summary of the number of SNPs and Ts/Tv ratio for each FILTER category.'
complete -c vcftools -l site-pi -d 'Measures nucleotide divergency on a per-site basis.'
complete -c vcftools -l window-pi -l window-pi-step -d 'Measures the nucleotide diversity in windows, with the number provided as the window size.' -x
complete -c vcftools -l weir-fst-pop -d 'This option is used to calculate an Fst estimate from Weir and Cockerham\'s 1984 paper.' -r
complete -c vcftools -l fst-window-size -l fst-window-step -d 'These options can be used with "--weir-fst-pop" to do the Fst calculations on a windowed basis instead of a per-site basis.' -x
complete -c vcftools -l het -d 'Calculates a measure of heterozygosity on a per-individual basis.'
complete -c vcftools -l hardy -d 'Reports a p-value for each site from a Hardy-Weinberg Equilibrium test (as defined by Wigginton, Cutler and Abecasis (2005)).'
complete -c vcftools -l TajimaD -d 'Outputs Tajima\'s D statistic in bins with size of the specified number.' -x
complete -c vcftools -l indv-freq-burden -d 'This option calculates the number of variants within each individual of a specific frequency.'
complete -c vcftools -l LROH -d 'This option will identify and output Long Runs of Homozygosity.'
complete -c vcftools -l relatedness -d 'This option is used to calculate and output a relatedness statistic based  on  the  method  of  Yang  et  al,  Nature  Genetics  2010'
complete -c vcftools -l relatedness2 -d 'This option is used to calculate and output a relatedness statistic based on the method of Manichaikul et  al.,  BIOINFORMATICS  2010'
complete -c vcftools -l site-quality -d 'Generates a file containing the per-site SNP quality, as found in the QUAL column of the VCF file.'
complete -c vcftools -l missing-indv -d 'Generates a file reporting the missingness on a per-individual basis.'
complete -c vcftools -l missing-site -d 'Generates a file reporting the missingness on a per-site basis.'
complete -c vcftools -l SNPdensity -d 'Calculates the number and density of SNPs in bins of size defined by this option.' -x
complete -c vcftools -l kept-sites -d 'Creates a file listing all sites that have been kept after filtering.'
complete -c vcftools -l removed-sites -d 'Creates a file listing all sites that have been removed after filtering.'
complete -c vcftools -l singletons -d 'This option will generate a file detailing the location of singletons, and the individual they occur in.'
complete -c vcftools -l hist-indel-len -d 'This option will generate a histogram file of the length of all indels (including SNPs).'
complete -c vcftools -l hapcount -d 'This option will output the number of unique haplotypes within user specified bins, as defined by the BED file.' -r
complete -c vcftools -l mendel -d 'This option is use to report mendel errors identified in trios.' -r
complete -c vcftools -l extract-FORMAT-info -d 'Extract information from the genotype fields in the VCF file relating to a specfied FORMAT identifier.' -x
complete -c vcftools -l get-INFO -d 'This option is used to extract information from the INFO field in the VCF file.' -x
complete -c vcftools -l recode -l recode-bcf -d 'These  options  are	used to generate a new file in either VCF or BCF from the input VCF or BCF file after applying the filtering op‐'
complete -c vcftools -l recode-INFO -l recode-INFO-all -d 'These options can be used with the above recode options to define an INFO key name to keep in the output file.' -x
complete -c vcftools -l contigs -d 'This option can be used in conjuction with the --recode-bcf when the input file does not have any contig declarations.' -x
complete -c vcftools -l 012 -d 'This option outputs the genotypes as a large matrix.'
complete -c vcftools -l IMPUTE -d 'This option outputs phased haplotypes in IMPUTE reference-panel format.'
complete -c vcftools -l ldhat -l ldhelmet -l ldhat-geno -d 'These options output data in LDhat/LDhelmet format.'
complete -c vcftools -l BEAGLE-GL -l BEAGLE-PL -d 'These options output genotype likelihood information for input into the BEAGLE program.'
complete -c vcftools -l plink -l plink-tped -l chrom-map -d 'These options output the genotype data in PLINK PED format.'
complete -c vcftools -l diff -l gzdiff -l diff-bcf -d 'These options compare the original input file to this specified VCF, gzipped VCF, or BCF file.' -r
complete -c vcftools -l diff-site -d 'Outputs the sites that are common / unique to each file.'
complete -c vcftools -l diff-indv -d 'Outputs the individuals that are common / unique to each file.'
complete -c vcftools -l diff-site-discordance -d 'This option calculates discordance on a site by site basis.'
complete -c vcftools -l diff-indv-discordance -d 'This option calculates discordance on a per-individual basis.'
complete -c vcftools -l diff-indv-map -d 'This option allows the user to specify a mapping of individual IDs in the second file to those in the first file.' -r
complete -c vcftools -l diff-discordance-matrix -d 'This option calculates a discordance matrix.'
complete -c vcftools -l diff-switch-error -d 'This option calculates phasing errors (specifically "switch errors").'
