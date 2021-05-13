# Completions for samtools

# subcommands
complete -c samtools -n __fish_use_subcommand -a dict -d "create a sequence dictionary file"
complete -c samtools -n __fish_use_subcommand -a faidx -d "index/extract FASTA"
complete -c samtools -n __fish_use_subcommand -a fqidx -d "index/extract FASTQ"
complete -c samtools -n __fish_use_subcommand -a index -d "index alignment"
complete -c samtools -n __fish_use_subcommand -a calmd -d "recalculate MD/NM tags and '=' bases"
complete -c samtools -n __fish_use_subcommand -a fixmate -d "fix mate information"
complete -c samtools -n __fish_use_subcommand -a reheader -d "replace BAM header"
complete -c samtools -n __fish_use_subcommand -a targetcut -d "cut fosmid regions (for fosmid pool only)"
complete -c samtools -n __fish_use_subcommand -a addreplacerg -d "adds or replaces RG tags"
complete -c samtools -n __fish_use_subcommand -a markdup -d "mark duplicates"
complete -c samtools -n __fish_use_subcommand -a ampliconclip -d "clip oligos from the end of reads"
complete -c samtools -n __fish_use_subcommand -a collate -d "shuffle and group alignments by name"
complete -c samtools -n __fish_use_subcommand -a cat -d "concatenate BAMs"
complete -c samtools -n __fish_use_subcommand -a merge -d "merge sorted alignments"
complete -c samtools -n __fish_use_subcommand -a mpileup -d "multi-way pileup"
complete -c samtools -n __fish_use_subcommand -a sort -d "sort alignment file"
complete -c samtools -n __fish_use_subcommand -a split -d "splits a file by read group"
complete -c samtools -n __fish_use_subcommand -a quickcheck -d "quickly check if SAM/BAM/CRAM file appears intact"
complete -c samtools -n __fish_use_subcommand -a fastq -d "converts a BAM to a FASTQ"
complete -c samtools -n __fish_use_subcommand -a fasta -d "converts a BAM to a FASTA"
complete -c samtools -n __fish_use_subcommand -a bedcov -d "read depth per BED region"
complete -c samtools -n __fish_use_subcommand -a coverage -d "alignment depth and percent coverage"
complete -c samtools -n __fish_use_subcommand -a depth -d "compute the depth"
complete -c samtools -n __fish_use_subcommand -a flagstat -d "simple stats"
complete -c samtools -n __fish_use_subcommand -a idxstats -d "BAM index stats"
complete -c samtools -n __fish_use_subcommand -a phase -d "phase heterozygotes"
complete -c samtools -n __fish_use_subcommand -a stats -d "generate stats (former bamcheck)"
complete -c samtools -n __fish_use_subcommand -a ampliconstats -d "generate amplicon specific stats"
complete -c samtools -n __fish_use_subcommand -a flags -d "explain BAM flags"
complete -c samtools -n __fish_use_subcommand -a tview -d "text alignment viewer"
complete -c samtools -n __fish_use_subcommand -a view -d "SAM<->BAM<->CRAM conversion"
complete -c samtools -n __fish_use_subcommand -a depad -d "convert padded BAM to unpadded BAM"

# dict
complete -c samtools -n "__fish_seen_subcommand_from dict" -s a -l assembly -d 'assembly' -x
complete -c samtools -n "__fish_seen_subcommand_from dict" -s A -l alias -l alternative-name -d 'add AN tag by adding/removing \'chr\''
complete -c samtools -n "__fish_seen_subcommand_from dict" -s H -l no-header -d 'do not print @HD line'
complete -c samtools -n "__fish_seen_subcommand_from dict" -s o -l output -d 'file to write out dict file [stdout]' -r
complete -c samtools -n "__fish_seen_subcommand_from dict" -s s -l species -d 'species' -x
complete -c samtools -n "__fish_seen_subcommand_from dict" -s u -l uri -d 'URI [file:///abs/path/to/file.fa]' -x

# faidx
complete -c samtools -n "__fish_seen_subcommand_from faidx" -s o -l output -d 'Write FASTA to file.' -r
complete -c samtools -n "__fish_seen_subcommand_from faidx" -s n -l length -d 'Length of FASTA sequence line.' -x
complete -c samtools -n "__fish_seen_subcommand_from faidx" -s c -l continue -d 'Continue after trying to retrieve missing region.'
complete -c samtools -n "__fish_seen_subcommand_from faidx" -s r -l region-file -d 'File of regions.' -r
complete -c samtools -n "__fish_seen_subcommand_from faidx" -s i -l reverse-complement -d ' Reverse complement sequences.' -r
complete -c samtools -n "__fish_seen_subcommand_from faidx" -l fai-idx -d 'name of the index file (default file.fa.fai).' -r
complete -c samtools -n "__fish_seen_subcommand_from faidx" -l gzi-idx -d 'name of compressed file index (default file.fa.gz.gzi).' -r
complete -c samtools -n "__fish_seen_subcommand_from faidx" -s f -l fastq -d 'File and index in FASTQ format.'
complete -c samtools -n "__fish_seen_subcommand_from faidx" -s h -l help -d 'This message.'

# fqidx
