#!/usr/bin/bash

# Output available S-plates
cat data/sanger/blast_results2.tsv | cut -f 1 | sort | uniq | egrep -v 's_plate|NA' | awk 'NR == 1 { print "CONTROL" } { print }' > data/sanger/splates.tsv

# Run them!
for s in `cat data/sanger/splates.tsv`; do
    Rscript -e "rmarkdown::render('02-blast.Rmd', output_file = '${s}.html', output_dir ='data/sanger/reports/')" ${s}
done;