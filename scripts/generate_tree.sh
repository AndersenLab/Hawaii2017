#!/usr/bin/bash

muscle -in sanger_30.fasta  -out sanger_30.msa
muscle -maketree -in sanger_30.msa -out sanger_30.phylo -cluster neighborjoining