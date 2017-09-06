#!/usr/bin/env python
"""
    Trims Sanger sequences with low quality.
"""
import glob
import os
from Bio import SeqIO
from Bio.Blast import NCBIWWW
from Bio.Blast import NCBIXML
from os.path import basename, dirname, join
from subprocess import Popen, PIPE

def ensure_dir(file_path):
    directory = os.path.dirname(file_path)
    if not os.path.exists(directory):
        os.makedirs(directory)

def get_git_dir():
    path = Popen(['git', 'rev-parse', '--git-dir'], stdout=PIPE).communicate()[0]
    return str(dirname(path), encoding='UTF-8')

def trim_sanger(seq, minimum = 10):
    """
        Trims the start and end of a sanger sequence
        until quality surpasses the minimum.
    """
    qualities = seq.letter_annotations['phred_quality']
    start = [x <= minimum for x in qualities].index(False)
    seq_length = len(qualities)
    end = seq_length - [x <= minimum for x in qualities[::-1]].index(False)
    return seq[start:end], start, (seq_length - end)


wd = get_git_dir()

blast_results = open(wd + '/data/sanger/blast_results3.tsv', 'w+')
blast_results.write("s_plate\tprimer\twell\tsname\thit_id\thit_def\taccession\tpositives\talign_length\tscore\tpositives\tidentities\texpect\tgaps\talignment_fname\tsanger_fname\tstart\tend\n")
blast_results.flush()

with open(wd + '/data/sanger/seqs.fasta', 'w') as fasta_seqs:
    with open(wd + '/data/sanger/sanger_trimming.tsv', 'w') as f:
        f.write('seq\ttrim_left\ttrim_right\tprimer_type\n')
        for s in glob.glob(wd + '/data/sanger/raw/*/*ab1')[11:]:
            primer_type = dirname(s).split("/")[-1]
            # Format an output fastq
            out = s.replace('raw', 'processed/trimmed').replace('ab1', 'fastq')
            ensure_dir(out)

            # Trim sanger sequence
            sanger, trim_left, trim_right = trim_sanger(SeqIO.read(s, 'abi'), 10)
            sname = basename(s).replace('.ab1', '') + "_" + primer_type
            print(sname)
            s_plate, primer, well = sname.split("_")[0:3]
            f.write('{}\t{}\t{}\t{}\n'.format(sname, trim_left, trim_right, primer_type))
            sanger.id = sname

            # Save
            SeqIO.write(sanger, out, 'fastq')
            SeqIO.write(sanger, fasta_seqs, 'fasta')
            result_handle = NCBIWWW.qblast("blastn", "nt", sanger.format('fasta'))
            blast_record = NCBIXML.read(result_handle)

            # Output record as text files and data file.
            for alignment in blast_record.alignments[0:3]:
                print(alignment)
                # Only take one alignment/match
                hsp = alignment.hsps[0]
                print(dir(hsp))
                alignment_fname = sname + "_" + alignment.hit_id.replace("|","").replace(".", "") + ".txt"
                result_line = '\t'.join(list(map(str,[s_plate,
                                                      primer,
                                                      well,
                                                      sname,
                                                      alignment.hit_id,
                                                      alignment.hit_def,
                                                      alignment.accession,
                                                      hsp.positives,
                                                      hsp.align_length,
                                                      hsp.score,
                                                      hsp.positives,
                                                      hsp.identities,
                                                      hsp.expect,
                                                      hsp.gaps,
                                                      alignment_fname,
                                                      s.replace(wd, ''), # Sanger filename
                                                      hsp.query_start,
                                                      hsp.query_end])))

                blast_results.write(result_line + '\n')
                blast_results.flush()

                # output alignment for hsp
                alignment_dir = dirname(dirname(s.replace('raw', 'processed/alignments')))
                ensure_dir(alignment_dir)
                with open(alignment_dir + "/" + alignment_fname, 'w') as alignment_txt:
                    alignment_txt.write(hsp.query + "\n")
                    alignment_txt.write(hsp.match + "\n")
                    alignment_txt.write(hsp.sbjct + "\n")


blast_results.close()

