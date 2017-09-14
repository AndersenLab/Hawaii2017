import json
import gzip
from subprocess import Popen, PIPE
from os.path import dirname
from collections import OrderedDict


def get_git_dir():
    path = Popen(['git',
                  'rev-parse',
                  '--git-dir'], stdout=PIPE).communicate()[0]
    return str(dirname(path), encoding='UTF-8')


wd = get_git_dir()
blast_json = wd + '/data/sanger/blast_results.json.gz'
blast_results = json.load(gzip.open(blast_json))['BlastOutput2']

header_out = False

with open(wd + '/data/sanger/blast_results.tsv', 'w') as f:
    for record in blast_results:
        output = OrderedDict()
        record = record['report']['results']['search']
        output.update({'seq': record['query_title']})
        if 'control' in record['query_title']:
            record['query_title'] = record['query_title'].replace("____control_","control-")
        elif record['query_title'].count("_") == 5:
            s_plate, primer, well, plate, sanger_id, rec = record['query_title'].split("_")
        else:
            s_plate, primer, well, sanger_id = record['query_title'].split("_")
            plate = "NA"
            rec = "NA"
        output.update({'s_plate': s_plate,
                    'primer': primer,
                    'well': well,
                    'plate': plate,
                    'sanger_id': sanger_id,
                    'rec': rec})
        for hit in record['hits'][:3]:
            output.update(hit['description'][0])
            alignment = hit['hsps'][0]
            output.update(alignment)

            if header_out is False:
                f.write('\t'.join(output.keys()) + "\n")
                header_out = True
            f.write('\t'.join(map(str, output.values())) + "\n")