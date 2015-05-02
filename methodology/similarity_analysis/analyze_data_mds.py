import csv
import os
from functools import partial

from acousticsim.main import analyze_directory

from acousticsim.helper import get_vowel_points

from acousticsim.distance.dtw import dtw_distance
from acousticsim.clustering import ClusterNetwork

data_dir = r'C:\Users\michael\Documents\Data\Dissertation\Cluster'

def word_distance(rep_one, rep_two):
    base, _ = os.path.splitext(rep_one._filepath)
    one_textgrid = base + '.TextGrid'
    one_begin,one_end = get_vowel_points(one_textgrid,
                tier_name = 'word', vowel_label = 'word')

    base, _ = os.path.splitext(rep_two._filepath)
    two_textgrid = base + '.TextGrid'
    two_begin,two_end = get_vowel_points(two_textgrid,
                    tier_name = 'word', vowel_label = 'word')

    return dtw_distance(rep_one[one_begin, one_end], rep_two[two_begin, two_end])

def sibilant_distance(rep_one, rep_two):
    base, _ = os.path.splitext(rep_one._filepath)
    one_textgrid = base + '.TextGrid'
    one_begin,one_end = get_vowel_points(one_textgrid,
                tier_name = 'sibilant', vowel_label = 'sibilant')

    base, _ = os.path.splitext(rep_two._filepath)
    two_textgrid = base + '.TextGrid'
    two_begin,two_end = get_vowel_points(two_textgrid,
                    tier_name = 'sibilant', vowel_label = 'sibilant')

    return dtw_distance(rep_one[one_begin, one_end], rep_two[two_begin, two_end])

def save_output(cn):
    with open('output_word.txt', 'w') as f:
        writer = csv.writer(f, delimiter = '\t')
        writer.writerow(['X', 'Y', 'Experiment', 'Type', 'Word'])

        for n in cn:
            x, y = n['pos']
            label = n['rep']._true_label
            path = n['rep']._filepath
            name = os.path.basename(path)
            name, ext = os.path.splitext(name)
            if 'Categ' not in label:
                name = name.split('_')[-1]
            exp , t = label.split('_')
            writer.writerow([x, y, exp.lower(), t, name])


def main():
    kwargs = {'rep':'mfcc',
                'match_function':word_distance,
                'use_multi':True,
                'num_cores':4,
                'return_rep': True,
                }
    scores, reps = analyze_directory(data_dir, **kwargs)

    cn = ClusterNetwork(reps)


    cluster_method = 'affinity'
    one_cluster = False

    cn.cluster(scores,cluster_method,one_cluster)
    save_output(cn)

def debug():
    directories = list()
    all_files = list()
    wavs = list()
    for f in os.listdir(data_dir):
        print(f)
        path = os.path.join(data_dir,f)
        all_files.append(path)
        if f.lower().endswith('.wav'):
            wavs.append(path)
        if os.path.isdir(path):
            directories.append(path)
    print(directories)

def find_durations():
    kwargs = {'rep':'mfcc',
                'match_function':word_distance,
                'use_multi':True,
                'num_cores':4,
                'return_rep': True,
                }
    scores, reps = analyze_directory(data_dir, **kwargs)
    out = list()
    with open('output_dur.txt', 'w') as f:
        writer = csv.writer(f, delimiter = '\t')
        writer.writerow(['Label','Duration'])
        for v in reps.values():
            if 'Exp3' not in v._true_label:
                continue
            base, _ = os.path.splitext(v._filepath)
            one_textgrid = base + '.TextGrid'
            one_begin,one_end = get_vowel_points(one_textgrid,
                        tier_name = 'sibilant', vowel_label = 'sibilant')
            dur = one_end - one_begin
            writer.writerow([v._true_label,str(dur)])





if __name__ == '__main__':
    find_durations()
    #debug()
