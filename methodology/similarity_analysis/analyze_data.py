import csv
import os
from functools import partial

from acousticsim.main import acoustic_similarity_directories

from acousticsim.helper import get_vowel_points
from acousticsim.praat.wrapper import (to_pitch_praat, to_formants_praat,
                                        to_intensity_praat, to_mfcc_praat)

from acousticsim.distance.point import point_distance, euclidean
from acousticsim.distance.dct import dct_distance
from acousticsim.distance.dtw import dtw_distance
from acousticsim.distance.xcorr import xcorr_distance

from acousticsim.representations import Envelopes


praat_path = r'C:\Users\michael\Documents\Praat\praatcon.exe'

data_dir = r'C:\Users\michael\Documents\Data\Dissertation'

experiments = ['Exp1', 'Exp2','Exp3']

possible_folders = ['S-Final','S-Initial','SH-Final','SH-Initial',
                    'S-FinalP','S-FinalU']

categorization_dir = os.path.join(data_dir,'categorization','sibilants')

## Representations
# MFCC (acousticsim)
# MFCC (Praat)
# Formants (Praat)
# Intensity (Praat)
# Pitch (Praat)
# AmpEnvs (acousticsim)

## Distance functions
# DTW
# XCorr
# DCT
# Vowel midpoint
# Vowel third

def callback(*value):
    print(*value)

praat_mfcc = partial(to_mfcc_praat, praat_path )

praat_formants = partial(to_formants_praat, praat_path)

praat_intensity = partial(to_intensity_praat, praat_path )

praat_pitch = partial(to_pitch_praat, praat_path )

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

def save_output(output):
    with open('output.txt', 'w') as f:
        writer = csv.writer(f, delimiter = '\t')
        writer.writerow(['Experiment', 'WordType', 'Unit', 'AverageAsim'])
        for line in output:
            writer.writerow(line)


def main():
    output = list()
    for e in experiments:
        for p in possible_folders:
            folder = os.path.join(data_dir, e, p)
            if not os.path.exists(folder):
                continue

            overall = acoustic_similarity_directories(folder,
                                categorization_dir,
                                rep = 'mfcc',
                                use_multi = True,
                                num_cores = 4,
                                match_function = word_distance)
            output.append([e,p,'word',overall])

            overall = acoustic_similarity_directories(folder, categorization_dir,
                                rep = 'mfcc',
                                use_multi = True,
                                num_cores = 4,
                                match_function = sibilant_distance)
            output.append([e,p,'sibilant',overall])
    save_output(output)


if __name__ == '__main__':
    main()
