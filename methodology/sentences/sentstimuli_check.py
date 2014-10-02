import os
import csv
from collections import Counter


experiment_paths = [r'H:\percept\sentences\Lists\101_pred_attend.txt',
                    r'H:\percept\sentences\Lists\201_unpred_attend.txt']

audio_dir = r'H:\percept\sentences\Audio'

wavs = os.listdir(audio_dir)

picture_dir = r'H:\percept\sentences\Pictures'

pics = os.listdir(picture_dir)

for p in experiment_paths:
    with open(p,'r') as f:
        reader = csv.reader(f,delimiter='\t')
        for line in reader:
            wav = line[5]
            pic1 = line[6]
            pic2 = line[7]

            if wav not in wavs:
                print(wav)
            if pic1 not in pics:
                print(pic1)
            if pic2 not in pics:
                print(pic2)

