import os
import random
import csv
import itertools

base_dir = os.path.dirname(os.path.abspath(__file__))

def load_file(filename):

    output = []

    with open(os.path.join(base_dir,filename),'r') as f:
        for line in f.readlines():
            output.append(line.strip().split('\t'))
    return output

def assign_cresp(l):
    n = len(l)
    half_n = n / 2
    random.shuffle(l)
    for i in range(n):
        w = l[i][1].strip()
        d = l[i][2].strip()
        if i < half_n:
            pict1 = w + ".bmp"
            pict2 = d + ".bmp"
            cresp = "1"
        else:
            pict1 = d + ".bmp"
            pict2 = w + ".bmp"
            cresp = "5"
        l[i] = l[i] + [pict1, pict2, cresp]
    return l

#Reinisch Weber Mitterer 2012
#No critical for the first 6 trials
#No critical trials in a row

NUMBER_OF_LISTS = 25

for k,v in enumerate(itertools.product(['attend','noattend'],['pred','unpred'])):
    t = v[1]
    a = v[0]
    for i in range(1,NUMBER_OF_LISTS+1):
        if i <= 6:
            version = 1
        elif i <= 12:
            version = 2
        elif i <= 18:
            version = 3
        elif i <= 24:
            version = 4
        else:
            version = k + 1
        print(k,i)
        print(version)
        good = False
        while not good:
            loop_count = 0
            pred_fillers = assign_cresp(load_file('sentence_filler_pred.txt'))
            unpred_fillers = assign_cresp(load_file('sentence_filler_unpred.txt'))
            pred_sh_fillers = assign_cresp(load_file('sentence-sh-final_pred.txt'))
            unpred_sh_fillers = assign_cresp(load_file('sentence-sh-final_unpred.txt'))
            targets = assign_cresp(load_file('sentence_s-final_%s.txt' % t))
            all_list = pred_fillers + unpred_fillers + pred_sh_fillers + unpred_sh_fillers + targets
            random.shuffle(all_list)
            output_list = []
            while len(output_list) < 6:
                index = random.randint(0,len(all_list)-1)
                if all_list[index][4] == 'Filler':
                    output_list.append(all_list.pop(index))
            prev_target = False
            while len(all_list) > 0:
                index = random.randint(0,len(all_list)-1)
                if all_list[index][4] != 'Filler':
                    if not prev_target:
                        output_list.append(all_list.pop(index))
                        prev_target = True
                        loop_count = 0
                    else:
                        loop_count += 1
                else:
                    output_list.append(all_list.pop(index))
                    prev_target = False
                if loop_count > 100:
                    break
            if loop_count <= 100:
                good = True

        with open(os.path.join(base_dir,'%d%02d_%s_%s.txt' % (k+1,i,t,a)),'w') as f:
            writer = csv.writer(f,delimiter='\t',lineterminator='\n')
            for line in output_list:
                writer.writerow(line)
