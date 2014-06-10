import os
import random
import csv

base_dir = os.path.dirname(os.path.abspath(__file__))

def load_file(filename):

    output = []

    with open(os.path.join(base_dir,filename),'r') as f:
        for line in f.readlines():
            output.append(line.strip().split('\t'))
    return output
    


#Reinisch Weber Mitterer 2012
#No critical for the first 6 trials
#No critical trials in a row

NUMBER_OF_LISTS = 10

for t in ['initial_targets','final_targets']:
    for i in range(NUMBER_OF_LISTS):
        good = False
        while not good:
            loop_count = 0
            fillers = load_file('filler.txt')
            sh_fillers = load_file('sh_filler.txt')
            targets = load_file('%s.txt' % t)
            all_list = fillers + sh_fillers + targets
            output_list = []
            while len(output_list) < 6:
                index = random.randint(0,len(all_list)-1)
                if all_list[index][2] == 'Filler':
                    output_list.append(all_list.pop(index))
            prev_target = False
            while len(all_list) > 0:
                index = random.randint(0,len(all_list)-1)
                if all_list[index][2] != 'Filler':
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
                
        with open(os.path.join(base_dir,'subject%02d.txt' % i),'w') as f:
            writer = csv.writer(f,delimiter='\t')
            for line in output_list:
                writer.writerow(line)
