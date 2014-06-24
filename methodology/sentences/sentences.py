import os
import csv
from collections import Counter

PRETEST_DIR = r'C:\Users\michael\Dropbox\Michael_Dissertation\Pretest'

senpred = []

with open(os.path.join(PRETEST_DIR,'senpred_results.txt'),'r') as f:
    reader = csv.DictReader(f,delimiter='\t')
    for line in reader:
        senpred.append(line)

bag_of_words = {}
inout = {}

for l in senpred:
    if (l['Word'],l['Predictive']) not in bag_of_words:
        bag_of_words[(l['Word'],l['Predictive'])] = []
        inout[(l['Word'],l['Predictive'])] = []
    
    initial_bag = [x for x in l['RESP'].split(',') if x != '']
    final_bag = []
    for w in initial_bag:
        if ' ' in w:
            final_bag += w.split(' ')
        else:
            final_bag.append(w)
    
    bag_of_words[(l['Word'],l['Predictive'])] += final_bag
    if l['Word'] in final_bag:
        inout[(l['Word'],l['Predictive'])] += [1]
    else:
        inout[(l['Word'],l['Predictive'])] += [0]
    
output = {}
for k,v in bag_of_words.items():
    
    nums = Counter(v)
    desired = 0
    if k[0] in nums:
        desired = nums[k[0]]
    present = sum(inout[k])/len(inout[k])
    output[k] = (desired / sum([x for x in nums.values()]),present)
    
with open(os.path.join(PRETEST_DIR,'senpred_parsed.txt'),'w') as f:
    writer = csv.writer(f,delimiter='\t')
    writer.writerow(['Word','Predictive','RespProp','PresentProp'])
    for k,v in output.items():
        writer.writerow([k[0],k[1],v[0],v[1]])
print(output)
#print(sentence_dict)
