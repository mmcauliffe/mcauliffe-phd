import os
import csv
from collections import Counter

PRETEST_DIR = r''

pictname = []

with open(os.path.join('picturenaming_results.txt'),'r') as f:
    reader = csv.DictReader(f,delimiter='\t')
    for line in reader:
        pictname.append(line)

bag_of_words = {}
responses = {}
inout = {}

for l in pictname:
    if l['Word'] not in bag_of_words:
        bag_of_words[l['Word']] = []
        inout[l['Word']] = []
        responses[l['Word']] = []

    initial_bag = [x for x in l['RESP'].split(',') if x != '']
    final_bag = []
    for w in initial_bag:
        if ' ' in w:
            final_bag += w.split(' ')
        else:
            final_bag.append(w)

    bag_of_words[l['Word']] += final_bag
    if l['Word'] in final_bag:
        inout[l['Word']] += [1]
    else:
        inout[l['Word']] += [0]

output = {}
for k,v in bag_of_words.items():
    responses[k] = Counter(responses[k])
    nums = Counter(v)
    desired = 0
    if k in nums:
        desired = nums[k]
    present = sum(inout[k])/len(inout[k])
    output[k] = (desired / sum([x for x in nums.values()]),present)

with open(os.path.join('picturenaming_parsed.txt'),'w') as f:
    writer = csv.writer(f,delimiter='\t')
    writer.writerow(['Word','RespProp','PresentProp'])
    for k,v in output.items():
        writer.writerow([k,v[0],v[1]])

#print(output)
#print(sentence_dict)
#for k,v in responses.items():
#    if output[k][1] < 0.5 and k[1] == 'yes':
#        print(k,v)
#        print('\n')
