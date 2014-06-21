import os
import csv
from collections import Counter

PRETEST_DIR = r'C:\Users\michael\Dropbox\Michael_Dissertation\Pretest'

senpred = []

with open(os.path.join(PRETEST_DIR,'senpred_results.txt'),'r') as f:
    reader = csv.DictReader(f,delimiter='\t')
    for line in reader:
        senpred.append(line)

sentence_dict = {}

for l in senpred:
    if (l['Word'],l['Predictive']) not in sentence_dict:
        sentence_dict[(l['Word'],l['Predictive'])] = []
    
    sentence_dict[(l['Word'],l['Predictive'])] += [x for x in l['RESP'].split(',') if x != '']
output = {}
for k,v in sentence_dict.items():
    nums = Counter(v)
    sentence_dict[k] = Counter(v)
    desired = 0
    if k[0] in nums:
        desired = nums[k[0]]
    output[k] = desired / sum([x for x in nums.values()])
    
with open(os.path.join(PRETEST_DIR,'senpred_parsed.txt'),'w') as f:
    writer = csv.writer(f,delimiter='\t')
    writer.writerow(['Word','Predictive','RespProp'])
    for k,v in output.items():
        writer.writerow([k[0],k[1],v])
print(output)
#print(sentence_dict)
