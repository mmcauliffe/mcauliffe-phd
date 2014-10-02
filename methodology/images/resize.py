
#requires imagemagick

import os
import subprocess
import re

in_directory = '/home/michael/Documents/Linguistics/Dissertation/PickedPictures'
out_dir = '/home/michael/Documents/Linguistics/Dissertation/output'

files = os.listdir(in_directory)

widths = [300,400,500]

for w in widths:
    w_dir = os.path.join(out_dir,str(w))
    try:
        os.makedirs(w_dir)
    except OSError:
        pass
    for f in files:
        inpath = os.path.join(in_directory,f)
        outname = f.split('.')[0]
        outname = re.sub('[0-9 ]','',outname)
        outpath = os.path.join(w_dir,outname +'.bmp')
        argstring = "'%s[%dx%d]'" % (inpath,w,w)
        subprocess.call(['convert',inpath,
                        '-resize','%dx%d'%(w,w),
                        '-density','72x72',
                        '-background','white',
                        '-flatten',
                        outpath])
        
