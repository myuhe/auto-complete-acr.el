import os
import re

filenamelist=[]

for dpath,dname,filename in os.walk("/usr/lib/R/library"):
######
    for filename in filename:
        if filename == ("NAMESPACE"):
            fpath = os.path.join(dpath,filename)
            filenamelist.append(fpath)

## for dpath,dname,filename in os.walk("/usr/lib/R/library"):
##     for filename in filename:
##         r =open(os.path.join(dpath,filename))
##         for line in r:
##             print line

mer=[]
            
for file__ in filenamelist:
    f = open(file__)

    p1 = re.compile('export\(')
    frag = 0
    strfrag = 0

    for line in f:
        if line[0:7] == 'export(' and ")" not in line:
            frag = 1
            line = line.strip()
            line = p1.sub("",line)
            mer.append(line)
        elif line[0:7] == 'export(' and ")" in line:
            frag= 0
            line = line.strip()
            line = re.sub("\)",",",line)
            line = p1.sub("",line)
            mer.append(line)
        elif frag == 1 and ")" not in line and "#" not in line:
            line = line.strip()
            mer.append(line)
   
        elif frag == 1 and ")" in line and "#" not in line:
            frag = 0
            line = line.strip()
            line = re.sub("\)",",",line)
            mer.append(line)

        elif frag == 1 and ")" not in line and "#" in line:
            frag = 1
            
        elif frag == 1 and ")" in line and "#" in line:
            frag = 1

        else:
            frag = 0

        out = "".join(mer)
        out = re.sub(" ","",out)
        out = re.sub('\"',"",out)
        out = re.sub(",","\" \"",out)
        out = re.sub("^""",'\"',out)
        out = re.sub("$""",'\"',out)

print out
