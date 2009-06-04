import os
import re

file =os.listdir("/usr/lib/R/library/base/R-ex")
file.sort(lambda x,y:cmp(x,y))


out= '" "'.join(file)
out = re.sub(".R","",out)
out = re.sub("^",'"',out)
out = re.sub("$",'"',out)
print out
 
