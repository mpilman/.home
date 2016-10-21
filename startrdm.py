#!/usr/bin/env python3

import os
import subprocess

f = open('/dev/null', 'a')
proc = subprocess.run(
    ['clang', '-E', '-x', 'c++', '-std=c++11', '-stdlib=libc++', '-', '-v'],
    stdin=f, stderr=subprocess.PIPE, stdout=None)

is_include_path = False
includes = ["/usr/include", "/usr/local/include"]
for line in proc.stderr.decode('utf-8').split('\n'):
    if not line:
        break
    if line.startswith('#include'):
        is_include_path = True
    elif is_include_path and line.startswith(' '):
        frm = ' (framework directory)'
        if line.endswith(frm):
            line = line[:-len(frm)]
        includes.append(line[1:])
    else:
        is_include_path = False

# remove duplicates
seen = set()

def addIfNotSeen(x):
    if x in seen:
        return False
    else:
        seen.add(x)
        return True
includes = filter(addIfNotSeen, includes)


f.close()
params = map(lambda x: '--isystem {}'.format(x), includes)
cmd = "rdm -i 100 -W -l {}".format(" ".join(params))
os.system(cmd)
