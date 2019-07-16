#Open map files and read as array
import os
import numpy as np
import matplotlib.pyplot as plt




def read_file(mapdir,basename,time):
    name_combined=basename+"_"+str(float(time))+".txt"
    filename = os.path.join(mapdir, name_combined)
    vals =[]
    f = open(filename, 'r')
    for line in f:
        vals.append(float(line))
    f.close()
    return vals

c1=[]
c2=[]


def read_maps(model_dir, tstart=60, tstop=15000, tstep=60):

    map_dir = os.path.join(model_dir,"maps")
    c1_map=[]
    c2_map=[]
    for time in range(tstart, tstop, tstep):
        vals1 = read_file(map_dir,"concentration1",time)
        vals2 = read_file(map_dir,"concentration2",time)
        c1_map.append(vals1)
        c2_map.append(vals2)
    return c1_map, c2_map


#    print("c1_map[",str(itime),"]="+str(vals1)+"]")
#    print("c2_map[",str(itime),"]="+str(vals2)+"]")
