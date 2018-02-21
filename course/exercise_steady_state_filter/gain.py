#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
Read Kalman gain from xml-file

@author: verlaanm
"""
import os
import fnmatch
from xml.dom import minidom
import numpy as np

def read(dirname):
    '''Read and partially parse kalman gain file. This is a very rough and sloppy parser '''
    #find the right xml file
    xmlpattern="*.xml"
    filename=""
    for f in os.listdir(dirname):
       if os.path.isfile(os.path.join(dirname,f)):
           if(fnmatch.fnmatch(f,xmlpattern)):
               filename=os.path.join(dirname,f)
    xmldoc = minidom.parse(filename)
    vectorlist = xmldoc.getElementsByTagName('vector')
    #print(len(vectorlist))
    k_parts=[]
    for v in vectorlist:
        #print(v.firstChild.data)
        k_parts.append(eval("["+v.firstChild.data+"]"))
    k=np.vstack(k_parts)
    return(k)

# If this is run as the main program then run the tests
if __name__ == '__main__':
    dirname="enkf_wave_185811270000"
    k=read(dirname)
    print("k="+str(k))

