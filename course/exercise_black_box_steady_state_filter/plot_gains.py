#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
Read Kalman gains from xml-files and make plots

@author: verlaanm
"""
import numpy as np
import matplotlib.pyplot as plt
import os
import fnmatch
from xml.dom import minidom

# The folders with kalman-gain files
dirnames =["enkf_gain_186212260000", "enkf_gain_186702030000", "enkf_gain_187103140000",\
        "enkf_gain_187504220000"]

# The kalman gain file looks like this:
# </comment>
#     <timeStampAsMJD>4500.0</timeStampAsMJD>
#     <timeStampAsDateTime>1871-03-14T01:00:00.000+01:00</timeStampAsDateTime>
#     <observations>
#         <observation>
#             <id>locA.concentration2</id>
#             <timeOffsetInDays>0.0</timeOffsetInDays>
#             <treeVector id="state" caption="State From Black Box Stoch Model Instance">
#                 <treeVectorLeaf id="noise_part_0" caption="noise_part_0">
#                     <vector>0.2336759029384301</vector>
#                 </treeVectorLeaf>
#                 <treeVectorLeaf id="noise_part_1" caption="noise_part_1">
#                     <vector>-0.116829878048781</vector>
#                 </treeVectorLeaf>
#                 <treeVectorLeaf id="concentration1.grid" caption="concentration1.grid">
#                     <vector>-7.80904407138262E-34,-7.80904407138262E-34,-7.80904407138262E-34,-7.80904407138262E-34,-7.80904407138262E-34,0.2211349585447442,0.23836821880944556,0.1940784465092705,0.10314082723371196,0.09486508288413271,0.1560963353848691,0.02618309478821236,0.04976655134294404,0.021515231917327238,0.040770766588895634,0.03987229090364141,0.02087748544600972,0.009406975311215031,0.012354595985904873,-0.01082876129218796,-7.560226653459288E-4,-0.0041265812126854715,-0.007176220208451965,-0.013497781358540678,0.003463948694941398,0.0020498558179553534,-1.686471665233762E-4,1.0068301444139389E-4,-0.004518154157544944,-0.002354494959796511,-0.10224058312491378,-0.10263523234214122,-0.09595064748907565,-0.15076574527359385,-0.12627810977484275,0.08738221664974516,-0.06189928067858391,-0.03065226367935009,-0.05272415065645906,0.018344890887550633,0.023811991666451454,0.024994716947980906,0.005930754157801085,-0.006951063059237723,-0.0017498117521876147,0.0039041259836657665,0.004537146032156205,0.0036967211412200526,-0.0076792554963937,0.007293305697261866,0.0021304399660037293,-0.0016898429607824334,-6.393994977055655E-4,-4.234898373390607E-4,-0.0018372697670435787,8.850529427750634E-4,-0.0017624281718314275,-2.834004985469989E-4,1.8216353251067965E-4,0.0016313516815730051,0.0011932921286906372</vector>
#                 </treeVectorLeaf>
#                 <treeVectorLeaf id="concentration2.grid" caption="concentration2.grid">
#                     <vector>-7.80904407138262E-34,-7.80904407138262E-34,-7.80904407138262E-34,-7.80904407138262E-34,-7.80904407138262E-34,-7.80904407138262E-34,0.014224918883115282,0.023912370447747175,0.019688961682567636,0.024964770879972974,0.05315252184063406,0.011008515178160734,0.025300997187919297,0.012685988380169252,0.0286338643427517,0.03178812590174154,0.01918978344887824,0.009391634314490043,0.01389705465013643,-0.014277441201406242,-8.83673505874088E-4,-0.006304190081218546,-0.012367991805617581,-0.025377292578691348,0.007594603870168682,0.004158160711462258,-5.414381876979407E-4,8.486272994726093E-4,-0.013023216736457031,-0.007246710616883664,-4.99586130781437E-4,-0.007378859936326287,-0.003943866804679671,-0.02555830550386718,-0.022887703012396454,0.02635553858850961,-0.02963635202007454,-0.007534657044519628,-0.023301692068191972,0.014606811008355843,0.028606787761334834,0.016730058258023785,0.00841843949895951,-0.006286612107184673,0.0037356449652135963,0.01372945585365322,0.013058602254483728,-8.81981896276674E-4,-0.021560182610563094,0.01025960469538214,0.0019997191661309103,-0.006601237297107262,-0.006439653007657398,-0.003684587392874068,-0.009322203042681734,0.0075740584569731805,-0.005733242464125535,-0.006811732072827412,7.675815345021303E-4,0.005238169898930842,0.00408725057284161</vector>
#                 </treeVectorLeaf>
#             </treeVector>
#         </observation>
# ... and then the next <observation> repeating the pattern ...
def read_gain(dirname):
    '''Read and partially parse kalman gain file. This is a very rough and sloppy parser '''
    #find the right xml file
    xmlpattern="*.xml"
    filename=""
    for f in os.listdir(dirname):
       if os.path.isfile(os.path.join(dirname,f)):
           if(fnmatch.fnmatch(f,xmlpattern)):
               filename=os.path.join(dirname,f)
    xmldoc = minidom.parse(filename)
    vectorlist = xmldoc.getElementsByTagName('treeVector')
    #print(len(vectorlist))
    k_parts_c1=[]
    k_parts_c2=[]
    for v in vectorlist:
        leafs=v.getElementsByTagName('vector')
        #print(v.firstChild.data)
        # parts 0 and 1 are ar(1) models for the sources
        k_parts_c1.append(eval("["+leafs[-2].firstChild.data+"]"))
        k_parts_c2.append(eval("["+leafs[-1].firstChild.data+"]"))
    k_c1=np.vstack(k_parts_c1)
    k_c2=np.vstack(k_parts_c2)
    # now the time
    time_field=xmldoc.getElementsByTagName('timeStampAsMJD')
    time_gain=eval("["+time_field[0].firstChild.data+"]")
    return((time_gain,k_c1,k_c2))


# read the kalman gains
k_all=[]
for dir in dirnames:
    k_all.append(read_gain(dir));


(nObs,nGrid)=np.shape(k_all[0][1])

plt.close("all")

# plot gain for each time it was saved
for itime in range(len(k_all)):
    f,ax = plt.subplots(2,1)
    k_time=k_all[itime][0]
    k_c1=k_all[itime][-2].T
    k_c2=k_all[itime][-1].T
    ax[0].plot(k_c1)
    ax[0].set_ylabel("concentration 1")
    ax[0].set_xlabel("x [index]")
    ax[0].set_title(f'Kalman gain at time {k_time}')
    #plt.title(f'Kalman gain at time {k_time}')
    ax[1].plot(k_c2)
    ax[1].set_ylabel("concentration 2")
    ax[1].set_xlabel("x [index]")
    #plt.legend(("station 1","station 2", "station 3"))
    plt.savefig(f'figure_gain_{itime}.png')


