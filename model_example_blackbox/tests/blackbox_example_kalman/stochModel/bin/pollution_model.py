#! /usr/bin/python

'''A one dimensional pollution model. '''

import sys
import math


def defaultInput():
    input={}
    # grid
    input['x']= [0.0, 1.0, 4.0] 
    # stationary flow
    input['u'] = [1.0, 1.0, 1.0, 1.0, 1.0]
    # cross sectional area
    input['a'] = [1.0, 1.0, 1.0, 1.0, 1.0]
    # initial concentrations
    input['c'] = [0.1, 0.2, 0.3, 0.4, 0.5]
    # simulation timespan
    input['refdate'] = '01 dec 2000'
    #unit is always seconds
    input['unit'] = 'seconds'
    input['time'] = [0.0, 1.0, 10.0]
    # sources mass/m^3/s
    input['source_locations'] = [2]
    input['source_labels'] = ['default']
    input['source_values']= {}
    input['source_values']['default'] = [5.0]
    # boundaries
    input['bound_labels']=['left', 'right']
    input['bound_locations']=[0, -1]
    input['bound_values']={}
    input['bound_values']['left']=[-1000.0, 0.01, 0.02, 0.03]
    input['bound_values']['right']=[0.0]
    #output (index based and 0 based)
    input['output_file'] = 'default.output'
    input['output_locations'] = [1, 2]
    input['output_labels']=['defaultOutput1', 'defaultOutput2']
    return input

def initOutput(input):
    output={}
    #output (index based and 0 based)
    output['output_file'] = input['output_file']
    output['output_locations'] = input['output_locations']
    output['output_labels']=input['output_labels']
    output['output_values']={}
    for label in output['output_labels']:
        output['output_values'][label]=[]
    output['refdate']=input['refdate']
    output['unit']=input['unit']
    output['time']=input['time']
    return output

def computeNextTimeStep(tIndex, c, input):
    #print 'computing next timestep'
    cNext = [0.0 for dummy in c]
    #print 'c='+str(cNext)
    #print 'transport '
    time=input['time']
    x=input['x']
    u=input['u']
    for i in xrange(0, len(c), 1):
        #print 'computing for gridpoint '+str(i)
        di = u[i]*time[1]/x[1]
        iLeft = i+int(math.floor(di))
        #print 'i = %d di= %f iLeft = %f' % (i, di, iLeft)
        weightRight= (di-math.floor(di))
        weightLeft=1.0-weightRight
        iRight = iLeft+1
        if((iLeft>=0) & (iLeft<len(cNext))):
            cNext[iLeft]  +=  c[i]*weightLeft;
        if((iRight>=0) & (iRight<len(cNext))):
            cNext[iRight] += c[i]*weightRight
    #print 'c='+str(cNext)
    #print 'add sources'
    source_locations=input['source_locations']
    source_labels=input['source_labels']
    source_values=input['source_values']
    a=input['a']
    for iSource  in range(len(source_locations)):
        iLoc = source_locations[iSource]
        iLabel=source_labels[iSource]
        cValues = source_values[iLabel]
        if(tIndex<len(cValues)):
            cValue = cValues[tIndex]
        else:
            cValue = cValues[-1]
        cValue = max(cValue, 0.0)
        cNext[iLoc]+=cValue*time[1]/x[1]/a[iLoc]
    #print 'c='+str(cNext)
    #print 'inflow boundaries'
    bound_values=input['bound_values']
    if (u[0]>0.0):
        bValues=bound_values['left']
        if(tIndex<len(bValues)):
            bValue = bValues[tIndex]
        else:
            bValue = bValues[-1]
        bValue=max(bValue, 0.0)
        cNext[0] = bValue
    if (u[-1]<0.0):
        bValues=bound_values['right']
        if(tIndex<len(bValues)):
            bValue = bValues[tIndex]
        else:
            bValue = bValues[-1]
        cNext[-1]=bValue
    #print 'c='+str(cNext)
    return cNext

def readInputFile(fileName):
    input={}
    source_values= {}
    bound_values= {}
    output_values= {}
    print 'reading input from file '+fileName
    inFile=open(fileName, 'r')
    counter =1
    for line in inFile.xreadlines():
        #print "%d : %s" %(counter, line[:-1])
        exec "global x;"+line  in globals(),  locals()
        counter+=1
    inFile.close()
    input['x']=x
    input['u']= u
    input['a']= a
    input['c']= c
    input['refdate']= refdate
    input['unit']= unit 
    input['time']= time
    input['source_locations']= source_locations
    input['source_labels']= source_labels
    input['source_values']= source_values
    input['output_file']= output_file
    input['output_locations']= output_locations
    input['output_labels']= output_labels
    input['bound_labels']= bound_labels
    input['bound_locations']= bound_locations
    input['bound_values']= bound_values
    return input
    
def collectOutput(c, output):
    for i in range(len(output['output_locations'])):
        iOutput =output['output_locations'][i]
        iLabel=output['output_labels'][i]
        output['output_values'][iLabel].append(c[iOutput])
        #print 'c[%d]=%f' % (iOutput, c[iOutput])
    #print 'c='+str(c)

def writeOutput(output, c):
    outFile=open(output['output_file'], 'w')
    print "writing output to file %s" % output['output_file']
    outFile.write("output_labels=["+','.join([ "'"+label+"'" for label in output['output_labels']])+"]\n")
    output_locations=output['output_locations']
    for i in range(len(output_locations)):
        if (output_locations[i]<0):
            output_locations[i] += len(output_locations)
    outFile.write("output_locations=["+','.join(map(str, output_locations[:]))+"]\n")
    output_values=output['output_values']
    for i in range(len(output_locations)):
        outFile.write("output_values['"+output['output_labels'][i]+"']=["+','.join(map(str, output_values[output['output_labels'][i]]))+"]\n")
    outFile.write("c=["+','.join(map(str, c))+"]\n")
    outFile.write("refdate='%s'\n"%output['refdate'])
    outFile.write("unit='%s'\n" % output['unit'])
    outFile.write("time=[%f,%f,%f] \n" %(output['time'][0],output['time'][1],output['time'][2])) 
    outFile.close()

def frange(start, end=None, inc=None):
    "A range function, that does accept float increments..."
    if end == None:
        end = start + 0.0
        start = 0.0
    if inc == None:
        inc = 1.0
    L = []
    while 1:
        next = start + len(L) * inc
        if inc > 0 and next >= end:
            break
        elif inc < 0 and next <= end:
            break
        L.append(next)        
    return L


if __name__ == '__main__':
    # look for input file
    input={}
    if (sys.argv[-1].endswith(".input")):
        inputFile = sys.argv[-1]
        input=readInputFile(inputFile)
    else:
        print "using internal default input"
        input=defaultInput()
    output=initOutput(input)
    print 'main computations'
    tIndex = 0
    cNow=input['c'][:]
    time=input['time']
    collectOutput(cNow, output)
    for t in frange(time[0], time[2], time[1]):
        print 'computing from time '+str(t)+' to '+str(t+time[1])+'  '+str(100*(t)/(time[2]-time[0]))+'%'
        cNow=computeNextTimeStep(tIndex, cNow, input)
        collectOutput(cNow, output)
        tIndex+=1
    writeOutput(output, cNow)
    print 'simulation ended successfully'
