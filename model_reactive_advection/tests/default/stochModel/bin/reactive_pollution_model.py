#! /usr/bin/env python3

'''A one dimensional reactive pollution model. '''

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
    input['c1'] = [0.1, 0.2, 0.3, 0.4, 0.5]
    input['c2'] = [1.1, 1.2, 1.3, 1.4, 1.5]
    # simulation timespan
    input['refdate'] = '01 dec 2000'
    #unit is always seconds
    input['unit'] = 'seconds'
    input['time'] = [0.0, 1.0, 10.0]
    # reaction time (inverse of rate) in seconds
    input['reaction_time'] = [3.0]
    # sources mass/m^3/s
    input['source_locations'] = [2]
    input['source_substance'] = [1]
    input['source_labels'] = ['c1_default']
    input['source_values']= {}
    input['source_values']['c1_default'] = [5.0]
    # boundaries
    input['bound_labels']=['c1_left', 'c1_right', 'c2_left', 'c2_right']
    input['bound_locations']=[0, -1, 0 ,-1]
    input['bound_values']={}
    input['bound_values']['c1_left']=[-1000.0, 0.01, 0.02, 0.03]
    input['bound_values']['c1_right']=[0.0]
    input['bound_values']['c2_left']=[-2000.0, 1.01, 1.02, 1.03]
    input['bound_values']['c2_right']=[0.0]
    #output (index based and 0 based)
    input['output_file'] = 'default.output'
    input['matlab_output_file'] = 'default_output.m'
    input['output_map_times'] = list(input['time'])
    input['output_locations'] = [1, 2, 1 ,3]
    input['output_substance'] = [1, 1, 2 ,2]
    input['output_labels']=['c1_1', 'c1_2', 'c2_1', 'c2_3']
    return input

def initOutput(input):
    output={}
    #output (index based and 0 based)
    output['output_file'] = input['output_file']
    output['matlab_output_file'] = input['matlab_output_file']
    output['output_locations'] = input['output_locations']
    output['output_substance'] = input['output_substance']
    output['output_labels']=input['output_labels']
    output['output_values']={}
    for label in output['output_labels']:
        output['output_values'][label]=[]
    output['refdate']=input['refdate']
    output['unit']=input['unit']
    output['time']=input['time']
    return output

def computeNextTimeStep(tIndex, c1, c2, input):
    #print 'computing next timestep'
    c1Next = [0.0 for dummy in c1]
    c2Next = [0.0 for dummy in c2]
    #print 'transport '
    time=input['time']
    reaction_time=input['reaction_time']
    x=input['x']
    u=input['u']
    for i in xrange(0, len(c1), 1):
        #print 'computing for gridpoint '+str(i)
        di = u[i]*time[1]/x[1]
        iLeft = i+int(math.floor(di))
        #print 'i = %d di= %f iLeft = %f' % (i, di, iLeft)
        weightRight= (di-math.floor(di))
        weightLeft=1.0-weightRight
        iRight = iLeft+1
        if((iLeft>=0) & (iLeft<len(c1Next))):
            c1Next[iLeft]  +=  c1[i]*weightLeft;
            c2Next[iLeft]  +=  c2[i]*weightLeft;
        if((iRight>=0) & (iRight<len(c1Next))):
            c1Next[iRight] += c1[i]*weightRight
            c2Next[iRight] += c2[i]*weightRight
	# reaction
	rate = time[1]/reaction_time[0];
        for j in xrange(0, 10, 1):
		c1Next[i] += - c1Next[i] * rate/10.0
		c2Next[i] +=   c1Next[i] * rate/10.0
    #print 'c1='+str(c1Next)
    #print 'c2='+str(c2Next)
    #print 'add sources'
    source_locations=input['source_locations']
    source_substance=input['source_substance']
    source_labels=input['source_labels']
    source_values=input['source_values']
    a=input['a']
    for iSource  in range(len(source_locations)):
        iLoc = source_locations[iSource]
        iSubstance = source_substance[iSource]
        iLabel=source_labels[iSource]
        cValues = source_values[iLabel]
        if(tIndex<len(cValues)):
            cValue = cValues[tIndex]
        else:
            cValue = cValues[-1]
        cValue = max(cValue, 0.0)
	if(iSubstance==1):
           c1Next[iLoc]+=cValue*time[1]/x[1]/a[iLoc]
	else:
           c2Next[iLoc]+=cValue*time[1]/x[1]/a[iLoc]
    #print 'c1='+str(c1Next)
    #print 'c2='+str(c2Next)
    #print 'inflow boundaries'
    bound_values=input['bound_values']
    if (u[0]>0.0):
        bValues=bound_values['c1_left']
        if(tIndex<len(bValues)):
            bValue = bValues[tIndex]
        else:
            bValue = bValues[-1]
        bValue=max(bValue, 0.0)
        c1Next[0] = bValue
    if (u[-1]<0.0):
        bValues=bound_values['c1_right']
        if(tIndex<len(bValues)):
            bValue = bValues[tIndex]
        else:
            bValue = bValues[-1]
        c1Next[-1]=bValue
    if (u[0]>0.0):
        bValues=bound_values['c2_left']
        if(tIndex<len(bValues)):
            bValue = bValues[tIndex]
        else:
            bValue = bValues[-1]
        bValue=max(bValue, 0.0)
        c2Next[0] = bValue
    if (u[-1]<0.0):
        bValues=bound_values['c2_right']
        if(tIndex<len(bValues)):
            bValue = bValues[tIndex]
        else:
            bValue = bValues[-1]
        c2Next[-1]=bValue
    #print 'c1='+str(c1Next)
    #print 'c2='+str(c2Next)
    return (c1Next,c2Next)

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
    input['c1']= c1
    input['c2']= c2
    input['refdate']= refdate
    input['unit']= unit 
    input['time']= time
    input['reaction_time']= reaction_time
    input['source_locations']= source_locations
    input['source_substance']= source_substance
    input['source_labels']= source_labels
    input['source_values']= source_values
    input['output_file']= output_file
    input['matlab_output_file'] = matlab_output_file
    input['output_map_times'] = output_map_times
    input['output_locations']= output_locations
    input['output_substance']= output_substance
    input['output_labels']= output_labels
    input['bound_labels']= bound_labels
    input['bound_locations']= bound_locations
    input['bound_substance']= bound_substance
    input['bound_values']= bound_values
    return input
    
def collectOutput(c1, c2, output):
    for i in range(len(output['output_locations'])):
        iOutput =output['output_locations'][i]
        iSubstance =output['output_substance'][i]
        iLabel=output['output_labels'][i]
	if (iSubstance==1):
           output['output_values'][iLabel].append(c1[iOutput])
           #print 'c1[%d]=%f' % (iOutput, c1[iOutput])
	else:
           output['output_values'][iLabel].append(c2[iOutput])
           #print 'c2[%d]=%f' % (iOutput, c2[iOutput])
    #print 'c1='+str(c1)
    #print 'c2='+str(c2)

def writeOutput(output, c1, c2):
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
    outFile.write("output_substance=["+','.join(map(str, output['output_substance']))+"]\n")
    outFile.write("c1=["+','.join(map(str, c1))+"]\n")
    outFile.write("c2=["+','.join(map(str, c2))+"]\n")
    outFile.write("refdate='%s'\n"%output['refdate'])
    outFile.write("unit='%s'\n" % output['unit'])
    outFile.write("time=[%f,%f,%f] \n" %(output['time'][0],output['time'][1],output['time'][2])) 
    outFile.close()

def writeMatlabOutput(matlabOutFile, c1, c2):
    matlabOutFile.write("output_labels=["+','.join([ "'"+label+"'" for label in output['output_labels']])+"];\n")
    output_locations=output['output_locations']
    for i in range(len(output_locations)):
        if (output_locations[i]<0):
            output_locations[i] += len(output_locations)
    matlabOutFile.write("output_locations=["+','.join(map(str, output_locations[:]))+"];\n")
    output_values=output['output_values']
    for i in range(len(output_locations)):
        matlabOutFile.write("output_values."+output['output_labels'][i]+"=["+','.join(map(str, output_values[output['output_labels'][i]]))+"];\n")
    matlabOutFile.write("output_substance=["+','.join(map(str, output['output_substance']))+"];\n")
    #sources
    matlabOutFile.write("source_labels=["+','.join([ "'"+label+"'" for label in input['source_labels']])+"];\n")
    matlabOutFile.write("source_locations=["+','.join(map(str, input['source_locations']))+"];\n")
    matlabOutFile.write("source_substance=["+','.join(map(str, input['source_substance']))+"];\n")
    source_locations=input['source_locations']
    for i in range(len(source_locations)):
        if (source_locations[i]<0):
            source_locations[i] += len(source_locations)
    matlabOutFile.write("source_locations=["+','.join(map(str, source_locations[:]))+"];\n")
    source_values=input['source_values']
    for i in range(len(source_locations)):
        matlabOutFile.write("source_values."+input['source_labels'][i]+"=["+','.join(map(str, source_values[input['source_labels'][i]]))+"];\n")
    #final state and times
    matlabOutFile.write("c1=["+','.join(map(str, c1))+"];\n")
    matlabOutFile.write("c2=["+','.join(map(str, c2))+"];\n")
    matlabOutFile.write("refdate='%s';\n"%output['refdate'])
    matlabOutFile.write("unit='%s';\n" % output['unit'])
    matlabOutFile.write("time=[%f,%f,%f]; \n" %(output['time'][0],output['time'][1],output['time'][2])) 

def writeMatlabMapOutput(matlabOutFile, c1, c2, timeIndex):
    matlabOutFile.write("c1_map{"+str(timeIndex)+"}=["+','.join(map(str, c1))+"];\n")
    matlabOutFile.write("c2_map{"+str(timeIndex)+"}=["+','.join(map(str, c2))+"];\n")

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
    matlabOutFile=open(output['matlab_output_file'], 'w')
    print 'main computations'
    tIndex = 0
    c1Now=input['c1'][:]
    c2Now=input['c2'][:]
    time=input['time']
    collectOutput(c1Now, c2Now, output)
    for t in frange(time[0], time[2], time[1]):
        print 'computing from time '+str(t)+' to '+str(t+time[1])+'  '+str(100*(t)/(time[2]-time[0]))+'%'
        (c1Now,c2Now)=computeNextTimeStep(tIndex, c1Now, c2Now, input)
        collectOutput(c1Now, c2Now, output)
        tIndex+=1
        writeMatlabMapOutput(matlabOutFile, c1Now, c2Now, tIndex)
    writeOutput(output, c1Now, c2Now)
    writeMatlabOutput(matlabOutFile, c1Now, c2Now)
    matlabOutFile.close()
    print 'simulation ended successfully'
