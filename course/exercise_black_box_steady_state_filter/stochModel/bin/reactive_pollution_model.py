#!/usr/bin/env python3

'''A one dimensional reactive pollution model. '''
from __future__ import print_function
import csv
import sys
import math
import string
import argparse
import logging
import os.path
import yaml

DEFAULT_INPUT = {
    'reaction_time': [3.0],      # reaction time (inverse of rate) in seconds
    'time': [0.0, 1.0, 10.0]     # [t_start, dt, t_end]
}

EX_CONFIG = 78

def defaultInput():
    inputValues={}
    # grid
    inputValues['x']= [0.0, 1.0, 4.0]
    # stationary flow
    inputValues['u'] = [1.0, 1.0, 1.0, 1.0, 1.0]
    # cross sectional area
    inputValues['a'] = [1.0, 1.0, 1.0, 1.0, 1.0]
    # initial concentrations
    inputValues['c1'] = [0.1, 0.2, 0.3, 0.4, 0.5]
    inputValues['c2'] = [1.1, 1.2, 1.3, 1.4, 1.5]
    # simulation timespan
    inputValues['refdate'] = '01 dec 2000'
    #unit is always seconds
    inputValues['unit'] = 'seconds'
    # sources mass/m^3/s
    inputValues['source_locations'] = [2]
    inputValues['source_substance'] = [1]
    inputValues['source_labels'] = ['c1_default']
    inputValues['source_values']= {}
    inputValues['source_values']['c1_default'] = [5.0]
    # boundaries
    inputValues['bound_labels']=['c1_left', 'c1_right', 'c2_left', 'c2_right']
    inputValues['bound_locations']=[0, -1, 0 ,-1]
    inputValues['bound_values']={}
    inputValues['bound_values']['c1_left']=[-1000.0, 0.01, 0.02, 0.03]
    inputValues['bound_values']['c1_right']=[0.0]
    inputValues['bound_values']['c2_left']=[-2000.0, 1.01, 1.02, 1.03]
    inputValues['bound_values']['c2_right']=[0.0]
    #output (index based and 0 based)
    inputValues['output_file'] = 'default.output'
    inputValues['matlab_output_file'] = 'default_output.m'
    inputValues['output_locations'] = [1, 2, 1 ,3]
    inputValues['output_substance'] = [1, 1, 2 ,2]
    inputValues['output_labels']=['c1_1', 'c1_2', 'c2_1', 'c2_3']
    return inputValues

def initOutput(config):
    output={}
    #output (index based and 0 based)
    try:
        output['output_timeseries'] = config['output']['timeseries']
        for item in output['output_timeseries']:
            item['data'] =[]
    except Exception as e:
        logger.error("error initializing time series output %s",e)
        raise e
    return output

def interp(x,y,x0,index=0):
    for i in range( index, len(x)-1 ):
        if (x0  - 1e-6 ) < x[i+1]:
            w = ( x0 -x[i]) /(x[i+1] - x[i])
            value = w * y[i+1] + (1.0-w) * y[i]
            index = i
            break
    logger.debug('interp x {} [{},{}] {}'.format(i,x[i],x[i+1], x0))
    logger.debug('interp y {} [{},{}] {}'.format(w, y[i],y[i+1], value))
    return value, index

def computeNextTimeStep(currentTime, c1, c2, inputValues):
    logger.debug('computing next timestep')
    c1Next = [0.0 for dummy in c1]
    c2Next = [0.0 for dummy in c2]
    logger.debug('transport ')
    time=inputValues['time']
    reaction_time=inputValues['reaction_time']
    x=inputValues['x']
    u=inputValues['u']
    for i in range(0, len(c1), 1):
        # logger.debug('computing for gridpoint '+str(i))
        di = u[i]*time[1]/x[1]
        iLeft = i+int(math.floor(di))
        # logger.debug('i = %d di= %f iLeft = %f' % (i, di, iLeft))
        weightRight= (di-math.floor(di))
        weightLeft=1.0-weightRight
        iRight = iLeft+1
        if((iLeft>=0) & (iLeft<len(c1Next))):
            c1Next[iLeft]  +=  c1[i]*weightLeft
            c2Next[iLeft]  +=  c2[i]*weightLeft
        if((iRight>=0) & (iRight<len(c1Next))):
            c1Next[iRight] += c1[i]*weightRight
            c2Next[iRight] += c2[i]*weightRight
        # reaction
        rate = time[1]/reaction_time
        c1Next[i] += - c1Next[i] * rate
        c2Next[i] +=   c1Next[i] * rate
    # logger.debug('c1='+str(c1Next))
    # logger.debug('c2='+str(c2Next))
    logger.debug('add sources')
    logger.debug(inputValues['sources'])

    a=inputValues['a']
    for source in inputValues['sources']:
        cValue = source['values'][0]
        if 'times' in source:        
            cValue, source['currentIndex'] = interp(source['times'],source['values'],currentTime, source['currentIndex'])
        cValue = max(cValue,0)
        iLoc = source['location']
        if(source['substance']==1):
           c1Next[iLoc]+=cValue*time[1]/x[1]/a[iLoc]
        else:
           c2Next[iLoc]+=cValue*time[1]/x[1]/a[iLoc]
    # logger.debug('c1='+str(c1Next))
    # logger.debug('c2='+str(c2Next))
    logger.debug('inflow boundaries')

    for boundary in inputValues['boundaries']:
        bValue = boundary['values'][0]
        if 'times' in boundary:
            cValue, boundary['currentIndex'] = interp(boundary['times'],boundary['values'],currentTime, boundary['currentIndex'])
        bValue = max(bValue,0)
        location = boundary['location']
        logger.debug(boundary)
        if boundary['id'].endswith('left'):
            if u[location]>0 :            
                if boundary['substance'] == 1:
                    c1Next[location] = bValue
                if boundary['substance'] == 2:
                    c2Next[location] = bValue
        if boundary['id'].endswith('right'):
            if u[location]<0 :            
                if boundary['substance'] == 1:
                    c1Next[location] = bValue
                if boundary['substance'] == 2:
                    c2Next[location] = bValue

    # logger.debug('c1='+str(c1Next))
    # logger.debug('c2='+str(c2Next))
    return (c1Next,c2Next)

def readInputFile(fileName):
    inputValues={}
    logger.info('reading input from file '+fileName)
    counter =1
    localParams = dict.fromkeys([
        'x',
        'u',
        'a',
        'refdate',
        'unit',
        'source_locations',
        'source_substance',
        'source_labels',
        'source_values',
        'output_file',
        'matlab_output_file',
        'output_map_times',
        'output_locations',
        'output_substance',
        'output_labels',
        'output_values',
        'bound_labels',
        'bound_locations',
        'bound_substance',
        'bound_values',
        ])

    localParams['source_values'] = {} 
    localParams['bound_values'] = {}
    localParams['output_values'] = {} 

    with open(fileName, 'r') as inFile:
     for line in inFile:
        logger.debug("%d : %s" %(counter, line[:-1]))
        if not line.startswith("#"):
            exec(line, None, localParams)
        counter+=1
    inputValues['x']= localParams['x']
    inputValues['u']= localParams['u']
    inputValues['a']= localParams['a']
    inputValues['refdate']= localParams['refdate']
    inputValues['unit']= localParams['unit']
    inputValues['source_locations']= localParams['source_locations']
    inputValues['source_substance']= localParams['source_substance']
    inputValues['source_labels']= localParams['source_labels']
    inputValues['source_values']= localParams['source_values']
    inputValues['output_file']= localParams['output_file']
    inputValues['matlab_output_file'] = localParams['matlab_output_file']
    inputValues['output_map_times'] = localParams['output_map_times']
    inputValues['output_locations']= localParams['output_locations']
    inputValues['output_substance']= localParams['output_substance']
    inputValues['output_labels']= localParams['output_labels']
    inputValues['bound_labels']= localParams['bound_labels']
    inputValues['bound_locations']= localParams['bound_locations']
    inputValues['bound_substance']= localParams['bound_substance']
    inputValues['bound_values']= localParams['bound_values']
    return inputValues

def readASCIIFile(file_name):
    """ Reads an ASCII file containing a float value on each line. """
    logger.info('reading from ASCII file %s',file_name)
    try:
        with open(file_name, 'r') as fin:
            file_contents = fin.readlines()
    except EnvironmentError as exception:
        logger.fatal(exception)
        sys.exit(-1)

    try:
        file_contents = [float(val) for val in file_contents]
    except ValueError:
        logger.fatal("ERROR: Could not cast the values in the file %s to floats." % file_name)
        raise

    logger.debug(file_contents)
    return file_contents

def writeASCIIFile(file_name, values):
    """ Write an ASCII file containing a float value on each line. """
    logger.info('writing to ASCII file %s',file_name)
    directory = os.path.dirname(file_name)
    if not os.path.isdir(directory):
        try:
            os.makedirs(directory)
        except Exception as e:
            logger.fatal("Cannot create directory: %s", directory)
            raise(e)
    try:
        with open(file_name, 'w') as fout:
            for value in values:
                fout.write("{0:0.2f}\n".format(value))
    except EnvironmentError as exception:
        logger.fatal(exception)
        sys.exit(-1)
    return

def collectOutput(c1, c2, output,time):
    """Add current values of c1, c2 at output locations to the appropriate time series."""

    logger.debug(output)
    for item in output['output_timeseries']:
        if (item['substance']==1):
            item['data'].append({'time': time, 'value' : c1[item['location']]})
        else:
            item['data'].append({'time': time, 'value' : c2[item['location']]})

def writeOutput(outFile, c1, c2):
    logger.info("writing output to file %s", outFile.name)
    outFile.write("source_values= {}\n")
    outFile.write("bound_values= {}\n")
    outFile.write("output_values= {}\n")
    outFile.write("source_labels=["+','.join([ "'"+label+"'" for label in inputValues['source_labels']])+"]\n")
    outFile.write("source_locations=["+','.join(map(str, inputValues['source_locations']))+"]\n")
    outFile.write("source_substance=["+','.join(map(str, inputValues['source_substance']))+"]\n")
    source_locations=inputValues['source_locations']
    for i in range(len(source_locations)):
        if (source_locations[i]<0):
            source_locations[i] += len(source_locations)
    outFile.write("source_locations=["+','.join(map(str, source_locations[:]))+"]\n")
    source_values=inputValues['source_values']
    for i in range(len(source_locations)):
        outFile.write("source_values['"+inputValues['source_labels'][i]+"']=["+','.join(map(str, source_values[inputValues['source_labels'][i]]))+"]\n")

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

def writeMatlabOutput(matlabOutFile, c1, c2):
    logger.info("writing output to MATLAB file %s", matlabOutFile.name)
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
    matlabOutFile.write("source_labels=["+','.join([ "'"+label+"'" for label in inputValues['source_labels']])+"];\n")
    matlabOutFile.write("source_locations=["+','.join(map(str, inputValues['source_locations']))+"];\n")
    matlabOutFile.write("source_substance=["+','.join(map(str, inputValues['source_substance']))+"];\n")
    source_locations=inputValues['source_locations']
    for i in range(len(source_locations)):
        if (source_locations[i]<0):
            source_locations[i] += len(source_locations)
    matlabOutFile.write("source_locations=["+','.join(map(str, source_locations[:]))+"];\n")
    source_values=inputValues['source_values']
    for i in range(len(source_locations)):
        matlabOutFile.write("source_values."+inputValues['source_labels'][i]+"=["+','.join(map(str, source_values[inputValues['source_labels'][i]]))+"];\n")
    #final state and times
    matlabOutFile.write("c1=["+','.join(map(str, c1))+"];\n")
    matlabOutFile.write("c2=["+','.join(map(str, c2))+"];\n")
    matlabOutFile.write("refdate='%s';\n"%output['refdate'])
    matlabOutFile.write("unit='%s';\n" % output['unit'])
    matlabOutFile.write("time=[%f,%f,%f]; \n" %(output['time'][0],output['time'][1],output['time'][2]))

def readBoundaries(config):
    result = config
    for item in result:
        item['currentIndex'] = 0
        if 'file' in item:
            logger.info("Reading time series for '{}'".format(item['id']))
            if item['values']:
                logger.warning('Overwriting values for {}'.format(item['id']))
            time_series=readTimeSeriesFromCsv(item['file'])
            item['times'] = time_series['times']
            item['values'] = time_series['values']
    return result

def readTimeSeriesFromCsv(file):
    time_series= {'times':[], 'values': [] }
    try:
        with open(file, 'r') as csv_file:
            csv_reader = csv.reader(csv_file,delimiter=',')
            line_count = 0
            for row in csv_reader:
                if line_count == 0:
                    logger.debug('Read header {}'.format(row))
                else:
                    try:
                        element = row[0]
                        time_series['times'].append(float(element))
                        element = row[1]
                        time_series['values'].append(float(element))
                    except ValueError:
                        logger.fatal("Could not convert '{}' to a float.".format(element))
                        exit(EX_CONFIG)
                line_count +=1
    except EnvironmentError as exception:
        logger.fatal(exception)
        sys.exit(-1)
    return time_series

def writeMatlabMapOutput(matlabOutFile, c1, c2, timeIndex):
    matlabOutFile.write("c1_map{"+str(timeIndex)+"}=["+','.join(map(str, c1))+"];\n")
    matlabOutFile.write("c2_map{"+str(timeIndex)+"}=["+','.join(map(str, c2))+"];\n")

def writePythonMapOutputInit(pythonOutFile):
    pythonOutFile.write("c1_map={}\n")
    pythonOutFile.write("c2_map={}\n")

def writePythonMapOutput(pythonOutFile, c1, c2, timeIndex):
    pythonOutFile.write("c1_map["+str(timeIndex-1)+"]=["+','.join(map(str, c1))+"]\n")
    pythonOutFile.write("c2_map["+str(timeIndex-1)+"]=["+','.join(map(str, c2))+"]\n")

def frange(start, end=None, inc=None):
    "A range function, that does accept float increments..."
    if end == None:
        end = start + 0.0
        start = 0.0
    if inc == None:
        inc = 1.0
    L = []
    while 1:
        nextValue = start + len(L) * inc
        if inc > 0 and nextValue >= end:
            break
        elif inc < 0 and nextValue <= end:
            break
        L.append(nextValue)
    return L


if __name__ == '__main__':

    # logging.basicConfig(level=logging.INFO,
    #     format="%(asctime)s %(levelname)s:%(message)s",
    #     datefmt='%H:%M:%S')

    logging.basicConfig(filename='reactive_pollution_model.log',
                            filemode='a',
                            format='%(asctime)s,%(msecs)d %(name)s %(levelname)s %(message)s',
                            datefmt='%H:%M:%S',
                            level=logging.INFO)

    # set up logging to console
    console = logging.StreamHandler()
    console.setLevel(logging.DEBUG)
    # set a format which is simpler for console use
    formatter = logging.Formatter('%(levelname)-8s %(message)s')
    console.setFormatter(formatter)
    # add the handler to the root logger

    # parse command line arguments
    parser = argparse.ArgumentParser(description="Run a reactive pollution model.")
    parser.add_argument("-c","--config", default='config.yaml',
                        help="YAML configuration file.")
    parser.add_argument("--log_level", default="INFO",
                            help="Set logging level")

    args = vars(parser.parse_args())

    level = logging.getLevelName(args['log_level'])
    logger = logging.getLogger(__name__)
    logger.setLevel(level)
    logger.addHandler(console)
    
    logger.debug(args)
    # read input files, or use defaults
    inputValues={}
    logger.info('start of program')
    logger.info('Reading {}'.format(args['config']))
    try:
        with open(args['config'], 'r') as stream:
            try:
                config = yaml.safe_load(stream)
            except yaml.YAMLError as exc:
                logger.fatal(exc)
                sys.exit(EX_CONFIG)
    except EnvironmentError as exception:
        logger.fatal(exception)
        sys.exit(-1)

    try: 
        inputValues['u'] = config['u']
        inputValues['x'] = config['x']
        inputValues['a'] = config['a']
        inputValues['time'] = config['time']
        inputValues['reaction_time'] = config['reaction_time']
    except Exception as e:
        logger.fatal('Failed setting inputValues from config file: %s',e)
        sys.exit(EX_CONFIG)

    # read initial fields
    for item in config['initial_values']:
        inputValues[item['id']] = readASCIIFile(item['file'])
    if not 'c1' in inputValues:
        inputValues['c1'] = [0.] * len(inputValues['u']) # Default: put concentrations to 0.
    if not 'c2' in inputValues:
        inputValues['c2'] = [0.] * len(inputValues['u']) # Default: put concentrations to 0.

    # read forcings + read boundary values
    inputValues['sources'] = readBoundaries(config['sources'])
    inputValues['boundaries'] = readBoundaries(config['boundaries'])

    output=initOutput(config)

    # File handles to csv output files (time series and concentration maps).
    file_handles = {}
    csv_writers = {}
    for item in config['output']['timeseries']:
        path = "%s.csv" % item['id']
        exists = os.path.isfile(path)
        try:
            file_handles.update({item['id']: open(path, 'a', newline='')})
        except EnvironmentError as exception:
            logger.fatal(exception)
            sys.exit(-1)

        dict_writer =  csv.DictWriter(file_handles[item['id']], fieldnames=['time','value'])
        csv_writers.update({item['id']: dict_writer})
        if not exists:
            logger.info("create new output file '%s' for timeseries '%s'",path, item['id'])
            dict_writer.writeheader()
        else:
            logger.info("append timeseries output '%s' to file '%s'",item['id'], path)


    logger.info('main computations')
    logger.debug(inputValues['c1'])

    c1Now=inputValues['c1'][:]
    c2Now=inputValues['c2'][:]
    
    time=config['time']
    logger.debug('Time {}'.format(time))

    logger.debug('Collect Output {}'.format(time[0]))
    collectOutput(c1Now, c2Now, output, time[0])

    # Set next output time for maps
    for maps in config['output']['maps']:
        if (time[0]) > maps['times'][2]:
            continue
        elif abs( (time[0] - maps['times'][0])) < 1.0e-6:
            maps['next_output_time'] = maps['times'][0] + maps['times'][1]
        elif (time[0]) < maps['times'][0]:
            maps['next_output_time'] = maps['times'][0]
        else:
            delta = (time[0] - maps['times'][0])/maps['times'][1]
            maps['next_output_time'] = maps['times'][0] + math.ceil(delta)* maps['times'][1]

    logger.info('computing from time %f to %f', time[0], time[2])
    for t in frange(time[0], time[2], time[1]):
        logger.debug('computing from time '+str(t)+' to '+str(t+time[1])+'  '+str(100*(t)/(time[2]-time[0]))+'%')
        (c1Now,c2Now)=computeNextTimeStep(t, c1Now, c2Now, inputValues)

        collectOutput(c1Now, c2Now, output, t+time[1])

        # Append to time series.
        for item in output['output_timeseries']:
            csv_writers[item['id']].writerow(item['data'][-1])

        # Write maps
        currentTime = t+time[1]
        for maps in config['output']['maps']:
            if 'next_output_time' in maps and abs( currentTime - maps['next_output_time']) < 1.0e-6:
                maps['next_output_time'] =  maps['next_output_time'] + maps['times'][1]
                if maps['next_output_time'] > maps['times'][2]:
                    maps.pop('next_output_time')
                outputFile = maps['file'].format(currentTime)
                if maps['substance'] == 1:
                    writeASCIIFile(outputFile, c1Now)
                if maps['substance'] == 2:
                    writeASCIIFile(outputFile, c2Now)

    #  write restart files
    for restart in config['output']['restart']:
        if restart['substance'] == 1:
            writeASCIIFile(restart['file'], c1Now)
        if restart['substance'] == 2:
            writeASCIIFile(restart['file'], c2Now)

    # close timeseries writers
    for label in file_handles:
        file_handles[label].close()

    logger.info('simulation ended successfully')
