#!/usr/bin/env python

import sys, argparse
import numpy as np


def print_line(items):
    print(items)

def read_profiles(inputfile, outputfile, params, dimension,depth1,depth2, reverse=False, format="%g"):
    from netCDF4 import Dataset, num2date, date2num
    rootgrp = Dataset(inputfile, 'r', format='NETCDF4')
    f= open(outputfile,'wb');
    # loop over time steps
    ntime = rootgrp.variables['time'].size;
    nz = rootgrp.variables['z'].size;
    print(ntime)
    for timestep in range(0,ntime-1):
       # print header
       table = np.zeros( [nz , len(params)] );
       meanval = np.zeros([1,len(params)+1])
       zvec = rootgrp.variables['z'];
       # find dimension
       index = 0
       for param in params:
           try:
              if 'time' in rootgrp.variables[param].dimensions:
                   table[:,index] = rootgrp.variables[param][timestep].flatten()
              else:
                   table[:,index] = rootgrp.variables[param][:].flatten()
              index+=1
           except KeyError:
              print("Variable '{0}' does not exist".format(param))
              sys.exit(64)


       invert = 2 if reverse else 1
       #print("reverse " )
       #print(reverse)
       header = ' '.join( ( str(num2date(rootgrp.variables['time'][timestep],rootgrp.variables['time'].units)) , str(1) , str(invert) ) )

       # calculate the mean

       try:
           if reverse:
               temp1 = next(zvec for zvec in zvec if abs(zvec) >= depth1);
               temp2= np.where(zvec==temp1)
               startmean = temp2[0]
               if depth2>abs(zvec[-1]):
                  endmean = -1
               else:
                  temp1 = next(zvec for zvec in zvec if abs(zvec) >= depth2);
                  temp2= np.where(zvec==temp1)
                  endmean = temp2[0]
               #print("startmean " )
               #print(startmean)
               #print("endmean ")
               #print(endmean)
           else:
               if depth1< abs(zvec[-1]):
                  endmean = -1
               else:
                  temp1 =next(zvec for zvec in zvec if abs(zvec) <= depth1);
                  temp2 = np.where(zvec==temp1)
                  endmean = temp2[0]
               temp1 = next(zvec for zvec in zvec if abs(zvec) <= depth2);
               temp2 = np.where(zvec==temp1)
               startmean = temp2[0]
               #print(depth1)
               #print(abs(zvec[-1]))
               #print("startmean " )
               #print(startmean)
               #print("endmean ")
               #print(endmean)

           meanval[0,0]=depth2-depth1
           index = 1
           for param in params:
               meanval[0,index] = sum(table[startmean:endmean,index-1])/len(table[startmean:endmean,index-1])
               index+=1

       except ValueError as e:
           print("Error in calculation of mean: {0}".format(e))

       try:
           np.savetxt(f, meanval, delimiter=" ", fmt=format , header=header, comments='')
       except OSError as e:
           print("Cannot write to file '{0}': {1}".format(outputfile, e.strerror))
       except IOError as e:
           print("I/O error({0}): {1}".format(e.errno, e.strerror))
       except ValueError as e:
           print("Error: {0}".format(e))
    f.close()
    rootgrp.close()



def main(argv):

    parser = argparse.ArgumentParser(description='''
        Extract a profile from the provided NetCDF GOTM output file
    '''
    )
    parser.add_argument('-d','--dimension',  type=str, default='z' ,help='get profile along this dimension')
    parser.add_argument('--variables',  type=str , metavar='V1(,V2)', required=True ,help='variables')
    parser.add_argument('--depth1', type=float, default='0.0', help='Starting depth of mean from surface [m]')
    parser.add_argument('--depth2', type=float, default='15.0', help='Ending depth of mean from surface [m]')
    parser.add_argument('--reverse', action='store_true',help='reverse axis')
    parser.add_argument('--format', type=str, default="%g", help='format specifier')
    parser.add_argument('inputfile', type=str, help='input netcdf file')
    parser.add_argument('outputfile', type=str, help='output profile file')
    args = parser.parse_args()

    read_profiles(args.inputfile, args.outputfile, str.split(args.variables,',') , args.dimension,args.depth1,args.depth2, reverse=args.reverse, format=args.format)


if __name__ == "__main__":
    main(sys.argv[1:])

