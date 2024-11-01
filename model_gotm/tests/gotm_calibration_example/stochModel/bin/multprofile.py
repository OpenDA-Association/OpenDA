#!/usr/bin/env python

import sys, argparse
import numpy as np


def print_line(items):
    print(items)

def read_profiles(inputfile, outputfile, params, dimension, reverse=False, format="%g"):
    from netCDF4 import Dataset, num2date, date2num
    rootgrp = Dataset(inputfile, 'r', format='NETCDF4')
    # print header
    f= open(outputfile,'wb');
    # loop over time steps
    ntime = rootgrp.variables['time'].size;
    print(ntime)
    for timestep in range(0,ntime-1):
       nz = rootgrp.variables['z'].size;
       # print header
       table = np.zeros( [nz , len(params)] );
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
          header = ' '.join( ( str(num2date(rootgrp.variables['time'][timestep],rootgrp.variables['time'].units)) , str(nz) , str(invert) ) )

       try:
          if reverse:
              np.savetxt(f, table[::-1], delimiter=" ", fmt=format , header=header, comments='')
          else:
              np.savetxt(f, table, delimiter=" ", fmt=format , header=header, comments='')
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
    parser.add_argument('--reverse', action='store_true',help='reverse axis')
    parser.add_argument('--format', type=str, default="%g", help='format specifier')
    parser.add_argument('inputfile', type=str, help='input netcdf file')
    parser.add_argument('outputfile', type=str, help='output profile file')
    args = parser.parse_args()

    read_profiles(args.inputfile, args.outputfile, str.split(args.variables,',') , args.dimension, reverse=args.reverse, format=args.format)


if __name__ == "__main__":
    main(sys.argv[1:])

