#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct 15 15:23:24 2018

@author: hegeman
"""

import csv
##file = open('test.txt','r')
#file = open('meas_l021triad001_loc.tab','r')
#print(file.read())
def get_obs(file_name):
    obs = []
    with open(file_name, 'r') as file:
        array_reader = csv.reader(file, delimiter = ' ', skipinitialspace = True)
        count = -1
        for row in array_reader:
            if len(row) > 2 and row[0] == '%' and count == -1:
                for (i, row_i) in enumerate(row):
                    if row_i == 'Yp':
                        count = 0
                    elif count != -1 and row_i != '':
                        count += 1
            elif len(row) > count and row[0] != '%':
                if row[-1] == '':
                    obs += row[-count-1:-1]
                else:
                    obs += row[-count:]
    #        print('! '.join(row))
    file.close()
    obs = [float(i) for i in obs]
    return obs

def main():
    print(get_obs('meas_l021triad001_loc.tab'))

if __name__ == '__main__':
    main()
    