#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
This module contains a class for reading data from files. The input and output is
formatted in such a way that the class is competible with PythonDataObject.java.
Created on Mon Oct 29 15:43:48 2018

@author: hegeman
"""
import csv

def _get_names_(array_reader):
    """
    Find all quantities, except 'Xp' and 'Yp', as well as the positions of Xp and Yp.
    For internal use only.
    :param array_reader: csv.reader object of the relevant file.
    :return x_start: position along the row where we can find the values for 'Xp'.
    :return y_start: position along the row where we can find the values for 'Yp'.
    :return parameters: list of the names of the different parameters.
    """
    x_start = -1
    y_start = -1
    parameters = []
    for row in array_reader:
        if len(row) > 2 and row[0] == '%' and row[1] == 'Xp':
            for (i, row_i) in enumerate(row):
                if x_start != -1 and y_start != -1 and row_i != '':
                    parameters.append(row_i)
                elif row_i == 'Xp':
                    x_start = i-1
                elif row_i == 'Yp':
                    y_start = i-1
            break
    return(x_start, y_start, parameters)

class swanDataObject:
    """
    Class for reading data from files.
    """
    def __init__(self, *args):
        """
        :param *args: First argument is the absolute file path of the file to be read,
        others are unused.
        """
        self.file_name = args[0]
        if len(args) > 1:
            self.args = args[1:]

    def get_exchange_item_ids(self):
        """
        Finds the names of all observed variables found in file_name;
        first read from left to right, then from top to bottom.
        :return: list of variable names as used by the Java code.
        """
        ids = []
        with open(self.file_name, 'r') as file:
            array_reader = csv.reader(file, delimiter=' ', skipinitialspace=True)
            (x_start, y_start, parameters) = _get_names_(array_reader)
            for row in array_reader:
                if len(row) > len(parameters) and row[0] != '%':
                    location = str(float(row[x_start])) + ',' + str(float(row[y_start]))
                    ids += [i + ' @ ' + location for i in parameters]
        #        print('! '.join(row))
        file.close()
        return ids

    def get_data_object_exchange_item(self, exchange_item_id):
        """
        Finds the values corresponding to the given names.
        :param exchange_item_ids: list of names of the variables we are interested in.
        :return: list of desired values.
        """
        #get_values maar dan met een enkele string ipv lijst
        obs = []
        with open(self.file_name, 'r') as file:
            array_reader = csv.reader(file, delimiter=' ', skipinitialspace=True)
            (x_start, y_start, parameters) = _get_names_(array_reader)
            find_parameter = exchange_item_id.split(' @ ')[0]
            try:
                find_location = exchange_item_id.split(' @ ')[1]
            except IndexError:
                file.close()
                return obs
            for row in array_reader:
                if len(row) > len(parameters) and row[0] != '%':
                    location = str(float(row[x_start])) + ',' + str(float(row[y_start]))
                    if find_location == location:
                        if find_location == location:
                            ind = y_start + 1 + parameters.index(find_parameter)
                            obs.append(float(row[ind]))
                        break
        #        print('! '.join(row))
        file.close()
        return obs

#    def get_values2(self, exchange_item_ids):
#        """
#        Finds the values corresponding to the given names.
#        :param exchange_item_ids: list of names of the variables we are interested in.
#        :return: list of desired values.
#        """
#        obs = []
#        with open(self.file_name, 'r') as file:
#            array_reader = csv.reader(file, delimiter = ' ', skipinitialspace = True)
#            (x_start, y_start, parameters) = _get_names_(array_reader)
#            parameters = list(filter(None, parameters))
#            for row in array_reader:
#                if len(row) > len(parameters) and row[0] != '%':
#                    for (i, exchange_item_id) in enumerate(exchange_item_ids):
#                        find_parameter = exchange_item_id.split(' @ ')[0]
#                        find_location = exchange_item_id.split(' @ ')[1]
#                        if row[x_start][-1] == '.':
#                            row[x_start] += '0'
#                        if row[y_start][-1] == '.':
#                            row[y_start] += '0'
#                        location = row[x_start] + ',' + row[y_start]
#                        if find_location == location:
#                            #if row[-1] == '':
#                            #    obs.append(float(row[-len(parameters)-1 + parameters.index(find_parameter)]))
#                            #else:
#                            ind =  y_start + 1 + parameters.index(find_parameter)
#                            obs.append(float(row[ind]))
#        #        print('! '.join(row))
#        file.close()
#        return obs

def main():
#    test = swanDataObject('swanObservations.txt')
    test = swanDataObject('/v3/Stage/Rick/openda/openda_public/model_swan/tests/swan_python/stochObserver/observ/meas_l021triad001_loc.tab', 'bla', 'die', 'bla')
    a = test.get_exchange_item_ids()
    print(a)
    print(test.get_data_object_exchange_item('Tm_10 @ 0.0,0.0'))
if __name__ == '__main__':
    main()
