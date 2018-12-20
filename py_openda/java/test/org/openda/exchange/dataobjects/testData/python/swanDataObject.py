import csv

def _get_names_(array_reader):
    """
    Find all quantities, except 'Xp' and 'Yp', as well as the positions of Xp and Yp.
    For internal use only.
    :param array_reader: csv.reader object of the relevant file.
    :return x_start: position along the row where we can find the values for 'Xp'.
    :return y_start: position along the row where we can find the values for 'Yp'.
    :return forenames: list of the names of the different quantities.
    """
    x_start = -1
    y_start = -1
    forenames = []
    for row in array_reader:
        if len(row) > 2 and row[0] == '%' and row[1] == 'Xp':
            for (i, row_i) in enumerate(row):
                if x_start != -1 and y_start != -1 and row_i != '':
                    forenames.append(row_i)
                elif row_i == 'Xp':
                    x_start = i-1
                elif row_i == 'Yp':
                    y_start = i-1
            break
    return(x_start, y_start, forenames)

class swanDataObject:
    """
    Class for reading data from files.
    """
    def __init__(self, file_name):
        """
        :param file_name: full path name of the file to be read.
        """
        self.file_name = file_name


# FIXME: Mag weg?
#    def get_both_as_hash(self):
#        
#        hash_map = {}
#        with open(self.file_name, 'r') as file:
#            array_reader = csv.reader(file, delimiter = ' ', skipinitialspace = True)
#            (x_start, y_start, forenames) = _get_names_(array_reader)
#            for row in array_reader:
#                if len(row) > len(forenames) and row[0] != '%':
#                    surname = str(float(row[x_start])) + ',' + str(float(row[y_start]))
#                    full_names = [i + ' @ ' + surname for i in forenames]
#                    if row[-1] == '':
#                        obs = row[-len(forenames)-1:-1]
#                    else:
#                        obs = row[-len(forenames):]
#                    next_hash = {i: float(j) for i, j in zip(full_names,obs)}
#                    hash_map.update(next_hash)
#        #        print('! '.join(row))
#        file.close()
#        return hash_map

    def get_ids(self):
        """
        Finds the names of all observed variables found in file_name;
        first read from left to right, then top to bottom.
        :return: list of variable names as used by the Java code.
        """
        ids = []
        with open(self.file_name, 'r') as file:
            array_reader = csv.reader(file, delimiter = ' ', skipinitialspace = True)
            (x_start, y_start, forenames) = _get_names_(array_reader)
            for row in array_reader:
                if len(row) > len(forenames) and row[0] != '%':
                    surname = str(float(row[x_start])) + ',' + str(float(row[y_start]))
                    ids += [i + ' @ ' + surname for i in forenames]
        #        print('! '.join(row))
        file.close()
        print('bla')
        return ids

    


    def get_values(self, test_name):
        """
        Finds the values corresponding to the given names.
        :param test_names: list of names of the variables we are interested in.
        :return: list of desired values.
        """
        #get_values maar dan met een enkele string ipv lijst
        obs = []
        with open(self.file_name, 'r') as file:
            array_reader = csv.reader(file, delimiter = ' ', skipinitialspace = True)
            (x_start, y_start, forenames) = _get_names_(array_reader)
            find_fore = test_name.split(' @ ')[0]
            try: 
                find_sur = test_name.split(' @ ')[1]
            except IndexError:
                file.close()
                return obs
            for row in array_reader:
                if len(row) > len(forenames) and row[0] != '%':
                    surname = str(float(row[x_start])) + ',' + str(float(row[y_start]))
                    if find_sur == surname:
                        obs.append(float(row[-len(forenames) + forenames.index(find_fore)]))
                        break
        #        print('! '.join(row))
        file.close()
        return obs

    # FIXME: Welke van de twee mag weg????????

    def get_values2(self, test_names):
        """
        Finds the values corresponding to the given names.
        :param test_names: list of names of the variables we are interested in.
        :return: list of desired values.
        """
        obs = []
        with open(self.file_name, 'r') as file:
            array_reader = csv.reader(file, delimiter = ' ', skipinitialspace = True)
            (x_start, y_start, forenames) = _get_names_(array_reader)
            for row in array_reader:
                if len(row) > len(forenames) and row[0] != '%':
                    for (i, test_name) in enumerate(test_names):
                        find_fore = test_name.split(' @ ')[0]
                        find_sur = test_name.split(' @ ')[1]
                        if row[x_start][-1] == '.':
                            row[x_start] += '0'
                        if row[y_start][-1] == '.':
                            row[y_start] += '0'
                        surname = row[x_start] + ',' + row[y_start]
                        if find_sur == surname:
                            obs.append(float(row[-len(forenames) + forenames.index(find_fore)]))
        #        print('! '.join(row))
        file.close()
        return obs

def main():
    test = swanDataObject('swanObservations.txt')
    a = test.get_ids()
    print(test.get_values2(['Depth @ 206767.0,622696.0']))
    print(test.get_values2(a))
    print(test.get_values('Depth @ 206767.0,622696.0'))
if __name__ == '__main__':
    main()
