import numpy as np
import sys
import logging

from bmi import EBmi, BmiGridType
#from scipy import ndimage

class IncrementModel (EBmi):
    _var_units = {'var1': 'unit1'}
    _name = 'Increment Model'
    _author = 'Rolf Hut'
    _input_var_names = ['var1']
    _output_var_names = ['var1']

    def __init__ (self):
        self._dt = 0
        self._shape = (0, 0)
        self._spacing = (0., 0.)
        self._origin = (0., 0.)
        self._t = 0.
        self._startTime = 0.
        self._endTime = 0.

        self._state = None
        
        self._value = {}

    #ebmi function
    def initialize_config(self, config_file):
        self._dt = 1.
        self._t = 1.
        self._startTime = 1.
        self._endTime = 20.

        self._shape = (10, 10)

        self._spacing = (1., 1.)
        self._origin = (0., 0.)
    
    #ebmi function
    def initialize_model(self):
        self._state = np.zeros (self._shape) + self._startTime
        
        self._value['var1'] = "_state"

    def initialize (self, config_file):
        self.initialize_config(config_file)
        self.initialize_model()
       
	#this is a warning to allow testing of the logging system 
        logging.warn('initialized model')

    def update (self):
        if self._t >= self._endTime:
		    raise Exception("endTime already reached, model not updated")
        self._state = self._state + 1
        self._t += self._dt

    def update_until (self, t):
        if (t<self._t) or t>self._endTime:
            raise Exception("wrong time input: smaller than model time or larger than endTime")
        while self._t < t:
            self.update ()

    def update_frac(self, time_frac):
        raise Exception("unsupported operation")
    
    def finalize (self):
        self._dt = 0
        self._t = 0

        self._state = np.array ([])

    def get_var_type (self, long_var_name):
        return str (self.get_value (long_var_name).dtype)
    def get_var_units (self, long_var_name):
        return self._var_units[long_var_name]
    def get_var_rank (self, long_var_name):
        return self.get_value (long_var_name).ndim

    def get_value (self, long_var_name):
        return getattr(self,self._value[long_var_name])
    
    def get_value_at_indices (self, long_var_name, indices):
        return self.get_value(long_var_name)[indices]
    
    def set_value (self, long_var_name, src):
        val = self.get_value (long_var_name)
        val[:] = src
        
    def set_value_at_indices (self, long_var_name, indices, src):
        val = self.get_value (long_var_name)
        
        sys.stderr.write(str(indices))
        sys.stderr.write("\n")
        sys.stderr.write(str(src))
        sys.stderr.write("\n")
        sys.stderr.write(str(val[indices].shape))
        sys.stderr.write("\n")
        sys.stderr.write(str(src.shape))
        sys.stderr.write("\n")
        sys.stderr.write(str(val))
        sys.stderr.write("\n")
        sys.stderr.write("\n")

        val.flat[indices] = src
        
        sys.stderr.write(str(val))
        sys.stderr.write("\n")


    def get_component_name (self):
        return self._name
    def get_input_var_names (self):
        return self._input_var_names
    def get_output_var_names (self):
        return self._output_var_names

    def get_grid_shape (self, long_var_name):
        return self.get_value (long_var_name).shape

    def get_grid_spacing (self, long_var_name):
        return self._spacing

    def get_grid_origin (self, long_var_name):
        return self._origin

    def get_grid_type (self, long_var_name):
        if self._value.has_key (long_var_name):
            return BmiGridType.UNIFORM
        else:
            return BmiGridType.UNKNOWN

    def get_start_time (self):
        return self._startTime
    def get_end_time (self):
        return self._endTime
    def get_current_time (self):
        return self._t

    def get_var_nbytes(self, long_var_name):
        return self._state.nbytes
    
    def get_var_size(self, long_var_name):
        return self._state.size

    def get_time_step(self):
        return 1.

    def get_time_units(self):
        return "seconds"

    def get_grid_x(self, long_var_name):
        raise Exception("unsupported operation")

    def get_grid_y(self, long_var_name):
        raise Exception("unsupported operation")

    def get_grid_z(self, long_var_name):
        raise Exception("unsupported operation")

    def get_grid_connectivity(self, long_var_name):
        raise Exception("unsupported operation")

    def get_grid_offset(self, long_var_name):
        raise Exception("unsupported operation")

    
    # extended BMI functions

    def save_state(self, destination_directory):
        raise Exception("unsupported operation")
    
    def load_state(self, destination_directory):
        raise Exception("unsupported operation")

    
    def set_start_time(self, start_time):
        self._startTime = start_time
    
    def set_end_time(self, end_time):
        self._endTime = end_time
    
    def get_attribute_names(self):
        return ['author']
    
    def get_attribute_value(self, attribute_name):
        if attribute_name == 'author':
            return self._author
        
        raise Exception('unknown attribute: ' + attribute_name)
    
    def set_attribute_value(self, attribute_name, attribute_value):
        if attribute_name == 'author':
           self._author = attribute_value
        else:
            raise Exception('unknown attribute: ' + attribute_name)

def main ():
    import doctest
    doctest.testmod ()

if __name__ == '__main__':
    main ()
