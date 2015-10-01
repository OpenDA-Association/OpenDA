#! /usr/bin/env python



import numpy as np

from BMI import BMI, BmiGridType
#from scipy import ndimage

class BMIAR1 (BMI):
    _var_units = {'state': '[-]'}
    _name = 'Example Python AR1 model, BMI'
    _input_var_names = ['state']
    _output_var_names = ['state']

    def __init__ (self):
        self._dt = 0
        self._shape = (0, 0)
        self._spacing = (0., 0.)
        self._origin = (0., 0.)
        self._t = 0.
        self._startTime = 0.
        self._endTime = 0.

        self._state = None
        self._forcing = None
        
        self._value = {}


    def initialize (self):

        self._dt = 1.
        self._t = 0.
        self._startTime = 1.
        self._endTime = 20.

        self._shape = (3, 1)

        self._spacing = (1., 1.)
        self._origin = (0., 0.)

        self._state = np.zeros (self._shape)
        
        self._timeSteps = int((self._endTime - self._startTime)/self._dt)
        
        self._forcing = np.random.rand(self._shape[0],self._timeSteps)
        self._forcingTimeIndex = 0 
        
        self._ARcoefMatrix=np.array([[0.5, 0.2, 0.3],[0.2, 0.5, 0.3],[0.3, 0.3, 0.4]])
        
        self._value['state'] = "_state"
        self._value['forcing'] = "_forcing"
        

    def update (self):
        if self._t >= self._endTime:
		    raise "endTime already reached, model not updated"
        
        self._state = np.dot(self._ARcoefMatrix,self._state) + self._forcing[:,self._forcingTimeIndex]
        
        self._forcingTimeIndex += 1
        self._t += self._dt
        
    def update_until (self, t):
        if (t<self._t) or t>self._endTime:
            raise "wrong time input: smaller than model time or larger than endTime"
        while self._t < t:
            self.update ()

    def finalize (self):
        self._dt = 0
        self._t = 0
        self._forcingTimeIndex = 0;

        self._state = np.array ([])
        self._forcing = np.array ([])

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
        val[indices] = src

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

def main ():
    import doctest
    doctest.testmod ()

if __name__ == '__main__':
    main ()
