#! /usr/bin/env python



import numpy as np

from BMI import BMI, BmiGridType
#from scipy import ndimage

class BMILorenz (BMI):
    _var_units = {'x': '[-]', 'y': '[-]', 'z': '[-]'}
    _name = 'Example Python Lorenz model, BMI'
    _input_var_names = ['x','y','z']
    _output_var_names = ['x','y','z']

    def __init__ (self):
        self._dt = 0
        self._t = 0.
        self._startTime = 0.
        self._endTime = 0.

        self._x = None
        self._y = None
        self._z = None
        
        self._value = {}
        
        self._shape = (0, 0)
        self._spacing = (0., 0.)
        self._origin = (0., 0.)


    def initialize (self):

        self._dt = 1e-3
        self._t = 0.
        self._startTime = 0.
        self._endTime = 20.
        
        self._sigma = 10.
        self._rho = 28.
        self._beta = 8.0/3.



        self._x = np.zeros(1)
        self._y = np.zeros(1)
        self._z = np.zeros(1)

        self._value['x'] = "_x"
        self._value['y'] = "_y"
        self._value['z'] = "_z"

        self._shape = (1, 1)
        self._spacing = (1., 1.)
        self._origin = (0., 0.)

        

    def update (self):
        if self._t >= self._endTime:
		    raise "endTime already reached, model not updated"
        
        self._x = self._x + self._dt * (self._sigma * (self._y - self._x))
        self._y = self._y + self._dt * (self._x * (self._rho - self._z) - self._y)
        self._z = self._z + self._dt * (self._x * self._y - self._beta * self._z)
        
        self._t += self._dt
        
    def update_until (self, t):
        if (t<self._t) or t>self._endTime:
            raise "wrong time input: smaller than model time or larger than endTime"
        while self._t < t:
            self.update ()

    def finalize (self):
        self._dt = 0
        self._t = 0

        self._x = 0
        self._y = 0
        self._z = 0

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
