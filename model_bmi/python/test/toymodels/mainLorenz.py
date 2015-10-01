#! /usr/bin/env python


import numpy as np
import numpy.random
import BMILorenz

#make instance of BMILorenz
toy=BMILorenz.BMILorenz ()

#initialize and ask for varlists
toy.initialize ()

print "component name: "
print toy.get_component_name()
print "input variables: "
print toy.get_input_var_names()
print "output variables: "
print toy.get_output_var_names()
print "Variable type of 'x'"
print toy.get_var_type('x')
print "Unit type of 'x'"
print toy.get_var_units('x')
print "Rank of 'x'"
print toy.get_var_rank('x')
print "Value of 'x':"
print toy.get_value ('x')

print "Start time, end time and current time:"
print toy.get_start_time()
print toy.get_end_time()
print toy.get_current_time()


#change 1 value
print "changing value of x to 3"
toy.set_value ('x', 3)
print "Value of 'x'"
print toy.get_value ('x')

#update 1 timestep and check values

print "updateing 1 timestep"
toy.update()
print "Value of 'x'"
print toy.get_value ('x')

#update 5 steps
print "updating until timestep t=5"
toy.update_until(5)
print "Value of 'x'"
print toy.get_value ('x')
print "setting value of 'x' to 2" 
toy.set_value('x',2)
print "Value of 'x'"
print toy.get_value ('x')


#finaliz
print "finalize model"
toy.finalize()
print "Value of 'x'"
print toy.get_value ('x')
