#! /usr/bin/env python


import numpy as np
import numpy.random
import BMIAR1

#make instance of BMIAR1
toy=BMIAR1.BMIAR1 ()

#initialize and ask for varlists
toy.initialize ()

print "component name: "
print toy.get_component_name()
print "input variables: "
print toy.get_input_var_names()
print "output variables: "
print toy.get_output_var_names()
print "Variable type of 'state'"
print toy.get_var_type('state')
print "Unit type of 'state'"
print toy.get_var_units('state')
print "Rank of 'state'"
print toy.get_var_rank('state')
print "Value of 'state':"
print toy.get_value ('state')
print "Value of 'state' at indeces [2,1]"
print toy.get_value_at_indices('state',(2))

print "Start time, end time and current time:"
print toy.get_start_time()
print toy.get_end_time()
print toy.get_current_time()


#change 1 value
print "changing value at [2,1] to 3"
toy.set_value_at_indices ('state', (2), 3)
print "Value of 'state'"
print toy.get_value ('state')

#update 1 timestep and check values
print "Value of 'forcing'"
print toy.get_value ('forcing')

print "updateing 1 timestep"
toy.update()
print "Value of 'state'"
print toy.get_value ('state')

#update 5 steps
print "updating until timestep 6"
toy.update_until(6)
print "Value of 'state'"
print toy.get_value ('state')
print "setting value of 'state' to 2" 
toy.set_value('state',np.zeros([3,1])+2)
print "Value of 'state'"
print toy.get_value ('state')


#finaliz
print "finalize model"
toy.finalize()
print "Value of 'state'"
print toy.get_value ('state')
