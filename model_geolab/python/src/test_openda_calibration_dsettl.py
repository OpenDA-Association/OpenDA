import openda_calibration as op

def d_settlement(inpt):
    a = inpt[0]
    b = inpt[1]
    x = inpt[2]
    # execute D_settlement
    y = a * x + b
    return [y]


# call linear function
x_array = 1.
a_value = 1.
b_value = 0.
y = d_settlement([a_value, b_value, x_array])

# define variables
observed_values = y  # InSar
observed_std = [0.01]  # InSar
model_input_values = [a_value * 1.1, b_value * .9, x_array]  # DSettlement
model_input_std = [0.2, 0.2, 0.0001]
# calibrate
calibration = op.openda_calibration(".")
optimal_params = calibration.calibrate(d_settlement,
                                       observed_values, observed_std,
                                       model_input_values, model_input_std)
