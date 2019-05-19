from OpendaCalibrationLib import *


class OpendaCalibration:

    def __init__(self, working_directory):
        self.working_directory = working_directory
        self.opendaCalibration = OpendaCalibrationLib(working_directory)

    def get_algorithm_names(self):
        """
        returns: list containing the names of the available algorithms
        """
        return self.opendaCalibration.get_algorithm_names()

    def get_algorithm_setting_names(self, algorithm_name):
        """
        returns: the settings for the specified algorithm
        """
        return self.opendaCalibration.get_algorithm_setting_names(algorithm_name)

    def get_algorithm_setting_value(self, algorithm_name, setting_name):
        """
        returns: the settings for the specified algorithm
        """
        return self.opendaCalibration.get_algorithm_setting_value(algorithm_name, setting_name)

    def get_algorithm_setting_default(self, algorithm_name, setting_name):
        """
        returns: the settings for the specified algorithm
        """
        return self.opendaCalibration.get_algorithm_setting_default(algorithm_name, setting_name)

    def set_algorithm_setting_value(self, algorithm_name, setting_name, value):
        """
        returns: the settings for the specified algorithm
        """
        return self.opendaCalibration.set_algorithm_setting_value(algorithm_name, setting_name, value)

    def calibrate(self, evaluate, obs_values, obs_stddevs, model_params, model_params_stddevs):

        self.opendaCalibration.observer_set_observations_and_stddevs(obs_values, obs_stddevs)
        self.opendaCalibration.model_set_parameter_definitions(model_params, model_params_stddevs)

        next_parameter_values = self.opendaCalibration.algorithm_get_next_parameter_values()
        while (next_parameter_values != None):
            model_results = evaluate(next_parameter_values)
            self.opendaCalibration.model_set_results(model_results)
            next_parameter_values = self.opendaCalibration.algorithm_get_next_parameter_values()

        res = self.opendaCalibration.algorithm_get_optimal_parameter_values()
        return res


if __name__ == "__main__":

    from math import sin

    def evaluate_sinus(sinus_params):
        print(sinus_params)
        amplitude = sinus_params[0]
        period = sinus_params[1]
        phase = sinus_params[2]
        offset = sinus_params[3]
        values = [0.0] * 100
        for i in range(0, 99):
            values[i] = amplitude * sin(phase + 1.0 * i / period) + offset
        return values


    working_directory = "."

    obs_sinus_params = [2.0, 0.5, 0.0, 0.0]  # amplitude, period, phase, offset
    obs_values = evaluate_sinus(obs_sinus_params)
    obs_std_devs = [0.2] * len(obs_values)

    model_sinus_params = [2.1, 0.55, 0.1, 0.1]  # amplitude, period, phase and offset
    model_sinus_params_stddevs = [0.2, 0.05, 0.02, 0.02]

    # calibrate
    calibration = OpendaCalibration(".")
    optimal_params = calibration.calibrate(evaluate_sinus, obs_values, obs_std_devs, model_sinus_params,
                                           model_sinus_params_stddevs)
