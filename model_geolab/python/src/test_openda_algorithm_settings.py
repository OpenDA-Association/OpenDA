import openda_calibration as oda_cal

calibration = oda_cal.OpendaCalibration(".")
algorithm_names = calibration.get_algorithm_names()
print(algorithm_names)
algorithm_name = algorithm_names[0]
settings = calibration.get_algorithm_setting_names(algorithm_name)
print(settings)
setting = settings[5]
print(setting)
setting_value = calibration.get_algorithm_setting_value(algorithm_name, setting)
setting_default = calibration.get_algorithm_setting_default(algorithm_name, setting)
print(setting_value)
print(setting_default)
calibration.set_algorithm_setting_value(algorithm_name, setting, 7.8)
setting_value = calibration.get_algorithm_setting_value(algorithm_name, setting)
setting_default = calibration.get_algorithm_setting_default(algorithm_name, setting)
print(setting_value)
print(setting_default)
