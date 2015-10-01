namespace java org.openda.model_bmi.thrift
namespace py openda.bmi.thrift


/*
 * BMI thrift file. Specifically written to match the python BMI interface 
 * (https://github.com/csdms/bmi/blob/master/bindings/python/bmi/BMI.py)
 * from the csdms project (https://github.com/csdms/bmi)
 *
 * 2015-03: Updated to bmi.py as used in OpenDA project
 * 2015-07 Added a few more extension functions
 */

enum BmiGridType {
    UNKNOWN = 0,
    UNIFORM = 1,
    RECTILINEAR = 2,
    STRUCTURED = 3,
    UNSTRUCTURED = 4
}

exception ModelException {

    1: required string message
    
}

service BMIService {
	
    void initialize(1:string file) throws (1:ModelException error);
    
    void update() throws (1:ModelException error);
    
    void update_until(1:double time) throws (1:ModelException error);
    
    void update_frac(1:double frac) throws (1:ModelException error);
    
    void finalize_model() throws (1:ModelException error);
    
    string get_component_name() throws (1:ModelException error);
    
    list<string> get_input_var_names();
    
    list<string> get_output_var_names();

    string get_var_type (1:string long_var_name);

    string get_var_units (1:string long_var_name);

    i32 get_var_rank (1:string long_var_name);
    
    i32 get_var_size (1:string long_var_name);
    
    i32 get_var_nbytes (1:string long_var_name);

    double get_start_time();
    
    double get_current_time();

    double get_end_time();
    
    double get_time_step();
    
    string get_time_units();
    
    binary get_value(1:string long_var_name) throws (1:ModelException error);

    binary get_value_at_indices(1:string long_var_name, 2:list<i32> inds) throws (1:ModelException error);
 
    void set_value (1:string long_var_name, 2:binary src) throws (1:ModelException error);
    
    void set_value_at_indices (1:string long_var_name, 2:list<i32> inds, 3:binary src) throws (1:ModelException error);
    
    BmiGridType get_grid_type(1:string long_var_name) throws (1:ModelException error);

    list<i32> get_grid_shape(1:string long_var_name);
    
    list<double> get_grid_spacing(1:string long_var_name);
    
    list<double> get_grid_origin(1:string long_var_name);
    
    list<double> get_grid_x(1:string long_var_name);
    
    list<double> get_grid_y(1:string long_var_name);
    
    list<double> get_grid_z(1:string long_var_name);
    
    list<i32> get_grid_connectivity(1:string long_var_name);

    list<i32> get_grid_offset(1:string long_var_name);
    
    //extensions of BMI used in OpenDA && eWaterCycle
    
    void initialize_config(1:string file) throws (1:ModelException error);
    
    void initialize_model() throws (1:ModelException error);

    #only possible before initialize
    void set_start_time(1:double start_time) throws (1:ModelException error);

    #only possible before initialize
    void set_end_time(1:double end_time) throws (1:ModelException error);
    
    list<string> get_attribute_names();

    string get_attribute_value(1:string attribute_name) throws (1:ModelException error);

    void set_attribute_value(1:string attribute_name, 2:string attribute_value) throws (1:ModelException error);

    void save_state(1:string destination_directory) throws (1:ModelException error);
    void load_state(1:string source_directory) throws (1:ModelException error);

}
