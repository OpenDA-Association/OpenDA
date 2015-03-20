
This test is not about the contents of the runs, but about generating useful output.
You can add one or more resultWriters to catch the output of an algorithm. It also
works for a model or observer if it supports the OpenDA resultWriter mechanism.
In the main OpenDa input file (*.oda) you can specify what data to collect. The class
used for the resultWriter determines the file format.

eg.

	<resultWriters>
		<!-- dump everyting to a matlab readable ascii file -->
		<resultWriter className="org.openda.resultwriters.MatlabResultWriter">
			<workingDirectory>.</workingDirectory>
			<configFile>results_dud.m</configFile>
		</resultWriter>
		<!-- write calibration summary table as ascii table -->
		<resultWriter className="org.openda.resultwriters.TextTableWriter">
			<workingDirectory>.</workingDirectory>
			<configFile>results_dud.csv</configFile>
		</resultWriter>
		<!-- dump everything as netcdf -->
		<resultWriter className="org.openda.resultwriters.NetcdfResultWriter">
			<workingDirectory>.</workingDirectory>
			<configFile>results_dud_.nc</configFile>
		</resultWriter>
	</resultWriters>


Selections can be added to limit the output for a resultWriter.

eg. 

	<resultWriters>
		<resultWriter className="org.openda.resultwriters.MatlabResultWriter">
			<workingDirectory>.</workingDirectory>
			<configFile>results_dud_algorithm.m</configFile>
			<selection>
				<!-- only algorithm items-->
				<doLog algorithm="true" model="false" observer="false" nonConfiguredItems="false" />
			</selection>
		</resultWriter>
		<resultWriter className="org.openda.resultwriters.MatlabResultWriter">
			<workingDirectory>.</workingDirectory>
			<configFile>results_dud_model.m</configFile>
			<selection>
				<!-- only model items-->
				<doLog algorithm="false" model="true" observer="false" nonConfiguredItems="false" />
			</selection>
		</resultWriter>
		<resultWriter className="org.openda.resultwriters.MatlabResultWriter">
			<workingDirectory>.</workingDirectory>
			<configFile>results_dud_observer.m</configFile>
			<selection>
				<!-- only observer items-->
				<doLog algorithm="false" model="false" observer="true" nonConfiguredItems="false" />
			</selection>
		</resultWriter>
		<resultWriter className="org.openda.resultwriters.MatlabResultWriter">
			<workingDirectory>.</workingDirectory>
			<configFile>results_dud_other.m</configFile>
			<selection>
				<!-- only other items-->
				<doLog algorithm="false" model="false" observer="false" nonConfiguredItems="true" />
			</selection>
		</resultWriter>
		<resultWriter className="org.openda.resultwriters.MatlabResultWriter">
			<workingDirectory>.</workingDirectory>
			<configFile>results_dud_costTotal_only.m</configFile>
			<selection>
				<doLog algorithm="false" model="false" observer="false" nonConfiguredItems="false" />
				<resultItem id="costTotal" doLog="true" />
			</selection>
		</resultWriter>
	</resultWriters>

