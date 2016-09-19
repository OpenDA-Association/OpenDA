package org.openda.model_openfoam;

import junit.framework.TestCase;
import org.openda.exchange.iotools.DataCopier;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

/**
 * Created by werner on 25/03/16.
 */
public class ConversionDataObjectTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
        testData = new OpenDaTestSupport(ConversionDataObjectTest.class,"model_openfoam");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testCopyProbeToTextMain() {
        File inputFile = new File(testRunDataDir, "probes/0/p");
        File outputFile = new File(testRunDataDir, "conversion.txt");
        DataCopier.main(new String[]{"-c", "org.openda.model_openfoam.ProbeDataObject", "-a", "2015-12-01T00:00:00Z", inputFile.getAbsolutePath(),
            "-c", "org.openda.exchange.dataobjects.TestDataObject", outputFile.getAbsolutePath()});

    }

//    public void testCopyCsvTimeSeriesToProbe() {
//        File inputFile = new File(testRunDataDir, "CsvTimeSeries/SENSORID_QUANTITY.csv");
//        File outputFile = new File(testRunDataDir, "convertedToProbeFormat");
//        DataCopier.main(new String[]{"-c", "org.openda.model_openfoam.CsvTimeSeriesDataObject",  inputFile.getAbsolutePath(),
//            "-c", "org.openda.model_openfoam.ProbeDataObject", "-a", "2015-12-01T00:00:00Z", outputFile.getAbsolutePath()});
//
//    }



}
