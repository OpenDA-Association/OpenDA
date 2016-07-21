package org.openda.model_openfoam;

import junit.framework.TestCase;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

/**
 * Created by werner on 25/03/16.
 */
public class MeshDataObjectTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
        testData = new OpenDaTestSupport(MeshDataObjectTest.class,"model_openfoam");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testInitializeScalarFieldGZip() {
        IDataObject dataObject = new MeshDataObject();
        dataObject.initialize(testRunDataDir, new String[]{"results/0/T.gz"} );
    }

    public void testInitializeVectorFieldGzip() {
        IDataObject dataObject = new MeshDataObject();
        dataObject.initialize(testRunDataDir, new String[]{"results/OPENFOAM_TIME_DIR/U.gz"} );
    }

    public void testGetExchangeItemIDs() {
        IDataObject scalarObject = new MeshDataObject();
        scalarObject.initialize(testRunDataDir, new String[]{"results/OPENFOAM_TIME_DIR/T"} );
        String[] ids = scalarObject.getExchangeItemIDs();
        assertEquals("T", ids[0]);
        IDataObject vectorObject = new MeshDataObject();
        vectorObject.initialize(testRunDataDir, new String[]{"results/OPENFOAM_TIME_DIR/U"} );
        ids = vectorObject.getExchangeItemIDs();
		Arrays.sort(ids);
		assertEquals("U1", ids[0]);
        assertEquals("U2", ids[1]);
        assertEquals("U3", ids[2]);
    }

    public void testGetExchangeItemIDs1()  {
        IDataObject scalarObject = new MeshDataObject();
        scalarObject.initialize(testRunDataDir, new String[]{"results/OPENFOAM_TIME_DIR/T"} );
        String[] ids = scalarObject.getExchangeItemIDs(IPrevExchangeItem.Role.InOut);
        assertEquals("T", ids[0]);
        IDataObject vectorObject = new MeshDataObject();
        vectorObject.initialize(testRunDataDir, new String[]{"results/OPENFOAM_TIME_DIR/U"} );
        ids = vectorObject.getExchangeItemIDs(IPrevExchangeItem.Role.InOut);
        Arrays.sort(ids);
		assertEquals("U1", ids[0]);
        assertEquals("U2", ids[1]);
        assertEquals("U3", ids[2]);
    }

    public void testGetDataObjectExchangeItem() {
        IDataObject scalarObject = new MeshDataObject();
        scalarObject.initialize(testRunDataDir, new String[]{"results/OPENFOAM_TIME_DIR/T"} );
        IExchangeItem item = scalarObject.getDataObjectExchangeItem("T");
        assertEquals("T",item.getId());
    }

    public void testFinishEmpty() {
        IDataObject scalarObject = new MeshDataObject();
        scalarObject.initialize(testRunDataDir, new String[]{"output/T.gz", "T", "scalar"} );
        scalarObject.finish();

        IDataObject vectorObject = new MeshDataObject();
        vectorObject.initialize(testRunDataDir, new String[]{"output/U.gz", "U", "vector"} );
        vectorObject.finish();
    }

    public void testFinish() {
        IDataObject scalarObject = new MeshDataObject();
        scalarObject.initialize(testRunDataDir, new String[]{"results/OPENFOAM_TIME_DIR/T.gz"} );
        scalarObject.finish();

        IDataObject vectorObject = new MeshDataObject();
        vectorObject.initialize(testRunDataDir, new String[]{"results/OPENFOAM_TIME_DIR/U.gz"} );
        vectorObject.finish();
    }


}
