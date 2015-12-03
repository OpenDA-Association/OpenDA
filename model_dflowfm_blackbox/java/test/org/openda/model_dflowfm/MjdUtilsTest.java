package org.openda.model_dflowfm;

import junit.framework.TestCase;
import org.springframework.util.Assert;

import java.text.SimpleDateFormat;
import java.util.*;

/**
 * Created by prevel on 02-Dec-15.
 */
public class MjdUtilsTest extends TestCase
{
	protected void setUp(){}
	protected void tearDown(){}

	public void testConvertDateTimesWithUnitToModifiedJulianDays_ThrowsExceptionForInvalidDate()
	{
		List<Double> values = new ArrayList<>();
		values.add(0.0);
		values.add(15.0);
		values.add(30.0);
		values.add(45.0);
		values.add(60.0);

		Exception expectedException = null;
		try	{ MjdUtils.ConvertDateTimesWithUnitToModifiedJulianDays("minutes since 2015-11-20 AA:BB:CC", values); }
		catch(Exception ex)	{ expectedException = ex; }
		Assert.notNull(expectedException);

		expectedException = null;
		try	{ MjdUtils.ConvertDateTimesWithUnitToModifiedJulianDays("some other unit 2015-12-03 00:00:00", values); }
		catch(Exception ex)	{ expectedException = ex; }
		Assert.notNull(expectedException);
	}

	public void testConvertModifiedJulianDaysToDateTimesWithUnit_ThrowsExceptionForInvalidDate()
	{
		Exception expectedException = null;
		try	{ MjdUtils.ConvertModifiedJulianDaysToDateTimesWithUnit("minutes since 2015-12-03 AA:BB:CC", null);	}
		catch(Exception ex)	{ expectedException = ex; }
		Assert.notNull(expectedException);

		expectedException = null;
		try	{ MjdUtils.ConvertModifiedJulianDaysToDateTimesWithUnit("some other unit 2015-12-03 00:00:00", null);	}
		catch(Exception ex)	{ expectedException = ex; }
		Assert.notNull(expectedException);
	}

	public void testMjdUtils_SimpleModifiedJulianDayConversionWorks()
	{
		Calendar dateTime = new GregorianCalendar();

		try	{ dateTime.setTime(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse("2015-12-03 12:00:00")); }
		catch(Exception ex)	{ /* swallow exception for tests */ }

		double mjdValue = MjdUtils.ConvertDateTimeToModifiedJulianDay(dateTime);
		Assert.isTrue(Double.compare(57359.5, mjdValue) == 0);

		Calendar retrievedDateTime = MjdUtils.ConvertModifiedJulianDayToDateTime(mjdValue);
		Assert.isTrue(retrievedDateTime.getTimeInMillis() == dateTime.getTimeInMillis());
	}

	public void testMjdUtils_DateTimeModifiedJulianDayConversionWorks()
	{
		List<Double> values = new ArrayList<>();
		values.add(0.0);
		values.add(15.0);
		values.add(30.0);
		values.add(45.0);
		values.add(60.0);

		List<Double> modifiedValues = MjdUtils.ConvertDateTimesWithUnitToModifiedJulianDays("minutes since 2015-11-20 00:00:00", values);

		Assert.isTrue(Double.compare(modifiedValues.get(0), 57346) == 0);
		Assert.isTrue(Double.compare(modifiedValues.get(1), 57346.010416666664) == 0);
		Assert.isTrue(Double.compare(modifiedValues.get(2), 57346.020833333336) == 0);
		Assert.isTrue(Double.compare(modifiedValues.get(3), 57346.03125) == 0);
		Assert.isTrue(Double.compare(modifiedValues.get(4), 57346.041666666664) == 0);

		List<Double> retrievedValues = MjdUtils.ConvertModifiedJulianDaysToDateTimesWithUnit("minutes since 2015-11-20 00:00:00", modifiedValues);

		for(int i = 0; i < values.size(); i++)
		{
			Assert.isTrue(Double.compare(values.get(i), retrievedValues.get(i)) == 0);
		}
	}
}
