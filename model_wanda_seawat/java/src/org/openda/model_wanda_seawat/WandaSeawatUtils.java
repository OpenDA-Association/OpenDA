package org.openda.model_wanda_seawat;

import org.openda.exchange.TimeInfo;
import org.openda.exchange.timeseries.TimeUtils;

import java.io.File;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;
import java.util.TreeMap;

public class WandaSeawatUtils {
	static TimeInfo getTimeInfo(String filePrefix, TreeMap<String, File> fileNamesToFiles) {
		try {
			SimpleDateFormat simpleDateFormat = new SimpleDateFormat('\'' + filePrefix + "'yyyyMMddHHmmss", Locale.US);
			simpleDateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
			double[] timesArray = new double[fileNamesToFiles.size()];
			String[] strings = fileNamesToFiles.keySet().toArray(new String[0]);
			for (int i = 0; i < strings.length; i++) {
				String fileName = strings[i];
				Date dateFromFileName = simpleDateFormat.parse(fileName);
				timesArray[i] = TimeUtils.date2Mjd(dateFromFileName);
			}
			return new TimeInfo(timesArray);
		} catch (ParseException e) {
			throw new RuntimeException(e);
		}
	}
}
