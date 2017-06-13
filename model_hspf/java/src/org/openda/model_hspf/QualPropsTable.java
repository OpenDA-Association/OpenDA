/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
* All rights reserved.
* 
* This file is part of OpenDA. 
* 
* OpenDA is free software: you can redistribute it and/or modify 
* it under the terms of the GNU Lesser General Public License as 
* published by the Free Software Foundation, either version 3 of 
* the License, or (at your option) any later version. 
* 
* OpenDA is distributed in the hope that it will be useful, 
* but WITHOUT ANY WARRANTY; without even the implied warranty of 
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
* GNU Lesser General Public License for more details. 
* 
* You should have received a copy of the GNU Lesser General Public License
* along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
*/

package org.openda.model_hspf;

import java.util.*;

/**
 * Class to read a PERLND or IMPLND QUAL-PROPS table that specifies the QUALIDs for the next QUAL-INPUT table from a UCI state file of a HSPF model.
 *
 * The HSPF model can be installed as part of the BASINS package, which is available from:
 * http://water.epa.gov/scitech/datait/models/basins/index.cfm
 *
 * @author Arno Kockx
 */
public class QualPropsTable {
	public static final String TABLE_NAME = "QUAL-PROPS";

	public static boolean isQualPropsTable(String tableName) {
		return TABLE_NAME.equals(tableName);
	}

	/**
	 * If given outputLines != null, then also copies qualProps table to output unchanged.
	 */
	public static String readQualId(String moduleName, Iterator<String> inputLines, List<String> outputLines) {
		Map<Integer, String> qualIds = new HashMap<>();

		//read uci file.
		while (inputLines.hasNext()) {
			String inputLine = inputLines.next();
			if (outputLines != null) {
				//copy inputLine to output unchanged.
				outputLines.add(inputLine);
			}
			if (inputLine.trim().toUpperCase().startsWith("END")) break;

			String[] columns = UciUtils.splitAfter10And22Characters(inputLine);
			if (columns == null || columns.length < 2) {//if empty row.
				//skip row.
				continue;
			}
			String firstColumn = columns[0];
			String secondColumn = columns[1];

			//if contains at least one digit.
			if (firstColumn.matches(".*\\d.*")) {//if values row.
				int firstSegmentNumber = UciUtils.readFirstLocationNumber(moduleName, TABLE_NAME, firstColumn);
				int lastSegmentNumber = UciUtils.readLastLocationNumber(moduleName, TABLE_NAME, firstColumn, firstSegmentNumber);

				String qualId = secondColumn.trim();
				if (qualId.isEmpty()) throw new IllegalArgumentException("Empty qualId found in " + moduleName + " table '" + TABLE_NAME + "' in uci state file.");
				qualId = mapQualId(qualId);
				for (int segmentNumber = firstSegmentNumber; segmentNumber <= lastSegmentNumber; segmentNumber++) {
					qualIds.put(segmentNumber, qualId);
				}

				continue;
			}

			//do nothing, skip row.
		}
		if (qualIds.isEmpty()) throw new IllegalArgumentException(moduleName + " table '" + TABLE_NAME + "' contains no valid values for QUALID in uci state file.");

		//Note: this code only supports QUAL-PROPS tables that contain the same qualId for all segments, otherwise multiple QUAL-INPUT tables cannot be distinguished when writing.
		HashSet<String> uniqueQualIds = new HashSet<>(qualIds.values());
		if (uniqueQualIds.size() > 1) {
			throw new IllegalArgumentException("Multiple qualIds present in " + moduleName + " table '" + QualPropsTable.TABLE_NAME + "' found in uci state file. Only supports "
					+ QualPropsTable.TABLE_NAME + " tables that contain the same qualId for all segments.");
		}
		assert uniqueQualIds.size() == 1;
		return uniqueQualIds.iterator().next();
	}

	private static String mapQualId(String qualId) {
		//TAM = Total AMmonia nitrogen = NH3 + NH4 (use TAM instead of TAN for consistency with a.o. TAM in NUT-DINIT table).
		if ("NH3+NH4".equals(qualId)) return "TAM";

		//PO4 is the same as ortho-phosphorous.
		if ("ORTHO P".equals(qualId)) return "PO4";

		//return unmapped qualId.
		return qualId;
	}
}
