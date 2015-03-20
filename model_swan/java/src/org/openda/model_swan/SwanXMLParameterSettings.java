/* MOD_V2.0
* Copyright (c) 2012 OpenDA Association
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

package org.openda.model_swan;

import org.openda.model_swan.SwanParameters;
import org.openda.utils.io.CastorUtils;
import org.openda.model_swan.io.castorgenerated.swivt.*;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * SWIVT paramters settings for a swan computation
 */
public class SwanXMLParameterSettings {

    private Root swivtCaseRoot;
    private WCAP wcapXML = null;
    private WCAP1 wcap1XML;
    private QUAD quadXML = null;
    private FRIC fricXML = null;
    private BREA breaXML = null;
    private BREA1 brea1XML = null;
    private TRIAD triadXML = null;
    private NUMREFRL numrefrlXML = null;
    private NUM numXML = null;

    private HashMap<String, HashMap<String, String>> groups = new HashMap<String, HashMap<String, String>>();

    public SwanXMLParameterSettings(File parameterFile) {

        swivtCaseRoot = (Root) CastorUtils.parse(parameterFile, Root.class);

        SettingsForSwan userSettings = swivtCaseRoot.getSwivt().getCase().getSettings().getPhysics().getUser();
        User userNumerics = swivtCaseRoot.getSwivt().getCase().getSettings().getNumerics().getUser();

        GEN3 gen3XML = userSettings.getGEN3();
        wcapXML = userSettings.getWCAP();
        wcap1XML = userSettings.getWCAP1();
        quadXML = userSettings.getQUAD();
        fricXML = userSettings.getFRIC();
        breaXML = userSettings.getBREA();
        brea1XML = userSettings.getBREA1();
        triadXML = userSettings.getTRIAD();
        numrefrlXML = userSettings.getNUMREFRL();
        numXML = userNumerics.getNUM();

        boolean wcapSelected = wcapXML != null && wcapXML.getSelected().toString().equalsIgnoreCase("on");
        boolean wcap1Selected = wcap1XML != null && wcap1XML.getSelected().toString().equalsIgnoreCase("on");
        boolean quadSelected = quadXML != null && quadXML.getSelected().toString().equalsIgnoreCase("on");
        boolean fricSelected = fricXML != null && fricXML.getSelected().toString().equalsIgnoreCase("on");
        boolean breaSelected = breaXML != null && breaXML.getSelected().toString().equalsIgnoreCase("on");
        boolean brea1Selected = brea1XML != null && brea1XML.getSelected().toString().equalsIgnoreCase("on");
        boolean triadSelected = triadXML != null && triadXML.getSelected().toString().equalsIgnoreCase("on");
        boolean numrefrlSelected = numrefrlXML != null && numrefrlXML.getSelected().toString().equalsIgnoreCase("on");
        boolean numSelected = numXML != null && numXML.getSelected().toString().equalsIgnoreCase("on");

        HashMap<String, String> gen3  = new HashMap<String, String>();
        gen3.put("model", gen3XML.getModel());
        groups.put("GEN3", gen3);

        if (wcapSelected) {
            HashMap<String, String> wcap  = new HashMap<String, String>();
            wcap.put("cds2", wcapXML.getCds2());
            wcap.put("stpm", wcapXML.getStpm());
            wcap.put("powst", String.valueOf(wcapXML.getPowst()));
            wcap.put("delta", String.valueOf(wcapXML.getDelta()));
            wcap.put("powk", String.valueOf(wcapXML.getPowk()));
            groups.put("WCAP", wcap);
        }
        if (wcap1Selected) {
            HashMap<String, String> wcap1  = new HashMap<String, String>();
            wcap1.put("cds2", wcap1XML.getCds2());
            wcap1.put("br", wcap1XML.getBr());
            wcap1.put("p0", wcap1XML.getP0());
            wcap1.put("powst", wcap1XML.getPowst());
            wcap1.put("powk", wcap1XML.getPowk());
            wcap1.put("nldisp", String.valueOf(wcap1XML.getNldisp()));
            wcap1.put("cds3", wcap1XML.getCds3());
            wcap1.put("powfsh", wcap1XML.getPowfsh());
            groups.put("WCAP1", wcap1);
        }
        if (quadSelected) {
            HashMap<String, String> quad = new HashMap<String, String>();
            quad.put("iquad", String.valueOf(quadXML.getIquad()));
            quad.put("lambda", quadXML.getLambda());
            quad.put("Cnl4", String.valueOf(quadXML.getCnl4()));
            quad.put("ursell", String.valueOf(quadXML.getUrsell()));
            quad.put("qb", String.valueOf(quadXML.getQb()));
            groups.put("QUAD", quad);
        }
        if (fricSelected) {
            HashMap<String, String> fric = new HashMap<String, String>();
            fric.put("cfjon", fricXML.getCfjon());
            groups.put("FRIC", fric);
        }
        if (breaSelected) {
            HashMap<String, String> brea = new HashMap<String, String>();
            brea.put("alpha", String.valueOf(breaXML.getAlpha()));
            brea.put("gamma", breaXML.getGamma());
            groups.put("BREA", brea);
        }
        if (brea1Selected) {
            HashMap<String, String> brea1 = new HashMap<String, String>();
            brea1.put("alpha", String.valueOf(brea1XML.getAlpha()));
            brea1.put("pown", brea1XML.getPown());
            brea1.put("bref", brea1XML.getBref());
            brea1.put("shfac", brea1XML.getShfac());
            groups.put("BREA1", brea1);
        }
        if (triadSelected) {
            HashMap<String, String> triad = new HashMap<String, String>();
            triad.put("trfac", triadXML.getTrfac());
            triad.put("cutfr", triadXML.getCutfr());
            groups.put("TRIAD", triad);
        }
        if (numrefrlSelected) {
            HashMap<String, String> numrefrl = new HashMap<String, String>();
            numrefrl.put("frlim", numrefrlXML.getFrlim());
            numrefrl.put("power", numrefrlXML.getPower());
            groups.put("NUMREFRL", numrefrl);
        }
        if (numSelected) {
            HashMap<String, String> num = new HashMap<String, String>();
            num.put("dabs", numXML.getDabs());
            num.put("drel", numXML.getDrel());
            num.put("curvat", numXML.getCurvat());
            num.put("npnts", numXML.getNpnts());
            num.put("mxitst", String.valueOf(numXML.getMxitst()));
            groups.put("NUM", num);
        }
    }

    public void writeToFile(File parameterFile) {

        // note: GEN3 group and NUMON/NUMMOFF groups will not be manipulated by cal.instr.,
        //       so there is no need to restore them into the xml-tree.        
        for (String groupKey : SwanParameters.groupKeys) {
            HashMap<String, String> group = groups.get(groupKey);
            if (group != null) {
                for (String paramID : group.keySet()) {
                    String valueString = group.get(paramID);

                    // WCAP, fields for both WCAP and WCAP1
                    if (paramID.equals("cds2")) {
                        if (groupKey.equals("WCAP")) {
                            wcapXML.setCds2(valueString);
                        } else {
                            wcap1XML.setCds2(valueString);
                        }
                    } else if (paramID.equals("powst")) {
                        if (groupKey.equals("WCAP")) {
                            wcapXML.setPowst(parseIntValue(valueString));
                        } else {
                            wcap1XML.setPowst(valueString);
                        }
                    } else if (paramID.equals("powk")) {
                        if (groupKey.equals("WCAP")) {
                            wcapXML.setPowk(parseIntValue(valueString));
                        } else {
                            wcap1XML.setPowk(valueString);
                        }
                    }
                    // WCAP, fields for WCAP
                    else if (paramID.equals("stpm")) wcapXML.setStpm(valueString);
                    else if (paramID.equals("delta")) wcapXML.setDelta(parseIntValue(valueString));
                    // WCAP, fields for WCAP1
                    else if (paramID.equals("br")) wcap1XML.setBr(valueString);
                    else if (paramID.equals("p0")) wcap1XML.setP0(valueString);
                    else if (paramID.equals("nldisp")) wcap1XML.setNldisp(parseIntValue(valueString));
                    else if (paramID.equals("cds3")) wcap1XML.setCds3(valueString);
                    else if (paramID.equals("powfsh")) wcap1XML.setPowfsh(valueString);

                    // QUAD
                    else if (paramID.equals("iquad")) quadXML.setIquad(parseIntValue(valueString));
                    else if (paramID.equals("lambda")) quadXML.setLambda(valueString);
                    else if (paramID.equals("Cnl4")) quadXML.setCnl4(parseIntValue(valueString));
                    else if (paramID.equals("ursell")) quadXML.setUrsell(parseIntValue(valueString));
                    else if (paramID.equals("qb")) quadXML.setQb(parseIntValue(valueString));

                    // FRIC
                    else if (paramID.equals("cfjon")) fricXML.setCfjon(valueString);

                    // BREA, fields for both BREA and BREA1
                    else if (paramID.equals("alpha")) {
                        if (groupKey.equals("BREA")) {
                            breaXML.setAlpha(parseIntValue(valueString));
                        } else {
                            brea1XML.setAlpha(valueString);
                        }
                    }
                    // BREA, fields for both BREA
                    else if (paramID.equals("gamma")) breaXML.setGamma(valueString);
                    // BREA, fields for BREA1
                    else if (paramID.equals("pown")) brea1XML.setPown(valueString);
                    else if (paramID.equals("bref")) brea1XML.setBref(valueString);
                    else if (paramID.equals("shfac")) brea1XML.setShfac(valueString);

                    // TRIAD
                    else if (paramID.equals("trfac")) triadXML.setTrfac(valueString);
                    else if (paramID.equals("cutfr")) triadXML.setCutfr(valueString);

                    // NUMREFL
                    else if (paramID.equals("frlim")) numrefrlXML.setFrlim(valueString);
                    else if (paramID.equals("power")) numrefrlXML.setPower(valueString);

                    // NUM
                    else if (paramID.equals("dabs")) numXML.setDabs(valueString);
                    else if (paramID.equals("drel")) numXML.setDrel(valueString);
                    else if (paramID.equals("curvat")) numXML.setCurvat(valueString);
                    else if (paramID.equals("npnts")) numXML.setNpnts(valueString);
                    else if (paramID.equals("mxitst")) numXML.setMxitst(parseIntValue(valueString));
                }
            }
        }
        CastorUtils.write(swivtCaseRoot, parameterFile, "Root", null, "http://swivt.deltares.nl/swivt_v1_4.xsd");
    }

    private int parseIntValue(String valueString) {
        double value = Double.parseDouble(valueString);
        return (int) Math.round(value);
    }

    public ArrayList<String> getIds() {
        ArrayList<String> ids = new ArrayList<String>();
        for (String groupKey : SwanParameters.groupKeys) {
            HashMap<String, String> group = groups.get(groupKey);
            if (group != null) {
                for (String paramID : group.keySet()) {
                    ids.add(composeId(groupKey, paramID));
                }
            }
        }
        return ids;
    }

    public Object getValue(String id) {
        String groupKey = parseGroupFromID(id);
        String paramId = parseParamFromID(id);
        HashMap<String, String> group = groups.get(groupKey);
        if (group == null) {
            throw new RuntimeException("Invalid parameter group \"" + groupKey + "\" in getValue(" + id + ")");
        }
        String paramValue = group.get(paramId);
        if (paramValue == null) {
            throw new RuntimeException("Invalid parameter \"" + paramId + "\" in getValue(" + id + ")");
        }
        if (parameterIsInteger(paramId)) {
            return Integer.parseInt(paramValue);
        } if (parameterIsString(paramId)) {
            return paramValue;
        } else {
            return Double.parseDouble(paramValue);
        }
    }

    private boolean parameterIsString(String paramId) {
        return paramId.equals("model");
    }

    private boolean parameterIsInteger(String paramId) {
        return paramId.equals("iquad") || paramId.equals("mxitst");
    }

    public void setValue(String id, double value) {
        String groupKey = parseGroupFromID(id);
        String paramId = parseParamFromID(id);
        HashMap<String, String> group = groups.get(groupKey);
        if (group == null) {
            throw new RuntimeException("Invalid parameter group \"" + groupKey + "\" in setValue(" + id + ")");
        }
        String paramValue = group.get(paramId);
        if (paramValue == null) {
            throw new RuntimeException("Invalid parameter \"" + paramId + "\" in setValue(" + id + ")");
        }
        group.remove(paramId);
        group.put(paramId, String.valueOf(value));
    }

    public boolean getGroupActive(String groupKey) {
        return groups.containsKey(groupKey);
    }

    public static String parseGroupFromID(String id) {
        return id.substring(0, id.indexOf("."));
    }

    public static String parseParamFromID(String id) {
        return id.substring(id.indexOf(".")+1);
    }

    private static String composeId(String groupId, String paramId) {
        return groupId + "." + paramId;
    }
}
