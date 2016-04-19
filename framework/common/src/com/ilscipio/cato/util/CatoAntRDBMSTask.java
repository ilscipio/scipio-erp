package com.ilscipio.cato.util;

import java.io.File;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

public class CatoAntRDBMSTask extends Task {

    private static final String FS_RDBMS_NOT_PRESENT = "notPresent";
    private static final String FS_RDBMS_PRESENT_WITH_DATA = "presentWithData";
    private static final String FS_RDBMS_PRESENT_WITH_NO_DATA = "presentWithNoData";

    private static final String DEFAULT_FS_RDBMS = "derby";

    @Override
    public void execute() throws BuildException {
        try {
            String ofbizHome = getProject().getProperty("ofbiz.home.dir");
            if (ofbizHome != null && !ofbizHome.equals("")) {
                String ofbizEntityEnginePath = ofbizHome + "/framework/entity/config/entityengine.xml";
                String ofbizDefaultRDBMSFileSystemPath = ofbizHome + "/runtime/data/derby";
                String result = FS_RDBMS_NOT_PRESENT;
                File ofbizEntityEngineFile = new File(ofbizEntityEnginePath);

                String datasource = DEFAULT_FS_RDBMS;
                boolean isFileSystemRDBMS = false;
                if (ofbizEntityEngineFile.exists() && ofbizEntityEngineFile.isFile() && ofbizEntityEngineFile.canRead()) {
                    Scanner scanner = new Scanner(ofbizEntityEngineFile);
                    boolean checkDatasources = false;
                    while (scanner.hasNextLine()) {
                        String defaultDelegatorBegin = scanner.findInLine("<delegator name=\"default\".*>");
                        String defaultDelegatorEnd = scanner.findInLine("</delegator>");
                        if (defaultDelegatorEnd != null)
                            break;
                        if (defaultDelegatorBegin != null)
                            checkDatasources = true;

                        String line = scanner.nextLine();
                        if (checkDatasources) {
                            String matchedDataSource = scanner.findInLine("datasource-name=\".*?" + DEFAULT_FS_RDBMS + "\"");
                            System.out.println("datasource ====> " + matchedDataSource);
                            Matcher matcher = Pattern.compile("datasource-name=\"(.*?)\"").matcher(line);
                            if (matcher.matches())
                                datasource = matcher.group(1);
                            System.out.println("datasource ====> " + datasource);
                            if (matchedDataSource != null) {
                                isFileSystemRDBMS = true;
                                break;
                            }

                        }
                    }
                    scanner.close();
                }
                if (isFileSystemRDBMS) {
                    File ofbizDefaultFileSystemRDBMSFile = new File(ofbizDefaultRDBMSFileSystemPath);
                    if (!ofbizDefaultFileSystemRDBMSFile.exists())
                        result = FS_RDBMS_PRESENT_WITH_NO_DATA;
                    else
                        result = FS_RDBMS_PRESENT_WITH_DATA;
                }
                getProject().setNewProperty("fs.rdbms.result", result);
                getProject().setNewProperty("fs.rdbms.name", datasource);
            }

        } catch (Exception e) {
            System.err.println(e);
        }
        super.execute();
    }

}
