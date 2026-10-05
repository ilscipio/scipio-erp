/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
/**
 * SCIPIO: data export script.
 * based on component://webtools/webapp/webtools/WEB-INF/actions/entity/XmlDsDump.groovy
 */

import java.io.*;
import org.ofbiz.base.util.*;
import org.ofbiz.entity.model.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.entity.transaction.*;
import org.ofbiz.entity.condition.*;
import com.ilscipio.scipio.cms.data.CmsEntityInfo;
import com.ilscipio.scipio.cms.data.importexport.CmsDataExportWorker;
import com.ilscipio.scipio.cms.data.importexport.CmsDataExportWorker.GenericWorkerArgs;
import com.ilscipio.scipio.cms.data.importexport.CmsDataExportWorker.RecordGrouping
import com.ilscipio.scipio.cms.data.importexport.CmsDataExportWorker.OutputMode

final module = "CmsDataExport.groovy";
final LOG_PREFIX = CmsDataExportWorker.LOG_PREFIX;
cmsErrorHandler = context.cmsErrorHandler;
locale = context.locale;

// SCIPIO: TODO: we should move much of this code to the cmsExportData service
// so it can be reused more easily (the Worker is already reusable but it's easier for Ofbiz users to reuse services)
// NOTE: for the JSP will have to pass executeExport=false to cmsExportDataAsXml and get the dataExportWorker

// NOTE: See cmsExportData* service interfaces for descriptions of worker options

cmsEntityInfo = CmsEntityInfo.getInst(delegator);
context.cmsEntityBasePkg = cmsEntityInfo.getCmsEntityBasePkg();
context.cmsEntityBasePkgPrefix = cmsEntityInfo.getCmsEntityBasePkgPrefix();
// 2017-08-29: here we get the combined "real" entity names and the "fake/virtual" ones (like CmsMedia) -
// the worker will automatically split up passedEntityNames again after.
context.entityNamesByPkg = cmsEntityInfo.getCombinedCmsEntityNamesByPkg();
context.majorEntityNames = cmsEntityInfo.getCombinedMajorCmsEntityNames();
doExport = "Y".equals(context.doExport ?: parameters.doExport);

getPropMsg = { resource, name, args=null -> return UtilProperties.getMessage(resource, name, (Map) args, locale); };
addSelOpt = { opts, value, description=null ->
    if (opts == null) opts = [];
    opts.add(["value":value, "description":description?:value]);
    return opts;  
};
limitOptVal = { value, opts ->
    if (value) value = value.toString();
    for(opt in opts) {
        if (value == opt.value) return value;
    }
    return null;
};

// SCIPIO: 2017-05-31: SPECIAL: we now clear this session attribute
// whenever we reload the export options page instead of when visiting the target JSP.
// This is because it's extremely easy to accidentally mis-click due to browsers and have
// to start over. So instead we just clear it whenever we reload the options page.
session.removeAttribute("cmsDataExportWorker");

preConfiguredSetName = parameters.preConfiguredSetName;
context.preConfiguredSetName = preConfiguredSetName;
entityPresetMap = CmsDataExportWorker.EntityPresetMap.getInst(delegator);
context.entityPresetMap = entityPresetMap;
context.entityAllPresetNames = entityPresetMap.getAllPresetNames();
context.entitySimplePresetNames = entityPresetMap.getSimplePresetNames();

outpath = parameters.outpath;
filename = parameters.filename;
maxRecStr = parameters.maxrecords;
passedEntityNames = null;
if (parameters.entityName) passedEntityNames = (parameters.entityName instanceof Collection) ? parameters.entityName as TreeSet : [parameters.entityName] as TreeSet;
maxRecordsPerFile = 0;

outputModeList = OutputMode.getDisplayValues(security, context.userLogin);
context.outputModeList = outputModeList;
defOutputMode = OutputMode.fromStringSafe(context.defOutputMode) ?: OutputMode.getDefault();
context.defOutputMode = defOutputMode;
outputMode = OutputMode.fromStringSafe(parameters.outputMode ? parameters.outputMode.toString() : null);
if (outputMode != null) {
    try {
        outputMode.checkAllowed(security, context.userLogin);
    } catch (Exception e) {
        cmsErrorHandler.addContextError(context, e.getMessage());
        doExport = false;
        outputMode = null;
    }
}
context.outputMode = outputMode;
selOutputMode = outputMode ?: defOutputMode;
context.selOutputMode = selOutputMode;

entityDateCond = null;
entityCondMap = [:];

// SCIPIO: new params
exportFilesAsTextData = "Y".equals(parameters.exportFilesAsTextData);
singleFileRecGrp = parameters.singleFileRecGrp ? parameters.singleFileRecGrp.toString() : null;
includeMajorDeps = "Y".equals(parameters.includeMajorDeps);

// NOTE: the enums/conditions from the worker may have problems, see the methods
pmpsMappingTypeEnums = CmsDataExportWorker.getPageSpecialMappingCondEnumerationsSafe(delegator);
pmpsMappingTypeOpts = [];
for(value in pmpsMappingTypeEnums) {
    addSelOpt(pmpsMappingTypeOpts, value.enumId, value.get("description", locale));
}
context.pmpsMappingTypeOpts = pmpsMappingTypeOpts;
pmpsMappingTypeId = parameters.pmpsMappingTypeId ? limitOptVal(parameters.pmpsMappingTypeId, pmpsMappingTypeOpts) : null;
context.pmpsMappingTypeId = pmpsMappingTypeId;

includeContentRefsOpts = addSelOpt([], "NONE", getPropMsg("CommonUiLabels", "CommonNone"));
includeContentRefsOpts = addSelOpt(includeContentRefsOpts, "ALL", getPropMsg("CommonUiLabels", "CommonAll"));
context.includeContentRefsOpts = includeContentRefsOpts;
includeContentRefs = parameters.includeContentRefs ? limitOptVal(parameters.includeContentRefs, includeContentRefsOpts) : "ALL";
context.includeContentRefs = includeContentRefs;

attribTmplAssocTypeOpts = addSelOpt([], "PAGE_TEMPLATE", "Page Template");
attribTmplAssocTypeOpts = addSelOpt(attribTmplAssocTypeOpts, "ASSET_TEMPLATE", "Asset Template");
context.attribTmplAssocTypeOpts = attribTmplAssocTypeOpts;
attribTmplAssocType = parameters.attribTmplAssocType ? limitOptVal(parameters.attribTmplAssocType, attribTmplAssocTypeOpts) : null;
context.attribTmplAssocType = attribTmplAssocType;

try {
    if (maxRecStr) {
        maxRecordsPerFile = Integer.parseInt(maxRecStr);
    }
} catch(Exception e) {
    cmsErrorHandler.addContextError(context, getPropMsg("CMSErrorUiLabels", "CmsInvalidParameter",
        ["paramName":"Max Records per File"]) + ": " + e.getMessage());
    doExport = false;
}
try {
    entityDateCond = CmsDataExportWorker.makeEntityDateCond(delegator, parameters.entityFrom, parameters.entityThru);
} catch(Exception e) {
    cmsErrorHandler.addContextError(context, getPropMsg("CMSErrorUiLabels", "CmsInvalidParameter",
        ["paramName":"Entity Date"]) + ": " + e.getMessage());
    doExport = false;
}

if (doExport && !passedEntityNames) {
    cmsErrorHandler.addContextErrors(context, getPropMsg("CMSUiLabels", "CmsNoEntityNamesSpecified"));
    doExport = false;
} else if (doExport && outputMode) {
    // SCIPIO: NOTE: the JSP will now receive the same EFO as the screen
    // NOTE: the presetConfig is redundant here, because we apply it via JS and receive all the parameters again,
    // but it _shouldn't_ do harm... if something goes wrong, try removing the presetConfig arg from this call...
    GenericWorkerArgs workerArgs = new GenericWorkerArgs(delegator).applyPreset((String) preConfiguredSetName);
    workerArgs.setTargetEntityNames(passedEntityNames).setEntityDateCond(entityDateCond)
        .setEntityCondMap(entityCondMap).setIncludeContentRefs(!"NONE".equals(includeContentRefs))
        .setPmpsMappingTypeId(pmpsMappingTypeId).setAttribTmplAssocType(attribTmplAssocType).setIncludeMajorDeps(includeMajorDeps)
        .setExportFilesAsTextData(exportFilesAsTextData)
        .setCommonEfo().setNewTrans(true); // SCIPIO: NOTE: we must set newTrans otherwise rollbacks halt the screen render
    if (outputMode.isSingle()) {
        workerArgs.setRecordGrouping(singleFileRecGrp);
    }
    
    if (outputMode == OutputMode.SF_DL) { // download, with intermediate page, worker stored in session
        try {
            CmsDataExportWorker worker = CmsDataExportWorker.makeSingleFileWorker(workerArgs);
            
            // SAVE WORKER TO SESSION - picked up by CmsDataExportRaw.jsp
            session.setAttribute("cmsDataExportWorker", worker);
            
        } catch(Exception e) {
            Debug.logError(e, LOG_PREFIX+"Error during " + outputMode + " export: " + e.getMessage(), module);
            cmsErrorHandler.addContextError(context, getPropMsg("CMSErrorUiLabels", "CmsErrorDuringExport", 
                ["outputMode": outputMode.getLabel(locale), "errMsg":e.getMessage()]));
        }
    } else {
        // FIXME: the effective number of entities may be different than this (Content/DataResource/ElectronicText)...
        numberOfEntities = passedEntityNames?.size() ?: 0;
        context.numberOfEntities = numberOfEntities;
        
        handleSingleResults = { execResult ->
            cmsErrorHandler.addContextErrors(context, execResult.getErrorMessages());
            results = [];
            results.add(getPropMsg("WebtoolsUiLabels", "WebtoolsWroteXMLForAllDataIn",
                ["numberOfEntities":numberOfEntities]));
            if (outputMode == OutputMode.SF_FS) {
                results.add(getPropMsg("WebtoolsUiLabels", "WebtoolsWroteNRecordsToXMLFile",
                    ["numberWritten":execResult.getNumberWritten(), "parameters":parameters]));
            } else {
                results.add(getPropMsg("CMSUiLabels", "CmsWroteNRecords",
                    ["numberWritten":execResult.getNumberWritten(), "parameters":parameters]));
            }
            results.addAll(execResult.getResultMessages());
            cmsErrorHandler.addContextEventMsgs(context, results);
            context.numberWritten = execResult.getNumberWritten();
        };
        
        if (outputMode == OutputMode.SF_IL) { // output direct in browser (textarea)
            try {
                StringWriter sw = new StringWriter();
                CmsDataExportWorker worker = CmsDataExportWorker.makeSingleFileWorker(workerArgs);
                
                // EXECUTE
                execResult = worker.executeExport(sw);
                
                handleSingleResults(execResult);
                context.resultText = sw.toString();
            } catch(Exception e) {
                Debug.logError(e, LOG_PREFIX+"Error during " + outputMode + " export: " + e.getMessage(), module);
                cmsErrorHandler.addContextError(context, getPropMsg("CMSErrorUiLabels", "CmsErrorDuringExport",
                    ["outputMode": outputMode.getLabel(locale), "errMsg":e.getMessage()]));
            }
        } else if (outputMode == OutputMode.SF_FS && filename) { // single file to file system
            try {
                file = new File(filename);
                CmsDataExportWorker worker = CmsDataExportWorker.makeSingleFileWorker(workerArgs);
                
                // EXECUTE
                execResult = worker.executeExport(file);
                
                handleSingleResults(execResult);
            } catch(Exception e) {
                Debug.logError(e, LOG_PREFIX+"Error during " + outputMode + " export: " + e.getMessage(), module);
                cmsErrorHandler.addContextError(context, getPropMsg("CMSErrorUiLabels", "CmsErrorDuringExport", 
                    ["outputMode": outputMode.getLabel(locale), "errMsg":e.getMessage()]));
            }
        } else if (outputMode == OutputMode.MF_FS && outpath) { // multiple files in a directory
            try {
                outdir = new File(outpath);
                if (!outdir.exists()) {
                    outdir.mkdir();
                }
                
                workerArgs.setMaxRecordsPerFile(maxRecordsPerFile);
                CmsDataExportWorker worker = CmsDataExportWorker.makeMultiFileWorker(workerArgs);
                
                // EXECUTE
                execResult = worker.executeExport(outdir);
                
                cmsErrorHandler.addContextErrors(context, execResult.getErrorMessages());
                cmsErrorHandler.addContextEventMsgs(context, execResult.getResultMessages());
                context.numberWritten = execResult.getNumberWritten();
            } catch(Exception e) {
                Debug.logError(e, LOG_PREFIX+"Error during " + outputMode + " export: " + e.getMessage(), module);
                cmsErrorHandler.addContextError(context, getPropMsg("CMSErrorUiLabels", "CmsErrorDuringExport", 
                    ["outputMode": outputMode.getLabel(locale), "errMsg":e.getMessage()]));
            }
        } else {
            cmsErrorHandler.addContextError(context, getPropMsg("CMSErrorUiLabels", "CmsMissingParameter",
                ["paramName":"outputMode"]));
            doExport = false;
        }
    }
}
context.doExport = doExport;
context.passedEntityNames = passedEntityNames;

