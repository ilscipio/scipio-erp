/**
 * SCIPIO: Data prep for hosted web version of ftl doc template (ftlDocTemplateWeb.ftl).
 * Performs two different operations, the second is separated so the special
 * ftl wrapped values have less chance of interfering with the regular namespace.
 * (these are times when you wish everything had its own namespace)
 */

import com.ilscipio.scipio.ce.webapp.ftl.doc.FtlDocCompiler
import java.util.Arrays;
import java.util.List;
import java.io.File;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import com.ilscipio.scipio.ce.webapp.ftl.doc.OfbizDebugMsgHandler;


// VAR & HELPERS

final module = "ftlDocTemplateWebGroovy";
final msgHandler = new OfbizDebugMsgHandler(module);

def addCtxErrorMsg(errMsg) {
    errorMessageList = context.errorMessageList;
    if (errorMessageList == null) {
        errorMessageList = [];
    }
    errorMessageList.add(errMsg);
    context.errorMessageList = errorMessageList;
    context.isError = true;
}


// SCRIPT SETUP

fdtwArgs = context.fdtwArgs ?: [:];
context.remove("fdtwArgs");

// Script implements these operations:
doCompile = Boolean.TRUE.equals(fdtwArgs.doCompile);
doPrepFtlCtx = Boolean.TRUE.equals(fdtwArgs.doPrepFtlCtx);


// COMPILATION OPERATION

if (doCompile) {

// Reset results
context.targetLibPath = null;
context.targetLibName = null;
context.targetLibShortName = null;
context.docOpenError = false;
context.docParseError = false;
context.docContext = [:];

// Gets inputs, defaults
allLibNames = fdtwArgs.allLibNames ?: [
    "utilities",
    "standard/htmlContent",
    "standard/htmlForm",
    "standard/htmlInfo",
    "standard/htmlNav",
    "standard/htmlScript",
    "standard/htmlStructure",
    "standard/htmlTemplate"
];

// TODO?: Localize? (nothing else is)
libTitleMap = fdtwArgs.libTitleMap ?: [
    "utilities" : "Utilities",
    "standard/htmlContent" : "Content Elements",
    "standard/htmlForm" : "Form Elements",
    "standard/htmlInfo" : "Information & Notifications",
    "standard/htmlNav" : "Navigation & Menus",
    "standard/htmlScript" : "HTML & Script Wrappers",
    "standard/htmlStructure" : "Grid & Structure",
    "standard/htmlTemplate" : "General & Master Include"
];
context.docLibTitleMap = libTitleMap; 

defaultTargetLibName = fdtwArgs.defaultTargetLibName ?: "standard/htmlTemplate";
libSrcSuffix = fdtwArgs.libSrcSuffix ?: ".ftl";
libOutSuffix = fdtwArgs.libOutSuffix ?: ""; // OLD: ".html"

targetLibName = fdtwArgs.targetLibName;
targetLibPath = fdtwArgs.targetLibPath;

if (targetLibPath && targetLibPath.startsWith("/")) {
    targetLibPath = targetLibPath.substring(1);
    if (!targetLibPath) {
        targetLibPath = null;
    }
}

if (!targetLibName && !targetLibPath) {
    targetLibName = defaultTargetLibName;
    targetLibPath = targetLibName + libOutSuffix;
} else if (!targetLibPath) {
    targetLibPath = targetLibName + libOutSuffix;
} else if (!targetLibName) {
    if (targetLibPath.endsWith(libOutSuffix)) {
        targetLibName = targetLibPath.substring(0, targetLibPath.length() - libOutSuffix.length());
    } else {
        targetLibName = targetLibPath;
    }
}

if (!allLibNames.contains(targetLibName)) {
    addCtxErrorMsg("Template API doc page \"" + targetLibName + "\" (path: \"" + targetLibPath + "\") not found.");
    context.docOpenError = true;
    return;
}

index = targetLibName.lastIndexOf('/');
targetLibShortName = (index >= 0) ? targetLibName.substring(index+1) : targetLibName;

context.targetLibPath = targetLibPath;
context.targetLibName = targetLibName;
context.targetLibShortName = targetLibShortName;

compileArgs = [:];
compileArgs.targetLibName = targetLibName;
compileArgs.targetLibPath = targetLibPath;

compileArgs.defaultLibFormat = fdtwArgs.defaultLibFormat ?: "scipio-lib";
compileArgs.docPurpose = fdtwArgs.docPurpose ?: "templating";
compileArgs.srcFolderPath = fdtwArgs.srcFolderPath ?: "framework/common/webcommon/includes/scipio/lib";
compileArgs.libFilenames = allLibNames.collect{ it + libSrcSuffix };

ofbizHome = System.getProperty("ofbiz.home");
if (!ofbizHome) {
    Debug.logWarning("Property ofbiz.home is missing!", module);
} else {
    compileArgs.srcFolderPath = (new File(ofbizHome, compileArgs.srcFolderPath)).getPath();
}

reloadDataModel = Boolean.TRUE.equals(fdtwArgs.reloadDataModel);

try {
    targetDataModel = new HashMap<>();
    
    srcFileDataModels = null; // this is the mega motherload data model covering all the files
    if (!reloadDataModel) {
        // check motherload in cache (relaxed)
        srcFileDataModels = request.getSession().getAttribute("fdtwDataModelsCached");
    }
    
    FtlDocCompiler compiler = FtlDocCompiler.getInstance();
    compiler.setInFileExtension(libSrcSuffix);
    compiler.setOutFileExtension(libOutSuffix);
    compiler.setMsgHandler(msgHandler);
    
    if (srcFileDataModels != null) {
        compiler.setSrcFileDataModels(srcFileDataModels); // the exec call will use it and ignore the already done parts
    } 
    compiler.execDataLoadOnly(compileArgs.defaultLibFormat, compileArgs.docPurpose, 
        compileArgs.srcFolderPath, compileArgs.libFilenames, compileArgs.targetLibName, 
        targetDataModel);
    
    if (targetDataModel.isEmpty()) {
        throw new IOException("No data received from parsing (reason: unknown)");
    } else if (targetDataModel.libFormat == null) {
        throw new IOException("Data model incomplete from parsing (reason: unknown)");
    } else {
        // (re-)cache the motherload with latest
        request.getSession().setAttribute("fdtwDataModelsCached", compiler.getSrcFileDataModels());
    }
    
    context.docContext = Collections.unmodifiableMap(targetDataModel); // for regular access from screens
    
} catch (Throwable t) {
    Debug.logError(t, "Error during Freemarker Template API on-the-fly document compilation. " +
        "args: " + compileArgs.toString(), module);
    addCtxErrorMsg("Error during Freemarker Template API Documentation on-the-fly compilation (see logs).");
    context.docParseError = true;
    return;
}

}  else { // doCompile false

    targetDataModel = context.docContext ?: [:];

}


// FREEMARKER PREP OPERATION

if (doPrepFtlCtx) {
    
    // SPECIAL OPERATION:
    // after the previous data model build, now need to transfer the data model to context, BUT only with
    // basic templatemodels.
    // can simply pre-wrap all values using a simple ObjectWrapper
    // everything else in context is left untouched.
    // NOTE: this is a non-copying wrapper and should be fairly close to the one created
    // by FtlDocCompiler during standalone compile
    objectWrapper = FreeMarkerWorker.getDefaultSimpleTypeWrapper();
    targetDataModel.each { k, v -> context[k] = objectWrapper.wrap(v)}
    
}




