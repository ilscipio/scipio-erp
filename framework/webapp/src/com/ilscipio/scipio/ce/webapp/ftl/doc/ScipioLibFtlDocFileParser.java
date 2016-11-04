package com.ilscipio.scipio.ce.webapp.ftl.doc;

import java.io.File;
import java.util.ArrayList;
import java.util.IllegalFormatException;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.ilscipio.scipio.ce.webapp.ftl.doc.FtlDocException.ParseException;

/**
 * scipio-lib FTL doc format parser.
 * <p>
 * NOTE: many of the regexp assume no trailing spaces in lines.
 * tmplHelper.cleanTextValue removes them.
 * <p>
 * Also see {@link ScipioLibTemplateHelper}.
 */
public class ScipioLibFtlDocFileParser extends FtlDocFileParser {

    protected final ScipioLibTemplateHelper tmplHelper;
    protected String advancedArgDefaultArgsSuffix = "_defaultArgs";
    
    public ScipioLibFtlDocFileParser(String libFilename, File srcFile, 
            String inFileExtension, String outFileExtension) {
        super(libFilename, srcFile, inFileExtension, outFileExtension);
        tmplHelper = new ScipioLibTemplateHelper(inFileExtension, outFileExtension);
        tmplHelper.setMsgHandler(this.msgHandler);
    }
    
    public void setAdvancedArgDefaultArgsSuffix(String advancedArgDefaultArgsSuffix) {
        this.advancedArgDefaultArgsSuffix = advancedArgDefaultArgsSuffix;
    }
    
    @Override
    public String getLibFormat() {
        return FtlDocCompiler.SCIPIO_LIB_FORMAT;
    }
    
    /**
     * Sets top-level data model info about the library 
     * (mostly based on metadata, without parsing the file).
     */
    @Override
    public void setLibProperties(Map<String, Object> dataModel) throws IllegalFormatException {
        super.setLibProperties(dataModel);
    }

    private static final Pattern commentPat = Pattern.compile("<#--(.*?)-->", Pattern.DOTALL);
    
    // Delimited by *********, at least 30 stars
    private static final Pattern sectionPat = Pattern.compile(
            "[*]{30,}\\n" +
            "[*][ ]*([^\\n]+?)(?:[ ]*[*])?\\n" +
            "[*]{30,}\\n" +
            "(.*?)"
            , Pattern.DOTALL);
    // Delimited by *******, between 10 and 20 stars
    private static final Pattern entryPat = Pattern.compile(
            "[*]{10,20}\\n" +
            "[*][ ]*([^\\n]+?)(?:[ ]*[*])?\\n" +
            "[*]{10,20}\\n" +
            "(.*?)"
            , Pattern.DOTALL);

    /**
     * Main lib parsing method.
     * <p>
     * NOTE: this is very inefficient, but doesn't matter.
     */
    public void parseLib(Map<String, Object> dataModel, String fullText) throws ParseException {
        Matcher m;

        // Normalize all text so we have an easier time with regexp.
        fullText = tmplHelper.normalizeText(fullText);
        
        // First comment is file description
        m = commentPat.matcher(fullText);
        if (!m.find()) {
            throw new ParseException("Missing file top comment");
        }
        
        String introText = tmplHelper.stripCommentLeadingAsterix(tmplHelper.cleanTextValue(m.group(1)));
        dataModel.put("introText", introText);
        Matcher introm = tmplHelper.getFirstLineMatcher(introText);
        if (introm.matches()) { // NOTE: not .find()
            dataModel.put("pageTitle", tmplHelper.cleanTextValue(introm.group(1)));
            dataModel.put("pageDesc", tmplHelper.cleanTextValue(introm.group(2)));
        }
        
        
        String currentSectionName = "default";
        Map<String, Object> sectionInfo = makeObjectMap();
        sectionInfo.put("name", currentSectionName);
        sectionInfo.put("title", null);
        sectionInfo.put("type", "default");
        sectionInfo.put("comment", "");
        Map<String, Map<String, Object>> sectionEntryMap = makeDataMap();
        sectionInfo.put("entryMap", sectionEntryMap);
        
        Map<String, Map<String, Object>> entryMap = makeDataMap();
        Map<String, Map<String, Object>> sectionMap = makeDataMap();
        sectionMap.put("default", sectionInfo);
        
        while (m.find()) {
            String comment = tmplHelper.cleanTextValue(m.group(1));
            Matcher subm;
            subm = sectionPat.matcher(comment);
            if (subm.matches()) { // NOTE: not .find()
                currentSectionName = tmplHelper.cleanTextValue(subm.group(1));
                sectionInfo = makeObjectMap();
                sectionInfo.put("name", currentSectionName);
                sectionInfo.put("title", currentSectionName);
                sectionInfo.put("type", "sub");
                sectionInfo.put("comment", tmplHelper.cleanTextValue(tmplHelper.stripCommentLeadingAsterix(subm.group(2))));
                sectionEntryMap = makeDataMap();
                sectionInfo.put("entryMap", sectionEntryMap);
                sectionMap.put(currentSectionName, sectionInfo);
            }
            else {
                subm = entryPat.matcher(comment);
                if (subm.matches()) { // NOTE: not .find()
                    String entryTitle = tmplHelper.cleanTextValue(subm.group(1));
                    String entryBody = tmplHelper.cleanTextValue(subm.group(2));
                    String postEntryText = fullText.substring(m.end()); // FIXME: This is ridiculous inefficient

                    try {
                        Map<String, Object> entryInfo = parseEntry(entryTitle, entryBody, postEntryText);
                        entryInfo.put("sectionName", currentSectionName);
                        String entryName = (String) entryInfo.get("name");
                        if (entryName == null || entryName.isEmpty()) {
                            throw new ParseException("Could not determine a formal name for the entry (function, macro or variable name)");
                        }
                        if (entryMap.containsKey(entryName)) {
                            throw new ParseException("Duplicate entry: " + entryName);
                        }
                        entryMap.put(entryName, entryInfo);
                        sectionEntryMap.put(entryName, entryInfo);
                    }
                    catch (ParseException e) {
                        throw new ParseException("Error parsing entry '" + entryTitle + "': " + e.getMessage(), e);
                    } 
                }
                else {
                    ; // Ignore.
                }
            }
        }
        
        msgHandler.logInfo(" - entries: " + entryMap.size() + ", sections: " + sectionMap.size());
        
        dataModel.put("sectionMap", sectionMap);
        dataModel.put("entryMap", entryMap);
    }
    
    // These must all test for space and newline at beginning
    private static final Pattern commentedEntryPostPat = Pattern.compile(
            "^\\s*" +
            "<#--\\s*([^\\n]*?)\\s*" + // NOTE: crazy look-ahead needed
            "(<#[^-].*?)" +
            "\\s*-->"
            , Pattern.DOTALL);
    
    // WARN: postEntryText is NOT cleaned
    protected Map<String, Object> parseEntry(String title, String body, CharSequence postEntryText) throws ParseException {
        Map<String, Object> info = makeObjectMap();
        info.put("title", title);
        info.put("body", body); // just in case template needs, usually not
        
        Matcher m;
        
        msgHandler.logDebug("entry " + title);
        
        //msgHandler.printDebug("postEntryText: " + postEntryText.toString().substring(0, postEntryText.length()));
        
        m = commentedEntryPostPat.matcher(postEntryText);
        if (m.find()) {
            // NOTE: Even if commented, there must be a valid function, var or assign here
            // FIXME: super inefficient !!
            String commentedEntry = tmplHelper.cleanTextValue(m.group(2)); 
            msgHandler.logDebug(" is commented");
            Map<String, Object> funcMacroVarInfo = parseFunctionMacroVar(commentedEntry);
            if (funcMacroVarInfo == null) {
                throw new ParseException("Expected a commented #assign, #function, or #macro declaration, but got nothing; please make sure " +
                        "even commented code still contains valid Freemarker and a placeholder commented entry remains; please make sure there "
                        + "is not an extra comment between the main entry documentation and the declaration (or commented declaration); "
                        + "please make sure the commented placeholder entry is in a separate comment from the entry documentation comment");
            }
            info.putAll(funcMacroVarInfo);
            info.put("isCommented", Boolean.TRUE);
            String commentedImplComment = tmplHelper.cleanTextValue(m.group(1));
            info.put("commentedImplComment", commentedImplComment);
        }
        else {
            msgHandler.logDebug(" is NOT commented");
            Map<String, Object> funcMacroVarInfo = parseFunctionMacroVar(postEntryText);
            if (funcMacroVarInfo == null) {
                throw new ParseException("Expected #assign, #function, or #macro declaration, or a commented placeholder, but got nothing");
            }
            info.putAll(funcMacroVarInfo);
            info.put("isCommented", Boolean.FALSE);
        }

        Map<String, Object> entryBodyInfo = parseEntryBody(body);
        info.putAll(entryBodyInfo);
        
        amendArgListInfo(info);
        
        setEntryProperties(info);

        return info;
    }
    
    protected void setEntryProperties(Map<String, Object> info) throws ParseException {
        String commentedImplComment = (String) info.get("commentedImplComment");
        info.put("isTransform", Boolean.FALSE);
        info.put("isImplemented", Boolean.TRUE);
        info.put("isDeprecated", Boolean.FALSE);
        info.put("isOverride", Boolean.FALSE); 
        info.put("isAbstract", Boolean.FALSE); 
        if (!info.containsKey("isAdvancedArgs")) {
            info.put("isAdvancedArgs", Boolean.FALSE);
        }
        
        if (commentedImplComment != null) {
            if (commentedImplComment.matches("(?s).*IMPLEMENTED\\s+AS\\s+(JAVA\\S+)?TRANSFORM.*")) {
                info.put("isTransform", Boolean.TRUE);
            }
            if (commentedImplComment.matches("(?s).*NOT\\s+IMPLEMENTED.*")) {
                info.put("isImplemented", Boolean.FALSE);
            }
            if (commentedImplComment.matches("(?s).*DEPRECATED.*")) {
                info.put("isDeprecated", Boolean.TRUE);
            }
            if (commentedImplComment.matches("(?s).*OVERRIDE.*")) {
                info.put("isOverride", Boolean.TRUE);
            }
            if (commentedImplComment.matches("(?s).*ABSTRACT.*")) {
                info.put("isAbstract", Boolean.TRUE);
            }
        }
        
        String shortDesc = (String) info.get("shortDesc");
        if (shortDesc != null) {
            if (shortDesc.matches("(?s).*IMPLEMENTED\\s+AS\\s+(JAVA\\S+)?TRANSFORM.*")) {
                info.put("isTransform", Boolean.TRUE);
            }
            if (shortDesc.matches("(?s).*NOT\\s+IMPLEMENTED.*")) {
                info.put("isImplemented", Boolean.FALSE);
            }
            if (shortDesc.matches("(?s).*DEPRECATED.*")) {
                info.put("isDeprecated", Boolean.TRUE);
            }
            if (shortDesc.matches("(?s).*OVERRIDE.*")) {
                info.put("isOverride", Boolean.TRUE);
            }
            if (shortDesc.matches("(?s).*ABSTRACT.*")) {
                info.put("isAbstract", Boolean.TRUE);
            }
        }
    }
    
    protected void amendArgListInfo(Map<String, Object> info) throws ParseException {
        List<String> argList = (List<String>) info.get("argList");
        Map<String, Object> argMap = null;
        if (argList != null) {
            argMap = keyListToObjectMapMap(argList);
        }
        Map<String, Object> paramDescMap = (Map<String, Object>) info.get("paramDescMap");
        
        Map<String, Object> argMapUnaccounted = null;
        Map<String, Object> argMapPlusParamDescMap = null;
        Map<String, Object> paramDescMapPlusArgMapUnaccounted = null;
        
        Map<String, Object> paramDescMapSplitKeys = null;
        if (paramDescMap != null) {
            paramDescMapSplitKeys = makeDataMap();
            for(Map.Entry<String, Object> paramDescMapEntry : paramDescMap.entrySet()) {
                String fullKey = paramDescMapEntry.getKey();
                String[] allKeys = fullKey.split(",");
                for(String key : allKeys) {
                    String k = key.trim();
                    if (k.length() > 0) {
                        paramDescMapSplitKeys.put(k, paramDescMapEntry.getValue());
                    }
                }
            }
        }
        
        if (paramDescMap != null || argMap != null) {
            if (paramDescMap == null) {
                paramDescMap = makeDataMap();
            }
            if (paramDescMapSplitKeys == null) {
                paramDescMapSplitKeys = makeDataMap();
            }
            if (argMap == null) {
                argMap = makeDataMap();
            }
            argMapUnaccounted = makeDataMap();
            for(Map.Entry<String, Object> argMapEntry : argMap.entrySet()) {
                if (!paramDescMapSplitKeys.containsKey(argMapEntry.getKey())) { // NOTE: must use split keys map!
                    argMapUnaccounted.put(argMapEntry.getKey(), argMapEntry.getValue());
                }
            }

            paramDescMapPlusArgMapUnaccounted = makeDataMap(paramDescMap);
            paramDescMapPlusArgMapUnaccounted.putAll(argMapUnaccounted);
            
            argMapPlusParamDescMap = makeDataMap(argMap);
            argMapPlusParamDescMap.putAll(paramDescMapSplitKeys); // NOTE: must use split keys map!
        }
        
        info.put("argMapUnaccounted", argMapUnaccounted);
        info.put("argMapWithParamDescMapEntries", argMapPlusParamDescMap);
        info.put("paramDescMapPlusArgMapUnaccounted", paramDescMapPlusArgMapUnaccounted);
        info.put("paramDescMapSplitKeys", paramDescMapSplitKeys);
    }
    
    private static final Pattern entryBodySectionsPat = Pattern.compile(
            "(?:^|\\n)[ ]{1,2}[*][ ]+([^\\n]*?)[ ]+[*]\\n"
            , Pattern.DOTALL);

    protected Map<String, Object> parseEntryBody(CharSequence text) throws ParseException {
        Map<String, Object> info = makeObjectMap();
        
        Map<String, CharSequence> secTitleMap = parseSubSections(text, entryBodySectionsPat, "Main Description");

        Map<String, Map<String, Object>> entrySections = makeDataMap();
        
        for(Map.Entry<String, CharSequence> entry : secTitleMap.entrySet()) {
            String secTitle = entry.getKey();
            String secText = entry.getValue().toString();
            // some of the sections need text without first indent trimmed, so save a version without trim
            String rawSecText = tmplHelper.cleanTextValueSafe(secText);
            secText = tmplHelper.cleanTextValue(secText);
            
            Map<String, Object> secInfo = makeObjectMap();
            secInfo.put("title", secTitle);
            secInfo.put("text", secText);
            secInfo.put("rawText", rawSecText);
            
            String secName;
            if (secTitle.matches("(?i)Main Description")) {
                secName = "mainDesc";
                // First sentence is the short desc
                Matcher shortdescm = tmplHelper.getFirstLineMatcher(secText);
                if (shortdescm.matches()) {
                    String shortDesc = tmplHelper.cleanTextValue(shortdescm.group(1));
                    String extraDesc = tmplHelper.cleanTextValue(shortdescm.group(2));
                    secInfo.put("shortDesc", shortDesc);
                    secInfo.put("extraDesc", extraDesc);
                    info.put("shortDesc", shortDesc); // convenience
                    info.put("extraDesc", extraDesc);
                }
                else {
                    throw new ParseException("There is no short description for the entry. "
                            + "The first sentence should be separated by two carriage return");
                }
            }
            else if (secTitle.matches("(?i)Usage\\s+example[s]?")) {
                secName = "examples";
                info.put("exampleText", rawSecText);
            }
            else if (secTitle.matches("(?i)Parameters")) {
                secName = "parameters";
                Map<String, Object> paramsInfo = parseParamsBody(rawSecText);
                secInfo.putAll(paramsInfo);
                info.put("paramDescMap", paramsInfo.get("paramDescMap")); // convenience
            }
            else if (secTitle.matches("(?i)Return\\s+values?")) {
                secName = "returnValues";
                info.put("returnValueText", secText);
            }
            else if (secTitle.matches("(?i)Related?")) {
                secName = "related";
                String[] relatedNames = secText.split("[,;\\s]+");
                secInfo.put("relatedNames", relatedNames);
                info.put("relatedNames", relatedNames);
            }
            else {
                // Unknown, just include anyway and template will render how/where it wants
                secName = tmplHelper.makeCamelCaseNameFromTitle(secTitle);
            }
            secInfo.put("name", secName);
            entrySections.put(secName, secInfo);
        }
        
        info.put("sections", entrySections);
        
        return info;
    }
    
    /**
     * Parse sections. headerTitlePat group 1 must be the title.
     * Titles are cleaned but bodies are NOT.
     */
    protected Map<String, CharSequence> parseSubSections(CharSequence text, Pattern headerTitlePat, String defaultSectionTitle) {
        Matcher m = headerTitlePat.matcher(text);
        
        Map<String, CharSequence> secTitleMap = makeDataMap();
        
        int lastEndIndex = 0;
        String lastSecTitle = tmplHelper.cleanTextValue(defaultSectionTitle);
        
        while(m.find()) {
            if (m.start() > 0) { // if (m.start() == 0) we assume first section supplants the default
                CharSequence lastSecText = text.subSequence(lastEndIndex, m.start());
                // extra check (augments (m.start() > 0) check): if this is the first, make sure don't have only whitespace
                if ((lastEndIndex > 0) || lastSecText.toString().trim().length() > 0) {
                    secTitleMap.put(lastSecTitle, lastSecText);
                }
            }
            
            lastSecTitle = tmplHelper.cleanTextValue(m.group(1));
            lastEndIndex = m.end();
        }
        
        secTitleMap.put(lastSecTitle, text.subSequence(lastEndIndex, text.length()));
        
        return secTitleMap;
    }
    
    private static final Pattern paramSectionsPat = Pattern.compile(
            "(?:^|\\n)[ ]{3,5}[*][ ]+([^\\n]*?)[ ]+[*]\\n"
            , Pattern.DOTALL);
    
    protected Map<String, Object> parseParamsBody(String rawText) {
        Map<String, Object> paramsInfo = makeObjectMap();
        
        // There's a global paramDescMap for the entry, and each paramSectionMap has a paramDescMap too
        Map<String, Map<String, Object>> paramDescMap = makeDataMap();
        Map<String, Map<String, Object>> paramSectionMap = makeDataMap();
        
        Map<String, CharSequence> secTitleMap = parseSubSections(rawText, paramSectionsPat, "General");

        for(Map.Entry<String, CharSequence> entry : secTitleMap.entrySet()) {
            String secTitle = entry.getKey();
            String rawSecText = tmplHelper.cleanTextValueSafe(entry.getValue().toString());

            Map<String, Map<String, Object>> sectionParamDescMap = parseParamEntries(rawSecText);

            // Add all params to global list for entry
            paramDescMap.putAll(sectionParamDescMap);
            
            // Make map for this section
            Map<String, Object> paramSectionInfo = makeObjectMap();
            paramSectionInfo.put("paramDescMap", sectionParamDescMap);
            paramSectionInfo.put("title", secTitle);
            String secName = tmplHelper.makeCamelCaseNameFromTitle(secTitle);
            paramSectionInfo.put("name", secName);
            paramSectionMap.put(secName, paramSectionInfo);
        }
        
        paramsInfo.put("paramDescMap", paramDescMap);
        paramsInfo.put("paramSectionMap", paramSectionMap);
        
        return paramsInfo;
    }
    
    private static final Pattern parameterPat = Pattern.compile(
            "^[ ]{1,8}([^\\s=][^=]*?[^\\s=])[ ]*=[ ]*"
            , Pattern.DOTALL + Pattern.MULTILINE);
    
    protected Map<String, Map<String, Object>> parseParamEntries(String rawText) {
        Map<String, Map<String, Object>> paramDescMap = makeDataMap();

        Matcher paramm = parameterPat.matcher(rawText);
        int lastParamEndIndex = 0;
        String lastParamName = "";
        String lastParamNameMatch = "";
        
        while(paramm.find()) {
            if (!lastParamName.isEmpty()) {
                String lastParamVal = tmplHelper.cleanTextValue(rawText.substring(lastParamEndIndex, paramm.start()));
                lastParamVal = tmplHelper.stripIndent(lastParamVal, lastParamNameMatch.length());
                paramDescMap.put(lastParamName, parseParamDesc(lastParamVal));
            }
            
            lastParamName = tmplHelper.cleanTextValue(paramm.group(1));
            lastParamName = lastParamName.replaceAll("\\n+", " "); // this may happen when multiple grouped together
            lastParamNameMatch = paramm.group();
            lastParamEndIndex = paramm.end();
        }   
        
        if (!lastParamName.isEmpty()) {
            String lastParamVal = tmplHelper.cleanTextValue(rawText.substring(lastParamEndIndex, rawText.length()));
            lastParamVal = tmplHelper.stripIndent(lastParamVal, lastParamNameMatch.length());
            paramDescMap.put(lastParamName, parseParamDesc(lastParamVal));
        }
        
        return paramDescMap;
    }

    
    protected Map<String, Object> parseParamDesc(String text) {
        Map<String, Object> info = makeObjectMap();
        info.put("text", text);
        
        String remain = text;
        int i;
        
        if (remain.startsWith("(")) {
            i = tmplHelper.getClosingCharIndex(remain, 1, '(', ')');
            if (i >= 0) {
                String typeStr = remain.substring(0, i+1);
                // TODO: go into more detail (type, default, required, etc.)
                info.put("typeStr", typeStr);
                // NO TRIM here
                remain = tmplHelper.cleanTextValueSafe(remain.substring(i+1));
            }
        }
        
        String shortDesc; 
        String extraDesc;
                
        i = remain.indexOf("\n");
        if (i >= 0) {
            shortDesc = tmplHelper.cleanTextValue(remain.substring(0, i));
            extraDesc = tmplHelper.cleanTextValueSafe(remain.substring(i+1));
            //extraDesc = tmplHelper.cleanTextValue(remain.substring(i+1));
        }
        else {
            shortDesc = remain;
            extraDesc = null;
        }
        
        info.put("shortDesc", shortDesc);
        info.put("extraDesc", extraDesc);
        return info;
    }

    private static final Pattern assignPat = Pattern.compile(
            "^\\s*" +
            "<#assign\\s(\\w+)\\s*=\\s*(\\s[^>]*?)\\s*/?>" 
            , Pattern.DOTALL);
    
    // WARN: text is not cleaned
    protected Map<String, Object> parseFunctionMacroVar(CharSequence text) throws ParseException {
        Matcher m;
        
        m = assignPat.matcher(text);
        if (m.find()) {
            Map<String, Object> info = makeObjectMap();
            
            // Either a variable or an advanced args pattern
            String varName = tmplHelper.cleanTextValue(m.group(1));
            if (varName.endsWith(advancedArgDefaultArgsSuffix)) {
                msgHandler.logDebug(" is advanced arg pattern");
                
                String argStr = tmplHelper.cleanTextValue(m.group(2));
                
                //msgHandler.printMsg("Has suffix");
                // FIXME: super inefficient!!!
                CharSequence postVarText = text.toString().substring(m.end());
                
                Map<String, Object> functionMacroInfo = parseFunctionMacro(postVarText);
                if (functionMacroInfo == null) {
                    throw new ParseException("Expected to find function or macro at this point, but got something else (or nothing)");
                }
                info.putAll(functionMacroInfo);
                
                // override arguments with special ones from #assign
                
                info.put("argStr", argStr);
                info.put("argList", parseArgStringMap(argStr));
                
                info.put("isAdvancedArgs", Boolean.TRUE);
            }
            else {
                msgHandler.logDebug(" is regular var");
                info.put("type", "variable");
                info.put("name", varName);
                info.put("defaultVal", tmplHelper.cleanTextValue(m.group(2))); // NOTE: only var gets default for now
            }
            return info;
        }
        else {
            Map<String, Object> info = parseFunctionMacro(text);
            if (info != null) {
                info.put("isAdvancedArgs", Boolean.FALSE);
                return info;
            }
            else {
                return null;
            }
        }
    }
    
    private Map<String, Object> keyListToObjectMapMap(List<String> keys) {
        Map<String, Object> res = makeDataMap();
        if (keys != null) {
            for(String key : keys) {
                res.put(key, makeObjectMap());
            }
        }
        return res;
    }
    
    private static final Pattern macroPat = Pattern.compile(
            "^\\s*" +
            "<#macro\\s(\\w+)(\\s[^>]*?)?>(.*?)</#macro>" +
            ".*" // extra needed for .matches()
            , Pattern.DOTALL);
    private static final Pattern functionPat = Pattern.compile(
            "^\\s*" +
            "<#function\\s(\\w+)(\\s[^>]*?)?>(.*?)</#function>" +
            ".*" // extra needed for .matches()
            , Pattern.DOTALL);
    
    // WARN: text is not cleaned
    protected Map<String, Object> parseFunctionMacro(CharSequence text) throws ParseException {
        Map<String, Object> info = makeObjectMap();
        String type;

        Matcher m;
        m = macroPat.matcher(text);
        if (m.find()) {
            msgHandler.logDebug(" is macro");
            type = "macro";
            info.put("name", tmplHelper.cleanTextValue(m.group(1)));
            String argStr = tmplHelper.cleanTextValue(m.group(2));
            info.put("argStr", argStr);
            info.put("argList", parseArgStringMacro(argStr));
        }
        else {
            msgHandler.logDebug(" is function");
            m = functionPat.matcher(text);
            if (m.find()) {
                type = "function";
                info.put("name", tmplHelper.cleanTextValue(m.group(1)));
                String argStr = tmplHelper.cleanTextValue(m.group(2));
                info.put("argStr", argStr);
                info.put("argList", parseArgStringFunction(argStr));
            }
            else {
                return null;
            }
        }
        
        info.put("type", type);
        return info;
    }
    
    
    private static final Pattern mapArgSinglePat = Pattern.compile(
            "(['\"])(.*?)\\1\\s*:.*"
            , Pattern.DOTALL);
    
    /**
     * FIXME: This only supports very basic syntax!!! Will break extremely easily!!!
     * <p>
     * NOTE: INTENTIONALLY omitting default values because generally misleading.
     */
    protected List<String> parseArgStringMap(String argStr) throws ParseException {
        List<String> argList = new ArrayList<>();
        if (argStr == null) {
            return argList;
        }
        argStr = argStr.trim();
        if (argStr.startsWith("{")) {
            argStr = argStr.substring(1).trim();
            if (argStr.endsWith("}")) {
                argStr = argStr.substring(0, argStr.length() - 1).trim();
            }
            else {
                throw new ParseException("Error parsing map variable (in #assign) - unterminated bracket");
            }
        }
        String[] args = argStr.toString().split(",");
        for (String argEntry : args) {
            String arg = argEntry.trim();
            Matcher m = mapArgSinglePat.matcher(arg);
            if (m.matches()) {
                String name = m.group(2).trim();
                argList.add(name);
            }
            else {
                msgHandler.logDebug(" text '" + arg + "' could not be matched");
                throw new ParseException("Error parsing map variable (in #assign)."
                        + " WARN: Any non-simple syntax breaks easily here.");
            }
        }
        return argList;
    }    
    
    /**
     * FIXME: This only supports very basic syntax!!! Will break extremely easily!!!
     * <p>
     * NOTE: INTENTIONALLY omitting default values because generally misleading.
     */
    protected List<String> parseArgStringFunction(CharSequence argStr) throws ParseException {
        List<String> argList = new ArrayList<>();
        if (argStr == null) {
            return argList;
        }
        String[] args = argStr.toString().split("\\s+");
        for (String arg : args) {
            String[] parts = arg.split("=");
            String name = parts[0].trim();
            argList.add(name);
        }
        return argList;
    }
    
    /**
     * FIXME: This only supports very basic syntax!!! Will break easily!!! Spaces will break it!!! It's terrible!!!
     * <p>
     * NOTE: INTENTIONALLY omitting default values because generally misleading.
     */
    protected List<String> parseArgStringMacro(CharSequence argStr) throws ParseException {
        List<String> argList = new ArrayList<>();
        String[] args = argStr.toString().split("(\\s|\\n)+");
        for (String arg : args) {
            String[] parts = arg.split("=");
            String name = parts[0];
            argList.add(name);
        }
        return argList;
    }

}