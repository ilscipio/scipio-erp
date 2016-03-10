package com.ilscipio.cato.ce.webapp.ftl.doc;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.IllegalFormatException;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateExceptionHandler;

/**
 * Cato: Customized Freemarker library documentation compiler and generator.
 * Transforms a set of source FTL files into HTML documentation files.
 * <p>
 * <strong>REQUIRES FREEMARKER 2.3.22 OR LATER IN CLASSPATH FOR INVOCATION.</strong>
 * <p>
 * Main command line arguments (minimum 5 arguments needed for invocation):<br/>
 * <code>[lib format] [base source folder] [output folder] [FTL doc-formatting template location] [series of relative source file paths, at least one]</code><br/>
 * The only currently supported lib format is "cato-lib".
 * <p>
 * TODO: This is currently compiled as part of the Ofbiz compilation, but
 * it should be in its own separate project.
 * <p>
 * FIXME?: Only supports one lib format for all sources.
 * <p>
 * DEV NOTE: <strong>WARN</strong>: Do not use Ofbiz libraries here.
 * <p>
 * TODO: Could consider use of markdown (or other) in descriptions.
 * <p>
 * TODO: log4j
 * <p>
 * TODO: License
 */
public class FtlDocCompiler {

    public static final String CATO_LIB_FORMAT = "cato-lib";
    public static final Charset DEFAULT_FILE_ENCODING = StandardCharsets.UTF_8;
    private static final boolean DEBUG = true;
    
    protected MsgHandler msgHandler = new VoidMsgHandler();
    
    protected String srcFolderPath = null;
    protected File srcFolderPathFile = null;
    protected List<String> libFilenames = null;
    protected List<File> libFiles = null;
    protected String templatePath = null;
    protected File templatePathFile = null;
    protected String outFolderPath = null;
    protected File outFolderPathFile = null;
    
    protected static final String inFileExtension = ".ftl";
    protected static final String outFileExtension = ".html";
    
    protected Configuration cfg = null;
    
    protected String defaultLibFormat = CATO_LIB_FORMAT;
    protected Charset defaultEncoding = DEFAULT_FILE_ENCODING;


    public FtlDocCompiler() {
    }
    
    /**
     * Gets default instance, which parses Ilscipio Cato macro doc format.
     * <p>
     * FIXME: this may not make sense anymore, but whatever
     */
    public static FtlDocCompiler getInstance() {
        return new FtlDocCompiler();
    }
    
    
    public static void main(String[] args) {
        MsgHandler msgHandler = new SysOutMsgHandler();
        
        if (args.length >= 5) {
            String defaultLibFormat = args[0];
            String srcFolderPath = args[1];
            String outFolderPath = args[2];
            String templatePath = args[3];
            List<String> libFilenames = Arrays.asList(args).subList(4, args.length);

            try {
                FtlDocCompiler compiler = FtlDocCompiler.getInstance();
                compiler.setMsgHandler(msgHandler);
                compiler.execBasic(defaultLibFormat, srcFolderPath, libFilenames, templatePath, outFolderPath);
            } catch (Throwable t) {
                msgHandler.printError(t.getMessage());
                t.printStackTrace();
                System.exit(1);
            }
        }
        else {
            msgHandler.printError("Insufficient arguments");
            System.exit(1);
        }
    }
    
    public void execBasic(String defaultLibFormat, String srcFolderPath, List<String> libFilenames, 
            String templatePath, String outFolderPath) throws IllegalArgumentException, IOException, TemplateException {
        msgHandler.printMsg("Setting sources and output...");
        setDefaultLibFormat(defaultLibFormat);
        setSources(srcFolderPath, libFilenames, templatePath);
        setOutputFolder(outFolderPath);
        
        msgHandler.printMsg("Initializing...");
        init();
        
        msgHandler.printMsg("Begin compiling...");
        compile();
        msgHandler.printMsg("Compilation complete.");
    }
    
    public void setMsgHandler(MsgHandler msgHandler) {
        this.msgHandler = msgHandler;
    }

    public void setDefaultLibFormat(String defaultLibFormat) {
        this.defaultLibFormat = defaultLibFormat;
    }
    
    public void setSources(String srcFolderPath, List<String> libFilenames, String templatePath) throws FileNotFoundException, IllegalArgumentException {
        this.srcFolderPath = validateFilename(srcFolderPath);
        this.srcFolderPathFile = new File(this.srcFolderPath);
        if (!this.srcFolderPathFile.exists()) {
            throw new FileNotFoundException("Base source folder '" + this.srcFolderPathFile + "' does not exist");
        }
        if (!this.srcFolderPathFile.isDirectory()) {
            throw new IllegalArgumentException("Base source folder '" + this.srcFolderPathFile + "' is not a directory");
        }
        
        this.libFilenames = new ArrayList<String>(libFilenames.size());
        this.libFiles = new ArrayList<File>(libFilenames.size());
        for(String filename : libFilenames) {
            String validFn = validateFilename(filename);
            if (!validFn.endsWith(inFileExtension)) {
                throw new IllegalArgumentException("Input file " + filename + " does not end in .ftl");
            }
            File libFile = new File(this.srcFolderPathFile, validFn);
            if (!libFile.exists()) {
                throw new FileNotFoundException("Source file '" + libFile.getPath() + "' does not exist");
            }
            if (!libFile.isFile()) {
                throw new IllegalArgumentException("Source file '" + libFile.getPath() + "' is not a file");
            }
            this.libFilenames.add(validateFilename(filename));
            this.libFiles.add(libFile);
        }
        
        this.templatePath = validateFilename(templatePath);
        this.templatePathFile = new File(this.templatePath);
        if (!this.templatePathFile.exists()) {
            throw new FileNotFoundException("Template '" + this.templatePathFile + "' does not exist");
        }
        if (!this.templatePathFile.isFile()) {
            throw new IllegalArgumentException("Template '" + this.templatePathFile + "' is not a file");
        }
    }
    
    public void setOutputFolder(String outFolderPath) throws FileNotFoundException, IllegalArgumentException {
        this.outFolderPath = validateFilename(outFolderPath);
        this.outFolderPathFile = new File(this.outFolderPath);
        // If doesn't exist, will create, but if exists, must be a folder
        if (this.outFolderPathFile.exists()) {
            if (!this.outFolderPathFile.isDirectory()) {
                throw new IllegalArgumentException("Output folder '" + this.outFolderPathFile + "' is not a directory");
            }
        }
    }
    
    public void init() throws IOException {
         cfg = new Configuration(Configuration.VERSION_2_3_22);
         cfg.setDirectoryForTemplateLoading(new File("."));
         cfg.setDefaultEncoding(defaultEncoding.name());
         cfg.setTemplateExceptionHandler(TemplateExceptionHandler.HTML_DEBUG_HANDLER);
    }
    
    public void compile() throws IOException, ParseException, TemplateException {
        msgHandler.printMsg("Parsing to data models...");
        Map<String, Map<String, Object>> srcFileDataModels = parseSourcesToDataModels(this.defaultLibFormat);
        msgHandler.printMsg("Got " + srcFileDataModels.size() + " data models");
        
        msgHandler.printMsg("Loading FTL doc template...");
        Template template = cfg.getTemplate(this.templatePath);
        
        msgHandler.printMsg("Rendering...");
        render(template, srcFileDataModels);
    }
    
    
    /*
     **********************************************************
     * PARSING (TO DATA MODEL) *
     **********************************************************
     */
    
    /**
     * For each input file, creates a data model. 
     * 
     * @return a map of file names to data models
     */
    protected Map<String, Map<String, Object>> parseSourcesToDataModels(String defaultLibFormat) throws IOException {
        Map<String, Map<String, Object>> srcFileDataModels = new LinkedHashMap<>();
        
        for(String libFilename : libFilenames) {
            File srcFile = new File(srcFolderPathFile, libFilename);
            
            msgHandler.printMsg("Parsing " + srcFile.toString());
            Map<String, Object> dataModel = parseSourceToDataModel(libFilename, srcFile, defaultLibFormat);
            srcFileDataModels.put(libFilename, dataModel);
        }
        return srcFileDataModels;
    }
    
    protected Map<String, Object> parseSourceToDataModel(String libFilename, File srcFile, String defaultLibFormat) throws ParseException, IOException {
        Map<String, Object> dataModel = new HashMap<>();
        
        // Super lazy, load entire file as a string.
        String text = readFileAsString(srcFile.getPath(), this.defaultEncoding);
        
        try {
            FtlDocFileParser parser = FtlDocFileParser.getInstance(libFilename, srcFile, defaultLibFormat);
            parser.setMsgHandler(msgHandler);
            parser.addSourcePropertiesToDataModel(dataModel);
            parser.parseSourceTextToDataModel(dataModel, text);
        }
        catch (ParseException e) {
            throw new ParseException("Error parsing file '" + srcFile.toString() + "': " + e.getMessage(), e);
        }
        catch (IllegalArgumentException e) {
            throw new IllegalArgumentException("Error parsing file '" + srcFile.toString() + "': " + e.getMessage(), e);
        }
        return dataModel;
    }
    
    public static abstract class FtlDocFileParser {
        
        protected MsgHandler msgHandler = new VoidMsgHandler();
        
        protected final String libFilename;
        protected final File srcFile;
        
        protected FtlDocFileParser(String libFilename, File srcFile) {
            this.libFilename = libFilename;
            this.srcFile = srcFile;
        }

        public void setMsgHandler(MsgHandler msgHandler) {
            this.msgHandler = msgHandler;
        }

        protected static FtlDocFileParser getInstance(String libFilename, File srcFile, String defaultLibFormat) {
            // TODO?: currently only supports one defaultLibFormat
            if (CATO_LIB_FORMAT.equals(defaultLibFormat)) {
                return new CatoLibFtlDocFileParser(libFilename, srcFile);
            }
            else {
                throw new IllegalArgumentException("Invalid lib format: " + defaultLibFormat);
            }
        }
        
        protected abstract void parseSourceTextToDataModel(Map<String, Object> dataModel, String text) throws ParseException;
        
        protected void addSourcePropertiesToDataModel(Map<String, Object> dataModel) throws IllegalFormatException {
            // get file name only 
            String libTopName = srcFile.getName();
            dataModel.put("libTopName", replaceExtension(libTopName, ""));
            dataModel.put("libFilename", getLibFilename());
            dataModel.put("libFormat", getLibFormat());
            dataModel.put("libName", getLibName());
            dataModel.put("libDocPath", getLibDocPath());
        }
        
        public String getLibFilename() {
            return libFilename;
        }
        
        public String getLibName() {
            return replaceExtension(libFilename, "");
        }
        
        public String getLibDocPath() {
            return replaceExtension(libFilename, outFileExtension);
        }
        
        public abstract String getLibFormat();
    }
    
    /**
     * cato-lib FTL doc format parser.
     * <p>
     * NOTE: many of the regexp assume no trailing spaces in lines.
     * tmplHelper.cleanTextValue removes them.
     */
    public static class CatoLibFtlDocFileParser extends FtlDocFileParser {
    
        protected final CatoLibTemplateHelper tmplHelper = new CatoLibTemplateHelper();
        protected String advancedArgDefaultArgsSuffix = "_defaultArgs";
        
        public CatoLibFtlDocFileParser(String libFilename, File srcFile) {
            super(libFilename, srcFile);
        }
        
        public void setAdvancedArgDefaultArgsSuffix(String advancedArgDefaultArgsSuffix) {
            this.advancedArgDefaultArgsSuffix = advancedArgDefaultArgsSuffix;
        }
        
        @Override
        public String getLibFormat() {
            return CATO_LIB_FORMAT;
        }
        
        private final Pattern commentPat = Pattern.compile("<#--(.*?)-->", Pattern.DOTALL);
        
        // [^\\S\\n] means "whitespace but not newline"
        
        // Delimited by *********, at least 30 stars
        private final Pattern sectionPat = Pattern.compile(
                "[*]{30,}\\n" +
                "[*][^\\S\\n]*([^\\n]+?)(?:[^\\S\\n]*[*])?\\n" +
                "[*]{30,}\\n" +
                "(?:[*]\\s*)?(.*?)"
                , Pattern.DOTALL);
        // Delimited by *******, between 10 and 20 stars
        private final Pattern entryPat = Pattern.compile(
                "[*]{10,20}\\n" +
                "[*][^\\S\\n]*([^\\n]+?)(?:[^\\S\\n]*[*])?\\n" +
                "[*]{10,20}\\n" +
                "(.*?)"
                , Pattern.DOTALL);
    
        /**
         * NOTE: this is very inefficient, but doesn't matter.
         */
        public void parseSourceTextToDataModel(Map<String, Object> dataModel, String fullText) throws ParseException {
            Matcher m;
    
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
            Map<String, Object> sectionInfo = new LinkedHashMap<>();
            sectionInfo.put("name", currentSectionName);
            sectionInfo.put("title", null);
            sectionInfo.put("type", "default");
            sectionInfo.put("comment", "");
            Map<String, Map<String, Object>> sectionEntryMap = new LinkedHashMap<>();
            sectionInfo.put("entryMap", sectionEntryMap);
            
            Map<String, Map<String, Object>> entryMap = new LinkedHashMap<>();
            Map<String, Map<String, Object>> sectionMap = new LinkedHashMap<>();
            sectionMap.put("default", sectionInfo);
            
            while (m.find()) {
                String comment = tmplHelper.cleanTextValue(m.group(1));
                Matcher subm;
                subm = sectionPat.matcher(comment);
                if (subm.matches()) { // NOTE: not .find()
                    currentSectionName = tmplHelper.cleanTextValue(subm.group(1));
                    sectionInfo = new LinkedHashMap<>();
                    sectionInfo.put("name", currentSectionName);
                    sectionInfo.put("title", currentSectionName);
                    sectionInfo.put("type", "sub");
                    sectionInfo.put("comment", tmplHelper.cleanTextValue(subm.group(2)));
                    sectionEntryMap = new LinkedHashMap<>();
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
                            Map<String, Object> entryInfo = parseCatoFtlLibEntry(entryTitle, entryBody, postEntryText);
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
            
            msgHandler.printMsg(" - entries: " + entryMap.size() + ", sections: " + sectionMap.size());
            
            dataModel.put("sectionMap", sectionMap);
            dataModel.put("entryMap", entryMap);
        }
        
        // These must all test for space and newline at beginning
        private final Pattern commentedEntryPostPat = Pattern.compile(
                "^\\s*" +
                "<#--\\s*([^\\n]*?)\\s*" + // NOTE: crazy look-ahead needed
                "(<#[^-].*?)" +
                "\\s*-->"
                , Pattern.DOTALL);
        
        // WARN: postEntryText is NOT cleaned
        protected Map<String, Object> parseCatoFtlLibEntry(String entryTitle, String entryBody, CharSequence postEntryText) throws ParseException {
            Map<String, Object> info = new LinkedHashMap<>();
            info.put("title", entryTitle);
            info.put("body", entryBody); // just in case template needs, usually not
            
            Matcher m;
            
            msgHandler.printDebug("entry " + entryTitle);
            
            //msgHandler.printDebug("postEntryText: " + postEntryText.toString().substring(0, postEntryText.length()));
            
            m = commentedEntryPostPat.matcher(postEntryText);
            if (m.find()) {
                // NOTE: Even if commented, there must be a valid function, var or assign here
                // FIXME: super inefficient !!
                String commentedEntry = tmplHelper.cleanTextValue(m.group(2)); 
                msgHandler.printDebug(" is commented");
                Map<String, Object> funcMacroVarInfo = parseFunctionMacroVarInfo(commentedEntry);
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
                msgHandler.printDebug(" is NOT commented");
                Map<String, Object> funcMacroVarInfo = parseFunctionMacroVarInfo(postEntryText);
                if (funcMacroVarInfo == null) {
                    throw new ParseException("Expected #assign, #function, or #macro declaration, or a commented placeholder, but got nothing");
                }
                info.putAll(funcMacroVarInfo);
                info.put("isCommented", Boolean.FALSE);
            }

            Map<String, Object> entryBodyInfo = parseEntryBody(entryBody);
            info.putAll(entryBodyInfo);
            
            checkEntrySpecialStatus(info);
    
            return info;
        }
        
        protected void checkEntrySpecialStatus(Map<String, Object> info) throws ParseException {
            String commentedImplComment = (String) info.get("commentedImplComment");
            info.put("isTransform", Boolean.FALSE);
            info.put("isImplemented", Boolean.TRUE);
            info.put("isDeprecated", Boolean.FALSE);
            
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
            }
        }
        
        private final Pattern entryBodySectionsPat = Pattern.compile(
                "(?:^|\\n)[^\\S\\n]{1,4}[*][^\\S\\n]+([^\\n]*?)[^\\S\\n]+[*]\\n"
                , Pattern.DOTALL);
        private final Pattern parameterPat = Pattern.compile(
                "(?:^|\\n)[^\\S\\n]{1,8}([^=]+?)[^\\S\\n]*=[^\\S\\n]*?"
                , Pattern.DOTALL);
    
        protected Map<String, Object> parseEntryBody(CharSequence text) throws ParseException {
            Map<String, Object> info = new LinkedHashMap<>();
            
            Matcher secm = entryBodySectionsPat.matcher(text);
            
            Map<String, String> secTitleMap = new LinkedHashMap<>();
    
            int lastEndIndex = 0;
            String lastSecTitle = "Main Description"; // must be trimmed
            
            while(secm.find()) {
                CharSequence lastSecText = text.subSequence(lastEndIndex, secm.start());
                secTitleMap.put(lastSecTitle, lastSecText.toString());
                
                lastSecTitle = tmplHelper.cleanTextValue(secm.group(1));
                lastEndIndex = secm.end();
            }
            
            secTitleMap.put(lastSecTitle, text.subSequence(lastEndIndex, text.length()).toString());
           
            Map<String, Map<String, Object>> entrySections = new LinkedHashMap<>();
            
            int secCount = 0;
            
            for(Map.Entry<String, String> entry : secTitleMap.entrySet()) {
                String secTitle = entry.getKey();
                String secText = entry.getValue();
                String rawSecText = tmplHelper.cleanTextValueNoTrim(secText);
                secText = tmplHelper.cleanTextValue(secText);
                
                Map<String, Object> secInfo = new LinkedHashMap<>();
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
                        info.put("shortDesc", shortDesc);
                        info.put("extraDesc", extraDesc);
                    }
                    else {
                        throw new ParseException("There is no short description for the entry. "
                                + "The first sentence should be separated by two carriage return");
                    }
                }
                else if (secTitle.matches("(?i)Usage\\s+examples?")) {
                    secName = "examples";
                    info.put("exampleText", rawSecText);
                }
                else if (secTitle.matches("(?i)Parameters")) {
                    secName = "parameters";
                    
                    // FIXME: !!! FORGOT THAT PARAMETERS HAVE EXTRA SUB-HEADERS TO COMPLICATE THINGS !!!
                    
                    Map<String, String> parameters = new LinkedHashMap<>();
                    
                    Matcher paramm = parameterPat.matcher(secText);
                    int lastParamEndIndex = 0;
                    String lastParamName = "";
                    String lastParamNameMatch = "";
                    
                    while(paramm.find()) {
                        if (!lastParamName.isEmpty()) {
                            String lastParamVal = tmplHelper.cleanTextValue(secText.substring(lastParamEndIndex, paramm.start()));
                            lastParamVal = tmplHelper.stripIndent(lastParamVal, lastParamNameMatch.length());
                            parameters.put(lastParamName, lastParamVal);
                        }
                        
                        lastParamName = tmplHelper.cleanTextValue(paramm.group(1));
                        lastParamNameMatch = paramm.group();
                        lastParamEndIndex = paramm.end();
                    }   
                    
                    if (!lastParamName.isEmpty()) {
                        String lastParamVal = tmplHelper.cleanTextValue(secText.substring(lastParamEndIndex, secText.length()));
                        lastParamVal = tmplHelper.stripIndent(lastParamVal, lastParamNameMatch.length());
                        parameters.put(lastParamName, lastParamVal);
                    }
                    
                    secInfo.put("parameters", parameters);
                    info.put("parameters", parameters);
                }
                else if (secTitle.matches("(?i)Return\\s+values?")) {
                    secName = "returnValues";
                    info.put("returnValueText", secText);
                }
                else if (secTitle.matches("(?i)Related?")) {
                    secName = "related";
                    String[] relatedNames = secText.split("\\s+");
                    secInfo.put("relatedNames", relatedNames);
                    info.put("relatedNames", relatedNames);
                }
                else {
                    // Unknown, can include anyway
                    secName = "other-" + secCount;
                }
                secInfo.put("sectionName", secName);
                entrySections.put(secName, secInfo);
                secCount++;
            }
            
            info.put("sections", entrySections);
            
            return info;
        }
        
        private final Pattern assignPat = Pattern.compile(
                "^\\s*" +
                "<#assign\\s(\\w+)\\s*=\\s*(\\s[^>]*?)\\s*/?>" 
                , Pattern.DOTALL);
        
        // WARN: text is not cleaned
        protected Map<String, Object> parseFunctionMacroVarInfo(CharSequence text) throws ParseException {
            Matcher m;
            
            m = assignPat.matcher(text);
            if (m.find()) {
                Map<String, Object> info = new LinkedHashMap<>();
                
                // Either a variable or an advanced args pattern
                String varName = tmplHelper.cleanTextValue(m.group(1));
                if (varName.endsWith(advancedArgDefaultArgsSuffix)) {
                    msgHandler.printDebug(" is advanced arg pattern");
                    
                    //msgHandler.printMsg("Has suffix");
                    // FIXME: super inefficient!!!
                    CharSequence postVarText = text.toString().substring(m.end());
                    
                    Map<String, Object> functionMacroInfo = parseFunctionMacroInfo(postVarText);
                    if (functionMacroInfo == null) {
                        throw new ParseException("Expected to find function or macro at this point, but got something else (or nothing)");
                    }
                    info.putAll(functionMacroInfo);
                    
                    // override arguments with special ones from #assign
                    info.put("argStr", tmplHelper.cleanTextValue(m.group(2)));
                    info.put("argList", parseMapArgString(tmplHelper.cleanTextValue(m.group(2))));
                    
                    info.put("isAdvancedArgs", Boolean.TRUE);
                }
                else {
                    msgHandler.printDebug(" is regular var");
                    info.put("type", "variable");
                    info.put("name", varName);
                    info.put("defaultVal", tmplHelper.cleanTextValue(m.group(2))); // NOTE: only var gets default for now
                }
                return info;
            }
            else {
                Map<String, Object> info = parseFunctionMacroInfo(text);
                if (info != null) {
                    info.put("isAdvancedArgs", Boolean.FALSE);
                    return info;
                }
                else {
                    return null;
                }
            }
        }
        
        private final Pattern macroPat = Pattern.compile(
                "^\\s*" +
                "<#macro\\s(\\w+)(\\s[^>]*?)?>(.*?)</#macro>" +
                ".*" // extra needed for .matches()
                , Pattern.DOTALL);
        private final Pattern functionPat = Pattern.compile(
                "^\\s*" +
                "<#function\\s(\\w+)(\\s[^>]*?)?>(.*?)</#function>" +
                ".*" // extra needed for .matches()
                , Pattern.DOTALL);
        
        // WARN: text is not cleaned
        protected Map<String, Object> parseFunctionMacroInfo(CharSequence text) throws ParseException {
            Map<String, Object> info = new LinkedHashMap<>();
            String type;
    
            Matcher m;
            m = macroPat.matcher(text);
            if (m.find()) {
                msgHandler.printDebug(" is macro");
                type = "macro";
                info.put("name", tmplHelper.cleanTextValue(m.group(1)));
                String argStr = tmplHelper.cleanTextValue(m.group(2));
                info.put("argStr", argStr);
                info.put("argList", parseMacroArgString(argStr));
            }
            else {
                msgHandler.printDebug(" is function");
                m = functionPat.matcher(text);
                if (m.find()) {
                    type = "function";
                    info.put("name", tmplHelper.cleanTextValue(m.group(1)));
                    String argStr = tmplHelper.cleanTextValue(m.group(2));
                    info.put("argStr", argStr);
                    info.put("argList", parseFunctionArgString(argStr));
                }
                else {
                    return null;
                }
            }
            
            info.put("type", type);
            return info;
        }
        
        
        private final Pattern mapArgSinglePat = Pattern.compile(
                "(['\"])(.*?)\\1\\s*:.*"
                , Pattern.DOTALL);
        
        /**
         * FIXME: This only supports very basic syntax!!! Will break extremely easily!!!
         * <p>
         * NOTE: INTENTIONALLY omitting default values because generally misleading.
         */
        protected List<String> parseMapArgString(String argStr) throws ParseException {
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
                    msgHandler.printDebug(" text '" + arg + "' could not be matched");
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
        protected List<String> parseFunctionArgString(CharSequence argStr) throws ParseException {
            List<String> argList = new ArrayList<>();
            if (argStr == null) {
                return argList;
            }
            String[] args = argStr.toString().split(",");
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
        protected List<String> parseMacroArgString(CharSequence argStr) throws ParseException {
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

   
    
    /*
     **********************************************************
     * RENDERING *
     **********************************************************
     */
    
    protected void render(Template template, Map<String, Map<String, Object>> srcFileDataModels) throws TemplateException, IOException {
        
        // Create out folder if doesn't exist
        File outFolderFile = new File(this.outFolderPath);
        outFolderFile.mkdirs();
        
        for(Map.Entry<String, Map<String, Object>> entry : srcFileDataModels.entrySet()) {
            Map<String, Object> srcFileDataModel = entry.getValue();
            
            String libDocPath = (String) srcFileDataModel.get("libDocPath");
            
            // Get out file path
            File outFile = new File(outFolderFile, libDocPath);
            
            msgHandler.printMsg("Rendering to output file " + outFile.toString());
            
            // Create any missing sub-folders
            outFile.getParentFile().mkdirs();
            
            FileOutputStream fos = null;
            OutputStreamWriter out = null;
            
            try {
                fos = new FileOutputStream(outFile);
                out = new OutputStreamWriter(fos, this.defaultEncoding);
                
                render(template, srcFileDataModel, srcFileDataModels, out);
            }
            finally {
                if (out != null) {
                    out.close();
                }
                if (fos != null) {
                    fos.close();
                }
            }
        }
    }
    
    protected void render(Template template, Map<String, Object> srcFileDataModel, Map<String, Map<String, Object>> srcFileDataModels, 
            Writer out) throws TemplateException, IOException {
        
        Map<String, Object> dataModel = new HashMap<>();
        dataModel.put("libMap", srcFileDataModels);
        dataModel.putAll(srcFileDataModel);
        
        dataModel.put("tmplHelper", TemplateHelper.getInstance((String) dataModel.get("libFormat")));
        
        template.process(dataModel, out);
    }
    
    
    /*
     **********************************************************
     * TEMPLATING UTILITIES *
     **********************************************************
     */
    
    /**
     * Template helper class that can be used by both the compiler and FTL doc templates.
     * <p>
     * TODO: Some of this code probably belongs in extending class.
     */
    public static abstract class TemplateHelper {
        
        public static TemplateHelper getInstance(String libFormat) {
            if (CATO_LIB_FORMAT.equals(libFormat)) {
                return new CatoLibTemplateHelper();
            }
            else {
                throw new IllegalArgumentException("Unrecognized or missing lib format when trying to find appropriate template helper");
            }
        }
        
        
        public String stripIndent(String text, int charsToRemove) {
            return text.replaceAll("(\\n)[ ]{" + charsToRemove + "}", "$1");
        }
         
        private static final Pattern firstLeadAsterixPat = Pattern.compile("^(\\s*\\n)?[*].*", Pattern.DOTALL);
        // [^\\S\\n] means "whitespace but not newline"
        private static final Pattern leadAsterixPat = Pattern.compile("^[*]([^\\S\\n]|[^\\S\\n]*$)", Pattern.DOTALL + Pattern.MULTILINE);
        
        public String stripCommentLeadingAsterix(String text) {
            if (firstLeadAsterixPat.matcher(text).matches()) {
                return leadAsterixPat.matcher(text).replaceAll("");
            }
            else {
                return text;
            }
        }
        
        private final Pattern firstLinePat = Pattern.compile(
                "^\\s*(.*?)(?:\\n\\n(.*)|[\\s\\n]*?)$"
                , Pattern.DOTALL);
        
        public Matcher getFirstLineMatcher(String text) {
            return firstLinePat.matcher(text);
        }
        
        public String getFirstLine(String text) {
            Matcher m = getFirstLineMatcher(text);
            if (m.matches()) {
                return m.group(1);
            }
            else {
                return "";
            }
        }
        
        public List<String> splitToParagraphs(String text) {
            // if two returns in a row (may be some extra spaces)
            return Arrays.asList(text.split("\\s*\\n\\s*\\n[\\s\\n]*"));
        }
        
        
        /**
         * Trims and removes all trailing whitespace.
         * <p>
         * This should be applied to all text values before passed to FTL, but is also
         * available to FTL.
         */
        public String cleanTextValue(String text) {
            if (text != null) {
                // trim and remove all trailing whitespace
                return text.trim().replaceAll("[^\\S\\n]+(\\n)", "$1");
            }
            else {
                return text;
            }
        }
        
        public String cleanTextValueNoTrim(String text) {
            if (text != null) {
                // remove all trailing whitespace
                return text.replaceAll("[^\\S\\n]+(\\n)", "$1");
            }
            else {
                return text;
            }
        }
        
        private final Pattern bulletPat = Pattern.compile(
                "^([^\\S\\n]*)[*]"
                , Pattern.DOTALL + Pattern.MULTILINE);

        /**
         * WARN: assumes no trailing whitespace
         */
        public boolean hasBulletList(String text) {
            return bulletPat.matcher(text).find();
        }
        
        
        public Map<String, Object> parseAsBulletList(String text) {
            // get the first bullet
            Matcher m = bulletPat.matcher(text);
            if (m.find()) {
                Map<String, Object> listInfo = new LinkedHashMap<>();
                
                int indentSize = m.group(1).length();

                List<String> listItemTexts = new LinkedList<>();
                
                Pattern listItemPat;
                if (indentSize > 0) {
                    listItemPat = Pattern.compile("^([^\\S\\n]{" + indentSize + "})[*]", Pattern.DOTALL + Pattern.MULTILINE);
                }
                else {
                    listItemPat = Pattern.compile("^[*]", Pattern.DOTALL + Pattern.MULTILINE);
                }
                
                // This is basically String.split. 
                // For each item, clean and strip any indent.
                int lastItemEndIndex = 0;
                m = listItemPat.matcher(text);
                while(m.find()) {
                    String itemText = cleanTextValue(text.substring(lastItemEndIndex, m.start()));
                    itemText = stripIndent(itemText, indentSize + 2);
                    listItemTexts.add(itemText);
                    lastItemEndIndex = m.end();
                }
                {
                    String itemText = cleanTextValue(text.substring(lastItemEndIndex));
                    itemText = stripIndent(itemText, indentSize + 2);
                    listItemTexts.add(itemText);
                }
                
                // The first item is actually the title or first text part
                String leadingText = listItemTexts.remove(0);
                listInfo.put("leadingText", leadingText);
                
                List<Object> items = new ArrayList<>();
                
                for(String itemText : listItemTexts) {
                    Map<String, Object> subEntry = parseAsBulletList(itemText);
                    if (subEntry != null) {
                        items.add(subEntry);
                    }
                    else {
                        items.add(itemText);
                    }
                }
                
                listInfo.put("items", items);
                return listInfo;
            }
            else {
                return null;
            }
        }
        
        
        private final Pattern titlePat = Pattern.compile(
                "^(?:[^\\S\\n]*)[*](?:[^\\S\\n]*)([^\\n]*?)(?:[^\\S\\n]*)[*](?:[^\\S\\n]*)$"
                , Pattern.DOTALL);
        
        /**
         * Gets info about an entry/paragraph. Only works for single paragraphs.
         * <p>
         * Returns a map describing the data.
         * <p>
         * Possible types are: 
         * text: simple text value
         * title: a title
         * list: the object will be a list, which may go recursively
         * 
         * @see #splitToParagraphs
         */
        public Map<String, Object> parseDescEntry(String text) {
            Map<String, Object> dataInfo = new LinkedHashMap<>();
            
            Matcher m = titlePat.matcher(text);
            if (m.matches()) {
                dataInfo.put("type", "title");
                dataInfo.put("value", cleanTextValue(m.group(1)));
            }
            else {
                Object listInfo = parseAsBulletList(text);
                if (listInfo != null) {
                    dataInfo.put("type", "list");
                    dataInfo.put("value", listInfo);
                }
                else {
                    dataInfo.put("type", "text");
                    dataInfo.put("value", cleanTextValue(text));
                }

            }

            return dataInfo;
        }
        
        
        @SuppressWarnings("unchecked")
        public Map<String, Object> findEntryGlobal(String nameRef, Map<String, Map<String, Object>> entryMap, 
                Map<String, Map<String, Object>> libMap) {
            String rawName = getEntryNameOnly(nameRef);
            // check in our entry map first
            if (isEntryNameOnly(nameRef) && entryMap.containsKey(rawName)) {
                Map<String, Object> info = new HashMap<>();
                info.put("entry", entryMap.get(rawName));
                info.put("rawName", rawName);
                return info;
            }
            else {
                String libName = getEntryLibLoc(nameRef);
                if (libName != null) {
                    Map<String, Object> dataModel = libMap.get(libName);
                    if (dataModel != null) {
                        entryMap = (Map<String, Map<String, Object>>) dataModel.get("entryMap");
                        if (entryMap.containsKey(rawName)) {
                            Map<String, Object> info = new HashMap<>();
                            info.put("entry", entryMap.get(rawName));
                            info.put("libName", libName);
                            info.put("libDocPath", dataModel.get("libDocPath"));
                            info.put("rawName", rawName);
                            return info;
                        }
                    }
                    return null;
                }
                else {
                    // No full path, must search for best guess, return the first (usually unique but not guaranteed)
                    for(Map.Entry<String, Map<String, Object>> mapEntry : libMap.entrySet()) {
                        libName = mapEntry.getKey();
                        Map<String, Object> dataModel = mapEntry.getValue();
                        entryMap = (Map<String, Map<String, Object>>) dataModel.get("entryMap");
                        if (entryMap.containsKey(rawName)) {
                            Map<String, Object> info = new HashMap<>();
                            info.put("entry", entryMap.get(rawName));
                            info.put("libName", libName);
                            info.put("libDocPath", dataModel.get("libDocPath"));
                            info.put("rawName", rawName);
                            return info;
                        }
                    }
                    return null;
                }
            }
        }
        
        public boolean isEntryNameOnly(String nameRef) {
            return (nameRef.lastIndexOf('@') <= 0) && (nameRef.lastIndexOf('#') <= 0);
        }
        
        public boolean isEntryNameFullPath(String nameRef) {
            return (nameRef.lastIndexOf('@') >= 1) && (nameRef.lastIndexOf('#') >= 1);
        }
        
        public String getEntryNameOnly(String nameRef) {
            int index = nameRef.lastIndexOf('@');
            if (index > 0) {
                return nameRef.substring(index + 1);
            }
            index =  nameRef.lastIndexOf('#');
            if (index > 0) {
                return nameRef.substring(index + 1);
            }
            return nameRef;
        }
        
        public String getEntryLibLoc(String nameRef) {
            int index = nameRef.lastIndexOf('@');
            if (index > 0) {
                return nameRef.substring(0, index);
            }
            index =  nameRef.lastIndexOf('#');
            if (index > 0) {
                return nameRef.substring(0, index);
            }
            return null;
        }
        
        
        public String getTargetRelLibDocPath(String targetLibDocPath, String currLibDocPath) {
            String[] targetParts = targetLibDocPath.split("/");
            String[] currParts = currLibDocPath.split("/");
            if (targetParts.length == currParts.length) {
                return targetLibDocPath;
            }
            else if (targetParts.length > currParts.length) {
                return join(targetParts, "/", targetParts.length - currParts.length, targetParts.length);
            }
            else { // if (targetParts.length < currParts.length) {
                String res = "";
                for(int i=0; i<(currParts.length - targetParts.length); i++) {
                    res += "../";
                }
                return res + targetParts[targetParts.length - 1];
            }
        }
    }
    
    public static class CatoLibTemplateHelper extends TemplateHelper {
        ;
    }
    
    
    /*
     **********************************************************
     * CLASS UTILITIES *
     **********************************************************
     */
    
    public static String readFileAsString(String path, Charset encoding) throws IOException {
        byte[] encoded = Files.readAllBytes(Paths.get(path));
        return new String(encoded, encoding);
    }
    
    private static String validateFilename(String fn) {
        if (fn == null) {
            throw new IllegalArgumentException("Missing filename");
        }
        fn = fn.trim();
        if (fn.isEmpty()) {
            throw new IllegalArgumentException("Missing filename");
        }
        return fn;
    }
    
    public static String replaceExtension(String filePath, String newExt) {
        Matcher m = Pattern.compile("^.*([.][a-zA-Z0-9]+)$").matcher(filePath);
        if (!m.matches()) {
            throw new IllegalArgumentException("Can't replace file extension, has none");
        }
        String ext = m.group(1);
        return filePath.substring(0, filePath.length() - ext.length()) + newExt;
    }
    
    
    private static String join(String[] parts, String sep, int start, int end) {
        String res = parts[start];
        for(int i=start+1; i<end; i++) {
            res += sep + parts[i];
        }
        return res;
    }

    @SuppressWarnings("serial")
    public static class ParseException extends IOException {

        public ParseException() {
            super();
        }

        public ParseException(String message, Throwable cause) {
            super(message, cause);
        }

        public ParseException(String message) {
            super(message);
        }

        public ParseException(Throwable cause) {
            super(cause);
        }
        
    }
    
    public static interface MsgHandler {
        public void printMsg(String msg);
        public void printError(String msg);
        public void printDebug(String msg);
    }
    
    public static class SysOutMsgHandler implements MsgHandler {
        @Override
        public void printMsg(String msg) {
            System.out.println(msg);
        }

        @Override
        public void printError(String msg) {
            System.out.println("ERROR: " + msg);
        }

        @Override
        public void printDebug(String msg) {
            System.out.println(" - " + msg);
        }
    }
    
    public static class VoidMsgHandler implements MsgHandler {
        @Override
        public void printMsg(String msg) {
        }

        @Override
        public void printError(String msg) {
        }

        @Override
        public void printDebug(String msg) {            
        }
    }    

}
