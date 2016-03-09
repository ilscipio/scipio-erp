package com.ilscipio.cato.ce.webapp.ftl;

import java.io.File;
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
 * main arguments:
 * [base source folder] [series of relative source file paths, at least one] [FTL doc-formatting template location] [output folder]
 * <p>
 * This is currently compiled as part of the Ofbiz compilation, but
 * may (should) be invoked independently (FIXME?).
 * <p>
 * DEV NOTE: <strong>WARN</strong>: Don't use Ofbiz libraries here.
 * <p>
 * TODO: License
 */
public class FtlDocCompiler {

    protected String baseFolderPath = null;
    protected List<String> libFilenames = null;
    protected String templatePath = null;
    protected String outFolderPath = null;
    
    protected final String inFileExtension = ".ftl";
    protected final String outFileExtension = ".html";
    
    protected Configuration cfg = null;
    
    // Parsing
    protected final String advancedArgDefaultArgsSuffix = "_defaultArgs";


    public FtlDocCompiler() {
    }
    
    public void setSources(String baseFolderPath, List<String> libFilenames, String templatePath) {
        this.baseFolderPath = validateFilename(baseFolderPath);
        this.libFilenames = new ArrayList<String>(libFilenames.size());
        for(String filename : libFilenames) {
            String validFn = validateFilename(filename);
            if (!validFn.endsWith(inFileExtension)) {
                throw new IllegalArgumentException("Input file " + filename + " does not end in .ftl");
            }
            this.libFilenames.add(validateFilename(filename));
        }
        this.templatePath = validateFilename(templatePath);
    }
    
    public void setOutputFolder(String outFolderPath) {
        this.outFolderPath = validateFilename(outFolderPath);
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
    
    /**
     * Gets default instance, which parses Ilscipio Cato macro doc format.
     */
    public static FtlDocCompiler getInstance() {
        return new FtlDocCompiler();
    }


    public static void main(String[] args) {
        // TODO: formalize this
        if (args.length >= 5) {
            String baseFolderPath = args[1];
            List<String> libFilenames = Arrays.asList(args).subList(1, args.length - 2);
            String templatePath = args[args.length - 2];
            String outFolderPath = args[args.length - 1];

            try {
                FtlDocCompiler compiler = FtlDocCompiler.getInstance();
                System.out.println("Setting sources and output");
                compiler.setSources(baseFolderPath, libFilenames, templatePath);
                compiler.setOutputFolder(outFolderPath);
                
                System.out.println("Initializing");
                compiler.init();
                
                System.out.println("Compiling (parsing and rendering)");
                compiler.compile();
                System.out.println("Compilation complete.");
                
            } catch (Exception e) {
                System.out.println("Error during compilation: " + e.getMessage());
                e.printStackTrace();
                System.exit(1);
            }
        }
        else {
            System.out.println("Insufficient arguments. Expecting: [input folder] [-series of relative filenames-] [template path] [output folder]");
            System.exit(1);
        }
    }
 
    public void init() throws Exception {
         cfg = new Configuration(Configuration.VERSION_2_3_22);
         cfg.setDirectoryForTemplateLoading(new File("."));
         cfg.setDefaultEncoding("UTF-8");
         cfg.setTemplateExceptionHandler(TemplateExceptionHandler.HTML_DEBUG_HANDLER);
    }
    
    
    public void compile() throws Exception {
        Map<String, Map<String, Object>> srcFileDataModels = parseSourcesToDataModels();

        Template template = cfg.getTemplate(this.templatePath);
        
        render(template, srcFileDataModels);
    }
    
    /* PARSING TO DATA MODEL */
    
    
    /**
     * For each input file, creates a data model. 
     * 
     * @return a map of file names to data models
     */
    protected Map<String, Map<String, Object>> parseSourcesToDataModels() throws IOException {
        Map<String, Map<String, Object>> srcFileDataModels = new LinkedHashMap<>();
        
        for(String libFilename : this.libFilenames) {
            String srcFilePath = Paths.get(this.baseFolderPath, libFilename).toString();
            Map<String, Object> dataModel = parseSourceToDataModel(libFilename, srcFilePath);
            srcFileDataModels.put(libFilename, dataModel);
        }
        return srcFileDataModels;
    }
    
    protected Map<String, Object> parseSourceToDataModel(String libFilename, String srcFilePath) throws IOException {
        // Super lazy, load entire file as a string.
        Map<String, Object> dataModel = new HashMap<>();
        File srcFile = new File(srcFilePath);
        String text = readFileAsString(srcFilePath, StandardCharsets.UTF_8);
        try {
            addSourcePropertiesToDataModel(dataModel, libFilename, srcFile);
            parseSourceTextToDataModel(dataModel, text);
        }
        catch (ParseException e) {
            throw new IOException("Error parsing file '" + srcFile.toString() + "': " + e.getMessage(), e);
        }
        return dataModel;
    }
    
    protected void parseSourceTextToDataModel(Map<String, Object> dataModel, String text) throws ParseException {
        parseCatoFtlLibDocToDataModel(dataModel, text);
    }
    
    
    private static final Pattern commentPat = Pattern.compile("<#--(.*?)-->", Pattern.DOTALL);
    // Delimited by *********, at least 30 stars
    private static final Pattern sectionPat = Pattern.compile(
            "\\s*" +
            "[*]{30,}\\s*\\n\\s*" +
            "[*]\\s*(.*?)\\s*[*]\\s*\\n" +
            "[*]{30,}\\s+\\n\\s*" +
            "(?:[*]\\s*)?(.*?)" + 
            "\\s*"
            , Pattern.DOTALL);
    // Delimited by *******, between 10 and 20 stars
    private static final Pattern entryPat = Pattern.compile(
            "\\s*" +
            "[*]{10,20}\\s*\\n\\s*" +
            "[*]\\s*(.*?)\\s*[*]\\s*\\n" +
            "[*]{10,20}\\s+\\n\\s*" +
            "(.*?)" +
            "\\s*"
            , Pattern.DOTALL);

    /**
     * NOTE: this is very inefficient, but doesn't matter.
     */
    protected void parseCatoFtlLibDocToDataModel(Map<String, Object> dataModel, String fullText) throws ParseException {
        Matcher m;

        // First comment is description
        m = commentPat.matcher(fullText);
        if (!m.find()) {
            throw new ParseException("Missing file top comment");
        }
        
        String introComment = m.group(1).trim();
        dataModel.put("introComment", introComment);
        
        
        String currentSectionName = "default";
        Map<String, Object> sectionInfo = new LinkedHashMap<>();
        sectionInfo.put("name", currentSectionName);
        sectionInfo.put("type", "default");
        sectionInfo.put("comment", "");
        
        
        Map<String, Map<String, Object>> entries = new LinkedHashMap<>();
        Map<String, Map<String, Object>> sectionMap = new LinkedHashMap<>();
        sectionMap.put("default", sectionInfo);
        
        while (m.find()) {
            String comment = m.group(1).trim();
            Matcher subm;
            subm = sectionPat.matcher(comment);
            if (subm.matches()) {
                currentSectionName = subm.group(1).trim();
                sectionInfo = new LinkedHashMap<>();
                sectionInfo.put("name", currentSectionName);
                sectionInfo.put("type", "sub");
                sectionInfo.put("comment", subm.group(2).trim());
                sectionMap.put(currentSectionName, sectionInfo);
            }
            else {
                subm = entryPat.matcher(comment);
                if (subm.matches()) {
                    String entryTitle = subm.group(1).trim();
                    String entryBody = subm.group(2).trim();
                    String postEntryText = fullText.substring(m.end()); // FIXME: This is ridiculous inefficient
                    try {
                        Map<String, Object> entryInfo = parseCatoFtlLibEntry(entryTitle, entryBody, postEntryText);
                        entryInfo.put("sectionName", currentSectionName);
                        entries.put(entryTitle, entryInfo);
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
        
    }
    
    // These must all test for space and newline at beginning
    private static final Pattern commentedEntryPostPat = Pattern.compile(
            "(?:\\s|\\n)*" +
            "<#--\\s*(.*?)\\n" + // WARN: THERE MUST BE A NEWLINE HERE
            "(.*?)" +
            "\\s*-->"
            , Pattern.DOTALL);
    private static final Pattern assignPat = Pattern.compile(
            "(?:\\s|\\n)*" +
            "<#assign\\s(\\w+)\\s*=\\s*(\\s[^>]*?)\\s*/?>"
            , Pattern.DOTALL);
    private static final Pattern macroPat = Pattern.compile(
            "(?:\\s|\\n)*" +
            "<#macro\\s(\\w+)(\\s[^>]*?)?>(.*?)</#macro>"
            , Pattern.DOTALL);
    private static final Pattern functionPat = Pattern.compile(
            "(?:\\s|\\n)*" +
            "<#function\\s(\\w+)(\\s[^>]*?)?>(.*?)</#function>"
            , Pattern.DOTALL);
    
    protected Map<String, Object> parseCatoFtlLibEntry(String entryTitle, String entryBody, CharSequence postEntryText) throws ParseException {
        Map<String, Object> info = new LinkedHashMap<>();
        info.put("entryTitle", entryTitle);
        info.put("entryBody", entryBody); // just in case
        
        Matcher m;
        
        m = commentedEntryPostPat.matcher(postEntryText);
        if (m.matches()) {
            // NOTE: Even if commented, there must be a valid function, var or assign here
            // FIXME: super inefficient !!
            String commentedEntry = m.group(2).trim();
            Map<String, Object> funcMacroVarInfo = parseFunctionMacroVarInfo(commentedEntry);
            if (funcMacroVarInfo == null) {
                throw new ParseException("Expected a commented #assign, #function, or #macro declaration, but got nothing; please make sure " +
                        "even commented code still contains valid Freemarker and a placeholder commented entry remains; please make sure there "
                        + "is not an extra comment between the main entry documentation and the declaration (or commented declaration)");
            }
            info.putAll(funcMacroVarInfo);
            info.put("isCommented", true);
            info.put("commentedImplComment", m.group(1).trim());
        }
        else {
            Map<String, Object> funcMacroVarInfo = parseFunctionMacroVarInfo(postEntryText);
            if (funcMacroVarInfo == null) {
                throw new ParseException("Expected #assign, #function, or #macro declaration, or a commented placeholder, but got nothing");
            }
            info.putAll(funcMacroVarInfo);
            info.put("isCommented", false);

        }
        
        Map<String, Object> entryBodyInfo = parseEntryBody(entryBody);
        info.putAll(entryBodyInfo);

        return info;
    }
    
    
    // TODO? this regexp probably won't work
    private static final Pattern entryBodySectionsPat = Pattern.compile(
            "\\n\\s{2,4}[*]\\s+(.*?)\\s+[*]\\s*\\n"
            , Pattern.DOTALL);
    private static final Pattern shortDescPat = Pattern.compile(
            "^(.*?[.])(?:\n\n|\\s*$)"
            , Pattern.DOTALL);
    private static final Pattern parameterPat = Pattern.compile(
            "(?:^|\\n)\\s{2,8}([^=]+?)\\s*=\\s*?"
            , Pattern.DOTALL);

    protected Map<String, Object> parseEntryBody(CharSequence text) throws ParseException {
        Map<String, Object> info = new LinkedHashMap<>();
        
        Matcher secm = entryBodySectionsPat.matcher(text);
        
        Map<String, String> secTitleMap = new LinkedHashMap<>();

        int lastEndIndex = 0;
        String lastSecTitle = "Main Description"; // must be trimmed
        
        while(secm.find()) {
            CharSequence lastSecText = text.subSequence(lastEndIndex, secm.start());
            secTitleMap.put(lastSecTitle, lastSecText.toString().trim());
            
            lastSecTitle = secm.group(1).trim();
            lastEndIndex = secm.end();
        }
        
        secTitleMap.put(lastSecTitle, text.subSequence(lastEndIndex, text.length()).toString().trim());
       
        Map<String, Map<String, Object>> entrySections = new LinkedHashMap<>();
        
        int secCount = 0;
        
        for(Map.Entry<String, String> entry : secTitleMap.entrySet()) {
            String secTitle = entry.getKey();
            String secText = entry.getValue();
            
            Map<String, Object> secInfo = new LinkedHashMap<>();
            secInfo.put("sectionTitle", secTitle);
            secInfo.put("sectionText", secText);
            
            String secName;
            if (secTitle.matches("(?i)Main Description")) {
                secName = "mainDesc";
                // First sentence is the short desc
                Matcher shortdescm = shortDescPat.matcher(secText);
                if (shortdescm.matches()) {
                    secInfo.put("shortDesc", shortdescm.group(1).trim());
                    secInfo.put("extraDesc", secText.substring(shortdescm.end()));
                }
                else {
                    throw new ParseException("There is no short description for the entry. "
                            + "The first sentence should be separated by two carriage return");
                }
            }
            else if (secTitle.matches("(?i)Usage\\s+examples?")) {
                secName = "examples";
            }
            else if (secTitle.matches("(?i)Parameters")) {
                secName = "parameters";
                
                Map<String, String> parameters = new LinkedHashMap<>();
                
                Matcher paramm = parameterPat.matcher(secText);
                int lastParamEndIndex = 0;
                String lastParamName = "";
                String lastParamNameMatch = "";
                
                while(paramm.find()) {
                    if (!lastParamName.isEmpty()) {
                        String lastParamVal = secText.substring(lastParamEndIndex, paramm.start()).trim();
                        lastParamVal = stripTextIntendation(lastParamVal, lastParamNameMatch.length());
                        parameters.put(lastParamName, lastParamVal);
                    }
                    
                    lastParamName = paramm.group(1).trim();
                    lastParamNameMatch = paramm.group();
                    lastParamEndIndex = paramm.end();
                }   
                
                if (!lastParamName.isEmpty()) {
                    String lastParamVal = secText.substring(lastParamEndIndex, secText.length()).trim();
                    lastParamVal = stripTextIntendation(lastParamVal, lastParamNameMatch.length());
                    secTitleMap.put(lastParamName, lastParamVal);
                }
                
                secInfo.put("parameters", parameters);
            }
            else if (secTitle.matches("(?i)Return\\s+values?")) {
                secName = "returnValues";
            }
            else if (secTitle.matches("(?i)Related?")) {
                secName = "related";
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
    
    protected static String stripTextIntendation(String text, int charsToRemove) {
        return text.replaceAll("\\n[ ]{" + charsToRemove + "}", "");
    }
    
    
    protected Map<String, Object> parseFunctionMacroVarInfo(CharSequence text) throws ParseException {

        Matcher m;
        m = assignPat.matcher(text);
        if (m.matches()) {
            Map<String, Object> info = new LinkedHashMap<>();
            
            // Either a variable or an advanced args pattern
            String varName = m.group(1).trim();
            if (varName.endsWith(advancedArgDefaultArgsSuffix)) {
                // FIXME: super inefficient!!!
                CharSequence postVarText = text.toString().substring(m.end());
                
                Map<String, Object> functionMacroInfo = parseFunctionMacroInfo(postVarText);
                if (functionMacroInfo == null) {
                    throw new ParseException("Expected to find function or macro at this point");
                }
                info.putAll(functionMacroInfo);
                
                // override arguments with special ones from #assign
                info.put("argStr", m.group(2).trim());
                info.put("argList", parseMapArgString(m.group(2).trim()));
            }
            else {
                info.put("type", "variable");
                info.put("name", varName);
                info.put("defaultVal", m.group(2).trim()); // NOTE: only var gets default for now
            }
        }
        else {
            Map<String, Object> functionMacroInfo = parseFunctionMacroInfo(text);
            if (functionMacroInfo != null) {
                return functionMacroInfo;
            }
        }

        return null;
    }
    
    protected Map<String, Object> parseFunctionMacroInfo(CharSequence text) throws ParseException {
        Map<String, Object> info = new LinkedHashMap<>();
        String type ;

        Matcher m;
        m = macroPat.matcher(text);
        if (m.matches()) {
            type = "macro";
            info.put("name", m.group(1).trim());
            info.put("argStr", m.group(2).trim());
            info.put("argList", parseMacroArgString(m.group(2).trim()));
        }
        else {
            m = functionPat.matcher(text);
            if (m.matches()) {
                type = "function";
                info.put("name", m.group(1).trim());
                info.put("argStr", m.group(2).trim());
                info.put("argList", parseFunctionArgString(m.group(2).trim()));
            }
            else {
                return null;
            }
        }
        
        info.put("type", type);
        return info;
    }
    
    
    private static final Pattern mapArgSinglePat = Pattern.compile(
            "(['\"])(.*?)\1\\s*:.*"
            , Pattern.DOTALL);
    
    /**
     * FIXME: This only supports very basic syntax!!! Will break extremely easily!!!
     * <p>
     * NOTE: INTENTIONALLY omitting default values because generally misleading.
     */
    protected List<String> parseMapArgString(String argStr) throws ParseException {
        argStr = argStr.trim();
        if (argStr.startsWith("{")) {
            argStr = argStr.substring(1).trim();
        }
        if (argStr.startsWith("}")) {
            argStr = argStr.substring(0, argStr.length() - 1).trim();
        }
        
        List<String> argList = new ArrayList<>();
        String[] args = argStr.toString().split(",");
        for (String argEntry : args) {
            String arg = argEntry.trim();
            Matcher m = mapArgSinglePat.matcher(arg);
            if (m.matches()) {
                String name = m.group(2).trim();
                argList.add(name);
            }
            else {
                throw new ParseException("Error parsing map variable (in #assign)");
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

    protected void addSourcePropertiesToDataModel(Map<String, Object> dataModel, String libFilename, File srcFile) throws IllegalFormatException {
        // get file name only 
        String libTopName = srcFile.getName();
        dataModel.put("libTopName", replaceExtension(libTopName, ""));
    }
    
    
    /* RENDERING */
    
    protected void render(Template template, Map<String, Map<String, Object>> srcFileDataModels) throws TemplateException, IOException {
        
        // Create out folder if doesn't exist
        File outFolderFile = new File(this.outFolderPath);
        outFolderFile.mkdirs();
        
        for(Map.Entry<String, Map<String, Object>> entry : srcFileDataModels.entrySet()) {
            String libFilename = entry.getKey();
            Map<String, Object> srcFileDataModel = entry.getValue();
            
            // Get out file path
            File outFile = new File(outFolderFile, replaceExtension(libFilename, outFileExtension));
            
            // Create any missing sub-folders
            outFile.getParentFile().mkdirs();
            
            FileOutputStream fos = null;
            OutputStreamWriter out = null;
            
            try {
                fos = new FileOutputStream(outFile);
                out = new OutputStreamWriter(fos, StandardCharsets.UTF_8);
                
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
        
        template.process(dataModel, out);
    }
    

    public static String replaceExtension(String filePath, String newExt) {
        Matcher m = Pattern.compile("^.*([.][a-zA-Z0-9]+)$").matcher(filePath);
        if (!m.matches()) {
            throw new IllegalArgumentException("Can't replace file extension, has none");
        }
        String ext = m.group(1);
        return filePath.substring(0, filePath.length() - ext.length()) + newExt;
    }
    
    
    /* UTILITIES */
    
    public static String readFileAsString(String path, Charset encoding) throws IOException {
        byte[] encoded = Files.readAllBytes(Paths.get(path));
        return new String(encoded, encoding);
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
    
}
