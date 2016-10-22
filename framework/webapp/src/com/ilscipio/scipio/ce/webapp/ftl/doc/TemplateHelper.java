package com.ilscipio.scipio.ce.webapp.ftl.doc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Template helper class that can be used by both the compiler and FTL doc templates.
 * <p>
 * An instance of this is stored as tmplHelper in data model.
 * <p>
 * TODO: Some of this code probably belongs in extending class.
 */
public abstract class TemplateHelper {
    
    protected MsgHandler msgHandler = new MsgHandler.VoidMsgHandler();
    
    protected final String inFileExtension;
    protected final String outFileExtension;
    
    protected TemplateHelper(String inFileExtension, String outFileExtension) {
        this.inFileExtension = (inFileExtension != null) ? inFileExtension : "";
        this.outFileExtension = (outFileExtension != null) ? outFileExtension : "";
    }

    public void setMsgHandler(MsgHandler msgHandler) {
        this.msgHandler = msgHandler;
    }


    public static TemplateHelper getInstance(String libFormat, String inFileExtension, String outFileExtension) {
        if (FtlDocCompiler.SCIPIO_LIB_FORMAT.equals(libFormat)) {
            return new ScipioLibTemplateHelper(inFileExtension, outFileExtension);
        }
        else {
            throw new IllegalArgumentException("Unrecognized or missing lib format when trying to find appropriate template helper");
        }
    }
    
    public String getInFileExtension() {
        return inFileExtension;
    }

    public String getOutFileExtension() {
        return outFileExtension;
    }

    public String stripIndent(String text, int charsToRemove) {
        return text.replaceAll("(\\n)[ ]{" + charsToRemove + "}", "$1");
    }
     
    private static final Pattern firstLeadAsterixPat = Pattern.compile("^(\\s*\\n)?[*].*", Pattern.DOTALL);
    private static final Pattern leadAsterixPat = Pattern.compile("^[*]([ ]|[ ]*$)", Pattern.DOTALL + Pattern.MULTILINE);
    
    public String stripCommentLeadingAsterix(String text) {
        if (firstLeadAsterixPat.matcher(text).matches()) {
            return leadAsterixPat.matcher(text).replaceAll("");
        }
        else {
            return text;
        }
    }
    
    private static final Pattern firstLinePat = Pattern.compile(
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
     * Abstracted clean method, should be called for individual text values.
     * <p>
     * This should be applied to all text values before passed to FTL, but is also
     * available to FTL.
     */
    public String cleanTextValue(String text) {
        if (text != null) {
            // trim and remove all trailing whitespace
            // Assume this was done at beginning of doc
            //return text.trim().replaceAll("[^\\S\\n]+(\\n)", "$1");
            return text.trim();
        }
        else {
            return text;
        }
    }
    
    /**
     * Will never remove leading whitespace...
     */
    public String cleanTextValueSafe(String text) {
        if (text != null) {
            // remove all trailing whitespace
            // Assume this was done at beginning of doc
            //return text.replaceAll("[^\\S\\n]+(\\n)", "$1");
            return text;
        }
        else {
            return text;
        }
    }
    
    
    /**
     * Normalizes text by removing all trailing spaces and converting tabs to spaces.
     * <p>
     * Usually this can be done once at the beginning of the document and never worry
     * about it again.
     */
    public String normalizeText(String text) {
        return text.replaceAll("\\t", "    ").replaceAll("[^\\S\\n]+(\\n)", "$1").replaceAll("[^\\S\\n]", " ");
    }
    

    @SuppressWarnings("unchecked")
    public Map<String, Object> findEntryGlobal(String nameRef, Map<String, Map<String, Object>> entryMap, 
            Map<String, Map<String, Object>> libMap) {
        String rawName = getEntryNameOnly(nameRef);
        // check in our entry map first
        if (isEntryNameOnly(nameRef) && entryMap.containsKey(rawName)) {
            Map<String, Object> info = FtlDocFileParser.makeObjectMap();
            info.put("entry", entryMap.get(rawName));
            info.put("rawName", rawName);
            info.put("name", nameRef); // save the orig name for convenience
            info.put("type", "entryref");
            info.put("origText", nameRef);
            return info;
        }
        else {
            String libName = getEntryLibLoc(nameRef);
            if (libName != null) {
                Map<String, Object> dataModel = libMap.get(libName);
                if (dataModel != null) {
                    entryMap = (Map<String, Map<String, Object>>) dataModel.get("entryMap");
                    if (entryMap.containsKey(rawName)) {
                        Map<String, Object> info = FtlDocFileParser.makeObjectMap();
                        info.put("entry", entryMap.get(rawName));
                        info.put("libName", libName);
                        info.put("libDocPath", dataModel.get("libDocPath"));
                        info.put("rawName", rawName);
                        info.put("name", nameRef); // save the orig name for convenience
                        info.put("type", "entryref");
                        info.put("origText", nameRef);
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
                        Map<String, Object> info = FtlDocFileParser.makeObjectMap();
                        info.put("entry", entryMap.get(rawName));
                        info.put("libName", libName);
                        info.put("libDocPath", dataModel.get("libDocPath"));
                        info.put("rawName", rawName);
                        info.put("name", nameRef); // save the orig name for convenience
                        info.put("type", "entryref");
                        info.put("origText", nameRef);
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
        return (nameRef.lastIndexOf('@') >= 1) || (nameRef.lastIndexOf('#') >= 1);
    }
    
    public String getEntryNameOnly(String nameRef) {
        int index = nameRef.lastIndexOf('@');
        if (index >= 0) {
            return nameRef.substring(index + 1);
        }
        index =  nameRef.lastIndexOf('#');
        if (index >= 0) {
            return nameRef.substring(index + 1);
        }
        return nameRef;
    }
    
    public String getEntryLibLoc(String nameRef) {
        String res = null;
        int index = nameRef.indexOf('@');
        if (index > 0) {
            res = nameRef.substring(0, index);
        }
        else {
            index = nameRef.indexOf('#');
            if (index > 0) {
                res = nameRef.substring(0, index);
            }
        }
        if (res != null && !inFileExtension.isEmpty()) {
            if (res.endsWith(inFileExtension)) {
                res = res.substring(0, res.length() - inFileExtension.length());
            }
        }
        return res;
    }
    
    public String getTargetRelLibDocPath(String targetLibDocPath, String currLibDocPath) {
        String[] currParts = currLibDocPath.split("/");
        String res = "";
        for(int i = 0; i < (currParts.length - 1); i++) {
            res += "../";
        }
        return res + targetLibDocPath;
    }
    
    
    private static final Pattern simpleWordPat = Pattern.compile("[a-zA-Z0-9]+");
    
    /**
     * This makes a cleaned camel-case name from an English title, 
     * using simple-character words extracted from it (only).
     */
    public String makeCamelCaseNameFromTitle(String title) {
        StringBuilder sb = new StringBuilder();
        Matcher m = simpleWordPat.matcher(title);
        while (m.find()) {
            String val = m.group();
            if (sb.length() == 0) {
                sb.append(val.toLowerCase());
            }
            else {
                sb.append(val.substring(0, 1).toUpperCase());
                sb.append(val.substring(1).toLowerCase());
            }
        }
        return sb.toString();
    }
    
    /**
     * Gets the index of the given closing char in the string.
     * Supports nesting. start should be the open char index + 1.
     */
    public int getClosingCharIndex(String text, int start, char openChar, char closingChar) {
        int i = start;
        while(i < text.length()) {
            char curr = text.charAt(i);
            if (curr == closingChar) {
                return i;
            }
            else if (curr == openChar) {
                // Recursive for nesting
                i = getClosingCharIndex(text, i+1, openChar, closingChar);
                if (i < 0) {
                    return -1;
                }
            }
            i++;
        }
        return -1;
    } 
    
    
    public String makeRegexRangeExpr(int minSize, int maxSize) {
        if (minSize > 0) {
            if (maxSize < 0) {
                if (minSize == 1) {
                    return "+";
                }
                else {
                    return "{" + minSize + ",}";
                }
            }
            else if (maxSize > minSize) {
                return "{" + minSize + "," + maxSize + "}";
            }
            else if (maxSize == minSize) {
                return "{" + minSize + "}";
            }
            else {
                throw new IllegalArgumentException("Invalid regexp end range: maxSize: " + maxSize + "; minSize: " + minSize);
            }
        }
        else {
            if (maxSize < 0) {
                return "*";
            }
            else if (maxSize == 0) {
                // is this valid? whatever
                return "{0}";
            }
            else {
                return "{0," + maxSize + "}";
            }
        }
    }
    
    /**
     * Entries are string for text, map with groups for match.
     * <p>
     * Beware dep on regexp, may not consume newlines and empty entries between consec lines.
     */
    public List<Object> splitByPat(String text, Pattern pat, Map<String, Object> modelMap, String... groupKeyNames) {
        List<Object> res = new ArrayList<>();
        
        Matcher m = pat.matcher(text);
        
        int lastEndIndex = 0;
        
        while(m.find()) {
            // add intermediate text
            if (lastEndIndex < m.start()) {
                res.add(cleanTextValueSafe(text.substring(lastEndIndex, m.start())));
            }
            
            Map<String, Object> info = FtlDocFileParser.makeObjectMap(modelMap);
            // Save group values (by index)
            for(int i = 0; i < groupKeyNames.length ; i++) {
                if (i < groupKeyNames.length && groupKeyNames[i] != null && !groupKeyNames[i].isEmpty()) {
                    info.put(groupKeyNames[i], m.group(i));
                }
            }
            
            // add the entry
            res.add(info);
            
            //if (pat == ScipioLibTemplateHelper.codeTextPat) {
            //    msgHandler.logDebug("code-text found: " + info);
            //}
            
            lastEndIndex = m.end();
        }
        
        // add last text part
        if (lastEndIndex < text.length()) {
            res.add(cleanTextValueSafe(text.substring(lastEndIndex)));
        }
        
        return res;
    }
}