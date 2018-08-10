package com.ilscipio.scipio.ce.webapp.ftl.doc;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * scipio-lib template helper.
 * <p>
 * An instance of this is stored as tmplHelper in data model.
 */
public class ScipioLibTemplateHelper extends TemplateHelper {
    // NOTE: some of the Parser methods could actually be moved here, but don't need for now
    
    public ScipioLibTemplateHelper(String inFileExtension, String outFileExtension) {
        super(inFileExtension, outFileExtension);
    }

    private static final Pattern bulletPat = Pattern.compile(
            "^([ ]*)[*]"
            , Pattern.DOTALL + Pattern.MULTILINE);

    /**
     * WARN: assumes no trailing whitespace
     */
    public boolean hasBulletList(String text) {
        return bulletPat.matcher(text).find();
    }
    
    
    private static final Pattern textLibEntryRefPat = Pattern.compile(
            "([a-zA-Z0-9][a-zA-Z0-9/._-]*[a-zA-Z0-9])?([@#])([a-zA-Z0-9_]{2,})"
            , Pattern.DOTALL);
    
    /**
     * Parses the text for lib-entry like references, basically those containing
     * "@" or "#" characters, and returns a list where each entry is either
     * a piece of text or a map describing a link to an entry.
     */
    public List<Object> splitByLibEntryRefs(String text, Map<String, Map<String, Object>> entryMap, 
            Map<String, Map<String, Object>> libMap) {
        List<Object> textList = new ArrayList<>();
        
        Matcher m = textLibEntryRefPat.matcher(text);
        
        int lastEndIndex = 0;
        while(m.find()) {
            // Add text in between
            if (lastEndIndex < m.start()) {
                textList.add(text.substring(lastEndIndex, m.start()));
            }
            
            String ref = m.group();
            
            Map<String, Object> entryInfo = findEntryGlobal(ref, entryMap, libMap);
            if (entryInfo != null) {
                textList.add(entryInfo);
            }
            else {
                // Got nothing. Just add as regular text, BUT if the previous was regular
                // text, combine it so don't affect other parsing
                if (textList.size() > 0) {
                    Object lastText = textList.get(textList.size() - 1);
                    if (lastText instanceof String) {
                        textList.remove(textList.size() - 1);
                        textList.add(((String) lastText) + ref);
                    }
                    else {
                        textList.add(ref);
                    }
                }
                else {
                    textList.add(ref);
                }
            }

            lastEndIndex = m.end();
        }
        
        // Add last part, but combine with previous if was only text
        if (lastEndIndex < text.length()) {
            String finalPart = text.substring(lastEndIndex);
            if (textList.size() > 0) {
                Object lastText = textList.get(textList.size() - 1);
                if (lastText instanceof String) {
                    textList.remove(textList.size() - 1);
                    textList.add(((String) lastText) + finalPart);
                }
                else {
                    textList.add(finalPart);
                }
            }
            else {
                textList.add(finalPart);
            }
        }
        return textList;
    }

    
    private static final Pattern rawTextPat = Pattern.compile(
            "\\[\\[\\[(.*?)\\]\\]\\]"
            , Pattern.DOTALL);
    private static final Pattern plainTextPat = Pattern.compile(
            "\\(\\(\\((.*?)\\)\\)\\)"
            , Pattern.DOTALL);
    
    private static final Pattern codeTextPat = Pattern.compile(
            "\\{\\{\\{(.*?)\\}\\}\\}"
            , Pattern.DOTALL);
    
    private static final Pattern linkManualPat = Pattern.compile(
            ">>>(.*?)<<<"
            , Pattern.DOTALL);
    
    private static final Pattern linkAutoPat = Pattern.compile(
            "(^|\\b)((https?|file)://([^)\\s\\n])+)"
            , Pattern.DOTALL);
    
    @SuppressWarnings("unchecked")
    private Object fixupLink(Object linkObj, Map<String, Map<String, Object>> entryMap, 
            Map<String, Map<String, Object>> libMap, Map<String, Object> libInfo) {
        if (linkObj instanceof Map && ((Map<String, Object>) linkObj).get("type").equals("link")) {
            Map<String, Object> linkInfo = (Map<String, Object>) linkObj;
            
            msgHandler.logDebug(
                    "====================================\n" + 
                    "linkInfo: " + linkInfo.toString() + "\n" +
                    "====================================");
            
            // FIXME?: Currently only support href==label
            String value = (String) linkInfo.get("value");
            
            if (value.startsWith("http://") || value.startsWith("https://") || value.startsWith("file://") || value.startsWith("//")) {
                // absolute, do nothing
                linkInfo.put("text", value);
            }
            else if (value.startsWith("/")) {
                // absolute to server, do nothing
                linkInfo.put("text", value);
            }
            else if (value.startsWith("./")) {
                // relative to current, just strip the prefix
                value = value.substring(2);
                linkInfo.put("text", value);
                
                // 2016-10-27: check if this is a valid doc link, so template can process it
                if (libMap.containsKey(value) || 
                        (value.endsWith(inFileExtension) && libMap.containsKey(value.substring(0, value.length() - inFileExtension.length())))) {
                    linkInfo.put("isDocLink", Boolean.TRUE);
                }
            }
            else if (value.startsWith("../")) {
                // relative to current
                // WARN: this currently bypasses the auto-doc stuff
                linkInfo.put("text", value);
            }
            else {
                //String libDocPath = (String) libInfo.get("libDocPath");
                linkInfo.put("text", value);
                // 2016-10-27: let the template process the link instead
                // relative to doc root. need to adjust the link.
                //value = getTargetRelLibDocPath(value, libDocPath);
                
                // 2016-10-27: check if this is a valid doc link, so template can process it
                if (libMap.containsKey(value) || 
                        (value.endsWith(inFileExtension) && libMap.containsKey(value.substring(0, value.length() - inFileExtension.length())))) {
                    linkInfo.put("isDocLink", Boolean.TRUE);
                }
            }
            if (value.endsWith(inFileExtension)) {
                value = value.substring(0, value.length() - inFileExtension.length()) + outFileExtension;
            }
            linkInfo.put("value", value);
            return linkInfo;
        }
        else {
            return linkObj;
        }
    }
    
    /**
     * Splits by [[[, ]]], (((, ))), {{{, }}}, and potential entry refs.
     */
    public List<Object> splitByTextualElems(String text, Map<String, Map<String, Object>> entryMap, 
            Map<String, Map<String, Object>> libMap, Map<String, Object> libInfo) {
        List<Object> res;
        Map<String, Object> modelMap;
        List<Object> prevSplit;
        
        msgHandler.logDebug(
                "====================================\n" + 
                "splitByTextualElems: " + (text.length() > 400 ? text.substring(0, 399) : text) + "\n" +
                "====================================");
        
        // split by raw text
        modelMap = FtlDocFileParser.makeObjectMap();
        modelMap.put("type", "text-raw");
        res = splitByPat(text, rawTextPat, modelMap, "origText", "value");
        
        // split remaining text parts by plain pat
        prevSplit = res;
        res = new ArrayList<>();
        modelMap = FtlDocFileParser.makeObjectMap();
        modelMap.put("type", "text-plain");
        for(Object part : prevSplit) {
            if (part instanceof String) {
                List<Object> listSplit = splitByPat((String) part, plainTextPat, modelMap, "origText", "value");
                res.addAll(listSplit);
            }
            else {
                res.add(part);
            }
        }
        
        // split remaining text parts by code pat
        prevSplit = res;
        res = new ArrayList<>();
        modelMap = FtlDocFileParser.makeObjectMap();
        modelMap.put("type", "text-code");
        for(Object part : prevSplit) {
            if (part instanceof String) {
                List<Object> listSplit = splitByPat((String) part, codeTextPat, modelMap, "origText", "value");
                res.addAll(listSplit);
            }
            else {
                res.add(part);
            }
        }

        
        // split remaining text parts by entry refs
        prevSplit = res;
        res = new ArrayList<>();
        for(Object part : prevSplit) {
            if (part instanceof String) {
                List<Object> listSplit = splitByLibEntryRefs((String) part, entryMap, libMap);
                res.addAll(listSplit);
            }
            else {
                res.add(part);
            }
        }
        
        // split remaining text parts by manual links
        prevSplit = res;
        res = new ArrayList<>();
        modelMap = FtlDocFileParser.makeObjectMap();
        modelMap.put("type", "link");
        for(Object part : prevSplit) {
            if (part instanceof String) {
                List<Object> listSplit = splitByPat((String) part, linkManualPat, modelMap, "origText", "value");
                List<Object> fixupListSplit = new ArrayList<>();
                for(Object listObj : listSplit) {
                    fixupListSplit.add(fixupLink(listObj, entryMap, libMap, libInfo));
                }
                res.addAll(fixupListSplit);
            }
            else {
                res.add(part);
            }
        }
        
        // split remaining text parts by automatic links
        prevSplit = res;
        res = new ArrayList<>();
        modelMap = FtlDocFileParser.makeObjectMap();
        modelMap.put("type", "link");
        for(Object part : prevSplit) {
            if (part instanceof String) {
                List<Object> listSplit = splitByPat((String) part, linkAutoPat, modelMap, "origText", null, "value");
                List<Object> fixupListSplit = new ArrayList<>();
                for(Object listObj : listSplit) {
                    fixupListSplit.add(fixupLink(listObj, entryMap, libMap, libInfo));
                }
                res.addAll(fixupListSplit);
            }
            else {
                res.add(part);
            }
        }

        // debug
        /*
        for(Object part : res) {
            if (part instanceof Map) {
                String type = (String) ((Map<String, Object>) part).get("type");
                if ("text-code".equals(type)) {
                    msgHandler.logDebug("text-code: " + part);
                }
                else if ("text-raw".equals(type)) {
                    msgHandler.logDebug("text-raw: " + part);
                }
            }
        }*/
        
        return res;
    }
    
    
    
    // NOTE: don't end this with $; capture the newline
    private static final Pattern strictTitlePat = Pattern.compile(
            "^[ ]*[*][ ]*([^*\\n]*?)[ ]*[*][ ]*(\\n|\\z)"
            , Pattern.DOTALL + Pattern.MULTILINE);
    
    /**
     * Splits text into a list of maps (titles) and strings (regular text).
     * Only parses strict top-level titles.
     */
    public List<Object> splitByTitles(String text) {
        Map<String, Object> modelMap = FtlDocFileParser.makeObjectMap();
        modelMap.put("type", "title");
        return splitByPat(text, strictTitlePat, modelMap, "origText", "value");
    }

    
    
    /**
     * Splits text into a list of maps (indented blocks) and strings (regular text).
     * Only parses strict top-level titles.
     */
    @SuppressWarnings("unchecked")
    public List<Object> splitByIndentBlocks(String text, int minSize, int maxSize) {
        // NOTE: don't use $ here; capture newline
        final Pattern indentLinePat = Pattern.compile(
                "^([ ]" + makeRegexRangeExpr(minSize, maxSize) + ")(\\S.*)(\\n|\\z)"
                , Pattern.MULTILINE); // NOTE: no DOTALL
        
        // Get all the indented lines
        Map<String, Object> modelMap = FtlDocFileParser.makeObjectMap();
        modelMap.put("type", "indent");
        List<Object> lineList = splitByPat(text, indentLinePat, modelMap, "origText", "indentSpaces", "value");
        
        // Combine the lines into blocks
        int lastBlockIndentSize = 0;
        
        msgHandler.logDebug(
                "====================================\n" + 
                "splitByIndentBlocks: entry count: " + lineList.size() + "\n" +
                "====================================");

        List<Object> res = new ArrayList<>();
        for(Object entry : lineList) {
            if (entry instanceof String) {
                String entryText = (String) entry;
                
                msgHandler.logDebug("TEXT: " + entryText);

                res.add(entryText);
                // a piece of text breaking up a code block
                lastBlockIndentSize = 0;

                msgHandler.logDebug("did reset");
            }
            else {
                Map<String, Object> lineInfo = (Map<String, Object>) entry;
                
                String indentSpaces = (String) lineInfo.get("indentSpaces");
                int indentSize = indentSpaces.length();
                lineInfo.put("indentSize", indentSize);
                
                
                msgHandler.logDebug("INDENT LINE: " + lineInfo.get("value").toString());
                
                msgHandler.logDebug("indentSize " + indentSize + " lastBlockIndentSize " + lastBlockIndentSize );
                
                if (lastBlockIndentSize > 0 && indentSize >= lastBlockIndentSize) {
                    msgHandler.logDebug("append: indentSize " + indentSize + " lastBlockIndentSize " + lastBlockIndentSize);
                    
                    // combine with last line
                    Map<String, Object> blockInfo = (Map<String, Object>) res.get(res.size() - 1);
                    // Add the text, but make sure to keep right number of spaces
                    
                    String blockText = (String) blockInfo.get("value");
                    String lineText = (String) lineInfo.get("value");
                    
                    blockText += "\n"; // excluded from value in regexp
                    for(int i = 0; i < (indentSize - lastBlockIndentSize); i++) {
                        blockText += " ";
                    }
                    blockText += lineText;
                    
                    blockInfo.put("value", blockText);
                }
                else {
                    msgHandler.logDebug("was new block");
                    
                    // new block
                    res.add(lineInfo); // already in valid format for block
                    lastBlockIndentSize = indentSize;
                }
            }
        }
        
        return res;
    }
    
    public List<Object> splitByIndentBlocks(String text) {
        return splitByIndentBlocks(text, 2, -1);
    }
    
    
    /**
     * Finds and parses the first list. The list will never have a "title" or
     * leading text; only sub-lists will. This parses using INDENTATION.
     * Currently non-recursive only and does not recognize sub-lists, caller must do that.
     * <p>
     * Version that can isolate a list without consuming excess text.
     */
    public Map<String, Object> findParseBulletList(String text, boolean recurse) {
        if (recurse == true) {
            throw new UnsupportedOperationException("findParseBulletList doesn't recurse at the moment, ambiguous");
        }
        
        // get the first bullet
        Matcher m = bulletPat.matcher(text);
        if (m.find()) {
            Map<String, Object> listInfo = FtlDocFileParser.makeObjectMap();
            
            int textStartIndex = m.start();
            int textEndIndex = text.length();   // don't know yet
            int indentSize = m.group(1).length();

            msgHandler.logDebug(
                    "====================================\n" + 
                    "findParseBulletList: found bullet match: '" + m.group() + "'\n" +
                    "====================================");
            
            // just cut text slow, but whatever
            String listText = text.substring(textStartIndex);
            
            // NOTE: these pats use NOT DOTALL. Go line-by-line. MULTILINE is for ^ and $.
            // NOTE: don't end this with $; capture the newline
            Pattern listLinePat;
            if (indentSize > 0) {
                listLinePat = Pattern.compile("^[ ]{" + indentSize + "}([* ])[ ](.*)$");
            }
            else {
                listLinePat = Pattern.compile("^([* ])[ ](.*)$");
            }
            
            msgHandler.logDebug("Pattern: " + listLinePat.toString());
            
            int lineCharsConsumed = 0;
            int numLinesConsumed = 0;
            // NOTE: intentionally hardcode \n for now
            String[] lines = listText.split("\n");
            List<Object> listItemTexts = new ArrayList<>();
            String listItemText = null;

            for(String line : lines) {
                Matcher linem = listLinePat.matcher(line);
                if (linem.matches()) {
                    lineCharsConsumed += line.length();
                    numLinesConsumed += 1;
                    
                    String firstChar = linem.group(1);
                    String lineText = linem.group(2);
                    
                    if ("*".equals(firstChar)) {
                        if (listItemText != null) {
                            // save previous
                            listItemTexts.add(cleanTextValueSafe(listItemText));
                        }
                        
                        // start new item
                        listItemText = lineText;
                    }
                    else {
                        if (listItemText == null) {
                            throw new IllegalStateException("Error parsing bullet lists... regexp not working");
                        }
                        // append
                        listItemText += "\n" + lineText;
                    }
                }
                else {
                    break;
                }
            }
            
            // finish off previous
            if (listItemText != null) {
                listItemTexts.add(cleanTextValueSafe(listItemText));
            }
            
            int charsConsumed;
            if (numLinesConsumed >= lines.length) {
                charsConsumed = lineCharsConsumed + ("\n".length() * (numLinesConsumed - 1));
            }
            else {
                charsConsumed = lineCharsConsumed + ("\n".length() * numLinesConsumed);
            }
            
            // must add original startIndex to get the real endIndex (on the orig text); relative
            textEndIndex = textStartIndex + charsConsumed;
            
            

            List<Object> items = listItemTexts;
            
            /* this used to treat sub-lists as special items, but get more versatile markup if we don't.
               also now let caller do this.
            List<Map<String, Object>> items = new ArrayList<>();
            
            // Go through items, and create sub-lists where necessary.
            for(String itemText : listItemTexts) {
                Map<String, Object> itemInfo = makeObjectMap();
                
                Map<String, Object> subList = findParseBulletList(itemText);
                if (subList != null) {
                    // leading text
                    int subStartIndex = (int) subList.get("startIndex");
                    if (subStartIndex > 0) {
                        itemInfo.put("leadingText", itemText.substring(0, subStartIndex));
                    }
                    // trailing text
                    // NOTE: there shouldn't be any trailing text normally...
                    int subEndIndex = (int) subList.get("endIndex");
                    if (subEndIndex < itemText.length()) {
                        itemInfo.put("trailingText", itemText.substring(subEndIndex));
                    }
                    
                    itemInfo.put("items", subList.get("items"));
                }
                else {
                    itemInfo.put("leadingText", itemText);
                }
                items.add(itemInfo);
            }*/
            
            if (text.contains("generic field arrangement of no specific pattern and no")) {
                msgHandler.logDebug("FOUND PROBLEMATIC");
            }
            msgHandler.logDebug("text length: " + text.length() + 
                    " startIndex: " + textStartIndex + " endIndex: " + textEndIndex);
            //msgHandler.logDebug("items: " + items.toString());
            
            
            listInfo.put("items", items);
            listInfo.put("startIndex", textStartIndex);
            listInfo.put("endIndex", textEndIndex);
            listInfo.put("type", "list");
            return listInfo;
        }
        else {
            return null;
        }
    }
    
    /**
     * Returns list of maps and strings. 
     * String is piece of text, while each map describes a list.
     * <p>
     * If recurse, the list elems are passed through splitByMarkupElems
     */
    @SuppressWarnings("unchecked")
    public List<Object> splitByLists(String text, boolean recurse) {
        List<Object> textList = new ArrayList<>();
        
        String remainText = text;
        
        msgHandler.logDebug(
                "====================================\n" + 
                "splitByLists\n" +
                "====================================");
        
        while (true) {
            Map<String, Object> listInfo = findParseBulletList(remainText, false); // we recurse ourselves below
            if (listInfo == null) {
                break;
            }

            int startIndex = (int) listInfo.get("startIndex");
            int endIndex = (int) listInfo.get("endIndex");
            
            // Add text before list
            if (startIndex > 0) {
                textList.add(remainText.substring(0, startIndex));
            }
            
            if (recurse) {
                List<Object> parsedItems = new ArrayList<>();
                List<Object> itemTexts = (List<Object>) listInfo.get("items");
                if (itemTexts != null && !itemTexts.isEmpty()) {
                    for(Object itemEntry : itemTexts) {
                        String itemText = (String) itemEntry;
                        List<Object> parsedItemList = splitByStructuralElems(itemText);
                        parsedItems.add(parsedItemList);
                    }
                }
                // just replace directly, we know we can
                listInfo.put("items", parsedItems);
            }
            
            // Add the list
            textList.add(listInfo);

            // Update matcher to start at updated position
            // FIXME: This method is really dumb, there must be a better way...
            // ... however also very simple
            remainText = remainText.substring(endIndex);
        }
        
        // last piece of text remaining
        if (!remainText.isEmpty()) {
            textList.add(remainText);
        }
        
        msgHandler.logDebug("textList size: " + textList.size());
        
        return textList;
    }
    
    
    // just try to match everything possible here. note will consume (and discard) leading space.
    // NOTE: don't end this with $; capture the newline (but leave outside value)
    private static final Pattern notesPat = Pattern.compile(
            "(?:[ ]+|^[ ]*)([A-Z]{3}[A-Z ]*[A-Z])([?]:|:)[ ]*(.*?)(\\n|\\z)"
            , Pattern.MULTILINE); // NOTE: no DOTALL
    
    /**
     * Splits text into a list of maps (titles) and strings (regular text).
     * <p>
     * Notes are hard to handle, they occur almost anywhere and need to treat specially.
     * NOTE: This does not combine notes and lists, see {@link #combineMarkupElems}
     * <p>
     * Also, notes will always consume the whitespace that comes before them, and whitespace
     * around value on same line.
     * It may have no value after this call.
     */
    @SuppressWarnings("unchecked")
    public List<Object> splitByNotes(String text) {
        Map<String, Object> modelMap = FtlDocFileParser.makeObjectMap();
        modelMap.put("type", "note");
        List<Object> bareList = splitByPat(text, notesPat, modelMap,  "origText", "label", "sep", "value");
        // This is too unwieldly to do in regex:
        // check if note started a new line or not, and also clean value found so far (same line)
        
        msgHandler.logDebug(
                "====================================\n" + 
                "splitByNotes: entry count: " + bareList.size() + "\n" +
                "====================================");
        
        
        List<Object> res = new ArrayList<>();
        boolean lastWasNewline = true;
        
        for(Object bare : bareList) {
            if (bare instanceof String) {
                String bareText = (String) bare;
                if (bareText.isEmpty()) {
                    msgHandler.logDebug(" - text was empty");
                    
                }
                else if (bareText.endsWith("\n")) {
                    lastWasNewline = true;
                }
                else {
                    lastWasNewline = false;
                }
                res.add(bare);
                
                msgHandler.logDebug("TEXT: " + bareText);
            }
            else {
                Map<String, Object> noteInfo = (Map<String, Object>) bare;

                String value = (String) noteInfo.get("value");
                if (value != null) {
                    value = cleanTextValue(value);
                }
                if (value.isEmpty()) {
                    value = null;
                }
                noteInfo.put("value", value);
                
                noteInfo.put("labelAndSep", ((String) noteInfo.get("label")) + ((String) noteInfo.get("sep")));
                
                noteInfo.put("ownLine", lastWasNewline);
                lastWasNewline = true;
                
                res.add(noteInfo);
                
                msgHandler.logDebug("NOTE: label: " + (String) noteInfo.get("label") + " value: " + value);

            }
        }
        
        return res;
    }
    
    
    /**
     * This performs high-level combination of elements already split.
     * <p>
     * Notes may consume lists and indent blocks.
     */
    @SuppressWarnings("unchecked")
    public List<Object> combineStructuralElems(List<Object> elemList) {
        List<Object> res = new ArrayList<>();
        
        int i = 0;
        
        msgHandler.logDebug(
                "====================================\n" + 
                "combineMarkupElems: entry count: " + elemList.size() + "\n" +
                "====================================");
        
        while(i < elemList.size()) {
            // NOTE: slow if not ArrayList
            Object elemObj = elemList.get(i);
            
            if (elemObj instanceof Map) {
                Map<String, Object> elemInfo = (Map<String, Object>) elemObj;
                String type = (String) elemInfo.get("type");
                
                // notes should only consume next elem if they were on their own line
                if (("note".equals(type) && 
                    Boolean.TRUE.equals(elemInfo.get("ownLine")))) {
                    
                    msgHandler.logDebug("Got NOTE on own line: " + elemInfo.toString());

                    Object nextElemObj = null;
                    if ((i+1) < elemList.size()) {
                        nextElemObj = elemList.get(i+1);
                    }
                    
                    msgHandler.logDebug("Next elem is: " + nextElemObj);

                    if (nextElemObj != null && nextElemObj instanceof Map) {
                        Map<String, Object> nextElemInfo = (Map<String, Object>) nextElemObj;
                        String nextElemType = (String) nextElemInfo.get("type");
                        
                        msgHandler.logDebug("Got next elem: " + nextElemInfo.toString());

                        // only allow consuming list if note has no value of its own
                        if ((elemInfo.get("value") == null) && "list".equals(nextElemType)) {
                            // consume it and put it as the value for the note
                            i++;
                            elemInfo.put("value", nextElemInfo);
                        }
                        else if ("indent".equals(nextElemType)) {
                            // consume it and put it (or add) as the TEXT value for the note
                            i++;
                            String currValue = (String) (elemInfo.get("value"));
                            String indentText = (String) nextElemInfo.get("value");
                            currValue = (currValue != null ? (currValue + "\n") : "") + indentText;
                            
                            elemInfo.put("value", currValue);
                        }
                    }
                }

                res.add(elemInfo);
            }
            else {
                res.add(elemObj);
            }
            i++;
        }
        
        return res;
    }
    
    
    
    /**
     * High-level, splits text into a list of maps (markup elements) and strings (regular text).
     * For lists, applies recursively.
     * NOTE: does not split by entry refs, main markup elems only.
     */
    public List<Object> splitByStructuralElems(String text) {
        List<Object> res;
        List<Object> prevSplit;

        // split by simple/strict titles first
        res = splitByTitles(text);
        
        // split remaining text parts by lists, and also do lists recursively
        prevSplit = res;
        res = new ArrayList<>();
        for(Object part : prevSplit) {
            if (part instanceof String) {
                List<Object> listSplit = splitByLists((String) part, true);
                res.addAll(listSplit);
            }
            else {
                res.add(part);
            }
        }
        
        // split remaining text parts by notes ("NOTE:")
        prevSplit = res;
        res = new ArrayList<>();            
        for(Object part : prevSplit) {
            if (part instanceof String) {
                List<Object> listSplit = splitByNotes((String) part);
                res.addAll(listSplit);
            }
            else {
                res.add(part);
            }
        }
        
        // split remaining text parts by indent blocks
        prevSplit = res;
        res = new ArrayList<>();   
        for(Object part : prevSplit) {
            if (part instanceof String) {
                List<Object> listSplit = splitByIndentBlocks((String) part);
                res.addAll(listSplit);
            }
            else {
                res.add(part);
            }
        }
        
        // combine some of the resulting elems with high-level logic
        res = combineStructuralElems(res);
        
        return res;
    }
    
}