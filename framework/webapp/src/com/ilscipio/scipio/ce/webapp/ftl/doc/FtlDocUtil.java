package com.ilscipio.scipio.ce.webapp.ftl.doc;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public abstract class FtlDocUtil {

    protected FtlDocUtil() {
        
    }
    
    public static String readFileAsString(String path, Charset encoding) throws IOException {
        byte[] encoded = Files.readAllBytes(Paths.get(path));
        return new String(encoded, encoding);
    }
    
    public static String replaceExtension(String filePath, String newExt) {
        Matcher m = Pattern.compile("^.*([.][a-zA-Z0-9]+)$").matcher(filePath);
        if (!m.matches()) {
            return filePath + newExt;
            //throw new IllegalArgumentException("Can't replace file extension, has none");
        }
        String ext = m.group(1);
        return filePath.substring(0, filePath.length() - ext.length()) + newExt;
    }
    
    
    public static String join(String[] parts, String sep, int start, int end) {
        String res = parts[start];
        for(int i = start + 1; i < end; i++) {
            res += sep + parts[i];
        }
        return res;
    }

}
