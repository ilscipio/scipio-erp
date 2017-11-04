package com.ilscipio.scipio.util;

import java.io.UnsupportedEncodingException;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import org.ofbiz.base.util.Debug;

/**
 * SCIPIO: URL character handling for SEO.
 */
public class SeoStringUtil {

    private static final String module = SeoStringUtil.class.getName();

    public static final String URL_HYPHEN = "-";
    public static final String ASCII_REGEX = "^[0-9-_a-zA-Z]*$";
    public static Pattern ASCII_PATTERN = Pattern.compile(ASCII_REGEX);
    
    /**
     * Generates a hash of a given plainText
     * 
     * @param plainText
     * @return md5-hash
     */
    public static String getHash(String plain) {
        return getHash(plain, "ISO-8859-1");
    }

    public static String getHash(String plain, String encoding) {
        byte[] bytes = null;
        try {
            bytes = plain.getBytes(encoding);
        }
        catch (UnsupportedEncodingException e) {
            String msg = "No such Encoding: " + e.getMessage();
            Debug.logError(msg, module);
            return null;
        }
        return org.apache.commons.codec.digest.DigestUtils.md5Hex(bytes);

    }

    /**
     * taken from http://stackoverflow.com/questions/25259/how-does-stackoverflow-generate-its-seo-friendly-urls
     * 
     * @param name
     * @return
     */
    public static String constructSeoName(String name) {
        if (name == null)
            return "";

        int maxlen = 110;
        int len = name.length();
        boolean prevdash = false;

        StringBuilder sb = new StringBuilder(len);
        char c;

        name = name.toLowerCase();

        for (int i = 0; i < len; i++) {
            c = name.charAt(i);
            if ((c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')) {
                sb.append(c);
                prevdash = false;
            }
            else if (c == ' ' || c == ',' || c == '.' || c == '/' || c == '\\' || c == '-' || c == '_' || c == '=' || c == '?' || c == ':'
                    || c == ';') {
                if (!prevdash && sb.length() > 0) {
                    sb.append('-');
                    prevdash = true;
                }
            }
            else if ((int) c >= 128) {
                int prevlen = sb.length();
                sb.append(remapInternationalCharToAscii(c));
                if (prevlen != sb.length())
                    prevdash = false;
            }
            if (i >= maxlen)
                break;
        }

        if (prevdash)
            return sb.toString().substring(0, sb.length() - 1);
        else
            return sb.toString();
    }

    /**
     * taken from http://meta.stackoverflow.com/questions/7435/non-us-ascii-characters-dropped-from-full-profile-url/7696#7696
     * 
     * @param c
     * @return
     */
    public static String remapInternationalCharToAscii(char c) {
        String s = new Character(c).toString();
        if (c == 'ä') {
            return "ae";
        }
        else if (c == 'ü') {
            return "ue";
        }
        else if (c == 'ö') {
            return "oe";
        }
        else if (c == 'ß') {
            return "ss";
        }
        else if ("àåáâãåą".contains(s)) {
            return "a";
        }
        else if ("èéêëę".contains(s)) {
            return "e";
        }
        else if ("ìíîïı".contains(s)) {
            return "i";
        }
        else if ("òóôõøőð".contains(s)) {
            return "o";
        }
        else if ("ùúûŭů".contains(s)) {
            return "u";
        }
        else if ("çćčĉ".contains(s)) {
            return "c";
        }
        else if ("żźž".contains(s)) {
            return "z";
        }
        else if ("śşšŝ".contains(s)) {
            return "s";
        }
        else if ("ñń".contains(s)) {
            return "n";
        }
        else if ("ýÿ".contains(s)) {
            return "y";
        }
        else if ("ğĝ".contains(s)) {
            return "g";
        }
        else if (c == 'ř') {
            return "r";
        }
        else if (c == 'ł') {
            return "l";
        }
        else if (c == 'đ') {
            return "d";
        }
        else if (c == 'Þ') {
            return "th";
        }
        else if (c == 'ĥ') {
            return "h";
        }
        else if (c == 'ĵ') {
            return "j";
        }
        else {
            return "";
        }
    }

    public static String calculateRuntime(long startTime, long endTime) {
        long runTime = endTime - startTime;

        long minutes = TimeUnit.MILLISECONDS.toMinutes(runTime);
        runTime -= TimeUnit.MINUTES.toMillis(minutes);
        long seconds = TimeUnit.MILLISECONDS.toSeconds(runTime);
        runTime -= TimeUnit.SECONDS.toMillis(seconds);
        long millis = runTime;

        String runTimeStr = String.format("%d min, %d sec, %d ms", minutes, seconds, millis);

        return runTimeStr;
    }

}
