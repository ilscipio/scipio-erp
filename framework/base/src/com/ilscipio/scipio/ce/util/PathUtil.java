package com.ilscipio.scipio.ce.util;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

/**
 * SCIPIO: Path utility class. Provides helper methods for various
 * file system and URL path operations.
 */
public class PathUtil {

    /* Static variables */
    private static URL dummyContextUrl;     // URL used to generate temporary URLs

    static {
        try {
            dummyContextUrl = new URL("http", "ilscipio.com", 80, "/");
        } catch (MalformedURLException e) {
            ; // should throw something here? We know this is valid.
        }
    }

    /**
     * Returns request path (~URI) from a URL or partial URL. Tries its best
     * and returns null if could not extract.
     *
     * @param address
     * @return  the request path
     */
    public static String getReqPathFromAddress(String address) {
        String reqPath = null;
        String cleanedAddr = address.trim();
        try {
            URL properUrl = new URL(dummyContextUrl, cleanedAddr);
            reqPath = properUrl.getPath();
        } catch (MalformedURLException e) {
        }
        return reqPath;
    }


    /**
     * Concatenates two paths using forward slashes.
     *
     * @param prefix
     * @param postfix
     * @return  the concatenation
     */
    public static String concatPaths(String prefix, String postfix) {
        String result = prefix;
        if (!result.endsWith("/")) {
            result += "/";
        }
        result += postfix.startsWith("/") ? postfix.substring(1) : postfix;
        return result;
    }

    public static String ensureStartDelim(String path) {
        if (path.startsWith("/")) {
            return path;
        } else {
            return "/" + path;
        }
    }

    public static String removeStartDelim(String path) {
        if (path.startsWith("/")) {
            return path.substring(1);
        } else {
            return path;
        }
    }

    public static String ensureTrailDelim(String path) {
        if (path.endsWith("/")) {
            return path;
        } else {
            return path + "/";
        }
    }

    public static String removeTrailDelim(String path) {
        if (path.endsWith("/")) {
            return path.substring(0, path.length()-1);
        } else {
            return path;
        }
    }

    public static String ensureStartAndNoTrailDelim(String path) {
        return ensureStartDelim(removeTrailDelim(path));
    }

    public static String ensureNoDelims(String path) {
        return removeStartDelim(removeTrailDelim(path));
    }

    public static void ensureStartDelim(StringBuilder path) {
        if (path.length() == 0) path.append('/');
        else {
            if (path.charAt(0) != '/') {
                path.insert(0, '/');
            }
        }
    }

    public static void removeStartDelim(StringBuilder path) {
        if (path.length() > 0) {
            if (path.charAt(0) == '/') {
                path.deleteCharAt(0);
            }
        }
    }

    public static void ensureTrailDelim(StringBuilder path) {
        if (path.length() == 0) path.append('/');
        else {
            if (path.charAt(path.length() - 1) != '/') {
                path.append('/');
            }
        }
    }

    public static void removeTrailDelim(StringBuilder path) {
        if (path.length() > 0) {
            if (path.charAt(path.length() - 1) == '/') {
                path.setLength(path.length() - 1);
            }
        }
    }

    public static void ensureStartAndNoTrailDelim(StringBuilder path) {
        ensureStartDelim(path);
        removeTrailDelim(path);
    }

    public static void ensureNoDelims(StringBuilder path) {
        removeStartDelim(path);
        removeTrailDelim(path);
    }


    /**
     * Transforms an absolute file path in the local file system into
     * a URL-compatible address for the file.
     *
     * @param filePath  the absolute path to the file, including drive (i.e., "C:\Program Files...")
     * @return          a URL address for the local file
     */
    public static String filePathToUrl(String filePath) {
        if (filePath.startsWith("/")) {
            return "file://" + filePath;
        } else {
            return "file:///" + filePath;
        }
    }

    public static boolean isPathPrefixOf(String testPath, String queryPath) {
        String testPathSep = testPath.endsWith("/") ? testPath : testPath + "/";
        String queryPathSep = queryPath.endsWith("/") ? queryPath : queryPath + "/";
        return testPathSep.startsWith(queryPathSep);
    }

    /**
     * Returns the Ofbiz temporary file directory (for uploaded files, etc).
     * <p>
     * Always use this call instead of hardcoding the location, so it can be
     * changed easily in this central location in the future, if needed.
     *
     * @return  a File object representing the temp file directory
     */
    public static File getTempFileDir() {
        return new File(new File(System.getProperty("ofbiz.home"), "runtime"), "tmp");
    }

    /**
     * For a request path like /first/second/third, generates:
     * /
     * /first
     * /first/second
     * /first/second/third
     */
    public static List<String> makeAllRequestPathPrefixes(String requestPath) {
        List<String> subPaths = new ArrayList<>();

        if (requestPath != null){
            int index = requestPath.indexOf("/", 0);
            if (index == 0 && requestPath.length() > 1) {
                index = 1;
            }

            while (index > 0) { // Also skip zero
                subPaths.add(requestPath.substring(0, index));
                index = requestPath.indexOf("/", index + 1);
            }

            // Add itself
            subPaths.add(requestPath);
        }

        return subPaths;
    }

    public static String removeLastDirPart(String path) {
        int i = path.lastIndexOf("/");
        if (i < 0) {
            return null;
        } else {
            return path.substring(0, i);
        }
    }

    public static boolean isFileParentOf(File maybeChild, File possibleParent) throws java.io.IOException {
        final File parent = possibleParent.getCanonicalFile();
        if (!parent.exists() || !parent.isDirectory()) {
            return false;
        }

        File child = maybeChild.getCanonicalFile();
        while (child != null) {
            if (child.equals(parent)) {
                return true;
            }
            child = child.getParentFile();
        }
        return false;
    }
}
