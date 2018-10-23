package org.ofbiz.webapp.control;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * SCIPIO: Webapp request descriptor, before any transformation to its logical arguments or
 * its final web URL form.
 * <p>
 * FIXME: this does not support inter-webapp links or all the new ofbiz URL parameters
 */
public abstract class RequestDescriptor {

    private static final Pattern pathParamPat = Pattern.compile(";(\\w+)=([^?#;/]*)");

    public static RequestDescriptor fromUriStringRepr(HttpServletRequest request, HttpServletResponse response, String uriRepr) {
        if (uriRepr.startsWith("ofbizUrl://")) {
            return OfbizRequestDescriptor.fromUriStringRepr(request, response, uriRepr);
        }
        else {
            return WebRequestDescriptor.fromUriStringRepr(request, response, uriRepr);
        }
    }

    /**
     * Returns type as a string.
     */
    public abstract String getType();

    /**
     * Returns string representation of this descriptor as a URI before transformation, i.e.
     * its original representation.
     */
    public abstract String getUriStringRepr();

    /**
     * Returns the main URI after transformation, i.e. stripped of any special protocols or arguments.
     */
    public abstract String getBaseUriString();

    /**
     * Builds a final output URL.
     */
    public abstract String getWebUrlString();

    public static class WebRequestDescriptor extends RequestDescriptor {
        protected final String uri;

        public WebRequestDescriptor(HttpServletRequest request, HttpServletResponse response, String uri) {
            super();
            this.uri = uri;
        }

        public static WebRequestDescriptor fromUriStringRepr(HttpServletRequest request, HttpServletResponse response, String uriRepr) {
            return new WebRequestDescriptor(request, response, uriRepr);
        }

        @Override
        public String getType() {
            return "web";
        }

        @Override
        public String getUriStringRepr() {
            return uri;
        }

        @Override
        public String getBaseUriString() {
            return uri;
        }

        @Override
        public String getWebUrlString() {
            return uri;
        }
    }

    /**
     * Parses a string URI in the form:
     * ofbizUrl://myRequest;fullPath=false;secure=false;encode=true?param1=val1
     */
    public static class OfbizRequestDescriptor extends RequestDescriptor {

        private static final Set<String> optionNames = new HashSet<String>(Arrays.asList("fullPath", "secure", "encode"));

        protected final HttpServletRequest request;
        protected final HttpServletResponse response;
        protected final String requestUri;
        protected final Boolean fullPath;
        protected final Boolean secure;
        protected final Boolean encode;

        public OfbizRequestDescriptor(HttpServletRequest request, HttpServletResponse response, String requestUri, Boolean fullPath, Boolean secure, Boolean encode) {
            super();
            this.request = request;
            this.response = response;
            this.requestUri = requestUri;
            this.fullPath = fullPath;
            this.secure = secure;
            this.encode = encode;
        }

        public static OfbizRequestDescriptor fromUriStringRepr(HttpServletRequest request, HttpServletResponse response, String uriRepr) {
            // assumes begins with "ofbizUrl://" or "ofbizUri://" for now
            String path = uriRepr.substring("ofbizXxx://".length());
            Boolean fullPath = null;
            Boolean secure = null;
            Boolean encode = null;
            if (path.contains(";")) { // optimized for most cases (no args)
                Map<String, String> optionVals = new HashMap<String, String>();
                path = findStripUriStringDescriptorParameters(path, optionNames, optionVals);
                fullPath = stringToBool(optionVals.get("fullPath"), null);
                secure = stringToBool(optionVals.get("secure"), null);
                encode = stringToBool(optionVals.get("encode"), null);
            }
            return new OfbizRequestDescriptor(request, response, path, fullPath, secure, encode);
        }

        @Override
        public String getType() {
            return "ofbizUrl";
        }

        @Override
        public String getUriStringRepr() {
            // TODO
            throw new UnsupportedOperationException("not implemented");
        }

        @Override
        public String getBaseUriString() {
            return requestUri;
        }

        @Override
        public String getWebUrlString() {
            return RequestHandler.makeUrl(request, response, requestUri, fullPath, secure, encode);
        }

        public Boolean getFullPath() {
            return fullPath;
        }

        public Boolean getSecure() {
            return secure;
        }

        public Boolean getEncode() {
            return encode;
        }

        private static Boolean stringToBool(String arg, Boolean defaultVal) {
            if ("true".equals(arg)) {
                return true;
            }
            else if ("false".equals(arg)) {
                return false;
            }
            else {
                return defaultVal;
            }
        }

    }

    /**
     * Isolates descriptor parameters in a URI string.
     * These are currently implemented as URI path parameters (;).
     */
    public static String findStripUriStringDescriptorParameters(String uri, Set<String> paramNames, Map<String, String> paramValues) {
        StringBuffer sb = new StringBuffer();
        int i = 0;
        Matcher m = pathParamPat.matcher(uri);
        while(m.find()) {
            String name = m.group(1);
            if (paramNames.contains(name)) {
                paramValues.put(name, m.group(2));
                sb.append(uri.substring(i, m.start()));
            }
            else {
                sb.append(uri.substring(i, m.end()));
            }
            i = m.end();
        }
        sb.append(uri.substring(i));
        return sb.toString();
    }

}
