package com.ilscipio.scipio.ce.webapp.filter.urlrewrite;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.webapp.ExtWebappInfo;
import org.tuckey.web.filters.urlrewrite.Conf;

public abstract class UrlConfUtil {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final UtilCache<String, Conf> pathCache = UtilCache.createUtilCache("scipio.urlrewrite.conf.path");
    private static final UtilCache<String, Conf> webappCache = UtilCache.createUtilCache("scipio.urlrewrite.conf.webapp");

    private UrlConfUtil() {
    }

    public static Conf getConfFromLocation(String urlConfPath) throws IOException {
        Conf conf = pathCache.get(urlConfPath);
        if (conf != null) {
            return conf;
        }

        URL confUrl;
        confUrl = FlexibleLocation.resolveLocation(urlConfPath);
        if (confUrl == null) {
            throw new IOException("Could not resolve urlrewrite conf path: " + urlConfPath);
        }
        String confUrlStr = confUrl.toString();

        InputStream inputStream = null;
        try {
            inputStream = confUrl.openStream();
            // attempt to retrieve from location other than local WEB-INF

            if (inputStream == null) {
                throw new IOException("Unable to find urlrewrite conf file at: " + urlConfPath);
            } else {
                conf = new Conf(null, inputStream, urlConfPath, confUrlStr, false);
            }
        } finally {
            try {
                if (inputStream != null) {
                    inputStream.close();
                }
            } catch (IOException e) {
                Debug.logError(e, module);
            }
        }
        pathCache.put(urlConfPath, conf);
        pathCache.put(confUrlStr, conf); // may help prevent duplicates between component: vs file: paths
        return conf;
    }

    public static Conf getConfFromWebapp(ExtWebappInfo webappInfo) throws IOException {
        if (webappInfo.getUrlRewriteRealConfPath() == null) {
            return null;
        }

        Conf conf = webappCache.get(webappInfo.getContextPath());
        if (conf != null) {
            return conf;
        }

        conf = getConfFromLocation("file:" + webappInfo.getUrlRewriteRealConfPath());

        webappCache.put(webappInfo.getContextPath(), conf);

        return conf;
    }

    public static Conf getConfFromLocationOrWebapp(String urlConfPath, ExtWebappInfo webappInfo) throws IOException {
        return (urlConfPath != null) ? getConfFromLocation(urlConfPath) : getConfFromWebapp(webappInfo);
    }

}
