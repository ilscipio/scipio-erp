package com.ilscipio.scipio.ce.webapp.filter.urlrewrite.reimpl;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.webapp.FullWebappInfo;
import org.tuckey.web.filters.urlrewrite.Condition;
import org.tuckey.web.filters.urlrewrite.Conf;
import org.tuckey.web.filters.urlrewrite.OutboundRule;

import com.ilscipio.scipio.ce.webapp.filter.urlrewrite.ScipioUrlRewriter;

/**
 * urlrewrite.xml file processor static re-implementation.
 * Reads urlrewrite.xml and provides its own re-implementation.
 * <p>
 * NOTE: 2018-08-06: This implementation is very incomplete and should
 * probably not be used anywhere anymore.
 * <p>
 * FIXME: processOutboundUrl does not acurately reproduce urlrewritefilter behavior; only some
 * elements of rules are recognized (best-effort) - so any urlrewrite.xml files too complex
 * may produce different results than if processed by urlrewritefilter itself!
 */
public class ReimplUrlRewriter extends ScipioUrlRewriter {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    protected final String urlConfPath;
    protected final Conf conf;
    protected final List<OutboundRule> supportedOutboundRules;
    
    protected ReimplUrlRewriter(String urlConfPath, Conf conf) throws IOException {
        this.urlConfPath = urlConfPath;
        this.conf = conf;
        this.supportedOutboundRules = getSupportedOutboundRules(urlConfPath, conf);
    }
    
    @SuppressWarnings("unchecked")
    protected static List<OutboundRule> getSupportedOutboundRules(String urlConfPath, Conf conf) {
        List<OutboundRule> filteredRules = new ArrayList<>();
        for (Object outboundRuleObj : conf.getOutboundRules()) {
            final OutboundRule outboundRule = (OutboundRule) outboundRuleObj;
            String matchType = outboundRule.getMatchType();
            if (matchType != null && !matchType.isEmpty() && !"regex".equals(matchType)) {
                Debug.logWarning("urlrewrite: outbound-rule in " + urlConfPath + " uses unsupported match-type: " + matchType + "; ignoring rule", module);
                continue;
            }
            if (UtilValidate.isEmpty(outboundRule.getFrom()) || outboundRule.getTo() == null) {
                Debug.logWarning("urlrewrite: outbound-rule in " + urlConfPath + " omits 'from' or 'to' elements; unsupported; ignoring rule", module);
                continue;
            }
            if (UtilValidate.isNotEmpty(outboundRule.getConditions()) && !checkConditionsSupported(outboundRule.getConditions())) {
                Debug.logWarning("urlrewrite: outbound-rule in " + urlConfPath + " has unsupported condition elements; unsupported; ignoring rule", module);
                continue;
            }
            filteredRules.add(outboundRule);
        }
        return filteredRules;
    }
    
    protected static boolean checkConditionsSupported(List<Condition> conds) {
        for(Condition cond : conds) {
            if (!checkConditionSupported(cond)) return false;
        }
        return true;
    }
    
    protected static boolean checkConditionSupported(Condition cond) {
        // TODO: try to support context-path regexp, etc.
        return false;
    }
    
    public static ReimplUrlRewriter loadConf(String urlConfPath) throws IOException {
        if (Debug.verboseOn()) {
            Debug.logVerbose("urlrewrite: loading " + urlConfPath, module);
        }
        // load config
        URL confUrl;
        InputStream inputStream = null;
        
        confUrl = FlexibleLocation.resolveLocation(urlConfPath);

        try {
            inputStream = confUrl.openStream();
            // attempt to retrieve from location other than local WEB-INF
    
            if (inputStream == null) {
                throw new IOException("Unable to find urlrewrite conf file at: " + urlConfPath);
            } else {
                Conf conf = new Conf(null, inputStream, urlConfPath, confUrl.toString(), false);
                
                return new ReimplUrlRewriter(urlConfPath, conf);
            }
        } finally {
            try {
                if (inputStream != null) inputStream.close();
            } catch (IOException e) {
            }
        }

    }
    
    public String getUrlConfPath() {
        return urlConfPath;
    }

    public Conf getConf() {
        return conf;
    }

    public List<OutboundRule> getSupportedOutboundRules() {
        return supportedOutboundRules;
    }
    
    /**
     * Use urlrewritefilter rules to convert urls - emulates urlrewritefilter - just like the original url would be
     * TODO: REVIEW: 2017: urlrewritefilter internals may have changed...
     * <p>
     * FIXME: this fails on complex rules with conditions
     */
    //@Override
    protected String processOutboundUrl(String url) {
        String rewriteUrl = url;

        // Taken over from urlrewritefilter
        if (Debug.verboseOn()) {
            Debug.logVerbose("urlrewrite: processing outbound url for " + url, module);
        }

        if (rewriteUrl == null) {
            return "";
        }

        for (OutboundRule outboundRule : getSupportedOutboundRules()) {
            
            // FIXME: even with fixes, this will still fail on complex rules with conditions

            Matcher m = Pattern.compile(outboundRule.getFrom()).matcher(rewriteUrl);
            boolean matched;
            String replacement = outboundRule.getTo();
            if ("-".equals(replacement)) {
                matched = m.find();
            } else {
                // CAN'T DO THIS - must test if actually matched, for isLast()
                //rewriteUrl = m.replaceAll(outboundRule.getTo());
                m.reset();
                matched = m.find();
                if (matched) {
                    boolean nextMatched;
                    StringBuffer sb = new StringBuffer();
                    do {
                        m.appendReplacement(sb, replacement);
                        nextMatched = m.find();
                    } while (nextMatched);
                    m.appendTail(sb);
                    rewriteUrl = sb.toString();
                }
            }
            
            if (matched && outboundRule.isLast()) {
                break;
            }
            
            // invalid check
//            if (rewriteUrl != null) {
//                // means this rule has matched
//                if (Debug.verboseOn()) {
//                    Debug.logVerbose("urlrewrite: \"" + outboundRule.getDisplayName() + "\" matched", module);
//                }
//                if (outboundRule.isLast()) {
//                    if (Debug.verboseOn()) {
//                        Debug.logVerbose("urlrewrite: rule is last", module);
//                    }
//                    // there can be no more matches on this request
//                    break;
//                }
//            }
        }

        return rewriteUrl;
    }

    @Override
    public String processOutboundUrl(String url, FullWebappInfo urlWebappInfo, Map<String, Object> context) {
        return processOutboundUrl(url);
    }

    @Override
    public String processOutboundUrl(String url, FullWebappInfo urlWebappInfo, HttpServletRequest request, HttpServletResponse response) {
        return processOutboundUrl(url);
    }
}
