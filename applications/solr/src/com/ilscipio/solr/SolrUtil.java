package com.ilscipio.solr;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrRequest.METHOD;
import org.apache.solr.client.solrj.impl.HttpSolrClient;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.util.ClientUtils;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;
import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.component.ComponentException;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;

/**
 * Solr utility class.
 */
public abstract class SolrUtil {
    
    public static final String module = SolrUtil.class.getName();
    
    public static final String solrConfigName = "solrconfig";
    public static final String solrUrl = makeSolrWebappUrl();
    public static final String solrFullUrl = makeFullSolrWebappUrl();
    
    private static final Set<String> noPrefixTerms = UtilMisc.unmodifiableHashSet("AND", "OR", "NOT");
    private static final Set<Character> noPrefixTermCharPrefixes = UtilMisc.unmodifiableHashSet('+', '-');
    private static final Map<Character, Character> termEnclosingCharMap; 
    static {
        Map<Character, Character> map = new HashMap<>();
        map.put('"', '"');
        map.put('{', '}');
        map.put('[', ']');
        map.put('(', ')');
        termEnclosingCharMap = Collections.unmodifiableMap(map);
    }
    
    private static final String solrContentLocalesStr;
    private static final List<Locale> solrContentLocales;
    static {
        String locStr = UtilProperties.getPropertyValue(solrConfigName, "solr.content.locales");
        if (locStr != null) locStr = locStr.trim();
        if (UtilValidate.isEmpty(locStr)) {
            locStr = UtilProperties.getPropertyValue("general", "locales.available");
        }
        if (UtilValidate.isEmpty(locStr)) {
            locStr = "en";
        }
        List<Locale> locList = new ArrayList<>();
        StringBuilder normLocStr = new StringBuilder();
        try {
            for(String tag : locStr.split("\\s*,\\s*")) {
                Locale locale = Locale.forLanguageTag(tag);
                if (locale == null) throw new IllegalArgumentException("invalid locale: " + tag);
                if (locList.contains(locale)) {
                    Debug.logWarning("Solr: Configured locale list contains duplicate locales: " + locStr, module);
                    continue;
                }
                locList.add(locale);
                if (normLocStr.length() > 0) normLocStr.append(",");
                normLocStr.append(locale.toString());
            }
            Debug.logInfo("Solr: Configured content locales: " + locStr, module);
        } catch(Exception e) {
            Debug.logError(e, "Solr: Could not parse content locales: " + locStr + ": " + e.getMessage(), module);
            locStr = "en";
            locList = UtilMisc.toList(Locale.ENGLISH);
        }
        solrContentLocalesStr = normLocStr.toString();
        solrContentLocales = Collections.unmodifiableList(locList);
    }
    
    private static final Locale solrContentLocaleDefault;
    static {
        Locale locale = null;
        try {
            String locStr = UtilProperties.getPropertyValue(solrConfigName, "solr.content.locales.default");
            if (UtilValidate.isNotEmpty(locStr)) {
                locale = Locale.forLanguageTag(locStr);
            }
        } catch(Exception e) {
            Debug.logError("Solr: Error reading default locale: " + e.getMessage(), module);
        }
        if (locale == null) locale = Locale.getDefault();
        locale = getSolrSchemaLangLocale(locale);
        if (!solrContentLocales.contains(locale)) {
            Locale firstLocale = solrContentLocales != null ? solrContentLocales.get(0) : null;
            Debug.logWarning("Solr: Configured content locale default/fallback (" + locale 
                    + ") is not present in solr locales list (you may need extra configuration)! Using first in list as default instead: " + firstLocale, module);
            locale = firstLocale;
        } else {
            Debug.logInfo("Solr: Configured content locale default/fallback: " + locale.toString(), module);
        }
        solrContentLocaleDefault = locale;
    }
    
    public static String getSolrConfigVersionStatic() {
        return UtilProperties.getPropertyValue("solrconfig", "solr.config.version");
    }
    
    /**
     * Gets content locales. FIXME: currently ignores product store!
     */
    public static String getSolrContentLocalesString(Delegator delegator, String productStoreId) {
        return solrContentLocalesStr; 
    }
    
    /**
     * Gets content locales. FIXME: currently ignores product store!
     */
    public static List<Locale> getSolrContentLocales(Delegator delegator, String productStoreId) {
        return solrContentLocales; 
    }
    
    /**
     * Gets default content locale. FIXME: currently ignores product store!
     */
    public static Locale getSolrContentLocaleDefault(Delegator delegator, String productStoreId) {
        return solrContentLocaleDefault; 
    }

    
    // not currently useful
    //public static final boolean SOLR_CONTENT_LOCALES_REQUIREALL = UtilProperties.getPropertyAsBoolean(solrConfigName, "solr.content.locales.requireAll", false);

    
    public static String makeSolrWebappUrl() {
        final String solrWebappProtocol = UtilProperties.getPropertyValue(solrConfigName, "solr.webapp.protocol");
        final String solrWebappDomainName = UtilProperties.getPropertyValue(solrConfigName, "solr.webapp.domainName");
        final String solrWebappPath = UtilProperties.getPropertyValue(solrConfigName, "solr.webapp.path");
        final String solrWebappPortOverride = UtilProperties.getPropertyValue(solrConfigName, "solr.webapp.portOverride");
        
        String solrPort;
        if (UtilValidate.isNotEmpty(solrWebappPortOverride)) {
            solrPort = solrWebappPortOverride;
        } else {
            solrPort = UtilProperties.getPropertyValue("url.properties", ("https".equals(solrWebappProtocol) ? "port.https" : "port.http"));
        }
        
        return solrWebappProtocol + "://" + solrWebappDomainName + ":" + solrPort + solrWebappPath;
    }
    
    public static String makeFullSolrWebappUrl() {
        final String solrDefaultCore = UtilProperties.getPropertyValue(solrConfigName, "solr.core.default");
        return makeSolrWebappUrl() + "/" + solrDefaultCore;
    }
    
    public static boolean isSolrEcaEnabled() {
        Boolean ecaEnabled = null;
        String sysProp = System.getProperty("ofbiz.solr.eca.enabled");
        if (UtilValidate.isNotEmpty(sysProp)) {
            if ("true".equalsIgnoreCase(sysProp))  {
                ecaEnabled = Boolean.TRUE;
            } else if ("false".equalsIgnoreCase(sysProp)) {
                ecaEnabled = Boolean.FALSE;
            }
        }
        if (ecaEnabled == null) {
            ecaEnabled = UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, "solr.eca.enabled", false);
        }
        return Boolean.TRUE.equals(ecaEnabled);
    }
    
    public static WebappInfo getSolrWebappInfo() {
        WebappInfo solrApp = null;
        try {
            ComponentConfig cc = ComponentConfig.getComponentConfig("solr");
            for(WebappInfo currApp : cc.getWebappInfos()) {
                if ("solr".equals(currApp.getName())) {
                    solrApp = currApp;
                    break;
                }
            }
        }
        catch(ComponentException e) {
            throw new IllegalStateException(e);
        }
        return solrApp;
    }
    
    public static boolean isSolrEcaWebappInitCheckPassed() {
        Boolean webappCheckEnabled = UtilProperties.getPropertyAsBoolean(solrConfigName, "solr.eca.useSolrWebappLoadedCheck", true);
        if (Boolean.TRUE.equals(webappCheckEnabled)) {
            return isSolrWebappInitialized();
        } else {
            // If webapp check disabled, then we say the check passed.
            return true;
        }
    }
    
    public static boolean isSolrWebappInitialized() {
        return OfbizSolrInfoServlet.isServletInitStatusReached();
    }
    
    public static boolean isEcaTreatConnectErrorNonFatal() {
        Boolean treatConnectErrorNonFatal = UtilProperties.getPropertyAsBoolean(solrConfigName, "solr.eca.treatConnectErrorNonFatal", true);
        return Boolean.TRUE.equals(treatConnectErrorNonFatal);
    }
    
    
    /**
     * @deprecated Use {@link ProductUtil#generateSolrProductDocument(Map)} instead
     */
    public static SolrInputDocument generateSolrDocument(Map<String, Object> context) throws GenericEntityException {
        return ProductUtil.generateSolrProductDocument(context);
    }
    
    public static Map<String, Object> categoriesAvailable(String catalogId, String categoryId, String productId, boolean displayproducts, int viewIndex, int viewSize) {
        return categoriesAvailable(catalogId,categoryId,productId,null,displayproducts,viewIndex,viewSize, null);
    }
    
    public static Map<String, Object> categoriesAvailable(String catalogId, String categoryId, String productId, String facetPrefix, boolean displayproducts, int viewIndex, int viewSize) {
        return categoriesAvailable(catalogId, categoryId, productId, facetPrefix, displayproducts, viewIndex, viewSize, null);
    }

    public static Map<String, Object> categoriesAvailable(String catalogId, String categoryId, String productId, String facetPrefix, boolean displayproducts, int viewIndex, int viewSize, String core) {
        // create the data model
        Map<String, Object> result = new HashMap<>();
        HttpSolrClient client = null;
        QueryResponse returnMap = new QueryResponse();
        try {
            // do the basic query
            client = getHttpSolrClient(core);
            // create Query Object
            String query = "inStock[1 TO *]";
            if (categoryId != null)
                query += " +cat:"+ SolrUtil.escapeTermFull(categoryId);
            else if (productId != null)
                query += " +productId:" + SolrUtil.escapeTermFull(productId);
            SolrQuery solrQuery = new SolrQuery();
            solrQuery.setQuery(query);

            if (catalogId != null)
                solrQuery.setFilterQueries("catalog:" + SolrUtil.escapeTermFull(catalogId));
            if (displayproducts) {
                if (viewSize > -1) {
                    solrQuery.setRows(viewSize);
                } else
                    solrQuery.setRows(50000);
                if (viewIndex > -1) {
                    // 2016-04-01: This must be calculated
                    //solrQuery.setStart(viewIndex);
                    if (viewSize > 0) {
                        solrQuery.setStart(viewSize * viewIndex);
                    }
                }
            } else {
                solrQuery.setFields("cat");
                solrQuery.setRows(0);
            }
            
            if(UtilValidate.isNotEmpty(facetPrefix)){
                solrQuery.setFacetPrefix(facetPrefix);
            }
            
            solrQuery.setFacetMinCount(0);
            solrQuery.setFacet(true);
            solrQuery.addFacetField("cat");
            solrQuery.setFacetLimit(-1);
            if (Debug.verboseOn()) Debug.logVerbose("solr: solrQuery: " + solrQuery, module);
            returnMap = client.query(solrQuery,METHOD.POST);
            result.put("rows", returnMap);
            result.put("numFound", returnMap.getResults().getNumFound());
        } catch (Exception e) {
            Debug.logError(e.getMessage(), module);
        }
        return result;
    }

    public static GenericValue getSolrStatus(Delegator delegator) {
        GenericValue solrStatus;
        try {
            solrStatus = EntityQuery.use(delegator).from("SolrStatus")
                    .where("solrId", "SOLR-MAIN").cache(false).queryOne();
            if (solrStatus == null) {
                Debug.logWarning("Could not get SolrStatus for SOLR-MAIN - seed data missing?", module);
            } else {
                return solrStatus;
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        }
        return null;
    }
    
    public static String getSolrDataStatusId(Delegator delegator) {
        GenericValue solrStatus = getSolrStatus(delegator);
        return solrStatus != null ? solrStatus.getString("dataStatusId") : null;
    }
    
    public static boolean setSolrDataStatusId(Delegator delegator, String dataStatusId, boolean updateVersion) {
        GenericValue solrStatus;
        try {
            solrStatus = EntityQuery.use(delegator).from("SolrStatus")
                    .where("solrId", "SOLR-MAIN").cache(false).queryOne();
            //solrStatus = delegator.findOne("SolrStatus", UtilMisc.toMap("solrId", "SOLR-MAIN"), false);
            if (solrStatus == null) {
                Debug.logWarning("Could not get SolrStatus for SOLR-MAIN - creating new", module);
                solrStatus = delegator.create("SolrStatus", 
                        "solrId", "SOLR-MAIN", 
                        "dataStatusId", dataStatusId, 
                        "dataCfgVersion", getSolrConfigVersionStatic());
            } else {
                solrStatus.setString("dataStatusId", dataStatusId);
                if (updateVersion) {
                    solrStatus.setString("dataCfgVersion", getSolrConfigVersionStatic());
                }
                solrStatus.store();
            }
            return true;
            
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return false;
        }
    }
    
    public static boolean setSolrDataStatusId(Delegator delegator, String dataStatusId) {
        return setSolrDataStatusId(delegator, dataStatusId, false);
    }
    
    /**
     * Returns the closest whole viewIndex.
     */
    public static Integer calcResultViewIndex(SolrDocumentList results, Integer viewSize) {
        Integer viewIndex = null;
        if (results != null && viewSize != null && viewSize > 0) {
            long start = results.getStart();
            viewIndex = (int) (start / (long) viewSize);
        }
        return viewIndex;
    }
    
    public static HttpSolrClient getHttpSolrClient(String core) {
        if (UtilValidate.isNotEmpty(core)) return new HttpSolrClient(SolrUtil.solrUrl + "/" + core);
        else return getHttpSolrClient();
    }
    
    public static HttpSolrClient getHttpSolrClient() {
        return new HttpSolrClient(SolrUtil.solrFullUrl);
    }
    
    /**
     * Escapes all special solr/query characters in the given query term
     * <em>not</em> enclosed in quotes (single term).
     * At current time, this includes at least: 
     * <code>+ - && || ! ( ) { } [ ] ^ " ~ * ? : \ /</code> and whitespace.
     * NOTE: The result should NOT be enclosed in quotes; use {@link #escapeTermForQuote} for that.
     * FIXME?: whitespace escaping appears to not always be honored by solr parser?...
     * @see #escapeTermForQuote
     */
    public static String escapeTermPlain(String term) {
        return ClientUtils.escapeQueryChars(term);
        // Reference implementation:
//        StringBuilder sb = new StringBuilder();
//        for (int i = 0; i < s.length(); i++) {
//          char c = s.charAt(i);
//          // These characters are part of the query syntax and must be escaped
//          if (c == '\\' || c == '+' || c == '-' || c == '!'  || c == '(' || c == ')' || c == ':'
//            || c == '^' || c == '[' || c == ']' || c == '\"' || c == '{' || c == '}' || c == '~'
//            || c == '*' || c == '?' || c == '|' || c == '&'  || c == ';' || c == '/'
//            || Character.isWhitespace(c)) {
//            sb.append('\\');
//          }
//          sb.append(c);
//        }
//        return sb.toString();
    }

    /**
     * Escapes all special solr/query characters in the given query term intended to be
     * enclosed in double-quotes (phrase).
     * At current time, this escapes the backslash and double-quote characters only.
     * @see #escapeTermPlain
     */
    public static String escapeTermForQuote(String term) {
        final String s = term;
        // Reference implementation: http://api.drupalhelp.net/api/apachesolr/SolrPhpClient--Apache--Solr--Service.php/function/Apache_Solr_Service%3A%3AescapePhrase/5
        // TODO: REVIEW: make sure this actually corresponds to the solr/lucene parser implementation,
        // w.r.t. the backslash handling; the php reference might be unofficial...
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            // there is no provided implementation for this...
            if (c == '\\' || c == '\"') {
                sb.append('\\');
            }
            sb.append(c);
        }
        return sb.toString();
    }
    
    /**
     * Escapes the term using {@link #escapeTermForQuote} and returns it within double-quotes.
     * Convenience method.
     */
    public static String escapeTermAndQuote(String term) {
        return "\"" + escapeTermForQuote(term) + "\"";
    }
    
    /**
     * ABSTRACTED escaping method that will fully escape the given term using either {@link #escapeTermAndQuote}
     * or {@link #escapeTermPlain} or another, at its own discretion or based on configuration.
     * The result should NOT and NEVER be placed in quotes; it should be treated as containing its own quotes, even
     * if the escaping method is changed.
     * <p>
     * DEV NOTE: this is to factor out the escaping code to simplify things later, because solr is not
     * honoring <code>escapeTermPlain</code> as expected.
     * <p>
     * 2017-07-21: At current time, uses {@link #escapeTermPlain} - SEE KNOWN ISSUES.
     */
    public static String escapeTermFull(String term) {
        return escapeTermPlain(term);
    }
    
    /**
     * BEST-EFFORT function to extract top-level terms in the solr query, quoted and unquoted.
     * WARN: RELIES ON WHITESPACE to split terms; this can't fully emulate the solr parser, which has a lot of quirks; we rely
     * on spaces, but solr parser inserts its own logical spaces, so this will never be exact.
     * can't reliably split on quote or parenthesis because of modifiers, which we're better off not
     * trying to deal with for now...
     * FIXME: only supports quotes and parenthesis
     * TODO: REVIEW: only partial solr syntax support
     * FIXME?: the quote escaping is probably not handled properly, solr parser may be different
     */
    public static List<String> extractTopTerms(String queryExpr) {
        List<String> terms = new ArrayList<>();

        queryExpr = queryExpr.trim().replaceAll("\\s+", " "); // normalize
        
        int backslashCount = 0;
        StringBuilder term = new StringBuilder();
        
        int i = 0;
        while (i < queryExpr.length()) {
            char c = queryExpr.charAt(i);
            int nextBackslashCount = 0;
            if (c == '\\') {
             // BEST-EFFORT emulate escaping (solr parser quirky)
                nextBackslashCount = backslashCount + 1;
                term.append(c);
            } else if (isQueryCharEscaped(c, backslashCount)) {
                term.append(c);
            } else {
                if (termEnclosingCharMap.containsKey(c)) {
                    int endIndex = findTermClosingCharIndex(queryExpr, i, c);
                    if (endIndex > i) {
                        term.append(queryExpr.substring(i, endIndex+1));
                        i = endIndex; // NOTE: gets ++ after
                    } else { 
                        // BAD SYNTAX (probably): append the rest, even though it will probably fail...
                        term.append(queryExpr.substring(i));
                        i = queryExpr.length(); // abort
                    }
                } else if (c == ' ') {
                    if (term.length() > 0) terms.add(term.toString());
                    term = new StringBuilder();
                } else {
                    term.append(c);
                }
            }
            backslashCount = nextBackslashCount;
            i++;
        }
        if (term.length() > 0) terms.add(term.toString());
        
        return terms;
    }
    
    static boolean isQueryCharEscaped(char c, int backslashCount) {
        // TODO: REVIEW: simplified escaping logic, works in some languages,
        // but solr parser might not...
        // WARN: 2017-08-25: solr parser 5 doesn't seem to respect whitespace escaping, but
        // because not sure if bug, will honor it here for now (not making special case)...
        return ((backslashCount % 2) != 0);
    }
    
    static boolean isQueryCharEscaped(String queryExpr, int charIndex, char theChar) {
        int backslashCount = 0;
        for(int i = (charIndex-1); i >= 0; i--) {
            char c = queryExpr.charAt(i);
            if (c == '\\') backslashCount++;
            else break;
        }
        return isQueryCharEscaped(theChar, backslashCount);
    }

    static boolean isQueryCharEscaped(String queryExpr, int charIndex) {
        return isQueryCharEscaped(queryExpr, charIndex, queryExpr.charAt(charIndex));
    }
    
    private static int findTermClosingCharIndex(String queryExpr, int start, char openChar) {
        int i = start + 1;
        char closingChar = termEnclosingCharMap.get(openChar); // NPE if bad openChar
        if (openChar == '"') { // for quote, can ignore all chars except quote and backslash
            int backslashCount = 0;
            while (i < queryExpr.length()) {
                char c = queryExpr.charAt(i);
                if (c == '"' && !isQueryCharEscaped(c, backslashCount)) {
                    return i;
                } else if (c == '\\') {
                    backslashCount += 1;
                } else {
                    backslashCount = 0;
                }
                i++;
            }
        } else { // for parenthesis, must be careful about nested paren AND quotess
            int backslashCount = 0;
            while (i < queryExpr.length()) {
                char c = queryExpr.charAt(i);
                if (c == '\\') {
                    backslashCount += 1;
                } else {
                    if (!isQueryCharEscaped(c, backslashCount)) {
                        if (c == closingChar) {
                            return i;
                        } else if (c == openChar) {
                            int endParenIndex = findTermClosingCharIndex(queryExpr, i, c); // RECURSE
                            if (endParenIndex > i) {
                                i = endParenIndex; // NOTE: gets ++ after
                            } else {
                                return -1; // abort
                            }
                        } else if (c == '"') {
                            int endQuoteIndex = findTermClosingCharIndex(queryExpr, i, c); // RECURSE
                            if (endQuoteIndex > i) {
                                i = endQuoteIndex; // NOTE: gets ++ after
                            } else {
                                return -1; // abort
                            }
                        }
                    }
                    backslashCount = 0;
                }
                i++;
            }
        } 
        return -1;
    }
    
    /**
     * BEST-EFFORT function that attempts to add a prefix ("+" or "-") to every term in the given
     * queryExpr.
     * TODO: REVIEW: should find a solr expression for this OR will need more work as time goes on...
     */
    public static String addPrefixToAllTerms(String queryExpr, String prefix) {
        return StringUtils.join(addPrefixToAllTerms(extractTopTerms(queryExpr), prefix), " ");
    }
    
    public static List<String> addPrefixToAllTerms(List<String> terms, String prefix) {
        List<String> newTerms = new ArrayList<>(terms.size());
        Set<Character> noCharPrefix = noPrefixTermCharPrefixes;
        if (prefix.length() == 1) { // optimization
            noCharPrefix = new HashSet<>(noCharPrefix);
            noCharPrefix.add(prefix.charAt(0));
            for(String term : terms) {
                if (!term.isEmpty() && !noPrefixTerms.contains(term) && !noCharPrefix.contains(term.charAt(0))) {
                    newTerms.add(prefix + term);
                } else {
                    newTerms.add(term);
                }
            }
        } else {
            for(String term : terms) {
                if (!term.isEmpty() && !noPrefixTerms.contains(term) && !noCharPrefix.contains(term.charAt(0)) && !term.startsWith(prefix)) {
                    newTerms.add(prefix + term);
                } else {
                    newTerms.add(term);
                }
            }
        }
        return newTerms;
    }
    
    /**
     * Makes an expression to match a category ID for a special category field, whose values
     * are in the format: <code>X/PARENT/CATEGORY</code> (where X is the category depth); 
     * assumes the passed category ID is already escaped.
     * NOTE: the field name is not escaped (should be hardcoded).
     */
    public static String makeCategoryIdFieldQueryRaw(String fieldName, String escapedProductCategoryId, boolean includeSubCategories) {
        // can be:
        // */CATID
        // */CATID/*
        // NOTE: at this time, should not be any CATID/*, 
        // because there should always be a category depth as first entry (this was the chosen convention)
        StringBuilder sb = new StringBuilder();
        sb.append(fieldName);
        sb.append(":(*\\/");
        sb.append(escapedProductCategoryId);
        if (includeSubCategories) {
            sb.append("* *\\/");
            sb.append(escapedProductCategoryId);
            sb.append("\\/*)");
        } else {
            sb.append(")");
        }
        return sb.toString();
    }
    
    /**
     * Makes an expression to match a category ID for a special category field, whose values
     * are in the format: <code>X/PARENT/CATEGORY</code>, and automatically escapes the passed category ID.
     * NOTE: the field name is not escaped (should be hardcoded).
     */
    public static String makeCategoryIdFieldQueryEscape(String fieldName, String escapedProductCategoryId, boolean includeSubCategories) {
        return makeCategoryIdFieldQueryRaw(fieldName, escapeTermFull(escapedProductCategoryId), includeSubCategories);
    }
   
    /**
     * Tries to return a field language code for the solr schema for the locale.
     * For "en_US", returns the "en" part.
     * TODO: REVIEW: sketchy
     */
    public static String getSolrSchemaLangCode(Locale locale) {
        if (locale == null) return null;
        return locale.getLanguage();
    }
    
    public static String getSolrSchemaLangCodeValid(Locale locale) {
        String res = getSolrSchemaLangCode(locale);
        if (SolrUtil.solrContentLocales.contains(Locale.forLanguageTag(res))) return res;
        else return null;
    }
    
    public static String getSolrSchemaLangCodeValidOrDefault(Locale locale) {
        String res = getSolrSchemaLangCodeValid(locale);
        return res != null ? res : getSolrSchemaLangCode(SolrUtil.solrContentLocaleDefault);
    }
    
    /**
     * Tries to return a field language locale for the solr schema for the locale.
     * For "en_US", returns "en" locale.
     * TODO: REVIEW: sketchy
     */
    public static Locale getSolrSchemaLangLocale(Locale locale) {
        return (locale == null) ? null : Locale.forLanguageTag(getSolrSchemaLangCode(locale));
    }
    
    public static Locale getSolrSchemaLangLocaleValid(Locale locale) {
        Locale res = getSolrSchemaLangLocale(locale);
        if (SolrUtil.solrContentLocales.contains(res)) return res;
        else return null;
    }
    
    public static Locale getSolrSchemaLangLocaleValidOrDefault(Locale locale) {
        Locale res = getSolrSchemaLangLocaleValid(locale);
        return res != null ? res : SolrUtil.solrContentLocaleDefault; 
    }
    
    /**
     * Checks if the exception extracted by {@link #getSolrNestedException} is a syntax error.
     * FIXME: AWFUL HEURISTIC
     */
    public static boolean isSolrQuerySyntaxError(Throwable t) {
        // exception message usually contains the string: "org.apache.solr.search.SyntaxError"
        // hopefully this is accurate enough... how else to check? cause is not set and
        // the root SyntaxError is from an inaccessible jar (CANNOT add it to classpath)
        return ((t instanceof SolrException) && t.getMessage().toLowerCase().contains("syntax")); 
    }
}
