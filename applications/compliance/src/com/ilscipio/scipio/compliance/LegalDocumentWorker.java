/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package com.ilscipio.scipio.compliance;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.codec.HtmlSanitizerPolicies;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.owasp.html.PolicyFactory;
import org.owasp.html.Sanitizers;

/**
 * Legal documents of a store: the published version for a locale, or the shipped template when the store
 * has published none, rendered with the store's data in place of the {{token}} placeholders.
 *
 * <p>Security: merchant text is never evaluated as a template. Placeholders are a fixed list, replaced with
 * HTML-escaped values; the body goes through an OWASP policy before and after the replacement.</p>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public final class LegalDocumentWorker {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String TEMPLATE_LOCATION = "component://compliance/data/templates/";
    public static final String STATUS_PUBLISHED = "LDS_PUBLISHED";

    // Matches {{name}} before and after sanitizing: the OWASP policy breaks "{{" as "{<!-- -->{" (and may
    // encode braces as &#123; / &#125;) to stop template injection.
    private static final Pattern TOKEN = Pattern.compile(
            "(?:\\{|&#123;)(?:<!-- -->)?(?:\\{|&#123;)\\s*([a-zA-Z][a-zA-Z0-9_.]*)\\s*(?:\\}|&#125;)(?:<!-- -->)?(?:\\}|&#125;)");
    private static final PolicyFactory POLICY = HtmlSanitizerPolicies.StrictOwaspPolicy.DEFAULT_STRICT_POLICY.and(Sanitizers.TABLES);
    private static final PolicyFactory POLICY_WITH_MARK = POLICY.and(new org.owasp.html.HtmlPolicyBuilder().allowElements("mark").toFactory());
    private static final String NO_TEMPLATE = "";
    private static final org.ofbiz.base.util.cache.UtilCache<String, String> templateCache =
            org.ofbiz.base.util.cache.UtilCache.createUtilCache("compliance.legalTemplates", 0, 0, 600000, false);

    private LegalDocumentWorker() {}

    /** Document types in footer order: enumId, slug (enumCode), description. */
    public static List<GenericValue> getDocTypes(Delegator delegator) {
        try {
            return EntityQuery.use(delegator).from("Enumeration").where("enumTypeId", "LEGAL_DOC_TYPE").orderBy("sequenceId").cache().queryList();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return new ArrayList<>();
        }
    }

    public static GenericValue getDocTypeBySlug(Delegator delegator, String slug) {
        for (GenericValue t : getDocTypes(delegator)) {
            if (t.getString("enumCode").equals(slug) || t.getString("enumId").equals(slug)) {
                return t;
            }
        }
        return null;
    }

    public static GenericValue getProfile(Delegator delegator, String productStoreId) {
        if (UtilValidate.isEmpty(productStoreId)) {
            return null;
        }
        try {
            return EntityQuery.use(delegator).from("StoreComplianceProfile").where("productStoreId", productStoreId).cache().queryOne();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return null;
        }
    }

    /** Returns the jurisdictions of the store profile, e.g. [EU, US, US-CA]; empty without a profile. */
    public static List<String> getJurisdictions(GenericValue profile) {
        List<String> out = new ArrayList<>();
        if (profile != null && UtilValidate.isNotEmpty(profile.getString("jurisdictions"))) {
            for (String j : profile.getString("jurisdictions").split(",")) {
                if (!j.trim().isEmpty()) {
                    out.add(j.trim().toUpperCase(Locale.ROOT));
                }
            }
        }
        return out;
    }

    public static boolean hasJurisdiction(GenericValue profile, String prefix) {
        for (String j : getJurisdictions(profile)) {
            if (j.equals(prefix) || j.startsWith(prefix + "-")) {
                return true;
            }
        }
        return false;
    }

    /** The latest published version for the locale (language fallback, then any locale), or null. */
    public static GenericValue getPublished(Delegator delegator, String productStoreId, String docTypeId, Locale locale) {
        try {
            List<GenericValue> docs = EntityQuery.use(delegator).from("LegalDocument")
                    .where("productStoreId", productStoreId, "docTypeId", docTypeId, "statusId", STATUS_PUBLISHED)
                    .orderBy("-versionNum").cache().queryList();
            if (docs.isEmpty()) {
                return null;
            }
            String full = locale != null ? locale.toString() : "";
            String lang = locale != null ? locale.getLanguage() : "";
            for (GenericValue d : docs) {
                if (full.equals(d.getString("localeString"))) {
                    return d;
                }
            }
            for (GenericValue d : docs) {
                String ls = d.getString("localeString");
                if (ls != null && (ls.equals(lang) || ls.startsWith(lang + "_"))) {
                    return d;
                }
            }
            for (GenericValue d : docs) {
                if (d.getString("localeString") == null || d.getString("localeString").startsWith("en")) {
                    return d;
                }
            }
            return docs.get(0);
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return null;
        }
    }

    /** Reads the shipped template text for the doc type slug; language fallback to English. Null if none. */
    public static String getTemplateText(String slug, Locale locale) {
        List<String> langs = new ArrayList<>();
        if (locale != null && !locale.getLanguage().isEmpty()) {
            langs.add(locale.getLanguage());
        }
        if (!langs.contains("en")) {
            langs.add("en");
        }
        for (String lang : langs) {
            String location = TEMPLATE_LOCATION + lang + "/" + slug + ".html";
            String text = templateCache.get(location);
            if (text == null) {
                text = NO_TEMPLATE;
                try (InputStream in = FlexibleLocation.resolveLocation(location).openStream()) {
                    text = new String(in.readAllBytes(), StandardCharsets.UTF_8);
                } catch (Exception e) {
                    // no template in this language
                }
                templateCache.put(location, text);
            }
            if (!text.isEmpty()) {
                return text;
            }
        }
        return null;
    }

    /**
     * Returns the document to show in the shop: keys title, bodyHtml, versionNum, publishedDate, isTemplate,
     * changeNote, slug, docTypeId. Null when the slug is unknown or no text exists.
     */
    public static Map<String, Object> getDisplayDocument(Delegator delegator, String productStoreId, String slug, Locale locale) {
        GenericValue docType = getDocTypeBySlug(delegator, slug);
        if (docType == null) {
            return null;
        }
        String docTypeId = docType.getString("enumId");
        GenericValue profile = getProfile(delegator, productStoreId);
        GenericValue published = getPublished(delegator, productStoreId, docTypeId, locale);
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("slug", docType.getString("enumCode"));
        out.put("docTypeId", docTypeId);
        String body;
        if (published != null) {
            body = published.getString("bodyText");
            out.put("title", UtilValidate.isNotEmpty(published.getString("title")) ? published.getString("title") : docType.get("description", locale));
            out.put("versionNum", published.get("versionNum"));
            out.put("publishedDate", published.getTimestamp("publishedDate"));
            out.put("changeNote", published.getString("changeNote"));
            out.put("isTemplate", Boolean.FALSE);
        } else {
            body = getTemplateText(docType.getString("enumCode"), locale);
            if (body == null) {
                return null;
            }
            out.put("title", extractTitle(body, (String) docType.get("description", locale)));
            out.put("isTemplate", Boolean.TRUE);
        }
        body = stripTitle(body);
        Map<String, Object> values = getTokenValues(delegator, productStoreId, profile, out, locale);
        out.put("bodyHtml", render(body, values));
        return out;
    }

    /** Renders a body: sanitize, replace tokens with escaped values or generated tables, sanitize again. */
    public static String render(String body, Map<String, Object> values) {
        if (body == null) {
            return "";
        }
        String clean = POLICY.sanitize(body);
        Matcher m = TOKEN.matcher(clean);
        StringBuffer sb = new StringBuffer();
        UtilCodec.SimpleEncoder html = UtilCodec.getEncoder("html");
        while (m.find()) {
            String name = m.group(1);
            Object value = values.get(name);
            String replacement;
            if (value instanceof RawHtml) {
                replacement = ((RawHtml) value).html;
            } else if (value != null && !value.toString().isEmpty()) {
                replacement = html.encode(value.toString());
            } else {
                replacement = "<mark>[" + html.encode(name) + "]</mark>";
            }
            m.appendReplacement(sb, Matcher.quoteReplacement(replacement));
        }
        m.appendTail(sb);
        return POLICY_WITH_MARK.sanitize(sb.toString());
    }

    /** Values for the placeholders. Generated tables are {@link RawHtml}; everything else is escaped. */
    public static Map<String, Object> getTokenValues(Delegator delegator, String productStoreId, GenericValue profile,
                                                     Map<String, Object> doc, Locale locale) {
        Map<String, Object> v = new LinkedHashMap<>();
        GenericValue store = null;
        try {
            store = EntityQuery.use(delegator).from("ProductStore").where("productStoreId", productStoreId).cache().queryOne();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        }
        String storeName = store != null ? store.getString("storeName") : null;
        v.put("store.name", storeName);
        if (profile != null) {
            v.put("store.legalName", firstNonEmpty(profile.getString("legalName"), store != null ? store.getString("companyName") : null, storeName));
            v.put("store.addressLine", profile.getString("addressLine"));
            v.put("store.postalCode", profile.getString("postalCode"));
            v.put("store.city", profile.getString("city"));
            v.put("store.country", countryName(delegator, profile.getString("countryGeoId"), locale));
            v.put("store.address", joinNonEmpty(", ", profile.getString("addressLine"),
                    joinNonEmpty(" ", profile.getString("postalCode"), profile.getString("city")), countryName(delegator, profile.getString("countryGeoId"), locale)));
            v.put("store.email", profile.getString("contactEmail"));
            v.put("store.phone", profile.getString("contactPhone"));
            v.put("store.registerCourt", profile.getString("registerCourt"));
            v.put("store.registerNumber", profile.getString("registerNumber"));
            v.put("store.vatId", profile.getString("vatId"));
            v.put("store.representedBy", profile.getString("representedBy"));
            v.put("dpo.contact", profile.getString("dpoContact"));
            v.put("supervisory.authority", profile.getString("supervisoryAuthority"));
            v.put("withdrawal.days", numberText(profile.get("withdrawalDays"), "14"));
            v.put("returns.days", numberText(profile.get("returnDays"), null));
            v.put("guarantee.years", numberText(profile.get("legalGuaranteeYears"), "2"));
            v.put("retention.years", numberText(profile.get("retentionYears"), null));
        } else {
            v.put("store.legalName", firstNonEmpty(store != null ? store.getString("companyName") : null, storeName));
            v.put("withdrawal.days", "14");
            v.put("guarantee.years", "2");
        }
        if (doc != null) {
            v.put("doc.version", doc.get("versionNum") != null ? doc.get("versionNum").toString() : null);
            Timestamp published = (Timestamp) doc.get("publishedDate");
            v.put("doc.lastUpdated", published != null
                    ? DateFormat.getDateInstance(DateFormat.LONG, locale != null ? locale : Locale.ENGLISH).format(published) : null);
        }
        List<ThirdPartyServiceRegistry.ServiceEntry> services = ThirdPartyServiceRegistry.getServices(delegator, productStoreId);
        v.put("services.table", new RawHtml(servicesTable(services, locale)));
        v.put("cookies.table", new RawHtml(cookiesTable(services, locale)));
        v.put("epr.numbers", new RawHtml(eprList(delegator, productStoreId, store, locale)));
        return v;
    }

    /** Marks a generated value that is already safe HTML. */
    public static final class RawHtml {
        final String html;
        public RawHtml(String html) { this.html = html; }
        @Override public String toString() { return html; }
    }

    private static String servicesTable(List<ThirdPartyServiceRegistry.ServiceEntry> services, Locale locale) {
        UtilCodec.SimpleEncoder html = UtilCodec.getEncoder("html");
        boolean de = locale != null && "de".equals(locale.getLanguage());
        StringBuilder sb = new StringBuilder("<table><thead><tr>");
        for (String h : de ? new String[] {"Dienst", "Anbieter", "Zweck", "Daten", "Kategorie", "Ort", "Rechtsgrundlage"}
                           : new String[] {"Service", "Provider", "Purpose", "Data", "Category", "Location", "Legal basis"}) {
            sb.append("<th>").append(h).append("</th>");
        }
        sb.append("</tr></thead><tbody>");
        for (ThirdPartyServiceRegistry.ServiceEntry s : services) {
            sb.append("<tr><td>");
            if (!s.getPrivacyUrl().isEmpty() && s.getPrivacyUrl().startsWith("https://")) {
                sb.append("<a href=\"").append(html.encode(s.getPrivacyUrl())).append("\">").append(html.encode(s.getName())).append("</a>");
            } else {
                sb.append(html.encode(s.getName()));
            }
            sb.append("</td><td>").append(html.encode(s.getProvider()))
              .append("</td><td>").append(html.encode(s.getPurpose()))
              .append("</td><td>").append(html.encode(s.getData()))
              .append("</td><td>").append(html.encode(categoryLabel(s.getCategory(), de)))
              .append("</td><td>").append(html.encode(s.getCountries()))
              .append("</td><td>").append(html.encode(s.getLegalBasis()))
              .append("</td></tr>");
        }
        sb.append("</tbody></table>");
        return sb.toString();
    }

    private static String cookiesTable(List<ThirdPartyServiceRegistry.ServiceEntry> services, Locale locale) {
        UtilCodec.SimpleEncoder html = UtilCodec.getEncoder("html");
        boolean de = locale != null && "de".equals(locale.getLanguage());
        StringBuilder sb = new StringBuilder("<table><thead><tr>");
        for (String h : de ? new String[] {"Cookie", "Dienst", "Kategorie"} : new String[] {"Cookie", "Service", "Category"}) {
            sb.append("<th>").append(h).append("</th>");
        }
        sb.append("</tr></thead><tbody>");
        for (ThirdPartyServiceRegistry.ServiceEntry s : services) {
            if (s.getCookies().isEmpty()) {
                continue;
            }
            sb.append("<tr><td>").append(html.encode(s.getCookies()))
              .append("</td><td>").append(html.encode(s.getName()))
              .append("</td><td>").append(html.encode(categoryLabel(s.getCategory(), de)))
              .append("</td></tr>");
        }
        sb.append("</tbody></table>");
        return sb.toString();
    }

    public static String categoryLabel(String category, boolean de) {
        switch (category) {
        case "NECESSARY": return de ? "Notwendig" : "Necessary";
        case "PREFERENCES": return de ? "Präferenzen (Einwilligung)" : "Preferences (consent)";
        case "STATISTICS": return de ? "Statistik (Einwilligung)" : "Statistics (consent)";
        case "MARKETING": return de ? "Marketing (Einwilligung)" : "Marketing (consent)";
        default: return category;
        }
    }

    private static String eprList(Delegator delegator, String productStoreId, GenericValue store, Locale locale) {
        UtilCodec.SimpleEncoder html = UtilCodec.getEncoder("html");
        String partyId = store != null ? store.getString("payToPartyId") : null;
        if (UtilValidate.isEmpty(partyId)) {
            return "";
        }
        try {
            List<GenericValue> regs = EntityQuery.use(delegator).from("EprRegistration").where("partyId", partyId)
                    .filterByDate().orderBy("countryGeoId", "schemeId").cache().queryList();
            if (regs.isEmpty()) {
                return "<mark>[epr.numbers]</mark>";
            }
            StringBuilder sb = new StringBuilder("<ul>");
            for (GenericValue r : regs) {
                GenericValue scheme = EntityQuery.use(delegator).from("Enumeration").where("enumId", r.getString("schemeId")).cache().queryOne();
                sb.append("<li>").append(html.encode(countryName(delegator, r.getString("countryGeoId"), locale)))
                  .append(", ").append(html.encode(scheme != null ? scheme.getString("description") : r.getString("schemeId")))
                  .append(": ").append(html.encode(r.getString("registrationNumber"))).append("</li>");
            }
            return sb.append("</ul>").toString();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return "";
        }
    }

    /** Footer and navigation links: list of maps slug, docTypeId, label; only types that have a text. */
    public static List<Map<String, Object>> getNavigation(Delegator delegator, String productStoreId, Locale locale) {
        GenericValue profile = getProfile(delegator, productStoreId);
        boolean us = profile == null || hasJurisdiction(profile, "US");
        boolean marketplace = profile != null && "Y".equals(profile.getString("marketplaceMode"));
        List<Map<String, Object>> out = new ArrayList<>();
        for (GenericValue t : getDocTypes(delegator)) {
            String docTypeId = t.getString("enumId");
            if ("LEGDOC_CA_NOTICE".equals(docTypeId) && !us) {
                continue;
            }
            if ("LEGDOC_SELLERS".equals(docTypeId) && !marketplace) {
                continue;
            }
            GenericValue published = getPublished(delegator, productStoreId, docTypeId, locale);
            String label;
            if (published != null && UtilValidate.isNotEmpty(published.getString("title"))) {
                label = published.getString("title");
            } else {
                String tpl = published == null ? getTemplateText(t.getString("enumCode"), locale) : null;
                if (published == null && tpl == null) {
                    continue;
                }
                label = extractTitle(tpl, (String) t.get("description", locale));
            }
            out.add(UtilMisc.toMap("slug", t.getString("enumCode"), "docTypeId", docTypeId, "label", label));
        }
        return out;
    }

    private static final Pattern H1 = Pattern.compile("<h1[^>]*>(.*?)</h1>", Pattern.CASE_INSENSITIVE | Pattern.DOTALL);

    static String extractTitle(String body, String fallback) {
        if (body != null) {
            Matcher m = H1.matcher(body);
            if (m.find()) {
                return m.group(1).replaceAll("<[^>]+>", "").trim();
            }
        }
        return fallback;
    }

    static String stripTitle(String body) {
        return body == null ? null : H1.matcher(body).replaceFirst("");
    }

    /** The country name in the language of the locale (entity labels), else the stored name. */
    private static String countryName(Delegator delegator, String geoId, Locale locale) {
        if (UtilValidate.isEmpty(geoId)) {
            return null;
        }
        try {
            GenericValue geo = EntityQuery.use(delegator).from("Geo").where("geoId", geoId).cache().queryOne();
            if (geo == null) {
                return geoId;
            }
            return locale != null ? (String) geo.get("geoName", locale) : geo.getString("geoName");
        } catch (GenericEntityException e) {
            return geoId;
        }
    }

    private static String numberText(Object n, String fallback) {
        if (n == null) {
            return fallback;
        }
        String s = n.toString();
        return s.endsWith(".0") ? s.substring(0, s.length() - 2) : s;
    }

    private static String firstNonEmpty(String... values) {
        for (String s : values) {
            if (UtilValidate.isNotEmpty(s)) {
                return s;
            }
        }
        return null;
    }

    private static String joinNonEmpty(String sep, String... parts) {
        StringBuilder sb = new StringBuilder();
        for (String p : parts) {
            if (UtilValidate.isNotEmpty(p)) {
                if (sb.length() > 0) {
                    sb.append(sep);
                }
                sb.append(p);
            }
        }
        return sb.length() > 0 ? sb.toString() : null;
    }
}
