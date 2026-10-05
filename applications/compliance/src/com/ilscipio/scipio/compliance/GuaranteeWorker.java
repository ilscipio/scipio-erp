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
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;

/**
 * EU harmonised notice on the legal guarantee and EU GARAN label (Implementing Regulation (EU) 2025/1960), with the
 * official Commission files in compliance-static/eu (unchanged; see the README there).
 *
 * <p>Rules from the Commission's practical guidelines: the full colour notice, legible at default size, opened
 * on the first click from a "Your legal guarantee rights" link, plus a clickable link to the QR code target; the
 * GARAN label only for a producer guarantee of durability longer than 2 years, tied to the product, as a nested
 * label that opens the full label.</p>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public final class GuaranteeWorker {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String STATIC_BASE = "/compliance-static/eu/";
    private static final String FILE_BASE = "component://compliance/webapp/compliance-static/eu/";
    public static final String ATTR_YEARS = "DURABILITY_GUARANTEE_YEARS";
    public static final String ATTR_BRAND = "GARAN_BRAND";
    public static final String ATTR_MODEL = "GARAN_MODEL";

    /** Languages with an official colour PNG; English exists only as SVG. */
    private static final String PNG_LANGS = "bg cs da de el es et fi fr ga hr hu it lt lv mt nl pl pt ro sk sl sv";

    /** Target of the QR code per language (Commission guidelines, section 2.3). */
    private static final Map<String, String> YOUR_EUROPE = new HashMap<>();
    static {
        YOUR_EUROPE.put("bg", "\u0433\u0430\u0440\u0430\u043d\u0446\u0438\u0438");
        YOUR_EUROPE.put("hr", "jamstva_hr");
        YOUR_EUROPE.put("cs", "z\u00e1ruky_cs");
        YOUR_EUROPE.put("da", "garantier");
        YOUR_EUROPE.put("nl", "garantie");
        YOUR_EUROPE.put("de", "garantien");
        YOUR_EUROPE.put("el", "\u03b5\u03b3\u03b3\u03c5\u03ae\u03c3\u03b5\u03b9\u03c2");
        YOUR_EUROPE.put("en", "guarantees");
        YOUR_EUROPE.put("et", "garantiid");
        YOUR_EUROPE.put("fi", "virhevastuu");
        YOUR_EUROPE.put("fr", "garanties");
        YOUR_EUROPE.put("hu", "j\u00f3t\u00e1ll\u00e1s");
        YOUR_EUROPE.put("ga", "r\u00e1tha\u00edochta\u00ed");
        YOUR_EUROPE.put("it", "garanzie");
        YOUR_EUROPE.put("lt", "garantijos");
        YOUR_EUROPE.put("lv", "garantijas");
        YOUR_EUROPE.put("mt", "garanziji");
        YOUR_EUROPE.put("pl", "gwarancje");
        YOUR_EUROPE.put("pt", "garantias");
        YOUR_EUROPE.put("ro", "garan\u021bii");
        YOUR_EUROPE.put("sk", "z\u00e1ruky_sk");
        YOUR_EUROPE.put("sl", "jamstva_sl");
        YOUR_EUROPE.put("es", "garant\u00edas");
        YOUR_EUROPE.put("sv", "reklamationsr\u00e4tt");
    }

    private static final UtilCache<String, String> svgCache = UtilCache.createUtilCache("compliance.garanSvg", 0, 0, 0, false);

    private GuaranteeWorker() {}

    private static String lang(Locale locale) {
        String l = locale != null ? locale.getLanguage() : "en";
        return YOUR_EUROPE.containsKey(l) ? l : "en";
    }

    /** URL of the official colour notice for the locale (PNG, or the English SVG). */
    public static String getNoticeUrl(Locale locale) {
        String l = lang(locale);
        return PNG_LANGS.contains(l) ? STATIC_BASE + "notice/notice-" + l + ".png" : STATIC_BASE + "notice/notice-en.svg";
    }

    /** The official PNG for e-mails, or null for English (the Commission provides no English raster file). */
    public static String getNoticePngUrl(Locale locale) {
        String l = lang(locale);
        return PNG_LANGS.contains(l) ? STATIC_BASE + "notice/notice-" + l + ".png" : null;
    }

    /** The QR code target of the notice, for the always-present text link. */
    public static String getYourEuropeUrl(Locale locale) {
        try {
            return new URI("https", "europa.eu", "/youreurope/" + YOUR_EUROPE.get(lang(locale)), null).toASCIIString();
        } catch (Exception e) {
            return "https://europa.eu/youreurope/guarantees";
        }
    }

    /** Display text of the link, e.g. europa.eu/youreurope/guarantees. */
    public static String getYourEuropeLabel(Locale locale) {
        return "europa.eu/youreurope/" + YOUR_EUROPE.get(lang(locale));
    }

    /** The store shows the notice when it sells in the EU (or has no profile yet). */
    public static boolean isNoticeRequired(Delegator delegator, String productStoreId) {
        GenericValue profile = LegalDocumentWorker.getProfile(delegator, productStoreId);
        return profile == null || LegalDocumentWorker.hasJurisdiction(profile, "EU");
    }

    /** GARAN data of a product: years, brand, model; null without a producer guarantee of more than 2 years. */
    public static Map<String, String> getGaranData(Delegator delegator, GenericValue product) {
        if (product == null) {
            return null;
        }
        try {
            String productId = product.getString("productId");
            String years = attr(delegator, productId, ATTR_YEARS);
            GenericValue parent = null;
            if (years == null && "Y".equals(product.getString("isVariant"))) {
                GenericValue assoc = EntityQuery.use(delegator).from("ProductAssoc").where("productIdTo", productId, "productAssocTypeId", "PRODUCT_VARIANT")
                        .filterByDate().cache().queryFirst();
                parent = assoc != null ? assoc.getRelatedOne("MainProduct", true) : null;
                years = parent != null ? attr(delegator, parent.getString("productId"), ATTR_YEARS) : null;
            }
            if (years == null) {
                return null;
            }
            int y;
            try {
                y = (int) Double.parseDouble(years.trim());
            } catch (NumberFormatException e) {
                return null;
            }
            if (y <= 2) {
                return null;
            }
            String brand = attr(delegator, productId, ATTR_BRAND);
            if (brand == null) {
                brand = UtilValidate.isNotEmpty(product.getString("brandName")) ? product.getString("brandName")
                        : (parent != null ? parent.getString("brandName") : null);
            }
            String model = attr(delegator, productId, ATTR_MODEL);
            if (model == null) {
                GenericValue gi = EntityQuery.use(delegator).from("GoodIdentification").where("productId", productId, "goodIdentificationTypeId", "MANUFACTURER_ID_NO").cache().queryFirst();
                model = gi != null ? gi.getString("idValue") : productId;
            }
            Map<String, String> out = new LinkedHashMap<>();
            out.put("years", String.valueOf(y));
            out.put("brand", brand != null ? brand : "");
            out.put("model", model);
            return out;
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return null;
        }
    }

    private static String attr(Delegator delegator, String productId, String name) throws GenericEntityException {
        GenericValue a = EntityQuery.use(delegator).from("ProductAttribute").where("productId", productId, "attrName", name).cache().queryOne();
        return a != null && UtilValidate.isNotEmpty(a.getString("attrValue")) ? a.getString("attrValue") : null;
    }

    /**
     * The official GARAN SVG (nested or full) with the product values filled in (XML-escaped) and its ids and
     * classes prefixed, ready to be inlined in a page (so the page's Inter font applies).
     */
    public static String renderGaranSvg(boolean nested, Map<String, String> data, String prefix) {
        String file = nested ? "garan/garan-nested.svg" : "garan/garan-colour.svg";
        String svg = svgCache.get(file);
        if (svg == null) {
            try (InputStream in = FlexibleLocation.resolveLocation(FILE_BASE + file).openStream()) {
                svg = new String(in.readAllBytes(), StandardCharsets.UTF_8);
                svg = svg.replaceFirst("(?s)^.*?(<svg\\b)", "$1"); // drop XML declaration and comments before <svg>
                svgCache.put(file, svg);
            } catch (Exception e) {
                Debug.logError(e, "Could not read " + file, module);
                return "";
            }
        }
        String p = prefix.replaceAll("[^A-Za-z0-9_-]", "_") + "-";
        String out = svg.replace(">XX<", ">" + xml(data.get("years")) + "<");
        if (!nested) {
            out = out.replaceFirst("(<text[^>]*>)<tspan x=\"0\" y=\"0\">Brand/</tspan>.*?</text>",
                            "$1<tspan x=\"0\" y=\"0\">" + java.util.regex.Matcher.quoteReplacement(xml(data.get("brand"))) + "</tspan></text>")
                     .replace(">Brand/Trademark<", ">" + xml(data.get("brand")) + "<")
                     .replace(">Model identifier<", ">" + xml(data.get("model")) + "<");
        }
        out = out.replaceAll("\\bcls-", p + "cls-")
                 .replaceAll("\\bid=\"([^\"]+)\"", "id=\"" + p + "$1\"")
                 .replaceAll("url\\(#([^)]+)\\)", "url(#" + p + "$1)")
                 .replaceAll("href=\"#([^\"]+)\"", "href=\"#" + p + "$1\"");
        return out.replaceFirst("<svg\\b", "<svg role=\"img\" focusable=\"false\"");
    }

    private static String xml(String s) {
        if (s == null) {
            return "";
        }
        return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;");
    }
}
