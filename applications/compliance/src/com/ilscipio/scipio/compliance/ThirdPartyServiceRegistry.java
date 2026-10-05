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

import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilURL;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Lists the third-party services that receive shopper data in a store: the "installed dependencies" of the
 * privacy policy, the cookie dialog and the checkout script inventory.
 *
 * <p>Sources, in order: the catalog {@code compliance/config/known-services.xml} (entries whose trigger
 * matches the store), each installed component's {@code config/third-party-services.xml}, then the merchant's
 * {@code ThirdPartyService} rows of the store (add, change, or hide with enabled=N).</p>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public final class ThirdPartyServiceRegistry {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String CATALOG_RESOURCE = "known-services.xml";
    public static final String COMPONENT_FILE = "config/third-party-services.xml";

    private static final UtilCache<String, List<ServiceEntry>> storeCache =
            UtilCache.createUtilCache("compliance.thirdPartyServices", 0, 0, 300000, false);

    private ThirdPartyServiceRegistry() {}

    /** One service as the privacy policy, the cookie dialog and the CSP see it. Immutable. */
    public static final class ServiceEntry {
        private final Map<String, String> fields;

        ServiceEntry(Map<String, String> fields) {
            this.fields = Collections.unmodifiableMap(new LinkedHashMap<>(fields));
        }

        public String getId() { return get("id"); }
        public String getName() { return get("name"); }
        public String getProvider() { return get("provider"); }
        public String getPurpose() { return get("purpose"); }
        public String getCategory() { return UtilValidate.isNotEmpty(get("category")) ? get("category") : "NECESSARY"; }
        public String getData() { return get("data"); }
        public String getCountries() { return get("countries"); }
        public String getLegalBasis() { return get("legal-basis"); }
        public String getPrivacyUrl() { return get("privacy-url"); }
        public String getCookies() { return get("cookies"); }
        public String getSource() { return get("source"); }
        public boolean isConsentRequired() { return !"NECESSARY".equals(getCategory()); }

        public List<String> getScriptDomains() {
            List<String> out = new ArrayList<>();
            String v = get("script-domains");
            if (UtilValidate.isNotEmpty(v)) {
                for (String d : v.split(",")) {
                    if (!d.trim().isEmpty()) {
                        out.add(d.trim());
                    }
                }
            }
            return out;
        }

        public String get(String name) {
            String v = fields.get(name);
            return v != null ? v : "";
        }

        /** For FreeMarker: all fields as a map. */
        public Map<String, String> getFields() { return fields; }

        @Override
        public String toString() { return "ServiceEntry" + fields; }
    }

    /**
     * Returns the services of the store, sorted by category then name. Cached for 5 minutes;
     * {@link #clearCache()} after a change.
     */
    public static List<ServiceEntry> getServices(Delegator delegator, String productStoreId) {
        String key = delegator.getDelegatorName() + "::" + productStoreId;
        List<ServiceEntry> services = storeCache.get(key);
        if (services == null) {
            services = Collections.unmodifiableList(buildServices(delegator, productStoreId));
            storeCache.put(key, services);
        }
        return services;
    }

    /** Returns the services of one category (NECESSARY, PREFERENCES, STATISTICS, MARKETING). */
    public static List<ServiceEntry> getServices(Delegator delegator, String productStoreId, String category) {
        List<ServiceEntry> out = new ArrayList<>();
        for (ServiceEntry s : getServices(delegator, productStoreId)) {
            if (s.getCategory().equals(category)) {
                out.add(s);
            }
        }
        return out;
    }

    /**
     * Returns a short hash of the service list. A published privacy policy stores it; a different current
     * hash means a service was added or removed since then.
     */
    public static String getRegistryHash(List<ServiceEntry> services) {
        List<String> parts = new ArrayList<>();
        for (ServiceEntry s : services) {
            parts.add(s.getId() + "|" + s.getCategory() + "|" + s.getProvider() + "|" + String.join(",", s.getScriptDomains()));
        }
        Collections.sort(parts);
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            byte[] hash = md.digest(String.join("\n", parts).getBytes(StandardCharsets.UTF_8));
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < 8; i++) {
                sb.append(String.format("%02x", hash[i]));
            }
            return sb.toString();
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }

    public static String getRegistryHash(Delegator delegator, String productStoreId) {
        return getRegistryHash(getServices(delegator, productStoreId));
    }

    public static void clearCache() {
        storeCache.clear();
    }

    private static List<ServiceEntry> buildServices(Delegator delegator, String productStoreId) {
        Set<String> webAnalyticsTypes = new HashSet<>();
        Set<String> gatewayTypes = new HashSet<>();
        Set<String> carrierParties = new HashSet<>();
        try {
            for (GenericValue webSite : EntityQuery.use(delegator).from("WebSite").where("productStoreId", productStoreId).cache().queryList()) {
                for (GenericValue wac : EntityQuery.use(delegator).from("WebAnalyticsConfig")
                        .where("webSiteId", webSite.getString("webSiteId")).cache().queryList()) {
                    webAnalyticsTypes.add(wac.getString("webAnalyticsTypeId"));
                }
            }
            for (GenericValue ps : EntityQuery.use(delegator).from("ProductStorePaymentSetting").where("productStoreId", productStoreId).cache().queryList()) {
                String configId = ps.getString("paymentGatewayConfigId");
                if (UtilValidate.isNotEmpty(configId)) {
                    GenericValue pgc = EntityQuery.use(delegator).from("PaymentGatewayConfig").where("paymentGatewayConfigId", configId).cache().queryOne();
                    if (pgc != null) {
                        gatewayTypes.add(pgc.getString("paymentGatewayConfigTypeId"));
                    }
                }
            }
            for (GenericValue sm : EntityQuery.use(delegator).from("ProductStoreShipmentMeth").where("productStoreId", productStoreId).cache().queryList()) {
                carrierParties.add(sm.getString("partyId"));
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "Could not read the store configuration of store [" + productStoreId + "] for the service registry", module);
        }

        Map<String, Map<String, String>> byId = new LinkedHashMap<>();
        for (Element el : readServiceElements(UtilURL.fromResource(CATALOG_RESOURCE))) {
            if (matches(el, webAnalyticsTypes, gatewayTypes, carrierParties)) {
                putEntry(byId, el, "catalog");
            }
        }
        for (ComponentConfig cc : componentsSafe()) {
            if (!cc.enabled()) {
                continue;
            }
            Path file = Paths.get(cc.getRootLocation(), COMPONENT_FILE);
            if (Files.isRegularFile(file)) {
                try {
                    for (Element el : readServiceElements(file.toUri().toURL())) {
                        putEntry(byId, el, "component:" + cc.getComponentName());
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Could not read " + file, module);
                }
            }
        }
        try {
            for (GenericValue row : EntityQuery.use(delegator).from("ThirdPartyService").where("productStoreId", productStoreId).queryList()) {
                String id = row.getString("serviceId");
                if ("N".equals(row.getString("enabled"))) {
                    byId.remove(id);
                    continue;
                }
                Map<String, String> f = byId.containsKey(id) ? byId.get(id) : new LinkedHashMap<>();
                f.put("id", id);
                putIfSet(f, "name", row.getString("serviceName"));
                putIfSet(f, "provider", row.getString("providerName"));
                putIfSet(f, "purpose", row.getString("purpose"));
                putIfSet(f, "category", row.getString("categoryId"));
                putIfSet(f, "cookies", row.getString("cookies"));
                putIfSet(f, "data", row.getString("dataCategories"));
                putIfSet(f, "countries", row.getString("countries"));
                putIfSet(f, "legal-basis", row.getString("legalBasis"));
                putIfSet(f, "privacy-url", row.getString("privacyUrl"));
                putIfSet(f, "script-domains", row.getString("scriptDomains"));
                f.put("source", "store");
                byId.put(id, f);
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "Could not read ThirdPartyService rows of store [" + productStoreId + "]", module);
        }

        List<ServiceEntry> out = new ArrayList<>();
        for (Map<String, String> f : byId.values()) {
            out.add(new ServiceEntry(f));
        }
        out.sort((a, b) -> {
            int c = Integer.compare(categoryOrder(a.getCategory()), categoryOrder(b.getCategory()));
            if (c != 0) {
                return c;
            }
            if ("this-store".equals(a.getId())) {
                return -1;
            }
            if ("this-store".equals(b.getId())) {
                return 1;
            }
            return a.getName().compareToIgnoreCase(b.getName());
        });
        return out;
    }

    private static int categoryOrder(String category) {
        switch (category) {
        case "NECESSARY": return 0;
        case "PREFERENCES": return 1;
        case "STATISTICS": return 2;
        case "MARKETING": return 3;
        default: return 4;
        }
    }

    private static boolean matches(Element el, Set<String> webAnalyticsTypes, Set<String> gatewayTypes, Set<String> carrierParties) {
        if ("true".equals(el.getAttribute("always"))) {
            return true;
        }
        String component = el.getAttribute("component");
        if (!component.isEmpty() && isComponentInstalled(component)) {
            return true;
        }
        String wat = el.getAttribute("web-analytics-type");
        if (!wat.isEmpty() && webAnalyticsTypes.contains(wat)) {
            return true;
        }
        String pgt = el.getAttribute("payment-gateway-type");
        if (!pgt.isEmpty() && gatewayTypes.contains(pgt)) {
            return true;
        }
        String carrier = el.getAttribute("carrier-party");
        return !carrier.isEmpty() && carrierParties.contains(carrier);
    }

    private static boolean isComponentInstalled(String componentName) {
        for (ComponentConfig cc : componentsSafe()) {
            if (componentName.equals(cc.getComponentName()) || componentName.equals(cc.getGlobalName())) {
                return cc.enabled();
            }
        }
        return false;
    }

    private static Collection<ComponentConfig> componentsSafe() {
        try {
            return ComponentConfig.getAllComponents();
        } catch (Exception e) {
            return Collections.emptyList();
        }
    }

    private static void putEntry(Map<String, Map<String, String>> byId, Element el, String source) {
        String id = el.getAttribute("id");
        if (id.isEmpty()) {
            return;
        }
        Map<String, String> f = byId.containsKey(id) ? byId.get(id) : new LinkedHashMap<>();
        for (String attr : new String[] {"id", "name", "provider", "purpose", "category", "data", "countries",
                "legal-basis", "privacy-url", "cookies", "script-domains"}) {
            if (el.hasAttribute(attr)) {
                f.put(attr, el.getAttribute(attr));
            }
        }
        f.put("source", source);
        byId.put(id, f);
    }

    private static void putIfSet(Map<String, String> f, String name, String value) {
        if (UtilValidate.isNotEmpty(value)) {
            f.put(name, value);
        }
    }

    private static List<Element> readServiceElements(URL url) {
        List<Element> out = new ArrayList<>();
        if (url == null) {
            return out;
        }
        try {
            Document doc = UtilXml.readXmlDocument(url, false);
            if (doc != null) {
                out.addAll(UtilXml.childElementList(doc.getDocumentElement(), "service"));
            }
        } catch (Exception e) {
            Debug.logError(e, "Could not read service catalog " + url, module);
        }
        return out;
    }
}
