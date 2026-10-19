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
package com.ilscipio.scipio.acp.feed;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.acp.AcpConfig;
import com.ilscipio.scipio.acp.AcpJson;
import com.ilscipio.scipio.acp.checkout.AcpContext;

/**
 * SCIPIO: 4.0.0: The ACP product feed of a store: {@code metadata.json} plus {@code products.jsonl} (one Product per
 * line; a virtual product carries its variants, any other product is its own single variant). The variant id is the
 * product id that the agent sends as line_items[].id. Files live in runtime/acp/feed/&lt;productStoreId&gt;/.
 */
public final class AcpFeed {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private AcpFeed() {}

    private static final java.util.concurrent.ConcurrentHashMap<String, Object> LOCKS = new java.util.concurrent.ConcurrentHashMap<>();

    /** The tenant of the delegator ("default" for the base delegator): pooled stores never share a feed. */
    public static String tenantKey(Delegator delegator) {
        String t = delegator.getDelegatorTenantId();
        return UtilValidate.isEmpty(t) ? "default" : t.replaceAll("[^A-Za-z0-9_-]", "_");
    }

    public static File root(Delegator delegator) {
        return new File(System.getProperty("ofbiz.home", "."), "runtime/acp/feed/" + tenantKey(delegator));
    }

    public static File dir(Delegator delegator, String productStoreId) {
        return new File(root(delegator), productStoreId.replaceAll("[^A-Za-z0-9_-]", "_"));
    }

    public static File productsFile(Delegator delegator, String productStoreId) {
        return new File(dir(delegator, productStoreId), "products.jsonl");
    }

    public static File metadataFile(Delegator delegator, String productStoreId) {
        return new File(dir(delegator, productStoreId), "metadata.json");
    }

    /** True when the file is missing or older than maxAgeMillis. */
    public static boolean isStale(Delegator delegator, String productStoreId, long maxAgeMillis) {
        File f = productsFile(delegator, productStoreId);
        return !f.isFile() || System.currentTimeMillis() - f.lastModified() > maxAgeMillis;
    }

    /** Builds the feed when it is older than maxAgeMillis; one build per feed at a time. */
    public static void ensureFresh(AcpContext ctx, long maxAgeMillis) throws IOException, GenericEntityException {
        Object lock = LOCKS.computeIfAbsent(dir(ctx.getDelegator(), ctx.getProductStoreId()).getPath(), k -> new Object());
        synchronized (lock) {
            if (isStale(ctx.getDelegator(), ctx.getProductStoreId(), maxAgeMillis)) {
                generate(ctx);
            }
        }
    }

    /** Writes both files (atomic replace). Returns the number of products. */
    public static int generate(AcpContext ctx) throws IOException, GenericEntityException {
        Object lock = LOCKS.computeIfAbsent(dir(ctx.getDelegator(), ctx.getProductStoreId()).getPath(), k -> new Object());
        synchronized (lock) {
            return generateLocked(ctx);
        }
    }

    private static int generateLocked(AcpContext ctx) throws IOException, GenericEntityException {
        File dir = dir(ctx.getDelegator(), ctx.getProductStoreId());
        if (!dir.isDirectory() && !dir.mkdirs()) {
            throw new IOException("Cannot create " + dir);
        }
        File tmp = File.createTempFile("products", ".tmp", dir);
        int n;
        try (BufferedWriter w = Files.newBufferedWriter(tmp.toPath(), StandardCharsets.UTF_8)) {
            n = write(ctx, w);
        } catch (IOException | GenericEntityException | RuntimeException e) {
            Files.deleteIfExists(tmp.toPath());
            throw e;
        }
        Files.move(tmp.toPath(), productsFile(ctx.getDelegator(), ctx.getProductStoreId()).toPath(), StandardCopyOption.REPLACE_EXISTING);
        Map<String, Object> meta = new LinkedHashMap<>();
        meta.put("id", "feed_" + ctx.getProductStoreId());
        GenericValue store = ctx.getStore();
        String country = store != null ? countryOf(ctx.getDelegator(), store) : null;
        if (country != null) meta.put("target_country", country);
        meta.put("updated_at", Instant.now().toString());
        meta.put("product_count", n);
        // the sidecar lets the daily job rebuild the feed with the address of the store webapp
        meta.put("x_scipio", UtilMisc.toMap("baseUrl", ctx.getBaseUrl(), "webSiteId", ctx.getWebSiteId(), "webappName", ctx.getWebappName()));
        Files.write(metadataFile(ctx.getDelegator(), ctx.getProductStoreId()).toPath(), AcpJson.write(meta).getBytes(StandardCharsets.UTF_8));
        return n;
    }

    /** Writes one JSON product per line. Returns the number of products. */
    public static int write(AcpContext ctx, Writer w) throws IOException, GenericEntityException {
        int max = AcpConfig.getInt(ctx.getDelegator(), "feed.maxProducts", 50000);
        int n = 0;
        for (String productId : productIds(ctx, max)) {
            Map<String, Object> p = product(ctx, productId);
            if (p == null) continue;
            w.write(AcpJson.write(p));
            w.write('\n');
            n++;
        }
        return n;
    }

    /** The sellable top products (no variants) of the browse categories of the store catalogs. */
    static Set<String> productIds(AcpContext ctx, int max) throws GenericEntityException {
        Delegator delegator = ctx.getDelegator();
        String only = AcpConfig.get(delegator, ctx.getProductStoreId(), "feed.prodCatalogId", null);
        List<GenericValue> catalogs = EntityUtil.filterByDate(EntityQuery.use(delegator).from("ProductStoreCatalog")
                .where("productStoreId", ctx.getProductStoreId()).orderBy("sequenceNum").queryList());
        Deque<String> todo = new ArrayDeque<>();
        Set<String> categories = new LinkedHashSet<>();
        for (GenericValue c : catalogs) {
            if (UtilValidate.isNotEmpty(only) && !only.equals(c.getString("prodCatalogId"))) continue;
            for (GenericValue pcc : EntityUtil.filterByDate(EntityQuery.use(delegator).from("ProdCatalogCategory")
                    .where("prodCatalogId", c.getString("prodCatalogId")).queryList())) {
                String type = pcc.getString("prodCatalogCategoryTypeId");
                if ("PCCT_BROWSE_ROOT".equals(type) || "PCCT_PROMOTIONS".equals(type) || "PCCT_BEST_SELL".equals(type)) {
                    todo.add(pcc.getString("productCategoryId"));
                }
            }
        }
        while (!todo.isEmpty() && categories.size() < 5000) {
            String cat = todo.poll();
            if (!categories.add(cat)) continue;
            for (GenericValue roll : EntityUtil.filterByDate(EntityQuery.use(delegator).from("ProductCategoryRollup")
                    .where("parentProductCategoryId", cat).queryList())) {
                todo.add(roll.getString("productCategoryId"));
            }
        }
        Set<String> ids = new LinkedHashSet<>();
        for (String cat : categories) {
            for (GenericValue m : EntityUtil.filterByDate(EntityQuery.use(delegator).from("ProductCategoryMember")
                    .where("productCategoryId", cat).orderBy("sequenceNum").queryList())) {
                ids.add(m.getString("productId"));
                if (ids.size() >= max) return ids;
            }
        }
        return ids;
    }

    /** One feed Product, or null when it is not sellable. */
    static Map<String, Object> product(AcpContext ctx, String productId) throws GenericEntityException {
        Delegator delegator = ctx.getDelegator();
        GenericValue p = EntityQuery.use(delegator).from("Product").where("productId", productId).cache().queryOne();
        if (p == null || "Y".equals(p.getString("isVariant")) || !sellable(p)) return null;
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("id", productId);
        out.put("title", title(p));
        Map<String, Object> desc = description(p);
        if (desc != null) out.put("description", desc);
        out.put("url", ctx.url("/control/product?product_id=" + productId));
        List<Map<String, Object>> media = media(ctx, p);
        if (!media.isEmpty()) out.put("media", media);
        List<Map<String, Object>> variants = new ArrayList<>();
        if ("Y".equals(p.getString("isVirtual"))) {
            for (GenericValue assoc : EntityUtil.filterByDate(EntityQuery.use(delegator).from("ProductAssoc")
                    .where("productId", productId, "productAssocTypeId", "PRODUCT_VARIANT").orderBy("sequenceNum").cache().queryList())) {
                GenericValue v = EntityQuery.use(delegator).from("Product").where("productId", assoc.getString("productIdTo")).cache().queryOne();
                if (v != null && sellable(v)) {
                    Map<String, Object> vm = variant(ctx, v, p);
                    if (vm != null) variants.add(vm);
                }
            }
        } else {
            Map<String, Object> vm = variant(ctx, p, null);
            if (vm != null) variants.add(vm);
        }
        if (variants.isEmpty()) return null;
        out.put("variants", variants);
        return out;
    }

    private static Map<String, Object> variant(AcpContext ctx, GenericValue v, GenericValue parent) throws GenericEntityException {
        Delegator delegator = ctx.getDelegator();
        String id = v.getString("productId");
        String currency = ctx.getDefaultCurrency();
        Map<String, Object> price;
        try {
            price = ctx.getDispatcher().runSync("calculateProductPrice", UtilMisc.toMap("product", v, "productStoreId", ctx.getProductStoreId(),
                    "webSiteId", ctx.getWebSiteId(), "currencyUomId", currency, "userLogin", ctx.getSystemLogin()));
        } catch (GenericServiceException e) {
            return null;
        }
        if (ServiceUtil.isError(price) || price.get("price") == null) return null;
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("id", id);
        String title = title(v);
        if (parent != null && title.equals(id)) title = title(parent);
        List<Map<String, Object>> options = options(delegator, id);
        if (parent != null && !options.isEmpty()) {
            StringBuilder sb = new StringBuilder(title(parent)).append(" -");
            for (Map<String, Object> o : options) sb.append(' ').append(o.get("value"));
            title = sb.toString();
        }
        m.put("title", title);
        Map<String, Object> desc = description(v);
        if (desc == null && parent != null) desc = description(parent);
        if (desc != null) m.put("description", desc);
        m.put("url", ctx.url("/control/product?product_id=" + (parent != null ? parent.getString("productId") + "&variant_id=" + id : id)));
        m.put("price", UtilMisc.toMap("amount", AcpJson.toMinor((BigDecimal) price.get("price"), currency), "currency", currency.toLowerCase(Locale.ROOT)));
        BigDecimal list = (BigDecimal) price.get("listPrice");
        if (list != null && list.compareTo((BigDecimal) price.get("price")) > 0) {
            m.put("list_price", UtilMisc.toMap("amount", AcpJson.toMinor(list, currency), "currency", currency.toLowerCase(Locale.ROOT)));
        }
        boolean available = available(ctx, id);
        m.put("availability", UtilMisc.toMap("available", available, "status", available ? "in_stock" : "out_of_stock"));
        List<Map<String, Object>> barcodes = new ArrayList<>();
        for (GenericValue g : EntityQuery.use(delegator).from("GoodIdentification").where("productId", id).cache().queryList()) {
            String type = g.getString("goodIdentificationTypeId");
            if ("UPCA".equals(type) || "UPCE".equals(type) || "EAN".equals(type) || "ISBN".equals(type)) {
                barcodes.add(UtilMisc.toMap("type", type.startsWith("UPC") ? "upc" : type.toLowerCase(Locale.ROOT), "value", g.getString("idValue")));
            }
        }
        if (!barcodes.isEmpty()) m.put("barcodes", barcodes);
        String catId = v.getString("primaryProductCategoryId") != null ? v.getString("primaryProductCategoryId")
                : parent != null ? parent.getString("primaryProductCategoryId") : null;
        if (catId != null) {
            GenericValue cat = EntityQuery.use(delegator).from("ProductCategory").where("productCategoryId", catId).cache().queryOne();
            if (cat != null) {
                m.put("categories", List.of(UtilMisc.toMap("value", UtilValidate.isNotEmpty(cat.getString("categoryName"))
                        ? cat.getString("categoryName") : catId, "taxonomy", "merchant")));
            }
        }
        if (!options.isEmpty()) m.put("variant_options", options);
        List<Map<String, Object>> media = media(ctx, v);
        if (!media.isEmpty()) m.put("media", media);
        return m;
    }

    private static List<Map<String, Object>> options(Delegator delegator, String productId) throws GenericEntityException {
        List<Map<String, Object>> out = new ArrayList<>();
        for (GenericValue a : EntityUtil.filterByDate(EntityQuery.use(delegator).from("ProductFeatureAndAppl")
                .where("productId", productId, "productFeatureApplTypeId", "STANDARD_FEATURE").orderBy("sequenceNum").cache().queryList())) {
            GenericValue type = EntityQuery.use(delegator).from("ProductFeatureType").where("productFeatureTypeId", a.getString("productFeatureTypeId")).cache().queryOne();
            String name = type != null && UtilValidate.isNotEmpty(type.getString("description")) ? type.getString("description") : a.getString("productFeatureTypeId");
            String value = UtilValidate.isNotEmpty(a.getString("description")) ? a.getString("description") : a.getString("productFeatureId");
            out.add(UtilMisc.toMap("name", name, "value", value));
        }
        return out;
    }

    private static boolean available(AcpContext ctx, String productId) {
        try {
            Map<String, Object> res = ctx.getDispatcher().runSync("isStoreInventoryAvailable", UtilMisc.toMap("productStoreId", ctx.getProductStoreId(),
                    "productId", productId, "quantity", BigDecimal.ONE, "userLogin", ctx.getSystemLogin()));
            return !ServiceUtil.isError(res) && !"N".equals(res.get("available"));
        } catch (GenericServiceException | GenericEntityException e) {
            return true;
        }
    }

    static boolean sellable(GenericValue p) {
        Timestamp now = UtilDateTime.nowTimestamp();
        if (p.getTimestamp("introductionDate") != null && p.getTimestamp("introductionDate").after(now)) return false;
        return p.getTimestamp("salesDiscontinuationDate") == null || !p.getTimestamp("salesDiscontinuationDate").before(now);
    }

    private static String title(GenericValue p) {
        String t = p.getString("productName");
        if (UtilValidate.isEmpty(t)) t = p.getString("internalName");
        return UtilValidate.isNotEmpty(t) ? t : p.getString("productId");
    }

    private static Map<String, Object> description(GenericValue p) {
        String d = p.getString("longDescription");
        if (UtilValidate.isEmpty(d)) d = p.getString("description");
        if (UtilValidate.isEmpty(d)) return null;
        String plain = d.replaceAll("<[^>]*>", " ").replaceAll("\\s+", " ").trim();
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("plain", plain.length() > 5000 ? plain.substring(0, 5000) : plain);
        return m;
    }

    private static List<Map<String, Object>> media(AcpContext ctx, GenericValue p) {
        List<Map<String, Object>> out = new ArrayList<>();
        for (String f : new String[] { "largeImageUrl", "detailImageUrl", "mediumImageUrl" }) {
            String u = p.getString(f);
            if (UtilValidate.isNotEmpty(u)) {
                out.add(UtilMisc.toMap("type", "image", "url", absolute(ctx, u)));
                break;
            }
        }
        return out;
    }

    private static String absolute(AcpContext ctx, String u) {
        if (u.startsWith("http://") || u.startsWith("https://")) return u;
        String base = ctx.getBaseUrl();
        int slash = base.indexOf('/', base.indexOf("//") + 2);
        return (slash > 0 ? base.substring(0, slash) : base) + (u.startsWith("/") ? u : "/" + u);
    }

    private static String countryOf(Delegator delegator, GenericValue store) {
        try {
            GenericValue addr = EntityQuery.use(delegator).from("PartyAndPostalAddress").where("partyId", store.getString("payToPartyId"))
                    .filterByDate().queryFirst();
            if (addr != null && addr.getString("countryGeoId") != null) {
                GenericValue geo = EntityQuery.use(delegator).from("Geo").where("geoId", addr.getString("countryGeoId")).cache().queryOne();
                if (geo != null && geo.getString("geoCode") != null) return geo.getString("geoCode");
            }
        } catch (GenericEntityException | RuntimeException e) {
            Debug.logVerbose("ACP feed: no store country: " + e.getMessage(), module);
        }
        return null;
    }
}
