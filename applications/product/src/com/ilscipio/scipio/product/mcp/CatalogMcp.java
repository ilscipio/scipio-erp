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
package com.ilscipio.scipio.product.mcp;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityFunction;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;

import com.ilscipio.scipio.mcp.catalog.ResultConverter;
import com.ilscipio.scipio.mcp.def.McpParam;
import com.ilscipio.scipio.mcp.def.McpResource;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.def.McpTopic;
import com.ilscipio.scipio.mcp.protocol.JsonRpc;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;
import com.ilscipio.scipio.mcp.tool.ImportDiff;

/**
 * SCIPIO: 4.0.0: MCP server profile for the product catalog webapp: products, categories, prices and inventory checks.
 */
@McpServer(name = "catalog", title = "Scipio Catalog", component = "product", webapps = {"catalog"},
        description = "Product catalog: products, categories, prices, catalogs, store setup and promotions.",
        featuredServices = {"createProduct", "updateProduct", "createProductPrice", "addProductToCategory",
                "calculateProductPrice", "getInventoryAvailableByFacility", "createProductCategory", "createProductAssoc"},
        entities = {"Product", "ProductCategory", "ProductCategoryMember", "ProductPrice", "ProductAssoc",
                "ProductFeature", "ProductStore", "InventoryItem"},
        serviceTools = {
            @McpServiceTool(service = "createProduct", topic = "product", name = "create",
                    description = "Create a product with type, name and description.", readOnly = false, destructive = "false", order = 40),
            @McpServiceTool(service = "updateProduct", topic = "product", name = "update",
                    description = "Update a product by id.", readOnly = false, destructive = "false", order = 41),
            @McpServiceTool(service = "markProductReviewed", topic = "product", name = "review_mark",
                    description = "The seller confirms (reviewed true) or withdraws (false) the AI values of a product; stored as attribute scipio.reviewed.",
                    readOnly = false, destructive = "false", order = 45),
            @McpServiceTool(service = "createProductCategory", topic = "product", name = "category_create",
                    description = "Create a product category.", readOnly = false, destructive = "false", order = 42),
            @McpServiceTool(service = "safeAddProductToCategory", topic = "product", name = "category_assign",
                    description = "Add a product to a category.",
                    readOnly = false, destructive = "false", order = 43),
            @McpServiceTool(service = "addProductCategoryToCategory", topic = "product", name = "category_child_add",
                    description = "Add a category as a child of a parent category.",
                    readOnly = false, destructive = "false", order = 44),
            @McpServiceTool(service = "createProductAssoc", topic = "product", name = "assoc_set",
                    description = "Associate two products, e.g. cross-sell, upgrade, variant.",
                    readOnly = false, destructive = "false", order = 45),
            @McpServiceTool(service = "createProductFeature", topic = "product", name = "feature_create",
                    description = "Create a product feature.",
                    readOnly = false, destructive = "false", order = 46),
            @McpServiceTool(service = "applyFeatureToProduct", topic = "product", name = "feature_apply",
                    description = "Apply a product feature to a product.",
                    readOnly = false, destructive = "false", order = 47),
            @McpServiceTool(service = "quickAddVariant", topic = "product", name = "variant_add",
                    description = "Create a product variant from selected feature ids.",
                    readOnly = false, destructive = "false", order = 48),
            @McpServiceTool(service = "createGoodIdentification", topic = "product", name = "identification_set",
                    description = "Record a product identifier, e.g. SKU, UPC.",
                    readOnly = false, destructive = "false", order = 49),
            @McpServiceTool(service = "createSimpleTextContentForProduct", topic = "product", name = "content_set",
                    description = "Attach simple text content to a product.",
                    readOnly = false, destructive = "false", order = 50),
            @McpServiceTool(service = "createProdCatalog", topic = "product", name = "catalog_create",
                    description = "Create a product catalog.",
                    readOnly = false, destructive = "false", order = 51),
            @McpServiceTool(service = "addProductCategoryToProdCatalog", topic = "product", name = "catalog_category_add",
                    description = "Add a category to a catalog.",
                    readOnly = false, destructive = "false", order = 52),
            @McpServiceTool(service = "createProductStoreCatalog", topic = "store", name = "catalog_add",
                    description = "Link a catalog to a store.",
                    readOnly = false, destructive = "false", order = 53),
            @McpServiceTool(service = "createProductStoreShipMeth", topic = "store", name = "shipment_method_add",
                    description = "Add a shipment method to a store.",
                    readOnly = false, destructive = "false", order = 54),
            @McpServiceTool(service = "createProductStorePaymentSetting", topic = "store", name = "payment_setting_add",
                    description = "Add a payment method setting to a store.",
                    readOnly = false, destructive = "false", order = 55),
            @McpServiceTool(service = "createShipmentEstimate", topic = "store", name = "shipment_estimate_create",
                    description = "Create a shipping cost estimate rule for a store method.",
                    readOnly = false, destructive = "false", order = 56),
            @McpServiceTool(service = "createProductPromo", topic = "store", name = "promo_create",
                    description = "Create a store promotion.",
                    readOnly = false, destructive = "false", order = 57),
            @McpServiceTool(service = "createProductStorePromoAppl", topic = "store", name = "promo_apply",
                    description = "Apply a promotion to a store.",
                    readOnly = false, destructive = "false", order = 58),
            @McpServiceTool(service = "createProductPromoCode", topic = "store", name = "promo_code_create",
                    description = "Create a promo code for a promotion.",
                    readOnly = false, destructive = "false", order = 59),
            @McpServiceTool(service = "createProductFacility", topic = "product", name = "facility_set",
                    description = "Set facility inventory settings for a product.",
                    readOnly = false, destructive = "false", order = 60)
        },
        topics = {
            @McpTopic(name = "product", title = "Products", order = 10, featured = true,
                    description = "Products, categories and catalogs: find, create, update, price, assign."),
            @McpTopic(name = "store", title = "Store setup", order = 20,
                    description = "Store catalogs, shipping, payment settings and promotions.")
        })
public final class CatalogMcp {

    private CatalogMcp() {}

    @McpTool(topic = "product", name = "find", description = "Find products by id, type, name or category.", readOnly = true)
    public static Object findProducts(McpCallContext ctx,
            @McpParam(name = "productId", description = "Exact product id", required = false) String productId,
            @McpParam(name = "productTypeId", description = "e.g. FINISHED_GOOD", required = false) String productTypeId,
            @McpParam(name = "productName", description = "Product or internal name (partial match)", required = false) String productName,
            @McpParam(name = "categoryId", description = "Only products in this category", required = false) String categoryId,
            @McpParam(name = "limit", description = "Max rows to return", required = false) Integer limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        int max = ctx.limit(limit);
        try {
            Set<String> categoryProductIds = null;
            if (categoryId != null) {
                categoryProductIds = new LinkedHashSet<>();
                for (GenericValue member : EntityQuery.use(delegator).from("ProductCategoryMember")
                        .where("productCategoryId", categoryId).queryList()) {
                    categoryProductIds.add(member.getString("productId"));
                }
            }
            List<EntityCondition> conditions = new ArrayList<>();
            if (productId != null) conditions.add(EntityCondition.makeCondition("productId", productId));
            if (productTypeId != null) conditions.add(EntityCondition.makeCondition("productTypeId", productTypeId));
            if (productName != null) {
                // case-insensitive: "candle" finds "Scented Jar Candle" (PostgreSQL LIKE is case-sensitive)
                String pattern = "%" + productName.toUpperCase(java.util.Locale.ROOT) + "%";
                conditions.add(EntityCondition.makeCondition(EntityOperator.OR,
                        EntityCondition.makeCondition(EntityFunction.UPPER_FIELD("productName"), EntityOperator.LIKE, pattern),
                        EntityCondition.makeCondition(EntityFunction.UPPER_FIELD("internalName"), EntityOperator.LIKE, pattern)));
            }
            List<Map<String, Object>> result = new ArrayList<>();
            for (GenericValue product : EntityQuery.use(delegator).from("Product").where(conditions).maxRows(max).queryList()) {
                if (categoryProductIds != null && !categoryProductIds.contains(product.getString("productId"))) continue;
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("productId", product.getString("productId"));
                row.put("productTypeId", product.getString("productTypeId"));
                row.put("productName", product.getString("productName"));
                row.put("internalName", product.getString("internalName"));
                // short fields for lists (the app catalog, the assistant): the subtitle, the list image, whether a long text exists
                row.put("description", product.getString("description"));
                row.put("smallImageUrl", product.getString("smallImageUrl"));
                row.put("hasLongDescription", product.get("longDescription") != null);
                result.add(row);
                if (result.size() >= max) break;
            }
            return result;
        } catch (GenericEntityException e) {
            throw new McpToolException("Product search failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "product", name = "get", description = "Get product detail with price and inventory availability.", readOnly = true)
    public static Object getProduct(McpCallContext ctx,
            @McpParam(name = "productId", description = "Product id", required = true) String productId,
            @McpParam(name = "productStoreId", description = "Store id for pricing; defaults to the session store", required = false) String productStoreId,
            @McpParam(name = "facilityId", description = "Facility id to check available-to-promise", required = false) String facilityId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue product = EntityQuery.use(delegator).from("Product").where("productId", productId).queryOne();
            if (product == null) {
                throw new McpToolException("Product not found: " + productId);
            }
            Map<String, Object> result = new LinkedHashMap<>();
            result.put("product", ResultConverter.toJson(product));
            String storeId = productStoreId != null ? productStoreId : ctx.getProductStoreId();
            String currencyUomId = ctx.getCurrencyUomId();
            if (storeId == null) {
                // A token call has no web store: a database with one product store (a hosted store) prices with that store
                List<GenericValue> stores = EntityQuery.use(delegator).from("ProductStore")
                        .select("productStoreId", "defaultCurrencyUomId").maxRows(2).queryList();
                if (stores.size() == 1) {
                    storeId = stores.get(0).getString("productStoreId");
                    if (stores.get(0).getString("defaultCurrencyUomId") != null) {
                        currencyUomId = stores.get(0).getString("defaultCurrencyUomId");
                    }
                }
            }
            if (storeId != null) {
                Map<String, Object> priceParams = new LinkedHashMap<>();
                priceParams.put("product", product);
                priceParams.put("productStoreId", storeId);
                priceParams.put("currencyUomId", currencyUomId);
                result.put("price", ResultConverter.toJsonMap(ctx.runService("calculateProductPrice", priceParams)));
            }
            // The active price rows also without a store (the price of each currency, type and purpose)
            result.put("prices", ResultConverter.toJson(EntityQuery.use(delegator).from("ProductPrice")
                    .where("productId", productId).filterByDate().queryList()));
            if (facilityId != null) {
                Map<String, Object> invParams = new LinkedHashMap<>();
                invParams.put("productId", productId);
                invParams.put("facilityId", facilityId);
                result.put("inventory", ResultConverter.toJsonMap(ctx.runService("getInventoryAvailableByFacility", invParams)));
            }
            result.put("categories", ResultConverter.toJson(
                    EntityQuery.use(delegator).from("ProductCategoryMember").where("productId", productId).queryList()));
            // SCIPIO: 4.0.0: the more photos of the product (ADDITIONAL_IMAGE_1 to 4, see image_add) as URLs
            List<Map<String, Object>> images = new ArrayList<>();
            for (GenericValue pc : EntityQuery.use(delegator).from("ProductContent").where(EntityCondition.makeCondition("productId", productId),
                    EntityCondition.makeCondition("productContentTypeId", EntityOperator.LIKE, ADDITIONAL_IMAGE + "%"))
                    .filterByDate().orderBy("productContentTypeId").queryList()) {
                String url = imageUrlOf(delegator, pc.getString("contentId"));
                if (url != null) {
                    Map<String, Object> image = new LinkedHashMap<>();
                    image.put("slot", pc.getString("productContentTypeId"));
                    image.put("contentId", pc.getString("contentId"));
                    image.put("url", url);
                    images.add(image);
                }
            }
            result.put("images", images);
            return result;
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to load product " + productId + ": " + e.getMessage());
        }
    }

    @McpTool(topic = "product", name = "category_list", description = "List child categories under a parent, or top-level categories.",
            readOnly = true)
    public static Object listCategories(McpCallContext ctx,
            @McpParam(name = "parentCategoryId", description = "Parent category id; omit for top-level categories", required = false) String parentCategoryId,
            @McpParam(name = "limit", description = "Max rows to return", required = false) Integer limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            List<EntityCondition> conditions = new ArrayList<>();
            if (parentCategoryId != null) conditions.add(EntityCondition.makeCondition("parentProductCategoryId", parentCategoryId));
            List<GenericValue> rollups = EntityQuery.use(delegator).from("ProductCategoryRollup")
                    .where(conditions).maxRows(ctx.limit(limit)).queryList();
            return ResultConverter.toJson(rollups);
        } catch (GenericEntityException e) {
            throw new McpToolException("Category listing failed: " + e.getMessage());
        }
    }

    /** The prefix of the product content types of the more photos (ADDITIONAL_IMAGE_1 to ADDITIONAL_IMAGE_4). */
    static final String ADDITIONAL_IMAGE = "ADDITIONAL_IMAGE_";
    static final int ADDITIONAL_IMAGES = 4;

    /** The URL of an image content (a SHORT_TEXT data resource that holds the URL, as addAdditionalViewForProduct makes it). */
    private static String imageUrlOf(Delegator delegator, String contentId) throws GenericEntityException {
        GenericValue content = EntityQuery.use(delegator).from("Content").where("contentId", contentId).queryOne();
        if (content == null || content.getString("dataResourceId") == null) {
            return null;
        }
        GenericValue dr = EntityQuery.use(delegator).from("DataResource").where("dataResourceId", content.getString("dataResourceId")).queryOne();
        return dr == null ? null : dr.getString("objectInfo");
    }

    @McpTool(topic = "product", name = "category_unassign", description = "Take a product out of a category: the active category rows end now. "
            + "A product out of the browse category of the web store is off the store.", readOnly = false, destructive = "false", order = 43)
    public static Object unassignCategory(McpCallContext ctx,
            @McpParam(name = "productId", description = "Product id", required = true) String productId,
            @McpParam(name = "productCategoryId", description = "Category id", required = true) String productCategoryId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            int ended = 0;
            java.sql.Timestamp now = org.ofbiz.base.util.UtilDateTime.nowTimestamp();
            for (GenericValue m : EntityQuery.use(delegator).from("ProductCategoryMember")
                    .where("productId", productId, "productCategoryId", productCategoryId).filterByDate().queryList()) {
                Map<String, Object> params = new LinkedHashMap<>();
                params.put("productId", productId);
                params.put("productCategoryId", productCategoryId);
                params.put("fromDate", m.getTimestamp("fromDate"));
                params.put("thruDate", now);
                ctx.runService("updateProductToCategory", params);
                ended++;
            }
            Map<String, Object> result = new LinkedHashMap<>();
            result.put("productId", productId);
            result.put("productCategoryId", productCategoryId);
            result.put("ended", ended);
            return result;
        } catch (GenericEntityException e) {
            throw new McpToolException("Category change failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "product", name = "image_add", description = "Add a photo (an image URL) to a product: the main image when the product has none, "
            + "else the next free more photo (ADDITIONAL_IMAGE_1 to 4).", readOnly = false, destructive = "false", order = 44)
    public static Object addImage(McpCallContext ctx,
            @McpParam(name = "productId", description = "Product id", required = true) String productId,
            @McpParam(name = "url", description = "Image URL (absolute, or a path of the store)", required = true) String url) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        if (url == null || url.trim().isEmpty() || url.length() > 2000) {
            throw new McpToolException("url is required (at most 2000 characters)");
        }
        url = url.trim();
        try {
            GenericValue product = EntityQuery.use(delegator).from("Product").where("productId", productId).queryOne();
            if (product == null) {
                throw new McpToolException("Product not found: " + productId);
            }
            Map<String, Object> result = new LinkedHashMap<>();
            result.put("productId", productId);
            result.put("url", url);
            if (product.getString("originalImageUrl") == null && product.getString("mediumImageUrl") == null) {
                Map<String, Object> params = new LinkedHashMap<>();
                params.put("productId", productId);
                for (String f : new String[] {"originalImageUrl", "largeImageUrl", "detailImageUrl", "mediumImageUrl", "smallImageUrl"}) {
                    params.put(f, url);
                }
                ctx.runService("updateProduct", params);
                result.put("slot", "MAIN");
                return result;
            }
            Set<String> used = new LinkedHashSet<>();
            for (GenericValue pc : EntityQuery.use(delegator).from("ProductContent").where(EntityCondition.makeCondition("productId", productId),
                    EntityCondition.makeCondition("productContentTypeId", EntityOperator.LIKE, ADDITIONAL_IMAGE + "%")).filterByDate().queryList()) {
                used.add(pc.getString("productContentTypeId"));
            }
            String slot = null;
            for (int i = 1; i <= ADDITIONAL_IMAGES && slot == null; i++) {
                if (!used.contains(ADDITIONAL_IMAGE + i)) {
                    slot = ADDITIONAL_IMAGE + i;
                }
            }
            if (slot == null) {
                throw new McpToolException("The product has " + (ADDITIONAL_IMAGES + 1) + " photos, the most it can have");
            }
            Map<String, Object> dr = new LinkedHashMap<>();
            dr.put("dataResourceTypeId", "SHORT_TEXT");
            dr.put("objectInfo", url);
            dr.put("dataResourceName", slot);
            String dataResourceId = (String) ctx.runService("createDataResource", dr).get("dataResourceId");
            Map<String, Object> content = new LinkedHashMap<>();
            content.put("dataResourceId", dataResourceId);
            content.put("contentTypeId", "DOCUMENT");
            content.put("contentName", productId + " " + slot);
            String contentId = (String) ctx.runService("createContent", content).get("contentId");
            Map<String, Object> pc = new LinkedHashMap<>();
            pc.put("productId", productId);
            pc.put("contentId", contentId);
            pc.put("productContentTypeId", slot);
            pc.put("fromDate", org.ofbiz.base.util.UtilDateTime.nowTimestamp());
            ctx.runService("createProductContent", pc);
            result.put("slot", slot);
            result.put("contentId", contentId);
            return result;
        } catch (GenericEntityException e) {
            throw new McpToolException("Image add failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "product", name = "price_set", description = "Set or create a product price for a currency and purpose.",
            readOnly = false, destructive = "true", requiresConfirmation = true, order = 75)
    public static Object setProductPrice(McpCallContext ctx,
            @McpParam(name = "productId", description = "Product id", required = true) String productId,
            @McpParam(name = "price", description = "Price amount", required = true) BigDecimal price,
            @McpParam(name = "currencyUomId", description = "Currency; default the session currency or USD", required = false) String currencyUomId,
            @McpParam(name = "productPriceTypeId", description = "Price type; default DEFAULT_PRICE", required = false) String productPriceTypeId,
            @McpParam(name = "productPricePurposeId", description = "Price purpose; default PURCHASE", required = false) String productPricePurposeId,
            @McpParam(name = "productStoreGroupId", description = "Store group; default _NA_", required = false) String productStoreGroupId) throws McpToolException {
        return upsertProductPrice(ctx, productId, price, currencyUomId, productPriceTypeId, productPricePurposeId, productStoreGroupId);
    }

    /** Upsert of one ProductPrice row: finds the active row by the five keys and updates it, else creates it. */
    private static Map<String, Object> upsertProductPrice(McpCallContext ctx, String productId, BigDecimal price,
            String currencyUomId, String productPriceTypeId, String productPricePurposeId, String productStoreGroupId) throws McpToolException {
        if (price == null) {
            throw new McpToolException("price is required");
        }
        String currency = currencyUomId != null ? currencyUomId : (ctx.getCurrencyUomId() != null ? ctx.getCurrencyUomId() : "USD");
        String priceTypeId = productPriceTypeId != null ? productPriceTypeId : "DEFAULT_PRICE";
        String pricePurposeId = productPricePurposeId != null ? productPricePurposeId : "PURCHASE";
        String storeGroupId = productStoreGroupId != null ? productStoreGroupId : "_NA_";
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue existing = EntityQuery.use(delegator).from("ProductPrice")
                    .where("productId", productId, "productPriceTypeId", priceTypeId, "productPricePurposeId", pricePurposeId,
                            "currencyUomId", currency, "productStoreGroupId", storeGroupId)
                    .filterByDate().queryFirst();
            Map<String, Object> params = new LinkedHashMap<>();
            params.put("productId", productId);
            params.put("productPriceTypeId", priceTypeId);
            params.put("productPricePurposeId", pricePurposeId);
            params.put("currencyUomId", currency);
            params.put("productStoreGroupId", storeGroupId);
            params.put("price", price);
            Map<String, Object> result = new LinkedHashMap<>();
            if (existing != null) {
                params.put("fromDate", existing.getTimestamp("fromDate"));
                ctx.runService("updateProductPrice", params);
                result.put("action", "updated");
            } else {
                ctx.runService("createProductPrice", params);
                result.put("action", "created");
            }
            result.put("productId", productId);
            result.put("price", price.toPlainString());
            result.put("currencyUomId", currency);
            result.put("productPriceTypeId", priceTypeId);
            result.put("productPricePurposeId", pricePurposeId);
            result.put("productStoreGroupId", storeGroupId);
            return result;
        } catch (GenericEntityException e) {
            throw new McpToolException("Price update failed for " + productId + ": " + e.getMessage());
        }
    }

    @McpTool(topic = "product", name = "import", description = "Bulk create or update products from rows; dry run unless apply is true.",
            readOnly = false, destructive = "false", order = 65)
    public static Object importProducts(McpCallContext ctx,
            @McpParam(name = "rows", description = "Array of product row objects", required = true, type = "array") List<Object> rowsArg,
            @McpParam(name = "apply", description = "Write the changes; default false (dry run)", required = false) Boolean apply,
            @McpParam(name = "force", description = "Write rows without doubts even when other rows have doubts", required = false) Boolean force) throws McpToolException {
        ImportDiff diff = new ImportDiff(apply, force);
        List<Map<String, Object>> rows = ImportDiff.rows(rowsArg);
        Delegator delegator = ctx.getDelegator();
        List<Map<String, Object>> rowPlans = new ArrayList<>();
        for (int i = 0; i < rows.size(); i++) {
            Map<String, Object> row = rows.get(i);
            boolean rowDoubt = false;
            String sku = ImportDiff.str(row, "sku", "productId", "partNumber");
            String name = ImportDiff.str(row, "name", "productName");
            if (sku == null && name == null) {
                diff.doubt(i, "row has no sku and no name");
                continue;
            }
            if (sku == null) {
                sku = ImportDiff.slug(name, 20);
                diff.doubt(i, "id derived from name: " + sku);
                rowDoubt = true;
            }
            String typeId = ImportDiff.str(row, "type", "productTypeId");
            if (typeId == null) typeId = "FINISHED_GOOD";
            BigDecimal price = ImportDiff.decimal(row, "price");
            String currency = ImportDiff.str(row, "currency", "currencyUomId");
            String categoryId = ImportDiff.str(row, "category", "productCategoryId");
            String description = ImportDiff.str(row, "description");
            String upc = ImportDiff.str(row, "upc");

            if (categoryId != null) {
                try {
                    if (EntityQuery.use(delegator).from("ProductCategory").where("productCategoryId", categoryId).queryOne() == null) {
                        diff.doubt(i, "category not found: " + categoryId);
                        rowDoubt = true;
                    }
                } catch (GenericEntityException e) {
                    throw new McpToolException("Category lookup failed: " + e.getMessage());
                }
            }

            GenericValue existing;
            try {
                existing = EntityQuery.use(delegator).from("Product").where("productId", sku).queryOne();
            } catch (GenericEntityException e) {
                throw new McpToolException("Product lookup failed: " + e.getMessage());
            }

            Map<String, Object> data = new LinkedHashMap<>();
            data.put("productId", sku);
            data.put("productTypeId", typeId);
            if (name != null) data.put("productName", name);
            if (description != null) data.put("description", description);
            if (price != null) data.put("price", price.toPlainString());
            if (currency != null) data.put("currencyUomId", currency);
            if (categoryId != null) data.put("productCategoryId", categoryId);
            if (upc != null) data.put("upc", upc);

            boolean create = (existing == null);
            boolean changed = create;
            if (!create) {
                Map<String, Object> before = new LinkedHashMap<>();
                before.put("productName", existing.getString("productName"));
                before.put("description", existing.getString("description"));
                changed = (name != null && !Objects.equals(existing.getString("productName"), name))
                        || (description != null && !Objects.equals(existing.getString("description"), description));
                if (changed) {
                    diff.update("product", sku, before, data);
                } else {
                    diff.unchanged("product", sku);
                }
            } else {
                diff.create("product", sku, data);
            }

            Map<String, Object> plan = new LinkedHashMap<>();
            plan.put("sku", sku);
            plan.put("create", create);
            plan.put("changed", changed);
            plan.put("doubt", rowDoubt);
            plan.put("productName", name);
            plan.put("description", description);
            plan.put("price", price);
            plan.put("currencyUomId", currency);
            plan.put("categoryId", categoryId);
            plan.put("upc", upc);
            rowPlans.add(plan);
        }

        if (diff.canWrite()) {
            for (Map<String, Object> plan : rowPlans) {
                if (Boolean.TRUE.equals(plan.get("doubt"))) continue;
                String sku = (String) plan.get("sku");
                boolean create = Boolean.TRUE.equals(plan.get("create"));
                boolean changed = Boolean.TRUE.equals(plan.get("changed"));
                String productName = (String) plan.get("productName");
                String description = (String) plan.get("description");
                if (create) {
                    Map<String, Object> params = new LinkedHashMap<>();
                    params.put("productId", sku);
                    params.put("productTypeId", "FINISHED_GOOD");
                    params.put("internalName", productName != null ? productName : sku);
                    if (productName != null) params.put("productName", productName);
                    if (description != null) params.put("description", description);
                    ctx.runService("createProduct", params);
                } else if (changed) {
                    Map<String, Object> params = new LinkedHashMap<>();
                    params.put("productId", sku);
                    if (productName != null) params.put("productName", productName);
                    if (description != null) params.put("description", description);
                    ctx.runService("updateProduct", params);
                }
                BigDecimal price = (BigDecimal) plan.get("price");
                if (price != null) {
                    upsertProductPrice(ctx, sku, price, (String) plan.get("currencyUomId"), null, null, null);
                }
                String categoryId = (String) plan.get("categoryId");
                if (categoryId != null) {
                    Map<String, Object> catParams = new LinkedHashMap<>();
                    catParams.put("productId", sku);
                    catParams.put("productCategoryId", categoryId);
                    // SCIPIO: 4.0.0: fromDate belongs to the ProductCategoryMember key and the service requires it
                    catParams.put("fromDate", org.ofbiz.base.util.UtilDateTime.nowTimestamp());
                    ctx.runService("safeAddProductToCategory", catParams);
                }
                String upc = (String) plan.get("upc");
                if (upc != null) {
                    Map<String, Object> upcParams = new LinkedHashMap<>();
                    upcParams.put("productId", sku);
                    upcParams.put("goodIdentificationTypeId", "UPCA");
                    upcParams.put("idValue", upc);
                    ctx.runService("createGoodIdentification", upcParams);
                }
                Map<String, Object> ids = new LinkedHashMap<>();
                ids.put("productId", sku);
                diff.written("product", sku, ids);
            }
        }
        return diff.result();
    }

    @McpResource(uri = "scipio://product/{productId}", name = "Product",
            description = "One product as JSON: product row, prices, categories and features.",
            mimeType = "application/json")
    public static String productResource(McpCallContext ctx, Map<String, String> uriParams) throws McpToolException {
        String productId = uriParams.get("productId");
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue product = EntityQuery.use(delegator).from("Product").where("productId", productId).queryOne();
            if (product == null) {
                throw new McpToolException("Product not found: " + productId);
            }
            Map<String, Object> result = new LinkedHashMap<>();
            result.put("product", ResultConverter.toJson(product));
            result.put("prices", ResultConverter.toJson(
                    EntityQuery.use(delegator).from("ProductPrice").where("productId", productId).filterByDate().queryList()));
            result.put("categories", ResultConverter.toJson(
                    EntityQuery.use(delegator).from("ProductCategoryMember").where("productId", productId).queryList()));
            result.put("features", ResultConverter.toJson(
                    EntityQuery.use(delegator).from("ProductFeatureAndAppl").where("productId", productId).queryList()));
            return JsonRpc.writePretty(result);
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to load product " + productId + ": " + e.getMessage());
        }
    }
}
