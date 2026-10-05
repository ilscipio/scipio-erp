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
package com.ilscipio.scipio.solr.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Services {

    @Service(
        name = "solrGenericPermission",
        engine = "simple",
        location = "component://solr/script/com/ilscipio/scipio/solr/SolrServices.xml",
        invoke = "solrGenericPermission",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface SolrGenericPermission {}

    /**
     * Checks if Solr webapp is loaded and available for queries
     */
    @Service(
        name = "checkSolrReady",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "checkSolrReady",
        description = "Checks if Solr webapp is loaded and available for queries",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "core", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "client", type = "org.apache.solr.client.solrj.impl.HttpSolrClient", mode = "IN", optional = "true"),
            @Attribute(name = "ready", type = "Boolean", mode = "OUT", optional = "true", description = "True if Solr is enabled, loaded and available"),
            @Attribute(name = "enabled", type = "Boolean", mode = "OUT", optional = "true", description = "True if Solr is enabled, or it is registered as an application in the system\n                (always true in stock Scipio config)")
        }
    )
    public interface CheckSolrReady {}

    /**
     * Returns only when Solr webapp is loaded and available for queries             (returns failure if Solr is not registered in the system or other error)
     */
    @Service(
        name = "waitSolrReady",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "waitSolrReady",
        description = "Returns only when Solr webapp is loaded and available for queries\n            (returns failure if Solr is not registered in the system or other error)",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "core", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "client", type = "org.apache.solr.client.solrj.impl.HttpSolrClient", mode = "IN", optional = "true"),
            @Attribute(name = "sleepTime", type = "Integer", mode = "IN", optional = "true", description = "Time (milliseconds) to wait between checks.\n                Default: value of solrconfig.properties/solr.service.waitSolrReady.sleepTime"),
            @Attribute(name = "maxChecks", type = "Integer", mode = "IN", optional = "true", description = "Max number of times to check (-1 for infinite), separated by sleepTime.\n                If passes this, returns failure. Default: value of solrconfig.properties/solr.service.waitSolrReady.maxChecks")
        }
    )
    public interface WaitSolrReady {}

    /**
     * Set SOLR data status ID
     */
    @Service(
        name = "setSolrDataStatus",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "setSolrDataStatus",
        description = "Set SOLR data status ID",
        attributes = {
            @Attribute(name = "dataStatusId", type = "String", mode = "IN")
        }
    )
    public interface SetSolrDataStatus {}

    /**
     * Mark SOLR data status as dirty
     */
    @Service(
        name = "markSolrDataDirty",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "markSolrDataDirty",
        description = "Mark SOLR data status as dirty"
    )
    public interface MarkSolrDataDirty {}

    @Service(
        name = "setSolrSystemProperty",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "setSolrSystemProperty",
        auth = "true",
        attributes = {
            @Attribute(name = "property", type = "String", mode = "IN"),
            @Attribute(name = "value", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "removeIfEmpty", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false")
        },
        permissionService = @PermissionService(service = "solrGenericPermission", mainAction = "ADMIN")
    )
    public interface SetSolrSystemProperty {}

    @Service(
        name = "removeSolrSystemProperty",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "removeSolrSystemProperty",
        auth = "true",
        attributes = {
            @Attribute(name = "property", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "solrGenericPermission", mainAction = "ADMIN")
    )
    public interface RemoveSolrSystemProperty {}

    /**
     * Reloads the security authorizations defined in security.json
     */
    @Service(
        name = "reloadSolrSecurityAuthorizations",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "reloadSolrSecurityAuthorizations",
        description = "Reloads the security authorizations defined in security.json",
        useTransaction = "false",
        permissionService = @PermissionService(service = "solrGenericPermission", mainAction = "ADMIN")
    )
    public interface ReloadSolrSecurityAuthorizations {}

    /**
     * Rebuild Solr index, all products
     */
    @Service(
        name = "rebuildSolrIndex",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "rebuildSolrIndex",
        description = "Rebuild Solr index, all products",
        transactionTimeout = "72000",
        semaphore = "fail",
        implemented = {@Implements(service = "scipioJobCtxInterface")},
        attributes = {
            @Attribute(name = "treatConnectErrorNonFatal", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "core", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "onlyIfDirty", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "ifConfigChange", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "bufSize", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "clearAndUseCache", type = "Boolean", mode = "IN", optional = "true", description = "see solrconfig.properties/solr.index.rebuild.clearAndUseCache"),
            @Attribute(name = "waitSolrReady", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, will wait for Solr to be loaded before running the indexing- see waitSolrReady service"),
            @Attribute(name = "deleteMode", type = "String", mode = "IN", optional = "true", defaultValue = "no-delete", description = "How old records get cleared from index or not. Supported values:\n                delete-all-first (clears whole index before reindexing, default),\n                no-delete (deleted products will remain in solr index, can use when you know no products have changed since last indexing to minimize impact of reindex)"),
            @Attribute(name = "includeMainStoreIds", type = "Collection", mode = "IN", optional = "true", description = "Only index products whose default/main store is one of these productStoreIds; warning: slow"),
            @Attribute(name = "includeAnyStoreIds", type = "Collection", mode = "IN", optional = "true", description = "Only index products linked to any of these productStoreIds; warning: slow"),
            @Attribute(name = "numDocs", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "executed", type = "Boolean", mode = "OUT", optional = "true")
        }
    )
    public interface RebuildSolrIndex {}

    /**
     * Rebuild Solr index, all products, without clearing index first
     */
    @Service(
        name = "rebuildSolrIndexNoDelete",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "rebuildSolrIndexNoDelete",
        description = "Rebuild Solr index, all products, without clearing index first",
        transactionTimeout = "72000",
        implemented = {@Implements(service = "rebuildSolrIndex")},
        attributes = {
            @Attribute(name = "deleteMode", type = "String", mode = "IN", optional = "true", defaultValue = "no-delete")
        }
    )
    public interface RebuildSolrIndexNoDelete {}

    /**
     * Rebuild Solr index, all products, if data status dirty or unknown or if config change
     */
    @Service(
        name = "rebuildSolrIndexIfDirty",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "rebuildSolrIndexIfDirty",
        description = "Rebuild Solr index, all products, if data status dirty or unknown or if config change",
        transactionTimeout = "72000",
        implemented = {@Implements(service = "rebuildSolrIndex")},
        attributes = {
            @Attribute(name = "onlyIfDirty", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "ifConfigChange", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "waitSolrReady", type = "Boolean", mode = "IN", optional = "true")
        }
    )
    public interface RebuildSolrIndexIfDirty {}

    /**
     * rebuild SOLR Index - auto-run service
     */
    @Service(
        name = "rebuildSolrIndexAuto",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "rebuildSolrIndexAuto",
        description = "rebuild SOLR Index - auto-run service",
        transactionTimeout = "72000",
        implemented = {@Implements(service = "rebuildSolrIndex")},
        attributes = {
            @Attribute(name = "onlyIfDirty", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "ifConfigChange", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "waitSolrReady", type = "Boolean", mode = "IN", optional = "true")
        }
    )
    public interface RebuildSolrIndexAuto {}

    /**
     * Aborts rebuildSolrIndex if possible
     */
    @Service(
        name = "abortRebuildSolrIndex",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "abortRebuildSolrIndex",
        description = "Aborts rebuildSolrIndex if possible",
        useTransaction = "false"
    )
    public interface AbortRebuildSolrIndex {}

    /**
     * Solr commit service, chained to entity indexing to receive documents
     */
    @Service(
        name = "commitToSolr",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "commitToSolr",
        description = "Solr commit service, chained to entity indexing to receive documents",
        useTransaction = "false",
        transactionTimeout = "7200",
        log = "quiet",
        implemented = {@Implements(service = "entityIndexingConsumer")}
    )
    public interface CommitToSolr {}

    /**
     * Immediately adds OR removes product to/from solr index by product instance or by productId - intended for use with ECAs/SECAs,             automatically called by registerUpdateToSolr
     */
    @Service(
        name = "updateToSolr",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "updateToSolr",
        description = "Immediately adds OR removes product to/from solr index by product instance or by productId - intended for use with ECAs/SECAs,\n            automatically called by registerUpdateToSolr",
        transactionTimeout = "72000",
        priority = "90",
        implemented = {@Implements(service = "scheduleProductIndexing")},
        overrideAttributes = {
            @OverrideAttribute(name = "topics", type = "List", mode = "IN", optional = "true", defaultValue = "[solr]")
        }
    )
    public interface UpdateToSolr {}

    /**
     * Immediately adds product to solr index by product instance or by productId, manual helper - NOT intended for use with ECAs/SECAs anymore (use registerUpdateToSolr/updateToSolr)
     */
    @Service(
        name = "addToSolr",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "addToSolr",
        description = "Immediately adds product to solr index by product instance or by productId, manual helper - NOT intended for use with ECAs/SECAs anymore (use registerUpdateToSolr/updateToSolr)",
        transactionTimeout = "72000",
        priority = "90",
        implemented = {@Implements(service = "updateToSolr")},
        overrideAttributes = {
            @OverrideAttribute(name = "action", defaultValue = "add")
        }
    )
    public interface AddToSolr {}

    /**
     * Immediately removes product from solr index by product instance or by productId, manual helper - NOT intended for use with ECAs/SECAs anymore (use registerUpdateToSolr/updateToSolr)
     */
    @Service(
        name = "removeFromSolr",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "removeFromSolr",
        description = "Immediately removes product from solr index by product instance or by productId, manual helper - NOT intended for use with ECAs/SECAs anymore (use registerUpdateToSolr/updateToSolr)",
        transactionTimeout = "72000",
        priority = "90",
        implemented = {@Implements(service = "updateToSolr")},
        overrideAttributes = {
            @OverrideAttribute(name = "action", defaultValue = "remove")
        }
    )
    public interface RemoveFromSolr {}

    /**
     * Registers (queues) a product add or removal to/from solr index by product instance or by productId - intended for use with ECAs/SECAs             - delays the solr index update (updateToSolr) to the current transaction's global-commit
     */
    @Service(
        name = "registerUpdateToSolr",
        engine = "java",
        location = "com.ilscipio.scipio.solr.EntityIndexer",
        invoke = "scheduleEntityIndexing",
        description = "Registers (queues) a product add or removal to/from solr index by product instance or by productId - intended for use with ECAs/SECAs\n            - delays the solr index update (updateToSolr) to the current transaction's global-commit",
        useTransaction = "false",
        log = "quiet",
        logEca = "quiet",
        implemented = {@Implements(service = "scheduleProductIndexing")}
    )
    public interface RegisterUpdateToSolr {}

    /**
     * Simple-type product attributes for addToSolrIndex Product             NOTE: It is preferable to use the "fields" map of the addToSolrIndex interface in custom implementations,             as is lessens the amount of patching required; solrProductAttributes* may be deprecated.
     */
    @Service(
        name = "solrProductAttributesSimple",
        engine = "interface",
        description = "Simple-type product attributes for addToSolrIndex Product\n            NOTE: It is preferable to use the \"fields\" map of the addToSolrIndex interface in custom implementations,\n            as is lessens the amount of patching required; solrProductAttributes* may be deprecated.",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "internalName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "manu", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "smallImage", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "mediumImage", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "largeImage", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "listPrice", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "defaultPrice", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inStock", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "isVirtual", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "isVariant", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "isDigital", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "isPhysical", type = "Boolean", mode = "IN", optional = "true")
        }
    )
    public interface SolrProductAttributesSimple {}

    /**
     * Complex-type product attributes for addToSolrIndex Product             NOTE: It is preferable to use the "fields" map of the addToSolrIndex interface in custom implementations,                 as is lessens the amount of patching required; solrProductAttributes* may be deprecated.
     */
    @Service(
        name = "solrProductAttributesComplex",
        engine = "interface",
        description = "Complex-type product attributes for addToSolrIndex Product\n            NOTE: It is preferable to use the \"fields\" map of the addToSolrIndex interface in custom implementations,\n                as is lessens the amount of patching required; solrProductAttributes* may be deprecated.",
        attributes = {
            @Attribute(name = "description", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "longDescription", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "title", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "category", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "features", type = "Set", mode = "IN", optional = "true"),
            @Attribute(name = "attributes", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "catalog", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "keywords", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "productStore", type = "List", mode = "IN", optional = "true")
        }
    )
    public interface SolrProductAttributesComplex {}

    /**
     * All product attributes for addToSolrIndex Product             NOTE: It is preferable to use the "fields" map of the addToSolrIndex interface in custom implementations,             as is lessens the amount of patching required; solrProductAttributes* may be deprecated.
     */
    @Service(
        name = "solrProductAttributes",
        engine = "interface",
        description = "All product attributes for addToSolrIndex Product\n            NOTE: It is preferable to use the \"fields\" map of the addToSolrIndex interface in custom implementations,\n            as is lessens the amount of patching required; solrProductAttributes* may be deprecated.",
        implemented = {@Implements(service = "solrProductAttributesSimple"), @Implements(service = "solrProductAttributesComplex")}
    )
    public interface SolrProductAttributes {}

    /**
     * Add a Product to Solr Index (Note: mainly used internally)
     */
    @Service(
        name = "addToSolrIndex",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "addToSolrIndex",
        description = "Add a Product to Solr Index (Note: mainly used internally)",
        transactionTimeout = "72000",
        implemented = {@Implements(service = "solrProductAttributes")},
        attributes = {
            @Attribute(name = "fields", type = "Map", mode = "IN", optional = "true", description = "A map of pre-formatted fields to add as-is to the solr document for indexing, with no field renaming or Solr type abstraction (added 2018-02-05)"),
            @Attribute(name = "treatConnectErrorNonFatal", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "Use entity cache for extra lookups"),
            @Attribute(name = "core", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "errorType", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface AddToSolrIndex {}

    /**
     * Add a List of Products to Solr Index and flush after all have been added (Note: mainly used internally)
     */
    @Service(
        name = "addListToSolrIndex",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "addListToSolrIndex",
        description = "Add a List of Products to Solr Index and flush after all have been added (Note: mainly used internally)",
        transactionTimeout = "72000",
        attributes = {
            @Attribute(name = "docList", type = "List", mode = "IN"),
            @Attribute(name = "treatConnectErrorNonFatal", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "Use entity cache for extra lookups"),
            @Attribute(name = "core", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "errorType", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface AddListToSolrIndex {}

    /**
     * Run a query on Solr and return the results
     */
    @Service(
        name = "runSolrQuery",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "runSolrQuery",
        description = "Run a query on Solr and return the results",
        transactionTimeout = "72000",
        attributes = {
            @Attribute(name = "query", type = "String", mode = "IN", allowHtml = "any"),
            @Attribute(name = "start", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "viewSize", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "viewIndex", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "queryFilter", type = "String", mode = "IN", optional = "true", description = "WARN: for legacy code reasons, this string is split on whitespace to produce multiple filters.\n                    To avoid splitting, use queryFilters instead (with one entry)."),
            @Attribute(name = "queryFilters", type = "List", mode = "IN", optional = "true", description = "List of strings, each used as-is as a filter (no splitting)."),
            @Attribute(name = "sortByList", type = "List", mode = "IN", optional = "true", description = "List of strings, each optionally suffixed by \" asc\" or \" desc\""),
            @Attribute(name = "sortBy", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sortByReverse", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "returnFields", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "facetQuery", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "facetQueryList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "facetFieldList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "facetMinCount", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "facetLimit", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "facet", type = "Boolean", mode = "IN", optional = "true", description = "DEPRECATED: 2019-08: this was a legacy parameter and you may simply omit it; default: false (changed 2017-09)"),
            @Attribute(name = "highlight", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "spellcheck", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "spellDict", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "queryType", type = "String", mode = "IN", optional = "true", description = "Name of a request handler defined in solrconfig.xml (starts with /)"),
            @Attribute(name = "defType", type = "String", mode = "IN", optional = "true", description = "Query language def handler type (dismax, edismax, ...), overrides the request handler"),
            @Attribute(name = "defaultOp", type = "String", mode = "IN", optional = "true", description = "OR or AND (default depends on configuration, usually OR); NOTE: Not honored by all defTypes (edismax supports)"),
            @Attribute(name = "queryFields", type = "String", mode = "IN", optional = "true", description = "For edismax defType only: the target fields to query, usually space-separated (\"qf\" parameter)"),
            @Attribute(name = "queryParams", type = "Map", mode = "IN", optional = "true", description = "Optional manual extra query options; NOTE: using above options is preferred when possible for better future-proofing of queries"),
            @Attribute(name = "core", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "solrUsername", type = "String", mode = "IN", optional = "true", description = "Username for Solr basic authentication (default: solrconfig.properties/solr.query.login.username)"),
            @Attribute(name = "solrPassword", type = "String", mode = "IN", optional = "true", description = "Password for Solr basic authentication (default: solrconfig.properties/solr.query.login.password)"),
            @Attribute(name = "lowercaseOperators", type = "Boolean", mode = "IN", optional = "true", description = "For edismax: If true, \"and\" and \"or\" in queries are treated same as \"AND\" and \"OR\" (default: true in Scipio)"),
            @Attribute(name = "queryResult", type = "org.apache.solr.client.solrj.response.QueryResponse", mode = "OUT", optional = "true"),
            @Attribute(name = "errorType", type = "String", mode = "OUT", optional = "true", description = "\"query-syntax\" for query syntax error, \"general\" otherwise (if error occurred) (added 2017-08-25)"),
            @Attribute(name = "nestedErrorMessage", type = "String", mode = "OUT", optional = "true", description = "Specific error message for the error, IF available (added 2017-08-25)")
        }
    )
    public interface RunSolrQuery {}

    @Service(
        name = "solrDefaultQueryFilters",
        engine = "interface",
        attributes = {
            @Attribute(name = "core", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStore", type = "GenericValue", mode = "IN", optional = "true", description = "Product store (used to determine the product filtering defaults)"),
            @Attribute(name = "useDefaultFilters", type = "Boolean", mode = "IN", optional = "true", description = "If true, unless overridden with the explicit filter flags, the product stock and discontinuation filters \n                are applied based on the ProductStore configuration; if false, filters are not applied unless explicitly\n                requested with the explicit filter flags (default: true)"),
            @Attribute(name = "useStockFilter", type = "Boolean", mode = "IN", optional = "true", description = "If true, filter out products with no inventory \n                (default: ProductStore.showOutOfStockProducts, or false)"),
            @Attribute(name = "useDiscFilter", type = "Boolean", mode = "IN", optional = "true", description = "If true, filter out products past their Product.salesDiscontinuationDate date \n                (default: ProductStore.showDiscontinuedProductsDefault, or false)"),
            @Attribute(name = "excludeVariants", type = "Boolean", mode = "IN", optional = "true", description = "If true, exclude variant products\n                (default: ProductStore.prodSearchExcludeVariants, or true)"),
            @Attribute(name = "filterTimestamp", type = "java.sql.Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface SolrDefaultQueryFilters {}

    @Service(
        name = "solrSearchCommon",
        engine = "interface",
        implemented = {@Implements(service = "solrDefaultQueryFilters")},
        attributes = {
            @Attribute(name = "core", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "queryFilters", type = "List", mode = "IN", optional = "true", description = "List of strings, each used as-is as a filter (no splitting).")
        }
    )
    public interface SolrSearchCommon {}

    /**
     * Run a query on Solr and return the results
     */
    @Service(
        name = "solrProductsSearch",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "solrProductsSearch",
        description = "Run a query on Solr and return the results",
        transactionTimeout = "72000",
        implemented = {@Implements(service = "solrSearchCommon")},
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "includeSubCategories", type = "Boolean", mode = "IN", optional = "true", description = "default: true (legacy behavior)"),
            @Attribute(name = "viewSize", type = "Object", mode = "IN", optional = "true", description = "Supports String, Integer (2017-09-11)"),
            @Attribute(name = "viewIndex", type = "Object", mode = "IN", optional = "true", description = "Supports String, Integer (2017-09-11)"),
            @Attribute(name = "sortByList", type = "List", mode = "IN", optional = "true", description = "List of strings, each optionally suffixed by \" asc\" or \" desc\""),
            @Attribute(name = "sortBy", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sortByReverse", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "facetQuery", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "facetQueryList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "facetFieldList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "facetMinCount", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "facetLimit", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "queryParams", type = "Map", mode = "IN", optional = "true", description = "Optional manual extra query options; NOTE: using above options is preferred when possible for better future-proofing of queries"),
            @Attribute(name = "facet", type = "Boolean", mode = "IN", optional = "true", description = "DEPRECATED: 2019-08: this was a legacy parameter and you may simply omit it; default: false (changed 2017-09)"),
            @Attribute(name = "highlight", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "solrUsername", type = "String", mode = "IN", optional = "true", description = "Username for Solr basic authentication (default: solrconfig.properties/solr.query.login.username)"),
            @Attribute(name = "solrPassword", type = "String", mode = "IN", optional = "true", description = "Password for Solr basic authentication (default: solrconfig.properties/solr.query.login.password)"),
            @Attribute(name = "results", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "facetQueries", type = "Map", mode = "OUT", optional = "true"),
            @Attribute(name = "facetFields", type = "Map", mode = "OUT", optional = "true"),
            @Attribute(name = "start", type = "Long", mode = "OUT", optional = "true"),
            @Attribute(name = "viewSize", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "viewIndex", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "listSize", type = "Long", mode = "OUT", optional = "true"),
            @Attribute(name = "errorType", type = "String", mode = "OUT", optional = "true", description = "\"query-syntax\" for query syntax error, \"general\" otherwise (if error occurred) (added 2017-08-25)"),
            @Attribute(name = "nestedErrorMessage", type = "String", mode = "OUT", optional = "true", description = "Specific error message for the error, IF available (added 2017-08-25)")
        }
    )
    public interface SolrProductsSearch {}

    /**
     * Run a query on Solr and return the results
     */
    @Service(
        name = "solrKeywordSearch",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "solrKeywordSearch",
        description = "Run a query on Solr and return the results",
        transactionTimeout = "72000",
        implemented = {@Implements(service = "solrSearchCommon")},
        attributes = {
            @Attribute(name = "query", type = "String", mode = "IN", allowHtml = "any"),
            @Attribute(name = "viewSize", type = "Object", mode = "IN", optional = "true", description = "Supports String or Integer (2017-09-11)"),
            @Attribute(name = "viewIndex", type = "Object", mode = "IN", optional = "true", description = "Supports String or Integer (2017-09-11)"),
            @Attribute(name = "queryFilter", type = "String", mode = "IN", optional = "true", description = "WARN: for legacy code reasons, this string is split on whitespace to produce multiple filters.\n                    To avoid splitting, use queryFilters instead (with one entry)."),
            @Attribute(name = "sortByList", type = "List", mode = "IN", optional = "true", description = "List of strings, each optionally suffixed by \" asc\" or \" desc\""),
            @Attribute(name = "sortBy", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sortByReverse", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "returnFields", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "facetQuery", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "facetQueryList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "facetFieldList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "facetMinCount", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "facetLimit", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "queryType", type = "String", mode = "IN", optional = "true", description = "Name of a request handler defined in solrconfig.xml (starts with /)"),
            @Attribute(name = "defType", type = "String", mode = "IN", optional = "true", description = "Query language def handler type (dismax, edismax, ...), overrides the request handler"),
            @Attribute(name = "defaultOp", type = "String", mode = "IN", optional = "true", description = "OR or AND (default depends on configuration, usually OR); NOTE: Not honored by all defTypes (edismax supports)"),
            @Attribute(name = "queryFields", type = "String", mode = "IN", optional = "true", description = "For edismax defType only: the target fields to query, usually space-separated (\"qf\" parameter)"),
            @Attribute(name = "queryParams", type = "Map", mode = "IN", optional = "true", description = "Optional manual extra query options; NOTE: using above options is preferred when possible for better future-proofing of queries"),
            @Attribute(name = "facet", type = "Boolean", mode = "IN", optional = "true", description = "DEPRECATED: 2019-08: this was a legacy parameter and you may simply omit it; default: false (changed 2017-09)"),
            @Attribute(name = "spellcheck", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "spellDict", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "highlight", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "solrUsername", type = "String", mode = "IN", optional = "true", description = "Username for Solr basic authentication (default: solrconfig.properties/solr.query.login.username)"),
            @Attribute(name = "solrPassword", type = "String", mode = "IN", optional = "true", description = "Password for Solr basic authentication (default: solrconfig.properties/solr.query.login.password)"),
            @Attribute(name = "lowercaseOperators", type = "Boolean", mode = "IN", optional = "true", description = "For edismax: If true, \"and\" and \"or\" in queries are treated same as \"AND\" and \"OR\" (default: true in Scipio)"),
            @Attribute(name = "results", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "isCorrectlySpelled", type = "Boolean", mode = "OUT", optional = "true", description = "Correctly-spelled flag - set if spellcheck was enabled\n                WARN: May not behave as you might expect: https://issues.apache.org/jira/browse/SOLR-4278\n                Can return false even if tokenSuggestions and fullSuggestions are empty"),
            @Attribute(name = "facetQueries", type = "Map", mode = "OUT", optional = "true"),
            @Attribute(name = "facetFields", type = "Map", mode = "OUT", optional = "true"),
            @Attribute(name = "start", type = "Long", mode = "OUT", optional = "true"),
            @Attribute(name = "viewSize", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "viewIndex", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "listSize", type = "Long", mode = "OUT", optional = "true"),
            @Attribute(name = "queryTime", type = "Long", mode = "OUT", optional = "true"),
            @Attribute(name = "suggestions", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "tokenSuggestions", type = "Map", mode = "OUT", optional = "true", description = "Maps token to lists of suggestions - set if spellcheck was enabled (added 2017-09-14)"),
            @Attribute(name = "fullSuggestions", type = "List", mode = "OUT", optional = "true", description = "List of strings - spellcheck suggestions in collated format - set if spellcheck was enabled (added 2017-09-14)"),
            @Attribute(name = "errorType", type = "String", mode = "OUT", optional = "true", description = "\"query-syntax\" for query syntax error, \"general\" otherwise (if error occurred) (added 2017-08-25)"),
            @Attribute(name = "nestedErrorMessage", type = "String", mode = "OUT", optional = "true", description = "Specific error message for the error, IF available (added 2017-08-25)")
        }
    )
    public interface SolrKeywordSearch {}

    /**
     * Run a query on Solr and return the results
     */
    @Service(
        name = "solrAvailableCategories",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "solrAvailableCategories",
        description = "Run a query on Solr and return the results",
        transactionTimeout = "72000",
        implemented = {@Implements(service = "solrSearchCommon")},
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "catalogId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "currentTrail", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "displayProducts", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "viewSize", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "viewIndex", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "numFound", type = "Long", mode = "OUT"),
            @Attribute(name = "categories", type = "java.util.Map", mode = "OUT", optional = "true")
        }
    )
    public interface SolrAvailableCategories {}

    /**
     * Run a query on Solr and return the results in a more detailed way like solrSideDeepCategory
     */
    @Service(
        name = "solrAvailableCategoriesExtended",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "solrAvailableCategoriesExtended",
        description = "Run a query on Solr and return the results in a more detailed way like solrSideDeepCategory",
        transactionTimeout = "72000",
        implemented = {@Implements(service = "solrAvailableCategories")},
        attributes = {
            @Attribute(name = "depth", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "categories", type = "java.util.List", mode = "OUT", optional = "true"),
            @Attribute(name = "categoriesMap", type = "java.util.Map", mode = "OUT", optional = "true")
        }
    )
    public interface SolrAvailableCategoriesExtended {}

    /**
     * Run a query on Solr and return the results
     */
    @Service(
        name = "solrSideDeepCategory",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "solrSideDeepCategory",
        description = "Run a query on Solr and return the results",
        transactionTimeout = "72000",
        implemented = {@Implements(service = "solrSearchCommon")},
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN"),
            @Attribute(name = "catalogId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "currentTrail", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "numFound", type = "Long", mode = "OUT"),
            @Attribute(name = "categories", type = "java.util.Map", mode = "OUT", optional = "true")
        }
    )
    public interface SolrSideDeepCategory {}

    /**
     * Get document by ID
     */
    @Service(
        name = "solrGetDoc",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "solrGetDoc",
        description = "Get document by ID",
        transactionTimeout = "72000",
        implemented = {@Implements(service = "solrSearchCommon")},
        attributes = {
            @Attribute(name = "id", type = "String", mode = "IN"),
            @Attribute(name = "doc", type = "Map", mode = "OUT", optional = "true", description = "The first result")
        }
    )
    public interface SolrGetDoc {}

    /**
     * Get documents by ID
     */
    @Service(
        name = "solrGetDocs",
        engine = "java",
        location = "com.ilscipio.scipio.solr.SolrProductSearch",
        invoke = "solrGetDocs",
        description = "Get documents by ID",
        transactionTimeout = "72000",
        implemented = {@Implements(service = "solrSearchCommon")},
        attributes = {
            @Attribute(name = "idList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "resultType", type = "String", mode = "IN", optional = "true", defaultValue = "list", description = "Values: list, map"),
            @Attribute(name = "docList", type = "List", mode = "OUT", optional = "true", description = "Document list, if resultType list"),
            @Attribute(name = "docMap", type = "Map", mode = "OUT", optional = "true", description = "Maps id to document, if resultType map")
        }
    )
    public interface SolrGetDocs {}

}
