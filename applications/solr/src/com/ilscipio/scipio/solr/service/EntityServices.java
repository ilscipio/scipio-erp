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
public class EntityServices {

    /**
     * Registers entity ID or values for indexing queueing at the end of the transaction or in global queue             immediately if no transaction or requested.
     */
    @Service(
        name = "scheduleEntityIndexing",
        engine = "java",
        location = "com.ilscipio.scipio.solr.EntityIndexer",
        invoke = "scheduleEntityIndexing",
        description = "Registers entity ID or values for indexing queueing at the end of the transaction or in global queue\n            immediately if no transaction or requested.",
        useTransaction = "false",
        log = "quiet",
        logEca = "quiet",
        attributes = {
            @Attribute(name = "event", type = "String", mode = "IN", optional = "true", defaultValue = "trans-commit", description = "Supported: trans-commit (default - appends to transaction, only indexes if committed), global-queue (skips transaction queue and appends to global EntityIndexer queue)"),
            @Attribute(name = "entityName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "idField", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "relationName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "topics", type = "Collection", mode = "IN", optional = "true", description = "Names of subscriber topics to limit which subscribers are triggered"),
            @Attribute(name = "action", type = "String", mode = "IN", optional = "true", description = "Supported values: add (same as addToSolr), remove (same as removeFromSolr),\n                auto (default - this either adds or removes based on whether or not the productId still exists in the database)"),
            @Attribute(name = "instance", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "id", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "entitiesToIndex", type = "Object", mode = "IN", optional = "true", description = "Map of entity names to ordered maps of entity PKs to EntityIndexer.Entry instances, used for trans-commit event"),
            @Attribute(name = "flush", type = "String", mode = "IN", optional = "true", description = "Whether to force-flush any of the queued entities. Values: auto (default), all (flush all products in global queue a.s.a.p. including these)")
        }
    )
    public interface ScheduleEntityIndexing {}

    /**
     * Processes entity ID or values for indexing from EntityIndexer global queue             NOTE: There is only ever one implementation running for each entity at a time, internally locked, and             this service should only be called internally.
     */
    @Service(
        name = "runEntityIndexing",
        engine = "java",
        location = "com.ilscipio.scipio.solr.EntityIndexer",
        invoke = "runEntityIndexing",
        description = "Processes entity ID or values for indexing from EntityIndexer global queue\n            NOTE: There is only ever one implementation running for each entity at a time, internally locked, and\n            this service should only be called internally.",
        useTransaction = "false",
        log = "quiet",
        attributes = {
            @Attribute(name = "entityName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "entityNames", type = "Collection", mode = "IN", optional = "true")
        }
    )
    public interface RunEntityIndexing {}

    /**
     * Registers all found entities for indexing queueing in global queue immediately.
     */
    @Service(
        name = "scheduleAllEntityIndexing",
        engine = "java",
        location = "com.ilscipio.scipio.solr.EntityIndexer",
        invoke = "scheduleAllEntityIndexing",
        description = "Registers all found entities for indexing queueing in global queue immediately.",
        attributes = {
            @Attribute(name = "entityName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "idField", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "topics", type = "Collection", mode = "IN", optional = "true", description = "Names of subscriber topics to limit which subscribers are triggered"),
            @Attribute(name = "action", type = "String", mode = "IN", optional = "true", description = "Supported values: add (same as addToSolr), remove (same as removeFromSolr),\n                auto (default - this either adds or removes based on whether or not the productId still exists in the database)"),
            @Attribute(name = "flush", type = "String", mode = "IN", optional = "true", description = "Whether to force-flush any of the queued entities. Values: auto (default), all (flush all products in global queue a.s.a.p. including these)"),
            @Attribute(name = "maxRows", type = "Integer", mode = "IN", optional = "true")
        }
    )
    public interface ScheduleAllEntityIndexing {}

    /**
     * Registers product ID or values for indexing queueing at the end of the transaction or in global queue             immediately if no transaction or requested. Replaces registerUpdateToSolr.
     */
    @Service(
        name = "scheduleProductIndexing",
        engine = "java",
        location = "com.ilscipio.scipio.solr.EntityIndexer",
        invoke = "scheduleEntityIndexing",
        description = "Registers product ID or values for indexing queueing at the end of the transaction or in global queue\n            immediately if no transaction or requested. Replaces registerUpdateToSolr.",
        useTransaction = "false",
        log = "quiet",
        logEca = "quiet",
        implemented = {@Implements(service = "scheduleEntityIndexing")},
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "updateVariants", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, also update the variant products immediately associated to this one (added 2018-07-19).\n                (NOTE: ignored if effective action is product removal)"),
            @Attribute(name = "updateVariantsDeep", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, also update the variant products associated to this one (added 2018-07-19), and their variants, etc.\n                (NOTE: ignored if effective action is product removal)"),
            @Attribute(name = "updateVirtual", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, also update the virtual product immediately associated to this one (added 2018-07-25).\n                (NOTE: ignored if effective action is product removal)"),
            @Attribute(name = "updateVirtualDeep", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, also update the virtual product associated to this one (added 2018-07-25), and its virtuals, etc.\n                (NOTE: ignored if effective action is product removal)")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "entityName", optional = "true", defaultValue = "Product"),
            @OverrideAttribute(name = "idField", type = "Object", optional = "true", defaultValue = "productId")
        }
    )
    public interface ScheduleProductIndexing {}

    /**
     * Registers all found products for indexing queueing in global queue immediately.
     */
    @Service(
        name = "scheduleAllProductIndexing",
        engine = "java",
        location = "com.ilscipio.scipio.solr.EntityIndexer",
        invoke = "scheduleAllEntityIndexing",
        description = "Registers all found products for indexing queueing in global queue immediately.",
        implemented = {@Implements(service = "scheduleAllEntityIndexing")},
        overrideAttributes = {
            @OverrideAttribute(name = "entityName", optional = "true", defaultValue = "Product"),
            @OverrideAttribute(name = "idField", type = "Object", optional = "true", defaultValue = "productId")
        }
    )
    public interface ScheduleAllProductIndexing {}

    @Service(
        name = "entityIndexingConsumer",
        engine = "interface",
        attributes = {
            @Attribute(name = "docs", type = "Collection", mode = "IN", optional = "true", description = "Collection of EntityIndexer.DocEntry"),
            @Attribute(name = "docsToRemove", type = "Collection", mode = "IN", optional = "true", description = "Collection of EntityIndexer.Entry"),
            @Attribute(name = "refEntries", type = "Collection", mode = "IN", optional = "true", description = "Collection of the original entity primary key PKs that triggered this consume, before expansion, for error handling"),
            @Attribute(name = "onError", type = "String", mode = "IN", optional = "true", description = "ignore (default), requeue-all (requeue products for all consumers - e.g. solr and google),\n                requeue-topic (requeue products for current consumer - solr or google)")
        }
    )
    public interface EntityIndexingConsumer {}

    @Service(
        name = "productIndexingConsumer",
        engine = "interface",
        implemented = {@Implements(service = "entityIndexingConsumer")},
        attributes = {
            @Attribute(name = "docs", type = "Collection", mode = "IN", optional = "true", description = "Collection of ProductIndexer.ProductDocEntry"),
            @Attribute(name = "docsToRemove", type = "Collection", mode = "IN", optional = "true", description = "Collection of ProductIndexer.ProductEntry"),
            @Attribute(name = "docs", type = "Collection", mode = "IN", optional = "true", description = "Collection of ProductIndexer.ProductDocEntry")
        }
    )
    public interface ProductIndexingConsumer {}

}
