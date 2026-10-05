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
package com.ilscipio.scipio.content.eeca;

import com.ilscipio.scipio.service.def.eeca.*;

/**
 * Auto-generated annotation-based entity ECA definitions.
 *
 * <p>Generated from eecas.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Eecas {

    /**
     * EECA for entity ElectronicText on store-remove/return.
     */
    @Eeca(
        entity = "ElectronicText",
        operation = "store-remove",
        event = "return",
        actions = {
            @EecaAction(
                service = "clearAssociatedRenderCache",
                mode = "sync"
            )
        }
    )
    public interface ElectronicTextStoreRemoveReturnEeca1 {}

    /**
     * EECA for entity Content on create/return.
     */
    @Eeca(
        entity = "Content",
        operation = "create",
        event = "return",
        actions = {
            @EecaAction(
                service = "indexContentKeywords",
                mode = "sync",
                valueAttr = "contentInstance"
            )
        }
    )
    public interface ContentCreateReturnEeca2 {}

    /**
     * EECA for entity Content on store/return.
     */
    @Eeca(
        entity = "Content",
        operation = "store",
        event = "return",
        actions = {
            @EecaAction(
                service = "indexContentKeywords",
                mode = "sync"
            )
        }
    )
    public interface ContentStoreReturnEeca3 {}

    /**
     * EECA for entity ContentAttribute on create-store/return.
     */
    @Eeca(
        entity = "ContentAttribute",
        operation = "create-store",
        event = "return",
        actions = {
            @EecaAction(
                service = "indexContentKeywords",
                mode = "sync"
            )
        }
    )
    public interface ContentAttributeCreateStoreReturnEeca4 {}

    /**
     * EECA for entity ContentMetaData on create-store/return.
     */
    @Eeca(
        entity = "ContentMetaData",
        operation = "create-store",
        event = "return",
        actions = {
            @EecaAction(
                service = "indexContentKeywords",
                mode = "sync"
            )
        }
    )
    public interface ContentMetaDataCreateStoreReturnEeca5 {}

    /**
     * EECA for entity ContentRole on create-store/return.
     */
    @Eeca(
        entity = "ContentRole",
        operation = "create-store",
        event = "return",
        actions = {
            @EecaAction(
                service = "indexContentKeywords",
                mode = "sync"
            )
        }
    )
    public interface ContentRoleCreateStoreReturnEeca6 {}

    /**
     * EECA for entity ProductContent on create-store/return.
     */
    @Eeca(
        entity = "ProductContent",
        operation = "create-store",
        event = "return",
        actions = {
            @EecaAction(
                service = "indexContentKeywords",
                mode = "sync"
            )
        }
    )
    public interface ProductContentCreateStoreReturnEeca7 {}

    /**
     * EECA for entity ProductCategoryContent on create-store/return.
     */
    @Eeca(
        entity = "ProductCategoryContent",
        operation = "create-store",
        event = "return",
        actions = {
            @EecaAction(
                service = "indexContentKeywords",
                mode = "sync"
            )
        }
    )
    public interface ProductCategoryContentCreateStoreReturnEeca8 {}

    /**
     * EECA for entity PartyContent on create-store/return.
     */
    @Eeca(
        entity = "PartyContent",
        operation = "create-store",
        event = "return",
        actions = {
            @EecaAction(
                service = "indexContentKeywords",
                mode = "sync"
            )
        }
    )
    public interface PartyContentCreateStoreReturnEeca9 {}

    /**
     * EECA for entity WebSiteContent on create-store/return.
     */
    @Eeca(
        entity = "WebSiteContent",
        operation = "create-store",
        event = "return",
        actions = {
            @EecaAction(
                service = "indexContentKeywords",
                mode = "sync"
            )
        }
    )
    public interface WebSiteContentCreateStoreReturnEeca10 {}

    /**
     * EECA for entity WorkEffortContent on create-store/return.
     */
    @Eeca(
        entity = "WorkEffortContent",
        operation = "create-store",
        event = "return",
        actions = {
            @EecaAction(
                service = "indexContentKeywords",
                mode = "sync"
            )
        }
    )
    public interface WorkEffortContentCreateStoreReturnEeca11 {}

}
