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
package com.ilscipio.scipio.common.service;

import com.ilscipio.scipio.service.def.*;
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class GeoServices {

    /**
     * Create a Country Tele Code
     */
    @Service(
        name = "createCountryTeleCode",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Country Tele Code",
        defaultEntityName = "CountryTeleCode",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCountryTeleCode {}

    /**
     * Update a Country Tele Code
     */
    @Service(
        name = "updateCountryTeleCode",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Country Tele Code",
        defaultEntityName = "CountryTeleCode",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCountryTeleCode {}

    /**
     * Delete a Country Tele Code
     */
    @Service(
        name = "deleteCountryTeleCode",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Country Tele Code",
        defaultEntityName = "CountryTeleCode",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCountryTeleCode {}

    /**
     * Create a Country Capital
     */
    @Service(
        name = "createCountryCapital",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Country Capital",
        defaultEntityName = "CountryCapital",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCountryCapital {}

    /**
     * Update a Country Capital
     */
    @Service(
        name = "updateCountryCapital",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Country Capital",
        defaultEntityName = "CountryCapital",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCountryCapital {}

    /**
     * Delete a Country Capital
     */
    @Service(
        name = "deleteCountryCapital",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Country Capital",
        defaultEntityName = "CountryCapital",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCountryCapital {}

    /**
     * Create a Country Code
     */
    @Service(
        name = "createCountryCode",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Country Code",
        defaultEntityName = "CountryCode",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCountryCode {}

    /**
     * Update a Country Code
     */
    @Service(
        name = "updateCountryCode",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Country Code",
        defaultEntityName = "CountryCode",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCountryCode {}

    /**
     * Delete a Country Code
     */
    @Service(
        name = "deleteCountryCode",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Country Code",
        defaultEntityName = "CountryCode",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCountryCode {}

    /**
     * Create a Country Address Format
     */
    @Service(
        name = "createCountryAddressFormat",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Country Address Format",
        defaultEntityName = "CountryAddressFormat",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCountryAddressFormat {}

    /**
     * Update a Country Address Format
     */
    @Service(
        name = "updateCountryAddressFormat",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Country Address Format",
        defaultEntityName = "CountryAddressFormat",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCountryAddressFormat {}

    /**
     * Delete a Country Address Format
     */
    @Service(
        name = "deleteCountryAddressFormat",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Country Address Format",
        defaultEntityName = "CountryAddressFormat",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCountryAddressFormat {}

}
