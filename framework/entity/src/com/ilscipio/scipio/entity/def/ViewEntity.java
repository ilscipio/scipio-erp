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
package com.ilscipio.scipio.entity.def;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a view-entity (SQL view) combining multiple entities.
 *
 * <p>Corresponds to view-entity element in entitymodel.xsd.</p>
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}ViewEntity(
 *     name = "UserLoginAndSecurityGroup",
 *     packageName = "org.ofbiz.security.securitygroup",
 *     title = "UserLogin And SecurityGroup View",
 *     neverCache = true,
 *     members = {
 *         {@literal @}MemberEntity(entityAlias = "ULSG", entityName = "UserLoginSecurityGroup"),
 *         {@literal @}MemberEntity(entityAlias = "UL", entityName = "UserLogin")
 *     },
 *     aliasAlls = {
 *         {@literal @}AliasAll(entityAlias = "ULSG"),
 *         {@literal @}AliasAll(entityAlias = "UL")
 *     },
 *     viewLinks = {
 *         {@literal @}ViewLink(entityAlias = "ULSG", relEntityAlias = "UL",
 *                    keyMaps = {@literal @}KeyMap(fieldName = "userLoginId"))
 *     }
 * )
 * public interface UserLoginAndSecurityGroupView {}
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface ViewEntity {

    /**
     * Entity name; required.
     *
     * <p>If not specified, defaults to the simple class name (without "Entity" or "View" suffix).</p>
     */
    String name() default "";

    /**
     * Package name for grouping; required.
     */
    String packageName();

    /**
     * Entity title for documentation; optional.
     */
    String title() default "";

    /**
     * Entity description; optional.
     */
    String description() default "";

    /**
     * Entity this depends on for loading order; optional.
     */
    String dependentOn() default "";

    /**
     * Default resource name for data files; optional.
     */
    String defaultResourceName() default "";

    /**
     * If true, never cache this entity; default false.
     */
    boolean neverCache() default false;

    /**
     * If true, auto-clear cache on changes; default true.
     */
    boolean autoClearCache() default true;

    /**
     * If true, this redefines an existing entity (suppresses warnings); default false.
     */
    boolean redefinition() default false;

    /**
     * Override alias-columns setting for this view; optional.
     * Empty string means use default from entityengine.xml.
     */
    String aliasColumns() default "";

    /**
     * Copyright notice; optional.
     */
    String copyright() default "";

    /**
     * Author; optional.
     */
    String author() default "";

    /**
     * Version; optional.
     */
    String version() default "";

    // ========== Nested definitions ==========

    /**
     * Member entities composing this view; at least one required.
     */
    MemberEntity[] members() default {};

    /**
     * Member entity dependency order (SCIPIO extension); optional.
     */
    String memberDependencyOrder() default "";

    /**
     * Alias-all definitions; optional.
     */
    AliasAll[] aliasAlls() default {};

    /**
     * Individual alias definitions; optional.
     */
    Alias[] aliases() default {};

    /**
     * View links (joins) between member entities; optional.
     */
    ViewLink[] viewLinks() default {};

    /**
     * Relations defined on this view; optional.
     */
    Relation[] relations() default {};

    /**
     * Entity condition for filtering/ordering; optional.
     */
    EntityCondition condition() default @EntityCondition;
}
