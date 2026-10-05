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
package com.ilscipio.scipio.service.def.mca;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * A mail condition action rule: the annotation form of a service-mca.xml {@code <mca>} element.
 *
 * <p>SCIPIO: 4.0.0: Added; service-mca.xml was the last service definition type with no
 * annotation form, so those rules had to stay in XML.</p>
 *
 * @see McaCondition
 * @see McaAction
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Repeatable(McaList.class)
public @interface Mca {

    /**
     * The rule name; unique across components.
     */
    String name();

    /**
     * Conditions that all must match for the actions to run.
     */
    McaCondition[] conditions() default {};

    /**
     * Services to invoke when the conditions match.
     */
    McaAction[] actions() default {};
}
