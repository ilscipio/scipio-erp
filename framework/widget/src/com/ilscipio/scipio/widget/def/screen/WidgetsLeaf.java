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
package com.ilscipio.scipio.widget.def.screen;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The widgets of a {@link SectionLeaf}: content plus one level of {@link ContainerLeaf},
 * and no screenlet or nested section.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 *
 * @see SectionLeaf
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({}) // Used as nested annotation only
public @interface WidgetsLeaf {

    Widget[] value() default {};

    ContainerLeaf[] containers() default {};

    IncludeForm[] includeForms() default {};

    IncludeScreen[] includeScreens() default {};

    IncludeMenu[] includeMenus() default {};

    Label[] labels() default {};

    HtmlTemplate[] htmlTemplates() default {};

    DecoratorSectionInclude[] decoratorSectionIncludes() default {};
}
