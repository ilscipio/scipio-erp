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

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a script action for screen widgets.
 *
 * <p>Executes a script (Groovy, BSH, etc.) either from a file or inline.</p>
 *
 * <p>Example XML equivalent (file-based):</p>
 * <pre>{@code
 * <script location="component://setup/webapp/setup/WEB-INF/actions/SetupWizard.groovy"/>
 * }</pre>
 *
 * <p>Example XML equivalent (inline):</p>
 * <pre>{@code
 * <script lang="groovy"><![CDATA[
 *     context.myValue = "Hello";
 * ]]></script>
 * }</pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
@Repeatable(ScriptActionList.class)
public @interface ScriptAction {

    /**
     * The script file location (e.g., "component://app/webapp/WEB-INF/actions/MyScript.groovy").
     * Either location or script must be specified.
     */
    String location() default "";

    /**
     * Inline script content.
     * Either location or script must be specified.
     */
    String script() default "";

    /**
     * The scripting language (groovy, bsh, javascript, etc.).
     * Default is "groovy".
     */
    String lang() default "groovy";
}
