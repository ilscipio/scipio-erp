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
 * Defines a Scipio screen widget, equivalent to widget-screen.xsd screen element.
 *
 * <p>This annotation can be applied to a class or method to define a screen.
 * For action-only screens, the annotated method's body serves as the actions logic.</p>
 *
 * <p>Example usage for an actions-only screen:</p>
 * <pre>
 * {@literal @}Screen(name = "MyActionsScreen")
 * {@literal @}SetAction(field = "myVar", value = "myValue")
 * {@literal @}ServiceAction(serviceName = "myService", resultMapName = "serviceResult")
 * public static void myActionsScreen(Map&lt;String, Object&gt; context) {
 *     // Optional: Additional programmatic actions
 * }
 * </pre>
 *
 * <p>Example usage for a decorator-based screen:</p>
 * <pre>
 * {@literal @}Screen(name = "MyScreen")
 * {@literal @}DecoratorScreen(name = "main-decorator", location = "${parameters.mainDecoratorLocation}")
 * {@literal @}DecoratorSection(name = "body")
 * {@literal @}IncludeForm(name = "MyForm", location = "component://myapp/widget/MyForms.xml")
 * public interface MyScreenDef {}
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
@Repeatable(ScreenList.class)
public @interface Screen {

    /**
     * Screen name; required.
     *
     * <p>This name must be unique within the screen location/file.</p>
     */
    String name();

    /**
     * Transaction timeout in seconds; optional.
     *
     * <p>Supports flexible expressions like "${parameters.timeout}".</p>
     */
    String transactionTimeout() default "";

    /**
     * SCIPIO: Transaction timeout parameter name.
     *
     * <p>If true (boolean), the screen allows the transaction timeout request parameter to control
     * the transaction timeout. If false (default), only the request, session, and application
     * attributes are checked for a transaction timeout attribute.</p>
     *
     * <p>This boolean (true/false) setting may optionally be set to an alternative name to use
     * as the timeout parameter name.</p>
     */
    String transactionTimeoutParam() default "";

    /**
     * Whether to use a transaction for this screen; optional.
     *
     * <p>Default is true.</p>
     */
    boolean useTransaction() default true;

    /**
     * Whether to cache this screen; optional.
     *
     * <p>Default is false.</p>
     */
    boolean useCache() default false;

    /**
     * Actions to execute before rendering; optional.
     *
     * <p>Can also be specified using separate {@link SetAction}, {@link ServiceAction},
     * {@link EntityOneAction}, {@link ScriptAction} annotations.</p>
     */
    Action[] actions() default {};

    /**
     * Decorator screen configuration; optional.
     *
     * <p>Can also be specified using separate {@link DecoratorScreen} annotation.</p>
     */
    DecoratorScreen decorator() default @DecoratorScreen(name = "");

    /**
     * Include screen configuration; optional.
     *
     * <p>For simple screens that just include another screen.</p>
     */
    IncludeScreen includeScreen() default @IncludeScreen(name = "");

    /**
     * Conditional sections for this screen; optional.
     *
     * <p>Allows defining sections with conditions, widgets, and fail-widgets.</p>
     */
    Section[] sections() default {};

    // ========================================================================
    // Location alias attributes for backward compatibility with XML references
    // ========================================================================

    /**
     * Single alias location for backward compatibility with XML references.
     *
     * <p>When specified, lookups for this component:// location will resolve to this
     * annotated screen instead of the XML file.</p>
     *
     * <p>Example: "component://setup/widget/SetupScreens.xml"</p>
     *
     * <p>SCIPIO: 4.0.0: Added for XML-to-annotation migration support.</p>
     */
    String location() default "";

    /**
     * Multiple alias locations for backward compatibility with XML references.
     *
     * <p>When specified, lookups for any of these component:// locations will resolve
     * to this annotated screen instead of the XML file.</p>
     *
     * <p>Example: {"component://setup/widget/SetupScreens.xml", "component://setup/widget/OldScreens.xml"}</p>
     *
     * <p>SCIPIO: 4.0.0: Added for XML-to-annotation migration support.</p>
     */
    String[] locations() default {};

    /** SCIPIO: 4.0.0: Condition of the screen root section (XML: screen/section/condition). */
    Condition condition() default @Condition;

    /** SCIPIO: 4.0.0: Fail-widgets of the screen root section (XML: screen/section/fail-widgets). */
    WidgetsForContainer failWidgets() default @WidgetsForContainer;

    /** SCIPIO: 4.0.0: Catch-actions of the screen root section (XML: screen/section/catch-actions). */
    Actions catchActions() default @Actions;

    /** SCIPIO: 4.0.0: Finally-actions of the screen root section (XML: screen/section/finally-actions). */
    Actions finallyActions() default @Actions;
}
