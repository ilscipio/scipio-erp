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
package com.ilscipio.scipio.widget.test;

import com.ilscipio.scipio.widget.def.screen.*;

/**
 * Test class demonstrating annotation-based screen definitions.
 *
 * <p>This class contains examples of various screen patterns using annotations
 * instead of XML definitions.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations testing.</p>
 */
public class TestScreensAnnotated {

    /**
     * Example 1: Simple actions-only screen.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;screen name="TestActionsScreen"&gt;
     *     &lt;actions&gt;
     *         &lt;set field="titleProperty" value="TestTitle"/&gt;
     *         &lt;set field="activeMainMenuItem" value="test"/&gt;
     *     &lt;/actions&gt;
     * &lt;/screen&gt;
     * </pre>
     */
    @Screen(name = "TestActionsScreen")
    @SetAction(field = "titleProperty", value = "TestTitle")
    @SetAction(field = "activeMainMenuItem", value = "test")
    public interface TestActionsScreen {}

    /**
     * Example 2: Screen with decorator.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;screen name="TestDecoratorScreen"&gt;
     *     &lt;section&gt;
     *         &lt;actions&gt;
     *             &lt;set field="titleProperty" value="PageTitle"/&gt;
     *         &lt;/actions&gt;
     *         &lt;widgets&gt;
     *             &lt;decorator-screen name="main-decorator" location="${parameters.mainDecoratorLocation}"&gt;
     *                 &lt;decorator-section name="body"&gt;
     *                     &lt;include-form name="TestForm" location="component://webtools/widget/TestForms.xml"/&gt;
     *                 &lt;/decorator-section&gt;
     *             &lt;/decorator-screen&gt;
     *         &lt;/widgets&gt;
     *     &lt;/section&gt;
     * &lt;/screen&gt;
     * </pre>
     */
    @Screen(name = "TestDecoratorScreen")
    @SetAction(field = "titleProperty", value = "PageTitle")
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(
                name = "body",
                includeForms = {@IncludeForm(name = "TestForm", location = "component://webtools/widget/TestForms.xml")}
            )
        }
    )
    public interface TestDecoratorScreen {}

    /**
     * Example 3: Screen with service action.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;screen name="TestServiceScreen"&gt;
     *     &lt;actions&gt;
     *         &lt;set field="orderId" from-field="parameters.orderId"/&gt;
     *         &lt;service service-name="getOrderHeader" result-map="orderHeader"&gt;
     *             &lt;field-map field-name="orderId" from-field="orderId"/&gt;
     *         &lt;/service&gt;
     *     &lt;/actions&gt;
     * &lt;/screen&gt;
     * </pre>
     */
    @Screen(name = "TestServiceScreen")
    @SetAction(field = "orderId", fromField = "parameters.orderId")
    @ServiceAction(
        serviceName = "getOrderHeader",
        resultMapName = "orderHeader",
        fieldMaps = @FieldMap(fieldName = "orderId", fromField = "orderId")
    )
    public interface TestServiceScreen {}

    /**
     * Example 4: Screen that includes another screen.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;screen name="TestIncludeScreen"&gt;
     *     &lt;section&gt;
     *         &lt;widgets&gt;
     *             &lt;include-screen name="CommonHeader" location="component://common/widget/CommonScreens.xml"/&gt;
     *         &lt;/widgets&gt;
     *     &lt;/section&gt;
     * &lt;/screen&gt;
     * </pre>
     */
    @Screen(
        name = "TestIncludeScreen",
        includeScreen = @IncludeScreen(name = "CommonHeader", location = "component://common/widget/CommonScreens.xml")
    )
    public interface TestIncludeScreen {}

    /**
     * Example 5: Screen with transaction settings.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;screen name="TestTransactionScreen" use-transaction="true" transaction-timeout="600"&gt;
     *     &lt;actions&gt;
     *         &lt;set field="processingMode" value="batch"/&gt;
     *     &lt;/actions&gt;
     * &lt;/screen&gt;
     * </pre>
     */
    @Screen(
        name = "TestTransactionScreen",
        useTransaction = true,
        transactionTimeout = "600"
    )
    @SetAction(field = "processingMode", value = "batch")
    public interface TestTransactionScreen {}
}
