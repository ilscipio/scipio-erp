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

import com.ilscipio.scipio.widget.def.menu.*;
import com.ilscipio.scipio.widget.def.screen.SetAction;

/**
 * Test class demonstrating annotation-based menu definitions.
 *
 * <p>This class contains examples of various menu patterns using annotations
 * instead of XML definitions.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for menu annotations testing.</p>
 */
public class TestMenusAnnotated {

    /**
     * Example 1: Simple navigation menu.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;menu name="TestSimpleMenu" type="simple"&gt;
     *     &lt;menu-item name="home" title="Home"&gt;
     *         &lt;link target="main" text="Home"/&gt;
     *     &lt;/menu-item&gt;
     *     &lt;menu-item name="products" title="Products"&gt;
     *         &lt;link target="products" text="Products"/&gt;
     *     &lt;/menu-item&gt;
     *     &lt;menu-item name="about" title="About"&gt;
     *         &lt;link target="about" text="About Us"/&gt;
     *     &lt;/menu-item&gt;
     * &lt;/menu&gt;
     * </pre>
     */
    @Menu(
        name = "TestSimpleMenu",
        type = MenuType.SIMPLE,
        items = {
            @MenuItem(name = "home", title = "Home",
                link = @MenuLink(target = "main", text = "Home")),
            @MenuItem(name = "products", title = "Products",
                link = @MenuLink(target = "products", text = "Products")),
            @MenuItem(name = "about", title = "About",
                link = @MenuLink(target = "about", text = "About Us"))
        }
    )
    public interface TestSimpleMenu {}

    /**
     * Example 2: Menu with link parameters.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;menu name="TestParamsMenu" type="simple"&gt;
     *     &lt;menu-item name="viewProduct" title="View Product"&gt;
     *         &lt;link target="ViewProduct" text="View"&gt;
     *             &lt;parameter param-name="productId" from-field="productId"/&gt;
     *         &lt;/link&gt;
     *     &lt;/menu-item&gt;
     * &lt;/menu&gt;
     * </pre>
     */
    @Menu(
        name = "TestParamsMenu",
        type = MenuType.SIMPLE,
        items = {
            @MenuItem(name = "viewProduct", title = "View Product",
                link = @MenuLink(
                    target = "ViewProduct",
                    text = "View",
                    parameters = @MenuParameter(paramName = "productId", fromField = "productId")
                )
            )
        }
    )
    public interface TestParamsMenu {}

    /**
     * Example 3: Horizontal menu with styles.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;menu name="TestStyledMenu" type="simple" orientation="horizontal"
     *       default-widget-style="nav-item" default-selected-style="active"&gt;
     *     &lt;menu-item name="dashboard" title="Dashboard" widget-style="dashboard-item"&gt;
     *         &lt;link target="Dashboard"/&gt;
     *     &lt;/menu-item&gt;
     *     &lt;menu-item name="reports" title="Reports"&gt;
     *         &lt;link target="Reports"/&gt;
     *     &lt;/menu-item&gt;
     * &lt;/menu&gt;
     * </pre>
     */
    @Menu(
        name = "TestStyledMenu",
        type = MenuType.SIMPLE,
        orientation = Orientation.HORIZONTAL,
        defaultWidgetStyle = "nav-item",
        defaultSelectedStyle = "active",
        items = {
            @MenuItem(name = "dashboard", title = "Dashboard",
                widgetStyle = "dashboard-item",
                link = @MenuLink(target = "Dashboard")),
            @MenuItem(name = "reports", title = "Reports",
                link = @MenuLink(target = "Reports"))
        }
    )
    public interface TestStyledMenu {}

    /**
     * Example 4: Menu with permission-based condition.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;menu name="TestConditionMenu" type="simple"&gt;
     *     &lt;menu-item name="admin" title="Admin"&gt;
     *         &lt;condition&gt;
     *             &lt;if-has-permission permission="ADMIN" action="_VIEW"/&gt;
     *         &lt;/condition&gt;
     *         &lt;link target="admin" text="Administration"/&gt;
     *     &lt;/menu-item&gt;
     *     &lt;menu-item name="public" title="Public"&gt;
     *         &lt;link target="public" text="Public Area"/&gt;
     *     &lt;/menu-item&gt;
     * &lt;/menu&gt;
     * </pre>
     */
    @Menu(
        name = "TestConditionMenu",
        type = MenuType.SIMPLE,
        items = {
            @MenuItem(name = "admin", title = "Admin",
                condition = @MenuItemCondition(
                    permission = "ADMIN", permissionAction = "_VIEW"
                ),
                link = @MenuLink(target = "admin", text = "Administration")),
            @MenuItem(name = "public", title = "Public",
                link = @MenuLink(target = "public", text = "Public Area"))
        }
    )
    public interface TestConditionMenu {}

    /**
     * Example 5: Menu with sub-menus (cascade style).
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;menu name="TestCascadeMenu" type="cascade"&gt;
     *     &lt;menu-item name="catalog" title="Catalog"&gt;
     *         &lt;link target="catalog"/&gt;
     *         &lt;sub-menu name="catalog-sub"&gt;
     *             &lt;menu-item name="categories" title="Categories"&gt;
     *                 &lt;link target="categories"/&gt;
     *             &lt;/menu-item&gt;
     *             &lt;menu-item name="products" title="Products"&gt;
     *                 &lt;link target="products"/&gt;
     *             &lt;/menu-item&gt;
     *         &lt;/sub-menu&gt;
     *     &lt;/menu-item&gt;
     *     &lt;menu-item name="orders" title="Orders"&gt;
     *         &lt;link target="orders"/&gt;
     *     &lt;/menu-item&gt;
     * &lt;/menu&gt;
     * </pre>
     */
    @Menu(
        name = "TestCascadeMenu",
        type = MenuType.CASCADE,
        items = {
            @MenuItem(name = "catalog", title = "Catalog",
                link = @MenuLink(target = "catalog"),
                subMenus = @SubMenu(
                    name = "catalog-sub",
                    items = {
                        @SubMenuItem(name = "categories", title = "Categories",
                            link = @MenuLink(target = "categories")),
                        @SubMenuItem(name = "products", title = "Products",
                            link = @MenuLink(target = "products"))
                    }
                )
            ),
            @MenuItem(name = "orders", title = "Orders",
                link = @MenuLink(target = "orders"))
        }
    )
    public interface TestCascadeMenu {}

    /**
     * Example 6: Vertical menu with selection tracking.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;menu name="TestVerticalMenu" type="simple" orientation="vertical"
     *       selected-menuitem-context-field-name="selectedItem"&gt;
     *     &lt;menu-item name="profile" title="Profile"&gt;
     *         &lt;link target="profile"/&gt;
     *     &lt;/menu-item&gt;
     *     &lt;menu-item name="settings" title="Settings"&gt;
     *         &lt;link target="settings"/&gt;
     *     &lt;/menu-item&gt;
     *     &lt;menu-item name="logout" title="Logout"&gt;
     *         &lt;link target="logout"/&gt;
     *     &lt;/menu-item&gt;
     * &lt;/menu&gt;
     * </pre>
     */
    @Menu(
        name = "TestVerticalMenu",
        type = MenuType.SIMPLE,
        orientation = Orientation.VERTICAL,
        selectedMenuItemContextFieldName = "selectedItem",
        items = {
            @MenuItem(name = "profile", title = "Profile",
                link = @MenuLink(target = "profile")),
            @MenuItem(name = "settings", title = "Settings",
                link = @MenuLink(target = "settings")),
            @MenuItem(name = "logout", title = "Logout",
                link = @MenuLink(target = "logout"))
        }
    )
    public interface TestVerticalMenu {}

    /**
     * Example 7: Menu that extends another menu.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;menu name="TestExtendedMenu" type="simple" extends="CommonAppBarMenu"
     *       extends-resource="component://common/widget/CommonMenus.xml"&gt;
     *     &lt;menu-item name="extra" title="Extra"&gt;
     *         &lt;link target="extra"/&gt;
     *     &lt;/menu-item&gt;
     * &lt;/menu&gt;
     * </pre>
     */
    @Menu(
        name = "TestExtendedMenu",
        type = MenuType.SIMPLE,
        extendsMenu = "CommonAppBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "extra", title = "Extra",
                link = @MenuLink(target = "extra"))
        }
    )
    public interface TestExtendedMenu {}

    /**
     * Example 8: Menu with link image.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;menu name="TestImageMenu" type="simple"&gt;
     *     &lt;menu-item name="home" title="Home"&gt;
     *         &lt;link target="main"&gt;
     *             &lt;image src="/images/home.png" alt="Home"/&gt;
     *         &lt;/link&gt;
     *     &lt;/menu-item&gt;
     * &lt;/menu&gt;
     * </pre>
     */
    @Menu(
        name = "TestImageMenu",
        type = MenuType.SIMPLE,
        items = {
            @MenuItem(name = "home", title = "Home",
                link = @MenuLink(
                    target = "main",
                    image = @MenuImage(src = "/images/home.png", alt = "Home")
                )
            )
        }
    )
    public interface TestImageMenu {}

    /**
     * Example 9: Menu with actions.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;menu name="TestActionsMenu" type="simple"&gt;
     *     &lt;actions&gt;
     *         &lt;set field="menuTitle" value="Dynamic Menu"/&gt;
     *     &lt;/actions&gt;
     *     &lt;menu-item name="dynamic" title="${menuTitle}"&gt;
     *         &lt;link target="dynamic"/&gt;
     *     &lt;/menu-item&gt;
     * &lt;/menu&gt;
     * </pre>
     */
    @Menu(
        name = "TestActionsMenu",
        type = MenuType.SIMPLE,
        actions = @MenuActions(
            set = @SetAction(field = "menuTitle", value = "Dynamic Menu")
        ),
        items = {
            @MenuItem(name = "dynamic", title = "${menuTitle}",
                link = @MenuLink(target = "dynamic"))
        }
    )
    public interface TestActionsMenu {}

    /**
     * Example 10: Menu with right-aligned items.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;menu name="TestAlignMenu" type="simple" orientation="horizontal"&gt;
     *     &lt;menu-item name="left1" title="Left 1"&gt;
     *         &lt;link target="left1"/&gt;
     *     &lt;/menu-item&gt;
     *     &lt;menu-item name="left2" title="Left 2"&gt;
     *         &lt;link target="left2"/&gt;
     *     &lt;/menu-item&gt;
     *     &lt;menu-item name="right1" title="Right 1" align="right"&gt;
     *         &lt;link target="right1"/&gt;
     *     &lt;/menu-item&gt;
     * &lt;/menu&gt;
     * </pre>
     */
    @Menu(
        name = "TestAlignMenu",
        type = MenuType.SIMPLE,
        orientation = Orientation.HORIZONTAL,
        items = {
            @MenuItem(name = "left1", title = "Left 1",
                link = @MenuLink(target = "left1")),
            @MenuItem(name = "left2", title = "Left 2",
                link = @MenuLink(target = "left2")),
            @MenuItem(name = "right1", title = "Right 1",
                align = Align.RIGHT,
                link = @MenuLink(target = "right1"))
        }
    )
    public interface TestAlignMenu {}

    /**
     * Example 11: Menu with confirmation dialog.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;menu name="TestConfirmMenu" type="simple"&gt;
     *     &lt;menu-item name="delete" title="Delete"&gt;
     *         &lt;link target="deleteItem" request-confirmation="true"
     *               confirmation-message="Are you sure you want to delete?"/&gt;
     *     &lt;/menu-item&gt;
     * &lt;/menu&gt;
     * </pre>
     */
    @Menu(
        name = "TestConfirmMenu",
        type = MenuType.SIMPLE,
        items = {
            @MenuItem(name = "delete", title = "Delete",
                link = @MenuLink(
                    target = "deleteItem",
                    requestConfirmation = true,
                    confirmationMessage = "Are you sure you want to delete?"
                )
            )
        }
    )
    public interface TestConfirmMenu {}

    /**
     * Example 12: Menu with external link.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;menu name="TestExternalMenu" type="simple"&gt;
     *     &lt;menu-item name="external" title="External Link"&gt;
     *         &lt;link target="https://example.com" url-mode="plain" target-window="_blank"
     *               text="Visit Example.com"/&gt;
     *     &lt;/menu-item&gt;
     * &lt;/menu&gt;
     * </pre>
     */
    @Menu(
        name = "TestExternalMenu",
        type = MenuType.SIMPLE,
        items = {
            @MenuItem(name = "external", title = "External Link",
                link = @MenuLink(
                    target = "https://example.com",
                    urlMode = UrlMode.PLAIN,
                    targetWindow = "_blank",
                    text = "Visit Example.com"
                )
            )
        }
    )
    public interface TestExternalMenu {}
}
