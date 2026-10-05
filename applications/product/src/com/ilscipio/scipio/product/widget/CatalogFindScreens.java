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
package com.ilscipio.scipio.product.widget;

import com.ilscipio.scipio.widget.def.screen.*;
import com.ilscipio.scipio.widget.def.condition.Condition;
import com.ilscipio.scipio.widget.def.condition.ConditionNode;
import com.ilscipio.scipio.widget.def.condition.NestedCondition;
import com.ilscipio.scipio.widget.def.condition.NestedCondition2;
import com.ilscipio.scipio.widget.def.condition.impl.*;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CatalogFindScreens {

    @Screen(name = "advancedsearch", location = "component://product/widget/catalog/FindScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleAdvancedSearch")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductCategory", list = "productCategories", conditions = {@ConditionExpr(fieldName = "showInSelect", operator = "not-equals", value = "N")}, orderBy = {"description"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProdCatalog", list = "prodCatalogs", orderBy = {"catalogName"})
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/find/advancedsearchoptions.groovy")
    @DecoratorScreen(
        name = "CommonFindDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/find/advancedsearch.ftl"
            )})
        }
    )
    public interface advancedsearch {}

    @Screen(name = "keywordsearch", location = "component://product/widget/catalog/FindScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleSearchResults")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/find/keywordsearch.groovy")
    @DecoratorScreen(
        name = "CommonFindDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/find/keywordsearch.ftl"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/find/keywordsearchactions.ftl"
            )})
        }
    )
    public interface keywordsearch {}

    @Screen(name = "exportproducts", location = "component://product/widget/catalog/FindScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleProductExport")
    @Action(type = ActionType.SET, field = "productExportList", fromField = "parameters.productExportList")
    @DecoratorScreen(
        name = "CommonFindDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/find/exportproducts.ftl"
            )})
        }
    )
    public interface exportproducts {}

    @Screen(name = "FindProductById", location = "component://product/widget/catalog/FindScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductFindProductWithIdValue")
    @Action(type = ActionType.SET, field = "idValue", fromField = "parameters.idValue")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Product", valueField = "idProduct", autoFieldMap = false, fieldMaps = {@FieldMap(fieldName = "productId", fromField = "idValue")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "GoodIdentification", list = "goodIdentifications", conditions = {@ConditionExpr(fieldName = "idValue", fromField = "idValue")}, orderBy = {"productId"})
    @DecoratorScreen(
        name = "CommonCatalogAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", overrideByAutoInclude = true, value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "leftbar", location = "component://product/widget/catalog/CommonScreens.xml"
            )}),
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/find/FindProductById.ftl"
            )})
        }
    )
    public interface FindProductById {}

}
