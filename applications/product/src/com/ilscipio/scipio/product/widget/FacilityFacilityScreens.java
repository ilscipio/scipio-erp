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
public class FacilityFacilityScreens {

    @Screen(name = "FindFacility", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductFindFacilities")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "facility")
    @DecoratorScreen(
        name = "CommonFacilityAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"facilityGenericPermission", "VIEW"
                })}), widgets = @InlineWidgets(decorator = @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(sections = {
                        @SectionLeaf(condition = @Condition(type = ServicePermission.class, params = {"facilityGenericPermission", "CREATE"
                    }), widgets = @WidgetsLeaf(containers = {
                        @ContainerLeaf(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductCreateNewFacility}", style = "${styles.link_nav} ${styles.action_add}", target = "EditFacility"
                        )})}))})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindFacilityOptions", location = "component://product/widget/facility/FacilityForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "FacilitySearchResults"
                        )}))})), failWidgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductFacilityViewPermissionError}", style = "common-msg-error-perm"
                        )}))})
        }
    )
    public interface FindFacility {}

    @Screen(name = "FacilitySearchResults", location = "component://product/widget/facility/FacilityScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"facilityGenericPermission", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "FindFacility", location = "component://product/widget/facility/FacilityForms.xml")}))
    public interface FacilitySearchResults {}

    @Screen(name = "EditFacility", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/facility/EditFacility.groovy")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.facility ? 'ProductEditFacility' : 'ProductNewFacility'}")
    @Action(type = ActionType.SET, field = "titleFormat", value = "\\${finalTitle} ${parameters.facilityId}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "${groovy: context.facility ? 'EditFacility' : 'NewFacility'}")
    @Action(type = ActionType.SET, field = "displayWithNoFacility", value = "${groovy: context.facility ? '' : 'Y'}")
    @DecoratorScreen(
        name = "CommonFacilityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"facility"})}), widgets = @InlineWidgets(sections = {
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = ServicePermission.class, params = {"facilityGenericPermission", "UPDATE"
                        })}), widgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.INCLUDE_MENU, name = "FacilitySubTabBar", location = "component://product/widget/facility/FacilityMenus.xml"
                        ),
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/facility/EditFacility.ftl"
                    )}), failWidgets = @WidgetsForContainer(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductFacilityViewPermissionError}", style = "common-msg-error-perm"
                    )}))}), failWidgets = @InlineWidgets(sections = {
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = ServicePermission.class, params = {"facilityGenericPermission", "CREATE"
                        })}), widgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/facility/EditFacility.ftl"
                        )}), failWidgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductFacilityViewPermissionError}", style = "common-msg-error-perm"
                        )}))}))})
        }
    )
    public interface EditFacility {}

    @Screen(name = "FindFacilityTransfers", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindFacilityTransfers")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Facility", valueField = "facility")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "StatusItem", list = "statusItems", conditions = {@ConditionExpr(fieldName = "statusTypeId", value = "INVENTORY_XFER_STTS")}, orderBy = {"description"})
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ProductInventoryXfers} ${facilityId}")
    @DecoratorScreen(
        name = "CommonFacilityInventoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/facility/FindFacilityTransfers.ftl"
            ),
            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ScipioFacilityTransferList", location = "component://product/widget/facility/FacilityScreens.xml"
            )})
        }
    )
    public interface FindFacilityTransfers {}

    @Screen(name = "ScipioFacilityTransferList", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/facility/FindFacilityTransfers.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/facility/FacilityTransferList.ftl")}))
    public interface ScipioFacilityTransferList {}

    @Screen(name = "TransferInventoryItem", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindFacilityTransfers")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ProductInventoryTransfer}")
    @DecoratorScreen(
        name = "CommonFacilityInventoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/inventory/TransferInventoryItem.ftl"
            )})
        }
    )
    public interface TransferInventoryItem {}

    @Screen(name = "TransferInventoryItemDetail", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindFacilityTransfers")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/inventory/TransferInventoryItem.groovy")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Facility", valueField = "facility")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/inventory/TransferInventoryItemDetail.ftl")}))
    public interface TransferInventoryItemDetail {}

    @Screen(name = "FindFacilityLocation", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindFacilityLocation")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/facility/FindFacilityLocation.groovy")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Facility", valueField = "facility")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ProductFacilityLocation} ${facilityId}")
    @DecoratorScreen(
        name = "CommonFacilitySettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://product/webapp/facility/facility/FindFacilityLocation.ftl"
                )})})
        }
    )
    public interface FindFacilityLocation {}

    @Screen(name = "EditFacilityLocation", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindFacilityLocation")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/facility/EditFacilityLocation.groovy")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Facility", valueField = "facility")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ProductLocation} ${facilityId}")
    @DecoratorScreen(
        name = "CommonFacilitySettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/facility/EditFacilityLocation.ftl"
            )})
        }
    )
    public interface EditFacilityLocation {}

    @Screen(name = "EditFacilityInventoryItems", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditFacilityInventoryItems")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Facility", valueField = "facility")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "facilityId", value = "${parameters.facilityId}")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ProductInventoryItems} ${facilityId}")
    @DecoratorScreen(
        name = "CommonFacilityInventoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductNewInventoryItem}", style = "${styles.link_nav} ${styles.action_add}", target = "EditInventoryItem"
                        ),
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductSearchInventoryItemsByLabels}", style = "${styles.link_nav} ${styles.action_find}", target = "SearchInventoryItemsByLabels"
                    )})})),
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "SearchInventoryItemsParams", location = "component://product/widget/facility/FacilityForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFacilityInventoryItems", location = "component://product/widget/facility/FacilityForms.xml"
                    )}))})})
        }
    )
    public interface EditFacilityInventoryItems {}

    @Screen(name = "SearchInventoryItemsByLabels", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleSearchInventoryItemsByLabels")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditFacilityInventoryItems")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Facility", valueField = "facility")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "InventoryItemLabelType", list = "labelTypes")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/facility/FindInventoryItemsByLabels.groovy")
    @DecoratorScreen(
        name = "CommonFacilityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductInventoryItemsFor} ${facility.facilityName} [${facility.facilityId}]", style = "heading"
            )}, containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductNewFacility}", style = "${styles.link_nav} ${styles.action_add}", target = "EditFacility"
                ),
                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductCreateNewInventoryItemFacility}", style = "${styles.link_nav} ${styles.action_add}", target = "EditInventoryItem"
            ),
            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductInventoryItems}", style = "${styles.link_nav} ${styles.action_view}", target = "EditFacilityInventoryItems"
            )}, position = 1)}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductSearchInventoryItemsByLabels}", includeForms = {
                    @IncludeForm(name = "ListFacilityInventoryItemsNoLocations", location = "component://product/widget/facility/FacilityForms.xml", position = 1
                )}, htmlTemplates = {
                    @HtmlTemplate(location = "component://product/webapp/facility/facility/searchInventoryItemsByLabelsForm.ftl", position = 0
                )}, position = 2)})
        }
    )
    public interface SearchInventoryItemsByLabels {}

    @Screen(name = "ViewFacilityInventoryByProduct", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewFacilityInventoryByProduct")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE_1", valueType = "Integer", defaultValue = "20")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX_1", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.SET, field = "facilityInventoryByProductScreen", value = "ViewFacilityInventoryByProduct")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.SET, field = "productTypeId", fromField = "parameters.productTypeId")
    @Action(type = ActionType.SET, field = "searchInProductCategoryId", fromField = "parameters.searchInProductCategoryId")
    @Action(type = ActionType.SET, field = "productSupplierId", fromField = "parameters.productSupplierId")
    @Action(type = ActionType.SET, field = "offsetQOHQty", fromField = "parameters.offsetQOHQty")
    @Action(type = ActionType.SET, field = "offsetATPQty", fromField = "parameters.offsetATPQty")
    @Action(type = ActionType.SET, field = "productsSoldThruTimestamp", fromField = "parameters.productsSoldThruTimestamp", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "internalName", fromField = "parameters.internalName")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.SET, field = "statusId", fromField = "parameters.statusId")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/facility/ViewFacilityInventoryByProduct.groovy")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Facility", valueField = "facility")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleFindFacilityInventoryItems}")
    @DecoratorScreen(
        name = "CommonFacilityInventoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "ViewFacilityInventoryByProductTabBar", location = "component://product/widget/facility/FacilityMenus.xml"
            )}, decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindFacilityInventoryByProduct", location = "component://product/widget/facility/FacilityForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFacilityInventoryByProduct", location = "component://product/widget/facility/FacilityForms.xml"
                    )}))})})
        }
    )
    public interface ViewFacilityInventoryByProduct {}

    @Screen(name = "ViewFacilityInventoryByProductSimple", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companyName", fromField = "uiLabelMap.FacilityCompanyName", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companySubtitle", fromField = "uiLabelMap.FacilityCompanySubtitle", global = true)
    @Action(type = ActionType.SET, field = "facilityInventoryByProductScreen", value = "ViewFacilityInventoryByProductSimple")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditFacilityInventoryItems")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.SET, field = "productTypeId", fromField = "parameters.productTypeId")
    @Action(type = ActionType.SET, field = "searchInProductCategoryId", fromField = "parameters.searchInProductCategoryId")
    @Action(type = ActionType.SET, field = "productSupplierId", fromField = "parameters.productSupplierId")
    @Action(type = ActionType.SET, field = "offsetQOHQty", fromField = "parameters.offsetQOHQty")
    @Action(type = ActionType.SET, field = "offsetATPQty", fromField = "parameters.offsetATPQty")
    @Action(type = ActionType.SET, field = "productsSoldThruTimestamp", fromField = "parameters.productsSoldThruTimestamp", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "internalName", fromField = "parameters.internalName")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/facility/CountFacilityInventoryByProduct.groovy")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "FindFacilityInventoryByProduct", location = "component://product/widget/facility/FacilityForms.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFacilityInventoryByProduct", location = "component://product/widget/facility/FacilityForms.xml"
            )}, containers = {
                @Container(labels = {
                    @Label(text = "${uiLabelMap.PageTitleEditFacilityInventoryItems} ${uiLabelMap.CommonFor}:", style = "heading"
                ),
                @Label(text = "${facility.facilityName} [${facilityId}]", style = "heading"
            )}, position = 0),
            @Container(widgets = {
                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonBack}", style = "${styles.link_nav_cancel}", target = "ViewFacilityInventoryByProduct"
            )}, position = 1)})
        }
    )
    public interface ViewFacilityInventoryByProductSimple {}

    @Screen(name = "ViewFacilityInventoryByProductReport", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "pageLayoutName", value = "simple-landscape")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditFacilityInventoryItems")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.SET, field = "productTypeId", fromField = "parameters.productTypeId")
    @Action(type = ActionType.SET, field = "searchInProductCategoryId", fromField = "parameters.searchInProductCategoryId")
    @Action(type = ActionType.SET, field = "productSupplierId", fromField = "parameters.productSupplierId")
    @Action(type = ActionType.SET, field = "offsetQOHQty", fromField = "parameters.offsetQOHQty")
    @Action(type = ActionType.SET, field = "offsetATPQty", fromField = "parameters.offsetATPQty")
    @Action(type = ActionType.SET, field = "productsSoldThruTimestamp", fromField = "parameters.productsSoldThruTimestamp", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "internalName", fromField = "parameters.internalName")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/facility/CountFacilityInventoryByProduct.groovy")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "FindFacilityInventoryByProduct", location = "component://product/widget/facility/FacilityForms.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFacilityInventoryByProduct", location = "component://product/widget/facility/FacilityForms.xml"
            )})
        }
    )
    public interface ViewFacilityInventoryByProductReport {}

    @Screen(name = "InventoryItemTotals", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditFacilityInventoryItems")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewFacilityInventoryByProduct")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "InventoryItemTotalsTab")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Facility", valueField = "facility", useCache = true)
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/inventory/InventoryItemTotals.groovy")
    @DecoratorScreen(
        name = "CommonFacilityInventoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "ViewFacilityInventoryByProductTabBar", location = "component://product/widget/facility/FacilityMenus.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductInventoryItemTotals} ${uiLabelMap.CommonFor} ${facility.facilityName}", includeForms = {
                    @IncludeForm(name = "ListInventoryItemTotals", location = "component://product/widget/facility/FacilityForms.xml"
                )})})
        }
    )
    public interface InventoryItemTotals {}

    @Screen(name = "InventoryItemGrandTotals", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewFacilityInventoryByProduct")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "InventoryItemGrandTotalsTab")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Facility", valueField = "facility", useCache = true)
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/inventory/InventoryItemTotals.groovy")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleInventoryItemGrandTotals}")
    @DecoratorScreen(
        name = "CommonFacilityInventoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "ViewFacilityInventoryByProductTabBar", location = "component://product/widget/facility/FacilityMenus.xml"
            )}, screenlets = {
                @Screenlet(title = "${title}", includeForms = {
                    @IncludeForm(name = "ListInventoryItemGrandTotals", location = "component://product/widget/facility/FacilityForms.xml"
                )})})
        }
    )
    public interface InventoryItemGrandTotals {}

    @Screen(name = "InventoryItemTotalsExport", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Facility", valueField = "facility", useCache = true)
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/inventory/InventoryItemTotals.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductInventoryItemTotals} ${uiLabelMap.CommonFor} ${facility.facilityName}", style = "heading"), @Widget(type = WidgetType.INCLUDE_FORM, name = "InventoryItemTotalsExport", location = "component://product/widget/facility/FacilityForms.xml"), @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductInventoryItemGrandTotals} ${uiLabelMap.CommonFor} ${facility.facilityName}", style = "heading"), @Widget(type = WidgetType.INCLUDE_FORM, name = "InventoryItemGrandTotalsExport", location = "component://product/widget/facility/FacilityForms.xml")}))
    public interface InventoryItemTotalsExport {}

    @Screen(name = "InventoryAverageCosts", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditFacilityInventoryItems")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewFacilityInventoryByProduct")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "InventoryAverageCostsTab")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Facility", valueField = "facility", useCache = true)
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/inventory/InventoryAverageCosts.groovy")
    @DecoratorScreen(
        name = "CommonFacilityInventoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "ViewFacilityInventoryByProductTabBar", location = "component://product/widget/facility/FacilityMenus.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductInventoryAverageCosts} ${uiLabelMap.CommonFor} ${facility.facilityName}", includeForms = {
                    @IncludeForm(name = "ListInventoryAverageCosts", location = "component://product/widget/facility/FacilityForms.xml"
                )})})
        }
    )
    public interface InventoryAverageCosts {}

    @Screen(name = "ViewContactMechs", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewContactMechs")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Facility", valueField = "facility")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ProductFacilityContactMech} ${facilityId}")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/facility/ViewContactMechs.groovy")
    @DecoratorScreen(
        name = "CommonFacilitySettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "button-bar", sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"facilityId"})}), widgets = @WidgetsForContainer(sections = {
                            @SectionNested2(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                @Condition(type = ServicePermission.class, params = {"facilityGenericPermission", "CREATE"
                            })}), widgets = @WidgetsForContainer2(value = {
                                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductNewContactMech}", style = "${styles.link_nav} ${styles.action_add}", target = "EditContactMech"
                            )}))}))}, position = 0)}, screenlets = {
                                @Screenlet(htmlTemplates = {
                                    @HtmlTemplate(location = "component://product/webapp/facility/facility/ViewContactMechs.ftl"
                                )}, position = 1)})
        }
    )
    public interface ViewContactMechs {}

    @Screen(name = "EditContactMech", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewContactMechs")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/facility/EditContactMech.groovy")
    @Action(type = ActionType.SET, field = "dependentForm", value = "editcontactmechform")
    @Action(type = ActionType.SET, field = "paramKey", value = "countryGeoId")
    @Action(type = ActionType.SET, field = "mainId", value = "countryGeoId")
    @Action(type = ActionType.SET, field = "dependentId", value = "stateProvinceGeoId")
    @Action(type = ActionType.SET, field = "requestName", value = "getAssociatedStateList")
    @Action(type = ActionType.SET, field = "responseName", value = "stateList")
    @Action(type = ActionType.SET, field = "dependentKeyName", value = "geoId")
    @Action(type = ActionType.SET, field = "descName", value = "geoName")
    @Action(type = ActionType.SET, field = "selectedDependentOption", fromField = "mechMap.postalAddress.stateProvinceGeoId", defaultValue = "_none_")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Facility", valueField = "facility")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleNewFacilityContactMech")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"parameters.contactMechId"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditFacilityContactMech")}))
    @DecoratorScreen(
        name = "CommonFacilitySettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/setDependentDropdownValuesJs.ftl"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/facility/EditContactMech.ftl"
            )})
        }
    )
    public interface EditContactMech {}

    @Screen(name = "EditInventoryItem", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditFacilityInventoryItems")
    @Action(type = ActionType.SET, field = "activeSubMenu2Item", value = "EditInventoryItem")
    @Action(type = ActionType.SET, field = "displayWithNoFacility", value = "Y")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.SET, field = "inventoryItemId", fromField = "parameters.inventoryItemId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Facility", valueField = "facility")
    @Action(type = ActionType.ENTITY_ONE, entityName = "InventoryItem", valueField = "inventoryItem")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "OrderItemShipGrpInvRes", list = "inventoryItemReservations", conditions = {@ConditionExpr(fieldName = "inventoryItemId", operator = "equals", fromField = "inventoryItemId")}, orderBy = {"reservedDatetime"})
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: (context.inventoryItem || (parameters.inventoryItemId && parameters.isCreate != 'true')) ? 'PageTitleEditInventoryItem':'ProductNewInventoryItem'}")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap[titleProperty]} ${facilityId}")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = NotEmpty.class, params = {"inventoryItem"}), @Condition(type = Empty.class, params = {"facility"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "parameters.facilityId", fromField = "inventoryItem.facilityId"), @Action(type = ActionType.SET, field = "facilityId", fromField = "inventoryItem.facilityId"), @Action(type = ActionType.ENTITY_ONE, entityName = "Facility", valueField = "facility")}))
    @DecoratorScreen(
        name = "CommonFacilityInventoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "EditInventoryItem", location = "component://product/widget/facility/InventoryForms.xml"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"inventoryItem"})}), position = 0
                ),
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"inventoryItemId"})}
                ), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(title = "${uiLabelMap.ProductInventoryItemReservations}", name = "inventory-item-reservations", initiallyCollapsed = true, includeForms = {
                        @IncludeForm(name = "InventoryItemReservations", location = "component://product/widget/facility/InventoryForms.xml"
                    )})}), position = 2),
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = Compare.class, params = {"inventoryItem.inventoryItemTypeId", "equals", "NON_SERIAL_INV_ITEM"
                    })}), actions = @Actions(value = {
                        @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/inventory/PhysicalInventoryVariance.groovy"
                    )}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.ProductPhysicalInventoryVariances}", name = "physical-inventory-variances", initiallyCollapsed = true, includeForms = {
                            @IncludeForm(name = "CreatePhysicalInventoryAndVariance", location = "component://product/widget/facility/InventoryForms.xml"
                        ),
                        @IncludeForm(name = "ViewPhysicalInventoryAndVariance", location = "component://product/widget/facility/InventoryForms.xml"
                    )})}), position = 3)})
        }
    )
    public interface EditInventoryItem {}

    @Screen(name = "ViewInventoryItemDetail", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditInventoryItem")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditFacilityInventoryItems")
    @Action(type = ActionType.SET, field = "activeSubMenu2Item", value = "ViewInventoryItemDetail")
    @Action(type = ActionType.SET, field = "inventoryItemId", fromField = "parameters.inventoryItemId")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "InventoryItemDetail", list = "inventoryItemDetails", conditions = {@ConditionExpr(fieldName = "inventoryItemId", operator = "equals", fromField = "inventoryItemId")}, orderBy = {"-inventoryItemDetailSeqId"})
    @DecoratorScreen(
        name = "CommonFacilityInventoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "InventoryItemTabBar", location = "component://product/widget/facility/FacilityMenus.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductInventoryDetails} ${uiLabelMap.CommonFor} [${inventoryItemId}]", includeForms = {
                    @IncludeForm(name = "ListInventoryItemDetail", location = "component://product/widget/facility/InventoryForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.ProductShipmentReceiptsFor} [${inventoryItemId}]", includeForms = {
                    @IncludeForm(name = "ViewInventoryItemShipmentReceipts", location = "component://product/widget/facility/InventoryForms.xml"
                )})})
        }
    )
    public interface ViewInventoryItemDetail {}

    @Screen(name = "EditInventoryItemLabels", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditInventoryItemLabels")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditFacilityInventoryItems")
    @Action(type = ActionType.SET, field = "activeSubMenu2Item", value = "EditInventoryItemLabels")
    @Action(type = ActionType.SET, field = "inventoryItemId", fromField = "parameters.inventoryItemId")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "InventoryItemLabelAppl", list = "inventoryItemLabelAppls", conditions = {@ConditionExpr(fieldName = "inventoryItemId", operator = "equals", fromField = "inventoryItemId")}, orderBy = {"sequenceNum"})
    @DecoratorScreen(
        name = "CommonFacilityInventoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "InventoryItemTabBar", location = "component://product/widget/facility/FacilityMenus.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "UpdateInventoryItemLabelAppls", location = "component://product/widget/facility/InventoryForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductInventoryItemLabels} ${uiLabelMap.CommonFor} [${inventoryItemId}]", name = "AddInventoryItemLabelPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddInventoryItemLabelAppl", location = "component://product/widget/facility/InventoryForms.xml"
                )}, position = 1)})
        }
    )
    public interface EditInventoryItemLabels {}

    @Screen(name = "FindFacilityPhysicalInventory", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PhysicalInventory")
    @Action(type = ActionType.SET, field = "facilityId", value = "${parameters.facilityId}")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/inventory/FindFacilityPhysicalInventory.groovy")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ProductPhysicalInventory} ${facilityId}")
    @DecoratorScreen(
        name = "CommonFacilityInventoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ScipioFacilityPhysicalInventoryList", location = "component://product/widget/facility/FacilityScreens.xml"
            )}, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "FindPhysicalInventory", location = "component://product/widget/facility/FacilityForms.xml"
                )}, position = 0)})
        }
    )
    public interface FindFacilityPhysicalInventory {}

    @Screen(name = "ScipioFacilityPhysicalInventoryList", location = "component://product/widget/facility/FacilityScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/inventory/PhysicalInventoryList.ftl")}))
    public interface ScipioFacilityPhysicalInventoryList {}

    @Screen(name = "ReceiveInventory", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ReceiveInventory")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Facility", valueField = "facility")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ProductReceiveInventoryItems} ${facilityId}")
    @DecoratorScreen(
        name = "CommonFacilityInventoryDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}12 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "ScipioReceiveInventoryDetail", location = "component://product/widget/facility/FacilityScreens.xml", position = 1
                    )}, htmlTemplates = {
                        @HtmlTemplate(location = "component://product/webapp/facility/inventory/ReceiveInventory.ftl", position = 0
                    )})}),
                    @Container(style = "${styles.grid_row}", containers = {
                        @Container2(style = "${styles.grid_large}12 ${styles.grid_cell}", includeScreens = {
                            @IncludeScreen(name = "ScipioReceiveReturnDetail", location = "component://product/widget/facility/FacilityScreens.xml", position = 1
                        )}, htmlTemplates = {
                            @HtmlTemplate(location = "component://product/webapp/facility/returns/ReceiveReturn.ftl", position = 0
                        )})})})
        }
    )
    public interface ReceiveInventory {}

    @Screen(name = "ScipioReceiveInventoryDetail", location = "component://product/widget/facility/FacilityScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"parameters.returnId"})}))
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/inventory/ReceiveInventory.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/inventory/ReceiveInventoryDetail.ftl")}))
    public interface ScipioReceiveInventoryDetail {}

    @Screen(name = "ScipioReceiveReturnDetail", location = "component://product/widget/facility/FacilityScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"parameters.productId"}), @Condition(type = Empty.class, params = {"parameters.purchaseOrderId"})}))
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/returns/ReceiveReturn.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/returns/ReceiveReturnDetail.ftl")}))
    public interface ScipioReceiveReturnDetail {}

    @Screen(name = "UpdatedInventoryItemStatus", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/returns/UpdatedInventoryItemStatus.ftl")}))
    public interface UpdatedInventoryItemStatus {}

    @Screen(name = "PicklistOptions", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitlePickListOptions")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PicklistOptions")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.SET, field = "groupByShippingMethod", fromField = "parameters.groupByShippingMethod")
    @Action(type = ActionType.SET, field = "groupByWarehouseArea", fromField = "parameters.groupByWarehouseArea")
    @Action(type = ActionType.SET, field = "groupByNoOfOrderItems", fromField = "parameters.groupByNoOfOrderItems")
    @Action(type = ActionType.SET, field = "maxNumberOfOrders", fromField = "parameters.maxNumberOfOrders", defaultValue = "50")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"facilityId"})}), actions = @Actions(value = {@Action(type = ActionType.SERVICE, serviceName = "findOrdersToPickMove", fieldMaps = {@FieldMap(fieldName = "facilityId"), @FieldMap(fieldName = "groupByShippingMethod"), @FieldMap(fieldName = "groupByWarehouseArea"), @FieldMap(fieldName = "groupByNoOfOrderItems")}), @Action(type = ActionType.ENTITY_CONDITION, entityName = "Picklist", list = "picklistActiveList", conditions = {@ConditionExpr(fieldName = "facilityId", operator = "equals", fromField = "parameters.facilityId"), @ConditionExpr(fieldName = "statusId", operator = "not-equals", value = "PICKLIST_PICKED"), @ConditionExpr(fieldName = "statusId", operator = "not-equals", value = "PICKLIST_PACKED"), @ConditionExpr(fieldName = "statusId", operator = "not-equals", value = "PICKLIST_CANCELLED")}, orderBy = {"picklistDate"})}), widgets = @Widgets(decorator = @DecoratorScreen(
        name = "CommonFacilityPickingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/facility/PicklistOptions.ftl"
            )})
        }
    )), failWidgets = @Widgets(decorator = @DecoratorScreen(
        name = "CommonFacilityPickingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/facility/PicklistOptions.ftl"
            )})
        }
    )))
    public interface PicklistOptions {}

    @Screen(name = "PrintPickSheets.fo", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.SET, field = "pickMoveInfoList", fromField = "parameters.pickMoveInfoList")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/CompanyHeader.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"pickMoveInfoList"})}), actions = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/PrintPickSheets.groovy")}), widgets = @Widgets(sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"orderHeaderList"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/facility/PrintPickSheets.fo.ftl", platform = "xsl-fo")}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/facility/NotReadyToPick.fo.ftl", platform = "xsl-fo")}))}), failWidgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/facility/NotReadyToPick.fo.ftl", platform = "xsl-fo")}))
    public interface PrintPickSheets_fo {}

    @Screen(name = "ReviewOrdersNotPickedOrPacked", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/ReviewOrdersNotPickedOrPacked.groovy")
    @DecoratorScreen(
        name = "CommonFacilityPickingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/facility/ReviewOrdersNotPickedOrPacked.ftl"
            )})
        }
    )
    public interface ReviewOrdersNotPickedOrPacked {}

    @Screen(name = "PicklistManage", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitlePickListOptions")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PicklistManage")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.viewIndex", defaultValue = "0")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.viewSize", defaultValue = "10")
    @Action(type = ActionType.SERVICE, serviceName = "getPicklistDisplayInfo", fieldMaps = {@FieldMap(fieldName = "facilityId")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PartyRoleAndPartyDetail", list = "partyRoleAndPartyDetailList", useCache = true, conditions = {@ConditionExpr(fieldName = "roleTypeId", value = "PICKER")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Picklist", list = "picklistActiveList", conditions = {@ConditionExpr(fieldName = "facilityId", operator = "equals", fromField = "parameters.facilityId"), @ConditionExpr(fieldName = "statusId", operator = "not-equals", value = "PICKLIST_PICKED"), @ConditionExpr(fieldName = "statusId", operator = "not-equals", value = "PICKLIST_PACKED"), @ConditionExpr(fieldName = "statusId", operator = "not-equals", value = "PICKLIST_CANCELLED")}, orderBy = {"picklistDate"})
    @DecoratorScreen(
        name = "CommonFacilityPickingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/facility/PicklistManage.ftl"
            )})
        }
    )
    public interface PicklistManage {}

    @Screen(name = "PickMoveStock", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitlePickingMoveStock")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PickMoveStock")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.SERVICE, serviceName = "findStockMovesNeeded", fieldMaps = {@FieldMap(fieldName = "facilityId")})
    @Action(type = ActionType.SET, field = "oiirWarningMessageList", fromField = "warningMessageList")
    @Action(type = ActionType.SERVICE, serviceName = "findStockMovesRecommended", fieldMaps = {@FieldMap(fieldName = "facilityId"), @FieldMap(fieldName = "stockMoveHandled")})
    @Action(type = ActionType.SET, field = "pflWarningMessageList", fromField = "warningMessageList")
    @DecoratorScreen(
        name = "CommonFacilityPickingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/facility/PickMoveStock.ftl"
            )})
        }
    )
    public interface PickMoveStock {}

    @Screen(name = "PickMoveStockSimple.fo", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductErrorUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companyName", fromField = "uiLabelMap.FacilityCompanyName", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companySubtitle", fromField = "uiLabelMap.FacilityCompanySubtitle", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitlePickingMoveStock")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.SERVICE, serviceName = "findStockMovesNeeded", fieldMaps = {@FieldMap(fieldName = "facilityId")})
    @Action(type = ActionType.SET, field = "oiirWarningMessageList", fromField = "warningMessageList")
    @Action(type = ActionType.SERVICE, serviceName = "findStockMovesRecommended", fieldMaps = {@FieldMap(fieldName = "facilityId"), @FieldMap(fieldName = "stockMoveHandled")})
    @Action(type = ActionType.SET, field = "pflWarningMessageList", fromField = "warningMessageList")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/facility/PickMoveStockSimple.fo.ftl", platform = "xsl-fo"
            )})
        }
    )
    public interface PickMoveStockSimple_fo {}

    @Screen(name = "PicklistReport.fo", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "picklistId", fromField = "parameters.picklistId")
    @Action(type = ActionType.SERVICE, serviceName = "getPickAndPackReportInfo", fieldMaps = {@FieldMap(fieldName = "picklistId")})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/facility/Picklist.fo.ftl", platform = "xsl-fo")}))
    public interface PicklistReport_fo {}

    @Screen(name = "ScheduleShipmentRouteSegment", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitlePackageShipmentScheduling")
    @Action(type = ActionType.SET, field = "activeScheduleSubMenuItem", value = "ScheduleTabButton")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ShipmentRouteSegmentDetail", list = "shipmentRouteSegments", conditions = {@ConditionExpr(fieldName = "originFacilityId", operator = "equals", fromField = "parameters.facilityId"), @ConditionExpr(fieldName = "statusId", operator = "equals", value = "SHIPMENT_PACKED"), @ConditionExpr(fieldName = "carrierServiceStatusId", operator = "equals", value = "SHRSCS_NOT_STARTED")}, orderBy = {"shipmentId DESC"})
    @Action(type = ActionType.ENTITY_ONE, entityName = "Facility", valueField = "facility")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Uom", valueField = "defaultWeightUom", autoFieldMap = false, useCache = true, fieldMaps = {@FieldMap(fieldName = "uomId", fromField = "facility.defaultWeightUomId")})
    @DecoratorScreen(
        name = "CommonFacilityScheduleDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "SchedulingList", location = "component://product/widget/facility/FacilityForms.xml"
                )})})
        }
    )
    public interface ScheduleShipmentRouteSegment {}

    @Screen(name = "Labels", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleLabelPrinting")
    @Action(type = ActionType.SET, field = "activeScheduleSubMenuItem", value = "LabelsTabButton")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ShipmentPackageRouteDetail", list = "shipmentPackageRouteSegments", conditions = {@ConditionExpr(fieldName = "labelPrinted", operator = "not-equals", value = "Y"), @ConditionExpr(fieldName = "carrierServiceStatusId", operator = "equals", value = "SHRSCS_CONFIRMED")})
    @DecoratorScreen(
        name = "CommonFacilityScheduleDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "Labels", location = "component://product/widget/facility/FacilityForms.xml"
                )}, htmlTemplates = {
                    @HtmlTemplate(location = "component://product/webapp/facility/facility/batchPrintMarkAsAccepted.ftl"
                )})})
        }
    )
    public interface Labels {}

    @Screen(name = "BatchPrintShippingLabels", location = "component://product/widget/facility/FacilityScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/facility/batchPrintShippingLabels.fo.ftl", platform = "xsl-fo")}))
    public interface BatchPrintShippingLabels {}

    @Screen(name = "FacilityLocationGeoLocation", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFacilityLocationGeoLocation")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/facility/FacilityLocationGeoLocation.groovy")
    @DecoratorScreen(
        name = "CommonFacilitySettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonLatitude} ${latestGeoPoint.latitude}"
            ),
            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonLongitude} ${latestGeoPoint.longitude}"
            ),
            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonElevation} ${latestGeoPoint.elevation} ${elevationUomAbbr}"
            ),
            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "geoChart", location = "component://common/widget/CommonScreens.xml"
            )})
        }
    )
    public interface FacilityLocationGeoLocation {}

    @Screen(name = "EditFacilityGeoPoint", location = "component://product/widget/facility/FacilityScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditFacilityGeoPoint")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Facility", valueField = "facility")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "facility", relationName = "GeoPoint", toValueField = "geoPoint")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "geoPoint", relationName = "ElevationUom", toValueField = "uom")
    @Action(type = ActionType.SET, field = "geoPoints[+0].lat", fromField = "geoPoint.latitude")
    @Action(type = ActionType.SET, field = "geoPoints[0].lon", fromField = "geoPoint.longitude")
    @Action(type = ActionType.SET, field = "geoChart.dataSourceId", fromField = "geoPoint.dataSourceId")
    @Action(type = ActionType.SET, field = "geoChart.width", value = "600px")
    @Action(type = ActionType.SET, field = "geoChart.height", value = "500px")
    @Action(type = ActionType.SET, field = "geoChart.points", fromField = "geoPoints")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.CommonGeoLocation} ${facilityId}")
    @DecoratorScreen(
        name = "CommonFacilitySettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "geoChart", location = "component://common/widget/CommonScreens.xml"
            )}, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditFacilityGeoPoint", location = "component://product/widget/facility/FacilityForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditFacilityGeoPoint {}

}
