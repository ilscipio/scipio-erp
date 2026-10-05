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
package com.ilscipio.scipio.commonext.widget;

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
public class OfbizsetupSetupScreens {

    @Screen(name = "InitialSetup", location = "component://commonext/widget/ofbizsetup/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItemTop", value = "organization")
    @Action(type = ActionType.SET, field = "titleProperty", value = "SetupCreateNewOrganization")
    @Action(type = ActionType.SET, field = "target", value = "createOrganization")
    @Action(type = ActionType.SET, field = "previousParams", fromField = "_PREVIOUS_PARAMS_", fromScope = "user")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCountryGeoId", resource = "general", property = "country.geo.id.default", defaultValue = "USA")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PartyRole", list = "parties", conditions = {@ConditionExpr(fieldName = "roleTypeId", operator = "equals", value = "INTERNAL_ORGANIZATIO")})
    @Action(type = ActionType.SET, field = "partyId", fromField = "parties[0].partyId")
    @Action(type = ActionType.SET, field = "parameters.partyId", fromField = "parties[0].partyId")
    @DecoratorScreen(
        name = "CommonSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Empty.class, params = {"parties"})}), widgets = @InlineWidgets(sections = {
                        @SectionNested(actions = @Actions(value = {
                            @Action(type = ActionType.SET, field = "dependentForm", value = "NewOrganization"
                        ),
                        @Action(type = ActionType.SET, field = "paramKey", value = "countryGeoId"
                    ),
                    @Action(type = ActionType.SET, field = "mainId", value = "USER_COUNTRY"
                ),
                @Action(type = ActionType.SET, field = "dependentId", value = "USER_STATE"
            ),
            @Action(type = ActionType.SET, field = "requestName", value = "getAssociatedStateList"
            ),
            @Action(type = ActionType.SET, field = "responseName", value = "stateList"
            ),
            @Action(type = ActionType.SET, field = "dependentKeyName", value = "geoId"
            ),
            @Action(type = ActionType.SET, field = "descName", value = "geoName"
            ),
            @Action(type = ActionType.SET, field = "selectedDependentOption", value = "_none_"
            )}), widgets = @WidgetsForContainer(value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/setDependentDropdownValuesJs.ftl"
            )}, screenlets = {
                @ScreenletNested(includeForms = {
                    @IncludeForm(name = "NewOrganization", location = "component://commonext/widget/ofbizsetup/SetupForms.xml"
                
            )})}))}), failWidgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "viewprofile", location = "component://commonext/widget/ofbizsetup/ProfileScreens.xml"
            )}))})
        }
    )
    public interface InitialSetup {}

    @Screen(name = "EditFacility", location = "component://commonext/widget/ofbizsetup/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductNewFacility")
    @Action(type = ActionType.SET, field = "activeSubMenuItemTop", value = "facility")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SCRIPT, location = "component://commonext/webapp/ofbizsetup/WEB-INF/actions/FindFacility.groovy")
    @DecoratorScreen(
        name = "CommonSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"facilityGenericPermission", "VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_FORM, name = "EditFacility", location = "component://commonext/widget/ofbizsetup/SetupForms.xml", position = 1
                )}, sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"facility"})}), widgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductEditFacility} ${facility.facilityName} [${facility.facilityId}]", style = "heading"
                        )}), failWidgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductNewFacility}", style = "heading"
                        )}), position = 0)}), failWidgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductFacilityViewPermissionError}", style = "common-msg-error-perm"
                        )}))})
        }
    )
    public interface EditFacility {}

    @Screen(name = "EditProductStore", location = "component://commonext/widget/ofbizsetup/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductStore")
    @Action(type = ActionType.SET, field = "activeSubMenuItemTop", value = "productstore")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditProductStore")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyGroup", valueField = "partyGroup")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @Action(type = ActionType.SCRIPT, location = "component://commonext/webapp/ofbizsetup/WEB-INF/actions/GetProductStoreAndWebSite.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://commonext/webapp/ofbizsetup/WEB-INF/actions/FindFacility.groovy")
    @DecoratorScreen(
        name = "CommonSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"showScreen", "equals", "origin"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(title = "${uiLabelMap.SetupEditProductStore}", includeForms = {
                        @IncludeForm(name = "EditProductStore", location = "component://commonext/widget/ofbizsetup/SetupForms.xml"
                    )}, position = 1)}, sections = {
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                            @Condition(type = Empty.class, params = {"productStoreId"})}), widgets = @WidgetsForContainer(value = {
                                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PageTitleEditProductStore} ${uiLabelMap.CommonFor}: ${productStore.storeName} [${productStoreId}]", style = "heading"
                            )}), position = 0)}), failWidgets = @InlineWidgets(sections = {
                                @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                    @Condition(type = Compare.class, params = {"showScreen", "equals", "message"
                                })}), widgets = @WidgetsForContainer(value = {
                                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.SetupPageError}", style = "common-msg-error"
                                )}))}))})
        }
    )
    public interface EditProductStore {}

    @Screen(name = "EditWebSite", location = "component://commonext/widget/ofbizsetup/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditWebSite")
    @Action(type = ActionType.SET, field = "activeSubMenuItemTop", value = "website")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditWebSite")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SCRIPT, location = "component://commonext/webapp/ofbizsetup/WEB-INF/actions/GetProductStoreAndWebSite.groovy")
    @DecoratorScreen(
        name = "CommonSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"showScreen", "equals", "origin"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(includeForms = {
                        @IncludeForm(name = "EditWebSite", location = "component://commonext/widget/ofbizsetup/SetupForms.xml"
                    )}, position = 1)}, sections = {
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                            @Condition(type = Empty.class, params = {"webSite"})}), widgets = @WidgetsForContainer(value = {
                                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PageTitleEditWebSite} ${uiLabelMap.CommonFor}: ${webSite.siteName} [${webSite.webSiteId}]", style = "heading"
                            )}), position = 0)}), failWidgets = @InlineWidgets(sections = {
                                @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                    @Condition(type = Compare.class, params = {"showScreen", "equals", "message"
                                })}), widgets = @WidgetsForContainer(value = {
                                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.SetupPageError}", style = "common-msg-error"
                                )}))}))})
        }
    )
    public interface EditWebSite {}

    @Screen(name = "EditProdCatalog", location = "component://commonext/widget/ofbizsetup/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductCatalog")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "productcatalog")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SCRIPT, location = "component://commonext/webapp/ofbizsetup/WEB-INF/actions/GetProdCatalog.groovy")
    @DecoratorScreen(
        name = "CommonFirstProductDecorator",
        location = "component://commonext/widget/ofbizsetup/SetupScreens.xml",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"showScreen", "equals", "origin"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductCatalog} ${uiLabelMap.CommonFor} \"${prodCatalog.catalogName}\" [${prodCatalogId}]", style = "heading"
                )}, screenlets = {
                    @Screenlet(includeForms = {
                        @IncludeForm(name = "EditProdCatalog", location = "component://commonext/widget/ofbizsetup/SetupForms.xml"
                    )})}))})
        }
    )
    public interface EditProdCatalog {}

    @Screen(name = "EditCategory", location = "component://commonext/widget/ofbizsetup/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductCategories")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "productcategory")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductCategory")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SCRIPT, location = "component://commonext/webapp/ofbizsetup/WEB-INF/actions/GetProdCatalog.groovy")
    @DecoratorScreen(
        name = "CommonFirstProductDecorator",
        location = "component://commonext/widget/ofbizsetup/SetupScreens.xml",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"showErrorMsg", "equals", "N"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap[labelTitleProperty]} ${uiLabelMap.CommonFor}: ${productCategory.description} [${productCategoryId}]  ${${extraFunctionName}}", style = "heading"
                )}, screenlets = {
                    @Screenlet(title = "${uiLabelMap.PageTitleEditProductCategory}", name = "CreateProductCategory", includeForms = {
                        @IncludeForm(name = "EditProductCategory", location = "component://commonext/widget/ofbizsetup/SetupForms.xml"
                    )})}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.SetupPageError}", style = "common-msg-error"
                    )}))})
        }
    )
    public interface EditCategory {}

    @Screen(name = "EditProduct", location = "component://commonext/widget/ofbizsetup/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProduct")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "product")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductProduct")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SCRIPT, location = "component://commonext/webapp/ofbizsetup/WEB-INF/actions/GetProdCatalog.groovy")
    @DecoratorScreen(
        name = "CommonFirstProductDecorator",
        location = "component://commonext/widget/ofbizsetup/SetupScreens.xml",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"showErrorMsg", "equals", "N"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_FORM, name = "EditProduct", location = "component://commonext/widget/ofbizsetup/SetupForms.xml", position = 1
                )}, containers = {
                    @Container(labels = {
                        @Label(text = "${uiLabelMap[labelTitleProperty]} ${uiLabelMap.CommonFor}: ${product.internalName} [${productId}]  ${${extraFunctionName}}", style = "heading"
                    )}, position = 0)}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.SetupPageError}", style = "common-msg-error"
                    )}))})
        }
    )
    public interface EditProduct {}

    @Screen(name = "CommonFirstProductDecorator", location = "component://commonext/widget/ofbizsetup/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItemTop", fromField = "activeSubMenuItemTop", defaultValue = "firstproduct")
    @DecoratorScreen(
        name = "CommonSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Or.class, tree = {
                        @ConditionNode(type = And.class),
                        @ConditionNode(parent = 0, not = true, type = Empty.class, params = {"showScreen"
                    }),
                    @ConditionNode(parent = 0, type = Compare.class, params = {"showScreen", "equals", "origin"
                }),
                @ConditionNode(not = true, type = Empty.class, params = {"showErrorMsg"
            })})}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "FirstProductTabBar", location = "component://commonext/widget/ofbizsetup/Menus.xml"
            )}), position = 0)})
        }
    )
    public interface CommonFirstProductDecorator {}

    @Screen(name = "nopartyAcctgPreference", location = "component://commonext/widget/ofbizsetup/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "SetupCreateNewOrganization")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "init")
    @DecoratorScreen(
        name = "CommonSetupAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.SetupEventMessage}", style = "errorMessage"
            )})
        }
    )
    public interface nopartyAcctgPreference {}

}
