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
package com.ilscipio.scipio.setup.widget;

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
public class SetupScreens {

    @Screen(name = "InitialSetup", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "organization")
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
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "viewprofile", location = "component://setup/widget/ProfileScreens.xml"
            )}))})
        }
    )
    public interface InitialSetup {}

    @Screen(name = "EditFacility", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductNewFacility")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "facility")
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
                    @Widget(type = WidgetType.INCLUDE_FORM, name = "EditFacility", location = "component://setup/widget/SetupForms.xml", position = 1
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

    @Screen(name = "EditProductStore", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductStore")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "productstore")
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
                        @IncludeForm(name = "EditProductStore", location = "component://setup/widget/SetupForms.xml"
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

    @Screen(name = "EditWebSite", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditWebSite")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "website")
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
                        @IncludeForm(name = "EditWebSite", location = "component://setup/widget/SetupForms.xml"
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

    @Screen(name = "EditProdCatalog", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductCatalog")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "productcatalog")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SCRIPT, location = "component://commonext/webapp/ofbizsetup/WEB-INF/actions/GetProdCatalog.groovy")
    @DecoratorScreen(
        name = "CommonFirstProductDecorator",
        location = "component://setup/widget/SetupScreens.xml",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"showScreen", "equals", "origin"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductCatalog} ${uiLabelMap.CommonFor} \"${prodCatalog.catalogName}\" [${prodCatalogId}]", style = "heading"
                )}, screenlets = {
                    @Screenlet(includeForms = {
                        @IncludeForm(name = "EditProdCatalog", location = "component://setup/widget/SetupForms.xml"
                    )})}))})
        }
    )
    public interface EditProdCatalog {}

    @Screen(name = "EditCategory", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductCategories")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "productcategory")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductCategory")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SCRIPT, location = "component://commonext/webapp/ofbizsetup/WEB-INF/actions/GetProdCatalog.groovy")
    @DecoratorScreen(
        name = "CommonFirstProductDecorator",
        location = "component://setup/widget/SetupScreens.xml",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"showErrorMsg", "equals", "N"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap[labelTitleProperty]} ${uiLabelMap.CommonFor}: ${productCategory.description} [${productCategoryId}]  ${${extraFunctionName}}", style = "heading"
                )}, screenlets = {
                    @Screenlet(title = "${uiLabelMap.PageTitleEditProductCategory}", name = "CreateProductCategory", includeForms = {
                        @IncludeForm(name = "EditProductCategory", location = "component://setup/widget/SetupForms.xml"
                    )})}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.SetupPageError}", style = "common-msg-error"
                    )}))})
        }
    )
    public interface EditCategory {}

    @Screen(name = "EditProduct", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProduct")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "product")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductProduct")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SCRIPT, location = "component://commonext/webapp/ofbizsetup/WEB-INF/actions/GetProdCatalog.groovy")
    @DecoratorScreen(
        name = "CommonFirstProductDecorator",
        location = "component://setup/widget/SetupScreens.xml",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"showErrorMsg", "equals", "N"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_FORM, name = "EditProduct", location = "component://setup/widget/SetupForms.xml", position = 1
                )}, containers = {
                    @Container(labels = {
                        @Label(text = "${uiLabelMap[labelTitleProperty]} ${uiLabelMap.CommonFor}: ${product.internalName} [${productId}]  ${${extraFunctionName}}", style = "heading"
                    )}, position = 0)}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.SetupPageError}", style = "common-msg-error"
                    )}))})
        }
    )
    public interface EditProduct {}

    @Screen(name = "CommonFirstProductDecorator", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "firstproduct")
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
                @Widget(type = WidgetType.INCLUDE_MENU, name = "FirstProductTabBar", location = "component://setup/widget/Menus.xml"
            )}), position = 0)})
        }
    )
    public interface CommonFirstProductDecorator {}

    @Screen(name = "nopartyAcctgPreference", location = "component://setup/widget/SetupScreens.xml")
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

    @Screen(name = "SetupOrganization", location = "component://setup/widget/SetupScreens.xml")
    @Action(order = 0, type = ActionType.SET, field = "setupStep", value = "organization")
    @Action(order = 1, type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/SetupWizardCommonActions.groovy")
    @Action(order = 2, type = ActionType.SET, field = "organizationData", fromField = "setupStepStates[setupStep].stepData")
    @Action(order = 3, type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/organization/SetupOrganization.groovy")
    @Action(order = 4, type = ActionType.SET, field = "isCreateForm", value = "${context.party == null}", valueType = "Boolean")
    @Action(order = 5, type = ActionType.SET, field = "titleProperty", value = "${groovy: !isCreateForm ? 'SetupEditOrganizationInformation' : 'SetupCreateNewOrganization'}")
    @Action(order = 6, type = ActionType.SET, field = "target", value = "${groovy: !isCreateForm ? 'setupUpdateOrganization' : 'setupCreateOrganization'}")
    @Action(order = 7, type = ActionType.SET, field = "submitFormId", value = "EditOrganization")
    @IfAction(order = 8, condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = True.class, params = {"organizationSelected"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "useDefaultSetupSubmitBar", value = "false", valueType = "Boolean")}))
    @DecoratorScreen(
        name = "CommonSetupWizardDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"PARTYMGR", "_CREATE"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(includeScreens = {
                        @IncludeScreen(name = "SelectOrganization", location = "component://setup/widget/SetupScreens.xml"
                    )}, position = 0)}, sections = {
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = True.class, params = {"organizationSelected"
                        })}), widgets = @WidgetsForContainer(screenlets = {
                            @ScreenletNested(htmlTemplates = {
                    @HtmlTemplate(location = "component://setup/webapp/setup/organization/EditOrganization.ftl"
                
                        )})}), position = 1)}), failWidgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrCreatePermissionError}", style = "common-msg-error-perm"
                        )}))}),
            @DecoratorSection(name = "extra-body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"PARTYMGR", "_CREATE"
                })}), widgets = @InlineWidgets(sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"party"})}), actions = @Actions(value = {
                            @Action(type = ActionType.SET, field = "partyInfoViewOnly", value = "false", valueType = "Boolean"
                        ),
                        @Action(type = ActionType.SET, field = "partyInfoSimpleFuncOnly", value = "true", valueType = "Boolean"
                    ),
                    @Action(type = ActionType.SET, field = "partyContactInfoUseSection", value = "false", valueType = "Boolean"
                )}), widgets = @WidgetsForContainer(value = {
                    @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/organization/OrganizationOverview.ftl"
                )}))}))})
        }
    )
    public interface SetupOrganization {}

    @Screen(name = "SetupOrganizationMain", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/generated/SetupOrganizationMain_script1.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = True.class, params = {"setStepSuccess"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "SetupOrganization")}), failWidgets = @Widgets(decorator = @DecoratorScreen(
        name = "CommonSetupWizardDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonErrorOccurredContactSupport}", style = "common-msg-error"
            )})
        }
    )))
    public interface SetupOrganizationMain {}

    @Screen(name = "SelectOrganization", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PartyRole", list = "parties", conditions = {@ConditionExpr(fieldName = "roleTypeId", operator = "equals", value = "INTERNAL_ORGANIZATIO")})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/organization/SelectOrganization.ftl")}))
    public interface SelectOrganization {}

    @Screen(name = "SetupUser", location = "component://setup/widget/SetupScreens.xml")
    @Action(order = 0, type = ActionType.SET, field = "setupStep", value = "user")
    @Action(order = 1, type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/SetupWizardCommonActions.groovy")
    @Action(order = 2, type = ActionType.SET, field = "userData", fromField = "setupStepStates[setupStep].stepData")
    @Action(order = 3, type = ActionType.SET, field = "storeData", fromField = "setupStepStates.store.stepData")
    @Action(order = 4, type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/user/SetupUser.groovy")
    @Action(order = 5, type = ActionType.SET, field = "isCreateForm", value = "${context.userParty == null}", valueType = "Boolean")
    @Action(order = 6, type = ActionType.SET, field = "titleProperty", value = "${groovy: !isCreateForm ? 'PartyChangeParty' : 'PartyNewUser'}")
    @Action(order = 7, type = ActionType.SET, field = "target", value = "${groovy: !isCreateForm ? 'setupUpdateUser' : 'setupCreateUser'}")
    @Action(order = 8, type = ActionType.SET, field = "submitFormId", value = "EditUser")
    @IfAction(order = 9, condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = True.class, params = {"userSelected"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "useDefaultSetupSubmitBar", value = "false", valueType = "Boolean")}))
    @DecoratorScreen(
        name = "CommonSetupWizardDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"PARTYMGR", "_CREATE"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(includeScreens = {
                        @IncludeScreen(name = "SelectUser", location = "component://setup/widget/SetupScreens.xml"
                    )}, position = 0)}, sections = {
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = True.class, params = {"userSelected"})}), widgets = @WidgetsForContainer(screenlets = {
                                @ScreenletNested(htmlTemplates = {
                                    @HtmlTemplate(location = "component://setup/webapp/setup/user/EditUser.ftl"
                                )})}), position = 1)}), failWidgets = @InlineWidgets(value = {
                                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrCreatePermissionError}", style = "common-msg-error-perm"
                                )}))}),
            @DecoratorSection(name = "extra-body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"PARTYMGR", "_CREATE"
                })}), widgets = @InlineWidgets(sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"userParty"})}), actions = @Actions(value = {
                            @Action(type = ActionType.SET, field = "partyInfoViewOnly", value = "false", valueType = "Boolean"
                        ),
                        @Action(type = ActionType.SET, field = "partyInfoSimpleFuncOnly", value = "true", valueType = "Boolean"
                    ),
                    @Action(type = ActionType.SET, field = "partyContactInfoUseSection", value = "false", valueType = "Boolean"
                ),
                @Action(type = ActionType.SET, field = "parameters.partyId", fromField = "userPartyId"
            )}), widgets = @WidgetsForContainer(value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/user/UserOverview.ftl"
            )}))}))})
        }
    )
    public interface SetupUser {}

    @Screen(name = "SelectUser", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PartyRelationship", list = "parties", distinct = true, conditions = {@ConditionExpr(fieldName = "partyIdFrom", operator = "equals", fromField = "parameters.orgPartyId"), @ConditionExpr(fieldName = "roleTypeIdFrom", operator = "equals", value = "INTERNAL_ORGANIZATIO")}, selectFields = {"partyIdTo"})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/user/SelectUser.ftl")}))
    public interface SelectUser {}

    @Screen(name = "SetupAccounting", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "setupStep", value = "accounting")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/SetupWizardCommonActions.groovy")
    @Action(type = ActionType.SET, field = "accountingData", fromField = "setupStepStates[setupStep].stepData")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/accounting/SetupAccounting.groovy")
    @Action(type = ActionType.SET, field = "isCreateForm", value = "false", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "target", value = "setupUpdateGlAccount")
    @Action(type = ActionType.SET, field = "submitFormId", value = "setupAccounting-preferences-form")
    @Action(type = ActionType.SET, field = "useDefaultSetupSubmitBar", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/generated/SetupAccounting_script1.groovy")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/accounting/control/ScpEgltCommon.js?t=${ScpEgltCommon}", global = true)
    @DecoratorScreen(
        name = "CommonSetupWizardDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"ACCOUNTING", "_CREATE"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(includeScreens = {
                        @IncludeScreen(name = "SelectGL", location = "component://setup/widget/SetupScreens.xml"
                    )}),
                    @Screenlet(htmlTemplates = {
                        @HtmlTemplate(location = "component://setup/webapp/setup/accounting/EditAccounting.ftl"
                    )})}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingCreatePermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface SetupAccounting {}

    @Screen(name = "SelectGL", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/accounting/SelectGL.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/accounting/SelectGL.ftl")}))
    public interface SelectGL {}

    @Screen(name = "EditAcctgPreferences", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/accounting/EditAcctgPreferences.groovy")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://setup/webapp/setup/accounting/EditAcctgPreferences.ftl")})}))
    public interface EditAcctgPreferences {}

    @Screen(name = "EditGLAccountTree", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "CommonSetupAccountingTabsAction", location = "component://setup/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "setupGlAccountForms.location", value = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "setupGlAccountForms.name", value = "SetupGlAccountForms")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = NotEmpty.class, params = {"topGlAccountId"}), @Condition(type = NotEmpty.class, params = {"treeMenuData"})}), widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://setup/webapp/setup/accounting/EditGLAccountTree.ftl")})}))
    public interface EditGLAccountTree {}

    @Screen(name = "SetupGlAccountForms", location = "component://setup/widget/SetupScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/accounting/EditGLAccount.ftl")}))
    public interface SetupGlAccountForms {}

    @Screen(name = "EditFiscalPeriods", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "CommonSetupAccountingTabsAction", location = "component://setup/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/accounting/EditFiscalPeriods.groovy")
    @Action(type = ActionType.SET, field = "setupTimePeriodForms.location", value = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "setupTimePeriodForms.name", value = "SetupTimePeriodForms")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/accounting/EditFiscalPeriods.ftl")}))
    public interface EditFiscalPeriods {}

    @Screen(name = "SetupTimePeriodForms", location = "component://setup/widget/SetupScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/accounting/EditFiscalPeriod.ftl")}))
    public interface SetupTimePeriodForms {}

    @Screen(name = "EditJournals", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "CommonSetupAccountingTabsAction", location = "component://setup/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/accounting/EditJournals.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/accounting/EditJournals.ftl")}))
    public interface EditJournals {}

    @Screen(name = "EditAccountingTransactions", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "CommonSetupAccountingTabsAction", location = "component://setup/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/accounting/EditAccountingTransactions.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/accounting/EditAccountingTransactions.ftl")}))
    public interface EditAccountingTransactions {}

    @Screen(name = "EditTaxAuthorities", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "CommonSetupAccountingTabsAction", location = "component://setup/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/accounting/EditTaxAuthorities.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/accounting/EditTaxAuthorities.ftl")}))
    public interface EditTaxAuthorities {}

    @Screen(name = "SetupAccountingTabError", location = "component://setup/widget/SetupScreens.xml")
    @DecoratorScreen(
        name = "CommonSetupAccountingTabsDecorator",
        location = "component://setup/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/accounting/SetupAccountingTabError.ftl"
            )})
        }
    )
    public interface SetupAccountingTabError {}

    @Screen(name = "SetupFacility", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "setupStep", value = "facility")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/SetupWizardCommonActions.groovy")
    @Action(type = ActionType.SET, field = "facilityData", fromField = "setupStepStates[setupStep].stepData")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/facility/SetupFacility.groovy")
    @Action(type = ActionType.SET, field = "isCreateForm", value = "${context.facility == null}", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: !isCreateForm ? 'ProductEditFacility' : 'ProductNewFacility'}")
    @Action(type = ActionType.SET, field = "target", value = "${groovy: !isCreateForm ? 'setupUpdateFacility' : 'setupCreateFacility'}")
    @Action(type = ActionType.SET, field = "submitFormId", value = "EditFacility")
    @DecoratorScreen(
        name = "CommonSetupWizardDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"facilityGenericPermission", "CREATE"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(htmlTemplates = {
                        @HtmlTemplate(location = "component://setup/webapp/setup/facility/EditFacility.ftl"
                    )})}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductFacilityCreatePermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface SetupFacility {}

    @Screen(name = "SetupCatalog", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "setupStep", value = "catalog")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/SetupWizardCommonActions.groovy")
    @Action(type = ActionType.SET, field = "catalogData", fromField = "setupStepStates[setupStep].stepData")
    @Action(type = ActionType.SET, field = "storeData", fromField = "setupStepStates.store.stepData")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/catalog/SetupCatalog.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/generated/SetupCatalog_script1.groovy")
    @Action(type = ActionType.SET, field = "newCatalogFormId", value = "setupCatalog-newCatalog")
    @Action(type = ActionType.SET, field = "newCatalogLinkHref", value = "javascript:jQuery('#${newCatalogFormId}').submit();void(0);")
    @Action(type = ActionType.SET, field = "setupCatalogForms.location", value = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "setupCatalogForms.name", value = "SetupCatalogForms")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/content/images/ScpContentCommon.js", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/generated/SetupCatalog_script2.groovy")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/catalog/ScpCatalogCommon.js?t=${ScpEgltCommon}", global = true)
    @DecoratorScreen(
        name = "CommonSetupWizardDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"CATALOG", "_CREATE"
                })}), widgets = @InlineWidgets(sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"productStoreCatalogList"
                    })}), widgets = @WidgetsForContainer(screenlets = {
                        @ScreenletNested(includeScreens = {
                    @IncludeScreen(name = "EditCatalogTree", location = "component://setup/widget/SetupScreens.xml"
                
                    )})}), failWidgets = @WidgetsForContainer(value = {
                        @Widget(type = WidgetType.INCLUDE_SCREEN, name = "${setupCatalogForms.name}", location = "${setupCatalogForms.location}"
                    )}))}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductCatalogCreatePermissionError}", style = "common-msg-error-perm"
                    )}))}),
            @DecoratorSection(name = "menu-functions", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, content = "<#include \"component://setup/webapp/setup/common/common.ftl\">\n                                  <@form method=\"get\" action=makePageUrl(\"setupCatalog\") id=newCatalogFormId>\n                                    <@defaultWizardFormFields exclude=[\"prodCatalogId\"]/>\n                                    <@field type=\"hidden\" name=\"setupContinue\" value=\"N\"/>\n                                    <@field type=\"hidden\" name=\"newCatalog\" value=\"Y\"/>\n                                  </@form>\n                                <#-- now part of catalog tree actions menu (less confusing)\n                                  <@menu type=\"button\">\n                                    <#if !isCreateForm>\n                                      <@menuitem type=\"link\" href=raw(newCatalogLinkHref) text=uiLabelMap.ProductNewCatalog class=\"+${styles.action_nav!} ${styles.action_add!}\"/>\n                                    </#if>\n                                  </@menu>\n                                -->"
            )}),
            @DecoratorSection(name = "extra-body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"CATALOG", "_CREATE"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/catalog/CatalogExtraLinks.ftl"
                )}))})
        }
    )
    public interface SetupCatalog {}

    @Screen(name = "SetupCatalogForms", location = "component://setup/widget/SetupScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/catalog/EditProdCatalog.ftl"), @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/catalog/EditProductCategory.ftl"), @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/catalog/EditProduct.ftl")}))
    public interface SetupCatalogForms {}

    @Screen(name = "EditCatalogTree", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/catalog/EditCatalogTree.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = NotEmpty.class, params = {"productStoreId"}), @Condition(type = NotEmpty.class, params = {"treeMenuData"})}), widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://setup/webapp/setup/catalog/EditCatalogTree.ftl")})}), failWidgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "${setupCatalogForms.name}", location = "${setupCatalogForms.location}")}))
    public interface EditCatalogTree {}

    @Screen(name = "ListCatalogs", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/generated/ListCatalogs_script1.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/catalog/ListCatalogs.ftl")}))
    public interface ListCatalogs {}

    @Screen(name = "SetupStore", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "setupStep", value = "store")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/SetupWizardCommonActions.groovy")
    @Action(type = ActionType.SET, field = "storeData", fromField = "setupStepStates[setupStep].stepData")
    @Action(type = ActionType.SET, field = "facilityData", fromField = "setupStepStates.facility.stepData")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/store/SetupStore.groovy")
    @Action(type = ActionType.SET, field = "websiteData", fromField = "storeData")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/store/SetupWebsite.groovy")
    @Action(type = ActionType.SET, field = "isCreateForm", value = "${context.productStore == null}", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: !isCreateForm ? 'PageTitleEditProductStore' : 'ProductNewProductStore'}")
    @Action(type = ActionType.SET, field = "target", value = "${groovy: !isCreateForm ? 'setupUpdateStore' : 'setupCreateStore'}")
    @Action(type = ActionType.SET, field = "submitFormId", value = "EditStore")
    @DecoratorScreen(
        name = "CommonSetupWizardDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"CATALOG", "_CREATE"
                })}), actions = @Actions(value = {
                    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/generated/_script1.groovy"
                )}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(htmlTemplates = {
                        @HtmlTemplate(location = "component://setup/webapp/setup/store/EditProductStore.ftl"
                    )}, sections = {
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = NotEmpty.class, params = {"webSiteCount"}),
                            @Condition(type = Compare.class, params = {"webSiteCount", "greater", "1"
                        })}), widgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/store/ListWebSites.ftl"
                        )}))})}), failWidgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductCatalogCreatePermissionError}", style = "common-msg-error-perm"
                        )}))})
        }
    )
    public interface SetupStore {}

    @Screen(name = "SetupFinished", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "setupStep", value = "finished")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/SetupWizardCommonActions.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/generated/SetupFinished_script1.groovy")
    @Action(type = ActionType.SET, field = "settingUpLabelProp", value = "${groovy: context.incompleteSteps ? 'SetupSetupEndReachedFor': 'SetupSetupCompleteFor'}")
    @DecoratorScreen(
        name = "CommonSetupWizardDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/wizard/setupfinished.ftl"
            )})
        }
    )
    public interface SetupFinished {}

    @Screen(name = "SetupError", location = "component://setup/widget/SetupScreens.xml")
    @Action(type = ActionType.SET, field = "setupStep", value = "error")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/SetupWizardCommonActions.groovy")
    @DecoratorScreen(
        name = "CommonSetupWizardDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "setupErrorMsg", value = "${uiLabelMap.SetupErrorOccurredInfo}"
                )}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_SCREEN, name = "SetupErrorMsg", location = "component://setup/widget/CommonScreens.xml"
                )}))})
        }
    )
    public interface SetupError {}

}
