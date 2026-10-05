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
package com.ilscipio.scipio.marketing.widget;

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
public class SfaCommonScreens {

    @Screen(name = "webapp-common-actions", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "MainSideBarMenu")
    @Action(type = ActionType.SET, field = "mainSideBarMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "mainComplexMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "menuCfg")
    public interface webapp_common_actions {}

    @Screen(name = "main-decorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "SecurityUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/partymgr/static/partymgr.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.styleSheets[]", value = "/partymgr/static/partymgr.css", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companyName", fromField = "uiLabelMap.SfaCompanyName", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companySubtitle", fromField = "uiLabelMap.MarketingCompanySubtitle", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuName", value = "SfaAppBar", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuLocation", value = "component://marketing/widget/sfa/SfaMenus.xml", global = true)
    @Action(type = ActionType.SET, field = "applicationTitle", value = "${uiLabelMap.SfaManagerApplication}", global = true)
    @Action(type = ActionType.SET, field = "parameters.parentPortalPageId", fromField = "parameters.parentPortalPageId", defaultValue = "SFA", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/GetParentPortalPageId.groovy")
    @Action(type = ActionType.SET, field = "menuCfg", fromField = "mainComplexMenuCfg")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "DeriveComplexSideBarMenuItems", location = "component://common/widget/CommonScreens.xml")
    @DecoratorScreen(
        name = "ApplicationDecorator",
        location = "component://commonext/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = EmptySection.class, params = {"left-column"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "left-column"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_SCREEN, name = "DefMainSideBarMenu", location = "${parameters.mainDecoratorLocation}"
                )}))}),
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface main_decorator {}

    @Screen(name = "CommonSfaAppDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSfaAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"MARKETING", "_VIEW"})}))
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", overrideByAutoInclude = true, value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonSfaAppSideBarMenu", location = "component://marketing/widget/sfa/CommonScreens.xml"
            )}),
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"commonSfaAppBasePermCond"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.MarketingViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonSfaAppDecorator {}

    @Screen(name = "CommonTrackingCodeDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://marketing/widget/TrackingCodeMenus.xml#TrackingCode")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "trackingCodeId", fromField = "parameters.trackingCodeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "TrackingCode", valueField = "trackingCode")
    @DecoratorScreen(
        name = "CommonSfaAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonTrackingCodeDecorator {}

    @Screen(name = "CommonMarketingDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://marketing/widget/MarketingMenus.xml#Marketing")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "marketingCampaignId", fromField = "parameters.marketingCampaignId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "MarketingCampaign", valueField = "marketingCampaign")
    @DecoratorScreen(
        name = "CommonSfaAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonMarketingDecorator {}

    @Screen(name = "CommonPromoTopDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://marketing/widget/MarketingMenus.xml#Marketing")
    @Action(type = ActionType.SET, field = "showMainExtendedBar", value = "true", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonSfaAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonPromoTopDecorator {}

    @Screen(name = "CommonPromoDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/catalog/CatalogMenus.xml#Promo")
    @Action(type = ActionType.SET, field = "showMainExtendedBar", value = "true", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonSfaAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"CATALOG", "_VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_MENU, name = "PromoSubTabBar", location = "component://product/widget/catalog/CatalogMenus.xml"
                ),
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}), failWidgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductCatalogViewPermissionError}", style = "common-msg-error-perm"
            )}))})
        }
    )
    public interface CommonPromoDecorator {}

    @Screen(name = "CommonSegmentGroupDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://marketing/widget/SegmentMenus.xml#SegmentGroup")
    @Action(type = ActionType.SET, field = "segmentGroupId", fromField = "parameters.segmentGroupId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "SegmentGroup", valueField = "segmentGroup")
    @DecoratorScreen(
        name = "CommonSfaAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonSegmentGroupDecorator {}

    @Screen(name = "CommonContactListDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @DecoratorScreen(
        name = "CommonContactListDecorator",
        location = "component://marketing/widget/CommonScreens.xml"
    )
    public interface CommonContactListDecorator {}

    @Screen(name = "main", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://marketing/webapp/marketing/WEB-INF/actions/generated/main_script1.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "main", location = "component://marketing/widget/CommonScreens.xml")}))
    public interface main {}

    @Screen(name = "leftbar", location = "component://marketing/widget/sfa/CommonScreens.xml")
    public interface leftbar {}

    @Screen(name = "rightbar", location = "component://marketing/widget/sfa/CommonScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"userLogin"})}))
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.SfaQuickAddContact}", name = "SfaQuickAddContactPanel", collapsible = true, includeForms = {@IncludeForm(name = "QuickAddContact", location = "component://marketing/widget/sfa/forms/ContactForms.xml")}), @Screenlet(title = "${uiLabelMap.SfaQuickAddLead}", name = "SfaQuickAddLeadPanel", collapsible = true, includeForms = {@IncludeForm(name = "QuickAddLead", location = "component://marketing/widget/sfa/forms/LeadForms.xml")})}))
    public interface rightbar {}

    @Screen(name = "CommonOpportunityDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://marketing/widget/SalesMenus.xml#Sales")
    @Action(type = ActionType.SET, field = "salesOpportunityId", fromField = "parameters.salesOpportunityId")
    @DecoratorScreen(
        name = "CommonSfaAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonOpportunityDecorator {}

    @Screen(name = "CommonPartyDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(order = 0, type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(order = 1, type = ActionType.SET, field = "partyTypeId", fromField = "parameters.partyTypeId")
    @Action(order = 2, type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party")
    @Action(order = 3, type = ActionType.ENTITY_ONE, entityName = "Person", valueField = "lookupPerson")
    @Action(order = 4, type = ActionType.ENTITY_ONE, entityName = "PartyGroup", valueField = "lookupGroup")
    @Action(order = 5, type = ActionType.SET, field = "accountDescription", value = "${groovy: session.getAttribute('accountDescription')}")
    @Action(order = 6, type = ActionType.SET, field = "contactDescription", value = "${groovy: session.getAttribute('contactDescription')}")
    @Action(order = 7, type = ActionType.SET, field = "leadDescription", value = "${groovy: session.getAttribute('leadDescription')}")
    @Action(order = 8, type = ActionType.SCRIPT, location = "component://marketing/webapp/marketing/WEB-INF/actions/generated/CommonPartyDecorator_script1.groovy")
    @IfAction(order = 9, condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifNotEmpty = {"leadDescription", "accountLeadDescription"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "Leads"), @Action(type = ActionType.SET, field = "currentCommonDecorator", value = "CommonLeadDecorator")}), elseIf = {@ElseIfBlock(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"contactDescription"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Contacts"), @Action(type = ActionType.SET, field = "currentCommonDecorator", value = "CommonContactDecorator")})), @ElseIfBlock(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"accountDescription"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Accounts"), @Action(type = ActionType.SET, field = "currentCommonDecorator", value = "CommonAccountDecorator")})), @ElseIfBlock(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"currentCommonDecorator"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Accounts"), @Action(type = ActionType.SET, field = "currentCommonDecorator", value = "CommonAccountDecorator")}))})
    @IfAction(order = 10, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"currentCommonDecorator"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "currentCommonDecorator", value = "CommonSfaAppDecorator")}))
    @DecoratorScreen(
        name = "${currentCommonDecorator}",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body", position = 1
                )}, sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = NotEmpty.class, params = {"party"}),
                        @Condition(type = Or.class, tree = {
                            @ConditionNode(not = true, type = Empty.class, params = {"lookupPerson"
                        }),
                        @ConditionNode(not = true, type = Empty.class, params = {"lookupGroup"
                    })})}), widgets = @WidgetsForContainer(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyTheProfileOf} ${lookupPerson.personalTitle} ${lookupPerson.firstName} ${lookupPerson.middleName} ${lookupPerson.lastName} ${lookupPerson.suffix} ${lookupGroup.groupName} [${partyId}]", style = "heading"
                    )}), position = 0)}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface CommonPartyDecorator {}

    @Screen(name = "ViewProfile", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewPartyProfile")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PartyTaxAuthInfos")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/partymgr/static/PartyProfileContent.js", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/ViewProfile.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/SetRoleVars.groovy")
    @Action(type = ActionType.SET, field = "parameters.partyId", fromField = "partyId")
    @Action(type = ActionType.SET, field = "parameters.partyGroupId", fromField = "partyGroupId")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"party"})}), widgets = @InlineWidgets(containers = {
                        @Container(style = "${styles.grid_row}", containers = {
                            @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                                @IncludeScreen(name = "Party", location = "component://party/widget/partymgr/ProfileScreens.xml"
                            ),
                            @IncludeScreen(name = "Contact", location = "component://party/widget/partymgr/ProfileScreens.xml"
                        ),
                        @IncludeScreen(name = "UserLogin", location = "component://party/widget/partymgr/ProfileScreens.xml"
                    ),
                    @IncludeScreen(name = "Visits", location = "component://party/widget/partymgr/ProfileScreens.xml"
                ),
                @IncludeScreen(name = "Subscriptions", location = "component://party/widget/partymgr/ProfileScreens.xml"
            ),
            @IncludeScreen(name = "Attributes", location = "component://party/widget/partymgr/ProfileScreens.xml"
            ),
            @IncludeScreen(name = "Content", location = "component://party/widget/partymgr/ProfileScreens.xml"
            ),
            @IncludeScreen(name = "ScipioListUserCommunications", location = "component://party/widget/partymgr/ProfileScreens.xml"
            )}),
            @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                @IncludeScreen(name = "FinAccounts", location = "component://party/widget/partymgr/ProfileScreens.xml"
            ),
            @IncludeScreen(name = "PaymentMethods", location = "component://party/widget/partymgr/ProfileScreens.xml"
            ),
            @IncludeScreen(name = "PartySalesOpportunities", location = "component://party/widget/partymgr/ProfileScreens.xml"
            ),
            @IncludeScreen(name = "LeadPartyDataSource", location = "component://marketing/widget/sfa/LeadScreens.xml"
            ),
            @IncludeScreen(name = "partyRelAccounts", location = "component://party/widget/partymgr/ProfileScreens.xml"
            ),
            @IncludeScreen(name = "partyRelContacts", location = "component://party/widget/partymgr/ProfileScreens.xml"
            )})}),
            @Container(style = "${styles.grid_row}", containers = {
                @Container2(style = "${styles.grid_large}12 ${styles.grid_cell}", includeScreens = {
                    @IncludeScreen(name = "Notes", location = "component://party/widget/partymgr/ProfileScreens.xml"
                )})})}), failWidgets = @InlineWidgets(containers = {
                    @Container(labels = {
                        @Label(text = "${uiLabelMap.PartyNoPartyFoundWithPartyId}: ${parameters.partyId}", style = "common-msg-error"
                    )})}))})
        }
    )
    public interface ViewProfile {}

    @Screen(name = "CommonCommunicationEventDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(order = 0, type = ActionType.SET, field = "activeSubMenuItem", fromField = "parameters.activeSubMenuItem")
    @Action(order = 1, type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @IfAction(order = 2, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"parameters.partyId"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "parameters.partyId", fromField = "parameters.partyIdFrom")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"parameters.partyId"})}), widgets = @Widgets(decorator = @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "OpportunitySubTabBar", location = "component://marketing/widget/sfa/SfaMenus.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleListCommunications}", decoratorSectionIncludes = {
                    @DecoratorSectionInclude(name = "body")})})
        }
    )), failWidgets = @Widgets(sections = {@SectionNested(actions = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://marketing/widget/sfa/SfaMenus.xml#Opportunity")}), widgets = @WidgetsForContainer(decorator = @DecoratorScreenNested(name = "CommonSfaAppDecorator", location = "${parameters.mainDecoratorLocation}", sections = {@DecoratorSectionNested(name = "body", widgets = @WidgetsForContainer4(value = {@Widget(type = WidgetType.INCLUDE_MENU, name = "OpportunitySubTabBar", location = "component://marketing/widget/sfa/SfaMenus.xml"), @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")}))})))}))
    public interface CommonCommunicationEventDecorator {}

    @Screen(name = "CommonAccountDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://marketing/widget/SalesMenus.xml#Sales")
    @DecoratorScreen(
        name = "CommonSfaAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface CommonAccountDecorator {}

    @Screen(name = "CommonContactDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://marketing/widget/SalesMenus.xml#Sales")
    @DecoratorScreen(
        name = "CommonSfaAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface CommonContactDecorator {}

    @Screen(name = "CommonServiceDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://marketing/widget/ServicesMenus.xml#Services")
    @DecoratorScreen(
        name = "CommonSfaAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface CommonServiceDecorator {}

    @Screen(name = "CommonAnalyticsDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://marketing/widget/AnalyticsMenus.xml#Analytics")
    @DecoratorScreen(
        name = "CommonSfaAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonAnalyticsDecorator {}

    @Screen(name = "CommonLeadDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://marketing/widget/SalesMenus.xml#Sales")
    @Action(type = ActionType.ENTITY_AND, entityName = "PartyRelationship", list = "relatedCompanies", filterByDate = true, fieldMaps = {@FieldMap(fieldName = "partyIdTo", fromField = "partyId"), @FieldMap(fieldName = "roleTypeIdFrom", value = "ACCOUNT_LEAD"), @FieldMap(fieldName = "roleTypeIdTo", value = "LEAD"), @FieldMap(fieldName = "partyRelationshipTypeId", value = "EMPLOYMENT")})
    @DecoratorScreen(
        name = "CommonSfaAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface CommonLeadDecorator {}

    @Screen(name = "CommonEventDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://marketing/widget/SalesMenus.xml#Sales")
    @DecoratorScreen(
        name = "CommonSfaAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "EventSubTabBar", location = "component://marketing/widget/sfa/SfaMenus.xml"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )})
        }
    )
    public interface CommonEventDecorator {}

    @Screen(name = "CommonWorkEffortDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @DecoratorScreen(
        name = "CommonEventDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml"
    )
    public interface CommonWorkEffortDecorator {}

    @Screen(name = "CommonCalendarDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "Calendar")
    @DecoratorScreen(
        name = "CommonSfaAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(actions = @Actions(value = {
                    @Action(type = ActionType.SCRIPT, location = "component://workeffort/webapp/workeffort/WEB-INF/actions/calendar/Days.groovy"
                )}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}))})
        }
    )
    public interface CommonCalendarDecorator {}

    @Screen(name = "MainSideBarMenu", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "menuCfg.location", value = "component://marketing/widget/sfa/SfaMenus.xml")
    @Action(type = ActionType.SET, field = "menuCfg.name", value = "SfaAppSideBar")
    @Action(type = ActionType.SET, field = "menuCfg.defLocation", value = "component://marketing/widget/sfa/SfaMenus.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ComplexSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface MainSideBarMenu {}

    @Screen(name = "DefMainSideBarMenu", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/PrepareDefComplexSideBarMenu.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "MainSideBarMenu")}))
    public interface DefMainSideBarMenu {}

    @Screen(name = "CommonSfaAppSideBarMenu", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSfaAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"MARKETING", "_VIEW"})}))
    @Action(type = ActionType.SET, field = "commonSideBarMenu.cond", fromField = "commonSideBarMenu.cond", valueType = "Boolean", defaultValue = "${commonSfaAppBasePermCond}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface CommonSfaAppSideBarMenu {}

}
