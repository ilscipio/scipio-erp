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
package com.ilscipio.scipio.party.widget;

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
public class PartymgrCommonScreens {

    @Screen(name = "webapp-common-actions", location = "component://party/widget/partymgr/CommonScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "MainSideBarMenu")
    @Action(type = ActionType.SET, field = "mainSideBarMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "mainComplexMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "menuCfg")
    public interface webapp_common_actions {}

    @Screen(name = "main-decorator", location = "component://party/widget/partymgr/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "SecurityUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "HumanResUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companyName", fromField = "uiLabelMap.PartyCompanyName", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companySubtitle", fromField = "uiLabelMap.PartyCompanySubtitle", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/partymgr/static/partymgr.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.styleSheets[]", value = "/partymgr/static/partymgr.css", global = true)
    @Action(type = ActionType.SET, field = "activeApp", value = "partymgr", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuName", value = "PartyAppBar", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuLocation", value = "component://party/widget/partymgr/PartyMenus.xml", global = true)
    @Action(type = ActionType.SET, field = "applicationTitle", value = "${uiLabelMap.Party}", global = true)
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

    @Screen(name = "CommonPartyAppDecorator", location = "component://party/widget/partymgr/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonPartyAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}))
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", overrideByAutoInclude = true, value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonPartyAppSideBarMenu", location = "component://party/widget/partymgr/CommonScreens.xml"
            )}),
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"commonPartyAppBasePermCond"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonPartyAppDecorator {}

    @Screen(name = "CommonPartyDecorator", location = "component://party/widget/partymgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://party/widget/partymgr/PartyMenus.xml#Profile")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "lookupParty")
    @Action(type = ActionType.SET, field = "party", fromField = "lookupParty", global = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "Person", valueField = "lookupPerson")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyGroup", valueField = "lookupGroup")
    @Action(type = ActionType.SET, field = "lookupGroup", fromField = "lookupGroup", global = true)
    @Action(type = ActionType.SET, field = "lookupPerson", fromField = "lookupPerson", global = true)
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonPartyAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.party}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonPartyAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "pre-body", useWhen = "${(context.widePage == true) and (context.commonPartyAppBasePermCond == true) and (not empty context.party)}", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "ProfileTabBar", location = "component://party/widget/partymgr/PartyMenus.xml"
            )})
        }
    )
    public interface CommonPartyDecorator {}

    @Screen(name = "CommonRequestDecorator", location = "component://party/widget/partymgr/CommonScreens.xml")
    @DecoratorScreen(
        name = "CommonPartyAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonRequestDecorator {}

    @Screen(name = "CommonOpportunityDecorator", location = "component://party/widget/partymgr/CommonScreens.xml")
    @DecoratorScreen(
        name = "CommonPartyAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonOpportunityDecorator {}

    @Screen(name = "CommonCommunicationEventDecorator", location = "component://party/widget/partymgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://party/widget/partymgr/PartyMenus.xml#CommEvent")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/communication/GetMyCommunicationEventRole.groovy")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonPartyAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}))
    @DecoratorScreen(
        name = "CommonPartyAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "pre-body", useWhen = "${(context.widePage == true) and (context.commonPartyAppBasePermCond == true)}", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "CommEventTabBar", location = "component://party/widget/partymgr/PartyMenus.xml"
            )}),
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "CommSubTabBar", location = "component://party/widget/partymgr/PartyMenus.xml"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )})
        }
    )
    public interface CommonCommunicationEventDecorator {}

    @Screen(name = "CommonMyCommunicationEventDecorator", location = "component://party/widget/partymgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://party/widget/partymgr/PartyMenus.xml#CommEvent")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/communication/GetMyCommunicationEventRole.groovy")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonPartyAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}))
    @DecoratorScreen(
        name = "CommonPartyAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "pre-body", useWhen = "${(context.widePage == true) and (context.commonPartyAppBasePermCond == true)}", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "CommEventTabBar", location = "component://party/widget/partymgr/PartyMenus.xml"
            )}),
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "CommSubTabBar", location = "component://party/widget/partymgr/PartyMenus.xml"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )})
        }
    )
    public interface CommonMyCommunicationEventDecorator {}

    @Screen(name = "CommonPartyClassificationDecorator", location = "component://party/widget/partymgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://party/widget/partymgr/PartyMenus.xml#PartyClassification")
    @DecoratorScreen(
        name = "CommonPartyAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonPartyClassificationDecorator {}

    @Screen(name = "CommonPartyInvitationDecorator", location = "component://party/widget/partymgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://party/widget/partymgr/PartyMenus.xml#PartyInvitation")
    @Action(type = ActionType.SET, field = "partyInvitationId", fromField = "parameters.partyInvitationId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyInvitation", valueField = "partyInvitation")
    @Action(type = ActionType.SET, field = "partyInvDescFormat", value = " \\${uiLabelMap.CommonFor} ${partyInvitation.partyIdFrom} [${partyInvitationId}]")
    @Action(type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle}${groovy: context.partyInvitation ? context.partyInvDescFormat : ''}")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonPartyAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.partyInvitation}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonPartyAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "pre-body", useWhen = "${(context.widePage == true) and (context.commonPartyAppBasePermCond == true) and (not empty context.partyInvitation)}", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "PartyInvitationTabBar", location = "component://party/widget/partymgr/PartyMenus.xml"
            )}),
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"partyInvitation"})}
                ), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_MENU, name = "PartyInvitationSubTabBar", location = "component://party/widget/partymgr/PartyMenus.xml"
                )}), position = 0)})
        }
    )
    public interface CommonPartyInvitationDecorator {}

    @Screen(name = "SecurityDecorator", location = "component://party/widget/partymgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "securityTargetDecoratorName", fromField = "securityTargetDecoratorName", defaultValue = "CommonPartyAppDecorator")
    @DecoratorScreen(
        name = "SecurityDecorator",
        location = "component://common/widget/SecurityScreens.xml"
    )
    public interface SecurityDecorator {}

    @Screen(name = "main", location = "component://party/widget/partymgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "main")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PartyUserManagement")
    @DecoratorScreen(
        name = "CommonPartyAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}12 ${styles.grid_cell}", screenlets = {
                        @ScreenletNested(title = "${uiLabelMap.PartyUserActivity}", containers = {
                    @ContainerInScreenlet(style = "${styles.grid_row}", containers = {
                        @ContainerInScreenlet2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                            @IncludeScreen(name = "LoggedInUsersScreen", location = "component://party/widget/partymgr/VisitScreens.xml"
                        
                    )}),
                        @ContainerInScreenlet2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                            @IncludeScreen(name = "ScipioNewRegistrations", location = "component://party/widget/partymgr/CommonScreens.xml"
                        
                )})})})})}),
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "ScipioLastCommunication", location = "component://party/widget/partymgr/CommonScreens.xml"
                    )}),
                    @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "ScipioNewRegistrationsList", location = "component://party/widget/partymgr/CommonScreens.xml"
                    )})})})
        }
    )
    public interface main {}

    @Screen(name = "ScipioNewRegistrations", location = "component://party/widget/partymgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "chartType", value = "bar")
    @Action(type = ActionType.SET, field = "chartLibrary", value = "chart")
    @Action(type = ActionType.SET, field = "chartIntervalScope", value = "month")
    @Action(type = ActionType.SET, field = "chartIntervalCount", value = "6")
    @Action(type = ActionType.SET, field = "chartDatasets", value = "1")
    @Action(type = ActionType.SET, field = "xlabel")
    @Action(type = ActionType.SET, field = "ylabel")
    @Action(type = ActionType.SET, field = "label1", value = "${uiLabelMap.PartyRegistrations}")
    @Action(type = ActionType.SCRIPT, location = "component://party/script/com/ilscipio/party/dashboard/PartyNewRegistrations.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"totalMap"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.PartyRegistrations}", htmlTemplates = {@HtmlTemplate(location = "component://party/webapp/partymgr/party/dashboard/PartyNewRegistrations.ftl")})}))
    public interface ScipioNewRegistrations {}

    @Screen(name = "ScipioNewRegistrationsList", location = "component://party/widget/partymgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", value = "10", valueType = "Integer")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PartyAndPerson", list = "registrations", orderBy = {"createdDate"})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"registrations"})}), widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://party/webapp/partymgr/party/dashboard/PartyNewRegistrationsList.ftl")})}))
    public interface ScipioNewRegistrationsList {}

    @Screen(name = "ScipioSecurityAlerts", location = "component://party/widget/partymgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "intervalScope", value = "month")
    @Action(type = ActionType.SET, field = "parameters.VIEW_SIZE", value = "10", valueType = "Integer", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://party/script/com/ilscipio/party/dashboard/PartySecurityAlerts.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"securityAlerts"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.CommonSecurity}", htmlTemplates = {@HtmlTemplate(location = "component://party/webapp/partymgr/party/dashboard/PartySecurityAlerts.ftl")})}))
    public interface ScipioSecurityAlerts {}

    @Screen(name = "ScipioLastCommunication", location = "component://party/widget/partymgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "intervalScope", value = "month")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.SET, field = "viewSize", value = "10", valueType = "Integer")
    @Action(type = ActionType.SCRIPT, location = "component://party/script/com/ilscipio/party/dashboard/PartyLastCommunications.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"lastCommunications"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.PartyLastCommunication}", htmlTemplates = {@HtmlTemplate(location = "component://party/webapp/partymgr/party/dashboard/PartyLastCommunications.ftl")})}))
    public interface ScipioLastCommunication {}

    @Screen(name = "MainSideBarMenu", location = "component://party/widget/partymgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "menuCfg.location", value = "component://party/widget/partymgr/PartyMenus.xml")
    @Action(type = ActionType.SET, field = "menuCfg.name", value = "PartyAppSideBar")
    @Action(type = ActionType.SET, field = "menuCfg.defLocation", value = "component://party/widget/partymgr/PartyMenus.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ComplexSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface MainSideBarMenu {}

    @Screen(name = "DefMainSideBarMenu", location = "component://party/widget/partymgr/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/PrepareDefComplexSideBarMenu.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "MainSideBarMenu")}))
    public interface DefMainSideBarMenu {}

    @Screen(name = "CommonPartyAppSideBarMenu", location = "component://party/widget/partymgr/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonPartyAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}))
    @Action(type = ActionType.SET, field = "commonSideBarMenu.cond", fromField = "commonSideBarMenu.cond", valueType = "Boolean", defaultValue = "${commonPartyAppBasePermCond}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface CommonPartyAppSideBarMenu {}

}
