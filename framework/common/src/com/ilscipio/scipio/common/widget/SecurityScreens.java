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
package com.ilscipio.scipio.common.widget;

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
public class SecurityScreens {

    @Screen(name = "CreateSecurityGroup", location = "component://common/widget/SecurityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "NewSecurityGroup")
    @Action(type = ActionType.SET, field = "cancelPage", fromField = "parameters.CANCEL_PAGE", defaultValue = "FindSecurityGroup")
    @DecoratorScreen(
        name = "SecurityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "CreateSecurityGroup", location = "component://common/widget/SecurityForms.xml"
            )})
        }
    )
    public interface CreateSecurityGroup {}

    @Screen(name = "CreateUserLogin", location = "component://common/widget/SecurityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "CreateUserLogin")
    @Action(type = ActionType.SET, field = "cancelPage", fromField = "parameters.CANCEL_PAGE", defaultValue = "FindUserLogin")
    @Action(type = ActionType.SET, field = "createUserLoginURI", value = "createUserLogin")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @DecoratorScreen(
        name = "SecurityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "AddUserLogin", location = "component://common/widget/SecurityForms.xml"
            )})
        }
    )
    public interface CreateUserLogin {}

    @Screen(name = "EditSecurityGroup", location = "component://common/widget/SecurityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditSecurityGroup")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditSecurityGroup")
    @Action(type = ActionType.SET, field = "groupId", fromField = "parameters.groupId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "SecurityGroup", valueField = "securityGroup")
    @DecoratorScreen(
        name = "SecurityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "EditSecurityGroup", location = "component://common/widget/SecurityForms.xml"
            )})
        }
    )
    public interface EditSecurityGroup {}

    @Screen(name = "EditSecurityGroupPermissions", location = "component://common/widget/SecurityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditSecurityGroupPermissions")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditSecurityGroupPermissions")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "groupId", fromField = "parameters.groupId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "SecurityGroup", valueField = "securityGroup")
    @DecoratorScreen(
        name = "SecurityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "page-title", labels = {
                    @Label(text = "${uiLabelMap.PageTitleEditSecurityGroupPermissions} - ${groupId}"
                )}, position = 0)}, screenlets = {
                    @Screenlet(title = "${uiLabelMap.AddPermissionToSecurityGroup}", name = "AddPermissionFromList", collapsible = true, includeForms = {
                        @IncludeForm(name = "AddSecurityGroupPermission", location = "component://common/widget/SecurityForms.xml"
                    )}, position = 1),
                    @Screenlet(title = "${uiLabelMap.AddPermissionManuallyToSecurityGroup}", name = "AddPermissionManual", collapsible = true, includeForms = {
                        @IncludeForm(name = "AddSecurityGroupPermissionManual", location = "component://common/widget/SecurityForms.xml"
                    )}, position = 2),
                    @Screenlet(title = "${uiLabelMap.Permissions}", includeForms = {
                        @IncludeForm(name = "ListSecurityGroupPermissions", location = "component://common/widget/SecurityForms.xml"
                    )}, position = 3)})
        }
    )
    public interface EditSecurityGroupPermissions {}

    @Screen(name = "EditSecurityGroupProtectedViews", location = "component://common/widget/SecurityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AddProtectedViewToSecurityGroup")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditSecurityGroupProtectedViews")
    @Action(type = ActionType.SET, field = "groupId", fromField = "parameters.groupId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "SecurityGroup", valueField = "securityGroup")
    @DecoratorScreen(
        name = "SecurityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListSecurityGroupProtectedViews", location = "component://common/widget/SecurityForms.xml"
            )}, containers = {
                @Container(style = "page-title", labels = {
                    @Label(text = "${uiLabelMap.ProtectedViews} - ${groupId}")}, position = 0
                )}, screenlets = {
                    @Screenlet(title = "${uiLabelMap.AddProtectedViewToSecurityGroup}", name = "AddSecurityGroupProtectedViewsPanel", collapsible = true, includeForms = {
                        @IncludeForm(name = "AddSecurityGroupProtectedView", location = "component://common/widget/SecurityForms.xml"
                    )}, position = 1)})
        }
    )
    public interface EditSecurityGroupProtectedViews {}

    @Screen(name = "EditSecurityGroupUserLogins", location = "component://common/widget/SecurityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AddUserLoginToSecurityGroup")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditSecurityGroupUserLogins")
    @Action(type = ActionType.SET, field = "groupId", fromField = "parameters.groupId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "SecurityGroup", valueField = "securityGroup")
    @DecoratorScreen(
        name = "SecurityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListSecurityGroupUserLogins", location = "component://common/widget/SecurityForms.xml"
            )}, containers = {
                @Container(style = "page-title", labels = {
                    @Label(text = "${uiLabelMap.UserLogins} - ${groupId}")}, position = 0
                )}, screenlets = {
                    @Screenlet(title = "${uiLabelMap.AddUserLoginToSecurityGroup}", name = "AddSecurityGroupUserLoginsPanel", collapsible = true, includeForms = {
                        @IncludeForm(name = "AddSecurityGroupUserLogin", location = "component://common/widget/SecurityForms.xml"
                    )}, position = 1)})
        }
    )
    public interface EditSecurityGroupUserLogins {}

    @Screen(name = "EditUserLogin", location = "component://common/widget/SecurityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "UserLoginUpdateSecuritySettings")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditUserLogin")
    @Action(type = ActionType.SET, field = "cancelPage", fromField = "parameters.CANCEL_PAGE", defaultValue = "FindUserLogin")
    @Action(type = ActionType.SET, field = "updatePasswordURI", value = "updatePassword")
    @Action(type = ActionType.SET, field = "userLoginId", fromField = "parameters.userLoginId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "UserLogin", valueField = "editUserLogin")
    @DecoratorScreen(
        name = "SecurityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "updateUserLoginSecurity", location = "component://common/widget/SecurityScreens.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.UserLoginChangePassword}", includeForms = {
                    @IncludeForm(name = "updatePassword", location = "component://common/widget/SecurityForms.xml"
                )})})
        }
    )
    public interface EditUserLogin {}

    @Screen(name = "updateUserLoginSecurity", location = "component://common/widget/SecurityScreens.xml")
    @Action(type = ActionType.SET, field = "updateUserLoginSecurityURI", value = "updateUserLoginSecurity")
    @Action(type = ActionType.ENTITY_ONE, entityName = "UserLogin", valueField = "editUserLogin")
    @Action(type = ActionType.SET, field = "userLoginId", fromField = "editUserLogin.userLoginId")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(includeForms = {@IncludeForm(name = "updateUserLoginSecurity", location = "component://common/widget/SecurityForms.xml")})}))
    public interface updateUserLoginSecurity {}

    @Screen(name = "EditUserLoginSecurityGroups", location = "component://common/widget/SecurityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditUserLoginSecurityGroups")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditUserLoginSecurityGroups")
    @Action(type = ActionType.SET, field = "addUserLoginSecurityGroupURI", value = "userLogin_addUserLoginToSecurityGroup")
    @Action(type = ActionType.SET, field = "removeUserLoginSecurityGroupURI", value = "userLogin_removeUserLoginFromSecurityGroup")
    @Action(type = ActionType.SET, field = "updateUserLoginSecurityGroupURI", value = "userLogin_updateUserLoginToSecurityGroup")
    @Action(type = ActionType.SET, field = "userLoginId", fromField = "parameters.userLoginId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "UserLogin", valueField = "editUserLogin")
    @DecoratorScreen(
        name = "SecurityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListUserLoginSecurityGroups", location = "component://common/widget/SecurityForms.xml"
            )}, containers = {
                @Container(style = "page-title", labels = {
                    @Label(text = "${uiLabelMap.PageTitleEditUserLoginSecurityGroups} - ${userLoginId}"
                )}, position = 0)}, screenlets = {
                    @Screenlet(title = "${uiLabelMap.AddUserLoginToSecurityGroup}", name = "AddUserLoginSecurityGroupsPanel", collapsible = true, includeForms = {
                        @IncludeForm(name = "AddUserLoginSecurityGroup", location = "component://common/widget/SecurityForms.xml"
                    )}, position = 1)})
        }
    )
    public interface EditUserLoginSecurityGroups {}

    @Screen(name = "EditX509IssuerProvisions", location = "component://common/widget/SecurityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditIssuerProvisions")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditCertIssuerProvisions")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "SecurityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "page-title", labels = {
                    @Label(text = "${uiLabelMap.PageTitleEditIssuerProvisions}")}, position = 0
                )}, screenlets = {
                    @Screenlet(title = "${uiLabelMap.PageTitleAddIssuerProvisions}", includeForms = {
                        @IncludeForm(name = "ViewCertificate", location = "component://common/widget/SecurityForms.xml"
                    )}, position = 1),
                    @Screenlet(title = "${uiLabelMap.CertIssuers}", includeForms = {
                        @IncludeForm(name = "CertIssuerList", location = "component://common/widget/SecurityForms.xml"
                    )}, position = 2)})
        }
    )
    public interface EditX509IssuerProvisions {}

    @Screen(name = "FindSecurityGroup", location = "component://common/widget/SecurityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindSecurityGroup")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindSecurityGroup")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "SecurityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListSecurityGroups", location = "component://common/widget/SecurityForms.xml"
            )}, containers = {
                @Container(style = "button-bar", widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonCreateNew}", style = "${styles.link_nav} ${styles.action_add}", target = "CreateNewSecurityGroup"
                )}, position = 0)})
        }
    )
    public interface FindSecurityGroup {}

    @Screen(name = "FindUserLogin", location = "component://common/widget/SecurityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "FindUserLogin")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindUserLogin")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "SecurityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListUserLogins", location = "component://common/widget/SecurityForms.xml"
            )}, containers = {
                @Container(style = "button-bar", widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonCreateNew}", style = "${styles.link_nav} ${styles.action_add}", target = "createnewlogin"
                )}, position = 0)})
        }
    )
    public interface FindUserLogin {}

    @Screen(name = "SecurityDecorator", location = "component://common/widget/SecurityScreens.xml")
    @Action(type = ActionType.SET, field = "securityTargetDecoratorName", fromField = "securityTargetDecoratorName", defaultValue = "main-decorator")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://common/widget/SecurityMenus.xml#SecurityGroup")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSecurityBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"securityPermissionCheck", "VIEW"})}))
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", fromField = "commonSecurityBasePermCond", valueType = "Boolean")
    @DecoratorScreen(
        name = "${securityTargetDecoratorName}",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${(context.widePage != true) && (context.securityTargetDecoratorName == 'main-decorator')}", overrideByAutoInclude = true, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"commonSecurityBasePermCond"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_SCREEN, name = "MainSideBarMenu", location = "${parameters.mainDecoratorLocation}"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_SCREEN, name = "DefMainSideBarMenu", location = "${parameters.mainDecoratorLocation}"
                )}))}),
            @DecoratorSection(name = "pre-body", useWhen = "${(context.widePage == true) and (context.commonSecurityBasePermCond == true)}", overrideByAutoInclude = true, value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "SecurityGroupTabBar", location = "component://common/widget/SecurityMenus.xml"
            )}),
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"commonSecurityBasePermCond"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.SecurityViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface SecurityDecorator {}

    @Screen(name = "ViewCertificate", location = "component://common/widget/SecurityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleImportCertificate")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditCertIssuerProvisions")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/ViewCertificate.groovy")
    @DecoratorScreen(
        name = "SecurityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "widget-container", htmlTemplates = {
                    @HtmlTemplate(location = "component://common/webcommon/includes/ViewCertificate.ftl"
                )})})
        }
    )
    public interface ViewCertificate {}

}
