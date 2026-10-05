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

import com.ilscipio.scipio.widget.def.form.*;
import com.ilscipio.scipio.widget.def.screen.SetAction;
import com.ilscipio.scipio.widget.def.screen.ServiceAction;
import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;
import com.ilscipio.scipio.widget.def.screen.ScriptAction;
import com.ilscipio.scipio.widget.def.screen.PropertyToFieldAction;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class SecurityForms {

    @Form(
        name = "AddSecurityGroupPermission",
        location = "component://common/widget/SecurityForms.xml",
        target = "addSecurityPermissionToSecurityGroup",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "addSecurityPermissionToSecurityGroup")
        },
        fields = {
            @FormField(name = "groupId", hidden = @HiddenField),
            @FormField(name = "permissionId", title = "${uiLabelMap.PermissionId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "SecurityPermission", description = "${permissionId} ${description}", orderBy = {@EntityOrderBy(fieldName = "permissionId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddSecurityGroupPermission {}

    @Form(
        name = "AddSecurityGroupPermissionManual",
        location = "component://common/widget/SecurityForms.xml",
        target = "addSecurityPermissionToSecurityGroup",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "addSecurityPermissionToSecurityGroup")
        },
        fields = {
            @FormField(name = "groupId", hidden = @HiddenField),
            @FormField(name = "permissionId", title = "${uiLabelMap.PermissionId}"),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddSecurityGroupPermissionManual {}

    @Form(
        name = "AddSecurityGroupProtectedView",
        location = "component://common/widget/SecurityForms.xml",
        target = "addProtectedViewToSecurityGroup",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "addProtectedViewToSecurityGroup")
        },
        fields = {
            @FormField(name = "groupId", hidden = @HiddenField),
            @FormField(name = "viewNameId", text = @TextField(size = 60, maxlength = 60)),
            @FormField(name = "maxHits", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "maxHitsDuration", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "tarpitDuration", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddSecurityGroupProtectedView {}

    @Form(
        name = "AddSecurityGroupUserLogin",
        location = "component://common/widget/SecurityForms.xml",
        target = "addUserLoginToSecurityGroup",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "addUserLoginToSecurityGroup")
        },
        fields = {
            @FormField(name = "groupId", hidden = @HiddenField),
            @FormField(name = "userLoginId", title = "${uiLabelMap.CommonUserLoginId}", lookup = @LookupField(targetFormName = "LookupUserLogin", size = 30)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddSecurityGroupUserLogin {}

    @Form(
        name = "AddUserLogin",
        location = "component://common/widget/SecurityForms.xml",
        target = "${createUserLoginURI}",
        focusFieldName = "userLoginId",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createUserLogin")
        },
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "enabled", hidden = @HiddenField),
            @FormField(name = "currentPassword", password = @PasswordField),
            @FormField(name = "currentPasswordVerify", password = @PasswordField),
            @FormField(name = "requirePasswordChange", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link")),
            @FormField(name = "cancelAction", title = " ", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "${cancelPage}", description = "${uiLabelMap.CommonCancelDone}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId")}))
        }
    )
    public interface AddUserLogin {}

    @Form(
        name = "AddUserLoginSecurityGroup",
        location = "component://common/widget/SecurityForms.xml",
        target = "${addUserLoginSecurityGroupURI}",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "addUserLoginToSecurityGroup")
        },
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField(value = "${partyId}")),
            @FormField(name = "userLoginId", hidden = @HiddenField),
            @FormField(name = "groupId", title = "${uiLabelMap.CommonGroup}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "SecurityGroup", description = "${groupId} ${description}", orderBy = {@EntityOrderBy(fieldName = "groupId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddUserLoginSecurityGroup {}

    @Form(
        name = "CertIssuerList",
        location = "component://common/widget/SecurityForms.xml",
        type = FormType.LIST,
        listName = "issuerProvisions",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "X509IssuerProvision", defaultFieldType = DefaultFieldType.DISPLAY)
        }
    )
    public interface CertIssuerList {}

    @Form(
        name = "CreateSecurityGroup",
        location = "component://common/widget/SecurityForms.xml",
        target = "createSecurityGroup",
        defaultMapName = "securityGroup",
        fields = {
            @FormField(name = "groupId", title = "${uiLabelMap.CommonSecurityGroupId}", requiredField = true, text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "groupName", title = "${uiLabelMap.CommonName}", text = @TextField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField(size = 60)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "cancelAction", title = " ", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "${cancelPage}", description = "${uiLabelMap.CommonCancelDone}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId")}))
        }
    )
    public interface CreateSecurityGroup {}

    @Form(
        name = "EditSecurityGroup",
        location = "component://common/widget/SecurityForms.xml",
        target = "updateSecurityGroup",
        defaultMapName = "securityGroup",
        fields = {
            @FormField(name = "groupId", title = "${uiLabelMap.CommonSecurityGroupId}", tooltip = "${uiLabelMap.CommonNotModifRecreat}", display = @DisplayField),
            @FormField(name = "groupName", title = "${uiLabelMap.CommonName}", text = @TextField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField(size = 60)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditSecurityGroup {}

    @Form(
        name = "ListSecurityGroupPermissions",
        location = "component://common/widget/SecurityForms.xml",
        type = FormType.LIST,
        listName = "securityGroupPermissions",
        paginateTarget = "EditSecurityGroupPermissions",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "groupId", hidden = @HiddenField),
            @FormField(name = "permissionId", title = "${uiLabelMap.PermissionId}", displayEntity = @DisplayEntityField(entityName = "SecurityPermission", description = "${permissionId} ${description}")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeSecurityPermissionFromSecurityGroup", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "permissionId"), @ParameterDef(paramName = "groupId")}))
        }
    )
    public interface ListSecurityGroupPermissions {}

    @Form(
        name = "ListSecurityGroupProtectedViews",
        location = "component://common/widget/SecurityForms.xml",
        type = FormType.LIST,
        target = "updateProtectedViewToSecurityGroup",
        listName = "securityGroupProtectedViewsList",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "groupId", hidden = @HiddenField),
            @FormField(name = "viewNameId", display = @DisplayField),
            @FormField(name = "maxHits", text = @TextField),
            @FormField(name = "maxHitsDuration", text = @TextField),
            @FormField(name = "tarpitDuration", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeProtectedViewFromSecurityGroup", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "groupId"), @ParameterDef(paramName = "viewNameId")}))
        }
    )
    public interface ListSecurityGroupProtectedViews {}

    @Form(
        name = "ListSecurityGroups",
        location = "component://common/widget/SecurityForms.xml",
        type = FormType.LIST,
        listName = "securityGroups",
        paginateTarget = "FindSecurityGroup",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "groupId", title = "${uiLabelMap.CommonSecurityGroupId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditSecurityGroup", description = "${groupId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "groupId")})),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField)
        }
    )
    public interface ListSecurityGroups {}

    @Form(
        name = "ListSecurityGroupUserLogins",
        location = "component://common/widget/SecurityForms.xml",
        type = FormType.LIST,
        target = "updateUserLoginToSecurityGroup",
        listName = "userLoginSecurityGroups",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "groupId", hidden = @HiddenField),
            @FormField(name = "userLoginId", widgetStyle = "${styles.link_nav_info_id} ${styles.action_view}", sortField = true, hyperlink = @HyperlinkField(target = "editlogin", description = "${userLoginId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "userLoginId")})),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", sortField = true, display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", sortField = true, dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeUserLoginFromSecurityGroup", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "userLoginId"), @ParameterDef(paramName = "groupId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "thruDate", fromField = "date:nowTimestamp()")}))
        },
        actions = @FormActions(set = {@SetAction(field = "sortField", fromField = "parameters.sortField", defaultValue = "userLoginId")})
    )
    public interface ListSecurityGroupUserLogins {}

    @Form(
        name = "ListUserLogins",
        location = "component://common/widget/SecurityForms.xml",
        type = FormType.LIST,
        listName = "securityGroups",
        paginateTarget = "FindUserLogin",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "userLoginId", widgetStyle = "${styles.link_nav_info_id}", sortField = true, hyperlink = @HyperlinkField(target = "editlogin", description = "${userLoginId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "userLoginId")})),
            @FormField(name = "enabled", sortField = true, display = @DisplayField),
            @FormField(name = "hasLoggedOut", sortField = true, display = @DisplayField),
            @FormField(name = "disabledDateTime", sortField = true, display = @DisplayField)
        },
        actions = @FormActions(set = {@SetAction(field = "sortField", fromField = "parameters.sortField", defaultValue = "userLoginId")})
    )
    public interface ListUserLogins {}

    @Form(
        name = "ListUserLoginSecurityGroups",
        location = "component://common/widget/SecurityForms.xml",
        type = FormType.LIST,
        target = "${updateUserLoginSecurityGroupURI}",
        listName = "userLoginSecurityGroups",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField(value = "${partyId}")),
            @FormField(name = "groupIdCol", title = "${uiLabelMap.CommonSecurityGroupId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditSecurityGroup", description = "${groupId}", parameters = {@ParameterDef(paramName = "groupId")})),
            @FormField(name = "groupId", title = "${uiLabelMap.CommonDescription}", displayEntity = @DisplayEntityField(entityName = "SecurityGroup")),
            @FormField(name = "userLoginId", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", redWhen = "before-now", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "${removeUserLoginSecurityGroupURI}", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "userLoginId"), @ParameterDef(paramName = "groupId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "thruDate", fromField = "date:nowTimestamp()")}))
        }
    )
    public interface ListUserLoginSecurityGroups {}

    @Form(
        name = "LookupUserLogin",
        location = "component://common/widget/SecurityForms.xml",
        target = "LookupUserLogin",
        fields = {
            @FormField(name = "userLoginId", title = "${uiLabelMap.CommonUserLoginId}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupUserLogin {}

    @Form(
        name = "ListLookedUpUserLogins",
        location = "component://common/widget/SecurityForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupUserLogin",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "userLoginId", title = "${uiLabelMap.CommonUserLoginId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${userLoginId}', '${userLoginId}', '${parameters.webSitePublishPoint}')", urlMode = UrlMode.PLAIN, description = "${userLoginId}", alsoHidden = false)),
            @FormField(name = "enabled", display = @DisplayField),
            @FormField(name = "hasLoggedOut", display = @DisplayField),
            @FormField(name = "disabledDateTime", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "orderBy", value = "userLoginId"), @FieldMap(fieldName = "entityName", value = "UserLogin"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListLookedUpUserLogins {}

    @Form(
        name = "updatePassword",
        location = "component://common/widget/SecurityForms.xml",
        target = "${updatePasswordURI}",
        focusFieldName = "currentPassword",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePassword")
        },
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "userLoginId", hidden = @HiddenField),
            @FormField(name = "currentPassword", password = @PasswordField),
            @FormField(name = "newPassword", password = @PasswordField),
            @FormField(name = "newPasswordVerify", password = @PasswordField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link")),
            @FormField(name = "cancelAction", title = " ", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "${cancelPage}", description = "${uiLabelMap.CommonCancelDone}", alsoHidden = false, parameters = {@ParameterDef(paramName = "userLoginId"), @ParameterDef(paramName = "partyId")}))
        },
        actions = @FormActions(set = {@SetAction(field = "passwordHint", fromField = "editUserLogin.passwordHint")})
    )
    public interface updatePassword {}

    @Form(
        name = "updateUserLoginSecurity",
        location = "component://common/widget/SecurityForms.xml",
        target = "${updateUserLoginSecurityURI}",
        defaultMapName = "editUserLogin",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateUserLoginSecurity")
        },
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "userLoginId", hidden = @HiddenField),
            @FormField(name = "userLdapDn", useWhen = "\"true\".equals(ldapEnabled)", text = @TextField),
            @FormField(name = "userLdapDn", useWhen = "!\"true\".equals(ldapEnabled)", ignored = @IgnoredField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface updateUserLoginSecurity {}

    @Form(
        name = "ViewCertificate",
        location = "component://common/widget/SecurityForms.xml",
        target = "ViewCertificate",
        headerRowStyle = "header-row",
        method = "get",
        fields = {
            @FormField(name = "certString", textarea = @TextareaField(rows = 10)),
            @FormField(name = "View Cert", title = "${uiLabelMap.ViewCert}", widgetStyle = "${styles.link_run_sys} ${styles.action_view}", submit = @SubmitField)
        }
    )
    public interface ViewCertificate {}

}
