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
public class LookupForms {

    @Form(
        name = "LookupGeo",
        location = "component://common/widget/LookupForms.xml",
        target = "LookupGeo",
        fields = {
            @FormField(name = "geoId", title = "${uiLabelMap.CommonGeoId}", textFind = @TextFindField),
            @FormField(name = "geoTypeId", title = "${uiLabelMap.CommonGeoTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GeoType", description = "${description}", keyFieldName = "geoTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "geoName", title = "${uiLabelMap.CommonGeoName}", textFind = @TextFindField),
            @FormField(name = "geoCode", title = "${uiLabelMap.CommonGeoCode}", textFind = @TextFindField),
            @FormField(name = "geoSecCode", title = "${uiLabelMap.CommonGeoSecCode}", textFind = @TextFindField),
            @FormField(name = "abbreviation", title = "${uiLabelMap.CommonGeoAbbr}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupGeo {}

    @Form(
        name = "listLookupGeo",
        location = "component://common/widget/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupGeo",
        fields = {
            @FormField(name = "geoId", title = "${uiLabelMap.CommonGeoId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${geoId}')", urlMode = UrlMode.PLAIN, description = "${geoId}", alsoHidden = false)),
            @FormField(name = "geoTypeId", title = "${uiLabelMap.CommonGeoTypeId}", displayEntity = @DisplayEntityField(entityName = "GeoType", keyFieldName = "geoTypeId", description = "${description}")),
            @FormField(name = "geoName", title = "${uiLabelMap.CommonGeoName}", display = @DisplayField),
            @FormField(name = "geoCode", title = "${uiLabelMap.CommonGeoCode}", display = @DisplayField),
            @FormField(name = "geoSecCode", title = "${uiLabelMap.CommonGeoSecCode}", display = @DisplayField),
            @FormField(name = "abbreviation", title = "${uiLabelMap.CommonGeoAbbr}", display = @DisplayField),
            @FormField(name = "wellKnownText", title = "${uiLabelMap.CommonGeoWellKnownText}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Geo"), @FieldMap(fieldName = "orderBy", value = "geoName"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupGeo {}

    @Form(
        name = "LookupGeoName",
        location = "component://common/widget/LookupForms.xml",
        target = "LookupGeoName",
        extendsForm = "LookupGeo"
    )
    public interface LookupGeoName {}

    @Form(
        name = "listLookupGeoName",
        location = "component://common/widget/LookupForms.xml",
        extendsForm = "listLookupGeo",
        paginateTarget = "LookupGeoName",
        fields = {
            @FormField(name = "geoId", title = "${uiLabelMap.CommonGeoId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_values('${geoName}', '${geoId}')", urlMode = UrlMode.PLAIN, description = "${geoId}", alsoHidden = false))
        }
    )
    public interface listLookupGeoName {}

    @Form(
        name = "LookupUserLogin",
        location = "component://common/widget/LookupForms.xml",
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
        location = "component://common/widget/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupUserLogin",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "userLoginId", title = "${uiLabelMap.CommonUserLoginId}", widgetStyle = "${styles.link_run_sys} ${styles.action_select}", hyperlink = @HyperlinkField(target = "javascript:set_value('${userLoginId}', '${userLoginId}', '${parameters.webSitePublishPoint}')", urlMode = UrlMode.PLAIN, description = "${userLoginId}", alsoHidden = false)),
            @FormField(name = "enabled", display = @DisplayField),
            @FormField(name = "hasLoggedOut", display = @DisplayField),
            @FormField(name = "disabledDateTime", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "orderBy", value = "userLoginId"), @FieldMap(fieldName = "entityName", value = "UserLogin"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListLookedUpUserLogins {}

    @Form(
        name = "LookupPortalPage",
        location = "component://common/widget/LookupForms.xml",
        target = "LookupPortalPage",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PortalPage", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupPortalPage {}

    @Form(
        name = "ListPortalPages",
        location = "component://common/widget/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupPortalPage",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PortalPage", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "portalPageId", widgetStyle = "${styles.link_run_sys} ${styles.action_select}", hyperlink = @HyperlinkField(target = "javascript:set_value('${portalPageId}')", urlMode = UrlMode.PLAIN, description = "${portalPageId}", alsoHidden = false))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "orderBy", value = "portalPageId"), @FieldMap(fieldName = "entityName", value = "PortalPage"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListPortalPages {}

    @Form(
        name = "LookupLocale",
        location = "component://common/widget/LookupForms.xml",
        target = "LookupLocale",
        fields = {
            @FormField(name = "localeString", textFind = @TextFindField(hideOptions = "true")),
            @FormField(name = "localeName", title = "${uiLabelMap.CommonLanguageTitle}", textFind = @TextFindField(hideOptions = "true")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupLocale {}

    @Form(
        name = "ListLocales",
        location = "component://common/widget/LookupForms.xml",
        type = FormType.LIST,
        listName = "locales",
        paginateTarget = "LookupLocale",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "localeString", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${localeString}', '${localeName}')", urlMode = UrlMode.PLAIN, description = "${localeString}", alsoHidden = false)),
            @FormField(name = "localeName", title = "${uiLabelMap.CommonLanguageTitle}", display = @DisplayField)
        }
    )
    public interface ListLocales {}

}
