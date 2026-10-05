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
package com.ilscipio.scipio.webtools.widget;

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
public class GeoManagementForms {

    @Form(
        name = "FindGeo",
        location = "component://webtools/widget/GeoManagementForms.xml",
        target = "${currentUrl}",
        id = "FindGeo",
        extendsForm = "LookupGeo",
        extendsResource = "component://common/widget/LookupForms.xml"
    )
    public interface FindGeo {}

    @Form(
        name = "ListGeos",
        location = "component://webtools/widget/GeoManagementForms.xml",
        extendsForm = "listLookupGeo",
        extendsResource = "component://common/widget/LookupForms.xml",
        paginateTarget = "${currentUrl}",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "geoId", title = "${uiLabelMap.CommonGeoId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditGeo", urlMode = UrlMode.PLAIN, description = "${geoId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "geoId")})),
            @FormField(name = "wellKnownText", title = "${uiLabelMap.CommonGeoWellKnownText}", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteGeo", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "geoId"), @ParameterDef(paramName = "noConditionFind", value = "Y")}))
        }
    )
    public interface ListGeos {}

    @Form(
        name = "EditGeo",
        location = "component://webtools/widget/GeoManagementForms.xml",
        target = "updateGeo",
        defaultMapName = "geo",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "geoId", title = "${uiLabelMap.CommonGeoId}", text = @TextField),
            @FormField(name = "geoTypeId", title = "${uiLabelMap.CommonGeoTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GeoType", description = "${description}", keyFieldName = "geoTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "geoName", title = "${uiLabelMap.CommonGeoName}", text = @TextField),
            @FormField(name = "geoCode", title = "${uiLabelMap.CommonGeoCode}", text = @TextField),
            @FormField(name = "geoSecCode", title = "${uiLabelMap.CommonGeoSecCode}", text = @TextField),
            @FormField(name = "abbreviation", title = "${uiLabelMap.CommonGeoAbbr}", text = @TextField),
            @FormField(name = "wellKnownText", title = "${uiLabelMap.CommonGeoWellKnownText}", textarea = @TextareaField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "geo==null", target = "createGeo")
        }
    )
    public interface EditGeo {}

    @Form(
        name = "ListLookupGeo",
        location = "component://webtools/widget/GeoManagementForms.xml",
        extendsForm = "ListGeos",
        fields = {
            @FormField(name = "geoId", title = "${uiLabelMap.CommonGeoId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${geoId}')", urlMode = UrlMode.PLAIN, description = "${geoId}", alsoHidden = false)),
            @FormField(name = "deleteAction", ignored = @IgnoredField)
        }
    )
    public interface ListLookupGeo {}

    @Form(
        name = "LinkGeos",
        location = "component://webtools/widget/GeoManagementForms.xml",
        target = "linkGeos",
        focusFieldName = "geoId",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "geoIds", title = "${uiLabelMap.CommonGeos}", dropDown = @DropDownField(allowMulti = true, entityOptions = @EntityOptions(entityName = "Geo", description = "${geoName} (${geoId})", keyFieldName = "geoId", orderBy = {@EntityOrderBy(fieldName = "geoId")}))),
            @FormField(name = "dummy", title = " ", display = @DisplayField),
            @FormField(name = "geoAssocTypeId", title = "${uiLabelMap.CommonGeoAssocTypeId}", event = "onChange", action = "typeValue = jQuery('#${asm_typeField}').val(); selectMultipleRelatedValues('${asm_requestName}', '${asm_paramKey}', '${asm_relatedField}', '${asm_multipleSelect}', '${asm_type}', typeValue, '${asm_responseName}');", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GeoAssocType", description = "${description}", keyFieldName = "geoAssocTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "geoId", title = "${uiLabelMap.CommonGeo}", dropDown = @DropDownField(current = "selected", entityOptions = @EntityOptions(entityName = "Geo", description = "${geoName} (${geoId})", keyFieldName = "geoId", orderBy = {@EntityOrderBy(fieldName = "geoId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface LinkGeos {}

}
