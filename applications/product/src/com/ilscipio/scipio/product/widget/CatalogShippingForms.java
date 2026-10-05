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
public class CatalogShippingForms {

    @Form(
        name = "ListQuantityBreaks",
        location = "component://product/widget/catalog/ShippingForms.xml",
        type = FormType.LIST,
        listName = "quantityBreaks",
        paginateTarget = "ListQuantityBreaks",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "QuantityBreak", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "quantityBreakId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "ListQuantityBreaks", description = "${quantityBreakId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "quantityBreakId")})),
            @FormField(name = "quantityBreakTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "QuantityBreakType", alsoHidden = false)),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteQuantityBreak", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "quantityBreakId")}))
        }
    )
    public interface ListQuantityBreaks {}

    @Form(
        name = "EditQuantityBreak",
        location = "component://product/widget/catalog/ShippingForms.xml",
        target = "createQuantityBreak",
        defaultMapName = "quantityBreak",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "QuantityBreak", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "quantityBreakId", hidden = @HiddenField),
            @FormField(name = "quantityBreakTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "QuantityBreakType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "quantityBreak!=null", target = "updateQuantityBreak")
        }
    )
    public interface EditQuantityBreak {}

    @Form(
        name = "ListShipmentMethodTypes",
        location = "component://product/widget/catalog/ShippingForms.xml",
        type = FormType.LIST,
        listName = "shipmentMethodTypes",
        paginateTarget = "ListShipmentMethodTypes",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ShipmentMethodType", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "shipmentMethodTypeId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "ListShipmentMethodTypes", description = "${shipmentMethodTypeId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "shipmentMethodTypeId")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteShipmentMethodType", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "shipmentMethodTypeId")}))
        }
    )
    public interface ListShipmentMethodTypes {}

    @Form(
        name = "EditShipmentMethodType",
        location = "component://product/widget/catalog/ShippingForms.xml",
        target = "createShipmentMethodType",
        defaultMapName = "shipmentMethodType",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createShipmentMethodType", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "shipmentMethodTypeId", useWhen = "shipmentMethodType!=null", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "shipmentMethodType!=null", target = "updateShipmentMethodType")
        }
    )
    public interface EditShipmentMethodType {}

    @Form(
        name = "ListCarrierShipmentMethods",
        location = "component://product/widget/catalog/ShippingForms.xml",
        type = FormType.LIST,
        listName = "carrierShipmentMethods",
        paginateTarget = "ListCarrierShipmentMethods",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "CarrierShipmentMethod", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "shipmentMethodTypeId", displayEntity = @DisplayEntityField(entityName = "ShipmentMethodType", alsoHidden = false)),
            @FormField(name = "roleTypeId", displayEntity = @DisplayEntityField(entityName = "RoleType", alsoHidden = false)),
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "ListCarrierShipmentMethods", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "shipmentMethodTypeId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteCarrierShipmentMethod", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "shipmentMethodTypeId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId")}))
        }
    )
    public interface ListCarrierShipmentMethods {}

    @Form(
        name = "EditCarrierShipmentMethod",
        location = "component://product/widget/catalog/ShippingForms.xml",
        target = "createCarrierShipmentMethod",
        defaultMapName = "carrierShipmentMethod",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createCarrierShipmentMethod", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "shipmentMethodTypeId", useWhen = "carrierShipmentMethod==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ShipmentMethodType", description = "${description} [${shipmentMethodTypeId}]", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "shipmentMethodTypeId", useWhen = "carrierShipmentMethod!=null", display = @DisplayField),
            @FormField(name = "partyId", useWhen = "carrierShipmentMethod!=null", display = @DisplayField),
            @FormField(name = "partyId", useWhen = "carrierShipmentMethod==null", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", useWhen = "carrierShipmentMethod!=null", display = @DisplayField),
            @FormField(name = "roleTypeId", useWhen = "carrierShipmentMethod==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "sequenceNumber", tooltip = "${uiLabelMap.ProductUsedForDisplayOrdering}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "carrierShipmentMethod!=null", target = "updateCarrierShipmentMethod")
        }
    )
    public interface EditCarrierShipmentMethod {}

    @Form(
        name = "NewCarrier",
        location = "component://product/widget/catalog/ShippingForms.xml",
        target = "createCarrier",
        extendsForm = "EditPartyGroup",
        extendsResource = "component://party/widget/partymgr/PartyForms.xml",
        fields = {
            @FormField(name = "roleTypeId", mapName = "carrierRole", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "cancelAction", title = " ", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "ListCarrierShipmentMethods", description = "${uiLabelMap.CommonCancelDone}", alsoHidden = false))
        },
        altTargets = {
            @AltTarget(useWhen = "true", target = "createCarrier")
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "RoleType", valueField = "carrierRole")})
    )
    public interface NewCarrier {}

}
