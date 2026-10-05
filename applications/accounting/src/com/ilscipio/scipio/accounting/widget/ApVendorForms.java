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
package com.ilscipio.scipio.accounting.widget;

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
public class ApVendorForms {

    @Form(
        name = "ListVendors",
        location = "component://accounting/widget/ap/VendorForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "findVendors",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "editVendor", description = "${partyId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId")})),
            @FormField(name = "manifestCompanyName", title = "${uiLabelMap.PartyManifestCompanyName}", display = @DisplayField),
            @FormField(name = "manifestCompanyTitle", title = "${uiLabelMap.PartyManifestCompanyTitle}", display = @DisplayField),
            @FormField(name = "manifestLogoUrl", title = "${uiLabelMap.PartyManifestLogoUrl}", display = @DisplayField),
            @FormField(name = "manifestPolicies", title = "${uiLabelMap.PartyManifestPolicies}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Vendor"), @FieldMap(fieldName = "orderBy", value = "partyId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListVendors {}

    @Form(
        name = "EditVendor",
        location = "component://accounting/widget/ap/VendorForms.xml",
        target = "updateVendor",
        defaultMapName = "vendor",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", useWhen = "partyId==null", requiredField = true, lookup = @LookupField(targetFormName = "LookupPerson")),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", useWhen = "partyId!=null&&vendor!=null", display = @DisplayField),
            @FormField(name = "manifestCompanyName", title = "${uiLabelMap.PartyManifestCompanyName}", useWhen = "partyId==null", text = @TextField),
            @FormField(name = "manifestCompanyName", title = "${uiLabelMap.PartyManifestCompanyName}", useWhen = "partyId!=null&&vendor!=null", text = @TextField(defaultValue = "${parameters.manifestCompanyName}")),
            @FormField(name = "manifestCompanyTitle", title = "${uiLabelMap.PartyManifestCompanyTitle}", useWhen = "partyId==null", text = @TextField),
            @FormField(name = "manifestCompanyTitle", title = "${uiLabelMap.PartyManifestCompanyTitle}", useWhen = "partyId!=null&&vendor!=null", text = @TextField(defaultValue = "${parameters.manifestCompanyTitle}")),
            @FormField(name = "manifestLogoUrl", title = "${uiLabelMap.PartyManifestLogoUrl}", useWhen = "partyId==null", text = @TextField),
            @FormField(name = "manifestLogoUrl", title = "${uiLabelMap.PartyManifestLogoUrl}", useWhen = "partyId!=null&&vendor!=null", text = @TextField(defaultValue = "${parameters.manifestLogoUrl}")),
            @FormField(name = "manifestPolicies", title = "${uiLabelMap.PartyManifestPolicies}", useWhen = "partyId==null", text = @TextField),
            @FormField(name = "manifestPolicies", title = "${uiLabelMap.PartyManifestPolicies}", useWhen = "partyId!=null&&vendor!=null", text = @TextField(defaultValue = "${parameters.manifestPolicies}")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "partyId==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "partyId!=null&&vendor!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "partyId==null", target = "createVendor"),
            @AltTarget(useWhen = "partyId!=null&&vendor!=null", target = "updateVendor")
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "Vendor", valueField = "vendor")})
    )
    public interface EditVendor {}

    @Form(
        name = "FindVendors",
        location = "component://accounting/widget/ap/VendorForms.xml",
        target = "findVendors",
        title = "Find and List Vendors",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "partyId", parameterName = "partyId", title = "${uiLabelMap.PartyVendor} ${uiLabelMap.CommonId}", textFind = @TextFindField),
            @FormField(name = "manifestCompanyName", parameterName = "manifestCompanyName", title = "${uiLabelMap.PartyManifestCompanyName}", textFind = @TextFindField),
            @FormField(name = "manifestCompanyTitle", parameterName = "manifestCompanyTitle", title = "${uiLabelMap.PartyManifestCompanyTitle}", textFind = @TextFindField),
            @FormField(name = "manifestLogoUrl", parameterName = "manifestLogoUrl", title = "${uiLabelMap.PartyManifestLogoUrl}", textFind = @TextFindField),
            @FormField(name = "manifestPolicies", parameterName = "manifestPolicies", title = "${uiLabelMap.PartyManifestPolicies}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "find", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindVendors {}

    @Form(
        name = "FindApPayments",
        location = "component://accounting/widget/ap/VendorForms.xml",
        target = "FindApPayments",
        extendsForm = "FindPayments",
        extendsResource = "component://accounting/widget/payments/PaymentForms.xml",
        fields = {
            @FormField(name = "parentTypeId", hidden = @HiddenField(value = "DISBURSEMENT")),
            @FormField(name = "paymentTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PaymentType", description = "${description}", constraints = {@EntityConstraint(name = "parentTypeId", value = "DISBURSEMENT")})))
        }
    )
    public interface FindApPayments {}

    @Form(
        name = "FindApPaymentGroups",
        location = "component://accounting/widget/ap/VendorForms.xml",
        target = "FindApPaymentGroups",
        extendsForm = "FindPaymentGroup",
        extendsResource = "component://accounting/widget/payments/PaymentGroupForms.xml",
        fields = {
            @FormField(name = "paymentGroupTypeId", hidden = @HiddenField(value = "CHECK_RUN"))
        }
    )
    public interface FindApPaymentGroups {}

}
