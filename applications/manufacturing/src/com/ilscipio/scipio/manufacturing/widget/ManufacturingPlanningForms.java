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
package com.ilscipio.scipio.manufacturing.widget;

import com.ilscipio.scipio.widget.def.form.*;
import com.ilscipio.scipio.widget.def.screen.SetAction;
import com.ilscipio.scipio.widget.def.screen.ServiceAction;
import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;
import com.ilscipio.scipio.widget.def.screen.ScriptAction;
import com.ilscipio.scipio.widget.def.screen.PropertyToFieldAction;

/**
 * MRP planning forms: MrpRuns list, MrpRun detail event lists, and the MrpProposals
 * filter/list/approve-reject forms.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class ManufacturingPlanningForms {

    @Form(
        name = "ListMrpRuns",
        location = "component://manufacturing/widget/manufacturing/PlanningForms.xml",
        type = FormType.LIST,
        listName = "mrpRuns",
        paginateTarget = "MrpRuns",
        oddRowStyle = "alternate-row",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "mrpId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "MrpRunDetail", description = "${mrpId}", parameters = {@ParameterDef(paramName = "mrpId")})),
            @FormField(name = "mrpName", title = "${uiLabelMap.ManufacturingMrpName}", display = @DisplayField),
            @FormField(name = "facilityId", title = "${uiLabelMap.ProductFacility}", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "startDate", title = "${uiLabelMap.CommonStartDate}", display = @DisplayField),
            @FormField(name = "finishDate", title = "${uiLabelMap.CommonEndDate}", display = @DisplayField),
            @FormField(name = "eventCount", title = "${uiLabelMap.ManufacturingEventCount}", display = @DisplayField),
            @FormField(name = "proposedProductionRuns", title = "${uiLabelMap.ManufacturingProposedProductionRuns}", display = @DisplayField),
            @FormField(name = "proposedPurchases", title = "${uiLabelMap.ManufacturingProposedPurchases}", display = @DisplayField),
            @FormField(name = "errorCount", title = "${uiLabelMap.ManufacturingErrorCount}", display = @DisplayField),
            @FormField(name = "runByUserLoginId", title = "${uiLabelMap.ManufacturingRunByUser}", display = @DisplayField)
        }
    )
    public interface ListMrpRuns {}

    @Form(
        name = "ListMrpRunEvents",
        location = "component://manufacturing/widget/manufacturing/PlanningForms.xml",
        type = FormType.LIST,
        listName = "mrpEvents",
        oddRowStyle = "alternate-row",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditProductBom", description = "${productId}", parameters = {@ParameterDef(paramName = "productId")})),
            @FormField(name = "eventDate", title = "${uiLabelMap.CommonDate}", display = @DisplayField),
            @FormField(name = "mrpEventTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "MrpEventType", description = "${description}")),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}", display = @DisplayField),
            @FormField(name = "eventName", title = "${uiLabelMap.CommonName}", display = @DisplayField),
            @FormField(name = "isLate", title = "${uiLabelMap.ManufacturingIsLate}", display = @DisplayField),
            @FormField(name = "facilityId", title = "${uiLabelMap.ProductFacility}", display = @DisplayField)
        }
    )
    public interface ListMrpRunEvents {}

    @Form(
        name = "ListMrpRunErrorEvents",
        location = "component://manufacturing/widget/manufacturing/PlanningForms.xml",
        type = FormType.LIST,
        listName = "mrpErrorEvents",
        oddRowStyle = "alternate-row",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditProductBom", description = "${productId}", parameters = {@ParameterDef(paramName = "productId")})),
            @FormField(name = "eventDate", title = "${uiLabelMap.CommonDate}", display = @DisplayField),
            @FormField(name = "mrpEventTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "MrpEventType", description = "${description}")),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}", display = @DisplayField),
            @FormField(name = "eventName", title = "${uiLabelMap.CommonName}", display = @DisplayField),
            @FormField(name = "facilityId", title = "${uiLabelMap.ProductFacility}", display = @DisplayField)
        }
    )
    public interface ListMrpRunErrorEvents {}

    @Form(
        name = "FilterMrpProposals",
        location = "component://manufacturing/widget/manufacturing/PlanningForms.xml",
        target = "MrpProposals",
        method = "get",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "facilityId", title = "${uiLabelMap.ProductFacility}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]"))),
            @FormField(name = "requirementTypeId", title = "${uiLabelMap.ManufacturingRequirementType}", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "INTERNAL_REQUIREMENT", description = "${uiLabelMap.ManufacturingProposedProductionRuns}"), @Option(key = "PRODUCT_REQUIREMENT", description = "${uiLabelMap.ManufacturingProposedPurchases}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FilterMrpProposals {}

    @Form(
        name = "ListMrpProposals",
        location = "component://manufacturing/widget/manufacturing/PlanningForms.xml",
        type = FormType.LIST,
        listName = "proposals",
        paginateTarget = "MrpProposals",
        oddRowStyle = "alternate-row",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "requirementId", title = "${uiLabelMap.OrderRequirement}", display = @DisplayField),
            @FormField(name = "requirementTypeId", title = "${uiLabelMap.ManufacturingRequirementType}", useWhen = "requirementTypeId == 'INTERNAL_REQUIREMENT'", display = @DisplayField(description = "${uiLabelMap.ManufacturingMake}")),
            @FormField(name = "requirementTypeId", title = "${uiLabelMap.ManufacturingRequirementType}", useWhen = "requirementTypeId == 'PRODUCT_REQUIREMENT'", display = @DisplayField(description = "${uiLabelMap.ManufacturingBuy}")),
            @FormField(name = "requirementTypeId", title = "${uiLabelMap.ManufacturingRequirementType}", useWhen = "requirementTypeId != 'INTERNAL_REQUIREMENT' && requirementTypeId != 'PRODUCT_REQUIREMENT'", displayEntity = @DisplayEntityField(entityName = "RequirementType", description = "${description}")),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditProductBom", description = "${productId}", parameters = {@ParameterDef(paramName = "productId")})),
            @FormField(name = "productFacilitiesAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_view}", hyperlink = @HyperlinkField(target = "/catalog/control/EditProductFacilities", urlMode = UrlMode.INTER_APP, description = "${uiLabelMap.ManufacturingStockSettings}", parameters = {@ParameterDef(paramName = "productId")})),
            @FormField(name = "supplierName", title = "${uiLabelMap.ManufacturingSupplier}", useWhen = "\"PRODUCT_REQUIREMENT\".equals(requirementTypeId)", display = @DisplayField),
            @FormField(name = "createPoAction", title = " ", useWhen = "\"PRODUCT_REQUIREMENT\".equals(requirementTypeId) && \"REQ_APPROVED\".equals(statusId) && supplierPartyId != null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", hyperlink = @HyperlinkField(target = "/ordermgr/control/ApprovedProductRequirements", urlMode = UrlMode.INTER_APP, description = "${uiLabelMap.ManufacturingCreatePurchaseOrder}", parameters = {@ParameterDef(paramName = "partyId", fromField = "supplierPartyId")})),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}", display = @DisplayField),
            @FormField(name = "requirementStartDate", title = "${uiLabelMap.OrderRequirementStartDate}", display = @DisplayField),
            @FormField(name = "requiredByDate", title = "${uiLabelMap.ManufacturingRequiredByDate}", display = @DisplayField),
            @FormField(name = "facilityId", title = "${uiLabelMap.ProductFacility}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "approveAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", hyperlink = @HyperlinkField(target = "approveMrpProposal", description = "${uiLabelMap.CommonApprove}", parameters = {@ParameterDef(paramName = "requirementId"), @ParameterDef(paramName = "statusId", value = "REQ_APPROVED")})),
            @FormField(name = "rejectAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "rejectMrpProposal", description = "${uiLabelMap.ManufacturingReject}", parameters = {@ParameterDef(paramName = "requirementId"), @ParameterDef(paramName = "statusId", value = "REQ_REJECTED")}))
        }
    )
    public interface ListMrpProposals {}

}
