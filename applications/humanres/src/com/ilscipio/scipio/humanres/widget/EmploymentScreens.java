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
package com.ilscipio.scipio.humanres.widget;

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
public class EmploymentScreens {

    @Screen(name = "FindEmployments", location = "component://humanres/widget/EmploymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResFindEmployment")
    @Action(type = ActionType.SET, field = "roleTypeIdFrom", fromField = "parameters.roleTypeIdFrom")
    @Action(type = ActionType.SET, field = "roleTypeIdTo", fromField = "parameters.roleTypeIdTo")
    @Action(type = ActionType.SET, field = "partyIdFrom", fromField = "parameters.partyIdFrom")
    @Action(type = ActionType.SET, field = "partyIdTo", fromField = "parameters.partyIdTo")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate")
    @Action(type = ActionType.SET, field = "employmentCtx", fromField = "parameters")
    @DecoratorScreen(
        name = "CommonEmploymentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.HumanResNewEmployment}", style = "${styles.link_nav} ${styles.action_add}", target = "EditEmployment"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindEmployments", location = "component://humanres/widget/forms/EmploymentForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListEmploymentsPerson", location = "component://humanres/widget/forms/EmploymentForms.xml"
                        )}))})})
        }
    )
    public interface FindEmployments {}

    @Screen(name = "EditEmployment", location = "component://humanres/widget/EmploymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResEditEmployment")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditEmployment")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Employment", valueField = "employment")
    @DecoratorScreen(
        name = "CommonEmploymentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditEmployment", location = "component://humanres/widget/forms/EmploymentForms.xml"
                )})})
        }
    )
    public interface EditEmployment {}

    @Screen(name = "ListEmployments", location = "component://humanres/widget/EmploymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResListEmployments")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListEmployment")
    @Action(type = ActionType.SET, field = "employmentCtx.partyIdTo", fromField = "parameters.partyId")
    @DecoratorScreen(
        name = "EmployeeDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResEmployment}", includeForms = {
                    @IncludeForm(name = "ListEmploymentsPerson", location = "component://humanres/widget/forms/EmploymentForms.xml"
                )})})
        }
    )
    public interface ListEmployments {}

    @Screen(name = "ListPayHistories", location = "component://humanres/widget/EmploymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResListPayHistories")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditPayHistory")
    @DecoratorScreen(
        name = "CommonEmploymentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListPayHistories", location = "component://humanres/widget/forms/EmploymentForms.xml"
                )})})
        }
    )
    public interface ListPayHistories {}

    @Screen(name = "EditPartyBenefits", location = "component://humanres/widget/EmploymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResListPartyBenefits")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditPartyBenefit")
    @Action(type = ActionType.SET, field = "roleTypeIdFrom", fromField = "parameters.roleTypeIdFrom")
    @Action(type = ActionType.SET, field = "roleTypeIdTo", fromField = "parameters.roleTypeIdTo")
    @Action(type = ActionType.SET, field = "partyIdTo", fromField = "parameters.partyIdTo")
    @Action(type = ActionType.SET, field = "partyIdFrom", fromField = "parameters.partyIdFrom")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp")
    @DecoratorScreen(
        name = "CommonEmploymentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPartyBenefits", location = "component://humanres/widget/forms/EmploymentForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddPartyBenefit}", name = "AddPartyBenefitPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPartyBenefit", location = "component://humanres/widget/forms/EmploymentForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditPartyBenefits {}

    @Screen(name = "EditPayrollPreferences", location = "component://humanres/widget/EmploymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResListPayrollPreferences")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditPayrollPreference")
    @Action(type = ActionType.SET, field = "roleTypeIdFrom", fromField = "parameters.roleTypeIdFrom")
    @Action(type = ActionType.SET, field = "roleTypeIdTo", fromField = "parameters.roleTypeIdTo")
    @Action(type = ActionType.SET, field = "partyIdTo", fromField = "parameters.partyIdTo")
    @Action(type = ActionType.SET, field = "partyIdFrom", fromField = "parameters.partyIdFrom")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "payrollPreferenceSeqId", fromField = "parameters.payrollPreferenceSeqId")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyIdTo")
    @Action(type = ActionType.SET, field = "roleTypeId", fromField = "parameters.roleTypeIdTo")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PayrollPreference", valueField = "payrollPreference")
    @DecoratorScreen(
        name = "CommonEmploymentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPayrollPreferences", location = "component://humanres/widget/forms/EmploymentForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddPayrollPreference}", name = "AddPayrollPreferencePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPayrollPreference", location = "component://humanres/widget/forms/EmploymentForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditPayrollPreferences {}

    @Screen(name = "EditUnemploymentClaims", location = "component://humanres/widget/EmploymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResAddUnemploymentClaim")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditUnemploymentClaims")
    @Action(type = ActionType.SET, field = "unemploymentClaimId", fromField = "parameters.unemploymentClaimId")
    @Action(type = ActionType.SET, field = "roleTypeIdFrom", fromField = "parameters.roleTypeIdFrom")
    @Action(type = ActionType.SET, field = "roleTypeIdTo", fromField = "parameters.roleTypeIdTo")
    @Action(type = ActionType.SET, field = "partyIdTo", fromField = "parameters.partyIdTo")
    @Action(type = ActionType.SET, field = "partyIdFrom", fromField = "parameters.partyIdFrom")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp")
    @Action(type = ActionType.ENTITY_ONE, entityName = "UnemploymentClaim", valueField = "unemploymentClaim")
    @DecoratorScreen(
        name = "CommonEmploymentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListUnemploymentClaims", location = "component://humanres/widget/forms/EmploymentForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddUnemploymentClaim}", name = "AddUnemploymentClaimPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddUnemploymentClaim", location = "component://humanres/widget/forms/EmploymentForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditUnemploymentClaims {}

    @Screen(name = "EditPerformanceNotes", location = "component://humanres/widget/EmploymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResPerfNote")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditPerformanceNotes")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPerformanceNotes", location = "component://humanres/widget/forms/EmploymentForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddPerfNote}", name = "AddPerformanceNotePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPerformanceNote", location = "component://humanres/widget/forms/EmploymentForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditPerformanceNotes {}

    @Screen(name = "EditAgreementEmploymentAppls", location = "component://humanres/widget/EmploymentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResEditAgreementEmploymentAppl")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditAgreementEmploymentAppls")
    @Action(type = ActionType.SET, field = "agreementId", fromField = "parameters.agreementId")
    @Action(type = ActionType.SET, field = "agreementItemSeqId", fromField = "parameters.agreementItemSeqId")
    @Action(type = ActionType.SET, field = "roleTypeIdFrom", fromField = "parameters.roleTypeIdFrom")
    @Action(type = ActionType.SET, field = "roleTypeIdTo", fromField = "parameters.roleTypeIdTo")
    @Action(type = ActionType.SET, field = "partyIdTo", fromField = "parameters.partyIdTo")
    @Action(type = ActionType.SET, field = "partyIdFrom", fromField = "parameters.partyIdFrom")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp")
    @DecoratorScreen(
        name = "CommonEmploymentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListAgreementEmploymentAppls", location = "component://humanres/widget/forms/EmploymentForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonAdd} ${uiLabelMap.HumanResAgreementEmploymentAppl}", name = "AddAgreementEmploymentApplPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddAgreementEmploymentAppl", location = "component://humanres/widget/forms/EmploymentForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditAgreementEmploymentAppls {}

}
