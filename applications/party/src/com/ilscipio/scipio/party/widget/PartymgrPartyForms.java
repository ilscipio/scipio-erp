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
public class PartymgrPartyForms {

    @Form(
        name = "EditPerson",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "updatePerson",
        focusFieldName = "salutation",
        defaultMapName = "personInfo",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePerson")
        },
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "personInfo!=null", display = @DisplayField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", tooltip = "${uiLabelMap.CommonIdGeneratedIfEmpty}", useWhen = "personInfo==null&&partyId==null", text = @TextField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", tooltip = "${uiLabelMap.CommonCannotBeFound}: [${partyId}]", useWhen = "personInfo==null&&partyId!=null", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "firstName", title = "${uiLabelMap.PartyFirstName}", requiredField = true, text = @TextField(size = 40, maxlength = 60)),
            @FormField(name = "lastName", title = "${uiLabelMap.PartyLastName}", requiredField = true, text = @TextField(size = 40, maxlength = 60)),
            @FormField(name = "gender", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "M", description = "${uiLabelMap.CommonMale}"), @Option(key = "F", description = "${uiLabelMap.CommonFemale}")})),
            @FormField(name = "maritalStatus", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "S", description = "${uiLabelMap.PartyMaritalStatusSingle}"), @Option(key = "M", description = "${uiLabelMap.PartyMaritalStatusMarried}"), @Option(key = "P", description = "${uiLabelMap.PartyMaritalStatusSeparated}"), @Option(key = "D", description = "${uiLabelMap.PartyMaritalStatusDivorced}"), @Option(key = "W", description = "${uiLabelMap.PartyMaritalStatusWidowed}")})),
            @FormField(name = "employmentStatusEnumId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description} [${enumCode}]", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "EMPLOY_STTS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "residenceStatusEnumId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description} [${enumCode}]", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "PTY_RESID_STTS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "existingCustomer", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "preferredCurrencyUomId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "person==null", hidden = @HiddenField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "person!=null && leadDescription==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "PARTY_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "person!=null && leadDescription!=null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusId", value = "LEAD_ASSIGNED,PARTY_ENABLED,PARTY_DISABLED", operator = "in")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "cancelAction", title = " ", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "${donePage}", description = "${uiLabelMap.CommonCancelDone}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId")}))
        },
        altTargets = {
            @AltTarget(useWhen = "personInfo==null", target = "createPerson")
        }
    )
    public interface EditPerson {}

    @Form(
        name = "EditPartyGroup",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "updatePartyGroup",
        focusFieldName = "groupName",
        defaultMapName = "partyGroup",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePartyGroup")
        },
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "partyGroup!=null", display = @DisplayField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", tooltip = "${uiLabelMap.CommonIdGeneratedIfEmpty}", useWhen = "partyGroup==null&&partyId==null", text = @TextField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", tooltip = "${uiLabelMap.CommonCannotBeFound}: [${partyId}]", useWhen = "partyGroup==null&&partyId!=null", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "groupName", requiredField = true),
            @FormField(name = "partyTypeId", ignored = @IgnoredField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField),
            @FormField(name = "preferredCurrencyUomId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "partyGroup==null", hidden = @HiddenField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "partyGroup!=null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "PARTY_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "cancelAction", title = " ", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "${donePage}", description = "${uiLabelMap.CommonCancelDone}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId")}))
        },
        altTargets = {
            @AltTarget(useWhen = "partyGroup==null", target = "createPartyGroup")
        }
    )
    public interface EditPartyGroup {}

    @Form(
        name = "AddUserLogin",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "createUserLogin",
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
            @FormField(name = "cancelAction", title = " ", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "backHome", description = "${uiLabelMap.CommonCancelDone}", alsoHidden = false))
        }
    )
    public interface AddUserLogin {}

    @Form(
        name = "updatePassword",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "updatePassword",
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
            @FormField(name = "cancelAction", title = " ", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "${donePage}", description = "${uiLabelMap.CommonCancelDone}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId")}))
        },
        actions = @FormActions(set = {@SetAction(field = "passwordHint", fromField = "editUserLogin.passwordHint")})
    )
    public interface updatePassword {}

    @Form(
        name = "updateUserLoginSecurity",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "updateUserLoginSecurity",
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
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link")),
            @FormField(name = "cancelAction", title = " ", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "${donePage}", description = "${uiLabelMap.CommonCancelDone}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId")}))
        }
    )
    public interface updateUserLoginSecurity {}

    @Form(
        name = "EditVendor",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "updateVendor",
        focusFieldName = "manifestCompanyName",
        defaultMapName = "vendor",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateVendor", mapName = "vendor")
        },
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField(value = "${partyId}")),
            @FormField(name = "manifestCompanyName", title = "${uiLabelMap.PartyManifestCompanyName}"),
            @FormField(name = "manifestCompanyTitle", title = "${uiLabelMap.PartyManifestCompanyTitle}"),
            @FormField(name = "manifestLogoUrl", title = "${uiLabelMap.PartyManifestLogoUrl}"),
            @FormField(name = "manifestPolicies", title = "${uiLabelMap.PartyManifestPolicies}", textarea = @TextareaField(rows = 15)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "vendor==null", target = "createVendor")
        }
    )
    public interface EditVendor {}

    @Form(
        name = "PartyLink",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "setPartyLink",
        focusFieldName = "partyId",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdTo", position = 2, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "submitButton", title = "${uiLabelMap.PartyLink}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(confirmationMessage = "${uiLabelMap.PartyLinkMessage1}", requestConfirmation = true))
        }
    )
    public interface PartyLink {}

    @Form(
        name = "PartyAction",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "setPartyAction",
        focusFieldName = "partyId",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdTo", position = 2, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "submitAction", title = "${uiLabelMap.PartyLink}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(confirmationMessage = "${uiLabelMap.PartyLinkMessage1}", requestConfirmation = true))
        }
    )
    public interface PartyAction {}

    @Form(
        name = "AddPartyRelationshipType",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "createPartyRelationshipType",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createPartyRelationshipType")
        },
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "hasTable", hidden = @HiddenField),
            @FormField(name = "parentTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyRelationshipType", description = "${partyRelationshipName}", keyFieldName = "partyRelationshipTypeId", orderBy = {@EntityOrderBy(fieldName = "partyRelationshipName")}))),
            @FormField(name = "roleTypeIdValidFrom", title = "${uiLabelMap.PartyRelationshipValidFromRoleType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "roleTypeIdValidTo", title = "${uiLabelMap.PartyRelationshipValidToRoleType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddPartyRelationshipType {}

    @Form(
        name = "AddPartyTaxAuthInfo",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "createPartyTaxAuthInfo",
        focusFieldName = "taxAuthGeoId",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createPartyTaxAuthInfo")
        },
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "taxAuthGeoId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Geo", description = "${uiLabelMap.CommonCountry}: [${geoId}] ${geoName}", keyFieldName = "geoId", constraints = {@EntityConstraint(name = "geoTypeId", value = "COUNTRY")}, orderBy = {@EntityOrderBy(fieldName = "geoId")}))),
            @FormField(name = "taxAuthPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "isExempt", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "isNexus", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddPartyTaxAuthInfo {}

    @Form(
        name = "UpdatePartyTaxAuthInfo",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.LIST,
        target = "updatePartyTaxAuthInfo",
        listName = "partyTaxInfos",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePartyTaxAuthInfo")
        },
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "taxAuthGeoId", displayEntity = @DisplayEntityField(entityName = "Geo", keyFieldName = "geoId", description = "[${geoId}] ${geoName}")),
            @FormField(name = "taxAuthPartyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "viewprofile", description = "${taxAuthPartyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "taxAuthPartyId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFromTime}", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePartyTaxAuthInfo", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "taxAuthPartyId"), @ParameterDef(paramName = "taxAuthGeoId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "isExempt", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "isNexus", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")}))
        }
    )
    public interface UpdatePartyTaxAuthInfo {}

    @Form(
        name = "AddPartyNote",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "createPartyNote",
        focusFieldName = "noteId",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createPartyNote")
        },
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "noteId", tooltip = "${uiLabelMap.PartyOptional}", hidden = @HiddenField),
            @FormField(name = "noteName", title = "${uiLabelMap.FormFieldTitle_noteName}", tooltip = "${uiLabelMap.PartyOptional}"),
            @FormField(name = "note", textarea = @TextareaField(cols = 70, rows = 10)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link")),
            @FormField(name = "cancelAction", title = " ", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "${donePage}", description = "${uiLabelMap.CommonCancelDone}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId")}))
        }
    )
    public interface AddPartyNote {}

    @Form(
        name = "AddPartyRate",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "updatePartyRate",
        focusFieldName = "rateTypeId",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "rateTypeId", title = "${uiLabelMap.WorkEffortTimesheetRateType}", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RateType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "periodTypeId", position = 2, displayEntity = @DisplayEntityField(entityName = "PeriodType")),
            @FormField(name = "rateAmount", tooltip = "${uiLabelMap.PartyOverrideDefaultRateAmount}", text = @TextField),
            @FormField(name = "rateCurrencyUomId", entryName = "defaultCurrencyUomId", title = "${uiLabelMap.ProductCurrencyUomId}", tooltip = "${uiLabelMap.PartyAdjustInAccountingComponent}", position = 2, displayEntity = @DisplayEntityField(entityName = "Uom", keyFieldName = "uomId")),
            @FormField(name = "percentageUsed", text = @TextField(size = 6)),
            @FormField(name = "defaultRate", position = 2, dropDown = @DropDownField(options = {@Option(key = "N"), @Option(key = "Y")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "periodTypeId", value = "RATE_HOUR")})
    )
    public interface AddPartyRate {}

    @Form(
        name = "ListPartyRates",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.LIST,
        target = "deletePartyRate",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "fromDate", hidden = @HiddenField),
            @FormField(name = "rateAmountFromDate", hidden = @HiddenField),
            @FormField(name = "rateCurrencyUomId", hidden = @HiddenField),
            @FormField(name = "rateTypeId", title = "${uiLabelMap.WorkEffortTimesheetRateType}", displayEntity = @DisplayEntityField(entityName = "RateType")),
            @FormField(name = "periodTypeId", displayEntity = @DisplayEntityField(entityName = "PeriodType", description = "${description}")),
            @FormField(name = "rateDefaultAmount", tooltip = "${uiLabelMap.PartyValueFromWorkEffortLevel}", useWhen = "\"workEffort\".equals(rateLevel)", display = @DisplayField(type = "currency")),
            @FormField(name = "rateDefaultAmount", tooltip = "${uiLabelMap.PartyValueFromPartyLevel}", useWhen = "\"party\".equals(rateLevel)", display = @DisplayField(type = "currency")),
            @FormField(name = "rateDefaultAmount", tooltip = "${uiLabelMap.PartyValueFromRateTypeLevel}", useWhen = "\"rateType\".equals(rateLevel)", display = @DisplayField(type = "currency")),
            @FormField(name = "rateDefaultAmount", tooltip = "${uiLabelMap.PartyRateNotSpecified}", useWhen = "rateLevel==null", display = @DisplayField),
            @FormField(name = "defaultRate", display = @DisplayField),
            @FormField(name = "percentageUsed", display = @DisplayField),
            @FormField(name = "deleteAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        },
        rowActions = @RowActions(set = {@SetAction(field = "rateDefaultAmount", fromField = "rateResult.rateAmount"), @SetAction(field = "periodTypeId", fromField = "rateResult.periodTypeId"), @SetAction(field = "rateCurrencyUomId", fromField = "rateResult.rateCurrencyUomId"), @SetAction(field = "rateLevel", fromField = "rateResult.level"), @SetAction(field = "rateAmountFromDate", fromField = "rateResult.fromDate")}, service = {@ServiceAction(serviceName = "getRateAmount", resultMapName = "rateResult")})
    )
    public interface ListPartyRates {}

    @Form(
        name = "NewUser",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "${target}${previousParams}",
        focusFieldName = "USER_TITLE",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "USER_PARTY_ID", title = "${uiLabelMap.PartyPartyId}", tooltip = "${uiLabelMap.CommonIdGeneratedIfEmpty}", text = @TextField),
            @FormField(name = "USE_ADDRESS", hidden = @HiddenField(value = "${USE_ADDRESS}")),
            @FormField(name = "require_email", hidden = @HiddenField(value = "${require_email}")),
            @FormField(name = "USER_TITLE", title = "${uiLabelMap.CommonTitle}", text = @TextField(size = 10, maxlength = 30)),
            @FormField(name = "USER_FIRST_NAME", title = "${uiLabelMap.PartyFirstName}", requiredField = true, text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "USER_MIDDLE_NAME", title = "${uiLabelMap.PartyMiddleInitial}", text = @TextField(size = 4, maxlength = 4)),
            @FormField(name = "USER_LAST_NAME", title = "${uiLabelMap.PartyLastName}", requiredField = true, text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "USER_SUFFIX", title = "${uiLabelMap.PartySuffix}", text = @TextField(size = 10, maxlength = 30)),
            @FormField(name = "ShippingAddressTitle", title = "${uiLabelMap.PartyAddressMailingShipping}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "USER_ADDRESS1", title = "${uiLabelMap.CommonAddress1}", requiredField = true, text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "USER_ADDRESS2", title = "${uiLabelMap.CommonAddress2}", text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "USER_CITY", title = "${uiLabelMap.CommonCity}", requiredField = true, text = @TextField(size = 30, maxlength = 60)),
            @FormField(name = "USER_POSTAL_CODE", title = "${uiLabelMap.CommonZipPostalCode}", requiredField = true, text = @TextField(size = 10, maxlength = 30)),
            @FormField(name = "USER_COUNTRY", title = "${uiLabelMap.CommonCountry}", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Geo", description = "${geoName}", keyFieldName = "geoId", constraints = {@EntityConstraint(name = "geoTypeId", value = "COUNTRY")}, orderBy = {@EntityOrderBy(fieldName = "geoId")}))),
            @FormField(name = "USER_STATE", title = "${uiLabelMap.CommonState}", dropDown = @DropDownField(allowEmpty = true)),
            @FormField(name = "USER_ADDRESS_ALLOW_SOL", title = "${uiLabelMap.PartyContactAllowAddressSolicitation}?", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "HomePhoneTitle", title = "${uiLabelMap.PartyHomePhone}", titleAreaStyle = "group-label", widgetStyle = "tooltip", display = @DisplayField(description = "${uiLabelMap.PartyPhoneNumberRequired}", alsoHidden = false)),
            @FormField(name = "USER_HOME_COUNTRY", title = "${uiLabelMap.CommonCountryCode}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "USER_HOME_AREA", title = "${uiLabelMap.PartyAreaCode}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "USER_HOME_CONTACT", title = "${uiLabelMap.PartyPhoneNumber}", text = @TextField(size = 15, maxlength = 15)),
            @FormField(name = "USER_HOME_EXT", title = "${uiLabelMap.PartyContactExt}", text = @TextField(size = 6, maxlength = 10)),
            @FormField(name = "USER_HOME_ALLOW_SOL", title = "${uiLabelMap.PartyContactAllowSolicitation}?", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "WorkPhoneTitle", title = "${uiLabelMap.PartyContactWorkPhoneNumber}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "USER_WORK_COUNTRY", title = "${uiLabelMap.CommonCountryCode}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "USER_WORK_AREA", title = "${uiLabelMap.PartyAreaCode}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "USER_WORK_CONTACT", title = "${uiLabelMap.PartyPhoneNumber}", text = @TextField(size = 15, maxlength = 15)),
            @FormField(name = "USER_WORK_EXT", title = "${uiLabelMap.PartyContactExt}", text = @TextField(size = 6, maxlength = 10)),
            @FormField(name = "USER_WORK_ALLOW_SOL", title = "${uiLabelMap.PartyContactAllowSolicitation}?", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "FaxPhoneTitle", title = "${uiLabelMap.PartyContactFaxPhoneNumber}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "USER_FAX_COUNTRY", title = "${uiLabelMap.CommonCountryCode}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "USER_FAX_AREA", title = "${uiLabelMap.PartyAreaCode}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "USER_FAX_CONTACT", title = "${uiLabelMap.PartyPhoneNumber}", text = @TextField(size = 15, maxlength = 15)),
            @FormField(name = "USER_FAX_EXT", title = "${uiLabelMap.PartyContactExt}", text = @TextField(size = 6, maxlength = 10)),
            @FormField(name = "USER_FAX_ALLOW_SOL", title = "${uiLabelMap.PartyContactAllowSolicitation}?", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "MobilePhoneTitle", title = "${uiLabelMap.PartyContactMobilePhoneNumber}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "USER_MOBILE_COUNTRY", title = "${uiLabelMap.CommonCountryCode}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "USER_MOBILE_AREA", title = "${uiLabelMap.PartyAreaCode}", text = @TextField(size = 4, maxlength = 10)),
            @FormField(name = "USER_MOBILE_CONTACT", title = "${uiLabelMap.PartyPhoneNumber}", text = @TextField(size = 15, maxlength = 15)),
            @FormField(name = "USER_MOBILE_ALLOW_SOL", title = "${uiLabelMap.PartyContactAllowSolicitation}?", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "EmailAddressTitle", title = "${uiLabelMap.PartyEmailAddress}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "USER_EMAIL", title = "${uiLabelMap.CommonEmail}", useWhen = "require_email!=null", requiredField = true, text = @TextField(size = 60)),
            @FormField(name = "USER_EMAIL", title = "${uiLabelMap.CommonEmail}", useWhen = "require_email==null", requiredField = true, text = @TextField(size = 60)),
            @FormField(name = "USER_EMAIL_ALLOW_SOL", title = "${uiLabelMap.PartyContactAllowSolicitation}?", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "USERNAME", title = "${uiLabelMap.CommonUsername}", useWhen = "displayPassword==true", requiredField = true, text = @TextField(size = 30)),
            @FormField(name = "PASSWORD", title = "${uiLabelMap.CommonPassword}", useWhen = "displayPassword==true", requiredField = true, password = @PasswordField(size = 15)),
            @FormField(name = "CONFIRM_PASSWORD", title = "${uiLabelMap.PartyRepeatPassword}", useWhen = "displayPassword==true", requiredField = true, password = @PasswordField(size = 15)),
            @FormField(name = "USERNAME", title = "${uiLabelMap.CommonUsername}", tooltip = "* ${uiLabelMap.PartyTemporaryPassword}", useWhen = "displayPassword!=true", requiredField = true, text = @TextField(size = 30)),
            @FormField(name = "PRODUCT_STORE_ID", title = "Product Store", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductStore", description = "${storeName} (${productStoreId})", keyFieldName = "productStoreId", orderBy = {@EntityOrderBy(fieldName = "productStoreId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface NewUser {}

    @Form(
        name = "ListSegmentRoles",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.LIST,
        target = "updateSegmentGroupRole",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "segmentGroupId", title = "${uiLabelMap.PartySegmentGroupId}", displayEntity = @DisplayEntityField(entityName = "SegmentGroup", subHyperlink = @SubHyperlink(target = "/marketing/control/viewSegmentGroup", description = "${segmentGroupId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "segmentGroupId")}))),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.PartyRole}", displayEntity = @DisplayEntityField(entityName = "RoleType", description = "${description}")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteSegmentGroupRole", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "segmentGroupId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId")}))
        }
    )
    public interface ListSegmentRoles {}

    @Form(
        name = "AddSegmentRole",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "createSegmentRole",
        focusFieldName = "segmentGroupId",
        defaultMapName = "segmentGroupRole",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "segmentGroupId", title = "${uiLabelMap.PartySegmentGroupId}", lookup = @LookupField(targetFormName = "LookupSegmentGroup")),
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface AddSegmentRole {}

    @Form(
        name = "EditPartyAttribute",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "updatePartyAttribute",
        focusFieldName = "attrName",
        defaultMapName = "attribute",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePartyAttribute", mapName = "attribute")
        },
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField(value = "${partyId}")),
            @FormField(name = "attrName", tooltip = "${uiLabelMap.PartyNotModifRecreateAttribute}", useWhen = "attribute!=null", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "cancelAction", title = " ", widgetStyle = "${styles.link_nav_cancel}", hyperlink = @HyperlinkField(target = "${cancelPage}", description = "${uiLabelMap.CommonCancelDone}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId")}))
        },
        altTargets = {
            @AltTarget(useWhen = "attribute==null", target = "createPartyAttribute")
        }
    )
    public interface EditPartyAttribute {}

    @Form(
        name = "AddPartyContent",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.UPLOAD,
        target = "updatePartyContent",
        focusFieldName = "contentTypeId",
        defaultMapName = "content",
        headerRowStyle = "header-row",
        attribs = "{'showProgress':'${showProgress}', 'progressSuccessAction':'${progressSuccessAction}', 'progressOptions':${groovy: context.progressOptions ?: \"''\"} }",
        fields = {
            @FormField(name = "partyId", mapName = "parameters", hidden = @HiddenField),
            @FormField(name = "dataResourceId", mapName = "dataResource", useWhen = "content!=null", hidden = @HiddenField),
            @FormField(name = "contentId", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "content!=null", display = @DisplayField),
            @FormField(name = "partyContentTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyContentType", description = "${description}"))),
            @FormField(name = "contentTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ContentType", description = "${description}"))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "content==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "CONTENT_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "content!=null", dropDown = @DropDownField(currentDescription = "${currentStatus.description}", entityOptions = @EntityOptions(entityName = "StatusValidChangeToDetail", description = "${transitionName} (${description})", keyFieldName = "statusIdTo", constraints = {@EntityConstraint(name = "statusId", value = "${content.statusId}")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "dataResourceName", title = "${uiLabelMap.CommonUpload}", file = @FileField),
            @FormField(name = "description", textarea = @TextareaField),
            @FormField(name = "mimeTypeId", tooltip = "${uiLabelMap.CommonLeaveEmptyToAutoDetermine}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "MimeType", description = "${description}"))),
            @FormField(name = "dataCategoryId", useWhen = "dataResource==null", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "DataCategory", description = "${categoryName}"))),
            @FormField(name = "dataCategoryId", mapName = "dataResource", useWhen = "dataResource!=null", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "DataCategory", description = "${categoryName}"))),
            @FormField(name = "isPublic", mapName = "dataResource", title = "${uiLabelMap.PartyIsPublic}", dropDown = @DropDownField(options = {@Option(key = "N", description = "${uiLabelMap.CommonNo}"), @Option(key = "Y", description = "${uiLabelMap.CommonYes}")})),
            @FormField(name = "fromDate", mapName = "partyContent", useWhen = "partyContent!=null", display = @DisplayField(type = "date-time", alsoHidden = false)),
            @FormField(name = "thruDate", mapName = "partyContent", useWhen = "partyContent!=null", display = @DisplayField(type = "date-time", alsoHidden = false)),
            @FormField(name = "lastModifiedDate", useWhen = "content!=null", display = @DisplayField(type = "date-time", alsoHidden = false)),
            @FormField(name = "createAction", title = "${uiLabelMap.CommonAdd}", useWhen = "content==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "content!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "content==null", target = "createPartyContent")
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "StatusItem", valueField = "currentStatus", autoFieldMap = false), @EntityOneAction(entityName = "DataResource", valueField = "dataResource", autoFieldMap = false)})
    )
    public interface AddPartyContent {}

    @Form(
        name = "ListPartyContents",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.LIST,
        paginateTarget = "${paginateTarget}",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "contentId", hidden = @HiddenField),
            @FormField(name = "contentName", displayEntity = @DisplayEntityField(entityName = "Content", keyFieldName = "contentId", description = "${contentName}", subHyperlink = @SubHyperlink(target = "EditPartyContents", description = "${contentId}", parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "partyId")}))),
            @FormField(name = "partyContentTypeId", displayEntity = @DisplayEntityField(entityName = "PartyContentType")),
            @FormField(name = "contentTypeId", displayEntity = @DisplayEntityField(entityName = "ContentType")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "localeString", displayEntity = @DisplayEntityField(entityName = "CountryCode", keyFieldName = "countryCode", description = "${countryName}${countryCode}")),
            @FormField(name = "mimeTypeId", displayEntity = @DisplayEntityField(entityName = "MimeType")),
            @FormField(name = "dataResourceId", title = "${uiLabelMap.FormFieldTitle_dataResourceName}", useWhen = "dataResourceId==null", display = @DisplayField),
            @FormField(name = "dataResourceId", title = "${uiLabelMap.FormFieldTitle_dataResourceName}", useWhen = "dataResourceId!=null", displayEntity = @DisplayEntityField(entityName = "DataResource", description = "${dataResourceName}")),
            @FormField(name = "fromDate", display = @DisplayField(type = "date-time")),
            @FormField(name = "lastModifiedDate", display = @DisplayField(type = "date-time")),
            @FormField(name = "viewAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_view}", hyperlink = @HyperlinkField(target = "stream", description = "${uiLabelMap.CommonView}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId", fromField = "contentId")})),
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditPartyContents", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "dataResourceId"), @ParameterDef(paramName = "partyContentTypeId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removePartyContent", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "dataResourceId"), @ParameterDef(paramName = "partyContentTypeId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListPartyContents {}

    @Form(
        name = "ApplyServiceCredit",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "applyServiceCredit",
        focusFieldName = "amount",
        defaultMapName = "serviceCredit",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createServiceCredit", mapName = "serviceCredit")
        },
        fields = {
            @FormField(name = "finAccountId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FinAccount", description = "${finAccountName} [${finAccountId}]", filterByDate = "true", constraints = {@EntityConstraint(name = "ownerPartyId", value = "${partyId}"), @EntityConstraint(name = "finAccountTypeId", value = "SVCCRED_ACCOUNT")}, orderBy = {@EntityOrderBy(fieldName = "-fromDate")}))),
            @FormField(name = "currencyUomId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${abbreviation} - ${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "abbreviation")}))),
            @FormField(name = "productStoreId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductStore", description = "${storeName} [${productStoreId}]", orderBy = {@EntityOrderBy(fieldName = "productStoreId")}))),
            @FormField(name = "partyId", hidden = @HiddenField(value = "${partyId}")),
            @FormField(name = "finAccountTypeId", ignored = @IgnoredField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface ApplyServiceCredit {}

    @Form(
        name = "ListCarrierAccounts",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.LIST,
        target = "updatePartyCarrierAccount",
        listName = "carrierAccounts",
        paginate = "true",
        paginateTarget = "viewprofile",
        paginateTargetAnchor = "ListCarrierAccounts",
        oddRowStyle = "alternate-row",
        viewSize = 3,
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePartyCarrierAccount")
        },
        fields = {
            @FormField(name = "partyId", display = @DisplayField),
            @FormField(name = "carrierPartyId", display = @DisplayField),
            @FormField(name = "fromDate", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "updatePartyCarrierAccount", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "carrierPartyId"), @ParameterDef(paramName = "fromDate"), @ParameterDef(paramName = "thruDate", fromField = "context.thruDate")}))
        }
    )
    public interface ListCarrierAccounts {}

    @Form(
        name = "EditCarrierAccount",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "createPartyCarrierAccount",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createPartyCarrierAccount")
        },
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "carrierPartyId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyRole", description = "${partyId}", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "CARRIER"), @EntityConstraint(name = "partyId", value = "_NA_", operator = "not-equals")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", requiredField = true),
            @FormField(name = "accountNumber", title = "${uiLabelMap.AccountingAccountNumber}", requiredField = true),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface EditCarrierAccount {}

    @Form(
        name = "ListSubscriptions",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.LIST,
        listName = "subscriptionList",
        fields = {
            @FormField(name = "subscriptionId", displayEntity = @DisplayEntityField(entityName = "Subscription", description = "${description}", subHyperlink = @SubHyperlink(target = "/catalog/control/EditSubscription", description = "[${subscriptionId}]", parameters = {@ParameterDef(paramName = "subscriptionId")}))),
            @FormField(name = "subscriptionTypeId", title = "${uiLabelMap.ProductSubscription} ${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "SubscriptionType", description = "${description}")),
            @FormField(name = "subscriptionResourceId", displayEntity = @DisplayEntityField(entityName = "SubscriptionResource", description = "${description}")),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "productId", displayEntity = @DisplayEntityField(entityName = "Product", description = "${productName}", subHyperlink = @SubHyperlink(target = "/catalog/control/ViewProduct", description = "${productId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "productId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField)
        }
    )
    public interface ListSubscriptions {}

    @Form(
        name = "ListRelatedContacts",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.LIST,
        listName = "contacts",
        fields = {
            @FormField(name = "partyIdTo", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "viewprofile", description = "[${partyIdTo}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyIdTo")}))),
            @FormField(name = "partyRelationshipTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "PartyRelationshipType", description = "${partyRelationshipName}")),
            @FormField(name = "comments", display = @DisplayField)
        }
    )
    public interface ListRelatedContacts {}

    @Form(
        name = "ListRelatedAccounts",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.LIST,
        listName = "accounts",
        fields = {
            @FormField(name = "partyIdFrom", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "viewprofile", description = "[${partyIdFrom}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyIdFrom")}))),
            @FormField(name = "partyRelationshipTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "PartyRelationshipType", description = "${partyRelationshipName}")),
            @FormField(name = "comments", display = @DisplayField)
        }
    )
    public interface ListRelatedAccounts {}

    @Form(
        name = "ListPartyRelationships",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.LIST,
        target = "updatePartyRelationship",
        listName = "partyRelationships",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField(value = "${parameters.partyId}")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.PartyPartyId}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "viewprofile", description = "[${partyIdTo}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyIdTo")}))),
            @FormField(name = "roleTypeIdTo", title = "${uiLabelMap.PartyPartyInTheRoleOf}", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "partyRelationshipTypeId", title = "${uiLabelMap.PartyIsA}", displayEntity = @DisplayEntityField(entityName = "PartyRelationshipType", description = "${partyRelationshipName}")),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.PartyOfParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "viewprofile", description = "[${partyIdFrom}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyIdFrom")}))),
            @FormField(name = "roleTypeIdFrom", title = "${uiLabelMap.PartyPartyInTheRoleOf}", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "fromDate", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", dateTime = @DateTimeField),
            @FormField(name = "comments", text = @TextField),
            @FormField(name = "updateAction", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePartyRelationship", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "partyIdFrom"), @ParameterDef(paramName = "partyIdTo"), @ParameterDef(paramName = "roleTypeIdTo"), @ParameterDef(paramName = "roleTypeIdFrom"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListPartyRelationships {}

    @Form(
        name = "AddOtherPartyRelationship",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "createPartyRelationship",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField(value = "${parameters.partyId}")),
            @FormField(name = "partyIdTo", entryName = "parameters.partyId", display = @DisplayField),
            @FormField(name = "roleTypeIdTo", title = "${uiLabelMap.PartyPartyInTheRoleOf}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleTypeAndParty", description = "${description}", keyFieldName = "roleTypeId", constraints = {@EntityConstraint(name = "partyId", value = "${partyId}")}, orderBy = {@EntityOrderBy(fieldName = "description"), @EntityOrderBy(fieldName = "roleTypeId")}))),
            @FormField(name = "partyRelationshipTypeId", title = "${uiLabelMap.PartyIsA}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyRelationshipType", description = "${partyRelationshipName}", orderBy = {@EntityOrderBy(fieldName = "partyRelationshipName")}))),
            @FormField(name = "partyIdFrom", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeIdFrom", title = "${uiLabelMap.PartyPartyInTheRoleOf}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "PARTY_REL_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "securityGroupId", title = "${uiLabelMap.CommonSecurityGroupId}", dropDown = @DropDownField(allowEmpty = true, textSize = 60, entityOptions = @EntityOptions(entityName = "SecurityGroup", description = "${description} [${groupId}]", keyFieldName = "groupId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "comments", text = @TextField),
            @FormField(name = "submitAction", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddOtherPartyRelationship {}

    @Form(
        name = "AddAccount",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "createPartyRelationshipContactAccount",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField(value = "${parameters.partyId}")),
            @FormField(name = "accountPartyId", lookup = @LookupField(targetFormName = "LookupAccount")),
            @FormField(name = "contactPartyId", hidden = @HiddenField(value = "${parameters.partyId}")),
            @FormField(name = "comments", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddAccount {}

    @Form(
        name = "AddContact",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "createPartyRelationshipContactAccount",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField(value = "${parameters.partyId}")),
            @FormField(name = "accountPartyId", hidden = @HiddenField(value = "${parameters.partyId}")),
            @FormField(name = "contactPartyId", lookup = @LookupField(targetFormName = "LookupContact")),
            @FormField(name = "comments", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddContact {}

    @Form(
        name = "ListInvoicesApplPayments",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.LIST,
        listName = "ListInvoicesApplPayments",
        oddRowStyle = "alternate-row",
        defaultTitleStyle = "tableheadtext",
        fields = {
            @FormField(name = "invoiceId", displayEntity = @DisplayEntityField(entityName = "Invoice", description = " ", subHyperlink = @SubHyperlink(target = "/accounting/control/invoiceOverview", description = "[${invoiceId}]", parameters = {@ParameterDef(paramName = "invoiceId")}))),
            @FormField(name = "invoiceTypeId", displayEntity = @DisplayEntityField(entityName = "InvoiceType", description = "${description}")),
            @FormField(name = "invoiceDate", display = @DisplayField(type = "date")),
            @FormField(name = "total", display = @DisplayField(type = "currency")),
            @FormField(name = "amountToApply", display = @DisplayField(type = "currency")),
            @FormField(name = "amountApplied", display = @DisplayField(type = "currency")),
            @FormField(name = "paymentId", displayEntity = @DisplayEntityField(entityName = "Payment", description = " ", subHyperlink = @SubHyperlink(target = "/accounting/control/paymentOverview", description = "[${paymentId}]", parameters = {@ParameterDef(paramName = "paymentId")}))),
            @FormField(name = "pmEffectiveDate", title = "${uiLabelMap.AccountingEffectiveDate}", display = @DisplayField(type = "date")),
            @FormField(name = "pmAmount", title = "${uiLabelMap.AccountingPaymentAmount}", useWhen = "actualCurrency==false", display = @DisplayField(type = "currency")),
            @FormField(name = "pmAmount", entryName = "pmActualCurrencyAmount", title = "${uiLabelMap.AccountingPaymentAmount}", useWhen = "actualCurrency==true", display = @DisplayField(type = "currency"))
        },
        actions = @FormActions(set = {@SetAction(field = "actualCurrency", fromField = "actualCurrency", type = "Boolean", defaultValue = "true"), @SetAction(field = "actualCurrencyUomId", fromField = "actualCurrencyUomId", defaultValue = "${defaultOrganizationPartyCurrencyUomId}")}),
        rowActions = @RowActions(set = {@SetAction(field = "total", value = "${groovy:org.ofbiz.accounting.invoice.InvoiceWorker.getInvoiceTotal(delegator, invoiceId, actualCurrency)}"), @SetAction(field = "amountToApply", value = "${groovy:org.ofbiz.accounting.invoice.InvoiceWorker.getInvoiceNotApplied(delegator,invoiceId, actualCurrency)}"), @SetAction(field = "amountApplied", value = "${groovy:org.ofbiz.accounting.invoice.InvoiceWorker.getInvoiceApplied(delegator,invoiceId, org.ofbiz.base.util.UtilDateTime.nowTimestamp(), actualCurrency)}")})
    )
    public interface ListInvoicesApplPayments {}

    @Form(
        name = "ListUnAppliedInvoices",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.LIST,
        listName = "ListUnAppliedInvoices",
        oddRowStyle = "alternate-row",
        defaultTitleStyle = "tableheadtext",
        fields = {
            @FormField(name = "invoiceId", displayEntity = @DisplayEntityField(entityName = "Invoice", description = "${description}", subHyperlink = @SubHyperlink(target = "/accounting/control/invoiceOverview", description = "[${invoiceId}]", parameters = {@ParameterDef(paramName = "invoiceId")}))),
            @FormField(name = "invoiceParentTypeId", displayEntity = @DisplayEntityField(entityName = "InvoiceType", keyFieldName = "invoiceTypeId", description = "${description}")),
            @FormField(name = "invoiceDate", display = @DisplayField(type = "date")),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "unAppliedAmount", display = @DisplayField(type = "currency"))
        },
        actions = @FormActions(set = {@SetAction(field = "actualCurrency", fromField = "actualCurrency", type = "Boolean", defaultValue = "true")}, script = {@ScriptAction(location = "component://party/webapp/partymgr/WEB-INF/actions/party/UnAppliedInvoicesForParty.groovy")})
    )
    public interface ListUnAppliedInvoices {}

    @Form(
        name = "ListUnAppliedPayments",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.LIST,
        listName = "paymentList",
        oddRowStyle = "alternate-row",
        defaultTitleStyle = "tableheadtext",
        fields = {
            @FormField(name = "paymentId", hidden = @HiddenField),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", displayEntity = @DisplayEntityField(entityName = "Payment", description = " ", subHyperlink = @SubHyperlink(target = "/accounting/control/paymentOverview", description = "[${paymentId}]", parameters = {@ParameterDef(paramName = "paymentId")}))),
            @FormField(name = "effectiveDate", display = @DisplayField(type = "date")),
            @FormField(name = "paymentTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "PaymentType", description = "${description}")),
            @FormField(name = "paymentParentTypeId", title = "${uiLabelMap.CommonPayment}", displayEntity = @DisplayEntityField(entityName = "PaymentType", keyFieldName = "paymentTypeId", description = "${description}")),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "unAppliedAmount", display = @DisplayField(type = "currency"))
        },
        actions = @FormActions(set = {@SetAction(field = "actualCurrency", fromField = "actualCurrency", type = "Boolean", defaultValue = "true")}, script = {@ScriptAction(location = "component://party/webapp/partymgr/WEB-INF/actions/party/UnAppliedPaymentsForParty.groovy")})
    )
    public interface ListUnAppliedPayments {}

    @Form(
        name = "partyFinancialSummary",
        location = "component://party/widget/partymgr/PartyForms.xml",
        title = "Financial summary",
        defaultMapName = "finanSummary",
        defaultTitleStyle = "tableheadtext",
        fields = {
            @FormField(name = "totalSalesInvoice", display = @DisplayField(type = "currency")),
            @FormField(name = "totalPurchaseInvoice", position = 2, display = @DisplayField(type = "currency")),
            @FormField(name = "totalPaymentsIn", display = @DisplayField(type = "currency")),
            @FormField(name = "totalPaymentsOut", position = 2, display = @DisplayField(type = "currency")),
            @FormField(name = "totalInvoiceNotApplied", display = @DisplayField(type = "currency")),
            @FormField(name = "totalPaymentNotApplied", position = 2, display = @DisplayField(type = "currency")),
            @FormField(name = "totalToBePaid", title = "${uiLabelMap.PartyToBeReceivedFrom} ${parameters.partyId}", useWhen = "finanSummary.get(\"totalToBePaid\")!=null", display = @DisplayField(type = "currency")),
            @FormField(name = "totalToBeReceived", title = "${uiLabelMap.PartyToBePaidTo} ${parameters.partyId}", useWhen = "finanSummary.get(\"totalToBeReceived\")!=null", display = @DisplayField(type = "currency"))
        },
        actions = @FormActions(set = {@SetAction(field = "actualCurrency", fromField = "actualCurrency", defaultValue = "true"), @SetAction(field = "actualCurrencyUomId", fromField = "actualCurrencyUomId", defaultValue = "${defaultOrganizationPartyCurrencyUomId}")}, script = {@ScriptAction(location = "component://party/webapp/partymgr/WEB-INF/actions/party/PartyFinancialHistory.groovy")})
    )
    public interface partyFinancialSummary {}

    @Form(
        name = "ViewPartyRoles",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.LIST,
        target = "viewroles",
        listName = "partyRoles",
        oddRowStyle = "alternate-row",
        defaultTitleStyle = "tableheadtext",
        fields = {
            @FormField(name = "roleTypeId", title = "${uiLabelMap.PartyRoleTypeId}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.PartyRole}", display = @DisplayField),
            @FormField(name = "parentTypeId", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "removeAction", title = "${uiLabelMap.CommonRemove}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleterole", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId")}))
        }
    )
    public interface ViewPartyRoles {}

    @Form(
        name = "AddPartyRole",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "addrole/viewroles",
        title = "Add a role to party",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField(value = "${parameters.partyId}")),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "add", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddPartyRole {}

    @Form(
        name = "AddPartyMainRole",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "addrole/viewroles",
        title = "${uiLabelMap.PartyAddToMainRole}",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField(value = "${parameters.partyId}")),
            @FormField(name = "roleTypeId", entryName = "dummy", event = "onChange", action = "ajaxUpdateArea('addPartySecondaryRole', 'addsecondaryroles', jQuery('#AddPartyMainRole').serialize());", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "add", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddPartyMainRole {}

    @Form(
        name = "AddPartySecondaryRoles",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "addrole/viewroles",
        title = "${uiLabelMap.PartyAddToSecondRole}",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField(value = "${parameters.partyId}")),
            @FormField(name = "roleTypeId", entryName = "dummy", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", constraints = {@EntityConstraint(name = "parentTypeId", value = "${parameters.roleTypeId}")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "add", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddPartySecondaryRoles {}

    @Form(
        name = "AddRoleType",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "createroletype",
        title = "Add a new roletype",
        listName = "parentRoleList",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField(value = "${parameters.partyId}")),
            @FormField(name = "roleTypeId", entryName = "dummy", title = "${uiLabelMap.PartyRoleTypeId}", requiredField = true, text = @TextField),
            @FormField(name = "parentTypeId", position = 2, dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "parentRoleList", keyName = "roleTypeId", description = "${description}"))),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", requiredField = true, text = @TextField),
            @FormField(name = "save", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddRoleType {}

    @Form(
        name = "ListPreference",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.LIST,
        target = "removePreference",
        listName = "enumTypeChildAndEnums",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        useRowSubmit = true,
        fields = {
            @FormField(name = "enumId", parameterName = "userPrefTypeId", hidden = @HiddenField(value = "${enumId}")),
            @FormField(name = "childEnumTypeId", parameterName = "userPrefGroupTypeId", hidden = @HiddenField(value = "${enumTypeId}")),
            @FormField(name = "childDescription", title = "${uiLabelMap.CommonPreferenceGroup}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonPreferenceName}", display = @DisplayField),
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "userPrefUserLoginId", hidden = @HiddenField),
            @FormField(name = "userPrefValue", title = "${uiLabelMap.CommonValue}", display = @DisplayField),
            @FormField(name = "submit", title = "${uiLabelMap.CommonRemove}", useWhen = "userPrefValue!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        },
        rowActions = @RowActions(set = {@SetAction(field = "userPrefValue", fromField = "values.userPrefValue")}, service = {@ServiceAction(serviceName = "getUserPreference", resultMapName = "values", fieldMaps = {@FieldMap(fieldName = "userPrefTypeId", fromField = "enumId")})})
    )
    public interface ListPreference {}

    @Form(
        name = "PartyBillingAccount",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.LIST,
        listName = "billingAccounts",
        fields = {
            @FormField(name = "billingAccountId", display = @DisplayField),
            @FormField(name = "accountLimit", display = @DisplayField(type = "currency")),
            @FormField(name = "accountBalance", display = @DisplayField(type = "currency")),
            @FormField(name = "description", display = @DisplayField)
        }
    )
    public interface PartyBillingAccount {}

    @Form(
        name = "PartyReturns",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.LIST,
        listName = "returnList",
        fields = {
            @FormField(name = "returnId", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "fromPartyId", display = @DisplayField),
            @FormField(name = "toPartyId", display = @DisplayField)
        }
    )
    public interface PartyReturns {}

    @Form(
        name = "PartySalesOpportunities",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.LIST,
        listName = "salesOpportunities",
        fields = {
            @FormField(name = "opportunityName", title = "${uiLabelMap.SfaOpportunityName}", widgetStyle = "${styles.link_nav_info_idname} ${styles.action_view}", hyperlink = @HyperlinkField(target = "ViewSalesOpportunity", description = "${opportunityName} [${salesOpportunityId}]", parameters = {@ParameterDef(paramName = "salesOpportunityId")})),
            @FormField(name = "opportunityStageId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "SalesOpportunityStage", description = "${description}")),
            @FormField(name = "estimatedAmount", title = "${uiLabelMap.SfaEstimatedAmount}", display = @DisplayField),
            @FormField(name = "partyId", display = @DisplayField)
        }
    )
    public interface PartySalesOpportunities {}

    @Form(
        name = "listPartyIdentification",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.LIST,
        target = "updatePartyIdentification",
        listName = "listIt",
        fields = {
            @FormField(name = "partyIdentificationTypeId", hidden = @HiddenField),
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "partyIdentTypeDesc", display = @DisplayField),
            @FormField(name = "idValue", text = @TextField),
            @FormField(name = "delete", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePartyIdentification", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "partyIdentificationTypeId")})),
            @FormField(name = "submit", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface listPartyIdentification {}

    @Form(
        name = "editPartyIdentification",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "createPartyIdentification",
        focusFieldName = "idValue",
        listName = "partyIdents",
        fields = {
            @FormField(name = "partyIdentificationTypeId", useWhen = "partyIdentification == null", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyIdentificationType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "partyIdentificationTypeId", useWhen = "partyIdentification != null", displayEntity = @DisplayEntityField(entityName = "PartyIdentificationType")),
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "idValue", requiredField = true, text = @TextField),
            @FormField(name = "submit", title = "${uiLabelMap.CommonCreate}", useWhen = "partyIdentification == null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submit", title = "${uiLabelMap.CommonUpdate}", useWhen = "partyIdentification != null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "partyIdentification != null", target = "updatePartyIdentification")
        }
    )
    public interface editPartyIdentification {}

    @Form(
        name = "EditProductStoreRole",
        location = "component://party/widget/partymgr/PartyForms.xml",
        extendsForm = "EditProductStoreRole",
        extendsResource = "component://product/widget/catalog/StoreForms.xml",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "productStoreId", title = "${uiLabelMap.ProductStoreId}", useWhen = "productStoreRole==null", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductStore", description = "${storeName} (${productStoreId})", keyFieldName = "productStoreId", orderBy = {@EntityOrderBy(fieldName = "productStoreId")}))),
            @FormField(name = "productStoreId", title = "${uiLabelMap.ProductStoreId}", useWhen = "productStoreRole!=null", display = @DisplayField)
        }
    )
    public interface EditProductStoreRole {}

    @Form(
        name = "FindProductStoreRole",
        location = "component://party/widget/partymgr/PartyForms.xml",
        extendsForm = "FindProductStoreRole",
        extendsResource = "component://product/widget/catalog/StoreForms.xml",
        fields = {
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "productStoreId", title = "${uiLabelMap.ProductStoreId}", requiredField = true, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductStore", description = "${storeName} (${productStoreId})", keyFieldName = "productStoreId", orderBy = {@EntityOrderBy(fieldName = "productStoreId")})))
        }
    )
    public interface FindProductStoreRole {}

    @Form(
        name = "ListProductStoreRole",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        defaultEntityName = "ProductStoreRole",
        paginateTarget = "FindProductStoreRoles",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "productStoreId", title = "${uiLabelMap.ProductStoreId}", displayEntity = @DisplayEntityField(entityName = "ProductStore", keyFieldName = "productStoreId", description = "${storeName}", subHyperlink = @SubHyperlink(target = "/catalog/control/EditProductStore", description = "${storeName} (${productStoreId})", parameters = {@ParameterDef(paramName = "productStoreId")}))),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.PartyRoleTypeId}", displayEntity = @DisplayEntityField(entityName = "RoleType")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "sequenceNum", display = @DisplayField),
            @FormField(name = "editAction", title = "${uiLabelMap.CommonEdit}", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "FindProductStoreRoles", description = "${uiLabelMap.CommonEdit}", parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "productStoreId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "storeRemoveRole", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "productStoreId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "fromDate")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "ProductStoreRole"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListProductStoreRole {}

    @Form(
        name = "ExportParty",
        location = "component://party/widget/partymgr/PartyForms.xml",
        target = "ExportPartyCsv.csv",
        fields = {
            @FormField(name = "partyId", tooltip = "blank for all", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_export}", submit = @SubmitField)
        }
    )
    public interface ExportParty {}

    @Form(
        name = "ExportPartyCsv",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginate = "false",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        viewSize = 99999,
        fields = {
            @FormField(name = "partyId", title = "partyId", display = @DisplayField),
            @FormField(name = "preferredCurrencyUomId", title = "preferredCurrencyUomId", display = @DisplayField),
            @FormField(name = "groupName", title = "groupName", display = @DisplayField),
            @FormField(name = "firstName", title = "firstName", display = @DisplayField),
            @FormField(name = "middleName", title = "middleName", display = @DisplayField),
            @FormField(name = "lastName", title = "lastName", display = @DisplayField),
            @FormField(name = "companyPartyId", title = "companyPartyId", display = @DisplayField),
            @FormField(name = "companyName", title = "companyName", display = @DisplayField),
            @FormField(name = "roleTypeId", title = "roleTypeId", display = @DisplayField),
            @FormField(name = "contactMechPurposeTypeId", title = "contactMechPurposeTypeId", display = @DisplayField),
            @FormField(name = "contactMechTypeId", title = "contactMechTypeId", display = @DisplayField),
            @FormField(name = "emailAddress", title = "emailAddress", display = @DisplayField),
            @FormField(name = "telCountryCode", title = "telCountryCode", display = @DisplayField),
            @FormField(name = "telAreaCode", title = "telAreaCode", display = @DisplayField),
            @FormField(name = "telContactNumber", title = "telContactNumber", display = @DisplayField),
            @FormField(name = "address1", title = "address1", display = @DisplayField),
            @FormField(name = "address2", title = "address2", display = @DisplayField),
            @FormField(name = "city", title = "city", display = @DisplayField),
            @FormField(name = "stateProvinceGeoId", title = "stateProvinceGeoId", display = @DisplayField),
            @FormField(name = "postalCode", title = "postalCode", display = @DisplayField),
            @FormField(name = "countryGeoId", title = "countryGeoId", display = @DisplayField)
        }
    )
    public interface ExportPartyCsv {}

    @Form(
        name = "ImportParty",
        location = "component://party/widget/partymgr/PartyForms.xml",
        type = FormType.UPLOAD,
        target = "uploadParty",
        attribs = "{'showProgress':'${showProgress}', 'progressSuccessAction':'${progressSuccessAction}', 'progressOptions':${groovy: context.progressOptions ?: \"''\"} }",
        fields = {
            @FormField(name = "uploadedFile", file = @FileField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpload}", widgetStyle = "${styles.link_run_sys} ${styles.action_import}", submit = @SubmitField)
        }
    )
    public interface ImportParty {}

}
