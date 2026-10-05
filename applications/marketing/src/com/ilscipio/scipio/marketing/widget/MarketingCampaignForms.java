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
package com.ilscipio.scipio.marketing.widget;

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
public class MarketingCampaignForms {

    @Form(
        name = "EditMarketingCampaign",
        location = "component://marketing/widget/MarketingCampaignForms.xml",
        target = "updateMarketingCampaign",
        defaultMapName = "marketingCampaign",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "MarketingCampaign")
        },
        fields = {
            @FormField(name = "marketingCampaignId", title = "${uiLabelMap.MarketingCampaignId}", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "marketingCampaign!=null", hidden = @HiddenField),
            @FormField(name = "marketingCampaignId", title = "${uiLabelMap.MarketingCampaignId}", useWhen = "marketingCampaign==null&&marketingCampaignId==null", text = @TextField),
            @FormField(name = "marketingCampaignId", title = "${uiLabelMap.MarketingCampaignId}", tooltip = "${uiLabelMap.CommonCannotBeFound}: [${marketingCampaignId}]", useWhen = "marketingCampaign==null&&marketingCampaignId!=null", display = @DisplayField),
            @FormField(name = "parentCampaignId", title = "${uiLabelMap.MarketingParentCampaignId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "MarketingCampaign", description = "${campaignName}", keyFieldName = "marketingCampaignId"))),
            @FormField(name = "campaignName", title = "${uiLabelMap.MarketingCampaignName}", text = @TextField(size = 55)),
            @FormField(name = "campaignSummary", title = "${uiLabelMap.MarketingCampaignSummary}", textarea = @TextareaField(rows = 5)),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "MKTG_CAMP_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "createdByUserLogin", ignored = @IgnoredField),
            @FormField(name = "lastModifiedByUserLogin", ignored = @IgnoredField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link"))
        },
        altTargets = {
            @AltTarget(useWhen = "marketingCampaign==null", target = "createMarketingCampaign")
        }
    )
    public interface EditMarketingCampaign {}

}
