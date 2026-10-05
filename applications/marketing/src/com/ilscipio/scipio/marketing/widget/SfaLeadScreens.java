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
public class SfaLeadScreens {

    @Screen(name = "FindLeads", location = "component://marketing/widget/sfa/LeadScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "MarketingFindLeads")
    @Action(type = ActionType.SET, field = "currentUrl", value = "FindLeads")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Leads")
    @Action(type = ActionType.SCRIPT, location = "component://marketing/webapp/marketing/WEB-INF/actions/generated/FindLeads_script1.groovy")
    @Action(type = ActionType.SET, field = "findScreenShowResults", value = "true")
    @DecoratorScreen(
        name = "CommonLeadDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "LeadSubTabBar", location = "component://marketing/widget/sfa/SfaMenus.xml"
            )}, decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindLeads", location = "component://marketing/widget/sfa/forms/LeadForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(sections = {
                        @SectionLeaf(widgets = @WidgetsLeaf(includeForms = {
                            @IncludeForm(name = "ListLeads", location = "component://marketing/widget/sfa/forms/LeadForms.xml"
                        )}))}))})})
        }
    )
    public interface FindLeads {}

    @Screen(name = "NewLead", location = "component://marketing/widget/sfa/LeadScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCreateLead")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCountryGeoId", resource = "general", property = "country.geo.id.default", defaultValue = "USA")
    @DecoratorScreen(
        name = "CommonLeadDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "createLead", location = "component://marketing/widget/sfa/forms/LeadForms.xml"
                )})})
        }
    )
    public interface NewLead {}

    @Screen(name = "ConvertLead", location = "component://marketing/widget/sfa/LeadScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ConvertLead")
    @DecoratorScreen(
        name = "CommonLeadDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"parameters.partyGroupId"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(title = "${uiLabelMap.SfaConvertLead}", includeForms = {
                        @IncludeForm(name = "ConvertLead", location = "component://marketing/widget/sfa/forms/LeadForms.xml"
                    )})}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "Please first add related company."
                    )}, screenlets = {
                        @Screenlet(title = "${uiLabelMap.PageTitleAddRelatedCompany}", includeForms = {
                            @IncludeForm(name = "AddRelatedCompany", location = "component://marketing/widget/sfa/forms/LeadForms.xml"
                        )})}))})
        }
    )
    public interface ConvertLead {}

    @Screen(name = "CloneLead", location = "component://marketing/widget/sfa/LeadScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "CloneLead")
    @Action(type = ActionType.SCRIPT, location = "component://marketing/webapp/sfa/WEB-INF/action/CloneLead.groovy")
    @DecoratorScreen(
        name = "CommonLeadDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.SfaCloneLead}", includeForms = {
                    @IncludeForm(name = "createLead", location = "component://marketing/widget/sfa/forms/LeadForms.xml"
                )})})
        }
    )
    public interface CloneLead {}

    @Screen(name = "MergeLeads", location = "component://marketing/widget/sfa/LeadScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "MergeLeads")
    @Action(type = ActionType.SCRIPT, location = "component://marketing/webapp/sfa/WEB-INF/action/MergeContacts.groovy")
    @DecoratorScreen(
        name = "CommonLeadDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.SfaMergeLeads}", includeForms = {
                    @IncludeForm(name = "MergeLeads", location = "component://marketing/widget/sfa/forms/LeadForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = CompareField.class, params = {"parameters.partyIdFrom", "not-equals", "parameters.partyIdTo"
                    })}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://marketing/webapp/sfa/lead/mergeLeads.ftl"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.SfaCanNotMergeSameLeads}"
                    )}))})
        }
    )
    public interface MergeLeads {}

    @Screen(name = "NewLeadFromVCard", location = "component://marketing/widget/sfa/LeadScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCreateLeadFromVCard")
    @DecoratorScreen(
        name = "CommonLeadDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "NewLeadFromVCard", location = "component://marketing/widget/sfa/forms/LeadForms.xml", position = 1
                )}, containers = {
                    @Container(labels = {
                        @Label(text = "${uiLabelMap.SfaAutoCreateLeadByImportingVCard}"
                    )}, position = 0)})})
        }
    )
    public interface NewLeadFromVCard {}

    @Screen(name = "LeadPartyDataSource", location = "component://marketing/widget/sfa/LeadScreens.xml")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_AND, entityName = "PartyDataSource", list = "partyDataSources", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyId")})
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.SfaLeadSource}", includeForms = {@IncludeForm(name = "AddLeadPartyDataSource", location = "component://marketing/widget/sfa/forms/LeadForms.xml"), @IncludeForm(name = "ViewLeadPartyDataSources", location = "component://marketing/widget/sfa/forms/LeadForms.xml")})}))
    public interface LeadPartyDataSource {}

    @Screen(name = "AddRelatedCompany", location = "component://marketing/widget/sfa/LeadScreens.xml")
    @DecoratorScreen(
        name = "CommonLeadDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddRelatedCompany}", includeForms = {
                    @IncludeForm(name = "AddRelatedCompany", location = "component://marketing/widget/sfa/forms/LeadForms.xml"
                )})})
        }
    )
    public interface AddRelatedCompany {}

}
