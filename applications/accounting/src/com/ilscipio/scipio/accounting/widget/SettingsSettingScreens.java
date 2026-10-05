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
public class SettingsSettingScreens {

    @Screen(name = "AddCustomTimePeriod", location = "component://accounting/widget/settings/SettingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingAddCustomTimePeriod")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TimePeriods")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/period/EditCustomTimePeriod.groovy")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/period/AddCustomTimePeriod.ftl"
            )})
        }
    )
    public interface AddCustomTimePeriod {}

    @Screen(name = "EditCustomTimePeriod", location = "component://accounting/widget/settings/SettingScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingTimePeriods")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TimePeriods")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/period/EditCustomTimePeriod.groovy")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/period/EditCustomTimePeriod.ftl"
            )})
        }
    )
    public interface EditCustomTimePeriod {}

    @Screen(name = "EditVendor", location = "component://accounting/widget/settings/SettingScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "findVendors")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingApPageTitleEditVendor")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditVendor", location = "component://accounting/widget/ap/VendorForms.xml"
                )})})
        }
    )
    public interface EditVendor {}

    @Screen(name = "FindVendors", location = "component://accounting/widget/settings/SettingScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "findVendors")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingApPageTitleFindVendors")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonNew} ${uiLabelMap.PartyVendor}", style = "${styles.link_nav} ${styles.action_add}", target = "editVendor"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindVendors", location = "component://accounting/widget/ap/VendorForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListVendors", location = "component://accounting/widget/ap/VendorForms.xml"
                        )}))})})
        }
    )
    public interface FindVendors {}

    @Screen(name = "Journals", location = "component://accounting/widget/settings/SettingScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Journals")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingGlJournals")
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @DecoratorScreen(
        name = "CommonSettingsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListGlJournals", location = "component://accounting/widget/settings/GlSetupForms.xml"
            )}, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditGlJournal", location = "component://accounting/widget/settings/GlSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface Journals {}

}
