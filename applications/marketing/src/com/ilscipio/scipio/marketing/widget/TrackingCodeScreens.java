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
public class TrackingCodeScreens {

    @Screen(name = "EditTrackingCode", location = "component://marketing/widget/TrackingCodeScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TrackingCode")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/marketing/control/FindTrackingCode")
    @Action(type = ActionType.SET, field = "trackingCodeId", fromField = "parameters.trackingCodeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "TrackingCode", valueField = "trackingCode")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.MarketingTrackingCode} ${trackingCodeId}")
    @DecoratorScreen(
        name = "CommonTrackingCodeDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"trackingCode"})}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(includeForms = {
                            @IncludeForm(name = "EditTrackingCode", location = "component://marketing/widget/TrackingCodeForms.xml", position = 1
                        )}, containers = {
                            @Container(widgets = {
                                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.MarketingTrackingCodeCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "EditTrackingCode"
                            )}, position = 0)})}), failWidgets = @InlineWidgets(screenlets = {
                                @Screenlet(includeForms = {
                                    @IncludeForm(name = "EditTrackingCode", location = "component://marketing/widget/TrackingCodeForms.xml"
                                )})}))})
        }
    )
    public interface EditTrackingCode {}

    @Screen(name = "ListTrackingCode", location = "component://marketing/widget/TrackingCodeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListTrackingCode")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TrackingCode")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListTrackingCode")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/marketing/control/FindTrackingCode")
    @Action(type = ActionType.SET, field = "trackingCodeId", fromField = "parameters.trackingCodeId")
    @Action(type = ActionType.SET, field = "entityName", value = "TrackingCode")
    @DecoratorScreen(
        name = "CommonTrackingCodeDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(containers = {
                    @Container(style = "button-bar", widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.MarketingTrackingCodeCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "EditTrackingCode"
                    )}),
                    @Container(style = "${styles.grid_row}", containers = {
                        @Container2(style = "${styles.grid_large}9 ${styles.grid_cell}", includeForms = {
                            @IncludeForm(name = "ListTrackingCode", location = "component://marketing/widget/TrackingCodeForms.xml"
                        )}),
                        @Container2(style = "${styles.grid_large}3 ${styles.grid_cell}", htmlTemplates = {
                            @HtmlTemplate(location = "component://marketing/webapp/marketing/tracking/listTrackingCode.ftl"
                        )})})})})
        }
    )
    public interface ListTrackingCode {}

    @Screen(name = "EditTrackingCodeOrder", location = "component://marketing/widget/TrackingCodeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditTrackingCodeOrder")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TrackingCodeOrder")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditTrackingCodeOrder")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/marketing/control/FindTrackingCode")
    @Action(type = ActionType.SET, field = "trackingCodeId", fromField = "parameters.trackingCodeId")
    @Action(type = ActionType.SET, field = "orderId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "trackingCodeTypeId", fromField = "parameters.trackingCodeTypeId")
    @Action(type = ActionType.SET, field = "isBillable", fromField = "parameters.isBillable")
    @Action(type = ActionType.ENTITY_ONE, entityName = "TrackingCodeOrder", valueField = "trackingCodeOrder")
    @DecoratorScreen(
        name = "CommonTrackingCodeDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditTrackingCodeOrder", location = "component://marketing/widget/TrackingCodeForms.xml"
                )})})
        }
    )
    public interface EditTrackingCodeOrder {}

    @Screen(name = "ListTrackingCodeOrders", location = "component://marketing/widget/TrackingCodeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListTrackingCodeOrder")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TrackingCodeOrder")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListTrackingCodeOrder")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/marketing/control/FindTrackingCode")
    @Action(type = ActionType.SET, field = "trackingCodeId", fromField = "parameters.trackingCodeId")
    @Action(type = ActionType.SET, field = "trackingCodeTypeId", fromField = "parameters.trackingCodeTypeId")
    @DecoratorScreen(
        name = "CommonTrackingCodeDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleListTrackingCodeOrdersFor} ${uiLabelMap.MarketingTrackingCodeTrackingCodeId}=${trackingCodeId}", includeForms = {
                    @IncludeForm(name = "ListTrackingCodeOrders", location = "component://marketing/widget/TrackingCodeForms.xml"
                )})})
        }
    )
    public interface ListTrackingCodeOrders {}

    @Screen(name = "FindTrackingCodeOrders", location = "component://marketing/widget/TrackingCodeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindTrackingCodeOrder")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TrackingCodeOrder")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindTrackingCodeOrder")
    @Action(type = ActionType.SET, field = "trackingCodeId", fromField = "parameters.trackingCodeId")
    @Action(type = ActionType.SET, field = "trackingCodeTypeId", fromField = "parameters.trackingCodeTypeId")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/marketing/control/EditTrackingCode?trackingCodeId=${trackingCodeId}")
    @DecoratorScreen(
        name = "CommonTrackingCodeDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "FindTrackingCodeOrders", location = "component://marketing/widget/TrackingCodeForms.xml"
                )})})
        }
    )
    public interface FindTrackingCodeOrders {}

    @Screen(name = "EditTrackingCodeVisit", location = "component://marketing/widget/TrackingCodeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditTrackingCodeVisit")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TrackingCodeVisit")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditTrackingCodeVisit")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/marketing/control/FindTrackingCode")
    @Action(type = ActionType.SET, field = "trackingCodeId", fromField = "parameters.trackingCodeId")
    @Action(type = ActionType.SET, field = "communicationEventId", fromField = "parameters.communicationEventId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @DecoratorScreen(
        name = "CommonTrackingCodeDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditTrackingCodeVisit", location = "component://marketing/widget/TrackingCodeForms.xml"
                )})})
        }
    )
    public interface EditTrackingCodeVisit {}

    @Screen(name = "ListTrackingCodeVisits", location = "component://marketing/widget/TrackingCodeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListTrackingCodeVisit")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TrackingCodeVisit")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListTrackingCodeVisit")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/marketing/control/FindTrackingCode")
    @Action(type = ActionType.SET, field = "trackingCodeId", fromField = "parameters.trackingCodeId")
    @Action(type = ActionType.SET, field = "entityName", value = "TrackingCodeVisit")
    @DecoratorScreen(
        name = "CommonTrackingCodeDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleListTrackingCodeFor} ${uiLabelMap.MarketingTrackingCodeTrackingCodeId}=${trackingCodeId}", includeForms = {
                    @IncludeForm(name = "ListTrackingCodeVisits", location = "component://marketing/widget/TrackingCodeForms.xml"
                )})})
        }
    )
    public interface ListTrackingCodeVisits {}

    @Screen(name = "FindTrackingCodeVisits", location = "component://marketing/widget/TrackingCodeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindTrackingCodeVisits")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TrackingCodeVisit")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindTrackingCodeVisits")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/marketing/control/FindTrackingCode")
    @Action(type = ActionType.SET, field = "trackingCodeId", fromField = "parameters.trackingCodeId")
    @DecoratorScreen(
        name = "CommonTrackingCodeDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleFindTrackingCodeVisit}", includeForms = {
                    @IncludeForm(name = "FindTrackingCodeVisits", location = "component://marketing/widget/TrackingCodeForms.xml"
                )})})
        }
    )
    public interface FindTrackingCodeVisits {}

    @Screen(name = "LookupVisit", location = "component://marketing/widget/TrackingCodeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleLookupVisit")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TrackingCode")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleLookupVisit")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupVisit}")
    @Action(type = ActionType.SET, field = "entityName", value = "CommunicationEvent")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "LookupVisit", location = "component://marketing/widget/TrackingCodeForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListLookupVisit", location = "component://marketing/widget/TrackingCodeForms.xml"
            )})
        }
    )
    public interface LookupVisit {}

    @Screen(name = "LookupTrackingCode", location = "component://marketing/widget/TrackingCodeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleLookupTrackingCode")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TrackingCode")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleLookupTrackingCode")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupTrackingCode}")
    @Action(type = ActionType.SET, field = "entityName", value = "TrackingCode")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "LookupTrackingCode", location = "component://marketing/widget/TrackingCodeForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListLookupTrackingCode", location = "component://marketing/widget/TrackingCodeForms.xml"
            )})
        }
    )
    public interface LookupTrackingCode {}

    @Screen(name = "EditTrackingCodeType", location = "component://marketing/widget/TrackingCodeScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TrackingCodeType")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/marketing/control/FindTrackingCodeType")
    @Action(type = ActionType.SET, field = "trackingCodeTypeId", fromField = "parameters.trackingCodeTypeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "TrackingCodeType", valueField = "trackingCodeType")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.MarketingTracking} ${trackingCodeTypeId}")
    @DecoratorScreen(
        name = "CommonTrackingCodeDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"trackingCodeType"})
                }), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(includeForms = {
                        @IncludeForm(name = "EditTrackingCodeType", location = "component://marketing/widget/TrackingCodeForms.xml", position = 1
                    )}, containers = {
                        @Container(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.MarketingTrackingCodeTypeCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "EditTrackingCodeType"
                        )}, position = 0)})}), failWidgets = @InlineWidgets(screenlets = {
                            @Screenlet(title = "${uiLabelMap.PageTitleAddTrackingCodeType}", includeForms = {
                                @IncludeForm(name = "EditTrackingCodeType", location = "component://marketing/widget/TrackingCodeForms.xml"
                            )})}))})
        }
    )
    public interface EditTrackingCodeType {}

    @Screen(name = "ListTrackingCodeType", location = "component://marketing/widget/TrackingCodeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListTrackingCodeType")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TrackingCodeType")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleListTrackingCodeType")
    @Action(type = ActionType.SET, field = "donePage", fromField = "parameters.DONE_PAGE", defaultValue = "/marketing/control/ListTrackingCodeType")
    @Action(type = ActionType.SET, field = "trackingCodeTypeId", fromField = "parameters.trackingCodeTypeId")
    @Action(type = ActionType.SET, field = "entityName", value = "TrackingCodeType")
    @DecoratorScreen(
        name = "CommonTrackingCodeDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListTrackingCodeType", location = "component://marketing/widget/TrackingCodeForms.xml", position = 1
                )}, containers = {
                    @Container(style = "button-bar", widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.MarketingTrackingCodeTypeCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "EditTrackingCodeType"
                    )}, position = 0)})})
        }
    )
    public interface ListTrackingCodeType {}

    @Screen(name = "LookupTrackingCodeType", location = "component://marketing/widget/TrackingCodeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleLookupTrackingCodeType")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TrackingCodeType")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleLookupTrackingCodeType")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupTrackingCodeType}")
    @Action(type = ActionType.SET, field = "entityName", value = "TrackingCodeType")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "LookupTrackingCodeType", location = "component://marketing/widget/TrackingCodeForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListLookupTrackingCodeType", location = "component://marketing/widget/TrackingCodeForms.xml"
            )})
        }
    )
    public interface LookupTrackingCodeType {}

}
