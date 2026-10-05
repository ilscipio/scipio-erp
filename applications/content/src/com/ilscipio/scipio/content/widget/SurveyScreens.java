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
package com.ilscipio.scipio.content.widget;

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
public class SurveyScreens {

    @Screen(name = "FindSurvey", location = "component://content/widget/SurveyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindSurvey")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Survey")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindSurvey")
    @DecoratorScreen(
        name = "CommonSurveyDecorator",
        location = "component://content/widget/SurveyScreens.xml",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindSurvey", location = "component://content/widget/survey/SurveyForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFindSurvey", location = "component://content/widget/survey/SurveyForms.xml"
                    )}))})})
        }
    )
    public interface FindSurvey {}

    @Screen(name = "CommonSurveyDecorator", location = "component://content/widget/SurveyScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://content/widget/SurveyMenus.xml#Survey")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.surveyId}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonContentAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ContentSurveyCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "EditSurvey"
                )}, position = 0)})
        }
    )
    public interface CommonSurveyDecorator {}

    @Screen(name = "EditSurvey", location = "component://content/widget/SurveyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditSurvey")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Survey")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditSurvey")
    @Action(type = ActionType.SET, field = "surveyId", fromField = "parameters.surveyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Survey", valueField = "survey")
    @DecoratorScreen(
        name = "CommonSurveyDecorator",
        location = "component://content/widget/SurveyScreens.xml",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Empty.class, params = {"survey"})}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.PageTitleCreateSurvey}", includeForms = {
                            @IncludeForm(name = "EditSurvey", location = "component://content/widget/survey/SurveyForms.xml"
                        )})}), failWidgets = @InlineWidgets(screenlets = {
                            @Screenlet(includeForms = {
                                @IncludeForm(name = "EditSurvey", location = "component://content/widget/survey/SurveyForms.xml"
                            ),
                            @IncludeForm(name = "BuildSurveyFromPdf", location = "component://content/widget/survey/SurveyForms.xml"
                        )})}))})
        }
    )
    public interface EditSurvey {}

    @Screen(name = "EditSurveyMultiResps", location = "component://content/widget/SurveyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditSurveyMultiResps")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "SurveyMultiResps")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditSurveyMultiResps")
    @Action(type = ActionType.SET, field = "surveyId", fromField = "parameters.surveyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Survey", valueField = "survey")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "SurveyMultiResp", list = "surveyMultiRespList", conditions = {@ConditionExpr(fieldName = "surveyId", fromField = "surveyId")}, orderBy = {"surveyMultiRespId"})
    @DecoratorScreen(
        name = "CommonSurveyDecorator",
        location = "component://content/widget/SurveyScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PageTitleEditSurveyMultiResps} ${uiLabelMap.ContentSurveySurveyId} ${surveyId}", style = "heading"
            ),
            @Widget(type = WidgetType.ITERATE_SECTION, list = "surveyMultiRespList", entry = "surveyMultiResp", name = "EditSurveyMultiResps-iterate1", location = "component://content/widget/SurveyScreens.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ContentSurveyAddSurveyMultiResp}", includeForms = {
                    @IncludeForm(name = "AddSurveyMultiResp", location = "component://content/widget/survey/SurveyForms.xml"
                )})})
        }
    )
    public interface EditSurveyMultiResps {}

    @Screen(name = "EditSurveyMultiResps-iterate1", location = "component://content/widget/SurveyScreens.xml")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.ContentSurveyEditSurveyMultiResp}", includeForms = {@IncludeForm(name = "EditSurveyMultiResp", location = "component://content/widget/survey/SurveyForms.xml"), @IncludeForm(name = "ListSurveyMultiRespColumns", location = "component://content/widget/survey/SurveyForms.xml")}), @Screenlet(title = "${uiLabelMap.ContentSurveyAddSurveyMultiRespColumn}", includeForms = {@IncludeForm(name = "AddSurveyMultiRespColumn", location = "component://content/widget/survey/SurveyForms.xml")})}))
    public interface EditSurveyMultiResps_iterate1 {}

    @Screen(name = "EditSurveyQuestions", location = "component://content/widget/SurveyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditSurveyQuestions")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "SurveyQuestions")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditSurveyQuestions")
    @Action(type = ActionType.SET, field = "surveyId", fromField = "parameters.surveyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Survey", valueField = "survey")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/survey/EditSurveyQuestions.groovy")
    @DecoratorScreen(
        name = "CommonSurveyDecorator",
        location = "component://content/widget/SurveyScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://content/webapp/content/survey/EditSurveyQuestions.ftl"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListSurveyPages", location = "component://content/widget/survey/SurveyForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditSurveyPages} ${uiLabelMap.ContentSurveySurveyId} ${surveyId}", name = "SurveyPagePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddSurveyPage", location = "component://content/widget/survey/SurveyForms.xml"
                )}, position = 1)})
        }
    )
    public interface EditSurveyQuestions {}

    @Screen(name = "FindSurveyResponse", location = "component://content/widget/SurveyScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindSurveyResponse")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindSurveyResponse")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindSurveyResponse")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "surveyId", fromField = "parameters.surveyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Survey", valueField = "survey")
    @DecoratorScreen(
        name = "CommonSurveyDecorator",
        location = "component://content/widget/SurveyScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleFindSurveyResponse} ${uiLabelMap.ContentSurveySurveyId} ${surveyId}", includeForms = {
                    @IncludeForm(name = "FindSurveyResponse", location = "component://content/widget/survey/SurveyForms.xml", position = 1
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ContentSurveyCreateResponse}", style = "${styles.link_nav} ${styles.action_add}", target = "EditSurveyResponse"
                    )}, position = 0)}),
                    @Screenlet(title = "${uiLabelMap.ContentSurveyBuildRespondeFromPDF}", includeForms = {
                        @IncludeForm(name = "BuildSurveyResponseFromPdf", location = "component://content/widget/survey/SurveyForms.xml"
                    )}),
                    @Screenlet(title = "${uiLabelMap.PageTitleListSurveyResponse}", includeForms = {
                        @IncludeForm(name = "ListFindSurveyResponse", location = "component://content/widget/survey/SurveyForms.xml"
                    )})})
        }
    )
    public interface FindSurveyResponse {}

    @Screen(name = "ViewSurveyResponses", location = "component://content/widget/SurveyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewSurveyResponses")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "SurveyResponses")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleViewSurveyResponses")
    @Action(type = ActionType.SET, field = "surveyId", fromField = "parameters.surveyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Survey", valueField = "survey")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/survey/ViewSurveyResponses.groovy")
    @DecoratorScreen(
        name = "CommonSurveyDecorator",
        location = "component://content/widget/SurveyScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ContentSurveyCreateResponse}", style = "${styles.link_nav} ${styles.action_add}", target = "EditSurveyResponse"
                )}, position = 0)}, screenlets = {
                    @Screenlet(title = "${uiLabelMap.PageTitleViewSurveyResponses} ${uiLabelMap.ContentSurveySurveyId} ${surveyId}", htmlTemplates = {
                        @HtmlTemplate(location = "component://content/webapp/content/survey/ViewSurveyResponses.ftl", position = 1
                    )}, sections = {
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                            @Condition(type = Empty.class, params = {"parameters.rootContentId"
                        })}), widgets = @WidgetsForContainer(containers = {
                            @Container2(widgets = {
                                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ContentCompDocGoBack} [${parameters.rootContentId}]", style = "${styles.link_nav_cancel_long}", target = "ViewCompDocInstanceTree"
                            )})}), position = 0)}, position = 1)})
        }
    )
    public interface ViewSurveyResponses {}

    @Screen(name = "EditSurveyResponse", location = "component://content/widget/SurveyScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditSurveyResponse")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "SurveyResponses")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditSurveyResponse")
    @Action(type = ActionType.SET, field = "surveyId", fromField = "parameters.surveyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Survey", valueField = "survey")
    @Action(type = ActionType.SET, field = "surveyResponseId", fromField = "parameters.surveyResponseId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "SurveyResponse", valueField = "surveyResponse")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/survey/EditSurveyResponse.groovy")
    @DecoratorScreen(
        name = "CommonSurveyDecorator",
        location = "component://content/widget/SurveyScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PageTitleEditSurveyResponse}, ${uiLabelMap.ContentSurveyResponse}: ${parameters.surveyResponseId}, ${uiLabelMap.ContentSurveySurveyId}: ${surveyId}", style = "heading"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://content/webapp/content/survey/EditSurveyResponse.ftl"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "BuildSurveyResponseFromPdf", location = "component://content/widget/survey/SurveyForms.xml"
            )})
        }
    )
    public interface EditSurveyResponse {}

    @Screen(name = "LookupSurvey", location = "component://content/widget/SurveyScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", fromField = "uiLabelMap.PageTitleLookupSurvey")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupSurvey", location = "component://content/widget/survey/SurveyForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupSurvey", location = "component://content/widget/survey/SurveyForms.xml"
            )})
        }
    )
    public interface LookupSurvey {}

    @Screen(name = "LookupSurveyResponse", location = "component://content/widget/SurveyScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", fromField = "uiLabelMap.PageTitleLookupSurveyResponse")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupSurveyResponse", location = "component://content/widget/survey/SurveyForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupSurveyResponse", location = "component://content/widget/survey/SurveyForms.xml"
            )})
        }
    )
    public interface LookupSurveyResponse {}

    @Screen(name = "ListFindSurveySearchResults", location = "component://content/widget/SurveyScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "ListFindSurvey", location = "component://content/widget/survey/SurveyForms.xml")}))
    public interface ListFindSurveySearchResults {}

    @Screen(name = "RenderSurveyResponse", location = "component://content/widget/SurveyScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/survey/RenderSurveyResponse.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"surveyString"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, content = "${raw(surveyString!)}")}))
    public interface RenderSurveyResponse {}

    @Screen(name = "SurveyResponseQaList", location = "component://content/widget/SurveyScreens.xml")
    @Action(type = ActionType.SET, field = "surveyTmplLoc", value = "component://content/template/survey/qalistresult.ftl")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "RenderSurveyResponse")}))
    public interface SurveyResponseQaList {}

    @Screen(name = "SurveyResponseDetail", location = "component://content/widget/SurveyScreens.xml")
    @Action(type = ActionType.SET, field = "surveyTmplLoc", value = "component://content/template/survey/genericresult.ftl")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "RenderSurveyResponse")}))
    public interface SurveyResponseDetail {}

}
