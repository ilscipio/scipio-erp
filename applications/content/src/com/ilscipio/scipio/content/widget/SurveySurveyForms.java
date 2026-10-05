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
public class SurveySurveyForms {

    @Form(
        name = "FindSurvey",
        location = "component://content/widget/survey/SurveyForms.xml",
        target = "FindSurvey",
        defaultMapName = "survey",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Survey", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "isAnonymous", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "allowMultiple", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "allowUpdate", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "acroFormContentId", lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindSurvey {}

    @Form(
        name = "ListFindSurvey",
        location = "component://content/widget/survey/SurveyForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindSurvey",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Survey", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "surveyId", title = "${uiLabelMap.ContentSurveySurveyId}", widgetStyle = "${styles.link_nav_info_id}", sortField = true, hyperlink = @HyperlinkField(target = "EditSurvey", description = "${surveyId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "surveyId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Survey"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        onEventUpdateAreas = {
            @OnEventUpdateArea(eventType = "paginate", areaId = "search-results", areaTarget = "ListFindSurveySearchResults")
        }
    )
    public interface ListFindSurvey {}

    @Form(
        name = "EditSurvey",
        location = "component://content/widget/survey/SurveyForms.xml",
        target = "updateSurvey",
        defaultMapName = "survey",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateSurvey")
        },
        fields = {
            @FormField(name = "surveyId", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "survey!=null", display = @DisplayField),
            @FormField(name = "surveyId", tooltip = "${uiLabelMap.CommonCannotBeFound}: [${surveyId}]", useWhen = "survey==null&&surveyId!=null", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "surveyId", useWhen = "survey==null&&surveyId==null", ignored = @IgnoredField),
            @FormField(name = "isAnonymous", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "allowMultiple", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "allowUpdate", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "acroFormContentId", useWhen = "survey!=null", lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "acroFormContentId", useWhen = "survey==null", ignored = @IgnoredField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "survey==null", target = "createSurvey")
        }
    )
    public interface EditSurvey {}

    @Form(
        name = "BuildSurveyFromPdf",
        location = "component://content/widget/survey/SurveyForms.xml",
        target = "buildSurveyFromPdf",
        defaultMapName = "survey",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "surveyId", mapName = "survey", hidden = @HiddenField),
            @FormField(name = "contentId", mapName = "emptyMap", title = "${uiLabelMap.ContentPDF}", lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "submitAction", title = "${uiLabelMap.ContentSurveyGenerateQuestions}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface BuildSurveyFromPdf {}

    @Form(
        name = "BuildSurveyResponseFromPdf",
        location = "component://content/widget/survey/SurveyForms.xml",
        target = "buildSurveyResponseFromPdf",
        defaultMapName = "survey",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "surveyId", mapName = "parameters", hidden = @HiddenField),
            @FormField(name = "surveyResponseId", mapName = "surveyResponse", hidden = @HiddenField),
            @FormField(name = "pdfFileNameIn", mapName = "emptyMap", text = @TextField),
            @FormField(name = "contentId", mapName = "emptyMap", title = "${uiLabelMap.ContentPDF}", lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "submitAction", title = "${uiLabelMap.ContentSurveyBuildRespondeFromPDF}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface BuildSurveyResponseFromPdf {}

    @Form(
        name = "EditSurveyMultiResp",
        location = "component://content/widget/survey/SurveyForms.xml",
        target = "updateSurveyMultiResp",
        defaultMapName = "surveyMultiResp",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateSurveyMultiResp", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "surveyId", hidden = @HiddenField),
            @FormField(name = "surveyMultiRespId", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditSurveyMultiResp {}

    @Form(
        name = "ListSurveyMultiRespColumns",
        location = "component://content/widget/survey/SurveyForms.xml",
        type = FormType.LIST,
        target = "updateSurveyMultiRespColumn",
        listName = "surveyMultiRespColumnList",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateSurveyMultiRespColumn", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "surveyId", hidden = @HiddenField),
            @FormField(name = "surveyMultiRespId", display = @DisplayField),
            @FormField(name = "surveyMultiRespColId", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListSurveyMultiRespColumns {}

    @Form(
        name = "AddSurveyMultiRespColumn",
        location = "component://content/widget/survey/SurveyForms.xml",
        target = "createSurveyMultiRespColumn",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createSurveyMultiRespColumn", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "surveyId", mapName = "surveyMultiResp", hidden = @HiddenField),
            @FormField(name = "surveyMultiRespId", mapName = "surveyMultiResp", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddSurveyMultiRespColumn {}

    @Form(
        name = "AddSurveyMultiResp",
        location = "component://content/widget/survey/SurveyForms.xml",
        target = "createSurveyMultiResp",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createSurveyMultiResp", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "surveyId", mapName = "survey", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddSurveyMultiResp {}

    @Form(
        name = "ListSurveyPages",
        location = "component://content/widget/survey/SurveyForms.xml",
        type = FormType.LIST,
        target = "updateSurveyPage",
        listName = "surveyPageList",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateSurveyPage", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "surveyId", hidden = @HiddenField),
            @FormField(name = "surveyPageSeqId", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListSurveyPages {}

    @Form(
        name = "AddSurveyPage",
        location = "component://content/widget/survey/SurveyForms.xml",
        target = "createSurveyPage",
        defaultMapName = "surveyPage",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createSurveyPage", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "surveyId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddSurveyPage {}

    @Form(
        name = "CreateSurveyQuestion",
        location = "component://content/widget/survey/SurveyForms.xml",
        target = "createSurveyQuestion",
        defaultMapName = "surveyQuestion",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createSurveyQuestion")
        },
        fields = {
            @FormField(name = "surveyQuestionId", useWhen = "surveyQuestion!=null", hidden = @HiddenField),
            @FormField(name = "surveyId", hidden = @HiddenField(value = "${surveyId}")),
            @FormField(name = "surveyQuestionSeqId", ignored = @IgnoredField),
            @FormField(name = "surveyQuestionCategoryId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "SurveyQuestionCategory", description = "${description}"))),
            @FormField(name = "surveyQuestionTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "SurveyQuestionType", description = "${description}"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "surveyQuestion!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "surveyQuestion!=null", target = "updateSurveyQuestion")
        }
    )
    public interface CreateSurveyQuestion {}

    @Form(
        name = "CreateSurveyQuestionCategory",
        location = "component://content/widget/survey/SurveyForms.xml",
        target = "createSurveyQuestionCategory",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createSurveyQuestionCategory")
        },
        fields = {
            @FormField(name = "surveyId", hidden = @HiddenField(value = "${surveyId}")),
            @FormField(name = "parentCategoryId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "SurveyQuestionCategory", description = "${description} [${surveyQuestionCategoryId}]", keyFieldName = "surveyQuestionCategoryId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateSurveyQuestionCategory {}

    @Form(
        name = "CreateSurveyQuestionOption",
        location = "component://content/widget/survey/SurveyForms.xml",
        target = "createSurveyQuestionOption",
        defaultMapName = "surveyQuestionOption",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createSurveyQuestionOption")
        },
        fields = {
            @FormField(name = "surveyId", hidden = @HiddenField(value = "${surveyId}")),
            @FormField(name = "surveyQuestionId", hidden = @HiddenField(value = "${surveyQuestionId}")),
            @FormField(name = "surveyOptionSeqId", useWhen = "surveyQuestionOption!=null", hidden = @HiddenField),
            @FormField(name = "amountBaseUomId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "durationUomId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} (${abbreviation})", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "TIME_FREQ_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "surveyQuestionOption!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "surveyQuestionOption==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "surveyQuestionOption!=null", target = "updateSurveyQuestionOption")
        }
    )
    public interface CreateSurveyQuestionOption {}

    @Form(
        name = "FindSurveyResponse",
        location = "component://content/widget/survey/SurveyForms.xml",
        target = "FindSurveyResponse",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "SurveyResponse", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindSurveyResponse {}

    @Form(
        name = "ListFindSurveyResponse",
        location = "component://content/widget/survey/SurveyForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindSurveyResponse",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "SurveyResponse", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "surveyResponseId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditSurveyResponse", description = "${surveyResponseId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "surveyResponseId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "SurveyResponse"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListFindSurveyResponse {}

    @Form(
        name = "lookupSurvey",
        location = "component://content/widget/survey/SurveyForms.xml",
        target = "LookupSurvey",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Survey", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "surveyId", title = "${uiLabelMap.ContentSurveySurveyId}", textFind = @TextFindField),
            @FormField(name = "isAnonymous", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "allowMultiple", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "allowUpdate", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupSurvey {}

    @Form(
        name = "listLookupSurvey",
        location = "component://content/widget/survey/SurveyForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupSurvey",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Survey", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "surveyId", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${surveyId}')", urlMode = UrlMode.PLAIN, description = "${surveyId}", alsoHidden = false))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Survey"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupSurvey {}

    @Form(
        name = "lookupSurveyResponse",
        location = "component://content/widget/survey/SurveyForms.xml",
        target = "LookupSurveyResponse",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "SurveyResponse", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupSurveyResponse {}

    @Form(
        name = "listLookupSurveyResponse",
        location = "component://content/widget/survey/SurveyForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupSurveyResponse",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "SurveyResponse", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "surveyResponseId", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${surveyResponseId}')", urlMode = UrlMode.PLAIN, description = "${surveyResponseId}", alsoHidden = false))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "SurveyResponse"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupSurveyResponse {}

}
