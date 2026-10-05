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
public class ContentsetupContentSetupForms {

    @Form(
        name = "AddContentType",
        location = "component://content/widget/contentsetup/ContentSetupForms.xml",
        target = "addContentType",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createContentType")
        },
        fields = {
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddContentType {}

    @Form(
        name = "UpdateContentType",
        location = "component://content/widget/contentsetup/ContentSetupForms.xml",
        type = FormType.LIST,
        target = "updateContentType",
        listName = "contentList",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateContentType")
        },
        fields = {
            @FormField(name = "contentTypeId", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeContentType", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentTypeId")}))
        }
    )
    public interface UpdateContentType {}

    @Form(
        name = "AddContentTypeAttr",
        location = "component://content/widget/contentsetup/ContentSetupForms.xml",
        target = "addContentTypeAttr",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createContentTypeAttr")
        },
        fields = {
            @FormField(name = "contentTypeId", entityName = "ContentType", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ContentType", description = "${description}", keyFieldName = "contentTypeId"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddContentTypeAttr {}

    @Form(
        name = "UpdateContentTypeAttr",
        location = "component://content/widget/contentsetup/ContentSetupForms.xml",
        type = FormType.LIST,
        target = "updateContentTypeAttr",
        listName = "contentList",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createContentTypeAttr")
        },
        fields = {
            @FormField(name = "contentTypeId", display = @DisplayField),
            @FormField(name = "attrName", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeContentTypeAttr", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentTypeId"), @ParameterDef(paramName = "attrName")}))
        }
    )
    public interface UpdateContentTypeAttr {}

    @Form(
        name = "AddContentAssocType",
        location = "component://content/widget/contentsetup/ContentSetupForms.xml",
        target = "addContentAssocType",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createContentAssocType")
        },
        fields = {
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddContentAssocType {}

    @Form(
        name = "UpdateContentAssocType",
        location = "component://content/widget/contentsetup/ContentSetupForms.xml",
        type = FormType.LIST,
        target = "updateContentAssocType",
        listName = "contentList",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateContentAssocType")
        },
        fields = {
            @FormField(name = "contentAssocTypeId", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeContentAssocType", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentAssocTypeId")}))
        }
    )
    public interface UpdateContentAssocType {}

    @Form(
        name = "AddContentPurposeType",
        location = "component://content/widget/contentsetup/ContentSetupForms.xml",
        target = "addContentPurposeType",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createContentPurposeType")
        },
        fields = {
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddContentPurposeType {}

    @Form(
        name = "UpdateContentPurposeType",
        location = "component://content/widget/contentsetup/ContentSetupForms.xml",
        type = FormType.LIST,
        target = "updateContentPurposeType",
        listName = "contentList",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateContentPurposeType")
        },
        fields = {
            @FormField(name = "contentPurposeTypeId", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeContentPurposeType", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentPurposeTypeId")}))
        }
    )
    public interface UpdateContentPurposeType {}

    @Form(
        name = "AddContentAssocPredicate",
        location = "component://content/widget/contentsetup/ContentSetupForms.xml",
        target = "addContentAssocPredicate",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createContentAssocPredicate")
        },
        fields = {
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddContentAssocPredicate {}

    @Form(
        name = "UpdateContentAssocPredicate",
        location = "component://content/widget/contentsetup/ContentSetupForms.xml",
        type = FormType.LIST,
        target = "updateContentAssocPredicate",
        listName = "contentList",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateContentAssocPredicate")
        },
        fields = {
            @FormField(name = "contentAssocPredicateId", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeContentAssocPredicate", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentAssocPredicateId")}))
        }
    )
    public interface UpdateContentAssocPredicate {}

    @Form(
        name = "AddContentOperation",
        location = "component://content/widget/contentsetup/ContentSetupForms.xml",
        target = "addContentOperation",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createContentOperation")
        },
        fields = {
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddContentOperation {}

    @Form(
        name = "UpdateContentOperation",
        location = "component://content/widget/contentsetup/ContentSetupForms.xml",
        type = FormType.LIST,
        target = "updateContentOperation",
        listName = "contentList",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateContentOperation")
        },
        fields = {
            @FormField(name = "contentOperationId", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeContentOperation", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentOperationId")}))
        }
    )
    public interface UpdateContentOperation {}

    @Form(
        name = "AddContentPurposeOperation",
        location = "component://content/widget/contentsetup/ContentSetupForms.xml",
        target = "addContentPurposeOperation",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createContentPurposeOperation")
        },
        fields = {
            @FormField(name = "contentPurposeTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContentPurposeType", description = "${description}", keyFieldName = "contentPurposeTypeId"))),
            @FormField(name = "contentOperationId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ContentOperation", description = "${description}", keyFieldName = "contentOperationId"))),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddContentPurposeOperation {}

    @Form(
        name = "UpdateContentPurposeOperation",
        location = "component://content/widget/contentsetup/ContentSetupForms.xml",
        type = FormType.LIST,
        listName = "contentList",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateContentPurposeOperation", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeContentPurposeOperation", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "contentPurposeTypeId"), @ParameterDef(paramName = "contentOperationId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "statusId"), @ParameterDef(paramName = "privilegeEnumId")}))
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "ContentPurposeOperation")})
    )
    public interface UpdateContentPurposeOperation {}

}
