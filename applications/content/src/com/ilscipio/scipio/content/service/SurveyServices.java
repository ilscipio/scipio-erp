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
package com.ilscipio.scipio.content.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class SurveyServices {

    /**
     * Create a Survey
     */
    @Service(
        name = "createSurvey",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "createSurvey",
        description = "Create a Survey",
        defaultEntityName = "Survey",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateSurvey {}

    /**
     * Update a Survey
     */
    @Service(
        name = "updateSurvey",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "updateSurvey",
        description = "Update a Survey",
        defaultEntityName = "Survey",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateSurvey {}

    /**
     * Delete Survey
     */
    @Service(
        name = "deleteSurvey",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "deleteSurvey",
        description = "Delete Survey",
        defaultEntityName = "Survey",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteSurvey {}

    /**
     * Create a SurveyMultiResp; surveyMultiRespId will be auto-sequenced
     */
    @Service(
        name = "createSurveyMultiResp",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "createSurveyMultiResp",
        description = "Create a SurveyMultiResp; surveyMultiRespId will be auto-sequenced",
        defaultEntityName = "SurveyMultiResp",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "surveyId", type = "String", mode = "IN"),
            @Attribute(name = "surveyMultiRespId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateSurveyMultiResp {}

    /**
     * Update a SurveyMultiResp
     */
    @Service(
        name = "updateSurveyMultiResp",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "updateSurveyMultiResp",
        description = "Update a SurveyMultiResp",
        defaultEntityName = "SurveyMultiResp",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateSurveyMultiResp {}

    /**
     * Delete SurveyMultiResp
     */
    @Service(
        name = "deleteSurveyMultiResp",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "deleteSurveyMultiResp",
        description = "Delete SurveyMultiResp",
        defaultEntityName = "SurveyMultiResp",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteSurveyMultiResp {}

    /**
     * Create a SurveyMultiRespColumn; surveyMultiRespColId will be auto-sequenced
     */
    @Service(
        name = "createSurveyMultiRespColumn",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "createSurveyMultiRespColumn",
        description = "Create a SurveyMultiRespColumn; surveyMultiRespColId will be auto-sequenced",
        defaultEntityName = "SurveyMultiRespColumn",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "surveyId", type = "String", mode = "IN"),
            @Attribute(name = "surveyMultiRespId", type = "String", mode = "IN"),
            @Attribute(name = "surveyMultiRespColId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateSurveyMultiRespColumn {}

    /**
     * Update a SurveyMultiRespColumn
     */
    @Service(
        name = "updateSurveyMultiRespColumn",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "updateSurveyMultiRespColumn",
        description = "Update a SurveyMultiRespColumn",
        defaultEntityName = "SurveyMultiRespColumn",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateSurveyMultiRespColumn {}

    /**
     * Delete SurveyMultiRespColumn
     */
    @Service(
        name = "deleteSurveyMultiRespColumn",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "deleteSurveyMultiRespColumn",
        description = "Delete SurveyMultiRespColumn",
        defaultEntityName = "SurveyMultiRespColumn",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteSurveyMultiRespColumn {}

    /**
     * Create a SurveyPage; the surveyPageSeqId will be auto-generated
     */
    @Service(
        name = "createSurveyPage",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "createSurveyPage",
        description = "Create a SurveyPage; the surveyPageSeqId will be auto-generated",
        defaultEntityName = "SurveyPage",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "surveyId", type = "String", mode = "IN"),
            @Attribute(name = "surveyPageSeqId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateSurveyPage {}

    /**
     * Update a SurveyPage
     */
    @Service(
        name = "updateSurveyPage",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "updateSurveyPage",
        description = "Update a SurveyPage",
        defaultEntityName = "SurveyPage",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateSurveyPage {}

    /**
     * Delete SurveyPage
     */
    @Service(
        name = "deleteSurveyPage",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "deleteSurveyPage",
        description = "Delete SurveyPage",
        defaultEntityName = "SurveyPage",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteSurveyPage {}

    /**
     * Create a SurveyApplType
     */
    @Service(
        name = "createSurveyApplType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "createSurveyApplType",
        description = "Create a SurveyApplType",
        defaultEntityName = "SurveyApplType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateSurveyApplType {}

    /**
     * Update a SurveyApplType
     */
    @Service(
        name = "updateSurveyApplType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "updateSurveyApplType",
        description = "Update a SurveyApplType",
        defaultEntityName = "SurveyApplType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateSurveyApplType {}

    /**
     * Delete SurveyApplType
     */
    @Service(
        name = "deleteSurveyApplType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "deleteSurveyApplType",
        description = "Delete SurveyApplType",
        defaultEntityName = "SurveyApplType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteSurveyApplType {}

    /**
     * Create a SurveyQuestion
     */
    @Service(
        name = "createSurveyQuestion",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "createSurveyQuestion",
        description = "Create a SurveyQuestion",
        defaultEntityName = "SurveyQuestion",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "surveyId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateSurveyQuestion {}

    /**
     * Update a SurveyQuestion
     */
    @Service(
        name = "updateSurveyQuestion",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "updateSurveyQuestion",
        description = "Update a SurveyQuestion",
        defaultEntityName = "SurveyQuestion",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateSurveyQuestion {}

    /**
     * Delete SurveyQuestion
     */
    @Service(
        name = "deleteSurveyQuestion",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "deleteSurveyQuestion",
        description = "Delete SurveyQuestion",
        defaultEntityName = "SurveyQuestion",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteSurveyQuestion {}

    /**
     * Create a SurveyQuestionOption
     */
    @Service(
        name = "createSurveyQuestionOption",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "createSurveyQuestionOption",
        description = "Create a SurveyQuestionOption",
        defaultEntityName = "SurveyQuestionOption",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "surveyQuestionId", type = "String", mode = "IN"),
            @Attribute(name = "surveyOptionSeqId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateSurveyQuestionOption {}

    /**
     * Update a SurveyQuestionOption
     */
    @Service(
        name = "updateSurveyQuestionOption",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "updateSurveyQuestionOption",
        description = "Update a SurveyQuestionOption",
        defaultEntityName = "SurveyQuestionOption",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateSurveyQuestionOption {}

    /**
     * Delete SurveyQuestionOption
     */
    @Service(
        name = "deleteSurveyQuestionOption",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "deleteSurveyQuestionOption",
        description = "Delete SurveyQuestionOption",
        defaultEntityName = "SurveyQuestionOption",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteSurveyQuestionOption {}

    /**
     * Create a SurveyQuestionAppl
     */
    @Service(
        name = "createSurveyQuestionAppl",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "createSurveyQuestionAppl",
        description = "Create a SurveyQuestionAppl",
        defaultEntityName = "SurveyQuestionAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateSurveyQuestionAppl {}

    /**
     * Update a SurveyQuestionAppl
     */
    @Service(
        name = "updateSurveyQuestionAppl",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "updateSurveyQuestionAppl",
        description = "Update a SurveyQuestionAppl",
        defaultEntityName = "SurveyQuestionAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateSurveyQuestionAppl {}

    /**
     * Delete SurveyQuestionAppl
     */
    @Service(
        name = "deleteSurveyQuestionAppl",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "deleteSurveyQuestionAppl",
        description = "Delete SurveyQuestionAppl",
        defaultEntityName = "SurveyQuestionAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteSurveyQuestionAppl {}

    /**
     * Create a SurveyQuestionCategory
     */
    @Service(
        name = "createSurveyQuestionCategory",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "createSurveyQuestionCategory",
        description = "Create a SurveyQuestionCategory",
        defaultEntityName = "SurveyQuestionCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateSurveyQuestionCategory {}

    /**
     * Update a SurveyQuestionCategory
     */
    @Service(
        name = "updateSurveyQuestionCategory",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "updateSurveyQuestionCategory",
        description = "Update a SurveyQuestionCategory",
        defaultEntityName = "SurveyQuestionCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateSurveyQuestionCategory {}

    /**
     * Delete SurveyQuestionCategory
     */
    @Service(
        name = "deleteSurveyQuestionCategory",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "deleteSurveyQuestionCategory",
        description = "Delete SurveyQuestionCategory",
        defaultEntityName = "SurveyQuestionCategory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteSurveyQuestionCategory {}

    /**
     * Create a SurveyQuestionType
     */
    @Service(
        name = "createSurveyQuestionType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "createSurveyQuestionType",
        description = "Create a SurveyQuestionType",
        defaultEntityName = "SurveyQuestionType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateSurveyQuestionType {}

    /**
     * Update a SurveyQuestionType
     */
    @Service(
        name = "updateSurveyQuestionType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "updateSurveyQuestionType",
        description = "Update a SurveyQuestionType",
        defaultEntityName = "SurveyQuestionType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateSurveyQuestionType {}

    /**
     * Delete SurveyQuestionType
     */
    @Service(
        name = "deleteSurveyQuestionType",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "deleteSurveyQuestionType",
        description = "Delete SurveyQuestionType",
        defaultEntityName = "SurveyQuestionType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteSurveyQuestionType {}

    /**
     * Create a SurveyTrigger
     */
    @Service(
        name = "createSurveyTrigger",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "createSurveyTrigger",
        description = "Create a SurveyTrigger",
        defaultEntityName = "SurveyTrigger",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", excludeFields = {"fromDate"}),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "fromDate", type = "String", mode = "INOUT", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "CREATE")
    )
    public interface CreateSurveyTrigger {}

    /**
     * Update a SurveyTrigger
     */
    @Service(
        name = "updateSurveyTrigger",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "updateSurveyQuestionType",
        description = "Update a SurveyTrigger",
        defaultEntityName = "SurveyTrigger",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "UPDATE")
    )
    public interface UpdateSurveyTrigger {}

    /**
     * Delete SurveyTrigger
     */
    @Service(
        name = "deleteSurveyTrigger",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "deleteSurveyTrigger",
        description = "Delete SurveyTrigger",
        defaultEntityName = "SurveyTrigger",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "contentManagerPermission", mainAction = "DELETE")
    )
    public interface DeleteSurveyTrigger {}

    /**
     * Create a Survey Response w/ Response Answers
     */
    @Service(
        name = "createSurveyResponse",
        engine = "simple",
        location = "component://content/script/org/ofbiz/content/survey/SurveyServices.xml",
        invoke = "createSurveyResponse",
        description = "Create a Survey Response w/ Response Answers",
        entityAttributes = {
            @EntityAttributes(entityName = "SurveyResponse", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "answers", type = "Map", mode = "IN"),
            @Attribute(name = "surveyResponseId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "productStoreSurveyId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "dataResourceId", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "surveyId", mode = "INOUT", optional = "false")
        }
    )
    public interface CreateSurveyResponse {}

    /**
     * Interface for Survey Response Processing services defined on the Survey
     */
    @Service(
        name = "surveyResponseProcessInterface",
        engine = "interface",
        description = "Interface for Survey Response Processing services defined on the Survey",
        attributes = {
            @Attribute(name = "surveyResponseId", type = "String", mode = "IN")
        }
    )
    public interface SurveyResponseProcessInterface {}

    /**
     * Create a Survey and related entities from AcroForm
     */
    @Service(
        name = "buildSurveyFromPdf",
        engine = "java",
        location = "org.ofbiz.content.survey.PdfSurveyServices",
        invoke = "buildSurveyFromPdf",
        description = "Create a Survey and related entities from AcroForm",
        attributes = {
            @Attribute(name = "pdfFileNameIn", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inputByteBuffer", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "surveyName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "surveyId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface BuildSurveyFromPdf {}

    /**
     * Create a Survey and related entities from AcroForm
     */
    @Service(
        name = "buildSurveyResponseFromPdf",
        engine = "java",
        location = "org.ofbiz.content.survey.PdfSurveyServices",
        invoke = "buildSurveyResponseFromPdf",
        description = "Create a Survey and related entities from AcroForm",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "pdfFileNameIn", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inputByteBuffer", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "surveyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "surveyResponseId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface BuildSurveyResponseFromPdf {}

    /**
     * Get fields from AcroForm
     */
    @Service(
        name = "getAcroFieldsFromPdf",
        engine = "java",
        location = "org.ofbiz.content.survey.PdfSurveyServices",
        invoke = "getAcroFieldsFromPdf",
        description = "Get fields from AcroForm",
        attributes = {
            @Attribute(name = "pdfFileNameIn", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inputByteBuffer", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "acroFieldMap", type = "Map", mode = "OUT")
        }
    )
    public interface GetAcroFieldsFromPdf {}

    /**
     * Get fields from AcroForm
     */
    @Service(
        name = "setAcroFieldsFromSurveyResponse",
        engine = "java",
        location = "org.ofbiz.content.survey.PdfSurveyServices",
        invoke = "setAcroFieldsFromSurveyResponse",
        description = "Get fields from AcroForm",
        attributes = {
            @Attribute(name = "pdfFileNameIn", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inputByteBuffer", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "surveyResponseId", type = "String", mode = "IN"),
            @Attribute(name = "outByteBuffer", type = "java.nio.ByteBuffer", mode = "OUT", optional = "true")
        }
    )
    public interface SetAcroFieldsFromSurveyResponse {}

    /**
     * Get fields from AcroForm
     */
    @Service(
        name = "setAcroFields",
        engine = "java",
        location = "org.ofbiz.content.survey.PdfSurveyServices",
        invoke = "setAcroFields",
        description = "Get fields from AcroForm",
        attributes = {
            @Attribute(name = "pdfFileNameIn", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inputByteBuffer", type = "java.nio.ByteBuffer", mode = "IN", optional = "true"),
            @Attribute(name = "acroFieldMap", type = "Map", mode = "IN"),
            @Attribute(name = "outByteBuffer", type = "java.nio.ByteBuffer", mode = "OUT", optional = "true")
        }
    )
    public interface SetAcroFields {}

    /**
     * Build Pdf From Survey Response
     */
    @Service(
        name = "buildPdfFromSurveyResponse",
        engine = "java",
        location = "org.ofbiz.content.survey.PdfSurveyServices",
        invoke = "buildPdfFromSurveyResponse",
        description = "Build Pdf From Survey Response",
        attributes = {
            @Attribute(name = "surveyResponseId", type = "String", mode = "IN"),
            @Attribute(name = "outByteBuffer", type = "java.nio.ByteBuffer", mode = "OUT")
        }
    )
    public interface BuildPdfFromSurveyResponse {}

    /**
     * Build list of questions and answers From Survey Response
     */
    @Service(
        name = "buildSurveyQuestionsAndAnswers",
        engine = "java",
        location = "org.ofbiz.content.survey.PdfSurveyServices",
        invoke = "buildSurveyQuestionsAndAnswers",
        description = "Build list of questions and answers From Survey Response",
        attributes = {
            @Attribute(name = "surveyResponseId", type = "String", mode = "IN"),
            @Attribute(name = "questionsAndAnswers", type = "List", mode = "OUT")
        }
    )
    public interface BuildSurveyQuestionsAndAnswers {}

}
