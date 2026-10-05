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
package com.ilscipio.scipio.widget.test;

import com.ilscipio.scipio.widget.def.form.*;
import com.ilscipio.scipio.widget.def.screen.SetAction;
import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.FieldMap;

/**
 * Test class demonstrating annotation-based form definitions.
 *
 * <p>This class contains examples of various form patterns using annotations
 * instead of XML definitions.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations testing.</p>
 */
public class TestFormsAnnotated {

    /**
     * Example 1: Simple single form with text fields.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;form name="TestSimpleForm" type="single" target="processSimple"&gt;
     *     &lt;field name="name" title="Name"&gt;
     *         &lt;text size="30" maxlength="100"/&gt;
     *     &lt;/field&gt;
     *     &lt;field name="description" title="Description"&gt;
     *         &lt;textarea cols="60" rows="5"/&gt;
     *     &lt;/field&gt;
     *     &lt;field name="submit" title="Submit"&gt;
     *         &lt;submit button-type="button"/&gt;
     *     &lt;/field&gt;
     * &lt;/form&gt;
     * </pre>
     */
    @Form(
        name = "TestSimpleForm",
        type = FormType.SINGLE,
        target = "processSimple",
        fields = {
            @FormField(name = "name", title = "Name",
                text = @TextField(size = 30, maxlength = 100)),
            @FormField(name = "description", title = "Description",
                textarea = @TextareaField(cols = 60, rows = 5)),
            @FormField(name = "submit", title = "Submit",
                submit = @SubmitField(buttonType = "button"))
        }
    )
    public interface TestSimpleForm {}

    /**
     * Example 2: Form with drop-down field and entity options.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;form name="TestDropDownForm" type="single" target="processDropDown"&gt;
     *     &lt;field name="statusId" title="Status"&gt;
     *         &lt;drop-down allow-empty="true"&gt;
     *             &lt;entity-options entity-name="StatusItem" key-field-name="statusId" description="${description}"&gt;
     *                 &lt;entity-constraint name="statusTypeId" value="ORDER_STATUS"/&gt;
     *                 &lt;entity-order-by field-name="sequenceId"/&gt;
     *             &lt;/entity-options&gt;
     *         &lt;/drop-down&gt;
     *     &lt;/field&gt;
     * &lt;/form&gt;
     * </pre>
     */
    @Form(
        name = "TestDropDownForm",
        type = FormType.SINGLE,
        target = "processDropDown",
        fields = {
            @FormField(name = "statusId", title = "Status",
                dropDown = @DropDownField(
                    allowEmpty = true,
                    entityOptions = @EntityOptions(
                        entityName = "StatusItem",
                        keyFieldName = "statusId",
                        description = "${description}",
                        constraints = @EntityConstraint(name = "statusTypeId", value = "ORDER_STATUS"),
                        orderBy = @EntityOrderBy(fieldName = "sequenceId")
                    )
                )
            )
        }
    )
    public interface TestDropDownForm {}

    /**
     * Example 3: List form with auto-fields from entity.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;form name="TestListForm" type="list" list-name="testingList"&gt;
     *     &lt;auto-fields-entity entity-name="Testing" default-field-type="display"/&gt;
     * &lt;/form&gt;
     * </pre>
     */
    @Form(
        name = "TestListForm",
        type = FormType.LIST,
        listName = "testingList",
        autoFieldsEntity = @AutoFieldsEntity(
            entityName = "Testing",
            defaultFieldType = DefaultFieldType.DISPLAY
        )
    )
    public interface TestListForm {}

    /**
     * Example 4: Form with actions and hidden field.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;form name="TestActionsForm" type="single" target="updateTesting" default-map-name="testing"&gt;
     *     &lt;actions&gt;
     *         &lt;entity-one entity-name="Testing" value-field="testing"/&gt;
     *     &lt;/actions&gt;
     *     &lt;field name="testingId"&gt;
     *         &lt;hidden/&gt;
     *     &lt;/field&gt;
     *     &lt;field name="testingName" title="Name"&gt;
     *         &lt;text size="30"/&gt;
     *     &lt;/field&gt;
     *     &lt;field name="submitButton" title="Update"&gt;
     *         &lt;submit/&gt;
     *     &lt;/field&gt;
     * &lt;/form&gt;
     * </pre>
     */
    @Form(
        name = "TestActionsForm",
        type = FormType.SINGLE,
        target = "updateTesting",
        defaultMapName = "testing",
        actions = @FormActions(
            entityOne = @EntityOneAction(entityName = "Testing", valueField = "testing")
        ),
        fields = {
            @FormField(name = "testingId",
                hidden = @HiddenField()),
            @FormField(name = "testingName", title = "Name",
                text = @TextField(size = 30)),
            @FormField(name = "submitButton", title = "Update",
                submit = @SubmitField())
        }
    )
    public interface TestActionsForm {}

    /**
     * Example 5: Form with hyperlink field.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;form name="TestHyperlinkForm" type="list" list-name="items"&gt;
     *     &lt;field name="itemId" title="Item ID"&gt;
     *         &lt;hyperlink target="ViewItem" description="${itemId}"&gt;
     *             &lt;parameter param-name="itemId" from-field="itemId"/&gt;
     *         &lt;/hyperlink&gt;
     *     &lt;/field&gt;
     *     &lt;field name="itemName" title="Name"&gt;
     *         &lt;display/&gt;
     *     &lt;/field&gt;
     * &lt;/form&gt;
     * </pre>
     */
    @Form(
        name = "TestHyperlinkForm",
        type = FormType.LIST,
        listName = "items",
        fields = {
            @FormField(name = "itemId", title = "Item ID",
                hyperlink = @HyperlinkField(
                    target = "ViewItem",
                    description = "${itemId}",
                    parameters = @ParameterDef(paramName = "itemId", fromField = "itemId")
                )
            ),
            @FormField(name = "itemName", title = "Name",
                display = @DisplayField())
        }
    )
    public interface TestHyperlinkForm {}

    /**
     * Example 6: Form with date-time and date-find fields.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;form name="TestDateForm" type="single" target="searchByDate"&gt;
     *     &lt;field name="createdDate" title="Created Date"&gt;
     *         &lt;date-time type="date"/&gt;
     *     &lt;/field&gt;
     *     &lt;field name="modifiedDate" title="Modified Date Range"&gt;
     *         &lt;date-find type="timestamp"/&gt;
     *     &lt;/field&gt;
     * &lt;/form&gt;
     * </pre>
     */
    @Form(
        name = "TestDateForm",
        type = FormType.SINGLE,
        target = "searchByDate",
        fields = {
            @FormField(name = "createdDate", title = "Created Date",
                dateTime = @DateTimeField(type = "date")),
            @FormField(name = "modifiedDate", title = "Modified Date Range",
                dateFind = @DateFindField(type = "timestamp"))
        }
    )
    public interface TestDateForm {}

    /**
     * Example 7: Form with lookup field.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;form name="TestLookupForm" type="single" target="processLookup"&gt;
     *     &lt;field name="partyId" title="Party"&gt;
     *         &lt;lookup target-form-name="LookupParty" size="20"/&gt;
     *     &lt;/field&gt;
     * &lt;/form&gt;
     * </pre>
     */
    @Form(
        name = "TestLookupForm",
        type = FormType.SINGLE,
        target = "processLookup",
        fields = {
            @FormField(name = "partyId", title = "Party",
                lookup = @LookupField(targetFormName = "LookupParty", size = 20))
        }
    )
    public interface TestLookupForm {}

    /**
     * Example 8: Form with field groups and sort order.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;form name="TestSortOrderForm" type="single" target="processSorted"&gt;
     *     &lt;sort-order&gt;
     *         &lt;field-group id="main" title="Main Fields"&gt;
     *             &lt;sort-field name="name"/&gt;
     *             &lt;sort-field name="description"/&gt;
     *         &lt;/field-group&gt;
     *         &lt;field-group id="additional" title="Additional Fields" collapsible="true" initially-collapsed="true"&gt;
     *             &lt;sort-field name="notes"/&gt;
     *         &lt;/field-group&gt;
     *         &lt;last-field name="submitButton"/&gt;
     *     &lt;/sort-order&gt;
     *     &lt;field name="name" title="Name"&gt;&lt;text/&gt;&lt;/field&gt;
     *     &lt;field name="description" title="Description"&gt;&lt;textarea/&gt;&lt;/field&gt;
     *     &lt;field name="notes" title="Notes"&gt;&lt;textarea/&gt;&lt;/field&gt;
     *     &lt;field name="submitButton" title="Submit"&gt;&lt;submit/&gt;&lt;/field&gt;
     * &lt;/form&gt;
     * </pre>
     */
    @Form(
        name = "TestSortOrderForm",
        type = FormType.SINGLE,
        target = "processSorted",
        sortOrder = @SortOrder(
            fieldGroups = {
                @FieldGroup(id = "main", title = "Main Fields"),
                @FieldGroup(id = "additional", title = "Additional Fields", collapsible = true, initiallyCollapsed = true)
            },
            sortFields = {
                @SortField(name = "name"),
                @SortField(name = "description"),
                @SortField(name = "notes")
            },
            lastFields = @LastField(name = "submitButton")
        ),
        fields = {
            @FormField(name = "name", title = "Name", text = @TextField()),
            @FormField(name = "description", title = "Description", textarea = @TextareaField()),
            @FormField(name = "notes", title = "Notes", textarea = @TextareaField()),
            @FormField(name = "submitButton", title = "Submit", submit = @SubmitField())
        }
    )
    public interface TestSortOrderForm {}

    /**
     * Example 9: Multi form with row submit.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;form name="TestMultiForm" type="multi" list-name="items" use-row-submit="true" target="updateMultiItems"&gt;
     *     &lt;row-actions&gt;
     *         &lt;set field="rowIndex" from-field="_rowSubmit_"/&gt;
     *     &lt;/row-actions&gt;
     *     &lt;field name="itemId"&gt;&lt;hidden/&gt;&lt;/field&gt;
     *     &lt;field name="quantity" title="Quantity"&gt;&lt;text size="6"/&gt;&lt;/field&gt;
     *     &lt;field name="_rowSubmit" title="Update"&gt;&lt;check/&gt;&lt;/field&gt;
     *     &lt;field name="submitButton" title="Submit Selected"&gt;&lt;submit/&gt;&lt;/field&gt;
     * &lt;/form&gt;
     * </pre>
     */
    @Form(
        name = "TestMultiForm",
        type = FormType.MULTI,
        listName = "items",
        useRowSubmit = true,
        target = "updateMultiItems",
        rowActions = @RowActions(
            set = @SetAction(field = "rowIndex", fromField = "_rowSubmit_")
        ),
        fields = {
            @FormField(name = "itemId", hidden = @HiddenField()),
            @FormField(name = "quantity", title = "Quantity", text = @TextField(size = 6)),
            @FormField(name = "_rowSubmit", title = "Update", check = @CheckField()),
            @FormField(name = "submitButton", title = "Submit Selected", submit = @SubmitField())
        }
    )
    public interface TestMultiForm {}

    /**
     * Example 10: Upload form for file uploads.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;form name="TestUploadForm" type="upload" target="uploadFile"&gt;
     *     &lt;field name="uploadedFile" title="File"&gt;
     *         &lt;file size="40"/&gt;
     *     &lt;/field&gt;
     *     &lt;field name="description" title="Description"&gt;
     *         &lt;text size="60"/&gt;
     *     &lt;/field&gt;
     *     &lt;field name="submitButton" title="Upload"&gt;
     *         &lt;submit button-type="button"/&gt;
     *     &lt;/field&gt;
     * &lt;/form&gt;
     * </pre>
     */
    @Form(
        name = "TestUploadForm",
        type = FormType.UPLOAD,
        target = "uploadFile",
        fields = {
            @FormField(name = "uploadedFile", title = "File",
                file = @FileField(size = 40)),
            @FormField(name = "description", title = "Description",
                text = @TextField(size = 60)),
            @FormField(name = "submitButton", title = "Upload",
                submit = @SubmitField(buttonType = "button"))
        }
    )
    public interface TestUploadForm {}

    /**
     * Example 11: Form with text-find for searching.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;form name="TestSearchForm" type="single" target="findItems"&gt;
     *     &lt;field name="itemName" title="Name"&gt;
     *         &lt;text-find default-option="contains" ignore-case="true"/&gt;
     *     &lt;/field&gt;
     *     &lt;field name="price" title="Price Range"&gt;
     *         &lt;range-find type="Double"/&gt;
     *     &lt;/field&gt;
     *     &lt;field name="submitButton" title="Find"&gt;
     *         &lt;submit/&gt;
     *     &lt;/field&gt;
     * &lt;/form&gt;
     * </pre>
     */
    @Form(
        name = "TestSearchForm",
        type = FormType.SINGLE,
        target = "findItems",
        fields = {
            @FormField(name = "itemName", title = "Name",
                textFind = @TextFindField(defaultOption = "contains", ignoreCase = true)),
            @FormField(name = "price", title = "Price Range",
                rangeFind = @RangeFindField()),
            @FormField(name = "submitButton", title = "Find",
                submit = @SubmitField())
        }
    )
    public interface TestSearchForm {}

    /**
     * Example 12: Form with display-entity field.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;form name="TestDisplayEntityForm" type="single"&gt;
     *     &lt;field name="partyId" title="Party"&gt;
     *         &lt;display-entity entity-name="PartyNameView" key-field-name="partyId" description="${firstName} ${lastName}"/&gt;
     *     &lt;/field&gt;
     * &lt;/form&gt;
     * </pre>
     */
    @Form(
        name = "TestDisplayEntityForm",
        type = FormType.SINGLE,
        fields = {
            @FormField(name = "partyId", title = "Party",
                displayEntity = @DisplayEntityField(
                    entityName = "PartyNameView",
                    keyFieldName = "partyId",
                    description = "${firstName} ${lastName}"
                )
            )
        }
    )
    public interface TestDisplayEntityForm {}

    /**
     * Example 13: Form with radio buttons and check boxes.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;form name="TestRadioCheckForm" type="single" target="processChoice"&gt;
     *     &lt;field name="gender" title="Gender"&gt;
     *         &lt;radio&gt;
     *             &lt;option key="M" description="Male"/&gt;
     *             &lt;option key="F" description="Female"/&gt;
     *         &lt;/radio&gt;
     *     &lt;/field&gt;
     *     &lt;field name="interests" title="Interests"&gt;
     *         &lt;check&gt;
     *             &lt;option key="SPORTS" description="Sports"/&gt;
     *             &lt;option key="MUSIC" description="Music"/&gt;
     *             &lt;option key="READING" description="Reading"/&gt;
     *         &lt;/check&gt;
     *     &lt;/field&gt;
     * &lt;/form&gt;
     * </pre>
     */
    @Form(
        name = "TestRadioCheckForm",
        type = FormType.SINGLE,
        target = "processChoice",
        fields = {
            @FormField(name = "gender", title = "Gender",
                radio = @RadioField(
                    options = {
                        @Option(key = "M", description = "Male"),
                        @Option(key = "F", description = "Female")
                    }
                )
            ),
            @FormField(name = "interests", title = "Interests",
                check = @CheckField(
                    options = {
                        @Option(key = "SPORTS", description = "Sports"),
                        @Option(key = "MUSIC", description = "Music"),
                        @Option(key = "READING", description = "Reading")
                    }
                )
            )
        }
    )
    public interface TestRadioCheckForm {}

    /**
     * Example 14: Form with AJAX update area.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;form name="TestAjaxForm" type="single"&gt;
     *     &lt;on-event-update-area event-type="submit" area-id="resultArea" area-target="AjaxResult"&gt;
     *         &lt;parameter param-name="searchTerm" from-field="searchTerm"/&gt;
     *     &lt;/on-event-update-area&gt;
     *     &lt;field name="searchTerm" title="Search"&gt;
     *         &lt;text&gt;
     *             &lt;on-field-event-update-area event-type="change" area-id="suggestionsArea" area-target="Suggestions"/&gt;
     *         &lt;/text&gt;
     *     &lt;/field&gt;
     * &lt;/form&gt;
     * </pre>
     */
    @Form(
        name = "TestAjaxForm",
        type = FormType.SINGLE,
        onEventUpdateAreas = @OnEventUpdateArea(
            eventType = "submit",
            areaId = "resultArea",
            areaTarget = "AjaxResult"
        ),
        fields = {
            @FormField(name = "searchTerm", title = "Search",
                text = @TextField(),
                onFieldEventUpdateAreas = @OnFieldEventUpdateArea(
                    eventType = "change",
                    areaId = "suggestionsArea",
                    areaTarget = "Suggestions"
                )
            )
        }
    )
    public interface TestAjaxForm {}

    /**
     * Example 15: Form that extends another form.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;form name="TestExtendedForm" type="single" extends="CreateSecurityGroup" extends-resource="component://common/widget/SecurityForms.xml"&gt;
     *     &lt;field name="extraField" title="Extra Field"&gt;
     *         &lt;text/&gt;
     *     &lt;/field&gt;
     * &lt;/form&gt;
     * </pre>
     */
    @Form(
        name = "TestExtendedForm",
        type = FormType.SINGLE,
        extendsForm = "CreateSecurityGroup",
        extendsResource = "component://common/widget/SecurityForms.xml",
        fields = {
            @FormField(name = "extraField", title = "Extra Field", text = @TextField())
        }
    )
    public interface TestExtendedForm {}
}
