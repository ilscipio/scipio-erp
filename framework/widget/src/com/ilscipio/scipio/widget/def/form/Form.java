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
package com.ilscipio.scipio.widget.def.form;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a Scipio form widget, equivalent to widget-form.xsd form element.
 *
 * <p>This annotation can be applied to a class or method to define a form.</p>
 *
 * <p>Example usage for a simple form:</p>
 * <pre>
 * {@literal @}Form(name = "MyForm", type = FormType.SINGLE, target = "processForm",
 *     fields = {
 *         {@literal @}FormField(name = "name", title = "Name", text = {@literal @}TextField(size = 30)),
 *         {@literal @}FormField(name = "submit", title = "Submit", submit = {@literal @}SubmitField)
 *     })
 * public interface MyFormDef {}
 * </pre>
 *
 * <p>Example usage for a list form:</p>
 * <pre>
 * {@literal @}Form(name = "MyListForm", type = FormType.LIST, listName = "items",
 *     autoFieldsEntity = {@literal @}AutoFieldsEntity(entityName = "MyEntity",
 *         defaultFieldType = DefaultFieldType.DISPLAY))
 * public interface MyListFormDef {}
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
@Repeatable(FormList.class)
public @interface Form {

    /**
     * Form name; required.
     */
    String name();

    /**
     * Form type: single, list, multi, upload.
     */
    FormType type() default FormType.SINGLE;

    /**
     * Target URL for form submission.
     */
    String target() default "";

    /**
     * Target window for form submission.
     */
    String targetWindow() default "";

    /**
     * Target type for the URL.
     */
    TargetType targetType() default TargetType.INTRA_APP;

    /**
     * HTML id attribute.
     */
    String id() default "";

    /**
     * CSS style class.
     */
    String style() default "";

    /**
     * Field to focus on form load.
     */
    String focusFieldName() default "";

    /**
     * Form title.
     */
    String title() default "";

    /**
     * Message to display when form data is empty.
     */
    String emptyFormDataMessage() default "";

    /**
     * Tooltip text.
     */
    String tooltip() default "";

    /**
     * List name in context for list/multi forms.
     */
    String listName() default "";

    /**
     * Entry name for iteration in list/multi forms.
     */
    String listEntryName() default "";

    /**
     * Default map name for field values.
     */
    String defaultMapName() default "";

    /**
     * Default entity name for field type derivation.
     */
    String defaultEntityName() default "";

    /**
     * Default service name for field type derivation.
     */
    String defaultServiceName() default "";

    /**
     * Form to extend.
     */
    String extendsForm() default "";

    /**
     * Resource location of form to extend.
     */
    String extendsResource() default "";

    // Pagination

    /**
     * Whether to paginate: true, false, or expression.
     */
    String paginate() default "";

    /**
     * Target for pagination links.
     */
    String paginateTarget() default "";

    /**
     * Parameter name for view size.
     */
    String paginateSizeField() default "viewSize";

    /**
     * Parameter name for view index.
     */
    String paginateIndexField() default "viewIndex";

    /**
     * Label for first page link.
     */
    String paginateFirstLabel() default "";

    /**
     * Label for previous page link.
     */
    String paginatePreviousLabel() default "";

    /**
     * Label for next page link.
     */
    String paginateNextLabel() default "";

    /**
     * Label for last page link.
     */
    String paginateLastLabel() default "";

    /**
     * Label for view size selector.
     */
    String paginateViewSizeLabel() default "";

    /**
     * CSS style for pagination controls.
     */
    String paginateStyle() default "";

    /**
     * Anchor for pagination links.
     */
    String paginateTargetAnchor() default "";

    /**
     * Override list size for pagination. Accepts ${} notation.
     */
    String overrideListSize() default "";

    /**
     * Item index separator for multi forms.
     */
    String itemIndexSeparator() default "";

    // Styles

    /**
     * CSS class for header row.
     */
    String headerRowStyle() default "";

    /**
     * CSS class for odd rows.
     */
    String oddRowStyle() default "";

    /**
     * CSS class for even rows.
     */
    String evenRowStyle() default "";

    /**
     * Default CSS class for table.
     */
    String defaultTableStyle() default "";

    /**
     * Default CSS class for titles.
     */
    String defaultTitleStyle() default "";

    /**
     * Default CSS class for widgets.
     */
    String defaultWidgetStyle() default "";

    /**
     * Default CSS class for tooltips.
     */
    String defaultTooltipStyle() default "";

    /**
     * Default CSS class for title areas.
     */
    String defaultTitleAreaStyle() default "";

    /**
     * Default CSS class for widget areas.
     */
    String defaultWidgetAreaStyle() default "";

    /**
     * CSS class for form title area in multi-form widget.
     */
    String formTitleAreaStyle() default "";

    /**
     * CSS class for form widget area in multi-form widget.
     */
    String formWidgetAreaStyle() default "";

    /**
     * Default CSS class for required field indicators.
     */
    String defaultRequiredFieldStyle() default "";

    /**
     * Parameter name for specifying sorted column.
     */
    String sortFieldParameterName() default "";

    /**
     * Default CSS class for sort field links.
     */
    String defaultSortFieldStyle() default "";

    /**
     * Default CSS class for ascending sort.
     */
    String defaultSortFieldAscStyle() default "";

    /**
     * Default CSS class for descending sort.
     */
    String defaultSortFieldDescStyle() default "";

    // Behavior

    /**
     * Whether to enable browser autocomplete.
     */
    boolean clientAutocompleteFields() default true;

    /**
     * Whether to use separate columns for labels.
     */
    boolean separateColumns() default false;

    /**
     * Whether to group columns in list/multi forms.
     */
    boolean groupColumns() default true;

    /**
     * Number of items per page.
     */
    int viewSize() default 0;

    /**
     * Row count expression.
     */
    String rowCount() default "";

    /**
     * Whether to hide the header row in list forms.
     */
    boolean hideHeader() default false;

    /**
     * Whether to enable row submit in multi forms.
     */
    boolean useRowSubmit() default false;

    /**
     * Whether to skip form start rendering.
     */
    String skipStart() default "";

    /**
     * Whether to skip form end rendering.
     */
    String skipEnd() default "";

    /**
     * SCIPIO: Whether to use request parameters for field values.
     */
    String useRequestParameters() default "";

    // SCIPIO-specific

    /**
     * SCIPIO: HTTP method: post, get.
     */
    String method() default "";

    /**
     * SCIPIO: JSON-like extra attributes for theme.
     */
    String attribs() default "";

    /**
     * SCIPIO: Default position span for fields.
     */
    int defaultPositionSpan() default 0;

    /**
     * SCIPIO: Condition for hiding header. EL expression.
     */
    String hideHeaderWhen() default "";

    /**
     * SCIPIO: Condition for hiding table. EL expression.
     */
    String hideTableWhen() default "";

    /**
     * SCIPIO: Condition for showing alternate text. EL expression.
     */
    String useAlternateTextWhen() default "";

    /**
     * SCIPIO: Alternate text to show when useAlternateTextWhen is true.
     */
    String alternateText() default "";

    /**
     * SCIPIO: Style for alternate text.
     */
    String alternateTextStyle() default "";

    /**
     * SCIPIO: Explicit total grid positions for this form.
     */
    int positions() default 0;

    /**
     * SCIPIO: Whether to combine adjacent action fields.
     */
    boolean defaultCombineActionFields() default true;

    // Content

    /**
     * Auto-fields from services.
     */
    AutoFieldsService[] autoFieldsService() default {};

    /**
     * Auto-fields from entities.
     */
    AutoFieldsEntity[] autoFieldsEntity() default {};

    /**
     * Form fields.
     */
    FormField[] fields() default {};

    /**
     * Form-level actions.
     */
    FormActions actions() default @FormActions(UNSET = true);

    /**
     * Row-level actions for list/multi forms.
     */
    RowActions rowActions() default @RowActions(UNSET = true);

    /**
     * Sort order configuration.
     */
    SortOrder sortOrder() default @SortOrder(UNSET = true);

    /**
     * Alternative targets.
     */
    AltTarget[] altTargets() default {};

    /**
     * Form-level AJAX update areas.
     */
    OnEventUpdateArea[] onEventUpdateAreas() default {};

    // ========================================================================
    // Location alias attributes for backward compatibility with XML references
    // ========================================================================

    /**
     * Single alias location for backward compatibility with XML references.
     *
     * <p>When specified, lookups for this component:// location will resolve to this
     * annotated form instead of the XML file.</p>
     *
     * <p>Example: "component://setup/widget/SetupForms.xml"</p>
     *
     * <p>SCIPIO: 4.0.0: Added for XML-to-annotation migration support.</p>
     */
    String location() default "";

    /**
     * Multiple alias locations for backward compatibility with XML references.
     *
     * <p>When specified, lookups for any of these component:// locations will resolve
     * to this annotated form instead of the XML file.</p>
     *
     * <p>Example: {"component://setup/widget/SetupForms.xml", "component://setup/widget/OldForms.xml"}</p>
     *
     * <p>SCIPIO: 4.0.0: Added for XML-to-annotation migration support.</p>
     */
    String[] locations() default {};
}
