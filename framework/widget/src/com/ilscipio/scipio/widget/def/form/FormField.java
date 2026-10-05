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
 * Defines a form field, equivalent to widget-form.xsd field element.
 *
 * <p>Exactly one field type should be set (text, hidden, display, dropdown, etc.).</p>
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
@Repeatable(FormFieldList.class)
public @interface FormField {

    /**
     * Field name; required.
     */
    String name();

    /**
     * Map name to get/put value from.
     */
    String mapName() default "";

    /**
     * Entity name for type derivation.
     */
    String entityName() default "";

    /**
     * Field name in entity/service (defaults to name).
     */
    String fieldName() default "";

    /**
     * Service name for type derivation.
     */
    String serviceName() default "";

    /**
     * Attribute name for type derivation.
     */
    String attributeName() default "";

    /**
     * Entry name in map (defaults to name).
     */
    String entryName() default "";

    /**
     * Parameter name for request (defaults to name).
     */
    String parameterName() default "";

    /**
     * Title/label shown to user.
     * Supports ${} expressions.
     */
    String title() default "";

    /**
     * Tooltip text shown on hover.
     * Supports ${} expressions.
     */
    String tooltip() default "";

    /**
     * Link for header in list forms.
     */
    String headerLink() default "";

    /**
     * CSS style for header link.
     */
    String headerLinkStyle() default "";

    // Positioning

    /**
     * Position in row (column for list forms).
     */
    int position() default 1;

    /**
     * SCIPIO: Position span. 0 means auto/occupy all available.
     */
    int positionSpan() default 0;

    /**
     * SCIPIO: Whether to combine with previous field.
     */
    boolean combinePrevious() default false;

    /**
     * Whether to place in separate column.
     */
    boolean separateColumn() default false;

    // Styles

    /**
     * CSS class for title.
     */
    String titleStyle() default "";

    /**
     * CSS class for title area.
     */
    String titleAreaStyle() default "";

    /**
     * SCIPIO: Inline CSS for title area.
     */
    String titleAreaInlineStyle() default "";

    /**
     * CSS class for widget.
     */
    String widgetStyle() default "";

    /**
     * CSS class for widget area.
     */
    String widgetAreaStyle() default "";

    /**
     * CSS class for tooltip.
     */
    String tooltipStyle() default "";

    // Behavior

    /**
     * Condition for when to use this field.
     * Supports Java/Groovy expressions.
     */
    String useWhen() default "";

    /**
     * Condition for when to ignore this field in list/multi forms.
     */
    String ignoreWhen() default "";

    /**
     * Whether to encode output for safety.
     */
    boolean encodeOutput() default true;

    /**
     * Whether this field is required.
     */
    boolean requiredField() default false;

    /**
     * CSS class for required field indicator.
     */
    String requiredFieldStyle() default "";

    /**
     * Whether this field can be sorted in list forms.
     */
    boolean sortField() default false;

    /**
     * CSS class for sort field link.
     */
    String sortFieldStyle() default "";

    /**
     * Help text for sort field.
     */
    String sortFieldHelpText() default "";

    /**
     * CSS class for ascending sort.
     */
    String sortFieldAscStyle() default "";

    /**
     * CSS class for descending sort.
     */
    String sortFieldDescStyle() default "";

    /**
     * When to show red: "never", "before-now", "after-now", "by-name".
     */
    String redWhen() default "by-name";

    // Event handling

    /**
     * JavaScript event to attach.
     */
    String event() default "";

    /**
     * JavaScript action for the event.
     */
    String action() default "";

    /**
     * HTML id for the field.
     */
    String idName() default "";

    /**
     * Parent form name (needed for lookups with skip-start).
     */
    String formName() default "";

    /**
     * HTML tabindex.
     */
    String tabindex() default "";

    /**
     * Condition group for search criteria.
     */
    String conditionGroup() default "";

    /**
     * SCIPIO: JSON-like extra attributes for theme.
     */
    String attribs() default "";

    // Field types (exactly one should be set)

    /**
     * Text input field.
     */
    TextField text() default @TextField(UNSET = true);

    /**
     * Textarea field.
     */
    TextareaField textarea() default @TextareaField(UNSET = true);

    /**
     * Password field.
     */
    PasswordField password() default @PasswordField(UNSET = true);

    /**
     * Dropdown select field.
     */
    DropDownField dropDown() default @DropDownField(UNSET = true);

    /**
     * Checkbox field.
     */
    CheckField check() default @CheckField(UNSET = true);

    /**
     * Radio button field.
     */
    RadioField radio() default @RadioField(UNSET = true);

    /**
     * Date-time field.
     */
    DateTimeField dateTime() default @DateTimeField(UNSET = true);

    /**
     * Display-only field.
     */
    DisplayField display() default @DisplayField(UNSET = true);

    /**
     * Display field with entity lookup.
     */
    DisplayEntityField displayEntity() default @DisplayEntityField(UNSET = true);

    /**
     * Hidden field.
     */
    HiddenField hidden() default @HiddenField(UNSET = true);

    /**
     * Ignored field.
     */
    IgnoredField ignored() default @IgnoredField(UNSET = true);

    /**
     * Hyperlink field.
     */
    HyperlinkField hyperlink() default @HyperlinkField(UNSET = true);

    /**
     * Submit button field.
     */
    SubmitField submit() default @SubmitField(UNSET = true);

    /**
     * Reset button field.
     */
    ResetField reset() default @ResetField(UNSET = true);

    /**
     * Lookup field with popup.
     */
    LookupField lookup() default @LookupField(UNSET = true);

    /**
     * File upload field.
     */
    FileField file() default @FileField(UNSET = true);

    /**
     * Image display field.
     */
    ImageField image() default @ImageField(UNSET = true);

    /**
     * Text search field.
     */
    TextFindField textFind() default @TextFindField(UNSET = true);

    /**
     * Date search field.
     */
    DateFindField dateFind() default @DateFindField(UNSET = true);

    /**
     * Whether the field is disabled.
     */
    boolean disabled() default false;

    /**
     * Range search field.
     */
    RangeFindField rangeFind() default @RangeFindField(UNSET = true);

    /**
     * Container field.
     */
    ContainerField container() default @ContainerField(UNSET = true);

    /**
     * Include screen in field.
     */
    IncludeScreenField includeScreen() default @IncludeScreenField(UNSET = true);

    /**
     * Include form in field.
     */
    IncludeFormField includeForm() default @IncludeFormField(UNSET = true);

    /**
     * Include menu in field.
     */
    IncludeMenuField includeMenu() default @IncludeMenuField(UNSET = true);

    /**
     * Include grid in field.
     */
    IncludeGridField includeGrid() default @IncludeGridField(UNSET = true);

    // AJAX updates

    /**
     * Field-level AJAX update areas.
     */
    OnFieldEventUpdateArea[] onFieldEventUpdateAreas() default {};
}
