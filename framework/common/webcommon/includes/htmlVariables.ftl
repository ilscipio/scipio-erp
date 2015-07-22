<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->


<#--
* 
* A set of global variables that define common classes.
* Automatically included at all times
*
-->

<#-- 
******************
* UTILITY VARIABLES *
******************
-->

<#-- Duplicates, only for reuse in styles hash itself -->
<#global "style_grid_cell"          = "columns" />
<#global "style_grid_small"         = "small-" />       <#-- Bootstrap: "col-sm-" -->
<#global "style_grid_medium"        = "medium-" />      <#-- Bootstrap: "col-md-" -->
<#global "style_grid_large"         = "large-" />       <#-- Bootstrap: "col-lg-" -->

<#global styles = {
<#-- Misc -->
    "disabled" : "disabled",
    "active" : "active",
    "button" : "button",
    "button_group" : "button-group",
    "button_force" : "force-button",
    "tiny" : "tiny",
    "small" : "small",
    "medium" : "medium",
    "large" : "large",
    "round" : "round",
    "radius" : "radius",
    "expand" : "expand",
    "collapse" : "collapse",
    "hidden" : "hide",
    "icon" : "fi",
    "icon_button" : "fi-widget",
    "icon_button_value" : "&#xf214;",
    "icon_calendar" : "fi-calendar",
    "icon_arrow" : "fi-arrow-right",

<#-- Colors -->
    "color_green" : "success",
    "color_red" : "alert",
    "color_grey" : "secondary",
    "color_secondary" : "secondary",
    "color_info" : "info",
    "color_warning" : "warning",
    "color_success" : "success",  
    "color_alert" : "alert",        <#-- Bootstrap: "danger" -->

<#-- Grid -->
    "grid_row" : "row",
    "grid_cell" : "${style_grid_cell}",
    "grid_display" : "display",      <#-- display grid -->
    "grid_end" : "end",
    "grid_small" : "${style_grid_small}",   
    "grid_medium" : "${style_grid_medium}",
    "grid_large" : "${style_grid_large}",       <#-- Bootstrap: "col-lg-" -->
    "grid_offset" : "offset-",      <#-- Bootstrap: "col-md-offset-" -->
    "grid_block" : "block-grid-",
    "grid_columns_1" : "${style_grid_large}1 ${style_grid_cell}",
    "grid_columns_2" : "${style_grid_large}2 ${style_grid_medium}3 ${style_grid_cell}",
    "grid_columns_3" : "${style_grid_large}3 ${style_grid_medium}4 ${style_grid_cell}",
    "grid_columns_4" : "${style_grid_large}4 ${style_grid_medium}5 ${style_grid_cell}",
    "grid_columns_5" : "${style_grid_large}5 ${style_grid_medium}6 ${style_grid_cell}",
    "grid_columns_6" : "${style_grid_large}6 ${style_grid_cell}",
    "grid_columns_7" : "${style_grid_large}7 ${style_grid_medium}6 ${style_grid_cell}",
    "grid_columns_8" : "${style_grid_large}8 ${style_grid_medium}7 ${style_grid_cell}",
    "grid_columns_9" : "${style_grid_large}9 ${style_grid_medium}8 ${style_grid_cell}",
    "grid_columns_10" : "${style_grid_large}10 ${style_grid_medium}9 ${style_grid_cell}",
    "grid_columns_11" : "${style_grid_large}11 ${style_grid_medium}10 ${style_grid_cell}",
    "grid_columns_12" : "${style_grid_large}12 ${style_grid_cell}",

<#-- Navigation -->
    "nav_subnav" : "sub-nav",

<#-- Lists -->
    "list_inline" : "inline-list",

<#-- Tile -->
    "tile_container" : "tile-container",
    "tile_wrap" : "tile",
    "tile_color" : "tile-color-",
    "tile_content" : "tile-content",
    "tile_icon" : "tile-icon",
    "tile_title" : "tile-title",
    "tile_overlay" : "tile-overlay",
    "tile_image" : "tile-image",

<#-- Breadcrumbs -->
    "nav_breadcrumbs" : "breadcrumbs",
    "nav_breadcrumbs_disabled" : "unavailable",
    "nav_breadcrumbs_active" : "current",

<#-- Panels -->
    "panel_wrap" : "panel",
    "panel_head" : "",
    "panel_title" : "",
    "panel_body" : "",

<#-- Alert Box -->
    "alert_wrap" : "alert-box",    <#-- Bootstrap: "alert" -->
    "alert_prefix_type" : "",             <#-- Bootstrap: "alert-" -->

<#-- Pricing Tables -->
    "pricing_wrap" : "pricing-table",
    "pricing_price" : "price",
    "pricing_description" : "description",
    "pricing_title" : "title",
    "pricing_cta" : "cta-button",
    "pricing_bullet" : "bullet-item",

<#-- Modal -->
    "modal_wrap" : "reveal-modal",

<#-- Chart -->
    "chart_legend" : "chart-legend",

<#-- Progress Bar -->
    "progress_container" : "progress",   <#-- Bootstrap: "progress" -->
    "progress_wrap" : "",           <#-- Bootstrap: "progress-bar" -->
    "progress_bar" : "meter"       <#-- Bootstrap: "sr-only" -->

<#-- UTLITY VARIABLES END -->
}>

<#-- compatibility mode: define styles hash entries as individual style_ vars -->
<#list styles?keys as name>
  <@'<#global "style_${name}" = "${styles[name]}">'?interpret />
</#list>


