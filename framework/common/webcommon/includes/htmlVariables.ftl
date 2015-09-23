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
<#compress>

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

<#global styles = {
<#-- Misc -->
    "disabled" : "disabled",
    "active" : "active",
    "button" : "button",
    "button_group" : "button-group",
    "button_force" : "force-button",
    "button_default" : "button tiny",
    "tiny" : "tiny",
    "small" : "small",
    "medium" : "medium",
    "large" : "large",
    "round" : "round",
    "radius" : "radius",
    "expand" : "expand",
    "collapse" : "collapse",
    "collapsed" : "collapsed",
    "hidden" : "hide",
    "icon" : "fi",
    "icon_button" : "fi-widget",
    "icon_button_value" : "&#xf214;",
    "icon_calendar" : "fi-calendar",
    "icon_arrow" : "fi-arrow-right",
    
<#-- Menus -->  
    "menu_default" : "",  
    "menu_default_item" : "", 
    "menu_default_itemlink" : "", 
    "menu_main" : "dropdown",  
    "menu_main_item" : "",
    "menu_main_itemlink" : "", 
    "menu_sidebar" : "side-nav", 
    "menu_sidebar_item" : "",
    "menu_sidebar_itemlink" : "", 
    "menu_tab" : "button-group force-button", 
    "menu_tab_item" : "", 
    "menu_tab_itemlink" : "button tiny", 
    "menu_subtab" : "button-group force-button",
    "menu_subtab_item" : "",
    "menu_subtab_itemlink" : "button tiny", 
    "menu_button" : "button-group force-button",
    "menu_button_item" : "",
    "menu_button_itemlink" : "button tiny",
    "menu_section" : "button-group",
    "menu_section_item" : "",
    "menu_section_itemlink" : "button tiny", 
    
<#-- Data and Tables -->  
    "table_default" : "",
    "table_default_cellspacing" : "",
    "table_data_list" : "basic-table",
    "table_data_list_cellspacing" : 0,
    "table_data_complex" : "basic-table",
    "table_data_complex_cellspacing" : 0,
    "table_summary" : "basic-table",
    "table_summary_cellspacing" : 0,
    "table_fields" : "",
    "table_fields_cellspacing" : 0,
    "row_reg" : "even",
    "row_alt" : "odd",
    "row_selected" : "selected",
    
    "link_action" : "button tiny", 
    "link_id" : "button tiny", 
    "link_name" : "button tiny", 
    "link_desc" : "",
    "link_text" : "",
    "link_image" : "",
    
<#-- Colors -->
    "color_green" : "success",
    "color_red" : "alert",
    "color_grey" : "secondary",
    "color_secondary" : "secondary",
    "color_info" : "info",
    "color_warning" : "warning",
    "color_success" : "success",  
    "color_alert" : "alert",        <#-- Bootstrap: "danger" -->
    "color_error" : "alert",
    
<#-- Grid -->
    "grid_row" : "row",
    "grid_cell" : "columns",
    "grid_display" : "display",      <#-- display grid -->
    "grid_end" : "end",
    "grid_small" : "small-",   
    "grid_medium" : "medium-",
    "grid_large" : "large-",       <#-- Bootstrap: "col-lg-" -->
    "grid_offset" : "offset-",      <#-- Bootstrap: "col-md-offset-" -->
    "grid_small_offset" : "small-offset-",     
    "grid_medium_offset" : "medium-offset-",    
    "grid_large_offset" : "large-offset-",   
    "grid_block" : "block-grid-",
    "grid_columns_1" : "large-1 columns",
    "grid_columns_2" : "large-2 medium-3 columns",
    "grid_columns_3" : "large-3 medium-4 columns",
    "grid_columns_4" : "large-4 medium-5 columns",
    "grid_columns_5" : "large-5 medium-6 columns",
    "grid_columns_6" : "large-6 columns",
    "grid_columns_7" : "large-7 medium-6 columns",
    "grid_columns_8" : "large-8 medium-7 columns",
    "grid_columns_9" : "large-9 medium-8 columns",
    "grid_columns_10" : "large-10 medium-9 columns",
    "grid_columns_11" : "large-11 medium-10 columns",
    "grid_columns_12" : "large-12 columns",

<#-- Floats -->
    "float_left" : "left",
    "float_right" : "right",
    "float_clearfix" : "clearfix",
    
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

<#-- compatibility mode: define styles hash entries as individual style_ vars
<#list styles?keys as name>
  <@'<#global "style_${name}" = "${styles[name]}">'?interpret />
</#list>
 -->

</#compress>