/**
 * Master HTML templating variables include, Bootstrap theme/framework.
 *
 * Overrides the default SCIPIO styles located in
 * htmlVariables.groovy - ofbiz_foundation/framework/common/webcommon/includes/scipio/lib/standard/
 *
 */

/*
*************************************
* MAIN STYLES FOR IGNITE (COREUI - Bootstrap) *
*************************************
*/

 /* EXTENDS/MODIFIES the default foundation styles. Adopted for Foundation 6*/
// GroovyUtil.runScriptAtLocation("component://common/webcommon/includes/scipio/lib/standard/htmlVariables.groovy", null, context);
  
context.styles = [
    
  /* Generic */
    "framework" : "bootstrap",
    "customSideBar" : false,
    "disabled" : "disabled",
    "active" : "active",
    "selected" : "selected",
    "button" : "btn btn-raised",
    "button_prefix" : "btn-",
    "button_group" : "btn-group",
    "button_force" : "force-button",
    "button_default" : "btn btn-raised btn-sm btn-default",
    "tiny" : "btn-xs",
    "small" : "btn-sm",
    "medium" : "btn-md",
    "large" : "btn-lg",
    "round" : "round",
    "radius" : "radius",
    "expand" : "expand",
    "expanded" : "expanded",
    "collapse" : "",
    "collapsed" : "",
    "required" : "required",
    "prefix" : "input-group-addon",
    "postfix" : "input-group-addon",
    "hidden" : "hide",
    "icon" : "fa",
    "icon_prefix" : "fa fa-",
    "icon_button" : "fa-widget",
    "icon_button_value" : "&#xf013;",
    "icon_calendar" : "fa-calendar",
    "icon_shopping_cart" : "fa-shopping-cart",
    "icon_arrow" : "fa-arrow-right",
    "icon_list" : "fa-list",
    "icon_user" : "fa-user",
    "icon_password" : "fa-lock",
    "icon_tenant" : "fa-cloud",
    "icon_error" : "fa-warning",
    "icon_edit" : "fa-pencil-square-o",
    "icon_check" : "fa-check",
    "heading_level_prefix" : "heading-level-",
    "switch" : "switch",
    "dropdown" : "dropdown-menu",
    "fullwidth" : "elem-fullwidth",
    "large_container_factor" : 6,
    
  /* Basic tooltip */
    "tooltip" : "has-tip",
    "tooltip_left" : "has-tip tip-left",
    "tooltip_top" : "has-tip tip-top",
    "tooltip_right" : "has-tip tip-right",
    "tooltip_bottom" : "has-tip tip-bottom",
    "tooltip_delim" : " - ",
    
  /* Common messages (default message type container styles) */
    "commonmsg_result" : "result-msg",
    "commonmsg_result_norecord" : "result-msg result-no-record-msg",
    "commonmsg_result_norecord_prop" : "CommonNoRecordFoundSentence",
    "commonmsg_info" : "info-msg",
    "commonmsg_info_important" : "info-msg info-important",
    "commonmsg_warning" : "warning-msg",
    "commonmsg_fail" : "fail-msg",
    "commonmsg_error" : "error-msg",
    "commonmsg_error_perm" : "error-msg error-perm",
    "commonmsg_error_security" : "error-msg error-security",
    "commonmsg_generic" : "generic-msg",
    "commonmsg_default" : "generic-msg",
    
  /* Menus */
    /* Generic menu types */
    "menu_generic" : "menu-type-generic",
    "menu_generic_altnested" : "menu-type-generic",
    "menu_generic_type" : "menu-type-generic",
    "menu_generic_active" : "menu-active",
    "menu_generic_activetarget" : "menu-active-target",
    "menu_generic_activeancestor" : "menu-active-ancestor",
    "menu_generic_levelprefix" : "menu-level-",
    "menu_generic_item" : "",
    "menu_generic_itemdisabled" : "disabled",
    "menu_generic_itemactive" : "active",
    "menu_generic_itemactivetarget" : "active-target",
    "menu_generic_itemactiveancestor" : "active-ancestor",
    "menu_generic_itemselected" : "selected",
    "menu_generic_item_contentdisabled" : "disabled",
    "menu_generic_item_contentactive" : "active",
    "menu_generic_item_contentactivetarget" : "active-target",
    "menu_generic_item_contentactiveancestor" : "active-ancestor",
    "menu_generic_item_contentselected" : "selected",
    "menu_generic_item_link" : "",
    "menu_generic_item_text" : "text-entry",
    "menu_generic_item_submit" : "",
    "menu_generic_item_generic" : "",
    "menu_button" : "menu-type-button",
    "menu_button_altnested" : "menu-type-button",
    "menu_button_type" : "menu-type-button",
    "menu_button_htmlwrap" : "div",
    "menu_button_item" : "",
    "menu_button_item_htmlwrap" : false,
    "menu_button_itemdisabled" : "disabled",
    "menu_button_itemactive" : "active",
    "menu_button_itemactivetarget" : "active-target",
    "menu_button_itemactiveancestor" : "active-ancestor",
    "menu_button_itemselected" : "selected",
    "menu_button_item_contentdisabled" : "disabled",
    "menu_button_item_contentactive" : "active",
    "menu_button_item_contentactivetarget" : "active-target",
    "menu_button_item_contentactiveancestor" : "active-ancestor",
    "menu_button_item_contentselected" : "selected",
    "menu_button_item_link" : "btn btn-raised btn-sm btn-default",
    "menu_button_item_text" : "text-entry",
    "menu_button_item_submit" : "btn btn-raised btn-sm btn-default",
    "menu_button_dropdown" : "dropdown-menu menu-type-button-dropdown",
    "menu_button_dropdown_htmlwrap" : "div",
    "menu_button_dropdown_altnested" : "menu-type-button-dropdown",
    "menu_button_dropdown_type" : "menu-type-button-dropdown",
    "menu_button_dropdown_specialtype" : "button-dropdown",
    "menu_button_dropdown_title" : "btn btn-raised btn-sm dropdown-toggle",
    "menu_button_dropdown_item" : "list-item",
    "menu_button_dropdown_item_htmlwrap" : false,
    "menu_button_dropdown_itemdisabled" : "disabled",
    "menu_button_dropdown_itemactive" : "active",
    "menu_button_dropdown_itemactivetarget" : "active-target",
    "menu_button_dropdown_itemactiveancestor" : "active-ancestor",
    "menu_button_dropdown_itemselected" : "selected",
    "menu_button_dropdown_item_contentdisabled" : "disabled",
    "menu_button_dropdown_item_contentactive" : "active",
    "menu_button_dropdown_item_contentactivetarget" : "active-target",
    "menu_button_dropdown_item_contentactiveancestor" : "active-ancestor",
    "menu_button_dropdown_item_contentselected" : "selected",
    "menu_button_dropdown_item_link" : "dropdown-item",
    "menu_button_dropdown_item_text" : "text-entry",
    "menu_button_dropdown_item_submit" : "",
    /* Main navigation menus */
    "menu_main" : "dropdown-menu menu-type-main",
    "menu_main_altnested" : "menu-type-main",
    "menu_main_type" : "menu-type-main",
    "menu_main_specialtype" : "main",
    "menu_main_wrap" : "nav-dropdown",
    "menu_main_item" : "nav-item",
    "menu_main_itemdisabled" : "disabled",
    "menu_main_itemactive" : "open",
    "menu_main_itemactivetarget" : "active-target",
    "menu_main_itemactiveancestor" : "active-ancestor",
    "menu_main_itemselected" : "selected",
    "menu_main_item_contentdisabled" : "disabled",
    "menu_main_item_contentactive" : "active",
    "menu_main_item_contentactivetarget" : "active-target",
    "menu_main_item_contentactiveancestor" : "active-ancestor",
    "menu_main_item_contentselected" : "selected",
    "menu_main_item_link" : "nav-link",
    "menu_main_item_text" : "text-entry",
    "menu_main_item_submit" : "",
    "menu_sidebar" : "nav menu-type-sidebar",
    "menu_sidebar_altnested" : "menu-type-sidebar nav-dropdown-items",
    "menu_sidebar_type" : "menu-type-sidebar",
    "menu_sidebar_specialtype" : "sidebar",
    "menu_sidebar_wrap" : "collapse navbar-collapse navbar-ex1-collapse",
    "menu_sidebar_active" : "active",
    "menu_sidebar_activeancestor" : "nav-dropdown-items",
    "menu_sidebar_item" : "nav-item nav-dropdown",
    "menu_sidebar_itemdisabled" : "disabled",
    "menu_sidebar_itemactive" : "open",
    "menu_sidebar_itemactivetarget" : "active-target active",
    "menu_sidebar_itemactiveancestor" : "nav-dropdown active-ancestor active",
    "menu_sidebar_itemselected" : "selected",
    "menu_sidebar_itemdashboard" : "dashboard",
    "menu_sidebar_item_contentdisabled" : "disabled",
    "menu_sidebar_item_contentactive" : "active",
    "menu_sidebar_item_contentactivetarget" : "active-target active",
    "menu_sidebar_item_contentactiveancestor" : "active-ancestor nav-dropdown",
    "menu_sidebar_item_contentselected" : "selected",
    "menu_sidebar_item_link" : "nav-link",
    "menu_sidebar_item_text" : "nav-title",
    "menu_sidebar_item_submit" : "",
    /* Tab and secondary navigation menus*/
    "menu_tab" : "nav nav-tabs menu-type-tab",
    "menu_tab_altnested" : "menu-type-tab",
    "menu_tab_type" : "menu-type-tab",
    "menu_tab_htmlwrap" : "ul",
    "menu_tab_item" : "nav-item",
    "menu_tab_item_htmlwrap" : false,
    "menu_tab_itemdisabled" : "disabled",
    "menu_tab_itemactive" : "active",
    "menu_tab_itemactivetarget" : "active-target",
    "menu_tab_itemactiveancestor" : "active-ancestor",
    "menu_tab_itemselected" : "selected",
    "menu_tab_item_contentdisabled" : "disabled",
    "menu_tab_item_contentactive" : "active",
    "menu_tab_item_contentactivetarget" : "active-target",
    "menu_tab_item_contentactiveancestor" : "active-ancestor",
    "menu_tab_item_contentselected" : "selected",
    "menu_tab_item_link" : "btn btn-sm btn-default",
    "menu_tab_item_text" : "text-entry",
    "menu_tab_item_submit" : "",
    "menu_subtab" : "nav nav-tabs menu-type-subtab",
    "menu_subtab_altnested" : "menu-type-subtab",
    "menu_subtab_type" : "menu-type-subtab",
    "menu_subtab_htmlwrap" : "div",
    "menu_subtab_item" : "",
    "menu_subtab_item_htmlwrap" : false,
    "menu_subtab_itemdisabled" : "disabled",
    "menu_subtab_itemactive" : "active",
    "menu_subtab_itemactivetarget" : "active-target",
    "menu_subtab_itemactiveancestor" : "active-ancestor",
    "menu_subtab_itemselected" : "selected",
    "menu_subtab_item_contentdisabled" : "disabled",
    "menu_subtab_item_contentactive" : "active",
    "menu_subtab_item_contentactivetarget" : "active-target",
    "menu_subtab_item_contentactiveancestor" : "active-ancestor",
    "menu_subtab_item_contentselected" : "selected",
    "menu_subtab_item_link" : "btn btn-raised btn-sm btn-default",
    "menu_subtab_item_text" : "text-entry",
    "menu_subtab_item_submit" : "",
    /* Default section menu */
    "menu_section" : "menu-type-section",
    "menu_section_altnested" : "menu-type-section",
    "menu_section_type" : "menu-type-section",
    "menu_section_htmlwrap" : "div",
    "menu_section_item" : "",
    "menu_section_item_htmlwrap" : false,
    "menu_section_itemdisabled" : "disabled",
    "menu_section_itemactive" : "active",
    "menu_section_itemactivetarget" : "active-target",
    "menu_section_itemactiveancestor" : "active-ancestor",
    "menu_section_itemselected" : "selected",
    "menu_section_item_contentdisabled" : "disabled",
    "menu_section_item_contentactive" : "active",
    "menu_section_item_contentactivetarget" : "active-target",
    "menu_section_item_contentactiveancestor" : "active-ancestor",
    "menu_section_item_contentselected" : "selected",
    "menu_section_item_link" : "btn btn-raised btn-sm btn-default",
    "menu_section_item_text" : "text-entry",
    "menu_section_item_submit" : "btn btn-raised btn-sm btn-default",
    /* Default section menu inlined with title
        FIXME: looks too big/clumsy with these buttons (floats right of title) */
    "menu_section_inline" : "menu-type-section-inline",
    "menu_section_inline_altnested" : "menu-type-section-inline",
    "menu_section_inline_type" : "menu-type-section-inline",
    "menu_section_inline_htmlwrap" : "div",
    "menu_section_inline_item" : "",
    "menu_section_inline_item_htmlwrap" : false,
    "menu_section_inline_itemdisabled" : "disabled",
    "menu_section_inline_itemactive" : "active",
    "menu_section_inline_itemactivetarget" : "active-target",
    "menu_section_inline_itemactiveancestor" : "active-ancestor",
    "menu_section_inline_itemselected" : "selected",
    "menu_section_inline_item_contentdisabled" : "disabled",
    "menu_section_inline_item_contentactive" : "active",
    "menu_section_inline_item_contentactivetarget" : "active-target",
    "menu_section_inline_item_contentactiveancestor" : "active-ancestor",
    "menu_section_inline_item_contentselected" : "selected",
    "menu_section_inline_item_link" : "btn btn-raised btn-sm btn-default",
    "menu_section_inline_item_text" : "text-entry",
    "menu_section_inline_item_submit" : "btn btn-raised btn-sm btn-default",
    /* default entry is used for any encountered menu types that have no specific entries in this hash */
    "menu_default" : "menu-type-default",
    "menu_default_altnested" : "menu-type-default",
    "menu_default_type" : "menu-type-default",
    "menu_default_toplevel" : "toplevel",
    "menu_default_nested" : "nested",
    "menu_default_nestedsame" : "nested-sametype",
    "menu_default_htmlwrap" : true,
    "menu_default_active" : "menu-active",
    "menu_default_activetarget" : "menu-active-target",
    "menu_default_activeancestor" : "menu-active-ancestor",
    "menu_default_levelprefix" : "menu-level-",
    "menu_default_item" : "",
    "menu_default_item_htmlwrap" : true,
    "menu_default_itemdisabled" : "disabled",
    "menu_default_itemactive" : "active",   // NOTE: this is for compatibility with old code that does not distinguish between active target and active ancestor
    "menu_default_itemactivetarget" : "active-target", // NOTE: in default fallback behavior, active/activetarget/ancestor are treated as one entry, all together
    "menu_default_itemactiveancestor" : "active-ancestor",
    "menu_default_itemselected" : "selected",
    "menu_default_item_contentdisabled" : "disabled",
    "menu_default_item_contentactive" : "active",
    "menu_default_item_contentactive" : "active",
    "menu_default_item_contentactivetarget" : "active-target",
    "menu_default_item_contentactiveancestor" : "active-ancestor",
    "menu_default_item_contentselected" : "selected",
    "menu_default_item_link" : "",
    "menu_default_item_text" : "text-entry",
    "menu_default_item_submit" : "",
    "menu_default_item_generic" : "",
    
    /* Misc menu styles */
    "menu_subappitem" : "subappitem",
    "menu_subappitem_link" : "subapplink",
    "menu_buttonstyle_alt1" : "button-style-1",
    "menu_buttonstyle_alt2" : "button-style-2",
    "menu_noclear" : "menu-no-clear",
    "menu_link_href_default" : "javascript:void(0);",
    
  /* Tree menu */
    
    /* Lib-specific styles */
    "treemenu_lib_jstree_defnodeicon" : "jstree-file",
    "treemenu_lib_jstree_defdiricon" : "jstree-folder",
    
    /* General and default styles */
    "treemenu_deflib" : "jstree",
    /* These can be used more easily from templates.
     * NOTE: if deflib is changed, all these must be changed too. These imply default library.
     */
    "treemenu_icon_node" : "jstree-file",        // default node icon (including leaf)
    "treemenu_icon_dirnode" : "jstree-folder",   // default dir node icon
    "treemenu_icon_file" : "jstree-file",
    "treemenu_icon_folder" : "jstree-folder",
    
  /* Sections */
    /* type-based styles */
    "section_generic_menulayouttitle" : "post-title",
    "section_generic_menulayoutgeneral" : "top",
    "section_generic_inline_title_menutitlecontainerclass" : "clearfix",
    "section_generic_inline_title_titlecontainerclass" : "left",
    "section_generic_inline_title_menucontainerclass" : "right",
    
    "section_default_menulayouttitle" : "post-title",
    "section_default_menulayoutgeneral" : "top",
    "section_default_inline_title_menutitlecontainerclass" : "clearfix",
    "section_default_inline_title_titlecontainerclass" : "left",
    "section_default_inline_title_menucontainerclass" : "right",
    
  /* Data and Tables */
    /* table type-based styles */
    "table_generic" : "table table-bordered table-condensed table-type-generic",
    "table_generic_type" : "table table-bordered table-striped table-condensed table-type-generic",
    "table_generic_cellspacing" : "",
    "table_generic_rowtype" : "generic",    /* the default row type is normally "content", but generic tables need "generic" */
    "table_data_list" : "table table-striped table-type-data table-type-data-list",
    "table_data_list_type" : "table-type-data table-type-data-list",
    "table_data_list_cellspacing" : 0,
    "table_data_list_responsive" : true,
    /* "table_data_list_autoaltrows" : true, */
    "table_data_list_multiform" : "table table-bordered table-striped table-condensed table-type-data table-type-data-list-multiform",
    "table_data_list_multiform_type" : "table-type-data table-type-data-list-multiform",
    "table_data_list_multiform_cellspacing" : 0,
    "table_data_list_multiform_responsive" : false,
    "table_data_complex" : "table table-type-data table-type-data-complex",
    "table_data_complex_type" : "table-type-data table-type-data-complex",
    "table_data_complex_cellspacing" : 0,
    "table_summary" : "table table-type-summary",
    "table_summary_type" : "table-type-summary",
    "table_summary_cellspacing" : 0,
    "table_fields" : "table table-condensed table-type-fields",
    "table_fields_type" : "table-type-fields",
    "table_fields_cellspacing" : 0,
    "table_fields_vert" : "basic-table table-type-fields-vert",
    "table_fields_vert_type" : "table-type-fields-vert",
    "table_fields_vert_cellspacing" : 0,
    /* default entry is used by @table for any encountered table types that have no specific entries in this hash.
         in other words, it is the default style for table types that this theme does not recognize, or "all others". */
    "table_default" : "table table-type-default",
    "table_default_type" : "table-type-default",
    "table_default_cellspacing" : "",
    "table_default_autoaltrows" : false,
    "table_default_rowtype" : "content",
    "table_default_responsive" : "",  /* boolean with support for empty value (ternary) */
    "table_default_responsive_wrap" : "table-responsive", /* responsive table wrapping div element (TODO?) */
    "table_default_scrollable" : "", /* boolean with support for empty value (ternary) */
    "table_default_responsive_options" : [
        "fixedHeader" : true,
        "scrollX" : true,
        "info" : false,
        "paging" : false,
        "searching" : false,
        "ordering" : true,
        "order": []
    ],
    "table_default_scrollable_options" : [
        "fixedHeader" : false,
        "scrollX" : true,
        "info" : false,
        "paging" : false,
        "searching" : false,
        "ordering" : false
    ],
    
    "table_formwidget" : "dark-grid",                   /* default class for form widget tables (does NOT include _type because _type always added) */
    "table_formwidget_type" : "table-type-formwidget",  /* specific identifier for form widget tables. NOTE: this is ALWAYS added so doesn't need to be part of table_formwidget */
    
    /* other table-related styles */
    "table_basic" : "table table-condensed",                            /* This is not a table type, but simply the basic-table style */

    "table_spacing_tiny_hint" : "table-spacing-tiny-hint",              /* Extra style to indicate a table that might look better with a little cellspacing or more spacing in general, but may be ignored depending on theme. */
    "table_spacing_small_hint" : "table-spacing-small-hint",            /* Hint for small spacing (2-4px?) */
    "table_spacing_medium_hint" : "table-spacing-medium-hint",          /* Hint for medium spacing (6-10px?) */
    "table_spacing_large_hint" : "table-spacing-large-hint",
    "table_spacing_tiny" : "table-spacing-tiny",                        /* Extra style to indicate a table that normally needs a bit of spacing such as cellspacing. If the spacing is more a secondary concern, use table_spacing_tiny_hint instead. */
    "table_spacing_small" : "table-spacing-small",
    "table_spacing_medium" : "table-spacing-medium",
    "table_spacing_large" : "table-spacing-large",
    
    "table_nocellspacing_hint" : "table-nocellspacing-hint",            /* Hint that table will look better without cellspacing. */
    "table_nocellspacing" : "table-nocellspacing",                      /* Request for table to always have zero cellspacing (collapse). Meant to be used for rare things like calendars. This combines logically with table_spacing_xxx and usually will mean intra-cell padding should be used instead of inter-cell spacing. */

    "table_border_on_hint" : "table-border-on-hint",
    "table_border_on" : "table-border-on",
    "table_noborder_hint" : "table-noborder-hint",
    "table_noborder" : "table-noborder",
    
    "row_reg" : "even",
    "row_alt" : "odd",
    "row_selected" : "selected",
    
  /* Generic action styles */
    "action_prefix" : "action-",

    /* Generic UI functionality action types */
    "action_nav" : "action-nav",
    "action_run" : "action-run",
    
    /* Action state scope styles */
    "action_scope_sys" : "action-scope-sys",
    "action_scope_session" : "action-scope-session",
    "action_scope_local" : "action-scope-local",
    
    /* Convenience combinations of generic functionality + scope, useful on menu items, image links, and any non-text-link elements that identify or perform actions */
    "action_nav_sys" : "action-nav action-scope-sys",
    "action_nav_session" : "action-nav action-scope-session",
    "action_nav_local" : "action-nav action-scope-local",
    "action_run_sys" : "action-run action-scope-sys",
    "action_run_session" : "action-run action-scope-session",
    "action_run_local" : "action-run action-scope-local",
    
    /* Specific action types */
    "action_generic" : "action-generic",
    "action_cancel" : "action-cancel",
    
    /* Specific state-changing actions */
    "action_modify" : "action-modify",
    "action_add" : "action-modify action-add btn-success",
    "action_update" : "action-modify action-update btn-default",
    "action_remove" : "action-modify action-remove btn-danger",
    "action_clear" : "action-modify action-clear btn-danger",
    "action_move" : "action-modify action-move",
    "action_copy" : "action-modify action-copy",
    "action_configure" : "action-modify action-configure",
    "action_begin" : "action-modify action-begin",
    "action_reset" : "action-modify action-reset",
    "action_hold" : "action-modify action-hold btn-warning",
    "action_terminate" : "action-modify action-terminate btn-danger",
    "action_complete" : "action-modify action-complete",
    "action_updatestatus" : "action-modify action-updatestatus btn-warning",
    "action_import" : "action-modify action-import",
    "action_transfer" : "action-modify action-transfer",
    "action_send" : "action-modify action-send",
    "action_receive" : "action-modify action-receive",
    "action_sync" : "action-modify action-sync",
    "action_register" : "action-modify action-login",
    "action_login" : "action-modify action-login",
    "action_logout" : "action-modify action-logout",
    "action_continue" : "action-modify action-continue",
    
    /* Specific read-only actions (may or retrieve or submit data for analysis without changing state meaningfully) */
    "action_readonly" : "action-readonly",
    "action_read" : "action-readonly action-read",
    "action_find" : "action-readonly action-read action-find",
    "action_view" : "action-readonly action-read action-view",
    "action_export" : "action-readonly action-read action-export",
    
    "action_visibility" : "action-readonly action-visibility",
    "action_show" : "action-readonly action-visibility action-show",
    "action_hide" : "action-readonly action-visibility action-hide",
    "action_close" : "action-readonly action-close",
    "action_select" : "action-readonly action-select",
    "action_reload" : "action-readonly action-reload",
    "action_verify" : "action-readonly action-verify",

    /* Misc action flags (fragments) */
    "action_external" : "action-external",

    "action_primary" : "action-primary",
    "action_primary_inline" : "action-primary-inline action-inline",
    "action_secondary" : "action-secondary",
    "action_secondary_inline" : "action-secondary-inline action-inline",
    
    "action_importance_low" : "action-importance-low",
    "action_importance_normal" : "action-importance-normal",
    "action_importance_high" : "action-importance-high",
    
    "action_inline" : "action-inline",
    
    
    /* Convenience shorthands */
    "action_nav_cancel" : "action-nav action-cancel btn-danger",
    "action_nav_sys_cancel" : "action-nav action-scope-sys action-cancel btn-danger",
    "action_nav_session_cancel" : "action-nav action-scope-session action-cancel btn-danger",
    "action_nav_local_cancel" : "action-nav action-scope-local action-cancel btn-danger",
    "action_run_sys_cancel" : "action-run action-scope-sys action-cancel btn-danger",
    "action_run_session_cancel" : "action-run action-scope-session action-cancel btn-danger",
    "action_run_local_cancel" : "action-run action-scope-local action-cancel btn-danger",

  /* Link styles (fragments) */
    /* misc/short link styles */
    "link_long" : "link-long",                                  /* style to identify long links */
    
    /* classes to identify content type of links */
    "link_type_text" : "link-type-text",
    "link_type_image" : "link-type-image",

  /* Link styles (standalone/full) */
    /* Action links (trigger an actual action in the system) - NOT for use for opening pages toward actions! */
    "link_run_sys" :                "link-type-text action-run action-scope-sys action-primary btn btn-raised btn-sm btn-success",
    "link_run_sys_long" :           "link-type-text action-run action-scope-sys action-primary link-long",
    "link_run_sys_inline" :         "link-type-text action-run action-scope-sys action-primary-inline action-inline",
    "link_run_sys_inline_long" :    "link-type-text action-run action-scope-sys action-primary-inline action-inline link-long",
    "link_run_sys_cancel" :         "link-type-text action-run action-scope-sys action-primary action-cancel btn btn-raised btn-sm btn-danger",
    "link_run_sys_cancel_long" :    "link-type-text action-run action-scope-sys action-primary action-cancel link-long",
    "link_run_sys_info" :           "link-type-text action-run action-scope-sys action-secondary btn btn-raised btn-sm btn-info",
    "link_run_sys_info_long" :      "link-type-text action-run action-scope-sys action-secondary link-long",
    "link_run_session" :            "link-type-text action-run action-scope-session action-primary btn btn-raised btn-sm btn-success",
    "link_run_session_long" :       "link-type-text action-run action-scope-session action-primary link-long",
    "link_run_session_inline" :     "link-type-text action-run action-scope-session action-primary-inline action-inline",                              /* informational session run-action (gives information first, triggers action as secondary purpose) */
    "link_run_session_inline_long" :"link-type-text action-run action-scope-session action-primary-inline action-inline link-long",
    "link_run_session_cancel" :     "link-type-text action-run action-scope-session action-primary action-cancel btn btn-raised btn-sm btn-danger",
    "link_run_session_cancel_long" :"link-type-text action-run action-scope-session action-primary action-cancel link-long",
    "link_run_session_info" :       "link-type-text action-run action-scope-session action-secondary btn btn-raised btn-sm btn-info",
    "link_run_session_info_long" :  "link-type-text action-run action-scope-session action-secondary link-long",
    "link_run_local" :              "link-type-text action-run action-scope-local action-primary btn btn-raised btn-sm btn-success",
    "link_run_local_long" :         "link-type-text action-run action-scope-local action-primary link-long",
    "link_run_local_inline" :       "link-type-text action-run action-scope-local action-primary-inline action-inline",                                /* informational local run-action (gives information first, triggers action as secondary purpose) */
    "link_run_local_inline_long" :  "link-type-text action-run action-scope-local action-primary-inline action-inline link-long",
    "link_run_local_cancel" :       "link-type-text action-run action-scope-local action-primary action-cancel btn btn-raised btn-sm btn-danger",
    "link_run_local_cancel_long" :  "link-type-text action-run action-scope-local action-primary action-cancel link-long",
    "link_run_local_info" :         "link-type-text action-run action-scope-local action-secondary",
    "link_run_local_info_long" :    "link-type-text action-run action-scope-local action-secondary link-long",
    
    /* Primary navigation text links (basic navigation and navigation toward actions) */
    "link_nav" :                    "link-type-text action-nav action-primary btn btn-raised btn-sm btn-primary",
    "link_nav_long" :               "link-type-text action-nav action-primary link-long",
    "link_nav_inline" :             "link-type-text action-nav action-primary-inline action-inline",
    "link_nav_inline_long" :        "link-type-text action-nav action-primary-inline action-inline link-long",
    "link_nav_cancel" :             "link-type-text action-nav action-primary action-cancel btn btn-raised btn-sm btn-danger",
    "link_nav_cancel_long" :        "link-type-text action-nav action-primary action-cancel link-long",
    "link_nav_info" :               "link-type-text action-nav action-secondary",
    "link_nav_info_long" :          "link-type-text action-nav action-secondary link-long",
    "link_nav_info_id" :            "link-type-text action-nav action-secondary",
    "link_nav_info_id_long" :       "link-type-text action-nav action-secondary link-long",
    "link_nav_info_name" :          "link-type-text action-nav action-secondary",
    "link_nav_info_name_long" :     "link-type-text action-nav action-secondary link-long",
    "link_nav_info_idname" :        "link-type-text action-nav action-secondary",
    "link_nav_info_idname_long" :   "link-type-text action-nav action-secondary link-long",
    "link_nav_info_desc" :          "link-type-text action-nav action-secondary",
    "link_nav_info_date" :          "link-type-text action-nav action-secondary",
    "link_nav_info_number" :        "link-type-text action-nav action-secondary",
    "link_nav_info_value" :         "link-type-text action-nav action-secondary",
    "link_nav_info_value_long" :    "link-type-text action-nav action-secondary link-long",
    "link_nav_info_uri" :           "link-type-text action-nav action-secondary",
    "link_nav_info_text" :          "link-type-text action-nav action-secondary",
    
  /* Colors */
    "color_green" : "success",
    "color_red" : "danger",
    "color_grey" : "secondary",
    "color_primary" : "primary",
    "color_secondary" : "secondary",
    "color_info" : "info",
    "color_warning" : "warning",
    "color_success" : "success",
    "color_alert" : "warning",
    "color_error" : "danger",
    
    "button_color_default" : "btn-default",
    "button_color_green" : "btn-success",
    "button_color_red" : "btn-danger",
    "button_color_grey" : "btn-secondary",
    "button_color_primary" : "btn-primary",
    "button_color_secondary" : "btn-secondary",
    "button_color_success" : "btn-success",
    "button_color_info" : "btn-info",
    "button_color_warning" : "btn-warning",
    "button_color_alert" : "btn-danger",
    "button_color_error" : "btn-danger",
    
    "text_color_primary" : "colored-text text-primary",
    "text_color_secondary" : "colored-text text-success",
    "text_color_info" : "colored-text text-info",
    "text_color_warning" : "colored-text text-warning",
    "text_color_success" : "colored-text text-success",
    "text_color_alert" : "colored-text text-warning",
    "text_color_error" : "colored-text text-danger",
    
    "elem_color_primary" : "colored-elem primary",
    "elem_color_secondary" : "colored-elem secondary",
    "elem_color_info" : "colored-elem info",
    "elem_color_warning" : "colored-elem warning",
    "elem_color_success" : "colored-elem success",
    "elem_color_alert" : "colored-elem danger",
    "elem_color_error" : "colored-elem danger",
    
  /* Grid */
    "grid_section" : "",
    "grid_row" : "row",
    "grid_cell" : "columns",
    "grid_cell_default" : "col-lg-12",
    "grid_display" : "display",      /* display grid */
    "grid_theme_pre" : "", /*pre-content-section*/
    "grid_theme" : "row-offcanvas row-offcanvas-left container", /*content-section*/
    "grid_end" : "end",
    "grid_small" : "col-sm-",
    "grid_medium" : "col-md-",
    "grid_large" : "col-lg-",
    "grid_offset" : "offset-md-",
    "grid_small_offset" : "offset-sm-",
    "grid_medium_offset" : "offset-md-",
    "grid_large_offset" : "offset-lg-",
    "grid_block_container" : "",
    "grid_block_wrap" : "block-grid",
    "grid_block_prefix" : "",
    "grid_block_postfix" : "block-grid-",
    "grid_postfix_container" : "input-group",
    "grid_columns_1" : "col-lg-1",
    "grid_columns_2" : "col-lg-2 col-md-3",
    "grid_columns_3" : "col-lg-3 col-md-4",
    "grid_columns_4" : "col-lg-4 col-md-5",
    "grid_columns_5" : "col-lg-5 col-md-6",
    "grid_columns_6" : "col-lg-6",
    "grid_columns_7" : "col-lg-7 col-md-6",
    "grid_columns_8" : "col-lg-8 col-md-7",
    "grid_columns_9" : "col-lg-9 col-md-8",
    "grid_columns_10" : "col-lg-10 col-md-9",
    "grid_columns_11" : "col-lg-11 col-md-10",
    "grid_columns_12" : "col-lg-12",
    "grid_centered" : "text-centered",
    "grid_small_centered" : "text-centered",
    "grid_medium_centered" : "text-centered",
    "grid_large_centered" : "text-centered",
    "grid_sidebar_0_main" : "col-12 col-lg-12",
    "grid_sidebar_1_main" : "col-12 col-lg-10",
    "grid_sidebar_1_side" : "col-2 col-md-2 sidebar-offcanvas",
    "grid_sidebar_2_main" : "col-12 col-lg-10",
    "grid_sidebar_2_side" : "col-2 col-md-2 sidebar-offcanvas",

  /* Text align */
    "text_left" : "text-left",
    "text_right" : "text-right",
    "text_center" : "text-center",
    "text_justify" : "text-justify",

  /* Floats */
    "float_left" : "left",
    "float_right" : "right",
    "float_clearfix" : "clearfix",
    
  /* Navigation */
    "nav_subnav" : "sub-nav",
    "nav_sidenav" : "",

    /* Breadcrumbs */
    "nav_breadcrumbs" : "breadcrumb",
    "nav_breadcrumb" : "breadcrumb-item",
    "nav_breadcrumb_disabled" : "disabled",
    "nav_breadcrumb_active" : "active",
    "nav_breadcrumb_link" : "",

    /* Steps */
    "nav_steps" : "steps",                        /* steps container */
    "nav_step" : "",                              /* step entry */
    "nav_step_disabled" : "disabled",
    "nav_step_active" : "active",
    "nav_step_completed" : "completed",
    "nav_step_expanded" : "expanded",
    "nav_step_icon_completed" : "fa fa-check",
  

  /* Pagination */
    "pagination_wrap" : "text-center",
    "pagination_list" : "pagination",
    "pagination_control" : "pull-right",
    "pagination_item" : "",
    "pagination_item_first" : "",
    "pagination_item_last" : "",
    "pagination_item_previous" : "",
    "pagination_item_next" : "",
    "pagination_item_active" : "active",
    "pagination_item_disabled" : "disabled",
    "pagination_layout" : "bottom",
    "pagination_noresultsmode" : "hide",
    "pagination_showcount" : true,
    "pagination_alwaysshowcount" : true,
    "pagination_countmsglabel" : "CommonDisplayingShort",
    "pagination_lowcountmsglabel" : "CommonDisplayingShort",
    
  /* Lists */
    "list_inline" : "inline-list",

  /* Tile */
    /* tiles-type-based args and styles */
    "tile_default_containerclass" : "tile-container-default",
    "tile_default_class" : "tile-default tile-common",
    "tile_default_size" : "normal",
    "tile_default_color" : "0",
    "tile_default_titletype" : "default",
    "tile_default_titlebgcolor" : "default-title",
    "tile_default_overlaytype" : "default",   /* "default" will be mapped to "tile_overlay_default" style below */
    "tile_default_overlaybgcolor" : "0",
    "tile_default_imagetype" : "default",
    "tile_default_imagebgcolor" : "none",
    "tile_default_icon" : "",
    "tile_default_linktarget" : "",
    
    "tile_generic_containerclass" : "tile-container-generic",
    "tile_generic_class" : "tile-generic",
    "tile_generic_size" : "normal",
    "tile_generic_color" : "none",
    "tile_generic_titletype" : "generic",
    "tile_generic_titlebgcolor" : "none",
    "tile_generic_overlaytype" : "static",
    "tile_generic_overlaybgcolor" : "none",
    "tile_generic_imagetype" : "generic",
    "tile_generic_imagebgcolor" : "none",
    "tile_generic_icon" : "",
    "tile_generic_linktarget" : "",
    
    "tile_gallery1_containerclass" : "tile-container-gallery1",
    "tile_gallery1_class" : "tile-gallery1 tile-common",
    "tile_gallery1_size" : "normal",
    "tile_gallery1_color" : "0",
    "tile_gallery1_titletype" : "default",
    "tile_gallery1_titlebgcolor" : "gallery-title-bg",  /* (delegated to CSS) */
    "tile_gallery1_overlaytype" : "default",
    "tile_gallery1_overlaybgcolor" : "gallery-overlay-bg",   /* (delegated to CSS) */
    "tile_gallery1_imagetype" : "contain",
    "tile_gallery1_imagebgcolor" : "none",
    "tile_gallery1_icon" : "",
    "tile_gallery1_linktarget" : "_blank",

    /* low-level tile individual styles */
    "tile_container" : "tile-container",
    "tile_wrap" : "tile",
    "tile_color_prefix" : "tile-color-",
    "tile_content" : "tile-content",
    "tile_icon" : "tile-icon",
    /* title styles */
    "tile_title" : "tile-title",
    /* default title styles (looked up based on title type) */
    "tile_title_default" : "tile-title-default tile-title-common",
    "tile_title_generic" : "tile-title-generic",
    /* overlay styles */
    "tile_overlay" : "tile-overlay",
    /* default overlay styles (looked up based on overlay type) */
    "tile_overlay_default" : "tile-overlay-slide-up",
    /* styles for specific overlay types (extensible by theme) (looked up based on overlay type) */
    "tile_overlay_slide_up" : "tile-overlay-slide-up",
    "tile_overlay_static" : "tile-overlay-static",
    /* image styles */
    "tile_image" : "tile-image",
    /* default image styles (looked up based on image type) */
    "tile_image_default" : "tile-image-cover tile-image-common",
    /* styles for specific tile types (extensible by theme) (looked up based on image type) */
    "tile_image_cover" : "tile-image-cover tile-image-common",
    "tile_image_contain" : "tile-image-contain tile-image-common",
    "tile_image_generic" : "tile-image-generic",

  /* Image galleries */
    "gallery_share_view_width" : 500,
    "gallery_share_view_height" : 500,

  /* Image container*/
    "image_responsive" : "img-fluid",

  /* Panels */
    "panel_wrap" : "card",
    "panel_head" : "card-header",
    "panel_title" : "card-title",
    "panel_body" : "card-block",
    "login_wrap" : "modal-dialog modal-content card",                  /* the login box is a special form of a panel in this case */
    "login_header": "modal-header card-header",
    "login_body" : "modal-body card-block",

  /* Alert Box */
    "alert_wrap" : "alert",
    "alert_prefix_type" : "alert-",     /* Bootstrap: "alert-"; Note: This may be removed */
    "alert_type_info" : "alert-info",
    "alert_type_success" : "alert-success",
    "alert_type_warning" : "alert-warning",
    "alert_type_secondary" : "alert-secondary",
    "alert_type_alert" : "alert-danger",
    "alert_type_error" : "alert-danger",
    
    /* Alert options by type */
    "alert_default_closable" : true,

  /* Pricing Tables */
    "pricing_wrap" : "card text-center",
    "pricing_price" : "price",
    "pricing_description" : "card-text",
    "pricing_ribbon" : "ribbon",
    "pricing_title" : "card-title card-header",
    "pricing_cta" : "card-link",
    "pricing_bullet" : "",

    /* Tabs */
    "tabs_container" : "",
    "tabs_wrap" : "nav nav-tabs",                  /* Bootstrap: "nav nav-tabs" */
    "tabs_item_title" : "nav-item",                /* Bootstrap: "nav-item" */
    "tabs_item_title_link" : "nav-link",           /* Bootstrap: "nav-link" */
    "tabs_item_title_link_active" : "active",
    "tabs_item_title_active" : "",
    "tabs_item_title_disabled" : "",
    "tabs_item_container": "tab-content",          /* Bootstrap: "tab-content" */
    "tabs_item_wrap" : "tab-pane",                 /* Bootstrap: "tab-pane" */
    "tabs_item_active" : "active",
    "tabs_item_disabled" : "disabled",

  /* Modal */
    "modal_wrap" : "modal fade",

  /* Slider */
    "slider_container" : "carousel slide",        /* Bootstrap: "carousel slide" */
    "slider_wrap" : "carousel-inner",             /* Bootstrap: "carousel-inner" */
    "slide_container" : "carousel-item",
    "slide_image" : "d-block img-fluid",
    "slide_content" : "carousel-caption",         /* Bootstrap: "carousel-caption" */
    "slide_linktarget" : "_blank",

  /* Chart */
    "chart_legend" : "chart-legend",

  /* Progress Bar */
    "progress_container" : "progress",
    "progress_wrap" : "progress-bar",
    "progress_bar" : "sr-only",

    "progress_state_alert" : "danger",
    "progress_state_info" : "info",
    "progress_state_success" : "success",

  /* Fields */
    /* fields type-based styles */
    "fields_default_labeltype" : "horizontal",
    "fields_default_labelposition" : "left",
    "fields_default_labelarea" : true,
    "fields_default_labelareaexceptions" : "submit submitarea",
    "fields_default_labelarearequirecontent" : false,
    "fields_default_labelareaconsumeexceptions" : "",
    "fields_default_collapse" : false,
    "fields_default_collapsepostfix" : true,
    "fields_default_collapsedinlinelabel" : "datetime",
    "fields_default_checkboxtype" : "",
    "fields_default_radiotype" : "",
    "fields_default_inlineitems" : "",
    "fields_default_fieldargs" : [
        "requiredClass" : "required",
        "requiredTooltip" : "#LABEL:CommonRequired",
        "invertedClass" : "field-inverted",
        "standardClass" : "field-standard"
    ],
    "fields_default_totalcolumns" : 12,
    "fields_default_labelcolumns" : 2,
    "fields_default_postfixcolumns" : 1,
    "fields_default_labelsmallcoldiff" : 1,
    "fields_default_widgetpostfixcombined" : false,
    
    "fields_formwidget_totalcolumns" : 11,
    
    "fields_default_alt1_labelareaconsumeexceptions" : "checkbox-single radio-single",
    "fields_default_alt1_checkboxtype" : "simple",

    "fields_default_nolabelarea_labeltype" : "none",
    "fields_default_nolabelarea_labelposition" : "none",
    "fields_default_nolabelarea_labelarea" : false,
    "fields_default_nolabelarea_labelareaexceptions" : "",
    "fields_default_nolabelarea_labelarearequirecontent" : false,

    "fields_default_compact_labeltype" : "vertical",
    "fields_default_compact_labelposition" : "left",
    "fields_default_compact_labelarea" : true,
    "fields_default_compact_labelareaexceptions" : "checkbox-single radio-single",
    "fields_default_compact_labelarearequirecontent" : true,

    "fields_default_manual_labeltype" : "horizontal",
    "fields_default_manual_labelposition" : "left",
    "fields_default_manual_labelarea" : false,
    "fields_default_manual_labelareaexceptions" : "",
    "fields_default_manual_labelarearequirecontent" : false,
    "fields_default_manual_labelareaconsumeexceptions" : "",
    "fields_default_manual_collapse" : false,
    "fields_default_manual_collapsepostfix" : true,
    "fields_default_manual_collapsedinlinelabel" : "",
    "fields_default_manual_checkboxtype" : "",
    
    "fields_default_manual_widgetonly_labeltype" : "horizontal",
    "fields_default_manual_widgetonly_labelposition" : "left",
    "fields_default_manual_widgetonly_labelarea" : false,
    "fields_default_manual_widgetonly_labelareaexceptions" : "",
    "fields_default_manual_widgetonly_labelarearequirecontent" : false,
    "fields_default_manual_widgetonly_labelareaconsumeexceptions" : "",
    "fields_default_manual_widgetonly_collapse" : false,
    "fields_default_manual_widgetonly_collapsepostfix" : true,
    "fields_default_manual_widgetonly_collapsedinlinelabel" : "",
    "fields_default_manual_widgetonly_checkboxtype" : "",
    "fields_default_manual_widgetonly_fieldargs" : [
        "widgetOnly" : "true"
    ],

    "fields_generic_labeltype" : "horizontal",
    "fields_generic_labelposition" : "left",
    "fields_generic_labelarea" : false,
    "fields_generic_labelareaexceptions" : "",
    "fields_generic_labelarearequirecontent" : false,
    "fields_generic_labelareaconsumeexceptions" : "",
    "fields_generic_collapse" : false,
    "fields_generic_collapsepostfix" : true,
    "fields_generic_collapsedinlinelabel" : "",
    "fields_generic_checkboxtype" : "",

    /* misc fields styles */
    "fields_wrap" : "input-group",
    "fields_label" : "input-group-addon",
    
    /* generic field styles*/
    "field_input_default" : "form-control",
    "field_textarea_default" : "form-control",
    "field_datetime_default" : "form-control",
    "field_datefind_default" : "form-control",
    "field_datefind_select_default" : "form-control",
    "field_select_default" : "form-control",
    "field_lookup_default" : "form-control",
    "field_file_default" : "form-control",
    "field_password_default" : "form-control",
    "field_reset_default" : "form-control",
    "field_submit_default" : "form-control",
    "field_textfind_default" : "form-control",
    "field_textfind_select_default" : "form-control",
    "field_rangefind_default" : "form-control",
    "field_rangefind_select_default" : "form-control",
    "field_display_default" : "",
    "field_generic_default" : "",

    /* checkbox type styles */
    "field_checkbox_default" : "switch btn-sm checkbox-type-switch",
    "field_checkbox_default_type" : "checkbox-type-switch",
    "field_checkbox_default_labeltype" : "extralabel",
    "field_checkbox_default_labelposition" : "after",
    "field_checkbox_simple" : "checkbox-type-simple",
    "field_checkbox_simple_type" : "checkbox-type-simple",
    "field_checkbox_simple_labeltype" : "spanonly",
    "field_checkbox_simple_labelposition" : "after",
    "field_checkbox_simple_standard" : "checkbox-type-simple-standard",
    "field_checkbox_simple_standard_type" : "checkbox-type-simple-standard",
    "field_checkbox_simple_standard_labeltype" : "standard",
    "field_checkbox_simple_standard_labelposition" : "after",
    
    /* radio type styles */
    "field_radio_default" : "radio-type-default",
    "field_radio_default_type" : "radio-type-default",
    "field_radio_default_labeltype" : "standard",
    "field_radio_default_labelposition" : "after",
    "field_radio_simple_standard" : "radio-type-simple-standard",
    "field_radio_simple_standard_type" : "radio-type-simple-standard",
    "field_radio_simple_standard_labeltype" : "standard",
    "field_radio_simple_standard_labelposition" : "after",
    
    /* datetime type styles */
    "field_datetime_default_title" : "LABEL+FORMAT",

    /* display type styles */
    "field_display_tooltip" : "has-tip field-tooltip-wrap",
    
    /* submit type styles */
    "field_submit_default_text" : "#PROP:CommonSubmit",
        
    /* generic type styles */
    "field_generic_tooltip" : "has-tip field-tooltip-wrap",

    /* default, for all fields */
    "field_default_tooltip" : "has-tip tip-top",
    "field_default_tooltip_attribs" : [
      "data-tooltip" : "_NO_VALUE_",
      "aria-haspopup" : "true",
      "data-options" : "disable_for_touch:true"
    ],

    /* misc single field styles */
    "field_inline" : "field-inline",
    "field_tooltip_wrap" : "field-tooltip-wrap",
    "field_inverted" : "field-inverted",
    "field_standard" : "field-standard",
    "field_extra" : "field-extra",

    /* Field type style name maps */
    "field_type_stylenames_scipio" : [
        "default": true
    ],
    "field_type_stylenames_ofbiz" : [
        "display": "display",
        "hyperlink": "hyperlink",
        "text": "input",
        "textarea": "textarea",
        "date-time": "datetime",
        "drop-down": "select",
        "check": "checkbox",
        "radio": "radio",
        "submit": "submit",
        "reset": "reset",
        "hidden": "hidden",
        "ignored": "ignored",
        "text-find": "textfind",
        "date-find": "datefind",
        "range-find": "rangefind",
        "lookup": "lookup",
        "file": "file",
        "password": "password",
        "image": "image",
        "display-entity": "displayentity",
        "container": "container",
        "default": "other"
    ],

    /* field types that should not get containers when they are children of other fields, by default */
    "field_type_nocontainer_whenchild" : [
        /* "submit":true */   /* only if parent is submitarea (below) */
        "radio":true,
        "checkbox":true,
        "option":true
    ],
    
    /* field types that when a child field has as parent, the child should not get a container, by default
        (in other words this parent's children should not get containers, by default) */
    "field_type_nocontainer_whenhasparent" : [
        "submitarea":true
    ],

    /* Form type to @table type maps */
    "form_type_tabletypes_ofbiz" : [
        "list": "data-list",
        "multi": "data-list-multiform",
        "default": "generic"
    ],

    /* Field select element multiple-select classes (jQuery asmselect) */
    "field_select_asmselect" : [
      "containerClass" : "asmContainer",
      "selectClass" : "asmSelect form-control",
      "optionDisabledClass" : "asmOptionDisabled",
      "listClass" : "asmList",
      "listSortableClass" : "asmListSortable",
      "listItemClass" : "asmListItem",
      "listItemLabelClass" : "asmListItemLabel",
      "removeClass" : "asmListItemRemove btn btn-raised btn-sm btn-default",
      "highlightClass" : "asmHighlight"
    ],

    /*List of icons that can be assigned per webapplication and specific menu_items. Uses font-awesome icons by default.*/
    "app_icon" : [
        "main" : "fa-home", // Dashboard link
        "admin" : "fa-wrench",
        "accounting" : "fa-balance-scale",
        "assetmaint" : "fa-cogs",
        "ofbizsetup" : "fa-check-square",
        "cms" : "fa-file-text",
        "CRM" : "fa-bullhorn",
        "humanres" : "fa-user-times",
        "manufacturing" : "fa-industry",
        "facility" : "fa-cube",
        "marketing" : "fa-comments",
        "order" : "fa-recycle",
        "party" : "fa-users",
        "catalog" : "fa-sitemap",
        "shop" : "fa-shopping-cart",
        "solr" : "fa-search",
        "workeffort" : "fa-sliders",
        "setup" : "fa-magic"
        ],

  /* Always declare last */
    "dummy" : ""
];
