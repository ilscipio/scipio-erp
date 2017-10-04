/**
 * Master HTML templating variables include, standard Scipio markup
 *
 * A set of global variables that define common styling classes, part of standard Scipio Freemarker API.
 * Automatically included at all times, unless overridden by properties or themes.
 * Intended to be swappable.
 *
 * NOTES:
 * * Currently targeted toward Foundation CSS.
 */

/*
*************************************
* MAIN STYLES *
*************************************
* Scipio automatically recognizes the "styles" hash variable and makes it available.
*/


context.styles = [
    
  /* Generic */
    "framework" : "foundation",
    "customSideBar" : false,
    "disabled" : "disabled",
    "active" : "active",
    "selected" : "selected",
    "button" : "button",
    "button_prefix" : "",
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
    "expanded" : "expanded",
    "collapse" : "collapse",
    "collapsed" : "collapsed",
    "required" : "required",
    "prefix" : "prefix",
    "postfix" : "postfix",
    "hidden" : "hide",
    "icon" : "fa",
    "icon_prefix" : "fa-",
    "icon_button" : "fa fa-cog",
    "icon_button_value" : "&#xf013;",
    "icon_calendar" : "fa fa-calendar",
    "icon_shopping_cart" : "fa fa-shopping-cart",
    "icon_arrow" : "fa fa-arrow-right",
    "icon_list" : "fa fa-list",
    "icon_user" : "fa fa-user",
    "icon_password" : "fa fa-lock",
    "icon_tenant" : "fa fa-cloud",
    "icon_error" : "fa fa-exclamation-triangle",
    "icon_edit" : "fa fa-pencil-square-o",
    "icon_check" : "fa fa-check",
    "icon_home" : "fa fa-home",
    "heading_level_prefix" : "heading-level-",
    "switch" : "switch",
    "dropdown" : "dropdown",
    "fullwidth" : "fullWidth",
    "large_container_factor" : 6,       /* This is a special value part of a heuristic to help detect when a parent container is logically "large" or "small". It is a float value. By default, currently we assume 6. */
    "closable" : "close",
    
  /* Basic tooltip */
    "tooltip" : "has-tip",
    "tooltip_left" : "has-tip tip-left",
    "tooltip_top" : "has-tip tip-top",
    "tooltip_right" : "has-tip tip-right",
    "tooltip_bottom" : "has-tip tip-bottom",
    "tooltip_delim" : " - ",
    
  /* Common messages (default message type container styles)
      NOTE: these styles are secondary to markup choices made by theme markup macro */
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
    
  /* Menus
       menu_xxx classes are looked up by the @menu and @menuitem macros as defaults for each menu type.
       NOTE: currently the disabled/active/selected styles are added in addition to the base styles,
          not replacing them; this is contrary to the current menu widgets implementation at the time of this
          writing, where disabled/active/selected styles replace their base styles.
          FIXME: we should use same behavior everywhere, but not clear which is more desirable at this time
              (menu widgets' way of replacing is more flexible, but not necessarily intuitive...)
          WARN: as of ofbiz 14.12 branch revision 1720933 the behavior is no longer clear or consistent for menu widgets
              (though it was before)
       NOTE: there is duplication despite the menu_default_ fallbacks (supported by @menu) because otherwise the styles
          are too hard to use from EL syntax (widgets) - can't easily code fallbacks.
       NOTE: The widget attribute "selected" usually translates to "active" in styles hash/FTL/CSS terminology (where "selected" may mean something else).
          This is because there's only one highlight option for widgets and by convention the selected widget attrib should practically always be used to select the "active" menu item. */
    /* Generic menu types */
    "menu_generic" : "menu-type-generic",
    "menu_generic_altnested" : "menu-type-generic",
    "menu_generic_type" : "menu-type-generic",  /* specific type style, normally already included in previous entry, but can reference individually */
    "menu_generic_active" : "menu-active",
    "menu_generic_activetarget" : "menu-active-target", // NOTE: this is added in ADDITION to active
    "menu_generic_activeancestor" : "menu-active-ancestor", // NOTE: this is added in ADDITION to active
    "menu_generic_levelprefix" : "menu-level-",
    "menu_generic_nameprefix" : "menu-name-",
    "menu_generic_item" : "",
    "menu_generic_itemdisabled" : "disabled",
    "menu_generic_itemactive" : "active",
    "menu_generic_itemactivetarget" : "active-target", // NOTE: this is added in ADDITION to active
    "menu_generic_itemactiveancestor" : "active-ancestor", // NOTE: this is added in ADDITION to active
    "menu_generic_itemselected" : "selected",
    "menu_generic_itemnameprefix" : "item-name-",
    "menu_generic_item_contentdisabled" : "disabled",
    "menu_generic_item_contentactive" : "active",
    "menu_generic_item_contentactivetarget" : "active-target",
    "menu_generic_item_contentactiveancestor" : "active-ancestor",
    "menu_generic_item_contentselected" : "selected",
    "menu_generic_item_link" : "",
    "menu_generic_item_text" : "text-entry",
    "menu_generic_item_submit" : "",
    "menu_generic_item_generic" : "",
    "menu_button" : "button-group force-button menu-type-button",
    "menu_button_altnested" : "menu-type-button",
    "menu_button_type" : "menu-type-button",
    "menu_button_item" : "",
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
    "menu_button_item_link" : "button tiny",
    "menu_button_item_text" : "text-entry",
    "menu_button_item_submit" : "button tiny",
    "menu_button_dropdown" : "f-dropdown menu-type-button-dropdown",
    "menu_button_dropdown_altnested" : "menu-type-button-dropdown",
    "menu_button_dropdown_type" : "menu-type-button-dropdown",
    "menu_button_dropdown_specialtype" : "button-dropdown",
    "menu_button_dropdown_title" : "button tiny dropdown",
    "menu_button_dropdown_item" : "",
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
    "menu_button_dropdown_item_link" : "",
    "menu_button_dropdown_item_text" : "text-entry",
    "menu_button_dropdown_item_submit" : "",
    /* Main navigation menus */
    "menu_main" : "dropdown menu-type-main",
    "menu_main_altnested" : "menu-type-main",
    "menu_main_type" : "menu-type-main",
    "menu_main_specialtype" : "main",       /* specialtype allows to reuse special markup across many custom menu types (specified here instead of hardcoding custom type checks in markup macros) */
    "menu_main_wrap" : "has-dropdown not-click active",
    "menu_main_item" : "",
    "menu_main_itemdisabled" : "disabled",
    "menu_main_itemactive" : "active",
    "menu_main_itemactivetarget" : "active-target",
    "menu_main_itemactiveancestor" : "active-ancestor",
    "menu_main_itemselected" : "selected",
    "menu_main_item_contentdisabled" : "disabled",
    "menu_main_item_contentactive" : "active",
    "menu_main_item_contentactivetarget" : "active-target",
    "menu_main_item_contentactiveancestor" : "active-ancestor",
    "menu_main_item_contentselected" : "selected",
    "menu_main_item_link" : "",
    "menu_main_item_text" : "text-entry",
    "menu_main_item_submit" : "",
    "menu_sidebar" : "side-nav menu-type-sidebar",
    "menu_sidebar_altnested" : "menu-type-sidebar",
    "menu_sidebar_type" : "menu-type-sidebar",
    "menu_sidebar_specialtype" : "sidebar",
    "menu_sidebar_wrap" : "",
    "menu_sidebar_item" : "",
    "menu_sidebar_itemdisabled" : "disabled",
    "menu_sidebar_itemactive" : "active",
    "menu_sidebar_itemactivetarget" : "active-target",
    "menu_sidebar_itemactiveancestor" : "active-ancestor",
    "menu_sidebar_itemselected" : "selected",
    "menu_sidebar_itemdashboard" : "dashboard",
    "menu_sidebar_item_contentdisabled" : "disabled",
    "menu_sidebar_item_contentactive" : "active",
    "menu_sidebar_item_contentactivetarget" : "active-target",
    "menu_sidebar_item_contentactiveancestor" : "active-ancestor",
    "menu_sidebar_item_contentselected" : "selected",
    "menu_sidebar_item_link" : "",
    "menu_sidebar_item_text" : "text-entry",
    "menu_sidebar_item_submit" : "",
    /* Tab and secondary navigation menus*/
    "menu_tab" : "button-group force-button menu-type-tab",
    "menu_tab_altnested" : "menu-type-tab",
    "menu_tab_type" : "menu-type-tab",
    "menu_tab_item" : "",
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
    "menu_tab_item_link" : "button tiny",
    "menu_tab_item_text" : "text-entry",
    "menu_tab_item_submit" : "button",
    "menu_subtab" : "button-group force-button menu-type-subtab",
    "menu_subtab_altnested" : "menu-type-subtab",
    "menu_subtab_type" : "menu-type-subtab",
    "menu_subtab_item" : "",
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
    "menu_subtab_item_link" : "button tiny",
    "menu_subtab_item_text" : "text-entry",
    "menu_subtab_item_submit" : "button tiny",
    /* Default section menu */
    "menu_section" : "button-group menu-type-section",
    "menu_section_altnested" : "menu-type-section",
    "menu_section_type" : "menu-type-section",
    "menu_section_item" : "",
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
    "menu_section_item_link" : "button tiny",
    "menu_section_item_text" : "text-entry",
    "menu_section_item_submit" : "button tiny",
    /* Default section menu inlined with title
        FIXME: looks too big/clumsy with these buttons (floats right of title) */
    "menu_section_inline" : "button-group menu-type-section-inline",
    "menu_section_inline_altnested" : "menu-type-section-inline",
    "menu_section_inline_type" : "menu-type-section-inline",
    "menu_section_inline_item" : "",
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
    "menu_section_inline_item_link" : "button tiny",
    "menu_section_inline_item_text" : "text-entry",
    "menu_section_inline_item_submit" : "button tiny",
    /* default entry is used for any encountered menu types that have no specific entries in this hash,
         and also for individual value fallbacks for values not defined in the type-specific entries above */
    "menu_default" : "menu-type-default",
    "menu_default_altnested" : "menu-type-default",     /* ALTERNATE to main "menu_default" class, used when 1) is child menu and 2) is of same type as parent */
    "menu_default_type" : "menu-type-default",
    "menu_default_toplevel" : "toplevel",                               /* ADDED to ALL menus detected or marked as top-level */
    "menu_default_nested" : "nested",                                   /* ADDED to ALL menus detected or marked as nested */
    "menu_default_nestedsame" : "nested-sametype",                      /* ADDED to ALL menus detected or marked as nested AND that are of same type as parent */
    "menu_default_htmlwrap" : true,
    "menu_default_active" : "menu-active",
    "menu_default_activetarget" : "menu-active-target", // NOTE: this is added in ADDITION to active
    "menu_default_activeancestor" : "menu-active-ancestor", // NOTE: this is added in ADDITION to active
    "menu_default_levelprefix" : "menu-level-",
    "menu_default_nameprefix" : "menu-name-",
    "menu_default_item" : "",
    "menu_default_item_htmlwrap" : true,
    "menu_default_itemdisabled" : "disabled",
    "menu_default_itemactive" : "active",
    "menu_default_itemactivetarget" : "active-target", // NOTE: this is added in ADDITION to active
    "menu_default_itemactiveancestor" : "active-ancestor", // NOTE: this is added in ADDITION to active
    "menu_default_itemselected" : "selected",
    "menu_default_itemnameprefix" : "item-name-",
    "menu_default_item_contentdisabled" : "disabled",
    "menu_default_item_contentactive" : "active",
    "menu_default_item_contentactivetarget" : "active-target",
    "menu_default_item_contentactiveancestor" : "active-ancestor",
    "menu_default_item_contentselected" : "selected",
    "menu_default_item_link" : "",
    "menu_default_item_text" : "text-entry",
    "menu_default_item_submit" : "",
    "menu_default_item_generic" : "",
    
    /* separate-menu configurations */
    "sepmenu_default_sidebar_config" : [
        "layout" : "before-inline",
        "sepTitle" : "#LABEL:CommonActions",
        "sepTitleItemClass" : "+heading",
        "nonsepTitle" : "#LABEL:CommonNavigation",
        "nonsepTitleItemClass" : "+heading",
        "nonsepTitleAlways" : true,
        "separatorEnabled" : true,
        "separatorItemClass" : "+separator",
        "sepMenuClass" : "+scipio-nav-actions-separate-menu", // NOTE: currently this might not be outputted... but still may have internal use
        "sepItemClass" : "+scipio-nav-actions-menu-item" // ugly kludge due to having no UL wrapper or any wrapper at all around actions menu items...
    ],
    
    /* Misc menu styles */
    "menu_subappitem" : "subappitem",
    "menu_subappitem_link" : "subapplink",
    "menu_buttonstyle_alt1" : "button-style-1",         /* stock Ofbiz remnant, "button-style-1" in originals */
    "menu_buttonstyle_alt2" : "button-style-2",         /* stock Ofbiz remnant, "button-style-2" in originals */
    "menu_noclear" : "menu-no-clear",                   /* A remnant from stock Ofbiz menu widgets, some of which label with a "no-clear" style */
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
    
  /* Data and Tables
    NOTES:
      * Currently, despite settings here, responsive tables will automatically be disabled for tables that have no header. See @table.
  */
    /* table type-based styles
         Used for default table class and attribute lookups by the @table macro, based on table type. */
    "table_generic" : "table-type-generic",    /* represents free-form <table>. probably should have no class (let template decide). */
    "table_generic_type" : "table-type-generic",  /* specific table identifier */
    "table_generic_cellspacing" : "",
    "table_generic_rowtype" : "generic",    /* the default row type is normally "content", but generic tables need "generic" */
    "table_data_list" : "basic-table table-type-data table-type-data-list",  /* analogous to ofbiz "list" type form widgets (and still very close to "multi" type) */
    "table_data_list_type" : "table-type-data table-type-data-list",
    "table_data_list_cellspacing" : 0,
    "table_data_list_responsive" : true,
    /* "table_data_list_autoaltrows" : true, */
    "table_data_list_multiform" : "basic-table table-type-data table-type-data-list-multiform",  /* analogous to ofbiz "multi" type form widgets (but still basically a "list" type) */
    "table_data_list_multiform_type" : "table-type-data table-type-data-list-multiform",
    "table_data_list_multiform_cellspacing" : 0,
    "table_data_list_multiform_responsive" : false,
    "table_data_complex" : "basic-table table-type-data table-type-data-complex", /* like data_list but with complex structure (no form widget equivalent) */
    "table_data_complex_type" : "table-type-data table-type-data-complex",
    "table_data_complex_cellspacing" : 0,
    "table_summary" : "basic-table table-type-summary",
    "table_summary_type" : "table-type-summary",
    "table_summary_cellspacing" : 0,
    "table_fields" : "basic-table table-type-fields",               /* similar to ofbiz "single" type form widgets; may be acceptable for display-only fields */
    "table_fields_type" : "table-type-fields",
    "table_fields_cellspacing" : 0,
    "table_fields_vert" : "basic-table table-type-fields-vert",     /* like fields but arranged like data with labels in a header row */
    "table_fields_vert_type" : "table-type-fields-vert",
    "table_fields_vert_cellspacing" : 0,
    /* default entry is used by @table for any encountered table types that have no specific entries in this hash.
         in other words, it is the default style for table types that this theme does not recognize, or "all others".
         it is also used for individual fallback values. */
    "table_default" : "basic-table table-type-default",
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
        "order": []         /* prevents ordering upon initialization */
    ],
    "table_default_scrollable_options" : [ /* this is only used for tables not marked to use responsive but need scrollable=true */
        "fixedHeader" : false,
        "scrollX" : true,   /* NOTE: this option gets overridden */
        "info" : false,
        "paging" : false,
        "searching" : false,
        "ordering" : false
    ],
    
    /* form widget styles in _addition_ to the @table styles above */
    "table_formwidget" : "dark-grid",                   /* default class for form widget tables (does NOT include _type because _type always added) */
    "table_formwidget_type" : "table-type-formwidget",  /* specific identifier for form widget tables. NOTE: this is ALWAYS added so doesn't need to be part of table_formwidget */
    
    /* other table-related styles */
    "table_basic" : "basic-table",                                      /* This is not a table type, but simply the basic-table style. */
    
    /* table spacing styles: currently "spacing" is a high level term meaning cellspacing and/or cellpadding; theme decides.
        In most cases spacing seems like a secondary display concern, which means use -hint style. If a table has to prevent cellspacing specifically, can set table_nocellspacing, but should be rarely needed.
        At time of creation these were mostly used (hint versions) on previously stock tables that used to specify non-zero cellspacing or cellpadding (adding both together).
        WARN: Higher-level CSS scipio and custom table classes should be preferred to these. */
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
    
    /* table border styles
        WARN: higher-level CSS scipio and custom table classes should be preferred to these. */
    "table_border_on_hint" : "table-border-on-hint",
    "table_border_on" : "table-border-on",
    
    "table_noborder_hint" : "table-noborder-hint",
    "table_noborder" : "table-noborder",
    
    /* row styles */
    "row_reg" : "even",
    "row_alt" : "odd",
    "row_selected" : "selected",
    
  /* Generic action styles - help the UI describe actions possible in the system
      action_xxx styles are fully generic and meant to apply to any interactive UI element (link, menu item, onclick, etc.) and
      for any element that directly or indirectly triggers or encourages an action (nav link, submit button, etc.).*/
    "action_prefix" : "action-",                          /* sometimes used to detect if a style already contains an action style or not */

    /* Generic UI functionality action types */
    "action_nav" : "action-nav",                          /* basically, identifies a navigation link. usually optional; added for clarity. */
    "action_run" : "action-run",                          /* identifies a link or item that actually runs an action, such as a form submit button, or download PDF button, or intermediate action like clearing a form. nav links should never have this. */
    
    /* Action state scope styles
        By default, most actions are assumed to be action_scope_sys (database updates).
        NOTE: these are decoupled from action_run, such that you could have these on a nav link (which
            would mean, "this function will indirectly modify this scope once it's actually run"), though not really useful. */
    "action_scope_sys" : "action-scope-sys",              /* action that changes system state, usually database */
    "action_scope_session" : "action-scope-session",      /* action that changes user session state */
    "action_scope_local" : "action-scope-local",          /* action that changes state on a page or form only (usually with javascript or input reset button) */
    
    /* Convenience combinations of generic functionality + scope, useful on menu items, image links, and any non-text-link elements that identify or perform actions */
    "action_nav_sys" : "action-nav action-scope-sys",
    "action_nav_session" : "action-nav action-scope-session",
    "action_nav_local" : "action-nav action-scope-local",
    "action_run_sys" : "action-run action-scope-sys",
    "action_run_session" : "action-run action-scope-session",
    "action_run_local" : "action-run action-scope-local",
    
    /* Specific action types
        DEV NOTE: these are intended to support specific actions, but only specific actions that are reusable/applicable to many situations, such that while specific they should be able to apply to a number of different elements (from entity values to something farfetched like media playback) */
    "action_generic" : "action-generic",                  /* generic action class; avoid using; always use more specific where possible */
    "action_cancel" : "action-cancel",                    /* cancel (another) action class. WARN: this is only for cancelling other actions or navigations. it does not mean "set order status to cancelled" (use an action_modify for that, like action_terminate); is only for actions that cancel other actions in progress, not new requests for modifying system state. */

    /* Specific state-changing actions */
    "action_modify" : "action-modify",                    /* generic modify action, can also be a collection of modifications */
    "action_add" : "action-modify action-add",            /* add item action: "Create Entity", "New Value", "Add", etc. */
    "action_update" : "action-modify action-update",      /* update item action: "Update Entity", "Edit", etc. NOTE: this tends to be used for any action that updates, but it's better to use more specific ones */
    "action_remove" : "action-modify action-remove",      /* (logical) remove item action: "Delete", "Remove", etc. */
    "action_clear" : "action-modify action-clear",        /* clear action: "Clear", "Empty Fields", "Wipe", etc. */
    "action_move" : "action-modify action-move",          /* move action: "Move", etc. */
    "action_copy" : "action-modify action-copy",          /* copy action: "Copy", "Duplicate", etc. */
    "action_configure" : "action-modify action-configure",/* configure action: "Configure", "Setup", etc. */
    "action_begin" : "action-modify action-begin",        /* begin action: "Begin", "Start", "Start Job", etc. */
    "action_reset" : "action-modify action-reset",        /* reset action: "Reset", "Restart", etc. */
    "action_hold" : "action-modify action-hold",          /* hold action: "Hold", "Pause", etc. */
    "action_terminate" : "action-modify action-terminate",/* terminate action: "Cancel Order", "Expire", "Stop", "Stop Job", etc. */
    "action_complete" : "action-modify action-complete",  /* complete action: "Complete Order", "Mark Success", etc. */
    "action_updatestatus" : "action-modify action-updatestatus",  /* update status action (any status not better accounted for in previous): "Approve", "Set to Xxx", etc. NOTE: this type was added later, so may be underused (action_update, action_terminate, action_complete usually found instead) */
    "action_import" : "action-modify action-import",      /* upload action: "Import", "Upload", etc. */
    "action_transfer" : "action-modify action-transfer",  /* generic transfer action: "Transfer", "Send", "Send Email", "Receive", "Export to Foreign System", etc. */
    "action_send" : "action-modify action-send",          /* logical send action (transfer in send direction): "Send", "Send Email", "Export to Ebay", "Export to Google", etc. */
    "action_receive" : "action-modify action-receive",    /* logical receive action (transfer in receive direction): "Receive", "Import from Ebay", etc. */
    "action_sync" : "action-modify action-sync",          /* sync action (two-way transfer): "Sync", etc. */
    "action_register" : "action-modify action-login",     /* register action: "Register", etc.; usually goes with system scope */
    "action_login" : "action-modify action-login",        /* login action: "Login", etc.; usually goes with session scope */
    "action_logout" : "action-modify action-logout",      /* logout action: "Logout", etc.; usually goes with session scope */
    "action_continue" : "action-modify action-continue",  /* continue action: "Continue", "Next". NOTE: this is a special case of "update" for styling needs. usually goes with action_run_session, but action_run_sys should be used if submits permanent data not just in session. */
    
    /* Specific read-only actions (may or retrieve or submit data for analysis without changing state meaningfully) */
    "action_readonly" : "action-readonly",                                /* readonly action or actions */
    "action_read" : "action-readonly action-read",                        /* generic read action, can also be a collection of read actions */
    "action_find" : "action-readonly action-read action-find",            /* find action: "Find", "Search", "Lookup", etc. */
    "action_view" : "action-readonly action-read action-view",            /* view item action: "View PDF", etc. */
    "action_export" : "action-readonly action-read action-export",        /* export action: "Export", "Download", "Stream", "Print", etc. NOTE: this is not for "Export to Ebay" or foreign system; use action_transfer or action_send for that. */
    
    "action_visibility" : "action-readonly action-visibility",            /* visibility change base action. NOTE: Do not confuse with action_find. action_find is a fresh find/filter query, while action_visibility could at most modify an existing query, depending on the wording (but mainly is for showing/hiding anything, not just queries). */
    "action_show" : "action-readonly action-visibility action-show",      /* (visibility change only) show item action: "Show Old", etc. */
    "action_hide" : "action-readonly action-visibility action-hide",      /* (visibility change only) hide item action: "Hide All", etc. */
    "action_close" : "action-readonly action-close",                      /* close dialog or UI element action: "Close Popup", etc. NOTE: Should not change state! Do not use for "closing" a record (use action_terminate or other). */
    "action_select" : "action-readonly action-select",                    /* select action: "Select", "Choose", "Pick", etc. */
    "action_reload" : "action-readonly action-reload",                    /* reload action: "Refresh", etc. */
    "action_verify" : "action-readonly action-verify",                    /* verify action (should not change system state!): "Verify", "Validate", etc. */
    
    /* Misc action flags (fragments) */
    "action_external" : "action-external",                                /* external action, notably for marking external links */
    
    /* NOTE: There is a dedicated action-primary-inline to prevent CSS selection issues. Using action_inline alone as extra style on a link is not a good idea. */
    "action_primary" : "action-primary",                                  /* indicates the UI elem's primary reason for existing is to provide this action. usually implied for action_run. most action_nav that are part of a menu or with a static word like "View" fall into this category. */
    "action_primary_inline" : "action-primary-inline action-inline",      /* primary action that should explicitly blend in with surroundings, e.g. a link that shouln't have a button */
    "action_secondary" : "action-secondary",                              /* indicates the UI elem is providing this action as a secondary or convenience function, secondary to something else like providing information. for example, many action_nav (link_nav) text links with an entity value ID would fall into here. */
    "action_secondary_inline" : "action-secondary-inline action-inline",  /* secondary action that should explicitly blend in with surroundings, e.g. a link that shouln't have a button */
    
    "action_importance_low" : "action-importance-low",                    /* lowered importance */
    "action_importance_normal" : "action-importance-normal",              /* normal importance (assumed default) */
    "action_importance_high" : "action-importance-high",                  /* high importance (assumed default). e.g. "Create Order" */
    
    "action_inline" : "action-inline",                                    /* NOTE: Avoid using this in templates; use action_xxx_inline above instead */
    
    /* Convenience shorthands */
    "action_nav_cancel" : "action-nav action-cancel",
    "action_nav_sys_cancel" : "action-nav action-scope-sys action-cancel",
    "action_nav_session_cancel" : "action-nav action-scope-session action-cancel",
    "action_nav_local_cancel" : "action-nav action-scope-local action-cancel",
    "action_run_sys_cancel" : "action-run action-scope-sys action-cancel",
    "action_run_session_cancel" : "action-run action-scope-session action-cancel",
    "action_run_local_cancel" : "action-run action-scope-local action-cancel",
    
  /* Link styles (fragments - part of other styles) */
    /* misc/short link styles */
    "link_long" : "link-long",                            /* style to identify long links */
    
    /* classes to identify content type of links
        DEV NOTE (2016-01-19): these are not currently used enough to be useful, but they add clarification.
            they _should_ be used because it's difficult to impossible to select links based on child content in CSS without using javascript. */
    "link_type_text" : "link-type-text",
    "link_type_image" : "link-type-image",
    
  /* Link styles (standalone/full - can be used alone though many should be accompanied by an extra action_xxx action type class)
  
    Link type information and style selection process
    
    TEXT LINK TYPES:
      * MAIN TEXT LINKS: The main link text types are navigation text links (link_nav) and run-action text links (link_run_xxx).
        These are based on the two more generic action categories, action_nav and action_run, but help factor out text-link-specific styling choices.
        Links given link_nav or link_run_xxx should have the primary purpose of providing an action.
        In addition, links can - and most should - have a secondary action_xxx action type class (action_modify, action_readonly, action_add, action_view, etc.) added that identifies
        the type of action the link is triggering or that the navigation link is encouraging the user to do (IF SHORT ON TIME: the most important is to identify the run-action links that belong to action_modify).
        * link_run_xxx (implies action_run, action_scope_xxx): Text links that directly trigger actions, changes in the system, often the last button in a chain of links leading to an action.
          * Should always have a specific action type such as action_add, action_find, etc.
          * The xxx in link_run_xxx specifies scope (required). See scopes below. Most are link_run_sys.
          * e.g.: submit button on entity creation form
                  submit button on a find form
                  export PDF button
                  <input type="submit" value="Create/Submit" class="${styles.link_run_sys!} ${styles.action_add!}" />
                  <a href="${makeOfbizUrl("order.pdf")}" class="${styles.link_run_sys!} ${styles.action_export!}">PDF</a>
        * link_nav (implies action_nav): Any text link whose primary purpose (action_primary) is basic navigation or navigation toward or encouraging an action.
          * If no specific action type is given, more or less implies action_view (in a general sense)
          * e.g.: a "New" link that leads to another page with a form for entity creation
                  a "View" button for an entity value
                  <a href="${makeOfbizUrl("EditEntity?mode=new")}" class="${styles.link_nav!} ${styles.action_add!}">New</a>
                  <a href="${makeOfbizUrl("ViewEntity?id=abc")}" class="${styles.link_nav!}">View Entity</a>
      * INLINE TEXT LINKS (link_nav_inline, link_run_sys_inline): These are primary link actions like main text links, but with an explicit request by the template
        to mark the link as inline and blending in, or in practical terms, a link without a button.
        NOTE: If the link text is information first, please use info links instead (secondary action).
      * CANCEL TEXT LINKS (link_xxx_cancel, implies action_cancel): These are a special case. They are not actions - they are anti-actions, and get their own convenience styles that also help with factoring styles (instead of link_run_xxx/link_nav + action_cancel).
        NOTE: This does NOT include "logical" cancel actions on records, such as "Cancel Order" or "Set Status Cancelled" links, which should usually get link_run_sys + action_terminate or other appropriate style instead. Do not use action_cancel or link_xxx_cancel on these.
        Anti-action cancel text links should be given the following styles (no extra action_xxx necessary):
        * link_run_xxx_cancel: Cancels a run-action in progress (of xxx scope)
          * e.g.: <a href="javascript:cancelUpload()" class="${styles.link_run_sys_cancel!}">Cancel Upload</a>
        * link_nav_cancel: Use for "Back" and "Cancel" navigation links that lead to previous page
          * NOTE: Often nav cancel/back links look like they're part of a submit form for a run action, and are placed next to a run action (submit button), but they are still nav cancel actions if they lead to previous page, so use link_nav_cancel on these.
          * e.g.: <a href="${donePageUri}" class="${styles.link_nav_cancel!}">Back/Cancel</a>
      * INFO TEXT LINKS: Each of the main text links categories has an "info" subtype/variant: informational run-action text links (link_run_xxx_info) and informational nav text links (link_nav_info/link_nav_info_xxx).
        These are almost the same as main text links but help identify links who have primary purpose of showing information with their text content and only a secondary purpose of allowing an action. This allows different emphasis when styling (e.g., button vs no button).
        * link_run_xxx_info (implies action_run, action_scope_xxx, action_secondary): Informational run-action text link - any text link whose main purpose is to show information (the link text), but secondarily (action_secondary) triggers a run-action.
          * Should always have a specific action type such as action_add, action_find, etc.
          * NOTE: These are rare and included mainly for consistency. Info links are usually of the link_nav_info type. For run-actions in almost all cases you want to put emphasis on the action and less on the info in the link.
        * link_nav_info/link_nav_info_xxx (implies action_nav, action_secondary): Informational nav text link - any text link whose main purpose is to show information (the link text), but secondarily (action_secondary) provides a nav link.
          * If no specific action type is given, implies action_view (usually to view a specific record)
          * If using link_nav_info_xxx, the xxx describes the text content of the link.
          * e.g.: a "WS10000" order ID in an order listing table
                  a party name that links to a profile
                  <a href="${makeOfbizUrl("ViewOrder?orderId=WS10000")}" class="${styles.link_nav_info_id!}">WS10000</a>


    TYPE MODIFIERS:
      * ACTION SCOPES: All links and actions can be given a specific scope (but usually not useful on nav links, whereas link_run_xxx always requires and implies scope). If missing, usually action_scope_sys is assumed.
        * action_scope_sys (link_run_sys): Affects or may affect system state (large majority of links/actions)
        * action_scope_session (link_run_session): Affects logical user session only; sometimes may affect system state, but should not be a primary effect (e.g. user login, shopping cart update)
        * action_scope_local (link_run_local): Affects local page only (mostly for javascript actions in forms, UI element visibility toggles that affect only current page, etc.)
      * LONG TEXT LINKS: Every text link style has a _long version (link_xxx_long, link_long), which should be used when the text content is very long (definition of long depends on context).
      * EXTERNAL LINKS: For external links and actions ({{{https://www.ilscipio.com}}}), simply use the above categories, and add this extra style:
        * action_external: Marks external links and foreign system contact
          * e.g.: {{{<a href="https://www.ilscipio.com" class="${styles.link_nav!} ${styles.action_external!}">Ilscipio</a>}}}

    OTHER TYPES:
      * NON-TEXT LINKS, MENU ITEMS, CUSTOM LINKS AND OTHER ELEMENTS: Image and other non-text links as well as non-link elements that trigger actions or identify with an action even without causing one (such as menu item <li> elements or any other UI element),
        should NOT receive link_xxx classes. Links that need custom styles can also use something other than link_xxx if they're not appropriate. For these or any other action-related UI element,
        use the main action_xxx styles directly instead of link_xxx, which have analogous categories (the only difference is they can't factor out styling decisions as directly as link_xxx styles do, and must rely more on CSS/sass).
        For image links, it's a good idea to add link_type_image as well, to help with limitations of CSS selectors, even if it makes it more verbose.
        * Main Types:
          * action_run_xxx: Run-action (with scope).
            * e.g.: <a href="${makeOfbizUrl("removeProduct?productId=10000")}" class="x-icon ${styles.link_type_image!} ${styles.action_run_sys!} ${styles.action_remove!}"><img src="x-icon.jpg"/></a>
                    <@menuitem type="link" href="report.pdf" class="+${styles.action_run_sys!} ${styles.action_export!}" text="PDF" />
          * action_nav: Navigation action.
            * e.g.: <a href="${makeOfbizUrl("ViewProduct?productId=10000")}" class="product-img-link ${styles.link_type_image!} ${styles.action_nav!}"><img src="product-image.jpg"/></a>
                    <@menuitem type="link" href="EditProduct" class="+${styles.action_nav!} ${styles.action_add!}" text="New Product" />
        * Special cases:
          * action_cancel: Anti-action cancel links. NOTE: Here these do not have a dedicated convenience/factoring style (not needed).
            * e.g.: <a href="javascript:cancelUpload()" class="x-icon ${styles.link_type_image!} ${styles.action_run_sys!} ${styles.action_cancel!}"><img src="x-icon.jpg"/></a>
                    <@menuitem type="link" href="${donePage}" class="+${styles.action_nav!} ${styles.action_cancel!}" text="Back/Cancel" />
        * Other modifiers:
          * action_primary and action_secondary (rarely needed), action_external, etc.
        * MENU ITEM SPECIAL NOTES:
          * class style attribute location: Usually the action classes will end up on the menu item element (<li>) instead of the link (<a>),
            because this is how Ofbiz labeled actions classically in menu widgets (from action classes such as the old "create"/"delete" to
            disabled/active styles). In FTL, @menuitem can do the same for consistency (using class="+${...}" instead of contentClass="+${...}",
            although some non-action classes may still need to be set with contentClass="+${...}"). In the end, the stylesheet should account for these cases and try to support both placements.
        
    MISC:
      * VIEW ACTIONS: For action_view, sometimes there is not a clear distinction between link_run_xxx and link_nav (or action_run and action_nav), and subjective interpretation is required.
      * THE MOST IMPORTANT: The most important for UI is to ensure that links and other UI elems that directly change system state when triggered are properly identified, so user knows when he is affecting the system state. System scope is most important. These links require:
        * action_run_sys (link_run_sys)
          * session and local scope changes should also be identified where possible, but are not as urgently needed as system state change identification. If unclear, but involves system, use system scope.
        * action_modify (or any other specific action that implies action_modify - see action type styles)
      * STYLING: See Scipio's base theme _base.scss for current examples on how to style with CSS.
      
    DEV NOTES:
      * TODO?: May need/want CSS to identify to style differently depending on if these land within form tables vs outside or other criteria. But should probably do that with CSS selectors instead of here...
            Maybe remove "button tiny" from all link_nav and link_run_xxx and delegate to SCSS, applying button style to the other classes instead... but has complications with CSS selection limits and SASS mixins... not trivial...
  */
    /* Action text links (trigger an actual action in the system - NOT for use for opening pages toward actions!) */
    "link_run_sys" :                "link-type-text action-run action-scope-sys action-primary button tiny",                        /* link that actually performs an action (run-action), in system scope, such as most form submit buttons, "Create Order", "Download PDF", etc. */
    "link_run_sys_long" :           "link-type-text action-run action-scope-sys action-primary link-long",
    "link_run_sys_inline" :         "link-type-text action-run action-scope-sys action-primary-inline action-inline",
    "link_run_sys_inline_long" :    "link-type-text action-run action-scope-sys action-primary-inline action-inline link-long",
    "link_run_sys_cancel" :         "link-type-text action-run action-scope-sys action-primary action-cancel button tiny",          /* link that cancels a system action in progress, such as cancelling an upload (but NOT if only a button that leads back to previous page - use link_nav_action_cancel, and NOT for changing an order status to cancelled - use link_run_sys with appropriate action_xxx appended) */
    "link_run_sys_cancel_long" :    "link-type-text action-run action-scope-sys action-primary action-cancel link-long",
    "link_run_sys_info" :           "link-type-text action-run action-scope-sys action-secondary",                                  /* informational sys run-action (gives information first, triggers action as secondary purpose) */
    "link_run_sys_info_long" :      "link-type-text action-run action-scope-sys action-secondary link-long",
    "link_run_session" :            "link-type-text action-run action-scope-session action-primary button tiny",                    /* link for any action (run-action) that only modifies current session (logical, not necessarily HTTP session), not meaningful permanent system data. */
    "link_run_session_long" :       "link-type-text action-run action-scope-session action-primary link-long",
    "link_run_session_inline" :     "link-type-text action-run action-scope-session action-primary-inline action-inline",                              /* informational session run-action (gives information first, triggers action as secondary purpose) */
    "link_run_session_inline_long" :"link-type-text action-run action-scope-session action-primary-inline action-inline link-long",
    "link_run_session_cancel" :     "link-type-text action-run action-scope-session action-primary action-cancel button tiny",      /* link for any action that cancels another session action (rare). */
    "link_run_session_cancel_long" :"link-type-text action-run action-scope-session action-primary action-cancel link-long",
    "link_run_session_info" :       "link-type-text action-run action-scope-session action-secondary",                              /* informational session run-action (gives information first, triggers action as secondary purpose) */
    "link_run_session_info_long" :  "link-type-text action-run action-scope-session action-secondary link-long",
    "link_run_local" :              "link-type-text action-run action-scope-local action-primary button tiny",                      /* link for any action (run-action) local to a page or that prepares a page for another action, such as "Clear" or "Reset" buttons that empty a form or form field and interactive javascript forms. */
    "link_run_local_long" :         "link-type-text action-run action-scope-local action-primary link-long",
    "link_run_local_inline" :       "link-type-text action-run action-scope-local action-primary-inline action-inline",                                /* informational local run-action (gives information first, triggers action as secondary purpose) */
    "link_run_local_inline_long" :  "link-type-text action-run action-scope-local action-primary-inline action-inline link-long",
    "link_run_local_cancel" :       "link-type-text action-run action-scope-local action-primary action-cancel button tiny",        /* link for any action that cancels another page-scope action (rare). */
    "link_run_local_cancel_long" :  "link-type-text action-run action-scope-local action-primary action-cancel link-long",
    "link_run_local_info" :         "link-type-text action-run action-scope-local action-secondary",                                /* informational local run-action (gives information first, triggers action as secondary purpose) */
    "link_run_local_info_long" :    "link-type-text action-run action-scope-local action-secondary link-long",

    /* Primary navigation text links (basic navigation and navigation toward actions; note that navigation itself is considered an action)
        NOTE: unlike link_run_xxx, we omit sys/session/local scope from these because "predicting" the scope of an action in a nav link is not really useful in a UI. */
    "link_nav" :                    "link-type-text action-nav action-primary button tiny",                                         /* navigation link toward another page, usually with static text like "New" or "Edit" or "View". the link should also be qualified with an "action_xxx" class where appropriate (see above), to indicate the action that the link is leading the user to do. */
    "link_nav_long" :               "link-type-text action-nav action-primary link-long",                                           /* very long or complex/non-static nav/viewing link: "Categories: All Products Sorted by Name" */
    "link_nav_inline" :             "link-type-text action-nav action-primary-inline action-inline",
    "link_nav_inline_long" :        "link-type-text action-nav action-primary-inline action-inline link-long",
    "link_nav_cancel" :             "link-type-text action-nav action-primary action-cancel button tiny",                           /* back/cancel/done navigation link that leads back to another page (could be said as: "cancels" the nagivation action): "Back", "Cancel", "Done", etc. NOTE: may often appear as if is part of a form submit (run action), but is not really. */
    "link_nav_cancel_long" :        "link-type-text action-nav action-primary action-cancel link-long",
    "link_nav_info" :               "link-type-text action-nav action-secondary",                                                   /* informational navigation text links: link whose main purpose is to show information and is secondarily a navigation link. the types below are specializations of this. */
    "link_nav_info_long" :          "link-type-text action-nav action-secondary link-long",
    /* DEV NOTE (2016-01-19): The categories below have become less important with the new categorizations (the categories above are more important to be followed for good user UI) and because all the categories above support _long versions. However I see no real harm in leaving these in for now (except for consistency concerns)... adds extra configure options.
            They total about 1000 links. If found to be not needed, they could simply be all changed back to simple link_nav_info[_long]. */
    "link_nav_info_id" :            "link-type-text action-nav action-secondary",                                                   /* the short ID or unique code of a record (1-20 chars): "WS10000", "10000", "ORDER_CANCELLED", etc. */
    "link_nav_info_id_long" :       "link-type-text action-nav action-secondary link-long",                                         /* the long ID of a record (more than 20-30 chars), records that do not have single IDs, and IDs with long extraneous words: "WS10000-ITEM10000", "Workspace-Timesheet: TS100000" */
    "link_nav_info_name" :          "link-type-text action-nav action-secondary",                                                   /* the name of a record: "My Order 23", "Some Value", "Cancelled", etc. */
    "link_nav_info_name_long" :     "link-type-text action-nav action-secondary link-long",                                         /* the long name of a record: "Mr. Title The Ambassador of Germany", etc. */
    "link_nav_info_idname" :        "link-type-text action-nav action-secondary",                                                   /* the name and id of a record: "My Order 23 (WS10000)", "WS10000 (My Order 23)" etc. */
    "link_nav_info_idname_long" :   "link-type-text action-nav action-secondary link-long",                                         /* long combination of IDs and names: "Mr. John Alberton Smith Junior (ID: 10000) (Group: 20000)" */
    "link_nav_info_desc" :          "link-type-text action-nav action-secondary",                                                   /* the description of a record: "Order that was placed by admin", "This is some value", "This means order cancelled", etc. In general, as soon as a link text contains more than one type of value, and not idname, it should be changed to link_nav_info_desc. */
    "link_nav_info_date" :          "link-type-text action-nav action-secondary",                                                   /* the date of a record (fromDate, thruDate, etc.) */
    "link_nav_info_number" :        "link-type-text action-nav action-secondary",                                                   /* the number of a record (index, sequence num, etc.) */
    "link_nav_info_value" :         "link-type-text action-nav action-secondary",                                                   /* link containing a value of type not previously listed (or cannot be known statically) */
    "link_nav_info_value_long" :    "link-type-text action-nav action-secondary link-long",                                         /* link containing a value of type not previously listed but that may be long (or cannot be known statically) */
    "link_nav_info_uri" :           "link-type-text action-nav action-secondary",                                                   /* link containing a URL, path or other location as its text ({{{<a href="http://ofbiz.apache.org">http://ofbiz.apache.org</a>}}}); may be IP, hostname, etc. */
    "link_nav_info_text" :          "link-type-text action-nav action-secondary",                                                   /* link containing any kind of free-form text */
    
  /* Colors */
    /* DEV NOTE: is it possible that color_ should always be avoided or even removed in favor of xxxx_color_xxx (further below)? or renamed?
        color_xxx class on its own does nothing, which is a little confusing, e.g. you might think setting these styles on a span will give the text a color, but it won't... */
    "color_green" : "success",
    "color_red" : "alert",
    "color_grey" : "secondary",
    "color_primary" : "primary",
    "color_secondary" : "secondary",
    "color_info" : "info",
    "color_warning" : "warning",
    "color_success" : "success",
    "color_alert" : "alert",        /* Bootstrap: "danger" */
    "color_error" : "alert",
    
    "button_color_default" : "",
    "button_color_red" : "alert",
    "button_color_grey" : "secondary",
    "button_color_green" : "success",
    "button_color_primary" : "primary",
    "button_color_secondary" : "secondary",
    "button_color_success" : "success",
    "button_color_info" : "info",
    "button_color_warning" : "warning",
    "button_color_alert" : "alert",
    "button_color_error" : "alert",
    
    /* text color. NOTE: this does not provide the best styling abstraction, but at least better than hardcoding */
    "text_color_primary" : "colored-text primary",
    "text_color_secondary" : "colored-text secondary",
    "text_color_info" : "colored-text info",
    "text_color_warning" : "colored-text warning",
    "text_color_success" : "colored-text success",
    "text_color_alert" : "colored-text alert",
    "text_color_error" : "colored-text alert",
    
    /* colors for any other element that supports coloring ("color" CSS attribute) */
    "elem_color_primary" : "colored-elem primary",
    "elem_color_secondary" : "colored-elem secondary",
    "elem_color_info" : "colored-elem info",
    "elem_color_warning" : "colored-elem warning",
    "elem_color_success" : "colored-elem success",
    "elem_color_alert" : "colored-elem alert",
    "elem_color_error" : "colored-elem alert",
    
  /* Grid */
    "grid_section" : "",
    "grid_row" : "row",
    "grid_cell" : "columns",
    "grid_cell_default" : "large-12",
    "grid_display" : "display",      /* display grid */
    "grid_theme_pre" : "row fullWidth", /*pre-content-section*/
    "grid_theme" : "row fullWidth", /*content-section*/
    "grid_end" : "end",
    "grid_small" : "small-",
    "grid_medium" : "medium-",
    "grid_large" : "large-",       /* Bootstrap: "col-lg-" */
    "grid_offset" : "offset-",      /* Bootstrap: "col-md-offset-" */
    "grid_small_offset" : "small-offset-",
    "grid_medium_offset" : "medium-offset-",
    "grid_large_offset" : "large-offset-",
    "grid_block_container" : "",
    "grid_block_wrap" : "",
    "grid_block_prefix" : "",
    "grid_block_postfix" : "block-grid-",
    "grid_postfix_container" : "",
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
    "grid_centered" : "centered",
    "grid_small_centered" : "small-centered",
    "grid_medium_centered" : "medium-centered",
    "grid_large_centered" : "large-centered",
    "grid_sidebar_0_main" : "large-12 columns",
    "grid_sidebar_1_main" : "large-10 medium-9 columns",
    "grid_sidebar_1_side" : "large-2 medium-3 columns",
    "grid_sidebar_2_main" : "large-8 medium-6 columns",
    "grid_sidebar_2_side" : "large-2 medium-3 columns",

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
    "nav_sidenav" : "navbar-inverse collapse navbar-collapse navbar-ex1-collapse ",
    
    /* Breadcrumbs */
    "nav_breadcrumbs" : "breadcrumbs",                  /* breadcrumb container */
    "nav_breadcrumb" : "",                              /* breadcrumb entry */
    /* NOTE: currently the active/disabled styles are added in addition to nav_breadcrumb, not replacing them */
    "nav_breadcrumb_disabled" : "unavailable",
    "nav_breadcrumb_active" : "current",
    "nav_breadcrumb_link" : "",                         /* breadcrumb link */

    /* Steps */
    "nav_steps" : "steps",                        /* steps container */
    "nav_step" : "",                              /* step entry */
    "nav_step_disabled" : "disabled",
    "nav_step_active" : "active",
    "nav_step_completed" : "fa fa-check completed",
    "nav_step_expanded" : "expanded",
    "nav_step_icon_completed" : "",
  
  /* Pagination */
    "pagination_wrap" : "pagination-centered",
    "pagination_list" : "pagination",
    "pagination_control" : "",
    "pagination_item" : "",
    "pagination_item_first" : "nav-first",
    "pagination_item_last" : "nav-last",
    "pagination_item_previous" : "nav-previous",
    "pagination_item_next" : "nav-next",
    "pagination_item_active" : "current",
    "pagination_item_disabled" : "unavailable",
    "pagination_layout" : "bottom",
    "pagination_noresultsmode" : "hide",
    "pagination_showcount" : true,           /* show count by default */
    "pagination_alwaysshowcount" : true,     /* show count even if no pagination controls */
    "pagination_countmsglabel" : "CommonDisplayingShort",     /* default count message property; must be in CommonUiLabels.xml */
    "pagination_lowcountmsglabel" : "CommonDisplayingShort",  /* default count message property for low counts; must be in CommonUiLabels.xml */
    
  /* Lists */
    "list_inline" : "inline-list",

  /* Tile */
    /* tiles-type-based args and styles
         as always, default is also used for individual value fallbacks when missing from specific types.
         NOTE: currently most of these values are macro args, which may not be straight stylenames (they get mapped to individual styles further below)
            see @tile macro interface. it's perfectly possible to ignore them and delegate styling to CSS by addressing the right classes.
            for example, the *bgcolor can be set to the value "none" to prevent setting a color class.
         DEV NOTE: the bg colors may seem like overkill, but they avoid redundancy in the CSS maybe? whatever, more configurable this way */
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
    "tile_generic_class" : "tile-generic",  /* NOTE: NO tile-common here */
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
    
  /* Image galleries (non-tile settings - tile settings contained in tile styles) */
    "gallery_share_view_width" : 500,
    "gallery_share_view_height" : 500,

  /* Panels */
    "panel_wrap" : "panel",
    "panel_head" : "panel-head",
    "panel_title" : "panel-title",
    "panel_body" : "panel-body",
    "login_wrap" : "large-3 login-box",                  /* the login box is a special form of a panel in this case */
    "login_header": "",
    "login_body" : "signup-panel right-solid",

  /* Alert Box */
    "alert_wrap" : "alert-box",    /* Bootstrap: "alert" */
    "alert_prefix_type" : "",             /* Bootstrap: "alert-"; NOTE: This may be removed */
    "alert_type_info" : "info",
    "alert_type_success" : "success",
    "alert_type_warning" : "warning",
    "alert_type_secondary" : "secondary",
    "alert_type_alert" : "alert",
    "alert_type_error" : "alert",
    
    /* Alert options by type */
    "alert_default_closable" : true,

  /* Pricing Tables */
    "pricing_wrap" : "pricing-table",
    "pricing_price" : "price",
    "pricing_description" : "description",
    "pricing_ribbon" : "ribbon",
    "pricing_title" : "title",
    "pricing_cta" : "cta-button",
    "pricing_bullet" : "bullet-item",
    
    /* Tabs */
    "tabs_container" : "",
    "tabs_wrap" : "tabs",                           /* Bootstrap: "nav nav-tabs" */
    "tabs_item_title" : "tab-title",                /* Bootstrap: "nav-item" */
    "tabs_item_title_link" : "",                    /* Bootstrap: "nav-link" */
    "tabs_item_title_link_active" : "",
    "tabs_item_title_active" : "active",
    "tabs_item_title_disabled" : "",
    "tabs_item_container": "tabs-content",          /* Bootstrap: "tab-content" */
    "tabs_item_wrap" : "content",                   /* Bootstrap: "tab-pane" */
    "tabs_item_active" : "active",
    "tabs_item_disabled" : "disabled",

  /* Modal */
    "modal_wrap" : "reveal-modal",
    
  /* Slider */
    "slider_container" : "example-orbit-content",   /* Bootstrap: "carousel slide" */
    "slider_wrap" : "",                             /* Bootstrap: "carousel-inner" */
    "slide_container" : "",
    "slide_image" : "",
    "slide_content" : "orbit-caption",              /* Bootstrap: "carousel-caption" */
    "slide_linktarget" : "_blank",

  /* Chart */
    "chart_legend" : "chart-legend",

  /* Progress Bar */
    "progress_container" : "progress",   /* Bootstrap: "progress" */
    "progress_wrap" : "",           /* Bootstrap: "progress-bar" */
    "progress_bar" : "meter",       /* Bootstrap: "sr-only" */

    "progress_state_alert" : "alert",
    "progress_state_info" : "info",
    "progress_state_success" : "success",

  /* Fields
       fields_xxx classes and settings are looked up by the @fields and @field macros as defaults for
       fields type and field type. */
    /* fields type-based styles
        NOTE: fields_default_xxx is also used as a fallback for the more specialized type for any
            missing individual definitions (same as menu and table defs)
            the other ones simply override them.
        NOTE: Some settings can be specified using explicitly-coded entries (fields_default_xxx) OR
            using the fieldargs map (fields_default_fieldargs.xxx). In general, the fieldargs
            settings are stronger than the explicitly-coded ones and will override everything but
            parameters manually specified on individual @field calls. The explicitly-coded ones are
            treated more like defaults and are more likely to get overridden themselves.
    */
    /* default styles for most forms */
    "fields_default_labeltype" : "horizontal",
    "fields_default_labelposition" : "left",
    "fields_default_labelarea" : true,
    "fields_default_labelareaexceptions" : "submit submitarea",
    "fields_default_labelarearequirecontent" : false,
    "fields_default_labelareaconsumeexceptions" : "",
    "fields_default_collapse" : false,
    "fields_default_collapsepostfix" : true,
    "fields_default_collapsedinlinelabel" : "datetime", /* Can be true, false, or list of type names */
    "fields_default_checkboxtype" : "",
    "fields_default_radiotype" : "",
    "fields_default_inlineitems" : "",                  /* Set this to boolean true or false to change global inline items default for radio and checkbox */
    "fields_default_fieldargs" : [                      /* This can be set to a map of new default args to use for @field calls. Can overlap with the other settings above. See @fields macro. NOTE: This is more forced than the settings implemented individually, but can often work well enough. */
        "requiredClass" : "required",
        "requiredTooltip" : "#LABEL:CommonRequired",
        "invertedClass" : "field-inverted",
        "standardClass" : "field-standard"
    ],
    "fields_default_totalcolumns" : 12,                 /* Logical field total column span. Doesn't have to be 12. NOTE: This could also be passed via fieldargs.totalColumns, but this also covers cases where @fields does not apply (calls to getDefaultFieldGridStyles_defaultArgs). These also give more control. */
    "fields_default_labelcolumns" : 2,
    "fields_default_postfixcolumns" : 1,
    "fields_default_labelsmallcoldiff" : 1,
    "fields_default_widgetpostfixcombined" : true,      /* NOTE: This is GENERAL default, or fallback default, because the actual flag may be more complex to determine. */
    
    /* form widgets (SPECIAL settings; these do not fully support all settings of the other types) */
    "fields_formwidget_totalcolumns" : 11,               /* FIXME?: Form widget field spans were made smaller (11) as a workaround for visual ugliness in forms with positions */
    
    /* alternate to default with different top-level checkbox styles */
    "fields_default_alt1_labelareaconsumeexceptions" : "checkbox-single radio-single",
    "fields_default_alt1_checkboxtype" : "simple",

    /* no left label area (usually for very short forms) */
    "fields_default_nolabelarea_labeltype" : "horizontal", /* NOTE: this should be "none" in principle, but this is used to implement collapsedinlinelabel. labelarea false is good enough. */
    "fields_default_nolabelarea_labelposition" : "left",
    "fields_default_nolabelarea_labelarea" : false,
    "fields_default_nolabelarea_labelareaexceptions" : "",
    "fields_default_nolabelarea_labelarearequirecontent" : false,

    /* limited space; labels on top */
    "fields_default_compact_labeltype" : "vertical",
    "fields_default_compact_labelposition" : "top",
    "fields_default_compact_labelarea" : true,
    "fields_default_compact_labelareaexceptions" : "checkbox-single radio-single", /* having label on top of checkbox or radio is really ugly; let the label inline itself for these */
    "fields_default_compact_labelarearequirecontent" : true,

    /* free-form/manual layout - NO imposed layout (but may have some low-level field styling) */
    "fields_default_manual_labeltype" : "horizontal",
    "fields_default_manual_labelposition" : "left",
    "fields_default_manual_labelarea" : false,
    "fields_default_manual_labelareaexceptions" : "",
    "fields_default_manual_labelarearequirecontent" : false,
    "fields_default_manual_labelareaconsumeexceptions" : "",
    "fields_default_manual_collapse" : false,
    "fields_default_manual_collapsepostfix" : true,
    "fields_default_manual_collapsedinlinelabel" : "", /* can be true, false, or list of type names */
    "fields_default_manual_checkboxtype" : "",

    /* free-form/manual widget-only layout - same as default-manual but all fields have no containers by default  */
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
        "widgetOnly" : "true",
        "asdfasdfasdf" : "asdfasdfasdf"
    ],

    /* completely generic - NO imposed styles - keep styles out of this */
    "fields_generic_labeltype" : "horizontal",
    "fields_generic_labelposition" : "left",
    "fields_generic_labelarea" : false,
    "fields_generic_labelareaexceptions" : "",
    "fields_generic_labelarearequirecontent" : false,
    "fields_generic_labelareaconsumeexceptions" : "",
    "fields_generic_collapse" : false,
    "fields_generic_collapsepostfix" : true,
    "fields_generic_collapsedinlinelabel" : "", /* can be true, false, or list of type names */
    "fields_generic_checkboxtype" : "",

    /* misc fields styles */
    "fields_wrap" : "collapse",
    "fields_label" : "prefix",
    
    /* generic field styles*/
    "field_input_container_default" : "", 
    "field_input_default" : "",
    "field_textarea_default" : "",
    "field_datetime_default" : "",
    "field_datefind_default" : "",
    "field_datefind_select_default" : "",
    "field_select_default" : "",
    "field_lookup_default" : "",
    "field_file_default" : "",
    "field_password_default" : "",
    "field_reset_default" : "",
    "field_submit_default" : "",
    "field_textfind_default" : "",
    "field_textfind_select_default" : "",
    "field_rangefind_default" : "",
    "field_rangefind_select_default" : "",
    "field_display_default" : "",
    "field_generic_default" : "",
    
    /* checkbox type styles */
    "field_checkbox_default" : "switch small checkbox-type-switch",         /* default checkbox styles */
    "field_checkbox_default_type" : "checkbox-type-switch",                 /* normally already included in previous, but can reference individually with this */
    "field_checkbox_default_labeltype" : "extralabel",                      /* FIXME? this should really be "standard" and the CSS made to cooperate with "standard" mode - see checkbox markup macro for issues */
    "field_checkbox_default_labelposition" : "after",
    "field_checkbox_simple" : "checkbox-type-simple",                       /* default simple checkbox styles */
    "field_checkbox_simple_type" : "checkbox-type-simple",
    "field_checkbox_simple_labeltype" : "spanonly",                         /* FIXME? this should be "standard", but because "extralabel" is implemented oddly, currently need "spanonly" to make label look the same */
    "field_checkbox_simple_labelposition" : "after",
    "field_checkbox_simple_standard" : "checkbox-type-simple-standard",    /* explicitly standard, simple checkbox styles */
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
    "field_datetime_default_title" : "LABEL+FORMAT",    /* can be a property label from CommonUiLabels; prefix with "#PROP:". Can also add colon and resource afterward. Also supports these predefined: LABEL, FORMAT, LABEL+FORMAT */
    
    /* display type styles */
    "field_display_tooltip" : "has-tip field-tooltip-wrap",
    
    /* submit type styles */
    "field_submit_default_text" : "#PROP:CommonSubmit",   /* can be a property label from CommonUiLabels; prefix with "#PROP:". */
    
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
    "field_inline" : "field-inline",                    /* a field (usually input) that has to inline with other inputs or inline html elements */
    "field_tooltip_wrap" : "field-tooltip-wrap",  /* for some @field types, an extra container is added to support tooltips; it receives this style */
    "field_inverted" : "field-inverted",                /* style added to outer container for inverted fields NOTE: duplicated in fields_default_fieldargs */
    "field_standard" : "field-standard",                /* style added to outer container for non-inverted fields NOTE: duplicated in fields_default_fieldargs */
    "field_extra" : "field-extra",                      /* extra field (non-essential) - relative term */
    
    /* Field type style name maps
        These map scipio and ofbiz field types to style names to represent the fields in CSS, so they can be unified.
        For any entries not specified, "default" entry is consulted.
        The boolean true means use the scipio or ofbiz type name as style name.
        Note these names are not full CSS style names, but prefixes or suffixes. */
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

    /*List of icons that can be assigned per webapplication and specific menu_items. Uses font-awesome icons by default.*/
    "app_icon" : [
        "main" : "fa-home", // Dashboard link
        "admin" : "fa-wrench",
        "accounting" : "fa-balance-scale",
        "assetmaint" : "fa-cogs",
        "ofbizsetup" : "fa-check-square",
        "cms" : "fa-file-text",
        "CRM" : "fa-bullhorn",
        "humanres" : "fa-vcard",
        "manufacturing" : "fa-industry",
        "facility" : "fa-cube",
        "marketing" : "fa-comments",
        "order" : "fa-recycle",
        "party" : "fa-users",
        "catalog" : "fa-sitemap",
        "setup" : "fa-cog",
        "shop" : "fa-shopping-cart",
        "solr" : "fa-search",
        "workeffort" : "fa-sliders"
        ],

  /* Always declare last */
    "dummy" : ""
];


/*
*************************************
* OTHER TEMPLATING GLOBALS *
*************************************
*/

/* (currently none) */


 








