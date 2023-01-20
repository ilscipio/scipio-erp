const $SIDEBAR_COOKIE = 'scpSidebar';
var $SIDEBAR_CLOSED=false;

document.addEventListener('DOMContentLoaded', () => {
    /**
     * Modal
     * */
    // Functions to open and close a modal
    function openModal($el) {
        $el.classList.add('is-active');
    }

    function closeModal($el) {
        $el.classList.remove('is-active');
    }

    function closeAllModals() {
        (document.querySelectorAll('.modal') || []).forEach(($modal) => {
            closeModal($modal);
        });
    }

    // Add a click event on buttons to open a specific modal
    (document.querySelectorAll('.js-modal-trigger') || []).forEach(($trigger) => {
        const modal = $trigger.dataset.target;
        const $target = document.getElementById(modal);

        $trigger.addEventListener('click', () => {
            openModal($target);
        });
    });

    // Add a click event on various child elements to close the parent modal
    (document.querySelectorAll('.modal-background, .modal-close, .modal-card-head .delete, .modal-card-foot .button') || []).forEach(($close) => {
        const $target = $close.closest('.modal');

        $close.addEventListener('click', () => {
            closeModal($target);
        });
    });

    // Add a keyboard event to close all modals
    document.addEventListener('keydown', (event) => {
        const e = event || window.event;

        if (e.keyCode === 27) { // Escape key
            closeAllModals();
        }
    });


    /**
     * Burger
     * */
    const $CLASS_ACTIVE = 'is-active';
    const $navbarBurgers = Array.prototype.slice.call(document.querySelectorAll('.navbar-burger,.navbar-apps'), 0);
    $navbarBurgers.forEach( el => {
        el.addEventListener('click', () => {

            // Get the target from the "data-target" attribute
            const target = el.dataset.target;
            const $target = document.getElementById(target);

            // Toggle the "is-active" class on both the "navbar-burger" and the "navbar-menu"
            el.classList.toggle($CLASS_ACTIVE);
            $target.classList.toggle($CLASS_ACTIVE);

        });
    });

    /**
     * Sidebar toggle
     * */
    const $sidebarToggle = Array.prototype.slice.call(document.querySelectorAll('.sidebar-toggle'), 0);
    $sidebarToggle.forEach( el => {
        const target = el.dataset.target;
        const $target = document.getElementById(target);
        el.addEventListener('click', () => {
            el.classList.toggle('is-active');
            $target.classList.toggle('is-active')
            $SIDEBAR_CLOSED=$target.classList.contains('is-active');
            setCookie($SIDEBAR_COOKIE,$SIDEBAR_CLOSED,90);
        });
    });

    window.addEventListener("resize", function() {
        $sidebarToggle.forEach( el => {
            const target = el.dataset.target;
            const $target = document.getElementById(target);
            $SIDEBAR_CLOSED=$target.classList.contains('is-active');

            if (document.body.clientWidth > 1023)  {
                if(!$SIDEBAR_CLOSED && $target.classList.contains('is-active')){
                    $target.classList.add('is-active')
                    el.classList.add('is-active');
                }
            } else {
                if($SIDEBAR_CLOSED){
                    $target.classList.remove('is-active')
                    el.classList.remove('is-active');
                }
            }
        })

    });

    //sidebar hover
    const $sidebarMenus = (document.querySelectorAll('aside .menu-list') || []);
    $sidebarMenus.forEach( el => {
        if(el.id !== 'menu_logo'){
            el.addEventListener('mouseover', () => {
                if (!el.classList.contains("is-hovered")) {
                    el.classList.add("is-hovered");
                }
                let pel = el.closest('aside');
                if (!pel.classList.contains("is-hovered")) {
                    pel.classList.add("is-hovered");
                }
            });

            el.addEventListener('mouseout', () => {
                if (el.classList.contains("is-hovered")) {
                    el.classList.remove("is-hovered");
                }
                let pel = el.closest('aside');
                if (pel.classList.contains("is-hovered")) {
                    pel.classList.remove("is-hovered");
                }
            });
        }
    });


    /**
     * Tab
     * */

    let tabsWithContent = function () {
        let tabs = document.querySelectorAll('.tabs li');
        let tabsContent = document.querySelectorAll('.tab-content');

        let deactvateAllTabs = function () {
            tabs.forEach(function (tab) {
                tab.classList.remove('is-active');
            });
        };

        let hideTabsContent = function () {
            tabsContent.forEach(function (tabContent) {
                tabContent.classList.remove('is-active');
            });
        };

        let activateTabsContent = function (tab) {
            tabsContent[getIndex(tab)].classList.add('is-active');
        };

        let getIndex = function (el) {
            return [...el.parentElement.children].indexOf(el);
        };

        tabs.forEach(function (tab) {
            tab.addEventListener('click', function () {
                deactvateAllTabs();
                hideTabsContent();
                tab.classList.add('is-active');
                activateTabsContent(tab);
            });
        })

        if(tabs[0] !== undefined){
            tabs[0].click();
        }
    };

    tabsWithContent();


    /**
     * Notifications
     * */
    (document.querySelectorAll('.notification .delete') || []).forEach(($delete) => {
        const $notification = $delete.parentNode;

        $delete.addEventListener('click', () => {
            $notification.parentNode.removeChild($notification);
        });
    });


    /**
     * Notifications
     * */
    (document.querySelectorAll('.fieldgroup-body') || []).forEach(($fieldgrpbody) => {
        const $parent = $fieldgrpbody.parentNode;
        const $legends = Array.prototype.slice.call($parent.querySelectorAll('legend'),0);
        if($legends.length>0){
            const $legend = $legends[0];
            const $icon = $legend.querySelector("i");
            console.log($icon);
            $legend.addEventListener('click', () => {
                $fieldgrpbody.style.display = $fieldgrpbody.style.display === 'none' ? '' : 'none';
                $fieldgrpbody.classList.toggle("is-open");
                $icon.classList.toggle("fa-arrow-right");
                $icon.classList.toggle("fa-arrow-down");
            });
        }
    });
});

/**
 * Sidebar
 * */
function openSidebarTab(id){
    var sidebarToggle = Array.prototype.slice.call(document.querySelectorAll('aside .menu-list'), 0);
    sidebarToggle.forEach( em => {
        em.classList.remove('is-open');
    } );
    document.getElementById(id).classList.add("is-open");
}


function setCookie(name,value,days) {
    var expires = "";
    if (days) {
        var date = new Date();
        date.setTime(date.getTime() + (days*24*60*60*1000));
        expires = "; expires=" + date.toUTCString();
    }
    document.cookie = name + "=" + (value || "")  + expires + "; path=/";
}