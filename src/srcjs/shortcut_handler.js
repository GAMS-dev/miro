/* global $:false Shiny:false */
import {
  rerenderHot,
} from './util';

export default class ShortcutHandler {
  constructor(maxTabsets) {
    this.maxTabsets = maxTabsets;
    this.refreshActiveSection();
  }

  static getActiveSection() {
    const sidebarEl = document.getElementById('miroSidebar');
    if (sidebarEl == null || sidebarEl.dataset == null) {
      return null;
    }
    return sidebarEl.dataset.value;
  }

  refreshActiveSection() {
    const newActiveSection = this.constructor.getActiveSection();
    if (this.activeSection !== newActiveSection) {
      this.nestLevel = 0;
      this.activeSection = newActiveSection;
    }
  }

  incrementNestLevel(increment) {
    this.refreshActiveSection();
    const newNestLevel = this.nestLevel + increment;
    if (newNestLevel <= 0) {
      this.nestLevel = 0;
    } else if (increment >= 100) {
      this.nestLevel = 100;
    } else {
      this.nestLevel = newNestLevel;
    }
  }

  incrementTab(increment) {
    this.refreshActiveSection();
    if ($('#scen-split-view').is(':visible')) {
      this.incrementTabInternal($('#scenSplit1_content .shiny-tab-input:visible'), increment);
      this.incrementTabInternal($('#scenSplit2_content .shiny-tab-input:visible'), increment);
      return;
    }
    this.incrementTabInternal($('.shiny-tab-input:visible'), increment);
  }

  incrementTabInternal(visibleTabsets, increment) {
    if (visibleTabsets.length === 0) {
      return;
    }
    if (this.nestLevel >= visibleTabsets.length) {
      this.nestLevel = visibleTabsets.length - 1;
    }
    const tabset = visibleTabsets.eq(this.nestLevel).find('li:not(.dropdown).active');
    if (increment === 1) {
      let nextTab = tabset.next('li');
      if (nextTab.hasClass('dropdown')) {
        nextTab = nextTab.find('li:not(.dropdown)').first();
      }
      nextTab
        .find('a')
        .trigger('click');
    } else if (increment === -1) {
      let prevTab = tabset.prev('li:not(.dropdown)');
      if (prevTab.length === 0) {
        const activeDropdownTab = tabset.closest('.dropdown.active');
        if (activeDropdownTab.length > 0) {
          // is first element in dropdown tab
          prevTab = activeDropdownTab.prev('li:not(.dropdown)');
        }
      }
      prevTab
        .find('a')
        .trigger('click');
    }
  }

  applyShortcut(event) {
    if (event.code === 'Enter' && !event.ctrlKey) {
      if ($('#shiny-modal').find('.selectize-input.input-active').length > 0
        || $('#shiny-modal').find('*[data-dismiss="modal"]').is(':focus')) {
        return;
      }

      $('.bt-gms-confirm:visible:enabled').trigger('click');
      return;
    } // ENTER will confirm modal dialogs

    if (!event.ctrlKey || !event.altKey) {
      return;
    }

    if (event.code === 'KeyI') {
      if ($('#btImport').is(':visible')) {
        $('#btImport').trigger('click');
      } else if ($('#btLoadScen').is(':visible')) {
        $('#btLoadScen').trigger('click');
      } else if ($('#scen-pivot-view').is(':visible')) {
        if ($('#pivotCompBtWrapper').is(':visible')) {
          $('#pivotCompBtWrapper').trigger('click');
        }
      }
      return;
    } // Import shortcut: CTRL + ALT + I

    if (event.code === 'KeyS') {
      Shiny.setInputValue('btSave', 1, {
        priority: 'event',
      });
      return;
    } // SAVE shortcut: CTRL + ALT + S

    if (event.code === 'Enter') {
      $('#btSolve:visible:enabled').trigger('click');
      return;
    } // Solve shortcut: CTRL + ALT + ENTER

    if (event.code === 'KeyR') {
      Shiny.setInputValue('btDelete', 1, {
        priority: 'event',
      });
      return;
    } // Remove shortcut: CTRL + ALT + R

    if (event.code === 'KeyC') {
      if ($('.btRemove').is(':visible')) {
        $('.btRemove:enabled').trigger('click');
        return;
      }
      if ($('#btScenSplit1_close').is(':visible')) {
        $('#btScenSplit1_close:enabled').trigger('click');
        $('#btScenSplit2_close:enabled').trigger('click');
        return;
      }
      if ($('#scenTabset .active .bt-cmp-close-scen').is(':visible')) {
        $('#scenTabset .active .bt-cmp-close-scen').trigger('click');
        return;
      }
      if ($('#btScenPivot_close').is(':visible')) {
        $('#btScenPivot_close:enabled').trigger('click');
        return;
      }
      return;
    } // Close shortcut (remove button in input sheet): CTRL + ALT + C

    if (event.code === 'KeyF') {
      $('body').toggleClass('sidebar-collapse');
      rerenderHot(400);
      return;
    } // Fullscreen mode (hide sidebar) shortcut: CTRL + ALT + F

    if (event.code === 'Digit1') {
      $('a[href="#shiny-tab-inputData"]').trigger('click');
      return;
    } // Select input menu shortcut: CTRL + ALT + 1

    if (event.code === 'Digit2') {
      $('a[href="#shiny-tab-outputData"]').trigger('click');
      return;
    } // Select output menu shortcut: CTRL + ALT + 2

    if (event.code === 'Digit3') {
      $('a[href="#shiny-tab-gamsinter"]').trigger('click');
      return;
    } // Select gams interaction menu shortcut: CTRL + ALT + 3

    if (event.code === 'Digit4') {
      $('a[href="#shiny-tab-loadResults"]').trigger('click');
      return;
    }// Select scenario menu shortcut: CTRL + ALT + 4

    if (event.code === 'Digit5') {
      $('a[href="#shiny-tab-scenarios"]').trigger('click');
      return;
    }// Select scenario menu shortcut: CTRL + ALT + 5

    if (event.code === 'KeyT') {
      if ($('#btGraphIn').is(':visible')) {
        $('#btGraphIn:enabled').trigger('click');
        return;
      }

      if ($('#outputTableView').is(':visible')) {
        $('#outputTableView').trigger('click');
        return;
      }

      for (let i = 2; i <= 50 + 3; i += 1) {
        $(`#btScenTableView${i}:visible`).trigger('click');
      }
      return;
    } // Table view (scenario compare mode) shortcut: CTRL + ALT + T

    if (event.code === 'ArrowRight') {
      this.incrementTab(1);
      return;
    } // Select next tab shortcut: CTRL + ALT + arrow right

    if (event.code === 'ArrowLeft') {
      this.incrementTab(-1);
      return;
    } // Select previous tab shortcut: CTRL + ALT + arrow left

    if (event.code === 'ArrowDown') {
      this.incrementNestLevel(1);
      return;
    } // Nest to next lower tabset shortcut: CTRL + ALT + arrow down

    if (event.code === 'ArrowUp') {
      this.incrementNestLevel(-1);
      return;
    } // Unnest to next higher tabset shortcut: CTRL + ALT + arrow up

    if (event.code === 'Space' && $('#btCompareScen').is(':enabled') && $('#btCompareScen').is(':visible')) {
      $('#btCompareScen').trigger('click');
    }// Activate/deactivate scenario comparison mode: CTRL + ALT + space
  }
}
