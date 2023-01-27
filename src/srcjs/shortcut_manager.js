/* global $:false Shiny:false */
import {
  rerenderHot,
} from './util';

export default class ShortcutManager {
  constructor(maxTabsets) {
    this.maxTabsets = maxTabsets;
    this.refreshActiveSection();
    if (navigator.platform.toUpperCase().includes('MAC')) {
      this.bindKeyText = '⌃ + ⌥';
    } else {
      this.bindKeyText = 'Ctrl + Alt';
    }
    this.shortcuts = {
      KeyI: {
        description: 'Import data',
        context: () => $('.modal.in').length === 0,
        handler: () => {
          if ($('#btImport').is(':visible')) {
            $('#btImport').trigger('click');
          } else if ($('#btLoadScen').is(':visible')) {
            $('#btLoadScen').trigger('click');
          } else if ($('#scen-pivot-view').is(':visible')) {
            if ($('#pivotCompBtWrapper').is(':visible')) {
              $('#pivotCompBtWrapper').trigger('click');
            }
          }
        },
      },
      KeyS: {
        description: 'Save sandbox scenario',
        context: () => $('.modal.in').length === 0,
        handler: () => {
          Shiny.setInputValue('btSave', 1, {
            priority: 'event',
          });
        },
      },
      Enter: {
        description: 'Solve sandbox scenario',
        handler: () => {
          $('#btSolve:visible:enabled').trigger('click');
        },
      },
      KeyR: {
        description: 'Delete sandbox scenario',
        context: () => $('.modal.in').length === 0,
        handler: () => {
          Shiny.setInputValue('btDelete', 1, {
            priority: 'event',
          });
        },
      },
      KeyC: {
        description: 'Close scenario',
        context: () => $('.modal.in').length === 0,
        handler: () => {
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
          }
        },
      },
      KeyF: {
        description: 'Fullscreen (collapse sidebar)',
        handler: () => {
          $('body').toggleClass('sidebar-collapse');
          rerenderHot(400);
        },
      },
      Digit1: {
        description: 'Go to input data section',
        handler: () => {
          $('a[href="#shiny-tab-inputData"]').trigger('click');
        },
      },
      Digit2: {
        description: 'Go to output data section',
        handler: () => {
          $('a[href="#shiny-tab-outputData"]').trigger('click');
        },
      },
      Digit3: {
        description: 'Go to GAMS interaction section',
        handler: () => {
          $('a[href="#shiny-tab-gamsinter"]').trigger('click');
        },
      },
      Digit4: {
        description: 'Go to load scenarios section',
        handler: () => {
          $('a[href="#shiny-tab-loadResults"]').trigger('click');
        },
      },
      Digit5: {
        description: 'Go to scenario comparison section',
        handler: () => {
          $('a[href="#shiny-tab-scenarios"]').trigger('click');
        },
      },
      KeyJ: {
        description: 'Go to split view comparison mode',
        handler: () => {
          $('a[href="#shiny-tab-scenarios"]').trigger('click');
          Shiny.setInputValue('btSplitView', 'splitView', { priority: 'event' });
        },
      },
      KeyK: {
        description: 'Go to tab view comparison mode',
        handler: () => {
          $('a[href="#shiny-tab-scenarios"]').trigger('click');
          Shiny.setInputValue('btSplitView', 'tabView', { priority: 'event' });
        },
      },
      KeyL: {
        description: 'Go to pivot view comparison mode',
        handler: () => {
          $('a[href="#shiny-tab-scenarios"]').trigger('click');
          Shiny.setInputValue('btSplitView', 'pivotView', { priority: 'event' });
        },
      },
      KeyT: {
        description: 'Toggle graph/table view',
        handler: () => {
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
        },
      },
      KeyD: {
        description: 'Remove duplicates',
        context: () => $('.modal.in').length === 0,
        handler: () => {
          Shiny.setInputValue('btRemoveDuplicates', 1, {
            priority: 'event',
          });
        },
      },
      KeyM: {
        description: 'Edit metadata',
        context: () => $('.modal.in').length === 0,
        handler: () => {
          Shiny.setInputValue('btEditMeta', 1, {
            priority: 'event',
          });
        },
      },
      KeyA: {
        description: 'Edit attachments',
        context: () => $('.modal.in').length === 0,
        handler: () => {
          Shiny.setInputValue('btEditMeta', 'attachments', {
            priority: 'event',
          });
        },
      },
      KeyV: {
        description: 'Edit views',
        context: () => $('.modal.in').length === 0,
        handler: () => {
          Shiny.setInputValue('btEditMeta', 'views', {
            priority: 'event',
          });
        },
      },
      ArrowRight: {
        description: 'Go to next tab',
        handler: () => {
          this.incrementTab(1);
        },
      },
      ArrowLeft: {
        description: 'Go to previous tab',
        handler: () => {
          this.incrementTab(-1);
        },
      },
      ArrowDown: {
        description: 'Go down one tab level',
        handler: () => {
          this.incrementNestLevel(1);
        },
      },
      ArrowUp: {
        description: 'Go up one tab level',
        handler: () => {
          this.incrementNestLevel(-1);
        },
      },
      KeyY: {
        description: 'Toggle synchronize tabs',
        context: () => $('#btCompareScen').is(':enabled') && $('#btCompareScen').is(':visible'),
        handler: () => {
          $('#btCompareScen').trigger('click');
        },
      },
    };
  }

  static getActiveSection() {
    const sidebarEl = document.getElementById('miroSidebar');
    if (sidebarEl == null || sidebarEl.dataset == null) {
      return null;
    }
    return sidebarEl.dataset.value;
  }

  static getKeyNameFromCode(keyCode) {
    if (keyCode == null) {
      return '';
    }
    if (keyCode.startsWith('Key')) {
      return keyCode.substring(3);
    }
    if (keyCode.startsWith('Digit')) {
      return keyCode.substring(5);
    }
    if (keyCode.startsWith('Arrow')) {
      const direction = keyCode.substring(5);
      if (direction === 'Up') {
        return '↑';
      }
      if (direction === 'Down') {
        return '↓';
      }
      if (direction === 'Left') {
        return '←';
      }
      if (direction === 'Right') {
        return '→';
      }
    }
    return keyCode;
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

  filterPalette(filterValue) {
    if (this.availableShortcuts == null) {
      return;
    }
    if (filterValue === this.currentFilterValue) {
      return;
    }
    this.currentFilterValue = filterValue;
    this.generateShortcutList(this.availableShortcuts
      .filter((shortcut) => shortcut != null && shortcut.description
        .toLowerCase().includes(filterValue)));
  }

  triggerActiveShortcut() {
    if (this.handlerToTrigger != null) {
      this.handlerToTrigger();
      this.handlerToTrigger = null;
    }
  }

  triggerEvent(keyCode) {
    if (keyCode == null) {
      if (this.selectedActionNode == null) {
        return;
      }
      $('#commandPalette').modal('hide');
      this.handlerToTrigger = this.shortcuts[this.selectedActionNode].handler;
      return;
    }
    $('#commandPalette').modal('hide');
    this.handlerToTrigger = this.shortcuts[keyCode].handler;
  }

  generateShortcutList(shortcuts) {
    const shortcutList = document.createElement('div');
    this.selectedActionNode = null;
    for (let i = 0; i < shortcuts.length; i += 1) {
      if (shortcuts[i] != null) {
        const shortcutNode = document.createElement('div');
        shortcutNode.classList.add('cp-shortcut-list-item');
        shortcutNode.dataset.keyId = shortcuts[i].keyCode;
        if (this.selectedActionNode === null) {
          shortcutNode.classList.add('cp-shortcut-list-item-highlight');
          this.selectedActionNode = shortcuts[i].keyCode;
        }
        shortcutNode.appendChild(document.createTextNode(shortcuts[i].description));
        const shortcutBadge = document.createElement('span');
        shortcutBadge.classList.add('badge', 'cp-shortcut-list-badge');
        const keyNameText = this.constructor.getKeyNameFromCode(shortcuts[i].keyCode);
        const bindKeyLabel = `${this.bindKeyText} + ${keyNameText}`;
        shortcutBadge.appendChild(document.createTextNode(bindKeyLabel));
        shortcutNode.appendChild(shortcutBadge);
        shortcutList.appendChild(shortcutNode);
      }
    }
    $('#cpScListWrapper').empty().append(shortcutList);
  }

  openCommandPalette() {
    if ($('.modal.in').length > 0) {
      // do not open command palette while already in modal dialog
      return;
    }
    this.currentFilterValue = '';
    this.handlerToTrigger = null;
    const paletteSearch = document.createElement('input');
    paletteSearch.onkeyup = (e) => {
      if (e.code === 'Enter') {
        this.triggerEvent();
      } else {
        this.filterPalette(e.target.value.toLowerCase());
      }
    };
    paletteSearch.type = 'text';
    paletteSearch.id = 'cpSearchInput';
    paletteSearch.className = 'form-control';
    const shortcutListWrapper = document.createElement('div');
    shortcutListWrapper.id = 'cpScListWrapper';
    shortcutListWrapper.onclick = (e) => {
      const keyId = $(e.target).closest('.cp-shortcut-list-item').attr('data-key-id');
      if (keyId != null) {
        this.triggerEvent(keyId);
      }
    };
    $('#commandPalette .modal-body')
      .empty()
      .append(paletteSearch)
      .append(shortcutListWrapper);
    this.availableShortcuts = Object.entries(this.shortcuts).map(([keyCode, action]) => {
      if (action.context == null || action.context() === true) {
        return {
          keyCode,
          description: action.description,
          icon: action.icon,
        };
      }
      return null;
    });
    this.generateShortcutList(this.availableShortcuts);
    $('#commandPalette').modal('show');
  }

  handleKeyPressEvent(event) {
    if (event.code === 'Enter' && !event.ctrlKey) {
      if ($('#shiny-modal').find('.selectize-input.input-active').length > 0
        || $('#shiny-modal').find('*[data-dismiss="modal"]').is(':focus')) {
        return;
      }

      $('.bt-gms-confirm:visible:enabled').trigger('click');
      return;
    } // ENTER will confirm modal dialogs

    if (!event.ctrlKey || !event.altKey) {
      if ((event.code === 'ArrowUp' || event.code === 'ArrowDown')
        && $('#commandPalette').is(':visible')) {
        const activeCpNode = document.querySelectorAll('#commandPalette .cp-shortcut-list-item-highlight')[0];
        const newActiveCpNode = event.code === 'ArrowDown' ? activeCpNode.nextElementSibling : activeCpNode.previousElementSibling;
        if (newActiveCpNode == null) {
          return;
        }
        activeCpNode.classList.remove('cp-shortcut-list-item-highlight');
        newActiveCpNode.classList.add('cp-shortcut-list-item-highlight');
        newActiveCpNode.scrollIntoView();
        this.selectedActionNode = newActiveCpNode.dataset.keyId;
      }
      return;
    }
    this.availableShortcuts = Object.entries(this.shortcuts).map(([keyCode, action]) => {
      if (action.context == null || action.context() === true) {
        if (event.code === keyCode) {
          action.handler();
        }
        return {
          keyCode,
          description: action.description,
          icon: action.icon,
        };
      }
      return null;
    });
    if (event.code === 'Space') {
      this.openCommandPalette();
    }
  }
}
