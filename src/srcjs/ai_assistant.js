/* global $:false, Shiny:false */

import { parseKatex } from './util';

export function copyChatContent(button) {
  const wrapper = button.closest('.miro-ai-chat-bubble-wrapper');
  const contentToCopy = wrapper.querySelector('.miro-ai-chat-content-raw').textContent;
  navigator.clipboard.writeText(contentToCopy).then(() => {
    const icon = button.querySelector('i');
    if (icon) {
      const originalClass = icon.className;
      icon.className = 'fas fa-check';
      button.style.color = '#28a745';

      setTimeout(() => {
        icon.className = originalClass;
        button.style.color = '';
      }, 1500);
    }
  }).catch((err) => {
    // eslint-disable-next-line no-console
    console.error('Failed to copy AI chat content: ', err);
  });
}
export function toggleChatCollapse(btn) {
  const bubble = btn.closest('.miro-ai-chat-bubble');
  const content = bubble.querySelector('.miro-ai-chat-content-html');
  const icon = btn.querySelector('i');

  if (content.classList.contains('miro-ai-text-collapsed')) {
    content.classList.remove('miro-ai-text-collapsed');
    icon.classList.remove('fa-chevron-down');
    icon.classList.add('fa-chevron-up');
  } else {
    content.classList.add('miro-ai-text-collapsed');
    icon.classList.remove('fa-chevron-up');
    icon.classList.add('fa-chevron-down');
  }
}
export function openMiroAiDrawer() {
  document.getElementById('miroAiDrawer').classList.add('open');
  document.body.classList.add('drawer-open');
  document.getElementById('miroAiUserText').focus();
  $('body').addClass('sidebar-collapse');
  setTimeout(() => {
    window.dispatchEvent(new Event('resize'));
  }, 300);
}
export function closeMiroAiDrawer() {
  document.getElementById('miroAiDrawer').classList.remove('open');
  document.body.classList.remove('drawer-open');
  $('body').removeClass('sidebar-collapse');
  setTimeout(() => {
    window.dispatchEvent(new Event('resize'));
  }, 300);
}

// Helper function for height adjustment
function adjustTextareaHeight(el) {
  el.style.height = 'auto';
  const maxHeight = parseFloat(getComputedStyle(el).lineHeight) * 10;
  if (el.scrollHeight <= maxHeight) {
    el.style.height = `${el.scrollHeight}px`;
    el.style.overflowY = 'hidden';
  } else {
    el.style.height = `${maxHeight}px`;
    el.style.overflowY = 'auto';
  }
}

function sendUserInputToShiny(userInput) {
  Shiny.setInputValue('miroAiUserText', userInput, {
    priority: 'event',
  });
}

export function registerAiHandlers() {
  $(document).on('keydown', '#miroAiUserText', function (e) {
    if (e.key === 'Enter' && !e.shiftKey) {
      // Prevent newline
      e.stopPropagation();
      e.stopImmediatePropagation();
      e.preventDefault();
      const content = this.value.trim();
      if (content) {
        sendUserInputToShiny(content);
      }
      this.value = '';
    }
  });
  $(document).on('input', '#miroAiUserText', function () {
    adjustTextareaHeight(this);
  });
  Shiny.addCustomMessageHandler('gms-addAiChatBubble', (content) => {
    const $newElement = $(content.html);
    $('#miroAiChatSpinner').before($newElement);

    const $chatLog = $('#miroAiChatLog');
    const textarea = document.getElementById('miroAiUserText');
    const stopButton = document.getElementById('btMiroAiStop');
    if (content.role === 'user') {
      textarea.value = '';
      textarea.disabled = true;
      stopButton.style.display = 'block';
      adjustTextareaHeight(textarea);
      $('#miroAiChatSpinner').show();
      $chatLog.scrollTop($chatLog[0].scrollHeight);
    } else {
      const contentDiv = $newElement.find('.miro-ai-chat-content-html').get(0);
      parseKatex(contentDiv, {
        delimiters: [
          { left: '$$', right: '$$', display: true },
          { left: '$', right: '$', display: false },
          { left: '\\(', right: '\\)', display: false },
          { left: '\\[', right: '\\]', display: true },
        ],
      });
      $('#miroAiChatSpinner').hide();
      const scrollToPosition = $newElement.position().top + $chatLog.scrollTop();
      $chatLog.animate({ scrollTop: scrollToPosition }, 300);
      stopButton.style.display = 'none';
      textarea.disabled = false;
    }
  });
}
