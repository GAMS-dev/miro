// Editor for the custom environment variables (miroEnv) in the settings window:
// one row per variable with a name and a value input.

const NAME_PATTERN = /^[A-Z_][A-Z0-9_]*$/;

const DEFAULT_LABELS = {
  miroEnvHdrVar: 'Variable',
  miroEnvHdrVal: 'Value',
  noEnvDesc: 'No environment variables defined.',
  btEnvAdd: 'Add variable',
  btEnvRemove: 'Remove variable',
  envNameInvalid:
    'Use uppercase letters, digits and underscores only; do not start with a digit.',
  envNameDuplicate: 'This variable is already defined.',
  envNameMissing: 'Enter a variable name.',
};

const REMOVE_ICON = '<svg viewBox="0 0 16 16" width="14" height="14" aria-hidden="true">'
  + '<path d="M4 4l8 8M12 4l-8 8" stroke="currentColor" stroke-width="1.6" '
  + 'stroke-linecap="round" fill="none"/></svg>';

class EnvEditor {
  constructor(container, onChange = () => {}) {
    this.container = container;
    this.onChange = onChange;
    this.labels = { ...DEFAULT_LABELS };
    this.disabled = false;
    container.classList.add('env-editor');
    this.container.innerHTML = `
      <div class="env-row env-row-header" aria-hidden="true">
        <span class="env-hdr-name"></span><span class="env-hdr-value"></span><span></span>
      </div>
      <div class="env-rows" role="list"></div>
      <p class="env-empty"></p>
      <button type="button" class="btn env-add-btn">
        <span class="env-add-icon" aria-hidden="true">+</span><span class="env-add-label"></span>
      </button>`;
    this.rowsEl = container.querySelector('.env-rows');
    this.addBtn = container.querySelector('.env-add-btn');
    this.addBtn.addEventListener('click', () => {
      this.addRow('', '').querySelector('.env-name').focus();
      this.refresh();
    });
    this.rowsEl.addEventListener('input', (e) => {
      if (e.target.classList.contains('env-name')) {
        // variable names are upper case, so save users from typing them that way
        const input = e.target;
        const upper = input.value.toUpperCase();
        if (upper !== input.value) {
          const { selectionStart, selectionEnd } = input;
          input.value = upper;
          input.setSelectionRange(selectionStart, selectionEnd);
        }
      }
      this.validate(false);
      this.onChange();
    });
    this.rowsEl.addEventListener('focusout', (e) => {
      if (e.target.classList.contains('env-name')) {
        this.validate(true);
      }
    });
    this.rowsEl.addEventListener('click', (e) => {
      const btn = e.target.closest('.env-remove-btn');
      if (!btn) {
        return;
      }
      const row = btn.closest('.env-row');
      const next = row.nextElementSibling || row.previousElementSibling;
      row.remove();
      if (next) {
        next.querySelector('.env-name').focus();
      } else {
        this.addBtn.focus();
      }
      this.refresh();
      this.validate(false);
      this.onChange();
    });
    this.setLabels({});
    this.refresh();
  }

  setLabels(labels) {
    Object.keys(DEFAULT_LABELS).forEach((key) => {
      if (labels[key]) {
        this.labels[key] = labels[key];
      }
    });
    const { container } = this;
    container.querySelector('.env-hdr-name').textContent = this.labels.miroEnvHdrVar;
    container.querySelector('.env-hdr-value').textContent = this.labels.miroEnvHdrVal;
    container.querySelector('.env-empty').textContent = this.labels.noEnvDesc;
    container.querySelector('.env-add-label').textContent = this.labels.btEnvAdd;
    this.rowsEl.querySelectorAll('.env-row').forEach((row) => this.labelRow(row));
    this.validate(false);
  }

  labelRow(row) {
    row.querySelector('.env-name').setAttribute('aria-label', this.labels.miroEnvHdrVar);
    row.querySelector('.env-value').setAttribute('aria-label', this.labels.miroEnvHdrVal);
    const btn = row.querySelector('.env-remove-btn');
    btn.setAttribute('aria-label', this.labels.btEnvRemove);
    btn.title = this.labels.btEnvRemove;
  }

  addRow(name, value) {
    const row = document.createElement('div');
    row.className = 'env-row';
    row.setAttribute('role', 'listitem');
    row.innerHTML = `
      <input type="text" class="form-control env-name" spellcheck="false" autocomplete="off" placeholder="MY_VARIABLE">
      <input type="text" class="form-control env-value" spellcheck="false" autocomplete="off">
      <button type="button" class="env-remove-btn">${REMOVE_ICON}</button>
      <div class="env-feedback"></div>`;
    row.querySelector('.env-name').value = name;
    row.querySelector('.env-value').value = value;
    this.labelRow(row);
    this.setRowDisabled(row);
    this.rowsEl.appendChild(row);
    return row;
  }

  setRowDisabled(row) {
    row.querySelectorAll('input, button').forEach((el) => {
      el.toggleAttribute('disabled', this.disabled);
    });
  }

  setEnv(env) {
    this.rowsEl.innerHTML = '';
    Object.entries(env || {}).forEach(([name, value]) => this.addRow(name, value));
    this.refresh();
  }

  setDisabled(disabled) {
    this.disabled = disabled === true;
    this.addBtn.disabled = this.disabled;
    this.rowsEl.querySelectorAll('.env-row').forEach((row) => this.setRowDisabled(row));
  }

  refresh() {
    this.container.classList.toggle('env-editor-empty', this.rowsEl.children.length === 0);
  }

  rows() {
    return [...this.rowsEl.querySelectorAll('.env-row')].map((row) => ({
      row,
      name: row.querySelector('.env-name').value.trim(),
      value: row.querySelector('.env-value').value,
    }));
  }

  // Marks invalid rows. Rows without a name are only flagged once the user leaves the field
  // (or on save), so a freshly added row is not red right away.
  validate(showMissing = true) {
    const seen = new Set();
    let valid = true;
    this.rows().forEach(({ row, name, value }) => {
      let error = null;
      if (name === '') {
        if (value !== '') {
          error = this.labels.envNameMissing;
          if (!showMissing && !row.classList.contains('env-row-invalid')) {
            valid = false;
            error = null;
          }
        }
      } else if (!NAME_PATTERN.test(name)) {
        error = this.labels.envNameInvalid;
      } else if (seen.has(name)) {
        error = this.labels.envNameDuplicate;
      }
      seen.add(name);
      row.classList.toggle('env-row-invalid', error !== null);
      row.querySelector('.env-name').classList.toggle('is-invalid', error !== null);
      const feedback = row.querySelector('.env-feedback');
      feedback.textContent = error || '';
      if (error !== null) {
        valid = false;
      }
    });
    return valid;
  }

  // the environment as an object, or null if no variable is defined;
  // rows left completely empty are ignored
  getEnv() {
    const env = {};
    this.rows().forEach(({ name, value }) => {
      if (name !== '') {
        env[name] = value;
      }
    });
    return Object.keys(env).length > 0 ? env : null;
  }

  focusFirstError() {
    const input = this.rowsEl.querySelector('.env-name.is-invalid');
    if (input) {
      input.focus();
    }
  }
}

module.exports = { EnvEditor };
