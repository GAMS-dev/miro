export default class MiroLogParser {
  isInitialized = false;

  initialize(
    logTabId,
    logContainerId,
    tabSheetMap,
    inputSymbolNames,
    inputScalars,
  ) {
    this.logTabId = logTabId;
    this.logContainer = document.getElementById(logContainerId);
    this.inputSymbolNames = inputSymbolNames;
    this.tabSheetMap = tabSheetMap;
    this.inputScalars = inputScalars != null ? inputScalars : [];
    this.reset();
    this.isInitialized = true;
  }

  reset() {
    this.symbolErrorMessages = {};
    this.firstValidationErrorTabId = [];
  }

  parseLogContainer() {
    this.reset();
    const parsedContent = this.parse(this.logContainer.textContent);
    this.logContainer.textContent = '';
    this.logContainer.appendChild(parsedContent);
  }

  getLogTabId() {
    return this.logTabId;
  }

  getTabId(symbolName) {
    const parentTabIndex = this.inputSymbolNames.findIndex(
      (el) => el === symbolName,
    );
    if (parentTabIndex !== -1) {
      const tabIdTmp = this.tabSheetMap[parentTabIndex];
      if (Array.isArray(tabIdTmp)) {
        return tabIdTmp;
      }
      return [tabIdTmp];
    }
    return [];
  }

  parse(textToParse) {
    if (this.inputSymbolNames == null || this.inputSymbolNames.length === 0) {
      return document.createTextNode(textToParse);
    }
    const lines = textToParse.split('\n');
    const fragment = document.createDocumentFragment();
    lines.forEach((line, index) => {
      const [candidateRaw, errorMessage] = line.split('::', 2);
      if (errorMessage != null) {
        const candidate = candidateRaw.trim();
        let symbolName = this.inputSymbolNames.find(
          (sym) => sym === candidate,
        );
        if (
          symbolName == null
          && this.inputScalars.find((sym) => sym === candidate)
        ) {
          symbolName = '_scalars';
        }
        if (symbolName != null) {
          if (this.firstValidationErrorTabId[0] == null) {
            this.firstValidationErrorTabId = this.getTabId(symbolName);
          }
          if (
            Object.prototype.hasOwnProperty.call(
              this.symbolErrorMessages,
              symbolName,
            )
          ) {
            this.symbolErrorMessages[symbolName].push([index, errorMessage]);
          } else {
            this.symbolErrorMessages[symbolName] = [[index, errorMessage]];
          }
          const markElement = document.createElement('mark');
          markElement.id = `mlogMark_${index}`;
          markElement.className = 'miro-log-mark';
          markElement.textContent = line;
          fragment.appendChild(markElement);
          fragment.appendChild(document.createTextNode('\n'));
          return;
        }
      }
      fragment.appendChild(document.createTextNode(`${line}\n`));
    });
    return fragment;
  }

  showValidationErrors() {
    Object.entries(this.symbolErrorMessages).forEach(
      ([symbolName, errorMessageList]) => {
        errorMessageList.forEach((errorMessageItem) => {
          const [logMarkId, errorMessage] = errorMessageItem;
          const validationErrorElement = document.createElement('li');
          validationErrorElement.textContent = errorMessage;
          validationErrorElement.className = 'miro-log-mark-link';
          validationErrorElement.dataset.logMarkId = logMarkId;
          document
            .getElementById(`valErr_${symbolName}`)
            .appendChild(validationErrorElement);
        });
      },
    );
  }
}
