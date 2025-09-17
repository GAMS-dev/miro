import modal from './modal';

export default class BaselineCompareErrorManager {
  errors = {};

  evaluateData(refData, data, tableId, sessionId) {
    if (Math.abs(refData - data) > 1e-4) {
      if (
        !Object.hasOwn(this.errors, tableId)
        || this.errors[tableId] !== sessionId
      ) {
        this.errors[tableId] = sessionId;
        setTimeout(() => {
          if (
            Object.hasOwn(this.errors, tableId)
            && this.errors[tableId] === sessionId
          ) {
            modal(
              "Something went wrong. Please don't trust the data! Also, please contact GAMS about this issue (id: 981273) via support@gams.com",
              'OK',
            );
          }
        }, 2000);
      }
    } else if (
      Object.hasOwn(this.errors, tableId)
      && this.errors[tableId] !== sessionId
    ) {
      // old error -> clear as we render a new table
      delete this.errors[tableId];
    }
  }
}
