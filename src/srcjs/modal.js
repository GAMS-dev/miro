/* global $:false Shiny: false */

export default function modal(
  msg,
  okButton,
  cancelButton,
  value,
  callback,
  ...callbackArgs
) {
  Shiny.modal.show({
    html: `<div id="shiny-modal" class="modal fade"
    tabindex="-1" data-backdrop="static" data-keyboard="false">
  <div class="modal-dialog modal-sm">
    <div class="modal-content">
      <div class="modal-body">
         ${
  value == null
    ? `<div class="text-break"><strong>${msg}</strong></div>`
    : `<div class="form-group shiny-input-container">
            <label class="control-label" for="miroPromptInput">${msg}</label>
            <input id="miroPromptInput" type="text" class="form-control" value="${value}"/>
          </div>`
}
      </div>
      <div class="modal-footer">
        ${cancelButton == null ? '' : `<button type="button" class="btn btn-default" data-dismiss="modal">${cancelButton}</button>`}
        <button id="miroModalConfirmButton" type="button"
        class="btn btn-default bt-highlight-1 bt-gms-confirm">${okButton}</button>
      </div>
    </div>
  </div>
  <script>$('#shiny-modal').modal().focus();</script>
</div>`,
  });
  $(document).off('click', '#miroModalConfirmButton');
  if (value == null) {
    $(document).on('click', '#miroModalConfirmButton', () => {
      if (callback == null || callback(...callbackArgs) !== false) {
        $('#shiny-modal').modal('hide');
      }
    });
  } else {
    $(document).on('click', '#miroModalConfirmButton', () => {
      if (
        callback(
          document.getElementById('miroPromptInput').value,
          ...callbackArgs,
        ) !== false
      ) {
        $('#shiny-modal').modal('hide');
      }
    });
  }
}
