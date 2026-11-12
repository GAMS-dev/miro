(() => {
  "use strict";

  const isInlineIcon = (img) => img.classList.contains("miro-inline-icon")
    || img.classList.contains("inline-icon")
    || (img.closest("a") && img.getBoundingClientRect().width > 0
      && img.getBoundingClientRect().width <= 32
      && img.getBoundingClientRect().height <= 32);

  const eligible = (img) => img.closest("main")
    && !img.closest(".sd-card, nav, header, footer, .navbar")
    && !isInlineIcon(img)
    && !img.closest("a");

  const initialize = () => document.querySelectorAll("main img").forEach((img) => {
    if (isInlineIcon(img) && img.closest("a")) {
      img.closest("a").classList.add("miro-inline-icon-link");
    }
    if (!eligible(img)) return;
    img.classList.add("miro-zoomable-image");
    img.tabIndex = 0;

    const open = () => {
      const scrollX = window.scrollX;
      const scrollY = window.scrollY;
      const dialog = document.createElement("dialog");
      dialog.className = "miro-image-lightbox";
      dialog.setAttribute("aria-label", img.alt || "Image preview");

      const enlarged = document.createElement("img");
      enlarged.src = img.currentSrc || img.src;
      enlarged.alt = img.alt || "";

      const close = document.createElement("button");
      close.type = "button";
      close.className = "miro-image-lightbox-close";
      close.setAttribute("aria-label", "Close image");
      close.textContent = "×";

      dialog.append(enlarged, close);
      document.body.append(dialog);

      const remove = () => {
        dialog.close();
        dialog.remove();
        window.scrollTo(scrollX, scrollY);
        img.focus({preventScroll: true});
      };
      close.addEventListener("click", remove);
      dialog.addEventListener("click", (event) => {
        if (event.target === dialog) remove();
      });
      dialog.addEventListener("cancel", (event) => {
        event.preventDefault();
        remove();
      });
      dialog.showModal();
      close.focus();
    };

    img.addEventListener("click", open);
    img.addEventListener("keydown", (event) => {
      if (event.key === "Enter" || event.key === " ") {
        event.preventDefault();
        open();
      }
    });
  });

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initialize, {once: true});
  } else {
    initialize();
  }
})();
