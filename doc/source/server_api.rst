MIRO Server API Documentation
=============================

The GAMS MIRO Server REST API can be used to manage (add, remove, update) MIRO applications as well as scenario data. The API is based on the `OpenAPI Specification 3.0 <https://swagger.io/resources/open-api/>`__. The REST API can be accessed at ``http(s)://mydomain.com/api``, and the interactive API documentation at ``http(s)://mydomain.com/api/docs``. The technical documentation for each HTTP endpoint can be found below.

.. raw:: html

   <redoc></redoc>
   <script src="_static/redoc.standalone.js"></script>
   <script>
     // ReDoc's colors are fixed at render time, so re-render when the theme changes.
     const redocDarkTheme = {
       colors: {
         text: { primary: "#ced6dd", secondary: "#9ca4af" },
         border: { dark: "rgba(255, 255, 255, 0.15)", light: "#14181e" },
         gray: { 50: "#222832", 100: "#29313d" },
       },
       schema: { nestedBackground: "#222832" },
       typography: {
         code: { color: "#ff8a80", backgroundColor: "rgba(255, 255, 255, 0.08)" },
       },
       sidebar: { backgroundColor: "#222832", textColor: "#ced6dd" },
     };
     let redocTheme;
     const renderRedoc = () => {
       const theme = document.documentElement.dataset.theme;
       // Unset until the theme script resolves it; identical writes are no changes.
       if (!theme || theme === redocTheme) return;
       redocTheme = theme;
       Redoc.init(
         "miro_server_api.json",
         {
           scrollYOffset: 100,
           hideDownloadButton: true,
           theme: {
             // The article is too narrow for a third column at any window size.
             breakpoints: { medium: "9999rem" },
             ...(theme === "dark" && redocDarkTheme),
           },
         },
         document.querySelector("redoc"),
       );
     };
     new MutationObserver(renderRedoc).observe(document.documentElement, {
       attributeFilter: ["data-theme"],
     });
     renderRedoc();
   </script>
