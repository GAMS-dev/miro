Release notes
=============

.. _v214:

GAMS MIRO 2.14
--------------

(xxxx-xx-xx)

Security
~~~~~~~~

- Various security improvements. Updating is recommended, especially for MIRO Server.

New features
~~~~~~~~~~~~

- Introduce `Mira <./ai_assistant.html>`__, a chat-based AI
  assistant that helps you get more out of your app:

  - Analyze the currently loaded sandbox scenario or a comparison of
    scenarios, and ask questions about your data directly within the app.
  - Ask Mira to `propose changes to your input data
    <./ai_assistant.html#changing-input-data>`__, e.g. "increase demand in
    the EU by 10%", and apply them with a single click after reviewing a
    before/after preview.
  - In Configuration Mode, get assistance writing `custom renderers
    <./ai_assistant.html#ai-assisted-custom-renderers>`__.

- New, modern design for MIRO apps, MIRO Desktop and MIRO Server.
- MIRO Desktop: Environment variables can now be edited directly in the settings window, so exporting and importing them is no longer necessary.
- Spanish language support.

Minor new features and improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Improved German and Chinese translations.

- Updated documentation with a new design and layout, resulting in clearer
  guides, improved navigation and search, and light/dark theme support.
- Enhanced how background tasks are managed, ensuring smoother job
  submissions, progress tracking, and result downloads without
  interrupting your workflow.
- Make the `thresholds at which MIRO Pivot truncates data before rendering
  <./configuration_json_only.html#pivot-truncation>`__ (e.g. the maximum
  number of chart data points, pie slices, or table columns) configurable via
  JSON.
- Add option to `hide specific input/output symbols
  <./configuration_general.html#hidden-compare-symbols>`__ in split, tab and
  pivot scenario comparison mode, while still displaying them normally
  everywhere else in the app.
- Introduce a global `Numerical Tolerance
  <./configuration_general.html#numerical-tolerance>`__ setting: values
  within this threshold of zero are now treated as exactly zero when the
  Dashboard and MIRO Pivot renderers compute a baseline comparison, avoiding
  division-by-zero or inflated percentage/normalized results caused by
  floating-point noise around zero.

- `Custom theme colors <./configuration_colors.html#custom-theme-colors>`__: The main background color can now also be set for Light mode. The body background color has been removed, as the new layout no longer shows it; existing color themes continue to work without changes. The highlighted sidebar entry now automatically adjusts its color to stay readable on dark sidebars.
- Add option to `prevent attachments from overwriting model files <./configuration_general.html#protect-model-files>`__.

Bug fixes
~~~~~~~~~

- Fix bug in the logging of the MIRO Server authentication container.
- Hypercube module: Fixed bug that, in certain situations, caused the
  number of scenarios in the Hypercube module to be calculated
  incorrectly due to issues with numerical precision.
- Fix incorrect conversion of ``null`` array values to ``"NA"`` in
  configuration files, which caused schema violations.

.. _v213:

GAMS MIRO 2.13
--------------

(2025-09-25)

GAMS MIRO 2.13.5
~~~~~~~~~~~~~~~~

(2026-06-30)

- Fix issue that prevented logging into Engine via OAuth2-based identity
  providers when using MIRO Desktop.
- Fix app version/authors not updated when updating an app.
- Introduce a new `Quick Start Guide <./getting_started.html>`__ with an
  end-to-end onboarding workflow for MIRO and reorganize the
  documentation landing pages.
- MIRO Desktop: Fix crash when changing language to German or Chinese.
- MIRO Desktop: Fix crash on macOS that occurred when using a GAMS
  Engine backend and attempting to launch an app.

GAMS MIRO 2.13.4
~~~~~~~~~~~~~~~~

(2026-02-24)

- MIRO Desktop: Add a missing timeout for Python/GAMS executable
  validation. This prevents MIRO from hanging when checking
  installations during startup or within the settings menu.
- Batch Load: No longer truncate long scenario names.
- MIRO Pivot: Fix issue that prevented opening the `presentation
  mode <./configuration_general.html#enable-presentation>`__ when a
  filter was configured as a single-choice dropdown menu.

GAMS MIRO 2.13.3
~~~~~~~~~~~~~~~~

(2026-01-23)

- MIRO Desktop: Increase maximum size for attachments to 500MB.

GAMS MIRO 2.13.2
~~~~~~~~~~~~~~~~

(2025-12-09)

- Linux Support: We have added a native ``.deb`` installer to resolve
  compatibility issues with Ubuntu 24.04. While the AppImage is still
  available, we now recommend the ``.deb`` package for all Ubuntu users
  to ensure the best experience.
- Server: Upgrade ShinyProxy dependency to resolve XSS vulnerabilities
  and enable support for Docker v29+.
- Fix responsive layout of dashboard userFilters and chart type
  selection so their dropdowns wrap cleanly on smaller screens instead
  of overlapping.
- Fix issue that occurred when loading the sandbox scenario in dashboard
  comparison mode.
- Usability Improvements:

  - MIRO Pivot: Improve view selection menu. The search bar now
    auto-focuses and stays sticky when scrolling. Additionally, the
    active view is now highlighted.
  - Tabset Panels: The "More..." tab search bar now auto-focuses on open
    and remains sticky when scrolling.

GAMS MIRO 2.13.1
~~~~~~~~~~~~~~~~

(2025-10-29)

- We've renamed the "MIRO app path" label in the Settings dialog to
  "MIRO workspace path" to be consistent with the documentation.
- Fix issue where apps added to MIRO Server through the REST API would
  fail to launch correctly.
- Server: Fix issue where admin panel did not launch when authentication
  was set to *none*.
- Server: Fix low text contrast in the login form's input fields for
  dark mode, making them easier to read.
- Fix value box labels not wrapping correctly in dashboards.
- When switching from another scenario comparison mode to dashboard
  comparison, previously loaded scenarios were not retained. They are
  now preselected in the selection dialog.
- When a filter couldn't be applied in a dashboard, charts/tables
  silently fell back to the first element of the dimension, with no
  notification. Affected views now show no data and display an
  informational message.
- Dashboard/MIRO Pivot: Baseline comparison data now remains accurate
  when sorting or searching the table.

New features
~~~~~~~~~~~~

- Custom Language Support: Add support for `custom language
  files <./configuration_general.html#custom-language>`__, allowing to
  override and personalize all or individual interface text and labels.

.. _minor-new-features-and-improvements-1:

Minor new features and improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- User Interface:

  - Key controls, like the button for switching between table and chart,
    have been moved into the header bar to reduce clutter and maximize
    viewing space. This, plus other minor spacing and design
    improvements, creates a much cleaner workspace.
  - The default navigation bar color in light mode has been switched to
    white.
  - The default primary color in dark mode has been switched to blue.
  - The buttons to load data and to solve the model are now also
    available in the output section.
  - Add "What's New" dialog to Help menu in MIRO UI and to MIRO Desktop
    library.
  - New *Engine Settings* dialog available in the *Help* menu. It shows
    a user's remaining quota and allows changing the default instance
    when supported by the Engine backend.

- `MIRO Desktop library <./deployment.html#sbs-install-app>`__:

  - Redesign of the initial screen (no apps), as well as general design
    improvements.
  - App no longer have a dedicated launch button. Instead, the entire
    app card is clickable now.

- `Dashboard renderer <./charts.html#dashboard>`__ and `dashboard
  Compare <./configuration_json_only.html#dashboard-compare>`__:

  - `Filters <./charts.html#dashboard-dropdown-filter>`__: Add options
    to configure default selections (explicit or positional), toggle
    single- vs. multi-select per dimension, and set the control label
    and placeholder. The legacy userFilter format (arrays of dimension
    names) is deprecated but still works.
  - Display customized `column
    headers <./configuration_symbols.html#naming>`__ in dashboard
    tables.

- `MIRO Pivot <./charts.html#pivot-chart>`__:

  - Add support for controlling the number of decimals to be displayed
    in the table/chart.
  - Limit the number of displayed series to 40 in pie chart and doughnut
    chart.

- MIRO Server: Minor design improvements.

.. _bug-fixes-1:

Bug fixes
~~~~~~~~~

- Dashboard: Fix issues with the baseline comparison feature that
  occurred in combination with multiple selected user filters and the
  calculation of rounded numbers.
- Dashboard/MIRO Pivot: Fixed an issue where rapidly selecting or
  deselecting filters could trigger a false error message from the
  baseline comparison module indicating that data was unreliable.
- Dashboard compare: Fix issue that occurred when output data was
  missing.
- MIRO Pivot: Fix missing series colors in pie chart and doughnut chart.
- MIRO Pivot: Fix issue that occurred when opening the view menu in
  certain situations.
- MIRO Server: Fix instance pools not available in job submission
  dialog.
- MIRO Server: Fix issue where updating an app that requires a database
  migration could leave the system in an inconsistent state.

.. _v212:

GAMS MIRO 2.12
--------------

(2025-06-13)

GAMS MIRO 2.12.1
~~~~~~~~~~~~~~~~

(2025-07-24)

- Fix interrupt not working on Windows for GAMSPy apps.
- Dashboard: Fix issue where the baseline comparison feature failed to
  function when used alongside ``customLabels`` and/or ``userFilter``.
- Dashboard: Fix issue where ``groupDimension`` feature did not work with the
  ``stackedbar`` chart type.
- Fix performance issues in `dashboard
  renderer <./charts.html#dashboard>`__ and `dashboard
  compare <./configuration_json_only.html#dashboard-compare>`__ when
  handling many views.
- Fix issue in dashboard where the user filter did not work properly
  when multiple aliased dimensions were present in the columns of a
  view.
- Add missing button text color to the MIRO Server color palette.

Breaking changes
~~~~~~~~~~~~~~~~

- Deprecated custom analysis scripts. Please contact
  `support@gams.com <mailto:support@gams.com?subject=MIRO%20custom%20analysis%20scripts%20deprecation&body=I%20am%20still%20using%20custom%20analysis%20scripts%20in%20MIRO%20and%20don't%20want%20them%20to%20be%20removed.>`__
  if you rely on this feature and don't want it to be removed in a
  future release.

.. _new-features-1:

New features
~~~~~~~~~~~~

- MIRO now allows you to use `custom theme
  colors <./configuration_colors.html#custom-theme-colors>`__. Define a
  small palette of brand- or project-specific colors, and MIRO will
  automatically apply them to tables, form elements, and highlight
  accents, giving each app its own distinct look and feel.

.. _minor-new-features-and-improvements-2:

Minor new features and improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- `MIRO Pivot <./charts.html#pivot-chart>`__, `dashboard
  renderer <./charts.html#dashboard>`__ and `dashboard
  Compare <./configuration_json_only.html#dashboard-compare>`__:

  - Stacked bar charts: add option to group stacks by a dimension.
  - Allow `custom patterns <./charts.html#series-patterns>`__ for series
    used in several options.
  - If custom chart colors are specified for only some data series, any
    series without a custom color now uses the default palette color
    instead of the previous gray fallback.
  - Add `Baseline Comparison <./charts.html#baseline-comparison>`__ -
    pick a baseline record and display up to two difference metrics per
    cell.

- Dashboard renderer and dashboard Compare: Add support for specifying
  `custom label
  ordering <./charts.html#dashboard-custom-labels-order>`__ on a
  per-dimension basis.
- Support adding `version and author
  information <./deployment.html#app-metadata>`__ to app_info.json file.
- Add GAMSPy example model: Battery Energy Storage System (BESS) sizing
  problem. This addition comes with a `comprehensive
  guide <./step_by_step_tutorial.html>`__ that walks you from creating
  the GAMSPy model to building a sophisticated application.
- Success messages for data import/export now include the name of the
  importer/exporter for improved clarity.
- Increase maximum number of attachments per scenario from 7 to 20.
- MIRO Desktop: Remove scenario permissions tab from edit metadata
  dialog as scenario permissions are only supported in MIRO Server.
- MIRO Server: Improve user `interface for entering environment
  variables <server_administration.html#app-environment>`__ as well as
  support setting default scenario permissions.
- MIRO Server: Support customizing the `app's
  favicon <deployment.html#favicon>`__.
- MIRO Server: The list of groups in permission selectors is filtered to
  include only those that are part of the app's configured access
  groups, ensuring more relevant and secure group selection.

.. _v211:

GAMS MIRO 2.11
--------------

(2025-01-21)

GAMS MIRO 2.11.2
~~~~~~~~~~~~~~~~

(2025-05-22)

- Fix issue where the view in the pivot compare mode was not properly
  reset in certain situations.
- Fix issue `signing/validating <./deployment.html#sbs-sign-app>`__ apps
  on macOS.
- Support `extra command line
  arguments <./configuration_general.html#command-line-args>`__ for
  GAMSPy models.
- Fix about dialog not opening in Configuration Mode.

GAMS MIRO 2.11.1
~~~~~~~~~~~~~~~~

(2025-02-19)

- Fix issue where the output data was cleared when using a custom
  importer with a subset of input/output symbols (using the ``symNames``
  property).
- Fix issue where the scalars dataframe was validated when using a
  custom importer, despite no scalars being intended for import.
- Fix issue where MIRO used excessive memory/cpu when submitting jobs to
  GAMS Engine.
- Fix issue where reading parameters from GDX with invalid domain
  information resulted in empty tables.
- Fix issue where the dashboard renderer threw an error in some
  situations if the data view configuration did not specify a row
  dimension.
- Fix issue where an incorrect internal error message was displayed when
  submitting a job to GAMS Engine under specific conditions.
- If an error occurs while interrupting the GAMS process, the error
  message is now less intrusive.
- Fix issue where the modified dataset count was off by one in certain
  situations during local data import.
- MIRO Server: Fix issue where the loading indicator was not hidden
  after canceling app data migration.
- MIRO Server: To prevent accidental overwriting of existing app data,
  administrators must now enter the app ID before the button to
  overwrite data can be clicked.
- Fix issue where value boxes in the dashboard renderer did not show
  pre-/postfixes in some situations.
- Add missing ``attachments`` argument to the custom renderer function when
  using the custom renderer editor in Configuration Mode.
- MIRO Server: Fix potential XSS vulnerabilities in admin panel.

.. _new-features-2:

New features
~~~~~~~~~~~~

- Add new optional scenario comparison module `dashboard
  Compare <./configuration_json_only.html#dashboard-compare>`__. Like
  the `dashboard renderer <./charts.html#dashboard>`__ the dashboard
  scenario comparison is an interactive tool that can display multiple
  charts and tables but for multiple scenarios at once.

.. _minor-new-features-and-improvements-3:

Minor new features and improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Dashboard renderer:

  - Add option to change the labels order in charts/tables.
  - Add option to hide the legend in charts.
  - Add option to show an alternative heatmap with different colors for
    positive and negative values.
  - Show comma as thousands separator in value boxes.
  - Support displaying the level of `scalar variables or
    equations <./model.html#display-scalar-vareq>`__ in value boxes.

- New options in `MIRO Pivot <./charts.html#pivot-chart>`__ and
  `dashboard renderer <./charts.html#dashboard>`__:

  - Support separate activation/deactivation of row/column summary.
  - Add option to configure filter dimensions as single-choice dropdown
    menus.
  - Add option to specify the font size for charts.
  - Add support for a second x/y-axis.
  - Add option to select the second chart type in a combo chart.
  - It is now possible to configure the stacking behavior of data series
    in combo charts (as part of a stacked chart), enabling the creation
    of grouped stacked bar charts, stacked bar/line charts, and more.
  - Add option to draw a line chart as stepped line chart.
  - Add option to set a dash pattern for individual series of a line
    chart.
  - Add option to set a border width for individual series.
  - Add option to set a static minimum/maximum value for axes.
  - Add option to show/hide data points in a chart.
  - Add option to show/hide the grid in a chart.
  - Add option to specify the fill opacity for stacked and regular area
    charts.
  - Add option to stack positive and negative values together for
    stacked charts.

- MIRO Pivot:

  - The `view dialog <./charts.html#pivot-view>`__ has been
    restructured.
  - The `settings dialog <./charts.html#pivot-settings>`__ has been
    restructured.
  - Support setting title for ``table`` and ``heatmap`` renderers.
  - Add option to activate/deactivate fixed table columns to the
    settings menu.
  - The ``enableHideEmptyCols`` option has been deprecated. The functionality to
    show/hide empty table columns is now always accessible via the
    settings menu.
  - The CSV and PNG download buttons have been consolidated into a
    dropdown menu.

- Pivot compare mode: Add support for setting default values for several
  configuration options that are also available in the regular Pivot
  tool.
- Configuration Mode: When activating checkbox to load scenario on app
  startup, the scenario name is now "default" by default.
- Improve error messages if the validation of the JSON configuration
  file fails.
- Add GAMSPy example model: tsp.
- Add option to `parse standard log (stdout) for MIRO log
  syntax <./configuration_general.html#miro-syntax-stdout>`__. This is
  particularly useful for GAMSPy-MIRO models, where it is easier to
  control the stdout of the process.

.. _bug-fixes-2:

Bug fixes
~~~~~~~~~

- MIRO Pivot: Fix issue where removing title from chart did not work
  properly.
- MIRO Pivot: Fix issue loading view with multiple single-choice
  dropdown menus.
- Pivot compare mode: Fix issue where removing all dimensions from rows
  could cause a problem in certain situations.
- Update Help menu link to point to the new `GAMS
  forum <https://forum.gams.com/c/gams-miro/12>`__.
- Fix issue where clicking the "Solve" button could cause MIRO with GAMS
  Engine backend to crash in certain situations.
- Fixes issue where updating `dependent value(s) of
  slider <./configuration_advanced.html#widgets-dependencies>`__
  resulted in broken slider with only ``NaN`` choices.
- Fixes issue where updating `dependent choices of
  dropdown <./configuration_advanced.html#widgets-dependencies>`__
  resulted in selected value to be reset.
- Fix issue where imported output datasets were not counted in the total
  number of imported sheets displayed in the alert message during local
  file imports.
- Fix issue where choosing no datasets during manual dataset selection
  for local file imports incorrectly resulted in all datasets being
  imported.
- Fix issue where adding GAMSPy apps to MIRO Desktop did not work in
  certain situations.
- Fix crash when opening dialog to download temporary files when number
  of temporary files was large (>500).
- Fix issue where rendering a Chart.js chart with an invalid
  configuration would break the canvas, preventing it from being reused
  for subsequent charts with valid configurations.
- MIRO Server: Fix issue where app logos did not load when ``server.servlet.context-path``
  was set to anything but ``/``.
- MIRO Pivot: Fix issue where incorrect labels were displayed in a chart
  in certain situations.
- Using a fixed `color mode <configuration_general.html#dark-mode>`__
  (*light* or *dark*) did not work properly for some components (e.g.
  dashboard).
- `Dashboard with custom code <./charts.html#dashboard-custom-code>`__:
  Due to a package update, the appearance of the value boxes in the
  dashboard has changed. To avoid this change, synchronize your custom
  dashboard code with the `official
  version <https://github.com/GAMS-dev/miro/blob/master/src/modules/renderers/dashboard.R>`__
  or delete the following line in your code: ``class = "shiny-html-output",``.

.. _v210:

GAMS MIRO 2.10
--------------

(2024-07-17)

GAMS MIRO 2.10.4
~~~~~~~~~~~~~~~~

(2024-10-29)

- Fix determining the delimiter of a CSV file did not work for quoted
  strings.
- Fix issue when using the dashboard as a custom renderer (to use custom
  code).
- MIRO Server: Fix issue that causes the Docker Socket Proxy to crash on
  certain Linux distributions and/or certain Docker versions.
- MIRO Server: Fix potential Denial of service (DoS) vulnerability in
  API server.

GAMS MIRO 2.10.3
~~~~~~~~~~~~~~~~

(2024-10-07)

- Fix XSS vulnerability in MIRO Pivot.
- Configuration Mode: Support customizing order of `custom scalar input
  widget <./configuration_json_only.html#custom-input-widgets>`__ tab.
- MIRO Server: Fix issue where MIRO incorrectly displayed the error *An
  app with this ID already exists* after uploading an app failed for
  some reason (e.g. interrupted internet connection).
- Support for importing individual scalars via `custom import
  functions <./configuration_json_only.html#custom-import-export>`__.
- MIRO Desktop: Fix issue where custom environment could cause apps to
  not start anymore.

GAMS MIRO 2.10.2
~~~~~~~~~~~~~~~~

(2024-08-21)

- Allow configuration of `maximum upload size in MIRO
  Server <./server_setup.html#config-max-upload-size>`__.
- JSON schema validation: Allows strings in addition to length-1 arrays
  of strings. This fixes some issues where starting Configuration Mode
  could cause JSON validation to fail.

GAMS MIRO 2.10.1
~~~~~~~~~~~~~~~~

(2024-08-08)

- Improve error message when trying to render a map with flows that have
  incomplete latitude/longitude information.
- Fix multiple issues with MIRO apps that have no input symbols.
- Fix misformatted log output when running MIRO from Windows
  command-line interpreters.
- Fix json schema validation for some chart types in `pivot
  renderer <./charts.html#pivot-chart>`__ and `dashboard
  renderer <./charts.html#dashboard>`__.
- Fix json schema validation for value boxes in the `dashboard
  renderer <./charts.html#dashboard>`__ to allow for ``null`` values
  in the ``decimals`` property.
- `Dashboard <./charts.html#dashboard>`__: Since GAMS MIRO v2.10.0, all
  keys in the <modelname>.json file are sorted alphabetically when the
  Configuration Mode is started. In order to leave the order of
  charts/tables within a dataView in the dashboard unaffected by this,
  it is now possible to use an array of objects for the dataViews. More
  on this `here <./charts.html#dashboard-dataviews>`__.
- Fix crash when the app configuration has an invalid default comparison
  mode setting (``defCompMode`` property).

.. _breaking-changes-1:

Breaking changes
~~~~~~~~~~~~~~~~

- The long deprecated charting tool ``pivot`` (not MIRO Pivot) has been
  removed. Apps using this tool will no longer start. If you want to
  continue using the tool, please use a `custom
  renderer <./charts.html#custom-graphics>`__ instead.

.. _new-features-3:

New features
~~~~~~~~~~~~

- Add new renderer "`dashboard <./charts.html#dashboard>`__". A
  dashboard is an extensive and interactive tool that can display
  multiple charts and tables.
- MIRO Pivot:

  - Add new charting types: pie, doughnut, horizontal bar, horizontal
    stacked bar.
  - Add option to mix charting types (e.g. bar and line chart) as part
    of a `view <./charts.html#pivot-view>`__. Depending on the base
    chart type, one can now add additional bar/line series to the chart.

.. _minor-new-features-and-improvements-4:

Minor new features and improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Add an option to render the size of a chart in a
  `map <charts.html#map-chart>`__ proportional to its value(s).
- It is now possible to select how many symbols should be placed next to
  each other when a `symbol group <./configuration_symbols.html>`__ is
  displayed in the same tab.
- Add GAMSPy example model: vehicle routing with time windows.
- The JSON configuration file is now written with a consistent (sorted)
  order of keys when written from Configuration Mode. This makes it
  easier to track changes when the file is checked into version control.

.. _bug-fixes-3:

Bug fixes
~~~~~~~~~

- Fix sandbox modification marker not triggered sometimes when using
  editable MIRO Pivot input table.
- Fix issue where some datatable options did not work when used as a
  renderer for output data.
- Config Mode: Fix issue with misconfigured ``pivotCols`` option when
  selecting not to display a chart for an output symbol.
- Config Mode: Fix issue where the dropdown menu to select ``pivotCols``
  was not visible in certain situations.
- Fix issue where the valuebox configuration of output scalars was lost
  when the output scalars were displayed on the same tab with other
  symbols.
- Fix issue where setting ``readonly`` globally for all default input
  tables did not work.
- MIRO Server: Fix bad URL in `nginx example
  configuration <./server_setup.html#nginx-example>`__ for MIRO Server
  API: The location should be ``/api/`` instead of ``/api``.

.. _v29:

GAMS MIRO 2.9
-------------

(2024-02-21)

GAMS MIRO 2.9.3
~~~~~~~~~~~~~~~

(2024-07-17)

- Fix issue that could cause solve to fail due to a problem with the
  attachments module.

GAMS MIRO 2.9.2
~~~~~~~~~~~~~~~

(2024-04-18)

- Fix crash on macOS when submitting jobs to Engine.
- MIRO Pivot: Fix renderer triggered too often in certain situations
  resulting in view not being loaded properly.

GAMS MIRO 2.9.1
~~~~~~~~~~~~~~~

(2024-04-12)

- Support `PKCE extension to
  OpenID <https://tools.ietf.org/html/rfc7636>`__ in `MIRO
  Server <./server_setup.html>`__.
- MIRO Server: Fix issue where app logo was not properly displayed when
  using an `App ID <./deployment.html#app-id>`__.
- MIRO Server: Add missing `documentation for some security
  features <./server_setup.html#config-security-features>`__:
  *same-site-cookie*, *secure-cookies*, *frame-options*.
- Minor improvements to GAMSPy example models.
- Fix issue configuring widget for multi-dimensional sets.
- Fix issue where the mathematical typesetting in table labels did not
  work.

.. _breaking-changes-2:

Breaking changes
~~~~~~~~~~~~~~~~

- MIRO Server: Drop support for Windows. Existing customers can continue
  to update and use GAMS MIRO Server on Windows. However, we will no
  longer offer support for Windows for new customers.

.. _new-features-4:

New features
~~~~~~~~~~~~

- Add support for `GAMSPy </sales/gamspy_facts/>`__. Demo models can be
  found `here <./download.html#demo-section>`__ and the documentation
  has been updated to discuss GAMSPy as well.

.. _minor-new-features-and-improvements-5:

Minor new features and improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- When adding/selecting scenario tags, existing tags are now suggested
  and can be selected.
- Remove the limit of max. 50 events in a Gantt chart.
- Add option to `group
  widgets <./configuration_json_only.html#hcube-widget-groups>`__ in
  `Hypercube module <./hc-module.html>`__.
- Allow specifying `custom input
  widgets <./configuration_json_only.html#custom-input-widgets>`__ for
  scalar symbols.
- MIRO Desktop: Moved login screen for GAMS Engine from the MIRO app
  window to the `preferences <./deployment.html#sbs-customize-app>`__
  window.
- MIRO Desktop: Add support for logging in to GAMS Engine via *JWT* as
  well as *LDAP*, *OAuth2* and *OIDC* identity providers.

.. _bug-fixes-4:

Bug fixes
~~~~~~~~~

- MIRO Pivot: Fix widget getting stuck in certain situations when using
  domain filters.
- MIRO Pivot: Fix warning when using row summary functions.

.. _v28:

GAMS MIRO 2.8
-------------

(2023-08-11)

GAMS MIRO 2.8.3
~~~~~~~~~~~~~~~

(2024-02-21)

- Fix misleading handling of the slider range widget.
- Fix issue that could cause MIRO to crash in certain situations when
  importing job via the job list.
- Fix issue where creating Hypercube job from scenario with long name
  did not work.

GAMS MIRO 2.8.2
~~~~~~~~~~~~~~~

(2023-10-26)

- Fix crash in mobile mode when launching deployed apps with default
  input tables.
- Fix crash when using the ``pivotCols`` feature in input tables in
  combination with ``Inf``/``-Inf`` values.
- Fix crash when using the ``pivotCols`` feature in input tables in
  combination with duplicate records. The input table is now set to
  read-only and a warning is issued.
- When setting custom comparison mode as default, the ``Synchronize tabs`` button
  was erroneously displayed.

GAMS MIRO 2.8.1
~~~~~~~~~~~~~~~

(2023-10-04)

- MIRO Pivot: Fix text in view selection dropdown button overflows if
  view name is long.
- Fix issue where multi-dropdown menus were not working.
- MIRO Pivot: Fix issue where row summarization did not work when
  certain columns were aggregated.
- MIRO Pivot: Fix several issues with empty tables (no data).

.. _breaking-changes-3:

Breaking changes
~~~~~~~~~~~~~~~~

- Removed `backward dependencies (table
  filters) <./configuration_advanced.html#widgets-dependencies>`__ from
  dropdown widgets as well as the undocumented feature of having
  (forward) dependencies for checkboxes. Please use `custom input
  widgets <./configuration_json_only.html#custom-input-widgets>`__ to
  achieve this functionality.

.. _new-features-5:

New features
~~~~~~~~~~~~

- MIRO now allows you to access data from any (other) input widgets (not
  just scalar ones) in `custom input
  widgets <./configuration_json_only.html#custom-input-widgets>`__.

.. _minor-new-features-and-improvements-6:

Minor new features and improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- MIRO Pivot input tables now support the ``readonly`` option.
- Our scripts now use Docker Compose version 2, as version 1 has reached
  its end of life.
- Batch Load: MIRO now allows you to select a custom (static) prefix
  when using batch load columns for naming scenarios.
- `Custom data
  connector <./configuration_json_only.html#custom-import-export>`__
  functions now get a new argument: *customRendererDir* with the path to
  custom renderer directory.
- MIRO Pivot Tables now display the currently selected view in the view
  selection drop-down menu.
- Comparison modes: Allow `custom comparison
  modules <./configuration_json_only.html#custom-compare-mode>`__ to be
  selected as default comparison mode. Also, the default comparison mode
  is now preselected when selecting scenarios in the `Batch Load
  module <./start.html#scenario-loading>`__.

.. _bug-fixes-5:

Bug fixes
~~~~~~~~~

- Fix updating the logo of an existing app on MIRO Server.
- Fix issue where an error occurred when adding an app to MIRO Desktop
  when there were multiple files with the *\_logo* extension in the
  *static\_<modelname>* directory.
- Fix issue deploying app with `custom data
  connectors <./configuration_json_only.html#custom-import-export>`__.

.. _v27:

GAMS MIRO 2.7
-------------

(2023-03-30)

GAMS MIRO 2.7.1
~~~~~~~~~~~~~~~

(2023-06-08)

- Fix issue where MIRO Pivot displayed an out-of-bounds error if the
  table did not contain column headers.
- Fix crash when trying to load an LST file that contains non-UTF-8
  characters.

.. _minor-new-features-and-improvements-7:

Minor new features and improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Add option to `remove duplicate
  records <./start.html#data-cleaning>`__ from input data.
- MIRO Server: Add support for `OpenID
  Connect <./server_setup.html#server-auth-oidc>`__ identity providers
  (for authentication).
- MIRO Desktop: Add support for ARM64 (Apple silicon) processors for
  macOS.

.. _bug-fixes-6:

Bug fixes
~~~~~~~~~

- MIRO Server: Fix issue where the custom page title was not applied on
  the login page.
- Fix regression introduced with MIRO 2.6.0 where the content of the
  MIRO log file was duplicated multiple times when the (normal) log file
  was disabled.

.. _v26:

GAMS MIRO 2.6
-------------

(2022-12-22)

.. _minor-new-features-and-improvements-8:

Minor new features and improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- When using GAMS MIRO with a `GAMS Engine <https://gams.com/engine>`__
  backend, the currently active job can now be detached. After
  detaching, the job can still be accessed via the job list.
- Accessing attachment data from `custom scenario comparison
  modules <./configuration_json_only.html#custom-compare-mode>`__ is now
  possible via the new CustomComparisonData$getAttachmentData() method.
- Add settings to customize the number of tabs to display before
  collapsing more tabs under a ``More »`` drop-down tab in the
  input/output sections and in comparison modes.
- `MIRO Server API <./server_api.html>`__: Add ``GET /configuration`` endpoint,
  which returns version information, and ``POST /login`` endpoint to generate
  an authentication token and retrieve access groups.
- Carriage returns are now correctly displayed in the logs in the GAMS
  interaction section (i.e. the previous line is updated instead of
  carriage returns being ignored).
- The `appId <./deployment.html#app-id>`__ introduced with MIRO 2.5 can
  now be used on MIRO Desktop and MIRO Server to register multiple MIRO
  apps based on the same model.

.. _bug-fixes-7:

Bug fixes
~~~~~~~~~

- Fix issue where attachment accessibility was not updated in certain
  situations.
- Fix crash when deploying an app with an app info file that does not
  include ``appId`` field.
- When importing data from a local file (``csv``, ``gdx`` or
  ``xlsx``) and selecting "Replace" as the import strategy, data of
  missing symbols is now skipped instead of being cleared.
- Fix crash in Configuration Mode when configuring graphs (value boxes)
  for output scalars.
- MIRO Desktop: Fix color theme selection did not work when the language
  was set to something other than English.
- MIRO Server: Fix issue that could cause the app logo to become
  corrupted.

.. _v25:

GAMS MIRO 2.5
-------------

(2022-09-19)

GAMS MIRO 2.5.3
~~~~~~~~~~~~~~~

(2022-12-02)

- MIRO Desktop: Fix issue introduced with MIRO 2.5.2 where updating apps
  did not work in certain situations.

GAMS MIRO 2.5.2
~~~~~~~~~~~~~~~

(2022-11-24)

- Fix issue on Windows that caused apps to fail to launch when GAMS path
  was not set and could not be found in the default location
  (``C:\GAMS``).
- Fix issue with `migration wizard <./deployment.html#db-migration>`__
  not working in MIRO Server.
- Fix issue with apps not launching if the main GMS file contains
  certain characters such as spaces. An error is now issued when trying
  to deploy or import such apps. To deploy such applications, the
  `appId <./deployment.html#app-id>`__ field must be specified manually
  via the
  `app_info.json <./deployment.html#default-title-description>`__ file.
- Fix `Hypercube jobs <./hc-module.html>`__ not imported with correct
  username on MIRO Server.

GAMS MIRO 2.5.1
~~~~~~~~~~~~~~~

(2022-10-12)

- Fix ``NA`` values not being ignored when calculating table summary
  for MIRO Pivot Tables.
- Fix filtered elements not being visible on small screens in MIRO Pivot
  presentation mode.

.. _minor-new-features-and-improvements-9:

Minor new features and improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: Add option to show
  table summary (sum, mean, count, ... for rows/columns).
- MIRO Server: When a user without permissions on the MIRO Server
  namespace opens an application, it is now launched in read-only mode,
  i.e. the GAMS interaction section is hidden and the sandbox scenario
  cannot be saved/deleted. `A new
  setting <./server_setup.html#config-disable-readonly>`__ allows you to
  disable this behavior.

.. _bug-fixes-8:

Bug fixes
~~~~~~~~~

- Fix issue in Configuration Mode where the `fixed
  columns <./charts.html#fixed-columns>`__ setting was not applied for
  input tables.
- Fix issue where sorting or downloading data from an editable MIRO
  Pivot input table could display incorrect data. However, the correct
  data was communicated with GAMS.
- MIRO Desktop: Fix issue where importing a MIROSCEN file by
  double-clicking caused an infinite loop with a prompt to overwrite the
  scenario if it already exists.
- MIRO Desktop: Fix issue where importing data files under Windows
  caused MIRO to hang indefinitely.

.. _v24:

GAMS MIRO 2.4
-------------

(2022-06-16)

GAMS MIRO 2.4.1
~~~~~~~~~~~~~~~

(2022-08-02)

- Fix crash when clicking button to submit a new asynchronous job.

.. _new-features-6:

New features
~~~~~~~~~~~~

- Add option to specify `custom scenario comparison
  modules <./configuration_json_only.html#custom-compare-mode>`__.

.. _minor-new-features-and-improvements-10:

Minor new features and improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Access to views, attachments, and other metadata of the sandbox
  scenario is now possible within `custom data
  connectors <./configuration_json_only.html#custom-import-export>`__.
- Add option to use a second Y-axis as well as axis ranges for bar
  charts.
- A notification is now displayed when new output attachments are added.
- It is now possible to manually select specific input symbols to be
  imported from MIROSCEN files or database scenarios. In this case, the
  metadata (including views and attachments) will not be imported.
- MIRO Server: When MIRO Server is updated and custom packages are
  present in the ``additional_packages`` file, MIRO now asks if a new custom image
  should be created as part of the MIRO update.
- MIRO Server: The Dockerfile used to extend the MIRO UI image with
  custom packages has been updated to install the R build tools required
  for certain R packages.
- MIRO Server: Add new ``backup`` and ``restore`` commands to ``miro-server``
  setup script to allow backing up and restoring a MIRO Server instance
  (Linux only).
- MIRO Pivot Tables: Symbol dimensions, which do not add data to the
  current view, are now slightly grayed out.

.. _bug-fixes-9:

Bug fixes
~~~~~~~~~

- Fix issue where using a label for an output symbol caused the chart to
  be hidden.

.. _v23:

GAMS MIRO 2.3
-------------

(2022-03-14)

GAMS MIRO 2.3.1
~~~~~~~~~~~~~~~

(2022-04-03)

- MIRO Server: Third-party dependency update that includes a fix for
  CVE-2022-22965 (Spring RCE vulnerability). Currently, there is no
  known way to exploit MIRO Server. Nevertheless, we strongly recommend
  updating MIRO Server as soon as possible!

.. _breaking-changes-4:

Breaking changes
~~~~~~~~~~~~~~~~

- The `remote data import and export
  feature <./configuration_json_only.html#remote-import-export>`__ has
  been deprecated and will be removed in one of the next MIRO releases.

.. _new-features-7:

New features
~~~~~~~~~~~~

- Add option to specify `custom data
  connectors <./configuration_json_only.html#custom-import-export>`__.
- It is now possible to `sign <./deployment.html#sbs-sign-app>`__ MIRO
  apps to ensure that the app was created by a verified developer. A new
  setting has been added in MIRO Server to `force apps to be
  signed <./server_setup.html#config-enforce-signed>`__. This ensures
  that only apps from trusted developers can be imported.
- Add option to specify `views <./start.html#metadata-views>`__ globally
  for each scenario.

.. _minor-new-features-and-improvements-11:

Minor new features and improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Add option to show `tooltips <./widgets.html#slider-option-tooltip>`__
  for input widgets.
- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: Charts can now be
  zoomed and panned.
- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: When configuring a
  new view, the user can now select a logarithmic scale for the y-axis.
- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: New renderer: time
  series chart.
- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: Stacked bar/area
  charts now display tooltips for all series in a chart.
- Custom renderers for input symbols can now access data from other
  input datasets.
- Add option to `validate
  columns <./configuration_json_only.html#table-validate-columns>`__ of
  input tables against predefined criteria.
- Add option to specify an external view as the default view for `MIRO
  Pivot Tables <./charts.html#pivot-chart>`__. If this view is not
  present, MIRO Pivot falls back to the default configuration.
- `Add
  option <./configuration_json_only.html#pivot-compare-default-view>`__
  to specify an external view as the default view for the `Pivot compare
  mode <./start.html#compare-pivot>`__.
- Add various color themes that can be used for `MIRO
  Desktop <./deployment.html#sbs-customize-app>`__ and for `MIRO
  Server <./server_setup.html#server-customize>`__.
- Improved organization in the Configuration Mode.

  - The `general settings <./configuration_general.html>`__ have been
    revised and divided into new categories and additional
    subcategories.
  - The configuration of input tables is now done in section `Input
    Widgets <./widgets.html#widget-table>`__.
  - The option to enable/disable manual column resizing in input tables
    has been removed (always enabled).

.. _bug-fixes-10:

Bug fixes
~~~~~~~~~

- Fix issue importing Excel spreadsheet with index that contains neither
  *rdim* nor *cdim* information.
- Fix issue displaying large text entries via job list section.
- `MIRO Pivot tables <./charts.html#pivot-chart>`__: Fix issue where
  editable tables were not correctly marked as "read-only"/"editable" in
  certain situations.
- Compare mode: Fix crash when using scalar input symbols in custom
  input widget.
- Fix issue where downloading attachments did not work in certain
  situations.
- Fix issue where attachments were duplicated in certain situations.
- Fix issue where MIRO crashed when starting Configuration Mode with
  certain dropdown configurations.
- Fix issue with integer column names in GAMS symbol headers (e.g. a
  GAMS table with dimensions "i", "1", "2").
- Fix issue where an incorrect symbol alias was used in some situations
  when saving a new GAMS option or double dash parameter in
  Configuration Mode.
- Fix issue where an incorrect label was used in some situations when
  saving a graph configuration in Configuration Mode.

.. _v22:

GAMS MIRO 2.2
-------------

(2021-12-08)

GAMS MIRO 2.2.2
~~~~~~~~~~~~~~~

(2022-01-05)

- Support assigning a machine instance to be used for a job when
  submitting it to GAMS Engine.
- A warning is now issued when running out of quotas soon when
  submitting to GAMS Engine. If the quotas are exceeded, a more
  informative error is displayed.
- Fix issue saving scenario of a model without solve statement but with
  the saveTraceFile option enabled.
- Fix several issues related to keyboard shortcuts.
- Server: Fix crash in admin panel when app is updated and the database
  needs to be migrated.

GAMS MIRO 2.2.1
~~~~~~~~~~~~~~~

(2021-12-09)

- Fix issue where the MIRO window did not open in development mode (e.g.
  when launched via GAMS Studio) when using the built-in browser.
- Configuration Mode: Fix issue where communication of additional input
  datasets with a custom renderer did not work properly in certain
  situations.

.. _breaking-changes-5:

Breaking changes
~~~~~~~~~~~~~~~~

- The Hypercube Mode was removed. You can now `submit Hypercube
  jobs <./hc-module.html>`__ from the Base Mode if you are using GAMS
  MIRO with a `GAMS Engine <https://gams.com/engine>`__ backend (*GAMS
  MIRO Server* or *GAMS MIRO Desktop - boosted by GAMS Engine*).
- An error is now issued if the MIRO app name (filename of the main .gms
  file) starts with: '~$'.

.. _new-features-8:

New features
~~~~~~~~~~~~

- You can now define a `custom input
  widget <./configuration_json_only.html#custom-input-widgets>`__ for
  multiple symbols.
- Apps and scenarios in `MIRO Server <./server.html>`__ can now be
  managed via a new `REST API <./server_api.html>`__. Note: If you plan
  to update your MIRO Server instance to use this new API, the *auth*
  section of the *docker-compose.yml* file must also be updated. You can
  download the latest *docker-compose.yml* file
  `here <./download.html#miro-server>`__.

.. _minor-new-features-and-improvements-12:

Minor new features and improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- The appearance of MIRO on devices with small screens (smartphones) has
  been significantly improved. In addition to general improvements,
  there are the following changes on small screens (<768px width):

  - `Big data tables <./widgets.html#input-bigdata-table>`__ are now
    displayed by default for input symbols.
  - In `MIRO Pivot tables <./charts.html#pivot-chart>`__, columns are
    not fixed anymore.

- When importing data from local files (CSV, Excel or GDX), users now
  have the option to choose whether to replace existing data or merge
  the new data with the existing data.
- Add option to show/hide `fixed
  columns <./charts.html#fixed-columns>`__ in MIRO pivot.
- MIRO now allows you to add a `custom web app
  manifest <./deployment.html#web-app-manifest>`__ to your app to
  customize the logo and title when your app is added to a user's home
  screen on mobile devices.
- The `custom renderer editor in Configuration
  Mode <./charts.html#custom-graphics>`__ now supports full-screen mode.
- Output renderers can now be annotated with Markdown.
- Log/MIRO log and lst files can now be downloaded directly from the
  GAMS interaction section.
- MIRO now supports reading domain sets (``dSet``) from
  `Excel <./start.html#excel-import-rules>`__ files.
- With custom renderers and custom input widgets you can now `add and
  access
  attachments <./configuration_advanced.html#custom-renderer-attachments>`__.
- It is now possible to update an app: Simply drag&drop an app onto an
  existing one or double-click a MIROAPP file.
- It is now possible to add scenario data (gdx/csv/xls/miroscen) to an
  existing app via drag&drop.
- It is now possible to add a `custom
  stylesheet <./configuration_general.html#custom-css>`__ to a MIRO app.
- `MIRO Server <./server.html>`__: Improve visibility and accessibility
  of the navigation bar.
- `MIRO Server <./server.html>`__: Remove 'Containers' panel for admin
  users.
- `MIRO Server <./server.html>`__: Groups that contain uppercase letters
  in their name are now ignored by MIRO Server because they are
  currently not supported.

.. _bug-fixes-11:

Bug fixes
~~~~~~~~~

- Fix issue in Configuration Mode where the option to enable `pivot
  presentation
  mode <./configuration_general.html#enable-presentation>`__ by default
  was not applied correctly.
- Fix issue with the `Batch Load
  module <./start.html#scenario-loading>`__ when singleton sets but no
  scalars are defined.
- Linux: Fix title and icon of the application.
- Fix issue where attachments in the sandbox were incorrectly
  overwritten with attachments from the database in certain situations.
- Fix issue where attachments in the sandbox were not cleaned up
  properly in certain situations.
- Fix issue where downloading CSV from MIRO Pivot did not work when
  presentation mode was enabled by default.
- Fix issue in MIRO Server where sharing scenarios with groups whose
  names contain uppercase letters did not work. Groups are
  case-insensitive in GAMS MIRO Server and are always displayed with
  lowercase letters.
- `MIRO Server <./server.html>`__: Fix issue where the navigation bar
  was not visible on mobile devices.
- Fix issue where valid MIROSCEN files were not imported in certain
  situations.
- Fix issue in Configuration Mode where the configuration for output
  tables without graphics to display was not saved correctly in some
  situations.
- Fix issue in the `Batch Load module <./start.html#scenario-loading>`__
  where fetching data caused an error when using multiple empty
  condition blocks.

.. _v21:

GAMS MIRO 2.1
-------------

(2021-09-07)

GAMS MIRO 2.1.3
~~~~~~~~~~~~~~~

(2021-10-18)

- MIRO Server: Fix issue that caused MIRO to hang in certain situations
  when adding new MIRO applications.
- `Migration Wizard <./deployment.html#db-migration>`__: Fix issue that
  caused database migration to fail in certain situations.
- Fix out-of-bounds error when reading from Excel sheets.
- Fix issue with empty default input tables.

GAMS MIRO 2.1.2
~~~~~~~~~~~~~~~

(2021-10-11)

- Fix issue importing data from Excel with multiple ``rdim`` AND
  ``cdim``.
- Fix unnecessary warning when importing data from empty Excel sheets.
- Fix issue where existing attachments were removed when the maximum
  number of attachments was exceeded.
- Custom scripts are now automatically included when the MIRO app is
  deployed and no longer need to be manually added to the model assembly
  file.
- Linux: Fix issue opening MIRO in default browser.
- Linux: Fix issue displaying charts in MIRO Pivot.
- Fix issue where the MIRO Pivot table was not updated after editing in
  large input tables was enabled.

GAMS MIRO 2.1.1
~~~~~~~~~~~~~~~

(2021-09-17)

- Empty rows in default input tables are now ignored.
- Fix issue where reading data with 0 records from Excel would not work
  in certain situations.
- Fix issue with default input tables that caused old data to be
  communicated in certain situations.
- Fix issue with migrating old database schemas to MIRO 2.0 when
  database contains Hypercube scalars.
- Fix issue where incorrect filter elements were displayed in MIRO Pivot
  `presentation mode <./charts.html#presentation-mode>`__ in certain
  situations.
- MIRO Server: Fix issue where collapsed navigation bar would not open
  on mobile devices.

Deprecated features
~~~~~~~~~~~~~~~~~~~

- The Hypercube Mode is deprecated and will be removed in a future
  version of GAMS MIRO. You can now `submit Hypercube
  jobs <./hc-module.html>`__ from the Base Mode if you are using GAMS
  MIRO with a `GAMS Engine <https://gams.com/engine>`__ backend (*GAMS
  MIRO Server* or *GAMS MIRO Desktop - boosted by GAMS Engine*).

.. _new-features-9:

New features
~~~~~~~~~~~~

- Hypercube jobs can now be submitted and imported from the Base Mode if
  MIRO is connected to `GAMS Engine <https://gams.com/engine>`__.

.. _minor-new-features-and-improvements-13:

Minor new features and improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: When adding new
  views for bar charts, stacked bar charts, line charts and radar
  charts, you now have the option to assign custom colors to the
  different series.
- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: Views can now be
  edited.
- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: When downloading a
  CSV file while the switch to hide empty columns is active, hidden
  columns are not exported.
- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: New `presentation
  mode <./charts.html#presentation-mode>`__ that hides all pivot
  controls and puts more focus on the actual chart or table.
- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: New aggregation
  function: standard deviation.
- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: New renderers:
  scatter chart, area chart and stacked area chart.
- It is now possible to overwrite aliases of scalars in Configuration
  Mode.
- It is now possible to specify the number of decimal places per numeric
  column of default input tables in Configuration Mode.
- Whitespace and comments are now preserved when a custom renderer is
  saved via the `custom renderer editor in Configuration
  Mode <./charts.html#custom-graphics>`__.
- When downloading scenarios via the `Batch Load
  module <./start.html#scenario-loading>`__, you can now select the file
  format from a drop-down menu rather than via a new dialog.
- When adding a MIRO app to the library that requires additional R
  packages from CRAN, the user is now prompted before these packages are
  installed.
- When logging into `GAMS Engine <https://gams.com/engine>`__ and using
  a registered model, MIRO now also checks if the model exists in the
  namespace to provide a more informative error message.
- MIRO Server: Users who do not have permissions in the Engine namespace
  associated with MIRO Server are now authenticated when they can see
  some models. Users who do not have permissions in the namespace can
  see a model if they can see a group that is associated with a model in
  the namespace. Assigning models to groups is supported in Engine
  version 21.09.14 and later.
- Aliases for widgets are removed from Configuration Mode. The symbol
  aliases defined in the *General settings* section are used instead.
- Scenarios loaded from the `Batch Load
  module <./start.html#scenario-loading>`__ into the comparison modes
  can now be named based on one or more columns of the Batch Load table.
- Importing Excel files is now significantly faster when there are many
  symbols on a worksheet.

.. _bug-fixes-12:

Bug fixes
~~~~~~~~~

- Fix several issues with MIRO apps without external input data.
- Fix issues with configuring `MIRO Pivot
  Tables <./charts.html#pivot-chart>`__ in Configuration Mode:
  configuration was reset if a different aggregation function was
  specified and aggregation functions were not complete in certain
  situations.
- Fix wrong aggregation functions for input/output scalars in the `Pivot
  compare mode <./start.html#compare-pivot>`__.
- Fix issue with view IDs containing whitespace characters at the
  beginning or end.
- Fix incomplete error checks for defaut views for `MIRO Pivot
  renderers <./charts.html#pivot-chart>`__.
- Fix issues with installing additional packages that contain
  precompiled binaries on macOS.
- Configuration Mode: Hidden output scalars were not correctly forwarded
  to the chart preview, which caused errors when trying to access them.
- Fix an issue where hidden output scalars were not transmitted/saved in
  certain situations.
- Fix an issue with launching some MIRO applications that were deployed
  with MIRO 1.x.
- Fix issue where MIRO would not start when a set was configured to use
  the `MIRO pivot renderer <./charts.html#pivot-chart>`__.
- Fix issue where row order was not preserved when loading views in
  `MIRO Pivot renderers <./charts.html#pivot-chart>`__.
- Fix issue where incorrect column headers were displayed in output
  tables.
- When loading scenarios in split/tab comparison mode, scenarios that
  have already been loaded are now filtered out.
- Fix issue with exporting GDX files via the `Batch Load
  module <./start.html#scenario-loading>`__ in certain situations.
- Fix issue with detecting CSV delimiter in certain situations.
- Fix issue saving views in pivot comparison mode when some output
  symbols are hidden.
- Fix issue where dirty flag would not be set in certain situations.

.. _v20:

GAMS MIRO 2.0
-------------

(2021-06-15)

GAMS MIRO 2.0.1
~~~~~~~~~~~~~~~

(2021-09-07)

- Fix issue with loading values of slider ranges via the `Batch Load
  module <./start.html#scenario-loading>`__.
- Fix issue where the model sometimes did not terminate when the log
  file was hidden.
- Some internal database changes for MIRO Server.

.. _breaking-changes-6:

Breaking changes
~~~~~~~~~~~~~~~~

- The database layout that MIRO uses internally has been changed
  significantly. At the first start MIRO checks if there is an existing
  MIRO database and tries to migrate this database automatically to the
  new layout. A backup of the old database is kept in case something
  goes wrong. Some data can not be migrated automatically. The first
  time you start your MIRO app, you will be prompted with a migration
  dialog if some data needs to be migrated manually. Sliders that have
  been expanded in Hypercube Mode job configurations cannot be migrated!
- An error is now issued if the MIRO app name (filename of the main .gms
  file) is longer than 60 characters.
- Apps that use the (previously undocumented) *dropdown columns* feature
  in input tables must be redeployed.

.. _deprecated-features-1:

Deprecated features
~~~~~~~~~~~~~~~~~~~

- The API for `custom input
  widgets <./configuration_json_only.html#custom-input-widgets>`__ has
  been updated. Data is now passed in the form of reactive expressions
  instead of tibbles. The old API version is still the default for
  backward compatibility. However, you will now receive a deprecation
  warning if you are not using the new API version. The old API will be
  removed in a future MIRO release.

.. _new-features-10:

New features
~~~~~~~~~~~~

- `MIRO Server <./server.html>`__ is officially released.
- Custom renderers can now be edited with a `live preview via the
  Configuration Mode <./charts.html#custom-graphics>`__. This should
  increase productivity when developing custom renderers. Your existing
  renderers will still work, but will not be recognized by this new
  editor. To learn how to migrate them to be compatible with the new
  editor, see
  `here <./configuration_advanced.html#migrate-custom-renderers>`__.
- Scenarios in Base Mode can now be queried, loaded and exported via the
  new `Batch Load module <./start.html#scenario-loading>`__ that was
  previously only available in Hypercube Mode.

.. _minor-new-features-and-improvements-14:

Minor new features and improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Migrating database tables is now significantly more powerful thanks to
  the new `Migration Wizard <./deployment.html#db-migration>`__.
- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: Aggregation
  function: ``min`` also added for sets.
- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: It is now possible
  to select and drag multiple indexes at once.
- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: The maximum number
  of rendered columns has been reduced from 500 to 300.
- The error message reporting duplicate records when exporting a GDX
  file now shows the symbol alias instead of the symbol name and is
  localized.
- Input/output groups with more than 6 tabs now get a "More..." tab that
  combines subsequent tabs.
- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: When adding new
  views, existing views with the same name can now be overwritten.
- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: When adding new
  views for bar charts, stacked bar charts or line charts, you now have
  the option to assign a chart title as well as labels for the x- and
  y-axis.
- Improve performance when starting MIRO: input graphs/output tables are
  now generated dynamically.
- Improve performance when loading scenarios in scenario comparison
  mode: Data is now only fetched and graphs only rendered when required.
- Add "Close All" button in the tab view comparison mode to close all
  currently opened scenarios.
- Windows: Removed progress bar when starting MIRO.
- When submitting a job to `GAMS Engine <https://gams.com/engine>`__,
  the MIRO sandbox is now restored when importing the results. This
  means that attachments, views, tags, etc. are preserved.
- You can now select the default renderer for all output data via the
  Configuration Mode (pivot table or table).
- Changing only the views of a scenario in the data\_<modelname> folder
  now updates the scenario in the database.
- The new `custom input widget
  API <./configuration_json_only.html#custom-input-widgets>`__ now
  passes data wrapped in reactive expressions. Also, access to values
  from scalar widgets is now supported.
- In addition to HTML, MIRO can now display Markdown files generated by
  custom analysis scripts.
- It is now possible to `select and download multiple files from the
  temporary
  directory <./configuration_general.html#general-temp-dir>`__.
- Add option to `fix table columns to the
  left <./widgets.html#table-option-fixedColumnsLeft>`__ when scrolling
  horizontally.
- You can now specify a `custom
  environment <./deployment.html#custom-environments>`__ for your MIRO
  apps.
- Scenario name and tags can now be edited via the `Batch Load
  module <./start.html#scenario-loading>`__.

.. _bug-fixes-13:

Bug fixes
~~~~~~~~~

- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: Fix initialization
  issue that caused incorrect table layout in certain situations.
- Fix crash in metadata dialog that occurred when a MIRO app was
  configured without tabular input symbols.
- Fix issue where switching to the table or heatmap renderer in MIRO
  Pivot did not work in certain situations.
- When loading a view with heatmap renderer in MIRO Pivot, "Table" was
  incorrectly selected as renderer.
- Fix issue where keyboard shortcuts to navigate tabset panels in `pivot
  view <./start.html#compare-pivot>`__ comparison mode did not work.

.. _v13:

GAMS MIRO 1.3
-------------

(2021-02-05)

GAMS MIRO 1.3.2
~~~~~~~~~~~~~~~

(2021-03-10)

- Fix crash in Configuration Mode when adding output groups.

GAMS MIRO 1.3.1
~~~~~~~~~~~~~~~

(2021-02-18)

- Fix issue with downloading attachmets from sandbox in certain
  situations.
- Fix issue with detecting inconsistent database tables when running on
  MIRO Server.
- Jobs that failed to produce any output (e.g. due to a GAMS compilation
  error) can now still be downloaded when run on MIRO Server.
- Remove job results from GAMS Engine when job is discarded via job
  list.
- Jobs that are queued and not yet running are now shown as such in the
  job list.
- If the sandbox is changed while a (synchronous) job is running, a
  warning is now displayed before the results are imported into the
  sandbox.
- Fix issue when importing apps deployed with the "single user" or
  "local multi-user" option into the MIRO library.
- Fix issue with attaching the log file of a model containing uppercase
  letters on case-sensitive file systems.
- Closing a scenario now clears `MIRO log
  errors <./configuration_general.html#miro-log>`__ and the GAMS
  interaction section.
- Fix issue with reading an Excel workbook without cdim declared in
  index sheet.
- When finding empty records in an Excel workbook, the value cells are
  now ignored. This is consistent with the behavior of GDXXRW.
- It is now possible to import ``xlsm`` files as well.
- Symbols that are not mentioned in the index sheet when importing an
  Excel workbook are now skipped. Previously they were imported as empty
  tables.
- Increase maximum size for attachments to 50MB.
- Increase the contrast of the charts in `MIRO pivot
  tables <./charts.html#pivot-chart>`__ in dark mode.
- The button for selecting the comparison mode now shows the currently
  active mode.
- Reading symbol names from the Excel layout sheet is now
  case-insensitive.

.. _breaking-changes-7:

Breaking changes
~~~~~~~~~~~~~~~~

- When writing sets/parameters with duplicate records to GDX file, an
  error is now issued instead of silently ignoring the duplicates.
- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: Scalar tables are
  no longer treated as sets (*Scalar Value* column is treated as set
  text column). Instead, scalar values are converted to floating point
  values. Note that values of singleton sets that cannot be converted to
  a floating point value become NA.

.. _new-features-11:

New features
~~~~~~~~~~~~

- MIRO scenarios can now be imported and exported as MIROSCEN files. In
  contrast to other supported data files like GDX or Excel, a MIROSCEN
  file contains all scenario metadata like views and attachments.
- `Views <./start.html#metadata-views>`__ can now be used in `pivot
  view <./start.html#compare-pivot>`__ comparison mode.
- MIRO now supports importing Excel (xls/xlsx) files with an index
  sheet. The syntax is very similar to
  `GDXXRW <https://www.gams.com/latest/docs/T_GDXXRW.html>`__. Read
  `here <./start.html#excel-import-rules>`__ for more details.
- MIRO now supports delimited text files other than CSV. Currently
  supported delimiters are: ``,`` ``tab`` ``;`` ``|``
  ``:``. MIRO tries to detect the delimiter automatically and
  resorts to manual selection if it cannot uniquely determine the
  delimiter. In addition, MIRO supports the selection of a user-defined
  mapping of column headings from the delimited text file to symbol
  headings.

.. _minor-new-features-and-improvements-15:

Minor new features and improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: Add option to
  export charts as PNG.
- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: Add option to
  visualize data as heatmap.
- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: Add option to hide
  empty columns. This option is also available in the `Pivot compare
  mode <./start.html#compare-pivot>`__.
- `MIRO Pivot Tables <./charts.html#pivot-chart>`__: If a view is loaded
  with a filter value that is not found in the current scenario, an info
  box is now displayed.
- Dropdown menus in the input/output tab "More..." and the view list in
  MIRO Pivot can now be filtered.
- Sandbox scenario can now be loaded in `tab
  view <./start.html#compare-tab>`__ and `pivot
  view <./start.html#compare-pivot>`__ comparison modes.
- Add button to refresh sandbox scenario in comparison mode.
- Remove results from GAMS Engine after they are downloaded, freeing up
  resources.
- Increase timeout when downloading results from GAMS Engine to 10
  hours.
- Rename *Compare mode* button to `Synchronize
  tabs <./start.html#synchronize-tabs>`__.
- Move `group domain option <./charts.html#group-domain>`__ in graph
  configuration to section *ydata*.
- When comparing base scenarios in Hypercube Mode, they now get the
  names assigned to them in Base Mode.
- When comparing scenarios generated in Hypercube Mode, they now get the
  names: ``HC (1)``, ``HC (2)`` etc.
- Add a dropdown button to select between tab and pivot comparison modes
  when comparing scenarios in Hypercube Mode.
- When exporting spreadsheets, MIRO now attaches an `index
  sheet <./start.html#excel-import-rules>`__ with the layout of the
  exported data.
- In a `custom
  renderer <./configuration_advanced.html#custom-renderers>`__, the
  values of all output scalars can now be accessed via the argument
  ``outputScalarsFull``.
- It is now possible to pivot columns in output tables as well.
- Show "More..." tab only if there are at least 2 tabs in this group.
- An empty textinput widget for a scalar GAMS symbol is now always
  communicated as empty string.
- An empty numeric input widget is now always communicated with its
  default value. A placeholder is displayed if no value has been set.
- Custom aliases for GAMS symbols as well as symbol domains are now
  automatically removed when the Configuration Mode is started and the
  symbols for these custom aliases are no longer part of the data
  contract.
- Results of Hypercube jobs solved on `GAMS
  Engine <https://www.gams.com/engine/>`__ are now deleted after
  download.
- Add new example app: *Cutting Stock Problem*.

.. _bug-fixes-14:

Bug fixes
~~~~~~~~~

- Fix issue where views of a scenario were not loaded in tab/split
  comparison mode.
- Fix memory leak with views opened in tab/split comparison mode.
- Fix issue where the y2 (right) axis of a time series diagram was not
  visible when using the `group domain
  option <./charts.html#group-domain>`__.
- Fix rare case where a confusing error message appeared in the log
  after solving the model.
- MIRO Pivot: Fix issue where only "count" was selectable as aggregation
  function for variables and equations.
- Fix incorrect switch to pivot view when comparing scenarios in
  Hypercube Mode and the current mode is split view. Now switches to tab
  view correctly.
- Fix issue with solving Hypercube jobs with attachments.
- Fix issue with solving Hypercube jobs with the
  `clearValue <./widgets.html#dropdown-option-clearvalue>`__ option.
- The pivot comparison mode is now better integrated into the Hypercube
  Mode.
- Fix issue when importing/exporting slider and dropdown data from
  Hypercube job configurations.
- Fix import of a single scalar from CSV file.
- Fix issue where a numeric input widget would send wrong values in
  certain situations.
- Configuration Mode: When adding a new input widget group, the group
  name was not added to the list of input tabs used to set the tab
  order.
- Fix issue in Configuration Mode where the wrong widgets ended up in a
  widget group in certain situations.
- Date and date range widgets without selected date return an empty
  string now instead of NA.
- Fix checkbox return value to be 1/0 instead of TRUE/FALSE in Hypercube
  Mode.
- Fix job status and import of Hypercube jobs when using `GAMS
  Engine <https://www.gams.com/engine>`__ in Hypercube Mode.
- Fix issue where submitting a Hypercube job caused an error when having
  configured slider that is not expanded in Hypercube Mode.
- Fix issue where numericinput would send wrong values in certain
  situatios.
- Fix issue where custom R packages were reinstalled every time MIRO was
  restarted.
- Fix issue where setting custom MIRO app/log paths did not work.
- Fix issue where scrollbar was not visible in MIRO library on Windows.
- Fix issue on macOS that caused an error when trying to deploy apps in
  both modes.

.. _v12:

GAMS MIRO 1.2
-------------

(2020-10-05)

GAMS MIRO 1.2.3
~~~~~~~~~~~~~~~

(2020-11-16)

- Fix issue with arrays (e.g. y-data in a bar chart) in Configuration
  Mode sometimes not expanding.
- Fix regression introduced with MIRO 1.2: In Hypercube Mode, scenarios
  loaded in the Tab View would not appear in the "Local" tab in the
  Split View.
- Fix issue in Pivot Compare mode where domains were displayed as "NA"
  in the scalar input table in certain situations.
- Improve validation of Engine URL.

GAMS MIRO 1.2.2
~~~~~~~~~~~~~~~

(2020-11-10)

- Fix issue with views whose names are longer than 54 characters.
- Fix issue with scenario comparison when scalars table and widgets are
  in separate input groups.
- Fix issue where dialogs for adding/renaming columns in standard input
  tables did not work in the integrated browser.
- MIRO Pivot: Improve layout if there are many views (> 15).
- Fix a race condition that could cause stale input data to be sent to
  custom renderer functions.
- Fix issue when column in default input table was both pivoted and
  declared as readonly.
- Fix issue with input widgets being part of input groups.
- Fix a rare case where scalars were not imported when importing data
  from local files.
- Force the tab comparison mode the default comparison mode in Hypercube
  Mode because the scenarios selected for comparison will be loaded in
  this mode (this overwrites the *defCompMode* setting).
- Fix issue where downloading attachments did not work properly in
  certain situations.

GAMS MIRO 1.2.1
~~~~~~~~~~~~~~~

(2020-10-22)

- Add option to configure a custom icon for a valuebox by adding a new
  choice to the dropdown menu.
- Improve readability of symbol configuration in Configuration Mode.
- Fix issue where import of data from GDX for a dropdown with clearValue
  option set to TRUE did not work.
- Fix issues with remote execution on GAMS Engine and uppercase letters
  in main GMS file.
- Fix issue in Configuration Mode (valuebox configuration) where the
  user gets an error when clicking on *save* twice.
- When removing database tables, show only one dialog instead of two.
- Fix issue in the MIRO Pivot renderer where filter dropdown would be
  irresponsive after loading view with unknown filters.
- Fix issue in the MIRO Pivot renderer where table/charts would be
  rendered more often than necessary.
- MIRO Pivot renderer: Change the settings of bar charts so that they
  always start at 0, and of line charts so that they have no
  interpolation.
- Fix issue that caused MIRO to crash when loading app with default
  scalars table on first tab.
- Fix issue that caused multiple symbols in custom renderers not to work
  properly.
- Fix issue that caused path to custom renderer function not to be
  passed correctly.
- Fix issue in Configuration Mode that caused MIRO to crash in some
  situations when resetting a table configuration for a symbol.
- Fix issue in Configuration Mode that caused label of tables not to be
  applied in certain situations.
- Numbers in default input tables with pivoted columns are now formatted
  correctly.

.. _breaking-changes-8:

Breaking changes
~~~~~~~~~~~~~~~~

- Additional packages in custom renderers are now installed in the MIRO
  workspace. These packages are no longer attached because this could
  lead to conflicts with packages required by MIRO. Read
  `here <./configuration_advanced.html#additional-r-packages>`__ for
  more details.

.. _new-features-12:

New features
~~~~~~~~~~~~

- `MIRO Pivot Tables <./charts.html#pivot-chart>`__ introduced with MIRO
  1.1 can now also be used for data input.
- Input widgets can now be
  `grouped <./configuration_symbols.html#tab-grouping>`__, and the order
  of widgets within a group can be changed.
- `Views <./start.html#metadata-views>`__ can now be attached to a
  scenario. A view is a JSON encoded configuration of some renderer. The
  first renderer to support this new feature is the `MIRO pivot table
  renderer <./charts.html#pivot-chart>`__.
- New scenario comparison mode: `pivot
  view <./start.html#compare-pivot>`__. This mode allows you to merge
  datasets of multiple scenarios and analyze them with the `pivot
  table <charts.html#pivot-chart>`__ renderer.

.. _minor-new-features-and-improvements-16:

Minor new features and improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Settings for output tables can now be configured symbol-specific. Read
  more `here <./configuration_tables.html#symbol-table>`__.
- Add option to specify default app title and description when deploying
  MIRO apps. Read more `here <./deployment.html>`__.
- Besides GAMS tables now also normal parameters and sets can be
  configured as maps.
- Add option for text input widgets that causes `element label of a
  singleton set to be the empty
  string <./widgets.html#textbox-option-clearvalue>`__. The idea is to
  use singleton sets to define compile-time variables in the GAMS model
  (read more `here <./model.html#command-line>`__). Note that this is
  currently not supported in Hypercube Mode.
- Handsontables (default input tables) can now be searched. Matching
  results are highlighted and the viewport is changed to show the first
  result. You can move to the next result by pressing ``Enter`` and
  move to the previous result by pressing ``Shift`` + ``Enter``.
- Remove unnecessary options for global input table settings in
  Configuration Mode.
- The filters in the MIRO pivot renderer now remain expanded even after
  selecting a new element.
- Improve accessibility of MIRO apps.
- Development mode: New data is now only imported if the file has
  changed since the last start of MIRO.
- Added option to enable a `column visibility
  button <./configuration_tables.html#export%20buttons>`__ which allows
  to hide columns in big data tables.
- Increase maximum number of scenario attachments from 5 to 7.
- Input tables can now be annotated with Markdown.
- `Big Data Tables <./widgets.html#input-bigdata-table>`__ now support
  adding/removing rows.
- `Value boxes <./charts.html#valuebox>`__ can now be configured
  individually for each scalar value.
- Live preview of the graphics configuration can now be viewed in full
  screen.

.. _bug-fixes-15:

Bug fixes
~~~~~~~~~

- Fix major issue that could result in wrong input data being stored
  after solving a model.
- Improve support of non-ASCII characters in file paths on Windows.
- In Configuration Mode, allow symbols with multiple header aliases of
  the same name to be saved.
- Fix issue where stale data would be displayed when loading scenario
  while an input chart of the previous scenario was still open.
- Fix issue where wrong files could be deleted when a scenario was
  removed from the sandbox.
- Fix issue with downloading results after solving a job asynchronously.
- Fix issue that occurred when several asynchronously solved scenarios
  were downloaded one after the other and then imported into the
  sandbox.
- Fix issue that occurred when trying to display logs of two different
  asynchronously solved scenarios one after the other.
- Fix issue where removing and then adding attachments again would cause
  an unexpected error.
- Fix bug on Windows with local attachments.
- Fix issue where multiple flows could not be displayed on one map -
  only the last flow was visible.

.. _v11:

GAMS MIRO 1.1
-------------

(2020-07-02)

GAMS MIRO 1.1.3
~~~~~~~~~~~~~~~

(2020-07-31)

- Add C:\\GAMS\\XX to the list of paths searched under Windows to find
  GAMS versions >=32 installed in the default location.
- Fix issue where discarding attachments when saving a scenario would
  not work properly if an existing scenario had to be overwritten.
- Fix issue where having GAMS not in the PATH on Windows would result in
  an error message in the log.
- Fix issue where non-zero exit code was not redirected to the parent
  process when deploying a MIRO application.
- Fix issue where apps deployed as single-user could not be imported to
  the library.
- Fix issue with Studio always showing success message when deploying a
  MIRO app, even when there were problems.

GAMS MIRO 1.1.2
~~~~~~~~~~~~~~~

(2020-07-22)

- Fix bug where no groups specified in map configuration would lead to
  crash on next startup of Configuration Mode.
- Fix set configuration: Only show those tools that can be configured
  with with a set
- Fix bugs in Configuration mode with animation options.
- Fix bug that caused a line chart configuration to not be recognized as
  such in some situations when using a GAMS table symbol.
- Fix bug that caused empty xaxis title to be overwritten on next
  startup of Configuration Mode.
- Fix bug that caused slider range preview to fail when configuring a
  dynamic limit.
- Fix bug that caused configured checkbox starting value to be
  overwritten on next start of Configuration Mode.
- Fix bug in date & date range selector configuration that caused
  startview and weekstart option to be overwritten on next start of
  Configuration Mode.
- Fix bug in the configuration schema of the texinput widget that caused
  an error when configuring an empty starting value or placeholder.

GAMS MIRO 1.1.1
~~~~~~~~~~~~~~~

(2020-07-02)

- Fixed an issue where numericinput would send wrong values in certain
  situations

.. _new-features-13:

New features
~~~~~~~~~~~~

- New `pivot table renderer <./charts.html#pivot-chart>`__: An all new
  pivot table renderer can now be selected for input and/or output
  charts. This renderer is designed for large datasets (> 1 million
  elements) and will replace the old pivot table renderer. The old
  renderer is still going to work, but you can not configure it via the
  Configuration Mode any longer. In a future release, we might deprecate
  this old pivot table renderer, so please migrate to the new one! This
  new renderer is the default renderer for input and output symbols.
  This will make the default configuration of MIRO even more powerful!
- Configuration Mode: Graphics configuration now loaded at next start.
- Two GAMS symbols can now be displayed in the same tab (side by side)
  when `grouped <./configuration_symbols.html#tab-grouping>`__.
- Add option to specify files that should be attached to a scenario
  after a GAMS run executes successfully. This allows you to add
  additional data to the data displayed in the user interface and add
  states to your scenarios to implement processes. You can find further
  information
  `here <./configuration_general.html#general-output-attach>`__.
- Add option to include TeX notation in README files via the
  *enableMath* switch. Wrap your TeX formulas in ``$`` tags for
  inline rendering or between ``$$`` for rendering as a formula
  block. Find more information about this feature
  `here <./configuration_general.html#app-readme>`__.
- The `text input widget <./widgets.html#widget-textbox>`__ can now be
  configured for Singleton Sets.
- .. _release-multiple-symbols:

  Multiple GAMS symbols can now be used in one `custom
  renderer <./charts.html#custom-graphics>`__.

.. _minor-new-features-and-improvements-17:

Minor new features and improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Tabs for output symbols can now be hidden. This can be useful, for
  example, if you include a GAMS symbol in MIRO only for the purpose of
  using its data together with another symbol in a custom renderer (see
  `above <#release-multiple-symbols>`__).
- Symbol metadata (symtype and headers) are now passed to the custom
  renderer/custom widget via the options list. The nested list can be
  accessed via the key ``options[["_metadata_"]]``.
- The `MIRO log <./configuration_general.html#miro-log>`__ can now be
  used as an alternative to the GAMS log. This means that it will update
  while the model is running.
- Add option to define the decimal separator character and the thousand
  groups separator character for `numeric
  inputs <./widgets.html#widget-numeric>`__.
- Add legend configuration options to `time series
  diagrams <./charts.html#timeseries-chart>`__.
- Add option for static event lines to `time series
  diagrams <./charts.html#timeseries-chart>`__.
- Add option for dropdown menus that causes `element label of a
  singleton set to be the empty
  string <./widgets.html#dropdown-option-clearvalue>`__. This is only
  supported if aliases are defined. Also note that this is currently not
  supported in Hypercube Mode.
- Add option to `hide indexing
  column <./widgets.html#table-option-hide-indexing-column>`__ in input
  tables. Only available for default input tables and not for `big data
  tables <./widgets.html#input-bigdata-table>`__.
- Minor UI adjustments in input widgets tab
- Add caching for deployed apps which drastically increases their
  startup performance.
- Increase startup performance for the Configuration Mode.
- R is now bundled with MIRO on MacOS.
- Data is now imported when a new MIRO application is added to the
  library instead of at the first start of the application.
- When a new MIRO application is added to the library, the database
  tables are now checked for consistency.
- Add option to remove database tables when removing a MIRO application
  from the library.

.. _bug-fixes-16:

Bug fixes
~~~~~~~~~

- Fix issue with validating GAMS on Linux.
- Configuration Mode: Fixed a bug that caused adding a script with an
  already existing id not to be validated properly in certain
  situations.
- Configuration Mode: Fixed a bug where an incorrect widget
  configuration was displayed for symbols with a JSON configuration that
  is not (yet) supported by Configuration Mode.
- Orphaned tables were not found when model name includes underscores.
- An error that should only occur if the list/log file exceeds a certain
  file size was incorrectly displayed when starting a new GAMS run.
- Model names that included an underscore "\_" caused MIRO to throw an
  exception when downloading scenario data in Hyperube Mode.
- Did not properly set *idir* when running Hypercube jobs locally
  (currently, model has to be adjusted to work with idir).
- Fix bug that caused Hypercube to crash when configuring a dropdown
  menu that is not expanded in Hypercube Mode.
- Fix bug that caused Hypercube job to fail when a multi dropdown menu
  is empty.
- Fix bug that caused wrong value to be written to GDX when having an
  empty multi-dropown menu (no elements selected)
- Fix bug that caused datatables without numeric columns to throw an
  error (e.g. for *sets*)
- Fix bug where attachments with execute permissions were not extracted
  in workdir with SQLite database backend (default on MIRO Desktop)
- Fix bug that caused empty sets not to show up in GDX and therefore to
  throw an error on the GAMS side.
- Fix problem with Safari browser where marked text in the GAMS
  interaction section was unreadable.

.. _v10:

GAMS MIRO 1.0
-------------

(2020-04-08)

GAMS MIRO 1.0.3
~~~~~~~~~~~~~~~

(2020-04-19)

- macOS: Fixed a bug that caused GAMS versions >= 31.x to be rejected.
- Fixed a bug that caused csv files with filename equal to the dataset
  name not being imported. The dataset had to be selected manually.

GAMS MIRO 1.0.2
~~~~~~~~~~~~~~~

(2020-04-16)

- Fixed bug in Configuration Mode that caused custom table headers to be
  reverted to their defaults the next time Configuration Mode was
  started.
- Fixed a bug in Configuration Mode that caused custom scripts arguments
  not to be displayed correctly at the next start.

GAMS MIRO 1.0.1
~~~~~~~~~~~~~~~

(2020-04-14)

- Fixed bug that caused MIRO apps with same version as currently
  installed to be falsely rejected by library.

.. _new-features-14:

New features
~~~~~~~~~~~~

- New deployment environment for configured MIRO applications. You can
  find more information about this `here <./deployment.html>`__. The
  ``--mkapp=1`` option (shortcut creation) is no longer supported.
- MIRO now comes as an independent installer on the platforms Windows,
  macOS and Linux. The installation is independent from the GAMS system.
  All GAMS versions 30.2 or later support GAMS MIRO.
- [MIRO:table] syntax for pivoting a table column in MIRO is no longer
  supported. Instead, a symbol can now simply be declared as a table.
  Read more `here <./model.html#display-tables>`__.
- Added ability to `pivot tables <./widgets.html#table-option-pivot>`__
  in MIRO via the Configuration Mode. Unlike the `table
  statement <./model.html#display-tables>`__ in the GAMS model itself,
  this option does not affect the data contract between GAMS and MIRO
  resulting in variable columns.
- Full `dark mode <./configuration_general.html#dark-mode>`__ support.
- Added `miro log <./configuration_general.html#miro-log>`__ option for
  custom logs and input validation
- Added support for an individual `README starting
  page <./configuration_general.html#app-readme>`__ (markdown document)
  which is displayed in the input section. The Readme file can be
  added/edited in Configuration Mode.
- Added support for own custom scripts for analysis purposes in
  Base and Hypercube Mode.
- New widget type: `numeric Input <./widgets.html#widget-numeric>`__.
- One-dimensional sets can now be configured as dropdown menu.
- New widget type: `Multi-dropdown
  menu <./widgets.html#widget-multidropdown>`__ for one-dimensional,
  dynamic sets.
- Added support for `custom input
  widgets <./configuration_json_only.html#custom-input-widgets>`__.
- Added support for `remote data import and
  export <./configuration_json_only.html#remote-import-export>`__: users
  can import/export data from/to external sources via REST API.
- `Symbol links <./configuration_symbols.html#symbol-links>`__: Data of
  output symbols can now be used to populate tables of input symbols.
- `Sandbox scenarios <./start.html#scenario-concept>`__:

  - The MIRO user interface can be seen as a sandbox. Data that is
    loaded into the interface is therefore located in this sandbox as a
    sandbox scenario. The data of the sandbox scenario can come from the
    MIRO database, external databases, GDX containers, excel files, or
    can be entered manually.
  - File attachments are part of a scenario. If attachments are added to
    a sandbox scenario, they are included when saving this scenario.
  - Sandbox scenarios can be loaded in scenario comparison via *load
    active* button.

- Added filter option for graphs: One dimension of a symbol visualized
  as a graphic can be configured as a filter dimension. Its elements can
  then be selected via a (multi-)drop-down menu. Only the data belonging
  to this selection is displayed in the graphic.

.. _minor-new-features-and-improvements-18:

Minor new features and improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Bug fixes
- New MIRO logo (Thank you `Melina
  Steimel <https://www.linkedin.com/in/melina-steimel-17a829134>`__!)
- Full Chinese `language
  support <./configuration_advanced.html#language-files>`__ (Thank you
  `Ying
  Deng <https://de.linkedin.com/in/ying-deng-7a3554155?trk=profile-badge-cta>`__!)
- Completely restructured Hypercube Mode. This includes the change from
  CSV to GDX file format for scenario input and output data. This is
  especially relevant for the manual job submission.
- Changed directory name for custom renderers from *customRenderer* to
  *renderer\_<modelname>*
- Changed directory name for configuration files from *conf* to
  *conf\_<modelname>*
- Improved performance on startup of MIRO Base/Hypercube Mode
- Added Link to `GAMS World Forum <https://forum.gamsworld.org/>`__ in
  help section.
- *Stop* button in GAMS interaction now with soft kill first, then hard
  kill.
- Scenario comparison: input and output symbol tabs are now displayed in
  different colors.
- Added option to edit scenario access permissions.
- When creating new scenario via `Save as <./start.html#save-as>`__
  dialog, attachments of currently active scenario can now be
  duplicated. This does not apply to `scenario access
  permissions <./start.html#access-permissions>`__.
- Element text of singleton sets can now be appended with a double pipe
  ("\|\|") and will be aggregated/disaggregated when loaded/stored from
  GDX. The value and/or the element text of a singleton Set can be used
  in the
  `eval.Set <https://www.gams.com/latest/docs/UG_DollarControlOptions.html#DOLLARevalset>`__
  option to evaluate the value/text at compile time and place it into a
  scoped compile-time variable. This is useful any information about
  double-dash parameters and GAMS options configured via Configuration
  Mode gets lost when exporting a scenario as a GDX file. Read more
  `here <./model.html#display-sinset>`__.
- Changed prefix for double-dash parameters and GAMS options from
  *GMSPAR\_* / *GMSOPT\_* to
  `\_gmspar\_ <./configuration_advanced.html#widgets-commandLine>`__ /
  `\_gmsopt\_ <./configuration_advanced.html#widgets-commandLine>`__.
- Double-dash parameters and GAMS options can now have the same name as
  GAMS symbols.
- Maximum number of symbol tabs visible in Inpupt/Output section set to
  5 (rest available via dropdown).
- Bootstrap dropdown menus are now right aligned per default. They will
  be left-aligned when space on left is not sufficient.
- "More" dropdown menu (visible when more than 5 input or output tabs
  are shown in MIRO) will now show currently active tab.
- First two columns (symbol name and symbol text) of scalar input table
  are now automatically set to readonly.
- `Import data via spreadsheet <./start.html#excel-import-rules>`__:
  Added possibility to use a sheet dedicated to a single scalar with the
  sheet name being the name of the scalar and its value being in column
  A1.
- When importing scenarios from data\_<modelname> folder, these
  scenarios can now be read and executed by all users within the same
  user group as the user importing them. Before, only the user importing
  them could read and execute them.
- Adding/removing rows is now no longer allowed when at least one column
  is readonly.
- Variables and equations can now be exported via GDX.
- Changed `handling of NAs, zeros and EPS <./start.html#na-zero-eps>`__.
  Numeric NAs are not converted to 0 anymore; Zeros are now written to
  GDX as 0 instead of being squeezed out.
- Increased precision of numeric values input tables to maximum
  precision.
- Custom renderer functions are now extended with ellipsis arguments to
  make sure they are compatible with future MIRO versions.
- Configuration Mode:

  - New structure and design.
  - Added option to `rename tab titles and symbol
    headers <./configuration_symbols.html#naming>`__.
  - Added option to `change the application
    title <./configuration_general.html#application-title>`__.
  - Added option to `reorder symbol
    tabs <./configuration_symbols.html#tab-ordering>`__.
  - Removed option to (de)activate the *scenario functionality* (e.g.
    save scenarios in database). Is always activated now.
  - Removed comment functionality in input tables.
  - Input scalars can now be `used in
    charts <./configuration_symbols.html#hidden-scalars>`__ as well.
  - Charts configuration has now access to MIRO database. Previously
    saved scenario data can now be used for the graphics configuration
    (besides the local file upload).
  - Added option to display graphic and table in
    `split-screen <./charts.html#table-graph-split-screen>`__ for pie
    charts, maps and gantt charts.
  - Added option to remove invalid widget/graph configuration in
    Configuration Mode
  - Added `custom
    renderer <./configuration_advanced.html#custom-renderers>`__ option
    for graphics.
  - Added support for `multiple pie
    charts <./charts.html#multi-pie-chart>`__ via subplots
  - Added support for `heatmaps <./widgets.html#table-option-heatmap>`__
    (input tables).
  - Bar chart, Line chart, Scatter, plot, Bubble chart: Order logic of
    axis elements can now be adjusted.
  - Map: Markers can now be customized (icon, icon color, marker color).
  - Added option to set a fixed height and/or width for pie chart, donut
    chart, bar chart, histogram, scatter plot, line chart, bubble chart.
  - Added option to render a static picture instead of an interactive
    plot for pie chart, donut chart, bar chart, histogram, scatter plot,
    line chart, bubble chart.
  - Added option to set the axis scale ratio for line chart, scatter
    plot, bubble chart.
  - Added option 'area' and 'diameter' for bubble chart size measurement
  - Valuebox: Number of boxes in a row can now be customized.
  - Added option to configure number of decimal places for numericInput
  - Removed remote Execution setting from Configuration Mode. Moved to
    `preferences <./deployment.html#sbs-customize-app>`__ that are
    accessible via the MIRO library.

.. _v06:

GAMS MIRO 0.6
-------------

(2019-06-18)

- MIRO Configuration Mode: The GAMS MIRO Configuration Mode can be used
  to change the settings of your MIRO app as well as to generate new
  widgets and charts. When configuring your widgets and charts, you get
  a live preview so that you immediately see the results of your
  configuration. You don't need to touch JSON code anymore.
- New data import mechanism: Instead of creating an Excel file that the
  user then has to upload in order to get the first data into MIRO, data
  relevant to MIRO is automatically extracted from your model and stored
  in the MIRO database.
- Tab grouping: Group multiple input tabs and output tabs together.
- Automated loading of data on startup: Automatically load data when
  your MIRO app launches, so tables won’t start out empty.
- Automated scenario import into DB: GDX files that you place in the
  data\_<modelname> folder of your MIRO app will be automatically
  imported into the MIRO database the next time you launch your app.
- Improved UTF-8 support: You can now use non-ASCII characters
  throughout MIRO
- Chinese language support (note that the MIRO Configuration Mode is not
  yet translated, though).
- Import and export of GDX files: Besides Excel spreadsheets you can now
  also import and export GDX containers.
- GAMS Studio toolbar to launch MIRO.
- Experimental support for Gantt charts.
- Bug fixes and performance improvements.

.. _v04:

GAMS MIRO 0.4
-------------

(2019-02-19)

Initial release
