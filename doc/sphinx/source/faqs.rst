FAQs
====

General
-------

What GAMS version supports MIRO?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All GAMS versions >= 30.2 support GAMS MIRO.

Which operating systems are supported?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The local version of MIRO runs on Windows, macOS as well as Linux.

Does all the data for my model need to come from MIRO?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

No. You can read external data from any data source supported by GAMS
just as you did before. For example, you can place an Excel file in your
model folder and import the data from this file into GAMS using the
`GDXXRW <https://www.gams.com/latest/docs/T_GDXXRW.html>`__ utility.

What languages are supported?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Currently GAMS MIRO ships with language files for English, German and
Chinese. If you would like to add another language, we are more than
happy to add more languages to this list. For more information about how
to generate language files see the section on `Language
Files <./configuration_advanced.html#language-files>`__

Can I contribute to improve GAMS MIRO?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Yes, absolutely! If you find bugs or have ideas about how to improve
MIRO, you are more than welcome to share those with us! Additionally,
you can contribute by supplying new `language
files <./configuration_advanced.html#language-files>`__ or `custom
renderers <./configuration_advanced.html#custom-renderers>`__! We also
welcome any prototypes/PoCs which you think that can help others to get
started, to improve and/or to come up with new ideas of what can be done
with GAMS MIRO and optimization in general. In any case, please contact
miro@gams.com!

Can I use all solvers which I have a license for with MIRO?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Yes, all solvers with a valid license can be used with MIRO.

Will there be limits to the run time or the model size?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

No, as long as a valid GAMS license exists, there are no limitations
regarding the run time or size of a model.

Can I use MIRO without a GAMS model but work with data only?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Yes, but it should be noted that GAMS MIRO is very closely interwoven
with GAMS. In order to be able to use MIRO without GAMS, adaptations to
the code of GAMS MIRO are necessary.

Can I use `Unicode <https://de.wikipedia.org/wiki/Unicode>`__ characters in MIRO?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Yes. Unicode characters are supported throughout MIRO. However, in case
you use Unicode characters in your GAMS model (e.g. in the explanatory
text), make sure that your source file is UTF-8 encoded. You can change
your file encoding in GAMS Studio via ``Edit->Encoding->convert to...->UTF-8``.

After updating some system libraries on Linux, MIRO displays an error when starting an application. How can I fix it?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Most likely, updating your system libraries has resulted in the removal
of an existing library against which one of the R dependencies was
linked when MIRO was installed. You should see an error message similar
to this: libicui18n.so.70: cannot open shared object file: No such file
or directory.. The error message usually also contains the name of the
affected R library as well as the path where the library is located. To
fix this problem, the library must be recompiled. To force recompilation
of one of the R libraries that MIRO depends on, simply remove the
directory from the MIRO R library (e.g. rm -r
~/.config/miro-library/2.3/stringi/) and add an empty file named
INSTALLING to the root of the library path (e.g. touch
~/.config/miro-library/2.3/INSTALLING). The next time you start MIRO,
the library will be recompiled and linked against the updated system
library.

What do you do in terms on security?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Every code change is peer reviewed by our experienced developers before
it is merged to the main branch. This includes a thorough review in
terms of security. In addition, existing code and application logic are
frequently re-evaluated and discussed. To be able to do this, our
developers constantly keep up to date with the latest developments in
the Infosec community regarding new vulnerabilities as well as new
strategies to attack software. This includes regular informal
penetration testing by our developers on our development servers. In
addition to manual reviews, all of our code is tested as part of our
continuous integration pipelines using multiple static (SAST)
application security testing tools. We also use automated tools to test
all of our third-party dependencies for known vulnerabilities (Software
Composition Analysis). You can find the latest vulnerability report
`here <./security_ci_report.html>`__.

Features
--------

How can I customize my MIRO app?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The configuration can be done via a `graphical configuration
interface <./customize.html#configurationMode>`__, with which you can
create plots and widgets or change certain settings with a few mouse
clicks, visually supported by a live preview. In addition, the
configuration can also be done
`manually <./configuration_advanced.html.html#jsonfiles>`__ via the
<modelname>.json file inside the directory:
<modeldirecory>/<modelname>.json.

Do I have visualization options other than those shipping with GAMS MIRO?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Yes. You can write your own `Custom
Renderers <./configuration_advanced.html#custom-renderers>`__ in R using
MIRO's R API.

Can I also write my own custom functions to generate input data?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Yes. MIRO has an API that allows you to use e.g. charts to produce input
data. We call this feature *custom input widgets*. These are not
available in Configuration Mode, but must be implemented manually. You
can find more information
`here <./configuration_json_only.html#custom-input-widgets>`__.

Can I use an Excel spreadsheet to import data?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Of course! You only have to make sure that it is `properly
formatted <./start.html#excel-import-rules>`__.

Can I set up GAMS MIRO on a server where multiple users can solve GAMS models?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Yes. Besides the presented MIRO Desktop version we also develop a `MIRO
Server version <./server.html>`__, which will offer features like
multi-application management, user access management, load balancing,
rolling updates, etc.

What happens when using input data which results in infeasible or unbounded results?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

| In GAMS MIRO both the log and the lst file of the current GAMS model
  run can be displayed. Any information about infeasibilities and
  unboundness is also visible here. Further checks on infeasibilities
  and unboundness are not performed. If a model is infeasible (or
  unbounded) this still leads to a normal completion of the GAMS job
  (unless you check the model status and trigger an abort based on an
  unexpected model status). A normal completion of GAMS is interpreted
  by GAMS MIRO as an error-free run. The view switches to the output
  section and will show the content of your output symbols (even if they
  are empty or show the infeasible solution provided by some first phase
  algorithm). If the GAMS model is aborted for any reason, this will
  also result in an abort in GAMS MIRO. A corresponding message is
  displayed in GAMS MIRO and no output is displayed.
| **Note**: You can write an `own report /
  log <./configuration_general.html#miro-log>`__ and integrate it into
  MIRO which is then visible in the GAMS interaction section. In such a
  custom report you can also deal with infeasibilities and unboundness.

.. _files:

GAMS MIRO related files
-----------------------

.. _file-io:

<modelname>_io.json
~~~~~~~~~~~~~~~~~~~~

The *<modelname>_io.json* file, for example *transport_io.json*, holds
the `GAMS/MIRO data contract <./model.html#data-contract>`__ and is
located in the *<modelname>/conf\_<modelname>* directory. It contains
the metadata of all input and output symbols defined between the
``$onExternalInput`` / ``$offExternalInput`` and ``$onExternalOutput`` /
``$offExternalOutput`` tags in the corresponding GAMS model. The
*<modelname>_io.json* file is created each time a MIRO app ist started
via GAMS Studio and should not be changed. If you use the command line,
please follow `these
steps <./getting_started.html#start-via-command-line>`__.

.. _file-config:

<modelname>.json
~~~~~~~~~~~~~~~~~

| The *<modelname>.json* file, for example *transport.json*, is located
  in the *<modelname>/conf\_<modelname>* directory. It contains the
  configuration of graphics, input widgets, language and all other
  model-specific configurations for a MIRO app. This file can be filled
  manually by the user. However, the easiest way for the configuration
  is the `Configuration Mode <./customize.html#configurationMode>`__.
| Like the *<modelname>_io.json* file, the structure of this file is
  based on a `schema <./schema.html>`__ with which the file is validated
  when a MIRO app is started.

.. _file-default:

default.gdx
~~~~~~~~~~~~

Every time you start a MIRO app via GAMS Studio or the command line
(i.e. in development mode), a gdx container is created in the directory
data\_<modelname>. This file contains data extracted from the GAMS model
and will be stored in the MIRO database under the scenario name:
*default* at the next start of your app.

.. _file-miroconf:

<modelname>\_<version_number>.miroconf / <modelname>\_<version_number>_hcube.miroconf
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These files are generated when starting a MIRO app in development mode.
They contain the configuration and initialization steps of an app and
can by used by an `executable <#file-executable>`__ for a fast startup
of MIRO. Learn more `here <./model.html#development-mode>`__.

Definitions
-----------

.. _def-model:

What is a **model** in the context of GAMS MIRO?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We call any .gms file that you run with the command line option miro a
model. Inside this .gms file you may include or call other files or
applications, just as you would do with any GAMS program.

.. _def-scenario:

What is a **scenario** in the context of GAMS MIRO?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We call the collection of input and output datasets that result from a
particular model run and which are communicated with MIRO a scenario.

.. _def-job:

What does **Job** mean in the context of GAMS MIRO?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A *Job* describes the submission of one or more GAMS runs.

.. _def-hypercube-job:

What does **Hypercube Job** mean in the context of GAMS MIRO?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A *Hypercube job* describes a set of scenarios for a particular model
which MIRO generated automatically by expanding the cartesian product
over all selected scalars. Input widgets are automatically expanded when
a Hypercube job is created (e.g. a slider becomes a slider range, a
single dropdown becomes a multi-dropdown, etc.). This way you can
configure multiple scenarios at once. For more information see the
section about `Hypercube jobs <./hc-module.html>`__.

.. _def-job-tag:

What is a (Hypercube) **job tag**?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A Hypercube *job tag* is an identifier attached to all scenarios of a
`Hypercube job <#def-hypercube-job>`__. Job tags can help you find
scenarios of a particular Hypercube job. They can also be useful for
scenario analysis.

.. _def-development-mode:

What is meant by **development mode**?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You are in *development mode* when you start a MIRO app from GAMS Studio
(or via the command line with the environment variable ``MIRO_DEV_MODE`` set
to ``true``). Every time you launch MIRO in development mode, your app
resources (configuration, README file, data files etc.) are loaded and
validated, so your application is always in sync with your current GAMS
source. Once you want to share your application with other people you
can `deploy <./deployment.html>`__ it. Deployed apps work with a
compiled version of your resources, so they tend to launch faster.
However, this also means that you can no longer change your app without
deploying a new version.

Licensing
---------

What license is GAMS MIRO released under?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GAMS MIRO is released under version 3 of the GNU General Public License.
For a copy of this license, see http://www.gnu.org/licenses/. Full
license information, including third-party software included in MIRO,
can be found `here <./license.html>`__. The entire source code of GAMS
MIRO can be found on `GitHub <https://github.com/GAMS-dev/miro>`__.

Troubleshooting
---------------

Why do I get "Domain Violation" errors when editing sets or parameters?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Modifying a parameter index in MIRO can throw a domain violation error
because the underlying set was not imported into MIRO.

**Fix (GAMS):**

Use `Implicit Set
Definition <https://www.gams.com/latest/docs/UG_SetDefinition.html#UG_SetDefinition_ImplicitSetDefinition>`__.
Attach a "<" to the domain in the parameter declaration:

.. code:: gams

   Parameter d(i<,j<);

This dynamically populates the set from the parameter values.

**Fix (GAMSPy):**

Use `domain
forwarding <https://gamspy.readthedocs.io/en/latest/user/basics/set.html#implicit-set-definition-via-domain-forwarding>`__:

.. code:: python

   domain_forwarding=True

Why can't MIRO find GAMS or Python?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

MIRO may fail to execute the model because it cannot locate the GAMS
installation or the Python environment.

Open MIRO Desktop and go to:

- **Windows/Linux:** ``File → Preferences → Paths``
- **macOS:** ``GAMS MIRO → Preferences → Paths``

Explicitly specify the correct GAMS path and Python path.

Why do I get "Encoding Not Supported" errors?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All GAMS and GAMSPy source files used with MIRO must be UTF-8 encoded.

In GAMS Studio, you can convert file encodings via:

``Edit → Encoding → convert to... → UTF-8``

Why do deployed apps fail with "File Not Found" errors?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The app may run correctly during development but fail after deployment
because required external files were not included in the assembly
process.

Ensure that all auxiliary files (Excel files, CSVs, scripts, etc.) are
included in the `model assembly
file <./deployment.html#model-assembly-file>`__ .

When using the deployment option *Local multi-user (uses idir)*, the
model executes in a temporary directory. Relative paths in ``$call`` or
``execute`` statements can therefore break.

Use:

.. code:: text

   %gams.idir1%

as a prefix for deployed file references, for example:

.. code:: text

   $call gdxxrw i=%gams.idir1%test.xlsx

Why are charts empty or aggregated into a single line?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default, MIRO only recognizes the configured value column. If no
grouping dimension is selected, all data may be aggregated into a single
series.

Open the chart configuration and go to:

*Filters & Domains*

Then select the appropriate grouping dimension (for example a stock
symbol, region, or scenario) so MIRO can separate the data correctly.

Support
-------

Who should I contact in case I have problems or remarks?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can use any of the channels you also have if you face problems with
the core GAMS system. However, the best way is via our
`forum <https://forum.gams.com/c/gams-miro/12>`__. This way, other users
who might have similar questions can profit as well from the answers you
receive. Alternatively you can also contact us by mail at miro@gams.com
