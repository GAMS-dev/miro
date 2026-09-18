GAMS MIRO Server
================

Introduction
------------

In the most basic setup - GAMS MIRO Desktop - both the MIRO application
and GAMS itself are installed on the same computer and run exclusively
on this machine. But MIRO is not limited to this setup: You can run your
MIRO applications locally, but solve the GAMS jobs in the cloud. We call
this setup *GAMS MIRO Desktop - boosted by GAMS Engine*. `GAMS
Engine <https://www.gams.com/engine/>`__ is a server software that
allows you to run GAMS models on centralized compute resources. It uses
state-of-the-art technology to handle the entire job management,
scheduling and load balancing of your computationally intensive
optimization problems.

GAMS Engine is also used for *GAMS MIRO Server*. This setup allows you
to run both the GAMS execution engine as well as the MIRO application
itself in the cloud. Your optimization applications can then be accessed
from any device with a modern web browser like a PC, smartphone or
tablet.

.. image:: ../../assets/images/remote-execution.png
   :alt: GAMS MIRO Server
   :class: miro-image-max-height-400pt
   :align: center

.. admonition:: Note:
   :class: warning

   Both *GAMS MIRO Desktop - boosted by GAMS Engine* and *GAMS MIRO Server*
   make use of `GAMS Engine <https://www.gams.com/sales/engine_facts/>`__,
   which must be purchased separately. Please contact
   `sales@gams.com <mailto:miro@gams.com>`__ if you are interested in one
   of these setups.

.. admonition:: Note:
   :class: warning

   Currently, GAMS MIRO Server is only available for MIRO applications
   based on GAMS models, not for GAMSPy models.

.. _cards-wrapper:

.. grid:: 1 2 3 3
   :gutter: 2


   .. grid-item-card::
      :text-align: center
      :link: server_general.html
      :link-type: url

      .. raw:: html

         <i class="miro-card-icon fa-solid fa-globe" aria-hidden="true"></i>

      Moving to the cloud
      ^^^

      Make MIRO apps available to people all over the world.

   .. grid-item-card::
      :text-align: center
      :link: server_setup.html
      :link-type: url

      .. raw:: html

         <i class="miro-card-icon fa-solid fa-server" aria-hidden="true"></i>

      Server Setup
      ^^^

      Install and configure GAMS MIRO Server

   .. grid-item-card::
      :text-align: center
      :link: server_administration.html
      :link-type: url

      .. raw:: html

         <i class="miro-card-icon fa-solid fa-users-cog" aria-hidden="true"></i>

      Administration
      ^^^

      App- and user management, access control

   .. grid-item-card::
      :text-align: center
      :link: server_api.html
      :link-type: url

      .. raw:: html

         <i class="miro-card-icon fa-solid fa-box-open" aria-hidden="true"></i>

      MIRO Server API
      ^^^

      Manage apps and scenario data via a RESTful API

   .. grid-item-card::
      :text-align: center
      :link: server_architecture.html
      :link-type: url

      .. raw:: html

         <i class="miro-card-icon fa-solid fa-sitemap" aria-hidden="true"></i>

      Architecture
      ^^^

      A look behind the scenes

.. toctree::
   :caption: Server documentation
   :maxdepth: 2
   :hidden:

   server_general
   server_setup
   server_administration
   server_architecture
   server_api
