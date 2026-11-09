File formats
============

.. _file-formats:

Introduction
------------

The following section describes the custom file formats used in GAMS MIRO, namely *.miroapp* for app bundles and *.miroscen* for MIRO scenarios.

This section is intended for readers with a technical background who want to implement their own tools for handling these files or better understand how MIRO works under the hood.

*.miroapp* files are used to bundle a MIRO app including all the model files. *.miroscen* files bundle all the data that belongs to a MIRO scenario (data as well as metadata). Both file formats are (unencrypted) ZIP files (using `zlib <https://zlib.net>`__).

.. _miroapp-file:

MIROAPP
~~~~~~~

The table below lists all the files contained in a *.miroapp* file:

.. list-table::
   :header-rows: 1

   * - filename
     - description
   * - *miroapp.json*
     - A JSON file containing the app metadata. The schema for this file is given below.
   * - *.miroconf*
     - An .RData file that contains the validated app configuration in binary form.
   * - *<modelName>.zip*
     - An (unencrypted) ZIP file (zlib) containing all the model files specified in the `model assembly file <./deployment.html#model-assembly-file>`__. Here *modelName* is the lowercase filename of the main GMS file without the file extension.
   * - *data_<modelName>/*
     - (optional) Directory with scenario data that will be imported to the MIRO database when adding this app.
   * - *static_<modelName>/*
     - (optional) Directory with static files such as images and stylesheets. This directory will be added to the Shiny web server so that all files in this directory are accessible from the browser.
   * - *renderer_<modelName>/*
     - (optional) Directory with custom renderer and custom importer/exporter files.
   * - *scripts_<modelName>/*
     - (optional) Directory with custom analysis scripts.

The JSON schema used to validate the *miroapp.json* is included below:

.. code-block:: json

   {
       "$schema":"http://json-schema.org/draft-07/schema#",
       "title":"MIROAPP metadata schema",
       "type":"object",
       "properties": {
           "version": {
           "description":"The version of the MIROAPP file (current: 1)",
           "type":"integer",
           "minimum": 1
           },
           "api_version": {
           "description":"The MIRO API version that was used to deploy this application. When adding a new MIRO app, the system checks whether the MIRO API version matches the API version of this app. If this is not the case, the app is rejected.",
           "type":"string"
           },
           "miro_version": {
           "description":"The MIRO version that was used to deploy this application. When adding a new MIRO app, the system checks whether the MIRO version is greater than the MIRO version with which this app was deployed. If this is not the case, the app is rejected.",
           "type":"string"
           },
           "main_gms_name": {
           "description":"The (case-sensitive) filename of the main GMS file including the file extension.",
           "type":"string"
           },
           "timestamp": {
           "description":"The time the app was deployed. Format: yyyy-MM-dd hh:mm:ss zzz (e.g.: 2021-12-15 10:28:33 UTC).",
           "type":"string"
           },
           "host_os": {
           "description":"The operating system that was used to deploy this application.",
           "type":"string",
           "enum": ["windows", "osx", "linux"]
           },
           "modes_included": {
           "description":"The modes included in this app. Since MIRO version 2.2 only base is allowed.",
           "type":"string",
           "enum": ["base"]
           },
           "use_temp_dir": {
           "description":"Whether to run the model in a temporary directory. MIRO Server will reject apps where this flag is false.",
           "type":"string"
           },
           "created_by": {
           "description":"Human-readable string indicating which tool was used to create this MIROAPP file.",
           "type":"string"
           }
       }
   }

.. _signed-miroapp-file:

Signed MIROAPP
~~~~~~~~~~~~~~

*.miroapp* files can be optionally signed to verify the integrity of the app and the authenticity of the author. `RSA <https://en.wikipedia.org/wiki/RSA_(cryptosystem)>`__ keys with a minimum length of 2048 bits are supported for signing MIRO apps. Signing a MIRO app works as follows:

1. A file *.miro_hashes* is created that contains the SHA-256 hashes of all the files in the app bundle. Each line of this file identifies a file in the bundle as follows: ``<fileName>\/\<sha256Hash>`` where *fileName* is the full path of the file relative to the root of the *.miroapp* archive. The lines are sorted in ascending order.
2. The file *.miro_hashes* created in 1) is hashed using the SHA-512 algorithm and then signed with the RSA private key using the *EVP_PKEY_sign* function of OpenSSL. The resulting signature is written to the binary file: *.miro_sig*.
3. The RSA public key is written to a file *.miro_pubkey*.
4. The files *.miro_hashes*, *.miro_sig* and *.miro_pubkey* are appended to the root of the .miroapp ZIP archive.

To validate the authenticity of the app, it is first checked whether the app is signed (the files *.miro_hashes* and *.miro_sig* exist). If this is the case, the integrity of the MIRO app is checked as follows:

1. All files of the archive are listed, except the signature files (*.miro_hashes*, *.miro_sig* and *.miro_pubkey*).
2. The SHA-256 hashes of all files found in the archive are calculated and a *.miro_hashes_actual* file is created using the same scheme as described in step 1).
3. The *.miro_hashes* file is compared with the *.miro_hashes_actual* file. If they are not identical, MIRO reports an invalid signature.
4. The SHA-512 hash of the *.miro_hashes* file is computed and the signature *.miro_sig* is verified using the *EVP_PKEY_verify* function of OpenSSL. To verify the signature, all public keys registered on the computer where MIRO is installed are tried. If none of the public keys work, the *.miro_pubkey* public key included in the archive is tried. If this does not work either, MIRO reports an invalid signature. If it works, MIRO computes the OpenSSH fingerprint (using the SHA256 hashing algorithm) of this key and the user is asked whether to trust this key. If the user agrees, the *.miro_pubkey* public key is added to the list of known public keys and the app is imported.

.. _miroscen-file:

MIROSCEN
~~~~~~~~

The table below lists all the files contained in a *.miroscen* file:

.. list-table::
   :header-rows: 1

   * - filename
     - description
   * - *metadata.json*
     - A JSON file containing the scenario metadata. The schema for this file is given below.
   * - *data.gdx*
     - A GDX file containing the input and output data of all symbols specified in the GAMS/MIRO data contract.
   * - *views.json*
     - (optional) A file containing scenario-specific/local views.
   * - *attachments/*
     - (optional) Directory with files attached to the scenario. The metadata of the attachments must be contained in the *metadata.json* file (see schema below).

The JSON schema used to validate the *metadata.json* is included below:

.. code-block:: json

   {
       "$schema":"http://json-schema.org/draft-07/schema#",
       "title":"MIROSCEN metadata schema",
       "type":"object",
       "properties": {
           "version": {
           "description":"The version of the MIROSCEN file (current: 1)",
           "type":"integer",
           "minimum": 1
           },
           "scen_name": {
           "description":"The name of the scenario as specified by the user.",
           "type":"string",
           "minLength": 1,
           "maxLength": 64
           },
           "scen_tags": {
           "description":"Tags assigned to the scenario. The total number of characters when combining the array into a comma-separated string must not exceed 1000.",
           "type":"array",
           "uniqueItems":true,
           "items":{
               "type":"string",
               "minLength": 1
           }
           },
           "cl_args": {
           "description":"Scenario-specific command line arguments (dollar control options as well as GAMS options).",
           "type":"array",
           "uniqueItems":true,
           "items":{
               "type":"string",
               "minLength": 1
           }
           },
           "attachments": {
           "description":"Attachment metadata.",
           "type":"array",
           "items": {
               "type": "object",
               "properties": {
               "name": {
                   "description":"The filename of the attachment (including extension).",
                   "type":"string"
               },
               "execPerm": {
                   "description":"Whether the model can read the attachment (if true, the attachment is downloaded to the working directory before a run).",
                   "type":"boolean"
               }
               }
           }
           },
           "model_raw": {
           "description":"The (case-sensitive) filename of the main GMS file excluding the file extension.",
           "type":"string"
           },
           "uid": {
           "description":"The user ID of the owner of the scenario.",
           "type":"string",
           "minLength": 1
           },
           "time_created": {
           "description":"The time the MIROSCEN file was created. Format: yyyy-MM-dd hh:mm:ss zzz (e.g.: 2021-12-15 10:28:33 UTC).",
           "type":"string"
           },
           "created_by": {
           "description":"Human-readable string indicating which tool was used to create this MIROSCEN file.",
           "type":"string"
           }
       }
   }
