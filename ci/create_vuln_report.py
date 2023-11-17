import json
import os
import sys

import pandas as pd

JS_DEP_REPORT_LOCATION = 'npm-audit-report.json'
PIP_AUDIT_REPORT_LOCATION = 'pip-audit-report.json'
R_AUDIT_REPORT_LOCATION = 'r-audit-report.json'
CONTAINER_SCAN_REPORT_PREFIX = 'gl-container-scanning-report-'
with open('package.json', 'r') as f:
    MIRO_VERSION = json.loads(f.read())['version']


def create_vuln_df_js_dep(report_location, whitelist_location=None):
    with open(report_location, 'r') as f:
        vuln_raw = json.loads(f.read())
    vuln_grouped = []
    whitelist_raw = {}
    if whitelist_location:
        with open(whitelist_location, 'r') as f:
            whitelist_raw = json.loads(f.read())

    for component in ['launcher', 'ui', 'server_admin']:
        whitelist = []
        if component in whitelist_raw:
            for fingerprint in whitelist_raw[component]:
                whitelist.append(fingerprint)
            if 'global' in whitelist_raw:
                whitelist.extend(whitelist_raw['global'])
        no_vuln = len(vuln_raw[component]['vulnerabilities'])
        no_whitelisted = 0
        for package in vuln_raw[component]['vulnerabilities']:
            vuln = vuln_raw[component]['vulnerabilities'][package]
            cve_url = vuln['via'][0]['url']
            if cve_url is not None and cve_url in whitelist:
                no_whitelisted += 1
            else:
                if cve_url is None:
                    cve_url = '!!!UNDEFINED CVE URL!!!'
                print(
                    f"{component}: {vuln['severity']} ({cve_url}): {vuln['via'][0]['title']}")

        vuln_grouped.append({
            'service_id': component,
            'tool': 'npm audit',
            'description': 'Testing for vulnerable Javascript dependencies',
            'identified': no_vuln,
            'confirmed': no_vuln - no_whitelisted,
            'passed': no_vuln == no_whitelisted
        })
    return pd.DataFrame.from_records(vuln_grouped)


def create_vuln_df_container_scan(report_location, whitelist_location=None):
    container_name = report_location[len(
        CONTAINER_SCAN_REPORT_PREFIX):-len('.json')]
    with open(report_location, 'r') as f:
        vuln_raw = json.loads(f.read())
    no_vuln = len(vuln_raw['vulnerabilities'])
    whitelist = []
    if whitelist_location:
        with open(whitelist_location, 'r') as f:
            whitelist_raw = json.loads(f.read())
            if container_name in whitelist_raw:
                for fingerprint in whitelist_raw[container_name]:
                    whitelist.append(fingerprint)
            if 'global' in whitelist_raw:
                whitelist.extend(whitelist_raw['global'])

    no_whitelisted = 0

    for vuln in vuln_raw['vulnerabilities']:
        cve_id = None
        for identifier in vuln['identifiers']:
            if identifier['type'] == 'cve':
                cve_id = identifier['value']
                break
        if cve_id is not None and cve_id in whitelist:
            no_whitelisted += 1
        else:
            if cve_id is None:
                cve_id = '!!!UNDEFINED CVE ID!!!'
            print(
                f"{container_name}: {vuln['severity']} ({cve_id}): {vuln['description']}")

    return pd.DataFrame.from_records([{
        'service_id': container_name,
        'tool': vuln_raw['scan']['scanner']['id'],
        'description': 'Testing for vulnerable dependencies in container image',
        'identified': no_vuln,
        'confirmed': no_vuln - no_whitelisted,
        'passed': no_vuln == no_whitelisted
    }])


def create_vuln_df_pip_audit(report_location, whitelist_location=None):
    with open(report_location, 'r') as f:
        vuln_raw = json.loads(f.read())
    whitelist = {}
    if whitelist_location:
        with open(whitelist_location, 'r') as f:
            whitelist = json.loads(f.read())

    report_data = []

    for service_id in vuln_raw['vuln_data']:
        no_vuln = len(vuln_raw['vuln_data'][service_id])
        no_whitelisted = 0
        for vuln in vuln_raw['vuln_data'][service_id]:
            if service_id in whitelist and vuln['id'] in whitelist[service_id].keys():
                no_whitelisted += 1
            else:
                print(vuln['message'])
        report_data.append({
            'service_id': service_id,
            'tool': 'pip-audit',
            'description': 'Testing for vulnerable Python dependencies',
            'identified': no_vuln,
            'confirmed': no_vuln - no_whitelisted,
            'passed': no_vuln == no_whitelisted
        })

    return pd.DataFrame.from_records(report_data)


def create_vuln_df_r_audit(report_location, whitelist_location=None):
    with open(report_location, 'r') as f:
        vuln_raw = json.loads(f.read())
    whitelist = {}
    if whitelist_location:
        with open(whitelist_location, 'r') as f:
            whitelist = json.loads(f.read())

    report_data = []

    no_vuln = len(vuln_raw['vuln_data'])
    no_whitelisted = 0
    for vuln in vuln_raw['vuln_data']:
        if vuln['cvss_id'] in whitelist.keys():
            no_whitelisted += 1
        else:
            print(vuln['description'])
    report_data.append({
        'service_id': 'ui',
        'tool': 'oysteR',
        'description': 'Testing for vulnerable R dependencies',
        'identified': no_vuln,
        'confirmed': no_vuln - no_whitelisted,
        'passed': no_vuln == no_whitelisted
    })

    return pd.DataFrame.from_records(report_data)


def main():
    print(f'Generating vulnerability report for MIRO version: {MIRO_VERSION}')

    reports_dfs = []
    reports_dfs.append(create_vuln_df_js_dep(JS_DEP_REPORT_LOCATION,
                                             'ci/vuln_whitelists/npm_audit_whitelist.json'))
    reports_dfs.append(create_vuln_df_r_audit(R_AUDIT_REPORT_LOCATION,
                                              'ci/vuln_whitelists/r_audit_whitelist.json'))

    container_scan_reports = [f for f in os.listdir(
        '.') if f.startswith(CONTAINER_SCAN_REPORT_PREFIX)]

    for container_scan_report in container_scan_reports:
        reports_dfs.append(create_vuln_df_container_scan(container_scan_report,
                                                         'ci/vuln_whitelists/container_scan_whitelist.json'))

    reports_dfs.append(create_vuln_df_pip_audit(PIP_AUDIT_REPORT_LOCATION,
                       'ci/vuln_whitelists/pip_audit_whitelist.json'))

    report_df = pd.concat(reports_dfs)
    if len(report_df[report_df['passed'] == False].index) > 0:
        print(report_df[report_df['passed'] == False])
        sys.exit(1)

    report_df = report_df.rename(columns={
        'service_id': 'Component',
        'tool': 'Tool',
        'description': 'Description',
        'identified': 'Identified',
        'confirmed': 'Confirmed',
        'passed': 'Passed'
    })
    html_to_write = report_df.to_html(index=False)
    html_to_write = html_to_write.replace('border="1"', '').replace(
        'class="dataframe"', 'class="table table-striped"').replace(' style="text-align: right;"', '').replace(
            '<th>Identified</th>', '<th style="white-space:nowrap;">Identified <a href="#" data-toggle="tooltip" data-placement="bottom" title="Number of vulnerabilities identified by the scanner"><i class="fas fa-info-circle"></i></a></th>').replace(
                '<th>Confirmed</th>', '<th style="white-space:nowrap;">Confirmed <a href="#" data-toggle="tooltip" data-placement="bottom" title="Number of vulnerabilities after manual review of vulnerabilities detected by the scanner"><i class="fas fa-info-circle"></i></a></th>').replace(
                    'True', '<i class="fas fa-check-circle"></i>')
    html_to_write = f'''<p>Vulnerability scan report for GAMS MIRO {MIRO_VERSION}</p>
    <div class="table-responsive">
        {html_to_write}
    </div>
    '''

    with open('ci-vuln-report.html', 'w') as f:
        f.write(html_to_write)


if __name__ == '__main__':
    main()
