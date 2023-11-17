import json
import sys


def main():
    report_location = sys.argv[1]
    audit_files = sys.argv[2:]

    audit_data_raw = []

    for audit_file in audit_files:
        if not (audit_file.startswith('audit-') and audit_file.endswith('.json')):
            raise ValueError(f'Invalid audit file name: {audit_file}')

        with open(audit_file, 'r') as f:
            audit_data_tmp = json.loads(f.read())
            audit_data_tmp['service_id'] = audit_file[len('audit-'):-len('.json')]
            audit_data_raw.append(audit_data_tmp)

    audit_report = {'data_raw': audit_data_raw, 'vuln_data': {}}

    for dep_file in audit_data_raw:
        audit_report['vuln_data'][dep_file['service_id']] = []
        for dependency in dep_file['dependencies']:
            for vuln in dependency['vulns']:
                vuln['service_id'] = dep_file['service_id']
                vuln['message'] = f"{dep_file['service_id']}: {dependency['name']} {dependency['version']}"
                audit_report['vuln_data'][dep_file['service_id']].append(vuln)

    with open(report_location, 'w') as f:
        f.write(json.dumps(audit_report))


if __name__ == '__main__':
    main()
